/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include "include.h"
#include <memory.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <setjmp.h>

#undef GLOBAL				/* avoid conflicts */
#undef HAVE_STDLIB_H
#include <jpeglib.h>
#include <jerror.h>

#define RESCALE(v, o, n)	((v) * (n) / (o))

extern void	jpeg_iostream_src(j_decompress_ptr cinfo, IOSTREAM* infile);

struct my_jpeg_error_mgr
{ struct jpeg_error_mgr	jerr;
  jmp_buf 		jmp_context;
};


static void
my_exit(j_common_ptr cl)
{ struct jpeg_decompress_struct *cinfo = (struct jpeg_decompress_struct *)cl;
  struct my_jpeg_error_mgr *jerr = (struct my_jpeg_error_mgr *)cinfo->err;

  longjmp(jerr->jmp_context, 1);
}


static XImage *
freshXImage(Display *dsp, int depth, int width, int height)
{ XImage *i;
  int bitmap_pad;

  switch(depth)
  { case 16:
      bitmap_pad = 16;
      break;
    case 24:
    case 32:
      bitmap_pad = 32;
      break;
    default:
      assert(0);
      return NULL;
  }

  /* First pass data and bytes_per_line as 0.  XCreateImage() fills these
     with proper values
  */

  if ( !(i = XCreateImage(dsp,
			  DefaultVisual(dsp, DefaultScreen(dsp)),
			  depth, ZPixmap, 0,
			  NULL,
			  width, height,
			  bitmap_pad, 0)) )
    return NULL;

  if ( !(i->data = malloc(i->bytes_per_line * height)) )
  { XDestroyImage(i);

    return NULL;
  }

  return i;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Try to load a JPEG file directly into memory.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

img_status
staticColourReadJPEGFile(Image image, IOSTREAM *fd, XImage **return_image)
{ struct jpeg_decompress_struct cinfo;
  struct my_jpeg_error_mgr jerr;
  long here = Stell(fd);
  XImage *img = NULL;
  JSAMPLE *line = NULL;
  int r_shift, g_shift, b_shift, r_bright, g_bright, b_bright;
  int y;
  DisplayObj d = image->display;
  DisplayWsXref r;
  img_status rval = IMG_OK;

					/* check enviroment */
  if ( isNil(d) )
    d = CurrentDisplay(image);
  r = d->ws_ref;

  if ( r->depth < 16 )			/* better test? */
    return IMG_NO_STATIC_COLOUR;
  
					/* setup JPEG error handling */
  cinfo.err = jpeg_std_error((struct jpeg_error_mgr *)&jerr);
  if ( setjmp(jerr.jmp_context) )
  { switch(jerr.jerr.msg_code)
    { case JERR_OUT_OF_MEMORY:
	rval = IMG_NOMEM;
	break;
      case JERR_NO_SOI:
	rval = IMG_UNRECOGNISED;
	break;
      default:
      DEBUG(NAME_image,
	    { char buf[1024];

	      (*jerr.jerr.format_message)((j_common_ptr)&cinfo, buf);
	      Cprintf("JPEG: %s\n", buf);
	    });
        rval = IMG_INVALID;
    }

    jpeg_destroy_decompress(&cinfo);

    Sseek(fd, here, SEEK_SET);

    return rval;
  }
  jerr.jerr.error_exit = my_exit;

					/* open the image */
  jpeg_create_decompress(&cinfo);
  jpeg_iostream_src(&cinfo, fd);
  jpeg_save_markers(&cinfo, JPEG_COM, 0xffff);
  jpeg_read_header(&cinfo, TRUE);
  jpeg_start_decompress(&cinfo);

  if ( !(line = pceMalloc(3*cinfo.output_width*sizeof(JSAMPLE))) )
  { rval = IMG_NOMEM;
    goto out;
  }
  if ( !(img = freshXImage(r->display_xref,
			   r->depth,
			   cinfo.output_width,
			   cinfo.output_height)) )
  { rval = IMG_NOMEM;
    goto out;
  }

  r_shift  = shift_for_mask(img->red_mask);
  g_shift  = shift_for_mask(img->green_mask);
  b_shift  = shift_for_mask(img->blue_mask);
  r_bright = img->red_mask   >> r_shift;
  g_bright = img->green_mask >> g_shift;
  b_bright = img->blue_mask  >> b_shift;

  for( y=0; cinfo.output_scanline < cinfo.output_height; y++ )
  { JSAMPLE *i;
    int x;

    jpeg_read_scanlines(&cinfo, &line, 1);

    for(x=0, i=line; x<cinfo.output_width; x++)
    { unsigned long pixel;
      unsigned long r = *i++;
      unsigned long g = *i++;
      unsigned long b = *i++;

      pixel = ( (RESCALE(r, 255, r_bright) << r_shift) |
		(RESCALE(g, 255, g_bright) << g_shift) |
		(RESCALE(b, 255, b_bright) << b_shift) );
      
      XPutPixel(img, x, y, pixel);
    }
  }

  if ( cinfo.marker_list )
  { jpeg_saved_marker_ptr m;
    Chain ch;

    attributeObject(image, NAME_comment, (ch=newObject(ClassChain, EAV)));

    for(m = cinfo.marker_list; m; m = m->next )
    { if ( m->marker == JPEG_COM )
      { string s;

	str_inithdr(&s, ENC_ASCII);
	s.size = m->data_length;
	s.s_text8 = m->data;

	appendChain(ch, StringToString(&s));
      }
    }
  }

  jpeg_finish_decompress(&cinfo);
out:
  if ( line )
    pceFree(line);
  jpeg_destroy_decompress(&cinfo);
  
  if ( img )
    *return_image = img;

  return rval;
}


