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

#include "include.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Actually, this module does both medium-level   JPEG and GIF writing. The
low-level routines are in the img   directory. These routines are called
by msimage.c, which in  turn  implements   the  OS  specific  version of
gra/image.c implementing class image.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef __CYGWIN32__
#define XMD_H
#endif

#ifdef HAVE_LIBJPEG
/*#define boolean jpeg_boolean*/
#undef GLOBAL				/* conflict */
#include <jpeglib.h>
#include <jerror.h>

extern void jpeg_iostream_dest(j_compress_ptr cinfo, IOSTREAM *outfile);
extern void jpeg_iostream_src(j_decompress_ptr cinfo, IOSTREAM* infile);
extern void void attach_dib_image(Image image, BITMAPINFO *bmi, BYTE *bits);

int
write_jpeg_file(IOSTREAM *fd, Image image, HBITMAP bm)
{ BITMAP bitmap;
  int width, height, depth;
  int y;
  HDC hdc;
  HBITMAP obm;
  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;
  JSAMPLE *row;
  DisplayObj d = image->display;
  HPALETTE ohpal=0, hpal;

  if ( !GetObject(bm, sizeof(BITMAP), &bitmap) )
  { Cprintf("write_jpeg_file(): GetObject() failed\n");
    return -1;
  }

  if ( isNil(d) )
    d = CurrentDisplay(image);
  if ( instanceOfObject(d->colour_map, ClassColourMap) )
    hpal = getPaletteColourMap(d->colour_map);
  else
    hpal = NULL;

  width  = bitmap.bmWidth;
  height = bitmap.bmHeight;
  depth  = bitmap.bmPlanes * bitmap.bmBitsPixel;

  hdc = CreateCompatibleDC(NULL);
  if ( hpal )
  { ohpal = SelectPalette(hdc, hpal, FALSE);
    RealizePalette(hdc);
  }
  obm = ZSelectObject(hdc, bm);

  row = pceMalloc(sizeof(JSAMPLE)*3*width);

  cinfo.err = jpeg_std_error(&jerr);
  jpeg_create_compress(&cinfo);
  jpeg_iostream_dest(&cinfo, fd);

  cinfo.image_width = width;
  cinfo.image_height = height;
  cinfo.input_components = 3;
  cinfo.in_color_space = JCS_RGB;
  jpeg_set_defaults(&cinfo);

  jpeg_start_compress(&cinfo, TRUE);

  for(y=0; y<height; y++)
  { int x;
    JSAMPLE *s = row;

    for(x=0; x<width; x++)
    { COLORREF c = GetPixel(hdc, x, y);

      *s++ = GetRValue(c);
      *s++ = GetGValue(c);
      *s++ = GetBValue(c);
      DEBUG(NAME_jpeg, Cprintf("#%02x%02x%02x", s[-3], s[-2], s[-1]));
    }
    DEBUG(NAME_jpeg, Cprintf("\n"));

    jpeg_write_scanlines(&cinfo, &row, 1);
  }

  pceFree(row);
  jpeg_finish_compress(&cinfo);
  jpeg_destroy_compress(&cinfo);

  ZSelectObject(hdc, obm);
  if ( ohpal )
    SelectPalette(hdc, ohpal, FALSE);
  DeleteDC(hdc);

  return 0;
}

		 /*******************************
		 *	   READING JPEG		*
		 *******************************/

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


status
read_jpeg_file(IOSTREAM *fd, Image image)
{ struct jpeg_decompress_struct cinfo;
  struct my_jpeg_error_mgr jerr;
  long here = Stell(fd);
  int width, height, bwidth, image_size;
  BYTE *data;
  BITMAPINFO *dib = pceMalloc(sizeof(*dib));
  BITMAPINFOHEADER *header = &dib->bmiHeader;

  cinfo.err = jpeg_std_error((struct jpeg_error_mgr *)&jerr);
  if ( setjmp(jerr.jmp_context) )
  { switch(jerr.jerr.msg_code)
    { case JERR_OUT_OF_MEMORY:
	return sysPce("Not enough memory");
      case JERR_NO_SOI:
	fail;				/* invalid */
      default:
      DEBUG(NAME_image,
	    { char buf[1024];

	      (*jerr.jerr.format_message)((j_common_ptr)&cinfo, buf);
	      Cprintf("JPEG: %s\n", buf);
	    });
        fail;				/* also invalid */
    }

    jpeg_destroy_decompress(&cinfo);

    Sseek(fd, here, SEEK_SET);
    fail;
  }
  jerr.jerr.error_exit = my_exit;

  jpeg_create_decompress(&cinfo);
  jpeg_iostream_src(&cinfo, fd);

  jpeg_save_markers(&cinfo, JPEG_COM, 0xffff);
  jpeg_read_header(&cinfo, TRUE);
  jpeg_start_decompress(&cinfo);
  row_stride = cinfo.output_width * cinfo.output_components;
  buffer = (*cinfo.mem->alloc_sarray)((j_common_ptr)&cinfo,
				      JPOOL_IMAGE, row_stride, 1);

  width  = cinfo.image_width;
  height = cinfo.image_height;
  bwidth = ((width*3+3)&0xfffc);
  image_size = bwidth*height;
  
  data = pceMalloc(image_size);
  dest = data + bwidth*(height-1);

  while(cinfo.output_scanline < cinfo.output_height)
  { jpeg_read_scanlines(&cinfo, buff, 1);
    i = width;
    src = (char *)buff[0];

    while(i--)
    { *dest++ = src[2];
      *dest++ = src[1];
      *dest++ = src[0];
      src += 3;
    }

    memset(dest, 0, bwidth - width*3);
    dest -= bwidth+width*3;
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
  jpeg_destroy_decompress(&cinfo);

  memset(dib, 0, sizeof(*dib));
  header->biSize      =	sizeof(BITMAPINFOHEADER) ;
  header->biWidth     =	width;
  header->biHeight    =	height;
  header->biPlanes    =	1;
  header->biBitCount  =	24;
  header->biSizeImage =	image_size;

  attach_dib_image(image, dib, data);
  
  succeed;
}

#endif /*HAVE_LIBJPEG*/



		 /*******************************
		 *	 GIF (SHOULD MOVE)	*
		 *******************************/

#ifdef O_GIFWRITE
#include <img/gifwrite.h>
typedef unsigned char GSAMPLE;

int
write_gif_file(IOSTREAM *fd, Image image, HBITMAP bm)
{ BITMAP bitmap;
  int width, height, depth;
  int y, rval;
  HDC hdc;
  HBITMAP obm;
  GSAMPLE *data, *s;
  DisplayObj d = image->display;
  HPALETTE ohpal=0, hpal;

  if ( !GetObject(bm, sizeof(BITMAP), &bitmap) )
  { Cprintf("write_gif_file(): GetObject() failed\n");
    return -1;
  }

  if ( isNil(d) )
    d = CurrentDisplay(image);
  if ( instanceOfObject(d->colour_map, ClassColourMap) )
    hpal = getPaletteColourMap(d->colour_map);
  else
    hpal = NULL;

  width  = bitmap.bmWidth;
  height = bitmap.bmHeight;
  depth  = bitmap.bmPlanes * bitmap.bmBitsPixel;

  hdc = CreateCompatibleDC(NULL);
  if ( hpal )
  { ohpal = SelectPalette(hdc, hpal, FALSE);
    RealizePalette(hdc);
  }
  obm = ZSelectObject(hdc, bm);

  s = data = pceMalloc(sizeof(GSAMPLE)*3*width*height);

  for(y=0; y<height; y++)
  { int x;

    for(x=0; x<width; x++)
    { COLORREF c = GetPixel(hdc, x, y);

      *s++ = GetRValue(c);
      *s++ = GetGValue(c);
      *s++ = GetBValue(c);
    }
  }

  rval = gifwrite_rgb(fd, data, width, height);
  pceFree(data);

  ZSelectObject(hdc, obm);
  if ( ohpal )
    SelectPalette(hdc, ohpal, FALSE);
  DeleteDC(hdc);

  return rval;
}

#endif /*HAVE_LIBJPEG*/
