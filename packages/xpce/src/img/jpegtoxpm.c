/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

#ifdef WIN32
#include <msw/include.h>
#ifdef HAVE_LIBXPM
#define  FOR_MSW 1
#include <msw/xpm.h>
#endif
#define boolean jpeg_boolean
#else /*WIN32*/
#include <h/kernel.h>
#ifdef HAVE_LIBXPM
#include <X11/xpm.h>
#endif
#endif /*WIN32*/

#undef GLOBAL				/* conflict */

#ifdef HAVE_LIBJPEG

#include <jpeglib.h>
#include <jerror.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <setjmp.h>

/* left to free(), so use malloc() rather then pceMalloc() */
#define XpmMalloc(n) (void *)malloc(n)
typedef unsigned int XpmPixel;

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

extern void	jpeg_iostream_src(j_decompress_ptr cinfo, IOSTREAM* infile);

static int
convert_colourmap(int ncolors, int ncomponents,
		  JSAMPARRAY colourmap,
		  XpmImage *img)
{ int i;

  img->ncolors = ncolors;
  if ( !(img->colorTable = XpmMalloc(sizeof(XpmColor) * ncolors)) )
    return XpmNoMemory;

  memset(img->colorTable, 0, sizeof(XpmColor) * ncolors);

  for(i=0; i<ncolors; i++)
  { XpmColor *c = &img->colorTable[i];

    if ( (c->c_color = XpmMalloc(8)) )
    { int r, g, b;

      switch(ncomponents)
      { case 3:
	  r = colourmap[0][i];
	  g = colourmap[1][i];
	  b = colourmap[2][i];
	  break;
	case 1:
	  r = colourmap[0][i];
	  g = b = r;
	  break;
	default:
	  r = g = b = 0;		/* keep compiler happy */
	  sysPce("JPEG: Unknown number of colour components: %d\n",
		 ncomponents);
      }
      
      sprintf(c->c_color, "#%02x%02x%02x", r, g, b);
    } else
      return XpmNoMemory;
  }

  return XpmSuccess;
}


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


int
readJPEGtoXpmImage(IOSTREAM *fd, XpmImage *img)
{ struct jpeg_decompress_struct cinfo;
  struct my_jpeg_error_mgr jerr;
  long row_stride;
  JSAMPLE **buffer;
  int rval;
  long here = Stell(fd);

  if ( !img )
    return XpmNoMemory;
  img->ncolors    = 0;
  img->colorTable = NULL;
  img->data       = NULL;

  cinfo.err = jpeg_std_error((struct jpeg_error_mgr *)&jerr);
  if ( setjmp(jerr.jmp_context) )
  { switch(jerr.jerr.msg_code)
    { case JERR_OUT_OF_MEMORY:
	rval = XpmNoMemory;
	break;
      case JERR_NO_SOI:
	rval = XpmFileInvalid;
	break;
      default:
      DEBUG(NAME_image,
	    { char buf[1024];

	      (*jerr.jerr.format_message)((j_common_ptr)&cinfo, buf);
	      Cprintf("JPEG: %s\n", buf);
	    });
      rval = XpmFileInvalid;
    }

    jpeg_destroy_decompress(&cinfo);

    Sseek(fd, here, SEEK_SET);
    return rval;
  }
  jerr.jerr.error_exit = my_exit;

  jpeg_create_decompress(&cinfo);
  jpeg_iostream_src(&cinfo, fd);

  jpeg_read_header(&cinfo, TRUE);
  cinfo.quantize_colors = TRUE;
  jpeg_start_decompress(&cinfo);
  
  if ( (rval=convert_colourmap(cinfo.actual_number_of_colors,
			       cinfo.out_color_components,
			       cinfo.colormap,
			       img) != XpmSuccess) )
    return rval;

  row_stride = cinfo.output_width * cinfo.output_components;
  buffer = (*cinfo.mem->alloc_sarray)((j_common_ptr)&cinfo,
				      JPOOL_IMAGE, row_stride, 1);
  img->width  = cinfo.output_width;
  img->height = cinfo.output_height;
  img->data   = XpmMalloc(sizeof(XpmPixel) *
			  cinfo.output_width *
			  cinfo.output_height);
  if ( !img->data )
    return XpmNoMemory;

  while ( cinfo.output_scanline < cinfo.output_height )
  { XpmPixel *o;
    JSAMPLE *i;
    int x;

    jpeg_read_scanlines(&cinfo, buffer, 1);

    o = &img->data[cinfo.output_width*(cinfo.output_scanline - 1)];
    i = buffer[0];
    x = cinfo.output_width;

    while(--x >= 0)
      *o++ = *i++;
  }

  jpeg_finish_decompress(&cinfo);
  jpeg_destroy_decompress(&cinfo);

  return XpmSuccess;
}

#endif /*HAVE_LIBJPEG*/
