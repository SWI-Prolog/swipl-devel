/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

#include "include.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Actually, this module does both medium-level   JPEG and GIF writing. The
low-level routines are in the img   directory. These routines are called
by msimage.c, which in  turn  implements   the  OS  specific  version of
gra/image.c implementing class image.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef HAVE_LIBJPEG
#define boolean jpeg_boolean
#undef GLOBAL				/* conflict */
#include <jpeglib.h>
#include <jerror.h>

extern void jpeg_iostream_dest(j_compress_ptr cinfo, IOSTREAM *outfile);

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

#endif /*HAVE_LIBJPEG*/

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
