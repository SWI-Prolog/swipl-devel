/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

#include "include.h"

#ifdef HAVE_LIBJPEG
#undef GLOBAL				/* conflict */
#include <jpeglib.h>
#include <jerror.h>

extern void jpeg_iostream_dest(j_compress_ptr cinfo, IOSTREAM *outfile);

int
write_jpeg_file(IOSTREAM *fd, HBITMAP bm)
{ BITMAP bitmap;
  int width, height, depth;
  int y;
  HDC hdc;
  HBITMAP obm;
  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;
  JSAMPLE *row;

  if ( !GetObject(bm, sizeof(BITMAP), &bitmap) )
  { Cprintf("write_jpeg_file(): GetObject() failed\n");
    return -1;
  }

  width  = bitmap.bmWidth;
  height = bitmap.bmHeight;
  depth  = bitmap.bmPlanes * bitmap.bmBitsPixel;

  hdc = CreateCompatibleDC(NULL);
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
    }

    jpeg_write_scanlines(&cinfo, &row, 1);
  }

  jpeg_finish_compress(&cinfo);
  jpeg_destroy_compress(&cinfo);

  ZSelectObject(hdc, obm);
  DeleteDC(hdc);

  return 0;
}

#endif /*HAVE_LIBJPEG*/
