/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include "include.h"
#include <h/unix.h>

/* Using ws_ref for storing the bits and the xref mechanism for storing
   the Windows HBITMAP handle
*/


static int ws_sizeof_bits(int w, int h);


void
ws_init_image(Image image)
{ image->ws_ref = NULL;
}


void
ws_destroy_image(Image image)
{ WsBits r;

  if ( (r=image->ws_ref) )
  { unalloc(ws_sizeof_bits(r->w, r->h), r->data);
    unalloc(sizeof(ws_bits), image->ws_ref);
    
    image->ws_ref = NULL;
  }
}


status
ws_store_image(Image image, FileObj file)
{ fail;
}


status
loadXImage(Image image, FILE *fd)
{ fail;
}


status
ws_load_old_image(Image image, FILE *fd)
{ fail;
}


status
ws_load_image_file(Image image)
{ status rval = FAIL;

  if ( send(image->file, NAME_open, NAME_read, 0) )
  { int w, h;
    unsigned char *data;
    
    DEBUG(NAME_image, printf("Trying to read %s", pp(image->file->path)));
    if ( (data = read_bitmap_data(image->file->fd, &w, &h)) )
    { ws_create_image_from_x11_data(image, data, w, h);
      free(data);
      rval = SUCCEED;
    }

    send(image->file, NAME_close, 0);
  }

  return rval;
}


status
ws_save_image_file(Image image, FileObj file)
{ fail;
}


static HBITMAP
windows_bitmap_from_bits(Image image)
{ WsBits r;

  if ( (r=image->ws_ref) )
  { HBITMAP bm = CreateBitmap(r->w, r->h, 1, 1, NULL);
    SetBitmapBits(bm, ws_sizeof_bits(r->w, r->h), r->data);

    return bm;
  }

  return 0;
}


status
ws_open_image(Image image, DisplayObj d)
{ HBITMAP bm;
  int w = valInt(image->size->w);
  int h = valInt(image->size->h);

  if ( image->ws_ref )
  { bm = windows_bitmap_from_bits(image);
    if ( bm ) 
    { registerXrefObject(image, d, (void *) bm);

      succeed;
    }

    fail;
  }

  if ( notNil(image->file) )
  { TRY(loadImage(image, DEFAULT, DEFAULT));
    return XopenImage(image, d);
  }

  if ( w != 0 && h != 0 && image->access == NAME_both )
  { assign(image, display, d);

    if ( image->kind == NAME_pixmap )
    { HDC hdc = GetDC(NULL);
      BITMAP bitmap;

      if ( isDefault(image->background) )
	assign(image, background, d->background);
      if ( isDefault(image->foreground) )
	assign(image, foreground, d->foreground);
      
      bm = CreateCompatibleBitmap(hdc, w, h);
      GetObject(bm, sizeof(BITMAP), &bitmap);
      assign(image, depth, toInt(bitmap.bmPlanes * bitmap.bmBitsPixel));
      ReleaseDC(NULL, hdc);
    } else
    { assign(image, depth, ONE);
      bm = CreateBitmap(w, h, 1, 1, NULL);
    }

    if ( bm )
    { registerXrefObject(image, d, (void *) bm);
      d_image(image, 0, 0, w, h);
      r_clear(0, 0, w, h);
      d_done();

      succeed;
    }
  }
  
  fail;
}


void
ws_close_image(Image image, DisplayObj d)
{ Xref r;

  while( (r = unregisterXrefObject(image, d)) )
    DeleteObject((HBITMAP) r->xref);
}


status
ws_resize_image(Image image, Int w, Int h)
{ if ( notNil(image->display) )
  { DisplayObj d = image->display;
    HBITMAP sbm = (HBITMAP) getExistingXrefObject(image, d);

    if ( sbm )
    { if ( w == ZERO || h == ZERO )
      { XcloseImage(image, d);
      } else
      { HDC hdcsrc = CreateCompatibleDC(NULL);
	HDC hdcdst = CreateCompatibleDC(hdcsrc);
	HBITMAP osbm = SelectObject(hdcsrc, sbm);
	HBITMAP  dbm = CreateCompatibleBitmap(hdcsrc, valInt(w), valInt(h));
	HBITMAP odbm = SelectObject(hdcdst, dbm);
	int minw = min(valInt(w), valInt(image->size->w));
	int minh = min(valInt(h), valInt(image->size->h));

	BitBlt(hdcdst, 0, 0, minw, minh, hdcsrc, 0, 0, SRCCOPY);
      
	SelectObject(hdcsrc, osbm);
	SelectObject(hdcdst, odbm);
	DeleteDC(hdcsrc);
	DeleteDC(hdcdst);

	XcloseImage(image, d);
	registerXrefObject(image, d, (void *) dbm);
      }
    }
  }

  return setSize(image->size, w, h);
}


void
ws_postscript_image(Image image)
{ int w = valInt(image->size->w);
  int h = valInt(image->size->h);

  d_image(image, 0, 0, w, h);
  postscriptDrawable(0, 0, w, h);
  d_done();
}


static int
ws_sizeof_bits(int w, int h)
{ int bytes = ((w+15)/16) * 2 * h;

  return ((bytes + 3)/4) * 4;		/* round on longs */
}


static void
print_bits(unsigned long *addr)
{ unsigned long bits = *addr;
  int i;

  printf("Bits at %p: ", addr);
  for(i=0; i<32; i++)
  { putchar(bits & 0x80000000 ? '1' : '0');
    bits <<= 1;
  }
}


static unsigned int
mirror_byte(unsigned int b)
{ unsigned int copy = 0;
  int n;

  for(n=0; n<8; n++, b >>= 1)
  { copy <<= 1;
    if ( b & 0x01L )
      copy |= 0x01L;
  }
    
  return copy;
}


void
ws_create_image_from_x11_data(Image image, unsigned char *data, int w, int h)
{ WsBits r;
  unsigned short *dest;
  int y;
  int byte = 0;

  r = image->ws_ref = alloc(sizeof(ws_bits));
  r->w = w;
  r->h = h;
  dest = r->data = alloc(ws_sizeof_bits(w, h));

  for(y=0; y<h; y++)
  { int x;
    unsigned short s;

    for(x=0; x<w; x+=8)
    { unsigned char b = ~mirror_byte(*data++);

      if ( ++byte == 1 )
      { s = b;
      } else
      { s |= (unsigned short) b << 8;
	*dest++ = s;
	byte = 0;
      }
    }

    if ( byte )
    { *dest++ = s;
      byte = 0;
    }
  }

  assign(image, depth, ONE);
  setSize(image->size, toInt(w), toInt(h));
}


void *
ws_image_bits_for_cursor(Image image, Name kind, int w, int h)
{ WsBits r;
  unsigned short *c, *cbits = malloc(ws_sizeof_bits(w, h));
  unsigned short *d, *dbits;
  int alloced;
  int dw, dh;
  int x, y;

  if ( (r = image->ws_ref) )
  { dbits = r->data;
    alloced = 0;
    dw = r->w;
    dh = r->h;
  } else
  { HBITMAP bm = (HBITMAP) getXrefObject(image, image->display);
    int bytes;

    dw = valInt(image->size->w);
    dh = valInt(image->size->h);
    bytes = ws_sizeof_bits(dw, dh);
    dbits = alloc(bytes);
    alloced = bytes;
   
    DEBUG(NAME_cursor, printf("Alloced %d bytes at 0x%lx\n",
			       alloced, (long) dbits));

    if ( bytes != GetBitmapBits(bm, bytes, dbits) )
      printf("GetBitmapBits() failed\n");

    DEBUG(NAME_cursor, printf("Got %d bytes image from %s\n",
			      bytes, pp(image)));
  }
		   
  for(y=0; y<h; y++)
  { c = cbits + y*((w+15)/16);
    d = dbits + y*((dw+15)/16);
    x = 0;

    DEBUG(NAME_cursor, printf("Copy line %d from %p to %p\n", y, d, c));

    if ( y < dh )
    { for(; x < w && x < dw; x += 16)
	*c++ = ~(*d++);
      if ( x-dw > 0 )			/* need partial padding */
      { unsigned short mask = (0xffff0000L >> (16-(x-dw)));

	c[-1] &= mask;
      }
    }
    for(; x < w; x += 16)
      *c++ = 0x0000;
  }

  if ( alloced )
    unalloc(alloced, dbits);

  DEBUG(NAME_cursor, printf("Returning %dx%d bits\n", w, h));
  return cbits;
}


