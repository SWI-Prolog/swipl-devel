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
{ WsImage r;
  Xref xref;

  if ( (r=image->ws_ref) )
  { if ( r->data )
      free(r->data);
    if ( r->msw_info )
      free(r->msw_info);
    unalloc(sizeof(ws_image), image->ws_ref);
    
    image->ws_ref = NULL;
  }

  while((xref = unregisterXrefObject(image, DEFAULT)))
  { HBITMAP bm = (HBITMAP) xref->xref;
    ZDeleteObject(bm);
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


#ifndef BM				/* SDK tells me this exists, but */
#define BM 0x4d42			/* it doesn't.  Hope it is correct! */
#endif

static int
color_quads_in_bitmap_info(BITMAPINFOHEADER *hdr)
{ if ( hdr->biClrUsed )
    return hdr->biClrUsed;
  else if ( hdr->biBitCount == 24 )
    return 0;				/* direct color */
  else
  { assert(hdr->biBitCount <= 8);
    return 1 << hdr->biBitCount;
  }
}

static BITMAPINFO *
read_bitmap_info(FileObj f)
{ FILE *fd = f->fd;
  BITMAPINFOHEADER bmih;
  int rgbquads;
  BITMAPINFO *bmi;
  
  if ( fread(&bmih, sizeof(bmih), 1, fd) != 1 )
  { errorPce(f, NAME_ioError, OsError());
    return NULL;
  }
  rgbquads = color_quads_in_bitmap_info(&bmih);
  DEBUG(NAME_image, Cprintf("%dx%d; %d rgbquads\n",
			    bmih.biWidth, bmih.biHeight, rgbquads));
  bmi = malloc(sizeof(bmih) + sizeof(RGBQUAD)*rgbquads);
  memcpy(&bmi->bmiHeader, &bmih, sizeof(bmih));
  if ( fread(&bmi->bmiColors, sizeof(RGBQUAD), rgbquads, fd) != rgbquads )
  { errorPce(f, NAME_ioError, OsError());
    return NULL;
  }

  return bmi;
}


static void
attach_dbi_image(Image image, BITMAPINFO *bmi, BYTE *bits)
{ WsImage wsi;
  BITMAPINFOHEADER *bmih = &bmi->bmiHeader;

  wsi           = alloc(sizeof(ws_image));
  wsi->data     = bits;
  wsi->msw_info = bmi;
  image->ws_ref = wsi;

  assign(image->size, w, toInt(bmih->biWidth));
  assign(image->size, h, toInt(bmih->biHeight));
  assign(image, depth, toInt(bmih->biBitCount));
  assign(image, kind, image->depth == ONE ? NAME_bitmap : NAME_pixmap);
}


static status
ws_load_windows_bmp_file(Image image, FileObj f)
{ BITMAPFILEHEADER bmfh;
  BITMAPINFO *bmi;
  BITMAPINFOHEADER *bmih;
  BYTE *aBitmapBits;
  int databytes;
  FILE *fd = f->fd;
  long pos = ftell(fd);

  if ( fread(&bmfh, sizeof(bmfh), 1, fd) != 1 ||
       bmfh.bfType != BM ||
       !(bmi=read_bitmap_info(f)) )
  { fseek(fd, pos, SEEK_SET);
    fail;				/* not a MS-Windows .bmp file */
  }
  databytes = bmfh.bfSize - bmfh.bfOffBits;
  bmih = &bmi->bmiHeader;
  DEBUG(NAME_image,
	Cprintf("%dx%dx%d image; %d data bytes\n",
		bmih->biWidth, bmih->biHeight, bmih->biBitCount, databytes));
  aBitmapBits = malloc(databytes);
  if ( fread(aBitmapBits, sizeof(BYTE), databytes, fd) != databytes )
  { free(bmi);
    free(aBitmapBits);

    return errorPce(f, NAME_ioError, getOsErrorPce(PCE));
  }
  
  attach_dbi_image(image, bmi, aBitmapBits);
  succeed;
}


#define OsError() getOsErrorPce(PCE)

typedef struct tagICONDIRENTRY
{ BYTE	bWidth;
  BYTE	bHeight;
  BYTE	bColorCount;
  BYTE	bReserved;
  WORD	wPlanes;
  WORD	wBitCount;
  DWORD	dwBytesInRes;
  DWORD	dwImageOffset;
} ICONDIRENTRY;

typedef struct ICONDIR
{ WORD	idReserved;
  WORD  idType;
  WORD	idCount;
  ICONDIRENTRY idEntries[1];
} ICONHEADER;
    
static status
ws_load_windows_ico_file(Image image)
{ FILE *fd = image->file->fd;
  ICONHEADER ico_hdr;
  ICONDIRENTRY ico_entry;
  BITMAPINFO *bmi;
  BYTE *bits;
  long pos = ftell(fd);
  int databytes;

  if ( fread(&ico_hdr, sizeof(ico_hdr) - sizeof(ICONDIRENTRY), 1, fd) != 1 ||
       ico_hdr.idType != 1 )
  { fseek(fd, pos, SEEK_SET);
    fail;				/* not a MS-Windows .bmp file */
  }
  DEBUG(NAME_image, Cprintf("idType = %d, idCount = %d\n",
			    ico_hdr.idType, ico_hdr.idCount));
  if ( ico_hdr.idCount > 1 )
    errorPce(image->file, NAME_moreThanOneIcon);

#define BadDimension(x) (x!=16 && x!=32 && x!=64)
#define BadColorCount(x) (x!=2 && x!=8 && x!=16)
  if ( fread(&ico_entry, sizeof(ico_entry), 1, fd) != 1 ||
       BadDimension(ico_entry.bWidth) ||
       BadDimension(ico_entry.bHeight) ||
       BadColorCount(ico_entry.bColorCount) )
  { fseek(fd, pos, SEEK_SET);
    fail;				/* not a MS-Windows .bmp file */
  }
#undef BadDimension
#undef BadColorCount

  DEBUG(NAME_image,
	Cprintf("%dx%d icon with %d colors\n",
		ico_entry.bWidth, ico_entry.bHeight, ico_entry.bColorCount));
  DEBUG(NAME_image,
	Cprintf("dwBytesInRes = %d, dwImageOffset = %d\n",
		ico_entry.dwBytesInRes, ico_entry.dwImageOffset));

  if ( fseek(fd, ico_entry.dwImageOffset, SEEK_SET) ||
       !(bmi=read_bitmap_info(image->file)) )
    return errorPce(image->file, NAME_ioError, OsError());
  bmi->bmiHeader.biWidth  = ico_entry.bWidth; /* MS-Windows bug! */
  bmi->bmiHeader.biHeight = ico_entry.bHeight;
  databytes = ico_entry.dwBytesInRes - ftell(fd);
  bits = malloc(databytes);
  if ( fread(bits, sizeof(BYTE), databytes, fd) != databytes )
  { free(bmi);
    return errorPce(image->file, NAME_ioError, OsError());
  }

  attach_dbi_image(image, bmi, bits);
  succeed;
}


status
ws_load_image_file(Image image)
{ status rval = FAIL;

  assign(image->file, kind, NAME_binary);

  if ( send(image->file, NAME_open, NAME_read, 0) )
  { int w, h;
    unsigned char *data;
    
    DEBUG(NAME_image, Cprintf("Trying to read bitmap from %s\n",
			      pp(image->file->path)));
    if ( (data = read_bitmap_data(image->file->fd, &w, &h)) )
    { ws_create_image_from_x11_data(image, data, w, h);
      free(data);
      rval = SUCCEED;
    } else if ( ws_load_windows_bmp_file(image, image->file) )
    { rval = SUCCEED;
    } else
      rval = ws_load_windows_ico_file(image);

    send(image->file, NAME_close, 0);
  }

  return rval;
}


status
ws_save_image_file(Image image, FileObj file)
{ fail;
}


static HBITMAP
windows_bitmap_from_dbi(Image image)
{ WsImage wsi;

  if ( (wsi=image->ws_ref) && wsi->msw_info )
  { HDC hdc;
    HBITMAP bm;

    hdc = GetDC(NULL);
    bm = CreateDIBitmap(hdc,
			(LPBITMAPINFOHEADER) wsi->msw_info,
			CBM_INIT,
			wsi->data,
			(LPBITMAPINFO) wsi->msw_info,
			DIB_RGB_COLORS);
    assign(image, depth, toInt(GetDeviceCaps(hdc, BITSPIXEL)));
    assign(image, kind, image->depth == 1 ? NAME_bitmap : NAME_pixmap);
    ReleaseDC(NULL, hdc);
    return bm;
  }

  return NULL;
}


static HBITMAP
windows_bitmap_from_bits(Image image)
{ WsImage r;

  if ( (r=image->ws_ref) )
  { HBITMAP bm;

    if ( r->msw_info )
    { bm = windows_bitmap_from_dbi(image);
    } else
    { bm = CreateBitmap(r->w, r->h, 1, 1, NULL);
      SetBitmapBits(bm, ws_sizeof_bits(r->w, r->h), r->data);
    }

    return bm;
  }

  return 0;
}


status
ws_open_image(Image image, DisplayObj d)
{ HBITMAP bm;
  int w = valInt(image->size->w);
  int h = valInt(image->size->h);

  assign(image, display, d);

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
    succeed;
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
    ZDeleteObject((HBITMAP) r->xref);
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
	HBITMAP osbm = ZSelectObject(hdcsrc, sbm);
	HBITMAP  dbm = CreateCompatibleBitmap(hdcsrc, valInt(w), valInt(h));
	HBITMAP odbm = ZSelectObject(hdcdst, dbm);
	int minw = min(valInt(w), valInt(image->size->w));
	int minh = min(valInt(h), valInt(image->size->h));

	BitBlt(hdcdst, 0, 0, minw, minh, hdcsrc, 0, 0, SRCCOPY);
      
	ZSelectObject(hdcsrc, osbm);
	ZSelectObject(hdcdst, odbm);
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

  Cprintf("Bits at %p: ", addr);
  for(i=0; i<32; i++)
  { Cputchar(bits & 0x80000000 ? '1' : '0');
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
{ WsImage r;
  unsigned short *dest;
  int y;
  int byte = 0;

  r = image->ws_ref = alloc(sizeof(ws_image));
  r->msw_info = NULL;			/* X11 data */
  r->w = w;
  r->h = h;
  dest = r->data = malloc(ws_sizeof_bits(w, h));

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
{ WsImage r;
  unsigned short *c, *cbits = malloc(ws_sizeof_bits(w, h));
  unsigned short *d, *dbits;
  int alloced;
  int dw, dh;
  int x, y;
  int saidpad=0;

  if ( (r = image->ws_ref) && !r->msw_info )
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
   
    DEBUG(NAME_cursor, Cprintf("Alloced %d bytes at 0x%lx\n",
			       alloced, (long) dbits));

    if ( bytes != GetBitmapBits(bm, bytes, dbits) )
      Cprintf("GetBitmapBits() failed\n");

    DEBUG(NAME_cursor, Cprintf("Got %d bytes image from %s\n",
			       bytes, pp(image)));
  }
		   
  for(y=0; y<h; y++)
  { c = cbits + y*((w+15)/16);
    d = dbits + y*((dw+15)/16);
    x = 0;

    DEBUG(NAME_cursor, Cprintf("Copy line %d from %p to %p\n", y, d, c));

    if ( y < dh )
    { for(; x < w && x < dw; x += 16)
	*c++ = ~(*d++);
      if ( x-dw > 0 )			/* need partial padding */
      { unsigned short mask = 0xffff0000L >> (16-(x-dw));
	unsigned short m2 = ((mask >> 8) & 0x00ff) | ((mask << 8) & 0xff00);

	DEBUG(NAME_cursor,
	      if ( saidpad++ == 0 )
	        Cprintf("mask = 0x%04x; ms = 0x%04x\n", mask, m2));

	c[-1] &= m2;
      }
    }
    for(; x < w; x += 16)
      *c++ = 0x0000;
  }

  if ( alloced )
    unalloc(alloced, dbits);

  DEBUG(NAME_cursor, Cprintf("Returning %dx%d bits\n", w, h));
  return cbits;
}


