/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#define OEMRESOURCE 1			/* get OBM_* constants */
#include "include.h"
#include <h/unix.h>
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#define OsError() getOsErrorPce(PCE)

#ifdef O_IMGLIB
#include "imglib.h"

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#else
#ifndef MAXPATHLEN
#define MAXPATHLEN 256
#endif
#endif
#endif /*O_IMGLIB*/

#ifdef O_GIFREAD
extern LPVOID CALLBACK gifLoad(char *);
#endif /*O_GIFREAD*/

#ifdef O_XPM
#define FOR_MSW 1
#include <msw/xpm.h>
#endif

/* Using ws_ref for storing the bits and the xref mechanism for storing
   the Windows HBITMAP handle
*/


static int	ws_sizeof_bits(int w, int h);


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
      pceFree(r->data);
    if ( r->msw_info )
      pceFree(r->msw_info);
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
{ HBITMAP bm;
  DisplayObj d = image->display;

  if ( isNil(d) )
    d = CurrentDisplay(image);

  if ( (bm = getXrefObject(image, d)) )
  { putc('P', file->fd);
    DEBUG(NAME_ppm, Cprintf("Saving PNM image from index %d\n",
			    ftell(file->fd)));
    if ( write_pnm_file(file->fd, bm, 0, 0, PNM_RUNLEN) < 0 )
      fail;
  }

  succeed;
}


status
loadXImage(Image image, FILE *fd)
{ fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
loadPNMImage() is used for loading saved-objects holding images.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
loadPNMImage(Image image, FILE *fd)
{ Name kind;
  HBITMAP bm = read_ppm_file(fd, &kind);

  if ( isNil(image->display) )
    assign(image, display, CurrentDisplay(NIL));

  if ( bm )
  { BITMAP bitmap;

    if ( !GetObject(bm, sizeof(BITMAP), &bitmap) )
      Cprintf("loadPPMImage(): GetObject() failed\n");
    assign(image->size, w, toInt(bitmap.bmWidth));
    assign(image->size, h, toInt(bitmap.bmHeight));
    assign(image, kind, kind);
    assign(image, depth, toInt(bitmap.bmPlanes * bitmap.bmBitsPixel));
    registerXrefObject(image, image->display, (void *) bm);

    succeed;
  }

  fail;
}


status
loadPPMImage(Image image, FileObj f)
{ return loadPNMImage(image, f->fd);
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
  bmi = pceMalloc(sizeof(bmih) + sizeof(RGBQUAD)*rgbquads);
  memcpy(&bmi->bmiHeader, &bmih, sizeof(bmih));
  if ( fread(&bmi->bmiColors, sizeof(RGBQUAD), rgbquads, fd) != rgbquads )
  { errorPce(f, NAME_ioError, OsError());
    return NULL;
  }

  return bmi;
}


static void
attach_dib_image(Image image, BITMAPINFO *bmi, BYTE *bits)
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


#ifndef O_IMGLIB

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
  aBitmapBits = pceMalloc(databytes);
  if ( fread(aBitmapBits, sizeof(BYTE), databytes, fd) != databytes )
  { pceFree(bmi);
    pceFree(aBitmapBits);

    return errorPce(f, NAME_ioError, getOsErrorPce(PCE));
  }
  
  attach_dib_image(image, bmi, aBitmapBits);
  succeed;
}

#endif /*O_IMGLIB*/

#define OsError() getOsErrorPce(PCE)
#define SWORD unsigned short
#define ICOFHDRSIZE 6			/* Designed for 16-bit rounding */

typedef struct tagICONDIRENTRY
{ BYTE	bWidth;
  BYTE	bHeight;
  BYTE	bColorCount;
  BYTE	bReserved;
  SWORD	wPlanes;
  SWORD	wBitCount;
  DWORD	dwBytesInRes;
  DWORD	dwImageOffset;
} ICONDIRENTRY;

typedef struct ICONDIR
{ SWORD	idReserved;
  SWORD idType;
  SWORD	idCount;
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

  if ( fread(&ico_hdr, ICOFHDRSIZE, 1, fd) != 1 ||
       ico_hdr.idType != 1 )
  { fseek(fd, pos, SEEK_SET);
    fail;				/* not a MS-Windows .ico file */
  }
  DEBUG(NAME_image, Cprintf("Header to %d: idType = %d, idCount = %d\n",
			    ftell(fd), ico_hdr.idType, ico_hdr.idCount));
  if ( ico_hdr.idCount > 1 )
    errorPce(image->file, NAME_moreThanOneIcon);

#define BadDimension(x) (x!=16 && x!=32 && x!=64)
#define BadColorCount(x) (x!=2 && x!=8 && x!=16)
  if ( fread(&ico_entry, sizeof(ico_entry), 1, fd) != 1 ||
       BadDimension(ico_entry.bWidth) ||
       BadDimension(ico_entry.bHeight) ||
       BadColorCount(ico_entry.bColorCount) )
  { fseek(fd, pos, SEEK_SET);
    fail;				/* not a MS-Windows .ico file */
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
  bits = pceMalloc(databytes);
  if ( fread(bits, sizeof(BYTE), databytes, fd) != databytes )
  { pceFree(bmi);
    return errorPce(image->file, NAME_ioError, OsError());
  }

  attach_dib_image(image, bmi, bits);
  succeed;
}


status
ws_load_image_file(Image image)
{ status rval = FAIL;

  assign(image->file, kind, NAME_binary);

#ifdef O_XPM
{ XImage *img, *shape;
  HDC hdc  = CreateCompatibleDC(NULL);
  int rval;
  DisplayObj d;

  DEBUG(NAME_xpm, Cprintf("Reading XPM file using DC = 0x%x\n", hdc));
  rval = XpmReadFileToImage(&hdc, strName(getOsNameFile(image->file)),
			    &img, &shape, NULL);
  DeleteDC(hdc);
  switch(rval)
  { case XpmOpenFailed:
      return errorPce(image->file, NAME_openFile,
		      NAME_read, getOsErrorPce(PCE));
    case XpmFileInvalid:
      goto noxpm;
    case XpmNoMemory:
      return sysPce("Not enough memory");
    case XpmSuccess:
      break;
    default:
      return errorPce(image, NAME_unknownError, toInt(rval));
  }

  d = image->display;
  if ( isNil(d) )
  { d = CurrentDisplay(image);
    assign(image, display, d);
  }

  assign(image, kind, img->depth == 1 ? NAME_bitmap : NAME_pixmap);
  assign(image->size, w, toInt(img->width));
  assign(image->size, h, toInt(img->height));
  assign(image, depth, toInt(img->depth));
  registerXrefObject(image, d, img->bitmap);
  XImageFree(img);

  if ( shape )
  { assign(image, mask, newObject(ClassImage, NIL,
				  toInt(shape->width),
				  toInt(shape->height),
				  NAME_bitmap, 0));
    registerXrefObject(image->mask, d, shape->bitmap);
    XImageFree(shape);
  }

  DEBUG(NAME_xpm, Cprintf("%s: loaded XPM file%s\n",
			  pp(image), shape ? " with shape" : ""));

  succeed;

  noxpm:
    ;
}
#endif /*O_XPM*/

#ifdef O_IMGLIB
{ char fname[MAXPATHLEN];
  Name fn = getOsNameFile(image->file);
  BITMAPINFO *bmi;

  _xos_os_filename(strName(fn), fname);

  if ( (bmi = ReadFileIntoDIB(fname))
#ifdef O_GIFREAD
       || (bmi = gifLoad(fname))
#endif
     )
  { HDC      hdc       = GetDC(NULL);
    int     iPixelBits = GetDeviceCaps (hdc, BITSPIXEL);
    int     iPlanes    = GetDeviceCaps (hdc, PLANES);
    int     lColors    = 1 << iPixelBits * iPlanes;
    BITMAPINFO *displdib;

    displdib = ReduceDIB (bmi, lColors, TRUE);
    if ( displdib && displdib != bmi )
    { DIBFree(bmi);
      bmi = displdib;
    }

    ReleaseDC(NULL, hdc);

    { RGBQUAD *colors  = (RGBQUAD *)((char *)bmi + bmi->bmiHeader.biSize);
      BYTE    *data    = (BYTE *)&colors[bmi->bmiHeader.biClrUsed];
    
      attach_dib_image(image, bmi, data);
    }

    rval = SUCCEED;
  } else				/* other formats */
  { if ( send(image->file, NAME_open, NAME_read, 0) )
    { if ( ws_load_windows_ico_file(image) )
	rval = SUCCEED;

      send(image->file, NAME_close, 0);
    }
  }
}
#else /*O_IMGLIB*/
  if ( send(image->file, NAME_open, NAME_read, 0) )
  { int w, h;
    unsigned char *data;
    
    DEBUG(NAME_image, Cprintf("Trying to read bitmap from %s\n",
			      pp(image->file->path)));
    if ( (data = read_bitmap_data(image->file->fd, &w, &h)) )
    { ws_create_image_from_x11_data(image, data, w, h);
      pceFree(data);
      rval = SUCCEED;
    } else if ( ws_load_windows_bmp_file(image, image->file) )
    { rval = SUCCEED;
    } else if ( ws_load_windows_ico_file(image) )
    { rval = SUCCEED;
    } else
      rval = loadPPMImage(image, image->file);

    send(image->file, NAME_close, 0);
  }
#endif /*O_IMGLIB*/

  return rval;
}


status
ws_save_image_file(Image image, FileObj file, Name fmt)
{ DisplayObj d = image->display;

  if ( isNil(d) )
    d = CurrentDisplay(image);

  if ( fmt == NAME_xbm )
  { Cprintf("No support for writing XBM files, try format PNM\n");
    fail;
  } else
  { int pnm_fmt;
    HBITMAP bm;
    status rval;

    if ( fmt == NAME_pnm )	pnm_fmt = PNM_PNM;
    else if ( fmt == NAME_pbm )	pnm_fmt = PNM_PBM;
    else if ( fmt == NAME_pgm )	pnm_fmt = PNM_PGM;
    else if ( fmt == NAME_ppm )	pnm_fmt = PNM_PPM;
    else fail;
    
    if ( (bm = getXrefObject(image, d)) )
    { send(file, NAME_kind, NAME_binary, 0);
      TRY(send(file, NAME_open, NAME_write, 0));
      if ( write_pnm_file(file->fd, bm, 0, 0, PNM_RAWBITS) < 0 )
	rval = errorPce(image, NAME_xError);
      else
	rval = SUCCEED;
      send(file, NAME_close, 0);
    } else
      rval = FAIL;

    return rval;
  }
}


static HBITMAP
windows_bitmap_from_dib(Image image)
{ WsImage wsi;

  if ( (wsi=image->ws_ref) && wsi->msw_info )
  { HDC hdc;
    HBITMAP bm;

    if ( image->kind == NAME_bitmap )
    { int w = valInt(image->size->w);
      int h = valInt(image->size->h);
      HBITMAP obm;

      bm = ZCreateBitmap(w, h, 1, 1, NULL);
      hdc = CreateCompatibleDC(NULL);
      obm = ZSelectObject(hdc, bm);
      if ( StretchDIBits(hdc, 0, 0, w, h, 0, 0, w, h,
			 wsi->data, wsi->msw_info,
			 DIB_RGB_COLORS, SRCCOPY) == GDI_ERROR )
	Cprintf("StretchDIBits() failed");
      ZSelectObject(hdc, obm);
      DeleteDC(hdc);
    } else
    { hdc = GetDC(NULL);
      bm = ZCreateDIBitmap(hdc,
			   (LPBITMAPINFOHEADER) wsi->msw_info,
			   CBM_INIT,
			   wsi->data,
			   (LPBITMAPINFO) wsi->msw_info,
			   DIB_RGB_COLORS);
      assign(image, depth, toInt(GetDeviceCaps(hdc, BITSPIXEL)));
      assign(image, kind, image->depth == ONE ? NAME_bitmap : NAME_pixmap);
      ReleaseDC(NULL, hdc);
    }

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
    { bm = windows_bitmap_from_dib(image);
    } else
    { bm = ZCreateBitmap(r->w, r->h, 1, 1, NULL);
      SetBitmapBits(bm, ws_sizeof_bits(r->w, r->h), r->data);
    }

    return bm;
  }

  return 0;
}


status
ws_open_image(Image image, DisplayObj d)
{ HBITMAP bm;
  HBRUSH brush = 0;
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
  { if ( loadImage(image, DEFAULT, DEFAULT) &&
	 image->ws_ref &&
	 (bm = windows_bitmap_from_bits(image)) )
    { registerXrefObject(image, d, (void *) bm);
      succeed;
    }

    fail;
  }

  if ( image->access == NAME_read )
    brush = standardWindowsBrush(image);

  if ( w != 0 && h != 0 &&
       (image->access == NAME_both || brush) )
  { assign(image, display, d);

    if ( image->kind == NAME_pixmap )
    { HDC hdc = GetDC(NULL);
      BITMAP bitmap;

      if ( isDefault(image->background) )
	assign(image, background, d->background);
      if ( isDefault(image->foreground) )
	assign(image, foreground, d->foreground);
      
      bm = ZCreateCompatibleBitmap(hdc, w, h);
      GetObject(bm, sizeof(BITMAP), &bitmap);
      assign(image, depth, toInt(bitmap.bmPlanes * bitmap.bmBitsPixel));
      ReleaseDC(NULL, hdc);
    } else
    { assign(image, depth, ONE);
      bm = ZCreateBitmap(w, h, 1, 1, NULL);
    }

    if ( bm )
    { registerXrefObject(image, d, (void *) bm);
      d_image(image, 0, 0, w, h);
      if ( brush )
      { r_clear(0, 0, w, h);		/* clear to deal with hollow, etc */
	r_fill(0, 0, w, h, image);	/* @win_..._brush */
      } else
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
	HBITMAP  dbm = ZCreateCompatibleBitmap(hdcsrc, valInt(w), valInt(h));
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


Image
ws_scale_image(Image image, int w, int h)
{ Image copy = answerObject(ClassImage, NIL,
			    toInt(w), toInt(h), image->kind, 0);
  DisplayObj d = image->display;

  if ( isNil(d) )
    d = CurrentDisplay(image);

  if ( copy && d )
  { HBITMAP sbm = (HBITMAP) getXrefObject(image, d);

    if ( sbm )
    { HDC hdcsrc   = CreateCompatibleDC(NULL);
      HDC hdcdst   = CreateCompatibleDC(hdcsrc);
      HBITMAP osbm = ZSelectObject(hdcsrc, sbm);
      HBITMAP  dbm = ZCreateCompatibleBitmap(hdcsrc, w, h);
      HBITMAP odbm = ZSelectObject(hdcdst, dbm);

      StretchBlt(hdcdst,
		 0, 0, w, h,		/* dest rectangle */
		 hdcsrc,
		 0, 0, valInt(image->size->w), valInt(image->size->h),
		 SRCCOPY);
      
      ZSelectObject(hdcsrc, osbm);
      ZSelectObject(hdcdst, odbm);
      DeleteDC(hdcsrc);
      DeleteDC(hdcdst);

      registerXrefObject(copy, d, (void *) dbm);
    }
  }

  answer(copy);
}

#define falmost(f1, f2) (fabs((f1)-(f2)) < 0.001)

Image
ws_rotate_image(Image image, int a)	/* 0<angle<360 */
{ int ow = valInt(image->size->w);
  int oh = valInt(image->size->h);
  int w, h;
  float angle = ((float)a * M_PI)/180.0;
  float sina, cosa;
  int rot90;				/* rotation by 0,90,180,270 */
  Image copy;				/* rotated image */
  DisplayObj d;

  if ( falmost(angle, M_PI/2) )		/* 90 degrees */
  { w = oh;
    h = ow;
    sina = 1.0;
    cosa = 0.0;
    rot90 = TRUE;
  } else if ( falmost(angle, M_PI) )	/* 180 degrees */
  { w = ow;
    h = oh;
    cosa = -1.0;
    sina = 0.0;
    rot90 = TRUE;
  } else if ( falmost(angle, 3*M_PI/2) ) /* 270 degrees */
  { w = oh;
    h = ow;
    sina = -1.0;
    cosa = 0.0;
    rot90 = TRUE;
  } else
  { rot90 = FALSE;
    sina = sin(angle);
    cosa = cos(angle);

    w = fabs((float)oh*sina) + fabs((float)ow*cosa) + 0.99999;
    h = fabs((float)oh*cosa) + fabs((float)ow*sina) + 0.99999;
  }

  copy = answerObject(ClassImage, NIL,
		      toInt(w), toInt(h), image->kind, 0);
  d = image->display;

  if ( isNil(d) )
    d = CurrentDisplay(image);

  if ( copy && d )
  { HBITMAP sbm = (HBITMAP) getXrefObject(image, d);

    if ( sbm )
    { HDC hdcsrc   = CreateCompatibleDC(NULL);
      HDC hdcdst   = CreateCompatibleDC(hdcsrc);
      HBITMAP osbm = ZSelectObject(hdcsrc, sbm);
      HBITMAP  dbm = ZCreateCompatibleBitmap(hdcsrc, w, h);
      HBITMAP odbm = ZSelectObject(hdcdst, dbm);
      XFORM xform;
      int ogm;
      float acangle = angle;		/* anti-clockwise-angle */

      angle = -angle;			/* Windows wants clockwise */

      registerXrefObject(copy, d, (void *) dbm);
      assign(copy, background, image->background);
      assign(copy, foreground, image->foreground);
      { Any bg = (isDefault(copy->background) ? d->background
					      : copy->background);
	COLORREF rgb = (COLORREF) getXrefObject(bg, d);
	HBRUSH hbrush = ZCreateSolidBrush(GetNearestColor(hdcdst, rgb));
	RECT rect;

	rect.left   = 0;
	rect.right  = w;
	rect.top    = 0;
	rect.bottom = h;

	FillRect(hdcdst, &rect, hbrush);
	ZDeleteObject(hbrush);
      }

      xform.eM11 = cos(angle);
      xform.eM12 = sin(angle);
      xform.eM21 = -xform.eM12;
      xform.eM22 = xform.eM11;
      xform.eDx  = 0;
      xform.eDy  = 0;

      if ( acangle < M_PI/2 )		/* 0<angle<90 */
      { xform.eDy = sin(acangle) * (float)ow;
      } else if ( acangle < M_PI )	/* 90<angle<180 */
      { xform.eDy = h;
	xform.eDx = -cos(acangle) * (float)ow;
      } else if ( acangle < 3*M_PI/2 )	/* 180<angle<270 */
      { xform.eDx = w;
	xform.eDy = h + (sin(acangle) * (float)ow);
      } else				/* 270<angle<360 */
      { xform.eDx = -sin(acangle) * (float)oh;
      }

      DEBUG(NAME_rotate,
	    Cprintf("dx=%g, dy=%g, w = %d, h = %d\n",
		    xform.eDx, xform.eDy, w, h));

      if ( (ogm = SetGraphicsMode(hdcdst, GM_ADVANCED)) &&
	   SetWorldTransform(hdcdst, &xform) )
      { BitBlt(hdcdst,
	       0, 0, ow, oh,		/* dest rectangle */
	       hdcsrc,
	       0, 0,
	       SRCCOPY);
      
	xform.eM11 = 1.0;
	xform.eM12 = 0.0;
	xform.eM21 = 0.0;
	xform.eM22 = 1.0;
	xform.eDx  = 0;
	xform.eDy  = 0;

	SetWorldTransform(hdcdst, &xform);
	SetGraphicsMode(hdcdst, ogm);
      } else
      { Cprintf("No image rotation in win32s\n");
	freeObject(copy);
	copy = FAIL;
      }

      ZSelectObject(hdcsrc, osbm);
      ZSelectObject(hdcdst, odbm);
      DeleteDC(hdcsrc);
      DeleteDC(hdcdst);
    }
  }

  answer(copy);
}


void
ws_postscript_image(Image image, Int depth)
{ int w = valInt(image->size->w);
  int h = valInt(image->size->h);
  int d = valInt(depth);

  d_image(image, 0, 0, w, h);
  postscriptDC(d_current_hdc(), 0, 0, w, h, d);
/*postscriptDrawable(0, 0, w, h);*/
  d_done();
}

#undef roundup
#define roundup(v, n)		((((v)+(n)-1)/(n))*(n))
#define rescale(v, o, n)	((v) * (n) / (o))
#define putByte(b) { ps_put_char(print[(b >> 4) & 0xf]); \
		     ps_put_char(print[b & 0xf]); \
 		     if ( (++bytes % 32) == 0 ) ps_put_char('\n'); \
		     bits = 8; c = 0; \
		   }

static int
brightness(COLORREF rgb, int bright)
{ int r, g, b, i;

  if ( rgb == 0 )
    return 0;
  if ( rgb == RGB(255, 255, 255) )
    return bright;

  r = GetRValue(rgb);
  b = GetBValue(rgb);
  g = GetGValue(rgb);

  i = (r*20 + g*32 + b*18)/(20+32+18);

  return rescale(i, 256, bright);
}


status
postscriptDC(HDC hdc,				/* HDC to print from */
	     int fx, int fy, int w, int h,	/* area to print */
	     int depth)				/* PostScript depth */
{ static char print[] = { '0', '1', '2', '3', '4', '5', '6', '7',
			  '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };
  int x, y, w8, psbright;
  int bits, bytes;
  int c;
  int bmdepth  = GetDeviceCaps(hdc, BITSPIXEL);
  int bmbright = 256;

  if ( depth == 0 )			/* PostScript depth is 1, 2, 4, or 8 */
  { depth = bmdepth;

    if ( depth == 3 )
      depth = 2;
    else if ( depth > 4 && depth < 8 )
      depth = 4;
    else if ( depth > 8 )
      depth = 8;
  }

  w8 = roundup(w, 8);
  psbright = (1<<depth)-1;
  for(bytes = c = 0, bits = 8, y = fy; y < h; y++)
  { for(x = fx; x < w8; x++)
    { int pixval;

      bits -= depth;

      if ( x < w )
      { COLORREF c = GetPixel(hdc, x, y);
	pixval = brightness(c, psbright);
      } else
	pixval = psbright;

      c |= pixval << bits;
      if ( bits == 0 )
        putByte(c);
    }
  }

  succeed;
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
  dest = r->data = pceMalloc(ws_sizeof_bits(w, h));

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
  unsigned short *c, *cbits = pceMalloc(ws_sizeof_bits(w, h));
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

		 /*******************************
		 *    WINDOWS SYSTEM BRUSHES	*
		 *******************************/

struct system_brush
{ char *name;
  int  id;
};

struct system_image
{ char *name;
  int  id;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Windows system colors as obtained from GetSysColor()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static struct system_brush window_brushes[] =
{ { "win_black_brush",	BLACK_BRUSH },
  { "win_dkgray_brush",	DKGRAY_BRUSH },
  { "win_gray_brush",	GRAY_BRUSH },
  { "win_hollow_brush",	HOLLOW_BRUSH },
  { "win_ltgray_brush",	LTGRAY_BRUSH },
  { "win_null_brush",	NULL_BRUSH },
  { NULL,		0 }
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Win32 predefined images.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static struct system_image window_images[] =
{ { "win_btncorners",	OBM_BTNCORNERS },	
  { "win_btsize", 	OBM_BTSIZE },
  { "win_check",	OBM_CHECK },	
  { "win_checkboxes",	OBM_CHECKBOXES },	
  { "win_close",	OBM_CLOSE },	
  { "win_combo",	OBM_COMBO },	
  { "win_dnarrow",	OBM_DNARROW },	
  { "win_dnarrowd",	OBM_DNARROWD },	
  { "win_dnarrowi",	OBM_DNARROWI },	
  { "win_lfarrow",	OBM_LFARROW },	
  { "win_lfarrowd",	OBM_LFARROWD },	
  { "win_lfarrowi",	OBM_LFARROWI },	
  { "win_mnarrow",	OBM_MNARROW },	
/* Not supported in NT 4.0
  { "win_old_close",	OBM_OLD_CLOSE },	
  { "win_old_dnarrow",	OBM_OLD_DNARROW },	
  { "win_old_lfarrow",	OBM_OLD_LFARROW },	
  { "win_old_reduce",	OBM_OLD_REDUCE },	
  { "win_old_restore",	OBM_OLD_RESTORE },
  { "win_old_rgarrow",	OBM_OLD_RGARROW },
  { "win_old_uparrow",	OBM_OLD_UPARROW },
  { "win_old_zoom",	OBM_OLD_ZOOM }, 
*/
  { "win_reduce",	OBM_REDUCE },
  { "win_reduced",	OBM_REDUCED },
  { "win_restore",	OBM_RESTORE },
  { "win_restored",	OBM_RESTORED },
  { "win_rgarrow",	OBM_RGARROW },
  { "win_rgarrowd",	OBM_RGARROWD },
  { "win_rgarrowi",	OBM_RGARROWI },
  { "win_size",		OBM_SIZE },
  { "win_uparrow",	OBM_UPARROW },
  { "win_uparrowd",	OBM_UPARROWD },
  { "win_uparrowi",	OBM_UPARROWI },	
  { "win_zoom",		OBM_ZOOM },
  { "win_zoomd",	OBM_ZOOMD },
  { NULL,		0 }
};


static void
ws_system_brushes(DisplayObj d)
{ struct system_brush *sb = window_brushes;

  for( ; sb->name; sb++)
  { Name name = CtoKeyword(sb->name);
    HBRUSH brush = GetStockObject(sb->id);

    if ( brush )
    { Image image = globalObject(name, ClassImage, name,
				 toInt(16), toInt(16), NAME_pixmap, 0);
      assign(image, access, NAME_read);
      declareWindowsBrush(image, brush);
    } else
      Cprintf("Could not GetStockObject for %s\n", sb->name);
  }
}


void
ws_system_images(DisplayObj d)
{ struct system_image *si = window_images;

  for( ; si->name; si++)
  { Name name = CtoKeyword(si->name);
    HBITMAP bm = LoadBitmap(NULL, (const char *)si->id);

    if ( bm )
    { BITMAP bitmap;
      Image image;
      Name kind;
      int depth;

      if ( !GetObject(bm, sizeof(BITMAP), &bitmap) )
	Cprintf("ws_system_images(): GetObject() failed\n");

      depth = bitmap.bmPlanes * bitmap.bmBitsPixel;
      kind = (depth == 1 ? NAME_bitmap : NAME_pixmap);
      image = globalObject(name, ClassImage, name,
			   toInt(bitmap.bmWidth),
			   toInt(bitmap.bmHeight),
			   kind,
			   0);
      assign(image, depth, toInt(depth));
      registerXrefObject(image, d, (void *)bm);
      assign(image, access, NAME_read);
    } else
      Cprintf("Could not LoadBitmap for %s\n", si->name);
  }

  ws_system_brushes(d);
}


		 /*******************************
		 *	      PALETTE		*
		 *******************************/

ColourMap
ws_colour_map_for_image(Image img)
{ WsImage wsi = img->ws_ref;

  if ( wsi && wsi->msw_info )
  { HPALETTE hpal = CreateDIBPalette(wsi->msw_info);

    if ( hpal )
    { ColourMap cm = answerObject(ClassColourMap, 0);
      
      setPaletteColourMap(cm, hpal);

      answer(cm);
    }
  }

  fail;
}
