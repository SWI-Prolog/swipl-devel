/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/unix.h>
#include <h/graphics.h>
#include "include.h"


static XImage *
getXImageImage(Image image)
{ return image->ws_ref;
}


void
ws_init_image(Image image)
{ setXImageImage(image, NULL);
}


void
ws_destroy_image(Image image)
{ XImage *i;

  if ( (i=getXImageImage(image)) && i->f.destroy_image )
    XDestroyImage(i);

  setXImageImage(image, NULL);
}


		 /*******************************
		 *	    LOAD/STORE		*
		 *******************************/

#define DataSize(Image) ((Image)->bytes_per_line * (Image)->height)

static status
dumpXImage(XImage *image, FILE *fd)
{ putc('I', fd);
  putstdw(image->width, fd);
  putstdw(image->height, fd);
  putstdw(image->xoffset, fd);
  putstdw(image->format, fd);
  putstdw(image->byte_order, fd);
  putstdw(image->bitmap_unit, fd);
  putstdw(image->bitmap_bit_order, fd);
  putstdw(image->bitmap_pad, fd);
  putstdw(image->depth, fd);
  putstdw(image->bytes_per_line, fd);
  fwrite(image->data, 1, DataSize(image), fd);

  succeed;
}


static void
getXImageImageFromScreen(Image image)
{ if ( notNil(image->display) && image->kind == NAME_bitmap )
  { DisplayWsXref r = image->display->ws_ref;
    XImage *i;

    i = XGetImage(r->display_xref,
		  (Drawable) getXrefObject(image, image->display),
		  0, 0,
		  valInt(image->size->w), valInt(image->size->h),
		  AllPlanes, ZPixmap);

    setXImageImage(image, i);
  }
}


status
ws_store_image(Image image, FileObj file)
{ XImage *i;

  if ( !(i=getXImageImage(image)) )
  { getXImageImageFromScreen(image);
    i=getXImageImage(image);
  }

  if ( i )
  { putc('X', file->fd);
    dumpXImage(i, file->fd);
  } else
    return errorPce(image, NAME_cannotSaveObject, NAME_noImage);

  succeed;
}




status
loadXImage(Image image, FILE *fd)
{ XImage *im, *tmp = (XImage *)XMalloc(sizeof(XImage));
  char *data;
  char c;
  int size;
  DisplayObj d = image->display;
  DisplayWsXref r;

  if ( (c = getc(fd)) != 'I' )
  { ungetc(c, fd);
    fail;
  }

  tmp->width            = loadWord(fd);
  tmp->height           = loadWord(fd);
  tmp->xoffset          = loadWord(fd);
  tmp->format           = loadWord(fd);
  tmp->byte_order       = loadWord(fd);
  tmp->bitmap_unit      = loadWord(fd);
  tmp->bitmap_bit_order = loadWord(fd);
  tmp->bitmap_pad       = loadWord(fd);
  tmp->depth            = loadWord(fd);
  tmp->bytes_per_line   = loadWord(fd);

  size = DataSize(tmp);
  data = XMalloc(size);
  tmp->data = data;
  fread(data, 1, size, fd);

  if ( isNil(d) )
    d = CurrentDisplay(image);
  r = d->ws_ref;

  im = XCreateImage(r->display_xref,
		    DefaultVisual(r->display_xref,
				  DefaultScreen(r->display_xref)),
		    tmp->depth, tmp->format, tmp->xoffset, tmp->data,
		    tmp->width, tmp->height,
		    tmp->bitmap_pad, tmp->bytes_per_line);

  XFree(tmp);
  setXImageImage(image, im);
  assign(image, depth, toInt(im->depth));
  if ( restoreVersion < 7 )
    setSize(image->size, toInt(im->width), toInt(im->height));

  succeed;
}


status
ws_load_old_image(Image image, FILE *fd)
{ XImage *im = readImageFile(fd);

  setXImageImage(image, im);
  if ( im )
    setSize(image->size, toInt(im->width), toInt(im->height));

  succeed;
}


status
ws_load_image_file(Image image)
{ XImage *i;

  TRY( openFile(image->file, NAME_read, DEFAULT, DEFAULT) );
  i = readImageFile(image->file->fd);
  closeFile(image->file);

  if ( i )
  { if ( getXImageImage(image) )
    { XcloseImage(image, DEFAULT);
      ws_destroy_image(image);
    }
    setXImageImage(image, i);
    setSize(image->size, toInt(i->width), toInt(i->height));
  } else
    return errorPce(image->file, NAME_badFile, NAME_image);

  succeed;
}


status
ws_save_image_file(Image image, FileObj file)
{ DisplayObj d = image->display;
  Pixmap pix;
  DisplayWsXref r;

  if ( isNil(d) )
    d = CurrentDisplay(image);
  r = d->ws_ref;

  pix = (Pixmap) getXrefObject(image, d);

  if ( XWriteBitmapFile(r->display_xref,
			strName(file->name),
			pix,
			valInt(image->size->w), valInt(image->size->h),
			-1, -1) != BitmapSuccess )
    return errorPce(image, NAME_xError);
  
  succeed;
}


		 /*******************************
		 *	    OPEN/CLOSE		*
		 *******************************/


status
ws_open_image(Image image, DisplayObj d)
{ int w = valInt(image->size->w);
  int h = valInt(image->size->h);
  Pixmap pixmap = 0;
  DisplayWsXref r;

  openDisplay(d);
  r = d->ws_ref;

  if ( getXImageImage(image) )
  { XImage *i = getXImageImage(image);

    if ( isDefault(image->depth) )
      assign(image, depth, toInt(i->depth));
    if ( (pixmap = XCreatePixmap(r->display_xref,
				 XtWindow(r->shell_xref),
				 w, h, i->depth)) != 0 )
    { XPutImage(r->display_xref, pixmap,
		image->kind == NAME_bitmap ? r->bitmap_context->copyGC
					   : r->pixmap_context->copyGC,
		i, 0, 0, 0, 0, i->width, i->height);
    }
  } else if ( notNil(image->file) )
  { TRY(loadImage(image, DEFAULT, DEFAULT));
    return XopenImage(image, d);
  } else if ( w != 0 && h != 0 && image->access == NAME_both )
  { if ( notNil(image->display) && image->display != d )
    { errorPce(image, NAME_xMovedDisplay, d);
      XcloseImage(image, image->display);
    }

    assign(image, display, d);

    if ( image->kind == NAME_pixmap )
    { assign(image, depth, toInt(r->depth));
      if ( isDefault(image->background) )
	assign(image, background, d->background);
      if ( isDefault(image->foreground) )
	assign(image, foreground, d->foreground);
    }

    if ( (pixmap = XCreatePixmap(r->display_xref,
				 XtWindow(r->shell_xref),
				 w, h, valInt(image->depth))) != 0 )
    { int w = valInt(image->size->w);
      int h = valInt(image->size->h);

      registerXrefObject(image, d, (XtPointer) pixmap);
      d_image(image, 0, 0, w, h);
      r_clear(0, 0, w, h);
      d_done();
    }

    succeed;
  }
    
  if ( pixmap != 0 )
    return registerXrefObject(image, d, (XtPointer) pixmap);
  
  fail;
}


void
ws_close_image(Image image, DisplayObj d)
{ Xref r;

  while( (r = unregisterXrefObject(image, d)) )
  { DisplayWsXref xr = r->display->ws_ref;
    XFreePixmap(xr->display_xref, (Pixmap) r->xref);
  }
}

		 /*******************************
		 *	      RESIZE		*
		 *******************************/


status
ws_resize_image(Image image, Int w, Int h)
{ if ( notNil(image->display) )
  { DisplayObj d = image->display;
    DisplayWsXref r = d->ws_ref;
    Display *display = r->display_xref;
    Int iw = image->size->w;
    Int ih = image->size->h;

    if ( iw != w || ih != h )
    { Pixmap old = (Pixmap) getExistingXrefObject(image, d);
      Pixmap new = 0;

      if ( old == 0 )
	return setSize(image->size, w, h);

      if ( valInt(w) > 0 && valInt(h) > 0 )
      { DrawContext gcs = (image->kind == NAME_bitmap ? r->bitmap_context
						      : r->pixmap_context);

	new = XCreatePixmap(display, XtWindow(r->shell_xref),
			    valInt(w), valInt(h), valInt(image->depth));
	if ( new == 0 )
	  return errorPce(image, NAME_xError);

	if ( valInt(w) > valInt(iw) || valInt(h) > valInt(ih) )
	  XFillRectangle(display, new, gcs->clearGC,
			 0, 0, valInt(w), valInt(h));

	XCopyArea(display, old, new, gcs->copyGC, 0, 0,
		  min(valInt(iw), valInt(w)),
		  min(valInt(ih), valInt(h)), 0, 0);
      }

      XcloseImage(image, d);
      registerXrefObject(image, d, (XtPointer) new);
    }
  }

  return setSize(image->size, w, h);
}


void
ws_postscript_image(Image image)
{ int w = valInt(image->size->w);
  int h = valInt(image->size->h);
  XImage *im;

  if ( (im = getXImageImage(image)) && im->f.get_pixel )
  { ulong bg;

    if ( image->kind == NAME_bitmap )
      bg = 0L;
    else
    { DisplayObj d = image->display;

      if ( isNil(d) )
	d = CurrentDisplay(image);
      openDisplay(d);

      bg = getPixelColour(notDefault(image->background) ? image->background
			  				: d->background,
			  d);
    }

    postscriptXImage(im, w, h, bg);
  } else
  { d_image(image, 0, 0, w, h);
    postscriptDrawable(0, 0, w, h);
    d_done();
  }
}

		 /*******************************
		 *	     X11 SOURCE		*
		 *******************************/

void
ws_create_image_from_x11_data(Image image,
			      unsigned char *data,
			      int w, int h)
{ XImage *i;

  i = CreateXImageFromData(data, w, h);
  setXImageImage(image, i);
}
