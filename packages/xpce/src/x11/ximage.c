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
#include <h/unix.h>
#include <h/graphics.h>
#include <math.h>
#include "include.h"

#ifdef HAVE_LIBXPM
#include <X11/xpm.h>
#endif

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#ifndef ROUNDUP
#define ROUNDUP(v, n) ((((v)+(n)-1)/(n)) * (n))
#endif

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

static XImage *
getXImageImageFromScreen(Image image)
{ if ( notNil(image->display) )
  { DisplayWsXref r = image->display->ws_ref;
    Display *d = r->display_xref;
    XImage *i;

    i = XGetImage(d,
		  (Drawable) getXrefObject(image, image->display),
		  0, 0,
		  valInt(image->size->w), valInt(image->size->h),
		  AllPlanes, ZPixmap);

				/* very dubious; Seems to work though */
    if ( i && image->kind == NAME_bitmap )
    { assert(i->depth == 1);
      i->format = XYBitmap;
    }

    if ( i && !i->red_mask && i->depth > 8 )
    { Visual *v = DefaultVisual(d, DefaultScreen(d));

      if ( v )
      { i->red_mask   = v->red_mask;
	i->green_mask = v->green_mask;
	i->blue_mask  = v->blue_mask;
      }

      assert(i->red_mask);
    }

    return i;
  }

  return NULL;
}


status
ws_store_image(Image image, FileObj file)
{ XImage *i;
  int dofree=FALSE;

  if ( !(i=getXImageImage(image)) )
  { if ( (i = getXImageImageFromScreen(image)) )
      dofree = TRUE;
  }

  if ( i )
  { DisplayObj d = image->display;
    DisplayWsXref r;
    IOSTREAM *fd = Sopen_FILE(file->fd, SIO_OUTPUT); /* HACK */

    if ( isNil(d) )
      d = CurrentDisplay(image);
    
    r = d->ws_ref;

    Sputc('P', fd);
    DEBUG(NAME_ppm, Cprintf("Saving PNM image from index %d\n", Stell(fd)));
    if ( write_pnm_file(fd, i, r->display_xref, 0, 0, 0, PNM_RUNLEN) < 0 )
    { Sclose(fd);
      fail;
    }

    if ( dofree )
      XDestroyImage(i);

    Sclose(fd);
    DEBUG(NAME_ppm, Cprintf("Saved PNM image to index %d\n", Stell(fd)));
  } else
    return errorPce(image, NAME_cannotSaveObject, NAME_noImage);

  succeed;
}




status
loadXImage(Image image, IOSTREAM *fd)
{ XImage *im, *tmp = (XImage *)XMalloc(sizeof(XImage));
  char *data;
  int c;
  int size;
  DisplayObj d = image->display;
  DisplayWsXref r;

  if ( (c = Sgetc(fd)) != 'I' )
  { Sungetc(c, fd);
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
  Sfread(data, 1, size, fd);

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
loadPNMImage(Image image, IOSTREAM *fd)
{ DisplayWsXref r;
  Display *d;
  XImage *i;

  if ( isNil(image->display) )
    assign(image, display, CurrentDisplay(image));
  openDisplay(image->display);

  r = image->display->ws_ref;
  d = r->display_xref;

  DEBUG(NAME_ppm, Cprintf("Loading PNM image from index %d\n", Stell(fd)));
  if ( (i = read_ppm_file(d, 0, 0, fd)) )
  { setXImageImage(image, i);
    assign(image, depth, toInt(i->depth));

    DEBUG(NAME_ppm, Cprintf("Image loaded, index = %d\n", Stell(fd)));
    succeed;
  }

  DEBUG(NAME_ppm, Cprintf("Failed to load image\n"));
  fail;
}


status
ws_load_old_image(Image image, IOSTREAM *fd)
{ XImage *im = readImageFile(image, fd);

  setXImageImage(image, im);
  if ( im )
    setSize(image->size, toInt(im->width), toInt(im->height));

  succeed;
}


status
ws_load_image_file(Image image)
{ XImage *i;
  IOSTREAM *fd;

  if ( !(fd = Sopen_object(image->file, "rbr")) )
    fail;

  if ( !(i = readImageFile(image, fd)) )
  { DisplayWsXref r;
    Display *d;

    if ( isNil(image->display) )
      assign(image, display, CurrentDisplay(image));
    openDisplay(image->display);

    r = image->display->ws_ref;

    d = r->display_xref;

    i = read_ppm_file(d, 0, 0, fd);
  }
  Sclose(fd);

  if ( i )
  { if ( getXImageImage(image) )
    { XcloseImage(image, DEFAULT);
      ws_destroy_image(image);
    }
    assign(image, depth, toInt(i->depth));
    assign(image, kind, image->depth == ONE ? NAME_bitmap : NAME_pixmap);
    setXImageImage(image, i);
    setSize(image->size, toInt(i->width), toInt(i->height));

    succeed;
  }

  return errorPce(image->file, NAME_badFile, NAME_image);
}


Image
ws_std_xpm_image(Name name, Image *global, char **data)
{
#ifdef HAVE_LIBXPM
  extern XImage *attachXpmImageImage(Image image, XpmImage *xpm);

  Image image = globalObject(name, ClassImage, name, ZERO, ZERO, EAV);
  XpmImage img;
  XpmInfo info;
  XImage *i;

  assign(image, display, CurrentDisplay(NIL));
  XpmCreateXpmImageFromData(data, &img, &info);
  if ( (i=attachXpmImageImage(image, &img)) )
  { assign(image, depth, toInt(i->depth));
    assign(image, kind, image->depth == ONE ? NAME_bitmap : NAME_pixmap);
    setXImageImage(image, i);
    setSize(image->size, toInt(i->width), toInt(i->height));
  }
  XpmFreeXpmImage(&img);
  assign(image, access, NAME_read);
  if ( global )
    *global = image;

  return image;
#else
  assert(0);
  fail;
#endif
}


static FileObj
mustBeFile(SourceSink ss)
{ if ( instanceOfObject(ss, ClassFile) )
    return (FileObj)ss;

  errorPce(ss, NAME_unexpectedType, nameToType(NAME_file));
  fail;
}


status
ws_save_image_file(Image image, SourceSink into, Name fmt)
{ DisplayObj d = image->display;
  DisplayWsXref r;

  if ( isNil(d) )
    d = CurrentDisplay(image);
  r = d->ws_ref;

  if ( fmt == NAME_xbm )
  { Pixmap pix;
    FileObj file;

    if ( !(file=mustBeFile(into)) )
      fail;

    pix = (Pixmap) getXrefObject(image, d);

    if ( XWriteBitmapFile(r->display_xref,
			  strName(file->name),
			  pix,
			  valInt(image->size->w), valInt(image->size->h),
			  -1, -1) != BitmapSuccess )
      return errorPce(image, NAME_xError);
  } else if ( fmt == NAME_xpm )
  {
#ifdef HAVE_LIBXPM
    Pixmap pix = (Pixmap) getXrefObject(image, d);
    Pixmap pmsk = 0;
    int as = XpmAttributesSize();
    XpmAttributes *atts = (XpmAttributes *)alloca(as);

    FileObj file;

    if ( !(file=mustBeFile(into)) )
      fail;

    memset(atts, 0, as);
    atts->width     = valInt(image->size->w);
    atts->height    = valInt(image->size->h);
    atts->valuemask = XpmSize;

    if ( notNil(image->hot_spot) )
    { atts->x_hotspot = valInt(image->hot_spot->x);
      atts->y_hotspot = valInt(image->hot_spot->y);
      atts->valuemask |= XpmHotspot;
    }
    if ( notNil(image->mask) )
      pmsk = (Pixmap) getXrefObject(image->mask, d);

    if ( XpmWriteFileFromPixmap(r->display_xref,
				strName(file->name),
				pix,
				pmsk,
				atts) != XpmSuccess )
      return errorPce(image, NAME_xError);
#else /*HAVE_LIBXPM*/
    return errorPce(image, NAME_noImageFormat, NAME_xpm);
#endif /*HAVE_LIBXPM*/
  } else if ( fmt == NAME_jpeg )
  {
#ifdef HAVE_LIBJPEG
    XImage *i;
    int dofree = FALSE;
      
    if ( !(i=getXImageImage(image)) )
    { if ( (i=getXImageImageFromScreen(image)) )
	dofree = TRUE;
    }
  
    if ( i )
    { status rval;
      IOSTREAM *fd;
    
      if ( !(fd=Sopen_object(into, "wbr")) )
	fail;

      if ( write_jpeg_file(fd, i, r->display_xref, 0, image) < 0 )
	rval = errorPce(image, NAME_xError);
      else
	rval = SUCCEED;

      if ( dofree )
	XDestroyImage(i);

      if ( Sclose(fd) != 0 )
	rval = FAIL;

      return rval;
    } else
      fail;
#else
    return errorPce(image, NAME_noImageFormat, NAME_jpeg);
#endif /*HAVE_LIBJPEG*/
  } else if ( fmt == NAME_gif )
  {
#ifdef O_GIFWRITE
    XImage *i, *msk=NULL;
    int dofree = FALSE;
      
    if ( !(i=getXImageImage(image)) )
    { if ( (i=getXImageImageFromScreen(image)) )
	dofree = TRUE;
    }
  
    if ( i )
    { status rval;
      IOSTREAM *fd;

      if ( notNil(image->mask) )
      { if ( !(msk = getXImageImage(image->mask)) )
	  msk = getXImageImageFromScreen(image->mask);
      }

      if ( !(fd=Sopen_object(into, "wbr")) )
	fail;

      if ( write_gif_file(fd, i, msk, r->display_xref, 0) < 0 )
	rval = errorPce(image, NAME_xError);
      else
	rval = SUCCEED;

      if ( dofree )
	XDestroyImage(i);

      if ( Sclose(fd) != 0 )
	rval = FAIL;

      return rval;
    }
#else
    return errorPce(image, NAME_noImageFormat, NAME_gif);
#endif /*O_GIFWRITE*/
  } else
  { int pnm_fmt;
    XImage *i;
    int dofree = FALSE;

    if ( fmt == NAME_pnm )	pnm_fmt = PNM_PNM;
    else if ( fmt == NAME_pbm )	pnm_fmt = PNM_PBM;
    else if ( fmt == NAME_pgm )	pnm_fmt = PNM_PGM;
    else if ( fmt == NAME_ppm )	pnm_fmt = PNM_PPM;
    else fail;
    
    if ( !(i=getXImageImage(image)) )
    { if ( (i=getXImageImageFromScreen(image)) )
	dofree = TRUE;
    }
  
    if ( i )
    { IOSTREAM *fd;
      status rval;

      if ( !(fd=Sopen_object(into, "wbr")) )
	fail;
      if ( write_pnm_file(fd, i, r->display_xref,
			  0,		/* colour map */
			  0,		/* scale [256] */
			  pnm_fmt,	/* format */
			  PNM_RAWBITS)	/* encoding */
	     < 0 )
	rval = errorPce(image, NAME_xError);
      else
	rval = SUCCEED;

      if ( dofree )
	XDestroyImage(i);

      if ( Sclose(fd) != 0 )
	rval = FAIL;

      return rval;
    } else
      fail;
  }
  
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
  XImage *i;

  openDisplay(d);
  r = d->ws_ref;

  if ( (i=getXImageImage(image)) )
  { if ( isDefault(image->depth) )
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
  {
#ifdef O_PPM
    if ( notNil(image->display) && image->display != d )
    { errorPce(image, NAME_xMovedDisplay, d);
      XcloseImage(image, image->display);
    }
    assign(image, display, d);
#endif
    TRY(loadImage(image, DEFAULT, DEFAULT));
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
		 /*******************************
		 *	   CREATE XIMAGE	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
MakeXImage() creates an Ximage structure  with   the  same properties as
oimage of the `prototype' XImage structure. Used  by scale and rotate to
create the target image.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static XImage *
MakeXImage(Display *dpy, XImage *oimage, int w, int h)
{ XImage *I;
  char *data;
  int bytes_per_line = ROUNDUP((w * oimage->bits_per_pixel + 7)/8,
			       oimage->bitmap_pad/8);
    
  DEBUG(NAME_image,
	if ( oimage->depth != oimage->bits_per_pixel )
	  Cprintf("depth = %d, bits_per_pixel = %d\n",
		  oimage->depth, oimage->bits_per_pixel));

  /* reserve memory for image */
  data = malloc(bytes_per_line * h);
  if ( data == NULL )
    return NULL;
  memset(data, 0, bytes_per_line * h);
    
  /* create the XImage */
  I = XCreateImage(dpy, DefaultVisual(dpy, DefaultScreen(dpy)),
		   oimage->depth, oimage->format,
		   0, data,
		   w, h,
		   oimage->bitmap_pad, bytes_per_line);
  if ( I == NULL )
    return NULL;
    
  return I;
}

		 /*******************************
		 *	SCALE/ZOOM IMAGES	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This code is inspired by  the  module   zoom.c  from  the xloadimage 4.1
package written by Jim Frost. The extend   of the `inspiration' makes me
feel free to leave the original copyright  notice out, where it is noted
that the original  copyright  allows   for  modification,  embedding and
commercial usage of the code without a fee and the original author is in
no way responsible for the consequences  of   using  his  code. I hereby
thank him for providing a starting point.

XPCE's bitmap scaling isn't  very   professional.  Scaling  up generally
provides acceptable results, but scaling   down monochrome images should
exploit better heuristics,  while  scaling   down  colour  images should
exploit the freedom of the colour system.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static unsigned int *
buildIndex(unsigned width, unsigned rwidth)
{ float         fzoom;
  int		 zoom;
  unsigned int *index;
  unsigned int  a;


  if ( width == rwidth )
  { zoom  = FALSE;
    fzoom = 100.0;
  } else
  { zoom  = TRUE;
    fzoom = (float)rwidth / (float) width;
  }

  index = pceMalloc(sizeof(unsigned int) * rwidth);

  if ( zoom )
  { for(a=0; a < rwidth; a++)
      index[a] = rfloat((float)a/fzoom);
  } else
  { for(a=0; a < rwidth; a++)
      index[a] = a;
  }

  return index;
}

static XImage *
ZoomXImage(Display *dsp, Visual *v, XImage *oimage,
	   unsigned xwidth, unsigned ywidth)
{ XImage       *image;
  unsigned int *xindex, *yindex;
  unsigned int  x, y, xsrc, ysrc;
  unsigned long value;

  xindex = buildIndex(oimage->width,  xwidth);
  yindex = buildIndex(oimage->height, ywidth);

					/* determine dimensions */
  image = MakeXImage(dsp, oimage, xwidth, ywidth);

  switch( oimage->format )
  { case XYBitmap:
    default:				/* colour images */
      for(y = 0; y < ywidth; y++)
      { ysrc = yindex[y];

	for(x= 0; x < xwidth; x++)
	{ xsrc = xindex[x];

	  value = XGetPixel(oimage, xsrc, ysrc);
	  XPutPixel(image, x, y, value);
	}
      }
      break;
  }

  pceFree(xindex);
  pceFree(yindex);

  return(image);
}


Image
ws_scale_image(Image image, int w, int h)
{ Image copy = answerObject(ClassImage, NIL,
			    toInt(w), toInt(h), image->kind, EAV);
  XImage *i;
  DisplayObj d = image->display;
  DisplayWsXref r;
  int dofree = FALSE;

  if ( isNil(d) )
    d = CurrentDisplay(image);
  r = d->ws_ref;

  if ( !(i=getXImageImage(image)) )
  { if ( (i=getXImageImageFromScreen(image)) )
      dofree = TRUE;
  }

  if ( i )
  { XImage *ic = ZoomXImage(r->display_xref,
			    DefaultVisual(r->display_xref,
					  DefaultScreen(r->display_xref)),
			    i, w, h);

    setXImageImage(copy, ic);
    assign(copy, depth, toInt(ic->depth));

    if ( dofree )
      XDestroyImage(i);
  }

  answer(copy);
}


		 /*******************************
		 *	       ROTATE		*
		 *******************************/


#define falmost(f1, f2) (fabs((f1)-(f2)) < 0.001)


static XImage *
RotateXImage(Display *dsp, XImage *oimage, float angle, Pixel bg)
{ int ow = oimage->width;
  int oh = oimage->height;
  int w, h;
  float sina, cosa;
  XImage *nimage;
  float dj;
  int byte_w_in, byte_w_out;
  float xl, xr, xinc;
  int it, jt;
  int i, j;
  int rot90;				/* rotation by 0,90,180,270 */

					/* angle = 0.0 has been handled! */
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

    w = fabs((float)oh*sina) + fabs((float)ow*cosa) + 0.99999 + 2;
    h = fabs((float)oh*cosa) + fabs((float)ow*sina) + 0.99999 + 2;
    if ( w%2 == 0 )
      w++;
    if ( h%2 == 0 )
      h++;
  }

					/* make a new image */
  if ( !(nimage = MakeXImage(dsp, oimage, w, h)) )
    return NULL;
    
  byte_w_in  = oimage->bytes_per_line;
  byte_w_out = nimage->bytes_per_line;
    
  /* vertical distance from centre */
  dj = 0.5-(float)h/2;

  /* where abouts does text actually lie in rotated image? */
  if ( rot90 )
  { xl = 0;
    xr = (float)w;
    xinc = 0;
  } else if ( angle < M_PI )
  { xl = (float)w/2 + (dj-(float)oh/(2*cosa)) / tan(angle)-2;
    xr = (float)w/2 + (dj+(float)oh/(2*cosa)) / tan(angle)+2;
    xinc = 1./tan(angle);
  } else
  { xl=(float)w/2 + (dj+(float)oh/(2*cosa)) / tan(angle)-2;
    xr=(float)w/2 + (dj-(float)oh/(2*cosa)) / tan(angle)+2;
	
    xinc=1./tan(angle);
  }

  DEBUG(NAME_rotate, Cprintf("bg = %ld\n", bg));

  /* loop through all relevent bits in rotated image */
  for(j=0; j<h; j++)
  { /* no point re-calculating these every pass */
    float di=(float)((xl<0)?0:(int)xl)+0.5-(float)w/2;
    int byte_out=(h-j-1)*byte_w_out;
	
    /* loop through meaningful columns */
    for(i=((xl<0)?0:(int)xl); i<((xr>=w)?w:(int)xr); i++)
    {	    
      /* rotate coordinates */ 
      it=(float)ow/2 + ( di*cosa + dj*sina);
      jt=(float)oh/2 - (-di*sina + dj*cosa);
	    
      /* set pixel if required */
      if ( it>=0 && it<ow && jt>=0 && jt<oh )
      { if ( oimage->depth == 1 )	/* monochrome */
	{ if ( oimage->bitmap_bit_order == MSBFirst )
	  { if ( (oimage->data[jt*byte_w_in+it/8] & 128>>(it%8))>0 )
	      nimage->data[byte_out+i/8]|=128>>i%8;
	  } else
	  { if ( (oimage->data[jt*byte_w_in+it/8] & 1<<(it%8))>0 )
	      nimage->data[byte_out+i/8]|=1<<i%8;
	  }
	} else				/* General case (colour) */
	{ unsigned long pxl = XGetPixel(oimage, it, jt);
	  XPutPixel(nimage, i, h-j-1, pxl);
	}
      } else
      { if ( oimage->depth != 1 )	/* fill background */
	  XPutPixel(nimage, i, h-j-1, bg);
      }

      di+=1;
    }

    for(i=0; i<xl; i++)			/* more background */
      XPutPixel(nimage, i, h-j-1, bg);
    for(i=xr; i<w; i++)
      XPutPixel(nimage, i, h-j-1, bg);

    dj+=1;
    xl+=xinc;
    xr+=xinc;
  }

  return nimage;
}


Image
ws_rotate_image(Image image, float angle)	/* 0.0<angle<360.0 */
{ XImage *i;
  DisplayObj d = image->display;
  DisplayWsXref r;
  int dofree = FALSE;

  if ( isNil(d) )
    d = CurrentDisplay(image);
  r = d->ws_ref;

  if ( !(i=getXImageImage(image)) )
  { if ( (i=getXImageImageFromScreen(image)) )
      dofree = TRUE;
  }

  if ( i )
  { XImage *ic;
    Image copy;
    Pixel bg;

    if ( image->kind == NAME_pixmap )
    { if ( instanceOfObject(image->background, ClassColour) )
	bg = getPixelColour(image->background, d);
      else
      { DisplayWsXref r = d->ws_ref;
	
	bg = r->pixmap_context->background_pixel;
      }
    } else
      bg = 0L;

    ic   = RotateXImage(r->display_xref, i, (angle * M_PI)/180.0, bg);
    copy = answerObject(ClassImage, NIL,
			toInt(ic->width), toInt(ic->height),
			image->kind, EAV);
    assign(copy, background, image->background);
    assign(copy, foreground, image->foreground);
    setXImageImage(copy, ic);
    assign(copy, depth, toInt(ic->depth));

    if ( dofree )
      XDestroyImage(i);

    return(copy);
  }

  fail;
}

		 /*******************************
		 *       COLOUR --> MONO	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is rather simple, but it will do   for the moment. A `real' version
should consider the actual colours, properly  dithering the colour image
on the monochrome one.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Image
ws_monochrome_image(Image image)
{ Image mono;
  Int w = image->size->w;
  Int h = image->size->h;

  mono = answerObject(ClassImage, NIL, w, h, NAME_bitmap, EAV);
  d_image(mono, 0, 0, valInt(w), valInt(h));
  d_modify();
  r_image(image, 0, 0, 0, 0, valInt(w), valInt(h), OFF);
  d_done();

  answer(mono);
}


void
ws_prepare_image_mask(Image image)
{ /* nothing to do here */
}


		 /*******************************
		 *	     POSTSCRIPT		*
		 *******************************/


void
ws_postscript_image(Image image, Int depth, int iscolor)
{ int w = valInt(image->size->w);
  int h = valInt(image->size->h);
  XImage *i;
  int dofree = FALSE;

  if ( !(i=getXImageImage(image)) )
  { if ( (i=getXImageImageFromScreen(image)) )
      dofree = TRUE;
  }

  if ( i && i->f.get_pixel )
  { DisplayObj d = image->display;
    DisplayWsXref r;

    if ( isNil(d) )
      d = CurrentDisplay(image);
    openDisplay(d);
    r = d->ws_ref;

    postscriptXImage(i,
		     0, 0, i->width, i->height,
		     r->display_xref,
		     r->colour_map,
		     isDefault(depth) ? 0 : valInt(depth),
		     iscolor);
  } else
  { d_image(image, 0, 0, w, h);
    postscriptDrawable(0, 0, w, h,
		       isDefault(depth) ? 0 : valInt(depth),
		       iscolor);
    d_done();
  }

  if ( dofree )
    XDestroyImage(i);
}

		 /*******************************
		 *	   XLI INTERFACE	*
		 *******************************/

#ifdef O_XLI

#include <libxli.h>

/*typedef Ximage * (*xliloadf)(const char *name, XliOptions options);*/

status
loadXliImage(Image image, FileObj file, Int bright)
{ DisplayWsXref r;
  Display *d;
  XImage *im;
  xli_options opts;

  if ( isNil(image->display) )
    assign(image, display, CurrentDisplay(image));
  openDisplay(image->display);

  r = image->display->ws_ref;
  d = r->display_xref;

  memset(&opts, 0, sizeof(opts));

  opts.verbose = TRUE;			/*FALSE;*/
  opts.display = d;
  opts.screen  = DefaultScreen(d);
  opts.visual  = DefaultVisual(d, opts.screen);
  opts.bright  = (isDefault(bright) ? 100 : valInt(bright));
  
  if ( (im = XliLoadXImage(strName(file->name), &opts)) )
  { setSize(image->size, toInt(im->width), toInt(im->height));
    assign(image, depth, toInt(im->depth));

    if ( image->depth != ONE )
      assign(image, kind, NAME_pixmap);

    setXImageImage(image, im);
    succeed;
  } else
    fail;
}

#endif /*O_XLI*/

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


		 /*******************************
		 *	     COLOUR-MAP		*
		 *******************************/

ColourMap
ws_colour_map_for_image(Image image)
{ fail;
}


void
ws_system_images()
{
}
