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

#define O_GIFTOXPM 1

#ifdef HAVE_LIBXPM
#include <X11/xpm.h>
static XImage *readXpmFile(Image image, IOSTREAM *fd);
#ifdef O_GIFTOXPM
#include <img/gif.h>
static XImage *readGIFFile(Image image, IOSTREAM *fd);
#endif
#include <img/jpeg.h>
#ifdef HAVE_LIBJPEG
static XImage *readJPEGFile(Image image, IOSTREAM *fd);
#endif
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Read various bitmap-formats and  convert them into  an X11 bitmap-data
string to be used with XCreateBitmapFromData().  Functions provided:

char *read_bitmap_file(IOSTREAM *fd, int *w, int *h)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static unsigned char *read_x11_bitmap_file(IOSTREAM *, int *, int *);
static unsigned char *read_sun_icon_file(IOSTREAM *, int *, int *);

#define Round(n, r) ((((n) + (r) - 1) / (r)) * (r))

static unsigned char *
read_bitmap_data(IOSTREAM *fd, int *w, int *h)
{ long offset = Stell(fd);
  unsigned char *rval;
  int c0;

  c0 = Sgetc(fd);
  Sungetc(c0, fd);

  switch(c0)
  { case '#':
      if ( (rval = read_x11_bitmap_file(fd, w, h)) != NULL )
	return rval;
      Sseek(fd, offset, SIO_SEEK_SET);
      break;
    case '/':
      if ( (rval = read_sun_icon_file(fd, w, h)) != NULL )
	return rval;
      Sseek(fd, offset, SIO_SEEK_SET);
      break;
  }

  return NULL;
}

		/********************************
		*          ENTRY POINT		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We should pass the display into these functions!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Display *
defaultXDisplay()
{ DisplayObj d = CurrentDisplay(NIL);
  DisplayWsXref r;

  openDisplay(d);
  r = d->ws_ref;

  return r->display_xref;
}


XImage *
CreateXImageFromData(unsigned char *data, int width, int height)
{ Display *disp = defaultXDisplay();
  XImage *image;

  image = XCreateImage(disp,
		       DefaultVisual(disp, DefaultScreen(disp)),
		       1,
		       XYBitmap,
		       0,
		       data,
		       width, height,
		       8, (width+7)/8);

  image->bits_per_pixel = 1;
  image->byte_order = LSBFirst;
  image->bitmap_unit = 8;
  image->bitmap_bit_order = LSBFirst;
  image->bitmap_pad = 8;

  return image;
}


XImage *
readImageFile(Image image, IOSTREAM *fd)
{ unsigned char *data;
  int w, h;
  XImage *img=NULL;
  char hdr[64];
  int hdrlen;
  long offset = Stell(fd);
  int fmt;

  hdrlen = Sfread(hdr, 1, sizeof(hdr), fd);
  Sseek(fd, offset, SIO_SEEK_SET);

  switch((fmt=image_type_from_data(hdr, hdrlen)))
  { case IMG_IS_UNKNOWN:
    case IMG_IS_XBM:
    case IMG_IS_SUNICON:
      if ( (data = read_bitmap_data(fd, &w, &h)) != NULL )
	return CreateXImageFromData(data, w, h);
      if ( fmt != IMG_IS_UNKNOWN )
	break;
#ifdef HAVE_LIBJPEG
    case IMG_IS_JPEG:
      switch(staticColourReadJPEGFile(image, fd, &img))
      { case IMG_OK:
	  return img;
	case IMG_NOMEM:
	  return NULL;
	default:
	  break;
      }
      if ( (img=readJPEGFile(image, fd)) )
	return img;
      if ( fmt != IMG_IS_UNKNOWN )
	break;
#endif  
#ifdef HAVE_LIBXPM
#ifdef O_GIFTOXPM
    case IMG_IS_GIF:
      if ( (img=readGIFFile(image, fd)) )
	return img;
      if ( fmt != IMG_IS_UNKNOWN )
	break;
#endif
    case IMG_IS_XPM:
      if ( (img=readXpmFile(image, fd)) )
	return img;
      if ( fmt != IMG_IS_UNKNOWN )
	break;
#endif
    default:
      if ( fmt != IMG_IS_UNKNOWN )
      { DEBUG(NAME_image,
	      Cprintf("Image format %d not supported\n", fmt));
      }
  }

  return NULL;
}


		/********************************
		*         X10/X11 FORMAT	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The code below is copied from the MIT X11R5 distribution and modified for
the interface required in XPCE.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAX_SIZE 255

/* shared data for the image read/parse logic */
static short hexTable[256];		/* conversion value */
static int initialized = FALSE;		/* easier to fill in at run time */

#define HT_NODIGIT (-2)

/*
 *	Table index for the hex values. Initialized once, first time.
 *	Used for translation value or delimiter significance lookup.
 */
static void
initHexTable(void)
{ unsigned int i;

  for(i=0; i<(sizeof(hexTable)/sizeof(short)); i++)
    hexTable[i] = -HT_NODIGIT;

  hexTable['0'] = 0;	hexTable['1'] = 1;
  hexTable['2'] = 2;	hexTable['3'] = 3;
  hexTable['4'] = 4;	hexTable['5'] = 5;
  hexTable['6'] = 6;	hexTable['7'] = 7;
  hexTable['8'] = 8;	hexTable['9'] = 9;
  hexTable['A'] = 10;	hexTable['B'] = 11;
  hexTable['C'] = 12;	hexTable['D'] = 13;
  hexTable['E'] = 14;	hexTable['F'] = 15;
  hexTable['a'] = 10;	hexTable['b'] = 11;
  hexTable['c'] = 12;	hexTable['d'] = 13;
  hexTable['e'] = 14;	hexTable['f'] = 15;

  /* delimiters of significance are flagged w/ negative value */
  hexTable[' '] = -1;	hexTable[','] = -1;
  hexTable['}'] = -1;	hexTable['\n'] = -1;
  hexTable['\t'] = -1;
	
  initialized = TRUE;
}

/*
 *	read next hex value in the input stream, return -1 if EOF
 */

static int
NextInt(IOSTREAM *fstream)
{ int value = 0;
  int gotone = 0;
  int done = 0;
    
    /* loop, accumulate hex value until find delimiter  */
    /* skip any initial delimiters found in read stream */

  while (!done)
  { int ch = Sgetc(fstream);

    if ( ch == EOF )
    { return -1;
    } else if ( ch == '\r' )
    { continue;
    } else
    { int dvalue = hexTable[ch];

      if ( dvalue >= 0 )
      { value = (value << 4) + dvalue;
	gotone++;
      } else if ((hexTable[ch]) == -1 && gotone)
	done++;
    }
  }

  return value;
}


static unsigned char *
read_x11_bitmap_file(IOSTREAM *fd, int *w, int *h)
{ unsigned char *data = NULL;
  char line[LINESIZE];
  int size = 0;
  char name_and_type[LINESIZE];		/* an input line */
  char *type;				/* for parsing */
  int value;				/* from an input line */
  int version10p;			/* bool, old format */
  int padding;				/* to handle alignment */
  int bytes_per_line;			/* per scanline of data */
  unsigned int ww = 0;			/* width */
  unsigned int hh = 0;			/* height */
  int hx = -1;				/* x hotspot */
  int hy = -1;				/* y hotspot */

  if (initialized == FALSE)
    initHexTable();

#define	RETURN_ERROR { if (data) XFree(data); return NULL; }

  while (Sfgets(line, LINESIZE, fd))
  { if ( sscanf(line,"#define %s %d",name_and_type,&value) == 2)
    { if (!(type = strrchr(name_and_type, '_')))
	type = name_and_type;
      else
	type++;

      if (!strcmp("width", type))
	ww = (unsigned int) value;
      if (!strcmp("height", type))
	hh = (unsigned int) value;
      if (!strcmp("hot", type))
      { if (type-- == name_and_type || type-- == name_and_type)
	  continue;
	if (!strcmp("x_hot", type))
	  hx = value;
	if (!strcmp("y_hot", type))
	  hy = value;
      }
      continue;
    }
    
    if (sscanf(line, "static short %s = {", name_and_type) == 1)
      version10p = 1;
    else if (sscanf(line,"static unsigned char %s = {",name_and_type) == 1)
      version10p = 0;
    else if (sscanf(line, "static char %s = {", name_and_type) == 1)
      version10p = 0;
    else
      continue;

    if (!(type = strrchr(name_and_type, '_')))
      type = name_and_type;
    else
      type++;

    if (strcmp("bits[]", type))
      continue;
    
    if (!ww || !hh)
      RETURN_ERROR;

    if ((ww % 16) && ((ww % 16) < 9) && version10p)
      padding = 1;
    else
      padding = 0;

    bytes_per_line = (ww+7)/8 + padding;

    size = bytes_per_line * hh;
    data = (unsigned char *) XMalloc(size);

    if (version10p)
    { unsigned char *ptr;
      int bytes;

      for (bytes=0, ptr=data; bytes<size; (bytes += 2))
      { if ((value = NextInt(fd)) < 0)
	  RETURN_ERROR;
	*(ptr++) = value;
	if (!padding || ((bytes+2) % bytes_per_line))
	  *(ptr++) = value >> 8;
      }
    } else
    { unsigned char *ptr;
      int bytes;
      
      for (bytes=0, ptr=data; bytes<size; bytes++, ptr++)
      { if ((value = NextInt(fd)) < 0) 
	  RETURN_ERROR;
	*ptr=value;
      }
    }
  }

  if (data == NULL) {
    RETURN_ERROR;
  }

  *w = ww;
  *h = hh;

  return data;
}


		/********************************
		*        SUN-ICON EDITOR	*
		********************************/

static unsigned char *
read_sun_icon_file(IOSTREAM *fd, int *width, int *height)
{ unsigned char *data, *dst;
  int c, x, y, w, h;
  int size;
  int skip_last;
  char hdr[256];

  if ( !Sfgets(hdr, sizeof(hdr), fd) ||
       sscanf(hdr, 
"/* Format_version=1, Width=%d, Height=%d, Depth=1, Valid_bits_per_item=16", 
	&w, &h) != 2 )
    return NULL;
  while( (c=Sgetc(fd)) != EOF && c != '/' ) /* skip to end of comment */
    ;

  if (initialized == FALSE)
    initHexTable();

  size = Round(w, 8) * h;
  dst = data = (unsigned char *) XMalloc(size);
  
  skip_last = (w % 16) <= 8 && (w % 16) > 0;

  for(y = 0; y < h; y++)
  { for(x = ((w+15)/16) - 1; x >= 0 ; x--)
    { int s;
      int d = 0;
      int n;
      
      s = NextInt(fd);

      for( n=0; n < 16 ; n++ )		/* revert all bits in the short */
        d |= ((s >> n) & 1) << (15-n);

      *dst++ = d & 0xff;
      if ( x != 0 || !skip_last )
	*dst++ = (d >> 8) & 0xff;
    }
  }

  *width = w;
  *height = h;
  return data;
}


#ifdef HAVE_LIBXPM
#include <sys/stat.h>

		 /*******************************
		 *          XPM SUPPORT		*
		 *******************************/

static void
setXpmAttributesImage(Image image, XImage *shape, XpmAttributes *atts)
{ if ( atts->valuemask & XpmHotspot )
    assign(image, hot_spot, newObject(ClassPoint,
				      toInt(atts->x_hotspot),
				      toInt(atts->y_hotspot), EAV));
  else
    assign(image, hot_spot, NIL);

  if ( shape )
  { assign(image, mask, newObject(ClassImage, NIL,
				  toInt(shape->width),
				  toInt(shape->height),
				  NAME_bitmap, EAV));
    setXImageImage(image->mask, shape);
  }
}


XImage *
attachXpmImageImage(Image image, XpmImage *xpm)
{ int as = XpmAttributesSize();
  XpmAttributes *atts = (XpmAttributes *)alloca(as);
  XImage *i = NULL;
  XImage *shape = NULL;
  Display *disp = defaultXDisplay();

  memset(atts, 0, as);
  atts->exactColors = FALSE;
  atts->closeness   = (1<<16)-1;	/* always continue */
  atts->valuemask   = XpmExactColors|XpmCloseness;

  if ( XpmCreateImageFromXpmImage(disp, xpm, &i,
				  &shape, atts) != XpmSuccess )
    return NULL;
  setXpmAttributesImage(image, shape, atts);

  return i;
}


static XImage *
readXpmFile(Image image, IOSTREAM *fd)
{ int offset = Stell(fd);
  char *buffer = NULL;			/* make compiler happy */
  XImage *i = NULL;
  XImage *shape = NULL;
  int malloced = FALSE;
  Display *disp = defaultXDisplay();

  if ( offset == 0 )			/* only entire file for now */
  { int size;

    if ( (size = Ssize(fd)) >= 0 )
    { int as = XpmAttributesSize();
      XpmAttributes *atts = (XpmAttributes *)alloca(as);

      memset(atts, 0, as);
    
      if ( size < 10000 )
      { buffer = (char *)alloca(size+1);
      } else
      { buffer = pceMalloc(size+1);
	malloced = TRUE;
      }

      if ( Sfread(buffer, sizeof(char), size, fd) != size )
	goto out;

      buffer[size] = '\0';

      atts->exactColors = FALSE;
      atts->closeness   = (1<<16)-1;	/* always continue */
      atts->valuemask   = XpmExactColors|XpmCloseness;
      
      if ( XpmCreateImageFromBuffer(disp, buffer,
				    &i, &shape, atts) != XpmSuccess )
	i = NULL;

      setXpmAttributesImage(image, shape, atts);
    }
  }

out:
  if ( malloced )
    pceFree(buffer);
  if ( !i )
    Sseek(fd, offset, 0);

  return i;
}

#ifdef O_GIFTOXPM

static XImage *
readGIFFile(Image image, IOSTREAM *fd)
{ XpmImage img;

  switch( XpmReadGIF(fd, &img) )
  { case GIF_OK:
    { XImage *i = attachXpmImageImage(image, &img);
      XpmFreeXpmImage(&img);

      return i;
    }
    case GIF_NOMEM:
    case GIF_INVALID:
    default:
      return NULL;
  }
}

#endif /*O_GIFTOXPM*/
#ifdef HAVE_LIBJPEG

static XImage *
readJPEGFile(Image image, IOSTREAM *fd)
{ XpmImage img;

  switch( readJPEGtoXpmImage(fd, &img, image) )
  { case GIF_OK:
    { XImage *i = attachXpmImageImage(image, &img);
      XpmFreeXpmImage(&img);

      return i;
    }
    case GIF_NOMEM:
    case GIF_INVALID:
    default:
      return NULL;
  }
}
#endif /*HAVE_LIBJPEG*/
#endif /*HAVE_LIBXPM*/

#define rescale(v) ((v)>>8)

#ifdef HAVE_LIBJPEG
#undef GLOBAL				/* conflict */
#undef HAVE_STDLIB_H			/* from jconfig.h */
#include <jpeglib.h>
#include <jerror.h>
#define XBRIGHT ((1L<<16)-1)

extern void jpeg_iostream_dest(j_compress_ptr cinfo, IOSTREAM *outfile);


		 /*******************************
		 *	   WRITING JPEG		*
		 *******************************/

#define RESCALE(v, o, n)	((v) * (n) / (o))

int
write_jpeg_file(IOSTREAM *fd,
		XImage *img, Display *disp, Colormap cmap,
		Image image)
{ int width  = img->width;
  int height = img->height;
  int depth  = img->depth;
  XColor cdata[256];
  XColor *colorinfo;
  int y;
  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr jerr;
  JSAMPLE *row;
  Any comment;

  if ( depth <= 8 )
  { int entries	= 1<<img->depth;
    int i;

    for(i=0; i<entries; i++)
      cdata[i].pixel = i;

    if ( !cmap )
      cmap = DefaultColormap(disp, DefaultScreen(disp));
    XQueryColors(disp, cmap, cdata, entries);
    colorinfo = cdata;
  } else 
  { colorinfo = NULL;
  }

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
  if ( image &&
       hasGetMethodObject(image, NAME_comment) &&
       (comment=get(image, NAME_comment, EAV)) )
  { if ( instanceOfObject(comment, ClassCharArray) )
    { CharArray v = comment;
      String s = &v->data;

      jpeg_write_marker(&cinfo, JPEG_COM, s->s_text8, s->size);
    } else if ( instanceOfObject(comment, ClassChain) )
    { Chain ch = comment;
      Cell cell;

      for_cell(cell, ch) 
      { if ( instanceOfObject(cell->value, ClassCharArray) )
	{ CharArray v = cell->value;
	  String s= &v->data;
	
	  jpeg_write_marker(&cinfo, JPEG_COM, s->s_text8, s->size);
	} else
	  errorPce(comment, NAME_unexpectedType, TypeCharArray);
      }
    } else
    { errorPce(comment, NAME_unexpectedType, CtoType("char_array|chain"));
    }
  }

  for(y=0; y<height; y++)
  { int x;
    JSAMPLE *s = row;

    if ( colorinfo )
    { for(x=0; x<width; x++)
      { XColor *c;
  
	c = &colorinfo[XGetPixel(img, x, y)];
	*s++ = rescale(c->red);
	*s++ = rescale(c->green);
	*s++ = rescale(c->blue);
      }
    } else				/* direct color displays  */
    { int r_shift = shift_for_mask(img->red_mask);
      int g_shift = shift_for_mask(img->green_mask);
      int b_shift = shift_for_mask(img->blue_mask);
      int r_bright = img->red_mask   >> r_shift;
      int g_bright = img->green_mask >> g_shift;
      int b_bright = img->blue_mask  >> b_shift;

      for(x=0; x<width; x++)
      { unsigned long pixel;
	int r, g, b;

	pixel = XGetPixel(img, x, y);
	r = (pixel & img->red_mask)   >> r_shift;
	g = (pixel & img->green_mask) >> g_shift;
	b = (pixel & img->blue_mask)  >> b_shift;

	*s++ = RESCALE(r, r_bright, 255);
	*s++ = RESCALE(g, g_bright, 255);
	*s++ = RESCALE(b, b_bright, 255);
      }
    }

    jpeg_write_scanlines(&cinfo, &row, 1);
  }

  jpeg_finish_compress(&cinfo);
  jpeg_destroy_compress(&cinfo);
  pceFree(row);

  return 0;
}


#endif /*HAVE_LIBJPEG*/


#ifdef O_GIFWRITE
#include <img/gifwrite.h>

		 /*******************************
		 *	     WRITE GIF		*
		 *******************************/

typedef unsigned char GSAMPLE;

int
write_gif_file(IOSTREAM *fd, XImage *img, XImage *msk,
	       Display *disp, Colormap cmap)
{ int width  = img->width;
  int height = img->height;
  int depth  = img->depth;
  XColor cdata[256];
  XColor *colorinfo;
  GSAMPLE *data, *s;
  GSAMPLE *maskdata;
  int bytes_per_line;			/* for the mask bits */
  int y;

  if ( depth <= 8 )
  { int entries	= 1<<img->depth;
    int i;

    for(i=0; i<entries; i++)
      cdata[i].pixel = i;

    if ( !cmap )
      cmap = DefaultColormap(disp, DefaultScreen(disp));
    XQueryColors(disp, cmap, cdata, entries);
    colorinfo = cdata;
  } else 
  { colorinfo = NULL;
  }

  data = pceMalloc(sizeof(GSAMPLE)*3*width*height);
  s = data;
  if ( msk )
  { bytes_per_line = (width+7)/8;
    maskdata = pceMalloc(sizeof(GSAMPLE)*bytes_per_line*height);
  } else
  { bytes_per_line = 0;			/* make compiler happy */
    maskdata = NULL;
  }

  for(y=0; y<height; y++)
  { int x;
    GSAMPLE *mrow;
    GSAMPLE bt = 0;
    GSAMPLE m = 0x80;

    mrow = maskdata+y*bytes_per_line;

    if ( colorinfo )
    { for(x=0; x<width; x++)
      { if ( !msk || XGetPixel(msk, x, y) )
	{ XColor *c;
  
	  c = &colorinfo[XGetPixel(img, x, y)];
	  *s++ = rescale(c->red);
	  *s++ = rescale(c->green);
	  *s++ = rescale(c->blue);
	} else
	{ bt |= m;
	  *s++ = 255;
	  *s++ = 255;
	  *s++ = 255;			/* transparent: pass as white */
	}

	if ( msk )
	{ m >>= 1;
	  if ( !m )
	  { *mrow++ = bt;
	    m = 0x80;
	    bt = 0;
	  }
	}
      }
    } else				/* Direct colour displays */
    { int r_shift = shift_for_mask(img->red_mask);
      int g_shift = shift_for_mask(img->green_mask);
      int b_shift = shift_for_mask(img->blue_mask);
      int r_bright = img->red_mask   >> r_shift;
      int g_bright = img->green_mask >> g_shift;
      int b_bright = img->blue_mask  >> b_shift;

      for(x=0; x<width; x++)
      { unsigned long pixel;
	int r, g, b;

	if ( !msk || XGetPixel(msk, x, y) )
	{ pixel = XGetPixel(img, x, y);
	  r = (pixel & img->red_mask)   >> r_shift;
	  g = (pixel & img->green_mask) >> g_shift;
	  b = (pixel & img->blue_mask)  >> b_shift;

	  *s++ = RESCALE(r, r_bright, 255);
	  *s++ = RESCALE(g, g_bright, 255);
	  *s++ = RESCALE(b, b_bright, 255);
	} else
	{ bt |= m;
	  *s++ = 255;
	  *s++ = 255;
	  *s++ = 255;			/* transparent: pass as white */
	}

	if ( msk )
	{ m >>= 1;
	  if ( !m )
	  { *mrow++ = bt;
	    m = 0x80;
	    bt = 0;
	  }
	}
      }
      if ( msk )
      { *mrow++ = bt;
      }
    }
  }

  gifwrite_rgb(fd, data, maskdata, width, height);
  pceFree(data);
  if ( maskdata )
    pceFree(maskdata);

  return 0;
}

#endif /*O_GIFWRITE*/



