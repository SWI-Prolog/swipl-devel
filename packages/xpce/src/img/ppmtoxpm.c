/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The XPCE PNM interface. Converts between   X11 images and p[bgp]m images
for loading and saving XPCE  bitmaps  in   a  form  accessible for other
applications.

PNM (Portable aNy  Map)  is  a   public  simple  format  for  exchanging
bitmap/pixmap  information.  It  has  6  flavours,  three  of  them  are
`rawbits' (binary) versions of the  three   main  formats: PBM (Portable
BitMap), PGM (Portable GrayMap) and PPM (Portable PixMap).

The PNM package is public domain  and available as pbmplus10dec91.tar.gz
on many anonymous ftp servers. Search for   pbmplus is you want to check
for more recent releases.

The magnificient tool xv by John Bradley (bradley@cis.upenn.edu) version
3 can also generate ppm files.

This module implements two additional formats:   PGM  and PPM PNM_RUNLEN
encoded files. These are similar to the   RAWBIT files, but encode their
info as tuples <colour-info> <times-repeated>.  These files are normally
more compact and read/write faster. As   yet  this format is exclusively
used in saved object (object ->save_in_file).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <h/kernel.h>
#include "include.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#define BRIGHT ((1L<<16)-1)
#define NOPIXEL (~0L)

#undef roundup
#define valdigit(d)		((d) - '0')
#define roundup(v, n)		((((v)+(n)-1)/(n))*(n))
#define rescale(v, o, n)	((v) * (n) / (o))

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define Symbol PixSymbol		/* name-clash with kernel */
#define symbol pix_symbol

static int ncolours;			/* colours in image */
static int nmapped;			/* remapped colours */
static int nfailed;			/* failed conversions */

		 /*******************************
		 * HASH-TABLE FOR PIXEL LOOKUP	*
		 *******************************/

typedef struct symbol *Symbol;
typedef struct table  *Table;

struct symbol
{ unsigned long name;
  unsigned long value;
  Symbol	next;
};

struct table
{ int 		size;
  Symbol	symbols[1];
};


#define hashvalue(size, v) ((v) % size)

static Table
newTable(int size)
{ Table t = (Table)pceMalloc(sizeof(struct table) + (size-1) * sizeof(Symbol));
  int i;
  Symbol *s;

  t->size = size;
  for(i=size, s=t->symbols; --i >= 0; s++)
    *s = NULL;

  return t;
}


static void
freeTable(Table t)
{ Symbol *s;
  int i;

  for(i=t->size, s=t->symbols; --i >= 0; s++)
  { Symbol n, m = *s;

    for( ; m; m = n)
    { n = m->next;
      pceFree(m);
    }
  }

  pceFree(t);
}


static void
addTable(Table t, unsigned long name, unsigned long value)
{ Symbol *l = &t->symbols[hashvalue(t->size, name)];
  Symbol s = (Symbol)pceMalloc(sizeof(struct symbol));

  s->name = name;
  s->value = value;
  s->next = *l;
  *l = s;
}


static unsigned long
memberTable(Table t, unsigned long name)
{ Symbol s = t->symbols[hashvalue(t->size, name)];

  for(; s; s = s->next)
  { if ( s->name == name )
      return s->value;
  }

  return NOPIXEL;
}


		 /*******************************
		 *	   COLOUR PIXELS	*
		 *******************************/

static unsigned long
colourPixel(Display *disp, int depth, Colormap cmap,
	    Table t, int r, int g, int b)
{ unsigned long pixel;
  unsigned long direct = (r << 16) + (g << 8) + b;
  XColor c;

  if ( (pixel = memberTable(t, direct)) != NOPIXEL )
    return pixel;

  ncolours++;

  c.red   = r * 257;
  c.green = g * 257;
  c.blue  = b * 257;
  if ( !XAllocColor(disp, cmap, &c) )
  { if ( !allocNearestColour(disp, cmap, depth, DEFAULT, &c) )
    { Cprintf("PNM: failed to alloc pixel %d/%d/%d\n", r, g, b);
      c.pixel = 0;
      nfailed++;
    }
  }
  
  addTable(t, direct, c.pixel);

  DEBUG(NAME_ppm, Cprintf("PNM: Colour %d %d %d on pixel %d\n",
			  r, g, b, c.pixel));
  return c.pixel;
}

		 /*******************************
		 *     ASCII FORMAT PARSING	*
		 *******************************/

static int
getNum(FILE *fd)
{ int c;
  int v;

  for(;;)
  { do
    { c = getc(fd);
    } while(isspace(c));

    if ( isdigit(c) )
    { v = valdigit(c);
      for(;;)
      { c = getc(fd);
	if ( isdigit(c) )
	  v = v*10 + valdigit(c);
	else
	  break;
      }
      if ( !isspace(c) )
	ungetc(c, fd);

      return v;
    }
    if ( c == '#' )
    { do
      { c = getc(fd);
      } while( c != '\n' && c != EOF );
    } else
      return -1;
  }
}


XImage *
read_ppm_file(Display *disp, Colormap cmap, int depth, FILE *fd)
{ XImage *img;
  long here = ftell(fd);
  int c;
  int fmt, encoding;
  int width, height, bytes_per_line, scale=0;
  char *data;
  int pad = XBitmapPad(disp);
  Visual *v = DefaultVisual(disp, DefaultScreen(disp));

  ncolours = nmapped = nfailed = 0;	/* statistics */
  assert(pad%8 == 0);

  if ( (c=getc(fd)) != 'P' )
  { ungetc(c, fd);
    return NULL;
  }

  if ( !cmap )
    cmap = DefaultColormap(disp, DefaultScreen(disp));

  c = getc(fd);
  if ( c < '1' || c > '9' )
    goto errout;
  c -= '0';
  fmt      = ((c - 1) % 3) + 1;
  encoding = c - fmt;

  width = getNum(fd);
  height = getNum(fd);

  if ( fmt == PNM_PBM )
  { depth = 1;
  } else
  { scale = getNum(fd);
    if ( !depth )
      depth = DefaultDepth(disp, DefaultScreen(disp));
  }

  if ( width < 0 || height < 0 || scale < 0 )
    goto errout;

  bytes_per_line = roundup((width*depth+7)/8, pad/8);
  data = (char *)pceMalloc(height * bytes_per_line);

  img = XCreateImage(disp,
		     v,
		     depth,
		     fmt == PNM_PBM ? XYBitmap : ZPixmap,
		     0,
		     data,
		     width, height,
		     pad, bytes_per_line);
  img->bits_per_pixel = depth;

  switch(encoding)
  { int x, y;
    
    case PNM_ASCII:
    { switch(fmt)
      { case PNM_PBM:
	  for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { int value = getNum(fd);
  
	      if ( value < 0 || value > 1 )
		goto errout;
  
	      XPutPixel(img, x, y, value);
	    }
	  }
	  break;
	case PNM_PGM:
	{ Table t = newTable(64);
  
	  for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { int g = getNum(fd);
	      unsigned long pixel;
  
	      if ( g < 0 || g > scale )
		goto errout;
	      if ( scale != 255 )
		g = rescale(g, scale, 255);
  
	      pixel = colourPixel(disp, depth, cmap, t, g, g, g);
	      XPutPixel(img, x, y, pixel);
	    }
	  }
	  freeTable(t);
	      
	  break;
	}
	case PNM_PPM:
	{ Table t = newTable(64);
  
	  for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { int r = getNum(fd);
	      int g = getNum(fd);
	      int b = getNum(fd);
	      unsigned long pixel;
  
	      if ( r < 0 || r > scale ||
		   g < 0 || g > scale ||
		   b < 0 || b > scale )
		goto errout;
  
	      if ( scale != 255 )
	      { r = rescale(r, scale, 255);
		g = rescale(g, scale, 255);
		b = rescale(b, scale, 255);
	      }
  
	      pixel = colourPixel(disp, depth, cmap, t, r, g, b);
  
	      XPutPixel(img, x, y, pixel);
	    }
	  }
	  freeTable(t);
  
	  break;
	}
	break;
      }
      break;
    }
    case PNM_RAWBITS:
    { switch(fmt)
      { case PNM_PBM:
	{ int byte = 0;
	  int bit = 0;
	
	  for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { if ( !bit )
	      { byte = getc(fd);
		bit = 8;
	      }
  
	      bit--;
	      XPutPixel(img, x, y, (byte & (1<<bit)) ? 1 : 0);
	    }
	    bit = 0;			/* scanlines are byte-aligned */
	  }
	  break;
	}
	case PNM_PGM:
	{ Table t = newTable(64);
  
	  for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { unsigned int g;
	      unsigned long pixel;
  
	      if ( feof(fd) || (g=getc(fd)) > scale )
		goto errout;
	      if ( scale != 255 )
		g = rescale(g, scale, 255);
  
	      pixel = colourPixel(disp, depth, cmap, t, g, g, g);
	      XPutPixel(img, x, y, pixel);
	    }
	  }
	  freeTable(t);
	      
	  break;
	}
	case PNM_PPM:
	{ Table t = newTable(64);
  
	  for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { unsigned int r, g, b;
	      unsigned long pixel;
  
	      if ( feof(fd) ||
		   (r=getc(fd)) > scale ||
		   (g=getc(fd)) > scale ||
		   (b=getc(fd)) > scale )
		goto errout;
  
	      if ( scale != 255 )
	      { r = rescale(r, scale, 255);
		g = rescale(g, scale, 255);
		b = rescale(b, scale, 255);
	      }
  
	      pixel = colourPixel(disp, depth, cmap, t, r, g, b);
  
	      XPutPixel(img, x, y, pixel);
	    }
	  }
	  freeTable(t);
  
	  break;
	}
	break;
      }
      break;
    }
    case PNM_RUNLEN:
    { int rlen = 0;
      unsigned long cpixel = NOPIXEL;

      switch(fmt)
      { case PNM_PGM:
	{ Table t = newTable(64);
  
	  DEBUG(NAME_pnm, Cprintf("Reading runlength encoded graymap\n"));

	  for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { if ( rlen-- > 0 )
	      { XPutPixel(img, x, y, cpixel);
	      } else
	      { unsigned int g;
  
		if ( (g=getc(fd)) > scale ||
		     (rlen = getc(fd)) == EOF )
		  goto errout;
		rlen &= 0xff;
		if ( scale != 255 )
		  g = rescale(g, scale, 255);
  
		cpixel = colourPixel(disp, depth, cmap, t, g, g, g);
		XPutPixel(img, x, y, cpixel);
		rlen--;
	      }
	    }
	  }
	  freeTable(t);
	      
	  break;
	}
	case PNM_PPM:
	{ Table t = newTable(64);
  
	  for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { if ( rlen-- > 0 )
	      { XPutPixel(img, x, y, cpixel);
	      } else
	      { unsigned int r, g, b;
  
		if ( (r=getc(fd)) > scale ||
		     (g=getc(fd)) > scale ||
		     (b=getc(fd)) > scale ||
		     (rlen = getc(fd)) == EOF )
		  goto errout;

		rlen &= 0xff;
		if ( scale != 255 )
		{ r = rescale(r, scale, 255);
		  g = rescale(g, scale, 255);
		  b = rescale(b, scale, 255);
		}
  
		cpixel = colourPixel(disp, depth, cmap, t, r, g, b);
  
		XPutPixel(img, x, y, cpixel);
		rlen--;
	      }
	    }
	  }
	  freeTable(t);
  
	  break;
	}
      }
    }
  }

  DEBUG(NAME_ppm,
	Cprintf("PNM: Converted %dx%dx%d image, %d colours (%d mapped, %d failed)\n",
		width, height, depth, ncolours, nmapped, nfailed));

  return img;

errout:
  Cprintf("PNM: Format error, index = %d\n", ftell(fd));
  fseek(fd, here, SEEK_SET);
  return NULL;
}

		 /*******************************
		 *	     WRITING		*
		 *******************************/


static int file_col;

static int
putNum(int n, FILE *fd)
{ if ( file_col != 0 && putc(' ', fd) == EOF )
    return -1;

  do
  { if ( putc(n % 10 + '0', fd) == EOF )
	return -1;
    file_col++;
    n /= 10;
  } while( n > 0 );

  if ( file_col >= 70 )
  { if ( putc('\n', fd) == EOF )
      return -1;
    file_col = 0;
  }

  return 0;
}


int
write_pnm_file(FILE *fd, XImage *img,
	       Display *disp, Colormap cmap, int scale, int fmt, int encode)
{ int width  = img->width;
  int height = img->height;
  int depth  = img->depth;
  int colours;
  XColor **cinfo = NULL;
  int x, y;

  if ( !cmap )
    cmap = DefaultColormap(disp, DefaultScreen(disp));
  if ( !scale )
    scale = 255;
  if ( !fmt && img->format == XYBitmap )
    fmt = PNM_PBM;

  if ( fmt != PNM_PBM )
  { if ( depth > 16 )
    { Cprintf("PPM/PGM generation not yet supported for depth > 16\n");
      return -1;
    }
    if ( !(cinfo = makeSparceCInfo(disp, cmap, img, &colours)) )
      return -1;
    if ( !fmt )
    { int i, entries = 1<<depth;

      for(i=0; i<entries; i++)
      { if ( cinfo[i] )
	{ XColor *c = cinfo[i];
	  
	  if ( c->red != c->green || c->red != c->blue )
	  { fmt = PNM_PPM;
	    break;
	  }
	}
      }
      if ( !fmt )
	fmt = PNM_PGM;
    }
  } else if ( encode == PNM_RUNLEN )	/* no use to runlen encode a bitmap */
    encode = PNM_RAWBITS;

  fprintf(fd, "P%c\n", fmt + encode + '0');
  fprintf(fd, "# Creator: XPCE version %s\n", strName(get(PCE,NAME_version,0)));
  if ( fmt != PNM_PBM )
  { fprintf(fd, "# %d colours found\n", colours);
    fprintf(fd, "%d %d\n", width, height);
    fprintf(fd, "%d\n", scale);
  } else
    fprintf(fd, "%d %d\n", width, height);

  file_col = 0;
    
  switch(encode)
  { case PNM_ASCII:
    { switch(fmt)
      { case PNM_PBM:
	{ for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { if ( putNum(XGetPixel(img, x, y) ? 1 : 0, fd) < 0 )
		return -1;
	    }
	  }
	  break;
	}
	case PNM_PGM:
	{ greySparceCInfo(cinfo, depth);
  
	  for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { XColor *c;
	      unsigned int r;
  
	      c = cinfo[XGetPixel(img, x, y)];
	      r = rescale(c->red, BRIGHT, scale);
  
	      if ( putNum(r, fd) < 0 )
		return -1;
	    }
	  }
	  break;
	}
	case PNM_PPM:
	{ for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { XColor *c;
	      unsigned int r, g, b;
  
	      c = cinfo[XGetPixel(img, x, y)];
	      r = rescale(c->red,   BRIGHT, scale);
	      g = rescale(c->green, BRIGHT, scale);
	      b = rescale(c->blue,  BRIGHT, scale);
  
	      if ( putNum(r, fd) < 0 ||
		   putNum(g, fd) < 0 ||
		   putNum(b, fd) < 0 )
		return -1;
	    }
	  }
	  break;
	}
      }
      if ( file_col && putc('\n', fd) == EOF )
	return -1;
      file_col = 0;
    }
    case PNM_RAWBITS:
    { switch(fmt)
      { case PNM_PBM:
	{ int byte = 0;
	  int bit = 7;
  
	  for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { if ( XGetPixel(img, x, y) )
		byte |= 1<<bit;
	      if ( bit-- == 0 )
	      { if ( putc(byte, fd) == EOF )
		  return -1;
		bit = 7;
		byte = 0;
	      }
	    }
	    if ( bit != 7 )		/* flush after finishing scanline */
	    { if ( putc(byte, fd) == EOF )
		return -1;
	      bit = 7;
	      byte = 0;
	    }
	  }
  
	  if ( bit != 7 )
	  { if ( putc(byte, fd) == EOF )
	      return -1;
	  }
	  break;
	}
	case PNM_PGM:
	{ greySparceCInfo(cinfo, depth);
  
	  for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { XColor *c;
	      unsigned int r;
  
	      c = cinfo[XGetPixel(img, x, y)];
	      r = rescale(c->red, BRIGHT, scale);
  
	      if ( putc(r, fd) == EOF )
		return -1;
	    }
	  }
	  break;
	}
	case PNM_PPM:
	{ for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { XColor *c;
	      unsigned int r, g, b;
  
	      c = cinfo[XGetPixel(img, x, y)];
	      r = rescale(c->red,   BRIGHT, scale);
	      g = rescale(c->green, BRIGHT, scale);
	      b = rescale(c->blue,  BRIGHT, scale);
  
	      if ( putc(r, fd) == EOF ||
		   putc(g, fd) == EOF ||
		   putc(b, fd) == EOF )
		return -1;
	    }
	  }
  
	  break;
	}
      }
    }
    case PNM_RUNLEN:
    { int rlen=-1;
      unsigned long cpixel = NOPIXEL;

      switch(fmt)
      { case PNM_PGM:
	{ greySparceCInfo(cinfo, depth);
  
	  for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { unsigned long pixel = XGetPixel(img, x, y);

	      if ( pixel == cpixel && rlen < 255 )
		rlen++;
	      else
	      { XColor *c;
		int r;

		if ( rlen > 0 && putc(rlen, fd) == EOF )
		  return -1;
		cpixel = pixel;
		rlen = 1;
		c = cinfo[pixel];
		r = rescale(c->red, BRIGHT, scale);
  		if ( putc(r, fd) == EOF )
		  return -1;
	      }
	    }
	  }
	  if ( putc(rlen, fd) == EOF )
	    return -1;

	  break;
	}
	case PNM_PPM:
	{ for(y=0; y<height; y++)
	  { for(x=0; x<width; x++)
	    { unsigned long pixel = XGetPixel(img, x, y);

	      if ( pixel == cpixel && rlen < 255 )
		rlen++;
	      else
	      { XColor *c;
		unsigned int r, g, b;
  
		if ( rlen > 0 && putc(rlen, fd) == EOF )
		  return -1;
		cpixel = pixel;
		rlen = 1;
		c = cinfo[pixel];
		r = rescale(c->red,   BRIGHT, scale);
		g = rescale(c->green, BRIGHT, scale);
		b = rescale(c->blue,  BRIGHT, scale);
  
		if ( putc(r, fd) == EOF ||
		     putc(g, fd) == EOF ||
		     putc(b, fd) == EOF )
		  return -1;
	      }
	    }
	  }
	  if ( putc(rlen, fd) == EOF )
	    return -1;
  
	  break;
	}
      }
    }
  }

  if ( cinfo )
    freeSparceCInfo(cinfo, img->depth);

  return 0;
}
