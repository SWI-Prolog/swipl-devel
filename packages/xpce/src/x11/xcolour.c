/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <math.h>
#include "include.h"

#define XBRIGHT ((1<<16)-1)

static char *
x_colour_name(Name name)
{ static char buf[200];
  char *s, *q;

  for( s = strName(name), q = buf; *s; s++, q++ )
  { if ( *s == '_' || *s == syntax.word_separator )
      *q = ' ';
    else
      *q = tolower(*s);
  }
  *q = EOS;

  return buf;
}


int
intensityXColor(XColor *c)
{ unsigned int r = c->red / 4;			/* avoid overflow */
  unsigned int g = c->green / 4;
  unsigned int b = c->blue / 4;
  static double f;
  static int fdone = FALSE;
  int rval;
  
  if ( !fdone )
  { f = (double) XBRIGHT / sqrt((double) (3*(XBRIGHT/4)*(XBRIGHT/4)));
    fdone++;
  }

  rval = (int)(sqrt((double)(r*r + g*g + b*b)) * f);
  if ( rval > XBRIGHT )
    rval = XBRIGHT;

  return rval;
}


static int
distanceColours(Name vt, XColor *c1, XColor *c2)
{ if ( vt == NAME_greyScale )
  { int i1 = intensityXColor(c1);
    int i2 = intensityXColor(c2);

    return abs(i1 - i2);
  } else
  { int dr = ((int)c1->red - (int)c2->red) / 4;
    int dg = ((int)c1->green - (int)c2->green) / 4;
    int db = ((int)c1->blue - (int)c2->blue) / 4;

    return (int)sqrt((double)(dr*dr + dg*dg + db*db)) * 4;
  }
}


status
findNearestColour(Display *display, Colormap map, int depth, Name vt,
		  XColor *c)
{ XColor *colors;
  int entries = 1<<depth;

  if ( (colors = alloc(entries * sizeof(XColor))) )
  { int i;
      
    for(i=0; i<entries; i++)
      colors[i].pixel = i;

    DEBUG(NAME_colour, Cprintf("Looking for %d %d %d\n",
			       c->red, c->green, c->blue));

    if ( isDefault(vt) )		/* TBD */
    { Visual *v = XDefaultVisual(display, DefaultScreen(display));
      int vclass = v->class;

      switch(vclass)
      { case StaticGray: vt = NAME_staticGrey;
        case GrayScale:	 vt = NAME_greyScale;
      }
    }

    if ( XQueryColors(display, map, colors, entries) )
    { XColor *cb = NULL;
      int badness = 1000000;

      for(i=0; i<entries; i++)
      { XColor *e = &colors[i];
	int d = distanceColours(vt, c, e);

	DEBUG(NAME_colour, Cprintf("\t%d: %d %d %d (d=%d)\n",
				   i, e->red, e->green, e->blue, d));

	if ( d < badness )
	{ cb = e;
	  badness = d;
	}
      }

      assert(cb);
      
      DEBUG(NAME_colour, Cprintf("Mapped colour %d %d %d --> %d %d %d\n",
				 c->red, c->green, c->blue,
				 cb->red, cb->green, cb->blue));

      *c = *cb;
      unalloc(entries * sizeof(XColor), colors);
      succeed;
    }
  } 

  fail;
}


status
ws_create_colour(Colour c, DisplayObj d)
{ DisplayWsXref r = d->ws_ref;
  Display *display = r->display_xref;
  XColor exact;

  if ( isDefault(c->red) || isDefault(c->green) || isDefault(c->blue) )
  { XColor *screen = alloc(sizeof(XColor));

    if ( XAllocNamedColor(display, r->colour_map, x_colour_name(c->name),
			  screen, &exact) )
    { assign(c, red,   toInt(exact.red));
      assign(c, green, toInt(exact.green));
      assign(c, blue,  toInt(exact.blue));

      return registerXrefObject(c, d, (XtPointer) screen);
    } else
      XParseColor(display, r->colour_map, x_colour_name(c->name), &exact);
  } else
  { exact.red   = valInt(c->red);
    exact.green = valInt(c->green);
    exact.blue  = valInt(c->blue);

    if ( XAllocColor(display, r->colour_map, &exact) )
    { XColor *color = alloc(sizeof(XColor));
      
      *color = exact;
      return registerXrefObject(c, d, (XtPointer) color);
    }
  }

  if ( findNearestColour(r->display_xref, r->colour_map, r->depth, 
			 get(d, NAME_visualType, 0),
			 &exact) )
  { XColor *color = alloc(sizeof(XColor));
      
    *color = exact;
					/* can this go wrong? */
    if ( XAllocColor(display, r->colour_map, color) )
    { assign(c, red,   toInt(exact.red));
      assign(c, green, toInt(exact.green));
      assign(c, blue,  toInt(exact.blue));

      return registerXrefObject(c, d, (XtPointer) color);
    }
  }

  return errorPce(c, NAME_xOpen, d);
}


void
ws_uncreate_colour(Colour c, DisplayObj d)
{ Xref xr;

  while( (xr = unregisterXrefObject(c, d)) )
  { DisplayWsXref r = xr->display->ws_ref;
    XColor *xc = xr->xref;

    XFreeColors(r->display_xref, r->colour_map, &xc->pixel, 1, 0);
  }
}


status
ws_colour_name(DisplayObj d, Name name)
{ XColor edr, sdr;
  DisplayWsXref r;

  openDisplay(d);
  r = d->ws_ref;
  if ( XLookupColor(r->display_xref, r->colour_map, x_colour_name(name),
		    &edr, &sdr) )
    succeed;

  fail;
}


Colour
ws_pixel_to_colour(DisplayObj d, ulong pixel)
{ for_hash_table(ColourTable, s,
		 { Colour c = s->value;
		   XColor *color = (XColor *) getExistingXrefObject(c, d);

		   if ( color && color->pixel == pixel )
		     answer(c);
		 });

  fail;
}
