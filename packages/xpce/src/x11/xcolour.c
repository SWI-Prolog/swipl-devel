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

  if ( allocNearestColour(display, r->colour_map, r->depth, 
			  get(d, NAME_visualType, EAV),
			  &exact) )
  { XColor *color = alloc(sizeof(XColor));
      
    *color = exact;
    assign(c, red,   toInt(exact.red));
    assign(c, green, toInt(exact.green));
    assign(c, blue,  toInt(exact.blue));

    errorPce(c, NAME_replacedColour);

    return registerXrefObject(c, d, (XtPointer) color);
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
ws_pixel_to_colour(DisplayObj d, unsigned long pixel)
{ for_hash_table(ColourTable, s,
		 { Colour c = s->value;
		   XColor *color = (XColor *) getExistingXrefObject(c, d);

		   if ( color && color->pixel == pixel )
		     answer(c);
		 });

  fail;
}

		 /*******************************
		 *	     COLOURMAPS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ColourMap handling functions (TBD)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
ws_colour_cube(ColourMap cm, int size)
{
}


void
ws_colour_map_colours(ColourMap cm)
{
}


status
ws_create_colour_map(ColourMap cm, DisplayObj d)
{ fail;
}


status
ws_uncreate_colour_map(ColourMap cm, DisplayObj d)
{ fail;
}

