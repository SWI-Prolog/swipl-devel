/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include "include.h"

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

  if ( isDefault(c->red) || isDefault(c->green) || isDefault(c->blue) )
  { XColor exact, *screen = alloc(sizeof(XColor));

    if ( XAllocNamedColor(display, r->colour_map, x_colour_name(c->name),
			  screen, &exact) == 0 )
      return errorPce(c, NAME_xOpen, d);
      
    assign(c, red,   toInt(exact.red));
    assign(c, green, toInt(exact.green));
    assign(c, blue,  toInt(exact.blue));

    registerXrefObject(c, d, (XtPointer) screen);
  } else
  { XColor *color = alloc(sizeof(XColor));

    color->red   = valInt(c->red);
    color->green = valInt(c->green);
    color->blue  = valInt(c->blue);

    if ( XAllocColor(display, r->colour_map, color) == 0 )
      return errorPce(c, NAME_xOpen, d);

    registerXrefObject(c, d, (XtPointer) color);
  }

  succeed;
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
