/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include "include.h"

#define MAX_CHAR 256
#define BOUNDS(v, l, m) if ( (v) < (l) || (v) >= (m) ) ((v) = (l))

static cwidth *
make_font_char_widths(XFontStruct *info)
{ if ( info->per_char == NULL )
  { return NULL;
  } else if ( info->min_byte1 == 0 && info->max_byte1 == 0 ) /* 8-bit */
  { int i;
    int offset = info->min_char_or_byte2;
    cwidth *widths = alloc(MAX_CHAR * sizeof(cwidth));
    XCharStruct *defchar;
    int dchr = info->default_char;

    BOUNDS(dchr, offset, info->max_char_or_byte2);
    defchar = &info->per_char[dchr - offset];

    for(i=0; i<offset; i++)
      widths[i] = defchar->width;
    for(; i <= info->max_char_or_byte2; i++)
      if ( (widths[i] = info->per_char[i-offset].width) == 0 )
	widths[i] = defchar->width;
    for(; i < MAX_CHAR; i++)
      widths[i] = defchar->width;

    return widths;
  } else				/* 16-bit font */
  { /*int rows = info->max_byte1 - info->min_byte1 + 1;*/
    int cols = info->max_char_or_byte2 - info->min_char_or_byte2 + 1;
    cwidth *widths = alloc(MAX_CHAR * sizeof(cwidth) * MAX_CHAR);
    cwidth *cw = widths;
    int x, y;
    XCharStruct *c = info->per_char;
    int chrs = 0;
    int def_b1 = info->default_char / 256;
    int def_b2 = info->default_char % 256;
    XCharStruct *defchar;

    BOUNDS(def_b1, info->min_byte1, info->max_byte1);
    BOUNDS(def_b2, info->min_char_or_byte2, info->max_char_or_byte2);

    defchar = &info->per_char[((def_b1 - info->min_byte1) * cols) +
			      def_b2 - info->min_char_or_byte2];

    for(y=0; y<info->min_byte1; y++)
      for(x=0; x < MAX_CHAR; x++)
	*cw++ = defchar->width;

    for(y=info->min_byte1; y <= info->max_byte1; y++)
    { for(x=0; x<info->min_char_or_byte2; x++)
	*cw++ = defchar->width;
      for( ; x<=info->max_char_or_byte2; x++ )
      { if ( (*cw = c->width) == 0 )
	  *cw = defchar->width;
	else
	  chrs++;
	cw++, c++;
      }
      for(; x < MAX_CHAR; x++)
	*cw++ = defchar->width;
    }

    for( ; y<MAX_CHAR; y++)
      for(x=0; x < MAX_CHAR; x++)
	*cw++ = defchar->width;

    DEBUG(NAME_font, Cprintf("16-bit font rows %d to %d, %d characters\n",
			     info->min_byte1, info->max_byte1, chrs));

    return widths;
  }
}


status
ws_create_font(FontObj f, DisplayObj d)
{ XFontStruct *info;
  XpceFontInfo xref;
  DisplayWsXref r = d->ws_ref;

  if ( (info = XLoadQueryFont(r->display_xref, strName(f->x_name))) == NULL )
    return replaceFont(f, d);

  xref = alloc(sizeof(struct xpce_font_info));
  xref->info = info;
  xref->widths = make_font_char_widths(info);

  if ( info->per_char != NULL )
  { int oi = 'i' - info->min_char_or_byte2;
    int ow = 'w' - info->min_char_or_byte2;

    if ( oi >= 0 && ow >= 0 &&
	 info->per_char[oi].width != info->per_char[ow].width  )
      assign(f, fixed_width, OFF);
    else
      assign(f, fixed_width, ON);
  } else
    assign(f, fixed_width, ON);

					/* 16-bit: use max-bounds */
  if ( info->min_byte1 != 0 || info->max_byte1 != 0 )
  { assign(f, ex, toInt(info->max_bounds.width));
    assign(f, b16, ON);
  } else
    assign(f, b16, OFF);

  return registerXrefObject(f, d, xref);
}


void
ws_destroy_font(FontObj f, DisplayObj d)
{ XFontStruct *xref = (XFontStruct *) getExistingXrefObject(f, d);
  DisplayWsXref r = d->ws_ref;

  if ( xref )
  { XFreeFont(r->display_xref, xref);
    unregisterXrefObject(f, d);
  }
}
