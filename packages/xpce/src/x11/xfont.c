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

#ifdef USE_XFT

status
ws_create_font(FontObj f, DisplayObj d)
{ XpceFontInfo xref;
  DisplayWsXref r = d->ws_ref;
  XftFont xft;
  int screen = DefaultScreen(r->display_xref);

  if ( !instanceOfObject(f->x_name, ClassCharArray) )
    fail;
  
  xft = XftFontOpenXlfd(r->display_xref, screen, strName(f->x_name));
  if ( !xft )
    return replaceFont(f, d);

  xref = alloc(sizeof(*xref));
  xref->xft_font = xft;

  return registerXrefObject(f, d, xref);
}


void
ws_destroy_font(FontObj f, DisplayObj d)
{ XpceFontInfo xref = getExistingXrefObject(f, d);
  DisplayWsXref r = d->ws_ref;

  if ( xref )
  { /* TBD: cannot find the free function!? */

    unregisterXrefObject(f, d);
    unalloc(sizeof(*xref), xref);
  }  
}


status
ws_system_fonts(DisplayObj d)
{ succeed;
}

#else /*USE_XFT*/

#ifdef USE_XFONTSET

status
ws_create_font(FontObj f, DisplayObj d)
{ XpceFontInfo xref;
  DisplayWsXref r = d->ws_ref;
  XFontSet set;
  char **missing; int nmissing;
  char *def_string;

  if ( !instanceOfObject(f->x_name, ClassCharArray) )
    fail;
  
  set = XCreateFontSet(r->display_xref, strName(f->x_name),
		       &missing, &nmissing, &def_string);
  if ( !set )
    return replaceFont(f, d);

  xref = alloc(sizeof(*xref));
  xref->font_set   = set;
  xref->missing    = missing;
  xref->nmissing   = nmissing;
  xref->def_string = def_string;

  DEBUG(NAME_font,
	if ( nmissing > 0 )
	{ int i;
	  
	  Cprintf("Missing charsets for %s:\n", strName(f->x_name));
	  for(i=0; i<nmissing; i++)
	    Cprintf("\t%s\n", missing[i]);
	}
	if ( def_string && def_string[0] )
	Cprintf("Default string: \"%s\"\n", def_string));

  return registerXrefObject(f, d, xref);
}


void
ws_destroy_font(FontObj f, DisplayObj d)
{ XpceFontInfo xref = getExistingXrefObject(f, d);
  DisplayWsXref r = d->ws_ref;

  if ( xref )
  { XFreeFontSet(r->display_xref, xref->font_set);
    XFreeStringList(xref->missing);

    unregisterXrefObject(f, d);
    unalloc(sizeof(*xref), xref);
  }  
}


status
ws_system_fonts(DisplayObj d)
{ succeed;
}


#else /*NO XFT and NO XFONTSET*/

#define MAX_CHAR 256
#define BOUNDS(v, l, m) if ( (v) < (l) || (v) >= (m) ) ((v) = (l))

static cwidth *
make_font_char_widths(XFontStruct *info)
{ if ( info->per_char == NULL )
  { return NULL;
  } else if ( info->min_byte1 == 0 && info->max_byte1 == 0 ) /* 8-bit */
  { unsigned int i;
    unsigned int offset = info->min_char_or_byte2;
    cwidth *widths = alloc(MAX_CHAR * sizeof(cwidth));
    XCharStruct *defchar;
    unsigned int dchr = info->default_char;

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
    unsigned int x, y;
    XCharStruct *c = info->per_char;
    int chrs = 0;
    unsigned int def_b1 = info->default_char / 256;
    unsigned int def_b2 = info->default_char % 256;
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
  int iswide;

  if ( !instanceOfObject(f->x_name, ClassCharArray) )
    fail;

  if ( (info = XLoadQueryFont(r->display_xref, strName(f->x_name))) == NULL )
    return replaceFont(f, d);
  iswide = (info->min_byte1 != 0 || info->max_byte1 != 0);
  
  xref = alloc(sizeof(*xref));
  xref->info    = info;
  xref->widths  = make_font_char_widths(info);
  xref->maxchar = iswide ? 0xffff : 0xff;

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

  assign(f, iswide, iswide ? ON : OFF);

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


status
ws_system_fonts(DisplayObj d)
{ succeed;
}

#endif /*USE_XFONTSET*/
#endif /*USE_XFT*/
