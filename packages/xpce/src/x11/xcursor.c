/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include "include.h"

		 /*******************************
		 *	  THE CURSOR-FONT	*
		 *******************************/

static Sheet	cursorNames = NIL;

static struct standardCursor
{ char *name;				/* X name of the cursor */
  int	id;				/* X font id of the cursor */
} standard_cursors[] =
{ { "X_cursor",			0 },
  { "arrow",			2 },
  { "based_arrow_down",		4 },
  { "based_arrow_up",		6 },
  { "boat",			8 },
  { "bogosity",			10 },
  { "bottom_left_corner",	12 },
  { "bottom_right_corner",	14 },
  { "bottom_side",		16 },
  { "bottom_tee",		18 },
  { "box_spiral",		20 },
  { "center_ptr",		22 },
  { "circle",			24 },
  { "clock",			26 },
  { "coffee_mug",		28 },
  { "cross",			30 },
  { "cross_reverse",		32 },
  { "crosshair",		34 },
  { "diamond_cross",		36 },
  { "dot",			38 },
  { "dotbox",			40 },
  { "double_arrow",		42 },
  { "draft_large",		44 },
  { "draft_small",		46 },
  { "draped_box",		48 },
  { "exchange",			50 },
  { "fleur",			52 },
  { "gobbler",			54 },
  { "gumby",			56 },
  { "hand1",			58 },
  { "hand2",			60 },
  { "heart",			62 },
  { "icon",			64 },
  { "iron_cross",		66 },
  { "left_ptr",			68 },
  { "left_side",		70 },
  { "left_tee",			72 },
  { "leftbutton",		74 },
  { "ll_angle",			76 },
  { "lr_angle",			78 },
  { "man",			80 },
  { "middlebutton",		82 },
  { "mouse",			84 },
  { "pencil",			86 },
  { "pirate",			88 },
  { "plus",			90 },
  { "question_arrow",		92 },
  { "right_ptr",		94 },
  { "right_side",		96 },
  { "right_tee",		98 },
  { "rightbutton",		100 },
  { "rtl_logo",			102 },
  { "sailboat",			104 },
  { "sb_down_arrow",		106 },
  { "sb_h_double_arrow",	108 },
  { "sb_left_arrow",		110 },
  { "sb_right_arrow",		112 },
  { "sb_up_arrow",		114 },
  { "sb_v_double_arrow",	116 },
  { "shuttle",			118 },
  { "sizing",			120 },
  { "spider",			122 },
  { "spraycan",			124 },
  { "star",			126 },
  { "target",			128 },
  { "tcross",			130 },
  { "top_left_arrow",		132 },
  { "top_left_corner",		134 },
  { "top_right_corner",		136 },
  { "top_side",			138 },
  { "top_tee",			140 },
  { "trek",			142 },
  { "ul_angle",			144 },
  { "umbrella",			146 },
  { "ur_angle",			148 },
  { "watch",			150 },
  { "xterm",			152 },
  { NULL,			0 }
};


void
ws_init_cursor_font()
{ struct standardCursor *sc;

  cursorNames = globalObject(NAME_cursorNames, ClassSheet, 0);

  for(sc = standard_cursors; sc->name; sc++)
    valueSheet(cursorNames, (Any) CtoName(sc->name), toInt(sc->id));
}


Int
ws_cursor_font_index(Name name)
{ return getValueSheet(cursorNames, name);
}


		 /*******************************
		 *      CURSOR OPERATIONS	*
		 *******************************/

status
ws_create_cursor(CursorObj c, DisplayObj d)
{ Cursor xref = 0;
  DisplayWsXref r = d->ws_ref;

  if ( notNil(c->font_id) )
  { if ( isDefault(c->font_id) )
    { Int id;

      if ( !(id = ws_cursor_font_index(c->name)) )
	return errorPce(c, NAME_noNamedCursor, c->name);

      assign(c, font_id, id);
    }

    xref = XCreateFontCursor(r->display_xref, valInt(c->font_id));
  } else
  { Image is = getMonochromeImage(c->image);
    Image im = getMonochromeImage(c->mask);
    Pixmap source = (Pixmap) getXrefObject(is, d);
    Pixmap mask   = (Pixmap) getXrefObject(im, d);
    XColor *fg, *bg;

    fg = (XColor *) getXrefObject(isDefault(c->foreground) ? d->foreground
				  			   : c->foreground,
				  d);
    bg = (XColor *) getXrefObject(isDefault(c->background) ? d->background
				  			   : c->background,
				  d);

    xref = XCreatePixmapCursor(r->display_xref, source, mask, fg, bg,
			       valInt(c->hot_spot->x),
			       valInt(c->hot_spot->y));

    if ( is != c->image )
      freeObject(is);
    if ( im != c->mask )
      freeObject(im);
  }

  if ( !xref )
    return errorPce(c, NAME_xOpen, d);

  return registerXrefObject(c, d, (XtPointer) xref);
}


void
ws_destroy_cursor(CursorObj c, DisplayObj d)
{ Xref r;

  while( (r = unregisterXrefObject(c, d)) )
  { DisplayWsXref xr = r->display->ws_ref;
    XFreeCursor(xr->display_xref, (Cursor)r->xref);
  }
}
