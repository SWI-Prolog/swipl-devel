/*  $Id$ $

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/text.h>

#define X_MARGIN    3			/* Space from the border */

forwards Fragment scan_fragment_icons(TextMargin, SendFunc, Name, Any);

static status
initialiseTextMargin(TextMargin m, Editor e, Int w, Int h)
{ initialiseGraphical(m, ZERO, ZERO, w, h);
  assign(m, editor, e);
  assign(m, background, getClassVariableValueObject(m, NAME_background));
  assign(m, gap, newObject(ClassSize, 0));
  copySize(m->gap, getClassVariableValueObject(m, NAME_gap));

  succeed;
}


static Style
fragment_style(TextMargin m, Fragment f)
{ Attribute a = getMemberSheet(m->editor->styles, (Any) f->style);

  return a == FAIL ? NIL : a->value;
}


		/********************************
		*            REDRAW		*
		********************************/

static int margin_x, margin_y;

static status
paint_fragment(TextMargin m, int x, int y, Fragment fragment)
{ Image icon;
  Style s;
  int w, h;

  if ( notNil(s = fragment_style(m, fragment)) && notNil(icon = s->icon) )
  { x += margin_x;
    y += margin_y;
    w = valInt(icon->size->w);
    h = valInt(icon->size->h);

    r_image(icon, 0, 0, x, y, w, h, ON);
    if ( m->editor->selected_fragment == fragment )
      r_complement(x, y, w, h);
  }

  succeed;
}


static status
RedrawAreaTextMargin(TextMargin m, Area a)
{ int x, y, w, h;
  Elevation z = getClassVariableValueObject(m, NAME_elevation);
  Any obg;

  initialiseDeviceGraphical(m, &x, &y, &w, &h);

  margin_x = x;
  margin_y = y;

  obg = r_background(m->background);
  r_clear(x, y, w, h);
  if ( z && notNil(z) )
    r_3d_box(x, y, w, h, 0, z, FALSE);
  else
  { r_thickness(valInt(m->pen));
    r_dash(m->texture);
    r_box(x, y, w, h, 0, NIL);
  }

  scan_fragment_icons(m, paint_fragment, NAME_forSome, NIL);
  RedrawAreaGraphical(m, a);
  r_background(obg);

  succeed;
}


		/********************************
		*           ATTRIBUTES		*
		********************************/

static status
gapTextMargin(TextMargin m, Size size)
{ return assignGraphical(m, NAME_gap, size);
}


static status
backgroundTextMargin(TextMargin m, Any bg)
{ return assignGraphical(m, NAME_background, bg);
}



		/********************************
		*        EVENT HANDLING		*
		********************************/

typedef struct
{ int	x;
  int   y;
} position;


static status
find_fragment(TextMargin m, int x, int y, Fragment fragment, position *pos)
{ Style s;
  Size sz;
  int ex, ey;

  if ( isNil(s = fragment_style(m, fragment)) || isNil(s->icon) )
    fail;

  ex = pos->x; ey = pos->y;
  sz = s->icon->size;
  if ( ex >= x && ey >= y &&
       ex <= x + valInt(sz->w) && ey <= y + valInt(sz->h) )
    succeed;

  fail;
}


static Fragment
getFragmentTextMargin(TextMargin m, EventObj ev)
{ position pos;
  Int ex, ey;
  
  get_xy_event(ev, m, ON, &ex, &ey);
  pos.x = valInt(ex);
  pos.y = valInt(ey);

  answer(scan_fragment_icons(m, find_fragment, NAME_find, &pos));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
scan_fragment_icons(m, func, how, context)

Scans   through  the  fragmentlist associated   with   'e' and   calls
(*func)(e, x,  y, fragment)  for  all  fragments  that start   at some
displayed line. It assumes e->lines to be initialised. x and y are the
x-y coordinate  at which the top-left corner   of  the icon associated
with 'fragment' should be.

if how  == NAME_forAll succeed if all  succeed, fail on first  failing
function. NAME_find  succeed after   first succeeded fails  otherwise.
NAME_forSome always succeeds;
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Fragment
scan_fragment_icons(TextMargin m, SendFunc func, Name how, Any ctx)
{ Editor e = m->editor;
  TextBuffer tb = e->text_buffer;
  TextImage  ti = e->image;
  Fragment fragment = tb->first_fragment;
  int x = X_MARGIN, y = -1000, h;
  int mw = valInt(m->area->w);
  int line = 0, lines = ti->map->length;
  int gw = valInt(m->gap->w);
  int gh = valInt(m->gap->h);
  Style s;
  int skip = ti->map->skip;
  
  for( h=0; notNil(fragment) && line < lines; line++ )
  { TextLine tl = &ti->map->lines[line + skip];

    DEBUG(NAME_fragment, Cprintf("Scanning line from %ld\n", tl->start));
    while( notNil(fragment) && fragment->start < tl->start )
      fragment = fragment->next;

    if ( y + h + gh < tl->y )		/* open the icon-line */
    { y = tl->y;
      x = X_MARGIN;
      h = 0;
    }
    DEBUG(NAME_fragment, Cprintf("tl->y = %d\n", tl->y));

    for( ; notNil(fragment) && fragment->start < tl->end
	 ; fragment = fragment->next )
    { Image icon;
    
      if ( notNil(s = fragment_style(m, fragment)) && notNil(icon = s->icon) )
      { int aw = valInt(icon->size->w);

	if ( (x + aw) > mw - X_MARGIN && aw <= mw -X_MARGIN)
        { y += h + gh;			/* does not fit: next line */
          x = X_MARGIN;
          h = 0;
	}
	if ( equalName(how, NAME_forAll) )
	{ if ( (*func)(m, x, y, fragment, ctx) == FAIL )
	    fail;
	} else if ( equalName(how, NAME_forSome) )
	{ (*func)(m, x, y, fragment, ctx);
	} else if ( equalName(how, NAME_find) )
	{ if ( (*func)(m, x, y, fragment, ctx) == SUCCEED )
	    return fragment;
	}
        x += valInt(icon->size->w) + gw;
        if ( valInt(icon->size->h) > h )
          h = valInt(icon->size->h);
      }
    }
  }

  if ( equalName(how, NAME_find) )
    fail;

  return (Fragment) SUCCEED;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "editor=editor", "width=int", "height=int" };

/* Instance Variables */

static vardecl var_textMargin[] =
{ IV(NAME_editor, "editor", IV_GET,
     NAME_storage, "Editor I'm part of"),
  SV(NAME_gap, "size", IV_GET|IV_STORE, gapTextMargin,
     NAME_layout, "Distance between icons in X and Y"),
  SV(NAME_background, "[colour|pixmap]", IV_GET|IV_STORE, backgroundTextMargin,
     NAME_appearance, "Background colour")
};

/* Send Methods */

static senddecl send_textMargin[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseTextMargin,
     DEFAULT, "Create from editor, width and height")
};

/* Get Methods */

static getdecl get_textMargin[] =
{ GM(NAME_fragment, 1, "fragment", "event", getFragmentTextMargin,
     NAME_fragment, "Find the fragment at the event-position")
};

/* Resources */

static classvardecl rc_textMargin[] =
{ RC(NAME_gap, "size", "size(5,2)",
     "Distance between icons in X and Y"),
  RC(NAME_placement, "{left,right}", "left",
     "Placement relative to the image"),
  RC(NAME_elevation, "elevation*", "@nil",
     "Elevation from the background"),
  RC(NAME_background, "[colour|pixmap]", "white",
     "Background colour for the text")
};

/* Class Declaration */

static Name textMargin_termnames[] = { NAME_editor, NAME_width, NAME_height };

ClassDecl(textMargin_decls,
          var_textMargin, send_textMargin, get_textMargin, rc_textMargin,
          3, textMargin_termnames,
          "$Rev$");



status
makeClassTextMargin(Class class)
{ declareClass(class, &textMargin_decls);
  setRedrawFunctionClass(class, RedrawAreaTextMargin);

  succeed;
}

