/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialiseEllipse(Ellipse e, Int w, Int h)
{ initialiseGraphical(e, ZERO, ZERO, w, h);
  assign(e, shadow, ZERO);
  assign(e, fill_pattern, NIL);

  succeed;
}


static status
RedrawAreaEllipse(Ellipse e, Area a)
{ int x, y, w, h;

  initialiseDeviceGraphical(e, &x, &y, &w, &h);
  NormaliseArea(x, y, w, h);
  r_thickness(valInt(e->pen));
  r_dash(e->texture);

  if ( e->shadow != ZERO )
  { int shadow = valInt(e->shadow);
    Image fill = e->fill_pattern;

    if ( shadow > w ) shadow = w;
    if ( shadow > h ) shadow = h;

    r_colour(BLACK_COLOUR);
    r_ellipse(x+shadow, y+shadow, w-shadow, h-shadow, BLACK_IMAGE);
    r_colour(DEFAULT);
    r_ellipse(x, y, w-shadow, h-shadow, isNil(fill) ? WHITE_IMAGE : fill);

  } else
    r_ellipse(x, y, w, h, e->fill_pattern);

  return RedrawAreaGraphical(e, a);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "width=[int]", "height=[int]" };

/* Instance Variables */

static vardecl var_ellipse[] =
{ SV(NAME_shadow, "int", IV_GET|IV_STORE, shadowGraphical,
     NAME_appearance, "Shadow painted below/right"),
  SV(NAME_fillPattern, "image|colour*", IV_GET|IV_STORE, fillPatternGraphical,
     NAME_appearance, "Fill pattern for internals")
};

/* Send Methods */

static senddecl send_ellipse[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseEllipse,
     DEFAULT, "Create ellipse from width and height"),
  SM(NAME_DrawPostScript, 0, NULL, drawPostScriptEllipse,
     NAME_postscript, "Create PostScript")
};

/* Get Methods */

#define get_ellipse NULL
/*
static getdecl get_ellipse[] =
{ 
};
*/

/* Resources */

static classvardecl rc_ellipse[] =
{ RC(NAME_selectionHandles, RC_REFINE, "sides",
     NULL)
};

/* Class Declaration */

static Name ellipse_termnames[] = { NAME_width, NAME_height };

ClassDecl(ellipse_decls,
          var_ellipse, send_ellipse, get_ellipse, rc_ellipse,
          2, ellipse_termnames,
          "$Rev$");


status
makeClassEllipse(Class class)
{ declareClass(class, &ellipse_decls);

  cloneStyleVariableClass(class, NAME_fillPattern, NAME_reference);
  setRedrawFunctionClass(class, RedrawAreaEllipse);

  succeed;
}

