/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
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


extern drawPostScriptEllipse(Ellipse e);

status
makeClassEllipse(Class class)
{ sourceClass(class, makeClassEllipse, __FILE__, "$Revision$");

  localClass(class, NAME_shadow, NAME_appearance, "int", NAME_get,
	     "Shadow painted below/right");
  localClass(class, NAME_fillPattern, NAME_appearance,
	     "image|colour*", NAME_get,
	     "Fill pattern for internals");

  termClass(class, "ellipse", 2, NAME_width, NAME_height);
  cloneStyleVariableClass(class, NAME_fillPattern, NAME_reference);
  setRedrawFunctionClass(class, RedrawAreaEllipse);

  storeMethod(class, NAME_fillPattern, fillPatternGraphical);
  storeMethod(class, NAME_shadow,      shadowGraphical);

  sendMethod(class, NAME_initialise, DEFAULT, 2, "width=[int]", "height=[int]",
	     "Create ellipse from width and height",
	     initialiseEllipse);
  sendMethod(class, NAME_DrawPostScript, NAME_postscript, 0,
	     "Create PostScript",
	     drawPostScriptEllipse);

  refine_resource(class, "selection_handles", "sides");

  succeed;
}

