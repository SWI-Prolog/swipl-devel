/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialiseBox(Box b, Int w, Int h)
{ initialiseGraphical(b, ZERO, ZERO, w, h);
  assign(b, radius,	  ZERO);
  assign(b, shadow,	  ZERO);
  assign(b, fill_pattern, NIL);

  succeed;
}


static status
RedrawAreaBox(Box b, Area a)
{ int x, y, w, h;

  initialiseDeviceGraphical(b, &x, &y, &w, &h);

  r_thickness(valInt(b->pen));
  r_dash(b->texture);
  r_shadow_box(x, y, w, h,
	       valInt(b->radius), valInt(b->shadow), b->fill_pattern);

  return RedrawAreaGraphical(b, a);
}


static status
radiusBox(Box b, Int r)
{ if (r != b->radius)
  { CHANGING_GRAPHICAL(b, assign(b, radius, r);
		          changedEntireImageGraphical(b));
  }

  succeed;
}


extern drawPostScriptBox(Box b);

status
makeClassBox(Class class)
{ sourceClass(class, makeClassBox, __FILE__, "$Revision$");

  localClass(class, NAME_radius, NAME_appearance, "int", NAME_get,
	     "Rounding radius for corners");
  localClass(class, NAME_shadow, NAME_appearance, "int", NAME_get,
	     "Shadow at bottom-right of box");
  localClass(class, NAME_fillPattern, NAME_appearance,
	     "image|colour*", NAME_get,
	     "Fill pattern for internals");

  termClass(class, "box", 2, NAME_width, NAME_height);
  cloneStyleVariableClass(class, NAME_fillPattern, NAME_reference);
  setRedrawFunctionClass(class, RedrawAreaBox);

  storeMethod(class, NAME_fillPattern, fillPatternGraphical);
  storeMethod(class, NAME_radius,      radiusBox);
  storeMethod(class, NAME_shadow,      shadowGraphical);

  sendMethod(class, NAME_initialise, DEFAULT, 2, "width=[int]", "height=[int]",
	     "Create box from width and height",
	     initialiseBox);
  sendMethod(class, NAME_DrawPostScript, NAME_postscript, 0,
	     "Create PostScript",
	     drawPostScriptBox);

  succeed;
}
