/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialiseCircle(Circle c, Int w)
{ initialiseGraphical(c, ZERO, ZERO, w, w);
  assign(c, fill_pattern, NIL);

  succeed;
}


static status
RedrawAreaCircle(Circle c, Area a)
{ int x, y, w, h;

  initialiseDeviceGraphical(c, &x, &y, &w, &h);
  NormaliseArea(x, y, w, h);
  r_thickness(valInt(c->pen));
  r_dash(c->texture);
  r_ellipse(x, y, w, h, c->fill_pattern);

  return RedrawAreaGraphical(c, a);
}


static status
radiusCircle(Circle c, Int r)
{ Int d = mul(r, TWO);

  return setGraphical(c, DEFAULT, DEFAULT, d, d);
}


static Int
getRadiusCircle(Circle c)
{ answer(div(c->area->w,TWO));
}


static status
rotateCircle(Circle c)
{ succeed;
}


static status
diameterCircle(Circle c, Int n)
{ return setGraphical(c, DEFAULT, DEFAULT, n, n);
}


static Int
getDiameterCircle(Circle c)
{ answer(c->area->w);
}


static status
geometryCircle(Circle c, Int x, Int y, Int w, Int h)
{ Int d;

  if ( isDefault(w) )
    d = (isDefault(h) ? (Int) DEFAULT : h);
  else
    d = (isDefault(h) ? w : valInt(w) < valInt(h) ? w : h);

  return geometryGraphical(c, x, y, d, d);
}


extern drawPostScriptCircle(Circle c);

status
makeClassCircle(Class class)
{ sourceClass(class, makeClassCircle, __FILE__, "$Revision$");

  localClass(class, NAME_fillPattern, NAME_appearance,
	     "image|colour*", NAME_get,
	     "Fill pattern for internals");

  cloneStyleVariableClass(class, NAME_fillPattern, NAME_reference);

  termClass(class, "circle", 1, NAME_diameter);
  setRedrawFunctionClass(class, RedrawAreaCircle);

  storeMethod(class, NAME_fillPattern, fillPatternGraphical);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "diameter=[int]",
	     "Create circle from diameter",
	     initialiseCircle);
  sendMethod(class, NAME_geometry, NAME_area, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Force width and height to be equal",
	     geometryCircle);
  sendMethod(class, NAME_radius, NAME_area, 1, "int",
	     "Set radius (= half diameter)",
	     radiusCircle);
  sendMethod(class, NAME_rotate, NAME_rotate, 1, "int",
	     "Rotate (does nothing)",
	     rotateCircle);
  sendMethod(class, NAME_diameter, NAME_area, 1, "int",
	     "Set diameter",
	     diameterCircle);
  sendMethod(class, NAME_DrawPostScript, NAME_postscript, 0,
	     "Create PostScript",
	     drawPostScriptCircle);

  getMethod(class, NAME_radius, NAME_area, "int", 0,
	    "Radius (= half diameter",
	    getRadiusCircle);
  getMethod(class, NAME_diameter, NAME_area, "int", 0,
	    "Diameter (= twice radius)",
	    getDiameterCircle);

  attach_resource(class, "selection_style", "name", "side_handles",
		  "Visual feedback of <->selected");

  succeed;
}

