/*  $Id$ $

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include "math.h"

static status	drawArrow(int x1, int y1, int x2, int y2, int x3, int y3,
			  Image fill, int pen, Name texture, Name style);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Arrows are defined in terms of the point at the tip the length of the arrow 
head and the width of the two wings.  The direction of the arrow is 
determined by a reference point on an imaginery line through the reference 
point and the tip:


	         -     |\
	         |     |  \
	         |     |    \
	wing >=  |     |      \    <= tip (point)
	         |     |      /
	         |     |    /
	         |     |  /
	         -     |/

	               |------|    <= length

Although arrows are a sub-class of graphical this feature (in particular the 
area of the graphical) should not be used.  Changing the area of an arrow 
has no well-defined meaning.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
initialiseArrow(Arrow a, Int length, Int wing)
{ initialiseGraphical(a, ZERO, ZERO, ONE, ONE);
  assign(a, length, length);
  assign(a, wing, wing);
  assign(a, tip, newObject(ClassPoint, toInt(10), toInt(10), 0));
  assign(a, reference, newObject(ClassPoint, 0));
  assign(a, left, newObject(ClassPoint, 0));
  assign(a, right, newObject(ClassPoint, 0));
  assign(a, fill_pattern, DEFAULT);
  assign(a, style, DEFAULT);
  obtainResourcesObject(a);

  requestComputeGraphical(a, DEFAULT);

  succeed;
}


status
computeArrow(Arrow a)
{ if ( notNil(a->request_compute) )
  { int x1, y1, x2, y2;
    int x, y, w, h;
    int sx, sy, rx, ry;
    int xdiff, ydiff;
    int cdl1, sdl1, cl2, sl2;
    float l1, l2, d;
    float sin_theta, cos_theta;

    x1 = valInt(a->reference->x);
    y1 = valInt(a->reference->y);
    x2 = valInt(a->tip->x);
    y2 = valInt(a->tip->y);

    l1 = (float) (valInt(a->length));
    l2 = (float) (valInt(a->wing))/2.0;

    xdiff = x2 - x1;
    ydiff = y2 - y1;

    d = sqrt((float) (xdiff*xdiff + ydiff*ydiff));
    if (d < 0.0000001)
    { cos_theta = 1.0;
      sin_theta = 0.0;
    } else
    { cos_theta = (float) xdiff / d;
      sin_theta = (float) ydiff / d;
    }

    cdl1 = rfloat(cos_theta * (d-l1));
    sdl1 = rfloat(sin_theta * (d-l1));
    cl2 = rfloat(cos_theta * l2);
    sl2 = rfloat(sin_theta * l2);

    sx = x1 + cdl1 - sl2;
    sy = y1 + sdl1 + cl2;
    rx = x1 + cdl1 + sl2;
    ry = y1 + sdl1 - cl2;

    assign(a->left, x, toInt(sx));
    assign(a->left, y, toInt(sy));
    assign(a->right, x, toInt(rx));
    assign(a->right, y, toInt(ry));

    x = min(x2, min(sx, rx));
    y = min(y2, min(sy, ry));
    w = max(x2, max(sx, rx)) - x + 1;
    h = max(y2, max(sy, ry)) - y + 1;

    if ( notNil(a->device) )
      setGraphical(a, toInt(x), toInt(y), toInt(w), toInt(h));
    else
      setArea(a->area, toInt(x), toInt(y), toInt(w), toInt(h));

    assign(a, request_compute, NIL);
  }

  succeed;
}


static status
RedrawAreaArrow(Arrow a, Area area)
{ int x, y;
  int x1, y1, x2, y2, x3, y3;

  offsetDeviceGraphical(a, &x, &y);

  x1 = valInt(a->left->x) + x;
  y1 = valInt(a->left->y) + y;
  x2 = valInt(a->tip->x) + x;
  y2 = valInt(a->tip->y) + y;
  x3 = valInt(a->right->x) + x;
  y3 = valInt(a->right->y) + y;

  drawArrow(x1, y1, x2, y2, x3, y3, a->fill_pattern, valInt(a->pen), 
	    a->texture, a->style);

  return RedrawAreaGraphical(a, area);
}


static status
drawArrow(int x1, int y1, int x2, int y2, int x3, int y3,
	  Image fill, int pen, Name texture, Name style)
{ struct ipoint pts[3];

  if ( notNil(fill) )
  { pts[0].x = x1;
    pts[0].y = y1;
    pts[1].x = x2;
    pts[1].y = y2;
    pts[2].x = x3;
    pts[2].y = y3;
  
    r_fillpattern(fill);
    r_fill_polygon(pts, 3);
  } else
  { r_dash(texture);
    r_thickness(pen);
    r_line(x1, y1, x2, y2);
    r_line(x2, y2, x3, y3);
    if ( style == NAME_closed )
      r_line(x3, y3, x1, y1);
  }

  succeed;
}


static status
tipArrow(Arrow a, Point p)
{ return pointsArrow(a, p->x, p->y, DEFAULT, DEFAULT);
}

static status
tipXArrow(Arrow a, Int x)
{ return pointsArrow(a, x, DEFAULT, DEFAULT, DEFAULT);
}

static status
tipYArrow(Arrow a, Int y)
{ return pointsArrow(a, DEFAULT, y, DEFAULT, DEFAULT);
}


static status
referenceArrow(Arrow a, Point p)
{ return pointsArrow(a, DEFAULT, DEFAULT, p->x, p->y);
}

static status
referenceXArrow(Arrow a, Int x)
{ return pointsArrow(a, DEFAULT, DEFAULT, x, DEFAULT);
}

static status
referenceYArrow(Arrow a, Int y)
{ return pointsArrow(a, DEFAULT, DEFAULT, DEFAULT, y);
}


static Int
getTipXArrow(Arrow a)
{ answer(a->tip->x);
}

static Int
getTipYArrow(Arrow a)
{ answer(a->tip->y);
}

static Int
getReferenceXArrow(Arrow a)
{ answer(a->reference->x);
}


static Int
getReferenceYArrow(Arrow a)
{ answer(a->reference->y); 
}


static status
styleArrow(Arrow a, Name style)
{ if ( a->style != style )
  { CHANGING_GRAPHICAL(a,
	assign(a, style, style);
	changedEntireImageGraphical(a));
  }
  succeed;
}


static status
lengthArrow(Arrow a, Int l)
{ if ( a->length != l )
  { CHANGING_GRAPHICAL(a,
	assign(a, length, l);
	computeArrow(a);
	changedEntireImageGraphical(a));
  }
  succeed;
}


static status
wingArrow(Arrow a, Int w)
{ if (a->wing != w)
  { CHANGING_GRAPHICAL(a,
	assign(a, wing, w);
	computeArrow(a);
	changedEntireImageGraphical(a));
  }
  succeed;
}


status
pointsArrow(Arrow a, Int tx, Int ty, Int rx, Int ry)
{ Point tip = a->tip;
  Point ref = a->reference;
  
  if (isDefault(tx)) tx = tip->x;
  if (isDefault(ty)) ty = tip->y;
  if (isDefault(rx)) rx = ref->x;
  if (isDefault(ry)) ry = ref->y;

  if (tx != tip->x || ty != tip->y || rx != ref->x || ry != ref->y)
  { assign(tip, x, tx);
    assign(tip, y, ty);
    assign(ref, x, rx);
    assign(ref, y, ry);
    requestComputeGraphical(a, DEFAULT);
  }
  
  succeed;
}


status
paintArrow(Arrow a, Int tx, Int ty, Int rx, Int ry)
{ pointsArrow(a, tx, ty, rx, ry);
  computeArrow(a);

  drawArrow(valInt(a->left->x), valInt(a->left->y),
	    valInt(a->tip->x), valInt(a->tip->y),
	    valInt(a->right->x), valInt(a->right->y),
	    a->fill_pattern, valInt(a->pen), a->texture,
	    a->style);

  succeed;
}

extern drawPostScriptArrow(Arrow a);

status
makeClassArrow(Class class)
{ sourceClass(class, makeClassArrow, __FILE__, "$Revision$");

  localClass(class, NAME_tip, NAME_area, "point", NAME_get,
	     "Tip of the arrow");
  localClass(class, NAME_reference, NAME_area, "point", NAME_get,
	     "Where arrow points to");
  localClass(class, NAME_length, NAME_area, "int", NAME_get,
	     "Distance tip to base");
  localClass(class, NAME_wing, NAME_area, "int", NAME_get,
	     "Length of base");
  localClass(class, NAME_fillPattern, NAME_appearance,
	     "image|colour*", NAME_get,
	     "How it is filled");
  localClass(class, NAME_style, NAME_appearance, "{open,closed}", NAME_get,
	     "If `closed', the triangle is closed");
  localClass(class, NAME_left, NAME_internal, "point", NAME_none,
	     "Left-end of base");
  localClass(class, NAME_right, NAME_internal, "point", NAME_none,
	     "Right-end of base");

  cloneStyleVariableClass(class, NAME_fillPattern, NAME_reference);
  termClass(class, "arrow", 2, NAME_length, NAME_wing);
  setRedrawFunctionClass(class, RedrawAreaArrow);

  storeMethod(class, NAME_fillPattern, fillPatternGraphical);
  storeMethod(class, NAME_length, lengthArrow);
  storeMethod(class, NAME_reference, referenceArrow);
  storeMethod(class, NAME_style, styleArrow);
  storeMethod(class, NAME_tip, tipArrow);
  storeMethod(class, NAME_wing, wingArrow);

  sendMethod(class, NAME_initialise, DEFAULT, 2, "length=[int]", "wing=[int]",
	     "Create from length and wing",
	     initialiseArrow);
  sendMethod(class, NAME_points, NAME_area,
	     4, "tip_x=[int]", "tip_y=[int]",
	        "reference_x=[int]", "reference_y=[int]",
	     "Set XY of tip and reference",
	     pointsArrow);
  sendMethod(class, NAME_referenceX, NAME_area, 1, "int",
	     "Set X of reference",
	     referenceXArrow);
  sendMethod(class, NAME_referenceY, NAME_area, 1, "int",
	     "Set Y of reference",
	     referenceYArrow);
  sendMethod(class, NAME_tipX, NAME_area, 1, "int",
	     "Set X of tip",
	     tipXArrow);
  sendMethod(class, NAME_tipY, NAME_area, 1, "int",
	     "Set Y of tip",
	     tipYArrow);
  sendMethod(class, NAME_DrawPostScript, NAME_postscript, 0,
	     "Create PostScript",
	     drawPostScriptArrow);
  sendMethod(class, NAME_compute, DEFAULT, 0,
	     "Compute <-tip, <-left and <-right",
	     computeArrow);

  getMethod(class, NAME_referenceX, NAME_area, "int", 0,
	    "X of reference point",
	    getReferenceXArrow);
  getMethod(class, NAME_referenceY, NAME_area, "int", 0,
	    "Y of reference point",
	    getReferenceYArrow);
  getMethod(class, NAME_tipX, NAME_area, "int", 0,
	    "X of tip point",
	    getTipXArrow);
  getMethod(class, NAME_tipY, NAME_area, "int", 0,
	    "Y of tip  point",
	    getTipYArrow);

  refine_resource(class, "selection_handles", "corner_handles");
  attach_resource(class, "length", "int", "10",
		  "Distance tip to base (10)");
  attach_resource(class, "wing", "int", "7",
		  "Width of wing (7)");
  attach_resource(class, "fill_pattern", "image|colour", "@black_image",
		  "Fill pattern for the triangle");
  attach_resource(class, "style", "{open,closed}", "closed",
		  "Whether or not the wing is closed");

  succeed;
}

