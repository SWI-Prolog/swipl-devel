/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <ari/proto.h>

#define InitAreaA	int ax = valInt(a->x), ay = valInt(a->y), \
			    aw = valInt(a->w), ah = valInt(a->h)
#define InitAreaB	int bx = valInt(b->x), by = valInt(b->y), \
			    bw = valInt(b->w), bh = valInt(b->h)


static status
initialiseRegion(RegionObj r, Equation x, Equation y, Equation w, Equation h)
{ assign(r, x, x);
  assign(r, y, y);
  assign(r, w, w);
  assign(r, h, h);

  succeed;
}


#define XYWH	VarX, a->x, VarY, a->y, VarW, a->w, VarH, a->h

static Int
getAreaXRegion(RegionObj r, Area a)
{ answer(getValueExpression(r->x, XYWH, 0));
}


static Int
getAreaYRegion(RegionObj r, Area a)
{ answer(getValueExpression(r->y, XYWH, 0));
}

static Int
getAreaWRegion(RegionObj r, Area a)
{ answer(getValueExpression(r->w, XYWH, 0));
}

static Int
getAreaHRegion(RegionObj r, Area a)
{ answer(getValueExpression(r->h, XYWH, 0));
}


status
insideRegion(RegionObj r, Area a, Point p)
{ int x, y, w, h;
  int px = valInt(p->x);
  int py = valInt(p->y);

  x = valInt(getAreaXRegion(r, a));
  w = valInt(getAreaWRegion(r, a));

  if ((w >= 0 && (px < x || px > x+w)) || (w < 0 && (px < x+w || px > x)))
    fail;

  y = valInt(getAreaYRegion(r, a));
  h = valInt(getAreaHRegion(r, a));

  if ((h >= 0 && (py < y || py > y+h)) || (h < 0 && (py < y+h || py > y)))
    fail;
  
  succeed;
}


static Area
getAreaRegion(RegionObj r, Area a)
{ Int x=a->x, y=a->y, w=a->w, h=a->h;

  answer(answerObject(ClassArea, 
    getValueExpression(r->x, VarX, x, VarW, w, VarY, y, VarH, h, 0), 
    getValueExpression(r->y, VarX, x, VarW, w, VarY, y, VarH, h, 0), 
    getValueExpression(r->w, VarX, x, VarW, w, VarY, y, VarH, h, 0), 
    getValueExpression(r->h, VarX, x, VarW, w, VarY, y, VarH, h, 0), 
    0));
}


status
makeClassRegion(Class class)
{ sourceClass(class, makeClassRegion, __FILE__, "$Revision$");

  localClass(class, NAME_x, NAME_dimension, "expression", NAME_both,
	     "X of area expressed in XYWH of area");
  localClass(class, NAME_y, NAME_dimension, "expression", NAME_both,
	     "Y of area expressed in XYWH of area");
  localClass(class, NAME_width, NAME_dimension, "expression", NAME_both,
	     "W of area expressed in XYWH of area");
  localClass(class, NAME_height, NAME_dimension, "expression", NAME_both,
	     "H of area expressed in XYWH of area");

  termClass(class, "region", 4, NAME_x, NAME_y, NAME_width, NAME_height);

  sendMethod(class, NAME_initialise, DEFAULT, 4,
	     "x=expression", "y=expression",
	     "width=expression", "height=expression",
	     "Create region from XYWH-expressions",
	     initialiseRegion);
  sendMethod(class, NAME_inside, NAME_compare, 2, "area", "point",
	     "Test if point is inside region of area",
	     insideRegion);

  getMethod(class, NAME_area, NAME_calculate, "area", 1, "area",
	    "New area describing region of argument",
	    getAreaRegion);
  getMethod(class, NAME_areaX, NAME_calculate, "int", 1, "area",
	    "X of region of argument",
	    getAreaXRegion);
  getMethod(class, NAME_areaY, NAME_calculate, "int", 1, "area",
	    "Y of region of argument",
	    getAreaYRegion);
  getMethod(class, NAME_areaWidth, NAME_calculate, "int", 1, "area",
	    "W of region of argument",
	    getAreaWRegion);
  getMethod(class, NAME_areaHeight, NAME_calculate, "int", 1, "area",
	    "H of region of argument",
	    getAreaHRegion);

  succeed;
}

