/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialisePoint(Point p, Int x, Int y)
{ if ( isDefault(x) ) x = ZERO;
  if ( isDefault(y) ) y = ZERO;
  assign(p, x, x);
  assign(p, y, y);

  succeed;
}


static Point
getConvertPoint(Class class, Any obj)
{ if ( instanceOfObject(obj, ClassEvent) )
    return getPositionEvent((EventObj) obj, DEFAULT);
  else
  { CharArray ca = obj;
    int x, y;

    if ( isstr8(&ca->data) &&
	 sscanf(ca->data.s_text8, "%d,%d", &x, &y) == 2 )
      answer(newObject(ClassPoint, toInt(x), toInt(y), 0));
  }

  fail;
}


static StringObj
getPrintNamePoint(Point p)
{ char buf[200];

  sprintf(buf, "%ld,%ld", valInt(p->x), valInt(p->y));
  answer(CtoString(buf));
}


status
equalPoint(Point p1, Point p2)
{ if (p1->x == p2->x && p1->y == p2->y)
    succeed;
  fail;
}


status
copyPoint(Point p1, Point p2)
{ assign(p1, x, p2->x);
  assign(p1, y, p2->y);

  succeed;
}


static Point
getCopyPoint(Point p)
{ answer(answerObject(p->class, p->x, p->y, 0));
}


status
setPoint(Point pt, Int x, Int y)
{ if ( notDefault(x) ) assign(pt, x, x);
  if ( notDefault(y) ) assign(pt, y, y);

  succeed;
}


status
offsetPoint(Point pt, Int x, Int y)
{ assign(pt, x, add(x,pt->x));
  assign(pt, y, add(y,pt->y));

  succeed;
}


static Point
getDifferencePoint(Point p, Point q)
{ answer(answerObject(ClassPoint, sub(p->x,q->x), sub(p->y,q->y),0));
}


int
get_distance_point(Point p, int x, int y)
{ int x1 = valInt(p->x);
  int y1 = valInt(p->y);

  return isqrt((x1-x) * (x1-x) + (y1-y) * (y1-y));
}


Int
getDistancePoint(Point p, Point q)
{ answer(toInt(get_distance_point(p, valInt(q->x), valInt(q->y))));
}


status
plusPoint(Point p, Point q)
{ assign(p, x, add(p->x,q->x));
  assign(p, y, add(p->y,q->y));

  succeed;
}


status
minusPoint(Point p, Point q)
{ assign(p, x, sub(p->x,q->x));
  assign(p, y, sub(p->y,q->y));

  succeed;
}


static status
mirrorPoint(Point p, Point q)
{ Int mx = ZERO, my = ZERO;

  if ( notDefault(q) )
  { mx = q->x; 
    my = q->y;
  }

  assign(p, x, sub(mx, p->x));
  assign(p, y, sub(my, p->y));

  succeed;
}


status
makeClassPoint(Class class)
{ sourceClass(class, makeClassPoint, __FILE__, "$Revision$");

  localClass(class, NAME_x, NAME_dimension, "int", NAME_both,
	     "x-coordinate of point");
  localClass(class, NAME_y, NAME_dimension, "int", NAME_both,
	     "y-coordinate of point");

  termClass(class, "point", 2, NAME_x, NAME_y);

  sendMethod(class, NAME_initialise, DEFAULT, 2, "x=[int]", "y=[int]",
	     "Create point from x- and y-value",
	     initialisePoint);
  sendMethod(class, NAME_copy, NAME_copy, 1, "from=point",
	     "Copy x- and y values from the argument",
	     copyPoint);
  sendMethod(class, NAME_equal, NAME_compare, 1, "to=point",
	     "Test if argument is same position",
	     equalPoint);
  sendMethod(class, NAME_offset, NAME_calculate, 2, "dx=int", "dy=int",
	     "Move the point by given x and y",
	     offsetPoint);
  sendMethod(class, NAME_set, NAME_calculate, 2, "x=[int]", "y=[int]",
	     "Set x- and y-values",
	     setPoint);
  sendMethod(class, NAME_plus, NAME_calculate, 1, "point",
	     "Add x- and y-values of argument point",
	     plusPoint);
  sendMethod(class, NAME_minus, NAME_calculate, 1, "point",
	     "Substract x and y of argument point",
	     minusPoint);
  sendMethod(class, NAME_mirror, NAME_calculate, 1, "origin=[point]",
	     "Mirror point around argument or (0,0)",
	     mirrorPoint);

  getMethod(class, NAME_printName, NAME_textual, "string", 0,
	    "Printed representation as %d,%d",
	    getPrintNamePoint);
  getMethod(class, NAME_difference, NAME_calculate, "point", 1, "to=point",
	    "New point that reflects distance",
	    getDifferencePoint);
  getMethod(class, NAME_distance, NAME_calculate, "int", 1, "to=point",
	    "Integer for distance between points",
	    getDistancePoint);
  getMethod(class, NAME_convert, NAME_textual, "point", 1, "event|char_array",
	    "Event <-position or the text `x,y'",
	    getConvertPoint);
  getMethod(class, NAME_copy, NAME_copy, "point", 0,
	    "New point with same <-x and <-y",
	    getCopyPoint);

  succeed;
}
