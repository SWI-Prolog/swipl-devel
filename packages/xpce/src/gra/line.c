/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <math.h>
#ifndef M_PI
#define M_PI (3.141593)
#endif

#ifdef MAXFLOAT
#define INFINITE MAXFLOAT
#elif defined(HUGE_VAL)
#define INFINITE HUGE_VAL
#else
#define INFINITE HUGE
#endif

#define points_area(x1, y1, x2, y2)	{ x = x1;			\
					  y = y1;			\
					  w = x2-x1;			\
					  w += (w>=0 ? 1 : -1);		\
					  h = y2-y1;			\
					  h += (h>=0 ? 1 : -1);		\
					}

#define area_points(x, y, w, h)		{ x1 = x;			\
					  y1 = y;			\
					  x2 = x+w;			\
					  x2 += (w>=0 ? -1 : 1);	\
					  y2 = y+h;			\
					  y2 += (h>=0 ? -1 : 1);	\
					}

status
initialiseLine(Line ln, Int xa, Int ya, Int xb, Int yb, Name arrows)
{ int x, y, w, h;
  int x1, y1, x2, y2;
  
  if ( isDefault(xa) )	xa = ZERO;
  if ( isDefault(ya) )	ya = ZERO;
  if ( isDefault(xb) )	xb = ZERO;
  if ( isDefault(yb) )	yb = ZERO;

  x1 = valInt(xa), y1 = valInt(ya), x2 = valInt(xb), y2=valInt(yb);

  points_area(x1, y1, x2, y2);
  initialiseJoint((Joint) ln, toInt(x), toInt(y), toInt(w), toInt(h), arrows);

  succeed;
}


status
copyLine(Line l1, Line l2)
{ copyJoint((Joint) l1, (Joint) l2);

  succeed;
}


static status
RedrawAreaLine(Line ln, Area a)
{ int x1, y1, x2, y2, x, y, w, h, pen;

  initialiseDeviceGraphical(ln, &x, &y, &w, &h);
  area_points(x, y, w, h);
  pen = valInt(ln->pen);

  if ( pen != 0 )
  { r_thickness(pen);
    r_dash(ln->texture);
    r_line(x1, y1, x2, y2);
  }

  if (notNil(ln->first_arrow))
    paintArrow(ln->first_arrow, toInt(x1), toInt(y1), toInt(x2), toInt(y2));
  if (notNil(ln->second_arrow))
    paintArrow(ln->second_arrow, toInt(x2), toInt(y2), toInt(x1), toInt(y1));

  return RedrawAreaGraphical(ln, a);
}


static status
startLine(Line ln, Point pos)
{ return pointsLine(ln, pos->x, pos->y, DEFAULT, DEFAULT);
}


static status
endLine(Line ln, Point pos)
{ return pointsLine(ln, DEFAULT, DEFAULT, pos->x, pos->y);
}


static status
startXLine(Line ln, Int x)
{ return pointsLine(ln, x, DEFAULT, DEFAULT, DEFAULT);
}


static status
startYLine(Line ln, Int y)
{ return pointsLine(ln, DEFAULT, y, DEFAULT, DEFAULT);
}


static status
endXLine(Line ln, Int x)
{ return pointsLine(ln, DEFAULT, DEFAULT, x, DEFAULT);
}


static status
endYLine(Line ln, Int y)
{ return pointsLine(ln, DEFAULT, DEFAULT, DEFAULT, y);
}


static Point
getStartLine(Line ln)
{ answer(answerObject(ClassPoint, ln->area->x, ln->area->y, 0));
}


Int
getStartXLine(Line ln)
{ answer(ln->area->x);
}


Int
getStartYLine(Line ln)
{ answer(ln->area->y);
}


static Point
getEndLine(Line ln)
{ Area a = ln->area;
  int x1, y1, x2, y2;

  area_points(valInt(a->x), valInt(a->y), valInt(a->w), valInt(a->h));

  answer(answerObject(ClassPoint, toInt(x2), toInt(y2), 0));
}


Int
getEndXLine(Line ln)
{ Area a = ln->area;
  int x1, y1, x2, y2;

  area_points(valInt(a->x), valInt(a->y), valInt(a->w), valInt(a->h));

  answer(toInt(x2));
}


Int
getEndYLine(Line ln)
{ Area a = ln->area;
  int x1, y1, x2, y2;

  area_points(valInt(a->x), valInt(a->y), valInt(a->w), valInt(a->h));

  answer(toInt(y2));
}


status
pointsLine(Line ln, Int sx, Int sy, Int ex, Int ey)
{ Area a = ln->area;
  int x1, y1, x2, y2;
  int x3, y3, x4, y4;
  int x, y, w, h;

  area_points(valInt(a->x), valInt(a->y), valInt(a->w), valInt(a->h));

  x3 = (isDefault(sx) ? x1 : valInt(sx));
  x4 = (isDefault(ex) ? x2 : valInt(ex));
  y3 = (isDefault(sy) ? y1 : valInt(sy));
  y4 = (isDefault(ey) ? y2 : valInt(ey));

  if (x1==x3 && y1==y3 && x2==x4 && y2==y4)
    succeed;
  
  points_area(x3, y3, x4, y4);
  return setGraphical(ln, toInt(x), toInt(y), toInt(w), toInt(h));
}


static status
inEventAreaLine(Line ln, Int x, Int y)
{ Area a = ln->area;
  int x1, y1, x2, y2;
  int d;

  area_points(valInt(a->x), valInt(a->y), valInt(a->w), valInt(a->h));
  d = distanceLineToPoint(x1, y1, x2, y2, valInt(x), valInt(y));
  if ( d < 4 )
    succeed;
  
  fail;
}


static Int
getLengthLine(Line ln)
{ Area a = ln->area;
  int x1, y1, x2, y2;

  area_points(valInt(a->x), valInt(a->y), valInt(a->w), valInt(a->h));

  answer(toInt(isqrt((x1-x2) * (x1-x2) + (y1-y2) * (y1-y2))));
}


static void
parms_line(Line ln, int *a, float *b)			/* y = a + bx */
        
       
         
{ Area area = ln->area;
  int x1, y1, x2, y2;

  area_points(valInt(area->x), valInt(area->y),
	      valInt(area->w), valInt(area->h));
  if ( x1 == x2 )
  { *b = INFINITE;			/* vertical */
    *a = 0;
  } else
  { *b = (float)(y2 - y1) / (float)(x2 - x1);
    *a = y1 - rfloat(*b * (float)x1);
  }

  DEBUG(NAME_intersection, printf("%d,%d --> %d,%d: y = %d + %2fx\n",
				  x1, y1, x2, y2, *a, *b));
}


Point
getIntersectionLine(Line l1, Line l2)
{ float b1, b2;
  int a1, a2;
  float xx;
  int xy;

  parms_line(l1, &a1, &b1);
  parms_line(l2, &a2, &b2);

  if ( b1 == b2 )
    fail;				/* parallel */
  if ( b1 == INFINITE )			/* l1 is vertical */
  { xx = (float) valInt(getEndXLine(l1));
    xy = a2 + rfloat(b2 * xx);
  } else if ( b2 == INFINITE )		/* l2 is vertical */
  { xx = (float) valInt(getEndXLine(l2));
    xy = a1 + rfloat(b1 * xx);
  } else
  { xx = (float)(a2 - a1) / (b1 - b2);
    xy = a1 + rfloat(b1 * xx);
  }

  answer(answerObject(ClassPoint, toInt(rfloat(xx)), toInt(xy), 0));
}


Real
getAngleLine(Line ln, Point p)
{ Area area = ln->area;
  int x1, y1, x2, y2;
  float angle;
  int rte = 0;				/* relative-to-end */
  
  area_points(valInt(area->x), valInt(area->y),
	      valInt(area->w), valInt(area->h));

  if ( notDefault(p) &&
       get_distance_point(p, x2, y2) < get_distance_point(p, x1, y1) )
    rte++;

  if ( rte )
    angle = atan2((float)(y2-y1), (float)(x1-x2));
  else
    angle = atan2((float)(y1-y2), (float)(x2-x1));
  if ( angle < 0 )
    angle = 2.0 * M_PI + angle;

  angle = (angle * 180.0) / M_PI;

  answer(CtoReal(angle));
}


static status
normaliseLine(Line ln)
{ succeed;
}


static status
orientationLine(Line ln, Name o)
{ succeed;
}


extern drawPostScriptLine(Line ln);
  
status
makeClassLine(Class class)
{ sourceClass(class, makeClassLine, __FILE__, "$Revision$");

  termClass(class, "line",
	    5, NAME_startX, NAME_startY, NAME_endX, NAME_endY, NAME_arrows);
  setRedrawFunctionClass(class, RedrawAreaLine);
  setInEventAreaFunctionClass(class, inEventAreaLine);

  sendMethod(class, NAME_initialise, DEFAULT, 5, 
	     "start_x=[int]", "start_y=[int]", "end_x=[int]", "end_y=[int]",
	     "arrows=[{none,first,second,both}]",
	     "Create line (X1,Y1) - (X2,Y2) with arrows",
	     initialiseLine);
  sendMethod(class, NAME_end, NAME_tip, 1, "point",
	     "Set end-point of line segment",
	     endLine);
  sendMethod(class, NAME_endX, NAME_tip, 1, "int",
	     "Set X of end-point",
	     endXLine);
  sendMethod(class, NAME_endY, NAME_tip, 1, "int",
	     "Set Y of end-point",
	     endYLine);
  sendMethod(class, NAME_points, NAME_tip, 4,
	     "start_x=[int]", "start_y=[int]", "end_x=[int]", "end_y=[int]",
	     "Reconfigure line (X1,Y1) - (X2,Y2)",
	     pointsLine);
  sendMethod(class, NAME_start, NAME_tip, 1, "point",
	     "Set start-point of line segement",
	     startLine);
  sendMethod(class, NAME_startX, NAME_tip, 1, "int",
	     "Set X of start-point",
	     startXLine);
  sendMethod(class, NAME_startY, NAME_tip, 1, "int",
	     "Set Y of start-point",
	     startYLine);
  sendMethod(class, NAME_DrawPostScript, NAME_postscript, 0,
	     "Create PostScript",
	     drawPostScriptLine);
  sendMethod(class, NAME_copy, NAME_copy, 1, "line",
	     "Copy attributes from other line",
	     copyLine);
  sendMethod(class, NAME_normalise, DEFAULT, 0,
	     "Redefined from graphical: no-op",
	     normaliseLine);
  sendMethod(class, NAME_orientation, DEFAULT,
	     1, "{north_west,south_east,north_east,south_east}",
	     "Redefined from graphical: no-op",
	     orientationLine);

  getMethod(class, NAME_end, NAME_tip, "point", 0,
	    "New point representing end-point",
	    getEndLine);
  getMethod(class, NAME_endX, NAME_tip, "int", 0,
	    "X of end-point",
	    getEndXLine);
  getMethod(class, NAME_endY, NAME_tip, "int", 0,
	    "Y of end-point",
	    getEndYLine);
  getMethod(class, NAME_start, NAME_tip, "point", 0,
	    "New point representing start-point",
	    getStartLine);
  getMethod(class, NAME_startX, NAME_tip, "int", 0,
	    "X of start-point",
	    getStartXLine);
  getMethod(class, NAME_startY, NAME_tip, "int", 0,
	    "Y of start-point",
	    getStartYLine);
  getMethod(class, NAME_length, NAME_calculate, "int", 0,
	    "Distance between start and end-points",
	    getLengthLine);
  getMethod(class, NAME_intersection, NAME_calculate, "point", 1, "with=line",
	    "Intersection between both infinitely extended lines",
	    getIntersectionLine);
  getMethod(class, NAME_angle, NAME_calculate, "degrees=real", 1,
	    "origin=[point]",
	    "Angle",
	    getAngleLine);

  refine_resource(class, "selection_handles", "line");

  succeed;
}
