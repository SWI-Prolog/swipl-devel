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

  DEBUG(NAME_intersection, Cprintf("%d,%d --> %d,%d: y = %d + %2fx\n",
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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_points[] =
        { "start_x=[int]", "start_y=[int]", "end_x=[int]", "end_y=[int]" };
static char *T_initialise[] =
        { "start_x=[int]", "start_y=[int]", "end_x=[int]", "end_y=[int]", "arrows=[{none,first,second,both}]" };

/* Instance Variables */

#define var_line NULL
/*
vardecl var_line[] =
{ 
};
*/

/* Send Methods */

static senddecl send_line[] =
{ SM(NAME_initialise, 5, T_initialise, initialiseLine,
     DEFAULT, "Create line (X1,Y1) - (X2,Y2) with arrows"),
  SM(NAME_normalise, 0, NULL, normaliseLine,
     DEFAULT, "Redefined from graphical: no-op"),
  SM(NAME_orientation, 1, "{north_west,south_east,north_east,south_east}", orientationLine,
     DEFAULT, "Redefined from graphical: no-op"),
  SM(NAME_copy, 1, "line", copyLine,
     NAME_copy, "Copy attributes from other line"),
  SM(NAME_DrawPostScript, 0, NULL, drawPostScriptLine,
     NAME_postscript, "Create PostScript"),
  SM(NAME_end, 1, "point", endLine,
     NAME_tip, "Set end-point of line segment"),
  SM(NAME_endX, 1, "int", endXLine,
     NAME_tip, "Set X of end-point"),
  SM(NAME_endY, 1, "int", endYLine,
     NAME_tip, "Set Y of end-point"),
  SM(NAME_points, 4, T_points, pointsLine,
     NAME_tip, "Reconfigure line (X1,Y1) - (X2,Y2)"),
  SM(NAME_start, 1, "point", startLine,
     NAME_tip, "Set start-point of line segment"),
  SM(NAME_startX, 1, "int", startXLine,
     NAME_tip, "Set X of start-point"),
  SM(NAME_startY, 1, "int", startYLine,
     NAME_tip, "Set Y of start-point")
};

/* Get Methods */

static getdecl get_line[] =
{ GM(NAME_angle, 1, "degrees=real", "origin=[point]", getAngleLine,
     NAME_calculate, "Angle"),
  GM(NAME_intersection, 1, "point", "with=line", getIntersectionLine,
     NAME_calculate, "Intersection between both infinitely extended lines"),
  GM(NAME_length, 0, "int", NULL, getLengthLine,
     NAME_calculate, "Distance between start and end-points"),
  GM(NAME_end, 0, "point", NULL, getEndLine,
     NAME_tip, "New point representing end-point"),
  GM(NAME_endX, 0, "int", NULL, getEndXLine,
     NAME_tip, "X of end-point"),
  GM(NAME_endY, 0, "int", NULL, getEndYLine,
     NAME_tip, "Y of end-point"),
  GM(NAME_start, 0, "point", NULL, getStartLine,
     NAME_tip, "New point representing start-point"),
  GM(NAME_startX, 0, "int", NULL, getStartXLine,
     NAME_tip, "X of start-point"),
  GM(NAME_startY, 0, "int", NULL, getStartYLine,
     NAME_tip, "Y of start-point")
};

/* Resources */

static resourcedecl rc_line[] =
{ RC(NAME_selectionHandles, RC_REFINE, "line",
     NULL)
};

/* Class Declaration */

static Name line_termnames[] =
	{ NAME_startX, NAME_startY, NAME_endX, NAME_endY, NAME_arrows };

ClassDecl(line_decls,
          var_line, send_line, get_line, rc_line,
          5, line_termnames,
          "$Rev$");

  
status
makeClassLine(Class class)
{ declareClass(class, &line_decls);

  setRedrawFunctionClass(class, RedrawAreaLine);
  setInEventAreaFunctionClass(class, inEventAreaLine);

  succeed;
}
