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


status
initialiseLine(Line ln, Int xa, Int ya, Int xb, Int yb, Name arrows)
{ if ( isDefault(xa) )	xa = ZERO;
  if ( isDefault(ya) )	ya = ZERO;
  if ( isDefault(xb) )	xb = ZERO;
  if ( isDefault(yb) )	yb = ZERO;

  assign(ln, start_x, xa);
  assign(ln, start_y, ya);
  assign(ln, end_x,   xb);
  assign(ln, end_y,   yb);

  initialiseJoint((Joint) ln, ZERO, ZERO, ZERO, ZERO, arrows);

  return requestComputeGraphical(ln, DEFAULT);
}


#define area_points(x, y, w, h)         { x1 = x;                       \
                                          y1 = y;                       \
                                          x2 = x+w;                     \
                                          x2 += (w>=0 ? -1 : 1);        \
                                          y2 = y+h;                     \
                                          y2 += (h>=0 ? -1 : 1);        \
                                        }

static status
loadLine(Line ln, FILE *fd, ClassDef def)
{ TRY(loadSlotsObject(ln, fd, def));

  if ( isNil(ln->start_x) )		/* convert old (pre-4.9.4) line */
  { Area a = ln->area;			/* representation */
    int x = valInt(a->x);
    int y = valInt(a->y);
    int w = valInt(a->w);
    int h = valInt(a->h);
    int x1, y1, x2, y2;

    area_points(x, y, w, h);
    assign(ln, start_x, toInt(x1));
    assign(ln, start_y, toInt(y1));
    assign(ln, end_x,   toInt(x2));
    assign(ln, end_y,   toInt(y2));
  }

  succeed;
}


status
adjustFirstArrowLine(Line ln)
{ if ( notNil(ln->first_arrow) )
  { Any av[4];

    av[0] = ln->start_x;
    av[1] = ln->start_y;
    av[2] = ln->end_x;
    av[3] = ln->end_y;

    if ( qadSendv(ln->first_arrow, NAME_points, 4, av) )
      return ComputeGraphical(ln->first_arrow);
  }

  fail;
}


status
adjustSecondArrowLine(Line ln)
{ if ( notNil(ln->second_arrow) )
  { Any av[4];

    av[0] = ln->end_x;
    av[1] = ln->end_y;
    av[2] = ln->start_x;
    av[3] = ln->start_y;

    if ( qadSendv(ln->second_arrow, NAME_points, 4, av) )
      return ComputeGraphical(ln->second_arrow);
  }

  fail;
}


status
computeLine(Line ln)
{ if ( notNil(ln->request_compute) )
  { int x1  = valInt(ln->start_x);
    int x2  = valInt(ln->end_x);
    int y1  = valInt(ln->start_y);
    int y2  = valInt(ln->end_y);
    int pen = valInt(ln->pen);
    int x, y, w, h;
    Area a = ln->area;
  
    if ( x1 < x2 )
    { x = x1;
      w = x2-x1;
    } else
    { x = x2;
      w = x1-x2;
    }
    if ( y1 < y2 )
    { y = y1;
      h = y2-y1;
    } else
    { y = y2;
      h = y1-y2;
    }
  
    if ( pen ==	1 )
    { w++;
      h++;
    } else if ( pen > 1 )
    { int ex = (h > 0 ? (pen*h)/(w+h) : 0); /* h = 0: horizontal line */
      int ey = (w > 0 ? (pen*w)/(w+h) : 0); /* w = 0: vertical line */
      int hx = ex/2;
      int hy = ey/2;

      x -= hx;
      w += ex;
      y -= hy;
      h += ey;
    }

    if ( ln->selected == ON )	/* should be solved elsewhere */
    { x -= 3;
      y -= 3;
      w += 6;
      h += 6;
    }

    CHANGING_GRAPHICAL(ln,
		       assign(a, x, toInt(x));
		       assign(a, y, toInt(y));
		       assign(a, w, toInt(w));
		       assign(a, h, toInt(h));

		       if ( adjustFirstArrowLine(ln) )
			 unionNormalisedArea(a, ln->first_arrow->area);
		       if ( adjustSecondArrowLine(ln) )
			 unionNormalisedArea(a, ln->second_arrow->area);

		       changedEntireImageGraphical(ln));

    assign(ln, request_compute, NIL);
  }

  succeed;
}


status
copyLine(Line l1, Line l2)
{ copyJoint((Joint) l1, (Joint) l2);

  assign(l1, start_x, l2->start_x);
  assign(l1, start_y, l2->start_y);
  assign(l1, end_x,   l2->end_x);
  assign(l1, end_y,   l2->end_y);

  succeed;
}


static status
RedrawAreaLine(Line ln, Area a)
{ int x, y, w, h;
  int x1 = valInt(ln->start_x);
  int x2 = valInt(ln->end_x);
  int y1 = valInt(ln->start_y);
  int y2 = valInt(ln->end_y);
  int pen = valInt(ln->pen);

  initialiseDeviceGraphical(ln, &x, &y, &w, &h);

  if ( pen != 0 )
  { r_thickness(pen);
    r_dash(ln->texture);
    r_line(x1, y1, x2, y2);
  }

  if ( adjustFirstArrowLine(ln) )
    RedrawArea(ln->first_arrow, a);
  if ( adjustSecondArrowLine(ln) )
    RedrawArea(ln->second_arrow, a);

  return RedrawAreaGraphical(ln, a);
}


status
paintSelectedLine(Line ln)		/* assumes device is initialised! */
{ r_complement(valInt(ln->start_x)-2, valInt(ln->start_y)-2, 5, 5);
  r_complement(valInt(ln->end_x)-2,   valInt(ln->end_y)-2,   5, 5);

  succeed;
}


static status
geometryLine(Line ln, Int x, Int y, Int w, Int h)
{ int needcompute = FALSE;
  Int dx = ZERO, dy = ZERO;

  if ( notDefault(w) )
  { assign(ln, end_x, add(ln->start_x, w));
    needcompute++;
  }
  if ( notDefault(h) )
  { assign(ln, end_y, add(ln->start_y, h));
    needcompute++;
  }

  if ( notDefault(x) )
  { dx = sub(x, ln->area->x);
    assign(ln, start_x, add(ln->start_x, dx));
    assign(ln, end_x, add(ln->end_x, dx));
  }
  if ( notDefault(y) )
  { dy = sub(y, ln->area->y);
    assign(ln, start_y, add(ln->start_y, dy));
    assign(ln, end_y, add(ln->end_y, dy));
  } 
    
  CHANGING_GRAPHICAL(ln,
		     if ( needcompute )
		       requestComputeGraphical(ln, DEFAULT);
		     else
		     { Area a = ln->area;
		       
		       assign(a, x, add(a->x, dx));
		       assign(a, y, add(a->y, dy));
		       changedEntireImageGraphical(ln);
		     });
		       
  succeed;
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
{ answer(answerObject(ClassPoint, ln->start_x, ln->start_y, 0));
}


static Point
getEndLine(Line ln)
{ answer(answerObject(ClassPoint, ln->end_x, ln->end_y, 0));
}


status
pointsLine(Line ln, Int sx, Int sy, Int ex, Int ey)
{ if ( !isDefault(sx) ) assign(ln, start_x, sx);
  if ( !isDefault(sy) ) assign(ln, start_y, sy);
  if ( !isDefault(ex) ) assign(ln, end_x,   ex);
  if ( !isDefault(ey) ) assign(ln, end_y,   ey);

  return requestComputeGraphical(ln, DEFAULT);
}


static status
resizeLine(Line ln, Real xfactor, Real yfactor, Point origin)
{ float xf, yf;
  int ox = valInt(ln->area->x);
  int oy = valInt(ln->area->y);

  init_resize_graphical(ln, xfactor, yfactor, origin, &xf, &yf, &ox, &oy);
  if ( xf != 1.0 || yf != 1.0 )
  { int x1, y1, x2, y2;

    x1 = ox + rfloat((float) (valInt(ln->start_x)-ox) * xf);
    x2 = ox + rfloat((float) (valInt(ln->end_x)-ox)   * xf);
    y1 = oy + rfloat((float) (valInt(ln->start_y)-oy) * yf);
    y2 = oy + rfloat((float) (valInt(ln->end_y)-oy)   * yf);
    
    assign(ln, start_x, toInt(x1));
    assign(ln, start_y, toInt(y1));
    assign(ln, end_x,   toInt(x2));
    assign(ln, end_y,   toInt(y2));

    return requestComputeGraphical(ln, DEFAULT);
  }
  
  succeed;
}


/* (JW)	Calculate the distance between the infinite extended line through
	(x1, y1) and (x2, y2) to the point (px, py).
 */

static int
distanceLineToPoint(int x1, int y1, int x2, int y2, int px, int py)
{ float a;

  if (y1 == y2)
    return abs(y1 - py);
  if (x1 == x2)
    return abs(x1 - px);

  a = ((float)(y2 - y1)) / ((float)(x2 - x1));
  return abs(rfloat((((float)(px - x1)) * a + ((float)(y1 - py))) /
					 sqrt(1.0 + a*a)));
}


static status
inEventAreaLine(Line ln, Int x, Int y)
{ int d;
  static evtol = -1;

  if ( evtol < 0 )
  { Int v = getResourceValueObject(ln, NAME_eventTolerance);
    evtol = (v ? valInt(v) : 5);
  }

  d = distanceLineToPoint(valInt(ln->start_x), valInt(ln->start_y),
			  valInt(ln->end_x), valInt(ln->end_y),
			  valInt(x), valInt(y));
  if ( d < evtol )
    succeed;
  
  fail;
}


static Int
getDistanceLine(Line ln, Any obj)
{ if ( instanceOfObject(obj, ClassPoint) )
  { Point pt = obj;

    answer(toInt(distanceLineToPoint(valInt(ln->start_x), valInt(ln->start_y),
				     valInt(ln->end_x), valInt(ln->end_y),
				     valInt(pt->x), valInt(pt->y))));
  } else
  { Graphical gr2 = obj;

    answer(getDistanceArea(ln->area, gr2->area));
  }
}



static Int
getLengthLine(Line ln)
{ int dx = valInt(ln->end_x) - valInt(ln->start_x);
  int dy = valInt(ln->end_y) - valInt(ln->start_y);

  answer(toInt(isqrt(dx*dx + dy*dy)));
}


static void
parms_line(Line ln, int *a, float *b)			/* y = a + bx */
{ int x1 = valInt(ln->start_x);
  int x2 = valInt(ln->end_x);
  int y1 = valInt(ln->start_y);
  int y2 = valInt(ln->end_y);

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
  { xx = (float) valInt(l1->end_x);
    xy = a2 + rfloat(b2 * xx);
  } else if ( b2 == INFINITE )		/* l2 is vertical */
  { xx = (float) valInt(l2->end_x);
    xy = a1 + rfloat(b1 * xx);
  } else
  { xx = (float)(a2 - a1) / (b1 - b2);
    xy = a1 + rfloat(b1 * xx);
  }

  answer(answerObject(ClassPoint, toInt(rfloat(xx)), toInt(xy), 0));
}


Real
getAngleLine(Line ln, Point p)
{ int x1 = valInt(ln->start_x);
  int x2 = valInt(ln->end_x);
  int y1 = valInt(ln->start_y);
  int y2 = valInt(ln->end_y);
  float angle;
  int rte = 0;				/* relative-to-end */

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


static status
penLine(Line ln, Int pen)
{ if ( ln->pen != pen )
  { assign(ln, pen, pen);

    return requestComputeGraphical(ln, DEFAULT);
  }

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_points[] =
        { "start_x=[int]", "start_y=[int]", "end_x=[int]", "end_y=[int]" };
static char *T_initialise[] =
        { "start_x=[int]", "start_y=[int]", "end_x=[int]", "end_y=[int]", "arrows=[{none,first,second,both}]" };
static char *T_resize[] =
	{ "factor_x=real", "factor_y=[real]", "origin=[point]" };
static char *T_geometry[] =
	{ "x=[int]", "y=[int]", "width=[int]", "height=[int]" };

/* Instance Variables */

vardecl var_line[] =
{ SV(NAME_startX, "int", IV_GET|IV_STORE, startXLine,
     NAME_tip, "X of start-point"),
  SV(NAME_startY, "int", IV_GET|IV_STORE, startYLine,
     NAME_tip, "Y of start-point"),
  SV(NAME_endX,   "int", IV_GET|IV_STORE, endXLine,
     NAME_tip, "X of end-point"),
  SV(NAME_endY,   "int", IV_GET|IV_STORE, endYLine,
     NAME_tip, "Y of end-point")
};


/* Send Methods */

static senddecl send_line[] =
{ SM(NAME_initialise, 5, T_initialise, initialiseLine,
     DEFAULT, "Create line (X1,Y1) - (X2,Y2) with arrows"),
  SM(NAME_normalise, 0, NULL, normaliseLine,
     DEFAULT, "Redefined from graphical: no-op"),
  SM(NAME_orientation, 1, "{north_west,south_west,north_east,south_east}", orientationLine,
     DEFAULT, "Redefined from graphical: no-op"),
  SM(NAME_compute, 0, NULL, computeLine,
     NAME_update, "Update <-area of the line"),
  SM(NAME_geometry, 4, T_geometry, geometryLine,
     NAME_resize, "Define start and vector"),
  SM(NAME_copy, 1, "line", copyLine,
     NAME_copy, "Copy attributes from other line"),
  SM(NAME_DrawPostScript, 0, NULL, drawPostScriptLine,
     NAME_postscript, "Create PostScript"),
  SM(NAME_end, 1, "point", endLine,
     NAME_tip, "Set end-point of line segment"),
  SM(NAME_points, 4, T_points, pointsLine,
     NAME_tip, "Reconfigure line (X1,Y1) - (X2,Y2)"),
  SM(NAME_start, 1, "point", startLine,
     NAME_tip, "Set start-point of line segment"),
  SM(NAME_resize, 3, T_resize, resizeLine,
     NAME_area, "Resize line with specified factor"),
  SM(NAME_pen, 1, "0..", penLine,
     NAME_appearance, "Thickness of drawing pen")
};

/* Get Methods */

static getdecl get_line[] =
{ GM(NAME_angle, 1, "degrees=real", "origin=[point]", getAngleLine,
     NAME_calculate, "Angle"),
  GM(NAME_intersection, 1, "point", "with=line", getIntersectionLine,
     NAME_calculate, "Intersection between both infinitely extended lines"),
  GM(NAME_length, 0, "int", NULL, getLengthLine,
     NAME_calculate, "Distance between start and end-points"),
  GM(NAME_distance, 1, "int", "graphical|point", getDistanceLine,
     NAME_calculate, "Distance between areas or to point"),
  GM(NAME_end, 0, "point", NULL, getEndLine,
     NAME_tip, "New point representing end-point"),
  GM(NAME_start, 0, "point", NULL, getStartLine,
     NAME_tip, "New point representing start-point")
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
  setLoadStoreFunctionClass(class, loadLine, NULL);

  succeed;
}
