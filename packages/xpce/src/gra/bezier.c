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

#define MAXPTS 100

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Implementation of Quadratic Bezier Curves.  Using algorithm description
from:

http://graphics.cs.ucdavis.edu/CAGDNotes/Divide-and-Conquer-Bezier-Curve/Divide-and-Conquer-Bezier-Curve.html

Cubic (using 4 control-points) are described in:

http://muldoon.cipic.ucdavis.edu/CAGDNotes/Cubic-Bezier-Curves/Cubic-Bezier-Curves.html
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
initialiseBezier(Bezier b, Point start, Point end,
		 Point c1, Point c2)
{ initialiseGraphical(b, ZERO, ZERO, ZERO, ZERO);

  if ( isDefault(c2) )
    c2 = NIL;

  assign(b, start,    start);
  assign(b, end,      end);
  assign(b, control1, c1);
  assign(b, control2, c2);

  return requestComputeGraphical(b, DEFAULT);
}


		 /*******************************
		 *	      ARROWS		*
		 *******************************/

status
adjustFirstArrowBezier(Bezier b)
{ if ( notNil(b->first_arrow) )
  { Any av[4];

    av[0] = b->start->x;
    av[1] = b->start->y;
    av[2] = b->control1->x;
    av[3] = b->control1->y;

    if ( qadSendv(b->first_arrow, NAME_points, 4, av) )
      return ComputeGraphical(b->first_arrow);
  }

  fail;
}


status
adjustSecondArrowBezier(Bezier b)
{ if ( notNil(b->second_arrow) )
  { Any av[4];

    av[0] = b->end->x;
    av[1] = b->end->y;
    if ( notNil(b->control2) )
    { av[2] = b->control2->x;
      av[3] = b->control2->y;
    } else
    { av[2] = b->control1->x;
      av[3] = b->control1->y;
    }

    if ( qadSendv(b->second_arrow, NAME_points, 4, av) )
      return ComputeGraphical(b->second_arrow);
  }

  fail;
}




		 /*******************************
		 *	     COMPUTING		*
		 *******************************/

static void
shiftpts(IPoint pts, int to, int shift)
{ 
  DEBUG(NAME_bezier, Cprintf("Shift to %d\n", to));
  to--;
  for(; to>=shift; to--)
    pts[to] = pts[to-shift];
}


#define mkmid(p, p1, p2) \
	{ p.x = (p1.x + p2.x + 1)/2; \
	  p.y = (p1.y + p2.y + 1)/2; \
	}

static int
splitQuadratic(IPoint pts, int i, int *n)
{ ipoint m;
  int md = 1;

  pts += i;
  mkmid(m, pts[0], pts[2]);
  
  if ( abs(m.x-pts[1].x) > md || abs(m.y-pts[1].y) > md )
  { ipoint p1;

    p1 = pts[1];

    *n = *n + 2;
    shiftpts(pts, *n-i, 2);

    mkmid(pts[1], pts[0], p1);
    mkmid(pts[3], p1,     pts[4]);
    mkmid(pts[2], pts[1], pts[3]);

    return TRUE;
  }

  return FALSE;
}


static int
splitCubic(IPoint pts, int i, int *n)
{ pts += i;

  if ( distanceLineToPoint(pts[0].x, pts[0].y, pts[3].x, pts[3].y,
			   pts[1].x, pts[1].y) > 1 ||
       distanceLineToPoint(pts[0].x, pts[0].y, pts[3].x, pts[3].y,
			   pts[2].x, pts[2].y) > 1 )
  { ipoint p1, p2, p12;
    
    p1 = pts[1];
    p2 = pts[2];

    *n = *n + 3;
    shiftpts(pts, *n-i, 3);

    mkmid(p12,    p1,     p2);
    mkmid(pts[1], pts[0], p1);
    mkmid(pts[5], p2,     pts[6]);
    mkmid(pts[2], pts[1], p12);
    mkmid(pts[4], pts[5], p12);
    mkmid(pts[3], pts[2], pts[4]);

    return TRUE;
  }

  return FALSE;
}


/*
static void
printPts(IPoint pts, int n)
{ int i;

  for(i=0; i<n; i++)
    Cprintf(" %d,%d", pts[i].x, pts[i].y);
    
  Cprintf("\n");
}
*/

static void
compute_points_bezier(Bezier b, IPoint pts, int *mx)
{ int mxpts = *mx;
  int npts;
  int i;
  IPoint p = pts;

  p->x = valInt(b->start->x);
  p->y = valInt(b->start->y);
  p++;
  p->x = valInt(b->control1->x);
  p->y = valInt(b->control1->y);
  p++;
  if ( notNil(b->control2) )
  { p->x = valInt(b->control2->x);
    p->y = valInt(b->control2->y);
    p++;
  }
  p->x = valInt(b->end->x);
  p->y = valInt(b->end->y);
  p++;

  npts = p-pts;
  if ( notNil(b->control2) )
  { for(i=0; i <= npts-3; i += 3)
    { if ( npts >= mxpts-3 )
	break;
      while(splitCubic(pts, i, &npts))
	;
    }
  } else
  { for(i=0; i <= npts-3; i += 2)
    { if ( npts >= mxpts-2 )
	break;
      while(splitQuadratic(pts, i, &npts))
	;
    }
  }

  *mx = npts;
}


typedef struct reg
{ int minx, miny, maxx, maxy;
} reg;


static void
include_in_reg(reg *r, Point pt)
{ int px = valInt(pt->x);
  int py = valInt(pt->y);

  r->minx = min(r->minx, px);
  r->maxx = max(r->maxx, px);
  r->miny = min(r->miny, py);
  r->maxy = max(r->maxy, py);
}


static status
computeBoundingBoxBezier(Bezier b)
{ reg r;

  r.minx = 1000000;
  r.miny = 1000000;
  r.maxx = -1000000;
  r.maxy = -10000000;

  if ( b->selected == ON )
  { int mw=5, mh=5;

    include_in_reg(&r, b->start);
    include_in_reg(&r, b->end);
    include_in_reg(&r, b->control1);
    if ( notNil(b->control2) )
      include_in_reg(&r, b->control2);

    r.minx -= (mw+1)/2;
    r.maxx += (mw+1)/2;
    r.miny -= (mh+1)/2;
    r.maxy += (mh+1)/2;
  } else
  { ipoint ptsbuf[MAXPTS];
    IPoint pts = ptsbuf;
    int npts = MAXPTS;
    int i;

    compute_points_bezier(b, pts, &npts);

    for(i=0; i<npts; i++,pts++)
    { if ( pts[0].x < r.minx ) r.minx = pts[0].x;
      if ( pts[0].x > r.maxx ) r.maxx = pts[0].x;
      if ( pts[0].y < r.miny ) r.miny = pts[0].y;
      if ( pts[0].y > r.maxy ) r.maxy = pts[0].y;
    }
  }

  if ( r.maxx >= r.minx && r.maxy >= r.miny )
  { int pens = valInt(b->pen) / 2;
    int pena = (valInt(b->pen) % 2 == 0 ? pens : pens + 1);

    r.minx -= pens; r.maxx += pena;
    r.miny -= pens; r.maxy += pena;

    assign(b->area, x, toInt(r.minx));
    assign(b->area, y, toInt(r.miny));
    assign(b->area, w, toInt(r.maxx - r.minx));
    assign(b->area, h, toInt(r.maxy - r.miny));
  } else
    clearArea(b->area);

  if ( adjustFirstArrowBezier(b) )
    unionNormalisedArea(b->area, b->first_arrow->area);
  if ( adjustSecondArrowBezier(b) )
    unionNormalisedArea(b->area, b->second_arrow->area);

  succeed;
}


static status
computeBezier(Bezier b)
{ if ( notNil(b->request_compute) )
  { CHANGING_GRAPHICAL(b,
		       { computeBoundingBoxBezier(b);
			 changedEntireImageGraphical(b);
		       });

    assign(b, request_compute, NIL);
  }

  succeed;
}


		 /*******************************
		 *	      REDRAW		*
		 *******************************/

static status
RedrawAreaBezier(Bezier b, Area a)
{ int x, y, w, h;
  ipoint pts[MAXPTS];
  int npts = MAXPTS;

  initialiseDeviceGraphical(b, &x, &y, &w, &h);

  r_thickness(valInt(b->pen));
  r_dash(b->texture);

  compute_points_bezier(b, pts, &npts);
  r_polygon(pts, npts, FALSE);

  if ( adjustFirstArrowBezier(b) )
    RedrawArea(b->first_arrow, a);
  if ( adjustSecondArrowBezier(b) )
    RedrawArea(b->second_arrow, a);

  return RedrawAreaGraphical(b, a);
}


static void
drawControlPt(Point pt)
{ int bx = valInt(pt->x);
  int by = valInt(pt->y);

  r_complement(bx-2, by-2, 5, 5);
}


static void
pt_line(Point p1, Point p2)
{ r_line(valInt(p1->x), valInt(p1->y),
	 valInt(p2->x), valInt(p2->y));
}


static status
paintSelectedBezier(Bezier b)
{ PceWindow sw = getWindowGraphical((Graphical) b);

  if ( sw && sw->selection_feedback == (Any) NAME_handles )
  { int x, y, w, h;

    initialiseDeviceGraphical(b, &x, &y, &w, &h);
    drawControlPt(b->start);
    drawControlPt(b->control1);
    if ( notNil(b->control2) )
      drawControlPt(b->control2);
    drawControlPt(b->end);

    if ( 1 )				/* draw lines */
    { r_dash(NAME_dotted);
      r_thickness(1);
      pt_line(b->start, b->control1);
      if ( notNil(b->control2) )
      { pt_line(b->control1, b->control2);
	pt_line(b->control2, b->end);
      } else
	pt_line(b->control1, b->end);
    }

    succeed;
  } else
    return paintSelectedGraphical((Graphical)b);
}



		 /*******************************
		 *	    PROPERTIES		*
		 *******************************/

static status
startBezier(Bezier b, Point pt)
{ return assignGraphical(b, NAME_start, pt);
}


static status
control1Bezier(Bezier b, Point pt)
{ return assignGraphical(b, NAME_control1, pt);
}


static status
control2Bezier(Bezier b, Point pt)
{ return assignGraphical(b, NAME_control2, pt);
}


static status
endBezier(Bezier b, Point pt)
{ return assignGraphical(b, NAME_end, pt);
}


static status
pointsBezier(Bezier b, Int sx, Int sy, Int ex, Int ey)
{ assign(b->start, x, sx);
  assign(b->start, y, sy);
  assign(b->end, x, ex);
  assign(b->end, y, ey);

  requestComputeGraphical(b, DEFAULT);

  CHANGING_GRAPHICAL(b,
		     { ComputeGraphical(b);
		       changedEntireImageGraphical(b);
		     });

  succeed;
}


static status
selectedBezier(Bezier b, Bool val)
{ return assignGraphical(b, NAME_selected, val);
}


		 /*******************************
		 *	PATH INTEGRATION	*
		 *******************************/

static status
setPointBezier(Bezier b, Point pt, Int x, Int y)
{ setPoint(pt, x, y);

  return requestComputeGraphical(b, DEFAULT);
}


static void
closerPoint(Point pt, Point pos, int *best, Point *r)
{ int d = valInt(getDistancePoint(pt, pos));

  if ( d < *best )
  { *best = d;
    *r = pt;
  }
}


static Point
getPointBezier(Bezier b, Point pos, Int dist)
{ Point rval = NIL;
  int bestd = 0;			/* make gcc happy */

  if ( instanceOfObject(pos, ClassEvent) && notNil(b->device) )
    pos = getPositionEvent((EventObj) pos, (Graphical) b->device);

  if ( isDefault(dist) )
    dist = toInt(10);			/* TBD */

  bestd = valInt(getDistancePoint(b->start, pos));
  rval  = b->start;
  closerPoint(b->end, pos, &bestd, &rval);
  closerPoint(b->control1, pos, &bestd, &rval);
  if ( notNil(b->control2) )
    closerPoint(b->control2, pos, &bestd, &rval);

  if ( notNil(rval) && bestd < valInt(dist) )
    answer(rval);

  fail;
}


		 /*******************************
		 *	      MOVING		*
		 *******************************/

static status
geometryBezier(Bezier b, Int x, Int y, Int w, Int h)
{ if ( notDefault(x) || notDefault(y) )
  { Int dx, dy;
    
    ComputeGraphical(b);
    if ( notDefault(x) )
      dx = sub(x, b->area->x);
    else
      dx = ZERO;

    if ( notDefault(y) )
      dy = sub(y, b->area->y);
    else
      dy = ZERO;
    
    if ( dx != ZERO || dy != ZERO )
    { offsetPoint(b->start, dx, dy);
      offsetPoint(b->end, dx, dy);
      offsetPoint(b->control1, dx, dy);
      if ( notNil(b->control2) )
	offsetPoint(b->control2, dx, dy);

      CHANGING_GRAPHICAL(b,
			 { assign(b->area, x, add(b->area->x, dx));
			   assign(b->area, y, add(b->area->y, dy));
			 });
    }
  }

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "start=point", "end=point",
	  "control1=point", "control2=[point]*"
	};
static char *T_points[] =
        { "start_x=int", "start_y=int", "end_x=int", "end_y=int" };
static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };
static char *T_setPoint[] =
        { "point=point", "x=[int]", "y=[int]" };
static char *T_point[] =
        { "near=point|event", "max_distance=[int]" };


/* Instance Variables */

static vardecl var_bezier[] =
{ SV(NAME_start, "point", IV_GET|IV_STORE, startBezier,
     NAME_tip, "Start point of Bezier curve"),
  SV(NAME_end, "point", IV_GET|IV_STORE, endBezier,
     NAME_tip, "End point of Bezier curve"),
  SV(NAME_control1, "point", IV_GET|IV_STORE, control1Bezier,
     NAME_appearance, "1st Control-point of the Bezier curve"),
  SV(NAME_control2, "point*", IV_GET|IV_STORE, control2Bezier,
     NAME_appearance, "2nd Control-point of the Bezier curve")
};

/* Send Methods */

static senddecl send_bezier[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseBezier,
     DEFAULT, "Create bezier from start, control and end"),
  SM(NAME_compute, 0, NULL, computeBezier,
     DEFAULT, "Recompute area"),
  SM(NAME_geometry, 4, T_geometry, geometryBezier,
     DEFAULT, "Move the Bezier curve"),
  SM(NAME_paintSelected, 0, NULL, paintSelectedBezier,
     NAME_appearance, "Paint inverted drops on control-points"),
  SM(NAME_points, 4, T_points, pointsBezier,
     NAME_tip, "Set start- and end-point"),
  SM(NAME_setPoint, 3, T_setPoint, setPointBezier,
     NAME_points, "Move (member) point to (X, Y)"),
  SM(NAME_selected, 1, "bool", selectedBezier,
     NAME_selection, "If @on, I'm selected"),
  SM(NAME_DrawPostScript, 0, NULL, drawPostScriptBezier,
     NAME_postscript, "Create PostScript")
};


/* Get Methods */

static getdecl get_bezier[] =
{ GM(NAME_point, 2, "point", T_point, getPointBezier,
     NAME_event, "Find closest point"),
};


/* Resources */

#define rc_bezier NULL
/*
static classvardecl rc_bezier[] =
{ 
};
*/

/* Class Declaration */

static Name bezier_termnames[] = { NAME_width, NAME_height };

ClassDecl(bezier_decls,
          var_bezier, send_bezier, get_bezier, rc_bezier,
          2, bezier_termnames,
          "$Rev$");


status
makeClassBezier(Class class)
{ declareClass(class, &bezier_decls);

  setRedrawFunctionClass(class, RedrawAreaBezier);

  succeed;
}
