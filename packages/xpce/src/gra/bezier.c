/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

#define MAXPTS 100

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Implementation of Quadratic Bezier Curves.  Using algorithm description
from:

http://graphics.cs.ucdavis.edu/CAGDNotes/Divide-and-Conquer-Bezier-Curve/Divide-and-Conquer-Bezier-Curve.html
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct bezier *Bezier;

NewClass(bezier)
  ABSTRACT_JOINT
  Point	start;
  Point control;
  Point end;
End;

static status
initialiseBezier(Bezier b,
		 Point start, Point control, Point end)
{ initialiseGraphical(b, ZERO, ZERO, ZERO, ZERO);

  assign(b, start,   start);
  assign(b, control, control);
  assign(b, end,     end);

  return requestComputeGraphical(b, DEFAULT);
}


		 /*******************************
		 *	     COMPUTING		*
		 *******************************/

static void
shiftpts(IPoint pts, int to)
{ 
  DEBUG(NAME_bezier, Cprintf("Shift to %d\n", to));
  to--;
  for(; to>3; to--)
    pts[to] = pts[to-2];
}


static int
split(IPoint pts, int i, int *n)
{ int mx, my;
  int md = 1;

  pts += i;
  mx = (pts[0].x + pts[2].x + 1)/2;
  my = (pts[0].y + pts[2].y + 1)/2;
  
  if ( abs(mx-pts[1].x) > md || abs(my-pts[1].y) > md )
  { int m1x, m1y;
    int m2x, m2y;
    int mmx, mmy;

    m1x = (pts[0].x + pts[1].x + 1)/2;
    m1y = (pts[0].y + pts[1].y + 1)/2;
    m2x = (pts[1].x + pts[2].x + 1)/2;
    m2y = (pts[1].y + pts[2].y + 1)/2;
    mmx = (m1x + m2x + 1)/2;
    mmy = (m1y + m2y + 1)/2;

    *n = *n + 2;
    shiftpts(pts, *n);
    pts[1].x = m1x;
    pts[1].y = m1y;
    pts[2].x = mmx;
    pts[2].y = mmy;
    pts[3].x = m2x;
    pts[3].y = m2y;

    return TRUE;
  }

  return FALSE;
}


static void
compute_points_bezier(Bezier b, IPoint pts, int *mx)
{ int mxpts = *mx;
  int npts = 3;
  int i;

  pts[0].x = valInt(b->start->x);
  pts[0].y = valInt(b->start->y);
  pts[1].x = valInt(b->control->x);
  pts[1].y = valInt(b->control->y);
  pts[2].x = valInt(b->end->x);
  pts[2].y = valInt(b->end->y);

  for(i=0; i <= npts-3; i += 2)
  { if ( npts >= mxpts-2 )
      break;
    while(split(pts, i, &npts))
    { DEBUG(NAME_bezier,
	    { int n;

	      Cprintf("Splitting to: ");
	      for(n=0; n<npts; n++)
	      { Cprintf(" %d,%d", pts[n].x, pts[n].y);
	      }
	      Cprintf("\n");
	    })
    }
  }

  *mx = npts;
}


static status
computeBezier(Bezier b)
{ if ( notNil(b->request_compute) )
  { int minx = 1000000, miny = 1000000, maxx = -1000000, maxy = -10000000;

    if ( b->selected == ON )
    { int mw=5, mh=5;

      minx = min(minx, valInt(b->start->x));
      minx = min(minx, valInt(b->control->x));
      minx = min(minx, valInt(b->end->x));

      miny = min(miny, valInt(b->start->y));
      miny = min(miny, valInt(b->control->y));
      miny = min(miny, valInt(b->end->y));

      maxx = max(maxx, valInt(b->start->x));
      maxx = max(maxx, valInt(b->control->x));
      maxx = max(maxx, valInt(b->end->x));

      maxy = max(maxy, valInt(b->start->y));
      maxy = max(maxy, valInt(b->control->y));
      maxy = max(maxy, valInt(b->end->y));

      minx -= (mw+1)/2;
      maxx += (mw+1)/2;
      miny -= (mh+1)/2;
      maxy += (mh+1)/2;
    } else
    { ipoint ptsbuf[MAXPTS];
      IPoint pts = ptsbuf;
      int npts = MAXPTS;
      int i;

      compute_points_bezier(b, pts, &npts);

      for(i=0; i<npts; i++,pts++)
      { if ( pts[0].x < minx ) minx = pts[0].x;
	if ( pts[0].x > maxx ) maxx = pts[0].x;
	if ( pts[0].y < miny ) miny = pts[0].y;
	if ( pts[0].y > maxy ) maxy = pts[0].y;
      }
    }

    if ( maxx >= minx && maxy >= miny )
    { int pens = valInt(b->pen) / 2;
      int pena = (valInt(b->pen) % 2 == 0 ? pens : pens + 1);
  
      minx -= pens; maxx += pena;
      miny -= pens; maxy += pena;
  
      assign(b->area, x, toInt(minx));
      assign(b->area, y, toInt(miny));
      assign(b->area, w, toInt(maxx - minx));
      assign(b->area, h, toInt(maxy - miny));
    } else
      clearArea(b->area);

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

  return RedrawAreaGraphical(b, a);
}


static void
drawControlPt(Point pt)
{ int bx = valInt(pt->x);
  int by = valInt(pt->y);

  r_complement(bx-2, by-2, 5, 5);
}


static status
paintSelectedBezier(Bezier b)
{ PceWindow sw = getWindowGraphical((Graphical) b);

  if ( sw && sw->selection_feedback == (Any) NAME_handles )
  { int x, y, w, h;

    initialiseDeviceGraphical(b, &x, &y, &w, &h);
    drawControlPt(b->start);
    drawControlPt(b->control);
    drawControlPt(b->end);

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
controlBezier(Bezier b, Point pt)
{ return assignGraphical(b, NAME_control, pt);
}


static status
endBezier(Bezier b, Point pt)
{ return assignGraphical(b, NAME_end, pt);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "start=point", "control=point", "end=[point]" };

/* Instance Variables */

static vardecl var_bezier[] =
{ SV(NAME_start, "point", IV_GET|IV_STORE, startBezier,
     NAME_tip, "Start point of Bezier curve"),
  SV(NAME_control, "point", IV_GET|IV_STORE, controlBezier,
     NAME_appearance, "Control-point of the Bezier curve"),
  SV(NAME_end, "point", IV_GET|IV_STORE, endBezier,
     NAME_tip, "End point of Bezier curve")
};

/* Send Methods */

static senddecl send_bezier[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseBezier,
     DEFAULT, "Create bezier from start, control and end"),
  SM(NAME_compute, 0, NULL, computeBezier,
     DEFAULT, "Recompute area"),
  SM(NAME_paintSelected, 0, NULL, paintSelectedBezier,
     NAME_appearance, "Paint inverted drops on control-points")
/*SM(NAME_DrawPostScript, 0, NULL, drawPostScriptBezier,
     NAME_postscript, "Create PostScript")*/
};

/* Get Methods */

#define get_bezier NULL
/*
static getdecl get_bezier[] =
{ 
};
*/

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
