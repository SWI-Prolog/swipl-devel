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

#define COS(x) cos(((x) * 2.0 * M_PI) / 360.0)
#define SIN(x) sin(((x) * 2.0 * M_PI) / 360.0)

static status
initialiseArc(Arc a, Int radius, Real start_angle, Real size_angle)
{ initialiseJoint((Joint) a, ZERO, ZERO, ZERO, ZERO, DEFAULT);

  if ( isDefault(radius) )
    radius = getResourceValueObject(a, NAME_radius);
  if ( isDefault(start_angle) )
    start_angle = CtoReal(0.0);
  if ( isDefault(size_angle) )
    size_angle = CtoReal(90.0);

  assign(a, size,	  newObject(ClassSize, radius, radius, 0));
  assign(a, position,	  newObject(ClassPoint, 0));
  assign(a, start_angle,  start_angle);
  assign(a, size_angle,	  size_angle);
  assign(a, close,	  NAME_none);

  return requestComputeGraphical(a, DEFAULT);
}


void
points_arc(Arc a, int *sx, int *sy, int *ex, int *ey)
{ int cx = valInt(a->position->x);
  int cy = valInt(a->position->y);
  float start = a->start_angle->value;
  float size = a->size_angle->value;
  

  if ( sx )
    *sx = cx + rfloat((float) valInt(a->size->w) * COS(start));
  if ( sy )
    *sy = cy - rfloat((float) valInt(a->size->h) * SIN(start));
  if ( ex )
    *ex = cx + rfloat((float) valInt(a->size->w) * COS(start + size));
  if ( ey )
    *ey = cy - rfloat((float) valInt(a->size->h) * SIN(start + size));
}


static status
RedrawAreaArc(Arc a, Area area)
{ int x, y, w, h;
  int ox, oy;
  int aw = valInt(a->size->w);
  int ah = valInt(a->size->h);
  int sx, sy, ex, ey;
  int cx, cy;

  initialiseDeviceGraphical(a, &x, &y, &w, &h);
  ox = x - valInt(a->area->x);
  oy = y - valInt(a->area->y);

  points_arc(a, &sx, &sy, &ex, &ey);
  sx += ox; sy += oy; ex += ox, ey += oy;
  cx = ox + valInt(a->position->x);
  cy = oy + valInt(a->position->y);

  r_thickness(valInt(a->pen));
  r_dash(a->texture);

#if !defined(__WINDOWS__) && !defined(__WIN32__)
  r_arcmode(a->close == NAME_none ? NAME_pieSlice : a->close);
  r_arc(valInt(a->position->x) + ox - aw, valInt(a->position->y) + oy - ah,
	2*aw, 2*ah,
	rfloat(a->start_angle->value*64), rfloat(a->size_angle->value*64),
	a->fill_pattern);

  if ( a->close != NAME_none && a->pen != ZERO )
  { if ( a->close == NAME_chord )
    { r_line(sx, sy, ex, ey);
    } else /* if ( a->close == NAME_pieSlice ) */
    { r_line(cx, cy, sx, sy);
      r_line(cx, cy, ex, ey);
    }
  }

#else

  r_msarc(valInt(a->position->x) + ox - aw, valInt(a->position->y) + oy - ah,
	  2*aw, 2*ah,
	  sx, sy, ex, ey,
	  a->close, a->fill_pattern);
#endif

  if (notNil(a->first_arrow))
  { if ( a->size_angle->value >= 0.0 )
      paintArrow(a->first_arrow, toInt(sx), toInt(sy),
		 toInt(sx+(sy-cy)), toInt(sy-(sx-cx)));
    else
      paintArrow(a->first_arrow, toInt(sx), toInt(sy),
		 toInt(sx-(sy-cy)), toInt(sy+(sx-cx)));
  }
  if (notNil(a->second_arrow))
  { if ( a->size_angle->value >= 0.0 )
      paintArrow(a->second_arrow, toInt(ex), toInt(ey),
	       toInt(ex-(ey-cy)), toInt(ey+(ex-cx)));
    else
      paintArrow(a->second_arrow, toInt(ex), toInt(ey),
	       toInt(ex+(ey-cy)), toInt(ey-(ex-cx)));
  }

  return RedrawAreaGraphical(a, area);
}


static status
angleInArc(Arc a, int angle)
{ int start = rfloat(a->start_angle->value);
  int size  = rfloat(a->size_angle->value);

  if ( size < 0 )
  { start += size;
    size = -size;
  }
  while(start < 0)
    start += 360;
  start = start % 360;

  if ( (angle >= start && angle <= start + size) ||
       (angle <  start && angle <= start + size - 360) )
    succeed;

  fail;
}


static status
computeArc(Arc a)
{ if ( notNil(a->request_compute) )
  { int minx, miny, maxx, maxy;
    int sx, sy, ex, ey;
    int px = valInt(a->position->x);
    int py = valInt(a->position->y);
    int sw = valInt(a->size->w);
    int sh = valInt(a->size->h);

    points_arc(a, &sx, &sy, &ex, &ey);
    minx = min(sx, ex);
    maxx = max(sx, ex);
    miny = min(sy, ey);
    maxy = max(sy, ey);

    if ( angleInArc(a, 0) )
      maxx = max(maxx, px + sw);
    if ( angleInArc(a, 90) )
      miny = min(miny, py - sh);
    if ( angleInArc(a, 180) )
      minx = min(minx, px - sw);
    if ( angleInArc(a, 270) )
      maxy = max(maxy, py + sh);

    if ( a->close == NAME_pieSlice ||
	 (a->close == NAME_none && notNil(a->fill_pattern)) )
    { maxx = max(maxx, px);
      minx = min(minx, px);
      miny = min(miny, py);
      maxy = max(maxy, py);
    }

    CHANGING_GRAPHICAL(a,
		       { setArea(a->area, toInt(minx), toInt(miny),
			       	          toInt(maxx-minx), toInt(maxy-miny));
		         changedEntireImageGraphical(a);
		       });

    assign(a, request_compute, NIL);
  }

  succeed;
}


static status
geometryArc(Arc a, Int x, Int y, Int w, Int h)
{ Int ox, oy;

  ox = isDefault(x) ? ZERO : sub(x, a->area->x);
  oy = isDefault(y) ? ZERO : sub(y, a->area->y);

  CHANGING_GRAPHICAL(a,
		     offsetPoint(a->position, ox, oy);
		     requestComputeGraphical(a, DEFAULT));

  succeed;
}


static status
setArc(Arc a, Int x, Int y, Int radius, float start, float size)
{ int changed = 0;

  if ( a->position->x != x || a->position->y != y )
  { setPoint(a->position, x, y);
    changed++;
  }
  if ( a->size->w != radius || a->size->h != radius )
  { setSize(a->size, radius, radius);
    changed++;
  }

  if ( a->start_angle->value != start || a->size_angle->value != size )
  { a->start_angle->value = start;
    a->size_angle->value = size;
    changed++;
  }
  
  if ( changed )
    requestComputeGraphical(a, DEFAULT);

  succeed;
}


static status
radiusArc(Arc a, Int r)
{ if ( a->size->w != r || a->size->h != r )
  { setSize(a->size, r, r);
    requestComputeGraphical(a, DEFAULT);
  }

  succeed;
}


static status
positionArc(Arc a, Point pos)
{ if ( !equalPoint(a->position, pos) )
  { copyPoint(a->position, pos);
    requestComputeGraphical(a, DEFAULT);
  }

  succeed;
}


static status
resizeArc(Arc a, Real xfactor, Real yfactor, Point origin)
{ float xf, yf;
  int ox = valInt(a->position->x);
  int oy = valInt(a->position->y);
  int nx, ny, nw, nh;

  init_resize_graphical(a, xfactor, yfactor, origin, &xf, &yf, &ox, &oy);
  if ( xf == 1.0 && yf == 1.0 )
    succeed;

  nx = ox + rfloat((float) (valInt(a->position->x)-ox) * xf);
  ny = oy + rfloat((float) (valInt(a->position->y)-oy) * yf);
  nw = rfloat((float) valInt(a->size->w) * xf);
  nh = rfloat((float) valInt(a->size->h) * yf);

  setSize(a->size, toInt(nw), toInt(nh));
  setPoint(a->position, toInt(nx), toInt(ny));

  return requestComputeGraphical(a, DEFAULT);
}


static Int
getRadiusArc(Arc a)
{ answer(a->size->w);
}


static Point
getStartArc(Arc a)
{ int sx, sy;

  points_arc(a, &sx, &sy, NULL, NULL);
  answer(answerObject(ClassPoint, toInt(sx), toInt(sy)));
}


static Point
getEndArc(Arc a)
{ int ex, ey;

  points_arc(a, NULL, NULL, &ex, &ey);
  answer(answerObject(ClassPoint, toInt(ex), toInt(ey)));
}


static status
sizeArc(Arc a, Size sz)
{ if ( !equalSize(a->size, sz) )
  { copySize(a->size, sz);
    requestComputeGraphical(a, DEFAULT);
  }

  succeed;
}


static status
startAngleArc(Arc a, Real s)
{ if ( a->start_angle->value != s->value )
  { valueReal(a->start_angle, s);
    requestComputeGraphical(a, DEFAULT);
  }

  succeed;
}


static status
sizeAngleArc(Arc a, Real e)
{ if ( a->size_angle->value != e->value )
  { valueReal(a->size_angle, e);
    requestComputeGraphical(a, DEFAULT);
  }

  succeed;
}


static status
endAngleArc(Arc a, Real e)
{ float size = e->value - a->start_angle->value;
  if ( size < 0.0 )
    size += 360.0;

  if ( a->size_angle->value != size )
  { a->size_angle->value = size;
    requestComputeGraphical(a, DEFAULT);
  }

  succeed;
}


static status
closeArc(Arc a, Name how)
{ if ( a->close != how )
  { assign(a, close, how);
    requestComputeGraphical(a, DEFAULT);
  }

  succeed;
}



		/********************************
		*        SPECIFICATIONS		*
		********************************/

static status
connectAngleArc(Arc a, Line l1, Line l2)
{ Point is;

  if ( !(is = getIntersectionLine(l1, l2)) )
    fail;				/* no intersection */

  positionArc(a, is);
  startAngleArc(a, getAngleLine(l1, is));
  endAngleArc(a, getAngleLine(l2, is));
  doneObject(is);

  succeed;
}


static status
pointsArc(Arc a, Int Sx, Int Sy, Int Ex, Int Ey, Int D)
{ int sx, sy, ex, ey, d;
  int cx, cy;
  int dx, dy;
  int l;
  int radius;
  float start, end, size;

  sx = valInt(Sx), sy = valInt(Sy), ex = valInt(Ex), ey = valInt(Ey);
  d = valInt(D);

  cx = (sx + ex + 1)/2;
  cy = (sy + ey + 1)/2;

  dx = ex - sx;
  dy = ey - sy;
  l = isqrt(dx*dx + dy*dy);
  dx *= d/l;
  dy *= d/l;

  cx -= dy;				/* center of circle */
  cy += dx;
  radius = isqrt((cx-sx)*(cx-sx) + (cy-sy)*(cy-sy));

  if ( ex != cx )
  { start = atan2((float)(cy-ey), (float)(ex-cx));
    if ( start < 0.0 )
      start = 2.0 * M_PI + start;
    start = (start * 180.0) / M_PI;

    end = atan2((float)(cy-sy), (float)(sx-cx));
    if ( end < 0.0 )
      end = 2.0 * M_PI + end;
    end = (end * 180.0) / M_PI;
  } else
  { start = end = 0.0;
  }

  if ( d < 0 )
  { float x = end;
    end = start;
    start = x;
  }

  size = end - start;
  if ( size < 0.0 )
    size += 360.0;

  if ( d > 0 )
  { start += size;
    size = -size;
  }

  return setArc(a, toInt(cx), toInt(cy), toInt(radius), start, size);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static const char *T_connectAngle[] =
        { "line", "line" };
static const char *T_initialise[] =
        { "radius=[int]", "start=[real]", "size=[real]" };
static const char *T_resize[] =
        { "real", "[real]", "[point]" };
static const char *T_points[] =
        { "start_x=int", "start_y=int", "end_x=int", "end_y=int", "curvature=int" };
static const char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };

/* Instance Variables */

static const vardecl var_arc[] =
{ SV(NAME_position, "point", IV_GET|IV_STORE, positionArc,
     NAME_area, "Position of the arc"),
  SV(NAME_size, "size", IV_GET|IV_STORE, sizeArc,
     NAME_area, "Size of the ellipse I'm part of"),
  SV(NAME_startAngle, "real", IV_GET|IV_STORE, startAngleArc,
     NAME_pie, "Start angle (degrees)"),
  SV(NAME_sizeAngle, "real", IV_GET|IV_STORE, sizeAngleArc,
     NAME_pie, "Size (degrees)"),
  SV(NAME_close, "{none,pie_slice,chord}", IV_GET|IV_STORE, closeArc,
     NAME_appearance, "How the arc is closed"),
  SV(NAME_fillPattern, "image|colour*", IV_GET|IV_STORE, fillPatternGraphical,
     NAME_appearance, "Fill pattern for the slice")
};

/* Send Methods */

static const senddecl send_arc[] =
{ SM(NAME_compute, 0, NULL, computeArc,
     DEFAULT, "Compute the bounding box area"),
  SM(NAME_geometry, 4, T_geometry, geometryArc,
     DEFAULT, "Only move the arc"),
  SM(NAME_initialise, 3, T_initialise, initialiseArc,
     DEFAULT, "Create Arc from radius, start_angle and size_angle (degrees)"),
  SM(NAME_resize, 3, T_resize, resizeArc,
     DEFAULT, "Resize arc with specified factor"),
  SM(NAME_connectAngle, 2, T_connectAngle, connectAngleArc,
     NAME_area, "Connect both lines with an angle"),
  SM(NAME_radius, 1, "int", radiusArc,
     NAME_dimension, "->width and ->height"),
  SM(NAME_endAngle, 1, "real", endAngleArc,
     NAME_pie, "Set ->size_angle to argument - <-start_angle"),
  SM(NAME_DrawPostScript, 0, NULL, drawPostScriptArc,
     NAME_postscript, "Create PostScript"),
  SM(NAME_points, 5, T_points, pointsArc,
     NAME_tip, "Arc between two points")
};

/* Get Methods */

static const getdecl get_arc[] =
{ GM(NAME_radius, 0, "int", NULL, getRadiusArc,
     NAME_area, "Equivalent to <-width"),
  GM(NAME_end, 0, "point", NULL, getEndArc,
     NAME_tip, "End position of arc"),
  GM(NAME_start, 0, "point", NULL, getStartArc,
     NAME_tip, "Start position of arc")
};

/* Resources */

static const resourcedecl rc_arc[] =
{ RC(NAME_radius, "int", "30",
     "Default radius")
};

/* Class Declaration */

static Name arc_termnames[] = { NAME_radius, NAME_startAngle, NAME_sizeAngle };

ClassDecl(arc_decls,
          var_arc, send_arc, get_arc, rc_arc,
          3, arc_termnames,
          "$Rev$");


status
makeClassArc(Class class)
{ declareClass(class, &arc_decls);

  cloneStyleVariableClass(class, NAME_fillPattern, NAME_reference);
  setRedrawFunctionClass(class, RedrawAreaArc);

  succeed;
}
