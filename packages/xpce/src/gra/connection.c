/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <math.h>

forwards int	bestConnectionPoint(Device, Name, int, int,
				    Graphical, Handle *, int *, int *);

static status
initialiseConnection(Connection c, Graphical from, Graphical to,
		     Link link, Name from_handle, Name to_handle)
{ static Link default_link = NIL;

  TRY(initialiseLine((Line) c, ZERO, ZERO, ZERO, ZERO, DEFAULT));

  if ( isDefault(link) )
  { if ( isNil(default_link) )
      default_link = globalObject(NAME_defaultLink, ClassLink, 0);
    link = default_link;
  }

  copyLine((Line) c, link->line);
  
  assign(c, link,        link);
  assign(c, from_handle, isDefault(from_handle) ? (Name) NIL : from_handle);
  assign(c, to_handle,   isDefault(to_handle)   ? (Name) NIL : to_handle);
  assign(c, fixed_from,  isDefault(from_handle) ? OFF : ON);
  assign(c, fixed_to,    isDefault(to_handle)   ? OFF : ON);
  
  attachConnectionGraphical(from, c);
  attachConnectionGraphical(to, c);
  assign(c, from, from);
  assign(c, to,   to);
  return updateDeviceConnection(c);
}


static status
unlinkConnection(Connection c)
{ if ( notNil(c->from) ) detachConnectionGraphical(c->from, c);
  if ( notNil(c->to) )   detachConnectionGraphical(c->to, c);

  return unlinkGraphical((Graphical) c);
}


static status
updateLineConnection(Connection c, Int x1, Int y1, Int x2, Int y2)
{ if ( x1 != getStartXLine((Line) c) ||
       y1 != getStartYLine((Line) c) ||
       x2 != getEndXLine((Line) c) ||
       y2 != getEndYLine((Line) c) )
    send(c, NAME_points, x1, y1, x2, y2, 0);

  succeed;
}


#define NO_POINTS	0
#define SAME_POINTS	1
#define POINTS_CHANGED	2

static status
getConnectionPointsConnection(Connection c, Graphical from, Graphical to,
			      int *x1, int *y1, int *x2, int *y2)
{ int cxfrom, cyfrom;
  Device dev = c->device;
  Handle hf = FAIL, ht = FAIL;
  int hf_ok = FALSE, ht_ok = FALSE;

  if ( notNil(c->from_handle) )
    hf = getHandleGraphical(from, c->from_handle);
  if ( notNil(c->to_handle) )
    ht = getHandleGraphical(to, c->to_handle);

  if ( c->fixed_from == ON && hf != FAIL )
  { *x1 = valInt(getXHandle(hf, from, dev));
    *y1 = valInt(getYHandle(hf, from, dev));
    hf_ok = TRUE;
  }

  if ( c->fixed_to == ON && ht != FAIL )
  { *x2 = valInt(getXHandle(ht, to, dev));
    *y2 = valInt(getYHandle(ht, to, dev));
    ht_ok = TRUE;
  }

  if ( hf_ok && ht_ok )			/* both fixed and available */
    return POINTS_CHANGED;
  if ( hf_ok && !ht_ok )		/* `to_handle' needs to be fixed */
  { if ( !bestConnectionPoint(dev, c->link->to, *x1, *y1, to, &ht, x2, y2) )
      return NO_POINTS;
    assign(c, to_handle, ht->name);
    return POINTS_CHANGED;
  }
  if ( !hf_ok && ht_ok )		/* `from_handle' needs to be fixed */
  { if ( !bestConnectionPoint(dev, c->link->from, *x2, *y2, from, &hf, x1, y1))
      return NO_POINTS;
    assign(c, from_handle, hf->name);
    return POINTS_CHANGED;
  }

  if ( hf != FAIL && ht != FAIL )
  { if ( getXHandle(hf, from, dev) == getStartXLine((Line) c) &&
	 getYHandle(hf, from, dev) == getStartYLine((Line) c) &&
	 getXHandle(ht, to,   dev) == getEndXLine((Line) c) &&
	 getYHandle(ht, to,   dev) == getEndYLine((Line) c) &&
	 hf->name == c->link->from &&
	 ht->name == c->link->to )
      return SAME_POINTS;
  }

  cxfrom = valInt(getAbsoluteXGraphical(from,dev)) + valInt(from->area->w)/2;
  cyfrom = valInt(getAbsoluteYGraphical(from,dev)) + valInt(from->area->h)/2;

  DEBUG(NAME_absolutePosition,
	Cprintf("getConnectionPointsConnection(): dev=%s\n", pp(dev)));

  TRY(bestConnectionPoint(dev, c->link->to, cxfrom, cyfrom, to, &ht, x2, y2));
  TRY(bestConnectionPoint(dev, c->link->from, *x2, *y2, from, &hf, x1, y1));
  TRY(bestConnectionPoint(dev, c->link->to, *x1, *y1, to, &ht, x2, y2));

  assign(c, from_handle, hf->name);
  assign(c, to_handle,   ht->name);

  return POINTS_CHANGED;
}


/* (JW)	Determine the best point to link up with a handle of specified
	type.  It assumes the connection line starts at (x, y).
 */

static int
bestConnectionPoint(Device dev, Name kind, int x, int y,
		    Graphical gr, Handle *hp, int *xp, int *yp)
{ int bestx = 0, besty = 0, bestd=10000000, bestdc=10000000;
  int cx, cy;
  int X, Y, D, DC;
  int found = FAIL;
  Cell cell;

  DEBUG(NAME_handle,
	Cprintf("bestConnectionPoint(%s, %s, %d, %d, %s) --> ",
		pp(dev), pp(kind), x, y, pp(gr)));

#define FindAHandle \
  { Handle h = cell->value; \
    if ( h->kind == kind ) \
    { if ( !found ) \
      { *hp = h; \
	found = SUCCEED; \
      } else \
	goto findbest; \
    } \
  }


  if ( notNil(gr->handles) )
  { for_cell(cell, gr->handles)
      FindAHandle;
  }
  if ( notNil(classOfObject(gr)->handles) )
  { for_cell(cell, classOfObject(gr)->handles)
      FindAHandle;
  }

  if ( found )
  { Int hx, hy;
    getXYHandle(*hp, gr, dev, &hx, &hy);

    *xp = valInt(hx);
    *yp = valInt(hy);

    DEBUG(NAME_handle, Cprintf("%s, %d, %d\n", pp((*hp)->name), *xp, *yp));
    succeed;
  }

findbest:

  cx = valInt(getAbsoluteXGraphical(gr, dev)) + valInt(gr->area->w)/2;
  cy = valInt(getAbsoluteYGraphical(gr, dev)) + valInt(gr->area->h)/2;

#define FindBestHandle \
  { Handle h = cell->value; \
    Int hx, hy; \
    if ( h->kind != kind ) \
      continue; \
    getXYHandle(h, gr, dev, &hx, &hy); \
    X = valInt(hx); Y = valInt(hy); \
    D = isqrt((x-X)*(x-X) + (y-Y)*(y-Y)); \
    DC = distanceLineToPoint(x, y, X, Y, cx, cy); \
    if ((D + DC < bestd + bestdc) || found == FAIL) \
    { bestd = D; \
      bestdc = DC; \
      bestx = X; \
      besty = Y; \
      *hp = h; \
      found = SUCCEED; \
    } \
  }

  if ( notNil(gr->handles) )
  { for_cell(cell, gr->handles)
      FindBestHandle;
  }
  if ( notNil(classOfObject(gr)->handles) )
  { for_cell(cell, classOfObject(gr)->handles)
      FindBestHandle;
  }

  if ( found == FAIL )
  { DEBUG(NAME_handle, Cprintf("FAIL\n"));
    fail;
  }

  DEBUG(NAME_handle, Cprintf("%s, %d, %d\n", pp((*hp)->name), bestx, besty));

  *xp = bestx;
  *yp = besty;

  return found;
}


status
updateHideExposeConnection(Connection c)
{ Device ldev;

  if ( isNil(ldev = c->device) )
    succeed;

  if ( ldev == c->from->device && ldev == c->to->device )
  { if ( beforeChain(ldev->graphicals, c->from, c->to) )
      exposeGraphical(c, c->to);
    else
      exposeGraphical(c, c->from);
  } else
    exposeGraphical(c, DEFAULT);

  succeed;
}


static status
computeConnection(Connection c)
{ if ( notNil(c->request_compute) )
  { Graphical from = c->from;
    Graphical to = c->to;
    Device dev = c->device;

    assign(c, request_compute, NIL);

    if ( getIsDisplayedGraphical(from, dev) == ON &&
	 getIsDisplayedGraphical(to, dev) == ON )
    { int x1, y1, x2, y2;

      switch( getConnectionPointsConnection(c, from, to, &x1, &y1, &x2, &y2) )
      { case POINTS_CHANGED:
	  updateLineConnection(c, toInt(x1), toInt(y1), toInt(x2), toInt(y2));
	  /*FALLTHROUGH*/
	case SAME_POINTS:
	  return DisplayedGraphical(c, ON);
	case NO_POINTS:
	  break;
      }
    }

    return DisplayedGraphical(c, OFF);
  }

  succeed;
}


status
updateDeviceConnection(Connection c)
{ Graphical from = c->from;
  Graphical to = c->to;
  Device device;

  if ( isNil(from) || isNil(to) ||
       (device = getCommonDeviceGraphical(c->from, c->to)) == FAIL )
    return DeviceGraphical(c, NIL);

  DeviceGraphical(c, device);
  return requestComputeGraphical(c, DEFAULT);
}


static status
updateLinkAttributesConnection(Connection c)
{ Line proto = c->link->line;

  CHANGING_GRAPHICAL(c,
	assign(c, texture, proto->texture);
	assign(c, pen, proto->pen);
	setArrowsJoint((Joint) c, proto->first_arrow, proto->second_arrow);
	changedEntireImageGraphical(c));

  return requestComputeGraphical(c, DEFAULT);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Connections cannot be moved themselves.  The only method understood by
a connection that changes it's start and end-position is ->points.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static in_points = 0;

static status
pointsConnection(Connection c, Int x1, Int y1, Int x2, Int y2)
{ in_points++;
  pointsLine((Line) c, x1, y1, x2, y2);
  in_points--;

  succeed;
}


static status
geometryConnection(Connection c, Int x, Int y, Int w, Int h)
{ if ( in_points > 0 )
    return geometryGraphical(c, x, y, w, h);

  succeed;
}


static Graphical
getOppositeConnection(Connection c, Graphical gr)
{ if ( c->to == gr )
    answer(c->from);
  if ( c->from == gr )
    answer(c->to);

  fail;
}


status
makeClassConnection(Class class)
{ sourceClass(class, makeClassConnection, __FILE__, "$Revision$");

  localClass(class, NAME_link, NAME_relation, "link", NAME_get,
	     "Generic definition of the link");
  localClass(class, NAME_from, NAME_relation, "graphical", NAME_get,
	     "Graphical at `from' side");
  localClass(class, NAME_to, NAME_relation, "graphical", NAME_get,
	     "Graphical at `to' side");
  localClass(class, NAME_fromHandle, NAME_relation, "name*", NAME_get,
	     "Name of 1st handle link is connected to");
  localClass(class, NAME_toHandle, NAME_relation, "name*", NAME_get,
	     "Name of 2nd handle link is connected to");
  localClass(class, NAME_fixedFrom, NAME_relation, "bool", NAME_get,
	     "From side is fixed");
  localClass(class, NAME_fixedTo, NAME_relation, "bool", NAME_get,
	     "To side is fixed");

  termClass(class, "connection",
	    5, NAME_from, NAME_to, NAME_link, NAME_fromHandle, NAME_toHandle);
  cloneStyleClass(class, NAME_relation);

  sendMethod(class, NAME_initialise, DEFAULT,
	     5, "from=graphical", "to=graphical", "link=[link]",
	        "handle_from=[name]*", "handle_to=[name]*",
	     "Create from graphicals, link and handle names",
	     initialiseConnection);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Detach from graphicals",
	     unlinkConnection);
  sendMethod(class, NAME_geometry, DEFAULT, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Do nothing: constrained by connected graphicals",
	     geometryConnection);
  sendMethod(class, NAME_compute, DEFAULT, 0,
	     "Recompute the line",
	     computeConnection);
  sendMethod(class, NAME_points, DEFAULT, 4,
	     "start_x=[int]", "start_y=[int]", "end_x=[int]", "end_y=[int]",
	     "Set X1, Y1, X2, Y2",
	     pointsConnection);
  sendMethod(class, NAME_updateLinkAttributes, NAME_update, 0,
	     "Re-read the link properties",
	     updateLinkAttributesConnection);

  getMethod(class, NAME_opposite, NAME_relation, "graphical", 1, "graphical",
	    "Other side of the connection",
	    getOppositeConnection);

  distanceLineToPoint(0, 0, 10, 10, 0, 10);	/* initialise */

  succeed;
}



/* (JW)	Calculate the distance between the infinite extended line through
	(x1, y1) and (x2, y2) to the point (px, py).
 */
#if 0
static
int
distanceLineToPoint(x1, y1, x2, y2, px, py)
int x1, y1, x2, y2, px, py;
{ float a;

  if (y1 == y2)
    return abs(y1 - py);
  if (x1 == x2)
    return abs(x1 - px);

  a = ((float)(y2 - y1)) / ((float)(x2 - x1));
  return abs(rfloat((((float)(px - x1)) * a + ((float)(y1 - py))) /
					 sqrt(1.0 + a*a)));
}
#endif

#define STEP 20
#define ENTRIES 200
#define EPS ((float) (ENTRIES / STEP))
#define EMS (ENTRIES * STEP)

int					/* Must be more precise!!! TBD */
distanceLineToPoint(int x1, int y1, int x2, int y2, int px, int py)
{ static int atable[ENTRIES+1];
  static int done = FALSE;
  int a, d;

  if (y1 == y2)
    return abs(y1 - py);
  if (x1 == x2)
    return abs(x1 - px);

  if ( !done )
  { int i;

    for(i=0; i <= ENTRIES; i++)
      atable[i] = rfloat((float) ENTRIES * sqrt(1.0 + (i/EPS)*(i/EPS)));

    done = TRUE;
  }

  a = (ENTRIES*(y2-y1)) / (x2-x1);
  if ( a < -EMS )
  { a = -EMS;
  } else
  { if ( a > EMS )
      a = EMS;
  }

  d = (a*(px-x1) + ENTRIES*(y1-py)) / atable[abs(a)/STEP];

  return abs(d);
}
