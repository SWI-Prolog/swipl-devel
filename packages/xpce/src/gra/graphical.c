/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <math.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

static int	distance_area(int, int, int, int, int, int, int, int);
static status	orientationGraphical(Graphical gr, Name orientation);
static Point	getCenterGraphical(Graphical gr);
static status	updateHideExposeConnectionsGraphical(Graphical gr);


		/********************************
		*         CREATE/DESTROY	*
		********************************/


status
initialiseGraphical(Any obj, Int x, Int y, Int w, Int h)
{ Graphical gr = obj;
  Class class = classOfObject(gr);

  assign(gr, displayed,       OFF);
  assign(gr, device,          NIL);
  assign(gr, area,            newObject(ClassArea, 0));
  assign(gr, pen,             ONE);
  assign(gr, texture,         NAME_none);
  assign(gr, colour,          getResourceValueObject(obj, NAME_colour));
  assign(gr, selected,        OFF);
  assign(gr, name,            class->name);
  assign(gr, handles,         NIL);
  assign(gr, inverted,        OFF);
  assign(gr, active,	      ON);
  assign(gr, cursor,          NIL);
  assign(gr, request_compute, NIL);
  if ( class->solid == ON )
    setFlag(gr, F_SOLID);

  setArea(gr->area, x, y, w, h);
  succeed;
}


status
unlinkGraphical(Graphical gr)
{ disconnectGraphical(gr, DEFAULT, DEFAULT, DEFAULT, DEFAULT);
  DeviceGraphical(gr, NIL);

  succeed;
}


status
copyGraphical(Any obj1, Any obj2)
{ Graphical gr1 = obj1;
  Graphical gr2 = obj2;

  copyArea(gr1->area, gr2->area);
  assign(gr1, device,    gr2->device);
  assign(gr1, pen,       gr2->pen);
  assign(gr1, texture,   gr2->texture);
  assign(gr1, handles,   gr2->handles);
  assign(gr1, selected,  gr2->selected);
  assign(gr1, inverted,  gr2->inverted);
  assign(gr1, displayed, gr2->displayed);
  assign(gr1, colour,	 gr2->colour);
  assign(gr1, cursor,    gr2->cursor);
  assign(gr1, name,      gr2->name);

  succeed;
}

		/********************************
		*           CONVERT		*
		********************************/

static Graphical
getConvertGraphical(Class class, Any obj)
{ Graphical gr;

  if ( isObject(obj) &&
       hasGetMethodObject(obj, NAME_image) &&
       (gr = get(obj, NAME_image, 0)) &&
       instanceOfObject(gr, ClassGraphical) )
    answer(gr);

  fail;
}


		/********************************
		*        DISPLAY/ERASE		*
		********************************/


static status
displayOnGraphical(Graphical gr, Device dev)
{ TRY( DeviceGraphical(gr, dev) );

  return DisplayedGraphical(gr, ON);
}


status
DeviceGraphical(Any obj, Device dev)
{ Graphical gr = obj;

  if ( gr->device == dev )
    succeed;

  return qadSendv(obj, NAME_device, 1, (Any *) &dev);
}


status
deviceGraphical(Any obj, Device dev)
{ Graphical gr = obj;

  if ( notNil(gr->device) )
    eraseDevice(gr->device, gr);

  if ( notNil(dev) )
    appendDevice(dev, gr);
    
  succeed;
}


status
reparentGraphical(Graphical gr)
{ if ( notNil(gr->connections) )
  { Cell cell;

    for_cell(cell, gr->connections)
      updateDeviceConnection(cell->value);
  }

  succeed;
}


status
DisplayedGraphical(Any obj, Bool val)
{ Graphical gr = obj;

  if ( gr->displayed != val )
    qadSendv(obj, NAME_displayed, 1, (Any *)&val);

  succeed;
}


status
displayedGraphical(Any obj, Bool val)
{ Graphical gr = obj;

  if ( notNil(gr->device) )
  { if ( notNil(gr->request_compute) )
    { PceWindow sw = getWindowGraphical(gr);

      if ( sw && sw->displayed == ON )
	ComputeGraphical(gr);
    }
    displayedGraphicalDevice(gr->device, gr, val);
  }

  assign(gr, displayed, val);
  succeed;
}


Bool
getIsDisplayedGraphical(Graphical gr, Device dev)
{ do
  { if ( gr->displayed == ON && gr->device == dev )
      return ON;

    if ( gr->displayed == OFF )
      return OFF;

    gr = (Graphical) gr->device;
  } while( notNil(gr) );

  return isDefault(dev) ? ON : OFF;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initialise the  device on  which  the graphical   is  displayed.  This
function is   called  before ->drawSelf  and sets  the device for  the
primitive drawing functions and returns the  absolute area to  be able
to draw the graphical.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
initialiseDeviceGraphical(Any obj, int *x, int *y, int *w, int *h)
{ Graphical gr = obj;

  *x = valInt(gr->area->x);
  *y = valInt(gr->area->y);
  *w = valInt(gr->area->w);
  *h = valInt(gr->area->h);

  succeed;
}


status
initialiseRedrawAreaGraphical(Any obj, Area a,
			      int *x, int *y, int *w, int *h,
			      IArea redraw)
{ struct iarea a2;
  Graphical gr = obj;

  initialiseDeviceGraphical(obj, x, y, w, h);

  redraw->x = *x + valInt(a->x) - valInt(gr->area->x); /* normalised! */
  redraw->y = *y + valInt(a->y) - valInt(gr->area->y);
  redraw->w = valInt(a->w);
  redraw->h = valInt(a->h);

  a2.x = *x, a2.y = *y, a2.w = *w, a2.h = *h;
  NormaliseArea(a2.x, a2.y, a2.w, a2.h);
  
  intersection_iarea(redraw, &a2);

  succeed;
}





/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Computes the offset caused by intermediate devices to the windows
coordinate system.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
offsetDeviceGraphical(Any obj, int *x, int *y)
{ Graphical gr = obj;
  register Device dev = gr->device;

  *x = 0;
  *y = 0;

  while( notNil(dev) && !instanceOfObject(dev, ClassWindow) )
  { Point p = dev->offset;
    *x += valInt(p->x);
    *y += valInt(p->y);

    dev = dev->device;
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Compute the area of a graphical in  the coordinate  system of the real
drawing device (the picture).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Area
getAbsoluteAreaGraphical(Graphical gr, Device device)
{ if ( gr->device == device || isNil(gr->device) )
    answer(gr->area);
  else
  { Device dev = gr->device;
    int x, y;

    x = valInt(gr->area->x);
    y = valInt(gr->area->y);

    while( notNil(dev) &&
	   !instanceOfObject(dev, ClassWindow) &&
	   dev != device )
    { x += valInt(dev->offset->x);
      y += valInt(dev->offset->y);

      dev = dev->device;
    }

    answer(answerObject(ClassArea, toInt(x), toInt(y),
			gr->area->w, gr->area->h, 0));
  }
}


Graphical
getRootGraphical(Graphical gr)
{ for(;; gr = (Graphical) gr->device)
  { if ( isNil(gr->device) )
      answer(gr);
  }
}


PceWindow
getWindowGraphical(Graphical gr)
{ while( notNil(gr) && !instanceOfObject(gr, ClassWindow) )
    gr = (Graphical) gr->device;

  if ( notNil(gr) )
    answer((PceWindow) gr);

  fail;
}


FrameObj
getFrameGraphical(Graphical gr)
{ Graphical root = getRootGraphical(gr);

  if ( instanceOfObject(root, ClassWindow) )
  { PceWindow sw = (PceWindow) root;

    if ( notNil(sw->frame) )
      return sw->frame;
  }

  fail;
}


DisplayObj
getDisplayGraphical(Graphical gr)
{ FrameObj fr = getFrameGraphical(gr);

  if ( fr ) 
    answer(fr->display);

  fail;
}


Device
getCommonDeviceGraphical(Graphical gr1, Graphical gr2)
{ register Device dev1 = gr1->device;
  register Device dev2 = gr2->device;

  if ( dev1 == dev2 )			/* very common case */
  { if ( notNil(dev1) )
      answer(dev1);
    fail;
  }
					/* Get the same level */
  if ( isNil(dev2) )
    fail;
  while( notNil(dev1) && valInt(dev1->level) > valInt(dev2->level) )
    dev1 = dev1->device;

  if ( isNil(dev1) )
    fail;
  while( notNil(dev2) && valInt(dev2->level) > valInt(dev1->level) )
    dev2 = dev2->device;

					/* Walk along both branches */
  while( notNil(dev1) && notNil(dev2) )
  { if ( dev1 == dev2 )
      answer(dev1);

    dev1 = dev1->device;
    dev2 = dev2->device;
  }

  fail;
}

		/********************************
		*            CHANGES		*
		********************************/

status
changedAreaGraphical(Any obj, Int x, Int y, Int w, Int h)
{ Graphical gr = obj;

  if ( notNil(gr->device) && gr->displayed == ON )
  { Device d;
    int offx=0, offy=0;			/* Offset to the window */

    requestComputeDevice(gr->device, DEFAULT);
    updateConnectionsGraphical(gr, gr->device->level);

    for(d = gr->device; notNil(d); d = d->device)
    { if ( d->displayed == OFF )
	succeed;

      offx += valInt(d->offset->x);
      offy += valInt(d->offset->y);

      if ( instanceOfObject(d, ClassWindow) )
      { PceWindow sw = (PceWindow) d;
	Area a = gr->area;
	int ox = valInt(x), oy = valInt(y),
	    ow = valInt(w), oh = valInt(h);
	int cx = valInt(a->x), cy = valInt(a->y),
            cw = valInt(a->w), ch = valInt(a->h);

	if ( !createdWindow(sw) )
	{ DEBUG(NAME_window, Cprintf("%s: Change on non-displayed window\n",
				     pp(sw)));
	  break;
	}

	NormaliseArea(ox, oy, ow, oh);
	NormaliseArea(cx, cy, cw, ch);
	ox += offx; oy += offy;
	cx += offx; cy += offy;

					/* HACKS ... */
	if ( instanceOfObject(gr, ClassJoint) )
	{ ox -= 5; oy -= 5; ow += 10; oh += 10;
	  cx -= 5; cy -= 5; cw += 10; ch += 10;
	} else if ( instanceOfObject(gr, ClassText) ||
		    instanceOfObject(gr, ClassTextItem) )
	{ ox -= 5; oy -= 0; ow += 10; oh += 5;
	  cx -= 5; cy -= 0; cw += 10; ch += 5;
	}
					/* end hacks! */

	changed_window(sw, ox, oy, ow, oh, TRUE);
	changed_window(sw, cx, cy, cw, ch, offFlag(gr, F_SOLID));

	addChain(ChangedWindows, sw);
	break;				/* A window stops propagation */
      }
    }
  }

  succeed;
}


status
changedImageGraphical(Any obj, Int x, Int y, Int w, Int h)
{ Graphical gr = obj;

  if ( notNil(gr->device) && gr->displayed == ON )
  { Device d;
    int ox=0, oy=0;			/* Offset to the window */

    for(d = gr->device; notNil(d); d = d->device)
    { if ( d->displayed == OFF )
	succeed;
      ox += valInt(d->offset->x);
      oy += valInt(d->offset->y);

      if ( instanceOfObject(d, ClassWindow) )
      { PceWindow sw = (PceWindow) d;
	int cx = valInt(x) + valInt(gr->area->x),
	    cy = valInt(y) + valInt(gr->area->y),
	    cw = valInt(w),
	    ch = valInt(h);

	NormaliseArea(cx, cy, cw, ch);
	cx += ox;
	cy += oy;

	if ( instanceOfObject(gr, ClassJoint) ) /* HACK (for arrows) */
	{ cx -= 5; cy -= 5; cw += 10; ch += 10;
	} else if ( instanceOfObject(gr, ClassText) ||
		    instanceOfObject(gr, ClassTextItem) )
	{ cx -= 5; cy -= 0; cw += 10; ch += 5;
	}

	changed_window(sw, cx, cy, cw, ch, offFlag(gr, F_SOLID));

	addChain(ChangedWindows, sw);
	break;
      }
    }
  }

  succeed;
}


status
changedEntireImageGraphical(Any obj)
{ Graphical gr = obj;

  return changedImageGraphical(gr, ZERO, ZERO, gr->area->w, gr->area->h);
}


status
redrawGraphical(Graphical gr, Area a)
{ if ( isDefault(a) )
    return changedEntireImageGraphical(gr);

  return changedImageGraphical(gr, a->x, a->y, a->w, a->h);
}


		/********************************
		*          COMPUTING		*
		********************************/

status
requestComputeGraphical(Any obj, Any val)
{ Graphical gr = obj;

  if ( isFreeingObj(gr) ||		/* not needed */
       (notNil(gr->request_compute) && isDefault(val)) ||
       gr->request_compute == val )
    succeed;

  if ( isDefault(val) )
    val = ON;

  assign(gr, request_compute, val);
    
  if ( notNil(gr->device) )
  { appendChain(gr->device->recompute, gr);
    requestComputeGraphical((Graphical) gr->device, DEFAULT);
  } else if ( instanceOfObject(gr, ClassWindow) && gr->displayed == ON )
    addChain(ChangedWindows, gr);

  succeed;
}


status
ComputeGraphical(Any obj)
{ Graphical gr = obj;

  if ( notNil(gr->request_compute) && !isFreeingObj(gr) )
  { qadSendv(gr, NAME_compute, 0, NULL);
    
    assign(gr, request_compute, NIL);
  }

  succeed;
}


static status
computeGraphical(Graphical gr)
{ assign(gr, request_compute, NIL);

  succeed;
}


		/********************************
		*           REPAINT		*
		********************************/

static void
selection_bubble(int x, int y, int w, int h, int wx, int wy)
{ int bw = min(5, w);
  int bh = min(5, h);
  int bx = x + (w - bw) * wx / 2;
  int by = y + (h - bh) * wy / 2;

  r_complement(bx, by, bw, bh);
}


static status
drawGraphical(Graphical gr, Point offset, Area area)
{ int ox = 0;
  int oy = 0;

  if ( notDefault(offset) )
  { ox = valInt(offset->x);
    oy = valInt(offset->y);
  }

  if ( isDefault(area) )
  { static Area large_area = NULL;

    if ( !large_area )
      large_area = globalObject(NIL, ClassArea,
				toInt(PCE_MIN_INT/2), toInt(PCE_MIN_INT/2),
				toInt(PCE_MAX_INT), toInt(PCE_MAX_INT), 0);

    area = large_area;
  }

  r_offset(ox, oy);
  RedrawArea(gr, area);
  r_offset(-ox, -oy);

  succeed;
}


status
RedrawArea(Any obj, Area area)
{ Graphical gr = obj;
  Any c, oc = NULL;
  status rval;

  ComputeGraphical(obj);		/* should not be necessary: */

  c = gr->colour;
  if ( gr->selected == ON )
  { PceWindow sw = getWindowGraphical(gr);

    if ( sw )
    { Any feedback = sw->selection_feedback;

      if ( instanceOfObject(feedback, ClassColour) )
	c = feedback;
    }
  }
  if ( gr->active == OFF )
  { Any c2;

    if ( (c2 = getResourceValueObject(gr, NAME_inactiveColour)) && notNil(c2) )
      c = c2;
  }

  if ( notDefault(c) )
    oc = r_default_colour(c);

  rval = qadSendv(gr, NAME_RedrawArea, 1, (Any *)&area);

  if ( oc )
    r_default_colour(oc);

  return rval;
}


status
paintSelectedGraphical(Graphical gr)
{ PceWindow sw = getWindowGraphical(gr);
  Any feedback = sw->selection_feedback;

  if ( notNil(feedback) )
  { int x, y, w, h;

    initialiseDeviceGraphical(gr, &x, &y, &w, &h);

    if ( feedback == (Any) NAME_invert )
    { r_complement(x, y, w, h);
    } else if ( feedback == (Any) NAME_handles )
    { Name which = getResourceValueObject(gr, NAME_selectionHandles);

      if ( which == NAME_corners )
      { selection_bubble(x, y, w, h, 0, 0);
	selection_bubble(x, y, w, h, 0, 2);
	selection_bubble(x, y, w, h, 2, 0);
	selection_bubble(x, y, w, h, 2, 2);
      } else if ( which == NAME_sides )
      { selection_bubble(x, y, w, h, 0, 1);
	selection_bubble(x, y, w, h, 1, 0);
	selection_bubble(x, y, w, h, 1, 2);
	selection_bubble(x, y, w, h, 2, 1);
      } else if ( which == NAME_line )
      { int lw = valInt(gr->area->w);
	int lh = valInt(gr->area->h);

	NormaliseArea(x, y, w, h);
	if ( (lw >= 0 && lh >= 0) || (lw < 0 && lh < 0) )
	{ r_complement(x-2,   y-2,   5, 5);
	  r_complement(x+w-3, y+h-3, 5, 5);
	} else
	{ r_complement(x+w-3, y-2,   5, 5);
	  r_complement(x-2,   y+h-3, 5, 5);
	}
      } else if ( which == NAME_cornersAndSides )
      { selection_bubble(x, y, w, h, 0, 0);
	selection_bubble(x, y, w, h, 0, 2);
	selection_bubble(x, y, w, h, 2, 0);
	selection_bubble(x, y, w, h, 2, 2);
	selection_bubble(x, y, w, h, 0, 1);
	selection_bubble(x, y, w, h, 1, 0);
	selection_bubble(x, y, w, h, 1, 2);
	selection_bubble(x, y, w, h, 2, 1);
      }
    } else if ( instanceOfObject(feedback, ClassElevation) )
    { r_3d_box(x, y, w, h, 0, feedback, TRUE);
    }
  }

  succeed;
}


status
RedrawAreaGraphical(Any obj, Area area)
{ int init = FALSE, x, y, w, h;
  Graphical gr = obj;

  if ( gr->inverted == ON )
  { initialiseDeviceGraphical(gr, &x, &y, &w, &h);
    init = TRUE;
    r_complement(x, y, w, h);
  }
  
  if ( gr->selected == ON )
    qadSendv(gr, NAME_paintSelected, 0, NULL);

  succeed;
}


status
flushGraphical(Any gr)
{ PceWindow sw;

  if ( (sw = getWindowGraphical(gr)) )
    flushWindow(sw);

  succeed;
}


status
synchroniseGraphical(Graphical gr, Bool always)
{ DisplayObj d;
#ifdef HAVE_GETTIMEOFDAY
  static struct timeval last;

  if ( always != ON )
  { struct timeval now;

    gettimeofday(&now, NULL);
    if ( ((now.tv_sec - last.tv_sec) * 1000 +
	  (now.tv_usec - last.tv_usec) / 1000) < 200 )
      succeed;
    last = now;
  }
#endif /*HAVE_GETTIMEOFDAY*/ 

  if ( (d = getDisplayGraphical(gr)) )
    synchroniseDisplay(d);

  succeed;
}

	      /********************************
	      *          HIDE/EXPOSE	      *
	      ********************************/

static status
hideGraphical(Any obj1, Any obj2)
{ Graphical gr1 = obj1;
  Graphical gr2 = obj2;
  
  if ( notNil(gr1->device) &&
       (isDefault(gr2) || gr2->device == gr1->device) )
  { hideDevice(gr1->device, gr1, gr2);
    updateHideExposeConnectionsGraphical(gr1);
  }

  succeed;
}


status
exposeGraphical(Any obj1, Any obj2)
{ Graphical gr1 = obj1;
  Graphical gr2 = obj2;

  if ( notNil(gr1->device) &&
       (isDefault(gr2) || gr2->device == gr1->device) )
  { exposeDevice(gr1->device, gr1, gr2);
    updateHideExposeConnectionsGraphical(gr1);
  }

  succeed;
}


static status
swapGraphical(Any obj1, Any obj2)
{ Graphical gr1 = obj1;
  Graphical gr2 = obj2;

  if ( gr1->device == gr2->device && notNil(gr1->device) )
    swapGraphicalsDevice(gr1->device, gr1, gr2);

  succeed;
}

	      /********************************
	      *      GEOMETRY MANAGEMENT      *
	      ********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The user accessible geometry management method  (->x, ->size, etc.)
are defined on  the super-class graphical.   All subclasses use  these
methods.  All these methods are translated into Graphical ->set:
[x], [y],   [w], [h].  Most  of  these  translations  are   done using
hard-coded functions calls.

To   allow for  sub-classes   of  graphical  to  specialise   geometry
management, Graphical ->set invokes ->geometry: [x], [y], [w], [h].
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
setGraphical(Any obj, Int x, Int y, Int w, Int h)
{ Graphical gr = obj;

#define Changed(a) (gr->area->a != a && notDefault(a))
  if ( Changed(x) || Changed(y) || Changed(w) || Changed(h) )
  { Int av[4];

    av[0] = x; av[1] = y; av[2] = w; av[3] = h;
    return qadSendv(gr, NAME_requestGeometry, 4, av);
  }
#undef Changed
  succeed;
}


status
doSetGraphical(Any obj, Int x, Int y, Int w, Int h)
{ Graphical gr = obj;

#define Changed(a) (gr->area->a != a && notDefault(a))
  if ( Changed(x) || Changed(y) || Changed(w) || Changed(h) )
  { Int av[4];

    av[0] = x; av[1] = y; av[2] = w; av[3] = h;
    return qadSendv(gr, NAME_geometry, 4, av);
  }
#undef Changed
  succeed;
}


status
requestGeometryGraphical(Any gr, Int x, Int y, Int w, Int h)
{ Int av[4];

  av[0] = x; av[1] = y; av[2] = w; av[3] = h;

  return qadSendv(gr, NAME_geometry, 4, av);
}


status
geometryGraphical(Any obj, Int x, Int y, Int w, Int h)
{ Graphical gr = obj;

  CHANGING_GRAPHICAL(gr, setArea(gr->area, x, y, w, h));

  succeed;
}


static status
areaGraphical(Graphical gr, Area area)
{ return setGraphical(gr, area->x, area->y, area->w, area->h);
}


status
xGraphical(Graphical gr, Int x)
{ return setGraphical(gr, x, DEFAULT, DEFAULT, DEFAULT);
}


status
yGraphical(Graphical gr, Int y)
{ return setGraphical(gr, DEFAULT, y, DEFAULT, DEFAULT);
}


static status
widthGraphical(Graphical gr, Int w)
{ return setGraphical(gr, DEFAULT, DEFAULT, w, DEFAULT);
}


status
heightGraphical(Graphical gr, Int h)
{ return setGraphical(gr, DEFAULT, DEFAULT, DEFAULT, h);
}


status
positionGraphical(Graphical gr, Point pos)
{ return setGraphical(gr, pos->x, pos->y, DEFAULT, DEFAULT);
}


static status
sizeGraphical(Graphical gr, Size size)
{ return setGraphical(gr, DEFAULT, DEFAULT, size->w, size->h);
}


static status
setCornerGraphical(Graphical gr, Int x, Int y)
{ if ( isDefault(x) ) x = add(gr->area->x, gr->area->w);
  if ( isDefault(y) ) y = add(gr->area->y, gr->area->h);

  return setGraphical(gr, DEFAULT, DEFAULT, sub(x, gr->area->x),
					    sub(y, gr->area->y));
}


static status
cornerGraphical(Graphical gr, Point pos)
{ return setCornerGraphical(gr, pos->x, pos->y);
}


static status
cornerXGraphical(Graphical gr, Int x)
{ return setCornerGraphical(gr, x, DEFAULT);
}


static status
cornerYGraphical(Graphical gr, Int y)
{ return setCornerGraphical(gr, DEFAULT, y);
}


status
centerGraphical(Graphical gr, Point pos)
{ ComputeGraphical(gr);
  return setGraphical(gr, dif(pos->x, gr->area->w),
		      dif(pos->y, gr->area->h),
		      DEFAULT, DEFAULT);
}


static status
centerXGraphical(Graphical gr, Int c)
{ ComputeGraphical(gr);
  return setGraphical(gr, dif(c, gr->area->w),
		      DEFAULT, DEFAULT, DEFAULT);
}


static status
centerYGraphical(Graphical gr, Int c)
{ ComputeGraphical(gr);
  return setGraphical(gr, DEFAULT,
		      dif(c, gr->area->h),
		      DEFAULT, DEFAULT);
}


status
relativeMoveGraphical(Graphical gr, Point pos)
{ return setGraphical(gr, add(gr->area->x, pos->x),
		      add(gr->area->y, pos->y),
		      DEFAULT, DEFAULT);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Highly  dubious.   What about lines   and bitmaps?  Should  the result
always be normalised?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
rotateGraphical(Graphical gr, Int degrees)
{ int d;
  Point pos;
  Size size;

  d = valInt(degrees);
  if ((d%90) != 0)
    return errorPce(gr, NAME_rotate90);
  d %= 360;

  if (d == 90 || d == 270)
  { pos = getCenterGraphical(gr);
    size = getSizeGraphical(gr);
    CHANGING_GRAPHICAL(gr,
	    widthGraphical(gr, size->h);
	    heightGraphical(gr, size->w);
	    centerGraphical(gr, pos));
  }

  succeed;
}

		/********************************
		*            RESIZING		*
		********************************/

status
init_resize_graphical(Any obj, Real xfactor, Real yfactor, Point origin,
		      float *xf, float *yf, int *ox, int *oy)
{ *xf = xfactor->value;
  *yf = (isDefault(yfactor) ? *xf : yfactor->value);

  if ( notDefault(origin) )
  { *ox = valInt(origin->x);
    *oy = valInt(origin->y);
  }

  succeed;
}


static status
resizeGraphical(Graphical gr, Real xfactor, Real yfactor, Point origin)
{ float xf, yf;
  int ox = valInt(gr->area->x);
  int oy = valInt(gr->area->y);
  int nx, ny, nw, nh;

  init_resize_graphical(gr, xfactor, yfactor, origin, &xf, &yf, &ox, &oy);
  if ( xf == 1.0 && yf == 1.0 )
    succeed;

  nx = ox + rfloat((float) (valInt(gr->area->x)-ox) * xf);
  ny = oy + rfloat((float) (valInt(gr->area->y)-oy) * yf);
  nw = rfloat((float) valInt(gr->area->w) * xf);
  nh = rfloat((float) valInt(gr->area->h) * yf);

  return setGraphical(gr, toInt(nx), toInt(ny), toInt(nw), toInt(nh));
}


		/********************************
		*         NORMALISATION		*
		********************************/

static status
normaliseGraphical(Graphical gr)
{ return orientationGraphical(gr, NAME_northWest);
}


static status
orientationGraphical(Graphical gr, Name orientation)
{ if ( instanceOfObject(gr, ClassBox) ||
       instanceOfObject(gr, ClassCircle) ||
       instanceOfObject(gr, ClassEllipse) )
    orientationArea(gr->area, orientation);

  succeed;
}


static Name
getOrientationGraphical(Graphical gr)
{ answer( getOrientationArea(gr->area) );
}


		/********************************
		*      GET AREA ATTRIBUTES	*
		********************************/

Area
getAreaGraphical(Graphical gr)
{ ComputeGraphical(gr);
  answer(gr->area);
}


static Int
getXGraphical(Graphical gr)
{ answer(getAreaGraphical(gr)->x);
}


static Int
getYGraphical(Graphical gr)
{ answer(getAreaGraphical(gr)->y);
}


static Int
getCornerXGraphical(Graphical gr)
{ answer(add(getAreaGraphical(gr)->x,
	     getAreaGraphical(gr)->w));
}


static Int
getCornerYGraphical(Graphical gr)
{ answer(add(getAreaGraphical(gr)->y,
	     getAreaGraphical(gr)->h));
}


static Int
getWidthGraphical(Graphical gr)
{ answer(getAreaGraphical(gr)->w);
}


static Int
getHeightGraphical(Graphical gr)
{ answer(getAreaGraphical(gr)->h);
}


Int
getLeftSideGraphical(Graphical gr)
{ Area a = getAreaGraphical(gr);

  if ( valInt(a->w) >= 0 )
    answer(a->x);
  else
    answer(add(a->x, a->w));
}


Int
getRightSideGraphical(Graphical gr)
{ Area a = getAreaGraphical(gr);

  if ( valInt(a->w) >= 0 )
    answer(add(a->x, a->w));
  else
    answer(a->x);
}


static Int
getTopSideGraphical(Graphical gr)
{ Area a = getAreaGraphical(gr);

  if ( valInt(a->h) >= 0 )
    answer(a->y);
  else
    answer(add(a->y, a->h));
}


static Int
getBottomSideGraphical(Graphical gr)
{ Area a = getAreaGraphical(gr);

  if ( valInt(a->h) >= 0 )
    answer(add(a->y, a->h));
  else
    answer(a->y);
}


static Point
getPositionGraphical(Graphical gr)
{ answer(answerObject(ClassPoint,getAreaGraphical(gr)->x,
		      getAreaGraphical(gr)->y,0));
}


status
get_absolute_xy_graphical(Graphical gr, Device *dev, Int *X, Int *Y)
{ int x, y;

  DEBUG(NAME_absolutePosition,
	Cprintf("get_absolutePosition(%s, %s) ... ", pp(gr), pp(*dev)));

  ComputeGraphical(gr);
  x = valInt(gr->area->x);
  y = valInt(gr->area->y);

  while( !instanceOfObject(gr->device, ClassWindow) &&
	 gr->device != *dev )
  { Point offset = gr->device->offset;

    x += valInt(offset->x);
    y += valInt(offset->y);
    gr = (Graphical)gr->device;
  }

  if ( notDefault(*dev) && gr->device != *dev )
  { DEBUG(NAME_absolutePosition, Cprintf("failed\n"));
    fail;
  }

  *dev = gr->device;
  *X = toInt(x);
  *Y = toInt(y);

  DEBUG(NAME_absolutePosition, Cprintf("X=%s; Y=%s\n", pp(*X), pp(*Y)));

  succeed;
}


Int
getAbsoluteXGraphical(Any gr, Device dev)
{ Int x, y;

  TRY( get_absolute_xy_graphical(gr, &dev, &x, &y) );

  answer(x);
}


Int
getAbsoluteYGraphical(Any gr, Device dev)
{ Int x, y;

  TRY( get_absolute_xy_graphical(gr, &dev, &x, &y) );

  answer(y);
}


static Point
getAbsolutePositionGraphical(Graphical gr, Device dev)
{ Int x, y;

  TRY( get_absolute_xy_graphical(gr, &dev, &x, &y) );

  answer(answerObject(ClassPoint, x, y, 0));
}


Point
getDisplayPositionGraphical(Graphical gr)
{ Int x, y;
  int ox, oy, wx, wy;
  PceWindow w = DEFAULT;

					/* relative to window system */
  if ( instanceOfObject(gr, ClassWindow) )
  { x = y = ZERO;
    w = (PceWindow) gr;
    ox = oy = 0;
  } else
  { get_absolute_xy_graphical(gr, (Device *)&w, &x, &y);
    if ( !instanceOfObject(w, ClassWindow) )
      fail;				/* not displayed */
    offset_window(w, &ox, &oy);
  }
					/* relative to display */
  get_display_position_window(w, &wx, &wy);

  x = toInt(valInt(x) + ox + wx);
  y = toInt(valInt(y) + oy + wy);
  
  answer(answerObject(ClassPoint, x, y, 0));
}


Size
getSizeGraphical(Graphical gr)
{ answer(answerObject(ClassSize,
		      getAreaGraphical(gr)->w,
		      getAreaGraphical(gr)->h, 0));
}


static Point
getCornerGraphical(Graphical gr)
{ Area a = getAreaGraphical(gr);

  answer(answerObject(ClassPoint, add(a->x,a->w), add(a->y,a->h), 0));
}


static Point
getCenterGraphical(Graphical gr)
{ Area a = getAreaGraphical(gr);

  answer(answerObject(ClassPoint, mid(a->x,a->w), mid(a->y,a->h), 0));
}


static Int
getCenterXGraphical(Graphical gr)
{ Area a = getAreaGraphical(gr);

  answer(mid(a->x, a->w));
}


static Int
getCenterYGraphical(Graphical gr)
{ Area a = getAreaGraphical(gr);

  answer(mid(a->y, a->h));
}


static Area
getBoundingBoxGraphical(Graphical gr)
{ answer(getAreaGraphical(gr));
}


static Int
getDistanceGraphical(Graphical gr, Graphical gr2)
{ answer(getDistanceArea(gr->area, gr2->area));
}


static Int
getDistanceXGraphical(Graphical gr, Graphical gr2)
{ answer(getDistanceXArea(gr->area, gr2->area));
}


static Int
getDistanceYGraphical(Graphical gr, Graphical gr2)
{ answer(getDistanceYArea(gr->area, gr2->area));
}


		 /*******************************
		 *	  DIALOG POSITIONS	*
		 *******************************/

static status
same_device(Graphical gr1, Graphical gr2)
{ if ( notNil(gr1) && notNil(gr2) && gr1->device != gr2->device )
  { if ( isNil(gr1->device) )
      displayDevice((Dialog) gr2->device, gr1, DEFAULT);
    else if ( isNil(gr2->device) )
      displayDevice((Dialog) gr1->device, gr2, DEFAULT);
    else
      return errorPce(gr1, NAME_alreadyShown, 12, gr2->device);
  }
  
  succeed;
}


static status
assignDialogItem(Graphical gr, Name slot, Any value)
{ Variable var;

  if ( (var = getInstanceVariableClass(classOfObject(gr), slot)) )
    return sendVariable(var, gr, 1, &value);

  if ( isNil(value) )
    return deleteAttributeObject(gr, slot);
  else
    return attributeObject(gr, slot, value);
}



status
aboveGraphical(Graphical gr1, Graphical gr2)
{ Graphical gr;

  TRY(same_device(gr1, gr2));

  if ( notNil(gr2) )
  { belowGraphical(gr2, NIL);
    assignDialogItem(gr2, NAME_below, gr1);
  }
  if ( (gr = get(gr1, NAME_above, 0)) && notNil(gr) )
    assignDialogItem(gr, NAME_below, NIL);
  
  assignDialogItem(gr1, NAME_above, gr2);

  succeed;
}


status
belowGraphical(Graphical gr1, Graphical gr2)
{ Graphical gr;

  TRY(same_device(gr1, gr2));

  if ( notNil(gr2) )
  { aboveGraphical(gr2, NIL);
    assignDialogItem(gr2, NAME_above, gr1);
  }
  if ( (gr = get(gr1, NAME_below, 0)) && notNil(gr) )
    assignDialogItem(gr, NAME_above, NIL);
  
  assignDialogItem(gr1, NAME_below, gr2);

  succeed;
}


status
rightGraphical(Graphical gr1, Graphical gr2)
{ Graphical gr;

  TRY(same_device(gr1, gr2));

  if ( notNil(gr2) )
  { leftGraphical(gr2, NIL);
    assignDialogItem(gr2, NAME_left, gr1);
  }
  if ( (gr = get(gr1, NAME_right, 0)) && notNil(gr) )
    assignDialogItem(gr, NAME_left, NIL);
  
  assignDialogItem(gr1, NAME_right, gr2);

  succeed;
}


status
leftGraphical(Graphical gr1, Graphical gr2)
{ Graphical gr;

  TRY(same_device(gr1, gr2));

  if ( notNil(gr2) )
  { rightGraphical(gr2, NIL);
    assignDialogItem(gr2, NAME_right, gr1);
  }
  if ( (gr = get(gr1, NAME_right, 0)) && notNil(gr) )
    assignDialogItem(gr, NAME_right, NIL);
  
  assignDialogItem(gr1, NAME_left, gr2);

  succeed;
}


static status
referenceGraphical(Graphical gr, Point ref)
{ return assignDialogItem(gr, NAME_reference, ref);
}


static status
alignmentGraphical(Graphical gr, Name alignment)
{ return assignDialogItem(gr, NAME_alignment, alignment);
}


static Name
getAlignmentGraphical(Graphical gr)
{ Name alignment;

  if ( isName(alignment = getAttributeObject(gr, NAME_alignment)) )
    answer(alignment);
  if ( isName(alignment = getResourceValueObject(gr, NAME_alignment)) )
    answer(alignment);

  answer(NAME_left);
}


static status
autoAlignGraphical(Graphical gr, Bool align)
{ return assignDialogItem(gr, NAME_autoAlign, align);
}


static status
autoLabelAlignGraphical(Graphical gr, Bool val)
{ return assignDialogItem(gr, NAME_autoLabelAlign, val);
}


static status
autoValueAlignGraphical(Graphical gr, Bool val)
{ return assignDialogItem(gr, NAME_autoValueAlign, val);
}


static Bool
getAutoAlignGraphical(Graphical gr)
{ Bool  rval;

  if ( (rval = getAttributeObject(gr, NAME_autoAlign)) &&
       instanceOfObject(rval, ClassBool) )
    answer(rval);

  if ( onFlag(gr, F_ATTRIBUTE) )
  { if ( getAttributeObject(gr, NAME_above) ||
	 getAttributeObject(gr, NAME_below) ||
	 getAttributeObject(gr, NAME_left) ||
	 getAttributeObject(gr, NAME_right) )
      answer(ON);
  }

  answer(OFF);
}


static Bool
getAutoLabelAlignGraphical(Graphical gr)
{ Bool  rval;

  if ( (rval = getAttributeObject(gr, NAME_autoLabelAlign)) &&
       instanceOfObject(rval, ClassBool) )
    answer(rval);

  answer(OFF);
}


static Bool
getAutoValueAlignGraphical(Graphical gr)
{ Bool  rval;

  if ( (rval = getAttributeObject(gr, NAME_autoValueAlign)) &&
       instanceOfObject(rval, ClassBool) )
    answer(rval);

  answer(OFF);
}


		/********************************
		*             PEN		*
		********************************/

status
penGraphical(Graphical gr, Int pen)
{ if (gr->pen != pen)
  { CHANGING_GRAPHICAL(gr, assign(gr, pen, pen);
		           changedEntireImageGraphical(gr));
  }

  succeed;
}


status
shadowGraphical(Graphical gr, Int s)
{ return assignGraphical(gr, NAME_shadow, s);
}


status
elevationGraphical(Graphical gr, Elevation e)
{ return assignGraphical(gr, NAME_elevation, e);
}


status
fillPatternGraphical(Graphical gr, Image pattern)
{ return assignGraphical(gr, NAME_fillPattern, pattern);
}


static status
textureGraphical(Graphical gr, Name texture)
{ if (gr->texture != texture)
  { CHANGING_GRAPHICAL(gr, assign(gr, texture, texture);
		           changedEntireImageGraphical(gr));
  }

  succeed;
}


status
colourGraphical(Graphical gr, Any c)
{ if ( gr->colour != c )
  { CHANGING_GRAPHICAL(gr, assign(gr, colour, c);
  			   changedEntireImageGraphical(gr));
  }
  
  succeed;  
}


Any
getDisplayColourGraphical(Graphical gr)
{ while( notNil(gr) )
  { if ( notDefault(gr->colour) )
      answer(gr->colour);

    gr = (Graphical) gr->device;
  }

  fail;
}


		/********************************
		*           SELECTION		*
		********************************/


static status
toggleSelectedGraphical(Graphical gr)
{ return send(gr, NAME_selected, gr->selected == ON ? OFF : ON, 0);
}


status
selectedGraphical(Graphical gr, Bool val)
{ if (gr->selected != val)
  { CHANGING_GRAPHICAL(gr, assign(gr, selected, val);
		           changedEntireImageGraphical(gr));
  }

  succeed;
}


		/********************************
		*            HANDLES		*
		********************************/

static status
handleGraphical(Graphical gr, Handle h)
{ if (isNil(gr->handles))
    assign(gr, handles, newObject(ClassChain, 0));

  return appendChain(gr->handles, h);
}


Handle
getHandleGraphical(Graphical gr, Name name)
{ Class class;

  if ( notNil(gr->handles) )
  { Cell cell;

    for_cell(cell, gr->handles)
    { Handle h = cell->value;
      if ( h->name == name )
	answer(h);
    }
  }

  class = classOfObject(gr);
  if ( notNil(class->handles) )
  { Cell cell;

    for_cell(cell, class->handles)
    { Handle h = cell->value;
      if ( h->name == name )
	answer(h);
    }
  }

  fail;
}


Point
getHandlePositionGraphical(Graphical gr, Name name, Device dev)
{ Int x, y;
  Handle h;

  if ( isDefault(dev) )
    dev = gr->device;

  TRY(h = getHandleGraphical(gr, name));
  TRY(x = getXHandle(h, gr, dev));
  TRY(y = getYHandle(h, gr, dev));
  
  answer(answerObject(ClassPoint, x, y, 0));
}


Chain
getHandlesGraphical(Graphical gr, Point pos, Name kind, Int distance)
{ int maxdx=0, maxdy=0;
  int px=0, py=0;
  Cell cell;
  Class class;
  Chain rval = NIL;
  int use_range;

  if ( notDefault(distance) && notDefault(pos) )
  { px = valInt(pos->x);
    py = valInt(pos->y);

    maxdx = (valInt(distance) * valInt(gr->area->w) + 99) / 100;
    maxdy = (valInt(distance) * valInt(gr->area->h) + 99) / 100;
    use_range = TRUE;
  } else
    use_range = FALSE;

  if ( notNil(gr->handles) )
  { for_cell(cell, gr->handles)
    { Handle h = cell->value;
      int hx, hy;

      if ( notDefault(kind) && h->kind != kind )
	continue;

      if ( use_range )
      { hx = valInt(getXHandle(h, gr, gr->device));
	hy = valInt(getYHandle(h, gr, gr->device));
	if ( abs(hx-px) > maxdx || abs(hy-py) > maxdy )
	  continue;
      }

      if ( isNil(rval) )
	rval = answerObject(ClassChain, h, 0);
      else
	appendChain(rval, h);
    }
  }

  class = classOfObject(gr);
  if ( notNil(class->handles) )
  { for_cell(cell, class->handles)
    { Handle h = cell->value;
      int hx, hy;

      if ( notDefault(kind) && h->kind != kind )
	continue;

      if ( use_range )
      { hx = valInt(getXHandle(h, gr, gr->device));
        hy = valInt(getYHandle(h, gr, gr->device));
	if ( abs(hx-px) > maxdx || abs(hy-py) > maxdy )
	  continue;
      }

      if ( isNil(rval) )
	rval = answerObject(ClassChain, h, 0);
      else
	appendChain(rval, h);
    }
  }

  if ( notNil(rval) )
    answer(rval);

  fail;
}

		/********************************
		*        MASKING PATTERNS	*
		********************************/


static status
invertedGraphical(Graphical gr, Bool val)
{ if ( gr->inverted != val )
    CHANGING_GRAPHICAL(gr,
		       assign(gr, inverted, val);
		       changedEntireImageGraphical(gr));
  succeed;
}


static status
activeGraphical(Graphical gr, Bool val)
{ if ( gr->active != val )
  { CHANGING_GRAPHICAL(gr,
		       assign(gr, active, val);
		       changedEntireImageGraphical(gr));
  }
  
  succeed;
}


		/********************************
		*            CURSOR		*
		********************************/

static status
cursorGraphical(Graphical gr, CursorObj cursor)
{ PceWindow w = getWindowGraphical(gr);

  assign(gr, cursor, cursor);
  
  if ( w != FAIL )
    updateCursorWindow(w);

  flushGraphical(gr);

  succeed;
}


status
focusCursorGraphical(Graphical gr, CursorObj cursor)
{ PceWindow w = getWindowGraphical(gr);

  if ( w != FAIL )
    return focusCursorWindow(w, cursor);

  succeed;
}

		/********************************
		*             FOCUS		*
		********************************/

status
focusGraphical(Graphical gr, Recogniser recogniser,
	       CursorObj cursor, Name button)
{ PceWindow sw = getWindowGraphical(gr);

  if ( sw != FAIL )
    focusWindow(sw, gr, recogniser, cursor, button);

  succeed;
}


static status
WantsKeyboardFocusGraphical(Graphical gr)
{ fail;
}


		/********************************
		*         CONNECTIONS		*
		********************************/


				/* Update connections due to move/resize */
status
updateConnectionsGraphical(Graphical gr, Int level)
{ if ( notNil(gr->connections) )
  { Cell cell;
    
    for_cell(cell, gr->connections)
    { Connection c = cell->value;

      if ( notNil(c->device) && valInt(c->device->level) <= valInt(level) )
	requestComputeGraphical(cell->value, DEFAULT);
    }
  }

  if ( instanceOfObject(gr, ClassWindow) ) /* HACK */
    updatePositionWindow((PceWindow) gr);

  succeed;
}


static status
updateHideExposeConnectionsGraphical(Graphical gr)
{ if ( notNil(gr->connections) )
  { Cell cell;
    
    for_cell(cell, gr->connections)
      updateHideExposeConnection(cell->value);
  }

  succeed;
}


status
connectGraphical(Graphical gr, Graphical gr2, Link link, Name from, Name to)
{ if ( get(link, NAME_connection, gr, gr2, from, to, 0) )
    succeed;

  fail;
}


status
attachConnectionGraphical(Graphical gr, Connection c)
{ if ( isNil(gr->connections) )
    assign(gr, connections, newObject(ClassChain, c, 0));
  else
    appendChain(gr->connections, c);

  succeed;
}
      

status
detachConnectionGraphical(Graphical gr, Connection c)
{ if ( notNil(gr->connections) &&
       deleteChain(gr->connections, c) &&
       emptyChain(gr->connections) )
    assign(gr, connections, NIL);

  succeed;
}


static status
match_connection(Connection c, Link link, Name from, Name to)
{ if ( (c->link        == link || isDefault(link)) &&
       (c->from_handle == from || isDefault(from)) &&
       (c->to_handle   == to   || isDefault(to)) )
    succeed;

  fail;
}


static status
connectedGraphical(Graphical gr, Graphical gr2,
		   Link link, Name from, Name to)
{ Chain ch;
  Cell cell;

  if ( notNil(ch = gr->connections) )
  { for_cell(cell, ch)
    { Connection c = cell->value;
    
      if ( (isDefault(gr2) || c->to == gr2 || c->from == gr2) &&
	   match_connection(c, link, from, to) )
	succeed;
    }
  }

  fail;
}


status
disconnectGraphical(Graphical gr, Graphical gr2,
		    Link link, Name from, Name to)
{ Chain ch;
  Cell cell, cell2;

  if ( notNil(ch = gr->connections) )
  { for_cell_save(cell, cell2, ch)
    { Connection c = cell->value;

      if ( (isDefault(gr2) || c->to == gr2 || c->from == gr2) &&
	   match_connection(c, link, from, to) )
	freeObject(c);
    }
  }

  succeed;
}


static Chain
getConnectionsGraphical(Graphical gr, Graphical gr2,
			Link link, Name from, Name to)
{ Chain ch;
  Cell cell;
  Chain rval = NIL;

  if ( isDefault(gr2) && isDefault(link) && isDefault(from) && isDefault(to) )
  { if ( notNil(gr->connections) )
      answer(gr->connections);
    fail;
  }

  if ( notNil(ch = gr->connections) )
  { for_cell(cell, ch)
    { Connection c = cell->value;

      if ( (isDefault(gr2) || c->to == gr2 || c->from == gr2) &&
	   match_connection(c, link, from, to) )
      { if ( isNil(rval) )
	  rval = newObject(ClassChain, c, 0);
	else
	  appendChain(rval, c);
      }
    }

    if ( notNil(rval) )
      answer(rval);
  }

  fail;
}


static status
extendNetworkGraphical(Graphical gr, Link link,
		       Name from, Name to, Chain members)
{ if ( memberChain(members, gr) == SUCCEED )
    succeed;

  appendChain(members, gr);

  if ( notNil(gr->connections) )
  { Cell cell;

    for_cell(cell, gr->connections)
    { Connection c = cell->value;

      if ( match_connection(c, link, from, to) )
	extendNetworkGraphical((c->to == gr ? c->from : c->to),
			       link, from, to, members);
    }
  }

  succeed;
}


static Chain
getNetworkGraphical(Graphical gr, Link link, Name from, Name to)
{ Chain connections;

  connections = answerObject(ClassChain, 0);

  extendNetworkGraphical(gr, link, from, to, connections);

  answer(connections);
}


		/********************************
		*            LAYOUT		*
		********************************/

/* (AA)	send(Graphical, layout, C1, C2, C3, C4, C5, C6)

	Heuristic layout of a graph (based on an algorithm given in: Eades, P.
	(1984), "A Heuristic for Graph Drawing", Congressus Numerantium, vol.
	42, pp. 149-160.  All figures (indirectly) related to Figure, i.e. the
	graph, are moved such: (a) the distance between two figures which are
	connected is constant and (b) the distance between two figures which are 
	not connected is as large as possible.  The algorithm computes the 
	forces on each	figure (the links can be seen as springs), where
	connected figures attract and unconnected figures repel each other.  The
	force on each figure is computed repeatedly, and incremental use of the
	algorithm produces better results until a stable state is reached. The
	algorithm is suitable for graphs in which the vertices are initially
	placed at random.
 */

#define MAX_DISTANCE	500+1	/* maximum distance still relevant */

static status
layoutGraphical(Graphical gr, Real argC1, Real argC2, Real argC3, Int argC4, Int argC5)
{ int **x, **y;		/* x[i][j] = force in x-direction on i, j */
  int **r;		/* r[i][j] = SUCCEED if graphicals i, j are connected */
  Graphical *g;		/* g[i] = graphicals in the graph */
  Picture p;
  int *fx, *fy, *fw, *fh, *m;
  int force;
  int dx, dy, d;
  int n, l, i, j;
  Cell cell;
  float C1 = (isDefault(argC1) ?  2.0 : argC1->value);
  float C2 = (isDefault(argC2) ? 30.0 : argC2->value);
  float C3 = (isDefault(argC3) ?  2.0 : argC3->value);
  int C4   = (isDefault(argC4) ?   15 : valInt(argC4));
  int C5   = (isDefault(argC5) ?  100 : valInt(argC5));
  int moved;
  Chain network;

  static float	PreviousC1 = 73464.347327;
  static float	PreviousC2 = 73464.347327;
  static float	PreviousC3 = 73464.347327;
  static int forceAttract[MAX_DISTANCE];
  static int forceRepel[MAX_DISTANCE];
  int IdealDistance = (int) C2;

/*	Attraction of connected vertices is given by:	C1 * log(d/C2)
	Repelling of unconnected vertices is given by: -C3 / sqrt(d)
	where d is the distance between the vertices.

 **	Tue Sep 15 15:14:42 1987   anjo@swivax.uucp (Anjo Anjewierden) */

  if (C1 != PreviousC1 || C2 != PreviousC2 || C3 != PreviousC3)
  { for (d=0; d<=10; d++)
    { forceAttract[d] = ((int) (2048.0 * C1 * log((float)10/C2)) / 10);
      forceRepel[d] = ((int) (-2048.0 * C3 / sqrt((float)10)) / 10);
    }
    for (d=11; d<MAX_DISTANCE; d++)
    { forceAttract[d] = ((int) (2048.0 * C1 * log((float)d/C2)) / d);
      forceRepel[d] = ((int) (-2048.0 * C3 / sqrt((float)d)) / d);
    }
    PreviousC1 = C1;
    PreviousC2 = C2;
    PreviousC3 = C3;
  }

  if (isNil(p = (Picture) gr->device))
    fail;

  network = get(gr, NAME_network, 0);

  n = valInt(getSizeChain(network));

  x = malloc(n*sizeof(int *));
  y = malloc(n*sizeof(int *));
  r = malloc(n*sizeof(int *));
  for (i=0; i<n; i++)
  { x[i] = malloc(sizeof(int)*n);
    y[i] = malloc(sizeof(int)*n);
    r[i] = malloc(sizeof(int)*n);
  }
  g  = malloc(sizeof(Graphical)*n);
  fx = malloc(sizeof(int)*n);
  fy = malloc(sizeof(int)*n);
  fh = malloc(sizeof(int)*n);
  fw = malloc(sizeof(int)*n);
  m  = malloc(sizeof(int)*n);

  for (cell=network->head, i=0; notNil(cell); i++, cell=cell->next)
  { g[i] = cell->value;
    fx[i] = valInt(g[i]->area->x);
    fy[i] = valInt(g[i]->area->y);
    fw[i] = valInt(g[i]->area->w);
    fh[i] = valInt(g[i]->area->h);
  }

  for (i=0; i<n; i++)
  { for (j=0; j<i; j++)
      r[i][j] = connectedGraphical(g[i], g[j], DEFAULT, DEFAULT, DEFAULT);
    m[i] = TRUE;
    x[i][i] = y[i][i] = 0;
  }

  moved = TRUE;

  for (l=1; l<=C5 && moved; l++)
  { for (i=0; i<n; i++)
    { int mi = m[i];
      for (j=0; j<i; j++)
      { if (mi == FALSE && m[j] == FALSE)
	  continue;
	d = distance_area(fx[i],fy[i],fw[i],fh[i],fx[j],fy[j],fw[j],fh[j]);
	if (d == 0)
	{ x[j][i] = -(x[i][j] = (IdealDistance << 10)/6);
	  y[j][i] = -(y[i][j] = (IdealDistance << 10)/6);
	  continue;
	}
	dx = ((fx[j] + fw[j]/2) - (fx[i] + fw[i]/2)) << 10;
	dy = ((fy[j] + fh[j]/2) - (fy[i] + fh[i]/2)) << 10;
	if (d > 500)
	  d = 500;
	force = (r[i][j] ? forceAttract[d] : forceRepel[d]);
	x[j][i] = -(x[i][j] = (dx * force) >> 11);
	y[j][i] = -(y[i][j] = (dy * force) >> 11);
      }
    }

    moved = FALSE;
    for (i=0; i<n; i++)
    { dx = dy = 0;
      for (j=0; j<n; j++)
      { dx += x[i][j];
	dy += y[i][j];
      }
      dx = (((dx * C4) / n) + 512) >> 10;
      dy = (((dy * C4) / n) + 512) >> 10;
      if (dx == 0 && dy == 0)
      { m[i] = FALSE;
	continue;
      }
      m[i] = moved = TRUE;
      fx[i] += dx;
      fy[i] += dy;
      if (fx[i] < 5)
	fx[i] = 5;
      if (fy[i] < 5)
	fy[i] = 5;
    }
  }

  for (i=0; i<n; i++)	/* update display */
    send(g[i], NAME_set, toInt(fx[i]), toInt(fy[i]), DEFAULT, DEFAULT, 0);

  for(i=0; i<n; i++)
  { free(r[i]);
    free(x[i]);
    free(y[i]);
  }
  free(r);
  free(x);
  free(y);
  free(g);
  free(fx);
  free(fy);
  free(fw);
  free(fh);
  free(m);

  succeed;
}


static int
distance_area(int ax, int ay, int aw, int ah, int bx, int by, int bw, int bh)
{ bx -= ax;				/* normalise on (ax,ay) == (0,0) */
  by -= ay;

  if (ah < by)					/* a above b */
  { if (bx+bw < 0)				/* b left a */
      return(distance(bx+bw, by, 0, ah));
    if (bx > aw)				/* a left b */
      return(distance(aw, ah, bx, by));
    return(by-(ah));
  }

  if (by+bh < 0)				/* b above a */
  { if (aw < bx)
      return(distance(aw, 0, bx, by+bh));
    if (bx+bw < 0)
      return(distance(bx+bw, by+bh, 0, 0));
    return(-(by+bh));
  }

  if (aw < bx)					/* a and b equal height */
    return(bx-(aw));

  if (bx+bw < 0)
    return(-(bx+bw));

  return(0);					/* overlap */
}

		/********************************
		*              EVENTS		*
		********************************/

status
eventGraphical(Any obj, EventObj ev)
{ Graphical gr = obj;

  if ( gr->active != OFF )
  { Chain recognisers;
    Cell cell;

    TRY( recognisers = getAllRecognisersGraphical(gr, OFF) );
  
    for_cell(cell, recognisers)
      if ( qadSendv(cell->value, NAME_event, 1, (Any*)&ev) )
	succeed;
  }

  fail;
}


static status
keyGraphical(Graphical gr, Name key)
{ fail;
}


static status
keyboardFocusGraphical(Graphical gr, Bool val)
{ PceWindow sw = getWindowGraphical(gr);

  if ( sw )
  { if ( val == OFF )
      send(sw, NAME_keyboardFocus, NIL, 0);
    else if ( send(gr, NAME_WantsKeyboardFocus, 0) )
      send(sw, NAME_keyboardFocus, gr, 0);
  }

  succeed;
}


status
generateEventGraphical(Graphical gr, Name name)
{ int rval;
  EventObj ev = tempObject(ClassEvent, name, getWindowGraphical(gr), 0);

  rval = postEvent(ev, gr, DEFAULT);
  considerPreserveObject(ev);
  
  return rval;
}


status
inEventAreaGraphical(Graphical gr, Int xc, Int yc)
{ Area a = gr->area;
  int ax = valInt(a->x), ay = valInt(a->y),
      aw = valInt(a->w), ah = valInt(a->h);
  int x = valInt(xc), y = valInt(yc);
#define MIN_EVENT_AREA 5

  NormaliseArea(ax, ay, aw, ah);
  if ( aw < MIN_EVENT_AREA ) ax -= (MIN_EVENT_AREA-aw)/2, aw = MIN_EVENT_AREA;
  if ( ah < MIN_EVENT_AREA ) ay -= (MIN_EVENT_AREA-ah)/2, ah = MIN_EVENT_AREA;
  
  if ( x >= ax && x <= ax + aw &&
       y >= ay && y <= ay + ah )
  { Class class = classOfObject(gr);

    if ( class->in_event_area_function != NULL )
      if ( (*class->in_event_area_function)(gr, xc, yc) == FAIL )
	fail;

    succeed;
  }

  fail;
}


static status
recogniserGraphical(Any gr, Any r)
{ Chain ch = getAllRecognisersGraphical(gr, ON);

  return appendChain(ch, r);
}


static status
prependRecogniserGraphical(Any gr, Any r)
{ Chain ch = getAllRecognisersGraphical(gr, ON);

  return prependChain(ch, r);
} 


static status
deleteRecogniserGraphical(Any gr, Any r)
{ Chain ch;

  TRY(ch = getAllRecognisersGraphical(gr, OFF));

  return deleteChain(ch, r);
}


Chain
getAllRecognisersGraphical(Any obj, Bool create)
{ if ( onFlag(obj, F_RECOGNISER) )
    answer(getMemberHashTable(ObjectRecogniserTable, obj));

  if ( create == ON )
  { Chain ch = newObject(ClassChain, 0);

    setFlag(obj, F_RECOGNISER);
    appendHashTable(ObjectRecogniserTable, obj, ch);

    answer(ch);
  }

  fail;
}

		/********************************
		*          MISCELLANEOUS	*
		********************************/

status
assignGraphical(Any obj, Name slot, Any value)
{ Graphical gr = obj;
  Class class = classOfObject(gr);
  Variable var;

  if ( (var = getInstanceVariableClass(class, (Any) slot)) != FAIL )
  { if ( getGetVariable(var, gr, 0, NULL) != value )
    { setSlotInstance(gr, var, value);
      requestComputeGraphical(gr, DEFAULT);
      if ( gr->displayed == ON )
      { CHANGING_GRAPHICAL(gr,
			   ComputeGraphical(gr);
			   changedEntireImageGraphical(gr));
      }
    }

    succeed;
  }

  fail;
}


static status
bellGraphical(Graphical gr, Int volume)
{ DisplayObj d;

  TRY( d = getDisplayGraphical(gr) );

  return send(d, NAME_bell, volume, 0);
}


static status
flashGraphical(Graphical gr)
{ Bool oldinv = gr->inverted;

  DEBUG(NAME_flash, Cprintf("Flash ... "));
  CHANGING_GRAPHICAL(gr,
		     assign(gr, inverted, oldinv == ON ? OFF : ON);
		     changedEntireImageGraphical(gr));
  flushGraphical(gr);
  DEBUG(NAME_flash, Cprintf("sleeping ... "));
  msleep(valInt((Int)getResourceValueObject(gr, NAME_visualBellDuration)));
  DEBUG(NAME_flash, Cprintf("waking up ... "));
  CHANGING_GRAPHICAL(gr,
		     assign(gr, inverted, oldinv);
		     changedEntireImageGraphical(gr));
  flushGraphical(gr);
  DEBUG(NAME_flash, Cprintf("done.\n"));

  succeed;
}


status
alertGraphical(Graphical gr)
{ if ( getResourceValueObject(gr, NAME_visualBell) == ON )
    return send(gr, NAME_flash, 0);
  else
    return send(gr, NAME_bell, 0);
}


Node
getNodeGraphical(Graphical gr)
{ Tree t = (Tree) gr->device;

  if ( instanceOfObject(t, ClassTree) )
    answer(getFindNodeNode(t->displayRoot, gr));

  fail;
}


static status
popupGraphical(Graphical gr, PopupObj popup)
{ if ( getInstanceVariableClass(classOfObject(gr), NAME_popup) )
    return send(gr, NAME_slot, NAME_popup, popup, 0);

  send(gr, NAME_attribute,  newObject(ClassAttribute,
				      NAME_popup, popup, 0), 0);
  send(gr, NAME_recogniser, popupGesture(), 0);

  succeed;
}


static PopupObj
getPopupGraphical(Graphical gr)
{ return getAttributeObject(gr, NAME_popup);
}


status
pointerGraphical(Graphical gr, Point pos)
{ Int x, y;
  PceWindow sw = DEFAULT;
  
  get_absolute_xy_graphical(gr, (Device *)&sw, &x, &y);
  if ( instanceOfObject(sw, ClassWindow) )
  { Point p2;

    p2 = tempObject(ClassPoint, add(x, pos->x), add(y, pos->y), 0);
    pointerWindow(sw, p2);
    considerPreserveObject(p2);
  }    

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Return the master of a graphical.  This is supposed to deal with the case
where another object is actually managing me.  For the moment this is just
a clutch.  A more principal solution for this problem is studied.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Any
getMasterGraphical(Graphical gr)
{ if ( instanceOfObject(gr->device, ClassTree) )
  { Tree t = (Tree) gr->device;
    Any master;

    if ( (master=getFindNodeNode(t->displayRoot, gr)) )
      answer(master);
    else
      answer(gr);			/* graphical displayed on tree */
  }

  answer(gr);
}


status
nameGraphical(Graphical gr, Name name)
{ assign(gr, name, name);

  succeed;
}


static status
overlapGraphical(Graphical gr, Any obj)
{ if ( instanceOfObject(obj, ClassGraphical) )
    return overlapArea(getAreaGraphical(gr), getAreaGraphical(obj));
  else
    return overlapArea(getAreaGraphical(gr), obj);
}


		/********************************
		*              VISUAL		*
		********************************/

static Any
getContainedInGraphical(Graphical gr)
{ if ( notNil(gr->device) )
  { if ( instanceOfObject(gr->device, ClassTree) )
      answer(getNodeGraphical(gr));

    answer(gr->device);
  }

  fail;
}


status
initialiseNewSlotGraphical(Graphical gr, Variable new)
{ if ( new->name == NAME_shadow )
    setSlotInstance(gr, new, ZERO);
  else if ( new->name == NAME_active )
    setSlotInstance(gr, new, ON);

  succeed;
}

		 /*******************************
		 *	    POSTSCRIPT		*
		 *******************************/

extern postscriptGraphical(Any obj);

static status
drawPostScriptGraphical(Graphical gr)
{ Image i;

  if ( (i=checkType(gr, nameToType(NAME_image), gr)) )
  { BitmapObj bm = answerObject(ClassBitmap, i, 0);
    
    setGraphical(bm, gr->area->x, gr->area->y, DEFAULT, DEFAULT);
    send(bm, NAME_DrawPostScript, 0);
    doneObject(bm);
    doneObject(i);

    succeed;
  }

  fail;
}


status
makeClassGraphical(Class class)
{ sourceClass(class, makeClassGraphical, __FILE__, "$Revision$");

  localClass(class, NAME_device, NAME_organisation, "device*", NAME_get,
	     "Device I'm displayed on");
  localClass(class, NAME_area, NAME_area, "area", NAME_none,
	     "Bounding box of affected pixels");
  localClass(class, NAME_displayed, NAME_visibility, "bool", NAME_get,
	     "If @on, graphical is visible");
  localClass(class, NAME_pen, NAME_appearance, "0..", NAME_get,
	     "Thickness of drawing pen");
  localClass(class, NAME_texture, NAME_appearance, "texture_name", NAME_get,
	     "Stipple pattern of drawing pen");
  localClass(class, NAME_colour, NAME_appearance, "[colour|pixmap]", NAME_get,
	     "Colour of drawing pen");
  localClass(class, NAME_handles, NAME_relation, "chain*", NAME_none,
	     "Connection points for connections");
  localClass(class, NAME_connections, NAME_relation, "chain*", NAME_none,
	     "Connections (links) to other graphicals");
  localClass(class, NAME_name, NAME_name, "name", NAME_both,
	     "Name of graphical");
  localClass(class, NAME_selected, NAME_selection, "bool", NAME_get,
	     "If @on, I'm selected");
  localClass(class, NAME_inverted, NAME_appearance, "bool", NAME_get,
	     "If @on, invert bounding box after painting");
  localClass(class, NAME_active, NAME_event, "bool", NAME_get,
	     "If @off, greyed out and insensitive");
  localClass(class, NAME_cursor, NAME_cursor, "cursor*", NAME_get,
	     "Cursor when in focus of events");
  localClass(class, NAME_requestCompute, NAME_update, "any*", NAME_get,
	     "Graphical requests recomputing");

  termClass(class, "graphical", 4, NAME_x, NAME_y, NAME_width, NAME_height);
  saveStyleVariableClass(class, NAME_device, NAME_nil);
  cloneStyleVariableClass(class, NAME_device, NAME_nil);
  setRedrawFunctionClass(class, RedrawAreaGraphical);

  storeMethod(class, NAME_area, areaGraphical);
  storeMethod(class, NAME_colour, colourGraphical);
  storeMethod(class, NAME_cursor, cursorGraphical);
  storeMethod(class, NAME_device, deviceGraphical);
  storeMethod(class, NAME_displayed, displayedGraphical);
  storeMethod(class, NAME_pen, penGraphical);
  storeMethod(class, NAME_texture, textureGraphical);
  storeMethod(class, NAME_selected, selectedGraphical);
  storeMethod(class, NAME_inverted, invertedGraphical);
  storeMethod(class, NAME_active, activeGraphical);

  sendMethod(class, NAME_initialise, DEFAULT,
	     4, "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Create from XYWH",
	     initialiseGraphical);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Erase from device",
	     unlinkGraphical);
  sendMethod(class, NAME_geometry, NAME_resize, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Resize graphical",
	     geometryGraphical);
  sendMethod(class, NAME_requestGeometry, NAME_resize, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Request resize for graphical",
	     requestGeometryGraphical);
  sendMethod(class, NAME_paintSelected, NAME_repaint, 0,
	     "Paint selection feedback",
	     paintSelectedGraphical);
  sendMethod(class, NAME_center, NAME_area, 1, "point",
	     "Move to make point the center",
	     centerGraphical);
  sendMethod(class, NAME_centerX, NAME_area, 1, "int",
	     "Move horizontal to make int the <-x_center",
	     centerXGraphical);
  sendMethod(class, NAME_centerY, NAME_area, 1, "int",
	     "Move vertical to make int the <-y_center",
	     centerYGraphical);
  sendMethod(class, NAME_corner, NAME_area, 1, "point",
	     "Resize to make opposite of origin point",
	     cornerGraphical);
  sendMethod(class, NAME_cornerX, NAME_area, 1, "int",
	     "Resize to set X of ->corner",
	     cornerXGraphical);
  sendMethod(class, NAME_cornerY, NAME_area, 1, "int",
	     "Resize to set Y of ->corner",
	     cornerYGraphical);
  sendMethod(class, NAME_focus, NAME_focus, 3,
	     "recogniser=[recogniser]", "cursor=[cursor]", "button=[name]",
	     "Set window focus to this graphical",
	     focusGraphical);
  sendMethod(class, NAME_focusCursor, NAME_focus, 1, "cursor*",
	     "Set cursor until focus in released",
	     focusCursorGraphical);
  sendMethod(class, NAME_WantsKeyboardFocus, NAME_focus, 0,
	     "Test if graphicals wants keyboard events (fail)",
	     WantsKeyboardFocusGraphical);
  sendMethod(class, NAME_displayOn, NAME_organisation, 1, "device*",
	     "Set device and ensure ->displayed: @on",
	     displayOnGraphical);
  sendMethod(class, NAME_expose, NAME_stacking, 1, "[graphical]",
	     "Place graphical on top or above argument",
	     exposeGraphical);
  sendMethod(class, NAME_handle, NAME_relation, 1, "handle",
	     "Add connection point for connection",
	     handleGraphical);
  sendMethod(class, NAME_height, NAME_area, 1, "int",
	     "Set height",
	     heightGraphical);
  sendMethod(class, NAME_hide, NAME_stacking, 1, "[graphical]",
	     "Place in background or below argument",
	     hideGraphical);
  sendMethod(class, NAME_flush, NAME_animate, 0,
	     "Flush changes to the display",
	     flushGraphical);
  sendMethod(class, NAME_synchronise, NAME_animate, 1, "[always=bool]",
	     "->flush and process all events",
	     synchroniseGraphical);
  sendMethod(class, NAME_move, NAME_area, 1, "point",
	     "Move origin to argument",
	     positionGraphical);
  sendMethod(class, NAME_position, NAME_area, 1, "point",
	     "Move origin to argument (as ->move)",
	     positionGraphical);
  sendMethod(class, NAME_relativeMove, NAME_area, 1, "point",
	     "Move origin by argument",
	     relativeMoveGraphical);
  sendMethod(class, NAME_rotate, NAME_rotate, 1, "int",
	     "Rotate (multiple of 90) degrees",
	     rotateGraphical);
  sendMethod(class, NAME_resize, NAME_area, 3,
	     "factor_x=real", "factor_y=[real]", "origin=[point]",
	     "Resize graphical with specified factor",
	     resizeGraphical);
  sendMethod(class, NAME_toggleSelected, NAME_selection, 0,
	     "Change selected status",
	     toggleSelectedGraphical);
  sendMethod(class, NAME_set, NAME_area, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Request new X, Y, W and H for graphical",
	     setGraphical);
  sendMethod(class, NAME_doSet, NAME_area, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Set X, Y, W and H for graphical",
	     doSetGraphical);
  sendMethod(class, NAME_size, NAME_area, 1, "size",
	     "Resize to specified size",
	     sizeGraphical);
  sendMethod(class, NAME_swap, NAME_stacking, 1, "graphical",
	     "Swap stacking order of graphicals",
	     swapGraphical);
  sendMethod(class, NAME_width, NAME_area, 1, "int",
	     "Width of graphical",
	     widthGraphical);
  sendMethod(class, NAME_x, NAME_area, 1, "int",
	     "Move graphical horizontally",
	     xGraphical);
  sendMethod(class, NAME_y, NAME_area, 1, "int",
	     "Move graphical vertically",
	     yGraphical);
  sendMethod(class, NAME_Postscript, NAME_postscript, 0,
	     "Create PostScript",
	     postscriptGraphical);
  sendMethod(class, NAME_layout, NAME_layout, 5,
	     "attract=[real]", "nominal=[real]",
	     "repell=[real]", "adapt=[int]", "iterations=[int]",
	     "Make graph-layout for connected graphicals",
	     layoutGraphical);
  sendMethod(class, NAME_normalise, NAME_area, 0,
	     "Make top-left corner the origin",
	     normaliseGraphical);
  sendMethod(class, NAME_orientation, NAME_area,
	     1, "{north_west,south_east,north_east,south_east}",
	     "Put origin at {north,south}_{west,east}",
	     orientationGraphical);
  sendMethod(class, NAME_bell, NAME_report, 1, "[int]",
	     "Ring the bell on associated display",
	     bellGraphical);
  sendMethod(class, NAME_alert, NAME_report, 0,
	     "Alert visual or using the bell",
	     alertGraphical);
  sendMethod(class, NAME_flash, NAME_report, 0,
	     "Alert visual by temporary inverting",
	     flashGraphical);
  sendMethod(class, NAME_keyboardFocus, NAME_event, 1, "[bool]",
	     "Get <-window's keyboard_focus if ->_wants_keyboard_focus",
	     keyboardFocusGraphical);
  sendMethod(class, NAME_generateEvent, NAME_event, 1, "event_id",
	     "Generate named event for graphical",
	     generateEventGraphical);
  sendMethod(class, NAME_event, NAME_event, 1, "event",
	     "Handle a user-event",
	     eventGraphical);
  sendMethod(class, NAME_recogniser, NAME_event, 1, "recogniser",
	     "Add recogniser for user events (last)",
	     recogniserGraphical);
  sendMethod(class, NAME_prependRecogniser, NAME_event, 1, "recogniser",
	     "Add recogniser for user events (first)",
	     prependRecogniserGraphical);
  sendMethod(class, NAME_deleteRecogniser, NAME_event, 1, "recogniser",
	     "Delete a recogniser",
	     deleteRecogniserGraphical);	     
  getMethod(class, NAME_allRecognisers, NAME_meta, "chain", 1,
	    "create=[bool]",
	    "Chain with all recognisers",
	    getAllRecognisersGraphical);
  sendMethod(class, NAME_popup, NAME_menu, 1, "popup",
	     "Associate a popup menu with the graphical",
	     popupGraphical);
  sendMethod(class, NAME_connect, NAME_relation, 4,
	     "to=[graphical]", "link=[link]",
	     "to_kind=[name]", "from_kind=[name]",
	     "Create a connection to another graphical",
	     connectGraphical);
  sendMethod(class, NAME_connected, NAME_relation, 4,
	     "to=[graphical]", "link=[link]",
	     "to_kind=[name]", "from_kind=[name]",
	     "Test if graphical has specified connection",
	     connectedGraphical);
  sendMethod(class, NAME_disconnect, NAME_relation, 4,
	     "to=[graphical]", "link=[link]",
	     "to_kind=[name]", "from_kind=[name]",
	     "Delete matching connections",
	     disconnectGraphical);
  sendMethod(class, NAME_pointer, NAME_pointer, 1, "point",
	     "Warp pointer relative to graphical",
	     pointerGraphical);
  sendMethod(class, NAME_inEventArea, NAME_event, 2, "x=int", "y=int",
	     "Test if (X,Y) is in the sensitive area for events",
	     inEventAreaGraphical);
  sendMethod(class, NAME_requestCompute, NAME_update, 1, "[any]*",
	     "Request a ->compute on next repaint",
	     requestComputeGraphical);
  sendMethod(class, NAME_compute, NAME_update, 0,
	     "Update status of graphical",
	     computeGraphical);
  sendMethod(class, NAME_reparent, NAME_organisation, 0,
	     "Graphicals parent-chain has changed",
	     reparentGraphical);
  sendMethod(class, NAME_redraw, NAME_repaint, 1, "[area]",
	     "Request to repaint indicated area",
	     redrawGraphical);
  sendMethod(class, NAME_overlap, NAME_stacking, 1, "graphical|area",
	     "Succeeds if graphical overlaps with argument",
	     overlapGraphical);
  sendMethod(class, NAME_key, NAME_accelerator, 1, "name",
	     "Accelerator-key pressed (fail)",
	     keyGraphical);
  sendMethod(class, NAME_autoAlign, NAME_layout, 1, "bool",
	     "Dialog_item integration",
	     autoAlignGraphical);
  sendMethod(class, NAME_reference, NAME_layout, 1, "point",
	     "Dialog item integration",
	     referenceGraphical);
  sendMethod(class, NAME_alignment, NAME_layout, 1,
	     "{left,center,right,column}",
	     "Dialog item integration",
	     alignmentGraphical);
  sendMethod(class, NAME_autoLabelAlign, NAME_layout, 1, "bool",
	     "Dialog item integration",
	     autoLabelAlignGraphical);
  sendMethod(class, NAME_autoValueAlign, NAME_layout, 1, "bool",
	     "Dialog item integration",
	     autoValueAlignGraphical);

  sendMethod(class, NAME_initialiseNewSlot, NAME_file, 1, "new=variable",
	     "Assigns <-shadow to ZERO, active to @off",
	     initialiseNewSlotGraphical);

  sendMethod(class, NAME_below, NAME_layout, 1, "graphical*",
	     "Put me below argument",
	     aboveGraphical);
  sendMethod(class, NAME_above, NAME_layout, 1, "graphical*",
	     "Put me above argument",
	     belowGraphical);
  sendMethod(class, NAME_left, NAME_layout, 1, "graphical*",
	     "Put me left of argument",
	     rightGraphical);
  sendMethod(class, NAME_right, NAME_layout, 1, "graphical*",
	     "Put me right of argument",
	     leftGraphical);
  sendMethod(class, NAME_apply, NAME_apply, 1, "[bool]",
	     "Virtual method",
	     virtualObject);
  sendMethod(class, NAME_restore, NAME_apply, 0,
	     "Virtual method",
	     virtualObject);
  sendMethod(class, NAME_draw, NAME_repaint, 2,
	     "offset=[point]", "area=[area]",
	     "Draw specified area",
	     drawGraphical);
  sendMethod(class, NAME_DrawPostScript, NAME_postscript, 0,
	     "Create PostScript using intermediate image object",
	     drawPostScriptGraphical);

  getMethod(class, NAME_absolutePosition, NAME_area, "point", 1, "[device]",
	    "Get position relative to device (or window)",
	    getAbsolutePositionGraphical);
  getMethod(class, NAME_absoluteX, NAME_area, "int", 1, "[device]",
	    "Get X-position relative to device",
	    getAbsoluteXGraphical);
  getMethod(class, NAME_absoluteY, NAME_area, "int", 1, "[device]",
	    "Get Y-position relative to device",
	    getAbsoluteYGraphical);
  getMethod(class, NAME_area, NAME_area, "area", 0,
	    "->compute and return area slot",
	    getAreaGraphical);
  getMethod(class, NAME_boundingBox, NAME_postscript, "area", 0,
	    "Same as <-area; used for PostScript",
	    getBoundingBoxGraphical);
  getMethod(class, NAME_center, NAME_area, "point", 0,
	    "New point representing center",
	    getCenterGraphical);
  getMethod(class, NAME_corner, NAME_area, "point", 0,
	    "New point from point opposite origin",
	    getCornerGraphical);
  getMethod(class, NAME_leftSide, NAME_area, "int", 0,
	    "Left-side of graphical",
	    getLeftSideGraphical);
  getMethod(class, NAME_rightSide, NAME_area, "int", 0,
	    "Right-side of graphical",
	    getRightSideGraphical);
  getMethod(class, NAME_topSide, NAME_area, "int", 0,
	    "Top-side of graphical",
	    getTopSideGraphical);
  getMethod(class, NAME_bottomSide, NAME_area, "int", 0,
	    "Bottom-side of graphical",
	    getBottomSideGraphical);
  getMethod(class, NAME_cornerX, NAME_area, "int", 0,
	    "X-coordinate of corner",
	    getCornerXGraphical);
  getMethod(class, NAME_cornerY, NAME_area, "int", 0,
	    "Y-coordinate of corner",
	    getCornerYGraphical);
  getMethod(class, NAME_centerX, NAME_area, "int", 0,
	    "X-coordinate of center",
	    getCenterXGraphical);
  getMethod(class, NAME_centerY, NAME_area, "int", 0,
	    "Y-coordinate of center",
	    getCenterYGraphical);
  getMethod(class, NAME_handle, NAME_relation, "handle", 1, "name",
	    "Find handle with given name",
	    getHandleGraphical);
  getMethod(class, NAME_handles, NAME_relation, "chain", 3,
	    "near=[point]", "kind=[name]", "distance=[int]",
	    "New chain with matching handles",
	    getHandlesGraphical);
  getMethod(class, NAME_height, NAME_area, "int", 0,
	    "Height of graphical",
	    getHeightGraphical);
  getMethod(class, NAME_window, NAME_organisation, "window", 0,
	    "Window graphical is displayed on",
	    getWindowGraphical);
  getMethod(class, NAME_frame, NAME_organisation, "frame", 0,
	    "Frame graphical is displayed on",
	    getFrameGraphical);
  getMethod(class, NAME_display, NAME_organisation, "display", 0,
	    "Display graphical is displayed on",
	    getDisplayGraphical);
  getMethod(class, NAME_position, NAME_area, "point", 0,
	    "New point representing origin",
	    getPositionGraphical);
  getMethod(class, NAME_size, NAME_area, "size", 0,
	    "New size representing size",
	    getSizeGraphical);
  getMethod(class, NAME_width, NAME_area, "int", 0,
	    "Width of graphical",
	    getWidthGraphical);
  getMethod(class, NAME_x, NAME_area, "int", 0,
	    "X or origin",
	    getXGraphical);
  getMethod(class, NAME_y, NAME_area, "int", 0,
	    "Y of origin",
	    getYGraphical);
  getMethod(class, NAME_commonDevice, NAME_organisation, "device", 1,
	    "with=graphical",
	    "Deepest device both are displayed on",
	    getCommonDeviceGraphical);
  getMethod(class, NAME_distance, NAME_organisation, "int", 1, "graphical",
	    "Closest distance between areas",
	    getDistanceGraphical);
  getMethod(class, NAME_distanceX, NAME_organisation, "int", 1, "graphical",
	    "Distance between graphicals's in X-direction",
	    getDistanceXGraphical);
  getMethod(class, NAME_distanceY, NAME_organisation, "int", 1, "graphical",
	    "Distance between graphicals's in Y-direction",
	    getDistanceYGraphical);
  getMethod(class, NAME_postscript, NAME_postscript, "string", 2,
	    "landscape=[bool]", "maximum_area=[area]",
	    "New string holding PostScript description",
	    getPostscriptObject);
  getMethod(class, NAME_network, NAME_relation, "chain", 3,
	    "link=[link]", "from_kind=[name]", "to_kind=[name]",
	    "New chain with connected graphicals",
	    getNetworkGraphical);
  getMethod(class, NAME_isDisplayed, NAME_visibility, "bool", 1, "[device]",
	    "@on if graphical is visible on device",
	    getIsDisplayedGraphical);
  getMethod(class, NAME_displayPosition, NAME_area, "point", 0,
	    "Position relative to display",
	    getDisplayPositionGraphical);
  getMethod(class, NAME_orientation, NAME_area,
	    "{north_west,south_east,north_east,south_east}", 0,
	    "Current orientation",
	    getOrientationGraphical);
  getMethod(class, NAME_displayColour, NAME_appearance, "colour|pixmap", 0,
	    "Colour graphical is displayed in",
	    getDisplayColourGraphical);
  getMethod(class, NAME_node, NAME_nodes, "node", 0,
	    "When image of node in tree, find the node",
	    getNodeGraphical);
  getMethod(class, NAME_connections, NAME_relation, "chain", 4,
	    "to=[graphical]", "link=[link]",
	    "from_kind=[name]", "to_kind=[name]",
	    "New chain with matching connections",
	    getConnectionsGraphical);
  getMethod(class, NAME_handlePosition, NAME_relation, "point", 2,
	    "name=name", "device=[device]",
	    "New point with position of handle",
	    getHandlePositionGraphical);
  getMethod(class, NAME_containedIn, DEFAULT, "device|node", 0,
	    "Device I'm contained in",
	    getContainedInGraphical);
  getMethod(class, NAME_convert, NAME_conversion, "graphical", 1, "object",
	    "Convert using <-image",
	    getConvertGraphical);
  getMethod(class, NAME_popup, NAME_menu, "popup", 0,
	    "Associated ->popup",
	    getPopupGraphical);

  getMethod(class, NAME_autoAlign, NAME_layout, "bool", 0,
	    "Dialog_item integration",
	    getAutoAlignGraphical);
  getMethod(class, NAME_alignment, NAME_layout, "name", 0,
	    "Dialog_item integration",
	    getAlignmentGraphical);
  getMethod(class, NAME_autoLabelAlign, NAME_layout, "bool", 0,
	    "Dialog_item integration",
	    getAutoLabelAlignGraphical);
  getMethod(class, NAME_autoValueAlign, NAME_layout, "bool", 0,
	    "Dialog_item integration",
	    getAutoValueAlignGraphical);
  getMethod(class, NAME_reference, NAME_layout, "point", 0,
	    "Dialog_item integration; fails",
	    getFailObject);
  getMethod(class, NAME_left, NAME_layout, "graphical", 0,
	    "Dialog_item integration; fails",
	    getFailObject);
  getMethod(class, NAME_right, NAME_layout, "graphical", 0,
	    "Dialog_item integration; fails",
	    getFailObject);
  getMethod(class, NAME_above, NAME_layout, "graphical", 0,
	    "Dialog_item integration; fails",
	    getFailObject);
  getMethod(class, NAME_below, NAME_layout, "graphical", 0,
	    "Dialog_item integration; fails",
	    getFailObject);

  attach_resource(class, "visual_bell", "bool", "@on",
		  "@on: flash; @off: ring bell on ->alert");
  attach_resource(class, "visual_bell_duration", "int", "100",
		  "Lenght of flash in milliseconds");
  attach_resource(class, "selection_handles",
		  "{corners,sides,corners_and_sides,line}*",
		  "corners_and_sides",
		  "Visual feedback of <->selected");
  attach_resource(class, "colour", "[colour|pixmap]", "@default",
		  "Default colour for this object");
  attach_resource(class, "inactive_colour", "colour|pixmap*", "grey",
		  "Colour when <-active == @off");

  ChangedWindows = globalObject(NAME_changedWindows, ClassChain, 0);

  succeed;
}

