/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static void	clipDevice P((Device, Area));
static void	unclipDevice P((Device));
static status	updateConnectionsDevice(Device dev, Int level);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class device is an abstract superclass used to define method common
to  Pictures and  Figures:  manipulating  a chain   of  graphicals and
dispatching events.

A device is a subclass of graphical and thus can be displayed on other
devices.

Devices  maintain the  graphical's  attribute  <->area  to reflect the
bounding box  of   all displayed  graphicals  (e.g.   graphicals  with
<->displayed equals @on).  To the X-Y  coordinate of this bounding box
the  <->offset is  added  to  ensure  smooth intergration  with  class
figure.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		/********************************
		*        CREATE/DESTROY		*
		********************************/

status
initialiseDevice(Device dev)
{ initialiseGraphical(dev, ZERO, ZERO, ZERO, ZERO);

  assign(dev, level, ZERO);
  assign(dev, offset, newObject(ClassPoint, 0));
  assign(dev, graphicals, newObject(ClassChain, 0));
  assign(dev, badBoundingBox, OFF);
  assign(dev, badFormat, OFF);
  assign(dev, format, NIL);
  assign(dev, pointed, newObject(ClassChain, 0));
  assign(dev, clip_area, NIL);
  assign(dev, recompute, newObject(ClassChain, 0));

  succeed;
}


status
unlinkDevice(Device dev)
{ if ( notNil(dev->graphicals) )
  { Graphical gr;

    for_chain(dev->graphicals, gr, DeviceGraphical(gr, NIL));
  }
  
  return unlinkGraphical((Graphical) dev);
}

		/********************************
		*             CURSOR		*
		********************************/

CursorObj
getFindCursorDevice(Device dev)
{ CursorObj rval = NIL;
  CursorObj c2;
  Cell cell;

  for_cell(cell, dev->pointed)
  { Graphical gr = cell->value;

    if ( instanceOfObject(gr, ClassDevice) &&
	 notNil(c2 = getFindCursorDevice((Device) gr)) )
      rval = c2;
    else if ( notNil(gr->cursor) )
    { rval = gr->cursor;
      DEBUG(NAME_cursor, printf("get cursor %s from %s\n", pp(rval), pp(gr)));
    }

    if ( notNil(rval) )
      break;
  }

  if ( isNil(rval) && notNil(dev->cursor) )
  { rval = dev->cursor;
    DEBUG(NAME_cursor, printf("get cursor %s from device %s\n",
			      pp(rval), pp(dev)));
  }

  answer(rval);
}


		/********************************
		*         EVENT HANDLING	*
		********************************/

static Chain
get_pointed_objects_device(Device dev, Int x, Int y, Chain ch)
{ Cell cell;

  if ( isDefault(ch) )
    ch = answerObject(ClassChain, 0);
  else
    clearChain(ch);

  for_cell(cell, dev->graphicals)
  { register Graphical gr = cell->value;

    if ( gr->displayed == ON &&
	 inEventAreaGraphical(gr, x, y) )
      prependChain(ch, gr);
  }

  if ( notDefault(ch) )
    answer(ch);

  fail;
}


Chain
getPointedObjectsDevice(Device dev, Any pos, Chain ch)
{ Int x, y;

  if ( instanceOfObject(pos, ClassPoint) )
  { Point pt = pos;

    x = pt->x;
    y = pt->y;
  } else /*if ( instanceOfObject(pos, ClassEvent) )*/
    get_xy_event(pos, dev, OFF, &x, &y);

  return get_pointed_objects_device(dev, x, y, ch);
}

#define MAX_ACTIVE 250			/* Objects under the mouse */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
updatePointedDevice() updates the <->pointed chain of the device.
The <->pointed chain is a  chain  holding all events that overlap with
the mouse  position and  are  editable  and  displayed.   It sends  an
area_enter event to all graphicals  that have  been added to the chain
and an area_exit event to all graphicals that have been deleted to the
chain.  Care is taken to prevent this function from creating  too many
intermediate objects as is it called very frequently.

The event area_cancel is ok, but area_resume should verify the buttons
are in the same  state as when  the area is  left and at  least one is
down.   This requires us  to store the status  of the button with  the
graphical object, costing us an additional 4 bytes on  each graphical.
To do or not to do?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
updatePointedDevice(Device dev, EventObj ev)
{ Cell cell, c2;
  Graphical active[MAX_ACTIVE];
  int n, an = 0;
  Int x, y;
  Name enter, exit;

  if ( allButtonsUpEvent(ev) )
  { enter = NAME_areaEnter;
    exit  = NAME_areaExit;
  } else
  { enter = NAME_areaResume;
    exit  = NAME_areaCancel;
  }

					/* Exit event: leave all children */
  if ( isAEvent(ev, NAME_areaExit) )
  { for_cell(cell, dev->pointed)
      generateEventGraphical(cell->value, exit);

    clearChain(dev->pointed);
    succeed;
  }

  get_xy_event(ev, dev, OFF, &x, &y);

					/* See which graphicals are left */
  for_cell_save(cell, c2, dev->pointed)
  { register Graphical gr = cell->value;

    if ( gr->displayed == OFF || !inEventAreaGraphical(gr, x, y) )
    { DEBUG(NAME_event, printf("Leaving %s\n", pp(gr)));
      deleteChain(dev->pointed, gr);
      generateEventGraphical(gr, exit);
    }
  }
  
					/* See which graphicals are entered */
  for_cell(cell, dev->graphicals)
  { register Graphical gr = cell->value;

    if ( gr->displayed == ON && inEventAreaGraphical(gr, x, y) )
    { active[an++] = gr;

      if ( memberChain(dev->pointed, gr) != SUCCEED )
      { DEBUG(NAME_event, printf("Entering %s\n", pp(gr)));
        generateEventGraphical(gr, enter);
      }

      if ( an == MAX_ACTIVE )		/* Shift to keep top ones */
      { int n;
        for( n = 0; n < MAX_ACTIVE-1; n++ )
	  active[n] = active[n+1];
	an--;
      }
    }
  }
    
					/* Update the ->pointed chain */
  for( cell = dev->pointed->head, n = an-1; n >= 0; n--, cell = cell->next )
  { if ( isNil(cell) )			/* Chain is out; extend it */
    { for( ; n >=0; n-- )
        appendChain(dev->pointed, active[n]);
      break;
    }

    cellValueChain(dev->pointed, PointerToInt(cell), active[n]);
  }
  
  while( notNil(cell) )			/* Remove the tail of the chain */
  { c2 = cell->next;
    deleteChain(dev->pointed, cell->value);
    cell = c2;
  }

  succeed;
}


status
inspectDevice(Device dev, EventObj ev)
{ Cell cell;

  for_cell(cell, dev->pointed)
  { if ( instanceOfObject(cell->value, ClassDevice) )
    { if ( inspectDevice(cell->value, ev) )
    	succeed;
    } else
    { if ( inspectDisplay(CurrentDisplay(dev), cell->value, ev) )
	succeed;
    }
  }

  return inspectDisplay(CurrentDisplay(dev), (Graphical) dev, ev);
}


static Graphical
get_find_device(Device dev, Int x, Int y, Code cond)
{ LocalArray(Graphical, grv, valInt(dev->graphicals->size));
  int i, grn;
  Cell cell;

  grn=0;
  for_cell(cell, dev->graphicals)
    grv[grn++] = cell->value;

  for(i=grn-1; i >= 0; i--)
  { Graphical gr = grv[i];

    if ( notDefault(x) && !inEventAreaGraphical(gr, x, y) )
      continue;

    if ( instanceOfObject(gr, ClassDevice) )
    { Device dev2 = (Device) gr;
      Any rval;

      if ( (rval=get_find_device(dev2,
				 isDefault(x) ? x : sub(x, dev2->offset->x),
				 isDefault(y) ? y : sub(y, dev2->offset->y),
				 cond)) )
	answer(rval);
    } else
    { if ( isDefault(cond) ||
	   forwardCodev(cond, 1, (Any *)&gr) )
	answer(gr);
    }
  }

  if ( isDefault(cond) ||
       forwardCodev(cond, 1, (Any *)&dev) )
    answer((Graphical) dev);

  fail;
}


static Graphical
getFindDevice(Device dev, Any location, Code cond)
{ Int x, y;

  if ( instanceOfObject(location, ClassEvent) )
    get_xy_event(location, dev, OFF, &x, &y);
  else if ( isDefault(location) )
  { x = y = (Int) DEFAULT;
  } else
  { Point p = location;

    x = p->x;
    y = p->y;
  }

  return get_find_device(dev, x, y, cond);
}


status
eventDevice(Any obj, EventObj ev)
{ Device dev = obj;

  if ( dev->active != OFF )
  { Cell cell;

    updatePointedDevice(dev, ev);
  
    for_cell(cell, dev->pointed)
      if ( postEvent(ev, cell->value, DEFAULT) )
	succeed;

    return eventGraphical(dev, ev);
  }

  fail;
}


static status
typedDevice(Device dev, EventId id, Bool delegate)
{ Any key = characterName(id);
  Graphical gr;

  for_chain(dev->graphicals, gr,
	    if ( sendv(gr, NAME_key, 1, &key) )
	      succeed);

  if ( delegate == ON && notNil(dev->device) )
    return send(dev->device, NAME_typed, id, delegate, 0);

  fail;
}


status
advanceDevice(Device dev, Graphical gr)
{ Cell cell;
  int skip = TRUE;
  Graphical first = NIL;
  PceWindow sw;

  if ( isDefault(gr) )
    gr = NIL;

  TRY( sw = getWindowGraphical((Graphical) dev) );

  for_cell(cell, dev->graphicals)
  { if ( skip )
    { if ( isNil(first) &&
	   send(cell->value, NAME_WantsKeyboardFocus, 0) )
	first = cell->value;
      if ( cell->value == gr )
        skip = FALSE;

      continue;
    }
      
    if ( send(cell->value, NAME_WantsKeyboardFocus, 0) )
    { keyboardFocusWindow(sw, cell->value);
      succeed;
    }
  }
  
  if ( ((Device) sw != dev) && !(isNil(gr) && notNil(first)) )
  { send(dev->device, NAME_advance, dev, 0);
  } else
  { if ( notNil(first) )
      keyboardFocusWindow(sw, first);
    else
      keyboardFocusWindow(sw, NIL);
  }

  succeed;
}


		/********************************
		*       REPAINT MANAGEMENT	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The device's repaint manager is responsible for  keeping  track of the
devices area (the bounding box of its displayed graphicals) and of the
area that  needs repainting if  a ->RedrawArea is received.  It should
issue to appropriate ->RedrawArea request on its associated graphicals
if it receives an ->RedrawArea from its parent.

A number of changes are recognised:

   *) A graphical is added to the device or its displayed attribute has
      changed to @on.
   *) A graphical is erased from the device or its displayed attribute
      has changed to @off.
   *) The image of a graphical has changed.
   *) The area of a graphical has changed.

Graphicals indicate changes through the following call:

      CHANGING_GRAPHICAL(gr,
	  <code>
	  [changedImageGraphical(gr, x, y, w, h)]
	  [changedEntireImageGraphical(gr)]
      )
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
requestComputeDevice(Device dev, Any val)
{ DEBUG(NAME_compute, printf("requestComputeDevice(%s)\n", pp(dev)));
  assign(dev, badBoundingBox, ON);
  assign(dev, badFormat, ON);

  return requestComputeGraphical(dev, val);
}


status
computeGraphicalsDevice(Device dev)
{ Chain ch = dev->recompute;

  while( !emptyChain(ch) )		/* tricky! */
  { Cell cell;
    int i, size = valInt(ch->size);
    ArgVector(array, size);

    for(i=0, cell = ch->head; notNil(cell); cell = cell->next)
      array[i++] = cell->value;

    clearChain(ch);
    for(i=0; i<size; i++)
    { Graphical gr = array[i];

      if ( !isFreedObj(gr) && notNil(gr->request_compute) )
      { qadSendv(gr, NAME_compute, 0, NULL);
	assign(gr, request_compute, NIL);
      }
    }
  }

  succeed;
}
   

status
computeDevice(Any obj)
{ Device dev = obj;

  if ( notNil(dev->request_compute) )
  { computeGraphicalsDevice(dev);
    computeFormatDevice(dev);
    computeBoundingBoxDevice(dev);

    assign(dev, request_compute, NIL);
  }

  succeed;
}


status
computeBoundingBoxDevice(Device dev)
{ if ( dev->badBoundingBox == ON )
  { Cell cell;
    Area a = dev->area;
    Int ax, ay, aw, ah;

    ax = a->x; ay = a->y; aw = a->w; ah = a->h;
    clearArea(a);

    DEBUG(NAME_compute,
	  printf("computeBoundingBoxDevice(%s) %ld %ld %ld %ld\n",
		 pp(dev),
		 valInt(ax), valInt(ay), valInt(aw), valInt(ah)));


    for_cell(cell, dev->graphicals)
    { Graphical gr = cell->value;

      if ( gr->displayed == ON )
	unionNormalisedArea(a, gr->area);
    }

    relativeMoveArea(a, dev->offset);

    if ( ax != a->x || ay != a->y || aw != a->w || ah != a->h )
    { DEBUG(NAME_compute,
	    printf("                          --> %ld %ld %ld %ld\n",
		   valInt(a->x), valInt(a->y), valInt(a->w), valInt(a->h)));

      if ( notNil(dev->device) )
      { requestComputeDevice(dev->device, DEFAULT);
      	updateConnectionsGraphical((Graphical) dev, sub(dev->level, ONE));
      }

      send(dev, NAME_changedUnion, a, 0);
    }

    if ( notNil(dev->clip_area) )
    { relativeMoveBackArea(a, dev->offset);
      if ( intersectionArea(dev->area, dev->clip_area) == FAIL )
      { assign(dev->area, w, ZERO);
        assign(dev->area, h, ZERO);
      }
      relativeMoveArea(a, dev->offset);
    }

    assign(dev, badBoundingBox, OFF);
  }

  succeed;
}


status
changedUnionDevice(Device dev, Area a)
{ succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Our  parent requests  us to  repaint an area.    This  area is in  the
coordinate system of  the device we  are  displayed on.  The requested
repaint area may be larger than the area of myself.

This algorithm can be made more clever on  a number of  points.  First
of all we could be  more  clever  with none-square graphicals, notably
lines.  Next,  we could determine that  objects  are obscured by other
objects and thus  do not need to be  repainted.  We  will  leave these
optimisations for later.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
RedrawAreaDevice(Device dev, Area a)
{ Cell cell;
  Int ax = a->x, ay = a->y, aw = a->w, ah = a->h;
  Point offset = dev->offset;
  int ox = valInt(offset->x);
  int oy = valInt(offset->y);

  if ( aw == ZERO || ah == ZERO )
    succeed;

  DEBUG(NAME_redraw, printf("RedrawAreaDevice(%s, %ld %ld %ld %ld)\n",
			    pp(dev), 
			    valInt(a->x), valInt(a->y),
			    valInt(a->w), valInt(a->h)));

  assign(a, x, toInt(valInt(a->x) - ox));
  assign(a, y, toInt(valInt(a->y) - oy));
  r_offset(ox, oy);

  if ( notNil(dev->clip_area) )
  { if ( !intersectionArea(a, dev->clip_area) )
      succeed;

    clipDevice(dev, a);
  }

  for_cell(cell, dev->graphicals)
  { Graphical gr = cell->value;

    if ( gr->displayed == ON && overlapArea(a, gr->area) )
      RedrawArea(gr, a);
  }

  if ( notNil(dev->clip_area) )
    unclipDevice(dev);

  r_offset(-ox, -oy);
  assign(a, x, ax);
  assign(a, y, ay);
  assign(a, w, aw);			/* why? should not change! */
  assign(a, h, ah);

  return RedrawAreaGraphical(dev, a);
}


static void
clipDevice(Device dev, Area a)
{ d_clip(valInt(a->x), valInt(a->y), valInt(a->w), valInt(a->h));
}


static void
unclipDevice(Device dev)
{ d_clip_done();
}


		/********************************
		*         DISPLAY/ERASE		*
		********************************/


static status
clearDevice(Device dev)
{ Chain ch = dev->graphicals;

  while( !emptyChain(ch) )
    eraseDevice(dev, getHeadChain(ch));

  succeed;
}


status
displayDevice(Any Dev, Any Gr, Point pos)
{ Device dev = Dev;
  Graphical gr = Gr;
  
  if ( notDefault(pos) )
    setGraphical(gr, pos->x, pos->y, DEFAULT, DEFAULT);

  DeviceGraphical(gr, dev);
  DisplayedGraphical(gr, ON);

  succeed;
}


status
appendDevice(Device dev, Graphical gr)
{ appendChain(dev->graphicals, gr);
  assign(gr, device, dev);
  if ( notNil(gr->request_compute) )
    appendChain(dev->recompute, gr);

  if ( gr->displayed == ON )
  { assign(gr, displayed, OFF);
    displayedGraphicalDevice(dev, gr, ON);
  }

  qadSendv(gr, NAME_reparent, 0, NULL);

  succeed;
}


status
eraseDevice(Device dev, Graphical gr)
{ if ( gr->device == dev )
  { PceWindow sw = getWindowGraphical((Graphical) dev);

    if ( sw != FAIL )
    { if ( sw->keyboard_focus == gr )
	keyboardFocusWindow(sw, NIL);
      if ( sw->focus == gr )
	focusWindow(sw, NIL, NIL, NIL, NIL);
    }

    if ( gr->displayed == ON )
      displayedGraphicalDevice(dev, gr, OFF);

    deleteChain(dev->recompute, gr);
    deleteChain(dev->pointed, gr);
    assign(gr, device, NIL);
    GcProtect(dev, deleteChain(dev->graphicals, gr));
    if ( !isFreedObj(gr) )
      qadSendv(gr, NAME_reparent, 0, NULL);
  }

  succeed;
}


status
displayedGraphicalDevice(Device dev, Graphical gr, Bool val)
{ if ( gr->displayed != val )
  { int solid = onFlag(gr, F_SOLID);

    if ( solid )
      clearFlag(gr, F_SOLID);

    if ( val == ON )
    { assign(gr, displayed, val);
      changedEntireImageGraphical(gr);
    } else
    { changedEntireImageGraphical(gr);
      assign(gr, displayed, val);
    }

    if ( solid )
      setFlag(gr, F_SOLID);

    if ( instanceOfObject(gr, ClassDevice) )
      updateConnectionsDevice((Device) gr, dev->level);
    else
      updateConnectionsGraphical(gr, dev->level);

    requestComputeDevice(dev, DEFAULT);		/* TBD: ON: just union */
  }

  succeed;
}

		/********************************
		*           EXPOSURE		*
		********************************/

status
exposeDevice(Device dev, Graphical gr, Graphical gr2)
{ if ( gr->device != dev || (notDefault(gr2) && gr2->device != dev) )
    fail;

  if ( isDefault(gr2) )
  { addCodeReference(gr);
    deleteChain(dev->graphicals, gr);
    appendChain(dev->graphicals, gr);
    delCodeReference(gr);
  } else
  { moveAfterChain(dev->graphicals, gr, gr2);
    changedEntireImageGraphical(gr2);
  }
  requestComputeDevice(dev, DEFAULT);		/* Actually only needs format */

  changedEntireImageGraphical(gr);

  succeed;
}


status
hideDevice(Device dev, Graphical gr, Graphical gr2)
{ if ( gr->device != dev || (notDefault(gr2) && gr2->device != dev) )
    fail;

  if ( isDefault(gr2) )
  { addCodeReference(gr);
    deleteChain(dev->graphicals, gr);
    prependChain(dev->graphicals, gr);
    delCodeReference(gr);
  } else
  { moveBeforeChain(dev->graphicals, gr, gr2);
    changedEntireImageGraphical(gr2);
  }
  requestComputeDevice(dev, DEFAULT);	/* Actually only needs format */

  changedEntireImageGraphical(gr);

  succeed;
}


status
swapGraphicalsDevice(Device dev, Graphical gr, Graphical gr2)
{ if ( gr->device != dev || (notDefault(gr2) && gr2->device != dev) )
    fail;

  swapChain(dev->graphicals, gr, gr2);

  changedEntireImageGraphical(gr);
  changedEntireImageGraphical(gr2);
  requestComputeDevice(dev, DEFAULT);		/* Actually only needs format */

  succeed;
}


		/********************************
		*          SELECTION		*
		********************************/


static status
selectionDevice(Device dev, Any obj)
{ Cell cell;

  if ( instanceOfObject(obj, ClassChain) )
  { for_cell(cell, dev->graphicals)
    { if ( memberChain(obj, cell->value) )
        send(cell->value, NAME_selected, ON, 0);
      else
	send(cell->value, NAME_selected, OFF, 0);
    }

    succeed;
  }

  for_cell(cell, dev->graphicals)
    send(cell->value, NAME_selected, cell->value == obj ? ON : OFF, 0);

  succeed;
}


static Chain
getSelectionDevice(Device dev)
{ Chain ch = answerObject(ClassChain, 0);
  Cell cell;

  for_cell(cell, dev->graphicals)
  { if ( ((Graphical)cell->value)->selected == ON )
      appendChain(ch, cell->value);
  }

  answer(ch);
}


		/********************************
		*           FORMATTING          *
		*********************************/

static status
formatDevice(Device dev, Any obj, Any arg)
{ status rval = SUCCEED;

  if ( isNil(obj) || instanceOfObject(obj, ClassFormat) )
  { assign(dev, format, obj);
  } else 
  { if ( isNil(dev->format) )
      assign(dev, format, newObject(ClassFormat, 0));

    rval = send(dev->format, (Name)obj, arg, 0);
  }
  requestComputeDevice(dev, DEFAULT);

  return rval;
}

static void
move_graphical(Graphical gr, int x, int y)
{ Int X = toInt(x);
  Int Y = toInt(y);

  if ( X != gr->area->x || Y != gr->area->y )
    setGraphical(gr, X, Y, DEFAULT, DEFAULT);
}

#define MAXCOLS 1024
#define MAXROWS 1024

status
computeFormatDevice(Device dev)
{ Format l;

  if ( dev->badFormat == OFF || isNil(l=dev->format) )
    succeed;

#define HV(h, v) (l->direction == NAME_horizontal ? (h) : (v))
#define MUSTBEVISIBLE(dev, gr) { if (gr->displayed == OFF) continue; }

  if ( l->columns == ON )
  { int cw[MAXCOLS];			/* column widths */
    int rh[MAXROWS];			/* row heights */
    char cf[MAXCOLS];			/* column format */
    int cs = valInt(l->column_sep);	/* column separator size */
    int rs = valInt(l->row_sep);	/* row separator size */
    Cell cell;
    int c, r = 0;
    int cols = valInt(l->width);
    int x = 0;
    int y = 0;

    for(c=0; c < cols; c++)
    { cw[c] = 0;
      cf[c] = 'l';
    }
    
    if ( notNil(l->adjustment) )
    { for(c=0; c < cols; c++)
      { Name format = (Name) getElementVector(l->adjustment, toInt(c+1));

	if ( equalName(format, NAME_center) )
	  cf[c] = 'c';
	else if ( equalName(format, NAME_right) )
	  cf[c] = 'r';
	else
	  cf[c] = 'l';
      }
    }

    rh[r] = c = 0;
    for_cell(cell, dev->graphicals)
    { Graphical gr = cell->value;
      int gw, gh;

      MUSTBEVISIBLE(dev, gr);
      gw = valInt(HV(gr->area->w, gr->area->h));
      gh = valInt(HV(gr->area->h, gr->area->w));

      cw[c] = max(cw[c], gw);
      rh[r] = max(rh[r], gh);

      if ( ++c >= cols )
      { c = 0;
        rh[++r] = 0;
      }
    }

    c = r = 0;

    for_cell(cell, dev->graphicals)
    { Graphical gr = cell->value;
      MUSTBEVISIBLE(dev, gr);

      if ( l->direction == NAME_horizontal )
      { switch( cf[c] )
        { case 'l':	move_graphical(gr, x, y);
			break;
          case 'r':	move_graphical(gr, x+cw[c]-valInt(gr->area->w), y);
			break;
	  case 'c':	move_graphical(gr, x+(cw[c]-valInt(gr->area->w))/2, y);
	  		break;
	}
      } else
      { switch( cf[c] )
        { case 'l':	move_graphical(gr, y, x);
			break;
          case 'r':	move_graphical(gr, y, x+cw[c]-valInt(gr->area->h));
			break;
	  case 'c':	move_graphical(gr, y, x+(cw[c]-valInt(gr->area->h))/2);
	  		break;
	}
      }

      if ( c+1 >= cols )
      { y += rh[r++] + rs;
        c = 0;
	x = 0;
      } else
      { x += cw[c++] + cs;
      }
    }
  } else				/* non-column device */
  { int x = 0;
    int y = 0;
    int w = valInt(l->width);
    int cs = valInt(l->column_sep);
    int rs = valInt(l->row_sep);
    int rh = 0;
    int first = TRUE;
    Cell cell;

    for_cell(cell, dev->graphicals)
    { Graphical gr = cell->value;
      int gw, gh;

      MUSTBEVISIBLE(dev, gr);
      gw = valInt(HV(gr->area->w, gr->area->h));
      gh = valInt(HV(gr->area->h, gr->area->w));

      if ( !first && x + gw > w )	/* start next column */
      { y += rh + rs;
        rh = 0;
        x = 0;
        first = TRUE;
      }
      move_graphical(gr, HV(x, y), HV(y, x));
      x += gw + cs;
      rh = max(rh, gh);
      first = FALSE;
    }
  }
#undef HV

  assign(dev, badFormat, OFF);

  succeed;
}


		/********************************
		*            LAYOUT		*
		********************************/

static HashTable PlacedTable = NULL;	/* placed objects */

#define MAX_L_ROWS	100
#define MAXCOLLUMNS	100

typedef struct _unit
{ Graphical item;			/* Item displayed here */
  short height;				/* Height above reference */
  short	depth;				/* Depth below reference */
  short right;				/* Right of reference point */
  short left;				/* Left of reference point */
  Name  alignment;			/* alignment of the item */
} unit, *Unit;

static unit empty_unit = {(Graphical) NIL, 0, 0, 0, 0, NAME_column};

typedef struct _matrix
{ unit units[MAXCOLLUMNS][MAX_L_ROWS];
} matrix, *Matrix;


#define IsPlaced(gr)  (getMemberHashTable(PlacedTable, gr) == ON)
#define SetPlaced(gr) (appendHashTable(PlacedTable, gr, ON))

static void
shift_x_matrix(Matrix m, int *max_x, int *max_y)
{ int x, y;

  for(y=0; y < *max_y; y++)
  { for(x = *max_x; x > 0; x--)
      m->units[x][y] = m->units[x-1][y];

    m->units[0][y] = empty_unit;
  }

  (*max_x)++;
}


static void
shift_y_matrix(Matrix m, int *max_x, int *max_y)
{ int x, y;

  for(x=0; x < *max_x; x++)
  { for(y = *max_y; y > 0; y--)
      m->units[x][y] = m->units[x][y-1];

    m->units[x][0] = empty_unit;
  }

  (*max_y)++;
}


static void
expand_x_matrix(Matrix m, int *max_x, int *max_y)
{ int y;

  for(y=0; y < *max_y; y++)
    m->units[*max_x][y] = empty_unit;

  (*max_x)++;
}


static void
expand_y_matrix(Matrix m, int *max_x, int *max_y)
{ int x;

  for(x=0; x < *max_x; x++)
    m->units[x][*max_y] = empty_unit;

  (*max_y)++;
}


static status
placeDialogItem(Device d, Matrix m, Graphical i,
		int x, int y, int *max_x, int *max_y)
{ Graphical gr;

  if ( IsPlaced(i) || get(i, NAME_autoAlign, 0) != ON )
    succeed;
  SetPlaced(i);

  DEBUG(NAME_layout, printf("placing %s\n", pp(i)));

  while( x < 0 ) { shift_x_matrix(m, max_x, max_y); x++; }
  while( y < 0 ) { shift_y_matrix(m, max_x, max_y); y++; }
  while( x >= *max_x ) expand_x_matrix(m, max_x, max_y); 
  while( y >= *max_y ) expand_y_matrix(m, max_x, max_y); 

  if ( isNil(i->device) )
    displayDevice(d, i, DEFAULT);

  m->units[x][y].item = i;
  m->units[x][y].alignment = get(i, NAME_alignment, 0);
  if ( !m->units[x][y].alignment )
    m->units[x][y].alignment = NAME_left;

  if ( instanceOfObject((gr = get(i, NAME_above, 0)), ClassGraphical) )
    placeDialogItem(d, m, gr, x, y-1, max_x, max_y);
  if ( instanceOfObject((gr = get(i, NAME_below, 0)), ClassGraphical) )
    placeDialogItem(d, m, gr, x, y+1, max_x, max_y);
  if ( instanceOfObject((gr = get(i, NAME_right, 0)), ClassGraphical) )
    placeDialogItem(d, m, gr, x+1, y, max_x, max_y);
  if ( instanceOfObject((gr = get(i, NAME_left, 0)), ClassGraphical)  )
    placeDialogItem(d, m, gr,  x-1, y, max_x, max_y);

  succeed;
}


status
layoutDialogDevice(Device d, Size gap)
{ matrix m;
  int x, y, max_x = 0, max_y = 0;
  int px, py;
  Graphical gr;
  Cell cell;

  if ( isDefault(gap) )			/* TBD: proper integration */
    gap = answerObject(ClassSize, toInt(15), toInt(8), 0);

  ComputeGraphical(d);
  if ( !PlacedTable )
    PlacedTable = createHashTable(toInt(32), OFF);

  clearHashTable(PlacedTable);	

  for(;;)
  { int found = 0;

    for_cell(cell, d->graphicals)
    { if ( !IsPlaced(cell->value) &&
	   get(cell->value, NAME_autoAlign, 0) == ON )
      { placeDialogItem(d, &m, cell->value, 0, 0, &max_x, &max_y);
	found++;
  	break;
      }
    }

    if ( !found )
      succeed;				/* finished */

    for(x=0; x<max_x; x++)		/* Align labels and values */
    { int lw = -1;
      int vw = -1;
      int align_flags[MAXCOLLUMNS];

#define AUTO_ALIGN_LABEL 1
#define AUTO_ALIGN_VALUE 2

      for(y=0; y<max_y; y++)
      { align_flags[y] = 0;

	if ( notNil(gr = m.units[x][y].item) &&
	     gr->displayed == ON &&
	     m.units[x][y].alignment == NAME_column )
	{ int w;

	  if ( get(gr, NAME_autoLabelAlign, 0) == ON )
	  { if ( (w = valInt(get(gr, NAME_labelWidth, 0))) > lw )
	      lw = w;
	    align_flags[y] |= AUTO_ALIGN_LABEL;
	  }

	  if ( get(gr, NAME_autoValueAlign, 0) == ON )
	  { if ( (w = valInt(get(gr, NAME_valueWidth, 0))) > vw )
	      vw = w;
	    align_flags[y] |= AUTO_ALIGN_VALUE;
	  }
	}
      }
      if ( lw >= 0 )
      { for(y=0; y<max_y; y++)
	  if ( (align_flags[y] & AUTO_ALIGN_LABEL) &&
	       m.units[x][y].alignment == NAME_column )
	    send(m.units[x][y].item, NAME_labelWidth, toInt(lw), 0);
      }
      if ( vw >= 0 )
      { for(y=0; y<max_y; y++)
	  if ( (align_flags[y] & AUTO_ALIGN_VALUE) &&
	       m.units[x][y].alignment == NAME_column )
	    send(m.units[x][y].item, NAME_valueWidth, toInt(vw), 0);
      }
    }

    ComputeGraphical(d);		/* recompute for possible changes */

    for(x=0; x<max_x; x++)		/* Get sizes */
    { for(y=0; y<max_y; y++)
      { if ( notNil(gr = m.units[x][y].item) )
	{ if ( gr->displayed == ON )
	  { Point reference = get(gr, NAME_reference, 0);
	    int rx = (reference ? valInt(reference->x) : 0);
	    int ry = (reference ? valInt(reference->y) : 0);
	  
	    m.units[x][y].left   = rx;
	    m.units[x][y].height = ry;
	    m.units[x][y].depth  = valInt(gr->area->h) - ry;
	    m.units[x][y].right  = valInt(gr->area->w) - rx;
	  } else
	  { m.units[x][y].left   = 0;
	    m.units[x][y].height = 0;
	    m.units[x][y].depth  = 0;
	    m.units[x][y].right  = 0;
	  }
	}
      }
    }


    for(x=0; x<max_x; x++)		/* Determine unit width */
    { int r = 0, l = 0;
  
      for(y=0; y<max_y; y++)
      { if ( m.units[x][y].alignment == NAME_column )
	{ if ( m.units[x][y].right > r ) r = m.units[x][y].right;
	  if ( m.units[x][y].left  > l ) l = m.units[x][y].left;
	}
      }

      for(y=0; y<max_y; y++)
      { if ( m.units[x][y].alignment == NAME_column )
	{ m.units[x][y].right = r;
	  m.units[x][y].left = l;
	}
      }
    }


    for(y=0; y<max_y; y++)		/* Determine unit height */
    { int h = -1000, d = -1000;

      for(x=0; x<max_x; x++)
      { if ( m.units[x][y].height > h ) h = m.units[x][y].height;
	if ( m.units[x][y].depth  > d ) d = m.units[x][y].depth;
      }
      for(x=0; x<max_x; x++)
      { m.units[x][y].height = h;
	m.units[x][y].depth = d;
      }
    }

					  /* Place the items */
    for(py = valInt(gap->h), y=0; y<max_y; y++)
    { int px = valInt(gap->w);
      int lx = px;			/* x for left aligned items */
      int gapw = valInt(gap->w);
      int gaph = valInt(gap->h);
      
      for(x = 0; x < max_x; x++)
      { if ( notNil(gr = m.units[x][y].item) &&
	     gr->displayed == ON )
	{ Point reference = get(gr, NAME_reference, 0);
	  int rx = (reference ? valInt(reference->x) : 0);
	  int ry = (reference ? valInt(reference->y) : 0);
	  int iy = py + m.units[x][y].height;
	  int ix = (m.units[x][y].alignment == NAME_column ? px : lx) +
		    					m.units[x][y].left;

	  setGraphical(gr, toInt(ix - rx), toInt(iy - ry), DEFAULT, DEFAULT);
	  lx = valInt(gr->area->x) + valInt(gr->area->w) + gapw;
	}
	px += m.units[x][y].left + m.units[x][y].right + gapw;
      }

      py += m.units[0][y].depth + m.units[0][y].height + gaph; 
    }

    ComputeGraphical(d);		/* recompute bounding-box */
    
    for(y = 0; y < max_y; y++)
    { if ( instanceOfObject(d, ClassWindow) )
      { PceWindow sw = (PceWindow) d;

	px = valInt(sw->bounding_box->x) +
	     valInt(sw->bounding_box->w) +
	     valInt(gap->w);
      } else
      { px = valInt(d->area->x) - valInt(d->offset->x) +
             valInt(d->area->w) + valInt(gap->w);
      } 

      for(x = max_x-1; x >= 0; x--)
      { if ( notNil(gr = m.units[x][y].item) &&
	     gr->displayed == ON )
	{ if ( m.units[x][y].alignment == NAME_right ||
	       m.units[x][y].alignment == NAME_center )
	  { Name algnmt = m.units[x][y].alignment;
	    int x2;
	    Graphical gr2 = NULL, grl = gr;
	    int tw, dx;

	    DEBUG(NAME_layout, printf("%s is aligned %s\n",
				      pp(gr), pp(algnmt)));

	    for(x2 = x-1; x2 >= 0; x2--)
	    { if ( notNil(gr2 = m.units[x2][y].item) &&
		   gr2->displayed == ON )
	      { if ( m.units[x2][y].alignment != algnmt )
		  break;
		else
		{ DEBUG(NAME_layout, printf("\tadding %s\n",
					    pp(m.units[x2][y].item)));
		  grl = gr2;
		}
	      }
	    }
	    
	    tw = valInt(getRightSideGraphical(gr)) - valInt(grl->area->x);

	    if ( m.units[x][y].alignment == NAME_right )
	      dx = px - tw - valInt(gap->w);
	    else
	    { int sx = (x2 < 0 ? 0 : valInt(getRightSideGraphical(gr2)));
	      DEBUG(NAME_layout, printf("sx = %d; ", sx));
	      dx = (px - sx - tw)/2 + sx;
	    }
	    dx -= valInt(getLeftSideGraphical(grl));

	    DEBUG(NAME_layout,
		  printf("R = %d; Total width = %d, shift = %d\n",
			 px, tw, dx));

	    for(; ; x--)
	    { if ( notNil(gr = m.units[x][y].item) &&
		   gr->displayed == ON )
	      { setGraphical(gr, toInt(valInt(gr->area->x) + dx),
			     DEFAULT, DEFAULT, DEFAULT);
		DEBUG(NAME_layout, printf("\t moved %s\n", pp(gr)));
		if ( gr == grl )
		  break;
	      }
	    }
	  }

	  px = valInt(gr->area->x);
	}
      }
    }
  }
  
  { PceWindow sw;

    if ( (sw = getWindowGraphical((Graphical) d)) &&
	 isNil(sw->keyboard_focus) )
      advanceDevice(d, NIL);
  }
      
  succeed;
}

status
appendDialogItemDevice(Device d, Graphical item, Name where)
{ Graphical di;
  Name algmnt;

  if ( emptyChain(d->graphicals) )
    return displayDevice(d, item, DEFAULT);

  di = getTailChain(d->graphicals);
  if ( isDefault(where) )
  { if ( instanceOfObject(di, ClassButton) &&
	 instanceOfObject(item, ClassButton) )
      where = NAME_right;
    else
      where = NAME_nextRow;
  } else if ( where == NAME_right &&
	      (algmnt = get(di, NAME_alignment, 0)) != NAME_column )
    send(item, NAME_alignment, algmnt, 0);

  if ( where == NAME_nextRow )
  { Graphical left;

    while ( (left = get(di, NAME_left, 0)) && notNil(left) )
      di = left;
    where = NAME_below;
  }

  return send(item, where, di, 0);
}


		/********************************
		*         MISCELENEOUS		*
		********************************/


static status
convertLoadedObjectDevice(Device dev, Int ov, Int cv)
{ if ( isNil(dev->recompute) )
    assign(dev, recompute, newObject(ClassChain, 0));
    
  succeed;
}


static status
reparentDevice(Device dev)
{ Cell cell;

  if ( isNil(dev->device) )
    assign(dev, level, ZERO);
  else
    assign(dev, level, add(dev->device->level, ONE));

  for_cell(cell, dev->graphicals)
    qadSendv(cell->value, NAME_reparent, 0, NULL);

  return reparentGraphical((Graphical) dev);
}


static status
roomDevice(Device dev, Area area)
{ register Cell cell;

  ComputeGraphical(dev);
  for_cell(cell, dev->graphicals)
  { Graphical gr = cell->value;

    if ( gr->displayed == ON &&
         overlapArea(gr->area, area) != FAIL )
      fail;
  }

  succeed;
}


static Chain
getInsideDevice(Device dev, Area a)
{ register Cell cell;
  Chain ch;

  ch = answerObject(ClassChain, 0);

  ComputeGraphical(dev);
  for_cell(cell, dev->graphicals)
  { if (insideArea(a, ((Graphical) cell->value)->area) == SUCCEED)
      appendChain(ch, cell->value);
  }

  answer(ch);
}


static status
resizeDevice(Device dev, Real xfactor, Real yfactor, Point origin)
{ float xf, yf;
  int ox = valInt(dev->offset->x);
  int oy = valInt(dev->offset->y);
  Point p;
  Cell cell;

  init_resize_graphical(dev, xfactor, yfactor, origin, &xf, &yf, &ox, &oy);
  if ( xf == 1.0 && yf == 1.0 )
    succeed;

  p = tempObject(ClassPoint, toInt(ox - valInt(dev->offset->x)),
		 	     toInt(oy - valInt(dev->offset->y)), 0);

  for_cell(cell, dev->graphicals)
    send(cell->value, NAME_resize, xfactor, yfactor, p, 0);
  considerPreserveObject(p);

  succeed;
}


		/********************************
		*         NAMED MEMBERS		*
		********************************/

Graphical
getMemberDevice(Device dev, Name name)
{ if ( notNil(dev->graphicals) )
  { Cell cell;

    for_cell(cell, dev->graphicals)
    { if (((Graphical)cell->value)->name == name)
	answer(cell->value);
    }
  }

  fail;
}


static status
forSomeDevice(Device dev, Name name, Code msg)
{ Cell cell, c2;

  for_cell_save(cell, c2, dev->graphicals)
  { Graphical gr = cell->value;

    if ( isDefault(name) || gr->name == name )
      forwardReceiverCode(msg, dev, gr, 0);
  }
  
  succeed;
}


static status
forAllDevice(Device dev, Name name, Code msg)
{ Cell cell, c2;

  for_cell_save(cell, c2, dev->graphicals)
  { Graphical gr = cell->value;

    if ( isDefault(name) || gr->name == name )
      TRY(forwardReceiverCode(msg, dev, gr, 0));
  }
  
  succeed;
}

		/********************************
		*            MOVING		*
		********************************/


static status
updateConnectionsDevice(Device dev, Int level)
{ Cell cell;

  for_cell(cell, dev->graphicals)
  { if ( instanceOfObject(cell->value, ClassDevice) )
      updateConnectionsDevice(cell->value, level);
    else
      updateConnectionsGraphical(cell->value, level);
  }

  return updateConnectionsGraphical((Graphical) dev, level);
}


status
geometryDevice(Device dev, Int x, Int y, Int w, Int h)
{ ComputeGraphical(dev);

  if ( isDefault(x) ) x = dev->area->x;
  if ( isDefault(y) ) y = dev->area->y;

  if ( x != dev->area->x || y != dev->area->y )
  { Int dx = sub(x, dev->area->x);
    Int dy = sub(y, dev->area->y);

    CHANGING_GRAPHICAL(dev,
	assign(dev->offset, x, add(dev->offset->x, dx));
	assign(dev->offset, y, add(dev->offset->y, dy));
	if ( notNil(dev->clip_area) )
	{ assign(dev, badBoundingBox, ON); /* TBD: ??? */
	  computeBoundingBoxDevice(dev);
	} else
	{ assign(dev->area, x, x);
	  assign(dev->area, y, y);
	});

    updateConnectionsDevice(dev, sub(dev->level, ONE));
  }

  succeed;
}


		/********************************
		*           REFERENCE		*
		********************************/


static status
referenceDevice(Device dev, Point pos)
{ Int x, y;

  if ( isDefault(pos) )
  { ComputeGraphical(dev);
    x = sub(dev->area->x, dev->offset->x);
    y = sub(dev->area->y, dev->offset->y);
  } else
  { x = pos->x;
    y = pos->y;
  }

  if ( x != ZERO || y != ZERO )
  { Cell cell;
    Point move = tempObject(ClassPoint, sub(ZERO, x), sub(ZERO, y), 0);

    offsetPoint(dev->offset, x, y);
    for_cell(cell, dev->graphicals)
      relativeMoveGraphical(cell->value, move);

    considerPreserveObject(move);
  }

  succeed;
}


static status
set_position_device(Device dev, Int x, Int y)
{ ComputeGraphical(dev);

  if ( isDefault(x) ) x = dev->offset->x;
  if ( isDefault(y) ) y = dev->offset->y;

  x = add(dev->area->x, sub(x, dev->offset->x));
  y = add(dev->area->y, sub(y, dev->offset->y));

  return setGraphical(dev, x, y, DEFAULT, DEFAULT);
}


static status
positionDevice(Device dev, Point pos)
{ return set_position_device(dev, pos->x, pos->y);
}


static status
xDevice(Device dev, Int x)
{ return set_position_device(dev, x, DEFAULT);
}


static status
yDevice(Device dev, Int y)
{ return set_position_device(dev, DEFAULT, y);
}


static Point
getPositionDevice(Device dev)
{ ComputeGraphical(dev);
  answer(dev->offset);
}


static Int
getXDevice(Device dev)
{ answer(getPositionDevice(dev)->x);
}


static Int
getYDevice(Device dev)
{ answer(getPositionDevice(dev)->y);
}


static Point
getOffsetDevice(Device dev)
{ ComputeGraphical(dev);
  answer(dev->offset);
}


		/********************************
		*           CATCH ALL		*
		********************************/

static Any
getCatchAllDevice(Device dev, Name name)
{ Name base;

  if ( (base = getDeleteSuffixName(name, NAME_Member)) )
    answer(getMemberDevice(dev, base));

  fail;
}

		/********************************
		*             VISUAL		*
		********************************/

static Chain
getContainsDevice(Device dev)
{ answer(dev->graphicals);
}


extern drawPostScriptDevice(Device dev);

status
makeClassDevice(Class class)
{ sourceClass(class, makeClassDevice, __FILE__, "$Revision$");

  localClass(class, NAME_level, NAME_organisation, "int", NAME_get,
	     "Nesting depth to topmost device");
  localClass(class, NAME_offset, NAME_area, "point", NAME_none,
	     "Offset of origin");
  localClass(class, NAME_clipArea, NAME_scroll, "area*", NAME_none,
	     "Clip all graphicals to this area");
  localClass(class, NAME_graphicals, NAME_organisation, "chain", NAME_get,
	     "Displayed graphicals (members)");
  localClass(class, NAME_pointed, NAME_event, "chain", NAME_get,
	     "Graphicals pointed-to by the mouse");
  localClass(class, NAME_format, NAME_layout, "format*", NAME_get,
	     "Use tabular layout");
  localClass(class, NAME_badFormat, NAME_update, "bool", NAME_none,
	     "Format needs to be recomputed");
  localClass(class, NAME_badBoundingBox, NAME_update, "bool", NAME_none,
	     "Indicate bounding box is out-of-date");
  localClass(class, NAME_recompute, NAME_update, "chain", NAME_none,
	     "Graphicals that requested a recompute");

  termClass(class, "device", 0);
  setRedrawFunctionClass(class, RedrawAreaDevice);

  sendMethod(class, NAME_initialise, DEFAULT, 0,
	     "Create an empty device",
	     initialiseDevice);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Clear device and unlink from super-device",
	     unlinkDevice);
  sendMethod(class, NAME_geometry, DEFAULT, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Move device",
	     geometryDevice);
  sendMethod(class, NAME_clear, NAME_organisation, 0,
	     "Erase all graphicals",
	     clearDevice);
  sendMethod(class, NAME_compute, NAME_update, 0,
	     "Recompute device",
	     computeDevice);
  sendMethod(class, NAME_appendDialogItem, NAME_organisation, 2,
	     "item=graphical", "relative_to_last=[{below,right,next_row}]",
	     "Append dialog_item {below,right,next_row} last",
	     appendDialogItemDevice);
  sendMethod(class, NAME_display, NAME_organisation, 2,
	     "graphical", "position=[point]",
	     "Display graphical at point",
	     displayDevice);
  sendMethod(class, NAME_erase, NAME_organisation, 1, "graphical",
	     "Erase a graphical",
	     eraseDevice);
  sendMethod(class, NAME_event, NAME_event, 1, "event",
	     "Process an event",
	     eventDevice);
  sendMethod(class, NAME_typed, NAME_accelerator, 2, "event_id", "[bool]",
	     "Handle accelerators",
	     typedDevice);
  sendMethod(class, NAME_format, NAME_layout, 2, "format*|name", "[any]",
	     "Use tabular layout",
	     formatDevice);
  sendMethod(class, NAME_layoutDialog, NAME_layout, 1, "[size]",
	     "(Re)compute layout of dialog_items",
	     layoutDialogDevice);
  sendMethod(class, NAME_room, NAME_layout, 1, "area",
	     "Test if no graphicals are in area",
	     roomDevice);
  sendMethod(class, NAME_selection, NAME_selection, 1, "graphical|chain*",
	     "Set selection to graphical or chain",
	     selectionDevice);
  sendMethod(class, NAME_advance, NAME_focus, 1, "[graphical]*",
	     "Advance keyboard focus to next item",
	     advanceDevice);
  sendMethod(class, NAME_DrawPostScript, NAME_postscript, 0,
	     "Create PostScript",
	     drawPostScriptDevice);
  sendMethod(class, NAME_foreground, NAME_appearance, 1, "colour",
	     "Default colour for all members",
	     colourGraphical);
  sendMethod(class, NAME_forSome, NAME_iterate, 2, "[name]", "code",
	     "Run code on all graphicals",
	     forSomeDevice);
  sendMethod(class, NAME_forAll, NAME_iterate, 2, "[name]", "code",
	     "Run code on graphicals; demand acceptance",
	     forAllDevice);
  sendMethod(class, NAME_reference, NAME_area, 1, "[point]",
	     "Move origin, while moving members opposite",
	     referenceDevice);
  sendMethod(class, NAME_position, NAME_area, 1, "point",
	     "Set origin",
	     positionDevice);
  sendMethod(class, NAME_move, NAME_area, 1, "point",
	     "Set origin",
	     positionDevice);
  sendMethod(class, NAME_x, NAME_area, 1, "int",
	     "Set X of origin",
	     xDevice);
  sendMethod(class, NAME_y, NAME_area, 1, "int",
	     "Set Y of origin",
	     yDevice);
  sendMethod(class, NAME_resize, NAME_area, 3,
	     "x_factor=real", "y_factor=[real]", "origin=[point]",
	     "Resize device with specified factor",
	     resizeDevice);
  sendMethod(class, NAME_updatePointed, NAME_event, 1, "event",
	     "Update <-pointed, sending area_enter and area_exit events",
	     updatePointedDevice);
  sendMethod(class, NAME_changedUnion, NAME_resize, 1, "area",
	     "Trap changes to the union of all graphicals",
	     changedUnionDevice);
  sendMethod(class, NAME_reparent, NAME_organisation, 0,
	     "Device's parent-chain has changed",
	     reparentDevice);
  sendMethod(class, NAME_convertLoadedObject, NAME_compatibility, 2,
	     "old_version=int", "new_version=int",
	     "Initialise recompute and request_compute",
	     convertLoadedObjectDevice);
  sendMethod(class, NAME_modifiedItem, NAME_virtual, 2, "graphical", "bool",
	     "Trap modification of item (fail)",
	     failObject);

  getMethod(class, NAME_catchAll, NAME_organisation, "graphical", 1, "name",
	    "Find named graphicals",
	    getCatchAllDevice);
  getMethod(class, NAME_pointedObjects, NAME_event, "chain", 2,
	    "at=point|event", "append_to=[chain]",
	    "New chain holding graphicals at point|event",
	    getPointedObjectsDevice);
  getMethod(class, NAME_find, NAME_search, "graphical", 2,
	    "at=[point|event]", "condition=[code]",
	    "Find most local graphical",
	    getFindDevice);
  getMethod(class, NAME_selection, NAME_selection, "chain", 0,
	    "New chain of selected graphicals",
	    getSelectionDevice);
  getMethod(class, NAME_inside, NAME_selection, "chain", 1, "area",
	    "New chain with graphicals inside area",
	    getInsideDevice);
  getMethod(class, NAME_member, NAME_organisation, "graphical", 1,
	    "graphical_name=name",
	    "Find named graphical",
	    getMemberDevice);
  getMethod(class, NAME_position, NAME_area, "point", 0,
	    "Get origin",
	    getPositionDevice);
  getMethod(class, NAME_x, NAME_area, "int", 0,
	    "Get X of origin",
	    getXDevice);
  getMethod(class, NAME_y, NAME_area, "int", 0,
	    "Get Y of origin",
	    getYDevice);
  getMethod(class, NAME_offset, NAME_area, "point", 0,
	    "Get origin (also <-position)",
	    getOffsetDevice);
  getMethod(class, NAME_contains, DEFAULT, "chain", 0,
	    "Chain with visuals contained",
	    getContainsDevice);

  initClass(class);

  succeed;
}
