/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialiseConnectGesture(ConnectGesture g,
			 Name button, Modifier modifier, Link link)
{ initialiseGesture((Gesture) g, button, modifier);

  assign(g, line, 	     newObject(ClassLine, 0));
  assign(g, link,   	     isDefault(link) ? (Link) newObject(ClassLink, 0)
	 				     : link);
  assign(g, from_handle,     DEFAULT);
  assign(g, to_handle,       DEFAULT);
  assign(g, from_indicators, newObject(ClassChain, 0));
  assign(g, to_indicators,   newObject(ClassChain, 0));
  assign(g, mark,	     getResourceValueObject(g, NAME_mark));

  succeed;
}


		/********************************
		*       GESTURE BEHAVIOUR	*
		********************************/

static status
verifyConnectGesture(ConnectGesture g, EventObj ev)
{ if ( isNil(ev->receiver->device) || isNil(g->link) )
    fail;

  assign(g, device, ev->receiver->device);

  succeed;
}


static status
initiateConnectGesture(ConnectGesture g, EventObj ev)
{ Device dev = g->device;
  Point pos = getPositionEvent(ev, dev);

  send(g->line, NAME_copy, g->link->line, 0);
  send(g->line, NAME_texture, NAME_dotted, 0);
  send(g->line, NAME_points, pos->x, pos->y, pos->x, pos->y, 0);
  send(dev, NAME_display, g->line, 0);
  send(g, NAME_indicate,
       ev->receiver, ev, g->link->from,
       g->from_indicators, NAME_fromHandle, 0);

  succeed;
}


static Chain
getPointedConnectGesture(ConnectGesture g, EventObj ev)
{ return getPointedObjectsDevice(g->device, ev, DEFAULT);
}


static status
dragConnectGesture(ConnectGesture g, EventObj ev)
{ Device dev = g->device;
  Point pos = getPositionEvent(ev, dev);
  Chain pointed = get(g, NAME_pointed, ev, 0);
  Cell cell;

  send(g->line, NAME_end, pos, 0);
  if ( instanceOfObject(pointed, ClassChain) )
  { for_cell(cell, pointed)
    { Graphical gr = cell->value;
      Chain handles;

      if ( gr != ev->receiver &&
	   (handles = getHandlesGraphical(gr, DEFAULT, g->link->to, DEFAULT)) )
      { doneObject(handles);
	send(g, NAME_indicate, gr, ev, g->link->to,
	     g->to_indicators, NAME_toHandle, 0);

	assign(g, to, gr);
	doneObject(pointed);
	succeed;
      }
    }
  }

  assign(g, to, NIL);
  for_cell(cell, g->to_indicators)
    DeviceGraphical(cell->value, NIL);

  succeed;
}


static status
terminateConnectGesture(ConnectGesture g, EventObj ev)
{ Cell cell;

  dragConnectGesture(g, ev);

  DeviceGraphical(g->line, NIL);
  for_cell(cell, g->to_indicators)
    DeviceGraphical(cell->value, NIL);
  for_cell(cell, g->from_indicators)
    DeviceGraphical(cell->value, NIL);

  if ( notNil(g->to) )
  { send(g, NAME_connect, ev->receiver, g->to, g->link,
	 g->from_handle, g->to_handle, 0);
    assign(g, to, NIL);
    assign(g, device, NIL);
  }

  succeed;
}

		/********************************
		*       REMAINING MEHODS	*
		********************************/

static status
indicateConnectGesture(ConnectGesture g, Graphical gr, EventObj ev,
		       Name kind, Chain chain, Name slot)
{ Cell cell;
  Chain handles;
  Point pos = getPositionEvent(ev, gr->device);

  for_cell(cell, chain)
    nameGraphical(cell->value, NAME_unused);

  if ( (handles = getHandlesGraphical(gr, pos, kind, toInt(10))) )
  { Handle h = getHeadChain(handles);

    send(g, NAME_indicateHandle, gr, h->name, chain, 0);
    slotObject(g, slot, h->name);
    doneObject(handles);
  } else if ( (handles = getHandlesGraphical(gr, pos, kind, DEFAULT)) )
  { for_cell(cell, handles)
    { Handle h = cell->value;

      send(g, NAME_indicateHandle, gr, h->name, chain, 0);
    }
    slotObject(g, slot, DEFAULT);
    doneObject(handles);
  }

  for_cell(cell, chain)
  { Graphical gr = cell->value;

    if ( gr->name == NAME_unused )
      DeviceGraphical(gr, NIL);
  }
   
  succeed;
}


static status
indicateHandleConnectGesture(ConnectGesture g,
			     Graphical gr, Name name, Chain ch)
{ Cell cell;
  Device dev = g->device;
  Point pos = getHandlePositionGraphical(gr, name, dev);
  BitmapObj bm;

  for_cell(cell, ch)
  { bm = cell->value;

    if ( bm->name == NAME_unused )
    { centerGraphical((Graphical) bm, pos);
      send(dev, NAME_display, bm, 0);
      assign(bm, name, NAME_used);
      succeed;
    }
  }
  
  bm = newObject(ClassBitmap, g->mark, 0);
  centerGraphical((Graphical) bm, pos);
  send(dev, NAME_display, bm, 0);
  assign(bm, name, NAME_used);
  appendChain(ch, bm);
  
  succeed;
}


static status
connectConnectGesture(ConnectGesture g, Graphical gr1, Graphical gr2,
		      Link link, Name fh, Name th)
{ return send(gr1, NAME_connect, gr2, link, fh, th, 0);
}



status
makeClassConnectGesture(Class class)
{ sourceClass(class, makeClassConnectGesture, __FILE__, "$Revision$");

  localClass(class, NAME_device, NAME_context, "device*", NAME_both,
	     "Device used for feedback");
  localClass(class, NAME_line, NAME_feedback, "line", NAME_get,
	     "Line for feedback (cf. outline)");
  localClass(class, NAME_mark, NAME_feedback, "image", NAME_both,
	     "Image used to mark handles");
  localClass(class, NAME_link, NAME_connection, "link*", NAME_both,
	     "Link used to connect the graphicals");
  localClass(class, NAME_fromIndicators, NAME_feedback, "chain", NAME_get,
	     "Chain of bitmaps to indicate handles");
  localClass(class, NAME_toIndicators, NAME_feedback, "chain", NAME_get,
	     "Chain of bitmaps to indicate handles");
  localClass(class, NAME_fromHandle, NAME_connection, "[name]", NAME_get,
	     "Name of handle at from side");
  localClass(class, NAME_toHandle, NAME_connection, "[name]", NAME_get,
	     "Name of handle at to side");
  localClass(class, NAME_to, NAME_connection, "graphical*", NAME_get,
	     "Graphical at to side (target)");

  termClass(class, "connect_gesture",
	    3, NAME_button, NAME_modifier, NAME_link);

  sendMethod(class, NAME_initialise, DEFAULT,
	     3, "button=[button_name]", "modifier=[modifier]", "link=[link]",
	     "Create from button, modifier and link",
	     initialiseConnectGesture);
  sendMethod(class, NAME_verify, DEFAULT, 1, "event",
	     "Verify object connected is displayed on a device",
	     verifyConnectGesture);
  sendMethod(class, NAME_initiate, DEFAULT, 1, "event",
	     "Initiate connect",
	     initiateConnectGesture);
  sendMethod(class, NAME_drag, DEFAULT, 1, "event",
	     "Drag to next position",
	     dragConnectGesture);
  sendMethod(class, NAME_terminate, DEFAULT, 1, "event",
	     "Finish the connect",
	     terminateConnectGesture);
  sendMethod(class, NAME_connect, NAME_execute, 5,
	     "from=graphical", "to=graphical", "link=link",
	     "from_handle=[name]", "to_handle=[name]",
	     "Connect the receiver with the target",
	     connectConnectGesture);
  sendMethod(class, NAME_indicate, NAME_feedback, 5,
	     "at=graphical", "near=event", "handle_kind=name",
	     "indicators=chain", "variable={to_handle,from_handle}",
	     "Indicate possible connectionpoints",
	     indicateConnectGesture);
  sendMethod(class, NAME_indicateHandle, NAME_feedback, 3,
	     "at=graphical", "handle=name", "indicators=chain",
	     "Indicate position of named handle",
	     indicateHandleConnectGesture);

  getMethod(class, NAME_pointed, NAME_event, "chain", 1, "event",
	    "Find the graphicals covered by the mouse",
	    getPointedConnectGesture);

  attach_resource(class, "button",   "button_name",   "left",
		  "Active on which button (left)");
  attach_resource(class, "modifier", "modifier", "",
		  "Modifier (none)");
  attach_resource(class, "mark",   "image", "@mark_handle_image",
		  "Marker for handle position");

  succeed;
}

