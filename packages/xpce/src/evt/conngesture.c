/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialiseConnectGesture(ConnectGesture g,
			 Name button, Modifier modifier, Link link)
{ initialiseGesture((Gesture) g, button, modifier);

  assign(g, line, 	     newObject(ClassLine, EAV));
  assign(g, link,   	     isDefault(link) ? (Link) newObject(ClassLink, EAV)
	 				     : link);
  assign(g, from_handle,     DEFAULT);
  assign(g, to_handle,       DEFAULT);
  assign(g, from_indicators, newObject(ClassChain, EAV));
  assign(g, to_indicators,   newObject(ClassChain, EAV));
  assign(g, mark,	     getClassVariableValueObject(g, NAME_mark));

  succeed;
}


		/********************************
		*       GESTURE BEHAVIOUR	*
		********************************/

static status
verifyConnectGesture(ConnectGesture g, EventObj ev)
{ Graphical gr = ev->receiver;

  if ( !instanceOfObject(gr, ClassGraphical) ||
       isNil(gr->device) ||
       isNil(g->link) )
    fail;

  if ( isNil(g->device) )
    assign(g, device, gr->device);

  succeed;
}


static status
initiateConnectGesture(ConnectGesture g, EventObj ev)
{ Device dev;
  Point pos;

  verifyConnectGesture(g, ev);		/* safety for redefinition */
  dev = g->device;
  pos = getPositionEvent(ev, dev);

  send(g->line, NAME_copy, g->link->line, EAV);
  send(g->line, NAME_texture, NAME_dotted, EAV);
  send(g->line, NAME_points, pos->x, pos->y, pos->x, pos->y, EAV);
  send(dev, NAME_display, g->line, EAV);
  send(g, NAME_indicate,
       ev->receiver, ev, g->link->from,
       g->from_indicators, NAME_fromHandle, EAV);

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
  Chain pointed = get(g, NAME_pointed, ev, EAV);
  Cell cell;

  send(g->line, NAME_end, pos, EAV);
  if ( instanceOfObject(pointed, ClassChain) )
  { for_cell(cell, pointed)
    { Graphical gr = cell->value;
      Chain handles;

      if ( gr != ev->receiver &&
	   (handles = getHandlesGraphical(gr, DEFAULT, g->link->to, DEFAULT)) )
      { doneObject(handles);
	send(g, NAME_indicate, gr, ev, g->link->to,
	     g->to_indicators, NAME_toHandle, EAV);

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

  send(g, NAME_drag, ev, EAV);

  DeviceGraphical(g->line, NIL);
  for_cell(cell, g->to_indicators)
    DeviceGraphical(cell->value, NIL);
  for_cell(cell, g->from_indicators)
    DeviceGraphical(cell->value, NIL);

  if ( notNil(g->to) )
  { send(g, NAME_connect, ev->receiver, g->to, g->link,
	 g->from_handle, g->to_handle, EAV);
    assign(g, to, NIL);
    assign(g, device, NIL);
  }

  assign(g, from_handle, DEFAULT);
  assign(g, to_handle,   DEFAULT);

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
  Handle h;

  for_cell(cell, chain)
    nameGraphical(cell->value, NAME_unused);

  if ( chain == g->from_indicators && notDefault(g->from_handle) &&
       (h = getHandleGraphical(gr, g->from_handle)) )
  { send(g, NAME_indicateHandle, gr, h->name, chain, EAV);
  } else if ( (handles = getHandlesGraphical(gr, pos, kind, toInt(10))) )
  { h = getHeadChain(handles);

    send(g, NAME_indicateHandle, gr, h->name, chain, EAV);
    slotObject(g, slot, h->name);
    doneObject(handles);
  } else if ( (handles = getHandlesGraphical(gr, pos, kind, DEFAULT)) )
  { for_cell(cell, handles)
    { h = cell->value;

      send(g, NAME_indicateHandle, gr, h->name, chain, EAV);
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

  if ( !pos )
    fail;

  for_cell(cell, ch)
  { bm = cell->value;

    if ( bm->name == NAME_unused )
    { centerGraphical((Graphical) bm, pos);
      send(dev, NAME_display, bm, EAV);
      assign(bm, name, NAME_used);
      succeed;
    }
  }
  
  bm = newObject(ClassBitmap, g->mark, EAV);
  centerGraphical((Graphical) bm, pos);
  send(dev, NAME_display, bm, EAV);
  assign(bm, name, NAME_used);
  appendChain(ch, bm);
  
  succeed;
}


static status
connectConnectGesture(ConnectGesture g, Graphical gr1, Graphical gr2,
		      Link link, Name fh, Name th)
{ return send(gr1, NAME_connect, gr2, link, fh, th, EAV);
}



		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_indicateHandle[] =
        { "at=graphical", "handle=name", "indicators=chain" };
static char *T_indicate[] =
        { "at=graphical", "near=event", "handle_kind=name", "indicators=chain", "variable={to_handle,from_handle}" };
static char *T_initialise[] =
        { "button=[button_name]", "modifier=[modifier]", "link=[link]" };
static char *T_connect[] =
        { "from=graphical", "to=graphical", "link=link", "from_handle=[name]", "to_handle=[name]" };

/* Instance Variables */

static vardecl var_connectGesture[] =
{ IV(NAME_device, "device*", IV_BOTH,
     NAME_context, "Device used for feedback"),
  IV(NAME_line, "line", IV_GET,
     NAME_feedback, "Line for feedback (cf. outline)"),
  IV(NAME_mark, "image", IV_BOTH,
     NAME_feedback, "Image used to mark handles"),
  IV(NAME_link, "link*", IV_BOTH,
     NAME_connection, "Link used to connect the graphicals"),
  IV(NAME_fromIndicators, "chain", IV_GET,
     NAME_feedback, "Chain of bitmaps to indicate handles"),
  IV(NAME_toIndicators, "chain", IV_GET,
     NAME_feedback, "Chain of bitmaps to indicate handles"),
  IV(NAME_fromHandle, "[name]", IV_GET,
     NAME_connection, "Name of handle at from side"),
  IV(NAME_toHandle, "[name]", IV_GET,
     NAME_connection, "Name of handle at to side"),
  IV(NAME_to, "graphical*", IV_GET,
     NAME_connection, "Graphical at to side (target)")
};

/* Send Methods */

static senddecl send_connectGesture[] =
{ SM(NAME_drag, 1, "event", dragConnectGesture,
     DEFAULT, "Drag to next position"),
  SM(NAME_initialise, 3, T_initialise, initialiseConnectGesture,
     DEFAULT, "Create from button, modifier and link"),
  SM(NAME_initiate, 1, "event", initiateConnectGesture,
     DEFAULT, "Initiate connect"),
  SM(NAME_terminate, 1, "event", terminateConnectGesture,
     DEFAULT, "Finish the connect"),
  SM(NAME_verify, 1, "event", verifyConnectGesture,
     DEFAULT, "Verify object connected is displayed on a device"),
  SM(NAME_connect, 5, T_connect, connectConnectGesture,
     NAME_execute, "Connect the receiver with the target"),
  SM(NAME_indicate, 5, T_indicate, indicateConnectGesture,
     NAME_feedback, "Indicate possible connectionpoints"),
  SM(NAME_indicateHandle, 3, T_indicateHandle, indicateHandleConnectGesture,
     NAME_feedback, "Indicate position of named handle")
};

/* Get Methods */

static getdecl get_connectGesture[] =
{ GM(NAME_pointed, 1, "chain", "event", getPointedConnectGesture,
     NAME_event, "Find the graphicals covered by the mouse")
};

/* Resources */

static classvardecl rc_connectGesture[] =
{ RC(NAME_button, "button_name", "left",
     "Active on which button (left)"),
  RC(NAME_mark, "image", "@mark_handle_image",
     "Marker for handle position"),
  RC(NAME_modifier, "modifier", "",
     "Modifier (none)")
};

/* Class Declaration */

static Name connectGesture_termnames[] =
	{ NAME_button, NAME_modifier, NAME_link };

ClassDecl(connectGesture_decls,
          var_connectGesture, send_connectGesture,
	  get_connectGesture, rc_connectGesture,
          3, connectGesture_termnames,
          "$Rev$");

status
makeClassConnectGesture(Class class)
{ return declareClass(class, &connectGesture_decls);
}

