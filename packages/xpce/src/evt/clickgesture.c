/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialiseClickGesture(ClickGesture g, Name button,
		       Modifier modifier, Name multi,
		       Code execute, Code preview, Code cancel)
{ if ( isDefault(execute) ) execute = NIL;
  if ( isDefault(preview) ) preview = NIL;
  if ( isDefault(cancel) )  cancel  = NIL;

  TRY(initialiseGesture((Gesture) g, button, modifier));

  assign(g, down_position,     newObject(ClassPoint, 0));
  assign(g, multiclick,        multi);
  assign(g, execute_message,   execute);
  assign(g, preview_message,   preview);
  assign(g, cancel_message,    cancel);
  assign(g, execute_cursor,    DEFAULT);
  assign(g, max_drag_distance, DEFAULT);

  succeed;
}


		/********************************
		*       GESTURE BEHAVIOUR	*
		********************************/

static status
verifyClickGesture(ClickGesture g, EventObj ev)
{ if ( isDefault(g->multiclick) || getMulticlickEvent(ev) == g->multiclick )
  { copyPoint(g->down_position, getPositionEvent(ev, DEFAULT));
    succeed;
  }

  fail;
}


static status
initiateClickGesture(ClickGesture g, EventObj ev)
{ if ( notNil(g->preview_message) )
    return forwardReceiverCode(g->preview_message, getMasterEvent(ev), ev, 0);

  succeed;
}


static status
dragClickGesture(ClickGesture g, EventObj ev)
{ if ( notNil(g->max_drag_distance) )
  { if ( valInt(getDistanceEvent(ev->window->focus_event, ev)) >
	 valInt(g->max_drag_distance) )
      send(g, NAME_cancel, ev, 0);
  }
  
  succeed;
}


static status
cancelClickGesture(ClickGesture g, EventObj ev)
{ if ( notNil(g->cancel_message) )
    forwardReceiverCode(g->cancel_message, getMasterEvent(ev), ev, 0);
  
  return cancelGesture((Gesture) g, ev);
}


static status
terminateClickGesture(ClickGesture g, EventObj ev)
{ if ( insideEvent(ev, DEFAULT) ||
       valInt(getDistancePoint(g->down_position,
			       getPositionEvent(ev, DEFAULT))) < 5 )
  { if ( notNil(g->execute_message) )
    { if ( getMulticlickEvent(ev) == NAME_single )
      { forwardReceiverCode(g->execute_message, getMasterEvent(ev), ev, 0);
      } else
      { DisplayObj d = getDisplayGraphical((Graphical) ev->window);

	busyCursorDisplay(d, DEFAULT, DEFAULT);
	forwardReceiverCode(g->execute_message, getMasterEvent(ev), ev, 0);
	busyCursorDisplay(d, NIL, DEFAULT);
      }
    }
  } else
  { if ( notNil(g->cancel_message) )
      forwardReceiverCode(g->cancel_message, getMasterEvent(ev), ev, 0);
  }
    
  succeed;
}


status
makeClassClickGesture(Class class)
{ sourceClass(class, makeClassClickGesture, __FILE__, "$Revision$");

  localClass(class, NAME_multiclick, NAME_modifier,
	     "[{single,double,triple}]", NAME_both,
	     "Demand single, double or triple click");
  localClass(class, NAME_downPosition, NAME_internal, "point", NAME_get,
	     "Position of the down event");
  localClass(class, NAME_executeMessage, NAME_action, "code*", NAME_both,
	     "Message sent on up inside area");
  localClass(class, NAME_previewMessage, NAME_feedback, "code*", NAME_both,
	     "Message sent on down");
  localClass(class, NAME_cancelMessage, NAME_feedback, "code*", NAME_both,
	     "Message sent on up outside area");
  localClass(class, NAME_executeCursor, NAME_feedback, "cursor*", NAME_both,
	     "Cursor displayed while message is executed");
  localClass(class, NAME_maxDragDistance, NAME_cancel, "int*", NAME_both,
	     "Cancel after dragging this far");

  termClass(class, "click_gesture",
	    6, NAME_button, NAME_modifier, NAME_multiclick,
	    NAME_executeMessage, NAME_previewMessage, NAME_cancelMessage);

  sendMethod(class, NAME_initialise, DEFAULT, 6,
	     "button=[button_name]", "modifier=[modifier]",
	     "multiple=[{single,double,triple}]",
	     "message=[code]*", "preview=[code]*", "cancel=[code]*",
	     "Create from button, modifier, multi, ...",
	     initialiseClickGesture);
  sendMethod(class, NAME_verify, DEFAULT, 1, "event",
	     "Verify modifier and multiclick",
	     verifyClickGesture);
  sendMethod(class, NAME_initiate, DEFAULT, 1, "event",
	     "Send preview message",
	     initiateClickGesture);
  sendMethod(class, NAME_drag, DEFAULT, 1, "event",
	     "Does nothing",
	     dragClickGesture);
  sendMethod(class, NAME_terminate, DEFAULT, 1, "event",
	     "Send execute or cancel message",
	     terminateClickGesture);
  sendMethod(class, NAME_cancel, DEFAULT, 1, "event",
	     "Cancel this gesture and try the next",
	     cancelClickGesture);

  attach_resource(class, "modifier", "modifier", "",
		  "Condition on shift, control and meta");
  attach_resource(class, "button", "button_name", "left",
		  "Active on which button (left)");
  attach_resource(class, "cursor", "[cursor]", "@default",
		  "Cursor while active");
  attach_resource(class, "execute_cursor", "cursor*", "watch",
		  "Cursor while running execute_message");
  attach_resource(class, "max_drag_distance", "int*", "5",
		  "Cancel after dragging this far");

  succeed;
}
