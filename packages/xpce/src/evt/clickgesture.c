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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static const char *T_initialise[] =
        { "button=[button_name]", "modifier=[modifier]", "multiple=[{single,double,triple}]", "message=[code]*", "preview=[code]*", "cancel=[code]*" };

/* Instance Variables */

static const vardecl var_clickGesture[] =
{ IV(NAME_multiclick, "[{single,double,triple}]", IV_BOTH,
     NAME_modifier, "Demand single, double or triple click"),
  IV(NAME_downPosition, "point", IV_GET,
     NAME_internal, "Position of the down event"),
  IV(NAME_executeMessage, "code*", IV_BOTH,
     NAME_action, "Message sent on up inside area"),
  IV(NAME_previewMessage, "code*", IV_BOTH,
     NAME_feedback, "Message sent on down"),
  IV(NAME_cancelMessage, "code*", IV_BOTH,
     NAME_feedback, "Message sent on up outside area"),
  IV(NAME_executeCursor, "cursor*", IV_BOTH,
     NAME_feedback, "Cursor displayed while message is executed"),
  IV(NAME_maxDragDistance, "int*", IV_BOTH,
     NAME_cancel, "Cancel after dragging this far")
};

/* Send Methods */

static const senddecl send_clickGesture[] =
{ SM(NAME_cancel, 1, "event", cancelClickGesture,
     DEFAULT, "Cancel this gesture and try the next"),
  SM(NAME_drag, 1, "event", dragClickGesture,
     DEFAULT, "Does nothing"),
  SM(NAME_initialise, 6, T_initialise, initialiseClickGesture,
     DEFAULT, "Create from button, modifier, multi, ..."),
  SM(NAME_initiate, 1, "event", initiateClickGesture,
     DEFAULT, "Send preview message"),
  SM(NAME_terminate, 1, "event", terminateClickGesture,
     DEFAULT, "Send execute or cancel message"),
  SM(NAME_verify, 1, "event", verifyClickGesture,
     DEFAULT, "Verify modifier and multiclick")
};

/* Get Methods */

static const getdecl get_clickGesture[] =
{ 
};

/* Resources */

static const resourcedecl rc_clickGesture[] =
{ RC(NAME_button, "button_name", "left",
     "Active on which button (left)"),
  RC(NAME_cursor, "[cursor]", "@default",
     "Cursor while active"),
  RC(NAME_executeCursor, "cursor*", "watch",
     "Cursor while running execute_message"),
  RC(NAME_maxDragDistance, "int*", "5",
     "Cancel after dragging this far"),
  RC(NAME_modifier, "modifier", "",
     "Condition on shift, control and meta")
};

/* Class Declaration */

static Name clickGesture_termnames[] =
	{ NAME_button, NAME_modifier, NAME_multiclick,
	  NAME_executeMessage, NAME_previewMessage, NAME_cancelMessage };

ClassDecl(clickGesture_decls,
          var_clickGesture, send_clickGesture,
	  get_clickGesture, rc_clickGesture,
          6, clickGesture_termnames,
          "$Rev$");

status
makeClassClickGesture(Class class)
{ return declareClass(class, &clickGesture_decls);
}
