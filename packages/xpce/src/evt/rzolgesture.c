/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialiseResizeOutlineGesture(ResizeOutlineGesture g, Name button,
			       Modifier modifier)
{ initialiseResizeGesture((ResizeGesture) g, button, modifier);
  assign(g, outline, newObject(ClassBox, 0));
  assign(g, outline_gesture, newObject(ClassResizeGesture,
				       g->button, g->modifier, 0));

  send(g->outline, NAME_texture, getResourceValueObject(g, NAME_texture), 0);
  
  succeed;
}


		/********************************
		*       GESTURE BEHAVIOUR	*
		********************************/

static status
initiateResizeOutlineGesture(ResizeOutlineGesture g, EventObj ev)
{ send(g->outline, NAME_area, ev->receiver->area, 0);
  send(ev->receiver->device, NAME_display, g->outline, 0);
  postEvent(ev, (Graphical) g->outline, (Recogniser) g->outline_gesture);

  succeed;
}


static status
dragResizeOutlineGesture(ResizeOutlineGesture g, EventObj ev)
{ postEvent(ev, (Graphical) g->outline, (Recogniser) g->outline_gesture);

  succeed;
}


static status
terminateResizeOutlineGesture(ResizeOutlineGesture g, EventObj ev)
{ Area a;

  postEvent(ev, (Graphical) g->outline, (Recogniser) g->outline_gesture);

  a = g->outline->area;
  send(ev->receiver, NAME_doSet, a->x, a->y, a->w, a->h, 0);
  send(g->outline, NAME_device, NIL, 0);

  succeed;
}


		/********************************
		*           ATTRIBUTES		*
		********************************/

static status
minSizeResizeOulineGesture(ResizeOutlineGesture g, Size sz)
{ assign(g->outline_gesture, min_size, sz);
  succeed;
}


static status
maxSizeResizeOulineGesture(ResizeOutlineGesture g, Size sz)
{ assign(g->outline_gesture, max_size, sz);
  succeed;
}


status
makeClassResizeOutlineGesture(Class class)
{ sourceClass(class, makeClassResizeOutlineGesture, __FILE__,
	      "1.1.1.1");

  localClass(class, NAME_outline, NAME_feedback,
	     "box", NAME_get,
	     "The outline resized");
  localClass(class, NAME_outlineGesture, NAME_internal,
	     "resize_gesture", NAME_get,
	     "The outline resized");

  termClass(class, "resize_outline_gesture", 2, NAME_button, NAME_modifier);

  storeMethod(class, NAME_minSize, minSizeResizeOulineGesture);
  storeMethod(class, NAME_maxSize, maxSizeResizeOulineGesture);

  sendMethod(class, NAME_initialise, DEFAULT,
	     2, "button=[button_name]", "modifier=[modifier]",
	     "Create from button and modifier",
	     initialiseResizeOutlineGesture);
  sendMethod(class, NAME_initiate, NAME_event, 1, "event",
	     "Display outline and change cursor",
	     initiateResizeOutlineGesture);
  sendMethod(class, NAME_drag, NAME_event, 1, "event",
	     "Drag outline to next position",
	     dragResizeOutlineGesture);
  sendMethod(class, NAME_terminate, NAME_event, 1, "event",
	     "Resize object and undisplay outline",
	     terminateResizeOutlineGesture);

  attach_resource(class, "texture", "texture_name", "dotted",
		  "Texture of the outline box");
  attach_resource(class, "button", "button_name", "middle",
		  "Active on which button (middle)");

  succeed;
}
