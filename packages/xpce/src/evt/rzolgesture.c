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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static const char *T_initialise[] =
        { "button=[button_name]", "modifier=[modifier]" };

/* Instance Variables */

static const vardecl var_resizeOutlineGesture[] =
{ IV(NAME_outline, "box", IV_GET,
     NAME_feedback, "The outline resized"),
  IV(NAME_outlineGesture, "resize_gesture", IV_GET,
     NAME_internal, "The outline resized")
};

/* Send Methods */

static const senddecl send_resizeOutlineGesture[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseResizeOutlineGesture,
     DEFAULT, "Create from button and modifier"),
  SM(NAME_drag, 1, "event", dragResizeOutlineGesture,
     NAME_event, "Drag outline to next position"),
  SM(NAME_initiate, 1, "event", initiateResizeOutlineGesture,
     NAME_event, "Display outline and change cursor"),
  SM(NAME_terminate, 1, "event", terminateResizeOutlineGesture,
     NAME_event, "Resize object and undisplay outline"),
  SM(NAME_maxSize, 1, "size*", maxSizeResizeOulineGesture,
     NAME_constraint, "Specify maximum size of the graphical"),
  SM(NAME_minSize, 1, "size*", minSizeResizeOulineGesture,
     NAME_constraint, "Specify minimum size of the graphical")
};

/* Get Methods */

static const getdecl get_resizeOutlineGesture[] =
{ 
};

/* Resources */

static const resourcedecl rc_resizeOutlineGesture[] =
{ RC(NAME_button, "button_name", "middle",
     "Active on which button (middle)"),
  RC(NAME_texture, "texture_name", "dotted",
     "Texture of the outline box")
};

/* Class Declaration */

static Name resizeOutlineGesture_termnames[] = { NAME_button, NAME_modifier };

ClassDecl(resizeOutlineGesture_decls,
          var_resizeOutlineGesture, send_resizeOutlineGesture,
	  get_resizeOutlineGesture, rc_resizeOutlineGesture,
          2, resizeOutlineGesture_termnames,
          "$Rev$");


status
makeClassResizeOutlineGesture(Class class)
{ return declareClass(class, &resizeOutlineGesture_decls);
}
