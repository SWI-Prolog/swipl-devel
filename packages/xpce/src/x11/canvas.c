/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include "md.h"

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "canvasP.h"

static XtResource resources[] = {
#define offset(field) XtOffset(CanvasWidget, canvas.field)
    /* {name, class, type, size, offset, default_type, default_addr}, */
 { XtNeventCallback,  XtCCallback, XtRCallback, sizeof(XtCallbackList),
	offset(event_callback),  XtRCallback, NULL },
 { XtNexposeCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
	offset(expose_callback), XtRCallback, NULL },
 { XtNresizeCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
	offset(resize_callback), XtRCallback, NULL },
#undef offset
};


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Actions
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
eventCanvas(Widget w, XEvent *event, String *params, Cardinal *num_params)
{ XtCallCallbacks(w, XtNeventCallback, (caddr_t) event);
}


static XtActionsRec actions[] =
{
  /* {name, procedure}, */
    {"event",	eventCanvas},
};

static char translations[] =
" <Message>:	event() \n\
  <Key>:	event()	\n\
  <BtnDown>:	event()	\n\
  <BtnUp>:	event()	\n\
  <Motion>:	event()	\n\
  <Enter>:	event() \n\
  <Leave>:	event() \n\
  <FocusIn>:	event() \n\
  <FocusOut>:	event() \n\
  <Map>:	event() \n\
";


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Redefined standard method (expose)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
exposeCanvas(Widget w, XEvent *event, Region region)
{ XtCallCallbacks(w, XtNexposeCallback, (caddr_t) region);
}


static void
resizeCanvas(Widget w)
{ XtCallCallbacks(w, XtNresizeCallback, NULL);
}


CanvasClassRec canvasClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &compositeClassRec,
    /* class_name		*/	"Canvas",
    /* widget_size		*/	sizeof(CanvasRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	NULL,
    /* initialize_hook		*/	NULL,
    /* realize			*/	XtInheritRealize,
    /* actions			*/	actions,
    /* num_actions		*/	XtNumber(actions),
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	NULL,
    /* resize			*/	resizeCanvas,
    /* expose			*/	exposeCanvas,
    /* set_values		*/	NULL,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	translations,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  }, {					/* Composite */
    /* geometry_manager   	*/      XtInheritGeometryManager,
    /* change_managed     	*/      XtInheritChangeManaged,
    /* insert_child	  	*/	XtInheritInsertChild,
    /* delete_child	  	*/	XtInheritDeleteChild,
    /* extension	  	*/	NULL
  }, { /* canvas fields */
    /* empty			*/	0
  }
};

WidgetClass canvasWidgetClass = (WidgetClass)&canvasClassRec;
