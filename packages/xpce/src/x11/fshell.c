/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include "md.h"
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "fshellP.h"

static XtResource top_level_resources[] = {
#define offset(field) XtOffset(TopLevelFrameWidget, top_level_frame.field)
    /* {name, class, type, size, offset, default_type, default_addr}, */
 { XtNeventCallback,  XtCCallback, XtRCallback, sizeof(XtCallbackList),
	offset(event_callback),  XtRCallback, NULL },
#undef offset
};


static XtResource override_resources[] = {
#define offset(field) XtOffset(OverrideFrameWidget, override_frame.field)
    /* {name, class, type, size, offset, default_type, default_addr}, */
 { XtNeventCallback,  XtCCallback, XtRCallback, sizeof(XtCallbackList),
	offset(event_callback),  XtRCallback, NULL },
#undef offset
};


static XtResource transient_resources[] = {
#define offset(field) XtOffset(TransientFrameWidget, transient_frame.field)
    /* {name, class, type, size, offset, default_type, default_addr}, */
 { XtNeventCallback,  XtCCallback, XtRCallback, sizeof(XtCallbackList),
	offset(event_callback),  XtRCallback, NULL },
#undef offset
};


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Actions
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
eventFrame(Widget w, XEvent *event, String *params, Cardinal *num_params)
{ XtCallCallbacks(w, XtNeventCallback, (caddr_t) event);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These structures cannot be shared because they are changed inplace when
the widget-class is initialised.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static XtActionsRec top_level_actions[] =
{
  /* {name, procedure}, */
    {"event",	eventFrame},
};

static XtActionsRec override_actions[] =
{
  /* {name, procedure}, */
    {"event",	eventFrame},
};

static XtActionsRec transient_actions[] =
{
  /* {name, procedure}, */
    {"event",	eventFrame},
};

static char translations[] =
#if _AIX
" <Message>:	event() \n\
  <Unmap>:	event() \n\
  <Map>:	event() \n\
  <Prop>:	event() \n\
  <Configure>:	event() \n\
  <Circ>:	event() \n\
  <Key>:	event() \n\
  <BtnDown>:	event()	\n\
  <BtnUp>:	event()	\n\
  <Motion>:	event()	\n\
  <Enter>:	event() \n\
  <Leave>:	event() \n\
  <FocusIn>:	event() \n\
  <FocusOut>:	event() \n\
";
#else
" <Message>:	event() \n\
  <Unmap>:	event() \n\
  <Map>:	event() \n\
  <Configure>:	event() \n\
  <Circ>:	event() \n\
  <Key>:	event() \n\
  <BtnDown>:	event()	\n\
  <BtnUp>:	event()	\n\
  <Motion>:	event()	\n\
  <Enter>:	event() \n\
  <Leave>:	event() \n\
  <FocusIn>:	event() \n\
  <FocusOut>:	event() \n\
";
#endif

static void
xpce_change_managed(Widget w)
{ /* don't do anything, this is done by XPCE itself */
}


TopLevelFrameClassRec topLevelFrameClassRec = {
  {
    /* superclass         */    (WidgetClass) &topLevelShellClassRec,
    /* class_name         */    "TopLevelFrame",
    /* size               */    sizeof(TopLevelFrameRec),
    /* Class Initializer  */	NULL,
    /* class_part_initialize*/	NULL,
    /* Class init'ed ?    */	FALSE,
    /* initialize         */    NULL,
    /* initialize_notify  */	NULL,		
    /* realize            */    XtInheritRealize,
    /* actions            */    top_level_actions,
    /* num_actions        */    XtNumber(top_level_actions),
    /* resources          */    top_level_resources,
    /* resource_count     */	XtNumber(top_level_resources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion    */    FALSE,
    /* compress_exposure  */    TRUE,
    /* compress_enterleave*/ 	FALSE,
    /* visible_interest   */    FALSE,
    /* destroy            */    NULL,
    /* resize             */    NULL,
    /* expose             */    NULL,
    /* set_values         */    NULL,
    /* set_values_hook    */	NULL,			
    /* set_values_almost  */	XtInheritSetValuesAlmost,  
    /* get_values_hook    */	NULL,			
    /* accept_focus       */    NULL,
    /* intrinsics version */	XtVersion,
    /* callback offsets   */    NULL,
    /* tm_table		  */    translations,
    /* query_geometry	  */    NULL,
    /* display_accelerator*/    NULL,
    /* extension	  */    NULL
  },{					/* Composite */
    /* geometry_manager   */    XtInheritGeometryManager,
    /* change_managed     */    xpce_change_managed,/*XtInheritChangeManaged,*/
    /* insert_child	  */	XtInheritInsertChild,
    /* delete_child	  */	XtInheritDeleteChild,
    /* extension	  */	NULL
  },{					/* Shell */
    /* extension	  */	NULL
  },{					/* WM */
    /* extension	  */	NULL
  },{					/* Vendor */
    /* extension	  */	NULL
  },{					/* Toplevel Shell */
    /* extension	  */	NULL
  },{					/* TopLevel Frame */
    /* extension	  */	NULL
  }
};

WidgetClass topLevelFrameWidgetClass = (WidgetClass) (&topLevelFrameClassRec);

OverrideFrameClassRec overrideFrameClassRec = {
  {
    /* superclass         */    (WidgetClass) &overrideShellClassRec,
    /* class_name         */    "OverrideFrame",
    /* size               */    sizeof(OverrideFrameRec),
    /* Class Initializer  */	NULL,
    /* class_part_initialize*/	NULL,
    /* Class init'ed ?    */	FALSE,
    /* initialize         */    NULL,
    /* initialize_notify  */	NULL,		
    /* realize            */    XtInheritRealize,
    /* actions            */    override_actions,
    /* num_actions        */    XtNumber(override_actions),
    /* resources          */    override_resources,
    /* resource_count     */	XtNumber(override_resources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion    */    FALSE,
    /* compress_exposure  */    TRUE,
    /* compress_enterleave*/ 	FALSE,
    /* visible_interest   */    FALSE,
    /* destroy            */    NULL,
    /* resize             */    NULL,
    /* expose             */    NULL,
    /* set_values         */    NULL,
    /* set_values_hook    */	NULL,			
    /* set_values_almost  */	XtInheritSetValuesAlmost,  
    /* get_values_hook    */	NULL,			
    /* accept_focus       */    NULL,
    /* intrinsics version */	XtVersion,
    /* callback offsets   */    NULL,
    /* tm_table		  */    translations,
    /* query_geometry	  */    NULL,
    /* display_accelerator*/    NULL,
    /* extension	  */    NULL
  },{					/* Composite */
    /* geometry_manager   */    XtInheritGeometryManager,
    /* change_managed     */    xpce_change_managed,/*XtInheritChangeManaged,*/
    /* insert_child	  */	XtInheritInsertChild,
    /* delete_child	  */	XtInheritDeleteChild,
    /* extension	  */	NULL
  },{					/* Shell */
    /* extension	  */	NULL
  },{					/* OverrideShell */
    /* extension	  */	NULL
  },{					/* OverrideFrame */
    /* extension	  */	NULL
  }
};

WidgetClass overrideFrameWidgetClass = (WidgetClass) (&overrideFrameClassRec);

TransientFrameClassRec transientFrameClassRec = {
  {
    /* superclass         */    (WidgetClass) &transientShellClassRec,
    /* class_name         */    "TransientFrame",
    /* size               */    sizeof(TransientFrameRec),
    /* Class Initializer  */	NULL,
    /* class_part_initialize*/	NULL,
    /* Class init'ed ?    */	FALSE,
    /* initialize         */    NULL,
    /* initialize_notify  */	NULL,		
    /* realize            */    XtInheritRealize,
    /* actions            */    transient_actions,
    /* num_actions        */    XtNumber(transient_actions),
    /* resources          */    transient_resources,
    /* resource_count     */	XtNumber(transient_resources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion    */    FALSE,
    /* compress_exposure  */    TRUE,
    /* compress_enterleave*/ 	FALSE,
    /* visible_interest   */    FALSE,
    /* destroy            */    NULL,
    /* resize             */    NULL,
    /* expose             */    NULL,
    /* set_values         */    NULL,
    /* set_values_hook    */	NULL,			
    /* set_values_almost  */	XtInheritSetValuesAlmost,  
    /* get_values_hook    */	NULL,			
    /* accept_focus       */    NULL,
    /* intrinsics version */	XtVersion,
    /* callback offsets   */    NULL,
    /* tm_table		  */    translations,
    /* query_geometry	  */    NULL,
    /* display_accelerator*/    NULL,
    /* extension	  */    NULL
  },{					/* Composite */
    /* geometry_manager   */    XtInheritGeometryManager,
    /* change_managed     */    xpce_change_managed,/*XtInheritChangeManaged,*/
    /* insert_child	  */	XtInheritInsertChild,
    /* delete_child	  */	XtInheritDeleteChild,
    /* extension	  */	NULL
  },{					/* Shell */
    /* extension	  */	NULL
  },{					/* WMShell */
    /* extension	  */	NULL
  },{					/* Vendor */
    /* extension	  */	NULL
  },{					/* TransientShell */
    /* extension	  */	NULL
  },{					/* TransientFrame */
    /* extension	  */	NULL
  }
};

WidgetClass transientFrameWidgetClass = (WidgetClass)(&transientFrameClassRec);
