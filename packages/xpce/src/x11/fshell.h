/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#ifndef _Frame_h
#define _Frame_h

/****************************************************************
 *
 * Frame widget
 *
 ****************************************************************/

/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 eventCallback	     Callback	        Callback	NULL
*/

/* define any special resource names here that are not in <X11/StringDefs.h> */

#define XtNeventCallback	"eventCallback"

/* declare specific FrameWidget class and instance datatypes */

typedef struct _TopLevelFrameClassRec*	TopLevelFrameWidgetClass;
typedef struct _TopLevelFrameRec*	TopLevelFrameWidget;

typedef struct _OverrideFrameClassRec*	OverrideFrameWidgetClass;
typedef struct _OverrideFrameRec*	OverrideFrameWidget;

typedef struct _TransientFrameClassRec*	TransientFrameWidgetClass;
typedef struct _TransientFrameRec*	TransientFrameWidget;

/* declare the class constant */

extern WidgetClass topLevelFrameWidgetClass;
extern WidgetClass overrideFrameWidgetClass;
extern WidgetClass transientFrameWidgetClass;

#endif /* _Frame_h */
