/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#ifndef _Canvas_h
#define _Canvas_h

/****************************************************************
 *
 * Canvas widget
 *
 ****************************************************************/

/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 eventCallback	     Callback	        Callback	NULL
 exposeCallback	     Callback		Callback	NULL
*/

/* define any special resource names here that are not in <X11/StringDefs.h> */

#define XtNeventCallback	"eventCallback"
#define XtNexposeCallback	"exposeCallback"
#define XtNresizeCallback	"resizeCallback"

/* declare specific CanvasWidget class and instance datatypes */

typedef struct _CanvasClassRec*	CanvasWidgetClass;
typedef struct _CanvasRec*	CanvasWidget;

/* declare the class constant */

extern WidgetClass canvasWidgetClass;

#endif /* _Canvas_h */
