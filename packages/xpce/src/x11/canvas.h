/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
