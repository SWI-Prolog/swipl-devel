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
#define XtNexposeCallback	"exposeCallback"

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
