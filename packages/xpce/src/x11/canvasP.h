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

#ifndef _CanvasP_h
#define _CanvasP_h

#include "canvas.h"
/* include superclass private header file */
#include <X11/CompositeP.h>

/* define unique representation types not found in <X11/StringDefs.h> */

#define XtRCanvasResource "CanvasResource"

typedef struct
{ XtPointer extension;
} CanvasClassPart;

typedef struct _CanvasClassRec
{ CoreClassPart		core_class;
  CompositeClassPart	composite_class;
  CanvasClassPart	canvas_class;
} CanvasClassRec;

extern CanvasClassRec canvasClassRec;

typedef struct
{   /* resources */
  XtCallbackList	event_callback;
  XtCallbackList	expose_callback;
  XtCallbackList	resize_callback;
    /* private state */
    /* (none) */
} CanvasPart;

typedef struct _CanvasRec
{ CorePart		core;
  CompositePart		composite;
  CanvasPart		canvas;
} CanvasRec;

#endif /* _CanvasP_h */
