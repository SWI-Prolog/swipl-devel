/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
