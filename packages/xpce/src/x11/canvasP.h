/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
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
