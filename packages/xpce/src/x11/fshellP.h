/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#ifndef _FrameP_h
#define _FrameP_h

#include "fshell.h"
/* include superclass private header file */
#include <X11/ShellP.h>

/* define unique representation types not found in <X11/StringDefs.h> */

#define XtRFrameResource "FrameResource"

		/********************************
		*           TOPLEVEL		*
		********************************/

typedef struct
{ XtPointer extension;
} TopLevelFrameClassPart;

typedef struct _TopLevelFrameClassRec
{ CoreClassPart			core_class;
  CompositeClassPart		composite_class;
  ShellClassPart		shell_class;
  WMShellClassPart		wm_shell_class;
  VendorShellClassPart		vendor_shell_class;
  TopLevelShellClassPart	top_level_shell_class;
  TopLevelFrameClassPart	top_level_frame_class;
} TopLevelFrameClassRec;

extern TopLevelFrameClassRec topLevelFrameClassRec;

typedef struct
{   /* resources */
  XtCallbackList		event_callback;
    /* private state */
    /* (none) */
} TopLevelFramePart;

typedef struct _TopLevelFrameRec
{ CorePart	 		core;
  CompositePart 		composite;
  ShellPart 			shell;
  WMShellPart			wm;
  VendorShellPart		vendor;
  TopLevelShellPart		top_level_shell;
  TopLevelFramePart		top_level_frame;
} TopLevelFrameRec;

		/********************************
		*          OVERRIDE		*
		********************************/

typedef struct
{ XtPointer extension;
} OverrideFrameClassPart;

typedef struct _OverrideFrameClassRec
{ CoreClassPart			core_class;
  CompositeClassPart		composite_class;
  ShellClassPart		shell_class;
  OverrideShellClassPart	override_shell_class;
  OverrideFrameClassPart	override_frame_class;
} OverrideFrameClassRec;

extern OverrideFrameClassRec overrideFrameClassRec;

typedef struct
{   /* resources */
  XtCallbackList		event_callback;
    /* private state */
    /* (none) */
} OverrideFramePart;

typedef struct _OverrideFrameRec
{ CorePart 			core;
  CompositePart 		composite;
  ShellPart 			shell;
  TopLevelShellPart		override_shell;
  OverrideFramePart		override_frame;
} OverrideFrameRec;


		/********************************
		*          TRANSIENT		*
		********************************/

typedef struct
{ XtPointer extension;
} TransientFrameClassPart;

typedef struct _TransientFrameClassRec
{ CoreClassPart			core_class;
  CompositeClassPart		composite_class;
  ShellClassPart		shell_class;
  WMShellClassPart		wm_shell_class;
  VendorShellClassPart		vendor_shell_class;
  TransientShellClassPart	transient_shell_class;
  TransientFrameClassPart	transient_frame_class;
} TransientFrameClassRec;

extern TransientFrameClassRec transientFrameClassRec;

typedef struct
{   /* resources */
  XtCallbackList		event_callback;
    /* private state */
    /* (none) */
} TransientFramePart;

typedef struct _TransientFrameRec
{ CorePart 			core;
  CompositePart 		composite;
  ShellPart 			shell;
  WMShellPart			wm;
  VendorShellPart		vendor;
  TransientShellPart		transient;	
  TransientFramePart		transient_frame;
} TransientFrameRec;

#endif /* _FrameP_h */

