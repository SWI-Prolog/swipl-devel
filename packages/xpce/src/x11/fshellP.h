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
  XtCallbackList		expose_callback;
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
  XtCallbackList		expose_callback;
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
  XtCallbackList		expose_callback;
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

