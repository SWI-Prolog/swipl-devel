/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include "include.h"
#include "canvas.h"

static Chain grabbedWindows = NIL;

static void event_window(Widget w, XtPointer xsw, XtPointer xevent);
static void expose_window(Widget w, XtPointer xsw, XtPointer xregion);
static void resize_window(Widget w, XtPointer xsw, XtPointer data);
static void destroy_window(Widget w, XtPointer xsw, XtPointer data);

		 /*******************************
		 *	 WIDGET REFERENCE	*
		 *******************************/

static status
setWidgetWindow(PceWindow sw, Widget w)
{ sw->ws_ref = (WsRef) w;

  if ( !w )
    assign(sw, displayed, OFF);

  succeed;
}


		/********************************
		*            (UN)CREATE		*
		********************************/

status
ws_created_window(PceWindow sw)
{ if ( widgetWindow(sw) != NULL )
    succeed;
    
  fail;
}


void
ws_uncreate_window(PceWindow sw)
{ Widget w;

  if ( notNil(grabbedWindows) )
    deleteChain(grabbedWindows, sw);

  if ( (w=widgetWindow(sw)) )
  { XtRemoveAllCallbacks(w, XtNeventCallback);
    XtRemoveAllCallbacks(w, XtNexposeCallback);
    XtRemoveAllCallbacks(w, XtNresizeCallback);
    XtRemoveAllCallbacks(w, XtNdestroyCallback);

    XtDestroyWidget(w);
    destroy_window(w, (XtPointer)sw, NULL); /* callback may be delayed */
					    /* too long */
  }
}


status
ws_create_window(PceWindow sw, PceWindow parent)
{ Widget w;
  DisplayObj d = getDisplayGraphical((Graphical)sw);

					/* create the widget */
  { Arg args[8];
    Cardinal n = 0;
    int pen = valInt(sw->pen);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Actually, it appears Xt is ignoring the geometry parameters.  Hence, 
ws_realise_frame() sends ->geometry to all windows.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    XtSetArg(args[n], XtNx,	      valInt(sw->area->x)); n++;
    XtSetArg(args[n], XtNy,	      valInt(sw->area->y)); n++;
    XtSetArg(args[n], XtNwidth,       valInt(sw->area->w) - 2*pen); n++;
    XtSetArg(args[n], XtNheight,      valInt(sw->area->h) - 2*pen); n++;
    XtSetArg(args[n], XtNborderWidth, pen); n++;
    XtSetArg(args[n], XtNinput,       True); n++;
    if ( instanceOfObject(sw->background, ClassColour) )
    { XtSetArg(args[n], XtNbackground, getPixelColour(sw->background, d));
      n++;
    } else
    { Pixmap pm = (Pixmap) getXrefObject(sw->background, d);

      XtSetArg(args[n], XtNbackgroundPixmap, pm); n++;
    }

    DEBUG(NAME_create, Cprintf("Calling XtCreateWidget ..."));
    w = XtCreateWidget(strName(sw->name),
		       canvasWidgetClass,
		       isDefault(parent) ? widgetFrame(sw->frame)
		       			 : widgetWindow(parent),
		       args, n);
    DEBUG(NAME_create, Cprintf("Widget = %ld\n", (ulong) w));
  }

  if ( !w )
    return errorPce(w, NAME_createFailed);

  setWidgetWindow(sw, w);

  XtAddCallback(w, XtNeventCallback,   event_window, sw);
  XtAddCallback(w, XtNexposeCallback,  expose_window, sw);
  XtAddCallback(w, XtNresizeCallback,  resize_window, sw);
  XtAddCallback(w, XtNdestroyCallback, destroy_window, sw);

  if ( notDefault(parent) )		/* make a sub-window */
  { XtManageChild(w);
    send(sw, NAME_displayed, ON, 0);
  }

  succeed;
}


void
ws_manage_window(PceWindow sw)
{ XtManageChild(widgetWindow(sw));
} 


void
ws_unmanage_window(PceWindow sw)
{ XtUnmanageChild(widgetWindow(sw));
} 


		 /*******************************
		 *	 DECORATE SUPPORT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Reassocicate the WS window of `from' with `to'.  Used by ->decorate.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
ws_reassociate_ws_window(PceWindow from, PceWindow to)
{ Widget w = widgetWindow(from);

  if ( w )
  { XtRemoveAllCallbacks(w, XtNeventCallback);
    XtRemoveAllCallbacks(w, XtNexposeCallback);
    XtRemoveAllCallbacks(w, XtNresizeCallback);
    setWidgetWindow(from, NULL);

    setWidgetWindow(to, w);
    XtAddCallback(w, XtNeventCallback,  event_window, to);
    XtAddCallback(w, XtNexposeCallback, expose_window, to);
    XtAddCallback(w, XtNresizeCallback, resize_window, to);
  }
}

		 /*******************************
		 *	      GEOMETRY		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Define the size and thickness of  outside   border  of  the window.  The
described area is the *outline* of the window *including* its border.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


void
ws_geometry_window(PceWindow sw, int x, int y, int w, int h, int pen)
{ Widget wid = widgetWindow(sw);

  w -= 2*pen;
  h -= 2*pen;
  if ( w < 1 ) w = 1;			/* avoid X-errors */
  if ( h < 1 ) h = 1;

  if ( wid )
    XtConfigureWidget(wid, x, y, w, h, pen);
}


		/********************************
		*      XT CALLBACK HANDLING	*
		********************************/

static void
event_window(Widget w, XtPointer xsw, XtPointer xevent)
{ EventObj ev;
  PceWindow sw = (PceWindow) xsw;
  XEvent *event = (XEvent *)xevent;

  if ( isFreeingObj(sw) || isFreedObj(sw) )
    return;

  ServiceMode(is_service_window(sw),
	      switch(event->xany.type)
	      { default:
		{ AnswerMark mark;
		  markAnswerStack(mark);
  
		  if ( (ev = CtoEvent(sw, event)) )
		  { addCodeReference(ev);
		    postEvent(ev, (Graphical) sw, DEFAULT);
		    delCodeReference(ev);
		    freeableObj(ev);
		  }
		  
		  rewindAnswerStack(mark, NIL);
		}
	      })
}


static void
expose_window(Widget w, XtPointer xsw, XtPointer xregion)
{ XRectangle rect;
  Area a;
  PceWindow sw = (PceWindow) xsw;
  Region region = (Region) xregion;
  Window win;

  DEBUG(NAME_window, Cprintf("Window %ld ---> %s\n", XtWindow(w), pp(sw)));
  if ( !getMemberHashTable(WindowTable, (Any) (win=XtWindow(w))) )
    appendHashTable(WindowTable, (Any) win, sw);

  XClipBox(region, &rect);
  ServiceMode(is_service_window(sw),
	      a = tempObject(ClassArea, toInt(rect.x), toInt(rect.y),
			     toInt(rect.width), toInt(rect.height), 0);
	      redrawWindow(sw, a);
	      considerPreserveObject(a));
}


static void
resize_window(Widget w, XtPointer xsw, XtPointer data)
{ PceWindow sw = (PceWindow) xsw;
  Area a = sw->area;
  Int ow = a->w, oh = a->h;

  ServiceMode(is_service_window(sw),
	      qadSendv(sw, NAME_resize, 0, NULL);
	      changedUnionWindow(sw, a->x, a->y, ow, oh));
}


static void
destroy_window(Widget w, XtPointer xsw, XtPointer data)
{ PceWindow sw = (PceWindow) xsw;

  DEBUG(NAME_window, Cprintf("destroy_window(%s)\n", pp(sw)));
  deleteHashTable(WindowTable, (Any) XtWindow(w));
  setWidgetWindow(sw, NULL);
}


		/********************************
		*     GRAB POINTER/KEYBOARD	*
		********************************/

#if XT_REVISION < 4

void
ws_grab_pointer_window(PceWindow sw, Bool val)
{ if ( isNil(grabbedWindows) )
    grabbedWindows = globalObject(NAME_GrabbedWindows, ClassChain, 0);

  if ( widgetWindow(sw) != NULL )
  { if ( val == ON )
    { XGrabPointer(getDisplayGraphical((Graphical)sw)->displayXref,
		   XtWindow(widgetWindow(sw)),
		   False,
		   ButtonPressMask|ButtonReleaseMask|
		   EnterWindowMask|LeaveWindowMask|
		   PointerMotionMask|ButtonMotionMask,
		   GrabModeAsync, GrabModeAsync,
		   None,
		   None,
		   CurrentTime);
      appendChain(grabbedWindows, sw);
    } else
    { deleteChain(grabbedWindows, sw);
      if ( notNil(grabbedWindows->tail) )
      { PceWindow sw2 = (PceWindow) grabbedWindows->tail->value;

	XGrabPointer(getDisplayGraphical((Graphical)sw2)->displayXref,
		     XtWindow(widgetWindow(sw2)),
		     False,
		     ButtonPressMask|ButtonReleaseMask|
		     EnterWindowMask|LeaveWindowMask|
		     PointerMotionMask|ButtonMotionMask,
		     GrabModeAsync, GrabModeAsync,
		     None,
		     None,
		     CurrentTime);
      }
    }
  }
}


void
ws_grab_keyboard_window(PceWindow sw, Bool val)
{ if ( widgetWindow(sw) != NULL )
  { DisplayObj d = getDisplayGraphical((Graphical)sw);

    if ( val == ON )
    { XSetInputFocus(d, XtWindow(widgetWindow(sw)),
		     RevertToPointerRoot,
		     current_event_time());
    } else
    { XSetInputFocus(d, PointerRoot,
		     0,
		     current_event_time());
    }
    succeed;
  }

  fail;
}

#else /*XT_REVISION >= 4*/

static status
do_grab_window(PceWindow sw)
{ if ( widgetWindow(sw) != NULL )
  { int rval;
    char *msg = NULL;

    rval = XtGrabPointer(widgetWindow(sw),
			 False,
			 ButtonPressMask|ButtonReleaseMask|
			 EnterWindowMask|LeaveWindowMask|
			 PointerMotionMask|ButtonMotionMask,
			 GrabModeAsync, GrabModeAsync,
			 None,
			 None,
			 CurrentTime);
    switch(rval)
    { case GrabNotViewable:	msg = "not viewable";	 break;
      case AlreadyGrabbed:	msg = "already grabbed"; break;
      case GrabFrozen:		msg = "grab frozen";     break;
      case GrabInvalidTime:	msg = "invalid time";    break;
    }
    if ( msg )
      return errorPce(sw, NAME_cannotGrabPointer, CtoName(msg));

    succeed;
  }

  fail;
}


void
ws_grab_pointer_window(PceWindow sw, Bool val)
{ if ( isNil(grabbedWindows) )
    grabbedWindows = globalObject(NAME_GrabbedWindows, ClassChain, 0);

  if ( widgetWindow(sw) != NULL )
  { if ( val == ON )
    { do_grab_window(sw);
      appendChain(grabbedWindows, sw);
    } else
    { XtUngrabPointer(widgetWindow(sw), CurrentTime);
      flushWindow(sw);
      deleteChain(grabbedWindows, sw);
      if ( notNil(grabbedWindows->tail) )
        do_grab_window(grabbedWindows->tail->value);
    }
  }
}


void
ws_grab_keyboard_window(PceWindow sw, Bool val)
{ if ( widgetWindow(sw) != NULL )
  { if ( val == ON )
      XtGrabKeyboard(widgetWindow(sw),
		     True,
		     GrabModeAsync,
		     GrabModeAsync,
		     CurrentTime);
    else
      XtUngrabKeyboard(widgetWindow(sw), CurrentTime);
  }
}

#endif /* XT_REVISION > 4 */

void
ws_ungrab_all()
{ if ( notNil(grabbedWindows) )
  { if ( notNil(grabbedWindows->tail) )
    { PceWindow sw = grabbedWindows->tail->value;

      if ( widgetWindow(sw) )
      { XtUngrabPointer(widgetWindow(sw), CurrentTime);
	flushWindow(sw);
      }
    }

    clearChain(grabbedWindows);
  }
}


void
ws_flash_area_window(PceWindow sw, int x, int y, int w, int h, int msecs)
{ if ( sw->displayed == ON )
  { int ox, oy, dw, dh;

    compute_window(sw, &ox, &oy, &dw, &dh);
    ox += valInt(sw->scroll_offset->x);
    oy += valInt(sw->scroll_offset->y);

    d_offset(ox, oy);
    d_window(sw, x, y, w, h, FALSE, TRUE);
    r_complement(x, y, w, h);
    d_flush();
    msleep(msecs);
    r_complement(x, y, w, h);
    d_flush();
    d_done();
  }
}


void
ws_flash_window(PceWindow sw, int msecs)
{ if ( sw->displayed == ON )
  { int w = valInt(sw->area->w);
    int h = valInt(sw->area->h);

    d_offset(0, 0);
    d_window(sw, 0, 0, w, h, FALSE, FALSE);

    r_complement(0, 0, w, h);
    d_flush();
    msleep(msecs);
    r_complement(0, 0, w, h);
    d_flush();

    d_done();
  }
}

		 /*******************************
		 *	      POINTER		*
		 *******************************/

void
ws_move_pointer(PceWindow sw, int x, int y)
{ DisplayObj d = getDisplayGraphical((Graphical)sw);
  DisplayWsXref r = d->ws_ref;

  XWarpPointer(r->display_xref,
	       None,
	       XtWindow(widgetWindow(sw)),
	       0, 0, 0, 0,
	       x, y);
}


void
ws_window_cursor(PceWindow sw, CursorObj cursor)
{ DisplayObj d = getDisplayGraphical((Graphical)sw);
  DisplayWsXref r = d->ws_ref;

  XDefineCursor(r->display_xref,
		XtWindow(widgetWindow(sw)),
		isNil(cursor) ? None
		              : (Cursor) getXrefObject(cursor, d));
}


		 /*******************************
		 *	      COLOURS		*
		 *******************************/

void
ws_window_background(PceWindow sw, Any c)
{ Widget w = widgetWindow(sw);

  if ( w )
  { Arg args[2];
    DisplayObj d = getDisplayGraphical((Graphical)sw);
    int i=0;

    if ( instanceOfObject(c, ClassColour) )
    { XtSetArg(args[i], XtNbackground, getPixelColour(c, d));		i++;
      XtSetArg(args[i], XtNbackgroundPixmap, XtUnspecifiedPixmap);	i++;
    } else
    { Pixmap pm = (Pixmap) getXrefObject(c, d);

      XtSetArg(args[i], XtNbackgroundPixmap, pm);			i++;
    }
      
    XtSetValues(w, args, i);
  }
}

		 /*******************************
		 *	     HIDE/EXPOSE	*
		 *******************************/

void
ws_raise_window(PceWindow sw)
{ DisplayObj d = getDisplayGraphical((Graphical)sw);
  DisplayWsXref r = d->ws_ref;
  Widget w = widgetWindow(sw);

  if ( w )
    XRaiseWindow(r->display_xref, XtWindow(w));
}

void
ws_lower_window(PceWindow sw)
{ DisplayObj d = getDisplayGraphical((Graphical)sw);
  DisplayWsXref r = d->ws_ref;
  Widget w = widgetWindow(sw);

  if ( w )
    XLowerWindow(r->display_xref, XtWindow(w));
}

