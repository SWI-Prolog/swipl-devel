/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include "include.h"
#include "fshell.h"

#if O_MOTIF
#define XMSTRINGDEFINES
#include <Xm/Xm.h>
#endif

static void	xEventFrame(Widget, FrameObj, XEvent *);
static void	expose_frame(Widget w, FrameObj fr, Region xregion);
static void	destroyFrame(Widget, FrameObj, XtPointer);
static status   updateAreaFrame(FrameObj fr, Int border);

#define MainWindow(fr)	     ( isNil(fr->members->head) ? (Any) fr : \
			       fr->members->head->value )


		 /*******************************
		 *	    REFERENCES		*
		 *******************************/

static frame_ws_ref *
ensureWsRefFrame(FrameObj fr)
{ if ( !fr->ws_ref )
  { fr->ws_ref = alloc(sizeof(frame_ws_ref));
    memset(fr->ws_ref, 0, sizeof(frame_ws_ref));
  }

  return fr->ws_ref;
}


static void
setWidgetFrame(FrameObj fr, Widget w)
{ ensureWsRefFrame(fr)->widget = w;
}


static void
setGravityFrame(FrameObj fr, int gravity)
{ ensureWsRefFrame(fr)->win_gravity = gravity;
}


static void
setBusyWindowFrame(FrameObj fr, Window w)
{ ensureWsRefFrame(fr)->busy_window = w;
}


static Window
busyWindowFrame(FrameObj fr)
{ return fr->ws_ref ? ((frame_ws_ref *)fr->ws_ref)->busy_window : 0;
}

		 /*******************************
		 *	     (UN)CREATE		*
		 *******************************/

status
ws_created_frame(FrameObj fr)
{ if ( widgetFrame(fr) )
    succeed;
  
  fail;
}


void
ws_uncreate_frame(FrameObj fr)
{ Widget w;

  if ( (w = widgetFrame(fr)) )
  { DEBUG(NAME_frame, Cprintf("ws_uncreate_frame(%s)\n", pp(fr)));

    XtPopdown(w);
    assign(fr, status, NAME_unmapped);
    setWidgetFrame(fr, NULL);

    XtRemoveCallback(w, XtNdestroyCallback,
		     (XtCallbackProc)destroyFrame, fr);
    XtRemoveCallback(w, XtNeventCallback,
		     (XtCallbackProc)xEventFrame, fr);

    XtDestroyWidget(w);

    if ( fr->ws_ref )
    { unalloc(sizeof(frame_ws_ref), fr->ws_ref);
      fr->ws_ref = NULL;
    }
  }
}


status
ws_create_frame(FrameObj fr)
{ Arg args[25];
  Cardinal n = 0;
  Widget w;
  DisplayObj d = fr->display;
  DisplayWsXref r = d->ws_ref;

  XtSetArg(args[n], XtNtitle,		  strName(fr->label));    n++;
  XtSetArg(args[n], XtNmappedWhenManaged, False);                 n++;
  XtSetArg(args[n], XtNwidth,      	  valInt(fr->area->w));   n++;
  XtSetArg(args[n], XtNheight,      	  valInt(fr->area->h));   n++;
  XtSetArg(args[n], XtNinput,		  True);		  n++;
  if ( instanceOfObject(fr->background, ClassColour) )
  { XtSetArg(args[n], XtNbackground, getPixelColour(fr->background, d));
    n++;
  } else
  { Pixmap pm = (Pixmap) getXrefObject(fr->background, d);

    XtSetArg(args[n], XtNbackgroundPixmap, pm); n++;
  }		

  if ( notNil(fr->icon_label) )
  { XtSetArg(args[n], XtNiconName, strName(getIconLabelFrame(fr)));
    n++;
  }
  if ( fr->kind == NAME_popup )
  { XtSetArg(args[n], XtNsaveUnder, True);
    n++;
  }
  if ( notNil(fr->icon_image) )
  { XtSetArg(args[n], XtNiconPixmap,
	     getXrefObject(fr->icon_image, fr->display));
    n++;
    if ( notNil(fr->icon_image->mask) )
    { XtSetArg(args[n], XtNiconMask,
	       getXrefObject(fr->icon_image->mask, fr->display));
      n++;
    }
  }
  if ( notNil(fr->icon_position) )
  { XtSetArg(args[n], XtNiconX, valInt(fr->icon_position->x)); n++;
    XtSetArg(args[n], XtNiconY, valInt(fr->icon_position->y)); n++;
  }
#if O_MOTIF
  XtSetArg(args[n], XmNdeleteResponse, XmDO_NOTHING); n++;
#endif

  w = XtCreatePopupShell(
		    strName(fr->label),
		    fr->kind == NAME_popup     ? overrideFrameWidgetClass  :
		    fr->kind == NAME_transient ? transientFrameWidgetClass :
					         topLevelFrameWidgetClass,
		    r->shell_xref,
		    args, n);

  if ( !w )
    return errorPce(fr, NAME_xOpen, fr->display);

  XtAddCallback(w, XtNeventCallback,
		(XtCallbackProc) xEventFrame, (caddr_t) fr);
  XtAddCallback(w, XtNexposeCallback,
		(XtCallbackProc) expose_frame, (caddr_t) fr);
  XtAddCallback(w, XtNdestroyCallback,
		(XtCallbackProc) destroyFrame, (caddr_t) fr);

  setWidgetFrame(fr, w);

  succeed;
}


void
ws_realise_frame(FrameObj fr)
{ LocalArray(Widget, children, valInt(getSizeChain(fr->members)));
  int n = 0;
  Widget w = widgetFrame(fr);
  Cell cell;
  DisplayWsXref r = fr->display->ws_ref;

  for_cell(cell, fr->members)
    children[n++] = widgetWindow(cell->value);

  XtManageChildren(children, n);
  XtRealizeWidget(w);

  for_cell(cell, fr->members)
    send(cell->value, NAME_geometry, EAV); /* see note at ws_create_window */

  if ( notNil(fr->transient_for) )
  { XSetTransientForHint(r->display_xref,
			 XtWindow(w),
			 XtWindow(widgetFrame(fr->transient_for)));
  }
#if O_MOTIF
  else
  { XDeleteProperty(r->display_xref,
		    XtWindow(w),
		    XA_WM_TRANSIENT_FOR);
  }
#endif

  ws_frame_background(fr, fr->background); /* Why is this necessary? */
}


		 /*******************************
		 *   FIND WINDOW HOLDING POINT	*
		 *******************************/

PceWindow
ws_window_holding_point_frame(FrameObj fr)
{ Cell cell;

  for_cell(cell, fr->members)
  { PceWindow sw = cell->value;

    if ( instanceOfObject(sw, ClassWindowDecorator) )
    { WindowDecorator dw = (WindowDecorator) sw;
      sw = dw->window;
    }

    if ( sw->has_pointer == ON )
      answer(sw);
  }

  fail;
}

		 /*******************************
		 *	    HIDE/EXPOSE		*
		 *******************************/

void
ws_raise_frame(FrameObj fr)
{ Widget w = widgetFrame(fr);
  DisplayWsXref r = fr->display->ws_ref;

  if ( w )
  { XMapWindow(r->display_xref, XtWindow(w));
    XRaiseWindow(r->display_xref, XtWindow(w));
  }

  send(fr, NAME_exposed, EAV);		/* doesn't appear to generate a */
					/* CirculateNotify event */
}


void
ws_lower_frame(FrameObj fr)
{ Widget w = widgetFrame(fr);
  DisplayWsXref r = fr->display->ws_ref;
  
  if ( w )
    XLowerWindow(r->display_xref, XtWindow(w));
}


		 /*******************************
		 *	   WM-PROTOCOL		*
		 *******************************/

status
ws_attach_wm_prototols_frame(FrameObj fr)
{ Atom *pr = (Atom *)alloca(valInt(getSizeChain(fr->wm_protocols->attributes))
			    * sizeof(Atom));
  Cell cell;
  int n = 0;
  DisplayWsXref r = fr->display->ws_ref;

  for_cell(cell, fr->wm_protocols->attributes)
  { Attribute a = cell->value;
    Name name = checkType(a->name, TypeName, fr);

    if ( name != FAIL )
      pr[n++] = FrameAtom(fr, name);
  }

  DEBUG(NAME_frame, Cprintf("Attaching WM_PROTOCOLS\n"));

  XSetWMProtocols(r->display_xref, XtWindow(widgetFrame(fr)), pr, n);

  assign(fr, wm_protocols_attached, ON);

  succeed;
}


#ifdef O_XDND
		 /*******************************
		 *	   DRAG-AND-DROP	*
		 *******************************/

struct xdnd_get_drop_info
{ FrameObj frame;			/* accepting frame */
  Window frameWindow;			/* X window of the frame */
  Window root;				/* XPCE root window */
  PceWindow window;			/* client window */
  unsigned char *drop_data;		/* raw drop-data */
  int drop_data_length;			/* length of this */
  int x, y;				/* position of the drop */
  int dropfile;				/* dropping a file */
  Atom XdndTextUriList;			/* text/uri */
  Atom return_type;			/* selected type */
  Atom return_action;			/* selected action */
  Atom *typelist;			/* accepted types */
  Atom *actionlist;			/* accepted actions */
};


static DndClass *
getDndDisplay(DisplayObj display)
{ DisplayWsXref wsref = display->ws_ref;

  if ( !wsref->dnd )
  { wsref->dnd = alloc(sizeof(DndClass));

    xdnd_init(wsref->dnd, wsref->display_xref);
    wsref->XdndTextUriList = XInternAtom(wsref->display_xref,
					 "text/uri-list", False);
  }

  return wsref->dnd;
}


status
setDndAwareFrame(FrameObj fr)
{ Window w = XtWindow(widgetFrame(fr));

  if ( w )
  { DEBUG(NAME_dnd, Cprintf("Registered %s for drag-and-drop\n", pp(fr)));
    xdnd_set_dnd_aware(getDndDisplay(fr->display), w, NULL);
  }
  
  succeed;
} 


static int
widget_insert_drop(DndClass * dnd, unsigned char *data,
		   int length, int remaining,
		   Window into, Window from, Atom type)
{ struct xdnd_get_drop_info *i;

  i = (struct xdnd_get_drop_info *) dnd->user_hook1;

  if (!i->drop_data)
  { i->drop_data = pceMalloc(length);
    if (!i->drop_data)
      return 1;
    memcpy(i->drop_data, data, length);
    i->drop_data_length = length;
  } else
  { unsigned char *t;
    t = pceMalloc(i->drop_data_length + length);
    if (!t)
    { pceFree(i->drop_data);
      i->drop_data = 0;
      return 1;
    }
    memcpy(t, i->drop_data, i->drop_data_length);
    memcpy(t + i->drop_data_length, data, length);
    pceFree(i->drop_data);
    i->drop_data = t;
    i->drop_data_length += length;
  }

  return 0;
}


static int
memberAtomList(Atom a, Atom *list)
{ int i;

  for(i=0; list[i]; i++)
  { if ( list[i] == a )
      succeed;
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x,y are positions in display coordinate-system.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
widget_apply_position(DndClass *dnd, Window widgets_window, Window from,
		      Atom action, int x, int y, Time t, Atom *typelist,
		      int *want_position,
		      Atom *supported_action_return,
		      Atom *desired_type,
		      XRectangle *rectangle)
{ struct xdnd_get_drop_info *info;
  PceWindow target = NIL;

  info = (struct xdnd_get_drop_info *) dnd->user_hook1;

  { DisplayWsXref r = info->frame->display->ws_ref;
    int dx, dy;
    Window child;

    XTranslateCoordinates(r->display_xref,
			  info->root, info->frameWindow,
			  x, y,
			  &dx, &dy, &child);
    if ( child != None &&
	 (target = getMemberHashTable(WindowTable, (Any) child)))
    { if ( instanceOfObject(target, ClassWindowDecorator) )
      { XTranslateCoordinates(r->display_xref, info->root, child,
			      x, y, &dx, &dy,
			      &child);
	if ( child != None )
	  target = getMemberHashTable(WindowTable, (Any) child);
	else
	  target = NIL;
      }
    }
  }

  if ( !target || !hasSendMethodObject(target, NAME_dropFiles) )
    return 0;
  if ( typelist &&
       !memberAtomList(info->XdndTextUriList, typelist) )
    return 0;
  if ( action != dnd->XdndActionCopy )
    return 0;

  *want_position = 1;
  *desired_type = info->XdndTextUriList;
  rectangle->x = rectangle->y = 0;
  rectangle->width = rectangle->height = 0;

  info->window = target;
  info->dropfile = TRUE;
  info->x = x;
  info->y = y;
  
  return 1;
}


static int
dndEventFrame(FrameObj fr, XEvent *xevent)
{ DndClass *dnd = getDndDisplay(fr->display);

  if ( xevent->type == ClientMessage &&
       xevent->xclient.message_type == dnd->XdndEnter)
  { struct xdnd_get_drop_info i;
    DisplayWsXref r = (DisplayWsXref)fr->display->ws_ref;
    XWindowAttributes atts;
    
    XGetWindowAttributes(r->display_xref, XtWindow(r->shell_xref), &atts);

    memset(&i, 0, sizeof(i));
    dnd->user_hook1   =	&i;
    i.frame	      = fr;
    i.root	      =	atts.root;
    i.frameWindow     =	XtWindow(widgetFrame(fr));
    i.XdndTextUriList =	r->XdndTextUriList;

    dnd->widget_insert_drop    = widget_insert_drop;
    dnd->widget_apply_position = widget_apply_position;

    for (;;)
    { xdnd_handle_drop_events(dnd, xevent);
      if ( dnd->stage == XDND_DROP_STAGE_IDLE )
	break;
      XNextEvent(dnd->display, xevent);
    }

    if ( i.dropfile )
    { DEBUG(NAME_dnd,
	    Cprintf("%s: got drop-file at %d,%d: %s\n",
		    pp(i.window), i.x, i.y,
		    i.drop_data));

      ServiceMode(is_service_window(i.window),
		  { AnswerMark mark;
		    Chain files;
		    Point pos;
		    unsigned char *s = i.drop_data;
		    unsigned char *e = s + i.drop_data_length;

		    markAnswerStack(mark);
		    files = answerObject(ClassChain, EAV);
		    pos   = answerObject(ClassPoint,
					 toInt(i.x), toInt(i.y), EAV);
		    
		    for(; s<e; )
		    { unsigned char *start;
		      string str;

		      start = s;
		      while(s<e && !(*s == '\r' || *s == '\n'))
			s++;
		      str_inithdr(&str, ENC_ASCII);

		      if ( e-start > 5 && strncmp(start, "file:", 5) == 0 )
			start += 5;

		      str.size = s-start;
		      str.s_text = start;
		      appendChain(files, StringToName(&str));
		      while(s<e && (*s == '\r' || *s == '\n'))
			s++;
		    }

		    pceFree(i.drop_data);

		    send(i.window, NAME_dropFiles, files, pos, EAV);
		    RedrawDisplayManager(TheDisplayManager());
		    rewindAnswerStack(mark, NIL);
		  });
    }

    succeed;
  }

  fail;
}

#endif /*O_XDND*/

		 /*******************************
		 *     XT-CALLBACK HANDLING	*
		 *******************************/

static int
service_frame(FrameObj fr)
{ Application app = fr->application;

  DEBUG(NAME_service, Cprintf("Event on %s, app %s, kind %s\n",
			      pp(fr), pp(app),
			      notNil(app) ? pp(app->kind) : "-"));

  return (notNil(app) && app->kind == NAME_service ? PCE_EXEC_SERVICE
						   : PCE_EXEC_USER);
}


static void
destroyFrame(Widget w, FrameObj fr, XtPointer data)
{ if ( fr->ws_ref )
  { unalloc(sizeof(frame_ws_ref), fr->ws_ref);
    fr->ws_ref = NULL;
  }

  ServiceMode(service_frame(fr),
	      freeObject(fr));
}


static void
x_event_frame(Widget w, FrameObj fr, XEvent *event)
{ 

#ifdef O_XDND
  if ( dndEventFrame(fr, event) )
    return;
#endif /*O_XDND*/

  switch( event->xany.type )
  { case ClientMessage:
    { DEBUG(NAME_frame, Cprintf("Received client message\n"));
					/* Window Manager Request */
      if ( event->xclient.message_type == WmProtocols(fr) )
      { Name name;
	Code msg;

	DEBUG(NAME_frame,
	      Cprintf("Protocol message %s\n",
		      FrameAtomToString(fr, event->xclient.data.l[0])));

	name = CtoName(FrameAtomToString(fr, event->xclient.data.l[0]));
	if ( (msg = checkType(getValueSheet(fr->wm_protocols, name),
			      TypeCode, fr)) )
	  forwardReceiverCode(msg, fr, MainWindow(fr), EAV);
      }
      return;
    }
    case ConfigureNotify:
    { updateAreaFrame(fr, toInt(event->xconfigure.border_width));
      return;
    }
    case PropertyNotify:
      if ( fr->wm_protocols_attached == OFF &&
	   event->xproperty.atom == WmProtocols(fr) )
      { if ( fr->kind != NAME_popup )
	  ws_attach_wm_prototols_frame(fr);
      }
      return;
    case MapNotify:
    { Cell cell;

      for_cell(cell, fr->members)
	send(cell->value, NAME_displayed, ON, EAV);
      updateAreaFrame(fr, DEFAULT);
      send(fr, NAME_mapped, ON, EAV);
      assign(fr, status, NAME_window);
      return;
    }
    case UnmapNotify:
    { Cell cell;

      for_cell(cell, fr->members)
	send(cell->value, NAME_displayed, OFF, EAV);
      if ( !isFreedObj(fr) || isFreeingObj(fr) )
	send(fr, NAME_mapped, OFF, EAV);
      assign(fr, status, NAME_hidden);
      return;
    }
    case CirculateNotify:
      if ( event->xcirculate.place == PlaceOnTop )
      	send(fr, NAME_exposed, EAV);
      else
	send(fr, NAME_hidden, EAV);
      return;
    case FocusIn:
#if 0
    { FrameObj fr2;

      if ( (fr2=blockedByModalFrame(fr)) )
      { PceWindow iw;
	DisplayWsXref r;

	if ( (iw = getKeyboardFocusFrame(fr2)) )
	{ ws_grab_keyboard_window(iw, ON);
	} else if ( (r=fr2->display->ws_ref) )
	{ XSetInputFocus(r->display_xref, XtWindow(w),
			 RevertToPointerRoot,
			 CurrentTime);
	}
	return;
      }
    }
#endif

      send(fr, NAME_inputFocus, ON, EAV);
      return;
    case FocusOut:
      send(fr, NAME_inputFocus, OFF, EAV);
      return;
    case KeyPress:
    { EventObj ev;
      PceWindow sw = getKeyboardFocusFrame(fr);

      if ( sw && (ev = CtoEvent(sw, event)) )
      { addCodeReference(ev);
	postEvent(ev, (Graphical) sw, DEFAULT);
	delCodeReference(ev);
	freeableObj(ev);
      }

      return;
    }
    default:
    { EventObj ev;
      AnswerMark mark;
      markAnswerStack(mark);
  
      if ( (ev = CtoEvent(fr, event)) )
      { addCodeReference(ev);
	send(fr, NAME_event, ev, EAV);
	delCodeReference(ev);
	freeableObj(ev);
      }

      rewindAnswerStack(mark, NIL);
      
      return;
    }
  }
}


static void
xEventFrame(Widget w, FrameObj fr, XEvent *event)
{ ServiceMode(service_frame(fr),
	      x_event_frame(w, fr, event));
}


static void
expose_frame(Widget w, FrameObj fr, Region region)
{ XRectangle rect;

  XClipBox(region, &rect);
  DEBUG(NAME_frame, Cprintf("expose_frame(%s, %d,%d,%d,%d)\n",
			    pp(fr), rect.x, rect.y, rect.width, rect.height));

  ServiceMode(service_frame(fr),
	      { Area a;

		a = tempObject(ClassArea, toInt(rect.x), toInt(rect.y),
			       toInt(rect.width), toInt(rect.height), EAV);
		redrawFrame(fr, a);
		considerPreserveObject(a);
	      });
}


		 /*******************************
		 *	      CURSOR		*
		 *******************************/

void
ws_frame_cursor(FrameObj fr, CursorObj cursor)
{ Widget w = widgetFrame(fr);

  if ( w )
  { DisplayWsXref r = fr->display->ws_ref;
    Display *d = r->display_xref;

    XDefineCursor(d,
		  XtWindow(w),
		  !instanceOfObject(cursor, ClassCursor)
		      ? None
		      : (Cursor) getXrefObject(cursor, fr->display));
  }
}


void
ws_grab_frame_pointer(FrameObj fr, Bool grab, CursorObj cursor)
{ Widget w = widgetFrame(fr);

  if ( w )
  { if ( grab == ON )
    { Cursor c = (!instanceOfObject(cursor, ClassCursor)
		      ? None
		      : (Cursor) getXrefObject(cursor, fr->display));

      XtGrabPointer(w,
		    False,
		    ButtonPressMask|ButtonReleaseMask|
		    EnterWindowMask|LeaveWindowMask|
		    PointerMotionMask|ButtonMotionMask,
		    GrabModeAsync, GrabModeAsync,
		    None,
		    c,
		    CurrentTime);
    } else
    { XtUngrabPointer(w, CurrentTime);
    }
  }
}


		 /*******************************
		 *     GEOMETRY MANAGEMENT	*
		 *******************************/

static status
updateAreaFrame(FrameObj fr, Int border)
{ Widget wdg;

  if ( (wdg = widgetFrame(fr)) )
  { DisplayWsXref r = fr->display->ws_ref;
    Display *d = r->display_xref;
    Window me, root, child;
    int x, y;
    unsigned int w, h, bw, depth;
    
    if ( (me = XtWindow(wdg)) )
    { Area a = fr->area;
      Int ow = a->w, oh = a->h;
    
      XGetGeometry(d, me, &root, &x, &y, &w, &h, &bw, &depth);
      XTranslateCoordinates(d, me, root, 0, 0, &x, &y, &child);

      assign(a, x, toInt(x));
      assign(a, y, toInt(y));
      assign(a, w, toInt(w));
      assign(a, h, toInt(h));
      if ( notDefault(border) )
	assign(fr, border, border);

      if ( a->w != ow || a->h != oh )
	send(fr, NAME_resize, EAV);
    }

    succeed;
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Returns the window which  we  believe   is  the  window manager's window
encapsulating out window as well as the offset of the client-area of our
window relative to the real outside  of the window-manager's window. The
latter is used to correct the position if   we send ->geometry to a life
window.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Window
getWMFrameFrame(FrameObj fr, int *dxp, int *dyp)
{ Widget wdg;
  Window w = 0;
  int dx = 0, dy = 0;

  if ( (wdg = widgetFrame(fr)) )
  { DisplayWsXref r = fr->display->ws_ref;
    Display *d = r->display_xref;

    w = XtWindow(wdg);
    if ( fr->kind != NAME_popup )
    { Window root, parent, *children;
      unsigned int nchildren;
      int m = 0;

      while( m++ < 5 )			/* avoid a loop */
      { if ( !XQueryTree(d, w, &root, &parent,
			 &children, &nchildren) )
	  break;
	XFree((char *) children);	/* declared char * ???? */

	if ( dxp || dyp )
	{ unsigned int x, y, width, h, bw, depth;

	  XGetGeometry(d, w, &root, &x, &y, &width, &h, &bw, &depth);

	  dx += bw;
	  dy += bw;

	  if ( parent != root )
	  { dx += x;
	    dy += y;
	  }

	  DEBUG(NAME_frame,
		Cprintf("w = %ld; root = %ld; parent = %ld; "
			"dx=%d; dy=%d; bw = %d\n",
			w, root, parent, dx, dy, bw));
	}

	if ( parent == root )
	  break;

	w = parent;
      }
    }
  }

  if ( dxp )
    *dxp = dx;
  if ( dyp )
    *dyp = dy;

  return w;
}


status
ws_frame_bb(FrameObj fr, int *x, int *y, int *w, int *h)
{ Window win;

  if ( (win = getWMFrameFrame(fr, NULL, NULL)) )
  { DisplayWsXref r = fr->display->ws_ref;
    XWindowAttributes atts;
    int bw = isDefault(fr->border) ? 1 : valInt(fr->border);

    XGetWindowAttributes(r->display_xref, win, &atts);
    *x = atts.x - bw;
    *y = atts.y - bw;
    *w = atts.width + 2*bw;
    *h = atts.height + 2*bw;
    
    succeed;
  }

  fail;
}


void
ws_x_geometry_frame(FrameObj fr, Name spec)
{ Widget wdg = widgetFrame(fr);
  
  DEBUG(NAME_frame, Cprintf("ws_x_geometry_frame(%s, %s)\n",
			    pp(fr), pp(spec)));

  if ( wdg )
  { int x, y, w, h, mask;
    Int X, Y, W, H;
    char def[50];
    Area a = fr->area;
    DisplayWsXref r = fr->display->ws_ref;
    Display *d = r->display_xref;
    Window wm, me = XtWindow(wdg);
    int dx, dy;

    sprintf(def,
	    "%ldx%ld+%ld+%ld",
	    valInt(a->w), valInt(a->h), valInt(a->x), valInt(a->y));

    mask = XGeometry(d, DefaultScreen(d),
		     strName(spec),
		     def,
		     isDefault(fr->border) ? 1 : valInt(fr->border),
		     1, 1,
		     0, 0,
		     &x, &y, &w, &h);
  
    if ( me && (wm = getWMFrameFrame(fr, &dx, &dy)) && me != wm )
    { Window root;
      int mex, mey, wmx, wmy;
      unsigned int mew, meh, wmw, wmh, mebw, wmbw;
      unsigned depth;

      XGetGeometry(d, me, &root, &mex, &mey, &mew, &meh, &mebw, &depth);
      XGetGeometry(d, wm, &root, &wmx, &wmy, &wmw, &wmh, &wmbw, &depth);

      DEBUG(NAME_frame,
	    Cprintf("wmbw %d; mew %d; meh %d; wmw %d; wmh %d; dx %d; dy %d\n",
		    wmbw, mew, meh, wmw, wmh, dx, dy));

      if ( (mask & XNegative) )
	x -= wmw-mew-dx;
      else
	x += dx;
      if ( (mask & YNegative) ) 
	y -= wmh-meh-dy;
      else
	y += dy;
    } else
    { DEBUG(NAME_frame, Cprintf("No WM frame yet\n"));
    }

    switch(mask & (XNegative|YNegative))
    { case 0:
	setGravityFrame(fr, NorthWestGravity);
        break;
      case XNegative|YNegative:
	setGravityFrame(fr, SouthEastGravity);
	break;
      case XNegative:
	setGravityFrame(fr, NorthEastGravity);
	break;
      case YNegative:
	setGravityFrame(fr, SouthWestGravity);
	break;
    }

    X = ( mask & XValue      ? toInt(x) : (Int) DEFAULT );
    Y = ( mask & YValue      ? toInt(y) : (Int) DEFAULT );
    W = ( mask & WidthValue  ? toInt(w) : (Int) DEFAULT );
    H = ( mask & HeightValue ? toInt(h) : (Int) DEFAULT );

    send(fr, NAME_set, X, Y, W, H, EAV);
  }
}


void
ws_geometry_frame(FrameObj fr, Int x, Int y, Int w, Int h)
{ Widget wdg = widgetFrame(fr);

  if ( wdg )
  { XtWidgetGeometry in, out;
    DisplayWsXref r = fr->display->ws_ref;

    in.request_mode = 0;
    if ( notDefault(x) ) in.request_mode |= CWX;
    if ( notDefault(y) ) in.request_mode |= CWY;
    if ( notDefault(w) ) in.request_mode |= CWWidth;
    if ( notDefault(h) ) in.request_mode |= CWHeight;

    in.x      = valInt(fr->area->x);
    in.y      = valInt(fr->area->y);
    in.width  = valInt(fr->area->w);
    in.height = valInt(fr->area->h);

    XtMakeGeometryRequest(wdg, &in, &out);

    if ( fr->kind != NAME_popup )
    { XSizeHints *hints = XAllocSizeHints();
      FrameWsRef wsref  = fr->ws_ref;
      
      if ( notDefault(x) || notDefault(y) ) hints->flags |= USPosition;
      if ( notDefault(w) || notDefault(h) ) hints->flags |= USSize;

      hints->x      = valInt(fr->area->x);
      hints->y      = valInt(fr->area->y);
      hints->width  = valInt(fr->area->w);
      hints->height = valInt(fr->area->h);

      if ( wsref->win_gravity )
      {  hints->win_gravity = wsref->win_gravity;
	 hints->flags |= PWinGravity;
      }

      XSetWMNormalHints(r->display_xref, XtWindow(wdg), hints);
      XFree(hints);
    }
  }
}


void
ws_border_frame(FrameObj fr, int b)
{ Widget w = widgetFrame(fr);

  if ( w )
  { XtWidgetGeometry in, out;
    in.request_mode = CWBorderWidth;
    in.border_width = b;

    XtMakeGeometryRequest(w, &in, &out);
  }
}

		 /*******************************
		 *	     CURSOR		*
		 *******************************/

void
ws_busy_cursor_frame(FrameObj fr, CursorObj c)
{ Widget w = widgetFrame(fr);
  DisplayWsXref r = fr->display->ws_ref;

  if ( !w )
    return;

#define BlockAllMask (KeyPressMask | KeyReleaseMask | \
		      ButtonPressMask | ButtonReleaseMask | \
		      PointerMotionMask)

  if ( !busyWindowFrame(fr) && notNil(c) )
  { unsigned long valuemask = CWCursor;
    XSetWindowAttributes attributes;
    Size size = getSizeDisplay(fr->display);
    Window bw;

    if ( isDefault(c) )
    { if ( !(c = getClassVariableValueObject(fr, NAME_busyCursor)) )
	return;				/* TBD: default? */
      if ( isNil(c) )
	goto out;
    }
    attributes.cursor = (Cursor) getXrefObject(c, fr->display);

    bw = XCreateWindow(r->display_xref,
		       XtWindow(widgetFrame(fr)), 0, 0,
		       valInt(size->w), valInt(size->h),
		       (unsigned int) 0, 0, InputOnly,
		       CopyFromParent, valuemask, &attributes);

    if ( bw )
      setBusyWindowFrame(fr, bw);
    else
      errorPce(fr, NAME_failedToCreate, 0); /* TBD */
  } else if ( busyWindowFrame(fr) && instanceOfObject(c, ClassCursor) )
  { unsigned long valuemask = 0L;
    XSetWindowAttributes attributes;

    if ( notDefault(c) )
    { valuemask |= CWCursor;
      attributes.cursor = (Cursor) getXrefObject(c, fr->display);
    }

    XChangeWindowAttributes(r->display_xref,
			    busyWindowFrame(fr),
			    valuemask, &attributes);
  }

out:
  if ( notNil(c) )
    XMapRaised(r->display_xref, busyWindowFrame(fr));
  else if ( busyWindowFrame(fr) )
    XUnmapWindow(r->display_xref, busyWindowFrame(fr));
}

		 /*******************************
		 *	      COLOUR		*
		 *******************************/

void
ws_frame_background(FrameObj fr, Any c)
{ Widget w = widgetFrame(fr);

  if ( w )
  { Arg args[2];
    DisplayObj d = fr->display;
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
		 *	      ICONS		*
		 *******************************/

void
ws_set_icon_frame(FrameObj fr)
{ Widget w = widgetFrame(fr);

  if ( w )
  { Arg args[3];
    int n=0;

    XtSetArg(args[n], XtNiconPixmap,
	     getXrefObject(fr->icon_image, fr->display));
    n++;
    if ( notNil(fr->icon_image->mask) )
    { XtSetArg(args[n], XtNiconMask,
	       getXrefObject(fr->icon_image->mask, fr->display));
      n++;
    }
    XtSetArg(args[n], XtNiconName,
	     strName(getIconLabelFrame(fr)));
    n++;

    XtSetValues(w, args, n);
  }
}


void
ws_set_icon_label_frame(FrameObj fr)
{ Widget w = widgetFrame(fr);

  if ( w )
  { Arg args[1];

    XtSetArg(args[0], XtNiconName,
	     strName(getIconLabelFrame(fr)));

    XtSetValues(w, args, 1);
  }
}


void
ws_set_icon_position_frame(FrameObj fr, int x, int y)
{ Widget w = widgetFrame(fr);
  DisplayWsXref r = fr->display->ws_ref;

  if ( w )
  { XWMHints hints;

    hints.flags = IconPositionHint;
    hints.icon_x = x;
    hints.icon_y = y;

    XSetWMHints(r->display_xref, XtWindow(w), &hints);
  }
}


status
ws_get_icon_position_frame(FrameObj fr, int *x, int *y)
{ Widget w = widgetFrame(fr);
  DisplayWsXref r = fr->display->ws_ref;

  if ( w )
  { XWMHints *hints = XGetWMHints(r->display_xref, XtWindow(w));

    if ( hints )
    { *x = hints->icon_x;
      *y = hints->icon_y;
      XFree((void *)hints);

      succeed;
    }
  }

  fail;
}


static void
ws_enable_frame(FrameObj fr, Bool val)
{ Widget w;

  if ( (w = widgetFrame(fr)) )
  { Arg args[1];

    XtSetArg(args[0], XtNinput, val == ON ? True : False);
    XtSetValues(w, args, 1);
  }
}


void
ws_enable_modal(FrameObj fr, Bool val)
{ if ( fr->modal == NAME_transient && notNil(fr->transient_for) )
  { ws_enable_frame(fr->transient_for, val);
  } else if ( fr->modal == NAME_application && notNil(fr->application) )
  { Cell cell;

    for_cell(cell, fr->application->members)
      ws_enable_frame(cell->value, val);
  }
}


void
ws_status_frame(FrameObj fr, Name status)
{ Widget w = widgetFrame(fr);

  if ( status == NAME_window || status == NAME_fullScreen )
  { if ( w )
      XtPopup(w, XtGrabNone);
    ws_enable_modal(fr, OFF);
  } else
  { if ( status == NAME_iconic )
    { if ( w )
      { Arg args[1];
	XtSetArg(args[0], XtNiconic, True);
	XtSetValues(w, args, 1);
      }
    } else if ( status == NAME_hidden )
    { if ( w )
	XtPopdown(w);
    }
    ws_enable_modal(fr, ON);
  }
}


void
ws_topmost_frame(FrameObj fr, Bool topmost)
{ 
}


		 /*******************************
		 *	       LABEL		*
		 *******************************/

void
ws_set_label_frame(FrameObj fr)
{ Widget w = widgetFrame(fr);
  DisplayWsXref r = fr->display->ws_ref;

  if ( w )
    XStoreName(r->display_xref, XtWindow(w), strName(fr->label));
}



		 /*******************************
		 *	   MISCELLANEOUS	*
		 *******************************/

Image
ws_image_of_frame(FrameObj fr)
{ Window win;

  if ( (win = getWMFrameFrame(fr, NULL, NULL)) )
  { Window root, child;
    DisplayWsXref r = fr->display->ws_ref;
    Display *d = r->display_xref;
    int x, y;
    unsigned int w, h, bw, depth;
    Image im;
    XImage *ix;

    XGetGeometry(d, win, &root, &x, &y, &w, &h, &bw, &depth);
    XTranslateCoordinates(d, win, root, 0, 0, &x, &y, &child);
    if ( notDefault(fr->border) )
      bw = valInt(fr->border);
    
    TRY(im = answerObject(ClassImage, NIL,
			  toInt(w+2*bw), toInt(h+2*bw), NAME_pixmap, EAV));
    
    ix = XGetImage(d, root,
		   x-bw, y-bw, w+2*bw, h+2*bw, AllPlanes, ZPixmap);
    setXImageImage(im, ix);
    assign(im, depth, toInt(ix->depth));
    answer(im);
  }
  
  fail;
}


void
ws_transient_frame(FrameObj fr, FrameObj fr2)
{ Widget w1 = widgetFrame(fr);
  Widget w2 = widgetFrame(fr2);
  

  if ( w1 && w2 )
  { DisplayWsXref r = fr->display->ws_ref;

    XSetTransientForHint(r->display_xref,
			 XtWindow(w1),
			 XtWindow(w2));
  }
}

static int
psdepthXImage(XImage *im)
{ if ( im->depth < 3 )			/* 1, 2 */
    return im->depth;
  if ( im->depth < 8 )
    return 4;
  return 8;
}


  
status
ws_postscript_frame(FrameObj fr)
{ Window win;

  if ( (win = getWMFrameFrame(fr, NULL, NULL)) )
  { Window root, child;
    DisplayWsXref r = fr->display->ws_ref;
    Display *d = r->display_xref;
    int x, y;
    unsigned int w, h, bw, depth;
    XImage *im;
    int iw, ih;
    XWindowAttributes atts;

    XGetGeometry(d, win, &root, &x, &y, &w, &h, &bw, &depth);
    XTranslateCoordinates(d, win, root, 0, 0, &x, &y, &child);
    XGetWindowAttributes(d, root, &atts);
    
    if ( notDefault(fr->border) )
      bw = valInt(fr->border);
    
    iw = w+2*bw; ih = h+2*bw;		/* include the frame-border */
    x -= bw; y -= bw;
    if ( x < 0 ) { iw += x; x = 0; }	/* clip to the display */
    if ( y < 0 ) { ih += y; y = 0; }
    if ( x + iw > atts.width )  iw = atts.width - x;
    if ( y + ih > atts.height ) ih = atts.height - y;

    DEBUG(NAME_postscript, Cprintf("frame at %d %d %d %d\n", x, y, iw, ih));

    im = XGetImage(d, root, x, y, iw, ih, AllPlanes, ZPixmap);
    
    ps_output("0 0 ~D ~D ~D greymap\n", iw, ih, psdepthXImage(im));
    postscriptXImage(im, 0, 0, iw, ih,
		     r->display_xref, r->colour_map, 0);
    ps_output("\n");

    XDestroyImage(im);
    succeed;
  }
  
  return errorPce(fr, NAME_mustBeOpenBeforePostscript);
}
