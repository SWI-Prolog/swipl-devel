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
#include <sys/time.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef FD_ZERO
#include <sys/select.h>
#endif
#ifdef HAVE_BSTRING_H
#include <bstring.h>
#endif

#define MAX_DECORATION_NESTING	4

		/********************************
		*       EVENT DISPATCHING	*
		********************************/

void
resetDispatch()
{ 
}


static void
is_pending(XtPointer ctx, int *source, XtInputId *id)
{ 
}

static void
is_timeout(XtPointer ctx, XtIntervalId *id)
{ 
}

#ifndef FD_ZERO
#define FD_ZERO(x)	{(x)->fds_bits[0] = 0;}
#define FD_SET(n, x)	{(x)->fds_bits[0] |= 1<<(n); }
#endif

static int	  dispatch_fd = -1;
static XtInputId  in_id;

status
ws_dispatch(Int FD, Int timeout)
{ XtIntervalId tid;
  int fd = (isDefault(FD) ? dispatch_fd : valInt(FD));

					/* No context: wait for input */
					/* timeout */
  if ( ThePceXtAppContext == NULL )
  { struct timeval timeout;
    fd_set readfds;

    timeout.tv_sec = 0;
    timeout.tv_usec = 250000;

    FD_ZERO(&readfds);
    if ( fd >= 0 )
      FD_SET(fd, &readfds);
    if ( select(fd+1, &readfds, NULL, NULL, &timeout) > 0 )
      succeed;
    else
      fail;
  }					/* A display: dispatch until there */
					/* is input or a timeout */

  if ( fd != dispatch_fd )
  { if ( in_id )
    { XtRemoveInput(in_id);
      in_id = 0;
    }

    if ( fd >= 0 )
    { in_id = XtAppAddInput(ThePceXtAppContext, fd,
			    (XtPointer) XtInputReadMask, is_pending, NULL);
      dispatch_fd = fd;
    }
  }

  if ( notNil(timeout) && valInt(timeout) > 0 )
    tid = XtAppAddTimeOut(ThePceXtAppContext, valInt(timeout),
			  is_timeout, NULL);
  else
    tid = 0;

  DEBUG(NAME_dispatch, Cprintf("Dispatch: tid = %d\n", tid));

  pceMTLock(LOCK_PCE);
  RedrawDisplayManager(TheDisplayManager());
  XtAppProcessEvent(ThePceXtAppContext,
		    XtIMXEvent|XtIMTimer|XtIMAlternateInput);
  pceMTUnlock(LOCK_PCE);
  if ( tid )
    XtRemoveTimeOut(tid);

  considerLocStillEvent();

  succeed;
}


static int
input_on_fd(int fd)
{ fd_set rfds;
  struct timeval tv;

  FD_ZERO(&rfds);
  FD_SET(fd, &rfds);
  tv.tv_sec = 0;
  tv.tv_usec = 0;

  return select(fd+1, &rfds, NULL, NULL, &tv) != 0;
}


void
ws_discard_input(const char *msg)
{ if ( dispatch_fd >= 0 && input_on_fd(dispatch_fd) )
  { char buf[1024];

    Cprintf("%s; discarding input ...", msg);
    read(dispatch_fd, buf, sizeof(buf));
    Cprintf("ok\n");
  }
}


		 /*******************************
		 *     WINDOW TRANSLATIONS	*
		 *******************************/

Any
ws_event_in_subwindow(EventObj ev, Any root)
{ DisplayObj d = getDisplayEvent(ev);
  DisplayWsXref r = d->ws_ref;
  Window src_w = XtWindow(widgetWindow(ev->window));
  int dx, dy;
  Window child;
  int root_is_display;
  
  if ( isDefault(root) )
    root = d;

  if ( (root_is_display = instanceOfObject(root, ClassDisplay)) )
  { XWindowAttributes atts;
    int depth = MAX_DECORATION_NESTING;

    if ( d != root )
    { errorPce(ev, NAME_notSameDisplay, root);
      fail;
    }

    XGetWindowAttributes(r->display_xref, XtWindow(r->shell_xref), &atts);
    XTranslateCoordinates(r->display_xref, src_w, atts.root,
			  valInt(ev->x), valInt(ev->y),
			  &dx, &dy, &child);

#if 0
    DEBUG(NAME_pointer,
					/* TEST STUFF */
	  ({ Window rr, cr;
	    int rx, ry, wx, wy, mask;
	    
	    if ( XQueryPointer(r->display_xref, atts.root, &rr, &cr,
			       &rx, &ry, &wx, &wy, &mask) )
	    { Cprintf("XTranslateCoordinates --> %d\nXQueryPointer --> %d\n",
		      child, cr);
	    }
	  }));
#endif

    while ( child != None && depth-- > 0 )
    { Cell cell;

      for_cell(cell, d->frames)
      { FrameObj fr = cell->value;
	Widget w;

	if ( (w=widgetFrame(fr)) && child == XtWindow(w) )
	  answer(fr);
      }
      
      XTranslateCoordinates(r->display_xref, src_w, child,
			    valInt(ev->x), valInt(ev->y),
			    &dx, &dy, &child);
    }

    fail;
  }

  if ( instanceOfObject(root, ClassFrame) )
  { FrameObj fr = root;
    PceWindow sw;

    XTranslateCoordinates(r->display_xref, src_w, XtWindow(widgetFrame(fr)),
			  valInt(ev->x), valInt(ev->y),
			  &dx, &dy, &child);
    if ( child != None && (sw = getMemberHashTable(WindowTable, (Any) child)))
    { if ( instanceOfObject(sw, ClassWindowDecorator) )
      { XTranslateCoordinates(r->display_xref, src_w, child,
			      valInt(ev->x), valInt(ev->y), &dx, &dy,
			      &child);

	if ( child != None )
	  answer(getMemberHashTable(WindowTable, (Any) child));
      }
      answer(sw);
    }
  } else /*if ( instanceOfObject(root, ClassWindow) )*/
  { PceWindow sw = root;

    XTranslateCoordinates(r->display_xref, src_w, XtWindow(widgetWindow(sw)),
			  valInt(ev->x), valInt(ev->y),
			  &dx, &dy, &child);
    if ( child != None )
      answer(getMemberHashTable(WindowTable, (Any) child));
  }

  fail;
}
