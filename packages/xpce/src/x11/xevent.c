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
{ status *p = (status *)ctx;

  *p = FAIL;
}

#ifndef FD_ZERO
#define FD_ZERO(x)	{(x)->fds_bits[0] = 0;}
#define FD_SET(n, x)	{(x)->fds_bits[0] |= 1<<(n); }
#endif

static int	  dispatch_fd = -1;

status
ws_dispatch(Int FD, Any timeout)
{ XtIntervalId tid = 0;
  XtInputId iid = 0;
  status rval = SUCCEED;
  int ofd = dispatch_fd;
  int fd = (isDefault(FD) ? dispatch_fd : 
	    isNil(FD)	  ? -1
	    		  : valInt(FD));

					/* No context: wait for input */
					/* timeout */
  if ( ThePceXtAppContext == NULL )
  { struct timeval to;
    struct timeval *tp = &to;
    fd_set readfds;
    int setmax = 0;
    int ready;

    if ( isNil(timeout) )
    { tp = NULL;
    } else if ( isDefault(timeout) )
    { to.tv_sec = 0;
      to.tv_usec = 250000;
    } else if ( isInteger(timeout) )
    { to.tv_sec  = valInt(timeout) / 1000;
      to.tv_usec = valInt(timeout) % 1000;
    } else /* if ( isReal(timeout) ) */
    { double v = valReal(timeout);

      to.tv_sec  = (long)v;
      to.tv_usec = (long)(v * 1000000.0) % 1000000;
    }			 

    FD_ZERO(&readfds);
    if ( fd >= 0 )
    { FD_SET(fd, &readfds);
      setmax = max(setmax, fd);
      dispatch_fd = fd;
    }

    ready = select(setmax+1, &readfds, NULL, NULL, tp);
    dispatch_fd = ofd;

    return (ready > 0 ? SUCCEED : FAIL);
  }					/* A display: dispatch until there */
					/* is input or a timeout */

  if ( fd >= 0 )
  { iid = XtAppAddInput(ThePceXtAppContext, fd,
			(XtPointer) XtInputReadMask, is_pending, NULL);
    dispatch_fd = fd;
  }

  if ( notNil(timeout) )
  { long to = -1;

    if ( isInteger(timeout) )
      to = valInt(timeout);
    else if ( instanceOfObject(timeout, ClassReal) )
      to = (long)(valReal(timeout)*1000.0);

    if ( to > 0 )
      tid = XtAppAddTimeOut(ThePceXtAppContext, to, is_timeout,
			    (XtPointer) &rval);
  }

  DEBUG(NAME_dispatch, Cprintf("Dispatch: timeout = %s, tid = %d\n",
			       pp(timeout), tid));

  pceMTLock(LOCK_PCE);
  RedrawDisplayManager(TheDisplayManager());
  pceMTUnlock(LOCK_PCE);
					/* All callbacks must be locked! */
  XtAppProcessEvent(ThePceXtAppContext,
		    XtIMXEvent|XtIMTimer|XtIMAlternateInput);

  if ( tid && rval )			/* if rval = FAIL, we had a timeout */
    XtRemoveTimeOut(tid);
  if ( iid )
    XtRemoveInput(iid);
  dispatch_fd = ofd;

  considerLocStillEvent();

  return rval;
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We would like to wait for at   most  0.25 seconds to distinguish between
C-x  (cut)  and  C-xC-x  (exchange-point-and-mark)    commands  for  the
emulation of the  CUA  mode.  This  isn't   really  ideal  as  it  waits
unconditionally  without  handling  any  messages.   We  inprove  a  bit
splitting it into a couple of shorter waits.

XCheckIfEvent() removes the  matching  event.   Hence  we  always return
FALSE, but set a flag if we find the target event.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
is_key_event(Display *dsp, XEvent *ev, XPointer arg)
{ if ( ev->xany.type == KeyPress )
  { int *p = (int *)arg;

    *p = TRUE;
  }

  return FALSE;
}


int
key_waiting(DisplayObj d)
{ DisplayWsXref r = d->ws_ref;
  int waiting = FALSE;
  XEvent event;

  XCheckIfEvent(r->display_xref, &event, is_key_event, (XPointer) &waiting);

  return waiting;
}


int
ws_wait_for_key(int maxwait)
{ msleep(maxwait);
  
  return key_waiting(CurrentDisplay(NIL));
}
