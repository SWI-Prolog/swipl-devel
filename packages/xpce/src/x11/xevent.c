/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include "include.h"
#include <sys/time.h>

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

#define DISPATCH_RUNNING	0
#define DISPATCH_INPUT		1
#define DISPATCH_TIMEOUT	2

static int* dispatch_status_address;

static void
is_pending(XtPointer ctx, int *source, XtInputId *id)
{ if ( dispatch_status_address != NULL )
    *dispatch_status_address = DISPATCH_INPUT;
}

static void
is_timeout(XtPointer ctx, XtIntervalId *id)
{ if ( dispatch_status_address != NULL )
    *dispatch_status_address = DISPATCH_TIMEOUT;
}

#ifndef FD_ZERO
#define FD_ZERO(x)	{(x)->fds_bits[0] = 0;}
#define FD_SET(n, x)	{(x)->fds_bits[0] |= 1<<(n); }
#endif

status
ws_dispatch(Int FD, Int timeout)
{ XtIntervalId tid;
  int dispatch_status = DISPATCH_RUNNING;
  static int dfd = -1;
  int fd = (isDefault(FD) ? dfd : valInt(FD));

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
    if ( select(1, &readfds, NULL, NULL, &timeout) > 0 )
      succeed;
    else
      fail;
  }					/* A display: dispatch until there */
					/* is input or a timeout */

  if ( fd != dfd && fd >= 0 )
  { XtAppAddInput(ThePceXtAppContext, fd,
		  (XtPointer) XtInputReadMask, is_pending, NULL);
    dfd = fd;
  }

  if ( notNil(timeout) && valInt(timeout) > 0 )
    tid = XtAppAddTimeOut(ThePceXtAppContext, valInt(timeout),
			  is_timeout, NULL);
  else
    tid = 0;

  while( dispatch_status == DISPATCH_RUNNING )
  { int *old_dispatch_status_address = dispatch_status_address;

    dispatch_status_address = &dispatch_status;
    RedrawDisplayManager(TheDisplayManager());
    XtAppProcessEvent(ThePceXtAppContext,
		      XtIMXEvent|XtIMTimer|XtIMAlternateInput);
    dispatch_status_address = old_dispatch_status_address;
  }

  considerLocStillEvent();
    
  if ( dispatch_status == DISPATCH_TIMEOUT )
  { fail;
  } else
  { if ( tid )
      XtRemoveTimeOut(tid);
    succeed;
  }
}


void
ws_discard_input(const char *msg)
{ char buf[1024];

  Cprintf("%s; discarding input ...", msg);
  Cgetline(buf, sizeof(buf));
  Cprintf("ok\n");
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
