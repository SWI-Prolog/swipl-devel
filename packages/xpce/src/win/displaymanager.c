/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialiseDisplayManager(DisplayManager dm)
{ assign(dm, members, newObject(ClassChain, 0));
  assign(dm, current, newObject(ClassChain, 0));
  
  protectObject(dm);

  succeed;
}


status
appendDisplayManager(DisplayManager dm, DisplayObj d)
{ appendChain(dm->members, d);
  if ( emptyChain(dm->current) )
    prependChain(dm->current, d);

  succeed;
}


DisplayObj
getMemberDisplayManager(DisplayManager dm, Name address)
{ Cell cell;

  for_cell(cell, dm->members)
  { DisplayObj d = cell->value;

    if ( d->address == address )
      answer(d);
  }

  fail;
}


static status
currentDisplayManager(DisplayManager dm, DisplayObj d)
{ return prependChain(dm->current, d);
}


static DisplayObj
getCurrentDisplayManager(DisplayManager dm)
{ if ( emptyChain(dm->current) )
  { errorPce(dm, NAME_noCurrentDisplay);
    fail;
  }

  answer(dm->current->head->value);
}


static status
popCurrentDisplayManager(DisplayManager dm)
{ if ( getSizeChain(dm->current) == ONE )
    return errorPce(dm, NAME_stackEmpty, NAME_current);

  return deleteHeadChain(dm->current);
}


DisplayObj
CurrentDisplay(Any obj)
{ DisplayObj d;

  if ( instanceOfObject(obj, ClassGraphical) &&
       (d = getDisplayGraphical((Graphical) obj)) )
    return d;

  return getCurrentDisplayManager(TheDisplayManager());
}


static status
eventQueuedDisplayManager(DisplayManager dm)
{ Cell cell;

  for_cell(cell, dm->members)
  { if ( ws_events_queued_display(cell->value) )
      succeed;
  }

  fail;
}

#ifdef __WINDOWS__
#define TestBreakDraw()
#else
#define TestBreakDraw() if ( eventQueuedDisplayManager(dm) ) fail;
#endif

status
RedrawDisplayManager(DisplayManager dm)
{ PceWindow sw = WindowOfLastEvent();

  TestBreakDraw();
  if ( memberChain(ChangedWindows, sw) )
    RedrawWindow(sw);

  while( !emptyChain(ChangedWindows) )
  { TestBreakDraw();

    for_chain(ChangedWindows, sw,
	      { if ( !instanceOfObject(sw, ClassWindowDecorator) )
		  RedrawWindow(sw);
	      });

    TestBreakDraw();

    for_chain(ChangedWindows, sw,
	      { if ( instanceOfObject(sw, ClassWindowDecorator) )
		RedrawWindow(sw);
	      });
  }

  succeed;
}


status
dispatchDisplayManager(DisplayManager dm, Int fd, Int timeout)
{ if ( isDefault(timeout) )
    timeout = toInt(250);

  return ws_dispatch(fd, timeout);
}


static status
dispatch_events(int fd, int timeout)
{ return dispatchDisplayManager(TheDisplayManager(),
				fd >= 0 ? toInt(fd) : NIL,
				toInt(timeout));
}

		/********************************
		*             VISUAL		*
		********************************/

static Chain
getContainsDisplayManager(DisplayManager dm)
{ answer(dm->members);
}


		 /*******************************
		 *	     GLOBAL		*
		 *******************************/

DisplayManager
TheDisplayManager()
{ static DisplayManager dm = NULL;

  if ( !dm )
    dm = findGlobal(NAME_displayManager);

  return dm;
}


status
makeClassDisplayManager(Class class)
{ sourceClass(class, makeClassDisplayManager, __FILE__, "$Revision$");

  localClass(class, NAME_members, NAME_display, "chain", NAME_get,
	     "Available displays");
  localClass(class, NAME_current, NAME_current, "chain", NAME_none,
	     "Stack with current displays");

  termClass(class, "display_manager", 0);

  sendMethod(class, NAME_initialise, DEFAULT, 0,
	     "Create the display manager",
	     initialiseDisplayManager);
  sendMethod(class, NAME_current, NAME_current, 1, "display",
	     "Make display the current display",
	     currentDisplayManager);
  sendMethod(class, NAME_popCurrent, NAME_current, 0,
	     "Pop the current stack",
	     popCurrentDisplayManager);
  sendMethod(class, NAME_append, NAME_display, 1, "display",
	     "Attach a new display to the manager",
	     appendDisplayManager);
  sendMethod(class, NAME_dispatch, NAME_event, 2,
	     "file_descriptor=[int]", "milliseconds=[int]*",
	     "Dispatch events for 1/4th second",
	     dispatchDisplayManager);

  getMethod(class, NAME_current, NAME_current, "display", 0,
	    "Get the current display",
	    getCurrentDisplayManager);
  getMethod(class, NAME_member, NAME_display, "display", 1, "name",
	    "Find display for specified address",
	    getMemberDisplayManager);
  getMethod(class, NAME_contains, DEFAULT, "chain", 0,
	    "Contained displays",
	    getContainsDisplayManager);

  globalObject(NAME_displayManager, ClassDisplayManager, 0);
  DispatchEvents = dispatch_events;
  realiseClass(ClassDisplay);

  succeed;
}

