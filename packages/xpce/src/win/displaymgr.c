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
  { realiseClass(ClassDisplay);
    
    if ( emptyChain(dm->current) )
    { errorPce(dm, NAME_noCurrentDisplay);
      fail;
    }
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

#define TestBreakDraw(dm) if ( eventQueuedDisplayManager(dm) ) fail;

status
RedrawDisplayManager(DisplayManager dm)
{ if ( ChangedWindows && !emptyChain(ChangedWindows) )
  { PceWindow sw = WindowOfLastEvent();

    TestBreakDraw(dm);
    if ( sw && memberChain(ChangedWindows, sw) )
      RedrawWindow(sw);

    while( !emptyChain(ChangedWindows) )
    { TestBreakDraw(dm);

      for_chain(ChangedWindows, sw,
		{ if ( !instanceOfObject(sw, ClassWindowDecorator) )
		    RedrawWindow(sw);
		});

      TestBreakDraw(dm);

      for_chain(ChangedWindows, sw,
		{ if ( instanceOfObject(sw, ClassWindowDecorator) )
		    RedrawWindow(sw);
		});
    }
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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_dispatch[] =
        { "file_descriptor=[int]", "milliseconds=[int]*" };

/* Instance Variables */

static vardecl var_displayManager[] =
{ IV(NAME_members, "chain", IV_GET,
     NAME_display, "Available displays"),
  IV(NAME_current, "chain", IV_NONE,
     NAME_current, "Stack with current displays")
};

/* Send Methods */

static senddecl send_displayManager[] =
{ SM(NAME_initialise, 0, NULL, initialiseDisplayManager,
     DEFAULT, "Create the display manager"),
  SM(NAME_current, 1, "display", currentDisplayManager,
     NAME_current, "Make display the current display"),
  SM(NAME_popCurrent, 0, NULL, popCurrentDisplayManager,
     NAME_current, "Pop the current stack"),
  SM(NAME_append, 1, "display", appendDisplayManager,
     NAME_display, "Attach a new display to the manager"),
  SM(NAME_dispatch, 2, T_dispatch, dispatchDisplayManager,
     NAME_event, "Dispatch events for 1/4th second")
};

/* Get Methods */

static getdecl get_displayManager[] =
{ GM(NAME_contains, 0, "chain", NULL, getContainsDisplayManager,
     DEFAULT, "Contained displays"),
  GM(NAME_current, 0, "display", NULL, getCurrentDisplayManager,
     NAME_current, "Get the current display"),
  GM(NAME_member, 1, "display", "name", getMemberDisplayManager,
     NAME_display, "Find display for specified address")
};

/* Resources */

#define rc_displayManager NULL
/*
static resourcedecl rc_displayManager[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(displayManager_decls,
          var_displayManager, send_displayManager,
	  get_displayManager, rc_displayManager,
          0, NULL,
          "$Rev$");


status
makeClassDisplayManager(Class class)
{ declareClass(class, &displayManager_decls);

  globalObject(NAME_displayManager, ClassDisplayManager, 0);
  DispatchEvents = dispatch_events;

  succeed;
}

