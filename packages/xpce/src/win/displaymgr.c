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

static status
initialiseDisplayManager(DisplayManager dm)
{ assign(dm, members, newObject(ClassChain, EAV));
  assign(dm, current, newObject(ClassChain, EAV));
  
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

#define TestBreakDraw(dm) \
	if ( dm->test_queue == ON && \
	     eventQueuedDisplayManager(dm) ) \
	  fail;

static status
redrawDisplayManager(DisplayManager dm)
{ if ( ChangedWindows && !emptyChain(ChangedWindows) )
  { PceWindow sw = WindowOfLastEvent();

    obtainClassVariablesObject(dm);

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
RedrawDisplayManager(DisplayManager dm)
{ return sendv(dm, NAME_redraw, 0, NULL);
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
     NAME_current, "Stack with current displays"),
  IV(NAME_testQueue, "bool", IV_BOTH,
     NAME_event, "Test queue in event-loop")
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
     NAME_event, "Dispatch events for 1/4th second"),
  SM(NAME_redraw, 0, NULL, redrawDisplayManager,
     NAME_event, "Flush all pending changes to the screen")
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

static classvardecl rc_displayManager[] =
{ RC(NAME_testQueue, "bool", "@on", NULL)
};

/* Class Declaration */

ClassDecl(displayManager_decls,
          var_displayManager, send_displayManager,
	  get_displayManager, rc_displayManager,
          0, NULL,
          "$Rev$");


status
makeClassDisplayManager(Class class)
{ declareClass(class, &displayManager_decls);

  globalObject(NAME_displayManager, ClassDisplayManager, EAV);
  DispatchEvents = dispatch_events;

  succeed;
}

