/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1996 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/dialog.h>


		/********************************
		*            CREATE		*
		********************************/

static status
initialiseTabStack(TabStack t, int argc, Tab tabs[])
{ int n;

  initialiseDevice((Device) t);
  for(n=0; n<argc; n++)
    TRY(send(t, NAME_append, tabs[n], 0));

  succeed;
}

		 /*******************************
		 *	      REDRAW		*
		 *******************************/

static status
RedrawAreaTabStack(TabStack t, Area a)
{ Device dev = (Device) t;
  device_draw_context ctx;

  if ( EnterRedrawAreaDevice(dev, a, &ctx) )
  { Cell cell;

    for_cell(cell, dev->graphicals)
    { Graphical gr = cell->value;

      if ( overlapArea(a, gr->area) )
	RedrawArea(gr, a);
    }

    ExitRedrawAreaDevice(dev, a, &ctx);
  }

  return RedrawAreaGraphical(dev, a);
}

		 /*******************************
		 *	       EVENT		*
		 *******************************/

static status
eventTabStack(TabStack t, EventObj ev)
{ Cell cell;

  for_cell(cell, t->graphicals)
  { if ( instanceOfObject(cell->value, ClassTab) )
    { Tab tab = cell->value;
      Int X, Y;
      int x, y;

      get_xy_event(ev, tab, OFF, &X, &Y);
      x = valInt(X), y = valInt(Y);

      if ( y < 0 )			/* tab-bar */
      { if ( isDownEvent(ev) &&
	     y > -valInt(tab->label_size->h) &&
	     x > valInt(tab->label_offset) &&
	     x < valInt(tab->label_offset) + valInt(tab->label_size->w) )
	{ send(t, NAME_onTop, tab, 0);	/* TBD: use gesture? */
	  succeed;
	}
      }
    }
  }

  return eventDevice(t, ev);
}


		 /*******************************
		 *	     MEMBERS		*
		 *******************************/

static status
appendTabStack(TabStack ts, Tab t)
{ setGraphical(t, ZERO, ZERO, DEFAULT, DEFAULT);
  displayDevice(ts, t, DEFAULT);

  if ( ts->graphicals->size == ONE )
  { send(t, NAME_status, NAME_onTop, 0);
  } else
  { send(t, NAME_status, NAME_hidden, 0);
    send(ts, NAME_layoutLabels, 0);
  }

  succeed;
}


		 /*******************************
		 *	      LAYOUT		*
		 *******************************/

static status
layoutLabelsTabStack(TabStack ts)
{ int offset = 0;
  Cell cell;

  for_cell(cell, ts->graphicals)
  { Tab t = cell->value;

    if ( instanceOfObject(t, ClassTab) )
    { send(t, NAME_labelOffset, toInt(offset), 0);
      offset += valInt(t->label_size->w);
    }
  }

  succeed;
}


static status
layoutDialogTabStack(TabStack ts, Size s)
{ int w, h;
  Tab first;
  Cell cell;

  if ( !(first = getHeadChain(ts->graphicals)) ||
       !instanceOfObject(first, ClassTab) )
    fail;

  if ( isDefault(s) )
  { struct area a;

    for_cell(cell, ts->graphicals)
    { Graphical gr = cell->value;
      Bool old = gr->displayed;

      assign(gr, displayed, ON);	/* why? */
      send(cell->value, NAME_layoutDialog, 0);
      assign(gr, displayed, old);
    }
    
    initHeaderObj(&a, ClassArea);
    a.x = a.y = a.w = a.h = ZERO;
    for_cell(cell, ts->graphicals)
    { Graphical gr = cell->value;

      unionNormalisedArea(&a, gr->area);
    }
    w = valInt(a.w);
    h = valInt(a.h);
  } else
  { w = valInt(s->w);
    h = valInt(s->h);
  }

  h -= valInt(first->label_size->h);

  for_cell(cell, ts->graphicals)
  { Size sz = answerObject(ClassSize, toInt(w), toInt(h), 0);

    send(cell->value, NAME_size, sz, 0);
  }

  succeed;
}


static status
onTopTabStack(TabStack ts, Tab t)
{ Cell cell;

  for_cell(cell, ts->graphicals)
  { send(cell->value, NAME_status,
	 (Tab)cell->value == t ? NAME_onTop : NAME_hidden, 0);
  }

  send(t, NAME_advance, 0);		/* initialise keyboard focus */

  succeed;
}


static Tab
getOnTopTabStack(TabStack ts)
{ Cell cell;

  for_cell(cell, ts->graphicals)
  { if ( instanceOfObject(cell->value, ClassTab) )
    { Tab t = cell->value;

      if ( t->status == NAME_onTop )
	answer(t);
    }
  }

  fail;
}


/* Instance Variables */

/* Send Methods */

static senddecl send_tab_stack[] =
{ SM(NAME_initialise, 1, "member=tab ...", initialiseTabStack,
     DEFAULT, "Create from list of tab objects"),
  SM(NAME_event, 1, "event", eventTabStack,
     NAME_event, "Process an event"),
  SM(NAME_append, 1, "tab", appendTabStack,
     NAME_organisation, "Append a tab object"),
  SM(NAME_layoutLabels, 0, NULL, layoutLabelsTabStack,
     NAME_layout, "Assign positions for the labels"),
  SM(NAME_layoutDialog, 1, "[size]", layoutDialogTabStack,
     NAME_layout, "Adjust the members"),
  SM(NAME_onTop, 1, "member:tab", onTopTabStack,
     NAME_stack, "Put indicated tab on top of the others")
};

/* Get Methods */

static getdecl get_tab_stack[] =
{ GM(NAME_onTop, 0, "tab", NULL, getOnTopTabStack,
     NAME_stack, "Find tab on top")
};

/* Class Declaration */

ClassDecl(tab_stack_decls,
          NULL, send_tab_stack, get_tab_stack, NULL,
          ARGC_UNKNOWN, NULL,
          "$Rev$");


status
makeClassTabStack(Class class)
{ declareClass(class, &tab_stack_decls);
  setRedrawFunctionClass(class, RedrawAreaTabStack);

  succeed;
}

