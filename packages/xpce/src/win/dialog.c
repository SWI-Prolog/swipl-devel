/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialiseDialog(Dialog d, Name name, Size size, DisplayObj display)
{ TileObj t;

  initialiseWindow((PceWindow) d, name, size, display);

  assign(d, gap, newObject(ClassSize, 0));
  copySize(d->gap, getResourceValueObject(d, NAME_gap));
  assign(d, size_given, OFF);

  t = getTileWindow((PceWindow) d);
  assign(t, horShrink,  ZERO);
  assign(t, verShrink,  ZERO);
  assign(t, horStretch, ZERO);
  assign(t, verStretch, ZERO);

  succeed;
}


status
displayDialog(Dialog d, Graphical item, Point pos)
{ if ( displayDevice(d, item, pos) )
  { if ( notDefault(pos) )
      send(item, NAME_autoAlign, OFF, 0);
    if ( instanceOfObject(item, ClassDialogItem) )
      d->graphicals->current = d->graphicals->tail;
    if ( isNil(d->keyboard_focus) && send(item, NAME_WantsKeyboardFocus, 0) )
      keyboardFocusWindow((PceWindow) d, item);

    succeed;
  }
  
  fail;
}


		/********************************
		*            TYPING		*
		********************************/

static status
typedDialog(Dialog d, EventId id, Bool delegate)
{ Name key = characterName(id);
  Graphical gr;

  for_chain(d->graphicals, gr,
	    if ( send(gr, NAME_key, key, 0) )
	      succeed);

  if ( delegate == ON && notNil(d->frame) )
    return send(d->frame, NAME_typed, id, 0);

  fail;
}


static status
appendDialog(Dialog d, Graphical item, Name where)
{ return appendDialogItemDevice((Device) d, item, where);
}


static status
layoutDialog(Dialog d)
{ return layoutDialogDevice((Device) d, d->gap);
}


static status
computeDesiredSizeDialog(Dialog d)
{ TRY(send(d, NAME_layout, 0));

  if ( isNil(d->keyboard_focus) )
    advanceDevice((Device) d, NIL);	/* select first text item */

  ComputeGraphical(d);

  if ( d->size_given != ON )
  { if ( emptyChain(d->graphicals) )
    { Size sz = getResourceValueObject(d, NAME_size);

      send(d, NAME_size, sz, 0);
    } else
    { Area a = d->bounding_box;
      int w = valInt(a->x) + valInt(a->w) + valInt(d->gap->w);
      int h = valInt(a->y) + valInt(a->h) + valInt(d->gap->h);

      send(d, NAME_set, DEFAULT, DEFAULT, toInt(w), toInt(h), 0);
    }
  }

  succeed;
}


static status
sizeDialog(Dialog d, Size size)
{ assign(d, size_given, ON);

  return setGraphical(d, DEFAULT, DEFAULT, size->w, size->h);
}

		/********************************
		*         MISCELLANEAUS		*
		********************************/

static Graphical
getMemberDialog(Dialog d, Any obj)
{ if ( isName(obj) )
    return getMemberDevice((Device) d, (Name) obj);
  
  if ( ((Graphical)obj)->device == (Device) d )
    answer(obj);

  fail;
}


static status
memberDialog(Dialog d, Any obj)
{ return getMemberDialog(d, obj) != FAIL ? SUCCEED : FAIL;
}


static Chain
getMembersDialog(Dialog d)
{ answer(d->graphicals);
}


static status
deleteDialog(Dialog d, Graphical gr)
{ return freeObject(gr);
}


static status
caretDialog(Dialog d, Graphical gr)
{ return keyboardFocusWindow((PceWindow) d, gr);
}


static status
activeDialog(Dialog d, Bool val)
{ assign(d, sensitive, val);

  succeed;
}


static Bool
getActiveDialog(Dialog d)
{ answer(d->sensitive);
}


		/********************************
		*         COMMUNICATION		*
		********************************/

static Name
defaultAccelerator(void)
{ static Name acc = NULL;

  if ( !acc )
    acc = CtoName("RET");

  return acc;
}


static status
defaultButtonDialog(Dialog d, Button b)
{ Cell cell;	

  for_cell(cell, d->graphicals)
  { if ( instanceOfObject(cell->value, ClassButton) )
    { Button b2 = cell->value;

      if ( b2->accelerator == defaultAccelerator() )
	send(b2, NAME_accelerator, NIL, 0);
    }
  }

  if ( notNil(b) )
    send(b, NAME_accelerator, defaultAccelerator(), 0);

  succeed;
}


static Button
getDefaultButtonDialog(Dialog d)
{ Cell cell;	

  for_cell(cell, d->graphicals)
  { if ( instanceOfObject(cell->value, ClassButton) )
    { Button b = cell->value;

      if ( b->accelerator == defaultAccelerator() )
	answer(b);
    }
  }

  fail;
}


static status
applyDialog(Dialog d, Bool always)
{ DialogItem di;
  
  for_chain(d->graphicals, di, send(di, NAME_apply, always, 0));
  succeed;
}


static status
restoreDialog(Dialog d)
{ DialogItem di;
  
  for_chain(d->graphicals, di, send(di, NAME_restore, 0));
  succeed;
}


static status
modifiedItemDialog(Dialog d, Graphical gr, Bool m)
{ Button b;

  if ( (b = getDefaultButtonDialog(d)) )
    return send(b, NAME_active, ON, 0);

  fail;
}

		/********************************
		*           REPORT		*
		********************************/

static Any
getReportToDialog(Dialog d)
{ Any reporter;

  if ( (reporter = get(d, NAME_member, NAME_reporter, 0)) )
    answer(reporter);

  answer(getReportToVisual((VisualObj) d));
}


status
makeClassDialog(Class class)
{ sourceClass(class, makeClassDialog, __FILE__, "$Revision$");

  localClass(class, NAME_gap, NAME_layout, "size", NAME_both,
	     "Distance in X and Y direction between items");
  localClass(class, NAME_sizeGiven, NAME_layout, "bool", NAME_none,
	     "User specified explicit size");

  termClass(class, "dialog", 1, NAME_label, NAME_displaySize, NAME_display);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "label=[name]", "size=[size]", "display=[display]",
	     "Create from label, size and display",
	     initialiseDialog);
  sendMethod(class, NAME_append, NAME_organisation, 2,
	     "item=graphical", "relative_to_last=[{below,right,next_row}]",
	     "Append dialog_item {below,right,next_row} last",
	     appendDialog);
  sendMethod(class, NAME_display, NAME_organisation, 2,
	     "graphical", "at=[point]",
	     "Display a graphical (or item) at point",
	     displayDialog);
  sendMethod(class, NAME_ComputeDesiredSize, NAME_layout, 0,
	     "Compute the desired size",
	     computeDesiredSizeDialog);
  sendMethod(class, NAME_member, NAME_organisation, 1, "name|dialog_item",
	     "Test if dialog_item or name is a member",
	     memberDialog);
  sendMethod(class, NAME_delete, NAME_organisation, 1, "member:graphical",
	     "Delete (named) dialog item",
	     deleteDialog);
  sendMethod(class, NAME_size, NAME_area, 1, "size",
	     "Give the dialog window an explicit size",
	     sizeDialog);
  sendMethod(class, NAME_active, NAME_active, 1, "bool",
	     "(DE)activate the entire window",
	     activeDialog);
  sendMethod(class, NAME_layout, NAME_layout, 0,
	     "(Re)compute layout of dialog_items",
	     layoutDialog);
  sendMethod(class, NAME_caret, NAME_focus, 1, "member:graphical",
	     "Assign the caret to an input object",
	     caretDialog);
  sendMethod(class, NAME_typed, NAME_accelerator, 2, "event_id", "[bool]",
	     "Handle accelerators",
	     typedDialog);
  sendMethod(class, NAME_apply, NAME_apply, 1, "always=[bool]",
	     "->apply all changed items",
	     applyDialog);
  sendMethod(class, NAME_restore, NAME_apply, 0,
	     "->restore all items to their <-default",
	     restoreDialog);
  sendMethod(class, NAME_modifiedItem, NAME_apply, 2,
	     "item=graphical", "modified=bool",
	     "Indicates item has changed state",
	     modifiedItemDialog);
  sendMethod(class, NAME_defaultButton, NAME_accelerator, 1, "member:button*",
	     "Button connected to `RET'",
	     defaultButtonDialog);

  getMethod(class, NAME_member, NAME_organisation, "graphical", 1,
	    "name|graphical",
	    "Find named dialog_item",
	    getMemberDialog);
  getMethod(class, NAME_members, NAME_organisation, "chain", 0,
	    "Equivalent to <-graphicals",
	    getMembersDialog);
  getMethod(class, NAME_active, NAME_active, "bool", 0,
	    "Equivalent to Window <-sensitive",
	    getActiveDialog);
  getMethod(class, NAME_defaultButton, NAME_accelerator, "button", 0,
	    "Current Button connected to `RET'",
	    getDefaultButtonDialog);
  getMethod(class, NAME_reportTo, NAME_report, "graphical|frame", 0,
	    "<-member: reporter or <-contained_in",
	    getReportToDialog);
	    
  attach_resource(class, "gap", "size", "size(15,8)",
		  "Distance between items in X and Y");

  succeed;
}

