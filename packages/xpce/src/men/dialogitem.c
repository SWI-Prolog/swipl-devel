/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/dialog.h>

static status	nameDialogItem(DialogItem di, Name name);

		/********************************
		*         CREATE/DESTROY	*
		********************************/

status
createDialogItem(Any obj, Name name)
{ DialogItem di = obj;

  initialiseGraphical(di, ZERO, ZERO, ZERO, ZERO);

  if ( isDefault(name) )
    name = getClassNameObject(di);
  nameDialogItem(di, name);

  assign(di, label_format,	 DEFAULT); /* resource */
  assign(di, background,	 DEFAULT); /* resource */
  assign(di, status,		 NAME_inactive);
  assign(di, message,		 NIL);
  assign(di, popup,		 NIL);
  assign(di, reference,		 newObject(ClassPoint, 0));
  assign(di, above,		 NIL);
  assign(di, below,		 NIL);
  assign(di, right,		 NIL);
  assign(di, left,		 NIL);
  assign(di, alignment,	 	 DEFAULT);
  assign(di, auto_label_align,	 ON);
  assign(di, auto_value_align,	 ON);
  assign(di, label_width,	 DEFAULT);
  assign(di, auto_align,	 ON);
  assign(di, look,		 DEFAULT);

  succeed;
}


status
unlinkDialogItem(DialogItem di)
{ Graphical gr = (Graphical) di;

  aboveGraphical(gr, NIL);		/* move to graphical??? */
  belowGraphical(gr, NIL);
  rightGraphical(gr, NIL);
  leftGraphical(gr, NIL);

  return unlinkGraphical(gr);
}


static status
deviceDialogItem(DialogItem di, Device dev)
{ if ( di->device != dev )
  { Graphical gr = (Graphical) di;

    aboveGraphical(gr, NIL);
    belowGraphical(gr, NIL);
    rightGraphical(gr, NIL);
    leftGraphical(gr, NIL);
  }

  return deviceGraphical((Graphical)di, dev);
}


		/********************************
		*          ATTRIBUTES		*
		********************************/

status
labelDialogItem(DialogItem di, Name label)
{ return assignGraphical(di, NAME_label, label);
}


static CharArray
getLabelNameDialogItem(DialogItem di, Name name)
{ Any suffix, label = getLabelNameName(name);

  if ( (suffix = getResourceValueObject(di, NAME_labelSuffix)) )
    label = getEnsureSuffixCharArray(label, suffix);

  answer(label);
}


static status
nameDialogItem(DialogItem di, Name name)
{ Any label = get(di, NAME_labelName, name, 0);

  assign(di, name, name);
  return labelDialogItem(di, label ? label : name);
}


static status
lookDialogItem(DialogItem di, Name look)
{ return assignGraphical(di, NAME_look, look);
}


static status
backgroundDialogItem(DialogItem di, Any bg)
{ return assignGraphical(di, NAME_background, bg);
}


static status
labelFormatDialogItem(DialogItem di, Name format)
{ return assignGraphical(di, NAME_labelFormat, format);
}


		/********************************
		*        EVENT_HANDLING		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Menus are treated special  here.  This is  not very clean.   How to do
this better?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
forwardDialogItem(DialogItem di, Code msg, EventObj ev)
{ if ( notNil(msg) && notDefault(msg) )
  { Any val;

    if ( (val = get(di, NAME_selection, 0)) == FAIL )
      val = (Any) di->label;

    forwardReceiverCode(msg, di, val, ev, 0);
  }

  succeed;
}


status
eventDialogItem(Any obj, EventObj ev)
{ DialogItem di = obj;
  
  if ( eventGraphical(di, ev) )
    succeed;

  if ( di->active == ON && notNil(di->popup) && isDownEvent(ev) &&
       send(popupGesture(), NAME_event, ev, 0) )
    succeed;

  fail;
}


static status
statusDialogItem(DialogItem di, Name stat)
{ assign(di, status, stat);
  changedDialogItem(di);

  succeed;
}


static status
cancelDialogItem(DialogItem di)
{ return send(di, NAME_status, NAME_inactive, 0);
}


		/********************************
		*           REPAINT		*
		********************************/


status
changedDialogItem(Any obj)
{ DialogItem di = obj;

  CHANGING_GRAPHICAL(di,
	changedEntireImageGraphical(di));

  succeed;
}


static status
showDialogItem(DialogItem di, Bool val)
{ if ( val == OFF )
  { PceWindow sw = getWindowGraphical((Graphical) di);

    if ( sw != FAIL && sw->keyboard_focus == (Graphical) di )
      advanceDevice(di->device, (Graphical) di);
  }

  return DisplayedGraphical(di, val);
}


static Bool
getShowDialogItem(DialogItem di)
{ answer(di->displayed);
}


		/********************************
		*          POSITIONS		*
		********************************/

Point
getReferenceDialogItem(Any obj)
{ DialogItem i = obj;

  ComputeGraphical(i);
  if ( notDefault(i->reference) )
    answer(i->reference);

  fail;
}


static status
resetDialogItem(DialogItem i)
{ send(i, NAME_status, NAME_inactive, 0);

  succeed;
}


static status
openDialogItem(DialogItem di)
{ if ( isNil(di->device) )
  { Dialog d;

    TRY( d = newObject(ClassDialog, 0) );
    TRY( send(d, NAME_append, di, 0) );
  }

  return send(di->device, NAME_open, 0);
}


		/********************************
		*        COMMUNICATION		*
		********************************/

Bool
getModifiedDialogItem(Dialog di)
{ answer(OFF);
}


status
modifiedDialogItem(Any di, Bool modified)
{ Dialog d = di;

  if ( modified == ON )
    return send(d->device, NAME_modifiedItem, d, ON, 0);

  succeed;
}


status
makeClassDialogItem(Class class)
{ sourceClass(class, makeClassDialogItem, __FILE__, "$Revision$");

  localClass(class, NAME_label, NAME_label, "name", NAME_get,
	     "Label of the item");
  localClass(class, NAME_labelWidth, NAME_layout, "[int]", NAME_none,
	     "Width of the label in pixels");
  localClass(class, NAME_labelFormat, NAME_layout,
	     "{left,center,right}", NAME_get,
	     "Align labels in their box");
  localClass(class, NAME_background, NAME_appearance, "image|colour*",
	     NAME_get,
	     "Opaque background for item");
  localClass(class, NAME_status, NAME_event,
	     "{inactive,active,preview,execute}", NAME_get,
	     "Status for event-processing");
  localClass(class, NAME_message, NAME_action, "[code]*", NAME_both,
	     "Associated command");
  localClass(class, NAME_popup, NAME_menu, "popup*", NAME_both,
	     "Associated popup menu");
  localClass(class, NAME_look, NAME_appearance,
	     "{x,open_look,motif}", NAME_get,
	     "Look-and-feel switch");
  localClass(class, NAME_autoAlign, NAME_layout, "bool", NAME_both,
	     "Item is automatically placed by its dialog");
  localClass(class, NAME_reference, NAME_layout, "[point]", NAME_send,
	     "Reference point for alignment");
  localClass(class, NAME_above, NAME_layout, "graphical*", NAME_get,
	     "DialogItem above me");
  localClass(class, NAME_below, NAME_layout, "graphical*", NAME_get,
	     "DialogItem below me");
  localClass(class, NAME_right, NAME_layout, "graphical*", NAME_get,
	     "DialogItem right of me");
  localClass(class, NAME_left, NAME_layout, "graphical*", NAME_get,
	     "DialogItem left of me");
  localClass(class, NAME_alignment, NAME_layout,
	     "{column,left,center,right}", NAME_both,
	     "Align in columns or right of item to the left");
  localClass(class, NAME_autoLabelAlign, NAME_layout, "bool", NAME_both,
	     "Automatically align label");
  localClass(class, NAME_autoValueAlign, NAME_layout, "bool", NAME_both,
	     "Automatically align value");

  termClass(class, "dialog_item", 1, NAME_label);  
  cloneStyleVariableClass(class, NAME_left,  NAME_nil); /* referenceOrNil? */
  cloneStyleVariableClass(class, NAME_right, NAME_nil);
  cloneStyleVariableClass(class, NAME_above, NAME_nil);
  cloneStyleVariableClass(class, NAME_below, NAME_nil);

  storeMethod(class, NAME_name,        nameDialogItem);
  storeMethod(class, NAME_label,       labelDialogItem);
  storeMethod(class, NAME_status,      statusDialogItem);
  storeMethod(class, NAME_device,      deviceDialogItem);
  storeMethod(class, NAME_look,        lookDialogItem);
  storeMethod(class, NAME_background,  backgroundDialogItem);
  storeMethod(class, NAME_labelFormat, labelFormatDialogItem);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "name=name",
	     "Create from name",
	     createDialogItem);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Remove left,right,above,below links",
	     unlinkDialogItem);
  sendMethod(class, NAME_reset, DEFAULT, 0,
	     "Change status to `inactive'",
	     resetDialogItem);
  sendMethod(class, NAME_cancel, NAME_event, 0,
	     "Cancel operation (enter inactive state)",
	     cancelDialogItem);
  sendMethod(class, NAME_show, NAME_organisation, 1, "bool",
	     "Equivalent to <->displayed",
	     showDialogItem);
  sendMethod(class, NAME_event, NAME_event, 1, "event",
	     "Process an event",
	     eventDialogItem);

  sendMethod(class, NAME_open, NAME_organisation, 0,
	     "Create dialog with this item and ->open",
	     openDialogItem);
  sendMethod(class, NAME_modified, NAME_apply, 1, "bool",
	     "Forward modification to associated <-device",
	     modifiedDialogItem);
  sendMethod(class, NAME_default, NAME_apply, 1, "any",
	     "Virtual method",
	     virtualObject);
  sendMethod(class, NAME_labelWidth, NAME_layout, 1, "[int]",
	     "Virtual method",
	     virtualObject);
  sendMethod(class, NAME_valueWidth, NAME_layout, 1, "[int]",
	     "Virtual method",
	     virtualObject);

  getMethod(class, NAME_labelName, NAME_label, "name", 1, "name",
	    "Determine default-label from the name",
	    getLabelNameDialogItem);
  getMethod(class, NAME_show, NAME_organisation, "bool", 0,
	    "Equivalent to <-displayed",
	    getShowDialogItem);
  getMethod(class, NAME_labelWidth, NAME_layout, "int", 0,
	    "Virtual method",
	    getVirtualObject);
  getMethod(class, NAME_valueWidth, NAME_layout, "int", 0,
	    "Virtual method",
	    getVirtualObject);
  getMethod(class, NAME_default, NAME_apply, "any", 0,
	    "Virtual method",
	    getVirtualObject);
  getMethod(class, NAME_modified, NAME_apply, "bool", 0,
	    "Virtual method (return @off)",
	    getModifiedDialogItem);
  getMethod(class, NAME_reference, NAME_layout, "point", 0,
	    "Reference point for alingment",
	    getReferenceDialogItem);

  attach_resource(class, "look", "{x,open_look,motif}", "x",
		  "Look-and-feel switch");
  attach_resource(class, "label_format", "{left,center,right}", "left",
		  "Alignment of the label in its box");
  attach_resource(class, "label_suffix", "name", ":",
		  "Ensured suffix of label");
  attach_resource(class, "alignment", "{column,left,center,right}", "column",
		  "Alignment in the row");
  attach_resource(class, "selection_style", "name", "none",
		  "Visual feedback of <->selected");
  attach_resource(class, "background", "colour|pixmap*", "@nil",
		  "Background of the item");
  attach_resource(class, "elevation", "int", "0",
		  "3-D elevation");

  succeed;
}

