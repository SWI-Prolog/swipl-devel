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
  assign(di, reference,		 DEFAULT);
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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_dialogItem[] =
{ SV(NAME_label, "name", IV_GET|IV_STORE, labelDialogItem,
     NAME_label, "Label of the item"),
  IV(NAME_labelWidth, "[int]", IV_NONE,
     NAME_layout, "Width of the label in pixels"),
  SV(NAME_labelFormat, "{left,center,right}", IV_GET|IV_STORE,
     labelFormatDialogItem,
     NAME_layout, "Align labels in their box"),
  SV(NAME_background, "image|colour*", IV_GET|IV_STORE, backgroundDialogItem,
     NAME_appearance, "Opaque background for item"),
  SV(NAME_status, "{inactive,active,preview,execute}", IV_GET|IV_STORE,
     statusDialogItem,
     NAME_event, "Status for event-processing"),
  IV(NAME_message, "[code]*", IV_BOTH,
     NAME_action, "Associated command"),
  IV(NAME_popup, "popup*", IV_BOTH,
     NAME_menu, "Associated popup menu"),
  SV(NAME_look, "{x,open_look,motif,win}|name",IV_GET|IV_STORE,lookDialogItem,
     NAME_appearance, "Look-and-feel switch"),
  IV(NAME_autoAlign, "bool", IV_BOTH,
     NAME_layout, "Item is automatically placed by its dialog"),
  IV(NAME_reference, "[point]", IV_SEND,
     NAME_layout, "Reference point for alignment"),
  IV(NAME_above, "graphical*", IV_GET,
     NAME_layout, "DialogItem above me"),
  IV(NAME_below, "graphical*", IV_GET,
     NAME_layout, "DialogItem below me"),
  IV(NAME_right, "graphical*", IV_GET,
     NAME_layout, "DialogItem right of me"),
  IV(NAME_left, "graphical*", IV_GET,
     NAME_layout, "DialogItem left of me"),
  IV(NAME_alignment, "{column,left,center,right}", IV_BOTH,
     NAME_layout, "Align in columns or right of item to the left"),
  IV(NAME_autoLabelAlign, "bool", IV_BOTH,
     NAME_layout, "Automatically align label"),
  IV(NAME_autoValueAlign, "bool", IV_BOTH,
     NAME_layout, "Automatically align value")
};

/* Send Methods */

static senddecl send_dialogItem[] =
{ SM(NAME_initialise, 1, "name=name", createDialogItem,
     DEFAULT, "Create from name"),
  SM(NAME_device, 1, "device*", deviceDialogItem,
     DEFAULT, "Device I'm displayed on"),
  SM(NAME_name, 1, "name", nameDialogItem,
     DEFAULT, "Change <-name, update <-label"),
  SM(NAME_reset, 0, NULL, resetDialogItem,
     DEFAULT, "Change status to `inactive'"),
  SM(NAME_unlink, 0, NULL, unlinkDialogItem,
     DEFAULT, "Remove left,right,above,below links"),
  SM(NAME_default, 1, "any", virtualObject,
     NAME_apply, "Virtual method"),
  SM(NAME_modified, 1, "bool", modifiedDialogItem,
     NAME_apply, "Forward modification to associated <-device"),
  SM(NAME_cancel, 0, NULL, cancelDialogItem,
     NAME_event, "Cancel operation (enter inactive state)"),
  SM(NAME_event, 1, "event", eventDialogItem,
     NAME_event, "Process an event"),
  SM(NAME_labelWidth, 1, "[int]", virtualObject,
     NAME_layout, "Virtual method"),
  SM(NAME_valueWidth, 1, "[int]", virtualObject,
     NAME_layout, "Virtual method"),
  SM(NAME_open, 0, NULL, openDialogItem,
     NAME_organisation, "Create dialog with this item and ->open"),
  SM(NAME_show, 1, "bool", showDialogItem,
     NAME_organisation, "Equivalent to <->displayed")
};

/* Get Methods */

static getdecl get_dialogItem[] =
{ GM(NAME_default, 0, "any", NULL, getVirtualObject,
     NAME_apply, "Virtual method"),
  GM(NAME_modified, 0, "bool", NULL, getModifiedDialogItem,
     NAME_apply, "Virtual method (return @off)"),
  GM(NAME_labelName, 1, "name", "name", getLabelNameDialogItem,
     NAME_label, "Determine default-label from the name"),
  GM(NAME_labelWidth, 0, "int", NULL, getVirtualObject,
     NAME_layout, "Virtual method"),
  GM(NAME_reference, 0, "point", NULL, getReferenceDialogItem,
     NAME_layout, "Reference point for alignment"),
  GM(NAME_valueWidth, 0, "int", NULL, getVirtualObject,
     NAME_layout, "Virtual method"),
  GM(NAME_show, 0, "bool", NULL, getShowDialogItem,
     NAME_organisation, "Equivalent to <-displayed")
};

/* Resources */

static resourcedecl rc_dialogItem[] =
{ RC(NAME_alignment, "{column,left,center,right}", "column",
     "Alignment in the row"),
  RC(NAME_background, "colour|pixmap*", "@nil",
     "Background of the item"),
  RC(NAME_elevation, "elevation*", "@nil",
     "3-D elevation"),
  RC(NAME_labelFont, "font", "bold",
     "Default font for labels"),
  RC(NAME_labelFormat, "{left,center,right}", "left",
     "Alignment of the label in its box"),
  RC(NAME_labelSuffix, "name", ":",
     "Ensured suffix of label"),
  RC(NAME_look, "{x,open_look,motif,win}", "x",
     "Look-and-feel switch"),
  RC(NAME_selectionHandles, RC_REFINE, "@nil",
     NULL),
  RC(NAME_valueFont, "font", "normal",
     "Default font for values")
};

/* Class Declaration */

static Name dialogItem_termnames[] = { NAME_label };

ClassDecl(dialogItem_decls,
          var_dialogItem, send_dialogItem, get_dialogItem, rc_dialogItem,
          1, dialogItem_termnames,
          "$Rev$");

status
makeClassDialogItem(Class class)
{ return declareClass(class, &dialogItem_decls);
}

