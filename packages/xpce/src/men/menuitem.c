/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/dialog.h>

static status	labelMenuItem(MenuItem mi, Any label);

static status
initialiseMenuItem(MenuItem m, Any value, Message msg, Any label,
		   Bool eg, Code cond, Name acc)
{ if ( isDefault(eg) )
    eg = OFF;
  if ( isDefault(label) && !(label = get(m, NAME_defaultLabel, value, 0)) )
    return errorPce(m, NAME_noDefaultLabel, value);
  if ( isDefault(cond) )
    cond = NIL;

  assign(m, value,     value);
  assign(m, message,   msg);
  assign(m, label,     label);
  assign(m, font,      DEFAULT);
  assign(m, colour,    DEFAULT);
  assign(m, selected,  OFF);
  assign(m, active,    ON);
  assign(m, condition, cond);
  assign(m, end_group, eg);

  if ( notDefault(acc) )
    assign(m, accelerator, acc);

  return labelMenuItem(m, label);
}


static status
unlinkMenuItem(MenuItem mi)
{ if ( notNil(mi->menu) )
    deleteMenu(mi->menu, mi);

  succeed;
}


static MenuItem
getConvertMenuItem(Class class, Any value)
{ if ( instanceOfObject(value, ClassPopup) )
  { PopupObj popup = value;
    MenuItem mi = newObject(ClassMenuItem, get(popup, NAME_label, 0), 0);

    assign(mi, popup, popup);
    assign(popup, context, mi);

    answer(mi);
  }

  answer(newObject(ClassMenuItem, value, 0));
}


		/********************************
		*      CHANGING ATTRIBUTES	*
		********************************/

static status
changedMenuItem(MenuItem mi)
{ if ( notNil(mi->menu) )
    return qadSendv(mi->menu, NAME_ChangedItem, 1, (Any *) &mi);

  succeed;
}


static status
labelMenuItem(MenuItem mi, Any label)
{ if ( mi->label != label )
  { assign(mi, label, label);
    if ( notNil(mi->menu) )
      requestComputeGraphical(mi->menu, DEFAULT); /* may change layout */
    changedMenuItem(mi);
  }

  succeed;
}


static status
valueMenuItem(MenuItem mi, Any value, Any label)
{ if ( isDefault(label) && !(label = get(mi, NAME_defaultLabel, value, 0)) )
    return errorPce(mi, NAME_noDefaultLabel, value);

  assign(mi, value, value);
  labelMenuItem(mi, label);

  succeed;
}


static status
fontMenuItem(MenuItem mi, FontObj font)
{ if ( mi->font != font )
  { assign(mi, font, font);
    changedMenuItem(mi);
  }
  
  succeed;
}


static status
colourMenuItem(MenuItem mi, Colour colour)
{ if ( mi->colour != colour )
  { assign(mi, colour, colour);
    changedMenuItem(mi);
  }
  
  succeed;
}


static status
activeMenuItem(MenuItem mi, Bool val)
{ if ( mi->active != val )
  { assign(mi, active, val);
    changedMenuItem(mi);
  }

  succeed;
}


status
selectedMenuItem(MenuItem mi, Bool val)
{ if ( mi->selected != val )
  { assign(mi, selected, val);
    changedMenuItem(mi);
  }

  succeed;
}


static status
endGroupMenuItem(MenuItem mi, Bool val)
{ if ( mi->end_group != val )
  { assign(mi, end_group, val);
    changedMenuItem(mi);
  }

  succeed;
}


static status
popupMenuItem(MenuItem mi, PopupObj p)
{ if ( mi->popup != p )
  { if ( isNil(p) || (isNil(mi->popup) && notNil(mi->menu)) )
      requestComputeGraphical(mi->menu, DEFAULT); /* HACK */
    assign(mi, popup, p);
    changedMenuItem(mi);
  }

  succeed;
}


static status
onMenuItem(MenuItem mi)
{ return activeMenuItem(mi, ON);
}


static status
offMenuItem(MenuItem mi)
{ return activeMenuItem(mi, OFF); 
}


		/********************************
		*            MESSAGES		*
		********************************/

static status
messageMenuItem(MenuItem mi, Code msg)
{ assign(mi, message, msg);
  if ( notNil(mi->popup) )
    assign(mi->popup, message, msg);

  succeed;
}


static Code
getMessageMenuItem(MenuItem mi)
{ answer(notNil(mi->popup) ? mi->popup->message
			   : mi->message);
}


status
hasValueMenuItem(MenuItem mi, Any value)
{ Name name;

  if ( mi->value == value )
    succeed;

  if ( (name = checkType(mi->value, TypeName, mi)) &&
       name == checkType(value, TypeName, mi) )
    succeed;

  fail;
}



static Any
getDefaultLabelMenuItem(MenuItem m, Any value)
{ Name name;
  Graphical gr;

  if ( (gr = checkType(value, nameToType(NAME_graphical), m)) )
  { Image image = answerObject(ClassImage, NIL,
			       getAreaGraphical(gr)->w,
			       getAreaGraphical(gr)->h, 0);
    Point p = tempObject(ClassPoint, 0);
    TRY( send(image, NAME_drawIn, gr, p, 0) );
    considerPreserveObject(p);

    answer(image);
  }

  if ( (name = checkType(value, TypeName, m)) )
    answer(getLabelNameName(name));
  
  if ( isObject(value) && (name = get(value, NAME_name, 0)) )
    answer(getLabelNameName(name));

  answer((Any) CtoName(pp(value)));
}


static CharArray
getPrintNameMenuItem(MenuItem mi)
{ return getv(mi->value, NAME_printName, 0, NULL);
}


		/********************************
		*             VISUAL		*
		********************************/

static Menu
getContainedInMenuItem(MenuItem mi)
{ answer(mi->menu);
}


status
makeClassMenuItem(Class class)
{ sourceClass(class, makeClassMenuItem, __FILE__, "$Revision$");

  localClass(class, NAME_menu, NAME_organisation, "menu*", NAME_get,
	     "Menu I'm part of");
  localClass(class, NAME_value, NAME_value, "any", NAME_get,
	     "Value of the item");
  localClass(class, NAME_message, NAME_action, "[code]*", NAME_both,
	     "Message sent when selected");
  localClass(class, NAME_label, NAME_appearance, "[name|image]", NAME_get,
	     "Image or string displayed");
  localClass(class, NAME_font, NAME_appearance, "[font]", NAME_get,
	     "Font for label");
  localClass(class, NAME_colour, NAME_appearance, "[colour]", NAME_get,
	     "Colour for label");
  localClass(class, NAME_selected, NAME_selection, "bool", NAME_get,
	     "Member of menu-selection");
  localClass(class, NAME_active, NAME_active, "bool", NAME_get,
	     "Can be selected by user");
  localClass(class, NAME_condition, NAME_active, "code*", NAME_both,
	     "If true, item becomes active");
  localClass(class, NAME_endGroup, NAME_group, "bool", NAME_get,
	     "Popup: add separation-line");
  localClass(class, NAME_popup, NAME_menu, "popup*", NAME_get,
	     "Associated popup (pull-right)");
  localClass(class, NAME_accelerator, NAME_accelerator, "name*", NAME_both,
	     "Activate when ->key: name is received");


  termClass(class, "menu_item",
	    6, NAME_value, NAME_message, NAME_label,
	       NAME_endGroup, NAME_condition, NAME_accelerator);
				   
  storeMethod(class, NAME_message,  messageMenuItem);
  storeMethod(class, NAME_active,   activeMenuItem);
  storeMethod(class, NAME_label,    labelMenuItem);
  storeMethod(class, NAME_font,     fontMenuItem);
  storeMethod(class, NAME_colour,   colourMenuItem);
  storeMethod(class, NAME_endGroup, endGroupMenuItem);
  storeMethod(class, NAME_popup,    popupMenuItem);
  storeMethod(class, NAME_selected, selectedMenuItem);

  sendMethod(class, NAME_initialise, DEFAULT, 6,
	     "value=any", "message=[code]*", "label=[name|image]",
	     "end_group=[bool]", "condition=[code]*", "accelerator=[name]",
	     "Create from value, message, label, end and cond",
	     initialiseMenuItem);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Unlink from menu",
	     unlinkMenuItem);
  sendMethod(class, NAME_on, NAME_active, 0,
	     "Activate item",
	     onMenuItem);
  sendMethod(class, NAME_off, NAME_active, 0,
	     "Deactivate item",
	     offMenuItem);
  sendMethod(class, NAME_value, DEFAULT, 2, "value=any", "label=[name|image]",
	     "Set value and recompute label",
	     valueMenuItem);

  getMethod(class, NAME_message, NAME_action, "message=[code]*", 0,
	    "Message that will be executed",
	    getMessageMenuItem);
  getMethod(class, NAME_defaultLabel, NAME_label, "label=name|image", 1,
	    "value=any",
	    "Compute default label from value",
	    getDefaultLabelMenuItem);
  getMethod(class, NAME_printName, DEFAULT, "char_array", 0,
	    "<-print_name of <-value",
	    getPrintNameMenuItem);
  getMethod(class, NAME_containedIn, DEFAULT, "menu", 0,
	    "Menu I'm contained in",
	    getContainedInMenuItem);
  getMethod(class, NAME_convert, DEFAULT, "menu_item", 1, "value=any",
	    "Convert value",
	    getConvertMenuItem);

  succeed;
}

