/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/dialog.h>

forwards Bool getDefaultButtonButton P((Button));

static status
initialiseButton(Button b, Name name, Message msg, Name acc)
{ createDialogItem(b, name);

  assign(b, pen,          DEFAULT);	/* resources */
  assign(b, radius,       DEFAULT);
  assign(b, font,         DEFAULT);
  assign(b, shadow,       DEFAULT);
  assign(b, popup_image,  DEFAULT);

  assign(b, message, msg);
  if ( notDefault(acc) )
    assign(b, accelerator, acc);

  return requestComputeGraphical(b, DEFAULT);
}


status
RedrawAreaButton(Button b, Area a)
{ int x, y, w, h;
  int pen    = valInt(b->pen);
  int radius = valInt(b->radius);
  int shadow = valInt(b->shadow);
  int defb = (getDefaultButtonButton(b) == ON);
  int rm = 0;				/* right-margin */
  int swapc = 0;

  if ( defb && b->look != NAME_openLook )
    pen++;

  initialiseDeviceGraphical(b, &x, &y, &w, &h);
  NormaliseArea(x, y, w, h);
  r_thickness(pen);
  r_dash(b->texture);

  if ( b->look == NAME_motif )
  { int z  = valInt(getResourceValueObject(b, NAME_elevation));

    if ( !(b->status == NAME_inactive || b->status == NAME_active) )
      z = -z;

    r_3d_box(x, y, w, h, abs(z), b->background, z > 0);
  } else
  { if ( b->status == NAME_inactive || b->status == NAME_active )
    { r_shadow_box(x, y, w, h, radius, shadow, NIL);
    } else if ( b->status == NAME_preview )
    { r_shadow_box(x, y, w, h, radius, shadow, BLACK_IMAGE);
      swapc = TRUE;
    } else if ( b->status == NAME_execute )
    { r_shadow_box(x, y, w, h, radius, shadow, GREY25_IMAGE);
    }
  }

  if ( swapc )
    r_swap_background_and_foreground();
  
  if ( defb && b->look == NAME_openLook )
    r_box(x+pen+1, y+pen+1, w-2*pen-2-shadow, h-2*pen-2-shadow, radius, NIL);
  
  if ( notNil(b->popup) )
  { if ( notNil(b->popup_image) )
    { int iw = valInt(b->popup_image->size->w);
      int ih = valInt(b->popup_image->size->h);

      rm = iw+8;
      r_image(b->popup_image, 0, 0, x+w-rm, y + (h-ih)/2, iw, ih, ON);
    } else
    { int z = 1;
      int th = 8;
      int tw = 9;
      int tx, ty;

      rm = tw+8;
      tx = x+w-rm;
      ty = y + (h-th)/2;

      r_3d_triangle(tx+tw/2, ty+th, tx, ty, tx+tw, ty, z);
    }
  }

  str_string(&b->label->data, b->font, x, y, w-rm, h,
	     NAME_center, NAME_center);

  if ( swapc )
    r_swap_background_and_foreground();

  return RedrawAreaGraphical(b, a);
}


static status
computeButton(Button b)
{ if ( notNil(b->request_compute) )
  { Size size = getResourceValueObject(b, NAME_size);
    int w, h;

    TRY(obtainResourcesObject(b));

    str_size(&b->label->data, b->font, &w, &h);
    h += 6; w += 10;
    if ( notNil(b->popup) )
    { if ( notNil(b->popup->popup_image) )
	w += valInt(b->popup->popup_image->size->w) + 5;
      else
	w += 9 + 5;
    }

    w = max(valInt(size->w), w);
    h = max(valInt(size->h), h);

    CHANGING_GRAPHICAL(b,
	 assign(b->area, w, toInt(w));
	 assign(b->area, h, toInt(h)));

    assign(b, request_compute, NIL);
  }

  succeed;
}


Point
getReferenceButton(Button b)
{ Point ref;

  if ( !(ref = getReferenceDialogItem(b)) )
    ref = answerObject(ClassPoint,
		       ZERO,
		       add(toInt(3), getAscentFont(b->font)),
		       0);
  
  answer(ref);
}


static status
statusButton(Button b, Name stat)
{ if ( stat != b->status )
  { Name oldstat = b->status;

    assign(b, status, stat);

					/* These are equal: do not redraw */
    if ( !( (stat == NAME_active || stat == NAME_inactive) &&
	    (oldstat == NAME_active || oldstat == NAME_inactive)
	  ) )
      changedDialogItem(b);
  }

  succeed;
}


status
makeButtonGesture()
{ if ( GESTURE_button != NULL )
    succeed;

  GESTURE_button =
    globalObject(NAME_ButtonGesture, ClassClickGesture,
		 NAME_left, DEFAULT, NAME_single,
		 newObject(ClassMessage, RECEIVER, NAME_execute, 0),
		 newObject(ClassMessage, RECEIVER, NAME_status,NAME_preview,0),
		 newObject(ClassMessage, RECEIVER, NAME_cancel, 0),
		 0);

  assert(GESTURE_button);
  succeed;
}


static status
eventButton(Button b, EventObj ev)
{ if ( eventDialogItem(b, ev) )
    succeed;

  if ( b->active == ON )
  { makeButtonGesture();

    return eventGesture(GESTURE_button, ev);
  }

  fail;
}


static status
keyButton(Button b, Name key)
{ if ( b->accelerator == key && b->active == ON )
    return send(b, NAME_execute, 0);

  fail;
}


static status
executeButton(Button b)
{ if ( notNil(b->message) )
  { statusButton(b, NAME_execute);
    flushGraphical(b);
    if ( notDefault(b->message) )
    { DisplayObj d = getDisplayGraphical((Graphical) b);

      busyCursorDisplay(d, DEFAULT, DEFAULT);
      forwardReceiverCode(b->message, b, 0);
      busyCursorDisplay(d, NIL, DEFAULT);
    }
    else if ( notNil(b->device) )
      send(b->device, b->name, 0);
  }

  if ( !isFreedObj(b) )
    statusButton(b, NAME_inactive);

  succeed;
}
  
		/********************************
		*          ATTRIBUTES		*
		********************************/

static status
defaultButtonButton(Button b, Bool val)
{ assignGraphical(b, NAME_accelerator, val != OFF ? CtoName("RET") : NIL);

  succeed;
}


static Bool
getDefaultButtonButton(Button b)
{ answer(b->accelerator == CtoName("RET") ? ON : OFF);
}


static status
radiusButton(Button b, Int radius)
{ return assignGraphical(b, NAME_radius, radius);
}


static status
popupButton(Button b, PopupObj p)
{ return assignGraphical(b, NAME_popup, p);
}


static PopupObj
getPopupButton(Button b, Bool create)
{ if ( notNil(b->popup) || create != ON )
    answer(b->popup);
  else
  { PopupObj p = newObject(ClassPopup, b->label, 0);

    send(p, NAME_append,
	 newObject(ClassMenuItem,
		   b->name,
		   newObject(ClassMessage, Arg(1), NAME_execute, 0),
		   b->label, 0), 0);
    popupButton(b, p);
    answer(p);
  }
}


static status
fontButton(Button b, FontObj font)
{ return assignGraphical(b, NAME_font, font);
}


static status
shadowButton(Button b, Int shadow)
{ return assignGraphical(b, NAME_shadow, shadow);
}


static status
popupImageButton(Button b, Image img)
{ return assignGraphical(b, NAME_popupImage, img);
}


static Name
getSelectionButton(Button b)
{ answer(b->label);
}


status
makeClassButton(Class class)
{ sourceClass(class, makeClassButton, __FILE__, "$Revision$");

  localClass(class, NAME_radius, NAME_appearance, "int", NAME_get,
	     "Rounding radius for corners");
  localClass(class, NAME_shadow, NAME_appearance, "int", NAME_get,
	     "Shadow shown around the box");
  localClass(class, NAME_font, NAME_appearance, "font", NAME_get,
	     "Font of command text");
  localClass(class, NAME_accelerator, NAME_accelerator, "key=name*", NAME_both,
	     "Activate when ->key: name is received");
  localClass(class, NAME_popupImage, NAME_appearance, "image*", NAME_get,
	     "Indication that button has a popup menu");

  termClass(class, "button", 3, NAME_label, NAME_message, NAME_accelerator);
  setRedrawFunctionClass(class, RedrawAreaButton);

  storeMethod(class, NAME_status,      statusButton);
  storeMethod(class, NAME_radius,      radiusButton);
  storeMethod(class, NAME_font,        fontButton);
  storeMethod(class, NAME_popup,       popupButton);
  storeMethod(class, NAME_shadow,      shadowButton);
  storeMethod(class, NAME_popupImage,  popupImageButton);

  sendMethod(class, NAME_initialise, DEFAULT,
	     3, "name=name", "message=[code]*", "label=[name]",
	     "Create from name and command",
	     initialiseButton);
  sendMethod(class, NAME_compute, DEFAULT, 0,
	     "Compute desired size (from command)",
	     computeButton);
  sendMethod(class, NAME_event, DEFAULT, 1, "event",
	     "Process an event",
	     eventButton);
  sendMethod(class, NAME_selection, NAME_label, 1, "name",
	     "Equivalent to ->label",
	     labelDialogItem);
  sendMethod(class, NAME_execute, NAME_action, 0,
	     "Execute associated command",
	     executeButton);
  sendMethod(class, NAME_key, NAME_accelerator, 1, "key=name",
	     "Handle accelerator key `name'",
	     keyButton);
  sendMethod(class, NAME_defaultButton, NAME_accelerator, 1, "bool",
	     "@on sets <-accelerator to `RET'",
	     defaultButtonButton);

  getMethod(class, NAME_selection, NAME_label, "name", 0,
	    "Equivalent to <-label",
	    getSelectionButton);
  getMethod(class, NAME_defaultButton, NAME_accelerator, "bool", 0,
	    "@on iff <-accelerator == `RET'",
	    getDefaultButtonButton);
  getMethod(class, NAME_popup, DEFAULT, "popup*", 1, "create=[bool]",
	    "Associated popup (make one if create = @on)",
	    getPopupButton);
  getMethod(class, NAME_reference, DEFAULT, "point", 0,
	    "Left, baseline of label",
	    getReferenceButton);


  attach_resource(class, "font", "font", "@helvetica_bold_14",
		  "Default font for text");
  attach_resource(class, "size", "size", "size(80,20)",
		  "Mimimum size in pixels");
  attach_resource(class, "pen", "int", "2",
		  "Thickness of box");
  attach_resource(class, "radius", "int", "4",
		  "Rounding radius of box");
  attach_resource(class, "shadow", "int", "0",
		  "Shadow shown around the box");
  attach_resource(class, "popup_image", "image*", "@nil",
		  "Image to indicate presence of popup menu");
  attach_resource(class, "label_suffix", "name", "",
		  "Ensured suffix of label");
  attach_resource(class, "alignment", "{column,left,center,right}", "left",
		  "Alignment in the row");

  succeed;
}

