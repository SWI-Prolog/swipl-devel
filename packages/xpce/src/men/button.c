/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/dialog.h>

forwards Bool getDefaultButtonButton(Button);

static status
initialiseButton(Button b, Name name, Message msg, Name acc)
{ createDialogItem(b, name);

  assign(b, pen,          DEFAULT);	/* resources */
  assign(b, radius,       DEFAULT);
  assign(b, label_font,   DEFAULT);
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
  int defb = (getDefaultButtonButton(b) == ON);
  int rm = 0;				/* right-margin */
  Elevation z = getResourceValueObject(b, NAME_elevation);

  initialiseDeviceGraphical(b, &x, &y, &w, &h);
  NormaliseArea(x, y, w, h);
  
  if ( z && notNil(z) )			/* 3-d style */
  { int up = (b->status == NAME_inactive || b->status == NAME_active);
   
    if ( b->look == NAME_motif || b->look == NAME_win )
    { int bx = x, by = y, bw = w, bh = h;
      PceWindow sw;
      int kbf;				/* Button has keyboard focus */
      int obhf;				/* Other button has focus */
      int focus;

      if ( (sw = getWindowGraphical((Graphical)b)) )
      { kbf   = (sw->keyboard_focus == (Graphical) b);
	obhf  = (!kbf && instanceOfObject(sw->keyboard_focus, ClassButton));
	focus = (sw->input_focus == ON);
      } else
	kbf = obhf = focus = FALSE;	/* should not happen */

      if ( b->look == NAME_motif )
      { if ( (defb && !obhf) || (focus && kbf) )
	{ static Elevation e = NULL;
  
	  if ( !e )
	    e = newObject(ClassElevation, ONE, 0);
  
	  bx -= 4; by -= 4; bw += 8; bh += 8;
	  r_3d_box(bx, by, bw, bh, valInt(b->radius), e, FALSE);
	}
	if ( kbf && focus )
	{ int pen = valInt(b->pen);
  
	  bx -= pen; by -= pen; bw += 2*pen; bh += 2*pen;
	  r_thickness(pen);
	  r_box(bx, by, bw, bh, valInt(b->radius), NIL);
	}
      } else
      { if ( defb )
	{ int pen = valInt(b->pen);
  
	  bx -= pen; by -= pen; bw += 2*pen; bh += 2*pen;
	  r_thickness(pen);
	  r_box(bx, by, bw, bh, valInt(b->radius), NIL);
	}
      }
    }

    r_3d_box(x, y, w, h, valInt(b->radius), z, up);

    if ( notNil(b->popup) )
    { if ( b->look == NAME_motif )
      { int bw = 12;
	int bh = 8;

	rm = bw+8;
	r_3d_box(x+w-bw-8, y+(h-bh)/2, bw, bh, 0, z, TRUE);
      } else
      { int th = (b->look == NAME_win ? 5 : 8);
	int tw = (b->look == NAME_win ? 7 : 9);
	int tx, ty;

	rm = tw+8;
	tx = x+w-rm;
	ty = y + (h-th)/2;

	if ( b->look == NAME_win )
	{ r_fillpattern(BLACK_COLOUR);
	  r_fill_triangle(tx+tw/2, ty+th, tx, ty, tx+tw, ty);
	} else
	  r_3d_triangle(tx+tw/2, ty+th, tx, ty, tx+tw, ty, z, up, 0x3);
      }
    }

    str_string(&b->label->data, b->label_font, x, y, w-rm, h,
	       NAME_center, NAME_center);
  } else				/* x, 2D-open_look */
  { int swapc  = 0;
    int pen    = valInt(b->pen);
    int radius = valInt(b->radius);
    int shadow = valInt(b->shadow);

    if ( defb && b->look != NAME_openLook )
      pen++;

    r_thickness(pen);
    r_dash(b->texture);

    if ( b->status == NAME_inactive || b->status == NAME_active )
    { r_shadow_box(x, y, w, h, radius, shadow, NIL);
    } else if ( b->status == NAME_preview )
    { r_shadow_box(x, y, w, h, radius, shadow, BLACK_IMAGE);
      swapc = TRUE;
    } else if ( b->status == NAME_execute )
    { r_shadow_box(x, y, w, h, radius, shadow, GREY25_IMAGE);
    }

    if ( swapc )
      r_swap_background_and_foreground();
  
    if ( defb && b->look == NAME_openLook )
    { int g = 0;			/* ??? */
      r_box(x+pen+g, y+pen+g, w-2*pen-2*g-shadow, h-2*pen-2*g-shadow,
	    radius, NIL);
    }

    if ( notNil(b->popup) && notNil(b->popup_image) )
    { int iw = valInt(b->popup_image->size->w);
      int ih = valInt(b->popup_image->size->h);

      rm = iw+8;
      r_image(b->popup_image, 0, 0, x+w-rm, y + (h-ih)/2, iw, ih, ON);
    }

    str_string(&b->label->data, b->label_font, x, y, w-rm, h,
	       NAME_center, NAME_center);

    if ( swapc )
      r_swap_background_and_foreground();
  }

  return RedrawAreaGraphical(b, a);
}


static status
computeButton(Button b)
{ if ( notNil(b->request_compute) )
  { Size size = getResourceValueObject(b, NAME_size);
    int w, h;

    TRY(obtainResourcesObject(b));

    str_size(&b->label->data, b->label_font, &w, &h);
    h += 6; w += 10;
    if ( notNil(b->popup) )
    { if ( notNil(b->popup->popup_image) )
	w += valInt(b->popup->popup_image->size->w) + 5;
      else if ( b->look == NAME_motif )
	w += 12 + 5;
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
  { int fh, ascent, h;

    ComputeGraphical(b);
    fh     = valInt(getHeightFont(b->label_font));
    ascent = valInt(getAscentFont(b->label_font));
    h      = valInt(b->area->h);

    ref = answerObject(ClassPoint, ZERO, toInt((h - fh)/2 + ascent), 0);
  }
  
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
		 NAME_left, DEFAULT, DEFAULT,
		 newObject(ClassMessage, RECEIVER, NAME_execute, 0),
		 newObject(ClassMessage, RECEIVER, NAME_status,NAME_preview,0),
		 newObject(ClassMessage, RECEIVER, NAME_cancel, 0),
		 0);

  assert(GESTURE_button);
  succeed;
}


static status
WantsKeyboardFocusButton(Button b)
{ if ( b->active == ON && (b->look == NAME_motif || b->look == NAME_win) )
    succeed;

  fail;
}


static status
eventButton(Button b, EventObj ev)
{ int infocus;

  if ( eventDialogItem(b, ev) )
    succeed;

  infocus = (getKeyboardFocusGraphical((Graphical) b) == ON);

  if ( ev->id == toInt(9) && infocus )
  { advanceDevice(b->device, (Graphical) b);
    succeed;
  }

  if ( b->active == ON )
  { makeButtonGesture();

    if ( ev->id == toInt(13) && infocus ) /* RETURN */
    { send(b, NAME_execute, 0);
      succeed;
    }
    
    if ( isAEvent(ev, NAME_msLeftDown) && !infocus )
      send(b, NAME_keyboardFocus, ON, 0);

    if ( isAEvent(ev, NAME_focus) )
    { changedDialogItem(b);
      succeed;
    }

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
labelFontButton(Button b, FontObj font)
{ return assignGraphical(b, NAME_labelFont, font);
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

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static const char *T_initialise[] =
        { "name=name", "message=[code]*", "label=[name]" };

/* Instance Variables */

static const vardecl var_button[] =
{ SV(NAME_radius, "int", IV_GET|IV_STORE, radiusButton,
     NAME_appearance, "Rounding radius for corners"),
  SV(NAME_shadow, "int", IV_GET|IV_STORE, shadowButton,
     NAME_appearance, "Shadow shown around the box"),
  SV(NAME_labelFont, "font", IV_GET|IV_STORE, labelFontButton,
     NAME_appearance, "Font of command text"),
  IV(NAME_accelerator, "key=name*", IV_BOTH,
     NAME_accelerator, "Activate when ->key: name is received"),
  SV(NAME_popupImage, "image*", IV_GET|IV_STORE, popupImageButton,
     NAME_appearance, "Indication that button has a popup menu")
};

/* Send Methods */

static const senddecl send_button[] =
{ SM(NAME_compute, 0, NULL, computeButton,
     DEFAULT, "Compute desired size (from command)"),
  SM(NAME_event, 1, "event", eventButton,
     DEFAULT, "Process an event"),
  SM(NAME_initialise, 3, T_initialise, initialiseButton,
     DEFAULT, "Create from name and command"),
  SM(NAME_status, 1, "{inactive,active,preview,execute}", statusButton,
     DEFAULT, "Status for event-processing"),
  SM(NAME_popup, 1, "popup*", popupButton,
     DEFAULT, "Associated popup menu"),
  SM(NAME_defaultButton, 1, "bool", defaultButtonButton,
     NAME_accelerator, "@on sets <-accelerator to `RET'"),
  SM(NAME_key, 1, "key=name", keyButton,
     NAME_accelerator, "Handle accelerator key `name'"),
  SM(NAME_execute, 0, NULL, executeButton,
     NAME_action, "Execute associated command"),
  SM(NAME_font, 1, "font", labelFontButton,
     NAME_appearance, "same as ->label_font"),
  SM(NAME_WantsKeyboardFocus, 0, NULL, WantsKeyboardFocusButton,
     NAME_event, "Test if ready to accept input"),
  SM(NAME_selection, 1, "name", labelDialogItem,
     NAME_label, "Equivalent to ->label")
};

/* Get Methods */

static const getdecl get_button[] =
{ GM(NAME_popup, 1, "popup*", "create=[bool]", getPopupButton,
     DEFAULT, "Associated popup (make one if create = @on)"),
  GM(NAME_reference, 0, "point", NULL, getReferenceButton,
     DEFAULT, "Left, baseline of label"),
  GM(NAME_defaultButton, 0, "bool", NULL, getDefaultButtonButton,
     NAME_accelerator, "@on iff <-accelerator == `RET'"),
  GM(NAME_selection, 0, "name", NULL, getSelectionButton,
     NAME_label, "Equivalent to <-label")
};

/* Resources */

static const resourcedecl rc_button[] =
{ RC(NAME_alignment, "{column,left,center,right}", "left",
     "Alignment in the row"),
  RC(NAME_labelSuffix, "name", "",
     "Ensured suffix of label"),
  RC(NAME_pen, "int", "2",
     "Thickness of box"),
  RC(NAME_popupImage, "image*", "@nil",
     "Image to indicate presence of popup menu"),
  RC(NAME_radius, "int", "4",
     "Rounding radius of box"),
  RC(NAME_shadow, "int", "0",
     "Shadow shown around the box"),
  RC(NAME_size, "size", "size(80,20)",
     "Minimum size in pixels")
};

/* Class Declaration */

static Name button_termnames[] = { NAME_label, NAME_message, NAME_accelerator };

ClassDecl(button_decls,
          var_button, send_button, get_button, rc_button,
          3, button_termnames,
          "$Rev$");


status
makeClassButton(Class class)
{ declareClass(class, &button_decls);
  setRedrawFunctionClass(class, RedrawAreaButton);

  succeed;
}

