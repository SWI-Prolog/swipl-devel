/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/dialog.h>

static status closePopup(PopupObj);

static status
initialisePopup(PopupObj p, Name label, Code msg)
{ if ( isDefault(label) )
    label = NAME_options;

  assign(p, update_message, NIL);
  assign(p, button,	    NAME_right);
  assign(p, default_item,   DEFAULT);	/* resource */
  assign(p, show_current,   OFF);
  initialiseMenu((Menu) p, label, NAME_popup, msg);

  succeed;
}


		/********************************
		*             WINDOW		*
		********************************/


static Chain windows = NIL;

static PceWindow
createPopupWindow(DisplayObj d)
{ Cell cell;
  PceWindow sw;
  Any frame;

  if ( isNil(windows) )
    windows = globalObject(NAME_PopupWindows, ClassChain, 0);

  for_cell(cell, windows)
  { sw = cell->value;

    if ( emptyChain(sw->graphicals) && sw->frame->display == d )
      return sw;
  }


  sw = newObject(ClassDialog, NAME_popup, DEFAULT, d, 0);

  send(sw, NAME_kind, NAME_popup, 0);
  send(sw, NAME_pen, ZERO, 0);
  send(sw, NAME_create, 0);
  frame = get(sw, NAME_frame, 0);
  send(frame, NAME_border, ONE, 0);
  send(getTileFrame(frame), NAME_border, ZERO, 0);

  appendChain(windows, sw);
  
  return sw;
}


		/********************************
		*            UPDATE		*
		********************************/

static Any updateContext;		/* HACK of pullright menus */

static status
updatePopup(PopupObj p, Any context)
{ updateContext = context;

  if ( notNil(p->update_message) )
    forwardReceiverCode(p->update_message, p, context, 0);

  return updateMenu((Menu) p, context);
}


static status
resetPopup(PopupObj p)
{ return closePopup(p);
}


static MenuItem
getDefaultMenuItemPopop(PopupObj p)
{ Cell cell;

  if ( isNil(p->default_item) ||
       equalName(p->default_item, NAME_first) )
  { for_cell(cell, p->members)
    { MenuItem mi = cell->value;

      if ( mi->active == ON )
	answer(mi);
    }

    fail;
  }

  if ( equalName(p->default_item, NAME_selection) )
  { for_cell(cell, p->members)
    { MenuItem mi = cell->value;

      if ( mi->selected == ON )
	answer(mi);
    }

    fail;
  }

  answer(findMenuItemMenu((Menu) p, (Any) p->default_item));
}

		/********************************
		*           OPEN/CLOSE		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Show a popup on a graphical.  Pos is the position relative to the graphical
on which to display the popup.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
openPopup(PopupObj p, Graphical gr, Point pos,
	  Bool pos_is_pointer, Bool warp_pointer, Bool ensure_on_display)
{ int dw, dh;				/* Display width and height */
  PceWindow sw;
  int moved = FALSE;			/* Cursor needs be moved */
  int cx, cy;				/* mouse X-Y */
  int px, py;				/* Popup X-Y */
  int pw, ph;				/* Popup W-H */
  int dx, dy;				/* Popup-Pointer offset */
  Point offset;
  DisplayObj d = CurrentDisplay(gr);
  MenuItem mi;

  if ( emptyChain(p->members) )
    fail;

  if ( isDefault(pos_is_pointer) )	pos_is_pointer = ON;
  if ( isDefault(warp_pointer) )	warp_pointer = ON;
  if ( isDefault(ensure_on_display) )	ensure_on_display = ON;

  dw = valInt(getWidthDisplay(d));
  dh = valInt(getHeightDisplay(d));

  sw = createPopupWindow(d);
  send(sw, NAME_display, p, 0);

  if ( !(offset = getDisplayPositionGraphical(gr)) )
    return errorPce(p, NAME_graphicalNotDisplayed, gr);

  plusPoint(pos, offset);
  doneObject(offset);

					/* get sizes and coordinates */
  ComputeGraphical((Graphical) p);
  dy = valInt(p->area->y);
  dx = valInt(p->area->x);

  if ( (mi = getDefaultMenuItemPopop(p)) != FAIL )
  { int ix, iy, iw, ih;
    area_menu_item((Menu) p, mi, &ix, &iy, &iw, &ih);
    dy += iy +ih/2;
    dx += ix;
  } else
  { mi = NIL;
    dy += 10;
  }

  if ( notNil(p->default_item) )
  { dx += 2;
    previewMenu((Menu) p, mi);
  } else
  { dx = -4;
    previewMenu((Menu) p, NIL);
  }
  pw = valInt(p->area->w);
  ph = valInt(p->area->h);

  if ( pos_is_pointer == ON )
  { cx = valInt(pos->x);
    cy = valInt(pos->y);
    px = cx - dx;
    py = cy - dy;
  } else
  { px = valInt(pos->x);
    py = valInt(pos->y);
    cx = px + dx;
    cy = py + dy;
    moved = TRUE;
  }

  if ( ensure_on_display == ON )
  { if ( px < 0 )       moved = TRUE, px = 0;
    if ( py < 0 )       moved = TRUE, py = 0;
    if ( px + pw > dw ) moved = TRUE, px = dw - pw;
    if ( py + ph > dh ) moved = TRUE, py = dh - ph;
  }

  send(get(sw, NAME_frame, 0), NAME_set,
       toInt(px), toInt(py), toInt(pw), toInt(ph), 0);
  send(sw, NAME_show, ON, 0);
  if ( moved && warp_pointer == ON )
  { Point pos = tempObject(ClassPoint, toInt(dx), toInt(dy), 0);
    send(sw, NAME_pointer, pos, 0);
    considerPreserveObject(pos);
  }

  send(sw, NAME_sensitive, ON, 0);

  succeed;
}


static status
closePopup(PopupObj p)
{ PceWindow sw;

  if ( notNil(p->pullright) )
  { send(p->pullright, NAME_close, 0);
    assign(p, pullright, NIL);
  }

  if ( notNil(sw = (PceWindow) p->device) )
  { send(sw, NAME_show, OFF, 0);
    send(sw, NAME_sensitive, OFF, 0);
    send(sw, NAME_clear, 0);
    assign(p, displayed, OFF);
  }

  succeed;
}


		/********************************
		*         EVENT HANDLING	*
		********************************/

status
keyPopup(PopupObj p, Name key)
{ Cell cell;

  for_cell(cell, p->members)
  { MenuItem mi = cell->value;
    
    if ( (mi->accelerator == key && mi->active == ON) ||
	 (notNil(mi->popup) && keyPopup(mi->popup, key)) )
    { assign(p, selected_item, mi);
      succeed;
    }
  }

  fail;
}


static status
execute_popup(PopupObj p, Any context)
{ if ( p->kind == NAME_cyclePopup )
  { Menu m = context;

    if ( instanceOfObject(m, ClassMenu) )
    { if ( notNil(p->selected_item) )
      { selectionMenu(m, p->selected_item);
	flushGraphical(m);
	forwardMenu(m, m->message, EVENT->value);
      }
    } else
      return errorPce(context, NAME_unexpectedType, ClassMenu);
  } else
  { Code def_msg = DEFAULT;

    for( ; instanceOfObject(p, ClassPopup); p = p->selected_item )
    { if ( notDefault(p->message) )
	def_msg = p->message;

      if ( instanceOfObject(p->selected_item, ClassMenuItem) )
      { MenuItem mi = p->selected_item;
    
	if ( p->multiple_selection == ON )
	{ toggleMenu((Menu) p, mi);
	  if ( isDefault(mi->message) )
	  { if ( notDefault(def_msg) && notNil(def_msg) )
	      forwardReceiverCode(def_msg, p,
				  mi->value, mi->selected, context, 0);
	  } else if ( notNil(mi->message) )
	    forwardReceiverCode(mi->message, p, mi->selected, context, 0);
	} else
	{ if ( isDefault(mi->message) )
	  { if ( notDefault(def_msg) && notNil(def_msg) )
	      forwardReceiverCode(def_msg, p, mi->value, context, 0);
	  } else if ( notNil(mi->message) )
	    forwardReceiverCode(mi->message, p, context, 0);
	}

	succeed;
      }
    }
  }

  succeed;
}


static status
executePopup(PopupObj p, Any context)
{ status rval;
  DisplayObj d = CurrentDisplay(context);

  busyCursorDisplay(d, DEFAULT, DEFAULT);
  rval = execute_popup(p, context);
  busyCursorDisplay(d, NIL, DEFAULT);

  return rval;
}


static int
pullright_x_offset(PopupObj p)
{ int ix = valInt(p->item_offset->x) +
	   valInt(p->item_size->w) -
	   valInt(p->border);

  if ( notNil(p->popup_image) )
    ix -= valInt(p->popup_image->size->w);
  else
    ix -= 8;

  return ix;
}


static status
showPullrightMenuPopup(PopupObj p, MenuItem mi, EventObj ev, Any context)
{ if ( isDefault(context) && validPceDatum(updateContext) )
    context = updateContext;

  send(mi->popup, NAME_update, context, 0);

  if ( !emptyChain(mi->popup->members) )
  { Point pos;		/* Create PULLRIGHT */
    int ih = valInt(p->item_size->h);
    int ic = valInt(getCenterYMenuItemMenu((Menu)p, mi));
    int ix = pullright_x_offset(p);
	    
    previewMenu((Menu) p, mi);
    pos = tempObject(ClassPoint, toInt(ix), toInt(ic - ih/2), 0);
	    
    assign(p, pullright, mi->popup);
    send(p->pullright, NAME_open, p, pos, OFF, OFF, ON, 0);
    considerPreserveObject(pos);
    assign(p->pullright, button, p->button);
    if ( notDefault(ev) )
      postEvent(ev, (Graphical) p->pullright, DEFAULT);

    succeed;
  }

  fail;
}


static status
dragPopup(PopupObj p, EventObj ev, Bool check_pullright)
{ MenuItem mi;

  if ( !(mi = getItemFromEventMenu((Menu) p, ev)) )
    previewMenu((Menu) p, NIL);
  else
  { if ( mi->active == ON )
    { previewMenu((Menu) p, mi);

      if ( notNil(mi->popup) && check_pullright != OFF )
      { Int ex, ey;
	int ix = pullright_x_offset(p);

	get_xy_event(ev, p, ON, &ex, &ey);
	if ( valInt(ex) > ix )
	  send(p, NAME_showPullrightMenu, mi, ev, 0);
      }
    } else
      previewMenu((Menu) p, NIL);
  }

  succeed;
}


static status
eventPopup(PopupObj p, EventObj ev)
{ 					/* Showing PULLRIGHT menu */
  if ( notNil(p->pullright) )
  { postEvent(ev, (Graphical) p->pullright, DEFAULT);

    if ( isDragEvent(ev) )
    { if ( isNil(p->pullright->preview) )
      { MenuItem mi;

	if ( (mi = getItemFromEventMenu((Menu) p, ev)) )
	{ send(p->pullright, NAME_close, 0);
	  assign(p, pullright, NIL);
	  return send(p, NAME_drag, ev, 0);
	}
      }
    } else if ( isUpEvent(ev) && getButtonEvent(ev) == p->pullright->button )
    { if ( notNil(p->pullright->selected_item) )
    	assign(p, selected_item, p->pullright);
      else
      	assign(p, selected_item, NIL);
      assign(p, pullright, NIL);
      send(p, NAME_close, 0);
    }

    succeed;
  }

					/* UP: execute */
  if ( isUpEvent(ev) )
  { if ( notNil(p->preview) &&
	 notNil(p->preview->popup) &&
	 valInt(getClickTimeEvent(ev)) < 400 &&
	 valInt(getClickDisplacementEvent(ev)) < 10 )
      send(p, NAME_showPullrightMenu, p->preview, 0);
    else if ( getButtonEvent(ev) == p->button )
    { assign(p, selected_item, p->preview);
      send(p, NAME_close, 0);
      succeed;
    }
  } else if ( isDownEvent(ev) )		/* DOWN: set button */
  { assign(p, selected_item, NIL);
    assign(p, button, getButtonEvent(ev));
    send(p, NAME_drag, ev, OFF, 0);
    succeed;
  } else if ( isDragEvent(ev) )		/* DRAG: highlight entry */
  { send(p, NAME_drag, ev, 0);
    succeed;
  } else if ( isAEvent(ev, NAME_locMove) )
  { MenuItem mi = getItemFromEventMenu((Menu) p, ev);

    previewMenu((Menu) p, mi && mi->active == ON ? mi : NIL);
  }

  fail;
}


		/********************************
		*            MENU ITEM		*
		********************************/


static status
endGroupPopup(PopupObj p, Bool val)
{ if ( notNil(p->context) )
    return send(p->context, NAME_endGroup, val, 0);

  fail;
}


status
defaultPopupImages(PopupObj p)
{ if ( p->show_current == ON )
  { if ( p->multiple_selection == ON && p->look == NAME_win )
      assign(p, on_image, MS_MARK_IMAGE);
    else
      assign(p, on_image, NAME_marked);
  } else
    assign(p, on_image, NIL);

  assign(p, off_image, NIL);

  succeed;
}


static status
showCurrentPopup(PopupObj p, Bool show)
{ assign(p, show_current, show);

  return defaultPopupImages(p);
}


status
makeClassPopup(Class class)
{ sourceClass(class, makeClassPopup, __FILE__, "$Revision$");

  localClass(class, NAME_context, NAME_context, "any*", NAME_both,
	     "Invoking context");
  localClass(class, NAME_updateMessage, NAME_active, "code*", NAME_both,
	     "Ran just before popup is displayed");
  localClass(class, NAME_pullright, NAME_part, "popup*", NAME_none,
	     "Currently shown pullright menu");
  localClass(class, NAME_selectedItem, NAME_selection,
	     "menu_item|popup*", NAME_get,
	     "Selected menu-item/sub-popup");
  localClass(class, NAME_button, NAME_event, "button_name", NAME_get,
	     "Name of invoking button");
  localClass(class, NAME_defaultItem, NAME_appearance, 
	     "{first,selection}|any*", NAME_both,
	     "Initial previewed item");
  localClass(class, NAME_showCurrent, NAME_appearance, "bool", NAME_get,
	     "If @on, show the currently selected value");

  termClass(class, "popup", 2, NAME_name, NAME_message);

  storeMethod(class, NAME_showCurrent, showCurrentPopup);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "name=[name]", "message=[code]*",
	     "Create from name and message",
	     initialisePopup);
  sendMethod(class, NAME_reset, NAME_reset, 0,
	     "Close popup after an abort",
	     resetPopup);
  sendMethod(class, NAME_event, DEFAULT, 1, "event",
	     "Handle an event",
	     eventPopup);
  sendMethod(class, NAME_drag, NAME_event, 2,
	     "event", "check_pullright=[bool]",
	     "Handle a drag event",
	     dragPopup);
  sendMethod(class, NAME_showPullrightMenu, NAME_event, 3,
	     "item=menu_item", "event=[event]", "context=[any]",
	     "Show pullright for this item",
	     showPullrightMenuPopup);
  sendMethod(class, NAME_update, NAME_active, 1, "context=any",
	     "Update entries using context object)",
	     updatePopup);
  sendMethod(class, NAME_open, NAME_open, 5,
	     "on=graphical", "offset=point",
	     "offset_is_pointer=[bool]", "warp=[bool]",
	     "ensure_on_display=[bool]",
	     "Open on point relative to graphical",
	     openPopup);
  sendMethod(class, NAME_close, NAME_open, 0,
	     "Finish after ->open",
	     closePopup);
  sendMethod(class, NAME_execute, NAME_execute, 1, "context=[object]*",
	     "Execute selected message of item",
	     executePopup);
  sendMethod(class, NAME_endGroup, NAME_appearance, 1, "bool",
	     "Pullright: separation line below item in super",
	     endGroupPopup);
  sendMethod(class, NAME_key, NAME_accelerator, 1, "key=name",
	     "Set <-selected_item according to accelerator",
	     keyPopup);

  attach_resource(class, "show_label", "bool", "@off",
		  "Label is visible");
  attach_resource(class, "accelerator_font", "font*", "small",
		  "Show the accelerators");
  attach_resource(class, "default_item", "name*", "first",
		  "Item to select as default");
  attach_resource(class, "cursor",     "cursor",  "right_ptr",
		  "Cursor when popup is active");
  attach_resource(class, "kind",       "name",  "popup",
		  "Menu kind");
  attach_resource(class, "multiple_selection", "bool",  "@off",
		  "Can have multiple selection");
  attach_resource(class, "value_width", "int", "80",
		  "Minimum width in pixels");
  attach_resource(class, "border",	"int", "2",
		  "Default border around items");
  attach_resource(class, "layout",	"name", "vertical",
		  "Put items below each other");
  attach_resource(class, "on_image",   "image*",   "@nil",
		  "Marker for items in selection");
  attach_resource(class, "off_image",  "image*",   "@nil",
		  "Marker for items not in selection");
  attach_resource(class, "popup_image","image*",   "@pull_right_image",
		  "Marker for items with popup");
  attach_resource(class, "feedback",   "name",   "image",
		  "Feedback style");
  attach_resource(class, "preview_feedback", "name", "invert",
		  "Feedback on `preview' item");
  attach_resource(class, "pen", "int", "0",
		  "Thickness of the drawing-pen");
  attach_resource(class, "label_suffix", "name", "",
		  "Ensured suffix of label");

  succeed;
}

