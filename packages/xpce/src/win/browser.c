/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/text.h>

static status
initialiseBrowser(Browser b, Name name, Size size, DisplayObj display)
{ ListBrowser lb;
  TileObj t;

  if ( isDefault(size) )
    size = getResourceValueObject(b, NAME_size);

  TRY(lb = newObject(ClassListBrowser, DEFAULT, size->w, size->h, 0) );
  initialiseWindow((PceWindow)b, name,
		   getSizeGraphical((Graphical) lb), display);
  t = get(b, NAME_tile, 0);
  assign(t, horShrink,  ZERO);
  assign(t, horStretch, ZERO);
  assign(b, list_browser, lb);
  send(lb, NAME_set, ZERO, ZERO, 0);
  send(b, NAME_display, lb, 0);
  send(b, NAME_resizeMessage,
       newObject(ClassMessage, lb, NAME_Size, Arg(2), 0),
       0);
  assign(b, keyboard_focus, lb);

  succeed;
}


static status
unlinkBrowser(Browser b)
{ ListBrowser lb = b->list_browser;

  assign(b, list_browser, NIL);
  freeObject(lb);

  return unlinkWindow((PceWindow) b);
}

		/********************************
		*           OVERRULE		*
		********************************/

status
requestGeometryBrowser(Browser b, Int x, Int y, Int w, Int h)
{ ListBrowser lb = b->list_browser;

  if ( notDefault(w) )
    w = mul(w, getExFont(lb->font));
  if ( notDefault(h) )
    h = mul(h, getHeightFont(lb->font));

  return requestGeometryWindow((PceWindow) b, x, y, w, h);
}

static Any
getSelectionBrowser(Browser b)
{ answer(getSelectionListBrowser(b->list_browser));
}

static status
selectionBrowser(Browser b, Any obj)
{ return selectionListBrowser(b->list_browser, obj);
}

static status
selectedBrowser(Browser b, Any obj)
{ return selectedListBrowser(b->list_browser, obj);
}

static status
scrollToBrowser(Browser b, Int line)
{ return scrollToListBrowser(b->list_browser, line);
}


static status
normaliseBrowser(Browser b, Any obj)
{ return normaliseListBrowser(b->list_browser, obj);
}


static status
clearBrowser(Browser b)
{ return clearDict(b->list_browser->dict);
}


static status
cursorBrowser(Browser b, CursorObj cursor)
{ return send(b->list_browser->image, NAME_cursor, cursor, 0);
}


static status
popupBrowser(Browser b, PopupObj pop)
{ assign(b->list_browser, popup, pop);

  succeed;
}


static PopupObj
getPopupBrowser(Browser b)
{ answer(b->list_browser->popup);
}


static DictItem
getMemberBrowser(Browser b, Any key)
{ answer(getMemberListBrowser(b->list_browser, key));
}


static Size
getSizeBrowser(Browser b)
{ answer(getSizeListBrowser(b->list_browser));
}

		/********************************
		*            VISUAL		*
		********************************/

static Chain
getContainsBrowser(Browser b)
{ answer(getContainsListBrowser(b->list_browser));
}

status
makeClassBrowser(Class class)
{ sourceClass(class, makeClassBrowser, __FILE__, "$Revision$");

  localClass(class, NAME_listBrowser, NAME_delegate, "list_browser", NAME_get,
	     "Displayed list_browser");

  termClass(class, "browser", 1, NAME_label, NAME_size, NAME_display);
  prependDelegateClass(class, NAME_listBrowser);

  storeMethod(class, NAME_popup, popupBrowser);
  storeMethod(class, NAME_cursor, cursorBrowser);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "label=[name]", "size=[size]", "display=[display]",
	     "Create from label, size and display",
	     initialiseBrowser);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Delete the list_browser",
	     unlinkBrowser);
  sendMethod(class, NAME_scrollTo, NAME_scroll, 1, "int",
	     "Scroll to nth-1 item",
	     scrollToBrowser);
  sendMethod(class, NAME_normalise, NAME_scroll, 1, "member:dict_item",
	     "Ensure (named) item is visible",
	     normaliseBrowser);
  sendMethod(class, NAME_clear, NAME_delete, 0,
	     "Delete all items",
	     clearBrowser);
  sendMethod(class, NAME_selection, NAME_selection, 1,
	     "member:dict_item|chain*",
	     "Set selected items",
	     selectionBrowser);
  sendMethod(class, NAME_selected, NAME_selection, 1, "member:dict_item",
	     "Test if object is selected",
	     selectedBrowser);
  sendMethod(class, NAME_requestGeometry, DEFAULT, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Map size to character units",
	     requestGeometryBrowser);

  getMethod(class, NAME_selection, NAME_selection, "chain|dict_item*", 0,
	    "Get selected items",
	    getSelectionBrowser);
  getMethod(class, NAME_size, NAME_area, "characters=size", 0,
	    "Size in character units",
	    getSizeBrowser);
  getMethod(class, NAME_popup, NAME_menu, "popup*", 0,
	    "Get popup menu of the list_browser",
	    getPopupBrowser);
  getMethod(class, NAME_member, NAME_lookup, "dict_item", 1, "name",
	    "Dict_item with given key value",
	    getMemberBrowser);
  getMethod(class, NAME_contains, DEFAULT, "chain", 0,
	    "The dict object contained",
	    getContainsBrowser);

  attach_resource(class, "pen",    "int",   "0",
		  "Pen (done by <-list_browser)");
  attach_resource(class, "size",   "size",  "size(25,10)",
		  "Size in `characters x lines'");

  succeed;
}

