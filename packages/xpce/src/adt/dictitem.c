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
initialiseDictItem(DictItem di, Name key, Name str, Any obj, Name style)
{ assign(di, key, key);
  assign(di, label, str);
  assign(di, index, ZERO);
  assign(di, object, (isDefault(obj) ? NIL : obj));
  assign(di, dict, NIL);
  assign(di, style, style);

  succeed;
}


static status
unlinkDictItem(DictItem di)
{ if ( notNil(di->dict) )
    return deleteDict(di->dict, di);

  succeed;
}


static DictItem
getConvertDictItem(Class class, Name name)
{ answer(newObject(ClassDictItem, name, 0));
}


static status
labelDictItem(DictItem di, Name str)
{ assign(di, label, str);

  if (notNil(di->dict) && notNil(di->dict->browser))
    send(di->dict->browser, NAME_ChangeItem, di, 0);

  succeed;
}


static Name
getLabelDictItem(DictItem di)
{ answer(isDefault(di->label) ? di->key : di->label);
}


static status
keyDictItem(DictItem di, Name key)
{ if (notNil(di->dict))
  { deleteHashTable(di->dict->table, di->key);
    assign(di, key, key);
    appendHashTable(di->dict->table, di->key, di);
  }

  if (notNil(di->dict) && notNil(di->dict->browser) && isDefault(di->label))
    send(di->dict->browser, NAME_ChangeItem, di, 0);

  succeed;
}


static status
styleDictItem(DictItem di, Name style)
{ assign(di, style, style);

  if ( notNil(di->dict) && notNil(di->dict->browser) )
    send(di->dict->browser, NAME_ChangeItem, di, 0);

  succeed;
}


static status
dictDictItem(DictItem di, Dict d)
{ status rval;

  addCodeReference(di);
  if ( notNil(di->dict) )
    deleteDict(di->dict, di);
  rval = appendDict(d, di);
  delCodeReference(di);

  return rval;
}


static Any
getImageDictItem(DictItem di)
{ Dict d;
  Any browser;

  if ( notNil(d = di->dict) &&
       notNil(browser = d->browser) )
    answer(browser);

  fail;
}  


static Point
getPositionDictItem(DictItem di)
{ ListBrowser lb;

  if ( (lb = getImageDictItem(di)) )
  { int index = valInt(di->index) * BROWSER_LINE_WIDTH;
    int x, y, w, h, b;

    if ( get_character_box_textimage(lb->image, index,
				     &x, &y, &w, &h, &b) )
    { x += valInt(lb->image->area->x);
      y += valInt(lb->image->area->y);

      answer(answerObject(ClassPoint, toInt(x), toInt(y), 0));
    }
  }

  fail;
}

		/********************************
		*             VISUAL		*
		********************************/

static Any
getContainedInDictItem(DictItem di)
{ Dict d;

  if ( notNil(d = di->dict) )
    answer(di->dict);

  fail;
}


status
makeClassDictItem(Class class)
{ sourceClass(class, makeClassDictItem, __FILE__, "$Revision$");

  localClass(class, NAME_key, NAME_value, "name", NAME_get,
	     "Key used to index from dict");
  localClass(class, NAME_label, NAME_appearance, "[name]", NAME_none,
	     "Label used to display in browser");
  localClass(class, NAME_object, NAME_delegate, "any", NAME_both,
	     "Associated data");
  localClass(class, NAME_style, NAME_appearance, "[name]", NAME_get,
	     "Display style for item");
  localClass(class, NAME_index, NAME_order, "int", NAME_get,
	     "Index in dict (0-based)");
  localClass(class, NAME_dict, NAME_organisation, "dict*", NAME_get,
	     "Dict holding me");

  termClass(class, "dict_item", 3, NAME_key, NAME_label, NAME_object);
  delegateClass(class, NAME_object);

  storeMethod(class, NAME_label, labelDictItem);
  storeMethod(class, NAME_key,   keyDictItem);
  storeMethod(class, NAME_style, styleDictItem);
  storeMethod(class, NAME_dict,  dictDictItem);

  sendMethod(class, NAME_initialise, DEFAULT,
	     4, "key=name", "label=[name]", "object=[any]*", "style=[name]",
	     "Create from key, label, object and style",
	     initialiseDictItem);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Delete from dict",
	     unlinkDictItem);

  getMethod(class, NAME_label, DEFAULT, "name", 0,
	    "<-label or <-key if <-label == @default",
	    getLabelDictItem);
  getMethod(class, NAME_image, NAME_popup, "list_browser", 0,
	    "<-browser of the <-dict (for popup menu's)",
	    getImageDictItem);
  getMethod(class, NAME_position, NAME_area, "point", 0,
	    "Position in coordinate-system of list_browser",
	    getPositionDictItem);
  getMethod(class, NAME_containedIn, DEFAULT, "dict", 0,
	    "dict object I'm contained in",
	    getContainedInDictItem);
  getMethod(class, NAME_convert, DEFAULT, "dict_item", 1, "name",
	    "Convert name to dict_item",
	    getConvertDictItem);

  succeed;
}

