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
initialiseDictItem(DictItem di, Any key, CharArray lbl, Any obj, Name style)
{ if ( instanceOfObject(key, ClassCharArray) && !isName(key) )
    key = toName(key);

  assign(di, key, key);
  assign(di, label, lbl);
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
getConvertDictItem(Class class, Any key)
{ answer(newObject(ClassDictItem, key, 0));
}


static status
labelDictItem(DictItem di, CharArray str)
{ assign(di, label, str);

  if (notNil(di->dict) && notNil(di->dict->browser))
    send(di->dict->browser, NAME_ChangeItem, di, 0);

  succeed;
}


CharArray
getLabelDictItem(DictItem di)
{ if ( isDefault(di->label) )
  { if ( instanceOfObject(di->key, ClassCharArray) )
      answer(di->key);
    else if ( isInteger(di->key) )	/* not an object! */
    { string s;

      toString(di->key, &s);

      return (CharArray) StringToString(&s);
    } else
      answer(qadGetv(di->key, NAME_printName, 0, NULL));
  } else
    answer(di->label);
}


static status
keyDictItem(DictItem di, Any key)
{ if ( notNil(di->dict) && notNil(di->dict->table) )
  { deleteHashTable(di->dict->table, di->key);
    assign(di, key, key);
    appendHashTable(di->dict->table, di->key, di);
  } else
    assign(di, key, key);

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

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "key=any", "label=[char_array]", "object=[any]*", "style=[name]" };

/* Instance Variables */

static vardecl var_dictItem[] =
{ SV(NAME_key, "any", IV_GET|IV_STORE, keyDictItem,
     NAME_value, "Key used to index from dict"),
  SV(NAME_label, "[char_array]", IV_NONE|IV_STORE, labelDictItem,
     NAME_appearance, "Label used to display in browser"),
  IV(NAME_object, "any", IV_BOTH,
     NAME_delegate, "Associated data"),
  SV(NAME_style, "[name]", IV_GET|IV_STORE, styleDictItem,
     NAME_appearance, "Display style for item"),
  IV(NAME_index, "int", IV_GET,
     NAME_order, "Index in dict (0-based)"),
  SV(NAME_dict, "dict*", IV_GET|IV_STORE, dictDictItem,
     NAME_organisation, "Dict holding me")
};

/* Send Methods */

static senddecl send_dictItem[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseDictItem,
     DEFAULT, "Create from key, label, object and style"),
  SM(NAME_unlink, 0, NULL, unlinkDictItem,
     DEFAULT, "Delete from dict")
};

/* Get Methods */

static getdecl get_dictItem[] =
{ GM(NAME_containedIn, 0, "dict", NULL, getContainedInDictItem,
     DEFAULT, "dict object I'm contained in"),
  GM(NAME_convert, 1, "dict_item", "any", getConvertDictItem,
     DEFAULT, "Convert <-key to dict_item"),
  GM(NAME_label, 0, "char_array", NULL, getLabelDictItem,
     DEFAULT, "<-label<-print_name or <-key if <-label == @default"),
  GM(NAME_position, 0, "point", NULL, getPositionDictItem,
     NAME_area, "Position in coordinate-system of list_browser"),
  GM(NAME_image, 0, "list_browser", NULL, getImageDictItem,
     NAME_popup, "<-browser of the <-dict (for popup menu's)")
};

/* Resources */

#define rc_dictItem NULL
/*
static resourcedecl rc_dictItem[] =
{ 
};
*/

/* Class Declaration */

static Name dictItem_termnames[] = { NAME_key, NAME_label, NAME_object };

ClassDecl(dictItem_decls,
          var_dictItem, send_dictItem, get_dictItem, rc_dictItem,
          3, dictItem_termnames,
          "$Rev$");

status
makeClassDictItem(Class class)
{ declareClass(class, &dictItem_decls);
  delegateClass(class, NAME_object);

  succeed;
}

