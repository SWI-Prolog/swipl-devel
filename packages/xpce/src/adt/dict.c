/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

#define HASH_DICT_THRESHOLD 50

static status
initialiseDictv(Dict dict, int argc, Any *argv)
{ int i;

  assign(dict, members, newObject(ClassChain, 0));
  assign(dict, browser, NIL);
  assign(dict, table,   NIL);

  for (i = 0; i < argc; i++)
    appendDict(dict, argv[i]);

  succeed;
}


static status
unlinkDict(Dict dict)
{ if ( notNil(dict->browser) && !isFreeingObj(dict->browser) )
    send(dict->browser, NAME_dict, NIL, 0);

  clearDict(dict);
  
  if ( notNil(dict->table) )
  { freeObject(dict->table);
    assign(dict, table, NIL);
  }

  succeed;
}

  
		 /*******************************
		 *	      TABLE		*
		 *******************************/

static HashTable
getTableDict(Dict dict)
{ if ( isNil(dict->table) )
  { Cell cell;

    assign(dict, table, newObject(ClassHashTable, 0));
    for_cell(cell, dict->members)
    { DictItem di = cell->value;
      appendHashTable(dict->table, di->key, di);
    }
  }

  answer(dict->table);
}



		/********************************
		*            RENUMBER		*
		********************************/

static status
renumberDict(Dict dict)
{ int index = 0;
  Cell cell;

  for_cell(cell, dict->members)
  { DictItem di = cell->value;

    if ( di->index != toInt(index) )
      assign(di, index, toInt(index));

    index++;
  }

  succeed;
}

		/********************************
		*           MEMBERSHIP		*
		********************************/

DictItem
getMemberDict(Dict dict, Any obj)
{ if ( instanceOfObject(obj, ClassDictItem) )
  { DictItem di = obj;

    if ( di->dict == dict )
      answer(di);

    fail;
  }

  if ( instanceOfObject(obj, ClassCharArray) )
    obj = toName(obj);

  if ( notNil(dict->table) )
    answer(getMemberHashTable(dict->table, obj));
  else if ( valInt(dict->members->size) > HASH_DICT_THRESHOLD )
    answer(getMemberHashTable(getTableDict(dict), obj));
  else
  { Cell cell;

    for_cell(cell, dict->members)
    { DictItem di = cell->value;
      
      if ( di->key == obj )
	answer(di);
    }

    fail;
  }

  fail;
}


static status
memberDict(Dict dict, Any obj)
{ if ( getMemberDict(dict, obj) )
    succeed;

  fail;
}


		/********************************
		*          APPEND/DELETE	*
		********************************/

status
deleteDict(Dict dict, Any obj)
{ DictItem di;

  if ( !isFreeingObj(dict) )
  { TRY( di = getMemberDict(dict, obj) );

    addCodeReference(dict);
    if ( notNil(dict->browser) && !isFreeingObj(dict->browser) )
      send(dict->browser, NAME_DeleteItem, di, 0);
    if ( notNil(dict->table) )
      deleteHashTable(dict->table, di->key);
    assign(di, dict, NIL);
    deleteChain(dict->members, di);
    renumberDict(dict);
    delCodeReference(dict);
  }

  succeed;
}


status
appendDict(Dict dict, DictItem di)
{ if ( di->dict == dict )
    succeed;

  if ( notNil(di->dict) )
  { addCodeReference(di);
    deleteDict(di->dict, di);
    delCodeReference(di);
  }

  assign(di, dict, dict);
  assign(di, index, dict->members->size);
  if ( notNil(dict->table) )
    appendHashTable(dict->table, di->key, di);
  appendChain(dict->members, di);

  if ( notNil(dict->browser) )
    send(dict->browser, NAME_InsertItem, di, 0);

  succeed;
}


status
membersDict(Dict dict, Chain members)
{ Cell cell;

  TRY(send(dict, NAME_clear, 0));
  for_cell(cell, members)
    TRY(send(dict, NAME_append, cell->value, 0));

  succeed;
}



static status
insertAfterDict(Dict dict, DictItem di, Any after)
{ DictItem a;

  if ( notNil(after) )
  { TRY(a = getMemberDict(dict, after));
  } else
    a = NIL;

  if ( notNil(di->dict) )
  { addCodeReference(di);
    deleteDict(di->dict, di);
    delCodeReference(di);
  }

  assign(di, dict, dict);
  if ( notNil(dict->table) )
    appendHashTable(dict->table, di->key, di);
  insertAfterChain(dict->members, di, a);
  renumberDict(dict);

  if ( notNil(dict->browser) )
    send(dict->browser, NAME_InsertItem, di, 0);

  succeed;
}


DictItem
getFindIndexDict(Dict dict, Int ln)
{ DictItem di;
  Cell cell;

  for_cell(cell, dict->members)
  { di = cell->value;
    if ( di->index == ln )
      answer(di);
  }
  fail;
}


DictItem
getFindPrefixDict(Dict dict, StringObj str, Int from, Bool ign_case)
{ Cell cell;

  if ( isDefault(ign_case) )
    ign_case = OFF;

  TRY(cell = getNth0CellChain(dict->members, isDefault(from) ? ZERO : from));
  for( ; notNil(cell); cell = cell->next )
  { DictItem di = cell->value;
    CharArray label = getLabelDictItem(di);

    if ( label )
    { if ( ign_case == OFF )
      { if ( str_prefix(&label->data, &str->data) )
	  answer(di);
      } else
      { if ( str_icase_prefix(&label->data, &str->data) )
	  answer(di);
      }
    }
  }
     
  fail;
}


		/********************************
		*           SORTING		*
		********************************/

static int	sort_ignore_case   = FALSE;
static int	sort_ignore_blanks = FALSE;

static int
compare_dict_items(const void *d1, const void *d2)
{ CharArray c1 = getLabelDictItem(*(DictItem *)d1);
  CharArray c2 = getLabelDictItem(*(DictItem *)d2);

  if ( c1 && c2 )
  { String s1 = &c1->data;
    String s2 = &c2->data;

    if ( sort_ignore_blanks )
    { LocalString(t1, s1, s1->size);
      LocalString(t2, s2, s2->size);
  
      str_cpy(t1, s1);
      str_cpy(t2, s2);
      str_strip(t1);
      str_strip(t2);
  
      if ( sort_ignore_case == TRUE )
	return str_icase_cmp(t1, t2);
      else
	return str_cmp(t1, t2);
    } else
    { if ( sort_ignore_case == TRUE )
	return str_icase_cmp(s1, s2);
      else
	return str_cmp(s1, s2);
    }
  }

  fail;
}


static status
sortDict(Dict dict, Any code_or_ign_case, Bool ign_blanks, Bool reverse)
{ int count, i=0;
  DictItem *items;
  Cell cell;
  Chain old;				/* reference count */
  int codesort = FALSE;
  int oldrev = qsortReverse;

  if ( valInt(dict->members->size) <= 1 )
    succeed;

  if ( instanceOfObject(code_or_ign_case, ClassCode) )
  { qsortCompareCode = code_or_ign_case;
    codesort++;
  } else
  { if ( isDefault(code_or_ign_case) )
      code_or_ign_case =  getResourceValueObject(dict, NAME_sortIgnoreCase);
    if ( isDefault(ign_blanks) )
      ign_blanks = getResourceValueObject(dict, NAME_sortIgnoreBlanks);

    sort_ignore_case   = (code_or_ign_case == ON);
    sort_ignore_blanks = (ign_blanks == ON);
  }

  old = dict->members;
  lockObject(old, ON);

  count = valInt(dict->members->size);
  items = malloc((count*sizeof(DictItem)));
  for_cell(cell, dict->members)
    items[i++] = cell->value;

  qsortReverse = (reverse == ON);
  qsort(items, count, sizeof(DictItem),
	codesort ? qsortCompareObjects : compare_dict_items);
  qsortReverse = oldrev;

  assign(dict, members, newObject(ClassChain, 0));
  
  if ( notNil(dict->table) )
  { clearHashTable(dict->table);
    assign(dict, table, NIL);
  }

  if ( notNil(dict->browser) )
    send(dict->browser, NAME_Clear, 0);

  for (i=0; i<count; i++)
  { assign(items[i], dict, NIL);
    appendDict(dict, items[i]);
  }
  free(items);

  freeObject(old);

  succeed;
}


static Chain
getMatchDict(Dict dict, CharArray name)
{ Cell cell;
  Chain matching;

  matching = answerObject(ClassChain, 0);

  for_cell(cell, dict->members)
  { DictItem di = cell->value;
    CharArray label = getLabelDictItem(di);
    
    if ( label && str_sub(&label->data, &name->data) )
      appendChain(matching, di);
  }

  answer(matching);
}


status
clearDict(Dict dict)
{ Cell cell;
  
  if ( dict->members->size == ZERO )
    succeed;

  if ( notNil(dict->browser) && !isFreeingObj(dict->browser) )
    send(dict->browser, NAME_Clear, 0);

  if ( notNil(dict->table) )
  { clearHashTable(dict->table);
    assign(dict, table, NIL);
  }
  for_cell(cell, dict->members)
  { DictItem di = cell->value;
    assign(di, dict, NIL);
  }
  clearChain(dict->members);

  succeed;
}


static status
forAllDict(Dict d, Code code, Bool safe)
{ return forAllChain(d->members, code, safe);
}


static status
forSomeDict(Dict d, Code code, Bool safe)
{ return forSomeChain(d->members, code, safe);
}


static Any
getFindDict(Dict d, Code code)
{ return getFindChain(d->members, code);
}


static Chain
getFindAllDict(Dict d, Code code)
{ return getFindAllChain(d->members, code);
}


Any
getBrowserDict(Dict d)
{ if ( notNil(d->browser) )
  { ListBrowser lb = d->browser;

    if ( instanceOfObject(lb->device, ClassBrowser) )
      answer(lb->device);
    else
      answer(lb);
  }

  fail;
}

		 /*******************************
		 *	      VISUAL		*
		 *******************************/

static Chain
getContainsDict(Dict d)
{ answer(d->members);
}



status
makeClassDict(Class class)
{ sourceClass(class, makeClassDict, __FILE__, "$Revision$");

  localClass(class, NAME_browser, NAME_visualisation, "list_browser*",
	     NAME_none,
	     "Associated browser (visualisation)");
  localClass(class, NAME_members, NAME_organisation, "chain", NAME_get,
	     "Objects in the dictionary");
  localClass(class, NAME_table, NAME_hashing, "hash_table*", NAME_none,
	     "Hashtable for access on key");

  termClass(class, "dict", 0);
  saveStyleVariableClass(class, NAME_table, NAME_nil);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "member=dict_item ...",
	     "Create a dict and append the arguments",
	     initialiseDictv);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Destroy hash-table and unlink from browser",
	     unlinkDict);
  sendMethod(class, NAME_append, NAME_add, 1, "item=dict_item",
	     "Append dict_item at the end",
	     appendDict);
  sendMethod(class, NAME_insertAfter, NAME_add, 2,
	     "after=dict_item", "item=any|dict_item*",
	     "Insert item after 2nd (or prepend)",
	     insertAfterDict);
  sendMethod(class, NAME_delete, NAME_delete, 1, "any|dict_item",
	     "Delete dict_item or name",
	     deleteDict);
  sendMethod(class, NAME_members, NAME_set, 1, "chain",
	     "->clear and ->append elements of chain",
	     membersDict);
  sendMethod(class, NAME_member, NAME_set, 1, "any|dict_item",
	     "Test if dict_item or name is member",
	     memberDict);
  sendMethod(class, NAME_sort, NAME_order, 3,
	     "[bool|code|function]", "ignore_blanks=[bool]", "reverse=[bool]",
	     "Sort contents",
	     sortDict);
  sendMethod(class, NAME_clear, NAME_delete, 0,
	     "Delete all members",
	     clearDict);
  sendMethod(class, NAME_forAll, NAME_iterate, 2, "action=code", "safe=[bool]",
	     "Run code on all dict_items, demand acceptance ([safe])",
	     forAllDict);
  sendMethod(class, NAME_forSome, NAME_iterate, 2,
	     "action=code", "safe=[bool]",
	     "Run code on all dict_items ([safe])",
	     forSomeDict);

  getMethod(class, NAME_match, NAME_search, "chain", 1, "char_array",
	    "New chain with items that match argument",
	    getMatchDict);
  getMethod(class, NAME_member, NAME_lookup, "dict_item", 1, "any|dict_item",
	    "Find dict_item from <-key",
	    getMemberDict);
  getMethod(class, NAME_findPrefix, NAME_search, "dict_item", 3,
	    "for=string", "from=[int]", "no_exact_case=[bool]",
	    "First item from index int that matches",
	    getFindPrefixDict);
  getMethod(class, NAME_find, NAME_search, "dict_item", 1, "test=code",
	    "First dict_item accepted by code",
	    getFindDict);
  getMethod(class, NAME_findAll, NAME_search, "chain", 1, "test=code",
	    "New chain with dict_items accepted",
	    getFindAllDict);
  getMethod(class, NAME_browser, NAME_visualisation, "list_browser|browser", 0,
	    "ListBrowser or Browser associated",
	    getBrowserDict);
  getMethod(class, NAME_contains, DEFAULT, "chain", 0,
	    "Equivalent to <-members",
	    getContainsDict);
  getMethod(class, NAME_containedIn, DEFAULT, "list_browser|browser", 0,
	    "Equivalent to <-browser",
	    getBrowserDict);
  getMethod(class, NAME_table, NAME_hashing, "hash_table", 0,
	    "Return hash-table key --> dict_item",
	    getTableDict);

  attach_resource(class, "sort_ignore_case",   "bool", "@off",
		  "@on: ignore case when sorting");
  attach_resource(class, "sort_ignore_blanks", "bool", "@off",
		  "@on: ignore leading blanks when sorting");

  succeed;
}
