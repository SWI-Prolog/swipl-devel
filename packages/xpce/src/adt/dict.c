/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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


static status
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
insertDict(Dict dict, DictItem di)
{ int codesort = FALSE;
  Cell cell;
  DictItem di2;
  int cmp;

					/* not sorted or empty: just append */
  if ( isNil(dict->sort_by) || dict->members->size == ZERO )
    return appendDict(dict, di);

					/* delete from possible old dict */
  if ( notNil(di->dict) )
  { addCodeReference(di);
    deleteDict(di->dict, di);
    delCodeReference(di);
  }

					/* find out sorting rules */
  if ( isDefault(dict->sort_by) )
  { sort_ignore_case   = FALSE;
    sort_ignore_blanks = FALSE;
  } else
  { qsortCompareCode = dict->sort_by;
    codesort++;
  }

					/* after the last: use append */
  di2 = getTailChain(dict->members);
  cmp = codesort ? qsortCompareObjects(&di, &di2) 
	         : compare_dict_items(&di, &di2);
  if ( cmp >= 0 )
    return appendDict(dict, di);

					/* associate with the dict */
  assign(di, dict, dict);
  if ( notNil(dict->table) )
    appendHashTable(dict->table, di->key, di);
  
					/* find its place */
  for_cell(cell, dict->members)
  { di2 = cell->value;

    cmp = codesort ? qsortCompareObjects(&di, &di2) 
		   : compare_dict_items(&di, &di2);
    if ( cmp < 0 )
    { dict->members->current = cell;
      insertChain(dict->members, di);
      break;
    }
  }

  renumberDict(dict);
  
  if ( notNil(dict->browser) )
    send(dict->browser, NAME_InsertItem, di, 0);

  succeed;
}


static status
sortDict(Dict dict, Any code_or_ign_case, Bool ign_blanks, Bool reverse)
{ int count, i=0;
  DictItem *items;
  Cell cell;
  Chain old;				/* reference count */
  int codesort = FALSE;
  int oldrev = qsortReverse;
  Code sortcode;

  if ( valInt(dict->members->size) <= 1 )
    succeed;

  if ( instanceOfObject((sortcode=code_or_ign_case), ClassCode) ||
       (isDefault(code_or_ign_case) &&
	instanceOfObject((sortcode=dict->sort_by), ClassCode)) )
  { qsortCompareCode = sortcode;
    codesort++;
  } else
  { if ( isDefault(code_or_ign_case) )
      code_or_ign_case =  getClassVariableValueObject(dict, NAME_sortIgnoreCase);
    if ( isDefault(ign_blanks) )
      ign_blanks = getClassVariableValueObject(dict, NAME_sortIgnoreBlanks);

    sort_ignore_case   = (code_or_ign_case == ON);
    sort_ignore_blanks = (ign_blanks == ON);
  }

  count = valInt(dict->members->size);
  items = pceMalloc((count*sizeof(DictItem)));
  for_cell(cell, dict->members)
    items[i++] = cell->value;

  qsortReverse = (reverse == ON);
  qsort(items, count, sizeof(DictItem),
	codesort ? qsortCompareObjects : compare_dict_items);
  qsortReverse = oldrev;

					/* see whether something changed */
  for(i=0, cell = dict->members->head; i < count; i++, cell = cell->next)
  { if ( cell->value != items[i] )
      break;
  }
  if ( i == count )			/* no change */
  { pceFree(items);
    succeed;
  }

  if ( notNil(dict->browser) )
    send(dict->browser, NAME_Clear, 0);

  old = dict->members;
  lockObject(old, ON);
  assign(dict, members, newObject(ClassChain, 0));
  
  if ( notNil(dict->table) )
  { clearHashTable(dict->table);
    assign(dict, table, NIL);
  }

  for (i=0; i<count; i++)
  { assign(items[i], dict, NIL);
    appendDict(dict, items[i]);
  }
  pceFree(items);

  freeObject(old);

  succeed;
}


static status
sortByDict(Dict dict, Code code)
{ assign(dict, sort_by, code);
  if ( notNil(code) )
    return send(dict, NAME_sort, 0);

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


static Any
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

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_sort[] =
        { "[bool|code|function]", "ignore_blanks=[bool]", "reverse=[bool]" };
static char *T_actionAcode_safeADboolD[] =
        { "action=code", "safe=[bool]" };
static char *T_insertAfter[] =
        { "after=dict_item", "item=any|dict_item*" };
static char *T_findPrefix[] =
        { "for=string", "from=[int]", "no_exact_case=[bool]" };

/* Instance Variables */

static vardecl var_dict[] =
{ IV(NAME_browser, "list_browser*", IV_NONE,
     NAME_visualisation, "Associated browser (visualisation)"),
  IV(NAME_members, "chain", IV_GET,
     NAME_organisation, "Objects in the dictionary"),
  IV(NAME_table, "hash_table*", IV_NONE,
     NAME_hashing, "Hashtable for access on key"),
  SV(NAME_sortBy, "[code]*", IV_GET|IV_STORE, sortByDict,
     NAME_order, "Sorting rule to apply")
};

/* Send Methods */

static senddecl send_dict[] =
{ SM(NAME_initialise, 1, "member=dict_item ...", initialiseDictv,
     DEFAULT, "Create a dict and append the arguments"),
  SM(NAME_unlink, 0, NULL, unlinkDict,
     DEFAULT, "Destroy hash-table and unlink from browser"),
  SM(NAME_append, 1, "item=dict_item", appendDict,
     NAME_add, "Append dict_item at the end"),
  SM(NAME_insert, 1, "item=dict_item", insertDict,
     NAME_add, "Insert dict_item according to <-sort_by"),
  SM(NAME_insertAfter, 2, T_insertAfter, insertAfterDict,
     NAME_add, "Insert item after 2nd (or prepend)"),
  SM(NAME_clear, 0, NULL, clearDict,
     NAME_delete, "Delete all members"),
  SM(NAME_delete, 1, "any|dict_item", deleteDict,
     NAME_delete, "Delete dict_item or name"),
  SM(NAME_forAll, 2, T_actionAcode_safeADboolD, forAllDict,
     NAME_iterate, "Run code on all dict_items, demand acceptance ([safe])"),
  SM(NAME_forSome, 2, T_actionAcode_safeADboolD, forSomeDict,
     NAME_iterate, "Run code on all dict_items ([safe])"),
  SM(NAME_sort, 3, T_sort, sortDict,
     NAME_order, "Sort contents"),
  SM(NAME_member, 1, "any|dict_item", memberDict,
     NAME_set, "Test if dict_item or name is member"),
  SM(NAME_members, 1, "chain", membersDict,
     NAME_set, "->clear and ->append elements of chain")
};

/* Get Methods */

static getdecl get_dict[] =
{ GM(NAME_containedIn, 0, "list_browser|browser", NULL, getBrowserDict,
     DEFAULT, "Equivalent to <-browser"),
  GM(NAME_contains, 0, "chain", NULL, getContainsDict,
     DEFAULT, "Equivalent to <-members"),
  GM(NAME_table, 0, "hash_table", NULL, getTableDict,
     NAME_hashing, "Return hash-table key --> dict_item"),
  GM(NAME_member, 1, "dict_item", "any|dict_item", getMemberDict,
     NAME_lookup, "Find dict_item from <-key"),
  GM(NAME_find, 1, "dict_item", "test=code", getFindDict,
     NAME_search, "First dict_item accepted by code"),
  GM(NAME_findAll, 1, "chain", "test=code", getFindAllDict,
     NAME_search, "New chain with dict_items accepted"),
  GM(NAME_findPrefix, 3, "dict_item", T_findPrefix, getFindPrefixDict,
     NAME_search, "First item from index int that matches"),
  GM(NAME_match, 1, "chain", "char_array", getMatchDict,
     NAME_search, "New chain with items that match argument"),
  GM(NAME_browser, 0, "list_browser|browser", NULL, getBrowserDict,
     NAME_visualisation, "ListBrowser or Browser associated")
};

/* Resources */

static classvardecl rc_dict[] =
{ RC(NAME_sortIgnoreBlanks, "bool", "@off",
     "@on: ignore leading blanks when sorting"),
  RC(NAME_sortIgnoreCase, "bool", "@off",
     "@on: ignore case when sorting")
};

/* Class Declaration */

ClassDecl(dict_decls,
          var_dict, send_dict, get_dict, rc_dict,
          0, NULL,
          "$Rev$");

status
makeClassDict(Class class)
{ declareClass(class, &dict_decls);
  saveStyleVariableClass(class, NAME_table, NAME_nil);

  succeed;
}
