/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


#include <h/kernel.h>

status
appendChainTable(ChainTable ct, Any name, Any value)
{ Chain ch;

  if ( (ch = getMemberHashTable((HashTable) ct, name)) )
    appendChain(ch, value);
  else
    appendHashTable((HashTable) ct, name, newObject(ClassChain, value, 0));

  succeed;
}


status
addChainTable(ChainTable ct, Any name, Any value)
{ Chain ch;

  if ( (ch = getMemberHashTable((HashTable) ct, name)) )
    addChain(ch, value);
  else
    appendHashTable((HashTable) ct, name, newObject(ClassChain, value, 0));

  succeed;
}


static status
prependChainTable(ChainTable ct, Any name, Any value)
{ Chain ch;

  if ( (ch = getMemberHashTable((HashTable) ct, name)) )
    prependChain(ch, value);
  else
    appendHashTable((HashTable) ct, name, newObject(ClassChain, value, 0));

  succeed;
}


static status
deleteChainTable(ChainTable ct, Any name, Any value)
{ Chain ch;

  if ( isDefault(value) )
    return deleteHashTable((HashTable)ct, name);

  if ( (ch = getMemberHashTable((HashTable) ct, name)) )
  { TRY(deleteChain(ch, value));
    if ( emptyChain(ch) )
      deleteHashTable((HashTable) ct, name);

    succeed;
  }

  fail;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_delete[] =
        { "key=any", "value=[any]" };
static char *T_keyAany_valueAany[] =
        { "key=any", "value=any" };

/* Instance Variables */

#define var_chainTable NULL
/*
static vardecl var_chainTable[] =
{ 
};
*/

/* Send Methods */

static senddecl send_chainTable[] =
{ SM(NAME_append, 2, T_keyAany_valueAany, appendChainTable,
     DEFAULT, "Append association to table"),
  SM(NAME_add, 2, T_keyAany_valueAany, addChainTable,
     NAME_add, "Add association to table"),
  SM(NAME_prepend, 2, T_keyAany_valueAany, prependChainTable,
     NAME_add, "Prepend association to table"),
  SM(NAME_delete, 2, T_delete, deleteChainTable,
     NAME_delete, "Delete all matching symbols")
};

/* Get Methods */

#define get_chainTable NULL
/*
static getdecl get_chainTable[] =
{ 
};
*/

/* Resources */

#define rc_chainTable NULL
/*
static classvardecl rc_chainTable[] =
{ 
};
*/

/* Class Declaration */

static Name chainTable_termnames[] = { NAME_buckets };

ClassDecl(chainTable_decls,
          var_chainTable, send_chainTable, get_chainTable, rc_chainTable,
          1, chainTable_termnames,
          "$Rev$");


status
makeClassChainTable(Class class)
{ return declareClass(class, &chainTable_decls);
}
