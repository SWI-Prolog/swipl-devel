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


status
makeClassChainTable(Class class)
{ sourceClass(class, makeClassChainTable, __FILE__, "$Revision$");

  termClass(class, "chain_table", 1, NAME_buckets);

  sendMethod(class, NAME_append, DEFAULT, 2, "key=any", "value=any",
	     "Append association to table",
	     appendChainTable);
  sendMethod(class, NAME_prepend, NAME_add, 2, "key=any", "value=any",
	     "Prepend association to table",
	     prependChainTable);
  sendMethod(class, NAME_add, NAME_add, 2, "key=any", "value=any",
	     "Add association to table",
	     appendChainTable);
  sendMethod(class, NAME_delete, NAME_delete, 2, "key=any", "value=[any]",
	     "Delete all matching symbols",
	     deleteChainTable);

  succeed;
}
