/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

static status	rehashAtable(Atable t);

static status
initialiseAtablev(Atable t, Vector names, Vector keys)
{ if ( names->size != keys->size ||
       names->offset != ZERO ||
       keys->offset != ZERO )
    return errorPce(t, NAME_badParameterKeyVector, names, keys);

  assign(t, names, names);
  assign(t, keys, keys);

  rehashAtable(t);

  succeed;
}


static status
unlinkAtable(Atable t)
{ int n, size = valInt(t->names->size);

  for(n=0; n<size; n++)
  { Any ht = t->tables->elements[n];

    if ( notNil(ht) )
      freeObject(ht);
  }

  succeed;
}


static status
rehashAtable(Atable t)
{ int n, size = valInt(t->names->size);
  ArgVector(av, size);

  for(n=0; n<size; n++)
  { Name kind = t->keys->elements[n];

    if ( equalName(kind, NAME_key) )
      av[n] = newObject(ClassChainTable, 0);
    else if ( equalName(kind, NAME_unique) )
      av[n] = newObject(ClassHashTable, 0);
    else
      av[n] = NIL;
  }
  assign(t, tables, newObjectv(ClassVector, size, av));

  succeed;
}


static status
clearAtable(Atable t)
{ int n, size = valInt(t->names->size);

  for(n=0; n<size; n++)
  { Any ht = t->tables->elements[n];

    if ( notNil(ht) )
      send(ht, NAME_clear, 0);
  }

  succeed;
}


static status
appendAtable(Atable t, Vector v)
{ int n, size = valInt(t->names->size);

  if ( v->size != t->names->size )
    return errorPce(t, NAME_badVectorSize, v, t->names->size);

  for(n=0; n<size; n++)
  { HashTable ht = t->tables->elements[n];

    if ( notNil(ht) )
      send(ht, NAME_append, v->elements[n], v, 0);
  }

  succeed;
}


static status
deleteAtable(Atable t, Vector v)
{ int n, size = valInt(t->names->size);

  for(n=0; n<size; n++)
  { HashTable ht = t->tables->elements[n];

    if ( notNil(ht) )
      send(ht, NAME_delete, v->elements[n], v, 0);
  }

  succeed;
}


static
HashTable
find_table(Atable t, Name name)
{ int index, size = valInt(t->names->size);
  Any *elements = t->names->elements;

  for(index=0; index < size && name != elements[index] ; index++)
    ;
  if ( index == size )
    return NIL;

  return t->tables->elements[index];
}


static Any
getVectorsAtable(Atable t, Name name, Any key)
{ HashTable ht;

  if ( isNil(ht=find_table(t, name)) )
    fail;

  answer(getMemberHashTable(ht, key));
}


static Chain
getMembersATable(Atable t)
{ int n, size = valInt(t->tables->size);
  Chain rval = answerObject(ClassChain, 0);

  for(n=0; n<size; n++)	
    if ( notNil(t->tables->elements[n]) )
    { HashTable ht = t->tables->elements[n];

      if ( instanceOfObject(ht, ClassChainTable) )
      { for_hash_table(ht, s,
		       { Cell cell;

			 for_cell(cell, (Chain) s->value)
			   appendChain(rval, cell->value);
		       });
      } else
      { for_hash_table(ht, s,
		       appendChain(rval, s->value));
      }

      answer(rval);
    }
  
  fail;
}


static Name
best_hashtable(Atable t, Vector v, HashTable *ht, Any *val)
{ int n, size = valInt(v->size);
  Bool found = OFF;

  if ( t->names->size != v->size )
  { errorPce(t, NAME_badVectorSize, v, t->names->size);
    fail;
  }
  
  for(n=0; n<size; n++)
  { if ( notDefault(v->elements[n]) )
    { Name kind = t->keys->elements[n];

      if ( equalName(kind, NAME_unique) )
      { *ht = t->tables->elements[n];
	*val = v->elements[n];
	return NAME_unique;
      } else if ( equalName(kind, NAME_key) )
      { if ( found == OFF )
	{ *ht = t->tables->elements[n];
	  *val = v->elements[n];
	  found = ON;
	}
      }
    }
  }

  if ( found == ON )
    return NAME_key;

  for(n=0; n<size; n++)
  { if ( notNil(t->tables->elements[n]) )
    { *ht =  t->tables->elements[n];
      *val = DEFAULT;
      return NAME_none;
    }
  }

  errorPce(t, NAME_noTable);
  fail;
}


static status
matchingVectors(Vector k, Vector v)
{ int n, size = valInt(k->size);

  if ( k->size != v->size )
    fail;
  for(n = 0; n < size; n++)
    if ( notDefault(k->elements[n]) && k->elements[n] != v->elements[n] )
      fail;

  succeed;
}


static Chain
getMatchATable(Atable t, Vector v)
{ HashTable ht;
  Any val;
  Name match;

  TRY(match = best_hashtable(t, v, &ht, &val));
  if ( match == NAME_unique )
  { Vector vm = getMemberHashTable(ht, val);

    if ( matchingVectors(v, vm) )
      answer(newObject(ClassChain, vm, 0));
    else
      fail;
  } else if ( match == NAME_key )
  { Chain ch, rval = FAIL;
    Cell cell;

    assert(instanceOfObject(ht, ClassChainTable));
    TRY(ch = getMemberHashTable(ht, val));
    for_cell(cell, ch)
    { if ( matchingVectors(v, cell->value) )
      { if ( rval )
	  appendChain(rval, cell->value);
	else
	  rval = newObject(ClassChain, cell->value, 0);
      }
    }

    answer(rval);
  } else /*if ( match == NAME_none )*/
  { Chain rval = FAIL;

    if ( instanceOfObject(ht, ClassChainTable) )
    { for_hash_table(ht, s,
		     { Cell cell;

		       for_cell(cell, (Chain) s->value)
		       { if ( matchingVectors(v, cell->value) )
			 { if ( rval )
			     appendChain(rval, cell->value);
			   else
			     rval = newObject(ClassChain, cell->value, 0);
			 }
		       }
		     });
    } else
    { for_hash_table(ht, s,
		     { assert(instanceOfObject(s->value, ClassVector));
		       if ( matchingVectors(v, s->value) )
		       { if ( rval )
			   appendChain(rval, s->value);
		         else
			   rval = newObject(ClassChain, s->value, 0);
		       }
		     });
    }
    answer(rval);
  }
}


status
makeClassAtable(Class class)
{ sourceClass(class, makeClassAtable, __FILE__, "$Revision$");

  localClass(class, NAME_keys, NAME_hashing, "vector", NAME_get,
	     "Vector to determine key columns");
  localClass(class, NAME_names, NAME_name, "vector", NAME_get,
	     "Vector to determine column names");
  localClass(class, NAME_tables, NAME_storage, "vector", NAME_get,
	     "Hashtables for key entries");

  termClass(class, "table", 2, NAME_names, NAME_keys);

  sendMethod(class, NAME_initialise, DEFAULT, 2, "names=vector", "keys=vector",
	     "Create table, given names and keys",
	     initialiseAtablev);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Clear the tables",
	     unlinkAtable);
  sendMethod(class, NAME_clear, NAME_delete, 0,
	     "Remove all vectors from the table",
	     clearAtable);
  sendMethod(class, NAME_append, NAME_add, 1, "association=vector",
	     "Add association",
	     appendAtable);
  sendMethod(class, NAME_delete, NAME_delete, 1, "association=vector",
	     "Delete association",
	     deleteAtable);

  getMethod(class, NAME_vectors, NAME_lookup, "chain|vector", 2,
	    "column=name", "value=any",
	    "Chain with vectors matching named field",
	    getVectorsAtable);
  getMethod(class, NAME_match, NAME_lookup, "associations=chain", 1,
	    "pattern=vector",
	    "New chain with vectors matching arg-1",
	    getMatchATable);
  getMethod(class, NAME_members, NAME_lookup, "chain", 0,
	    "New chain with all member-vectors",
	    getMembersATable);

  succeed;
}
