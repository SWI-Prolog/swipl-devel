/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_vectors[] =
        { "column=name", "value=any" };
static char *T_initialise[] =
        { "names=vector", "keys=vector" };

/* Instance Variables */

static vardecl var_atable[] =
{ IV(NAME_keys, "vector", IV_GET,
     NAME_hashing, "Vector to determine key columns"),
  IV(NAME_names, "vector", IV_GET,
     NAME_name, "Vector to determine column names"),
  IV(NAME_tables, "vector", IV_GET,
     NAME_storage, "Hashtables for key entries")
};

/* Send Methods */

static senddecl send_atable[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseAtablev,
     DEFAULT, "Create table, given names and keys"),
  SM(NAME_unlink, 0, NULL, unlinkAtable,
     DEFAULT, "Clear the tables"),
  SM(NAME_append, 1, "association=vector", appendAtable,
     NAME_add, "Add association"),
  SM(NAME_clear, 0, NULL, clearAtable,
     NAME_delete, "Remove all vectors from the table"),
  SM(NAME_delete, 1, "association=vector", deleteAtable,
     NAME_delete, "Delete association")
};

/* Get Methods */

static getdecl get_atable[] =
{ GM(NAME_match, 1, "associations=chain", "pattern=vector", getMatchATable,
     NAME_lookup, "New chain with vectors matching arg-1"),
  GM(NAME_members, 0, "chain", NULL, getMembersATable,
     NAME_lookup, "New chain with all member-vectors"),
  GM(NAME_vectors, 2, "chain|vector", T_vectors, getVectorsAtable,
     NAME_lookup, "Chain with vectors matching named field")
};

/* Resources */

#define rc_atable NULL
/*
static classvardecl rc_atable[] =
{ 
};
*/

/* Class Declaration */

static Name atable_termnames[] = { NAME_names, NAME_keys };

ClassDecl(atable_decls,
          var_atable, send_atable, get_atable, rc_atable,
          2, atable_termnames,
          "$Rev$");


status
makeClassAtable(Class class)
{ return declareClass(class, &atable_decls);
}
