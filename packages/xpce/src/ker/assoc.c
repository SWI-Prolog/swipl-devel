/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


#define INLINE_UTILITIES 1
#include <h/kernel.h>
#include <h/interface.h>

static int	host_handles;		/* # handles */
static long	itf_symbols;		/* # symbols */

#define SizeOfSymbol ((int)(&(((PceITFSymbol)NULL)->handle[host_handles])))

		/********************************
		*            SYMBOLS		*
		********************************/

PceITFSymbol
newSymbol(Any obj, Name name)
{ PceITFSymbol s = alloc(SizeOfSymbol);
  int n;

  s->object = obj;
  s->name   = name;

  for( n=0; n < host_handles; n++ )
    s->handle[n] = NULL;

  itf_symbols++;

  return s;
}


		/********************************
		*         CREATE/DELETE		*
		********************************/

void
deleteAssoc(Any obj)
{ if ( isObject(obj) && onFlag(obj, F_ASSOC) )
  { PceITFSymbol symbol = getMemberHashTable(ObjectToITFTable, obj);
    if ( symbol )
    { symbol->object = NULL;
      deleteHashTable(ObjectToITFTable, obj);
      clearFlag(obj, F_ASSOC);
    }
  }
}


void
newAssoc(Name name, Any obj)
{ PceITFSymbol symbol;
  Any old;

  if ( (old = getObjectAssoc(name)) )
    deleteAssoc(old);
  deleteAssoc(obj);

  if ( onFlag(name, F_ITFNAME) )
  { symbol = getMemberHashTable(NameToITFTable, name);
    symbol->object = obj;
    appendHashTable(ObjectToITFTable, obj, symbol);
    setFlag(obj, F_ASSOC);
  } else
  { symbol = newSymbol(obj, name);

    setFlag(name, F_ITFNAME);
    if ( isObject(obj) )
      setFlag(obj, F_ASSOC);

    appendHashTable(ObjectToITFTable, obj,  symbol);
    appendHashTable(NameToITFTable,   name, symbol);
  }

  if ( isObject(obj) )
    lockObj(obj);
}


Any
getObjectAssoc(Name name)
{ if ( onFlag(name, F_ITFNAME) )
  { PceITFSymbol symbol = getMemberHashTable(NameToITFTable, name);
    return symbol->object;
  }

  fail;
}


Name
getNameAssoc(Any obj)
{ if ( (isObject(obj) && onFlag(obj, F_ASSOC)) )
  { PceITFSymbol symbol = getMemberHashTable(ObjectToITFTable, obj);
    return symbol->name;
  }

  fail;
}


status
renameAssoc(Name old, Name new)
{ Any obj = getObjectAssoc(old);

  if ( obj != FAIL )
  { newAssoc(new, obj);
    succeed;
  }

  fail;
}


status
forSomeAssoc(Code code)
{ for_hash_table(ObjectToITFTable, s,
		 { PceITFSymbol symbol = s->value;

		   if ( symbol->object )
		     forwardCodev(code, 1, (Any *) (&symbol->name));
		 });
  succeed;
}


		/********************************
		*            SPECIALS           *
		*********************************/

void
initAssoc(int handles)
{ int n;

  host_handles = handles;

  ObjectToITFTable = createHashTable(toInt(1024), NAME_none);
  NameToITFTable   = createHashTable(toInt(1024), NAME_none);

  newAssoc(NAME_ObjectToItfTable, ObjectToITFTable);
  newAssoc(NAME_NameToItfTable,   NameToITFTable);

  for(n=0; n<host_handles; n++)
    HandleToITFTables[n] = createHashTable(toInt(64), NAME_none);
}


