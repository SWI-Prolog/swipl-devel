/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#ifdef PCE_INCLUDED
#if O_INLINE && INLINE_UTILITIES
#define INLINE static inline
#define USE_INLINE 1
#endif
#else
#include <h/kernel.h>
#define INLINE
#define USE_INLINE 1
#endif

#include <h/trace.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file defines various time-critical general purpose-functions.  Time
critical modules may wish to include this file in the following way:
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if USE_INLINE

		/********************************
		*     OBJECT MANIPULATIONS	*
		********************************/

INLINE constf status
instanceOfObject(const Any obj, const Class super)
{ if ( isObject(obj) )
  { Class class = classOfObject(obj);
    
    return class == super || (class->tree_index >= super->tree_index &&
			      class->tree_index <  super->neighbour_index);
  }

  fail;
}


INLINE constf status
objectIsInstanceOf(const Any obj, const Class super)
{ const Class class = classOfObject(obj);
    
  return class == super || (class->tree_index >= super->tree_index &&
			    class->tree_index <  super->neighbour_index);
}


INLINE status
isProperObject(const Any obj)
{ return (obj && isAddress(obj) && hasObjectMagic(obj));
}


		/********************************
		*           HASHTABLES		*
		********************************/


INLINE Any
getMemberHashTable(const HashTable ht, const Any name)
{ int hashkey = hashKey(name, ht->buckets);
  Symbol s = &ht->symbols[hashkey];

  COUNT(hash_lookups++);

  for(;;)
  { if ( s->name == name )
      return s->value;
    if ( !s->name )
      fail;
    COUNT(hash_cmp_failed++);
    if ( ++hashkey == ht->buckets )
    { hashkey = 0;
      s = ht->symbols;
    } else
      s++;
  }

  fail;
}


		 /*******************************
		 *     REFERENCES FROM CODE	*
		 *******************************/

INLINE void
unallocObject(Any obj)
{ unalloc(valInt(classOfObject(obj)->instance_size), obj);
}


INLINE void
addCodeReference(Any obj)
{ Instance i = obj;

  i->references += ONE_CODE_REF;
}


INLINE void
delCodeReference(Any obj)
{ Instance i = obj;

  i->references -= ONE_CODE_REF;
  if ( i->references <= 0 )
  { if ( noRefsObj(i) )
    { if ( isFreedObj(i) )
      { DEBUG(NAME_free,
	      Cprintf("Doing (code-)deferred unalloc on %s\n", pp(i)));
	unallocObject(i);
	deferredUnalloced--;
      }
    } else
    { errorPce(PCE, NAME_negativeRefCount, i);
    }
  }
}

		/********************************
		*             CODE		*
		********************************/

INLINE status
executeCode(Code c)
{ Class cl = classOfObject(c);
  status rval;

  addCodeReference(c);
  FixSendFunctionClass(cl, NAME_Execute);
  if ( onDFlag(c, D_SERVICE) )
  { ServiceMode(PCE_EXEC_SERVICE, rval = (*cl->send_function)(c));
  } else
    rval = (*cl->send_function)(c);
  delCodeReference(c);

  return rval;
}


INLINE status
forwardBlockv(Block b, int argc, const Any argv[])
{ status rval;

  if ( isNil(b->parameters) )
  { withArgs(argc, argv, rval = executeCode((Code) b));
  } else
  { withLocalVars({ int i;
		    Var *vars = (Var *) b->parameters->elements;
		    int nvars = valInt(b->parameters->size);
		    
		    for(i=0; i<argc; i++)
		    { if ( i < nvars )
			assignVar(vars[i], argv[i], DEFAULT);
		      else
			assignVar(Arg(i-nvars+1), argv[i], DEFAULT);
		    }
		    rval = executeCode((Code) b);
		  });
  }

  return rval;
}


INLINE status
forwardCodev(Code c, int argc, const Any argv[])
{ status rval;

/*if ( instanceOfObject(c, ClassBlock) )*/
  if ( c->class == ClassBlock )
    return forwardBlockv((Block) c, argc, argv);

  withArgs(argc, argv, rval = executeCode(c));

  return rval;
}


		/********************************
		*            FUNCTIONS		*
		********************************/

INLINE Any
getExecuteFunction(Function f)
{ Class cl = classOfObject(f);
  Any rval;

  addCodeReference(f);
  FixGetFunctionClass(cl, NAME_Execute);
  if ( onDFlag(f, D_SERVICE) )
  { ServiceMode(PCE_EXEC_SERVICE, rval = (*cl->get_function)(f));
  } else
    rval = (*cl->get_function)(f);
  delCodeReference(f);

  return rval;
}


INLINE Any
expandCodeArgument(Any arg)
{ if ( isFunction(arg) )
    return getExecuteFunction(arg);

  return arg;
}


		/********************************
		*           CLASSES		*
		********************************/

#define RealiseClass(class) if ( (class)->realised != ON ) realiseClass(class)

INLINE Any
getSendMethodClass(Class class, Name name)
{ Any rval;

  RealiseClass(class);
  if ( !(rval = getMemberHashTable(class->send_table, name)) )
    rval = getResolveSendMethodClass(class, name);

  if ( notNil(rval) )
    answer(rval);

  fail;
}


INLINE Any
getGetMethodClass(Class class, Name name)
{ Any rval;

  RealiseClass(class);
  if ( !(rval = getMemberHashTable(class->get_table, name)) )
    rval = getResolveGetMethodClass(class, name);

  if ( notNil(rval) )
    answer(rval);

  fail;
}

		 /*******************************
		 *	      TYPES		*
		 *******************************/

INLINE Any
checkType(const Any val, const Type t, const Any ctx)
{ if ( validateType(t, val, ctx) )
    return val;

  return getTranslateType(t, val, ctx);
}


INLINE Name
checkSelector(Any sel)
{ if ( isName(sel) )
    return sel;

  return checkType(sel, TypeName, NIL);
}

#else /*USE_INLINE*/

void		unallocObject(Any obj);
void		addCodeReference(Any obj);
void		delCodeReference(Any obj);
status	 constf instanceOfObject(const Any, const Class);
status   constf objectIsInstanceOf(const Any obj, const Class super);
status		isProperObject(const Any);
Any		getSendMethodClass(Class, Name);
Any		getGetMethodClass(Class, Name);
Any		getMemberHashTable(const HashTable, const Any);
status		executeCode(Code);
Any		getExecuteFunction(Function);
status		forwardCodev(Code, int, const Any[]);
status		forwardBlockv(Block, int, const Any[]);
Any		expandCodeArgument(Any);
Any		checkType(const Any val, const Type t, const Any ctx);
Name		checkSelector(Any sel);

/* Donot write below this line */
#endif /*USE_INLINE*/
