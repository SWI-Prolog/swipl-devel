/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
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


INLINE status
isProperObject(const Any obj)
{ if ( isObject(obj) && isAddress(obj) )
  { Class class = classOfObject(obj);
    
    if ( isAddress(class) && instanceOfObject(class, ClassClass) )
      succeed;
  }

  fail;
}


		/********************************
		*           HASHTABLES		*
		********************************/


INLINE Any
getMemberHashTable(HashTable ht, Any name)
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


		/********************************
		*             CODE		*
		********************************/

INLINE status
executeCode(Code c)
{ Class cl = classOfObject(c);
  status rval;
  goal goal;
  Goal g = &goal;

  pushGoal(g, c, c, NAME_execute, 0, NULL);
  traceEnter(g);
  addCodeReference(c);
  rval = (*cl->send_function)(c);
  delCodeReference(c);
  traceReturn(g, rval);
  popGoal();

  return rval;
}


INLINE status
forwardBlockv(Block b, int argc, const Any argv[])
{ status rval;

  if ( isNil(b->parameters) )
  { Mode(onDFlag(b, D_SYSTEM) ? MODE_SYSTEM : MODE_USER,
	 withArgs(argc, argv, rval = executeCode((Code) b)));
  } else
  { Mode(onDFlag(b, D_SYSTEM) ? MODE_SYSTEM : MODE_USER,
	 withLocalVars({ int i;
			 Var *vars = (Var *) b->parameters->elements;
			 int nvars = valInt(b->parameters->size);

			 for(i=0; i<argc; i++)
			 { if ( i < nvars )
			     assignVar(vars[i], argv[i], DEFAULT);
			   else
			     assignVar(Arg(i-nvars+1), argv[i], DEFAULT);
			 }
			 rval = executeCode((Code) b);
		        }));
  }

  return rval;
}


INLINE status
forwardCodev(Code c, int argc, const Any argv[])
{ status rval;

/*if ( instanceOfObject(c, ClassBlock) )*/
  if ( c->class == ClassBlock )
    return forwardBlockv((Block) c, argc, argv);

  Mode(onDFlag(c, D_SYSTEM) ? MODE_SYSTEM : MODE_USER,
       withArgs(argc, argv, rval = executeCode(c)));

  return rval;
}


		/********************************
		*            FUNCTIONS		*
		********************************/

INLINE Any
getExecuteFunction(Function f)
{ Class cl = classOfObject(f);
  Any rval;
  goal goal;
  Goal g = &goal;

  pushGoal(g, f, f, NAME_execute, 0, NULL);
  traceEnter(g);
  addCodeReference(f);
  rval = (*cl->get_function)(f);
  delCodeReference(f);
  traceAnswer(g, rval);
  popGoal();

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
    Mode(MODE_SYSTEM, rval = getResolveSendMethodClass(class, name));

  if ( notNil(rval) )
    answer(rval);

  fail;
}


INLINE Any
getGetMethodClass(Class class, Name name)
{ Any rval;

  RealiseClass(class);
  if ( !(rval = getMemberHashTable(class->get_table, name)) )
    Mode(MODE_SYSTEM, rval = getResolveGetMethodClass(class, name));

  if ( notNil(rval) )
    answer(rval);

  fail;
}

		 /*******************************
		 *	      TYPES		*
		 *******************************/

INLINE Any
CheckType(Any val, Type t, Any ctx)
{ if ( (*t->validate_function)(t, val, ctx) )
    return val;

  return checkType(val, t, ctx);
}


#else /*USE_INLINE*/

status	 constf instanceOfObject(const Any, const Class);
status		isProperObject(const Any);
Any		getSendMethodClass(const Class, const Name);
Any		getGetMethodClass(const Class, const Name);
Any		getMemberHashTable(const HashTable, const Any);
status		executeCode(Code);
Any		getExecuteFunction(Function);
status		forwardCodev(Code, int, const Any[]);
status		forwardBlockv(Block, int, const Any[]);
Any		expandCodeArgument(Any);
Any		CheckType(Any val, Type t, Any ctx);

/* Donot write below this line */
#endif /*USE_INLINE*/
