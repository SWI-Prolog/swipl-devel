/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

#include <stdarg.h>
#include <assert.h>
#include <sys/types.h>			/* size_t */
#include <h/interface.h>
#include <stdio.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef __WIN32__
#include <windows.h>
#define RaiseException PceRaiseException /* conflict */
#endif

/*#define SICSTUS 1*/			/* SICStus Prolog version 3.x */
/*#define SWI 1*/			/* SWI-Prolog version 2.5! and up */

#ifdef __GNUC__
#define TermVector(name, size)  Term name[size]
#define ObjectVector(name, size) PceObject name[size]
#else
#define TermVector(name, size) \
	Term *name = (Term *) alloca(size * sizeof(Term))
#define ObjectVector(name, size) \
	PceObject *name = (PceObject *) alloca(size * sizeof(Term))
#endif

#define O_MODULE 1			/* use module tags */
#define PREDICATE_PER_CLASS 1		/* compile to send(Selector, ...) */

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#ifndef EOS
#define EOS '\0'
#endif

#define NULLATOM ((Atom)0)

#ifndef MAXMODULENAME
#define MAXMODULENAME 1024
#endif

typedef void (*OnExitFunction)(void);	/* HOST_ONEXIT function */

		 /*******************************
		 *    PROLOG ELEMENTARY TYPES	*
		 *******************************/

#ifdef SICSTUS
#include <sicstus/sicstus.h>

typedef unsigned long	Atom;
typedef SP_term_ref	Term;
typedef int		foreign_t;
typedef Atom		Module;
typedef SP_pred_ref	Predicate;
typedef SP_term_ref	Fid;

#define AtomFromString(s)	SP_atom_from_string(s)
#define ModuleFromAtom(a)	((Module) (a))
#define ModuleName(m)		((Atom) (m))
#endif /*SICSTUS*/

#ifdef SWI
#define pl_get0		pce_get0	/* avoid name-conflicts */
#define pl_get2		pce_get2

#include <SWI-Stream.h>
#include <SWI-Prolog.h>

typedef atom_t		Atom;
typedef term_t		Term;
typedef module_t	Module;
typedef predicate_t	Predicate;
typedef functor_t	Functor;
typedef fid_t		Fid;

#define AtomFromString(s)	PL_new_atom(s)
#define ModuleFromAtom(a)	PL_new_module(a)
#define ModuleName(m)		PL_module_name(m)
#endif /*SWI*/


		 /*******************************
		 *	    PROTOTYPES		*
		 *******************************/

static PceObject	termToObject(Term t, Atom assoc, int new);
static PceName		pceSelector(Term t);
       foreign_t	pl_pce_init(Term a);


		 /*******************************
		 *	     CONSTANTS		*
		 *******************************/

static Module DefaultModule;		/* For module handling */

static Atom ATOM_call;			/* "call" */
static Atom ATOM_ref;			/* "@" */
static Atom ATOM_string;		/* "string" */
static Atom ATOM_new;			/* "new" */
static Atom ATOM_module;		/* ":" */
static Atom ATOM_pce;			/* "pce" */
static Atom ATOM_user;			/* "user" */
static Atom ATOM_badIntegerReference;	/* "bad_integer_reference" */
static Atom ATOM_unknownReference;	/* "unknown_reference" */
static Atom ATOM_badReference;		/* "bad_reference" */
static Atom ATOM_badStringArgument;	/* "bad_string_argument" */
static Atom ATOM_badObjectDescription;	/* "bad_object_description" */
static Atom ATOM_badSelector;		/* "bad_selector" */
static Atom ATOM_initialisation;	/* "initialisation" */
static Atom ATOM_procedure;		/* "procedure" */
static Atom ATOM_existence_error;	/* "existence_error" */
static Atom ATOM_slash;			/* "/" */
static Atom ATOM_read;			/* "read" */
static Atom ATOM_write;			/* "write" */
static Atom ATOM_append;		/* "append" */

static Module MODULE_user;		/* Handle for user-module */

static void
initPrologConstants()
{ ATOM_call			= AtomFromString("call");
  ATOM_ref			= AtomFromString("@");
  ATOM_string			= AtomFromString("string");
  ATOM_new			= AtomFromString("new");
  ATOM_module   		= AtomFromString(":");
  ATOM_user			= AtomFromString("user");
  ATOM_pce			= AtomFromString("pce");
  ATOM_badIntegerReference 	= AtomFromString("bad_integer_reference");
  ATOM_unknownReference		= AtomFromString("unknown_reference");
  ATOM_badReference		= AtomFromString("bad_reference");
  ATOM_badStringArgument	= AtomFromString("bad_string_argument");
  ATOM_badObjectDescription	= AtomFromString("bad_object_description");
  ATOM_badSelector		= AtomFromString("bad_selector");
  ATOM_initialisation		= AtomFromString("initialisation");
  ATOM_procedure		= AtomFromString("procedure");
  ATOM_existence_error		= AtomFromString("existence_error");
  ATOM_slash			= AtomFromString("/");
  ATOM_read			= AtomFromString("read");
  ATOM_write			= AtomFromString("write");
  ATOM_append			= AtomFromString("append");

  MODULE_user			= ModuleFromAtom(ATOM_user);
}

static PceObject	NIL;		/* @nil */
static PceObject	DEFAULT;	/* @default */
static PceObject	PROLOG;		/* @prolog */
static PceName		NAME_message;	/* "message" */
static PceName		NAME_obtain;	/* "?" */
static PceName		NAME_call;	/* "call" */
static PceName		NAME_functor;	/* "functor" */
static PceName		NAME_Arity;	/* "_arity" */
static PceName		NAME_Arg;	/* "_arg" */

static void
initPceConstants()
{ NAME_functor	= cToPceName("functor");
  NAME_Arity	= cToPceName("_arity");
  NAME_Arg	= cToPceName("_arg");
  NAME_message	= cToPceName("message");
  NAME_obtain	= cToPceName("?");
  NAME_call	= cToPceName("call");

  NIL		= cToPceAssoc("nil");
  DEFAULT	= cToPceAssoc("default");
  PROLOG	= cToPceAssoc("host");
}


		 /*******************************
		 *	   SICSTUS GLUE		*
		 *******************************/

#ifdef SICSTUS
#define GET_FUNCTOR_BUG 1

#define FUNCTOR_pce1	ATOM_pce, 1
#define	FUNCTOR_pce2	ATOM_pce, 2
#define	FUNCTOR_pce3	ATOM_pce, 3
#define	FUNCTOR_ref1	ATOM_ref, 1
#define	FUNCTOR_string1 ATOM_string, 1
#define	FUNCTOR_module2 ATOM_module, 2

#define initHostConstants()

#define AtomCharp(a)		SP_string_from_atom((a))
#define GetInteger(a, i)	SP_get_integer((a), (i))
#define GetAtom(a, n)		SP_get_atom((a), (n))
#define GetFloat(a, f)		SP_get_float((a), (f))
#define NewTerm()		SP_new_term_ref()
#ifdef GET_FUNCTOR_BUG
#define GetFunctor(t, n, a)	((SP_is_compound(t) || SP_is_atom(t)) && \
				 SP_get_functor((t), (n), (a)))
#else
#define GetFunctor(t, n, a)	SP_get_functor((t), (n), (a))
#endif
#define IsInteger(t)		SP_is_integer(t)
#define IsVar(t)		SP_is_variable(t)
#define GetArg(n, t, a)		SP_get_arg((n), (t), (a))
#define ConsFunctor		SP_cons_functor
#define PutFunctor(t, n, a)	SP_put_functor((t), (n), (a))
#define PutCharp(t, s)		SP_put_string((t), (s))
#define PutInteger(t, i)	SP_put_integer((t), (i))
#define PutFloat(t, f)		SP_put_float((t), (f))
#define Unify(t1, t2)		SP_unify((t1), (t2))
#define PutAtom(t, a)		SP_put_atom((t), (a))
#define PutTerm(t, f)		SP_put_term((t), (f))
#define RaiseException(t)	SP_raise_exception(t)
#define FindPredicate(n, a, m)	SP_pred(n, a, m)
#define OpenForeign()		SP_new_term_refs(0)
#define CloseForeign(fid)	SP_reset_term_refs(fid)
#define InstallPredicate(n, a, f, flags)

#define PROLOG_INSTALL_DISPATCH_FUNCTION(f) \
				{ SP_read_hook = f; }

static int
GetChars(Term t, char **s)
{ if ( SP_get_string(t, s) ||
       SP_get_list_chars(t, s) ||
       SP_get_number_chars(t, s) )
    return TRUE;

  return FALSE;
}


static void
StripModuleTag(Term t, Atom *module, Term p)
{ Atom name;
  int arity;
  Term a = NewTerm();

  PutTerm(p, t);

  while( GetFunctor(p, &name, &arity) &&
	 name == ATOM_module && arity == 2 )
  { Atom m;

    GetArg(1, p, a);
    if ( GetAtom(a, &m) )
    { *module = m;
      GetArg(2, p, p);
    } else
      break;
  }
}


static PceName
GetSelector(Term t, Module *m)
{ Term tmp = NewTerm();

  *m = 0;
  StripModuleTag(t, m, tmp);
  return pceSelector(tmp);
}


static void
UndefinedPredicate(Atom pred, int arity, Atom module)
{ Term et = NewTerm();			/* existence_error(G, 0, ...) */
  Term goal = NewTerm();		/* foo(_, _) */
  Term fa = NewTerm();			/* foo/2 */
  Term culprit = NewTerm();		/* Module:foo/2 */
  Term zero = NewTerm();		/* 0 */
  Term id = NewTerm();			/* procedure */
  Term name = NewTerm();		/* foo */
  Term ar = NewTerm();			/* 2 */
  Term m = NewTerm();			/* Module */
  
  PutFunctor(goal, pred, arity);
  PutInteger(zero, 0);
  PutAtom(id, ATOM_procedure);
  PutAtom(name, pred);
  PutAtom(m, module);
  PutInteger(ar, arity);
  ConsFunctor(fa, ATOM_slash, 2, name, ar);
  ConsFunctor(culprit, ATOM_module, 2, m, fa);
  ConsFunctor(et, ATOM_existence_error, 5, goal, zero, id, culprit, zero);
  
  SP_raise_exception(et);
}

#endif /*SICSTUS*/

		 /*******************************
		 *	   SWI-Prolog GLUE	*
		 *******************************/

#ifdef SWI
#define O_STRING 1
#define HAVE_PREDICATE_INFO 1
#define HAVE_XPCEREF 1			/* _PL_put/get/unify_xpce_reference */

static Functor	FUNCTOR_pce1;
static Functor	FUNCTOR_pce2;
static Functor	FUNCTOR_pce3;
static Functor	FUNCTOR_ref1;
static Functor	FUNCTOR_string1;
static Functor	FUNCTOR_module2;

static long			pl_max_integer;
static PL_dispatch_hook_t	old_dispatch_hook;

#define PROLOG_MAX_INTEGER pl_max_integer

#define AtomCharp(a)		PL_atom_chars((a))
#define GetInteger(a, i)	PL_get_long((a), (i))
#define GetAtom(a, n)		PL_get_atom((a), (n))
#define GetString(t, s, l)	PL_get_string((t), (s), (l))
#define GetFloat(a, f)		PL_get_float((a), (f))
#define NewTerm()		PL_new_term_ref()
#define GetFunctor(t, n, a)	PL_get_name_arity((t), (n), (a))
#define IsInteger(t)		PL_is_integer(t)
#define IsVar(t)		PL_is_variable(t)
#define GetArg(n, t, a)		PL_get_arg((n), (t), (a))
#define ConsFunctor		PL_cons_functor
#define PutFunctor(t, n, a)	PL_put_functor((t), PL_new_functor((n), (a)))
#define PutCharp(t, s)		PL_put_atom_chars((t), (s))
#define PutInteger(t, i)	PL_put_integer((t), (i))
#define PutFloat(t, f)		PL_put_float((t), (f))
#define Unify(t1, t2)		PL_unify((t1), (t2))
#define PutAtom(t, a)		PL_put_atom((t), (a))
#define StripModuleTag(t, m, p)	PL_strip_module((t), (m), (p))
#define FindPredicate(n, a, m)	PL_pred(PL_new_functor(n, a), m)
#define OpenForeign()		PL_open_foreign_frame()
#define CloseForeign(fid)	PL_discard_foreign_frame(fid)

#define META	PL_FA_TRANSPARENT
#define HIDDEN	PL_FA_NOTRACE
#define InstallPredicate(n, a, f, flags) \
	PL_register_foreign(n, a, f, flags)

#if defined(__WIN32__)
#define PROLOG_INSTALL_DISPATCH_FUNCTION(f) {}
#else
#define PROLOG_INSTALL_DISPATCH_FUNCTION(f) \
	(old_dispatch_hook = PL_dispatch_hook(f))
#endif

#define PROLOG_INSTALL_RESET_FUNCTION(f) \
				{ PL_abort_hook(f); }

#define PROLOG_DISPATCH_INPUT   PL_DISPATCH_INPUT
#define PROLOG_DISPATCH_TIMEOUT PL_DISPATCH_TIMEOUT

static void
initHostConstants()
{ pl_max_integer = PL_query(PL_QUERY_MAX_INTEGER);
  
  FUNCTOR_pce1	  = PL_new_functor(ATOM_pce, 1);
  FUNCTOR_pce2    = PL_new_functor(ATOM_pce, 2);
  FUNCTOR_pce3    = PL_new_functor(ATOM_pce, 3);
  FUNCTOR_ref1    = PL_new_functor(ATOM_ref, 1);
  FUNCTOR_string1 = PL_new_functor(ATOM_string, 1);
  FUNCTOR_module2 = PL_new_functor(ATOM_module, 2);
}


static int
GetChars(Term t, char **s)
{ return PL_get_chars(t, s, CVT_ALL|BUF_RING);
}


static void
RaiseException(Term t)
{ predicate_t pred = PL_predicate("pce_error", 1, "user");

  PL_call_predicate(MODULE_user, TRUE, pred, t);
  PL_action(PL_ACTION_TRACE, NULL);
}


static PceName
GetSelector(Term t, Module *m)
{ Term tmp = NewTerm();

  *m = 0;
  PL_strip_module(t, m, tmp);
  return pceSelector(tmp);
}


#ifdef __WIN32__
#include <console.h>

static IOSTREAM *S__iob;		/* Windows DLL version */
static RlcUpdateHook old_update_hook;

#define PROLOG_ITF_INIT() \
	{ S__iob = S__getiob(); }
#define PROLOG_INSTALL_REDRAW_FUNCTION(f) \
	{ old_update_hook = rlc_update_hook(f); }
#define O_SHAREDLIBRARY
#endif

#ifdef O_SHAREDLIBRARY
#ifndef PROLOG_ITF_INIT
#define PROLOG_ITF_INIT() { }
#endif
#define PROLOG_ONEXIT(f)	{ exitpce_hook = (OnExitFunction) f; }

static OnExitFunction		exitpce_hook;

install_t
install()
{ PL_register_foreign("$pce_init", 1, pl_pce_init, PL_FA_TRANSPARENT);
}

install_t
uninstall()
{ PL_dispatch_hook(old_dispatch_hook);
#ifdef __WIN32__
  rlc_update_hook(old_update_hook);
#endif
  if ( exitpce_hook )
    (*exitpce_hook)();
}

#endif /*O_SHAREDLIBRARY*/

#endif /*SWI*/

		 /*******************************
		 *         EXCEPTIONS		*
		 *******************************/

static void
PceException(Atom which, int argc, ...)
{ va_list args;
  Term et = NewTerm();
  Term id = NewTerm();

  PutAtom(id, which);

  va_start(args, argc);
  switch(argc)
  { case 0:
      ConsFunctor(et, FUNCTOR_pce1, id);
      break;
    case 1:
    { Term a0 = va_arg(args, Term);

      ConsFunctor(et, FUNCTOR_pce2, id, a0);
      break;
    }
    case 2:
    { Term a0 = va_arg(args, Term);
      Term a1 = va_arg(args, Term);

      ConsFunctor(et, FUNCTOR_pce3, id, a0, a1);
      break;
    }
    default:
      assert(0);
  }
  va_end(args);

  RaiseException(et);
}



		 /*******************************
		 *	     PRIMITIVES		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PceName	atomToName(Atom a)
	Translate a Prolog atom into an XPCE name object.  This version
	uses the XPCE handle table to cache direct associations between
	XPCE names and Prolog atoms.

PceObject referenceToObject(Term a)
	a is the argment to @/1.  Translate to an XPCE object or raise
	an error.  This function too caches using the 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static PceName
atomToName(Atom a)
{ PceITFSymbol symbol;
  PceName name;
  hostHandle handle = (hostHandle)a;

  if ( (symbol = pceLookupHandle(0, handle)) && symbol->name )
    return symbol->name;

  name = cToPceName(AtomCharp(a));
  pceRegisterName(0, handle, name);
  return name;
}


static Atom
nameToAtom(PceName name)
{ PceCValue value;

  if ( pceToC(name, &value) == PCE_NAME )
  { PceITFSymbol symbol = value.itf_symbol;

    if ( symbol->handle[0] != NULL )
      return (Atom)symbol->handle[0];
    else				/* register? */
      return AtomFromString(pceCharArrayToC(symbol->name));
  }
  
  return (Atom)0;
}


static PceName
pceSelector(Term t)
{ Atom name;

  if ( GetAtom(t, &name) )
    return atomToName(name);

  PceException(ATOM_badSelector, 1, t);
  return PCE_FAIL;
}


static PceName
atomToAssoc(Atom a)
{ return a ? atomToName(a) : NIL;
}


#ifdef HAVE_XPCEREF
static PceObject
refToObject(Term arg)
{ xpceref_t r;

  switch ( _PL_get_xpce_reference(arg, &r) )
  { case TRUE:
    { PceObject obj;

      if ( r.type == PL_INTEGER )
      { if ( !(obj = cToPceReference(r.value.i)) )
	{ Term a = NewTerm();
	  char *descr = pcePPReference(cToPceInteger(r.value.i));

	  PutCharp(a, descr);
	  PceException(ATOM_badIntegerReference, 1, a);

	  return PCE_FAIL;
	}
      } else
      { hostHandle handle = (hostHandle) r.value.a;
	PceITFSymbol symbol = pceLookupHandle(0, handle);

	if ( symbol && symbol->object )
	  obj = symbol->object;
	else if ( (obj = cToPceAssoc(AtomCharp(r.value.a))) )
	  pceRegisterAssoc(0, handle, obj);
	else
	{ PceException(ATOM_unknownReference, 1, arg);

	  return PCE_FAIL;
	}
      }

      return obj;
    }
    case FALSE:
      return PCE_FAIL;
    default:
      PceException(ATOM_badReference, 1, arg);
      return PCE_FAIL;
  }
}


#else /*HAVE_XPCEREF*/

static PceObject
referenceToObject(Term arg)
{ PceObject obj;
  long r;
  Atom name;

  if ( GetInteger(arg, &r) )
  {
#ifdef PROLOG_MAX_INTEGER		/* exploit unsigned representation */
    r &= PROLOG_MAX_INTEGER;
#endif
    
    if ( !(obj = cToPceReference(r)) )
    { Term a = NewTerm();
      char *descr = pcePPReference(cToPceInteger(r));

      PutCharp(a, descr);
      PceException(ATOM_badIntegerReference, 1, a);

      return PCE_FAIL;
    }
  } else if ( GetAtom(arg, &name) )
  { hostHandle handle = (hostHandle) name;
    PceITFSymbol symbol = pceLookupHandle(0, handle);

    if ( symbol && symbol->object )
      obj = symbol->object;
    else if ( (obj = cToPceAssoc(AtomCharp(name))) )
      pceRegisterAssoc(0, handle, obj);
    else
    { PceException(ATOM_unknownReference, 1, arg);

      return PCE_FAIL;
    }
  } else
  { Term t = NewTerm();

    ConsFunctor(t, FUNCTOR_ref1, arg);
    PceException(ATOM_badReference, 1, t);

    return PCE_FAIL;
  }


  return obj;
}

#endif /*HAVE_XPCEREF*/

static int
unifyReferenceArg(Term t, int type, PceCValue value)
{ Term t2 = NewTerm();			/* Exploit SWI-Prolog PL_unify-* */
  
  if ( type == PCE_REFERENCE )
  { PutInteger(t2, value.integer);
  } else
  { PceITFSymbol symbol = value.itf_symbol;
    Atom a = (Atom) symbol->handle[0];

    if ( a )
      PutAtom(t2, a);
    else
      PutCharp(t2, pceCharArrayToC(symbol->name));
  }

  return Unify(t, t2);
}


static int
unifyReference(Term t, int type, PceCValue value)
{
#ifdef HAVE_XPCEREF
  xpceref_t r;

  if ( type == PCE_REFERENCE )
  { r.type = PL_INTEGER;
    r.value.i = value.integer;
  } else
  { PceITFSymbol symbol = value.itf_symbol;

    r.type = PL_ATOM;
    if ( symbol->handle[0] )
      r.value.a = (Atom)symbol->handle[0];
    else
      r.value.a = AtomFromString(pceCharArrayToC(symbol->name));
  }
  return _PL_unify_xpce_reference(t, &r);

#else /*HAVE_XPCEREF*/
    
  Term t2 = NewTerm();
  Term r  = NewTerm();
  
  if ( type == PCE_REFERENCE )
  { PutInteger(t2, value.integer);
  } else
  { PceITFSymbol symbol = value.itf_symbol;

    if ( symbol->handle[0] )
      PutAtom(t2, (Atom)symbol->handle[0]);
    else
      PutCharp(t2, pceCharArrayToC(symbol->name));
  }
  ConsFunctor(r, FUNCTOR_ref1, t2);

  return Unify(t, r);
#endif /*HAVE_XPCEREF*/
}


		 /*******************************
		 *	   TERM-TO-OBJECT	*
		 *******************************/

static PceObject
do_new(Term ref, Term t)
{ Atom n;
  int arity;
  Atom assoc;
  PceObject rval;

  if ( GetFunctor(ref, &n, &arity) )
  { Term a = NewTerm();
    Atom name;

    if ( n != ATOM_ref || arity != 1 )
    { PceException(ATOM_badReference, 1, ref);
      return PCE_FAIL;
    }

    GetArg(1, ref, a);

    if ( GetAtom(a, &name) )		/* new(@foo, ...) */
      assoc = name;
    else if ( IsVar(a) )		/* new(@X, ...) */
      assoc = 0;
    else				/* new(@35, ...) */
    { PceException(ATOM_badReference, 1, ref);
      return PCE_FAIL;
    } 

    if ( (rval = termToObject(t, assoc, TRUE)) )
    { PceCValue value;
      int type = pceToCReference(rval, &value);

      if ( unifyReferenceArg(a, type, value) )
	return rval;
    }

    return PCE_FAIL;
  } else if ( IsVar(ref) )
  { if ( (rval = termToObject(t, NULLATOM, TRUE)) )
    { PceCValue value;
      int type = pceToCReference(rval, &value);

      if ( unifyReference(ref, type, value) )
	return rval;
    }

    return PCE_FAIL;
  } else
  { PceException(ATOM_badReference, 1, ref);

    return PCE_FAIL;
  }
}


static PceObject
termToObject(Term t, Atom assoc, int new)
{ Atom functor;
  int arity;
  PceObject rval;

#ifdef HAVE_XPCEREF
  if ( (rval = refToObject(t)) )
    return rval;
#endif

  if ( GetFunctor(t, &functor, &arity) )
  { PceName name = atomToName(functor);
    
					/* Just an atom */
    if ( arity == 0 )
      return (new ? pceNew(atomToAssoc(assoc), name, 0, NULL) : name);

#ifndef HAVE_XPCEREF
					/* @Ref */
    if ( functor == ATOM_ref && arity == 1 )
    { Term a = NewTerm();

      GetArg(1, t, a);
      rval = referenceToObject(a);

      return rval;
    }
#endif
					/* new/[1,2] */
    if ( functor == ATOM_new )
    { if ( arity == 1 )			/* new(chain) */
      { Term a = NewTerm();

	GetArg(1, t, a);
	return termToObject(a, 0, TRUE);

	return rval;
      }
      if ( arity == 2 )			/* new(B, box) */
      { Term r = NewTerm();
	Term n = NewTerm();

	GetArg(1, t, r);
	GetArg(2, t, n);
	return do_new(r, n);
      }
    }

					/* string(hello) */
    if ( functor == ATOM_string && arity == 1 )
    { char *s;
      Term a = NewTerm();
      PceName pceassoc = atomToAssoc(assoc);

      GetArg(1, t, a);
      if ( GetChars(a, &s) )
	return cToPceString(pceassoc, s);

      PceException(ATOM_badStringArgument, 1, a);
      return PCE_FAIL;
    }

#ifdef HAVE_XPCEREF			/* avoid new() on bad-references */
    if ( functor == ATOM_ref && arity == 1 )
      return PCE_FAIL;
#endif

					/* Class(Args) */
    { ArgVector(argv, arity);
      Term a = NewTerm();
      int done = 0;

      GetArg(1, t, a);
      if ( !(argv[done++] = termToObject(a, NULLATOM, FALSE)) )
	return PCE_FAIL;

#if O_MODULE
					/* message(@prolog, ?(@prolog, ...) */
      if ( arity >= 2 &&
	   argv[0] == PROLOG &&
	   (name == NAME_message || name == NAME_obtain) )
      { Atom a2;

	GetArg(2, t, a);
					/* message(@prolog, call, ...) */
	if ( GetAtom(a, &a2) && a2 == ATOM_call )
	{ argv[done++] = NAME_call;
	  GetArg(3, t, a);
	}

	StripModuleTag(a, &DefaultModule, a);
	if ( GetAtom(a, &a2) && DefaultModule )
	{ char tmp[MAXMODULENAME];
	  char *s;
	  char *d = tmp;
	      
	  for(s = (char *)AtomCharp(ModuleName(DefaultModule)); *d++ = *s++; );
	  d[-1] = ':';

	  s = (char *)AtomCharp(a2);
	  while( *s && *s != ':' )
	    *d++ = *s++;

	  if ( *s == ':' )
	  { argv[done++] = atomToName(a2);
	  } else
	  { *d = EOS;
	    argv[done++] = cToPceName(tmp);
	  }
	}
      }
#endif /*O_MODULE*/

      for( ; done < arity; done++ )
      { GetArg(done+1, t, a);
	if ( !(argv[done] = termToObject(a, NULLATOM, FALSE)) )
	  return PCE_FAIL;
      }

      return pceNew(atomToAssoc(assoc), name, arity, argv);
    }
  } else				/* not a term */
  { double f;

    if ( IsInteger(t) )			/* integer */
    { long r;

      GetInteger(t, &r);
      return cToPceInteger(r);
    }

#ifdef O_STRING
  { char *s;
    int len;
    if ( GetString(t, &s, &len) )	/* string object (if supported) */
      return cToPceString(atomToAssoc(assoc), s);
  }
#endif

    if ( GetFloat(t, &f) )		/* floating point number */
      return cToPceReal(f);

					/* anything else */
    PceException(ATOM_badObjectDescription, 1, t);
    return PCE_FAIL;
  }
}


		 /*******************************
		 *	  OBJECT-TO-TERM	*
		 *******************************/

static __inline int
atomIsName(Atom a, PceCValue value)
{ PceITFSymbol symbol = value.itf_symbol;

  if ( symbol->handle[0] != NULL )
    return a == (Atom)symbol->handle[0];
  else
    return strcmp(AtomCharp(a), pceCharArrayToC(symbol->name)) == 0;
}


static int
unifyObject(Term t, PceObject obj, int top)
{ PceCValue value;
  int pcetype;
  char *s;
  Term tmpt;

  switch( (pcetype = pceToC(obj, &value)) )
  { case PCE_INTEGER:			/* integer */
      tmpt = NewTerm();
      PutInteger(tmpt, value.integer);
      return Unify(t, tmpt);
    case PCE_REAL:			/* float (real object) */
      tmpt = NewTerm();
      PutFloat(tmpt, value.real);
      return Unify(t, tmpt);
    case PCE_NAME:			/* name */
    { PceITFSymbol symbol = value.itf_symbol;

      tmpt = NewTerm();
      if ( symbol->handle[0] != NULL )
	PutAtom(tmpt, (Atom)symbol->handle[0]);
      else
	PutCharp(tmpt, pceCharArrayToC(symbol->name));

      return Unify(t, tmpt);
    }
    case PCE_REFERENCE:
    case PCE_ASSOC:
      if ( !top )
      { Atom n;
	int a;

	if ( IsVar(t) )			/* get(R, S, Var) */
	  return unifyReference(t, pcetype, value);

					/* get(R, S, @something) */
	if ( GetFunctor(t, &n, &a) && n == ATOM_ref && a == 1 )
	{ tmpt = NewTerm();

	  GetArg(1, t, tmpt);
	  return unifyReferenceArg(tmpt, pcetype, value);
	}
      }
  }

  if ( (s = pceStringToC(obj)) )	/* string: handle special */
  { Atom name;
    int arity;

    if ( IsVar(t) )
    { Term a = NewTerm();

      PutCharp(a, s);
      tmpt = NewTerm();
      ConsFunctor(tmpt, FUNCTOR_string1, a);
      return Unify(t, tmpt);
    } else if ( GetFunctor(t, &name, &arity) &&
		name == ATOM_string && arity == 1 )
    { Term a = NewTerm();

      PutCharp(a, s);
      tmpt = NewTerm();
      GetArg(1, t, tmpt);
      return Unify(tmpt, a);
    } else
      return FALSE;
  }

  { Atom name;
    int n, arity;
    Atom pname;				/* name of Pce object */
    int parity;				/* its `arity' */
    PceObject got;			/* temp variable */
    Term at = NewTerm();

    if ( !(got = pceGet(obj, NAME_functor, 0, NULL)) ||
	 !(pname = nameToAtom(got)) )
      return FALSE;
    if ( !(got = pceGet(obj, NAME_Arity, 0, NULL)) ||
	 pceToC(got, &value) != PCE_INTEGER )
      return FALSE;
    parity = value.integer;
    
    if ( GetFunctor(t, &name, &arity) )
    { if ( name != pname || arity != parity )
	return FALSE;
      for(n=1; n<=arity; n++)
      { PceObject pcen = cToPceInteger(n);
  
	if ( (got = pceGet(obj, NAME_Arg, 1, &pcen)) )
	{ GetArg(n, t, at);
	  
	  if ( !unifyObject(at, got, FALSE) )
	    return FALSE;
	} else
	  return FALSE;
      }

      return TRUE;
    } else if ( IsVar(t) )
    { Term t2 = NewTerm();

      PutFunctor(t2, pname, parity);
      for(n=1; n<=parity; n++)
      { PceObject pcen = cToPceInteger(n);
  
	if ( (got = pceGet(obj, NAME_Arg, 1, &pcen)) )
	{ GetArg(n, t2, at);
	  
	  if ( !unifyObject(at, got, FALSE) )
	    return FALSE;
	} else
	  return FALSE;
      }
      return Unify(t, t2);
    } else
      return FALSE;
  }
}


		 /*******************************
		 *	  VMI FUNCTIONS		*
		 *******************************/

					/* NEW */
foreign_t
pl_new(Term assoc, Term descr)
{ AnswerMark mark;
  PceObject obj;
  Term d = NewTerm();

  DefaultModule = 0;
  StripModuleTag(descr, &DefaultModule, d);
  markAnswerStack(mark);
  obj = do_new(assoc, d);
  rewindAnswerStack(mark, obj);

  return obj ? TRUE : FALSE;
}

					/* SEND */
foreign_t
pl_send0(Term rec, Term sel)
{ int rval;
  AnswerMark mark;
  PceObject receiver;
  PceName selector;

  markAnswerStack(mark);
  rval = ((receiver = termToObject(rec, NULLATOM, FALSE)) &&
	  (selector = GetSelector(sel, &DefaultModule)) &&
	  pceSend(receiver, selector, 0, NULLATOM));
  rewindAnswerStack(mark, NIL);

  return rval ? TRUE : FALSE;
} 


foreign_t
pl_send1(Term rec, Term sel, Term arg)
{ int rval;
  AnswerMark mark;
  PceObject receiver;
  PceName selector;
  PceObject pcearg1;

  markAnswerStack(mark);
  rval = ((receiver = termToObject(rec, NULLATOM, FALSE)) &&
	  (selector = GetSelector(sel, &DefaultModule)) &&
	  (pcearg1  = termToObject(arg, NULLATOM, FALSE)) &&
	  pceSend(receiver, selector, 1, &pcearg1));
  rewindAnswerStack(mark, NIL);

  return rval;
} 


foreign_t
pl_send2(Term rec, Term sel, Term a1, Term a2)
{ int rval;
  AnswerMark mark;
  PceObject receiver;
  PceName selector;
  PceObject pa[2];

  markAnswerStack(mark);
  rval = ((receiver = termToObject(rec, NULLATOM, FALSE)) &&
	  (selector = GetSelector(sel, &DefaultModule)) &&
	  (pa[0]    = termToObject(a1, NULLATOM, FALSE)) &&
	  (pa[1]    = termToObject(a2, NULLATOM, FALSE)) &&
	  pceSend(receiver, selector, 2, pa));
  rewindAnswerStack(mark, NIL);

  return rval;
} 


foreign_t
pl_send3(Term rec, Term sel, Term a1, Term a2, Term a3)
{ int rval;
  AnswerMark mark;
  PceObject receiver;
  PceName selector;
  PceObject pa[3];

  markAnswerStack(mark);
  rval = ((receiver = termToObject(rec, NULLATOM, FALSE)) &&
	  (selector = GetSelector(sel, &DefaultModule)) &&
	  (pa[0]    = termToObject(a1, NULLATOM, FALSE)) &&
	  (pa[1]    = termToObject(a2, NULLATOM, FALSE)) &&
	  (pa[2]    = termToObject(a3, NULLATOM, FALSE)) &&
	  pceSend(receiver, selector, 3, pa));
  rewindAnswerStack(mark, NIL);

  return rval;
} 


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sendn(Rec, Sel, arguments(aap, noot, mies, ...))
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

foreign_t
pl_sendn(Term rec, Term sel, Term args)
{ int rval;
  AnswerMark mark;
  PceObject receiver;
  PceName selector;

  markAnswerStack(mark);
  if ( !(receiver = termToObject(rec, NULLATOM, FALSE)) ||
       !(selector = GetSelector(sel, &DefaultModule)) )
  { rval = FALSE;
  } else
  { int arity;
    Atom name;
    Term t = NewTerm();
    int i;

    if ( GetFunctor(args, &name, &arity) )
    { ObjectVector(av, arity);

      for(i=1; i<=arity; i++)
      { GetArg(i, args, t);
	if ( !(av[i-1] = termToObject(t, NULLATOM, FALSE)) )
	{ rval = FALSE;
	  goto out;
	}
      }
      rval = pceSend(receiver, selector, arity, av);
    } else
      rval = FALSE;
  }

out:
  rewindAnswerStack(mark, NIL);

  return rval;
} 

					/* GET */
foreign_t
pl_get0(Term rec, Term sel, Term ret)
{ AnswerMark mark;
  PceObject receiver, rval;
  PceName selector;

  markAnswerStack(mark);
  rval = (((receiver = termToObject(rec, NULLATOM, FALSE)) &&
	   (selector = GetSelector(sel, &DefaultModule)))
	      ? pceGet(receiver, selector, 0, NULLATOM)
	      : (PceObject)PCE_FAIL);
  rewindAnswerStack(mark, rval);

  return rval ? unifyObject(ret, rval, FALSE) : FALSE;
}


foreign_t
pl_get1(Term rec, Term sel, Term a1, Term ret)
{ AnswerMark mark;
  PceObject receiver, rval;
  PceName selector;
  PceObject av[1];

  markAnswerStack(mark);
  rval = (((receiver = termToObject(rec, NULLATOM, FALSE)) &&
	   (selector = GetSelector(sel, &DefaultModule)) &&
	   (av[0]    = termToObject(a1, NULLATOM, FALSE)))
	   	? pceGet(receiver, selector, 1, av)
	        : (PceObject)PCE_FAIL);
  rewindAnswerStack(mark, rval);

  return rval ? unifyObject(ret, rval, FALSE) : FALSE;
}


foreign_t
pl_get2(Term rec, Term sel, Term a1, Term a2, Term ret)
{ AnswerMark mark;
  PceObject receiver, rval;
  PceName selector;
  PceObject av[2];

  markAnswerStack(mark);
  rval = (((receiver = termToObject(rec, NULLATOM, FALSE)) &&
	   (selector = GetSelector(sel, &DefaultModule)) &&
	   (av[0]    = termToObject(a1, NULLATOM, FALSE)) &&
	   (av[1]    = termToObject(a2, NULLATOM, FALSE)))
		? pceGet(receiver, selector, 2, av)
	  	: (PceObject)PCE_FAIL);
  rewindAnswerStack(mark, rval);

  return rval ? unifyObject(ret, rval, FALSE) : FALSE;
}


foreign_t
pl_get3(Term rec, Term sel, Term a1, Term a2, Term a3, Term ret)
{ AnswerMark mark;
  PceObject receiver, rval;
  PceName selector;
  PceObject av[3];

  markAnswerStack(mark);
  rval = (((receiver = termToObject(rec, NULLATOM, FALSE)) &&
	   (selector = GetSelector(sel, &DefaultModule)) &&
	   (av[0]    = termToObject(a1, NULLATOM, FALSE)) &&
	   (av[1]    = termToObject(a2, NULLATOM, FALSE)) &&
	   (av[2]    = termToObject(a3, NULLATOM, FALSE)))
		? pceGet(receiver, selector, 3, av)
	  	: (PceObject)PCE_FAIL);
  rewindAnswerStack(mark, rval);

  return rval ? unifyObject(ret, rval, FALSE) : FALSE;
}


foreign_t
pl_getn(Term rec, Term sel, Term args, Term ret)
{ PceObject rval = NIL;
  int plrval;
  AnswerMark mark;
  PceObject receiver;
  PceName selector;


  markAnswerStack(mark);
  if ( !(receiver = termToObject(rec, NULLATOM, FALSE)) ||
       !(selector = GetSelector(sel, &DefaultModule)) )
  { plrval = FALSE;
  } else
  { int arity;
    Atom name;
    Term t = NewTerm();
    int i;

    if ( GetFunctor(args, &name, &arity) )
    { ObjectVector(av, arity);

      for(i=1; i<=arity; i++)
      { GetArg(i, args, t);
	if ( !(av[i-1] = termToObject(t, NULLATOM, FALSE)) )
	{ plrval = FALSE;
	  goto out;
	}
      }
      if ( (rval = pceGet(receiver, selector, arity, av)) )
	plrval = TRUE;
      else
	plrval = FALSE;
    } else
      plrval = FALSE;
  }

out:
  rewindAnswerStack(mark, rval);

  return plrval ? unifyObject(ret, rval, FALSE) : FALSE;
}



		 /*******************************
		 *	    OBJECT/[1,2]	*
		 *******************************/

foreign_t
pl_object1(Term ref)
{ Atom name;
  int arity;

  if ( GetFunctor(ref, &name, &arity) && name == ATOM_ref && arity == 1 )
  { Term a = NewTerm();
    Atom refname;

    GetArg(1, ref, a);
    if ( GetAtom(a, &refname) )
      return pceExistsAssoc(atomToName(refname));
    if ( IsInteger(a) )
    { long refi;
      
      GetInteger(a, &refi);
      return pceExistsReference(refi);
    }
  }

  return FALSE;
}


foreign_t
pl_object2(Term ref, Term description)
{ PceObject obj;

  if ( (obj = termToObject(ref, NULLATOM, FALSE)) )
    return unifyObject(description, obj, TRUE);

  return FALSE;
}


		 /*******************************
		 *	  PROLOG SEND/GET	*
		 *******************************/

#ifdef SWI
#define prolog_exception(r) (r)
#else
static int
prolog_exception(r)
int r;
{ switch(r)
  { case SP_SUCCESS:
      return TRUE;
    case SP_FAILURE:
      return FALSE;
    case SP_ERROR:
    default:
    { SP_term_ref t = SP_new_term_ref();
      SP_pred_ref p = SP_predicate("print_exception", 1, "pce_host");

      SP_exception_term(t);
      (void) SP_query(p, t);
    }
  }
}
#endif

static void
split_selector(PceObject sel, Atom *predname, Module *module)
{
#if O_MODULE
{ char *name = pceCharArrayToC(sel);
  char modname[MAXMODULENAME];
  char *q = modname;
  char *s = name;
  int i = MAXMODULENAME;
  
  while( *s && --i != 0 )
  { if ( *s == ':' )
    { *q = EOS;
      *module     = ModuleFromAtom(AtomFromString(modname));
      *predname   = AtomFromString(&s[1]);
      return;
    }
    *q++ = *s++;
  }
  *module = MODULE_user;
  *predname = nameToAtom(sel);
}
#else
  *module = MODULE_user;
  *predname = nameToAtom(sel);
#endif /*O_MODULE*/
}


static Predicate
find_predicate(PceObject sel, int argc)
{ Atom name;
  Module module;
  
  split_selector(sel, &name, &module);
  return FindPredicate(name, argc, module);
}


static int
PrologSend(PceObject prolog, PceObject sel, int argc, PceObject *argv)
{ Fid fid = OpenForeign();
  Predicate pred = find_predicate(sel, argc);
  int i, rval;

#ifdef SWI
  Term terms = PL_new_term_refs(argc);
  qid_t qid;

  for(i=0; i<argc; i++)
  { if ( !unifyObject(terms+i, argv[i], FALSE) )
    { rval = FALSE;
      goto out;
    }
  }

  qid  = PL_open_query(MODULE_user, TRUE, pred, terms);
  rval = PL_next_solution(qid);
  PL_close_query(qid);
#else /*~SWI*/
  if ( pred )
  { SP_term_ref terms[argc];
    SP_qid qid;

    for(i=0; i<argc; i++)
    { terms[i] = SP_new_term_ref();
      SP_put_variable(terms[i]);
      if ( !unifyObject(terms[i], argv[i], FALSE) )
      { rval = FALSE;
	goto out;
      }
    }

    if ( (qid = SP_open_query_array(pred, terms)) )
    { rval = prolog_exception(SP_next_solution(qid));
      SP_close_query(qid);
    } else
    { PceException(SP_atom_from_string("open_query"), 0);
      rval = FALSE;
    }
  } else
  { Atom name, module;

    split_selector(sel, &name, &module);
    UndefinedPredicate(name, argc, module);
    rval = FALSE;
  }
#endif /*SWI*/

out:
  CloseForeign(fid);
  return rval;
}


static PceObject
PrologGet(PceObject prolog, PceObject sel, int argc, PceObject *argv)
{ Fid fid = OpenForeign();
  Predicate pred = find_predicate(sel, argc+1);
  int i;
  PceObject obj;

#ifdef SWI
  Term terms = PL_new_term_refs(argc+1);
  qid_t qid;
  int rval;

  for(i=0; i<argc; i++)
  { if ( !unifyObject(terms+i, argv[i], FALSE) )
    { obj = PCE_FAIL;
      goto out;
    }
  }

  qid  = PL_open_query(MODULE_user, TRUE, pred, terms);
  rval = PL_next_solution(qid);
  PL_cut_query(qid);
  if ( rval )
    obj = termToObject(terms+argc, NULLATOM, FALSE);
  else
    obj = PCE_FAIL;

#else /*~SWI*/

  if ( pred )
  { SP_term_ref terms[argc+1];
    SP_qid qid;

    for(i=0; i<argc; i++)
    { terms[i] = SP_new_term_ref();
      SP_put_variable(terms[i]);
      if ( !unifyObject(terms[i], argv[i], FALSE) )
      { obj = PCE_FAIL;
	goto out;
      }
    }
    terms[argc] = SP_new_term_ref();
    SP_put_variable(terms[argc]);	/* trailing variable for result */

    if ( (qid = SP_open_query_array(pred, terms)) )
    { if ( prolog_exception(SP_next_solution(qid)) )
	obj = termToObject(terms[argc], NULLATOM, FALSE);
      else
	obj = PCE_FAIL;
      SP_close_query(qid);
    } else
    { PceException(SP_atom_from_string("open_query"), 0);
      obj = PCE_FAIL;
    }
  } else
  { Atom name, module;

    split_selector(sel, &name, &module);
    UndefinedPredicate(name, argc+1, module);
    obj = PCE_FAIL;
  }

#endif /*SWI*/

out:
  CloseForeign(fid);
  return obj;
}

		 /*******************************
		 *	 PREDICATE CALLBACK	*
		 *******************************/

#ifndef HAVE_PREDICATE_INFO
typedef struct
{ SP_pred_ref	predicate;		/* the actual predicate */
  Atom		name;			/* atom handle for name */
  Atom		module;			/* atom handle for the module */
  int		arity;			/* arity of the predicate */
} predicate_handle, *PredicateHandle;
#endif

foreign_t
pl_pce_predicate_reference(Term pred, Term ref)
{ if ( IsVar(pred) )
  { PceObject obj;
    void *ptr;

    if ( (obj = termToObject(ref, NULLATOM, FALSE)) &&
	 (ptr = pcePointerToC(obj)) )
    { Atom name;
      int arity;
      Module module;
      Term t = NewTerm();
      Term h = NewTerm();
      Term m = NewTerm();
#ifdef HAVE_PREDICATE_INFO
      PL_predicate_info(ptr, &name, &arity, &module);
#else
      PredicateHandle p = ptr;
      name = p->name;
      arity = p->arity;
      module = p->module;
#endif
      PutAtom(m, ModuleName(module));
      PutFunctor(h, name, arity);
      ConsFunctor(t, FUNCTOR_module2, m, h);
      return Unify(pred, t);
    }
  } else
  { Module m = 0;
    Atom name;
    int arity;
    Predicate predicate;
    Term p = NewTerm();
    
    StripModuleTag(pred, &m, p);
    if ( !GetFunctor(p, &name, &arity) )
      return FALSE;

    if ( (predicate = FindPredicate(name, arity, m)) )
    {
#ifdef HAVE_PREDICATE_INFO
      PceObject pceref = cToPcePointer(predicate);
#else
      PredicateHandle h = (PredicateHandle) malloc(sizeof(predicate_handle));
      PceObject pceref = cToPcePointer(h);
	
      h->predicate = predicate;
      h->name      = name;
      h->module    = m;
      h->arity     = arity;
#endif
      return unifyObject(ref, pceref, FALSE);
    }
  }

  return FALSE;
}


static void
#ifdef SWI
pushObject(Term t, PceObject obj)
{ PceCValue value;
  int pcetype;
  Atom avalue;
#else /*~SWI*/
pushObject(Term *pt, PceObject obj)
{ PceCValue value;
  int pcetype;
  Atom avalue;
  Term t;

  *pt = NewTerm();
  t = *pt;
#endif /*SWI*/

  switch( pcetype = pceToC(obj, &value) )
  { case PCE_REFERENCE:
    {
#ifdef HAVE_XPCEREF
      _PL_put_xpce_reference_i(t, value.integer);
#else
      Term t2 = NewTerm();

      PutInteger(t2, value.integer);
      ConsFunctor(t, FUNCTOR_ref1, t2);
#endif
      break;
    }
    case PCE_ASSOC:
    { PceITFSymbol symbol = value.itf_symbol;

      if ( symbol->handle[0] != NULL )
	avalue = (Atom) symbol->handle[0];
      else
	avalue = AtomFromString(pceCharArrayToC(symbol->name));

#ifdef HAVE_XPCEREF
      _PL_put_xpce_reference_a(t, avalue);
#else
      { Term t2 = NewTerm();
	PutAtom(t2, avalue);
	ConsFunctor(t, FUNCTOR_ref1, t2);
      }
#endif

      break;
    }
    case PCE_INTEGER:
      PutInteger(t, value.integer);

      break;
    case PCE_NAME:
      { PceITFSymbol symbol = value.itf_symbol;

	if ( symbol->handle[0] != NULL )
	  avalue = (Atom) symbol->handle[0];
	else
	  avalue = AtomFromString(pceCharArrayToC(symbol->name));
      }
      PutAtom(t, avalue);

      break;
    case PCE_REAL:
      PutFloat(t, value.real);

      break;
    default:
      assert(0);
  }
}


#ifdef PREDICATE_PER_CLASS
#define EXCESS_ARGS 2
#else
#define EXCESS_ARGS 1
#endif

static int
PrologCallProc(PceObject handle,
	       PceObject rec, PceObject sel, int argc, PceObject objv[])
{ void *ptr = pcePointerToC(handle);

  if ( ptr != PCE_NO_POINTER )
  { Fid fid = OpenForeign();
    int i, rval;

#ifdef SWI
    Predicate p = ptr;
    Term t0     = PL_new_term_refs(argc+EXCESS_ARGS);
    Term ap     = t0;
    qid_t qid;

#ifdef PREDICATE_PER_CLASS
    pushObject(ap++, sel);
#endif
    pushObject(ap++, rec);
    for(i=argc; i > 0; i--)
      pushObject(ap++, *objv++);
    
    if ( (qid = PL_open_query(MODULE_user, TRUE, p, t0)) )
    { rval = PL_next_solution(qid);
      PL_close_query(qid);
    }

#else /*~SWI*/

    PredicateHandle h = ptr;
    TermVector(tv, h->arity);
    Term *ap = tv;
    SP_pred_ref p = h->predicate;
    SP_qid qid;

    if ( argc+EXCESS_ARGS != h->arity )
    { PceException(AtomFromString("inconsistent_argc"), 0);
      return PCE_FAIL;
    }

#ifdef PREDICATE_PER_CLASS
    pushObject(ap++, sel);
#endif
    pushObject(ap++, rec);
    for(i=argc; i > 0; i--)
      pushObject(ap++, *objv++);
    
    if ( (qid = SP_open_query_array(p, tv)) )
    { rval = SP_next_solution(qid);
      SP_close_query(qid);
    }
    
    rval = prolog_exception(rval);

#endif /*SWI*/

    CloseForeign(fid);

    return rval;
  }

  PceException(AtomFromString("no_predicate_reference"), 0);
  return PCE_FAIL;
}


static PceObject
PrologCallFunc(PceObject handle,
	       PceObject rec, PceObject sel, int argc, PceObject objv[])
{ void *ptr = pcePointerToC(handle);

  if ( ptr != PCE_NO_POINTER )
  { Fid fid = OpenForeign();
    int i, rval;

#ifdef SWI
    Predicate p = ptr;
    Term t0 = PL_new_term_refs(argc+EXCESS_ARGS+1);
    Term ap = t0;
    PceObject obj;
    qid_t qid;

#ifdef PREDICATE_PER_CLASS
    pushObject(ap++, sel);
#endif
    pushObject(ap++, rec);
    for(i=argc; i > 0; i--)
      pushObject(ap++, *objv++);

    qid = PL_open_query(MODULE_user, TRUE, p, t0);
    rval = PL_next_solution(qid);
    PL_cut_query(qid);
    if ( rval )
      obj = termToObject(ap, NULLATOM, FALSE);
    else
      obj = PCE_FAIL;

    CloseForeign(fid);
    return obj;

#else /*~SWI*/
    PredicateHandle h = ptr;
    TermVector(tv, h->arity);
    Term *ap = tv;
    SP_pred_ref p = h->predicate;
    SP_qid qid;

    if ( argc+EXCESS_ARGS+1 != h->arity )
    { PceException(AtomFromString("inconsistent_argc"), 0);
      return PCE_FAIL;
    }

#ifdef PREDICATE_PER_CLASS
    pushObject(ap++, sel);
#endif
    pushObject(ap++, rec);
    for(i=argc; i > 0; i--)
      pushObject(ap++, *objv++);
    *ap = SP_new_term_ref();
    SP_put_variable(*ap);

    if ( (qid = SP_open_query_array(p, tv)) )
    { PceObject obj;
      rval = SP_next_solution(qid);

      if ( prolog_exception(rval) )
	obj = termToObject(*ap, NULLATOM, FALSE);
      else
	obj = PCE_FAIL;

      SP_close_query(qid);

      CloseForeign(fid);
      return obj;
    }
#endif /*SWI*/
  }

  PceException(AtomFromString("no_predicate_reference"), 0);
  return PCE_FAIL;
}

		 /*******************************
		 *	 STREAM CONNECTION	*
		 *******************************/

#ifdef SWI

static int
Swrite_pce(void *handle, char *buf, int size)
{ return pceWrite((int)handle, buf, size);
}


static int
Sread_pce(void *handle, char *buf, int size)
{ return pceRead((int)handle, buf, size);
}


static long
Sseek_pce(void *handle, long offset, int whence)
{ return pceSeek((int)handle, offset, whence);
}


static int
Sclose_pce(void *handle)
{ return pceClose((int)handle);
}


IOFUNCTIONS pceFunctions =
{ Sread_pce,
  Swrite_pce,
  Sseek_pce,
  Sclose_pce
};


static foreign_t
pl_pce_open(Term t, Term mode, Term plhandle)
{ PceObject obj;

  if ( (obj = termToObject(t, NULL, FALSE)) )
  { int flags, sflags = SIO_LBUF|SIO_RECORDPOS;
    int handle;
    Atom m = NULLATOM;

    if ( GetAtom(mode, &m) )
    { if ( m == ATOM_read )
      { flags = PCE_RDONLY;
	sflags |= SIO_INPUT;
      } else if ( m == ATOM_write )
      { flags = PCE_WRONLY|PCE_TRUNC;
	sflags |= SIO_OUTPUT;
      } else if ( m == ATOM_append )
      { flags = PCE_WRONLY|PCE_APPEND;
	sflags |= SIO_OUTPUT;
      }
    }

    if ( !m )
    { PceException(AtomFromString("domain_error"), 1, mode);
      PL_fail;
    }

    if ( (handle = pceOpen(obj, flags)) >= 0 )
    { IOSTREAM *s = Snew((void *)handle, sflags, &pceFunctions);

      return PL_open_stream(plhandle, s);
    } else
    { Term a = NewTerm();

      PutAtom(a, AtomFromString(pceOsError()));
    
      PceException(AtomFromString("system_error"), 1, a);
    }
  } else
    PceException(ATOM_badObjectDescription, 1, t);

  PL_fail;
}

#endif /*SWI*/

		 /*******************************
		 *	  EVENT-DISPATCH	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TIMEOUT defines  the number  of milliseconds  to  wait for  something to
happen in PCE.  If this value  is 0,  PCE  will wait indefinitely for an
event or input.

For linux this value is currently set to 250 because linux's select call
appears  not  to  be   broken  when   a   signal  (notably  SIGCHLD  in 
unx-process.c) arrives.  This way pce will anyway  see the signal ...  A
better solution for signal handling is to be searched for (also avoiding
the possibility of reentrance at moments this is not allowed in PCE ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef PROLOG_DISPATCH_INPUT
#define PROLOG_DISPATCH_INPUT 1
#define PROLOG_DISPATCH_TIMEOUT 0
#endif

#ifndef TIMEOUT
#define TIMEOUT 250
#endif

static int
pce_dispatch(int fd)
{ if ( pceDispatch(fd, TIMEOUT) == PCE_DISPATCH_INPUT )
    return PROLOG_DISPATCH_INPUT;

  return PROLOG_DISPATCH_TIMEOUT;
}

#ifdef SICSTUS

#define pl_malloc SP_malloc
#define pl_realloc SP_realloc
#define pl_free SP_free

		 /*******************************
		 *	    CONSOLE I/O		*
		 *******************************/

void
pl_Cvprintf(const char *fmt, va_list args)
{ char buf[2048];			/* No SP_vprintf() in SICStus */
  char *s;

  vsprintf(buf, fmt, args);
  for(s=buf; *s; s++)
    SP_putc(*s);
}


static int
pl_Cputchar(int c)
{ SP_putc(c);
  
  return c;
}


static void
pl_Cflush()
{ SP_fflush(SP_stdout);
}


static char *
pl_Cgetline(char *buf, int size)
{ int c, n = 0;

  do
  { c = SP_getc();

    if ( c == EOF )
      return NULL;

    buf[n++] = c;
    if ( n == size );
      return buf;
  } while ( c != '\n' && c != '\r' );

  return buf;
}


		 /*******************************
		 *	HOST ACTION/QUERY	*
		 *******************************/

typedef void (*sighandler_t)(int);

static int
PrologAction(int action, va_list args)
{ switch(action)
  { case HOST_TRACE:
      SP_action(SP_ACTION_TRACE, NULL);
      return PCE_SUCCEED;
    case HOST_HALT:
      SP_action(SP_ACTION_HALT, NULL);
      return PCE_FAIL;				/* should not get here */
    case HOST_BREAK:
    { SP_pred_ref pred = SP_predicate("break", 0, "user");

      SP_query_cut_fail(pred);
      return PCE_SUCCEED;
    }
    case HOST_ABORT:
      SP_action(SP_ACTION_ABORT, NULL);
      return PCE_SUCCEED;
    case HOST_SIGNAL:
    { int sig = va_arg(args, int);
      sighandler_t func = va_arg(args, sighandler_t);

      signal(sig, func);		/* Not clear whether or not to SP_ */
      return PCE_SUCCEED;
    }
    case HOST_RECOVER_FROM_FATAL_ERROR:
      SP_action(SP_ACTION_ABORT, NULL);
      return PCE_FAIL;			/* could not abort: failure */
#ifdef _CONSOLE_H_INCLUDED
    case HOST_CHECK_INTERRUPT:
      rlc_check_intr();
      return PCE_SUCCEED;
#endif
    case HOST_BACKTRACE:
    case HOST_ATEXIT:
    default:
      return PCE_FAIL;
  }
}


static int
PrologQuery(int what, PceCValue *value)
{ switch(what)
  { case HOST_SYMBOLFILE:
      if ( (value->string = (char *)SP_inquiry(SP_INQUIRY_SYMBOLFILE)) )
	return PCE_SUCCEED;
      return PCE_FAIL;
#ifdef _CONSOLE_H_INCLUDED		/* Win32 console */
    case HOST_CONSOLE:
      if ( (value->pointer = rlc_hwnd()) )
	return PCE_SUCCEED;
      return PCE_FAIL;
#endif
    default:
      return PCE_FAIL;
  }
}

#endif /*SICSTUS*/

#ifdef SWI

#define pl_malloc NULL
#define pl_realloc NULL
#define pl_free NULL

		 /*******************************
		 *	    CONSOLE I/O		*
		 *******************************/

void
pl_Cvprintf(const char *fmt, va_list args)
{ Svprintf(fmt, args);
}


static int
pl_Cputchar(int c)
{ return Sputchar(c);
}


static void
pl_Cflush()
{ Sflush(Soutput);
}


static char *
pl_Cgetline(char *buf, int size)
{ return Sfgets(buf, size, Sinput);
}


		 /*******************************
		 *	HOST ACTION/QUERY	*
		 *******************************/

typedef void (*sighandler_t)(int);
typedef void (*halthandler_t)(int, void *);

static int
PrologAction(int action, va_list args)
{ switch(action)
  { case HOST_TRACE:
      PL_action(PL_ACTION_TRACE, NULL);
      return PCE_SUCCEED;
    case HOST_HALT:
      PL_action(PL_ACTION_HALT, NULL);
      return PCE_FAIL;				/* should not get here */
    case HOST_BREAK:
      PL_action(PL_ACTION_BREAK, NULL);
      return PCE_SUCCEED;
    case HOST_ABORT:
      PL_action(PL_ACTION_ABORT, NULL);
      return PCE_SUCCEED;
    case HOST_SIGNAL:
    { int sig = va_arg(args, int);
      sighandler_t func = va_arg(args, sighandler_t);

      PL_signal(sig, func);
      return PCE_SUCCEED;
    }
    case HOST_RECOVER_FROM_FATAL_ERROR:
      PL_action(PL_ACTION_ABORT, NULL);
      return PCE_FAIL;			/* could not abort: failure */
#ifdef _CONSOLE_H_INCLUDED
    case HOST_CHECK_INTERRUPT:
      rlc_check_intr();
      return PCE_SUCCEED;
#endif
    case HOST_BACKTRACE:
    { int frames = va_arg(args, int);
      PL_action(PL_ACTION_BACKTRACE, (void *) frames);
      return PCE_SUCCEED;
    }
    case HOST_ATEXIT:
    { OnExitFunction f = va_arg(args, OnExitFunction);
#ifdef O_SHAREDLIBRARY
      exitpce_hook = f;
#else
      PL_on_halt((halthandler_t)f, NULL);
#endif      
      return PCE_SUCCEED;
    }
    default:
      return PCE_FAIL;
  }
}


static int
PrologQuery(int what, PceCValue *value)
{ switch(what)
  { case HOST_SYMBOLFILE:
      if ( (value->string = (char *)PL_query(PL_QUERY_SYMBOLFILE)) )
	return PCE_SUCCEED;
      return PCE_FAIL;
#ifdef _CONSOLE_H_INCLUDED		/* Win32 console */
    case HOST_CONSOLE:
      if ( (value->pointer = rlc_hwnd()) )
	return PCE_SUCCEED;
      return PCE_FAIL;
#endif
    default:
      return PCE_FAIL;
  }
}

#endif /*SWI*/


		 /*******************************
		 *	  SETUP CALLBACK	*
		 *******************************/

static pce_callback_functions callbackfunction =
{ PrologSend,
  PrologGet,
  PrologCallProc,
  PrologCallFunc,
  PrologQuery,
  PrologAction,
  pl_Cvprintf,
  pl_Cputchar,
  pl_Cflush,
  pl_Cgetline,
  pl_malloc,				/* Prolog memory management hooks */
  pl_realloc,
  pl_free
};


static void
init_pce_callbacks()
{ pceRegisterCallbacks(&callbackfunction);
}


static void
registerPredicates()
{ InstallPredicate("send",		2, pl_send0,	META);
  InstallPredicate("send",		3, pl_send1,	META);
  InstallPredicate("send",		4, pl_send2,	META);
  InstallPredicate("send", 		5, pl_send3,	META);
  InstallPredicate("$pce_send",		3, pl_sendn,	META|HIDDEN);
  InstallPredicate("get",		3, pl_get0,	META);
  InstallPredicate("get",		4, pl_get1,	META);
  InstallPredicate("get",		5, pl_get2,	META);
  InstallPredicate("get",		6, pl_get3,	META);
  InstallPredicate("$pce_get",	   	4, pl_getn,	META|HIDDEN);
  InstallPredicate("object",		1, pl_object1,	0);
  InstallPredicate("object",		2, pl_object2,	0);
  InstallPredicate("new",		2, pl_new,	META);
  InstallPredicate("pce_predicate_reference", 2,
					   pl_pce_predicate_reference, META);
  InstallPredicate("pce_open",		3, pl_pce_open, 0);
}


#ifndef PROLOG_ARGC
#define PROLOG_ARGC() 0
#endif
#ifndef PROLOG_ARGV
#define PROLOG_ARGV() ((char **)NULL)
#endif
#ifndef PROLOG_INSTALL_REINIT_FUNCTION
#define PROLOG_INSTALL_REINIT_FUNCTION(x)
#endif
#ifndef PROLOG_ITF_INIT
#define PROLOG_ITF_INIT()
#endif
#ifndef PROLOG_INSTALL_CALLBACKS
#define PROLOG_INSTALL_CALLBACKS()
#endif
#ifndef PROLOG_INSTALL_DISPATCH_FUNCTION
#define PROLOG_INSTALL_DISPATCH_FUNCTION(f)
#endif
#ifndef PROLOG_INSTALL_RESET_FUNCTION
#define PROLOG_INSTALL_RESET_FUNCTION(f)
#endif
#ifndef PROLOG_INSTALL_REDRAW_FUNCTION
#define PROLOG_INSTALL_REDRAW_FUNCTION(f)
#endif


foreign_t
pl_pce_init(Term a)
{ char **argv;
  int argc;
  const char *home;
  Atom ahome;
  static int initialised = FALSE;
  
  if ( GetAtom(a, &ahome) )
    home = AtomCharp(ahome);
  else
    home = NULL;

  argc = PROLOG_ARGC();
  argv = PROLOG_ARGV();

  if ( !initialised )
  { PceObject plname;

    PROLOG_INSTALL_REINIT_FUNCTION(pl_pce_init);
    PROLOG_ITF_INIT();

    pceRegisterCallbacks(&callbackfunction);
    if ( !pceInitialise(1, home, argc, argv) )
    { PceException(ATOM_initialisation, 0);
      return FALSE;
    }

    initPceConstants();			/* get code used PCE constants */  
    initPrologConstants();		/* Public prolog constants */
    initHostConstants();		/* Host-specific Prolog constants */
    registerPredicates();		/* make the interface known */

    plname = cToPceName("prolog");
    pceSend(PROLOG, cToPceName("name_reference"), 1, &plname);
    PROLOG_INSTALL_DISPATCH_FUNCTION(pce_dispatch);
    PROLOG_INSTALL_RESET_FUNCTION(pceReset);
    PROLOG_INSTALL_REDRAW_FUNCTION(pceRedraw);

    initialised = TRUE;
  }

  return TRUE;
}
