/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#ifndef NDEBUG
#define NDEBUG				/* delete assert() */
#endif

#define MODULE	1			/* Tag selector with module */
#define PREDICATE_PER_CLASS 1		/* Single predcate for all methods */

#include <SWI-Stream.h>			/* SWI-Prolog streams */
#include <SWI-Prolog.h>			/* SWI-Prolog <-> C interface */
#include "../../prolog/c/interface.h"	/* generic Prolog <->PCE part */
#include <stdlib.h>			/* exit() */

#ifdef PL_MAX_INTEGER
#define PROLOG_MAX_INTEGER PL_MAX_INTEGER
#else
#define PROLOG_MAX_INTEGER ((1<<26) - 1) /* default as long as not passed */
#endif

#ifdef __GNUC__
#define AtomicVector(name, size)  atomic_t name[size]
#else
#define AtomicVector(name, size) \
	atomic_t *name = (atomic_t *) alloca(size * sizeof(atomic_t))
#endif

#define pl_get0		pce_get0	/* avoid name-conflicts */
#define pl_get2		pce_get2

typedef term_t		Term;		/* generic Prolog term */
typedef atomic_t	Atom;		/* Atom handle */

static foreign_t	pl_pce_open(Term t, Term mode, Term plhandle);
static foreign_t	pl_pce_predicate_reference(term_t pred, term_t ref);
static void		init_pce_callbacks(void);

#ifdef __WIN32__
static IOSTREAM *S__iob;		/* Windows DLL version */
#define PROLOG_ITF_INIT() { S__iob = S__getiob(); }
#define O_SHAREDLIBRARY
#endif

#ifdef O_SHAREDLIBRARY
#ifndef PROLOG_ITF_INIT
#define PROLOG_ITF_INIT() { }
#endif
#define PROLOG_ONEXIT(f)  { exitpce_hook = f; }

static OnExitFunction exitpce_hook;

install_t
install()
{ /*Sdprintf("initialising XPCE ...");*/
  prolog_pce_init();
  /*Sdprintf(" ok\n");*/
}


install_t
uninstall()
{ if ( exitpce_hook )
    (*exitpce_hook)();
}

#endif /*O_SHAREDLIBRARY*/

static atomic	 ATOM_call;		/* call */
static atomic	 ATOM_read;		/* read */
static atomic	 ATOM_write;		/* write */
static atomic	 ATOM_append;		/* append */
static atomic	 ATOM_ref;		/* @ */
static atomic	 ATOM_new;		/* new */
static atomic	 ATOM_string;		/* string */
static functor_t FUNCTOR_ref1;		/* @/1 */
static functor_t FUNCTOR_new1;		/* new/1 */
static functor_t FUNCTOR_new2;		/* new/2 */
static functor_t FUNCTOR_string1;	/* string/1 */
static functor_t FUNCTOR_module2;	/* :/2 */
static module_t  MODULE_user;		/* Global module */

#define GetType(term)		PL_type(term)

#define PROLOG_INTEGER		PL_INTEGER	/* Term types */
#define PROLOG_ATOM		PL_ATOM
#define PROLOG_STRING		PL_STRING
#define PROLOG_FLOAT		PL_FLOAT
#define PROLOG_VARIABLE		PL_VARIABLE
#define PROLOG_COMPOUND 	PL_TERM

#define IsVar(t)		(PL_is_var(t))
#define IsInt(t)		(PL_is_int(t))
#define IsAtom(t)		(PL_is_atom(t))
#define IsFloat(t)		(PL_is_float(t))
#define IsCompound(t)		(PL_is_term(t))
#define IsString(t)		(PL_is_string(t))

#define Atomic(t)		PL_atomic(t)
#define AtomCharp(a)		PL_atom_value(a)
#define AtomHandle(t)		((hostHandle) PL_atomic(t))
#define IsAtomCall(t)		(PL_atomic(t) == ATOM_call)

#define IsPceReference(t)	(PL_functor(t) == FUNCTOR_ref1)
#define IsNew1(t)		(PL_functor(t) == FUNCTOR_new1)
#define IsNew2(t)		(PL_functor(t) == FUNCTOR_new2)
#define IsString1(t)		(PL_functor(t) == FUNCTOR_string1)


#define UnifyCompound(t, n, a)	PL_unify_functor(t, \
					 PL_new_functor(PL_new_atom(n), \
							a))
#define UnifyPceReference(t)	PL_unify_functor(t, FUNCTOR_ref1)
#define UnifyInteger(t, i)	PL_unify_atomic(t, PL_new_integer(i))
#define UnifyFloat(t, f)	PL_unify_atomic(t, PL_new_float(f))
#define UnifyCharp(t, s)	PL_unify_atomic(t, PL_new_atom(s))
#define UnifyAtom(t, a)		PL_unify_atomic(t, (atomic) a)
#define GetArg(t, n)		PL_arg(t, n)
#define GetArity(t)		PL_functor_arity(PL_functor(t))
#define GetFloat(t)		PL_float_value(PL_atomic(t))
#define GetString(t)		PL_string_value(PL_atomic(t))
#define GetInteger(t)		PL_integer_value(PL_atomic(t))
#define GetAtom(t)		PL_atom_value(PL_atomic(t))

#define INSTALL_PREDICATE(n, a, f) \
	{ extern foreign_t f(); \
	  PL_register_foreign(n, a, f, 0); \
	}
#define INSTALL_HIDDEN_PREDICATE(n, a, f) \
	{ extern foreign_t f(); \
	  PL_register_foreign(n, a, f, PL_FA_NOTRACE); \
	}
#define INSTALL_META_PREDICATE(n, a, f) \
	{ extern foreign_t f(); \
	  PL_register_foreign(n, a, f, PL_FA_TRANSPARENT); \
	}
#define INSTALL_HIDDEN_META_PREDICATE(n, a, f) \
	{ extern foreign_t f(); \
	  PL_register_foreign(n, a, f, PL_FA_NOTRACE|PL_FA_TRANSPARENT); \
	}


		/********************************
		*      ENVIRONMENT ACTIONS	*
		********************************/

#define PROLOG_TRACE()		PL_action(PL_ACTION_TRACE, NULL)
#define PROLOG_BACKTRACE(n)	PL_action(PL_ACTION_BACKTRACE, (void *) n)
#define PROLOG_HALT()		PL_action(PL_ACTION_HALT, NULL)
#define PROLOG_BREAK()		PL_action(PL_ACTION_BREAK, NULL)
#define PROLOG_ABORT()		PL_action(PL_ACTION_ABORT, NULL)
#define PROLOG_SIGNAL(s, f)	PL_signal(s, f)
#define PROLOG_WRITE(s)		PL_action(PL_ACTION_WRITE, s)
#define PROLOG_FLUSH()		PL_action(PL_ACTION_FLUSH, NULL)
#ifndef PROLOG_ONEXIT
#define PROLOG_ONEXIT(f)	PL_on_halt(f, NULL)
#endif

#define PROLOG_INSTALL_REINIT_FUNCTION(f) \
				{ PL_reinit_hook((PL_reinit_hook_t)(f)); }
#define PROLOG_INSTALL_RESET_FUNCTION(f) \
				{ PL_abort_hook(f); }
#if defined(__WATCOMC__) || defined(__WIN32__)
#define PROLOG_INSTALL_DISPATCH_FUNCTION(f) {}
#else
#define PROLOG_INSTALL_DISPATCH_FUNCTION(f) \
				{ PL_dispatch_hook(f); }
#endif

#define PROLOG_INSTALL_CALLBACKS() init_pce_callbacks()

#define PROLOG_DISPATCH_INPUT   PL_DISPATCH_INPUT
#define PROLOG_DISPATCH_TIMEOUT PL_DISPATCH_TIMEOUT


		/********************************
		*     ENVIRONMENT REQUESTS	*
		********************************/

#define PROLOG_SYMBOLFILE()	((char *) PL_query(PL_QUERY_SYMBOLFILE))
#define PROLOG_GETC()		((int) PL_query(PL_QUERY_GETC))
#define PROLOG_ARGC()		((int) PL_query(PL_QUERY_ARGC))
#define PROLOG_ARGV()		((char **) PL_query(PL_QUERY_ARGV))


static void
_Warning(char *fm, va_list args)
{ Sdprintf("[Prolog/PCE interface: ");
  Svdprintf(fm, args);
  Sdprintf("]\n");
  PROLOG_TRACE();
}


static void
_FatalError(char *fm, va_list args)
{ Sdprintf("[Prolog/PCE interface FATAL ERROR: ");
  Svdprintf(fm, args);
  Sdprintf("]\n");

  PROLOG_ABORT();
  
  exit(1);
}

static bool
Warning(char *fm, ...)
{ va_list args;

  va_start(args, fm);
  _Warning(fm, args);
  va_end(args);

  fail;
}


static void
FatalError(char *fm, ...)
{ va_list args;

  va_start(args, fm);
  _FatalError(fm, args);
  va_end(args);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Warning: this function returns a pointer to a static value.  As long as
it is used as now (see termToObject(), this will be ok.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Term
GetCompound(Term t, int *a)
{ static atomic_t n;

  functor f = PL_functor(t);
  n = PL_functor_name(f);
  *a = PL_functor_arity(f);

  return &n;
}


static char *
ListToCharp(Term t)
{ return PL_list_string_value(t);
}


static inline Term
StripModuleTag(Term t, char **m)
{ if ( *m != NULL )
  { module mod = (module) -1;		/* terrible hack ... */

    Test(t = PL_strip_module(t, &mod));
    if ( mod != (module) -1 )
      *m = PL_atom_value(PL_module_name(mod));
  } else
  { module mod = 0;

    Test(t = PL_strip_module(t, &mod));
    *m = PL_atom_value(PL_module_name(mod));
  }

  return t;
}


static void
InitPrologConstants(void)
{ ATOM_call		= PL_new_atom("call");
  ATOM_read		= PL_new_atom("read");
  ATOM_write		= PL_new_atom("write");
  ATOM_append		= PL_new_atom("append");
  ATOM_ref		= PL_new_atom("@");
  ATOM_new		= PL_new_atom("new");
  ATOM_string		= PL_new_atom("string");

  FUNCTOR_ref1		= PL_new_functor(PL_new_atom("@"), 1);
  FUNCTOR_new1		= PL_new_functor(PL_new_atom("new"), 1);
  FUNCTOR_new2		= PL_new_functor(PL_new_atom("new"), 2);
  FUNCTOR_string1	= PL_new_functor(PL_new_atom("string"), 1);
  FUNCTOR_module2	= PL_new_functor(PL_new_atom(":"), 2);

  MODULE_user		= PL_new_module(PL_new_atom("user"));

  PL_register_foreign("pce_open", 3,
		      pl_pce_open, 0);
  PL_register_foreign("pce_predicate_reference", 2,
		      pl_pce_predicate_reference, PL_FA_TRANSPARENT);
}


#include "../../prolog/c/interface.c"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Transform a vector of arguments and a selector into a Prolog term  which
we  can  give to PL_call(). The second argument indicates whether we want
to create a goal for hostSend() or hostGet(). For the latter we  add  an
extra variable for the result. NULL is returned on failure.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Term
vectorToGoal(PceObject sel, int argc, PceObject *argv, bool send, module *m)
{ Term goal = PL_new_term();
  Term st = goal;
  atomic f;
  char *s, *name;
  int n;

  name = pceCharArrayToC(sel);

#if MODULE
  if ( (s = strchr(name, ':')) && !strchr(&s[1], ':') )
  { *s = EOS;
    *m = PL_new_module(PL_new_atom(name));
    f = PL_new_atom(&s[1]);
    *s = ':';
  } else
  { *m = MODULE_user;
    f = PL_new_atom(name);
  }
#else
  f = PL_new_atom(name);
#endif /*MODULE*/

  if ( argc == 0 && send )
  { PL_unify_atomic(st, f);
    return goal;
  }

  PL_unify_functor(st, PL_new_functor(f, send ? argc : argc+1));

  for(n = 1; n <= argc; n++, argv++)
    if ( !unifyObject(GetArg(st, n), *argv, FALSE) )
      FatalError("Internal error in PCE object conversion");

  return goal;
}  

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PrologSend() is called by PCE to  invoke   behaviour  in Prolog.  As the
terms built by vectorToGoal() and the actual  calling are not related to
other material we can savely reset  the   global  stack's top pointer to
discard these terms.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
PrologSend(PceObject prolog, PceObject sel, int argc, PceObject *argv)
{ Term goal;
  module m;
  bktrk_buf buf;
  bool rval;

  PL_mark(&buf);

  if ((goal = vectorToGoal(sel, argc, argv, TRUE, &m)) == (Term) NULL)
    fail;

  rval = PL_call(goal, m);
  PL_bktrk(&buf);

  return rval;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PrologGet()  calls  prolog  and  transforms  the  last  argument  of   the
(succeeded) goal into an object.  It then returns this object to PCE.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static PceObject
PrologGet(PceObject prolog, PceObject sel, int argc, PceObject *argv)
{ Term goal;
  module m;
  bktrk_buf buf;
  PceObject answer;

  PL_mark(&buf);

  if ( !(goal = vectorToGoal(sel, argc, argv, FALSE, &m)) ||
       !PL_call(goal, m) )
    answer = PCE_FAIL;
  else
    answer = termToObject(GetArg(goal, argc+1), NULL, FALSE);

  PL_bktrk(&buf);

  return answer;
}


		 /*******************************
		 *    DIRECT CALLBACK SUPPORT	*
		 *******************************/

static foreign_t
pl_pce_predicate_reference(term_t pred, term_t ref)
{ if ( PL_is_var(pred) )
  { PceObject obj;
    void *ptr;

    if ( (obj = termToObject(ref, NULL, FALSE)) &&
	 (ptr = pcePointerToC(obj)) )
    { predicate_t predicate = ptr;
      atomic_t modname  = PL_module_name(PL_predicate_module(predicate));
      functor_t functor = PL_predicate_functor(predicate);
      
      if ( PL_unify_functor(pred, FUNCTOR_module2) &&
	   PL_unify_atomic(PL_arg(pred, 1), modname) &&
	   PL_unify_functor(PL_arg(pred, 2), functor) )
	PL_succeed;
    }

    PL_fail;
  } else
  { module_t module = NULL;

    if ( (pred = PL_strip_module(pred, &module)) )
    { functor_t functor;
      predicate_t predicate;

      if ( PL_is_term(pred) )
	functor = PL_functor(pred);
      else if ( PL_is_atom(pred) )
	functor = PL_new_functor(PL_atomic(pred), 0);
      else
      { Warning("pce_predicate_reference/2: illegal predicate specification");
	PL_fail;
      }

      if ( (predicate = PL_predicate(functor, module)) )
      { PceObject pceref = cToPcePointer(predicate);
	
	return unifyObject(ref, pceref, FALSE);
      }

      PL_fail;
    }
  }
}


static atomic_t
pushObject(PceObject obj)
{ PceCValue value;
  int pcetype;
  atomic_t avalue;

  switch( pcetype = pceToC(obj, &value) )
  { case PCE_REFERENCE:
      avalue = PL_new_integer(value.integer);
      return PL_new_compound(FUNCTOR_ref1, &avalue);
    case PCE_ASSOC:
      { PceITFSymbol symbol = value.itf_symbol;

	if ( symbol->handle[0] != NULL )
	  avalue = (atomic_t) symbol->handle[0];
	else
	  avalue = PL_new_atom(pceCharArrayToC(symbol->name));
      }
      return PL_new_compound(FUNCTOR_ref1, &avalue);
    case PCE_INTEGER:
      return PL_new_integer(value.integer);
    case PCE_NAME:
      { PceITFSymbol symbol = value.itf_symbol;

	if ( symbol->handle[0] != NULL )
	  avalue = (atomic_t) symbol->handle[0];
	else
	  avalue = PL_new_atom(pceCharArrayToC(symbol->name));
      }
      return avalue;
    case PCE_REAL:
      return PL_new_float(value.real);

    default:
      assert(0);
  }

  return 0;
}


#ifdef PREDICATE_PER_CLASS
#define EXCESS_ARGS 2
#else
#define EXCESS_ARGS 1
#endif


static int
PrologCallProc(PceObject handle, PceObject rec, PceObject sel, int argc, PceObject objv[])
{ void *ptr = pcePointerToC(handle);

  if ( ptr != PCE_NO_POINTER )
  { predicate_t pred = ptr;
    int arity = PL_predicate_arity(pred);
    AtomicVector(av, arity);
    term_t *termv = (term_t *) av;
    atomic *ap = av;
    int i, rval;
    bktrk_buf buf;

    if ( argc+EXCESS_ARGS != arity )
    { Warning("PrologCallProc(): inconsistent arity");
      return PCE_FAIL;
    }

    PL_mark(&buf);
#ifdef PREDICATE_PER_CLASS
    *ap++ = pushObject(sel);
#endif
    *ap++ = pushObject(rec);
    for(i=argc; i > 0; i--)
      *ap++ = pushObject(*objv++);
    PL_term_vector(arity, termv, av);
    
    rval = PL_call_predicate(NULL, TRUE, pred, termv) ? PCE_SUCCEED : PCE_FAIL;
    PL_bktrk(&buf);

    return rval;
  }

  Warning("PrologCallProc(): bad predicate reference");
  return PCE_FAIL;
}


static PceObject
PrologCallFunc(PceObject handle, PceObject rec, PceObject sel, int argc, PceObject objv[])
{ void *ptr = pcePointerToC(handle);

  if ( ptr != PCE_NO_POINTER )
  { predicate_t pred = ptr;
    int arity = PL_predicate_arity(pred);
    AtomicVector(av, arity);
    term_t *termv = (term_t *) av;
    atomic_t *ap = av;
    int i;
    PceObject answer;
    bktrk_buf buf;

    if ( argc+1+EXCESS_ARGS != arity )
    { Warning("PrologCallFunc(): inconsistent arity");
      return PCE_FAIL;
    }

    PL_mark(&buf);
#ifdef PREDICATE_PER_CLASS
    *ap++ = pushObject(sel);
#endif
    *ap++ = pushObject(rec);
    for(i=argc; i>0; i--)
      *ap++ = pushObject(*objv++);
    *ap = PL_new_var();
    PL_term_vector(arity, termv, av);

    if ( PL_call_predicate(NULL, TRUE, pred, termv) )
    { answer = termToObject(termv[arity-1], NULL, FALSE);
    } else
      answer = PCE_FAIL;
    PL_bktrk(&buf);

    return answer;
  }

  Warning("PrologCallFunc(): bad predicate reference");
  return PCE_FAIL;
}



		 /*******************************
		 *	      I/O		*
		 *******************************/

static void
pl_vCprintf(const char *fmt, va_list args)
{ Svprintf(fmt, args);

  Sflush(Soutput);
}


static int
pl_Cputchar(int chr)
{ return Sputchar(chr);
}


static char *
pl_Cgetline(char *line, int size)
{ return Sfgets(line, size, Sinput);
}

		 /*******************************
		 *     CALLBACK REGISTRATION	*
		 *******************************/

static pce_callback_functions callbackfunction =
{ PrologSend,
  PrologGet,
  PrologCallProc,
  PrologCallFunc,
  PrologQuery,
  PrologAction,
  pl_vCprintf,
  pl_Cputchar,
  pl_Cgetline
};


static void
init_pce_callbacks()
{ pceRegisterCallbacks(&callbackfunction);
}

		 /*******************************
		 *	 STREAM CONNECTION	*
		 *******************************/

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
  { int flags, sflags = SIO_FBUF|SIO_RECORDPOS;
    int handle;

    if ( PL_atomic(mode) == ATOM_read )
    { flags = PCE_RDONLY;
      sflags |= SIO_INPUT;
    } else if ( PL_atomic(mode) == ATOM_write )
    { flags = PCE_WRONLY|PCE_TRUNC;
      sflags |= SIO_OUTPUT;
    } else if ( PL_atomic(mode) == ATOM_append )
    { flags = PCE_WRONLY|PCE_APPEND;
      sflags |= SIO_OUTPUT;
    } else
    { PL_warning("pce_open/3: illegal mode");
      PL_fail;
    }

    if ( (handle = pceOpen(obj, flags)) >= 0 )
    { IOSTREAM *s = Snew((void *)handle, sflags, &pceFunctions);

      return PL_open_stream(s, plhandle);
    }
    PL_warning("pce_open/3: could not open: %s", pceOsError());
  } else
  { PL_warning("pce_open/3: bad object");
    PL_fail;
  }
}
