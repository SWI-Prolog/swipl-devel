/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#define NDEBUG				/* delete assert() */
#define MODULE	1			/* Tag selector with module */

#include <SWI-Prolog.h>			/* SWI-Prolog <-> C interface */
#include <SWI-Stream.h>			/* SWI-Prolog streams */
#include "../../prolog/c/interface.h"	/* generic Prolog <->PCE part */

#define pl_get0		pce_get0	/* avoid name-conflicts */
#define pl_get2		pce_get2

typedef term	Term;			/* generic Prolog term */
typedef void (*OnExitFunction)(int, void *);

static foreign_t pl_pce_open(Term t, Term mode, Term plhandle);

static atomic	ATOM_call;		/* call */
static atomic	FUNCTOR_ref1;		/* @/1 */
static atomic	FUNCTOR_new1;		/* new/1 */
static atomic	FUNCTOR_new2;		/* new/2 */
static atomic	FUNCTOR_string1;	/* string/1 */
static atomic	ATOM_read;		/* read */
static atomic	ATOM_write;		/* write */
static atomic	ATOM_append;		/* append */
static module   MODULE_user;		/* Global module */


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
#define PROLOG_ONEXIT(f, a)	PL_on_halt(f, a)

#define PROLOG_INSTALL_REINIT_FUNCTION(f) \
				{ PL_foreign_reinit_function = (void (*)())f; }
#define PROLOG_INSTALL_RESET_FUNCTION(f) \
				{ PL_abort_handle(f); }
#ifdef __WATCOMC__
#define PROLOG_INSTALL_DISPATCH_FUNCTION(f) {}
#else
#define PROLOG_INSTALL_DISPATCH_FUNCTION(f) \
				{ PL_dispatch_events = f; }
#endif

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
_Warning(fm, args)
char *fm;
va_list args;
{ fprintf(stderr, "[Prolog/PCE interface: ");
  vfprintf(stderr, fm, args);
  fprintf(stderr, "]\n");
  PROLOG_TRACE();
}


static void
_FatalError(fm, args)
char *fm;
va_list args;
{ fprintf(stderr, "[Prolog/PCE interface FATAL ERROR: ");
  vfprintf(stderr, fm, args);
  fprintf(stderr, "]\n");

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


static void
GetCompound(t, n, a)
Term t;
char **n;
int *a;
{ functor f = PL_functor(t);
  *n = PL_atom_value(PL_functor_name(f));
  *a = PL_functor_arity(f);
}


static char *
ListToCharp(t)
Term t;
{ return PL_list_string_value(t);
}


static inline Term
StripModuleTag(t, m)
Term t;
char **m;
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
InitPrologConstants()
{ ATOM_call		= PL_new_atom("call");
  ATOM_read		= PL_new_atom("read");
  ATOM_write		= PL_new_atom("write");
  ATOM_append		= PL_new_atom("append");

  FUNCTOR_ref1		= PL_new_functor(PL_new_atom("@"), 1);
  FUNCTOR_new1		= PL_new_functor(PL_new_atom("new"), 1);
  FUNCTOR_new2		= PL_new_functor(PL_new_atom("new"), 2);
  FUNCTOR_string1	= PL_new_functor(PL_new_atom("string"), 1);

  MODULE_user		= PL_new_module(PL_new_atom("user"));

  PL_register_foreign("pce_open", 3, pl_pce_open, 0);
}


#include "../../prolog/c/interface.c"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Transform a vector of arguments and a selector into a Prolog term  which
we  can  give to PL_call(). The second argument indicates whether we want
to create a goal for hostSend() or hostGet(). For the latter we  add  an
extra variable for the result. NULL is returned on failure.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Term
vectorToGoal(sel, argc, argv, send, m)
PceObject sel;
int argc;
PceObject *argv;
bool send;
module *m;
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
hostSend() is called by PCE to invoke behaviour in Prolog.  As the terms
built by vectorToGoal() and the actual calling are not related to  other
material  we  can savely reset the global stack's top pointer to discard
these terms.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
hostSend(prolog, sel, argc, argv)
PceObject prolog;
PceObject sel;
int argc;
PceObject *argv;
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
hostGet()  calls  prolog  and  transforms  the  last  argument  of   the
(succeeded) goal into an object.  It then returns this object to PCE.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

PceObject
hostGet(prolog, sel, argc, argv)
PceObject prolog;
PceObject sel;
int argc;
PceObject argv[];
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
