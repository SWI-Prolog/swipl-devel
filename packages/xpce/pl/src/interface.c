/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#if !defined(__WIN32__) && defined(WIN32)
#define __WIN32__ 1
#endif

#ifdef __WIN32__
#define HAVE_MALLOC_H 1
#define HAVE_SIGNAL_H 1
#else
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#endif

#define DEBUG(g) ((void)0)

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <sys/types.h>
#include <ctype.h>
#include <h/interface.h>
#include <string.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#ifdef _REENTRANT
#define LOCK()   pceMTLock(LOCK_PCE)
#define UNLOCK() pceMTUnlock(LOCK_PCE)
#else
#define LOCK()
#define UNLOCK()
#endif

#ifdef __WIN32__
#include <windows.h>
#endif

#define SWI 1			/* SWI-Prolog version 2.5! and up */

#ifdef __GNUC__
#define TermVector(name, size)  Term name[size]
#define ObjectVector(name, size) PceObject name[size]
#else
#define TermVector(name, size) \
	Term *name = (Term *) alloca(size * sizeof(Term))
#define ObjectVector(name, size) \
	PceObject *name = (PceObject *) alloca(size * sizeof(Term))
#endif

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#ifndef EOS
#define EOS '\0'
#endif

#define NULLATOM ((Atom)0)

#define ARG_ERROR	0
#define ARG_OK		1
#define ARG_HOSTDATA	2

#ifdef __GNUC__
#define DynamicVector(name, type, size)	type name[size]
#else
#define DynamicVector(name, type, size)	type *name = \
				  (type *) alloca((size)*sizeof(type))
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
#include <SWI-Stream.h>
#include <SWI-Prolog.h>

typedef atom_t		Atom;
typedef term_t		Term;
typedef record_t	Record;
typedef module_t	Module;
typedef predicate_t	Predicate;
typedef functor_t	Functor;
typedef fid_t		Fid;

#define AtomFromString(s)	PL_new_atom(s)
#define ModuleFromAtom(a)	PL_new_module(a)
#define ModuleName(m)		PL_module_name(m)
#endif /*SWI*/

typedef struct
{ atomic_t	method_id;		/* Identifier of the method */
  functor_t	functor;		/* Functor for the arguments */
  int		flags;			/* debugging, etc */
  int		argc;			/* #arguments */
} prolog_call_data;


		 /*******************************
		 *	    PROTOTYPES		*
		 *******************************/

static PceObject	termToObject(Term t, PceType type,
				     Atom assoc, int new);
static prolog_call_data *get_pcd(PceObject method);
static void		put_object(Term t, PceObject obj);
static void		put_trace_info(term_t id, prolog_call_data *pm);
       foreign_t	pl_pce_init(Term a);
static Module		pceContextModule();
static void		makeClassProlog();
static Term		getTermHandle(PceObject hd);


		 /*******************************
		 *	     CONSTANTS		*
		 *******************************/

static Module 	 DefaultModule;		/* For module handling */
static PceObject DefaultContext;	/* Executing context */

static Atom ATOM_append;		/* "append" */
static Atom ATOM_argument;		/* "argument" */
static Atom ATOM_argument_count;	/* "argument_count" */
static Atom ATOM_assign;		/* ":=" */
static Atom ATOM_badIntegerReference;	/* "bad_integer_reference" */
static Atom ATOM_badList;		/* "bad_list" */
static Atom ATOM_badObjectDescription;	/* "bad_object_description" */
static Atom ATOM_badReference;		/* "bad_reference" */
static Atom ATOM_badSelector;		/* "bad_selector" */
static Atom ATOM_badStringArgument;	/* "bad_string_argument" */
static Atom ATOM_behaviour;		/* "behaviour" */
static Atom ATOM_context;		/* "context" */
static Atom ATOM_default;		/* "default" */
static Atom ATOM_dot;			/* "." */
static Atom ATOM_domain_error;		/* "domain_error" */
static Atom ATOM_error;			/* "error" */
static Atom ATOM_existence_error;	/* "existence_error" */
static Atom ATOM_get;			/* "get" */
static Atom ATOM_initialisation;	/* "initialisation" */
static Atom ATOM_io_mode;		/* "io_mode" */
static Atom ATOM_module;		/* ":" */
static Atom ATOM_named_argument;	/* "named_argument" */
static Atom ATOM_named_reference;	/* "named_reference" */
static Atom ATOM_new;			/* "new" */
static Atom ATOM_object;		/* "object" */
static Atom ATOM_open;			/* "open" */
static Atom ATOM_pce;			/* "pce" */
static Atom ATOM_permission_error;	/* "permission_error" */
static Atom ATOM_procedure;		/* "procedure" */
static Atom ATOM_proper_list;		/* "proper_list" */
static Atom ATOM_read;			/* "read" */
static Atom ATOM_ref;			/* "@" */
static Atom ATOM_send;			/* "send" */
static Atom ATOM_slash;			/* "/" */
static Atom ATOM_spy;			/* "spy" */
static Atom ATOM_string;		/* "string" */
static Atom ATOM_trace;			/* "trace" */
static Atom ATOM_true;			/* "true" */
static Atom ATOM_type_error;		/* "type_error" */
static Atom ATOM_unknownReference;	/* "unknown_reference" */
static Atom ATOM_update;		/* "update" */
static Atom ATOM_user;			/* "user" */
static Atom ATOM_write;			/* "write" */
static Atom ATOM_prolog;		/* "prolog" */

static Module MODULE_user;		/* Handle for user-module */

static void
initPrologConstants()
{ ATOM_append			= AtomFromString("append");
  ATOM_argument			= AtomFromString("argument");
  ATOM_argument_count		= AtomFromString("argument_count");
  ATOM_assign		        = AtomFromString(":=");
  ATOM_badIntegerReference 	= AtomFromString("bad_integer_reference");
  ATOM_badList		        = AtomFromString("bad_list");
  ATOM_badObjectDescription	= AtomFromString("bad_object_description");
  ATOM_badReference		= AtomFromString("bad_reference");
  ATOM_badSelector		= AtomFromString("bad_selector");
  ATOM_badStringArgument	= AtomFromString("bad_string_argument");
  ATOM_behaviour		= AtomFromString("behaviour");
  ATOM_context			= AtomFromString("context");
  ATOM_default			= AtomFromString("default");
  ATOM_dot	   		= AtomFromString(".");
  ATOM_domain_error   		= AtomFromString("domain_error");
  ATOM_error			= AtomFromString("error");
  ATOM_existence_error		= AtomFromString("existence_error");
  ATOM_get			= AtomFromString("get");
  ATOM_initialisation		= AtomFromString("initialisation");
  ATOM_io_mode			= AtomFromString("io_mode");
  ATOM_module   		= AtomFromString(":");
  ATOM_named_argument		= AtomFromString("named_argument");
  ATOM_named_reference		= AtomFromString("named_reference");
  ATOM_new			= AtomFromString("new");
  ATOM_object		        = AtomFromString("object");
  ATOM_open		        = AtomFromString("open");
  ATOM_pce			= AtomFromString("pce");
  ATOM_permission_error	        = AtomFromString("permission_error");
  ATOM_procedure		= AtomFromString("procedure");
  ATOM_proper_list		= AtomFromString("proper_list");
  ATOM_read			= AtomFromString("read");
  ATOM_ref			= AtomFromString("@");
  ATOM_send			= AtomFromString("send");
  ATOM_slash			= AtomFromString("/");
  ATOM_spy			= AtomFromString("spy");
  ATOM_string			= AtomFromString("string");
  ATOM_trace			= AtomFromString("trace");
  ATOM_true			= AtomFromString("true");
  ATOM_type_error		= AtomFromString("type_error");
  ATOM_unknownReference		= AtomFromString("unknown_reference");
  ATOM_update			= AtomFromString("update");
  ATOM_user			= AtomFromString("user");
  ATOM_write			= AtomFromString("write");
  ATOM_prolog			= AtomFromString("prolog");

  MODULE_user			= ModuleFromAtom(ATOM_user);
}

static PceObject	NIL;		/* @nil */
static PceObject	DEFAULT;	/* @default */
static PceObject	PROLOG;		/* @prolog */
static PceClass		ClassBinding;	/* class(:=) */
static PceClass		ClassProlog;	/* class(prolog_term, host_data) */
static PceType		TypeProlog;	/* prolog_term|atomic */
static PceType		TypePrologTerm;	/* type representing above */
static PceName		NAME_functor;	/* "functor" */
static PceName		NAME_Arity;	/* "_arity" */
static PceName		NAME_Arg;	/* "_arg" */
static PceName		NAME_user;	/* "user" */
static PceName		NAME_includes;	/* "includes" */
static PceName		NAME_chain;	/* "chain" */
static PceName		NAME_vector;	/* "vector" */
static PceName		NAME_codeVector;/* "code_vector" */

static void
initPceConstants()
{ NAME_functor	  = cToPceName("functor");
  NAME_Arity	  = cToPceName("_arity");
  NAME_Arg	  = cToPceName("_arg");
  NAME_user       = cToPceName("user");
  NAME_includes   = cToPceName("includes");
  NAME_chain      = cToPceName("chain");
  NAME_vector     = cToPceName("vector");
  NAME_codeVector = cToPceName("code_vector");

  NIL		  = cToPceAssoc("nil");
  DEFAULT	  = cToPceAssoc("default");
  PROLOG	  = cToPceAssoc("host");
  
  ClassBinding    = cToPceAssoc(":=_class"); /* not so nice! */
  assert(ClassBinding);

  makeClassProlog();
}


		 /*******************************
		 *	   SICSTUS GLUE		*
		 *******************************/

#ifdef SICSTUS
#define GET_FUNCTOR_BUG 1

#define FUNCTOR_pce1		ATOM_pce, 1
#define	FUNCTOR_pce2		ATOM_pce, 2
#define	FUNCTOR_pce3		ATOM_pce, 3
#define	FUNCTOR_ref1		ATOM_ref, 1
#define	FUNCTOR_new1		ATOM_new, 1
#define	FUNCTOR_string1 	ATOM_string, 1
#define	FUNCTOR_module2 	ATOM_module, 2
#define FUNCTOR_namearg 	ATOM_assign, 2
#define FUNCTOR_error2  	ATOM_error, 2
#define FUNCTOR_domain_error2	ATOM_domain_error, 2

#define initHostConstants()

#define AtomCharp(a)		SP_string_from_atom((a))
#define GetInteger(a, i)	SP_get_integer((a), (i))
#define GetAtom(a, n)		SP_get_atom((a), (n))
#define GetFloat(a, f)		SP_get_float((a), (f))
#define NewTerm()		SP_new_term_ref()
#ifdef GET_FUNCTOR_BUG
#define GetNameArity(t, n, a)	((SP_is_compound(t) || SP_is_atom(t)) && \
				 SP_get_functor((t), (n), (a)))
#else
#define GetNameArity(t, n, a)	SP_get_functor((t), (n), (a))
#endif
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
#define FindPredicate(n, a, m)	SP_pred(n, a, m)
#define OpenForeign()		SP_new_term_refs(0)
#define CloseForeign(fid)	SP_reset_term_refs(fid)
#define InstallPredicate(n, a, f, flags)

#if defined(__WIN32__)
#define PROLOG_INSTALL_DISPATCH_FUNCTION(f) {}
#else
#define PROLOG_INSTALL_DISPATCH_FUNCTION(f) \
				{ SP_read_hook = f; }
#endif

#define PROLOG_INSTALL_RESET_FUNCTION(f) \
				{ SP_set_reinit_hook(f); }

static int
GetChars(Term t, char **s, unsigned int *len)
{ if ( SP_get_string(t, s) ||
       SP_get_list_chars(t, s) ||
       SP_get_number_chars(t, s) )
  { *len = strlen(*s);
    return TRUE;
  }

  return FALSE;
}


#ifndef SWI

static void
StripModuleTag(Term t, Atom *module, Term p)
{ Atom name;
  int arity;
  Term a = NewTerm();

  PutTerm(p, t);

  while( GetNameArity(p, &name, &arity) &&
	 name == ATOM_module && arity == 2 )
  { Atom m;

    QGetArg(1, p, a);
    if ( GetAtom(a, &m) )
    { *module = m;
      QGetArg(2, p, p);
    } else
      break;
  }
}

#endif /*~SWI*/

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
#define HAVE_XPCEREF 1			/* _PL_put/get/unify_xpce_reference */

static Functor	FUNCTOR_pce1;
static Functor	FUNCTOR_pce2;
static Functor	FUNCTOR_pce3;
static Functor	FUNCTOR_context2;
static Functor	FUNCTOR_ref1;
static Functor	FUNCTOR_new1;
static Functor	FUNCTOR_string1;
static Functor	FUNCTOR_module2;
static Functor  FUNCTOR_spy1;
static Functor  FUNCTOR_trace1;
static Functor  FUNCTOR_namearg;
static Functor  FUNCTOR_error2;
static Functor  FUNCTOR_send2;
static Functor  FUNCTOR_get2;
static Functor  FUNCTOR_existence_error2;
static Functor  FUNCTOR_permission_error3;
static Functor  FUNCTOR_type_error2;
static Functor  FUNCTOR_domain_error2;
static Functor  FUNCTOR_behaviour1;
static predicate_t PREDICATE_send_implementation;
static predicate_t PREDICATE_get_implementation;

static long			pl_max_integer;
static PL_dispatch_hook_t	old_dispatch_hook;

#define PROLOG_MAX_INTEGER pl_max_integer

#define AtomCharp(a)		PL_atom_chars((a))
#define GetInteger(a, i)	PL_get_long((a), (i))
#define GetAtom(a, n)		PL_get_atom((a), (n))
#define GetString(t, s, l)	PL_get_string((t), (s), (l))
#define GetFloat(a, f)		PL_get_float((a), (f))
#define NewTerm()		PL_new_term_ref()
#define CopyTerm(t)		PL_copy_term_ref(t)
#define GetNameArity(t, n, a)	PL_get_name_arity((t), (n), (a))
#define IsVar(t)		PL_is_variable(t)
#define GetArg(n, t, a)		PL_get_arg((n), (t), (a))
#define QGetArg(n, t, a)	_PL_get_arg((n), (t), (a))
#define ConsFunctor		PL_cons_functor
#define IsFunctor(t, f)		PL_is_functor((t), (f))
#define PutFunctor(t, n, a)	PL_put_functor((t), PL_new_functor((n), (a)))
#define PutCharp(t, s)		PL_put_atom_chars((t), (s))
#define PutInteger(t, i)	PL_put_integer((t), (i))
#define PutFloat(t, f)		PL_put_float((t), (f))
#define PutTerm(t, f)		PL_put_term((t), (f))
#define Unify(t1, t2)		PL_unify((t1), (t2))
#define UnifyAtom(t, a)		PL_unify_atom((t), (a))
#define UnifyFloat(t, a)	PL_unify_float((t), (a))
#define UnifyInteger(t, a)	PL_unify_integer((t), (a))
#define PutAtom(t, a)		PL_put_atom((t), (a))
#define PutVar(t)		PL_put_variable((t))
#define StripModuleTag(t, m, p)	PL_strip_module((t), (m), (p))
#define FindPredicate(n, a, m)	PL_pred(PL_new_functor(n, a), m)
#define OpenForeign()		PL_open_foreign_frame()
#define CloseForeign(fid)	PL_close_foreign_frame(fid)
#define DebugMode		(pceExecuteMode() == PCE_EXEC_USER \
					? PL_Q_NORMAL : PL_Q_NODEBUG)

#define META	PL_FA_TRANSPARENT
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
  
  FUNCTOR_behaviour1        = PL_new_functor(ATOM_behaviour, 1);
  FUNCTOR_error2  	    = PL_new_functor(ATOM_error, 2);
  FUNCTOR_existence_error2  = PL_new_functor(ATOM_existence_error, 2);
  FUNCTOR_get2    	    = PL_new_functor(ATOM_get, 2);
  FUNCTOR_module2 	    = PL_new_functor(ATOM_module, 2);
  FUNCTOR_namearg 	    = PL_new_functor(ATOM_assign, 2);
  FUNCTOR_context2	    = PL_new_functor(ATOM_context, 2);
  FUNCTOR_pce1	  	    = PL_new_functor(ATOM_pce, 1);
  FUNCTOR_pce2    	    = PL_new_functor(ATOM_pce, 2);
  FUNCTOR_pce3    	    = PL_new_functor(ATOM_pce, 3);
  FUNCTOR_permission_error3 = PL_new_functor(ATOM_permission_error, 3);
  FUNCTOR_ref1    	    = PL_new_functor(ATOM_ref, 1);
  FUNCTOR_new1    	    = PL_new_functor(ATOM_new, 1);
  FUNCTOR_send2   	    = PL_new_functor(ATOM_send, 2);
  FUNCTOR_spy1    	    = PL_new_functor(ATOM_spy, 1);
  FUNCTOR_string1 	    = PL_new_functor(ATOM_string, 1);
  FUNCTOR_trace1  	    = PL_new_functor(ATOM_trace, 1);
  FUNCTOR_type_error2       = PL_new_functor(ATOM_type_error, 2);
  FUNCTOR_domain_error2     = PL_new_functor(ATOM_domain_error, 2);

  PREDICATE_send_implementation = PL_predicate("send_implementation", 3,
					       "pce_principal");
  PREDICATE_get_implementation = PL_predicate("get_implementation", 4,
					       "pce_principal");
}


static int
GetChars(Term t, char **s, unsigned int *len)
{ return PL_get_nchars(t, len, s, CVT_ALL|BUF_RING);
}


#ifdef __WIN32__
#include <console.h>

static RlcUpdateHook old_update_hook;

static void *
getConsoleFunction(const char *name)
{ HMODULE hconsole;

  if ( (hconsole=GetModuleHandle("plterm")) )
  { return GetProcAddress(hconsole, name);
  }

  return NULL;
}


static RlcUpdateHook
indirect_rlc_update_hook(RlcUpdateHook hook)
{ RlcUpdateHook (*sethook)(RlcUpdateHook new);

  if ( (sethook = getConsoleFunction("rlc_update_hook")) )
    return (*sethook)(hook);

  return NULL;
}


static HWND
indirect_rlc_hwnd()
{ HWND (*f)(void);

  if ( (f = getConsoleFunction("rlc_hwnd")) )
    return (*f)();
  
  return 0;
}


#define PROLOG_ITF_INIT() \
	{ }
#define PROLOG_INSTALL_REDRAW_FUNCTION(f) \
	{ old_update_hook = indirect_rlc_update_hook(f); }
#define O_SHAREDLIBRARY
#endif

#ifdef O_SHAREDLIBRARY
#ifndef PROLOG_ITF_INIT
#define PROLOG_ITF_INIT() { }
#endif
#define PROLOG_ONEXIT(f)	{ exitpce_hook = (OnExitFunction) f; }

static OnExitFunction		exitpce_hook;

install_t
install_pl2xpce()
{ PL_register_foreign("$pce_init", 1, pl_pce_init, PL_FA_TRANSPARENT);
}

install_t
uninstall_pl2xpce()
{ static int doing = FALSE;		/* avoid recursion */

  if ( doing )
    return;
  doing = TRUE;

  DEBUG(Sdprintf("Removing hooks (%p and %p)\n",
		 old_dispatch_hook, old_update_hook));

  PL_dispatch_hook(old_dispatch_hook);
#ifdef __WIN32__
  indirect_rlc_update_hook(old_update_hook);
#endif
  if ( exitpce_hook )
    (*exitpce_hook)();
}

#endif /*O_SHAREDLIBRARY*/

#endif /*SWI*/

		 /*******************************
		 *         EXCEPTIONS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
XPCE calls raising errors are mapped into Prolog exceptions if the error
is directly related to an interface call.   The  error term is compliant
with the normal Prolog error-exceptions: error(Kind, Context).

Defined error Kinds:

	type_error(pce(Type), Actual)
	type_error(pce(too_many_arguments), N)
	existence_error(object, Ref)
+	existence_error(behaviour, send(Ref, Selector))
+	existence_error(behaviour, get(Ref, Selector))
	existence_error(named_argument,  Name)
	existence_error(argument, ArgN)
			
Defined context terms

	send(Obj, Msg)
	get(Obj, Msg)
	behaviour(Impl)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define EX_GOAL				1 /* goal, receiver, message */
#define EX_BAD_INTEGER_OBJECT_REF	2 /* <integer> */
#define EX_BAD_ATOM_OBJECT_REF		3 /* <atom> */
#define EX_BAD_OBJECT_REF		4 /* <term> */
#define EX_TYPE				5 /* <type-name>, <term> */
#define EX_DOMAIN			6 /* <domain-name>, <term> */
#define EX_PERMISSION			7 /* op, type, obj, msg */
#define EX_TOO_MANY_ARGUMENTS		8

static void
put_goal_context(Term ctx, PceGoal g, va_list args)
{ if ( g->flags & (PCE_GF_SEND|PCE_GF_GET) )
  { Term rec = va_arg(args, Term);
    Term msg = va_arg(args, Term);

    if ( g->flags & PCE_GF_SEND )
      ConsFunctor(ctx, FUNCTOR_send2, rec, msg);
    else
      ConsFunctor(ctx, FUNCTOR_get2, rec, msg);
  } else				/* new/2 */
  { Term descr = va_arg(args, Term);

    ConsFunctor(ctx, FUNCTOR_new1, descr);
  }
}


static int
add_list(PceObject e, void *closure)
{ Term tail = ((Term *)closure)[0];
  Term head = ((Term *)closure)[1];
  Term tmp  = ((Term *)closure)[2];

  PL_unify_list(tail, head, tail);
  put_object(tmp, e);
  return PL_unify(head, tmp);
}


static int
ThrowException(int id, ...)
{ va_list args;
  Term et  = NewTerm();			/* the error term */
  Term err = NewTerm();			/* the 1-st argument */
  Term ctx = NewTerm();			/* The 2-nd (context) argument */

  va_start(args, id);
  switch(id)
  { case EX_GOAL:			/* goal, receiver, message */
    { PceGoal g = va_arg(args, PceGoal);

      switch( g->errcode )
      { case PCE_ERR_ERROR:
	{ Term a1 = NewTerm();
	  Term a2 = NewTerm();
	  Term l[3];
	  l[0] = PL_copy_term_ref(a2);
	  l[1] = NewTerm();
	  l[2] = NewTerm();

	  put_object(a1, g->errc1);		/* error->id */
	  pceEnumElements(g->errc2, add_list, (void *)l);
	  PL_unify_nil(l[0]);			/* the tail */

	  ConsFunctor(err, FUNCTOR_pce2, a1, a2);
	  put_goal_context(ctx, g, args);
	  break;
	}
	default:
	  assert(0);
      }
      break;
    }
    case EX_BAD_INTEGER_OBJECT_REF:		/* , <integer> */
    { long ref = va_arg(args, long);
      char *descr = pcePPReference(cToPceInteger(ref));
      Term a1 = NewTerm();
      Term a2 = NewTerm();
      Term na = NewTerm();

      PutAtom(a1, ATOM_object);
      ConsFunctor(a1, FUNCTOR_pce1, a1);
      PutInteger(a2, ref);
      ConsFunctor(a2, FUNCTOR_ref1, a2);
      ConsFunctor(err, FUNCTOR_existence_error2, a1, a2);

      if ( descr[0] == '@' )
      { char *s;

	for(s=&descr[1]; *s && isdigit(*s&0xff); s++)
	  ;
	if ( *s )
	{ PutCharp(ctx, descr);			/* context(_, Message) */
	  ConsFunctor(ctx, FUNCTOR_context2, na, ctx);
	}
      }

      break;
    }
    case EX_BAD_ATOM_OBJECT_REF:		/* , <name> */
    { Atom ref = va_arg(args, long);
      Term a1 = NewTerm();
      Term a2 = NewTerm();

      PutAtom(a1, ATOM_object);
      ConsFunctor(a1, FUNCTOR_pce1, a1);
      PutAtom(a2, ref);
      ConsFunctor(a2, FUNCTOR_ref1, a2);

      ConsFunctor(err, FUNCTOR_existence_error2, a1, a2);
      break;
    }
    case EX_BAD_OBJECT_REF:			/* not @<name-or-int> */
    { Term ref = va_arg(args, long);
      Term a1  = NewTerm();

      PutAtom(a1, ATOM_object);
      ConsFunctor(a1, FUNCTOR_pce1, a1);

      ConsFunctor(err, FUNCTOR_type_error2, a1, ref);
      break;
    }
    case EX_TYPE:				/* type-name, arg */
    { Term a1 = NewTerm();
      Atom tn = va_arg(args, Atom);
      Term v  = va_arg(args, Term);

      PutAtom(a1, tn);
      ConsFunctor(a1, FUNCTOR_pce1, a1);
      
      ConsFunctor(err, FUNCTOR_type_error2, a1, v);
      break;
    }
    case EX_DOMAIN:				/* domain-name, arg */
    { Term a1 = NewTerm();
      Atom tn = va_arg(args, Atom);
      Term v  = va_arg(args, Term);

      PutAtom(a1, tn);
      ConsFunctor(err, FUNCTOR_domain_error2, a1, v);
      break;
    }
    case EX_PERMISSION:
    { Term a1 = NewTerm();
      Term a2 = NewTerm();
      Term a3 = NewTerm();
      Atom op = va_arg(args, Atom);
      Atom tp = va_arg(args, Atom);
      PceObject obj = va_arg(args, PceObject);
      Atom msg = va_arg(args, Atom);
      
      PutAtom(a1, op);
      PutAtom(a2, tp);
      put_object(a3, obj);
      ConsFunctor(err, FUNCTOR_permission_error3, a1, a2, a3);
      
      PutVar(a1);
      PutAtom(a2, msg);
      ConsFunctor(ctx, FUNCTOR_context2, a1, a2);
      break;
    }
    default:
      assert(0);
  }
  va_end(args);
  
  ConsFunctor(et, FUNCTOR_error2, err, ctx);

  return PL_raise_exception(et);
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

#include "table.c"

static Atom
nameToAtom(PceName name)
{ unsigned int len;
  char *s = pceCharArrayToC(name, &len);

  if ( s )
    return PL_new_atom_nchars(len, s);

  return (Atom)0;
}


static __inline PceName
atomToAssoc(Atom a)
{ return a ? atomToName(a) : NIL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get an XPCE object-reference from a Prolog term we already know to be of
the form @/1.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
get_object_from_refterm(Term t, PceObject *obj)
{ Term a = NewTerm();
  PceObject o;
  long r;
  Atom name;

  _PL_get_arg(1, t, a);

  if ( GetInteger(a, &r) )
  { if ( (o = cToPceReference(r)) )
    { *obj = o;

      return TRUE;
    }
    
    return ThrowException(EX_BAD_INTEGER_OBJECT_REF, r);
  }

  if ( GetAtom(a, &name) )
  { if ( (o = pceObjectFromName(atomToName(name))) )
    { *obj = o;

      return TRUE;
    }

    return ThrowException(EX_BAD_ATOM_OBJECT_REF, name);
  }

  return ThrowException(EX_BAD_OBJECT_REF, t);
}


static int
unifyReferenceArg(Term t, int type, PceCValue value)
{ Term t2 = NewTerm();			/* Exploit SWI-Prolog PL_unify-* */
  
  if ( type == PCE_REFERENCE )
  { PutInteger(t2, value.integer);
  } else
  { PceITFSymbol symbol = value.itf_symbol;

    PutAtom(t2, CachedNameToAtom(symbol->name));
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
    r.value.a = CachedNameToAtom(symbol->name);
  }
  return _PL_unify_xpce_reference(t, &r);

#else /*HAVE_XPCEREF*/
    
  Term t2 = NewTerm();
  Term r  = NewTerm();
  
  if ( type == PCE_REFERENCE )
  { PutInteger(t2, value.integer);
  } else
  { PceITFSymbol symbol = value.itf_symbol;

    PutAtom(t2, CachedNameToAtom(symbol->name));
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
{ PceObject rval;

  if ( IsVar(ref) )
  { if ( (rval = termToObject(t, NULL, NULLATOM, TRUE)) )
    { PceCValue value;
      int type = pceToCReference(rval, &value);

      if ( unifyReference(ref, type, value) )
	return rval;
    }

    return PCE_FAIL;
  } else if ( IsFunctor(ref, FUNCTOR_ref1) )
  { Term a = NewTerm();
    Atom assoc;

    QGetArg(1, ref, a);

    if ( !GetAtom(a, &assoc) )		/* new(@foo, ...) */
    { if ( IsVar(a) )
	assoc = 0;			/* new(@X, ...) */
      else
	goto error;
    }

    if ( (rval = termToObject(t, NULL, assoc, TRUE)) )
    { PceCValue value;
      int type = pceToCReference(rval, &value);

      if ( unifyReferenceArg(a, type, value) )
	return rval;
    }

    return PCE_FAIL;
  }

error:
  ThrowException(EX_TYPE, ATOM_named_reference, ref);
  return PCE_FAIL;
}

		 /*******************************
		 *      HOST DATA HANDLES	*
		 *******************************/

typedef struct _host_stack_entry
{ PceObject handle;
  struct _host_stack_entry *previous;
} host_stack_entry, *HostStackEntry;

static HostStackEntry host_handle_stack;

static __inline PceObject
pushHostHandle(PceObject h)
{ HostStackEntry e = pceAlloc(sizeof(host_handle_stack));

  e->handle   = h;
  e->previous = host_handle_stack;
  host_handle_stack = e;

  return h;
}


static __inline void
rewindHostHandles(HostStackEntry top)
{ if ( top != host_handle_stack )
  { HostStackEntry p, e = host_handle_stack;

    for( ; e && e != top; e = p )
    { p = e->previous;
  
      if ( !freeHostData(e->handle) )
      { Term t = getTermHandle(e->handle);
	Record r = PL_record(t);
  
	assert((((unsigned long)r & 0x1L) == 0L));
	setHostDataHandle(e->handle, r);
      }
  
      pceUnAlloc(sizeof(host_handle_stack), e);
    }
  
    host_handle_stack = top;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Term handles appear in two formats: as direct handles to Prolog terms and
as handles to the Prolog recorded database.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static PceObject
makeTermHandle(term_t t)
{ void *h = (void *)(((unsigned long)PL_copy_term_ref(t)<<1) | 0x1L);

  return pushHostHandle(CtoHostData(ClassProlog, h, 0));
}


static PceObject
makeRecordedTermHandle(term_t t)
{ Record r = PL_record(t);
  
  assert((((unsigned long)r & 0x1L) == 0L));
  return CtoHostData(ClassProlog, r, PCE_ANSWER);
}


static Term
getTermHandle(PceObject hd)
{ void *h;

  if ( (h = getHostDataHandle(hd)) )
  { unsigned long l = (unsigned long)h;

    if ( l & 1 )
      return (Term)(l>>1);
    else
    { Term t = NewTerm();

      PL_recorded(h, t);
      return t;
    }
  }

  return 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translate a message argument into an XPCE  object.

Returns  FALSE  and  raises  an  exception    of  the  the  argument  is
@<bad-reference>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
get_object_arg(term_t t, PceObject* obj)
{ term_value_t val;

  switch(PL_get_term_value(t, &val))
  { case PL_ATOM:
      *obj = atomToName(val.a);
      return TRUE;
    case PL_INTEGER:
      *obj = cToPceInteger(val.i);
      return TRUE;
    case PL_FLOAT:
      *obj = cToPceReal(val.f);
      return TRUE;
    case PL_TERM:			/* @reference */
      if ( val.t.name == ATOM_ref && val.t.arity == 1 )
	return get_object_from_refterm(t, obj);

      if ( val.t.name == ATOM_assign && val.t.arity == 2 )
      { Term a = NewTerm();
	Atom an;
    
	QGetArg(1, t, a);
	if ( GetAtom(a, &an) )
	{ PceObject av[2];

	  QGetArg(2, t, a);
	  av[0] = atomToName(an);
	  if ( !get_object_arg(a, &av[1]) )
	    return FALSE;

	  *obj = pceNew(NIL, ClassBinding, 2, av);

	  return TRUE;
	}
      }
    /*FALLTHROUGH*/
    default:
      *obj = makeTermHandle(t);
      return TRUE;
  }
}


static int
get_typed_object(PceGoal g, term_t t, PceType type, PceObject* rval)
{ PceObject obj = PCE_FAIL, obj2;
  term_value_t val;

  switch(PL_get_term_value(t, &val))
  { case PL_ATOM:
      obj = atomToName(val.a);
      break;
    case PL_INTEGER:
      obj = cToPceInteger(val.i);
      break;
    case PL_FLOAT:
      obj = cToPceReal(val.f);
      break;
    case PL_TERM:			/* @reference */
      if ( val.t.name == ATOM_ref && val.t.arity == 1 )
	get_object_from_refterm(t, &obj);
      break;
  }

  if ( !obj )
  { if ( pceIncludesHostDataType(type, ClassProlog) )
    { *rval = makeTermHandle(t);
      return TRUE;
    }

    if ( !(obj = termToObject(t, type, NULLATOM, FALSE)) )
      return pceSetErrorGoal(g, PCE_ERR_ARGTYPE, makeTermHandle(t));
  }

  if ( (obj2 = pceCheckType(g, type, obj)) )
  { *rval = obj2;
    return TRUE;
  }

  return pceSetErrorGoal(g, PCE_ERR_ARGTYPE, makeTermHandle(t));
}


static __inline PceObject
termToReceiver(term_t t)
{ return termToObject(t, NULL, NULLATOM, FALSE);
}


static int
get_answer_object(PceGoal g, Term t, PceType type, PceObject *rval)
{ PceObject obj = PCE_FAIL, obj2;
  term_value_t val;

  switch(PL_get_term_value(t, &val))
  { case PL_ATOM:
      obj = atomToName(val.a);
      break;
    case PL_INTEGER:
      obj = cToPceInteger(val.i);
      break;
    case PL_FLOAT:
      obj = cToPceReal(val.f);
      break;
    case PL_TERM:			/* @reference */
      if ( val.t.name == ATOM_ref && val.t.arity == 1 )
	get_object_from_refterm(t, &obj);
      break;
  }

  if ( !obj )
  { if ( pceIncludesHostDataType(type, ClassProlog) )
    { *rval = makeRecordedTermHandle(t);
      return TRUE;
    }

    if ( !(obj = termToObject(t, type, NULLATOM, FALSE)) )
      return pceSetErrorGoal(g, PCE_ERR_RETTYPE, makeRecordedTermHandle(t));
  }

  if ( (obj2 = pceCheckType(g, type, obj)) )
  { *rval = obj2;
    return TRUE;
  }

  return pceSetErrorGoal(g, PCE_ERR_RETTYPE, makeRecordedTermHandle(t));
}



		 /*******************************
		 *	   CLASS PROLOG		*
		 *******************************/

static int
unlinkProlog(PceObject hd)
{ void *h = getHostDataHandle(hd);
  
  if ( !((unsigned long)h & 0x1) )
  { /*Sdprintf("Erasing recorded Prolog term\n");*/
    PL_erase(h);			/* This is a record */
  }

  return PCE_SUCCEED;
}


static PceObject
getPrintNameProlog(PceObject hd)
{ char *buffer = NULL;
  int   size   = 0;
  PceObject rval;

  IOSTREAM *s = Sopenmem(&buffer, &size, "w");
  PL_write_term(s, getTermHandle(hd), 1200, 0);
  Sputc('\0', s);
  Sflush(s);
  rval = cToPceString(NIL, buffer, size-1, FALSE);
  Sclose(s);

  return rval;
}


static int
equalProlog(PceObject p1, PceObject p2)
{ term_t t1 = getTermHandle(p1);
  term_t t2 = getTermHandle(p2);

  if ( !(t2 = getTermHandle(p2)) )
  { Atom a = nameToAtom(p2);
    
    if ( a )
    { t2 = PL_new_term_ref();
      PL_put_atom(t2, a);
    } else
      return PCE_FAIL;
  }

  if ( PL_compare(t1, t2) == 0 )
    return PCE_SUCCEED;

  return PCE_FAIL;
}


static void
makeClassProlog()
{ PceObject av[4];
  PceObject supers;

  av[0] = cToPceName("prolog_term");
  av[1] = cToPceName("host_data");
  ClassProlog = pceNew(NIL, cToPceName("class"), 2, av);

  av[0] = cToPceName("none");
  pceSend(ClassProlog, NULL, cToPceName("clone_style"), 1, av);
  pceSendMethod(ClassProlog,			/* The class */
		"unlink",			/* Name of the method */
		NULL,				/* Group */
		0,				/* # arguments */
		"Discard associated term", 	/* Summary */
		unlinkProlog);			/* Function */
  pceGetMethod (ClassProlog,			/* The class */
		"print_name",			/* Name of the method */
		NULL,				/* Group */
		"string",			/* Return type */
		0,				/* # arguments */
		"Discard associated term", 	/* Summary */
		getPrintNameProlog);		/* Function */
  
  /* type(prolog, atomic, @default, chain(type(prolog_term))) */

  av[0] = cToPceName("prolog_term");
  av[1] = cToPceName("type");
  TypePrologTerm = pceGet(cToPceAssoc("pce"), NULL, cToPceName("convert"),
			  2, av);
  av[0] = TypePrologTerm;
  supers = pceNew(NIL, cToPceName("chain"), 1, av);

  av[0] = cToPceName("prolog");
  av[1] = cToPceName("atomic");
  av[2] = DEFAULT;
  av[3] = supers;
  TypeProlog = pceNew(NIL, cToPceName("type"), 4, av);

  assert(TypeProlog);

  pceSendMethod(ClassProlog,			/* The class */
		"equal",			/* Name of the method */
		NULL,				/* Group */
		1,				/* # arguments */
		"prolog",			/* Type arg1 */
		"Test equality (==)", 		/* Summary */
		equalProlog);			/* Function */
}





		 /*******************************
		 *	PROLOG --> XPCE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
termToObject(Term t, PceType targettype, Atom assoc, int new)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static PceObject
termToObject(Term t, PceType type, Atom assoc, int new)
{ Atom functor;
  int arity;

  if ( GetNameArity(t, &functor, &arity) )
  { 					/* Just an atom */
    if ( arity == 0 )
    { PceName name = atomToName(functor);

      return (new ? pceNew(atomToAssoc(assoc), name, 0, NULL) : name);
    }
					/* @Ref */
    if ( functor == ATOM_ref && arity == 1 )
    { PceObject rval;

      if ( get_object_from_refterm(t, &rval) )
	return rval;

      return PCE_FAIL;
    }
					/* new/[1,2] */
    if ( functor == ATOM_new )
    { if ( arity == 1 )			/* new(chain) */
      { Term a = NewTerm();

	QGetArg(1, t, a);
	return termToObject(a, NULL, NULLATOM, TRUE);
      }
      if ( arity == 2 )			/* new(B, box) */
      { Term r = NewTerm();
	Term n = NewTerm();

	QGetArg(1, t, r);
	QGetArg(2, t, n);
	return do_new(r, n);
      }
    }

					/* string(hello) */
    if ( functor == ATOM_string && arity == 1 )
    { char *s;
      unsigned int len;
      Term a = NewTerm();
      PceName pceassoc = atomToAssoc(assoc);

      QGetArg(1, t, a);
      if ( GetChars(a, &s, &len) )
	return cToPceString(pceassoc, s, len, TRUE);

      ThrowException(EX_TYPE, ATOM_string, t);
      return PCE_FAIL;
    }

					/* prolog(Term) */
    if ( functor == ATOM_prolog && arity == 1 )
    { PceObject h;
      Term a = NewTerm();
      double f;
      long r;
      
      QGetArg(1, t, a);

      if ( GetInteger(a, &r) )		/* pass atoms, ints and floats */
	return cToPceInteger(r);	/* as xpce objects anyhow */
      if ( GetFloat(t, &f) )
	return cToPceReal(f);

      h = makeTermHandle(a);		/* real terms */
      makeAnyHostData(h);		/* make acceptable to any/object */

      return h;
    }
					/* A list */
    if ( functor == ATOM_dot && arity == 2 )
    { Term tail = CopyTerm(t);
      Term head = NewTerm();
      int argsallocated = 16;
      int argc = 0;
      PceObject *argv = alloca(argsallocated*sizeof(PceObject));
      PceName classname = NAME_codeVector;

      while ( PL_get_list(tail, head, tail) )
      { PceObject a;

	if ( get_object_arg(head, &a) )
	{ if ( argc >= argsallocated )
	  { PceObject tmp = alloca(2*argsallocated*sizeof(PceObject));
	    memcpy(tmp, argv, argsallocated*sizeof(PceObject));
	    argv = tmp;
	    argsallocated *= 2;
	  }
	  argv[argc++] = a;
	} else
	  return PCE_FAIL;
      }

      if ( !PL_get_nil(tail) )
      { ThrowException(EX_TYPE, ATOM_proper_list, t);
	return PCE_FAIL;
      }

      if ( type )
      { if ( pceSend(type, NULL, NAME_includes,
		     1, (PceObject *)&NAME_chain) )
	  classname = NAME_chain;
	else if ( pceSend(type, NULL, NAME_includes, 1,
			  (PceObject *)&NAME_vector) )
	  classname = NAME_vector;
      }

      return pceNew(NIL, classname, argc, argv);
    }

					/* Class(...Args...) */
    { PceName name = atomToName(functor);
      ArgVector(argv, arity);
      Term a = NewTerm();
      int n;

      for(n=0 ; n < arity; n++ )
      { QGetArg(n+1, t, a);
	if ( !get_object_arg(a, &argv[n]) )
	  return PCE_FAIL;
      }

      return pceNew(atomToAssoc(assoc), name, arity, argv);
    }
  } else				/* not a term */
  { double f;
    long r;

    
    if ( GetInteger(t, &r) )
      return cToPceInteger(r);

#ifdef O_STRING
  { char *s;
    int len;
    if ( GetString(t, &s, &len) )	/* string object (if supported) */
      return cToPceString(atomToAssoc(assoc), s, len, FALSE);
  }
#endif

    if ( GetFloat(t, &f) )		/* floating point number */
      return cToPceReal(f);

					/* anything else */
    ThrowException(EX_TYPE, ATOM_object, t);
    return PCE_FAIL;
  }
}


		 /*******************************
		 *	  OBJECT-TO-TERM	*
		 *******************************/

static int
unifyObject(Term t, PceObject obj, int top)
{ PceCValue value;
  int pcetype;
  char *s;
  Term tmpt;

  switch( (pcetype = pceToC(obj, &value)) )
  { case PCE_INTEGER:			/* integer */
      return UnifyInteger(t, value.integer);
    case PCE_REAL:			/* float (real object) */
      return UnifyFloat(t, value.real);
    case PCE_NAME:			/* name */
    { unsigned int len;
      char *s = pceCharArrayToC(obj, &len);

      return PL_unify_atom_nchars(t, len, s);
    }
    case PCE_HOSTDATA:
      return Unify(t, getTermHandle(obj)); /* TBD: avoid redoing this */
    case PCE_REFERENCE:
    case PCE_ASSOC:
      if ( !top )
      { Atom n;
	int a;

	if ( IsVar(t) )			/* get(R, S, Var) */
	  return unifyReference(t, pcetype, value);

					/* get(R, S, @something) */
	if ( GetNameArity(t, &n, &a) && n == ATOM_ref && a == 1 )
	{ tmpt = NewTerm();

	  QGetArg(1, t, tmpt);
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
    } else if ( GetNameArity(t, &name, &arity) &&
		name == ATOM_string && arity == 1 )
    { Term a = NewTerm();

      PutCharp(a, s);
      tmpt = NewTerm();
      QGetArg(1, t, tmpt);
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

    if ( !(got = pceGet(obj, NULL, NAME_functor, 0, NULL)) ||
	 !(pname = nameToAtom(got)) )
      return FALSE;
    if ( !(got = pceGet(obj, NULL, NAME_Arity, 0, NULL)) ||
	 pceToC(got, &value) != PCE_INTEGER )
      return FALSE;
    parity = value.integer;
    
    if ( GetNameArity(t, &name, &arity) )
    { if ( name != pname || arity != parity )
	return FALSE;
      for(n=1; n<=arity; n++)
      { PceObject pcen = cToPceInteger(n);
  
	if ( (got = pceGet(obj, NULL, NAME_Arg, 1, &pcen)) )
	{ QGetArg(n, t, at);
	  
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
  
	if ( (got = pceGet(obj, NULL, NAME_Arg, 1, &pcen)) )
	{ QGetArg(n, t2, at);
	  
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

static __inline Module
PushDefaultModule()
{ Module odm = DefaultModule;

  DefaultModule = 0;
  return odm;
}

#define PopDefaultModule(o)	(DefaultModule = (o))

					/* NEW */
static foreign_t
pl_new(Term assoc, Term descr)
{ AnswerMark mark;
  PceObject obj;
  Term d = NewTerm();
  Module odm;
  pce_goal goal;
  HostStackEntry hmark;

  LOCK();
  odm		      =	PushDefaultModule();
  hmark               = host_handle_stack;
  goal.flags	      =	PCE_GF_CATCH;
  goal.errcode	      =	PCE_ERR_OK;
  goal.argc	      =	0;
  goal.receiver	      =	NIL;
  goal.implementation =	NIL;
  pcePushGoal(&goal);

  StripModuleTag(descr, &DefaultModule, d);
  markAnswerStack(mark);
  obj = do_new(assoc, d);
  rewindAnswerStack(mark, obj);
  rewindHostHandles(hmark);
  PopDefaultModule(odm);

  if ( !obj && (goal.flags & PCE_GF_THROW) )
    ThrowException(EX_GOAL, &goal, descr);

  pceFreeGoal(&goal);
  UNLOCK();
  
  return obj ? TRUE : FALSE;
}


static __inline int
get_pce_class(Term t, PceClass *cl)
{ if ( t )
  { Atom a;

    if ( GetAtom(t, &a) )
    { PceClass class = nameToExistingClass(atomToName(a));

      if ( class )
      { *cl = class;
        return TRUE;
      }
    }

    return FALSE;
    assert(0);				/* Raise exception */
  }

  *cl = NULL;
  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Put @default into the argument term
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
put_default(PceGoal g, int n, Term t)
{ PceObject v = pceCheckType(g, g->types[n], DEFAULT);

  if ( v == DEFAULT )			/* pass @default */
  { PutAtom(t, ATOM_default);
    ConsFunctor(t, FUNCTOR_ref1, t);
  } else if ( v )
  { put_object(t, v);			/* some converted object */
  } else
    return pceSetErrorGoal(g, PCE_ERR_MISSING_ARGUMENT, cToPceInteger(n));

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
put_prolog_argument()
    Does Prolog-to-Prolog conversion of arguments.  If the type has a
    natural Prolog counterpart, the reference is passed directly.  Otherwise
    it is translated to XPCE for conversion.  Atoms, Names and floats have
    these natural counterparts.  See alse get_object_arg().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
put_prolog_argument(PceGoal g, Term t, PceType type, Term f)
{ PceObject obj;
  term_value_t val;
					/* --> :prolog */
  if ( pceIncludesHostDataType(type, ClassProlog) )
  { PutTerm(t, f);
    return TRUE;
  }

  switch(PL_get_term_value(f, &val))
  { case PL_ATOM:
      if ( pceCheckNameType(type, AtomCharp(val.a)) )
      { PutAtom(t, val.a);
	return TRUE;
      }
      break;
    case PL_INTEGER:
      if ( pceCheckIntType(type, val.i) )
      { PutInteger(t, val.i);
	return TRUE;
      }
      break;
    case PL_FLOAT:
      if ( pceCheckFloatType(type, val.f) )
      { PutTerm(t, f);
	return TRUE;
      }
      break;
    case PL_TERM:
      if ( val.t.name == ATOM_ref && val.t.arity == 1 )
      { PceObject obj2;

	if ( !get_object_from_refterm(f, &obj) )
	{ g->errcode = PCE_ERR_OK;	/* TBD: Should be something else  */
	  return FALSE;
	}

	if ( (obj2 = pceCheckType(g, type, obj)) )
	{ if ( obj2 == obj )
	    PutTerm(t, f);
	  else
	    put_object(t, obj2);

	  return TRUE;
	}
      }
  }

					/* anything else */
  if ( (obj = termToObject(f, type, NULLATOM, FALSE)) )
  { PceObject obj2;

    if ( (obj2 = pceCheckType(g, type, obj)) )
    { put_object(t, obj2);
      return TRUE;
    }

    return pceSetErrorGoal(g, PCE_ERR_ARGTYPE, obj);
  }

  return pceSetErrorGoal(g, PCE_ERR_ARGTYPE, NIL);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stripArgName(?t, PceName *name)
    if t is <atom> := <value>, put value into <t> and <atom> as an XPCE
    name object into name.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static __inline void
stripArgName(Term t, PceName *name)
{ if ( IsFunctor(t, FUNCTOR_namearg) )	/* Name := Value */
  { Term a = NewTerm();
    Atom an;

    QGetArg(1, t, a);
    if ( GetAtom(a, &an) )
    { *name = atomToName(an);
      QGetArg(2, t, t);
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
invoke(Term receiver, Term class, Term selector, Term return)
    This function is the central code fore invoking both XPCE send- and
    get-methods.  `Class' and `return' may be 0 to indicate these arguments
    don't care.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
invoke(Term rec, Term cl, Term msg, Term ret)
{ int rval = FALSE;
  AnswerMark mark;
  PceObject receiver;
  Module odm;
  pce_goal goal;
  HostStackEntry hmark;

  LOCK();
  odm = PushDefaultModule();
  hmark = host_handle_stack;

  goal.flags = (ret ? PCE_GF_GET : PCE_GF_SEND)|PCE_GF_CATCH;
  goal.errcode = PCE_ERR_OK;
  goal.rval  = NIL;			/* for rewindAnswerStack() */

  markAnswerStack(mark);
  receiver = termToReceiver(rec);

  if ( receiver )
  { Atom name;
    int  arity;
    PceClass class;

    get_pce_class(cl, &class);
    StripModuleTag(msg, &DefaultModule, msg);
    if ( GetNameArity(msg, &name, &arity) )
    { PceName selector = atomToName(name);
      Term arg;

      if ( arity > 0 )
	arg = NewTerm();
      else
	arg = 0;			/* make compiler happy */

      goal.receiver = receiver;
      goal.class    = class;
      goal.selector = selector;

      if ( pceResolveImplementation(&goal) )
      { if ( goal.flags & PCE_GF_HOST )
	{ prolog_call_data *pcd = get_pcd(goal.implementation);
				/* Implemented in Prolog */
	  fid_t  fid = PL_open_foreign_frame();
	  term_t av  = PL_new_term_refs(4);
	  term_t mav = PL_new_term_refs(pcd->argc);
	  term_t tmp, tmp2, tail;
	  int n;
	  
	  goal.flags |= PCE_GF_HOSTARGS;
	  pceInitArgumentsGoal(&goal);

	  if ( goal.va_type )
	  {				/* used by PrologWriteGoalArgs() */
	    goal.host_closure = (void *)(mav+pcd->argc-1);
	    tail = PL_copy_term_ref(mav+pcd->argc-1);
	    tmp  = PL_new_term_ref();
	    tmp2 = PL_new_term_ref();
	  } else
	    tmp = tmp2 = tail = 0;

					/* push method identifier */
	  if ( (pcd->flags & (PCE_METHOD_INFO_TRACE|PCE_METHOD_INFO_BREAK)) )
	    put_trace_info(av+0, pcd);
	  else
	    _PL_put_atomic(av+0, pcd->method_id);

	  if ( goal.flags & PCE_GF_CATCHALL )
	  { goal.argn++;
	    PL_put_atom(mav, CachedNameToAtom(goal.selector));
	    goal.argv[0] = (PceObject)mav;
	  }

	  for(n=0; n<arity; n++)
	  { PceName name = NULL;
	    PceType type;
	    int i;

	    QGetArg(n+1, msg, arg);
	    stripArgName(arg, &name);
	    if ( !pceGetArgumentTypeGoal(&goal, name, &type, &i) )
	      goto plerror;
	    if ( !put_prolog_argument(&goal,
				      i < 0 ? tmp : (mav+i),
				      type,
				      arg) )
	    { if ( goal.errcode == PCE_ERR_ARGTYPE )
	      { goal.argn = (i<0 ? goal.argc : i);
		goal.errc1 = makeTermHandle(arg);
	      }

	      goto plerror;
	    }
	    if ( i < 0 )
	    { PL_unify_list(tail, tmp2, tail);
	      PL_unify(tmp2, tmp);
	    } else
	      goal.argv[i] = (PceObject)(mav+i);
	  }
	  if ( tail )
	    PL_unify_nil(tail);

	  for(n=0; n<goal.argc; n++)
	  { if ( !goal.argv[n] && !put_default(&goal, n, mav+n) )
	      goto plerror;
	  }

	  PL_cons_functor_v(av+1, pcd->functor, mav);
						/* push @Receiver */
	  put_object(av+2, goal.receiver);

	  if ( ret )
	  { rval = PL_call_predicate(MODULE_user,
				     DebugMode|PL_Q_PASS_EXCEPTION,
				     PREDICATE_get_implementation, av);
	    if ( rval )
	    { if ( IsFunctor(av+3, FUNCTOR_ref1) )
	      { if ( !get_object_from_refterm(av+3, &goal.rval) )
		  goto out;

		if ( !PL_unify(ret, av+3) )
		  rval = unifyObject(ret, goal.rval, FALSE);
	      } else
		rval = PL_unify(ret, av+3);
	    }
	  } else
	    rval = PL_call_predicate(MODULE_user,
				     DebugMode|PL_Q_PASS_EXCEPTION,
				     PREDICATE_send_implementation, av);

	  PL_close_foreign_frame(fid);	/* keep bindings */
	  goto out;
	plerror:
	  pceReportErrorGoal(&goal);
	  PL_close_foreign_frame(fid);	/* keep bindings */
	  goto out;
	} else				/* Implemented in XPCE itself */
	{ int n;

	  pceInitArgumentsGoal(&goal);
	  for(n=0; n<arity; n++)
	  { PceObject value;
	    PceName name = NULL;
	    PceType type;
	    int i;

	    QGetArg(n+1, msg, arg);
	    stripArgName(arg, &name);
	    if ( !pceGetArgumentTypeGoal(&goal, name, &type, &i) )
	      goto error;
	    if ( !get_typed_object(&goal, arg, type, &value) )
	    { if ( goal.errcode == PCE_ERR_ARGTYPE )
		goal.argn = (i < 0 ? goal.argc : i);
	      goto error;
	    }
	    if ( i >= 0 )
	      goal.argv[i] = value;
	    else
	      pceVaAddArgGoal(&goal, value);
	  }
	  rval = pceExecuteGoal(&goal);
	  
	  if ( ret && rval )
	    rval = unifyObject(ret, goal.rval, FALSE);

	  goto out;
	error:
	  pceReportErrorGoal(&goal);
	  goto out;
	}
      } else /* no implementation */
	pceReportErrorGoal(&goal);
    }
  }
out:
  if ( goal.flags & PCE_GF_THROW )
    rval = ThrowException(EX_GOAL, &goal, rec, msg);
  rewindHostHandles(hmark);
  rewindAnswerStack(mark, goal.rval);
  PopDefaultModule(odm);
  pceFreeGoal(&goal);
  UNLOCK();

  return rval;
} 


static foreign_t
pl_send(Term rec, Term msg)
{ return invoke(rec, 0, msg, 0);
}


static foreign_t
pl_send_class(Term rec, Term cl, Term msg)
{ return invoke(rec, cl, msg, 0);
}


static foreign_t
pl_get(Term rec, Term msg, Term ret)
{ return invoke(rec, 0, msg, ret);
}


static foreign_t
pl_get_class(Term rec, Term cl, Term msg, Term ret)
{ return invoke(rec, cl, msg, ret);
}



		 /*******************************
		 *	    OBJECT/[1,2]	*
		 *******************************/

static foreign_t
pl_object1(Term ref)
{ Atom name;
  int arity;

  if ( GetNameArity(ref, &name, &arity) &&
       name == ATOM_ref &&
       arity == 1 )
  { Term a = NewTerm();
    Atom refname;
    long refi;

    QGetArg(1, ref, a);
    if ( GetAtom(a, &refname) )
      return pceExistsAssoc(atomToName(refname));
    else if ( GetInteger(a, &refi) )
      return pceExistsReference(refi);
  }

  return FALSE;
}


static foreign_t
pl_object2(Term ref, Term description)
{ PceObject obj;
  int rval;

  LOCK();
  if ( (obj = termToObject(ref, NULL, NULLATOM, FALSE)) )
    rval = unifyObject(description, obj, TRUE);
  else
    rval = FALSE;
  UNLOCK();

  return rval;
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

static int
PrologSend(PceObject prolog, PceObject sel, int argc, PceObject *argv)
{ Fid fid = OpenForeign();
  Module m = pceContextModule();
  PceCValue value;
  Predicate pred = NULL;
  Term goal = 0;
  int rval;

  switch(pceToC(sel, &value))
  { case PCE_NAME:
    { PceITFSymbol symbol = value.itf_symbol;
      pred = FindPredicate(nameToAtom(symbol->name), argc, m);
      break;
    }
    case PCE_HOSTDATA:
      goal = getTermHandle(sel);
      break;
    default:
      assert(0);			/* should not be passed */
  }

#ifdef SWI
  if ( pred )
  { Term terms = PL_new_term_refs(argc);
    qid_t qid;
    int i;

    for(i=0; i<argc; i++)
      put_object(terms+i, argv[i]);

    qid  = PL_open_query(m, DebugMode, pred, terms);
    rval = PL_next_solution(qid);
    PL_cut_query(qid);
  } else
  { if ( argc > 0 )
      rval = FALSE;			/* TBD */
    rval = PL_call(goal, m);
  }
#else /*~SWI*/
  if ( pred )
  { SP_term_ref *terms = alloca(sizeof(SP_term_ref) * argc);
    SP_qid qid;

    for(i=0; i<argc; i++)
    { terms[i] = SP_new_term_ref();
      SP_put_variable(terms[i]);
      put_object(terms[i], argv[i]);
    }

    if ( (qid = SP_open_query_array(pred, terms)) )
    { rval = prolog_exception(SP_next_solution(qid));
      SP_close_query(qid);
    } else
      return ThrowException(EX_OPEN_QUERY, name, argc);
  } else
  { Atom name, module;

    split_selector(sel, &name, &module);
    UndefinedPredicate(name, argc, module);
    rval = FALSE;
  }
#endif /*SWI*/

  CloseForeign(fid);
  return rval;
}


static PceObject
PrologGet(PceObject prolog, PceObject sel, int argc, PceObject *argv)
{ Fid fid = OpenForeign();
  Module m = pceContextModule();
  Atom name = nameToAtom(sel);
  Predicate pred = FindPredicate(name, argc+1, m);
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

  qid  = PL_open_query(m, DebugMode, pred, terms);
  rval = PL_next_solution(qid);
  PL_cut_query(qid);
  if ( rval )
    obj = termToObject(terms+argc, NULL, NULLATOM, FALSE);
  else
    obj = PCE_FAIL;

#else /*~SWI*/

  if ( pred )
  { SP_term_ref *terms = alloca(sizeof(SP_term_ref) * (argc+1));
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
	obj = termToAnswer(terms[argc]);
      else
	obj = PCE_FAIL;
      SP_close_query(qid);
    } else
    { ThrowException(EX_OPEN_QUERY, name, argc+1);
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


static void
put_object(Term t, PceObject obj)
{ PceCValue value;
  int pcetype;
  Atom avalue;

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

      avalue = CachedNameToAtom(symbol->name);

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
    case PCE_HOSTDATA:
    { PutTerm(t, getTermHandle(obj));	/* TBD: Use saved handle */
      break;
    }
    case PCE_INTEGER:
      PutInteger(t, value.integer);

      break;
    case PCE_NAME:
      { PceITFSymbol symbol = value.itf_symbol;

	avalue = nameToAtom(symbol->name);
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


static foreign_t
pl_pce_method_implementation(term_t id, term_t msg)
{ prolog_call_data *pcd = pceAlloc(sizeof(prolog_call_data));

  memset(pcd, 0, sizeof(*pcd));

  if ( PL_is_atomic(id) )
  { pcd->method_id = _PL_get_atomic(id);
  } else
  { return PL_warning("pce_method_implementation/2: type error");
  }

  return unifyObject(msg, cToPcePointer(pcd), FALSE);
}


static prolog_call_data *
get_pcd(PceObject method)
{ pce_method_info m;

  m.flags = PCE_METHOD_INFO_HANDLE_ONLY;
  if ( pceGetMethodInfo(method, &m) )
  { prolog_call_data *pcd = m.handle;

    if ( !pcd->functor )
    { m.flags = 0;

      pceGetMethodInfo(method, &m);
    
      pcd->functor = PL_new_functor(nameToAtom(m.name), m.argc);
      pcd->argc    = m.argc;
    }

    pcd->flags = m.flags;

    return pcd;
  }

  return NULL;
}


static void
put_trace_info(term_t id, prolog_call_data *pm)
{ term_t a = PL_new_term_ref();
  functor_t f;

  _PL_put_atomic(a, pm->method_id);
  if ( (pm->flags & PCE_METHOD_INFO_BREAK) )
    f = FUNCTOR_spy1;
  else /*if ( (pm->flags & PCE_METHOD_INFO_TRACE) )*/
    f = FUNCTOR_trace1;

  PL_cons_functor(id, f, a);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Call a Prolog-defined implementation from  XPCE.   Note  that send/2 and
get/3 route methods defined in  Prolog   directly  back  to Prolog. This
definition only comes into action if something   in  XPCE calls a method
defined in Prolog.

NOTE: if the return-type is accepts  prolog, we return a term-reference.
Is this ok? Who is ensuring the consistency?   Should we throw it out of
the context immediately? It is returned to  C-code from inside XPCE. Who
says me what happens to it?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
PrologCall(PceGoal goal)
{ prolog_call_data *pcd;

  if ( (pcd = get_pcd(goal->implementation)) )
  { fid_t  fid = PL_open_foreign_frame();
    term_t av  = PL_new_term_refs(4);
    term_t mav = PL_new_term_refs(pcd->argc);
    int rval, n;

					/* push method identifier */
    if ( (pcd->flags & (PCE_METHOD_INFO_TRACE|PCE_METHOD_INFO_BREAK)) )
      put_trace_info(av+0, pcd);
    else
      _PL_put_atomic(av+0, pcd->method_id);

    for(n=0; n<goal->argc; n++)		/* push normal arguments */
      put_object(mav+n, goal->argv[n]);
    if ( goal->va_argc >= 0 )		/* push varargs excess args in list */
    { term_t l = mav+n;
      term_t tmp = PL_new_term_ref();

      PL_put_nil(l);
      for(n=goal->va_argc; --n >= 0; )
      { put_object(tmp, goal->va_argv[n]);
	PL_cons_list(l, tmp, l);
      }
    }
    PL_cons_functor_v(av+1, pcd->functor, mav);
					/* push @receiver */
    put_object(av+2, goal->receiver);

    if ( goal->flags & PCE_GF_SEND )
      rval = PL_call_predicate(MODULE_user, DebugMode,
			       PREDICATE_send_implementation, av);
    else
    { rval = PL_call_predicate(MODULE_user, DebugMode,
			       PREDICATE_get_implementation, av);
      if ( rval )
      { if ( !get_answer_object(goal, av+3, goal->return_type, &goal->rval) )
	{ pceReportErrorGoal(goal);
	  rval = PCE_FAIL;
	}
      }
    }

    PL_discard_foreign_frame(fid);

    return rval;
  }

  return PCE_FAIL;
}


static PceObject
getPrologContext(PceObject receiver)
{ if ( receiver == PROLOG )
  { atom_t mname;

    if ( DefaultModule )
    { mname = ModuleName(DefaultModule);

      return atomToName(mname);
    }
    
    return NAME_user;
  }
  
  return NIL;
}


static PceObject
setPrologContext(PceObject context)
{ PceObject old = DefaultContext;

  DefaultContext = context;
  
  return old;
}


static Module
pceContextModule()
{ Atom mname;

  if ( DefaultContext && (mname = nameToAtom(DefaultContext)) )
    return ModuleFromAtom(mname);

  return MODULE_user;
}


		 /*******************************
		 *	 STREAM CONNECTION	*
		 *******************************/

#ifdef SWI
#define fdFromHandle(h) ((int)((long)(h)))

static int
Swrite_pce(void *handle, char *buf, int size)
{ return pceWrite(fdFromHandle(handle), buf, size);
}


static int
Sread_pce(void *handle, char *buf, int size)
{ return pceRead(fdFromHandle(handle), buf, size);
}


static long
Sseek_pce(void *handle, long offset, int whence)
{ return pceSeek(fdFromHandle(handle), offset, whence);
}


static int
Sclose_pce(void *handle)
{ return pceClose(fdFromHandle(handle));
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

  if ( (obj = termToReceiver(t)) )
  { int flags, sflags = SIO_LBUF|SIO_RECORDPOS;
    int handle;
    Atom m;

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
      } else if ( m == ATOM_update )
      { flags = PCE_WRONLY;
	sflags |= SIO_OUTPUT;
      } else
	goto domain_error;
    } else
    { domain_error:
      
      return ThrowException(EX_DOMAIN, ATOM_io_mode, mode);
    }

    if ( (handle = pceOpen(obj, flags)) >= 0 )
    { IOSTREAM *s = Snew((void *)(long)handle, sflags, &pceFunctions);

      return PL_open_stream(plhandle, s);
    } else
    { Atom a = AtomFromString(pceOsError());
    
      return ThrowException(EX_PERMISSION,
			    ATOM_open, ATOM_object, obj,
			    a);
    }
  } 

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
#define TIMEOUT 250			/* just wait */
#endif

static int
pce_dispatch(int fd)
{ if ( pceDispatch(fd, TIMEOUT) == PCE_DISPATCH_INPUT )
    return PROLOG_DISPATCH_INPUT;

  return PROLOG_DISPATCH_TIMEOUT;
}

#ifdef SICSTUS

#ifdef __WIN32__

void *
pl_malloc(unsigned int size)
{ return SP_malloc(size);
}


void *
pl_realloc(void *ptr, unsigned int size)
{ return SP_realloc(ptr, size);
}


void
pl_free(void *ptr)
{ SP_free(ptr);
}

#else /* __WIN32__ */

#define pl_malloc SP_malloc
#define pl_realloc SP_realloc
#define pl_free SP_free

#endif /*__WIN32__*/


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
pl_Cflush(void)
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
    case HOST_BACKTRACE:
    case HOST_ATEXIT:
    default:
      return PCE_FAIL;
  }
}


static int
PrologQuery(int what, PceCValue *value)
{ switch(what)
  { 
#ifdef _CONSOLE_H_INCLUDED		/* Win32 console */
    case HOST_CONSOLE:
      if ( (value->pointer = indirect_rlc_hwnd()) )
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
pl_Cflush(void)
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
    case HOST_CHECK_INTERRUPT:
      PL_handle_signals();
      return PCE_SUCCEED;
    case HOST_BACKTRACE:
    { int frames = va_arg(args, int);
      PL_action(PL_ACTION_BACKTRACE, (void *) (long)frames);
      return PCE_SUCCEED;
    }
    case HOST_ATEXIT:
    { OnExitFunction f = va_arg(args, OnExitFunction);

      PL_on_halt((halthandler_t)f, NULL);

      return PCE_SUCCEED;
    }
    default:
      return PCE_FAIL;
  }
}


static int
PrologQuery(int what, PceCValue *value)
{ switch(what)
  { 
#ifdef _CONSOLE_H_INCLUDED		/* Win32 console */
    case HOST_CONSOLE:
      if ( (value->pointer = indirect_rlc_hwnd()) )
	return PCE_SUCCEED;
      return PCE_FAIL;
#endif
    default:
      return PCE_FAIL;
  }
}

#endif /*SWI*/

		 /*******************************
		 *	     RESOURCE		*
		 *******************************/

static IOSTREAM *
PrologOpenResource(const char *name, const char *rc_class, const char *mode)
{ return PL_open_resource(pceContextModule(), name, rc_class, mode);
}

		 /*******************************
		 *	     TRANSLATE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translate a Prolog term. This can be  any   term  that is not handled by
get_object_arg(): a compound, string or variable.   If it is a compound,
it may be new(class) or new(Ref, Class), as well as a list. 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static PceObject
PrologTranslate(PceObject hd, PceObject type)
{ term_t t;

  if ( (t = getTermHandle(hd)) )
    return termToObject(t, type, NULLATOM, FALSE);

  assert(0);
  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Called by writeGoal() to print the arguments  of the goal handled by the
host-language. The non-vararg arguments are in g->argv[], represented as
Prolog terms.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
PrologWriteGoalArgs(PceGoal g)
{ int i, argn = 0;
  Term l;

  for(i=0; i<g->argc; i++)
  { if ( argn++ )
      Sprintf(", ");
    if ( g->argv[i] )
      PL_write_term(Soutput, (Term)g->argv[i], 999, PL_WRT_PORTRAY);
    else
      Sprintf("(nil)");
  }

  if ( g->va_type && (l = (Term)g->host_closure) )
  { Term tail = PL_copy_term_ref(l);
    Term head = NewTerm();

    while( PL_get_list(tail, head, tail) )
    { if ( argn++ )
	Sprintf(", ");
      PL_write_term(Soutput, head, 999, PL_WRT_PORTRAY);
    }
  }
    
  return TRUE;
}



		 /*******************************
		 *	  SETUP CALLBACK	*
		 *******************************/

static pce_callback_functions callbackfunction =
{ PrologSend,
  PrologGet,
  PrologCall,
  PrologQuery,
  PrologAction,
  pl_Cvprintf,				/* Console output */
  pl_Cputchar,
  pl_Cflush,
  pl_Cgetline,
  pl_malloc,				/* Prolog memory management hooks */
  pl_realloc,
  pl_free,
  PrologOpenResource,			/* resource handling */
  getPrologContext,			/* (Module) context */
  setPrologContext,
  PrologTranslate,
  PrologWriteGoalArgs
};


static void
registerPredicates()
{ InstallPredicate("send",		2, pl_send,		META);
  InstallPredicate("get",		3, pl_get,		META);
  InstallPredicate("send_class",	3, pl_send_class,	META);
  InstallPredicate("get_class",		4, pl_get_class,	META);
  InstallPredicate("object",		1, pl_object1,		0);
  InstallPredicate("object",		2, pl_object2,		0);
  InstallPredicate("new",		2, pl_new,		META);
  InstallPredicate("pce_method_implementation", 2,
		   pl_pce_method_implementation, 0);
  InstallPredicate("pce_open",		3, pl_pce_open, 	0);
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


static void
do_reset(void)
{ pceReset();				/* reset XPCE itself */
  rewindHostHandles(NULL);		/* invalidate them all */
}


#ifdef WIN32
static void
do_redraw(void)
{ pceRedraw(FALSE);
}
#endif


foreign_t
pl_pce_init(Term a)
{ char **argv;
  int argc;
  const char *home;
  Atom ahome;
  static int initialised = 0;
  
  if ( GetAtom(a, &ahome) )
    home = AtomCharp(ahome);
  else
    home = NULL;

  argc = PROLOG_ARGC();
  argv = PROLOG_ARGV();

  if ( !initialised++ )
  { PceObject plname;

    PROLOG_INSTALL_REINIT_FUNCTION(pl_pce_init);
    PROLOG_ITF_INIT();

    pceRegisterCallbacks(&callbackfunction);
    initNameAtomTable();
    if ( !pceInitialise(0, home, argc, argv) )
      return FALSE;

    initPceConstants();			/* get code used PCE constants */  
    initPrologConstants();		/* Public prolog constants */
    initHostConstants();		/* Host-specific Prolog constants */
    registerPredicates();		/* make the interface known */

    plname = cToPceName("prolog");
    pceSend(PROLOG, NULL, cToPceName("name_reference"), 1, &plname);
    PROLOG_INSTALL_DISPATCH_FUNCTION(pce_dispatch);
    PROLOG_INSTALL_RESET_FUNCTION(do_reset);
    PROLOG_INSTALL_REDRAW_FUNCTION(do_redraw);
  }

  return TRUE;
}
