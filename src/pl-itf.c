/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: foreign language interface
*/

#include "pl-incl.h"
#include "pl-itf.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This modules  defines  the  functions  available  to  users  of  foreign
language  code.   Most  of  this  module  just is a small function layer
around primitives, normally provided via macros.   This  module  is  not
responsible for loading foreign language code (see pl-load.c). Note that
on  systems  on which pl-load.c is not portable, one can still use these
functions, link the .o files while linking prolog and call  the  foreign
module's initialisation function from main() in pl-main.c.  PCE normally
is linked this way.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		/********************************
		*           ANALYSIS            *
		*********************************/

int
PL_is_var(t)
register Word t;
{ return isVar(*t);
}

int
PL_is_int(t)
register Word t;
{ return isInteger(*t);
}

int
PL_is_atom(t)
register Word t;
{ return isAtom(*t);
}

int
PL_is_float(t)
register Word t;
{ return isReal(*t);
}

int
PL_is_string(t)
register Word t;
{ return isString(*t);
}

int
PL_is_term(t)
register Word t;
{ return isTerm(*t);
}

int
PL_type(t)
register Word t;
{ if ( isVar(*t) )		return PL_VARIABLE;
  if ( isInteger(*t) )		return PL_INTEGER;
  if ( isReal(*t) )		return PL_FLOAT;
#if O_STRING
  if ( isString(*t) )		return PL_STRING;
#endif /* O_STRING */
  if ( isAtom(*t) )		return PL_ATOM;
  if ( isTerm(*t) )		return PL_TERM;

  return sysError("PL_type(): illegal type");
}

double					/* double for standard arg passing */
PL_float_value(t)
register word t;
{ return valReal(t);
}

#if O_STRING
char *
PL_string_value(t)
register word t;
{ return valString(t);
}
#endif /* O_STRING */

char *
PL_list_string_value(t)
register Word t;
{ deRef(t);
  return listToString(*t);
}

long
PL_integer_value(t)
register word t;
{ return valNum(t);
}

char *
PL_atom_value(t)
register word t;
{ return stringAtom(t);
}

functor
PL_functor(t)
register Word t;
{ return isTerm(*t) ? (functor) functorTerm(*t) : (functor) NULL;
}

atomic
PL_functor_name(f)
register FunctorDef f;
{ return (atomic) f->name;
}

int
PL_functor_arity(f)
register FunctorDef f;
{ return f->arity;
}

term
PL_arg(t, n)
register Word t;
register int n;
{ register Word a = argTermP(*t, n-1);

  deRef(a);

  return (term) a;
}

term
PL_strip_module(t, m)
term t;
Module *m;
{ return (term) stripModule(t, m);
}

		/********************************
		*         CONSTRUCTION          *
		*********************************/

term
PL_new_term()
{ register Word var = allocGlobal(sizeof(word));

  setVar(*var);
  return var;
}

atomic
PL_new_atom(s)
char *s;
{ return (atomic) lookupAtom(s);
}

atomic
PL_new_integer(i)
int i;
{ return (atomic) consNum(i);
}

#if O_STRING
atomic
PL_new_string(s)
char *s;
{ return (atomic) globalString(s);
}
#endif /* O_STRING */

atomic
PL_new_float(f)
double f;
{ return (atomic) globalReal(f);
}

functor
PL_new_functor(f, a)
register atomic f;
register int a;
{ return (functor) lookupFunctorDef((Atom)f, a);
}

bool
PL_unify(t1, t2)
register Word t1, t2;
{ return (bool) pl_unify(t1, t2);
}

bool
PL_unify_atomic(t, w)
register Word t;
register word w;
{ return unifyAtomic(t, w);
}

bool
PL_unify_functor(t, f)
register Word t;
register FunctorDef f;
{ return unifyFunctor(t, f);
}

		/********************************
		*   UNDETERMINISTIC FOREIGNS    *
		********************************/

foreign_t
_PL_retry(v)
long v;
{ ForeignRedo(v);
}


long
PL_foreign_context(h)
long h;
{ return ForeignContext(h);
}

#ifdef __STDC__
void *
#else
char *
#endif
PL_foreign_context_address(h)
long h;
{ return ForeignContextAddress(h);
}


int
PL_foreign_control(h)
long h;
{ return ForeignControl(h);
}


		/********************************
		*      REGISTERING FOREIGNS     *
		*********************************/

static bool registerForeign P((char *, int, Func, va_list));

static bool
registerForeign(name, arity, f, args)
char *name;
int arity;
Func f;
va_list args;
{ SourceFile sf;
  Procedure proc;
  Definition def;
  int n;
  Module m;
  int attribute;
  Word t = newTerm();

  pl_seeing(t);
  deRef(t);
  sf = lookupSourceFile((Atom) *t);

  m = (environment_frame ? contextModule(environment_frame)
			 : MODULE_system);

  proc = lookupProcedure(lookupFunctorDef(lookupAtom(name), arity), m);
  def = proc->definition;

  if ( true(def, SYSTEM) )
    return warning("PL_register_foreign(): Attempt to redefine a system predicate: %s",
		   procedureName(proc));
  if ( def->source != (SourceFile) NULL && def->source != sf )
    warning("PL_register_foreign(): redefined %s", procedureName(proc));
  def->source = sf;

  if ( false(def, FOREIGN) && def->definition.clauses != (Clause) NULL )
    abolishProcedure(proc, m);

  def->definition.function = f;
  def->indexPattern = 0;
  def->indexCardinality = 0;
  def->flags = 0;
  set(def, FOREIGN|TRACE_ME);
  clear(def, NONDETERMINISTIC);

  for(n=0; (attribute = va_arg(args, int)) != 0; n++ )
  { switch( attribute )
    { case PL_FA_NOTRACE:	   clear(def, TRACE_ME);	break;
      case PL_FA_TRANSPARENT:	   set(def, TRANSPARENT);	break;
      case PL_FA_NONDETERMINISTIC: set(def, NONDETERMINISTIC);	break;
    }
    if ( n > 3 )
      return warning("PL_register_foreign(): %s/%d: argument list not closed",
								name, arity);
  }

  succeed;
}  

#if ANSI && !AIX
bool
PL_register_foreign(char *name, int arity, Func f, ...)
{ va_list args;
  bool rval;

  va_start(args, f);
  rval = registerForeign(name, arity, f, args);
  va_end(args);

  return rval;
}

#else

bool
PL_register_foreign(va_alist)
va_dcl
{ va_list args;
  char *name;
  int arity;
  Func f;
  bool rval;

  va_start(args);
  name  = va_arg(args, char *);
  arity = va_arg(args, int);
  f     = va_arg(args, Func);
  rval = registerForeign(name, arity, f, args);
  va_end(args);

  return rval;
}
#endif

		/********************************
		*        CALLING PROLOG         *
		*********************************/

void
PL_mark(buf)
register bktrk_buf *buf;
{ Mark(*((mark *)buf));
}

void
PL_bktrk(buf)
register bktrk_buf *buf;
{ Undo(*((mark *)buf));
}

bool
PL_call(t, m)
Word t;
Module m;
{ LocalFrame lSave   = lTop;
  LocalFrame envSave = environment_frame;
  Word *     aSave   = aTop;
  bool	     rval;

  deRef(t);

  if ( m == (Module) NULL )
    m = contextModule(environment_frame);

  lTop = (LocalFrame) addPointer(lTop, sizeof(LocalFrame));
  verifyStack(local);
  varFrame(lTop, -1) = (word) environment_frame;

  gc_status.blocked++;
  rval = interpret(m, *t, TRUE);
  gc_status.blocked--;

  lTop		    = lSave;
  aTop		    = aSave;
  environment_frame = envSave;

  return rval;
}  

		/********************************
		*            MODULES            *
		*********************************/

module
PL_context()
{ return (module) contextModule(environment_frame);
}

atomic
PL_module_name(m)
register Module m;
{ return (atomic) m->name;
}

module
PL_new_module(name)
register atomic name;
{ return (module) lookupModule((Atom) name);
}

		/********************************
		*            SIGNALS            *
		*********************************/

#if unix || EMX
void
(*PL_signal(sig, func))()
int sig;
void (*func)();
{ void (*old)();

  if ( sig < 0 || sig >= MAXSIGNAL )
  { fatalError("PL_signal(): illegal signal number: %d", sig);
    return NULL;
  }

  if ( signalHandlers[sig].catched == FALSE )
  { old = signal(sig, func);
    signalHandlers[sig].os = func;
    
    return old;
  }

  old = signalHandlers[sig].user;
  signalHandlers[sig].user = func;

  return old;
}
#endif


		/********************************
		*         RESET (ABORTS)	*
		********************************/

typedef struct abort_handle * AbortHandle;

static struct abort_handle
{ AbortHandle	next;			/* Next handle */
  void		(*function)();		/* The handle itself */
} * abort_head = NULL,
  * abort_tail = NULL;


void
PL_abort_handle(func)
void (*func)();
{ AbortHandle h = (AbortHandle) allocHeap(sizeof(struct abort_handle));

  h->next = NULL;
  h->function = func;

  if ( abort_head == NULL )
  { abort_head = abort_tail = h;
  } else
  { abort_tail->next = h;
    abort_tail = h;
  }
}


void
resetForeign()
{ AbortHandle h = abort_head;

  for(; h; h = h->next)
    (*h->function)();
}


		/********************************
		*           WARNINGS            *
		*********************************/

#if ANSI && !AIX
bool
PL_warning(char *fm, ...)
{ va_list args;

  va_start(args, fm);
  vwarning(fm, args);
  va_end(args);

  fail;
}

void
PL_fatal_error(char *fm, ...)
{ va_list args;

  va_start(args, fm);
  vfatalError(fm, args);
  va_end(args);
}

#else

bool
PL_warning(va_alist)
va_dcl
{ char *fm;
  va_list args;

  va_start(args);
  fm = va_arg(args, char *);
  vwarning(fm, args);
  va_end(args);

  fail;
}

void
PL_fatal_error(va_alist)
va_dcl
{ char *fm;
  va_list args;

  va_start(args);
  fm = va_arg(args, char *);
  vfatalError(fm, args);
  va_end(args);
}
#endif /* ANSI */

		/********************************
		*            ACTIONS            *
		*********************************/

bool
PL_action(action, arg)
int action;
void * arg;
{ switch(action)
  { case PL_ACTION_TRACE:
      return (bool) pl_trace();
    case PL_ACTION_DEBUG:
      return (bool) pl_debug();
    case PL_ACTION_BACKTRACE:
      backTrace(environment_frame, (int) arg);
      succeed;
    case PL_ACTION_BREAK:
      return (bool) pl_break();
    case PL_ACTION_HALT:
      return (bool) pl_halt();
    case PL_ACTION_ABORT:
      return (bool) pl_abort();
    case PL_ACTION_SYMBOLFILE:
      loaderstatus.symbolfile = lookupAtom((char *) arg);
      succeed;
    case PL_ACTION_WRITE:
      Putf("%s", (char *)arg);
      succeed;
    case PL_ACTION_FLUSH:
      pl_flush();
      succeed;
    default:
      sysError("PL_action(): Illegal action: %d", action);
      /*NOTREACHED*/
      fail;
  }
}

		/********************************
		*         QUERY PROLOG          *
		*********************************/

static int c_argc = -1;
static char **c_argv;

static void
init_c_args()
{ if ( c_argc == -1 )
  { int i;

    c_argv = alloc_heap(mainArgc * sizeof(char *));
    c_argv[0] = mainArgv[0];
    c_argc = 1;

    for(i=1; i<mainArgc; i++)
    { if ( mainArgv[i][0] == '-' )
      { switch(mainArgv[i][1])
	{ case 'x':
	  case 'g':
	  case 'd':
	  case 'f':
	  case 't':
	    i++;
	    continue;
	  case 'B':
	  case 'L':
	  case 'G':
	  case 'O':
	  case 'T':
	  case 'A':
	  case 'P':
	    continue;
	}
      }
      c_argv[c_argc++] = mainArgv[i];
    }
  }
}


long
PL_query(query)
int query;
{ switch(query)
  { case PL_QUERY_ARGC:
      init_c_args();
      return (long) c_argc;
    case PL_QUERY_ARGV:
      init_c_args();
      return (long) c_argv;
    case PL_QUERY_SYMBOLFILE:
      if ( getSymbols() == FALSE )
	return (long) NULL;
      return (long) stringAtom(loaderstatus.symbolfile);
    case PL_QUERY_ORGSYMBOLFILE:
      if ( getSymbols() == FALSE )
	return (long) NULL;
      return (long) stringAtom(loaderstatus.orgsymbolfile);
    case PL_QUERY_GETC:
      PopTty(&ttytab);			/* restore terminal mode */
      return (long) getchar();		/* normal reading */
    default:
      sysError("PL_query: Illegal query: %d", query);
      /*NOTREACHED*/
      fail;
  }
}
