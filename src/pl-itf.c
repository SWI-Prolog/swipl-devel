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

constf int
PL_is_var(const Word t)
{ return isVar(*t);
}

constf int
PL_is_int(const Word t)
{ return isInteger(*t);
}

constf int
PL_is_atom(const Word t)
{ return isAtom(*t);
}

constf int
PL_is_float(const Word t)
{ return isReal(*t);
}

constf int
PL_is_string(const Word t)
{ return isString(*t);
}

constf int
PL_is_term(const Word t)
{ return isTerm(*t);
}

constf int
PL_type(const Word t)
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
PL_float_value(const word t)
{ return valReal(t);
}

#if O_STRING
char *
PL_string_value(const word t)
{ return valString(t);
}
#endif /* O_STRING */

char *
PL_list_string_value(const Word t)
{ Word t2 = t;

  deRef(t2);
  return listToString(*t2);
}

constf long
PL_integer_value(const word t)
{ return valNum(t);
}

char * constf
PL_atom_value(const atomic t)
{ return stringAtom((Atom) t);
}

constf functor
PL_functor(const Word t)
{ return isTerm(*t) ? (functor) functorTerm(*t) : (functor) NULL;
}

constf atomic
PL_functor_name(const FunctorDef f)
{ return (atomic) f->name;
}

constf int
PL_functor_arity(const FunctorDef f)
{ return f->arity;
}

term constf
PL_arg(const term t, int n)
{ Word a = argTermP(*(Word)t, n-1);

  deRef(a);

  return (term) a;
}

constf term
PL_strip_module(const term t, Module *m)
{ return (term) stripModule(t, m);
}

		/********************************
		*         CONSTRUCTION          *
		*********************************/

term
PL_new_term(void)
{ register Word var = allocGlobal(sizeof(word));

  setVar(*var);
  return var;
}

atomic
PL_new_atom(char *s)
{ return (atomic) lookupAtom(s);
}

atomic
PL_new_integer(int i)
{ return (atomic) consNum(i);
}

#if O_STRING
atomic
PL_new_string(char *s)
{ return (atomic) globalString(s);
}
#endif /* O_STRING */

atomic
PL_new_float(double f)
{ return (atomic) globalReal(f);
}

functor
PL_new_functor( atomic f,  int a)
{ return (functor) lookupFunctorDef((Atom)f, a);
}

bool
PL_unify( Word t1,  Word t2)
{ return (bool) pl_unify(t1, t2);
}

bool
PL_unify_atomic( Word t,  word w)
{ return unifyAtomic(t, w);
}

bool
PL_unify_functor( Word t,  FunctorDef f)
{ return unifyFunctor(t, f);
}

		/********************************
		*   UNDETERMINISTIC FOREIGNS    *
		********************************/

foreign_t
_PL_retry(long int v)
{ ForeignRedo(v);
}


long
PL_foreign_context(long int h)
{ return ForeignContext(h);
}

#ifdef __STDC__
void *
#else
char *
#endif
PL_foreign_context_address(long int h)
{ return ForeignContextAddress(h);
}


int
PL_foreign_control(long int h)
{ return ForeignControl(h);
}


		/********************************
		*      REGISTERING FOREIGNS     *
		*********************************/

static bool registerForeign(char *, int, Func, va_list);

static bool
registerForeign(char *name, int arity, Func f, va_list args)
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
      case PL_FA_GCSAVE:	   set(def, GC_SAVE);		break;
      default:
	return warning("PL_register_foreign(): %s: bad argument",
		       procedureName(proc));
    }
    if ( n > 4 )
      return warning("PL_register_foreign(): %s: argument list not closed",
		     procedureName(proc));
  }

  succeed;
}  


bool
PL_register_foreign(char *name, int arity, Func f, ...)
{ va_list args;
  bool rval;

  va_start(args, f);
  rval = registerForeign(name, arity, f, args);
  va_end(args);

  return rval;
}


		/********************************
		*        CALLING PROLOG         *
		*********************************/

void
PL_mark(register bktrk_buf *buf)
{ Mark(*((mark *)buf));
}


void
PL_bktrk(register bktrk_buf *buf)
{ Undo(*((mark *)buf));
}


bool
PL_call(Word t, Module m)
{ deRef(t);

  return callGoal(m, *t, TRUE);
}  


void
_PL_lock(Word *t)
{ lockp(t);
}


void
_PL_unlock(Word *t)
{ unlockp(t);
}

		/********************************
		*            MODULES            *
		*********************************/

module
PL_context()
{ return (module) contextModule(environment_frame);
}

atomic
PL_module_name(register Module m)
{ return (atomic) m->name;
}

module
PL_new_module(register atomic name)
{ return (module) lookupModule((Atom) name);
}

		/********************************
		*            SIGNALS            *
		*********************************/

#if HAVE_SIGNAL
void
(*PL_signal(int sig, void (*func) (/* ??? */)))(/* ??? */)
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
PL_abort_handle(void (*func) (/* ??? */))
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
resetForeign(void)
{ AbortHandle h = abort_head;

  for(; h; h = h->next)
    (*h->function)();
}


		/********************************
		*           WARNINGS            *
		*********************************/

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


		/********************************
		*            ACTIONS            *
		*********************************/

bool
PL_action(int action, void *arg)
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
PL_query(int query)
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
