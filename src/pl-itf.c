/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: foreign language interface
*/

#include "pl-incl.h"
#include "pl-itf.h"

#undef term				/* get rid of compatibility stuff */
#undef atomic
#undef functor
#undef module

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This modules defines  the  functions  available   to  users  of  foreign
language code. Most of this module just is a small function layer around
primitives, normally provided via macros. This module is not responsible
for loading foreign language code (see  pl-load.c). Note that on systems
on which pl-load.c is not portable, one   can still use these functions.
Link runtime/<arch>/pl.o to a modified version   of pl-extend.c and your
.o files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		/********************************
		*           ANALYSIS            *
		*********************************/

__pl_constf int
PL_is_var(const Word t)
{ return isVar(*t);
}

__pl_constf int
PL_is_int(const Word t)
{ return isInteger(*t);
}

__pl_constf int
PL_is_atom(const Word t)
{ return isAtom(*t);
}

__pl_constf int
PL_is_float(const Word t)
{ return isReal(*t);
}

__pl_constf int
PL_is_string(const Word t)
{ return isString(*t);
}

__pl_constf int
PL_is_term(const Word t)
{ return isTerm(*t);
}

__pl_constf int
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

__pl_constf long
PL_integer_value(const word t)
{ return valNum(t);
}

char * __pl_constf
PL_atom_value(const atomic_t t)
{ return stringAtom((Atom) t);
}

__pl_constf functor_t
PL_functor(const Word t)
{ return isTerm(*t) ? (functor_t) functorTerm(*t) : (functor_t) NULL;
}

__pl_constf atomic_t
PL_functor_name(const FunctorDef f)
{ return (atomic_t) f->name;
}

__pl_constf int
PL_functor_arity(const FunctorDef f)
{ return f->arity;
}

term_t __pl_constf
PL_arg(const term_t t, int n)
{ Word a = argTermP(*(Word)t, n-1);

  deRef(a);

  return (term_t) a;
}


term_t *
PL_univg(const term_t t, atomic_t *name, int *size, term_t *argv)
{ if ( isTerm(*t) )
  { FunctorDef f = functorTerm(*t);
    int arity;

    *name = (atomic_t) f->name;
    arity = f->arity;

    if ( arity <= *size )
    { Word a0;
      Word *av;

    ok:
      av = argv;
      a0 = argTermP(*(Word)t, 0);
      *size = arity;
      for( ; --arity >= 0; a0++, av++)
	deRef2(a0, *av);

      return argv;
    }
    if ( argv == NULL )
    { argv = malloc(sizeof(term_t) * arity);
      goto ok;
    }

    *size = arity;
    return NULL;
  } else if ( isAtom(*t) )
  { *name = *t;
    *size = 0;

    return argv;
  }

  *name = (atomic_t) NULL;
  return NULL;
}


__pl_constf term_t
PL_strip_module(const term_t t, Module *m)
{ return (term_t) stripModule(t, m);
}

		/********************************
		*         CONSTRUCTION          *
		*********************************/

term_t
PL_new_term()
{ register Word var = allocGlobal(1);

  setVar(*var);
  return var;
}


term_t
PL_term(atomic_t a)
{ register Word t = allocGlobal(1);

  *t = a;
  return t;
}


void
PL_term_vector(int size, term_t *t, atomic_t *a)
{ Word b = allocGlobal(size);

  while(--size >= 0)
  { *b = *a++;
    *t++ = b++;
  }
}


atomic_t
PL_new_atom(char *s)
{ return (atomic_t) lookupAtom(s);
}


atomic_t
PL_new_integer(int i)
{ if ( i < PLMININT || i > PLMAXINT )
    return (atomic_t) globalReal((real) i);
  else
    return (atomic_t) consNum(i);
}


#if O_STRING
atomic_t
PL_new_string(char *s)
{ return (atomic_t) globalString(s);
}
#endif /* O_STRING */


atomic_t
PL_new_var()
{ return (atomic_t) NULL;
}


atomic_t
PL_new_compound(functor_t def, atomic_t *args)
{ int arity = def->arity;
  Functor f = allocGlobal(1 + arity);
  Word a;

  f->definition = def;
  for(a = argTermP(f, 0); arity > 0; a++, arity--)
    *a = *args++;

  return (atomic_t) f;
}


atomic_t
PL_new_float(double f)
{ return (atomic_t) globalReal(f);
}


functor_t
PL_new_functor(atomic_t f,  int a)
{ return (functor_t) lookupFunctorDef((Atom)f, a);
}


bool
PL_unify(Word t1,  Word t2)
{ return (bool) pl_unify(t1, t2);
}


bool
PL_unify_atomic(Word t,  word w)
{ return unifyAtomic(t, w);
}


bool
PL_unify_functor(Word t,  FunctorDef f)
{ return unifyFunctor(t, f);
}

		 /*******************************
		 *QUINTUS STYLE WRAPPER SUPPORT *
		 *******************************/

bool
PL_cvt_i_integer(Word p, long *c)
{ if ( isInteger(*p) )
  { *c = valNum(*p);
    succeed;
  } else
    fail;
}


bool
PL_cvt_i_float(Word p, double *c)
{ if ( isReal(*p) )
  { *c = valReal(*p);
    succeed;
  } else if ( isInteger(*p) )
  { *c = (double) valNum(*p);
    succeed;
  } else
    fail;
}


bool
PL_cvt_i_single(Word p, float *c)
{ if ( isReal(*p) )
  { *c = (float) valReal(*p);
    succeed;
  } else if ( isInteger(*p) )
  { *c = (float) valNum(*p);
    succeed;
  } else
    fail;
}


bool
PL_cvt_i_string(Word p, char **c)
{ if ( isAtom(*p) )
  { *c = stringAtom(*p);
    succeed;
  } else if ( isString(*p) )
  { *c = valString(*p);
    succeed;
  } else
    fail;
}


bool
PL_cvt_i_atom(Word p, atomic_t *c)
{ if ( isAtom(*p) )
  { *c = (atomic_t)(*p);
    succeed;
  } else
    fail;
}


bool
PL_cvt_o_integer(long c, Word p)
{ return unifyAtomic(p, consNum(c));
}


bool
PL_cvt_o_float(double c, Word p)
{ return unifyAtomic(p, globalReal(c));
}


bool
PL_cvt_o_single(float c, Word p)
{ return unifyAtomic(p, globalReal(c));
}


bool
PL_cvt_o_string(char *c, Word p)
{ return unifyAtomic(p, lookupAtom(c));
}


bool
PL_cvt_o_atom(atomic_t c, Word p)
{ return unifyAtomic(p, c);
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

void *
PL_foreign_context_address(long h)
{ return ForeignContextAddress(h);
}


int
PL_foreign_control(long int h)
{ return ForeignControl(h);
}


		/********************************
		*      REGISTERING FOREIGNS     *
		*********************************/

bool
PL_register_foreign(char *name, int arity, Func f, int flags)
{ SourceFile sf;
  Procedure proc;
  Definition def;
  Module m;
  Word t = newTerm();
  FunctorDef fdef = lookupFunctorDef(lookupAtom(name), arity);

  pl_seeing(t);
  deRef(t);
  sf = lookupSourceFile((Atom) *t);

  m = (environment_frame ? contextModule(environment_frame)
			 : MODULE_system);

  proc = lookupProcedure(lookupFunctorDef(lookupAtom(name), arity), m);
  def = proc->definition;

  if ( true(def, LOCKED) )
  { warning("PL_register_foreign(): Attempt to redefine a system predicate: %s",
	    procedureName(proc));
    fail;
  }

  if ( def->definition.function )
    warning("PL_register_foreign(): redefined %s", procedureName(proc));
  if ( false(def, FOREIGN) && def->definition.clauses != NULL )
    abolishProcedure(proc, m);

  def->definition.function = f;
  def->indexPattern = 0;
  def->indexCardinality = 0;
  def->flags = 0;
  set(def, FOREIGN|TRACE_ME);
  clear(def, NONDETERMINISTIC);

  if ( (flags & PL_FA_NOTRACE) )	  clear(def, TRACE_ME);
  if ( (flags & PL_FA_TRANSPARENT) )	  set(def, TRANSPARENT);
  if ( (flags & PL_FA_NONDETERMINISTIC) ) set(def, NONDETERMINISTIC);
  if ( (flags & PL_FA_GCSAFE) )           set(def, GC_SAFE);

  if ( status.initialised )
  { word goal;
    mark b;

    Mark(b);
    goal = globalFunctor(FUNCTOR_dforeign_registered2);
    argTerm(goal, 0) = (word) m->name;
    unifyFunctor(argTermP(goal, 1), fdef);

    callGoal(MODULE_system, goal, TRUE);

    Undo(b);
  }

  succeed;
}  


bool
PL_load_extensions(PL_extension *ext)
{ PL_extension *e;
  Module m;

  m = (environment_frame ? contextModule(environment_frame)
			 : MODULE_system);

  for(e = ext; e->predicate_name; e++)
  { short flags = TRACE_ME;
    register Definition def;
    register Procedure proc;

    if ( e->flags & PL_FA_NOTRACE )	     flags &= ~TRACE_ME;
    if ( e->flags & PL_FA_TRANSPARENT )	     flags |= TRANSPARENT;
    if ( e->flags & PL_FA_NONDETERMINISTIC ) flags |= NONDETERMINISTIC;
    if ( e->flags & PL_FA_GCSAFE )	     flags |= GC_SAFE;

    proc = lookupProcedure(lookupFunctorDef(lookupAtom(e->predicate_name),
					    e->arity), 
			   m);
    def = proc->definition;
    if ( true(def, LOCKED) )
    { warning("PL_load_extensions(): Attempt to redefine system predicate: %s",
	      procedureName(proc));
      continue;
    }
    if ( def->definition.function )
      warning("PL_load_extensions(): redefined %s", procedureName(proc));
    if ( false(def, FOREIGN) && def->definition.clauses != NULL )
      abolishProcedure(proc, m);
    set(def, FOREIGN);
    set(def, flags);
    def->definition.function = e->function;
    def->indexPattern = 0;
    def->indexCardinality = 0;
  }    

  succeed;
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

		 /*******************************
		 *	 EMBEDDING PROLOG	*
		 *******************************/

int
PL_toplevel(void)
{ return prolog(PL_new_atom("$toplevel"));
}


void
PL_halt(int status)
{ Halt(status);
}

		/********************************
		*            MODULES            *
		*********************************/

module_t
PL_context()
{ return (module_t) environment_frame ? contextModule(environment_frame)
				      : MODULE_user;
}

atomic_t
PL_module_name(register Module m)
{ return (atomic_t) m->name;
}

module_t
PL_new_module(register atomic_t name)
{ return (module_t) lookupModule((Atom) name);
}

		 /*******************************
		 *	    PREDICATES		*
		 *******************************/

predicate_t
PL_predicate(functor_t functor, module_t module)
{ if ( module == NULL )
    module = PL_context();

  return (predicate_t)lookupProcedure((FunctorDef)functor, (Module)module);
}


int
PL_predicate_arity(predicate_t pred)
{ return pred->definition->functor->arity;
}


atomic_t
PL_predicate_name(predicate_t pred)
{ return (atomic_t) pred->definition->functor->name;
}


functor_t
PL_predicate_functor(predicate_t pred)
{ return (functor_t) pred->definition->functor;
}


module_t
PL_predicate_module(predicate_t pred)
{ return (module_t) pred->definition->module;
}



		/********************************
		*            SIGNALS            *
		*********************************/

#if HAVE_SIGNAL
void
(*PL_signal(int sig, void (*func) (int)))(int)
{ void (*old)(int);

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
{ AbortHandle	  next;			/* Next handle */
  PL_abort_hook_t function;		/* The handle itself */
} * abort_head = NULL,
  * abort_tail = NULL;


void
PL_abort_hook(PL_abort_hook_t func)
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


int
PL_abort_unhook(PL_abort_hook_t func)
{ AbortHandle h = abort_head;

  for(; h; h = h->next)
  { if ( h->function == func )
    { h->function = NULL;
      return TRUE;
    }
  }

  return FALSE;
}


void
resetForeign(void)
{ AbortHandle h = abort_head;

  for(; h; h = h->next)
    if ( h->function )
      (*h->function)();
}


		/********************************
		*      REINITIALISE (SAVE)	*
		********************************/

typedef struct reinit_handle * ReinitHandle;

static struct reinit_handle
{ ReinitHandle	  next;			/* Next handle */
  PL_reinit_hook_t function;		/* The handle itself */
} * reinit_head = NULL,
  * reinit_tail = NULL;


void
PL_reinit_hook(PL_reinit_hook_t func)
{ ReinitHandle h = reinit_head;

  for(; h; h = h->next)
  { if ( h->function == func )
      return;				/* already there */
  }

  h = (ReinitHandle) allocHeap(sizeof(struct reinit_handle));

  h->next = NULL;
  h->function = func;

  if ( reinit_head == NULL )
  { reinit_head = reinit_tail = h;
  } else
  { reinit_tail->next = h;
    reinit_tail = h;
  }
}


int
PL_reinit_unhook(PL_reinit_hook_t func)
{ ReinitHandle h = reinit_head;

  for(; h; h = h->next)
  { if ( h->function == func )
    { h->function = NULL;
      return TRUE;
    }
  }

  return FALSE;
}


void
reinitForeign(int argc, char **argv)
{ ReinitHandle h = reinit_head;

  for(; h; h = h->next)
    (*h->function)(argc, argv);
}


		 /*******************************
		 *	      PROMPT		*
		 *******************************/

void
PL_prompt1(const char *s)
{ prompt1((char *) s);
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
#ifdef O_DEBUGGER
      backTrace(environment_frame, (int) arg);
      succeed;
#else
      warning("No Prolog backtrace in runtime version");
      fail;
#endif
    case PL_ACTION_BREAK:
      return (bool) pl_break();
    case PL_ACTION_HALT:
      Halt((int) arg);
      fail;				/* should not happen */
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
      return (long) Sgetchar();		/* normal reading */
    default:
      sysError("PL_query: Illegal query: %d", query);
      /*NOTREACHED*/
      fail;
  }
}
