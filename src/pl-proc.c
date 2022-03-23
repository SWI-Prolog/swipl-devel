/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2022, University of Amsterdam
                              VU University Amsterdam
			      CWI, Amsterdam
			      SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/*#define O_DEBUG 1*/
#include "pl-proc.h"
#include "pl-comp.h"
#include "pl-dbref.h"
#include "pl-event.h"
#include "pl-tabling.h"
#include "pl-transaction.h"
#include "pl-util.h"
#include "pl-supervisor.h"
#include "pl-index.h"
#include "pl-srcfile.h"
#include "pl-pro.h"
#include "pl-modul.h"
#include "pl-wam.h"
#include "pl-fli.h"
#include "pl-gc.h"
#include "pl-funct.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
General  handling  of  procedures:  creation;  adding/removing  clauses;
finding source files, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#undef LD
#define LD LOCAL_LD

#if USE_LD_MACROS
#define	visibleProcedure(f, m)		LDFUNC(visibleProcedure, f, m)
#define	registerDirtyDefinition(def)	LDFUNC(registerDirtyDefinition, def)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

static void	resetProcedure(Procedure proc, bool isnew);
static atom_t	autoLoader(Definition def);
static Procedure visibleProcedure(functor_t f, Module m);
static void	freeClauseRef(ClauseRef cref);
static int	setDynamicDefinition_unlocked(Definition def, bool isdyn);
static void	registerDirtyDefinition(Definition def);
static void	unregisterDirtyDefinition(Definition def);
static gen_t	ddi_oldest_generation(DirtyDefInfo ddi);
static void	gcClauseRefs(void);

#undef LDFUNC_DECLARATIONS

#ifdef O_DEBUG
static int	isProtectedCRef(ClauseRef cref);
#else
#define unprotectCRef(cref) (void)0
#define protectCRef(cref)   (void)0
#endif

/* Enforcing this limit demands we propagate NULL from lookupProcedure()
   through the whole system.  This is not done
*/
#define O_PROGLIMIT_INCL_PRED 0
#define SIZEOF_PROC (sizeof(struct procedure) + sizeof(struct definition))

Procedure
lookupProcedure(functor_t f, Module m)
{ GET_LD
  Procedure proc, oproc;
  Definition def;

  if ( (proc = lookupHTable(m->procedures, (void *)f)) )
  { DEBUG(MSG_PROC, Sdprintf("lookupProcedure(%s) --> %s\n",
			     PL_atom_chars(m->name),
			     procedureName(proc)));
    return proc;
  }

#if O_PROGLIMIT_INCL_PRED
  if ( m->code_limit &&
       m->code_size + SIZEOF_PROC > m->code_limit )
  { PL_error(NULL, 0, NULL, ERR_RESOURCE, ATOM_program_space);
    return NULL;
  }
#endif

  proc = (Procedure)  allocHeapOrHalt(sizeof(struct procedure));
  def  = (Definition) allocHeapOrHalt(sizeof(struct definition));
  proc->definition = def;
  proc->flags      = 0;
  proc->source_no  = 0;

  memset(def, 0, sizeof(*def));
  def->functor = valueFunctor(f);
  def->module  = m;
#ifdef __SANITIZE_ADDRESS__
  def->name = strdup(predicateName(def));
  __lsan_ignore_object(def->name);
#endif
  def->shared  = 1;
  if ( def->functor->arity > 0 )
  { def->impl.any.args = allocHeapOrHalt(sizeof(arg_info)*def->functor->arity);
    memset(def->impl.any.args, 0, sizeof(arg_info)*def->functor->arity);
  } else
  { def->impl.any.args = NULL;
  }
  resetProcedure(proc, TRUE);

  DEBUG(MSG_PROC_COUNT, Sdprintf("Created %s at %p\n",
				 procedureName(proc), proc));
  DEBUG(MSG_PRED_COUNT, Sdprintf("Created %s at %p\n",
				 predicateName(def), def));
  ATOMIC_INC(&GD->statistics.predicates);
  ATOMIC_ADD(&m->code_size, SIZEOF_PROC);

  if ( (oproc=addHTable(m->procedures, (void *)f, proc)) == proc )
  { return proc;
  } else
  { unallocProcedure(proc);
    return oproc;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
lingerDefinition() deals with (undefined) definitions  that are replaced
due to importing. These definitions can be   in  use with other threads.
This needs to be improved, possibly using a technique similar to the RDF
database. For now, we merely collect them in  a single place, so we know
what is going on. In addition, we can collect lingering definitions when
destroying a module, resulting in leak-free temporary modules.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
lingerDefinition(Definition def)
{ ListCell c = allocHeapOrHalt(sizeof(*c));
  Module m = def->module;
  ListCell o;

  c->value     = def;
  do
  { o            = m->lingering;
    c->next      = o;
  } while( !COMPARE_AND_SWAP_PTR(&m->lingering, o, c) );

  DEBUG(MSG_PROC_COUNT, Sdprintf("Linger %s\n", predicateName(def)));
  ATOMIC_SUB(&m->code_size, sizeof(*def));

  /*GC_LINGER(def);*/
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destroyDefinition() is called to destroy predicates from destroyModule()
as well as destroying thread-local  instantiations   while  a  thread is
being terminated. In both  cases  is  the   predicate  known  to  be not
referenced.

However, we cannot simply discard  everything   as  the predicate may be
involved in clause-GC. Therefore we need to leave the entire cleaning to
clause-GC. This is somewhat slower than  the   old  way around. The good
news is the it works towards more   general garbage collection for code,
e.g., eventually we may be able  to   destroy  modules even if we cannot
guarantee they are not in use.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
destroyDefinition(Definition def)
{ if ( def->module )
    ATOMIC_SUB(&def->module->code_size, sizeof(*def));

  DEBUG(MSG_CGC_PRED,
	Sdprintf("destroyDefinition(%s)\n", predicateName(def)));

  freeCodesDefinition(def, FALSE);

  if ( false(def, P_FOREIGN|P_THREAD_LOCAL) )	/* normal Prolog predicate */
  { GET_LD

    deleteIndexesDefinition(def);
    removeClausesPredicate(def, 0, FALSE);
    if ( GD->cleaning != CLN_DATA )
    { registerDirtyDefinition(def);
      DEBUG(MSG_PRED_COUNT, Sdprintf("Erased %s at\n", predicateName(def), def));
      def->module = NULL;
      set(def, P_ERASED);
      return;
    } else
    { free_lingering(&def->lingering, GEN_MAX);
    }
  } else					/* foreign and thread-local */
  {
#ifdef O_PLMT
    DEBUG(MSG_PRED_COUNT, Sdprintf("Unalloc foreign/thread-local: %s\n",
				   predicateName(def)));
    if ( true(def, P_THREAD_LOCAL) )
      destroyLocalDefinitions(def);
#endif
  }

  if ( true(def, P_DIRTYREG) )
    unregisterDirtyDefinition(def);
  unallocDefinition(def);
}

/* Finish the job when P_ERASED is det in destroyDefinition() */

static void
delayedDestroyDefinition(Definition def)
{ DEBUG(MSG_PRED_COUNT, Sdprintf("Delayed unalloc %s at %p\n",
				 predicateName(def), def));
  assert(def->module == NULL);
  if ( def->impl.clauses.first_clause == NULL ||
       GD->cleaning == CLN_DATA )
  { if ( GD->cleaning == CLN_DATA )
      removeClausesPredicate(def, 0, FALSE);

    DEBUG(1,
	  if ( def->lingering )
	  { Sdprintf("maybeUnregisterDirtyDefinition(%s): lingering data\n",
		     predicateName(def));
	  });
    unregisterDirtyDefinition(def);
    deleteIndexes(&def->impl.clauses, TRUE);
    unallocDefinition(def);
  }
}


void
unallocDefinition(Definition def)
{ if ( def->tabling )
    freeHeap(def->tabling, sizeof(*def->tabling));
  if ( def->impl.any.args )
    freeHeap(def->impl.any.args, sizeof(arg_info)*def->functor->arity);
  if ( def->events )
    destroy_event_list(&def->events);
  freeCodesDefinition(def, FALSE);
  free_lingering(&def->lingering, GEN_MAX);

  DEBUG(MSG_PRED_COUNT, Sdprintf("Freed %s at %p\n", predicateName(def), def));
  freeHeap(def, sizeof(*def));
  ATOMIC_DEC(&GD->statistics.predicates);
}


void
unallocProcedure(Procedure proc)
{ Definition def = proc->definition;

  DEBUG(MSG_PROC_COUNT, Sdprintf("Freed procedure %s at %p\n",
				 predicateName(def), proc));

  if ( unshareDefinition(def) == 0 )
  { def->module = NULL;
    DEBUG(MSG_PROC, Sdprintf("Reclaiming %s\n", predicateName(def)));
    destroyDefinition(def);
  }
  if ( proc->source_no )
    releaseSourceFileNo(proc->source_no);
  freeHeap(proc, sizeof(*proc));
}


static void
free_ddi_symbol(void *name, void *value)
{ DirtyDefInfo ddi = value;
  Definition def = name;

  PL_free(ddi);
  if ( true(def, P_ERASED) )
    delayedDestroyDefinition(def);
}


void
initProcedures(void)
{ GD->procedures.dirty = newHTable(32);
  GD->procedures.dirty->free_symbol = free_ddi_symbol;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cleanupProcedures()  deals  with  the  cleanup    during   halt.  Normal
procedures are cleaned by cleanupModules(). This   deals mostly with the
allocations pending on clause GC.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
cleanupProcedures(void)
{ if ( GD->procedures.dirty )
  { destroyHTable(GD->procedures.dirty);
    GD->procedures.dirty = NULL;
  }

  gcClauseRefs();
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Add (import) a definition to a module.  Used by loadImport() for loading
states and QLF files. Must be merged with import/1.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
importDefinitionModule(Module m, Definition def, int flags)
{ GET_LD
  functor_t functor = def->functor->functor;
  Procedure proc;
  int rc = TRUE;

  LOCKMODULE(m);
  if ( (proc = lookupHTable(m->procedures, (void *)functor)) )
  { if ( proc->definition != def )
    { if ( !isDefinedProcedure(proc) )
      { Definition odef = proc->definition;

	shareDefinition(def);
	proc->definition = def;
	if ( unshareDefinition(odef) == 0 )
	  lingerDefinition(odef);
      } else
      { if ( !(flags&PROC_WEAK) )
	  rc = warning("Failed to import %s into %s",
		       predicateName(def), PL_atom_chars(m->name));
      }
    }
  } else
  { proc = (Procedure) allocHeapOrHalt(sizeof(struct procedure));
    shareDefinition(def);
    proc->definition = def;
    proc->flags      = flags;
    proc->source_no  = 0;
    addNewHTable(m->procedures, (void *)functor, proc);
    DEBUG(MSG_PROC_COUNT, Sdprintf("Created %s at %p\n",
				   procedureName(proc), proc));
  }
  UNLOCKMODULE(m);

  return rc;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
resetProcedure() is called  by  lookupProcedure()   for  new  ones,  and
abolishProcedure() by abolish/2.

There are two cases where a  complete  reset   is  safe:  if  this is an
unreferenced dynamic predicate and if this is   a  predicate that has no
clause-list. Such predicates can't be active  and can't become active as
that requires clauses which, even under  MT,   can  only  be added after
locking the L_PREDICATE mutex.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
resetProcedure(Procedure proc, bool isnew)
{ Definition def = proc->definition;

  if ( (true(def, P_DYNAMIC) /*&& def->references == 0*/) ||
       !def->impl.any.defined )
    isnew = TRUE;

  if ( true(def, P_DIRTYREG) )
    ATOMIC_SUB(&GD->clauses.dirty, def->impl.clauses.number_of_clauses);

  def->flags ^= def->flags & ~(SPY_ME|P_DIRTYREG);
  if ( stringAtom(def->functor->name)[0] != '$' )
    set(def, TRACE_ME);
  def->impl.clauses.number_of_clauses = 0;
  if ( def->events )
    destroy_event_list(&def->events);
  if ( def->tabling )
    tbl_reset_tabling_attributes(def);

  if ( isnew )
  { deleteIndexes(&def->impl.clauses, TRUE);
    freeCodesDefinition(def, FALSE);
  } else
    freeCodesDefinition(def, TRUE);	/* carefully sets to S_VIRGIN */
}


Procedure
isCurrentProcedure(DECL_LD functor_t f, Module m)
{ return lookupHTable(m->procedures, (void *)f);
}


ClauseRef
hasClausesDefinition(Definition def)
{ if ( false(def, P_FOREIGN|P_THREAD_LOCAL) &&
       def->impl.clauses.first_clause )
  { GET_LD
    ClauseRef c;
    gen_t generation = global_generation();

    acquire_def(def);
    for(c = def->impl.clauses.first_clause; c; c = c->next)
    { Clause cl = c->value.clause;

      if ( visibleClauseCNT(cl, generation) )
	break;
    }
    release_def(def);

    return c;
  }

  return NULL;
}


bool
isDefinedProcedure(Procedure proc)
{ Definition def = proc->definition;

  if ( true(def, PROC_DEFINED) )
    succeed;

  return hasClausesDefinition(def) ? TRUE : FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find a procedure for defining it.  Here   we check whether the procedure
to be defined is a system predicate.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Procedure
isStaticSystemProcedure(functor_t fd)
{ GET_LD
  Procedure proc;

  if ( !SYSTEM_MODE &&
       MODULE_system &&
       (proc=isCurrentProcedure(fd, MODULE_system)) &&
       true(proc->definition, P_LOCKED) &&
       false(proc->definition, P_DYNAMIC) )
    return proc;

  return NULL;
}


int
checkModifySystemProc(functor_t fd)
{ Procedure proc;

  if ( (proc = isStaticSystemProcedure(fd)) &&
       true(proc->definition, P_ISO) )
    return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, proc);

  succeed;
}


int
overruleImportedProcedure(Procedure proc, Module target)
{ GET_LD
  Definition def = proc->definition;	/* we do *not* want a thread-local version */

  if ( true(def, P_AUTOLOAD) )
    return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
		    ATOM_redefine, ATOM_imported_procedure, proc);

  if ( def->module == target )
    return TRUE;

  if ( true(def->module, M_SYSTEM) )
  { return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
		    ATOM_redefine, ATOM_built_in_procedure, proc);
  } else
  { if ( proc->flags & PROC_WEAK )
    { if ( truePrologFlag(PLFLAG_WARN_OVERRIDE_IMPLICIT_IMPORT) )
      { term_t pi;

	if ( !(pi=PL_new_term_ref()) ||
	     !PL_unify_predicate(pi, proc, GP_NAMEARITY) ||
	     !printMessage(ATOM_warning,
			   PL_FUNCTOR_CHARS, "ignored_weak_import", 2,
			     PL_ATOM, target->name,
			     PL_TERM, pi) )
	  return FALSE;
      }

      abolishProcedure(proc, target);
      return TRUE;
    }
  }

  return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
		  ATOM_redefine, ATOM_imported_procedure, proc);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
lookupProcedureToDefine() locates the proc for  a   functor  in a module
with the aim of providing a  definition   for  this  procedure, e.g., to
declare it as a meta-predicate, dynamic, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Procedure
lookupProcedureToDefine(functor_t def, Module m)
{ GET_LD
  Procedure proc;

  if ( (proc = isCurrentProcedure(def, m)) )
  { if ( !overruleImportedProcedure(proc, m) )
      return NULL;

    return proc;
  }

  if ( checkModifySystemProc(def) )
    return lookupProcedure(def, m);

  return NULL;
}


Procedure
getDefinitionProc(Definition def)
{ GET_LD
  Procedure proc = isCurrentProcedure(def->functor->functor, def->module);
  assert(proc);
  return proc;
}


void
shareDefinition(Definition def)
{ int shared = ATOMIC_INC(&def->shared);
  assert(shared > 0);
  (void)shared;
}


int
unshareDefinition(Definition def)
{ return ATOMIC_DEC(&def->shared);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_functor() translates term  of  the   format  +Name/+Arity  into  the
internal functor represenation. It fails and  raises an exception on the
various possible format or representation errors.  ISO compliant.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
get_arity(term_t t, int extra, int maxarity, int *arity)
{ int a;

  if ( !PL_get_integer_ex(t, &a) )
    fail;
  if ( a < 0 )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    ATOM_not_less_than_zero, t);
  a += extra;
  if ( maxarity >= 0 && a > maxarity )
  { char buf[100];

    Ssprintf(buf, "limit is %d, request = %d", maxarity, a);

    return PL_error(NULL, 0, buf,
		    ERR_REPRESENTATION, ATOM_max_procedure_arity);
  }

  *arity = a;

  return TRUE;
}


int
get_functor(term_t descr, functor_t *fdef, Module *m, term_t h, int how)
{ GET_LD
  term_t head;
  int dcgpi=FALSE;

  if ( !(how&GP_NOT_QUALIFIED) )
  { head = PL_new_term_ref();
    if ( !PL_strip_module(descr, m, head) )
      return FALSE;
  } else
  { head = descr;
  }

  if ( PL_is_functor(head, FUNCTOR_divide2) ||
       (dcgpi=PL_is_functor(head, FUNCTOR_gdiv2)) )
  { term_t a = PL_new_term_ref();
    atom_t name;
    int arity = 0;

    _PL_get_arg(1, head, a);
    if ( !PL_get_atom_ex(a, &name) )
      fail;
    _PL_get_arg(2, head, a);
    if ( !get_arity(a,
		    (dcgpi ? 2 : 0),
		    (how&GF_PROCEDURE) ? MAXARITY : -1,
		    &arity ) )
      fail;
    *fdef = PL_new_functor(name, arity);
    if ( h )
      PL_put_term(h, head);

    succeed;
  } else if ( !(how&GF_NAMEARITY) && PL_get_functor(head, fdef) )
  { if ( h )
      PL_put_term(h, head);

    succeed;
  } else
  { if ( how & GP_TYPE_QUIET )
      fail;
    else
      return PL_error(NULL, 0, NULL, ERR_TYPE,
		      ATOM_predicate_indicator, head);
  }
}


int
get_head_functor(DECL_LD term_t head, functor_t *fdef, int how)
{ FunctorDef fd;

  if ( !PL_get_functor(head, fdef) )
  { if ( how&GP_TYPE_QUIET )
      fail;
    else
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, head);
  }

  fd = valueFunctor(*fdef);

  if ( fd->arity > MAXARITY )
  { if ( how&GP_TYPE_QUIET )
    { fail;
    } else
    { char buf[100];

      Ssprintf(buf, "limit is %d, request = %d", MAXARITY, fd->arity);

      return PL_error(NULL, 0, buf,
		      ERR_REPRESENTATION, ATOM_max_procedure_arity);
    }
  }

  if ( !isCallableAtom(fd->name) )
  { if ( how&GP_TYPE_QUIET )
    { fail;
    } else
    { return PL_error(NULL, 0, NULL,
		      ERR_TYPE, ATOM_callable, head);
    }
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get the specified procedure from a   Prolog  argument.  This argument is
either a head or a term of the form module:head.  If `create' is TRUE, a
procedure is created in the module.  Otherwise, the system traverses the
module-inheritance chain to find the existing procedure.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define get_module(mname, how) LDFUNC(get_module, mname, how)
static Module
get_module(DECL_LD atom_t mname, int how)
{ if ( mname )
  { switch(how&GP_HOW_MASK)
    { case GP_CREATE:
      case GP_DEFINE:
	return lookupModule(mname);
      case GP_FIND:
      case GP_FINDHERE:
      case GP_RESOLVE:
      { Module m;
	if ( (m=isCurrentModule(mname)) )
	  return m;
	return MODULE_user;
      }
    }
  }

  return (environment_frame ? contextModule(environment_frame)
			    : MODULE_user);
}


int
get_procedure(term_t descr, Procedure *proc, term_t h, int how)
{ GET_LD
  atom_t mname = 0;
  Module m = NULL;
  functor_t fdef;
  Procedure p;

  if ( (how&GP_NAMEARITY) )
  { if ( !get_functor(descr, &fdef, &m, h,
		      GF_PROCEDURE|(how&GP_TYPE_QUIET)) )
      fail;
  } else
  { term_t head = PL_new_term_ref();
    Word p;

    if ( !(p=stripModuleName(valTermRef(descr), &mname)) )
      return FALSE;
    *valTermRef(head) = linkValNoG(p);

    if ( !(m = get_module(mname, how)) )
      return FALSE;

    if ( h )
      PL_put_term(h, head);

    if ( !get_head_functor(head, &fdef, how) )
      fail;
  }

  switch( how & GP_HOW_MASK )
  { case GP_CREATE:
      *proc = lookupBodyProcedure(fdef, m);
      break;
    case GP_FINDHERE:
      if ( (p = isCurrentProcedure(fdef, m)) )
      { *proc = p;
        break;
      }
      goto notfound;
    case GP_FIND:
      if ( (p = visibleProcedure(fdef, m)) )
      { *proc = p;
        goto out;
      }
      goto notfound;
    case GP_DEFINE:
      if ( (p = lookupProcedureToDefine(fdef, m)) )
      { *proc = p;
        break;
      }
      fail;				/* permission error */
    case GP_RESOLVE:
      if ( (p = resolveProcedure(fdef, m)) )
      { *proc = p;
        break;
      }
      goto notfound;
    default:
      assert(0);
  }
out:
  succeed;

notfound:
  if ( (how & GP_EXISTENCE_ERROR) )
    return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_procedure, descr);
  fail;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This function  implements  $c_current_predicate/2.   current_predicate/2
itself  is  written  in  Prolog, based on this function.  Having dynamic
linking from super modules and dynamic loading from the  libraries,  the
definition  of current predicate has become a difficult issue.  Normally
it is used for meta-programming and program analysis.  I think it should
succeed  for  each  predicate  that  can   be   called.    The   current
implementation  is VERY slow due to all Prolog overhead.  This should be
reconsidered and probably a large part of this function should be  moved
to C.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_current_predicate(term_t name, term_t spec, control_t h)
{ GET_LD
  TableEnum e;
  atom_t n;
  functor_t f;
  Module m = (Module) NULL;
  Procedure proc;
  term_t functor = PL_new_term_ref();

  if ( ForeignControl(h) == FRG_CUTTED )
  { e = ForeignContextPtr(h);
    freeTableEnum(e);
    succeed;
  }

  if ( !PL_strip_module_flags(spec, &m, functor, SM_NOCREATE) )
    fail;

  if ( !PL_get_atom(name, &n) )
  { if ( PL_is_variable(name) )
      n = NULL_ATOM;
    else
      fail;
  }
  if ( !PL_get_functor(functor, &f) )
  { if ( PL_is_variable(functor) )
      f = 0;
    else
      fail;
  }

  if ( ForeignControl(h) == FRG_FIRST_CALL)
  { if ( f )
    { if ( (proc = isCurrentProcedure(f, m)) )
	return PL_unify_atom(name, nameFunctor(f));
      fail;
    }
    e = newTableEnum(m->procedures);
  } else
    e = ForeignContextPtr(h);

  while( advanceTableEnum(e, NULL, (void**)&proc) )
  { FunctorDef fdef;

    fdef = proc->definition->functor;

    if ( (n && n != fdef->name) ||
	 !PL_unify_atom(name, fdef->name) ||
	 !PL_unify_functor(functor, fdef->functor) )
      continue;

    ForeignRedoPtr(e);
  }

  freeTableEnum(e);
  fail;
}

		 /*******************************
		 *    ISO CURRENT-PREDICATE/1	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Patterns: ?Name/?Arity
	  ?Module:(?Name/?Arity)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ functor_t	functor;		/* Functor we are looking for */
  atom_t	name;			/* Name of target pred */
  int		arity;			/* arity of target pred */
  int		macq;			/* Module is acquired */
  Module	module;			/* Module to search in */
  Module	super;			/* Walking along super-chain */
  TableEnum	epred;			/* Predicate enumerator */
  ModuleEnum	emod;			/* Module enumerator */
} cur_enum;


static int
isDefinedOrAutoloadProcedure(Procedure proc)
{ Definition def = proc->definition;

  if ( true(def, PROC_DEFINED|P_AUTOLOAD) )
    succeed;

  return hasClausesDefinition(def) ? TRUE : FALSE;
}


static Procedure
visibleProcedure(DECL_LD functor_t f, Module m)
{ ListCell c;
  Procedure p;

  for(;;)
  { next:

    if ( (p = isCurrentProcedure(f, m)) && isDefinedOrAutoloadProcedure(p) )
      return p;

    for(c=m->supers; c; c=c->next)
    { if ( c->next )
      { if ( (p=visibleProcedure(f, c->value)) )
	  return p;
      } else
      { m = c->value;
	goto next;
      }
    }

    return NULL;
  }
}


static
PRED_IMPL("current_predicate", 1, current_predicate,
	  PL_FA_TRANSPARENT|PL_FA_NONDETERMINISTIC|PL_FA_ISO)
{ PRED_LD
  cur_enum e0;
  cur_enum *e;
  int rval = FALSE;
  term_t mt = 0;			/* module-term */
  term_t nt = 0;			/* name-term */
  term_t at = 0;			/* arity-term */
  unsigned int aextra = 0;
  term_t spec = A1;

  if ( CTX_CNTRL != FRG_CUTTED )
  { term_t pi = PL_copy_term_ref(spec);

    nt = PL_new_term_ref();
    at = PL_new_term_ref();

    while( PL_is_functor(pi, FUNCTOR_colon2) )
    { if ( !mt )
	mt = PL_new_term_ref();
      _PL_get_arg(1, pi, mt);
      _PL_get_arg(2, pi, pi);
    }

    if ( PL_is_functor(pi, FUNCTOR_divide2) )
    { _PL_get_arg(1, pi, nt);
      _PL_get_arg(2, pi, at);
    } else if ( PL_is_functor(pi, FUNCTOR_gdiv2) )
    { _PL_get_arg(1, pi, nt);
      _PL_get_arg(2, pi, at);
      aextra = 2;
    } else if ( PL_is_variable(pi) )
    { term_t a;

      if ( !(a=PL_new_term_ref()) ||
	   !PL_cons_functor(a, FUNCTOR_divide2, nt, at) ||
	   !PL_unify(pi, a) )
	return FALSE;			/* resource error */
    } else
      goto typeerror;
  }

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { e = &e0;
      memset(e, 0, sizeof(*e));

      if ( !PL_get_atom(nt, &e->name) )
      { if ( !PL_is_variable(nt) )
	  goto typeerror;
      }
      if ( PL_get_integer(at, &e->arity) )
      { if ( e->arity < 0 )
	  return PL_error(NULL, 0, NULL, ERR_DOMAIN,
			  ATOM_not_less_than_zero, at);

	e->arity += aextra;
      } else
      { if ( !PL_is_variable(at) )
	  goto typeerror;
	e->arity = -1;
      }

      if ( e->name && e->arity >= 0 )
	e->functor = PL_new_functor(e->name, e->arity);

      if ( mt )
      { atom_t mname;

	if ( PL_is_variable(mt) )
	{ if ( !(e->emod = newModuleEnum(0)) )
	    return PL_no_memory();

	  if ( !(e->module=advanceModuleEnum(e->emod)) )
	    return FALSE;
	} else if ( PL_get_atom_ex(mt, &mname) )
	{ if ( !(e->module = acquireModule(mname)) )
	    return FALSE;
	  e->macq = TRUE;
	} else
	{ return FALSE;
	}
      } else
      { if ( environment_frame )
	  e->module = contextModule(environment_frame);
	else
	  e->module = MODULE_user;
	e->super = e->module;
      }

      if ( e->functor )
      { if ( !e->emod )			/* fully specified */
	{ rval = (visibleProcedure(e->functor, e->module) != NULL);
	  goto clean;
	}
      } else
      { e->epred = newTableEnum(e->module->procedures);
      }

      e = allocForeignState(sizeof(*e));
      *e = e0;
      break;
    }
    case FRG_REDO:
      e = CTX_PTR;
      break;
    case FRG_CUTTED:
    { e = CTX_PTR;
      rval = TRUE;
      goto clean;
    }
    default:
    { e = NULL;
      assert(0);
    }
  }

  for(;;)
  { if ( e->functor )			/* _M:foo/2 */
    { if ( visibleProcedure(e->functor, e->module) )
      { PL_unify_atom(mt, e->module->name);

	if ( (e->module=advanceModuleEnum(e->emod)) )
	{ ForeignRedoPtr(e);
	} else
	{ rval = TRUE;
	  goto clean;
	}
      }
    } else
    { functor_t f;
      Procedure proc;
      while( advanceTableEnum(e->epred, (void**)&f, (void**)&proc) )
      { FunctorDef fd = valueFunctor(f);

	if ( (!e->name     || e->name == fd->name) &&
	     (e->arity < 0 || (unsigned int)e->arity == fd->arity) &&
	     fd->arity >= aextra &&
	     isDefinedProcedure(proc) )
	{ if ( mt )
	    PL_unify_atom(mt, e->module->name);
	  if ( !e->name )
	    PL_unify_atom(nt, fd->name);
	  if ( e->arity < 0 )
	    PL_unify_integer(at, fd->arity-aextra);

	  ForeignRedoPtr(e);
	}
      }
    }

    if ( e->emod )			/* enumerate all modules */
    { Module m;
      while( (m=advanceModuleEnum(e->emod)) )
      {
					/* skip hidden modules */
	if ( SYSTEM_MODE ||
	     m->name == ATOM_system ||
	     m->class != ATOM_system )
	  break;
      }
      if ( m )
	e->super = e->module = m;
      else
	break;
    } else if ( !e->functor && e->super && e->super->supers )
    { e->super = e->super->supers->value;	/* advance to user-modules */
					/* TBD: handle multiple supers */
    } else
      break;				/* finished all modules */

    if ( !e->functor )
    { freeTableEnum(e->epred);
      e->epred = newTableEnum(e->super->procedures);
    }
  }

clean:
  if ( e )
  { if ( e->epred )
      freeTableEnum(e->epred);
    if ( e->emod )
      freeModuleEnum(e->emod);
    if ( e->module && e->macq )
      releaseModule(e->module);
    if ( e != &e0 )
      freeForeignState(e, sizeof(*e));
  }

  return rval;

typeerror:
  return PL_error(NULL, 0, NULL, ERR_TYPE,
		  ATOM_predicate_indicator, spec);
}


		 /*******************************
		 *	 CLAUSE REFERENCES	*
		 *******************************/

#ifdef O_DEBUG
static Table retracted_clauses = NULL;

static void
registerRetracted(Clause cl)
{ DEBUG(MSG_CGC_CREF_PL, Sdprintf("/**/ r(%p).\n", cl));
  DEBUG(MSG_CGC_CREF_TRACK,
	{ if ( !retracted_clauses )
	    retracted_clauses = newHTable(1024);
	  addNewHTable(retracted_clauses, cl, (void*)1);
	});
}

static void
reclaimRetracted(Clause cl)
{ DEBUG(MSG_CGC_CREF_TRACK,
	{ void *v = deleteHTable(retracted_clauses, cl);
	  if ( v != (void*)1 && GD->cleaning == CLN_NORMAL )
	  { Definition def = cl->predicate;
	    Sdprintf("reclaim not retracted from %s\n", predicateName(def));
	  }
	});
}

#else

#define registerRetracted(cl) (void)0
#define reclaimRetracted(cl)  (void)0

#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Clause references are used  to  link   clauses  from  the main predicate
clause list as well as from additional  indexes. They form a linked list
of clauses, indexed according to a  specific   key.  This key is deduced
from the first argument for  the  main   predicate  clause  list or from
alternative arguments for secondary clause lists.

Traversing a list of clause  references   traverses  the ->next pointer,
possibly matches the key and then looks into the associated ->clause for
the born/died generations. If a clause erased, cleanDefinition() removes
the  references  to  it  from  the  linked    lists  and  adds  them  to
GD->lingering_clauses, which uses d.gnext to   link them together rather
then ->next because ->next might be used by some other thread traversing
the clause chain.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
acquire_clause(Clause cl)
{ ATOMIC_INC(&cl->references);
}

void
release_clause(Clause cl)
{ if ( ATOMIC_DEC(&cl->references) == 0 )
  { size_t size = sizeofClause(cl->code_size) + SIZEOF_CREF_CLAUSE;

    ATOMIC_SUB(&GD->clauses.erased_size, size);
    ATOMIC_DEC(&GD->clauses.erased);

    reclaimRetracted(cl);
    freeClause(cl);
  }
}


ClauseRef
newClauseRef(Clause clause, word key)
{ ClauseRef cref = allocHeapOrHalt(SIZEOF_CREF_CLAUSE);

  DEBUG(MSG_CGC_CREF_PL,
	Sdprintf("/**/ a(%p, %p, %d, '%s').\n",
		 cref, clause, clause->references,
		 predicateName(clause->predicate)));

  cref->next         = NULL;
  cref->d.key        = key;
  cref->value.clause = clause;
  acquire_clause(clause);

  return cref;
}


static void
freeClauseRef(ClauseRef cref)
{ Clause cl = cref->value.clause;

  DEBUG(MSG_CGC_CREF_PL,
	Sdprintf("/**/ d(%p, %p, %d).\n",
		 cref, cl, (int)cl->references));

  release_clause(cl);

  freeHeap(cref, SIZEOF_CREF_CLAUSE);
}


void
lingerClauseRef(ClauseRef cref)
{ ClauseRef o;

  DEBUG(0,
	if ( isProtectedCRef(cref) )
	{ Clause cl = cref->value.clause;
	  Sdprintf("Ditching clause for %s %lld-%lld\n",
		   predicateName(cl->predicate),
		   cl->generation.created,
		   cl->generation.erased);
	  assert(0);
	});

  if ( GD->cleaning != CLN_DATA )
  { do
    { o = GD->clauses.lingering;
      cref->d.gnext = o;
    } while(!COMPARE_AND_SWAP_PTR(&GD->clauses.lingering, o, cref) );

    ATOMIC_INC(&GD->clauses.lingering_count);
  } else
  { freeClauseRef(cref);
  }
}


static int activePredicate(const Definition *defs, const Definition def);

static void
gcClauseRefs(void)
{ ClauseRef cref;

  if ( !(cref = GD->clauses.lingering) ||
       !COMPARE_AND_SWAP_PTR(&GD->clauses.lingering, cref, NULL) )
    return;			/* no work or someone else doing it */
  GD->clauses.lingering_count = 0;

  if ( cref )
  { ClauseRef next;
    Definition *active_defs = predicates_in_use();
    int freed = 0;
    int kept = 0;

    for( ; cref; cref = next)
    { Definition def;

      next = cref->d.gnext;
      def = cref->value.clause->predicate;
      if ( !activePredicate(active_defs, def) )
      { freeClauseRef(cref);
	freed++;
      } else
      {	lingerClauseRef(cref);
	kept++;
      }
    }

    if ( active_defs )
      PL_free(active_defs);

    DEBUG(MSG_CGC_CREF, Sdprintf("GC clause references: freed %d, kept %d\n",
				 freed, kept));
  }
}

static int
activePredicate(const Definition *defs, const Definition def)
{ if ( defs )
  { for( ; *defs; defs++)
    { if ( *defs == def )
	return TRUE;
    }
  }

  return FALSE;
}

void
setLastModifiedPredicate(Definition def, gen_t gen, int flags)
{ Module m = def->module;

  if ( likely(gen < GEN_TRANSACTION_BASE) )
  { gen_t lmm;
    def->last_modified = gen;

    do
    { lmm = m->last_modified;
    } while ( lmm < gen &&
	      !COMPARE_AND_SWAP_UINT64(&m->last_modified, lmm, gen) );
#ifdef O_PLMT
    if ( true(def, P_DYNAMIC) )
      wakeupThreads(def, flags);
#endif
  } else
  { transaction_set_last_modified(def, gen, flags);
  }
}


static int
add_ssu_clause(Definition def, Clause clause)
{ if ( true(clause, SSU_COMMIT_CLAUSE|SSU_CHOICE_CLAUSE) )
  { if ( false(def, P_SSU_DET) )
    { if ( def->impl.clauses.number_of_clauses == 0 )
	set(def, P_SSU_DET);
      else
	return PL_error(NULL, 0, NULL, ERR_PERMISSION_SSU_DEF, def);
    }
  } else
  { if ( true(def, P_SSU_DET) )
    { if ( def->impl.clauses.number_of_clauses == 0 )
	clear(def, P_SSU_DET);
      else
	return PL_error(NULL, 0, NULL, ERR_PERMISSION_SSU_DEF, def);
    }
  }

  return TRUE;
}


		 /*******************************
		 *	      ASSERT		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Assert a clause to a procedure. Where askes to assert either at the head
or at the tail of the clause list.

The `where` argument is one of

  - CL_START (asserta)
  - CL_END   (assertz)
  - The clause reference before which the clause must be inserted.
    This is used by reconsult.

(*) This function updates the indexing information.  If we have a static
procedure, it deletes the supervisor. This is  probably a bit rough, but
deals with -for example- clauses for   term_expansion/2. After the first
definition this will be  called  and   an  S_TRUSTME  supervisor will be
installed, causing further clauses to have no effect.

This function returns NULL on failure. One  of the failure cases is that
the event notification vetoed the assert. In   this  case the clause has
become part of the predicate. The caller should not call freeClause() if
this function failed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ClauseRef
assertDefinition(DECL_LD Definition def, Clause clause, ClauseRef where)
{ word key;
  ClauseRef cref;

  if ( !add_ssu_clause(def, clause) )
  { freeClause(clause);
    return NULL;
  }

  argKey(clause->codes, 0, &key);
  if ( !(cref=newClauseRef(clause, key)) )
  { freeClause(clause);
    return PL_no_memory(),NULL;
  }

  clause->generation.created = max_generation(def);
  clause->generation.erased  = 1;

  LOCKDEF(def);
  acquire_def(def);
  if ( !def->impl.clauses.last_clause )
  { def->impl.clauses.first_clause = def->impl.clauses.last_clause = cref;
  } else if ( where == CL_START || where == def->impl.clauses.first_clause )
  { cref->next = def->impl.clauses.first_clause;
    def->impl.clauses.first_clause = cref;
  } else if ( where == CL_END )
  { ClauseRef last = def->impl.clauses.last_clause;

    last->next = cref;
    def->impl.clauses.last_clause = cref;
  } else				/* insert before */
  { ClauseRef cr;

    for(cr = def->impl.clauses.first_clause; cr; cr = cr->next)
    { if ( cr->next == where )
      { cref->next = where;
	cr->next = cref;
	break;
      }
    }
    assert(cr);
  }

  def->impl.clauses.number_of_clauses++;
  if ( false(clause, UNIT_CLAUSE) )
    def->impl.clauses.number_of_rules++;
  if ( true(def, P_DIRTYREG) )
    ATOMIC_INC(&GD->clauses.dirty);

  if ( false(def, P_DYNAMIC|P_LOCKED_SUPERVISOR) ) /* see (*) above */
    freeCodesDefinition(def, TRUE);

  addClauseToIndexes(def, clause, where);
  release_def(def);
  DEBUG(CHK_SECURE, checkDefinition(def));
  UNLOCKDEF(def);

  if ( unlikely(!!LD->transaction.generation) && def && true(def, P_DYNAMIC) )
  { if ( LD->transaction.generation < LD->transaction.gen_max )
    { clause->generation.created = ++LD->transaction.generation;
      clause->generation.erased  = max_generation(def);
    } else
    { PL_representation_error("transaction_generations");
      goto error;
    }
  } else
  { PL_LOCK(L_GENERATION);
    clause->generation.created = ++GD->_generation;
    clause->generation.erased  = max_generation(def);
    PL_UNLOCK(L_GENERATION);
  }

  if ( ( def->events &&
	 !(LD->transaction.flags&TR_BULK) &&
	 !predicate_update_event(def,
				 where == CL_START ? ATOM_asserta : ATOM_assertz,
				 clause, 0) ) )
  { error:
    retractClauseDefinition(def, clause, FALSE);
    return NULL;
  }

  setLastModifiedPredicate(def, clause->generation.created, TWF_ASSERT);

  if ( LD->transaction.generation &&
       clause->generation.created >= LD->transaction.gen_base )
    transaction_assert_clause(clause, where);

  return cref;
}


ClauseRef
assertProcedure(DECL_LD Procedure proc, Clause clause, ClauseRef where)
{ Definition def = getProcDefinition(proc);

  return assertDefinition(def, clause, where);
}


/*  Abolish a procedure.  Referenced  clauses  are   unlinked  and left
    dangling in the dark until the procedure referencing it deletes it.

    Since we have a foreign language interface we will allow to  abolish
    foreign  predicates  as  well.  Permission testing should be done by
    the caller.

 ** Sun Apr 17 16:18:50 1988  jan@swivax.UUCP (Jan Wielemaker)  */

bool
abolishProcedure(Procedure proc, Module module)
{ Definition def = proc->definition;

  DEBUG(MSG_PROC, Sdprintf("abolishProcedure(%s)\n", predicateName(def)));

  LOCKDEF(def);
  if ( def->module != module )		/* imported predicate; remove link */
  { Definition ndef	     = allocHeapOrHalt(sizeof(*ndef));
    Definition odef          = def;

    memset(ndef, 0, sizeof(*ndef));
    ndef->functor            = def->functor; /* should be merged with */
    ndef->impl.any.args	     = allocHeapOrHalt(sizeof(*ndef->impl.any.args)*
					       def->functor->arity);
    ndef->module             = module;	     /* lookupProcedure()!! */
    ndef->codes		     = SUPERVISOR(virgin);
    proc->definition         = ndef;
    ATOMIC_INC(&GD->statistics.predicates);
    ATOMIC_ADD(&module->code_size, sizeof(*ndef));
    resetProcedure(proc, TRUE);
    DEBUG(MSG_PRED_COUNT, Sdprintf("Created %s at %p\n",
				   predicateName(ndef), ndef));
    if ( unshareDefinition(odef) == 0 )
      lingerDefinition(odef);
  } else if ( true(def, P_FOREIGN) )	/* foreign: make normal */
  { def->impl.clauses.first_clause = def->impl.clauses.last_clause = NULL;
    resetProcedure(proc, TRUE);
  } else if ( true(def, P_THREAD_LOCAL) )
  { UNLOCKDEF(def);
    return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
		    ATOM_modify, ATOM_thread_local_procedure, proc);
  } else				/* normal Prolog procedure */
  { removeClausesPredicate(def, 0, FALSE);
    setDynamicDefinition_unlocked(def, FALSE);
    resetProcedure(proc, FALSE);
  }

  DEBUG(CHK_SECURE, checkDefinition(def));
  UNLOCKDEF(def);

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Remove (mark for  deletion)  all  clauses   that  come  from  the  given
source-file or any sourcefile. Note   that thread-local predicates don't
have clauses from files, so we don't   need to bother. Returns number of
clauses that has been deleted.

This is called only for (re)consult. What to do with dynamic predicates?

MT: Caller must hold L_PREDICATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

size_t
removeClausesPredicate(Definition def, int sfindex, int fromfile)
{ GET_LD
  ClauseRef c, next;
  size_t deleted = 0;
  size_t memory = 0;
  gen_t update;

  if ( true(def, P_THREAD_LOCAL) )
    return 0;

  if ( GD->cleaning != CLN_DATA )		/* normal operation */
  { PL_LOCK(L_GENERATION);
    update = global_generation()+1;
    acquire_def(def);
    for(c = def->impl.clauses.first_clause; c; c = next)
    { Clause cl = c->value.clause;

      next = c->next;

      if ( (sfindex == 0 || sfindex == cl->owner_no) &&
	   (!fromfile || cl->line_no > 0) &&
	   false(cl, CL_ERASED) )
      { set(cl, CL_ERASED);
#ifdef O_LOGICAL_UPDATE
	cl->generation.erased = update;
#endif
	deleted++;
	memory += sizeofClause(cl->code_size) + SIZEOF_CREF_CLAUSE;
	def->impl.clauses.number_of_clauses--;
	def->impl.clauses.erased_clauses++;
	if ( false(cl, UNIT_CLAUSE) )
	  def->impl.clauses.number_of_rules--;
	deleteActiveClauseFromIndexes(def, cl);
	registerRetracted(cl);
      }
    }
    release_def(def);
    GD->_generation = update;
    PL_UNLOCK(L_GENERATION);

    if ( deleted )
    { if ( def->module )
	ATOMIC_SUB(&def->module->code_size, memory);
      ATOMIC_ADD(&GD->clauses.erased_size, memory);
      ATOMIC_ADD(&GD->clauses.erased, deleted);
      if( true(def, P_DIRTYREG) )
	ATOMIC_SUB(&GD->clauses.dirty, deleted);

      registerDirtyDefinition(def);
      DEBUG(CHK_SECURE, checkDefinition(def));
    }
  } else				      /* final cleanup */
  { for(c = def->impl.clauses.first_clause; c; c = next)
    { Clause cl = c->value.clause;

      next = c->next;
      set(cl, CL_ERASED);
      freeClauseRef(c);
    }
    def->impl.clauses.first_clause = NULL;
    def->impl.clauses.last_clause = NULL;
  }

  return deleted;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Retract  a  clause  from  a  dynamic  procedure.  Called  from  erase/1,
retract/1 and retractall/1. Returns FALSE  if   the  clause  was already
retracted or retract is vetoed by  the   update  event handling. This is
also used by  trie_gen_compiled/3  to  get   rid  of  the  clauses  that
represent tries.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
retract_clause(DECL_LD Clause clause, gen_t generation)
{ Definition def = clause->predicate;
  size_t size = sizeofClause(clause->code_size) + SIZEOF_CREF_CLAUSE;

  if ( generation )
  { if ( clause->generation.erased > generation )
      clause->generation.erased = generation;
  } else if ( unlikely(!!LD->transaction.generation) &&
	      def && true(def, P_DYNAMIC) )
  { if ( LD->transaction.generation < LD->transaction.gen_max )
    { if ( LD->transaction.generation < LD->transaction.gen_max )
      { if ( clause->generation.erased >= LD->transaction.generation )
	  clause->generation.erased = ++LD->transaction.generation;
      }
    } else
    { return PL_representation_error("transaction_generations");
    }
  } else
  { PL_LOCK(L_GENERATION);
    if ( clause->generation.erased >= GD->_generation )
    { gen_t egen = GD->_generation+1;
      clause->generation.erased = egen;
      MEMORY_RELEASE();
      GD->_generation = egen;
    }
    PL_UNLOCK(L_GENERATION);
  }

  LOCKDEF(def);
  if ( true(clause, CL_ERASED) )
  { UNLOCKDEF(def);
    return FALSE;
  }

  DEBUG(CHK_SECURE, checkDefinition(def));
  set(clause, CL_ERASED);
  deleteActiveClauseFromIndexes(def, clause); /* just updates "dirtyness" */
  def->impl.clauses.number_of_clauses--;
  def->impl.clauses.erased_clauses++;
  if ( false(clause, UNIT_CLAUSE) )
    def->impl.clauses.number_of_rules--;
  DEBUG(CHK_SECURE, checkDefinition(def));
  UNLOCKDEF(def);

					/* update stats */
  registerRetracted(clause);
  if ( true(clause, DBREF_CLAUSE) )
    ATOMIC_INC(&GD->clauses.db_erased_refs);

  ATOMIC_SUB(&def->module->code_size, size);
  ATOMIC_ADD(&GD->clauses.erased_size, size);
  ATOMIC_INC(&GD->clauses.erased);
  if( true(def, P_DIRTYREG) )
    ATOMIC_DEC(&GD->clauses.dirty);

  registerDirtyDefinition(def);
  if ( generation != GEN_TR_DISCARD_ASSERT )
    setLastModifiedPredicate(def, clause->generation.erased, TWF_RETRACT);

  return TRUE;
}


int
retractClauseDefinition(Definition def, Clause clause, int notify)
{ GET_LD

  if ( def->events && notify &&
       !(LD->transaction.flags&TR_BULK) &&
       !predicate_update_event(def, ATOM_retract, clause, 0) )
    return FALSE;

  if ( LD->transaction.generation )
  { int rc;

    if ( (rc=transaction_retract_clause(clause)) == TRUE )
      return TRUE;
    if ( rc < 0 )
      return FALSE;			/* error */
  }

  return retract_clause(clause, 0);
}


void
unallocClause(Clause c)
{ ATOMIC_SUB(&GD->statistics.codes, c->code_size);
  ATOMIC_DEC(&GD->statistics.clauses);
  if ( c->source_no )			/* set by assert_term() */
  { if ( c->owner_no != c->source_no )
      releaseSourceFileNo(c->owner_no);
    releaseSourceFileNo(c->source_no);
  }

#ifdef ALLOC_DEBUG
#define ALLOC_FREE_MAGIC 0xFB
  size_t size = sizeofClause(c->code_size);
  memset(c, ALLOC_FREE_MAGIC, size);
#endif

  PL_free(c);
}


#ifdef O_DEBUG_ATOMGC
static void
unregister_atom_clause(atom_t a)
{ PL_unregister_atom(a);
}

static void
register_atom_clause(atom_t a)
{ PL_register_atom(a);
}
#endif

void
freeClause(Clause c)
{
#ifdef O_ATOMGC
#ifdef O_DEBUG_ATOMGC
  forAtomsInClause(c, unregister_atom_clause);
#else
  forAtomsInClause(c, PL_unregister_atom);
#endif
#endif

  if ( true(c, DBREF_CLAUSE) )		/* will be freed from symbol */
    set(c, DBREF_ERASED_CLAUSE);
  else
    unallocClause(c);
}



static int WUNUSED			/* FALSE if there was an error */
announceErasedClause(Clause clause)
{ return ( clearBreakPointsClause(clause) >= 0 &&
	   callEventHook(PLEV_ERASED_CLAUSE, clause) );
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cleanDefinition()
    This function has two tasks. If the predicate needs to be rehashed,
    this is done and all erased clauses from the predicate are returned
    as a linked list.

    We cannot delete the clauses immediately as the debugger requires a
    call-back and we have the L_PREDICATE mutex when running this code.

find_prev() finds the real  previous  clause.   The  not-locked  loop of
cleanDefinition() keep track of this, but  in the meanwhile the previous
may change due to an assert. Now that we are in the locked region we can
search for the real previous, using   the  one from cleanDefinition() as
the likely candidate.

The `ddi->oldest_generation` contains the latest  marked generation that
was found or GEN_MAX  if  the  predicate   is  not  active.  The `start`
generation contains the generation when pl_garbage_collect_clauses() was
started.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int	mustCleanDefinition(const Definition def);

static ClauseRef
find_prev(Definition def, ClauseRef prev, ClauseRef cref)
{ if ( (!prev && def->impl.clauses.first_clause == cref) ||
       ( prev && prev->next == cref) )
    return prev;

  DEBUG(MSG_PROC, Sdprintf("Fixing prev\n"));
  for(prev = def->impl.clauses.first_clause; prev; prev = prev->next)
  { if ( prev->next == cref )
      return prev;
  }

  assert(0);
  return NULL;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) This used to be acquire_def(def),  but announceErasedClause may call
Prolog, leading to nested acquired definition. This is not needed anyway
as the acquired definition is only  used   by  clause  GC, we are inside
clause GC and clause GC calls cannot run in parallel.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static size_t
cleanDefinition(Definition def, DirtyDefInfo ddi, gen_t start, Buffer tr_starts,
		int *rcp)
{ size_t removed = 0;

  DEBUG(CHK_SECURE,
	LOCKDEF(def);
	checkDefinition(def);
        UNLOCKDEF(def));

  if ( mustCleanDefinition(def) && true(ddi, DDI_MARKING) )
  { ClauseRef cref, prev = NULL;
#if O_DEBUG
    int left = 0;
#endif

    assert(GD->clauses.cgc_active);		/* See (*) */

    for(cref = def->impl.clauses.first_clause;
	cref && def->impl.clauses.erased_clauses;
	cref=cref->next)
    { Clause cl = cref->value.clause;

      if ( true(cl, CL_ERASED) && ddi_is_garbage(ddi, start, tr_starts, cl) )
      { if ( !announceErasedClause(cl) )
	  *rcp = FALSE;

	LOCKDEF(def);
	prev = find_prev(def, prev, cref);
	if ( !prev )
	{ def->impl.clauses.first_clause = cref->next;
	  if ( !cref->next )
	    def->impl.clauses.last_clause = NULL;
	} else
	{ prev->next = cref->next;
	  if ( cref->next == NULL)
	    def->impl.clauses.last_clause = prev;
	}
	removed++;
	def->impl.clauses.erased_clauses--;
	UNLOCKDEF(def);

	lingerClauseRef(cref);
      } else
      { prev = cref;
	DEBUG(MSG_PROC, left++);
      }
    }
    if ( removed )
    { LOCKDEF(def);
      cleanClauseIndexes(def, &def->impl.clauses, ddi, start, tr_starts);
      UNLOCKDEF(def);
    }
    gen_t active = ddi_oldest_generation(ddi);
    if ( start < active )
      active = start;
    free_lingering(&def->lingering, active);

    DEBUG(CHK_SECURE,
	  LOCKDEF(def);
	  checkDefinition(def);
	  UNLOCKDEF(def));

    DEBUG(MSG_PROC,
	  Sdprintf("cleanDefinition(%s): removed %d, left %d, erased %d\n",
		   predicateName(def), removed, left,
		   def->impl.clauses.erased_clauses));
  }

  return removed;
}


static int
mustCleanDefinition(const Definition def)
{ return ( def->impl.clauses.erased_clauses > 0 );
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Finalize a reloaded predicate. This atomically  makes the new definition
visible.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
reconsultFinalizePredicate(DECL_LD sf_reload *rl, Definition def, p_reload *r)
{ if ( true(r, P_MODIFIED) )
  { ClauseRef cref;
    gen_t update;
    size_t deleted = 0;
    size_t added   = 0;
    size_t memory  = 0;

    PL_LOCK(L_GENERATION);
    update = global_generation()+1;
    acquire_def(def);
    for(cref = def->impl.clauses.first_clause; cref; cref=cref->next)
    { Clause cl = cref->value.clause;

      if ( cl->generation.erased == rl->reload_gen && false(cl, CL_ERASED) )
      { set(cl, CL_ERASED);
	cl->generation.erased = update;
	deleted++;
	memory += sizeofClause(cl->code_size) + SIZEOF_CREF_CLAUSE;
	def->impl.clauses.number_of_clauses--;
	def->impl.clauses.erased_clauses++;
	if ( false(cl, UNIT_CLAUSE) )
	  def->impl.clauses.number_of_rules--;
	if ( true(def, P_DYNAMIC) )
	  deleteActiveClauseFromIndexes(def, cl);
	registerRetracted(cl);
      } else if ( cl->generation.created == rl->reload_gen )
      { cl->generation.created = update;
	added++;
      }
    }
    release_def(def);
    GD->_generation = update;
    PL_UNLOCK(L_GENERATION);

    DEBUG(MSG_RECONSULT_CLAUSE,
	  Sdprintf("%s: added %ld, deleted %ld clauses "
		   "at gen=%ld, GD->gen = %lld\n",
		   predicateName(def), (long)added, (long)deleted,
		   (long)update, (int64_t)global_generation()));

    if ( added || deleted )
    { int flags = 0;
      if ( added ) flags |= TWF_ASSERT;
      if ( deleted ) flags |= TWF_RETRACT;
      setLastModifiedPredicate(def, update, flags);
    }

    if ( deleted )
    { ATOMIC_SUB(&def->module->code_size, memory);
      ATOMIC_ADD(&GD->clauses.erased_size, memory);
      ATOMIC_ADD(&GD->clauses.erased, deleted);
      if( true(def, P_DIRTYREG) )
        ATOMIC_SUB(&GD->clauses.dirty, deleted);

      registerDirtyDefinition(def);
    }

    DEBUG(CHK_SECURE, checkDefinition(def));
  }
}


		 /*******************************
		 *	  META PREDICATE	*
		 *******************************/

/** meta_predicate :HeadList is det.

Declaration for meta-predicates. The  declaration   fills  the meta_info
field of a definition as well  as   the  P_META and P_TRANSPARENT flags.
P_META indicates that meta_info is   valid. P_TRANSPARENT indicates that
the declaration contains at least one meta-argument (: or 0..9).

@param HeadList	Comma separated list of predicates heads, where each
		predicate head has arguments 0..9, :,+,-,?
*/

int
isTransparentMetamask(Definition def, arg_info *args)
{ size_t i, arity = def->functor->arity;
  int transparent = FALSE;

  for(i=0; i<arity && !transparent; i++)
  { int ma = args[i].meta;
    if ( MA_NEEDS_TRANSPARENT(ma) )
      transparent = TRUE;
  }

  return transparent;
}


void
setMetapredicateMask(Definition def, arg_info *args)
{ size_t i, arity = def->functor->arity;

  for(i=0; i<arity; i++)
    def->impl.any.args[i].meta = args[i].meta;

  if ( isTransparentMetamask(def, args) )
    set(def, P_TRANSPARENT);
  else
    clear(def, P_TRANSPARENT);
  set(def, P_META);
}


static int
meta_declaration(term_t spec)
{ GET_LD
  term_t head = PL_new_term_ref();
  term_t arg = PL_new_term_ref();
  Procedure proc;
  atom_t name;
  size_t i, arity;

  if ( !get_procedure(spec, &proc, head, GP_DEFINE) ||
       !PL_get_name_arity(head, &name, &arity) )
    return FALSE;

#ifdef _MSC_VER
  arg_info *args = alloca(arity*sizeof(*args));
#else
  arg_info args[arity];			/* GCC dynamic allocation */
#endif

  for(i=0; i<arity; i++)
  { atom_t ma;

    _PL_get_arg(i+1, head, arg);

    if ( PL_is_integer(arg) )
    { int e;

      if ( !PL_get_integer_ex(arg, &e) )
	return FALSE;
      if ( e < 0 || e > 9 )
      { domain_error:
	return PL_error(NULL, 0, "0..9",
			ERR_DOMAIN, ATOM_meta_argument_specifier, arg);
      }
      args[i].meta = e;
    } else if ( PL_get_atom(arg, &ma) )
    { int m;

      if      ( ma == ATOM_plus )          m = MA_NONVAR;
      else if ( ma == ATOM_minus )         m = MA_VAR;
      else if ( ma == ATOM_question_mark ) m = MA_ANY;
      else if ( ma == ATOM_star )	   m = MA_ANY; /* * mapped to ? */
      else if ( ma == ATOM_colon )         m = MA_META;
      else if ( ma == ATOM_hat )           m = MA_HAT;
      else if ( ma == ATOM_gdiv )          m = MA_DCG;
      else goto domain_error;

      args[i].meta = m;
    } else
    { return PL_error(NULL, 0, "0..9",
			ERR_TYPE, ATOM_meta_argument_specifier, arg);;
    }
  }

  if ( ReadingSource )
  { SourceFile sf = lookupSourceFile(source_file_name, TRUE);
    int rc = setMetapredicateSource(sf, proc, args);
    releaseSourceFile(sf);
    return rc;
  } else
  { setMetapredicateMask(proc->definition, args);
    return TRUE;
  }
}


static
PRED_IMPL("meta_predicate", 1, meta_predicate, PL_FA_TRANSPARENT)
{ PRED_LD
  term_t tail = PL_copy_term_ref(A1);
  term_t head = PL_new_term_ref();

  while ( PL_is_functor(tail, FUNCTOR_comma2) )
  { _PL_get_arg(1, tail, head);
    if ( !meta_declaration(head) )
      return FALSE;
    _PL_get_arg(2, tail, tail);
  }

  if ( !meta_declaration(tail) )
    return FALSE;

  return TRUE;
}


#define unify_meta_argument(head, def, i) LDFUNC(unify_meta_argument, head, def, i)
static int
unify_meta_argument(DECL_LD term_t head, Definition def, int i)
{ term_t arg = PL_new_term_ref();
  int m = def->impl.any.args[i].meta;

  _PL_get_arg(i+1, head, arg);
  if ( m < 10 )
  { return PL_unify_integer(arg, m);
  } else
  { atom_t a;

    switch(m)
    { case MA_META:	a = ATOM_colon; break;
      case MA_VAR:	a = ATOM_minus; break;
      case MA_ANY:	a = ATOM_question_mark; break;
      case MA_NONVAR:	a = ATOM_plus; break;
      case MA_HAT:	a = ATOM_hat; break;
      case MA_DCG:	a = ATOM_gdiv; break;
      default:		a = NULL_ATOM; assert(0);
    }

    return PL_unify_atom(arg, a);
  }
}


static int
unify_meta_pattern(Procedure proc, term_t head)
{ GET_LD
  Definition def = proc->definition;

  if ( PL_unify_functor(head, def->functor->functor) )
  { int arity = def->functor->arity;
    int i;

    for(i=0; i<arity; i++)
    { if ( !unify_meta_argument(head, def, i) )
	return FALSE;
    }

    return TRUE;
  }

  return FALSE;
}


int
PL_meta_predicate(predicate_t proc, const char *spec_s)
{ Definition def = proc->definition;
  int arity = def->functor->arity;
  int i;
  int transparent = FALSE;
  const unsigned char *s = (const unsigned char*)spec_s;

  for(i=0; i<arity; i++, s++)
  { int spec_c = *s;
    int spec;

    switch(spec_c)
    { case '+':
	spec = MA_NONVAR;
        break;
      case '-':
	spec = MA_VAR;
        break;
      case '?':
	spec = MA_ANY;
        break;
      case ':':
	spec = MA_META;
        break;
      case '^':
	spec = MA_HAT;
        break;
      case '/':
        if ( s[1] == '/' )
	{ spec = MA_DCG;
	  s++;
	  break;
	} else
	{ goto invalid;
	}
      default:
	if ( spec_c >= '0' && spec_c <= '9' )
	{ spec = spec_c - '0';
	  break;
	}
      invalid:
        fatalError("Invalid meta-argument for %s: %s\n", procedureName(proc), spec_s);
	return FALSE;
    }

    def->impl.any.args[i].meta = spec;
    if ( MA_NEEDS_TRANSPARENT(spec) )
      transparent = TRUE;
  }

  if ( transparent )
    set(def, P_TRANSPARENT);
  else
    clear(def, P_TRANSPARENT);
  set(def, P_META);

  return TRUE;
}


void
clear_meta_declaration(Definition def)
{ int i;

  for(i=0; i<def->functor->arity; i++)
    def->impl.any.args[i].meta = MA_ANY;

  clear(def, P_META|P_TRANSPARENT);
}

#ifdef O_CLAUSEGC
		 /*******************************
		 *	     CLAUSE-GC		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Retracted clauses are  reclaimed  using   the  clause  garbage collector
(CGC). Retract itself merely sets the   erased  generation of the clause
and marks related clause indexes as `dirty'.   CGC  needs to run both to
reclaim the memory and to remove ClauseRef   objects  that point to dead
clauses and thus slow down the  search   for  clauses.  This logic is in
considerClauseGC().

CGC builds on the following components and invariants:

  - Dynamic predicates and static predicates with removed clauses are
    in the table GD->procedures.dirty.
  - CGC does:
    - Set the dirty generation of all dirty predicates to GEN_MAX
    - markPredicatesInEnvironments() finds all referenced predicates
      from frames and pushed explicitly by pushPredicateAccess()
    - Remove all ClauseRefs pointing at clauses removed before the
      oldest active generation from the clause list.  Keep them using
      lingerClauseRef() as someone may be traversing the clause list.
    - Call gcClauseRefs(), which
      - Finds all predicates whose clause-list is being traversed as
        monitored using acquire_def()/release_ref().
      - Call freeClauseRef() for each clause-ref associated with a
        not-being-traversed predicate.  Re-add the others to the
	lingering clause reference list.
      - If freeClauseRef() lowers the clause reference count to zero,
        destroy the clause.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define considerClauseGC(_) LDFUNC(considerClauseGC, _)
static int
considerClauseGC(DECL_LD)
{ size_t pending  = GD->clauses.erased_size - GD->clauses.erased_size_last;
  size_t codesize = GD->statistics.codes*sizeof(code);
  cgc_stats stats = {0};

  if ( GD->clauses.cgc_space_factor > 0 &&
       pending > codesize/GD->clauses.cgc_space_factor &&
       GD->cleaning == CLN_NORMAL )
  { DEBUG(MSG_CGC_CONSIDER,
	  Sdprintf("CGC? too much garbage: %lld bytes in %lld clauses\n",
		   (int64_t)GD->clauses.erased_size,
		   (int64_t)GD->clauses.erased));
    return TRUE;
  }

  if ( LD->statistics.inferences > LD->clauses.cgc_inferences )
  { int rgc;

    LD->clauses.cgc_inferences = LD->statistics.inferences + 500;

    stats.dirty_pred_clauses = GD->clauses.dirty;
    if ( stats.dirty_pred_clauses == (size_t)-1 )
      return FALSE;			/* already kicked in */

    if ( !cgc_thread_stats(&stats) )
      return FALSE;

    rgc =  ( (double)stats.erased_skipped >
	     (double)stats.local_size*GD->clauses.cgc_stack_factor +
	     (double)stats.dirty_pred_clauses*GD->clauses.cgc_clause_factor );
    rgc = rgc && (GD->cleaning == CLN_NORMAL);
    DEBUG(MSG_CGC_CONSIDER,
	  Sdprintf("GCG? [%s] %ld skipped; lsize=%ld; clauses=%ld\n",
		   rgc ? "Y" : " ",
		   (long)stats.erased_skipped,
		   (long)stats.local_size,
		   (long)stats.dirty_pred_clauses));

    return rgc;
  }

  return FALSE;
}

/** '$cgc_params'(-OldSpace, -OldStack, -OldClause,
 *		  +NewSpace, +NewStack, +NewClause)
 *
 * Query and set the clause GC parameters.
 */

static
PRED_IMPL("$cgc_params", 6, cgc_params, 0)
{ PRED_LD

  return ( PL_unify_integer(A1, GD->clauses.cgc_space_factor) &&
	   PL_unify_float(A2, GD->clauses.cgc_stack_factor) &&
	   PL_unify_float(A3, GD->clauses.cgc_clause_factor) &&
	   PL_get_integer_ex(A4, &GD->clauses.cgc_space_factor) &&
	   PL_get_float_ex(A5, &GD->clauses.cgc_stack_factor) &&
	   PL_get_float_ex(A6, &GD->clauses.cgc_clause_factor) );
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Dirty Definition Info handling.  This should respect

  - If a predicate is registered dirty after the preparation step
    calling ddi_reset(), none of its clauses may be collected.
  - If a DDI has seen ddi_reset(), the DDI info is valid and can be
    used to safely reclaim clauses.

ddi_add_access_gen() adds access  generations  to   the  dirty  def.  We
maintain two strategies:

  - Upto PROC_DIRTY_GENS, we simply add the predicates to the array.
  - Above, the array is an array of intervals (generation pairs)


A clause can be collected if it is invisible in all registered access
generations.

TBD: Use multiple intervals.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_DEBUG
static char *
ddi_generation_name(DirtyDefInfo ddi)
{ char s[1024];
  char *o = s;
  *o++ = '{';

  if ( false(ddi, DDI_INTERVALS) )
  { int i;

    for(i=0; i<ddi->count; i++)
    { Ssprintf(o, "%s%lld", i==0?"":" ", ddi->access[i]);
      o += strlen(o);
    }
  } else
  { int i;

    for(i=0; i<ddi->count; i++)
    { gen_t f = ddi->access[i++];
      gen_t t = ddi->access[i++];

      Ssprintf(o, "%s%lld-%lld", i==2?"":" ", f, t);
      o += strlen(o);
    }
  }
  *o++ = '}';
  *o = EOS;

  return buffer_string(s, BUF_DISCARDABLE);
}
#endif /*O_DEBUG*/

static DirtyDefInfo
ddi_new(Definition def)
{ DirtyDefInfo ddi = PL_malloc(sizeof(*ddi));

  ddi->predicate = def;
  ddi->flags = 0;
  return ddi;
}

static void
ddi_reset(DirtyDefInfo ddi)
{ ddi->count = 0;
  ddi->flags = DDI_MARKING;
}

int
ddi_contains_gen(DirtyDefInfo ddi, gen_t access)
{ if ( false(ddi, DDI_INTERVALS) )
  { int i;

    for(i=0; i<ddi->count; i++)
    { if ( ddi->access[i] == access )
	return TRUE;
    }
  } else
  { int i;

    for(i=0; i<ddi->count; )
    { if ( access >= ddi->access[i++] &&
	   access <= ddi->access[i++] )
	return TRUE;
    }
  }

  return FALSE;
}


static void
ddi_to_intervals(DirtyDefInfo ddi, gen_t access)
{ gen_t min = access;
  gen_t max = access;
  int i;

  DEBUG(MSG_CGC_GENERATION,
	Sdprintf("DDI for %s to interval: %s\n",
		 predicateName(ddi->predicate),
		 ddi_generation_name(ddi)));

  for(i=0; i<ddi->count; i++)
  { gen_t a = ddi->access[i];

    if ( a < min ) min = a;
    if ( a > max ) max = a;
  }

  ddi->access[0] = min;
  ddi->access[1] = max;
  ddi->count = 2;
  set(ddi, DDI_INTERVALS);
}


static void
ddi_interval_add_access_gen(DirtyDefInfo ddi, gen_t access)
{ if ( access < ddi->access[0] ) ddi->access[0] = access;
  if ( access > ddi->access[1] ) ddi->access[1] = access;
}


void
ddi_add_access_gen(DirtyDefInfo ddi, gen_t access)
{ if ( true(ddi, DDI_MARKING) )
  { PL_LOCK(L_CGCGEN);
    if ( !ddi_contains_gen(ddi, access) )
    { if ( false(ddi, DDI_INTERVALS) )
      { if ( ddi->count < PROC_DIRTY_GENS )
	  ddi->access[ddi->count++] = access;
	else
	  ddi_to_intervals(ddi, access);
      } else
      { ddi_interval_add_access_gen(ddi, access);
      }
    }
    PL_UNLOCK(L_CGCGEN);
  }
}

int
ddi_is_garbage(DirtyDefInfo ddi, gen_t start, Buffer tr_starts, Clause cl)
{ assert(true(ddi, DDI_MARKING));

  if ( cl->generation.erased >= start )
  { if ( cl->generation.erased >= GEN_TRANSACTION_BASE &&
	 tr_starts && !isEmptyBuffer(tr_starts) )
    { gen_t *g0  = baseBuffer(tr_starts, gen_t);
      gen_t *top = topBuffer(tr_starts, gen_t);

      for(; g0<top; g0++)
      { if ( cl->generation.erased >= *g0 &&
	     cl->generation.erased < GEN_TRMAX(*g0) )
	  return FALSE;
      }
    } else
    { return FALSE;
    }
  }

  if ( false(ddi, DDI_INTERVALS) )
  { int i;

    for(i=0; i<ddi->count; i++)
    { if ( GLOBALLY_VISIBLE_CLAUSE(cl, ddi->access[i]) )
	return FALSE;
    }
  } else
  { int i;

    assert(ddi->count == 2);

    for(i=0; i<ddi->count; )
    { gen_t f = ddi->access[i++];
      gen_t t = ddi->access[i++];

      if ( !(cl->generation.erased  < f ||
	     cl->generation.created > t) )
	return FALSE;
    }
  }

  return TRUE;
}

static gen_t
ddi_oldest_generation(DirtyDefInfo ddi)
{ gen_t oldest = GEN_MAX;

  if ( false(ddi, DDI_INTERVALS) )
  { int i;

    for(i=0; i<ddi->count; i++)
    { if ( ddi->access[i] < oldest )
	oldest = ddi->access[i];
    }
  } else
  { int i;

    for(i=0; i<ddi->count; i += 2)
    { gen_t f = ddi->access[i];

      if ( f < oldest )
	oldest = f;
    }
  }

  return oldest;
}

#ifdef O_DEBUG
static Table protectedCRefs = NULL;

static void
protectCRef(ClauseRef cref)
{ GET_LD
  void *k;

  if ( !protectedCRefs )		/* may waste a table.  Only O_DEBUG */
    COMPARE_AND_SWAP_PTR(&protectedCRefs, NULL, newHTable(64));

  if ( (k=lookupHTable(protectedCRefs, cref)) )
  { k = (void*)((intptr_t)k+1);
    updateHTable(protectedCRefs, cref, k);
    // Sdprintf("Protect %p --> %zd\n", cref, (intptr_t)k);
  } else
  { addNewHTable(protectedCRefs, cref, (void*)1);
    // Sdprintf("Protect %p\n", cref);
  }
}

static void
unprotectCRef(ClauseRef cref)
{ GET_LD
  void *k;

  if ( (k=lookupHTable(protectedCRefs, cref)) )
  { k = (void*)((intptr_t)k-1);
    if ( k )
    { updateHTable(protectedCRefs, cref, k);
      // Sdprintf("UnProtect %p --> %zd\n", cref, (intptr_t)k);
    } else
    { deleteHTable(protectedCRefs, cref);
      // Sdprintf("UnProtect %p\n", cref);
    }
  } else
  { assert(0);
  }
}

static int
isProtectedCRef(ClauseRef cref)
{ GET_LD

  return ( protectedCRefs &&
	   lookupHTable(protectedCRefs, cref) );
}
#endif /*O_DEBUG*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) We set the initial oldest_generation to "very old" (0). This ensures
that if a predicate is  registered   dirty  before clause-gc starts, the
oldest generation is 0 and thus no clause reference will be collected.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
registerDirtyDefinition(DECL_LD Definition def)
{ if ( unlikely(GD->cleaning == CLN_DATA) )
    return;

  if ( false(def, P_DIRTYREG) )
  { DirtyDefInfo ddi = ddi_new(def);

    if ( addHTable(GD->procedures.dirty, def, ddi) == ddi )
    { set(def, P_DIRTYREG);
      ATOMIC_ADD(&GD->clauses.dirty, def->impl.clauses.number_of_clauses);
    } else
    { PL_free(ddi);			/* someone else did this */
    }
  }
  if ( !isSignalledGCThread(SIG_CLAUSE_GC) &&	/* already asked for */
       !GD->clauses.cgc_active &&	/* currently running */
       considerClauseGC() )
  { if ( GD->clauses.db_erased_refs > GD->clauses.erased / 10 )
    { DEBUG(MSG_CGC_CONSIDER,
	    Sdprintf("CGC: %ld of %ld erased clauses has a clause ref; "
		     "asking for AGC\n",
		     (long)GD->clauses.db_erased_refs,
		     (long)GD->clauses.erased));
      signalGCThread(SIG_ATOM_GC);
    }
    signalGCThread(SIG_CLAUSE_GC);
  }
}

static void
unregisterDirtyDefinition(Definition def)
{ DirtyDefInfo ddi;

  if ( (ddi=deleteHTable(GD->procedures.dirty, def)) )
  { PL_free(ddi);
    clear(def, P_DIRTYREG);
    ATOMIC_SUB(&GD->clauses.dirty, def->impl.clauses.number_of_clauses);
  }
}


static void
maybeUnregisterDirtyDefinition(Definition def)
{ if ( true(def, P_DIRTYREG) &&
       def->impl.clauses.erased_clauses == 0 )
  { unregisterDirtyDefinition(def);
  }

  if ( true(def, P_ERASED) )
    delayedDestroyDefinition(def);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) We set the initial generation to   GEN_MAX  to know which predicates
have been marked. We can only reclaim   clauses  that were erased before
the start generation of the clause garbage collector.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

foreign_t
pl_garbage_collect_clauses(void)
{ GET_LD
  int rc = TRUE;

  if ( GD->procedures.dirty->size > 0 &&
       COMPARE_AND_SWAP_INT(&GD->clauses.cgc_active, FALSE, TRUE) )
  { size_t removed = 0;
    size_t erased_pending = GD->clauses.erased_size;
    double gct, t0 = ThreadCPUTime(CPU_USER);
    gen_t start_gen = global_generation();
    int verbose = truePrologFlag(PLFLAG_TRACE_GC) && !LD->in_print_message;
    tmp_buffer tr_starts;

    if ( verbose )
    { if ( (rc=printMessage(ATOM_informational,
			    PL_FUNCTOR_CHARS, "cgc", 1,
			      PL_CHARS, "start")) == FALSE )
	goto out;
    }

    DEBUG(MSG_CGC, Sdprintf("CGC @ %lld ... ", start_gen));
    DEBUG(MSG_CGC_STACK,
	  { Sdprintf("CGC @ %lld ... ", start_gen);
	    PL_backtrace(5,0);
	  });

					/* sanity-check */
    FOR_TABLE(GD->procedures.dirty, n, v)
    { DirtyDefInfo ddi = v;

      DEBUG(CHK_SECURE,
	    { Definition def = n;
	      LOCKDEF(def);
	      checkDefinition(def);
	      UNLOCKDEF(def);
	    });
      ddi_reset(ddi);			  /* see (*) */
    }

    initBuffer(&tr_starts);
    markPredicatesInEnvironments(LD, (Buffer)&tr_starts);
#ifdef O_PLMT
    forThreadLocalDataUnsuspended(markPredicatesInEnvironments,
				  (Buffer)&tr_starts);
#endif

    DEBUG(MSG_CGC, Sdprintf("(marking done)\n"));

    FOR_TABLE(GD->procedures.dirty, n, v)
    { Definition def = n;
      DirtyDefInfo ddi = v;

      if ( false(def, P_FOREIGN) &&
	   def->impl.clauses.erased_clauses > 0 )
      { size_t del = cleanDefinition(def, ddi,
				     start_gen, (Buffer)&tr_starts,
				     &rc);

	removed += del;
	DEBUG(MSG_CGC_PRED,
	      Sdprintf("cleanDefinition(%s, %s): "
		       "%zd clauses (left %d)\n",
		       predicateName(def),
		       ddi_generation_name(ddi),
		       del,
		       (int)def->impl.clauses.erased_clauses));
      }

      maybeUnregisterDirtyDefinition(def);
    }

    discardBuffer(&tr_starts);
    gcClauseRefs();
    GD->clauses.cgc_count++;
    GD->clauses.cgc_reclaimed	+= removed;
    GD->clauses.cgc_time        += (gct=ThreadCPUTime(CPU_USER) - t0);
    GD->clauses.erased_size_last = GD->clauses.erased_size;

    DEBUG(MSG_CGC, Sdprintf("CGC: removed %ld clauses "
			    "(%ld bytes reclaimed, %ld pending) in %2f sec.\n",
			    (long)removed,
			    (long)erased_pending - GD->clauses.erased_size,
			    (long)GD->clauses.erased_size,
			    gct));

    if ( verbose )
      rc = printMessage(
	      ATOM_informational,
	      PL_FUNCTOR_CHARS, "cgc", 1,
		PL_FUNCTOR_CHARS, "done", 4,
		  PL_INT64,  (int64_t)removed,
		  PL_INT64,  (int64_t)(erased_pending - GD->clauses.erased_size),
		  PL_INT64,  (int64_t)GD->clauses.erased_size,
		  PL_DOUBLE, gct);

  out:
    GD->clauses.cgc_active = FALSE;
  }

  return rc;
}

#endif /*O_CLAUSEGC*/

#ifdef O_DEBUG
		 /*******************************
		 *	    CHECKING		*
		 *******************************/

word
pl_check_definition(term_t spec)
{ GET_LD
  Procedure proc;
  Definition def;
  int nclauses = 0;
  int nerased = 0;
  int nindexable = 0;
  ClauseRef cref;

  if ( !get_procedure(spec, &proc, 0, GP_FIND) )
    return Sdprintf("$check_definition/1: can't find definition");
  def = getProcDefinition(proc);

  if ( true(def, P_FOREIGN) )
    succeed;

  acquire_def(def);
  for(cref = def->impl.clauses.first_clause; cref; cref = cref->next)
  { Clause clause = cref->value.clause;

    if ( cref->d.key == 0 )
      nindexable++;

    if ( false(clause, CL_ERASED) )
      nclauses++;
    else
      nerased++;
  }
  release_def(def);

  if ( nerased != def->impl.clauses.erased_clauses )
    Sdprintf("%s has %d erased clauses, claims %d\n",
	     predicateName(def), nerased, def->impl.clauses.erased_clauses);

  checkClauseIndexSizes(def, nindexable);

  if ( def->impl.clauses.number_of_clauses != nclauses )
    Sdprintf("%s has inconsistent number_of_clauses (%d, should be %d)",
	     predicateName(def), def->impl.clauses.number_of_clauses, nclauses);

  succeed;
}
#endif /*O_DEBUG*/

		/********************************
		*     UNDEFINED PROCEDURES      *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A dynamic call to `f' in `m' has to be made (via call/1 or from C). This
procedure returns the procedure to be run.   If no such procedure exists
an undefined procedure is created and returned. In this case interpret()
will later call trapUndefined() to generate   an  error message (or link
the procedure from the library via autoload).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Procedure
resolveProcedure(DECL_LD functor_t f, Module module)
{ Procedure proc;

  if ( (proc = visibleProcedure(f, module)) )
    return proc;

  return lookupProcedure(f, module);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
autoImport() tries to autoimport  f  into   module  `m'  and returns the
definition if this is possible.

PROBLEM: I'm not entirely  sure  it  is  save  to  deallocated  the  old
definition  structure  in  all  cases.   It  is  not  member of any heap
structure, thus sofar everything  is  alright.   After  a  dynamic  link
interpret()  picks up the new definition pointer, thus this should be ok
as well.  Any other C-code that  does  nasty  things  (non-deterministic
code  perhaps,  calls  indirect via C? (I do recall once conciously have
decided its not save, but can't recall why ...)

Its definitely not safe in MT context as   others  may be racing for the
definition.  How  do  we  get   this    working   without   locking  the
proc->definition fetch?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Definition
autoImport(functor_t f, Module m)
{ GET_LD
  Procedure proc;
  Definition def, odef;
  ListCell c;
					/* Defined: no problem */
  if ( (proc = isCurrentProcedure(f, m)) )
  { if ( isDefinedProcedure(proc) )
      return proc->definition;
    if ( true(proc->definition, P_AUTOLOAD) )
      return NULL;
  }

  for(c=m->supers; c; c=c->next)
  { Module s = c->value;

    if ( (def = autoImport(f, s)) )
      goto found;
  }
  return NULL;

found:
  if ( proc == NULL )			/* Create header if not there */
  { if ( !(proc = lookupProcedure(f, m)) )
      return NULL;
  }
					/* Now, take the lock also used */
					/* by lookupProcedure().  Note */
					/* that another thread may have */
					/* done the job for us. */
  LOCKMODULE(m);
  if ( (odef=proc->definition) != def )	/* Nope, we must link the def */
  { shareDefinition(def);
    proc->definition = def;

    if ( unshareDefinition(odef) == 0 )
    {
#ifdef O_PLMT
      PL_LOCK(L_THREAD);
      if ( (GD->statistics.threads_created -
	    GD->statistics.threads_finished) == 1 )
      { DEBUG(MSG_PROC_COUNT, Sdprintf("Unalloc %s\n", predicateName(odef)));
	unregisterDirtyDefinition(odef);
	unallocDefinition(odef);
      } else
      { DEBUG(MSG_PROC, Sdprintf("autoImport(%s,%s): Linger %s (%p)\n",
				 functorName(f), PL_atom_chars(m->name),
				 predicateName(odef), odef));
	lingerDefinition(odef);
      }
      PL_UNLOCK(L_THREAD);
#else
      unallocDefinition(odef);
#endif
    }
  }
  UNLOCKMODULE(m);

  return def;
}


#define test_autoload_loop(def) LDFUNC(test_autoload_loop, def)
static int
test_autoload_loop(DECL_LD Definition def)
{ DefinitionChain ch;

  for(ch=LD->autoload.nesting; ch; ch=ch->next)
  { if ( ch->definition == def )
    { LD->autoload.loop = def;

      Sdprintf("ERROR: autoload loop:\n");
      Sdprintf("ERROR:   %s\n", predicateName(def));

      for(ch=LD->autoload.nesting; ch; ch=ch->next)
      { Sdprintf("ERROR:   %s\n", predicateName(ch->definition));
	if ( ch->definition == def )
	  break;
      }

      return FALSE;
    }
  }

  return TRUE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Call the autoloader for the given definition.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static atom_t
autoLoader(Definition def)
{ GET_LD
  fid_t  cid;
  term_t argv;
  qid_t qid;
  atom_t answer = ATOM_nil;
  struct definition_chain cell;

  if ( !GD->procedures.undefinterc4 )
    GD->procedures.undefinterc4 = PL_pred(FUNCTOR_undefinterc4,
					  MODULE_system);

  if ( !(cid  = PL_open_foreign_frame()) ||
       !(argv = PL_new_term_refs(4)) )
    return answer;

  PL_put_atom(    argv+0, def->module->name);
  PL_put_atom(    argv+1, def->functor->name);
  PL_put_integer( argv+2, def->functor->arity);

  push_input_context(ATOM_autoload);
  cell.definition = def;
  cell.next = LD->autoload.nesting;
  LD->autoload.nesting = &cell;
  if ( (qid = PL_open_query(MODULE_system, PL_Q_NODEBUG|PL_Q_PASS_EXCEPTION,
			    GD->procedures.undefinterc4, argv)) )
  { if ( PL_next_solution(qid) )
      PL_get_atom(argv+3, &answer);
    PL_close_query(qid);
  } else if ( PL_exception(0) )
  { PL_clear_exception();
  }
  LD->autoload.nesting = LD->autoload.nesting->next;
  pop_input_context();
  PL_discard_foreign_frame(cid);

  return answer;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
According to Paulo Moura, predicates defined either dynamic, multifile or
discontiguous should not cause an undefined predicate warning.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Definition
trapUndefined(DECL_LD Definition def)
{ int retry_times = 0;
  Definition newdef;
  Module module = def->module;
  FunctorDef functor = def->functor;

  retry:
					/* Auto import */
  if ( (newdef = autoImport(functor->functor, module)) )
    return newdef;
					/* Pred/Module does not want to trap */
  if ( true(def, PROC_DEFINED) ||
       getUnknownModule(module) == UNKNOWN_FAIL )
    return def;

  DEBUG(5, Sdprintf("trapUndefined(%s)\n", predicateName(def)));

					/* Trap via exception/3 */
  if ( truePrologFlag(PLFLAG_AUTOLOAD) && !GD->bootsession )
  { if ( !test_autoload_loop(def) )
    { return def;
    } else
    { atom_t answer = autoLoader(def);

      def = lookupDefinition(functor->functor, module);

      if ( answer == ATOM_fail )
      { return def;
      } else if ( answer == ATOM_error )
      { goto error;
      } else if ( answer == ATOM_retry )
      { if ( retry_times++ )
	{ warning("[Thread %d]: exception handler failed to define %s\n",
		  PL_thread_self(),
		  predicateName(def));
	  return def;
	}
	goto retry;
      }
    }
  }
				/* No one wants to intercept */
error:
  if ( GD->bootsession )
  { sysError("Undefined predicate: %s", predicateName(def));
  } else
  { createUndefSupervisor(def);
  }

  return def;
}


		 /*******************************
		 *	  REQUIRE SUPPORT	*
		 *******************************/

word
pl_require(term_t pred)
{ Procedure proc;

  if ( !get_procedure(pred, &proc, 0, GP_RESOLVE) )
    return get_procedure(pred, &proc, 0, GP_DEFINE);

  succeed;
}


		/********************************
		*            RETRACT            *
		*********************************/

typedef struct
{ Definition def;
  struct clause_choice chp;
  int allocated;
} retract_context;

static retract_context *
alloc_retract_context(retract_context *ctx0)
{ retract_context *ctx = allocForeignState(sizeof(*ctx));

  *ctx = *ctx0;
  ctx->allocated = TRUE;

  return ctx;
}

#define free_retract_context(ctx) LDFUNC(free_retract_context, ctx)
static void
free_retract_context(DECL_LD retract_context *ctx)
{ popPredicateAccess(ctx->def);
  leaveDefinition(ctx->def);

  if ( ctx->allocated )
    freeForeignState(ctx, sizeof(*ctx));
}

static
PRED_IMPL("retract", 1, retract,
	  PL_FA_TRANSPARENT|PL_FA_NONDETERMINISTIC|PL_FA_ISO)
{ PRED_LD
  term_t term = A1;
  retract_context ctxbuf;
  retract_context *ctx;
  ClauseRef cref;

  if ( CTX_CNTRL == FRG_CUTTED )
  { ctx = CTX_PTR;

    unprotectCRef(ctx->chp.cref);
    free_retract_context(ctx);

    return TRUE;
  } else
  { Module m = NULL;
    term_t cl = PL_new_term_ref();
    term_t head = PL_new_term_ref();
    term_t body = PL_new_term_ref();
    Word argv;
    atom_t b;
    fid_t fid;
    definition_ref *dref = NULL;
    int rc;
    int flags = 0;

    if ( !PL_strip_module_ex(term, &m, cl) ||
	 !get_head_and_body_clause(cl, head, body, &m, &flags) )
      return FALSE;
    if ( PL_get_atom(body, &b) && b == ATOM_true )
      rc = PL_put_term(cl, head);
    else
      rc = PL_cons_functor(cl, FUNCTOR_prove2, head, body);
    if ( !rc )
      return FALSE;

    argv = valTermRef(head);
    deRef(argv);
    if ( isTerm(*argv) )		/* retract(foobar(a1, ...)) */
      argv = argTermP(*argv, 0);
    else
      argv = NULL;			/* retract(foobar) */

    if ( CTX_CNTRL == FRG_FIRST_CALL )
    { functor_t fd;
      Procedure proc;
      Definition def;

      if ( !PL_get_functor(head, &fd) )
	return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, head);
      if ( !(proc = isCurrentProcedure(fd, m)) )
      { checkModifySystemProc(fd);
	fail;
      }

      def = getProcDefinition(proc);

      if ( true(def, P_FOREIGN) )
	return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, proc);
      if ( false(def, P_DYNAMIC) )
      { if ( isDefinedProcedure(proc) )
	  return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, proc);
	setDynamicDefinition(def, TRUE); /* implicit */
	fail;				/* no clauses */
      }

      enterDefinition(def);			/* reference the predicate */
      dref = pushPredicateAccessObj(def);
      setGenerationFrameVal(environment_frame, dref->generation);
      cref = firstClause(argv, environment_frame, def, &ctxbuf.chp);
      DEBUG(MSG_CGC_RETRACT,
	    Sdprintf("Started retract from %s at gen = %s\n",
		     predicateName(def),
		     generationName(generationFrame(environment_frame))));
      if ( !cref )
      { popPredicateAccess(def);
	leaveDefinition(def);
	fail;
      }

      ctx = &ctxbuf;
      ctx->def = def;
      ctx->allocated = 0;
    } else
    { ctx  = CTX_PTR;
      DEBUG(MSG_CGC_RETRACT,
	    Sdprintf("Retry retract from %s at gen = %lld\n",
		     predicateName(ctx->def),
		     generationFrame(environment_frame)));
      unprotectCRef(ctx->chp.cref);
      cref = nextClause(&ctx->chp, argv, environment_frame, ctx->def);
    }

    if ( !(fid = PL_open_foreign_frame()) )
    { free_retract_context(ctx);
      return FALSE;
    }

    while( cref )
    { Clause clause = cref->value.clause;

      if ( (clause->flags&CLAUSE_SSU_FLAGS) == (flags&CLAUSE_SSU_FLAGS) &&
	   decompile(clause, cl, 0) )
      { if ( retractClauseDefinition(ctx->def, clause, TRUE) ||
	     CTX_CNTRL != FRG_FIRST_CALL )
	{ if ( !ctx->chp.cref )		/* deterministic last one */
	  { free_retract_context(ctx);
	    PL_close_foreign_frame(fid);
	    return TRUE;
	  }

	  if ( ctx == &ctxbuf )		/* non-determinisic; save state */
	    ctx = alloc_retract_context(ctx);

	  DEBUG(0,
		assert(visibleClause(
			   ctx->chp.cref->value.clause,
			   generationFrame(environment_frame))));
	  protectCRef(ctx->chp.cref);

	  PL_close_foreign_frame(fid);
	  ForeignRedoPtr(ctx);
	} else
	{ setGenerationFrame(environment_frame);
	  DEBUG(MSG_CGC_RETRACT,
		Sdprintf("Retract: first clause deleted; set gen to %lld\n",
			 generationFrame(environment_frame)));
	}
      }

      if ( PL_exception(0) )
	break;

      PL_rewind_foreign_frame(fid);
      cref = nextClause(&ctx->chp, argv, environment_frame, ctx->def);
    }

    PL_close_foreign_frame(fid);
    free_retract_context(ctx);
    return FALSE;
  }
}


#define allVars(argc, argv) LDFUNC(allVars, argc, argv)
static int
allVars(DECL_LD int argc, Word argv)
{ int i, r, allvars = TRUE;
  Word *reset = alloca(argc*sizeof(Word));

  for(i=0; i<argc; i++)
  { Word p2;

    deRef2(argv+i, p2);
    if ( isVar(*p2) )
    { reset[i] = p2;
      *p2 = ATOM_nil;
    } else
    { allvars = FALSE;
      break;
    }
  }

  for(r=0; r<i; r++)
    setVar(*reset[r]);

  return allvars;
}


static
PRED_IMPL("retractall", 1, retractall, PL_FA_NONDETERMINISTIC|PL_FA_ISO)
{ GET_LD
  term_t head = A1;
  term_t thehead = PL_new_term_ref();
  Procedure proc;
  Definition def;
  definition_ref *dref;
  ClauseRef cref;
  Word argv;
  int allvars = TRUE;
  fid_t fid;
  int rc = TRUE;

  if ( !get_procedure(head, &proc, thehead, GP_CREATE) )
    fail;

  def = getProcDefinition(proc);
  if ( true(def, P_FOREIGN) )
    return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, proc);
  if ( false(def, P_DYNAMIC) )
  { if ( isDefinedProcedure(proc) )
      return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, proc);
    if ( !setDynamicDefinition(def, TRUE) )
      fail;
    succeed;				/* nothing to retract */
  }

  if ( !retractall_event(def, thehead, FUNCTOR_start1) )
    return FALSE;

  argv = valTermRef(thehead);
  deRef(argv);
  if ( isTerm(*argv) )
  { int arity = arityTerm(*argv);
    argv = argTermP(*argv, 0);

    allvars = allVars(arity, argv);
  } else
  { allvars = TRUE;
    argv = NULL;
  }

  if ( !(dref=pushPredicateAccessObj(def)) )
    return FALSE;
  setGenerationFrameVal(environment_frame, dref->generation);
  enterDefinition(def);
  fid = PL_open_foreign_frame();

  DEBUG(CHK_SECURE,
	LOCKDEF(def);
	checkDefinition(def);
        UNLOCKDEF(def));
  if ( allvars )
  { gen_t gen = generationFrame(environment_frame);

    acquire_def(def);
    for(cref = def->impl.clauses.first_clause; cref; cref = cref->next)
    { if ( visibleClauseCNT(cref->value.clause, gen) )
      { if ( !(rc=retractClauseDefinition(def, cref->value.clause, TRUE)) )
	{ if ( PL_exception(0) )
	    break;
	}
      }
    }
    release_def(def);
    rc = TRUE;
  } else
  { struct clause_choice chp;

    if ( !(cref = firstClause(argv, environment_frame, def, &chp)) )
    { popPredicateAccess(def);
      leaveDefinition(def);
      return TRUE;
    }

    while( cref )
    { if ( decompileHead(cref->value.clause, thehead) )
      { if ( !(rc=retractClauseDefinition(def, cref->value.clause, TRUE)) )
	{ if ( PL_exception(0) )
	    break;
	}
      } else
      { if ( PL_exception(0) )
	{ rc = FALSE;
	  break;
	}
      }

      PL_rewind_foreign_frame(fid);

      if ( !chp.cref )
      { popPredicateAccess(def);
	leaveDefinition(def);
	return TRUE;
      }

      if ( argv )				/* may be shifted */
      { argv = valTermRef(thehead);
	argv = argTermP(*argv, 0);
      }

      cref = nextClause(&chp, argv, environment_frame, def);
      rc = TRUE;
    }
  }
  popPredicateAccess(def);
  leaveDefinition(def);
  DEBUG(CHK_SECURE,
	LOCKDEF(def);
	checkDefinition(def);
	UNLOCKDEF(def));

  if ( rc )
    rc = retractall_event(def, thehead, FUNCTOR_end1);

  return rc;
}

		/********************************
		*       PROLOG PREDICATES       *
		*********************************/

static word
do_abolish(Module m, term_t atom, term_t arity)
{ GET_LD
  functor_t f;
  Procedure proc;
  atom_t name;
  int a = 0;

  if ( !PL_get_atom_ex(atom, &name) ||
       !get_arity(arity, 0, MAXARITY, &a) )
    fail;

  if ( !(f = isCurrentFunctor(name, a)) )
    succeed;
  if ( !checkModifySystemProc(f) )
    fail;
  if ( !(proc = isCurrentProcedure(f, m)) )
    succeed;

  if ( truePrologFlag(PLFLAG_ISO) && false(proc->definition, P_DYNAMIC) )
    return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, proc);

  return abolishProcedure(proc, m);
}


word
pl_abolish(term_t name, term_t arity)	/* Name, Arity */
{ GET_LD
  Module m = NULL;

  return ( PL_strip_module(name, &m, name) &&
	   do_abolish(m, name, arity)
	 );
}


word
pl_abolish1(term_t spec)		/* Name/Arity */
{ GET_LD
  term_t name  = PL_new_term_ref();
  term_t arity = PL_new_term_ref();
  Module m = NULL;

  if ( !PL_strip_module(spec, &m, spec) )
    return FALSE;

  if ( !PL_is_functor(spec, FUNCTOR_divide2) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_predicate_indicator, spec);

  _PL_get_arg(1, spec, name);
  _PL_get_arg(2, spec, arity);

  return do_abolish(m, name, arity);
}


typedef struct patt_mask
{ atom_t	key;
  uint64_t      mask;
} patt_mask;

static const patt_mask patt_masks[] =
{ { ATOM_dynamic,	   P_DYNAMIC },
  { ATOM_multifile,	   P_MULTIFILE },
  { ATOM_locked,	   P_LOCKED },
  { ATOM_system,	   P_LOCKED },		/* compatibility */
  { ATOM_spy,		   SPY_ME },
  { ATOM_trace,		   TRACE_ME },
  { ATOM_hide_childs,	   HIDE_CHILDS },
  { ATOM_transparent,	   P_TRANSPARENT },
  { ATOM_discontiguous,	   P_DISCONTIGUOUS },
  { ATOM_volatile,	   P_VOLATILE },
  { ATOM_thread_local,	   P_THREAD_LOCAL },
  { ATOM_noprofile,	   P_NOPROFILE },
  { ATOM_iso,		   P_ISO },
  { ATOM_public,	   P_PUBLIC },
  { ATOM_non_terminal,	   P_NON_TERMINAL },
  { ATOM_quasi_quotation_syntax, P_QUASI_QUOTATION_SYNTAX },
  { ATOM_clausable,	   P_CLAUSABLE },
  { ATOM_autoload,	   P_AUTOLOAD },
  { ATOM_ssu,		   P_SSU_DET },
  { ATOM_det,		   P_DET },
  { ATOM_sig_atomic,	   P_SIG_ATOMIC },
  { (atom_t)0,		   0 }
};

static uint64_t
attribute_mask(atom_t key)
{ const patt_mask *p;

  for(p=patt_masks; p->key; p++)
  { if ( p->key == key )
      return p->mask;
  }

  { GET_LD
    term_t t;

    return ( (t = PL_new_term_ref()) &&
	     PL_put_atom(t, key) &&
	     PL_domain_error("predicate_property", t)
	   );
  }
}


#define num_visible_clauses(def, key, gen) LDFUNC(num_visible_clauses, def, key, gen)
static size_t
num_visible_clauses(DECL_LD Definition def, atom_t key, gen_t gen)
{ size_t count;

  if ( key == ATOM_number_of_clauses )
    count = def->impl.clauses.number_of_clauses;
  else
    count = def->impl.clauses.number_of_rules;

  if ( count > 0 &&
       (LD->reload.generation != GEN_INVALID || true(def, P_DYNAMIC)) )
  { ClauseRef c;

    if ( !gen )
      gen = current_generation(def);

    count = 0;
    acquire_def(def);
    for(c = def->impl.clauses.first_clause; c; c = c->next)
    { Clause cl = c->value.clause;
      if ( key == ATOM_number_of_rules && true(cl, UNIT_CLAUSE) )
        continue;
      if ( visibleClause(cl, gen) )
        count++;
    }
    release_def(def);
  }

  return count;
}


size_t
sizeof_predicate(Definition def)
{ GET_LD
  size_t size = sizeof(*def);

  size += sizeof_supervisor(def->codes);

  if ( false(def, P_FOREIGN) )
  { ClauseRef c;

    acquire_def(def);
    for(c = def->impl.clauses.first_clause; c; c = c->next)
    { Clause cl = c->value.clause;

      size += sizeofClause(cl->code_size);
      size += SIZEOF_CREF_CLAUSE;
    }
    release_def(def);

    size += sizeofClauseIndexes(def);
  }

  return size;
}


static
PRED_IMPL("$get_predicate_attribute", 3, get_predicate_attribute,
	  PL_FA_TRANSPARENT)
{ PRED_LD
  term_t pred  = A1;
  term_t what  = A2;
  term_t value = A3;
  Procedure proc;
  Definition def;
  functor_t fd;
  atom_t key;
  Module module = (Module) NULL;
  uint64_t att;
  term_t head = PL_new_term_ref();

  if ( !PL_strip_module(pred, &module, head) ||
       !PL_get_functor(head, &fd) ||
       ( !(proc = visibleProcedure(fd, module)) &&
	 !(proc = isCurrentProcedure(fd, module)) ) )
    fail;

  def = proc->definition;

  if ( !PL_get_atom(what, &key) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, what);

  if ( key == ATOM_imported )
  { if ( module == def->module )
      fail;
    return PL_unify_atom(value, def->module->name);
  } else if ( key == ATOM_indexed )
  { return unify_index_pattern(proc, value);
  } else if ( key == ATOM_meta_predicate )
  { if ( false(def, P_META) )
      fail;
    return unify_meta_pattern(proc, value);
  } else if ( key == ATOM_exported )
  { return PL_unify_integer(value, isPublicModule(module, proc));
  } else if ( key == ATOM_defined )
  { int d;

    if ( isDefinedProcedure(proc) )
      d = 1;
    else
      d = 0;

    return PL_unify_integer(value, d);
  } else if ( key == ATOM_line_count || key == ATOM_file )
  { int line;
    Clause clause;
    int rc = FALSE;

    if ( false(def, P_FOREIGN|P_THREAD_LOCAL) )
    { acquire_def(def);
      if ( def->impl.clauses.first_clause &&
	   (clause = def->impl.clauses.first_clause->value.clause) &&
	   (line=clause->line_no) )
      { if ( key == ATOM_line_count )
	{ rc = PL_unify_integer(value, line);
	} else
	{ SourceFile sf = indexToSourceFile(clause->source_no);

	  if ( sf )
	    rc = PL_unify_atom(value, sf->name);
	}
      }
      release_def(def);
    }

    return rc;
  } else if ( key == ATOM_foreign )
  { return PL_unify_integer(value, true(def, P_FOREIGN) ? 1 : 0);
  } else if ( key == ATOM_number_of_clauses )
  { size_t num_clauses;
    if ( def->flags & P_FOREIGN )
      fail;

    def = getProcDefinition(proc);
    num_clauses = num_visible_clauses(def, key, 0);
    if ( num_clauses == 0 && false(def, P_DYNAMIC) )
      fail;
    return PL_unify_int64(value, num_clauses);
  } else if ( key == ATOM_last_modified_generation )
  { gen_t g;

    if ( def->flags & P_FOREIGN )
      fail;
    def = getProcDefinition(proc);
    if ( true(def, P_DYNAMIC) && LD->transaction.generation )
      g = transaction_last_modified_predicate(def);
    else
      g = def->last_modified;

    return PL_unify_int64(value, g);
  } else if ( key == ATOM_number_of_rules )
  { if ( def->flags & P_FOREIGN )
      fail;

    def = getProcDefinition(proc);
    if ( def->impl.clauses.number_of_clauses == 0 && false(def, P_DYNAMIC) )
      fail;
    return PL_unify_integer(value, num_visible_clauses(def, key, 0));
  } else if ( key == ATOM_size )
  { def = getProcDefinition(proc);
    return PL_unify_integer(value, sizeof_predicate(def));
  } else if ( tbl_is_predicate_attribute(key) )
  { return tbl_get_predicate_attribute(def, key, value);
  } else if ( (att = attribute_mask(key)) )
  { return PL_unify_integer(value, (def->flags & att) ? 1 : 0);
  } else
  { return FALSE;
  }
}


static int
setDetDefinition(Definition def, bool isdet)
{ if ( ( isdet &&  true(def, P_DET)) ||
       (!isdet && false(def, P_DET)) )
    return TRUE;

  if ( isdet )				/* nondet --> det */
  { set(def, P_DET);
    freeCodesDefinition(def, TRUE);	/* reset to S_VIRGIN */
  } else				/* det --> nondet */
  { clear(def, P_DET);
    freeCodesDefinition(def, TRUE);	/* reset to S_VIRGIN */
  }

  return TRUE;
}


static int
setDynamicDefinition_unlocked(Definition def, bool isdyn)
{ GET_LD

  if ( ( isdyn &&  true(def, P_DYNAMIC)) ||
       (!isdyn && false(def, P_DYNAMIC)) )
    return TRUE;

  if ( isdyn )				/* static --> dynamic */
  { if ( truePrologFlag(PLFLAG_PROTECT_STATIC_CODE) &&
	 hasClausesDefinition(def) )
      return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PREDICATE, def);

    set(def, P_DYNAMIC);
    freeCodesDefinition(def, TRUE);	/* reset to S_VIRGIN */
  } else				/* dynamic --> static */
  { clear(def, P_DYNAMIC);
    freeCodesDefinition(def, TRUE);	/* reset to S_VIRGIN */
  }

  return TRUE;
}


int
setDynamicDefinition(Definition def, bool isdyn)
{ int rc;

  LOCKDEF(def);
  rc = setDynamicDefinition_unlocked(def, isdyn);
  UNLOCKDEF(def);

  return rc;
}

int
setThreadLocalDefinition(Definition def, bool val)
{
#ifdef O_PLMT

  LOCKDEF(def);
  if ( (val && true(def, P_THREAD_LOCAL)) ||
       (!val && false(def, P_THREAD_LOCAL)) )
  { UNLOCKDEF(def);
    return TRUE;
  }

  if ( val )				/* static --> local */
  { if ( def->impl.clauses.first_clause )
    { UNLOCKDEF(def);
      return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PREDICATE, def);
    }

    def->impl.local.local = new_ldef_vector();
    MEMORY_RELEASE();
    set(def, P_DYNAMIC|P_VOLATILE|P_THREAD_LOCAL);
    def->codes = SUPERVISOR(thread_local);

    UNLOCKDEF(def);
    return TRUE;
  } else				/* local --> static */
  { UNLOCKDEF(def);
    return PL_error(NULL, 0, "predicate is thread-local",
		    ERR_MODIFY_STATIC_PREDICATE, def);
  }
#else
  setDynamicDefinition(def, val);

  if ( val )
    set(def, P_VOLATILE|P_THREAD_LOCAL);
  else
    clear(def, P_VOLATILE|P_THREAD_LOCAL);

  succeed;
#endif
}


static int
setClausableDefinition(Definition def, int val)
{ GET_LD

  if ( val )
  { if ( truePrologFlag(PLFLAG_PROTECT_STATIC_CODE) &&
	 hasClausesDefinition(def) )
      return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PREDICATE, def);
    set(def, P_CLAUSABLE);
  } else
  { clear(def, P_CLAUSABLE);
  }

  return TRUE;
}

int
setAttrDefinition(Definition def, uint64_t attr, int val)
{ int rc;

  if ( attr == P_DYNAMIC )
  { rc = setDynamicDefinition(def, val);
  } else if ( attr == P_THREAD_LOCAL )
  { rc = setThreadLocalDefinition(def, val);
  } else if ( attr == P_CLAUSABLE )
  { rc = setClausableDefinition(def, val);
  } else if ( attr == P_DET )
  { rc = setDetDefinition(def, val);
  } else
  { if ( !val )
    { clear(def, attr);
    } else
    { set(def, attr);
    }

    rc = TRUE;
  }

  return rc;
}


#define get_bool_or_int_ex(t, val) LDFUNC(get_bool_or_int_ex, t, val)
static int
get_bool_or_int_ex(DECL_LD term_t t, int *val)
{ if ( PL_get_bool(t, val) )
    return TRUE;
  if ( PL_get_integer(t, val) && !(*val & ~1) )
    return TRUE;			/* accept 0 and 1 */
  return PL_get_bool_ex(t, val);	/* generate an error */
}


static
PRED_IMPL("$set_predicate_attribute", 3, set_predicate_attribute,
	  PL_FA_TRANSPARENT)
{ PRED_LD
  term_t pred  = A1;
  term_t what  = A2;
  term_t value = A3;
  Procedure proc;
  Definition def;
  atom_t key;
  int val;
  uint64_t att;

  if ( !PL_get_atom_ex(what, &key) )
    return FALSE;
  if ( tbl_is_predicate_attribute(key) )
  { if ( get_procedure(pred, &proc, 0, GP_DEFINE|GP_NAMEARITY) )
      return tbl_set_predicate_attribute(proc->definition, key, value);

    return FALSE;
  }

  if ( !get_bool_or_int_ex(value, &val) ||
       !(att = attribute_mask(key)) )
    return FALSE;

  if ( (att&SPY_ME) )
  { if ( !get_procedure(pred, &proc, 0, GP_RESOLVE) )
      fail;
  } else
  { if ( !get_procedure(pred, &proc, 0, GP_DEFINE|GP_NAMEARITY) )
      fail;
  }
  def = proc->definition;

  if ( ReadingSource && MODULE_parse == def->module )
  { SourceFile sf = lookupSourceFile(source_file_name, TRUE);
    int rc = setAttrProcedureSource(sf, proc, att, val);
    releaseSourceFile(sf);
    return rc;
  } else
  { return setAttrDefinition(def, att, val);
  }
}


static
PRED_IMPL("$default_predicate", 2, default_predicate, PL_FA_TRANSPARENT)
{ Procedure p1, p2;

  if ( get_procedure(A1, &p1, 0, GP_FIND) &&
       get_procedure(A2, &p2, 0, GP_FIND) )
  { if ( p1->definition == p2->definition || !isDefinedProcedure(p1) )
      succeed;
  }

  fail;
}


static
PRED_IMPL("$get_clause_attribute", 3, get_clause_attribute, 0)
{ GET_LD
  Clause clause;
  atom_t a;

  term_t ref   = A1;
  term_t att   = A2;
  term_t value = A3;

  if ( !PL_get_clref(ref, &clause) ||
       !PL_get_atom_ex(att, &a) )
    return FALSE;

  if ( a == ATOM_line_count )
  { if ( clause->line_no )
      return PL_unify_integer(value, clause->line_no);
  } else if ( a == ATOM_file )
  { SourceFile sf = indexToSourceFile(clause->source_no);

    if ( sf )
      return PL_unify_atom(value, sf->name);
  } else if ( a == ATOM_owner )
  { SourceFile sf = indexToSourceFile(clause->owner_no);

    if ( sf )
      return PL_unify_atom(value, sf->name);
  } else if ( a == ATOM_size )
  { size_t size = sizeofClause(clause->code_size);

    return PL_unify_int64(value, size);
  } else if ( a == ATOM_fact )
  { return PL_unify_atom(value,
			 true(clause, UNIT_CLAUSE) ? ATOM_true
						   : ATOM_false);
  } else if ( a == ATOM_erased )
  { atom_t erased;

    if ( visibleClause(clause, generationFrame(environment_frame)) )
      erased = ATOM_false;
    else
      erased = ATOM_true;

    return PL_unify_atom(value, erased);
  } else if ( a == ATOM_predicate_indicator )
  { if ( unify_definition(MODULE_user, value,
			  clause->predicate, 0,
			  GP_QUALIFY|GP_NAMEARITY) )
      return TRUE;
  } else if ( a == ATOM_module )
  { return PL_unify_atom(value, clauseBodyContext(clause)->name);
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
redefineProcedure() is called when a procedure   needs to be defined and
it seems to have a definition.

Sf is the `owning' source-file

(*) occurs if this is actually false. This   happens if a file holding a
running predicate is reloaded because the clauses cannot be wiped.
(**) there is a definition, but we are reloading and we have not yet
seen this predicate, so it isn't there.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
redefineProcedure(Procedure proc, SourceFile sf, unsigned int suppress)
{ GET_LD
  Definition def = proc->definition;

  if ( true(def, P_FOREIGN) )
  {			/* first call printMessage() */
			/* so we can provide info about the old definition */
    if ( !printMessage(ATOM_warning,
		       PL_FUNCTOR_CHARS, "redefined_procedure", 2,
		         PL_CHARS, "foreign",
		         _PL_PREDICATE_INDICATOR, proc) )
      return FALSE;
			/* ... then abolish */
    abolishProcedure(proc, def->module);
  } else if ( false(def, P_MULTIFILE) )
  { ClauseRef first;

    def = getLocalProcDefinition(def);
    if ( !(first = hasClausesDefinition(def)) )
      return TRUE;				/* (*) see above */

    if ( first->value.clause->owner_no == sf->index )
    { if ( sf->reload && !reloadHasClauses(sf, proc) )
	return TRUE;				/* (**) see above */

      if ( ((debugstatus.styleCheck & ~suppress) & DISCONTIGUOUS_STYLE) &&
	   false(def, P_DISCONTIGUOUS) &&
	   sf->current_procedure )
      { if ( !printMessage(ATOM_warning,
			   PL_FUNCTOR_CHARS, "discontiguous", 2,
			     _PL_PREDICATE_INDICATOR, proc,
			     _PL_PREDICATE_INDICATOR, sf->current_procedure) )
	  return FALSE;
      }
    } else if ( !hasProcedureSourceFile(sf, proc) )
    { if ( true(def, P_THREAD_LOCAL) )
	return PL_error(NULL, 0, NULL, ERR_MODIFY_THREAD_LOCAL_PROC, proc);

      if ( first )
      { if ( !printMessage(ATOM_warning,
			   PL_FUNCTOR_CHARS, "redefined_procedure", 2,
			     PL_CHARS, "static",
			     _PL_PREDICATE_INDICATOR, proc) )
	  return FALSE;
      }
			/* again, _after_ the printMessage() */
      abolishProcedure(proc, def->module);
    }
  }

  return TRUE;
}



/** copy_predicate(From:predicate_indicator, To:predicate_indicator) is det.

Copy all clauses of From into To. To is created as a dynamic predicate.
*/

static void
remoduleClause(Clause cl, Module old, Module new)
{ Code PC, end;
  int in_body = FALSE;

  if ( true(cl, UNIT_CLAUSE) )
    return;

  PC  = cl->codes;
  end = &PC[cl->code_size];
  for( ; PC < end; PC = stepPC(PC) )
  { code op = fetchop(PC);

    if ( in_body )
    { const char *ats=codeTable[op].argtype;
      int an;

      for(an=0; ats[an]; an++)
      { switch(ats[an])
	{ case CA1_PROC:
	  { Procedure op = (Procedure)PC[an+1];

	    if ( op->definition->module != MODULE_system )
	    { functor_t f = op->definition->functor->functor;

	      PC[an+1] = (code)lookupProcedure(f, new);
	    }
	    break;
	  }
	  case CA1_MODULE:
	  { if ( old == (Module)PC[an+1] )
	      PC[an+1] = (code)new;
	  }
	}
      }
    } else if ( op == I_ENTER )
    { in_body = TRUE;
    }
  }
}


static
PRED_IMPL("copy_predicate_clauses", 2, copy_predicate_clauses, PL_FA_TRANSPARENT)
{ PRED_LD
  Procedure from, to;
  Definition def, copy_def;
  ClauseRef cref;
  gen_t generation;

  if ( !get_procedure(A1, &from, 0, GP_NAMEARITY|GP_RESOLVE) )
    fail;
  if ( !isDefinedProcedure(from) )
    trapUndefined(getProcDefinition(from));
  def = getProcDefinition(from);
  generation = global_generation();		/* take a consistent snapshot */

  if ( true(def, P_FOREIGN) )
    return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
		    ATOM_access, ATOM_private_procedure, from);

  if ( !get_procedure(A2, &to, 0, GP_NAMEARITY|GP_CREATE) )
    return FALSE;

  copy_def = getProcDefinition(to);
  if ( true(copy_def, P_FOREIGN) )
    return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, to);
  if ( false(copy_def, P_DYNAMIC) )
  { if ( isDefinedProcedure(to) )
      return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, to);
    if ( !setDynamicDefinition(copy_def, TRUE) )
      fail;
  }

  enterDefinition(def);
  acquire_def(def);
  for( cref = def->impl.clauses.first_clause; cref; cref = cref->next )
  { Clause cl = cref->value.clause;

    if ( visibleClause(cl, generation) )
    { size_t size = sizeofClause(cl->code_size);
      Clause copy = PL_malloc_atomic(size);

      memcpy(copy, cl, size);
      copy->predicate = copy_def;
      if ( def->module != copy_def->module )
	remoduleClause(copy, def->module, copy_def->module);
#ifdef O_ATOMGC
#ifdef O_DEBUG_ATOMGC
      forAtomsInClause(copy, register_atom_clause);
#else
      forAtomsInClause(copy, PL_register_atom);
#endif
#endif
      assertProcedure(to, copy, CL_END);
    }
  }
  release_def(def);
  leaveDefinition(def);

  return TRUE;
}


#if defined(O_MAINTENANCE) || defined(O_DEBUG)

		 /*******************************
		 *	INTERNAL DEBUGGING	*
		 *******************************/


static void
listGenerations(Definition def)
{ GET_LD
  gen_t gen = generationFrame(environment_frame);
  ClauseRef cref;
  int i;

  Sdprintf("%s has %d clauses at generation %ld\n",
	   predicateName(def),
	   def->impl.clauses.number_of_clauses, gen);

  acquire_def(def);
  for(i=1,cref=def->impl.clauses.first_clause; cref; cref=cref->next, i++)
  { Clause clause = cref->value.clause;

    Sdprintf("%p: [%2d] %8u-%10u%s%s%s\n",
	     clause, i,
	     clause->generation.created,
	     clause->generation.erased,
	     true(clause, CL_ERASED) ? " erased" : "",
	     visibleClause(clause, gen) ? " v " : " X ",
	     keyName(cref->d.key));
  }
  release_def(def);

  listIndexGenerations(def, gen);
}


void
checkDefinition(Definition def)
{ GET_LD
  unsigned int nc, indexed = 0;
  ClauseRef cref;
  unsigned int erased = 0;
  Definition old;

						/* check basic clause list */
  acquire_def2(def, old);
  for(nc=0, cref = def->impl.clauses.first_clause; cref; cref=cref->next)
  { Clause clause = cref->value.clause;

    if ( false(clause, CL_ERASED) )
    { if ( cref->d.key )
	indexed++;
      nc++;
    } else
    { erased++;
    }
  }
  release_def2(def, old);

  assert(nc == def->impl.clauses.number_of_clauses);
  assert(erased == def->impl.clauses.erased_clauses);

  checkClauseIndexes(def);
}


foreign_t
pl_check_procedure(term_t desc)
{ GET_LD
  Procedure proc;
  Definition def;

  if ( !get_procedure(desc, &proc, 0, GP_FIND|GP_NAMEARITY) )
    fail;
  def = getProcDefinition(proc);

  if ( true(def, P_FOREIGN) )
    fail;

  LOCKDEF(def);
  checkDefinition(def);
  UNLOCKDEF(def);

  succeed;
}


foreign_t
pl_list_generations(term_t desc)
{ GET_LD
  Procedure proc;
  Definition def;

  if ( !get_procedure(desc, &proc, 0, GP_FIND|GP_NAMEARITY) )
    fail;
  def = getProcDefinition(proc);

  if ( true(def, P_FOREIGN) )
    fail;				/* permission error */

  listGenerations(def);

  succeed;
}


#endif /*O_MAINTENANCE*/


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(proc)
  PRED_DEF("retractall", 1, retractall, PL_FA_NONDETERMINISTIC|PL_FA_ISO)
  PRED_DEF("$set_predicate_attribute", 3, set_predicate_attribute,
	   PL_FA_TRANSPARENT)
  PRED_DEF("$get_predicate_attribute", 3, get_predicate_attribute,
	   PL_FA_TRANSPARENT)
  PRED_DEF("$default_predicate", 2, default_predicate, PL_FA_TRANSPARENT)
  PRED_DEF("meta_predicate", 1, meta_predicate, PL_FA_TRANSPARENT)
  PRED_DEF("$get_clause_attribute", 3, get_clause_attribute, 0)
  PRED_DEF("retract", 1, retract,
	   PL_FA_TRANSPARENT|PL_FA_NONDETERMINISTIC|PL_FA_ISO)
  PRED_DEF("current_predicate", 1, current_predicate,
	   PL_FA_TRANSPARENT|PL_FA_NONDETERMINISTIC|PL_FA_ISO)
  PRED_DEF("copy_predicate_clauses", 2, copy_predicate_clauses, PL_FA_TRANSPARENT)
  PRED_DEF("$cgc_params", 6, cgc_params, 0)
EndPredDefs
