/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Procedure (re) allocation
*/

/*#define O_DEBUG 1*/
#include "pl-incl.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
General  handling  of  procedures:  creation;  adding/removing  clauses;
finding source files, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define LOCK()   PL_LOCK(L_PREDICATE)
#define UNLOCK() PL_UNLOCK(L_PREDICATE)

forwards void		resetReferencesModule(Module);
forwards void		resetProcedure(Procedure proc);

static void	removeClausesProcedure(Procedure proc, int sfindex);

Procedure
lookupProcedure(functor_t f, Module m)
{ Procedure proc;
  Definition def;
  Symbol s;
  
  LOCK();
  if ( (s = lookupHTable(m->procedures, (void *)f)) )
  { UNLOCK();
    DEBUG(1, Sdprintf("lookupProcedure() --> %s\n", procedureName(s->value)));
    return (Procedure) s->value;
  }

  proc = (Procedure)  allocHeap(sizeof(struct procedure));
  def  = (Definition) allocHeap(sizeof(struct definition));
  proc->type = PROCEDURE_TYPE;
  proc->definition = def;
  def->functor = valueFunctor(f);
  def->module  = m;
  addHTable(m->procedures, (void *)f, proc);
  GD->statistics.predicates++;

  def->definition.clauses = NULL;
  def->lastClause = NULL;
  def->hash_info = NULL;
#ifdef O_PROFILE
  def->profile_ticks = 0;
  def->profile_calls = 0;
  def->profile_redos = 0;
  def->profile_fails = 0;
#endif /* O_PROFILE */
  clearFlags(def);
  def->references = 0;
  resetProcedure(proc);
  UNLOCK();

  DEBUG(1, Sdprintf("Created %s\n", procedureName(proc)));
  return proc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
resetProcedure() is called  by  lookupProcedure()   for  new  ones,  and
abolishProcedure() by abolish/2. In the latter   case, abolish may leave
dirty clauses when called on a   running predicate. Hence, NEEDSCLAUSEGC
should be retained. Bug found by Paulo Moura, LogTalk developer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
resetProcedure(Procedure proc)
{ Definition def = proc->definition;

  def->flags ^= def->flags & ~(SPY_ME|NEEDSCLAUSEGC);
  set(def, TRACE_ME);
  def->indexCardinality = 0;
  def->number_of_clauses = 0;
  if ( def->functor->arity == 0 )
  { def->indexPattern = 0x0;
  } else
  { def->indexPattern = (0x0 | NEED_REINDEX);
    set(def, AUTOINDEX);
  }
  
  if ( def->hash_info && def->references == 0 )
  { unallocClauseIndexTable(def->hash_info);
    def->hash_info = NULL;
  }
}

Procedure
isCurrentProcedure(functor_t f, Module m)
{ Symbol s;

  if ( (s = lookupHTable(m->procedures, (void *)f)) )
    return (Procedure) s->value;

  return NULL;
}

bool
isDefinedProcedure(Procedure proc)
{ Definition def = proc->definition;

  if ( true(def, PROC_DEFINED) )
    succeed;

  if ( def->definition.clauses )
  { ClauseRef c;
#ifdef O_LOGICAL_UPDATE
    unsigned long generation;
    if ( environment_frame )
      generation = generationFrame(environment_frame);
    else
      generation = ~0L-1;		/* any non-erased clause */
#else
#define generation (0)
#endif

    if ( false(def, NEEDSCLAUSEGC) )
      succeed;
    
    for(c = def->definition.clauses; c; c = c->next)
    { Clause cl = c->clause;

      if ( visibleClause(cl, generation) )
	succeed;
    }
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find a procedure for defining it.  Here   we check whether the procedure
to be defined is a system predicate.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Procedure
lookupProcedureToDefine(functor_t def, Module m)
{ Procedure proc;

  if ( (proc = isCurrentProcedure(def, m)) && false(proc->definition, SYSTEM) )
    return proc;

  if ( !SYSTEM_MODE &&
       MODULE_system &&
       (proc=isCurrentProcedure(def, MODULE_system)) &&
       true(proc->definition, LOCKED) &&
       false(proc->definition, DYNAMIC) )
  { PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
	     ATOM_redefine, PL_new_atom("built_in_procedure"),
	     proc->definition);

    return NULL;
  }
 
  return lookupProcedure(def, m);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_functor() translates term  of  the   format  +Name/+Arity  into  the
internal functor represenation. It fails and  raises an exception on the
various possible format or represenation errors.  ISO compliant.

The return value is 1 normally, -1  if no functor exists and GF_EXISTING
is defined, and 0 if an error was raised.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define GF_EXISTING	1
#define GF_PROCEDURE	2		/* check for max arity */

static int
get_functor(term_t descr, functor_t *fdef, Module *m, term_t h, int how)
{ term_t head = PL_new_term_ref();

  if ( !PL_strip_module(descr, m, head) )
    fail;

  if ( PL_is_functor(head, FUNCTOR_divide2) )
  { term_t a = PL_new_term_ref();
    atom_t name;
    int arity;

    PL_get_arg(1, head, a);
    if ( PL_get_atom(a, &name) )
    { PL_get_arg(2, head, a);
      if ( PL_get_integer(a, &arity) )
      { if ( arity < 0 )
	{ return PL_error(NULL, 0, NULL, ERR_DOMAIN,
			  ATOM_not_less_than_zero, a);
	} else if ( (how&GF_PROCEDURE) && arity > MAXARITY )
	{ char buf[100];

	  return PL_error(NULL, 0,
			  tostr(buf, "limit is %d, request = %d",
				MAXARITY, arity),
			  ERR_REPRESENTATION, ATOM_max_arity);
	} else
	{ *fdef = PL_new_functor(name, arity);
	  
	  if ( h )
	    PL_put_term(h, head);
	  
	  succeed;
	}
      } else
      { if ( PL_is_variable(a) )
	  goto ierror;

	return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, a);
      }
    } else
    { if ( PL_is_variable(a) )
	goto ierror;

      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, a);
    }
  } else if ( PL_get_functor(head, fdef) )
  { if ( h )
      PL_put_term(h, head);
	  
    succeed;
  } else
  { if ( PL_is_variable(head) )
    { ierror:
      return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
    } else
      return PL_error(NULL, 0, NULL, ERR_TYPE,
		      ATOM_predicate_indicator, head);
  }
}

      
int
get_head_functor(term_t head, functor_t *fdef)
{ int arity;

  if ( !PL_get_functor(head, fdef) )
    return PL_error(NULL, 0, NULL, ERR_TYPE,
		    ATOM_predicate_indicator, head);
  if ( (arity=arityFunctor(*fdef)) > MAXARITY )
  { char buf[100];
    return PL_error(NULL, 0,
		    tostr(buf, "limit is %d, request = %d",
			  MAXARITY, arity),
		    ERR_REPRESENTATION, ATOM_max_arity);
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get the specified procedure from a   Prolog  argument.  This argument is
either a head or a term of the form module:head.  If `create' is TRUE, a
procedure is created in the module.  Otherwise, the system traverses the
module-inheritance chain to find the existing procedure.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
get_procedure(term_t descr, Procedure *proc, term_t h, int how)
{ Module m = (Module) NULL;
  functor_t fdef;
  Procedure p;

  if ( (how&GP_NAMEARITY) )
  { if ( !get_functor(descr, &fdef, &m, h, GF_PROCEDURE) )
      fail;
  } else
  { term_t head = PL_new_term_ref();

    PL_strip_module(descr, &m, head);

    if ( h )
      PL_put_term(h, head);

    if ( !get_head_functor(head, &fdef) )
      fail;
  }
  
  switch( how & GP_HOW_MASK )
  { case GP_CREATE:
      *proc = lookupProcedure(fdef, m);
      break;
    case GP_FINDHERE:
      if ( (p = isCurrentProcedure(fdef, m)) )
      { *proc = p;
        break;
      }
      fail;
    case GP_FIND:
      for( ; m; m = m->super )
      { if ( (p = isCurrentProcedure(fdef, m)) && isDefinedProcedure(p) )
	{ *proc = p;
	  goto out;
	}
      }
      fail;
    case GP_DEFINE:
      if ( (p = lookupProcedureToDefine(fdef, m)) )
      { *proc = p;
        break;
      }
      fail;
    case GP_RESOLVE:
      if ( (p = resolveProcedure(fdef, m)) )
      { *proc = p;
        break;
      }
      fail;
    default:
      assert(0);
  }

out:

  succeed;
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
pl_current_predicate(term_t name, term_t spec, word h)
{ TableEnum e;
  atom_t n;
  functor_t f;
  Module m = (Module) NULL;
  Procedure proc;
  Symbol symb;
  term_t functor = PL_new_term_ref();

  if ( ForeignControl(h) == FRG_CUTTED )
  { e = ForeignContextPtr(h);
    freeTableEnum(e);
    succeed;
  }

  if ( !PL_strip_module(spec, &m, functor) )
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

  while( (symb=advanceTableEnum(e)) )
  { FunctorDef fdef;
    
    proc = symb->value;
    fdef = proc->definition->functor;

    if ( (n && n != fdef->name) ||
	 !PL_unify_atom(name, fdef->name) ||
	 !PL_unify_functor(functor, fdef->functor) )
      continue;

    ForeignRedoPtr(e);
    succeed;
  }

  freeTableEnum(e);
  fail;
}


ClauseRef
newClauseRef(Clause clause)
{ ClauseRef cref = allocHeap(sizeof(struct clause_ref));
  
  cref->clause = clause;
  cref->next   = NULL;

  return cref;
}


void
freeClauseRef(ClauseRef cref)
{ freeHeap(cref, sizeof(struct clause_ref));
}


/*  Assert a clause to a procedure. Where askes to assert either at the
    head or at the tail of the clause list.

 ** Fri Apr 29 12:44:08 1988  jan@swivax.UUCP (Jan Wielemaker)  */

bool
assertProcedure(Procedure proc, Clause clause, int where)
{ Definition def = proc->definition;
  ClauseRef cref = newClauseRef(clause);

  if ( def->references && (debugstatus.styleCheck & DYNAMIC_STYLE) )
    printMessage(ATOM_informational,
		     PL_FUNCTOR_CHARS, "modify_active_procedure", 2,
		       PL_CHARS, "assert",
		       _PL_PREDICATE_INDICATOR, proc);

  LOCK();
  if ( !def->lastClause )
  { def->definition.clauses = def->lastClause = cref;
  } else if ( where == CL_START )
  { cref->next = def->definition.clauses;
    def->definition.clauses = cref;
  } else
  { ClauseRef last = def->lastClause;

    last->next = cref;
    def->lastClause = cref;
  }

  def->number_of_clauses++;
#ifdef O_LOGICAL_UPDATE
  clause->generation.created = ++GD->generation;
  clause->generation.erased  = ~0L;	/* infinite */
#endif

  if ( def->hash_info )
    addClauseToIndex(def, clause, where);
  else
  { if ( def->number_of_clauses == 25 && true(def, AUTOINDEX) )
      def->indexPattern |= NEED_REINDEX;
  }
  UNLOCK();

  succeed;
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

  LOCK();
  if ( def->module != module )		/* imported predicate; remove link */
  { Definition ndef	     = allocHeap(sizeof(struct definition));

    proc->definition         = ndef;
    ndef->functor            = def->functor; /* should be merged with */
    ndef->module             = module;	     /* lookupProcedure()!! */
    ndef->definition.clauses = NULL;
    ndef->lastClause         = NULL;
    ndef->hash_info	     = NULL;
#ifdef O_PROFILE
    ndef->profile_ticks      = 0;
    ndef->profile_calls      = 0;
    ndef->profile_redos      = 0;
    ndef->profile_fails      = 0;
#endif /* O_PROFILE */
    clearFlags(ndef);
    ndef->references         = 0;
    resetProcedure(proc);
  } else if ( true(def, FOREIGN) )	/* foreign: make normal */
  { def->definition.clauses = def->lastClause = NULL;
    resetProcedure(proc);
  } else				/* normal Prolog procedure */
  { removeClausesProcedure(proc, 0);
    resetProcedure(proc);
  }
  UNLOCK();

  succeed;
}


static void
removeClausesProcedure(Procedure proc, int sfindex)
{ Definition def = proc->definition;
  ClauseRef c;

  enterDefinition(def);
#ifdef O_LOGICAL_UPDATE
  GD->generation++;
#endif

  for(c = def->definition.clauses; c; c = c->next)
  { Clause cl = c->clause;

    if ( (sfindex == 0 || sfindex == cl->source_no) && false(cl, ERASED) )
    { set(cl, ERASED);
      set(def, NEEDSCLAUSEGC);
#ifdef O_LOGICAL_UPDATE
      cl->generation.erased = GD->generation;
#endif
      def->number_of_clauses--;
    } 
  }
  if ( def->hash_info )
    def->hash_info->alldirty = TRUE;

  leaveDefinition(def);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Retract a clause from a procedure. When   a clause without references is
retracted it is actually removed from the  heap, otherwise the clause is
unlinked and marked as `erased'. Its next   pointer will not be changed.
to avoid the follow up clause  to  be   destroyed  it  is given an extra
reference.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
retractClauseProcedure(Procedure proc, Clause clause)
{ Definition def = proc->definition;
  bool rval;

  if ( true(clause, ERASED) )
    succeed;

  LOCK();
  if ( def->references )
  { set(clause, ERASED);
    set(def, NEEDSCLAUSEGC);
    if ( def->hash_info )
      markDirtyClauseIndex(def->hash_info, clause);
    def->number_of_clauses--;
#ifdef O_LOGICAL_UPDATE
    clause->generation.erased = ++GD->generation;
#endif
    rval = TRUE;
  } else
  { ClauseRef prev = NULL;
    ClauseRef c;

    rval = FALSE;
    startCritical;

    if ( def->hash_info )
      delClauseFromIndex(def->hash_info, clause);

    for(c = def->definition.clauses; c; prev = c, c = c->next)
    { if ( c->clause == clause )
      { if ( !prev )
	{ def->definition.clauses = c->next;
	  if ( !c->next )
	    def->lastClause = NULL;
	} else
	{ prev->next = c->next;
	  if ( c->next == NULL)
	    def->lastClause = prev;
	}


  	freeClauseRef(c);
#if O_DEBUGGER
	if ( PROCEDURE_event_hook1 &&
	     def != PROCEDURE_event_hook1->definition )
	  callEventHook(PLEV_ERASED, clause);
#endif
	freeClause(clause);
	def->number_of_clauses--;

	rval = TRUE;
	break;
      }
    }
    endCritical;
  }
  UNLOCK();

  return rval;
}


void
freeClause(Clause c)
{
#if O_DEBUGGER
  if ( true(c, HAS_BREAKPOINTS) )
    clearBreakPointsClause(c);
#endif

  GD->statistics.codes -= c->code_size;
#ifdef O_ATOMGC
  unregisterAtomsClause(c);
#endif
  freeHeap(c->codes, sizeof(code) * c->code_size);
  freeHeap(c, sizeof(struct clause));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
gcClausesDefinition()
    Called from leaveDefinition().  Its task is to garbage collect all
    clauses marked as ERASED.  leaveDefinition() is locked for
    multi-threading, so there is no reason to do that here.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
gcClausesDefinition(Definition def)
{ ClauseRef cref, prev = NULL;
  int rehash = 0;
#if O_DEBUG
  int left = 0, removed = 0;
#endif

  DEBUG(1, Sdprintf("gcClausesDefinition(%s) --> ", predicateName(def)));

  cref = def->definition.clauses;

  if ( def->hash_info )
  { if ( false(def, NEEDSREHASH) )
      gcClauseIndex(def->hash_info);
    else
    { rehash = def->hash_info->size * 2;
      unallocClauseIndexTable(def->hash_info);
      def->hash_info = NULL;
    }
  }

  while( cref )
  { if ( true(cref->clause, ERASED) )
    { ClauseRef c = cref;
      
      cref = cref->next;
      if ( !prev )
      { def->definition.clauses = c->next;
	if ( !c->next )
	  def->lastClause = NULL;
      } else
      { prev->next = c->next;
	if ( c->next == NULL)
	  def->lastClause = prev;
      }

      DEBUG(0, removed++);
#if O_DEBUGGER
      if ( PROCEDURE_event_hook1 && def != PROCEDURE_event_hook1->definition )
      { def->references++;		/* prevent recursion */
	callEventHook(PLEV_ERASED, c->clause);
	def->references--;
      }
#endif
      freeClause(c->clause);
      freeClauseRef(c);
    } else
    { prev = cref;
      cref = cref->next;
      DEBUG(0, left++);
    }
  }

  DEBUG(1, Sdprintf("removed %d, left %d\n", removed, left));

  if ( rehash )
    hashDefinition(def, rehash);

  clear(def, NEEDSCLAUSEGC|NEEDSREHASH);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
resetReferences() is called by abort() to clear all predicate references.
Erased clauses will be removed as well.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
resetReferencesModule(Module m)
{ Definition def;

#ifdef O_PROFILE
#define ClearProfileTicked(def) clear(def, PROFILE_TICKED)
#else
#define ClearProfileTicked(def)
#endif

  for_unlocked_table(m->procedures, s,
		     { def = ((Procedure) s->value)->definition;
		       ClearProfileTicked(def);
		       def->references = 1;
		       leaveDefinition(def);
		     })
}

void
resetReferences(void)
{ LOCK();
  for_unlocked_table(GD->tables.modules, s,
		     resetReferencesModule((Module) s->value));
  UNLOCK();
}

#ifdef O_DEBUG
		 /*******************************
		 *	    CHECKING		*
		 *******************************/

word
pl_check_definition(term_t spec)
{ Procedure proc;
  Definition def;
  int nclauses = 0;
  int nindexable = 0;

  ClauseRef cref;

  if ( !get_procedure(spec, &proc, 0, GP_FIND) )
    return Sdprintf("$check_definition/1: can't find definition");
  def = proc->definition;

  if ( true(def, FOREIGN) )
    succeed;
  for(cref = def->definition.clauses; cref; cref = cref->next)
  { Clause clause = cref->clause;

    if ( clause->index.varmask != 0 )
      nindexable++;

    if ( false(clause, ERASED) )
      nclauses++;
    else
    { if ( false(def, NEEDSCLAUSEGC) )
	Sdprintf("%s contains erased clauses and has no NEEDSCLAUSEGC",
		 predicateName(def));
    }
  }

  if ( def->hash_info )
  { if ( def->hash_info->size != nindexable )
      Sdprintf("%s has inconsistent def->hash_info->size",
	      predicateName(def));
  }

  if ( def->number_of_clauses != nclauses )
    Sdprintf("%s has inconsistent number_of_clauses (%d, should be %d)",
	     predicateName(def), def->number_of_clauses, nclauses);

  succeed;
}
#endif /*O_DEBUG*/

		/********************************
		*     UNDEFINED PROCEDURES      *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A dynamic call to `f' in `m' has to be made (via call/1, apply/2 or from
C). This procedure  returns  the  procedure  to  be  run.   If  no  such
procedure  exists  an  undefined  procedure is created and returned.  In
this case interpret() will later call  trapUndefined()  to  generate  an
error message (or link the procedure from the library via autoload).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Procedure
resolveProcedure(functor_t f, Module module)
{ Procedure proc;
  Module m;

  for( m = module; m; m = m->super )
  { if ( (proc = isCurrentProcedure(f, m)) && isDefinedProcedure(proc) )
      return proc;
  }

  return lookupProcedure(f, module);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
autoImport() tries to autoimport f into module `m' and  returns  success
if this is possible.

PROBLEM: I'm not entirely  sure  it  is  save  to  deallocated  the  old
definition  structure  in  all  cases.   It  is  not  member of any heap
structure, thus sofar everything  is  alright.   After  a  dynamic  link
interpret()  picks up the new definition pointer, thus this should be ok
as well.  Any other C-code that  does  nasty  things  (non-deterministic
code  perhaps,  calls  indirect via C? (I do recall once conciously have
decided its not save, but can't recall why ...)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Definition
autoImport(functor_t f, Module m)
{ Procedure proc;
  Definition def;
					/* Defined: no problem */
  if ( (proc = isCurrentProcedure(f, m)) && isDefinedProcedure(proc) )
    return proc->definition;
  
  if ( !m->super )			/* No super: can't import */
    return NULL;

  if ( !(def = autoImport(f, m->super)) )
    return NULL;

  if ( proc == NULL )			/* Create header if not there */
    proc = lookupProcedure(f, m);
					/* safe? */
  freeHeap(proc->definition, sizeof(struct definition));
  proc->definition = def;

  return def;
}

static int undefined_nesting;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
According to Paulo Moura, predicates defined either dynamic, multifile or
discontiguous should not cause an undefined predicate warning.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Definition
trapUndefined(Definition def)
{ int retry_times = 0;
  Definition newdef;
  Module module = def->module;
  FunctorDef functor = def->functor;

  retry:
					/* Auto import */
  if ( (newdef = autoImport(functor->functor, module)) )
    return newdef;
					/* Pred/Module does not want to trap */
  if ( true(def, PROC_DEFINED) || false(module, UNKNOWN) )
    return def;

  DEBUG(5, Sdprintf("trapUndefined(%s)\n", predicateName(def)));

					/* Trap via exception/3 */
  if ( trueFeature(AUTOLOAD_FEATURE) && !GD->bootsession )
  { if ( undefined_nesting > 100 )
    { undefined_nesting = 1;
      sysError("trapUndefined(): undefined: %s", predicateName(def));

      return def;
    } else
    { fid_t  cid  = PL_open_foreign_frame();
      term_t argv = PL_new_term_refs(4);
      static predicate_t MTOK_pred;
      qid_t qid;
      atom_t sfn = source_file_name;	/* needs better solution! */
      int  sln = source_line_no;
      atom_t answer = ATOM_nil;

      if ( !MTOK_pred )
	MTOK_pred = PL_pred(FUNCTOR_undefinterc4, MODULE_system);

      PL_put_atom(    argv+0, def->module->name);
      PL_put_atom(    argv+1, def->functor->name);
      PL_put_integer( argv+2, def->functor->arity);
      PL_put_variable(argv+3);

      undefined_nesting++;
      qid = PL_open_query(MODULE_system, PL_Q_NODEBUG, MTOK_pred, argv);
      if ( PL_next_solution(qid) )
	PL_get_atom(argv+3, &answer);
      PL_close_query(qid);
      undefined_nesting--;
      source_file_name = sfn;
      source_line_no   = sln;
      PL_discard_foreign_frame(cid);

      def = lookupProcedure(functor->functor, module)->definition;

      if ( answer == ATOM_fail )
      { return def;
      } else if ( answer == ATOM_error )
      { PL_error(NULL, 0, NULL, ERR_UNDEFINED_PROC, def);
	return def;
      } else if ( answer == ATOM_retry )
      { if ( retry_times++ )
	{ warning("exception handler failed to define predicate %s\n",
		  predicateName(def));
	  return def;
	}
	goto retry;
      }
    }
  }
				/* No one wants to intercept */
  if ( GD->bootsession )
    sysError("Undefined predicate: %s", predicateName(def));
  else
    warning("Undefined predicate: %s", predicateName(def));

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

word
pl_retract(term_t term, word h)
{ if ( ForeignControl(h) == FRG_CUTTED )
  { ClauseRef cref = ForeignContextPtr(h);
    leaveDefinition(cref->clause->procedure->definition);

    succeed;
  } else
  { Procedure proc;
    Definition def;
    Module m = (Module) NULL;
    ClauseRef cref;
    term_t cl = PL_new_term_ref();
    term_t head = PL_new_term_ref();
    term_t body = PL_new_term_ref();
    atom_t b;

    PL_strip_module(term, &m, cl);
    get_head_and_body_clause(cl, head, body, NULL);
    if ( PL_get_atom(body, &b) && b == ATOM_true )
      PL_put_term(cl, head);

    if ( ForeignControl(h) == FRG_FIRST_CALL )
    { functor_t fd;

      if ( !PL_get_functor(head, &fd) )
	return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, term);
      if ( !(proc = isCurrentProcedure(fd, m)) )
	fail;

      def = proc->definition;

      if ( true(def, FOREIGN) )
	return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
			ATOM_modify, PL_new_atom("foreign_procedure"), def);
      if ( false(def, DYNAMIC) )
      { if ( isDefinedProcedure(proc) )
	  return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
			  ATOM_modify, PL_new_atom("static_procedure"), def);
	set(def, DYNAMIC);		/* implicit */
	fail;				/* no clauses */
      }

      if ( def->references && (debugstatus.styleCheck & DYNAMIC_STYLE) )
	printMessage(ATOM_informational,
		     PL_FUNCTOR_CHARS, "modify_active_procedure", 2,
		       PL_CHARS, "retract",
		       _PL_PREDICATE_INDICATOR, proc);

      cref = def->definition.clauses;
      enterDefinition(def);			/* reference the predicate */
    } else
    { cref = ForeignContextPtr(h);
      proc = cref->clause->procedure;
      def  = proc->definition;
    }

    for(; cref; cref = cref->next)
    { bool det;
      Word argv;

      if ( PL_is_compound(head) )
      { argv = valTermRef(head);
	deRef(argv);
	argv = argTermP(*argv, 0);
      } else
	argv = NULL;

      if ( !(cref = findClause(cref, argv, environment_frame, def, &det)) )
      { leaveDefinition(def);
	fail;
      }

      { fid_t cid = PL_open_foreign_frame();

	if ( decompile(cref->clause, cl, 0) )
	{ retractClauseProcedure(proc, cref->clause);
	  PL_close_foreign_frame(cid);	/* necessary? */
	  if ( det == TRUE )
	  { leaveDefinition(def);
	    succeed;
	  }

	  ForeignRedoPtr(cref->next);
	}

	PL_discard_foreign_frame(cid);
      }

      continue;
    }

    leaveDefinition(def);
    fail;
  }
}


word
pl_retractall(term_t head)
{ term_t thehead = PL_new_term_ref();
  Procedure proc;
  Definition def;
  ClauseRef cref;

  if ( !get_procedure(head, &proc, thehead, GP_FINDHERE) )
    succeed;

  def = proc->definition;
  if ( true(def, FOREIGN) )
    return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
		    ATOM_modify, PL_new_atom("foreign_procedure"), def);
  if ( false(def, DYNAMIC) )
  { if ( isDefinedProcedure(proc) )
      return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
		      ATOM_modify, PL_new_atom("static_procedure"), def);
    set(def, DYNAMIC);			/* implicit.  Warn? */
    succeed;				/* nothing to retract */
  }

  enterDefinition(def);
  for(cref = def->definition.clauses; cref; cref = cref->next)
  { bool det;
    Word argv;

    if ( PL_is_compound(thehead) )
    { argv = valTermRef(thehead);
      deRef(argv);
      argv = argTermP(*argv, 0);
    } else
      argv = NULL;

    cref = findClause(cref, argv, environment_frame, def, &det);

    if ( cref )
    { fid_t cid = PL_open_foreign_frame();
    
      if ( det )
	leaveDefinition(def);

      if ( decompileHead(cref->clause, thehead) )
	retractClauseProcedure(proc, cref->clause);

      PL_discard_foreign_frame(cid);

      if ( det )
	succeed;
    } else
      break;
  }
  leaveDefinition(def);

  succeed;
}


		/********************************
		*       PROLOG PREDICATES       *
		*********************************/

word
pl_abolish(term_t atom, term_t arity)
{ functor_t f;
  Procedure proc;
  Module m = (Module) NULL;
  term_t tmp = PL_new_term_ref();
  atom_t name;
  int a;

  PL_strip_module(atom, &m, tmp);

  if ( !PL_get_atom(tmp, &name) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, atom);
  if ( !PL_get_integer(arity, &a) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, arity);

  if ( !(f = isCurrentFunctor(name, a)) ||
       !(proc = isCurrentProcedure(f, m)) )
    succeed;

  if ( true(proc->definition, LOCKED) && !SYSTEM_MODE && m == MODULE_system )
    return PL_error("abolish", 2, NULL, ERR_MODIFY_STATIC_PROC, proc);

  return abolishProcedure(proc, m);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
abolish(Name/Arity)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_abolish1(term_t spec)
{ Procedure proc;
  functor_t f;
  Module m = NULL;

  switch( get_functor(spec, &f, &m, 0, GF_PROCEDURE|GF_EXISTING) )
  { case FALSE:				/* exception */
      fail;
    case -1:				/* no functor */
      succeed;
  }

  if ( !(proc = isCurrentProcedure(f, m)) )
    succeed;

  if ( true(proc->definition, LOCKED) && !SYSTEM_MODE && m == MODULE_system )
    return PL_error("abolish", 1, NULL, ERR_MODIFY_STATIC_PROC, proc);

  return abolishProcedure(proc, m);
}


static unsigned long
attribute_mask(atom_t key)
{
#define TRACE_ANY (TRACE_CALL|TRACE_REDO|TRACE_EXIT|TRACE_FAIL)

  if (key == ATOM_dynamic)	 return DYNAMIC;
  if (key == ATOM_multifile)	 return MULTIFILE;
  if (key == ATOM_system)	 return SYSTEM;
  if (key == ATOM_locked)	 return LOCKED;
  if (key == ATOM_spy)		 return SPY_ME;
  if (key == ATOM_trace)	 return TRACE_ME;
  if (key == ATOM_trace_call)	 return TRACE_CALL;
  if (key == ATOM_trace_redo)	 return TRACE_REDO;
  if (key == ATOM_trace_exit)	 return TRACE_EXIT;
  if (key == ATOM_trace_fail)	 return TRACE_FAIL;
  if (key == ATOM_trace_any)	 return TRACE_ANY;
  if (key == ATOM_hide_childs)	 return HIDE_CHILDS;
  if (key == ATOM_transparent)	 return METAPRED;
  if (key == ATOM_discontiguous) return DISCONTIGUOUS;
  if (key == ATOM_volatile)	 return VOLATILE;

  return 0;
}


word
pl_get_predicate_attribute(term_t pred,
			   term_t what, term_t value)
{ Procedure proc;
  Definition def;
  functor_t fd;
  atom_t key;
  Module module = (Module) NULL;
  unsigned long att;
  term_t head = PL_new_term_ref();
  
  if ( !PL_strip_module(pred, &module, head) ||
       !PL_get_functor(head, &fd) ||
       !(proc = resolveProcedure(fd, module)) )
    fail;

  def = proc->definition;

  if ( !PL_get_atom(what, &key) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, what);

  if ( key == ATOM_imported )
  { if ( module == def->module )
      fail;
    return PL_unify_atom(value, def->module->name);
  } else if ( key == ATOM_indexed )
  { if ( def->indexPattern == 0x0 )
      fail;
    return unify_index_pattern(proc, value);
  } else if ( key == ATOM_exported )
  { return PL_unify_integer(value, isPublicModule(module, proc));
  } else if ( key == ATOM_defined )
  { int d;

    if ( isDefinedProcedure(proc) )
      d = 1;
    else
      d = 0;
      
    return PL_unify_integer(value, d);
  } else if ( key == ATOM_line_count )
  { int line;

    if ( false(def, FOREIGN) &&
	 def->definition.clauses &&
	 (line=def->definition.clauses->clause->line_no) )
      return PL_unify_integer(value, line);
    else
      fail;
  } else if ( key == ATOM_foreign )
  { return PL_unify_integer(value, (def->flags & FOREIGN) ? 1 : 0);
  } else if ( key == ATOM_hashed )
  { return PL_unify_integer(value, def->hash_info?def->hash_info->buckets:0);
  } else if ( key == ATOM_references )
  { return PL_unify_integer(value, def->references);
  } else if ( key == ATOM_number_of_clauses )
  { if ( def->flags & FOREIGN )
      fail;

    return PL_unify_integer(value, def->number_of_clauses);
  } else if ( (att = attribute_mask(key)) )
  { return PL_unify_integer(value, (def->flags & att) ? 1 : 0);
  } else
  { return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    PL_new_atom("procedure_property"), what);
  }
}
  

word
pl_set_predicate_attribute(term_t pred,
			   term_t what, term_t value)
{ Procedure proc;
  Definition def;
  atom_t key;
  int val;
  unsigned long att;

  if ( !PL_get_atom(what, &key) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, what);
  if ( !PL_get_integer(value, &val) || val & ~1 )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, value);
  if ( !(att = attribute_mask(key)) )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    PL_new_atom("procedure_property"), what);
  if ( att & (TRACE_ANY|SPY_ME) )
  { if ( !get_procedure(pred, &proc, 0, GP_RESOLVE) )
      fail;
  } else
  { if ( !get_procedure(pred, &proc, 0, GP_DEFINE|GP_NAMEARITY) )
      fail;
  }
  def = proc->definition;

  if ( !val )
  { clear(def, att);
  } else
  { set(def, att);
    if ( (att == DYNAMIC || att == MULTIFILE) && SYSTEM_MODE )
    { set(def, SYSTEM|HIDE_CHILDS);
    }
  }

  succeed;
}


word
pl_default_predicate(term_t d1, term_t d2)
{ Procedure p1, p2;

  if ( get_procedure(d1, &p1, 0, GP_FIND) &&
       get_procedure(d2, &p2, 0, GP_FIND) )
  { if ( p1->definition == p2->definition || !isDefinedProcedure(p1) )
      succeed;
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
reindexDefinition()
    Rebuilds the clause index for the predicate.  This predicate is prepared
    for multi-threading just by using enterDefinition()/leaveDefinition().
    This implies two threads may do the same job at the same time, but I
    think this is fully safe: they will create the same data and both
    threads are guaranteed not to continue before this is completed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
reindexDefinition(Definition def)
{ ClauseRef cref;
  int do_hash = 0;
  unsigned long pattern;

  DEBUG(2, if ( def->definition.clauses )
	   { Procedure proc = def->definition.clauses->clause->procedure;

	     Sdprintf("reindexDefinition(%s)\n", procedureName(proc));
	   });

  if ( true(def, AUTOINDEX) )
  { int canindex = 0;
    int cannotindex = 0;
    
    enterDefinition(def);
    for(cref = def->definition.clauses; cref; cref = cref->next)
    { word key;

      if ( true(cref->clause, ERASED) )
	continue;

      if ( arg1Key(cref->clause, &key) )
	canindex++;
      else
	cannotindex++;
    }
    leaveDefinition(def);

    if ( canindex == 0 )
    { DEBUG(2, if ( def->definition.clauses )
	       { Procedure proc = def->definition.clauses->clause->procedure;

		 Sdprintf("not indexed: %s\n", procedureName(proc));
	       });
      def->indexPattern = 0x0;
    } else
    { def->indexPattern = 0x1;
      if ( canindex > 5 && cannotindex <= 2 )
	do_hash = canindex / 2;
    }
  }

  enterDefinition(def);
  pattern = def->indexPattern & ~NEED_REINDEX;
  def->indexCardinality = cardinalityPattern(pattern);
  for(cref = def->definition.clauses; cref; cref = cref->next)
    reindexClause(cref->clause);
  def->indexPattern = pattern;
  leaveDefinition(def);

  if ( do_hash )
  { DEBUG(1,
	  if ( def->definition.clauses )
	  { Procedure proc = def->definition.clauses->clause->procedure;

	    Sdprintf("hash(%s, %d)\n", procedureName(proc), do_hash);
	  });
    hashDefinition(def, do_hash);
  }
}


word
pl_index(term_t pred)
{ Procedure proc;
  term_t head = PL_new_term_ref();

  if ( get_procedure(pred, &proc, head, GP_CREATE) )
  { Definition def = proc->definition;
    int arity = def->functor->arity;

    if ( true(def, FOREIGN) )
      return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
		      ATOM_index, PL_new_atom("foreign_procedure"), def);

    if ( arity > 0 )
    { unsigned long pattern = 0x0;
      int n, card = 0;
      term_t a = PL_new_term_ref();

      for(n=0; n<arity && n < 31; n++)
      { int ia;

	_PL_get_arg(n+1, head, a);
	if ( !PL_get_integer(a, &ia) || (ia & ~1) )
	  return PL_error(NULL, 0, "0 or 1", ERR_TYPE,
			  ATOM_integer, a);
	if ( ia )
	{ pattern |= 1 << n;
	  if (++card == 4)		/* maximal 4 indexed arguments */
	    break;
	}
      }
      
      clear(def, AUTOINDEX);
      if ( (def->indexPattern & ~NEED_REINDEX) == pattern)
	succeed;
      def->indexPattern = (pattern | NEED_REINDEX);
    }
    succeed;
  }

  fail;
}


word
pl_get_clause_attribute(term_t ref, term_t att, term_t value)
{ Clause clause;
  atom_t a;

  if ( !PL_get_pointer(ref, (void **)&clause)  ||
       !inCore(clause) || !isClause(clause) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_clause, ref);
  if ( !PL_get_atom(att, &a) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, a);

  if ( a == ATOM_line_count )
  { if ( clause->line_no )
      return PL_unify_integer(value, clause->line_no);
  } else if ( a == ATOM_file )
  { SourceFile sf = indexToSourceFile(clause->source_no);
    
    if ( sf )
      return PL_unify_atom(value, sf->name);
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
  }

  fail;
}


		/********************************
		*         SOURCE FILE           *
		*********************************/

static int source_index = 0;
static Table sourceTable = NULL;	/* atom --> SourceFile */

static void
registerSourceFile(SourceFile f)
{ if ( !GD->files.source_files.base )
    initBuffer(&GD->files.source_files);

  f->index = entriesBuffer(&GD->files.source_files, SourceFile) + 1;
    
  addBuffer(&GD->files.source_files, f, SourceFile);
}


SourceFile
lookupSourceFile(atom_t name)
{ SourceFile file;
  Symbol s;

  LOCK();
  if ( !sourceTable )
    sourceTable = newHTable(32);

  if ( (s=lookupHTable(sourceTable, (void*)name)) )
  { UNLOCK();
    return (SourceFile) s->value;
  }

  file = (SourceFile) allocHeap(sizeof(struct sourceFile));
  file->name = name;
  file->count = 0;
  file->time = 0L;
  file->index = ++source_index;
  file->system = GD->bootsession;
  file->procedures = NULL;

  PL_register_atom(file->name);

  registerSourceFile(file);

  addHTable(sourceTable, (void*)name, file);
  UNLOCK();

  return file;
}


SourceFile
indexToSourceFile(int index)
{ int n = entriesBuffer(&GD->files.source_files, SourceFile);

  index--;
  if ( index >= 0 && index < n )
    return fetchBuffer(&GD->files.source_files, index, SourceFile);
      
  return NULL;
}


void
addProcedureSourceFile(SourceFile sf, Procedure proc)
{ ListCell cell;

  LOCK();
  if ( true(proc->definition, FILE_ASSIGNED) )
  { for(cell=sf->procedures; cell; cell = cell->next)
    { if ( cell->value == proc )
      { UNLOCK();
	return;
      }
    }
  }

  cell = allocHeap(sizeof(struct list_cell));
  cell->value = proc;
  cell->next = sf->procedures;
  sf->procedures = cell;
  set(proc->definition, FILE_ASSIGNED);
  UNLOCK();
}


void
redefineProcedure(Procedure proc, SourceFile sf)
{ Definition def = proc->definition;

  if ( true(def, FOREIGN) )
  { abolishProcedure(proc, def->module);
    printMessage(ATOM_warning,
		 PL_FUNCTOR_CHARS, "redefined_procedure", 2,
		   PL_CHARS, "foreign",
		   _PL_PREDICATE_INDICATOR, proc);
  }

  if ( false(def, MULTIFILE) )
  { ClauseRef first = def->definition.clauses;

    while ( first && true(first->clause, ERASED) )
      first = first->next;

    if ( first && first->clause->source_no == sf->index )
    { if ( (debugstatus.styleCheck & DISCONTIGUOUS_STYLE) &&
	   false(def, DISCONTIGUOUS) )
	printMessage(ATOM_warning,
		     PL_FUNCTOR_CHARS, "discontiguous", 1,
		       _PL_PREDICATE_INDICATOR, proc);
    } else
    { abolishProcedure(proc, def->module);

      if ( proc->definition->references )
	printMessage(ATOM_informational,
		     PL_FUNCTOR_CHARS, "redefined_procedure", 2,
		       PL_CHARS, "active",
		       _PL_PREDICATE_INDICATOR, proc);
      else
	printMessage(ATOM_warning,
		     PL_FUNCTOR_CHARS, "redefined_procedure", 2,
		       PL_CHARS, "static",
		       _PL_PREDICATE_INDICATOR, proc);
    }
  }
}


word
pl_make_system_source_files(void)
{ int i, n = entriesBuffer(&GD->files.source_files, SourceFile);


  for(i=0; i<n; i++)
  { SourceFile f = fetchBuffer(&GD->files.source_files, i, SourceFile);

    f->system = TRUE;
  }

  succeed;
}


word
pl_source_file(term_t descr, term_t file, control_t h)
{ Procedure proc;
  ClauseRef cref;
  SourceFile sf;
  atom_t name;
  ListCell cell;
  

  if ( ForeignControl(h) == FRG_FIRST_CALL &&
       !PL_is_variable(descr) )
  { if ( !get_procedure(descr, &proc, 0, GP_FIND) ||
	 !proc->definition ||
	 true(proc->definition, FOREIGN) ||
	 !(cref = proc->definition->definition.clauses) ||
	 !(sf = indexToSourceFile(cref->clause->source_no)) )
      fail;

    return PL_unify_atom(file, sf->name);
  }

  if ( ForeignControl(h) == FRG_CUTTED )
    succeed;

  if ( !PL_get_atom(file, &name) ||
       !(sf = lookupSourceFile(name)) )
    fail;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      cell = sf->procedures;
      break;
    case FRG_REDO:
      cell = ForeignContextPtr(h);
      break;
    default:
      cell = NULL;
      assert(0);
  }
  
  for( ; cell; cell = cell->next )
  { Procedure proc = cell->value;
    Definition def = proc->definition;
    fid_t cid = PL_open_foreign_frame();

    if ( unify_definition(descr, def, 0, 0) )
    { PL_close_foreign_frame(cid);

      if ( cell->next )
	ForeignRedoPtr(cell->next);
      else
	succeed;
    }

    PL_discard_foreign_frame(cid);
  }

  fail;
}


word
pl_time_source_file(term_t file, term_t time, control_t h)
{ int index;
  int mx = entriesBuffer(&GD->files.source_files, SourceFile);

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      index = 0;
      break;
    case FRG_REDO:
      index = ForeignContextInt(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  for(; index < mx; index++)
  { SourceFile f = fetchBuffer(&GD->files.source_files, index, SourceFile);

    if ( !f->system )
    { if ( PL_unify_atom(file, f->name) && unifyTime(time, f->time) )
	ForeignRedoInt(index+1);
    }
  }

  fail;
}


void
startConsult(SourceFile f)
{ if ( f->count++ > 0 )
  { ListCell cell, next;

    for(cell = f->procedures; cell; cell = next)
    { Procedure proc = cell->value;

      next = cell->next;
      if ( proc->definition )
	removeClausesProcedure(proc, true(proc->definition, MULTIFILE)
						? f->index : 0);
      freeHeap(cell, sizeof(struct list_cell));
    }
    f->procedures = NULL;
  }

  f->current_procedure = NULL;
}


word
pl_start_consult(term_t file)
{ atom_t name;

  if ( PL_get_atom(file, &name) )
  { SourceFile f = lookupSourceFile(name);

    f->time = LastModifiedFile(stringAtom(name));
    startConsult(f);
    succeed;
  }

  fail;
}

		 /*******************************
		 *       DEBUGGER SUPPORT	*
		 *******************************/

word
pl_clause_from_source(term_t file, term_t line, term_t clause)
{ atom_t name;
  SourceFile f;
  int ln;
  ListCell cell;
  Clause c = NULL;

  if ( !PL_get_atom(file, &name) ||
       !(f = lookupSourceFile(name)) ||
       !PL_get_integer(line, &ln) )
    return warning("clause_from_source/3: instantiation fault");
  

  for(cell = f->procedures; cell; cell = cell->next)
  { Procedure proc = cell->value;
    Definition def = proc->definition;

    if ( def && false(def, FOREIGN) )
    { ClauseRef cref = def->definition.clauses;

      for( ; cref; cref = cref->next )
      { Clause cl = cref->clause;

	if ( cl->source_no == f->index )
	{ if ( ln >= cl->line_no )
	  { if ( !c || c->line_no < cl->line_no )
	      c = cl;
	  }
	}
      }
    }
  }

  if ( c )
    return PL_unify_pointer(clause, c);
  
  fail;
}
