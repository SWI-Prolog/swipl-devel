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

forwards void	resetReferencesModule(Module);

SourceFile 	sourceFileTable = (SourceFile) NULL;
SourceFile 	tailSourceFileTable = (SourceFile) NULL;
SourceFile 	isCurrentSourceFile(Atom name);
static void	removeClausesProcedure(Procedure proc, int sfindex);

Procedure
lookupProcedure(FunctorDef f, Module m)
{ Procedure proc;
  register Definition def;
  Symbol s;
  
  if ((s = lookupHTable(m->procedures, f)) != (Symbol) NULL)
    return (Procedure) s->value;

  proc = (Procedure)  allocHeap(sizeof(struct procedure));
  def  = (Definition) allocHeap(sizeof(struct definition));
  proc->type = PROCEDURE_TYPE;
  proc->definition = def;
  def->functor = f;
  def->module  = m;
  addHTable(m->procedures, f, proc);
  statistics.predicates++;

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

  return proc;
}

void
resetProcedure(Procedure proc)
{ register Definition def = proc->definition;

  def->flags ^= def->flags & ~SPY_ME;	/* Preserve the spy flag */
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
isCurrentProcedure(FunctorDef f, Module m)
{ Symbol s;

  if ((s = lookupHTable(m->procedures, f)) != (Symbol) NULL)
    return (Procedure) s->value;

  return (Procedure) NULL;
}

bool
isDefinedProcedure(register Procedure proc)
{ if ( /* true(proc->definition, FOREIGN) || not needed; union */
       proc->definition->definition.clauses ||
       true(proc->definition, DYNAMIC) )
    succeed;
  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find a procedure for defining it.  Here   we check whether the procedure
to be defined is a system predicate.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Procedure
lookupProcedureToDefine(FunctorDef def, Module m)
{ Procedure proc;

  if ( (proc = isCurrentProcedure(def, m)) && false(proc->definition, SYSTEM) )
    return proc;

  if ( !SYSTEM_MODE &&
       MODULE_system &&
       (proc=isCurrentProcedure(def, MODULE_system)) &&
       true(proc->definition, LOCKED) &&
       false(proc->definition, DYNAMIC) )
  { warning("Attempt to redefine a system predicate: %s/%d\n"
	    "\tUse :- redefine_system_predicate(+Head) if this is intended",
	    stringAtom(proc->definition->functor->name),
	    proc->definition->functor->arity);
    return NULL;
  }
 
  return lookupProcedure(def, m);
}


/*  Find a procedure from description `descr'. `descr' is one of:
    <term> or <module>:<term>. If the procedure does not exists NULL
    is returned.

 ** Tue Apr 19 16:11:25 1988  jan@swivax.UUCP (Jan Wielemaker)  */

Procedure
findProcedure(Word descr)
{ Module m = (Module) NULL;
  FunctorDef fd;
  Procedure proc;

  if ((descr = stripModule(descr, &m)) == (Word) NULL)
    return (Procedure) NULL;

  if (isAtom(*descr) )
    fd = lookupFunctorDef((Atom)*descr, 0);
  else if (isTerm(*descr) )
    fd = functorTerm(*descr);
  else
  { warning("Illegal predicate specification");
    return (Procedure) NULL;
  }
  
  for( ; m; m = m->super )
  { if ( (proc = isCurrentProcedure(fd, m)) != NULL )
      return proc;
  }

  return (Procedure) NULL;
}

Procedure
findCreateProcedure(Word descr)
{ Module m = (Module) NULL;

  if ((descr = stripModule(descr, &m)) == (Word) NULL)
  { warning("Illegal module specification");
    return (Procedure) NULL;
  }

  if (isAtom(*descr) )
    return lookupProcedure(lookupFunctorDef((Atom)*descr, 0), m);
  if (isTerm(*descr) )
    return lookupProcedure(functorTerm(*descr), m);

  warning("Illegal predicate specification");
  return (Procedure) NULL;
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
pl_current_predicate(Word name, Word functor, word h)
{ Atom n;
  FunctorDef f;
  Module m = (Module) NULL;
  Procedure proc;
  Symbol symb;

  if ( ForeignControl(h) == FRG_CUTTED )
    succeed;

  if ((functor = stripModule(functor, &m)) == (Word) NULL)
    fail;

  if (isAtom(*name) )
    n = (Atom) *name;
  else if (isVar(*name) )
    n = (Atom) NULL;
  else
    fail;

  if (isTerm(*functor) )
    f = functorTerm(*functor);
  else if (isAtom(*functor) )
    f = lookupFunctorDef((Atom)*functor, 0);
  else if (isVar(*functor) )
    f = (FunctorDef) NULL;
  else
    fail;

  if ( ForeignControl(h) == FRG_FIRST_CALL)
  { if (f != (FunctorDef) NULL) 
    { if ((proc = isCurrentProcedure(f, m)) != (Procedure) NULL)
      { TRY(unifyAtomic(name, f->name) );
	succeed;
      } else
	fail;
    }
    symb = firstHTable(m->procedures);
  } else
    symb = (Symbol) ForeignContextAddress(h);

  for(; symb; symb = nextHTable(m->procedures, symb) )
  { FunctorDef fdef;
    
    proc = (Procedure) symb->value;
    fdef = proc->definition->functor;

    if ( (n != (Atom) NULL && n != fdef->name) ||
	 !unifyAtomic(name, fdef->name) ||
	 !unifyFunctor(functor, fdef) )
      continue;

    if ((symb = nextHTable(m->procedures, symb)) != (Symbol) NULL)
      ForeignRedo(symb);

    succeed;
  }

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

  startCritical;
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

  if ( def->hash_info )
    addClauseToIndex(def, clause, where);
  else
  { if ( ++def->number_of_clauses == 25 && true(def, AUTOINDEX) )
      def->indexPattern |= NEED_REINDEX;
  }

  endCritical;  

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
{ register Definition def = proc->definition;

  if ( def->module != module )		/* imported predicate; remove link */
  { def  = (Definition) allocHeap(sizeof(struct definition));
    proc->definition = def;
    def->module = module;
    def->definition.clauses = NULL;
    def->lastClause = NULL;
#ifdef O_PROFILE
    def->profile_ticks = 0;
    def->profile_calls = 0;
    def->profile_redos = 0;
    def->profile_fails = 0;
#endif /* O_PROFILE */
    resetProcedure(proc);

    succeed;
  }

  if ( true(def, FOREIGN) )
  { startCritical;
    def->definition.clauses = def->lastClause = NULL;
    resetProcedure(proc);
    endCritical;

    succeed;
  }

  removeClausesProcedure(proc, 0);
  resetProcedure(proc);

  succeed;
}


static void
removeClausesProcedure(Procedure proc, int sfindex)
{ Definition def = proc->definition;
  ClauseRef c;

  def->references++;

  for(c = def->definition.clauses; c; c = c->next)
  { Clause cl = c->clause;

    if ( sfindex == 0 || sfindex == cl->source_no )
    { set(cl, ERASED);
      set(def, NEEDSCLAUSEGC);
      def->number_of_clauses--;
    } 
  }

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


  if ( def->references )
  { set(clause, ERASED);
    set(def, NEEDSCLAUSEGC);
    def->number_of_clauses--;
    succeed;
  } else
  { ClauseRef prev = NULL;
    ClauseRef c;
    bool rval = FALSE;

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
	freeClause(clause);
	def->number_of_clauses--;

	rval = TRUE;
	break;
      }
    }

    endCritical;

    return rval;
  }
}


void
freeClause(Clause c)
{ statistics.codes -= c->code_size;
  freeHeap(c->codes, sizeof(code) * c->code_size);
  freeHeap(c, sizeof(struct clause));
}


void
gcClausesDefinition(Definition def)
{ ClauseRef cref = def->definition.clauses, prev = NULL;
  int rehash = 0;
#if O_DEBUG
  int left = 0, removed = 0;
#endif

  DEBUG(1, Sdprintf("gcClausesDefinition(%s) --> ", predicateName(def)));

  startCritical;

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

  endCritical;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
resetReferences() is called by abort() to clear all predicate references.
Erased clauses will be removed as well.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
resetReferencesModule(Module m)
{ Definition def;
  Symbol s;

  for_table(s, m->procedures)
  { def = ((Procedure) s->value)->definition;
#ifdef O_PROFILE
    clear(def, PROFILE_TICKED);
#endif /* O_PROFILE */
    def->references = 1;
    leaveDefinition(def);
  }
}

void
resetReferences(void)
{ Symbol s;

  for_table(s, moduleTable)
    resetReferencesModule((Module) s->value);
}

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
resolveProcedure(FunctorDef f, Module module)
{ Procedure proc;
  Module m;

  for( m = module; m != (Module) NULL; m = m->super )
  { if ( (proc = isCurrentProcedure(f, m)) != (Procedure) NULL &&
       isDefinedProcedure(proc) )
      return proc;
  }

  return lookupProcedure(f, module);
}


Definition
findDefinition(FunctorDef f, Module m)
{ Procedure proc;
					/* Defined: no problem */
  for(;; m = m->super)
  { if ( (proc = isCurrentProcedure(f, m)) != NULL &&
	 isDefinedProcedure(proc) )
      return proc->definition;
  
    if ( !m->super )			/* No super: cannot import */
      return NULL;
  }
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
autoImport(FunctorDef f, Module m)
{ Procedure proc;
  Definition def;
					/* Defined: no problem */
  if ( (proc = isCurrentProcedure(f, m)) != NULL &&
       isDefinedProcedure(proc) )
    return proc->definition;
  
  if ( m->super == (Module) NULL )	/* No super: can't import */
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

Definition
trapUndefined(Definition def)
{ int retry_times = 0;
  Definition newdef;
  Module module = def->module;
  FunctorDef functor = def->functor;

  retry:
					/* Auto import */
  if ( (newdef = autoImport(functor, module)) )
    return newdef;
					/* Pred/Module does not want to trap */
  if ( true(def, DYNAMIC) || false(module, UNKNOWN) )
    return def;

  DEBUG(5, Sdprintf("trapUndefined(%s)\n", predicateName(def)));

					/* Trap via exception/3 */
  if ( status.autoload )
  { word goal;
    mark m;
    bool rval;
    Atom sfn = source_file_name;	/* needs better solution! */
    int  sln = source_line_no;

    if ( undefined_nesting++ == 1000 )
    { undefined_nesting = 1;
      sysError("trapUndefined(): undefined: %s", predicateName(def));
      return def;
    }

    Mark(m);
    goal = globalFunctor(FUNCTOR_undefinterc3);
    unifyAtomic(argTermP(goal, 0), def->module->name);
    unifyAtomic(argTermP(goal, 1), def->functor->name);
    unifyAtomic(argTermP(goal, 2), consNum(def->functor->arity));

    debugstatus.suspendTrace++;
    rval = callGoal(MODULE_system, goal, FALSE);
    debugstatus.suspendTrace--;
    source_file_name = sfn;
    source_line_no   = sln;

    Undo(m);
    undefined_nesting--;

    def = lookupProcedure(functor, module)->definition; /* ??? */

    if ( rval == TRUE )
    { extern int trace_continuation;	/* from pl-trace.c */

      switch( trace_continuation )
      { case ACTION_FAIL:
	  return def;
	case ACTION_RETRY:
	  if ( retry_times++ )
	  { warning("exception handler failed to define predicate %s\n",
		    predicateName(def));
	    break;
	  }
	  goto retry;
	default:
	  warning("Illegal return value from exception handler");
      }
    }
  }
					/* No one want to intercept */
  warning("Undefined predicate: %s", predicateName(def) );

  return def;
}

		 /*******************************
		 *	  REQUIRE SUPPORT	*
		 *******************************/

word
pl_require(Word pred)
{ FunctorDef fd;
  Module module = (Module) NULL;

  pred = stripModule(pred, &module);
  if ( isAtom(*pred) )
    fd = lookupFunctorDef((Atom) *pred, 0);
  else if ( isTerm(*pred) )
    fd = functorTerm(*pred);
  else
    fail;

  lookupProcedureToDefine(fd, module);
  
  succeed;
}


		/********************************
		*            RETRACT            *
		*********************************/

word
pl_retract(Word term, word h)
{ Procedure proc;
  Definition def;
  Word head, body;
  Module m = (Module) NULL;
  ClauseRef cref;

  if ( ForeignControl(h) == FRG_CUTTED )
  { cref = ForeignContextAddress(h);
    leaveDefinition(cref->clause->procedure->definition);

    succeed;
  }

  if ((term = stripModule(term, &m)) == (Word) NULL)
    fail;
 
  if ( !splitClause(term, &head, &body, NULL) )
    return warning("retract/1: illegal specification");

  if ( ForeignControl(h) == FRG_FIRST_CALL )
  { if ( isAtom(*head) )
      proc = isCurrentProcedure(lookupFunctorDef((Atom)*head, 0), m);
    else if ( isTerm(*head) )
      proc = isCurrentProcedure(functorTerm(*head), m);
    else
      return warning("retract/1: Illegal predicate specification");

    if ( proc == (Procedure) NULL )
      fail;

    def = proc->definition;

    if ( true(def, FOREIGN) )
      return warning("retract/1: cannot retract from a foreign predicate");
    if ( true(def, LOCKED) && false(def, DYNAMIC) )
      return warning("retract/1: Attempt to retract from a system predicate");

    cref = def->definition.clauses;
    def->references++;			/* reference the predicate */
  } else
  { cref = (ClauseRef) ForeignContextAddress(h);
    proc = cref->clause->procedure;
    def  = proc->definition;
  }

  for(; cref; cref = cref->next)
  { bool det;

    if ( isTerm(*head) )
    { if ( !(cref = findClause(cref, argTermP(*head, 0), def, &det)) )
      { leaveDefinition(def);
	fail;
      }
    } else /*if ( isAtom(*head) )*/
    { if ( true(cref->clause, ERASED) )
	continue;
      det = (cref->next == NULL);
    }

    { mark m;

      Mark(m);
      if ( decompile(cref->clause, term) )
      { retractClauseProcedure(proc, cref->clause);
	unlockMark(&m);
	if ( det == TRUE )
	{ leaveDefinition(def);
	  succeed;
	}

	ForeignRedo(cref->next);
      }
      Undo(m);
    }

    continue;
  }

  leaveDefinition(def);
  fail;
}


word
pl_retractall(Word head)
{ Module m = (Module) NULL;
  Procedure proc;
  Definition def;
  ClauseRef cref;

  if ( !(head = stripModule(head, &m)) )
    fail;

  if ( isAtom(*head) )
    proc = isCurrentProcedure(lookupFunctorDef((Atom)*head, 0), m);
  else if ( isTerm(*head) )
    proc = isCurrentProcedure(functorTerm(*head), m);
  else
    return warning("retractall/1: Illegal predicate specification");

  if ( proc == (Procedure) NULL )
    succeed;

  def = proc->definition;
  if ( true(def, FOREIGN) )
    return warning("retractall/1: cannot retract from a foreign predicate");
  if ( true(def, LOCKED) && false(def, DYNAMIC) )
    return warning("retractall/1: Attempt to retract from a system predicate");

  def->references++;
  for(cref = def->definition.clauses; cref; cref = cref->next)
  { bool det;

    if ( isTerm(*head) )
    { cref = findClause(cref, argTermP(*head, 0), def, &det);
    } else
    { while( cref && true(cref->clause, ERASED) )
	cref = cref->next;
      if ( cref )
	det = !cref->next;
    }

    if ( cref )
    { mark m;
    
      Mark(m);
      if ( decompileHead(cref->clause, head) )
	retractClauseProcedure(proc, cref->clause);
      Undo(m);

      if ( det )
	break;
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
pl_abolish(Word atom, Word arity)
{ FunctorDef f;
  Procedure proc;
  Module m = (Module) NULL;

  if ((atom = stripModule(atom, &m)) == (Word) NULL)
    fail;

  if (!isAtom(*atom) || !isInteger(*arity))
    return warning("abolish/2: instantiation fault");

  if ((f = isCurrentFunctor((Atom)*atom, (int)valNum(*arity))) == (FunctorDef) NULL)
    succeed;
  if ((proc = isCurrentProcedure(f, m)) == (Procedure) NULL)
    succeed;

  if ( true(proc->definition, LOCKED) && !SYSTEM_MODE && m == MODULE_system )
    return warning("abolish/2: attempt to abolish a system predicate");

  return abolishProcedure(proc, m);
}


static unsigned long
attribute_mask(Atom key)
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
  if (key == ATOM_transparent)	 return TRANSPARENT;
  if (key == ATOM_discontiguous) return DISCONTIGUOUS;
  if (key == ATOM_volatile)	 return VOLATILE;

  return 0;
}


word
pl_get_predicate_attribute(Word pred, Word what, Word value)
{ Procedure proc;
  FunctorDef fd;
  Definition def;
  Atom key;
  Module module = (Module) NULL;
  unsigned long att;

  pred = stripModule(pred, &module);
  if ( isAtom(*pred) )
    fd = lookupFunctorDef((Atom) *pred, 0);
  else if ( isTerm(*pred) )
    fd = functorTerm(*pred);
  else
    fail;

  proc = resolveProcedure(fd, module);
  def = proc->definition;

  if (!isAtom(*what) )
    return warning("$get_predicate_attribute/3: key should be an atom");
  key = (Atom) *what;

  if (key == ATOM_imported)
  { if (module == def->module)
      fail;
    return unifyAtomic(value, def->module->name);
  } else if (key == ATOM_indexed)
  { if (def->indexPattern == 0x0)
      fail;
    return indexPatternToTerm(proc, value);
  } else if (key == ATOM_exported)
  { return unifyAtomic(value, consNum(isPublicModule(module, proc)));
  } else if (key == ATOM_defined)
  { return unifyAtomic(value, consNum(true(def, FOREIGN) ||
				      def->definition.clauses ? 1 : 0));
  } else if (key == ATOM_line_count)
  { int line;

    if ( false(def, FOREIGN) &&
	 def->definition.clauses &&
	 (line=def->definition.clauses->clause->line_no) )
      return unifyAtomic(value, consNum(line));
    else
      fail;
  } else if (key == ATOM_foreign)
  { return unifyAtomic(value, consNum((def->flags & FOREIGN) ? 1 : 0));
  } else if (key == ATOM_hashed)
  { return unifyAtomic(value, consNum(def->hash_info ? def->hash_info->buckets
				      		     : 0));
  } else if (key == ATOM_references)
  { return unifyAtomic(value, consNum(def->references));
  } else if ( (att = attribute_mask(key)) )
  { return unifyAtomic(value, consNum((def->flags & att) ? 1 : 0));
  } else
  { return warning("$get_predicate_attribute/3: unknown key: %s",
		   stringAtom(key));
  }
}
  

word
pl_set_predicate_attribute(Word pred, Word what, Word value)
{ Procedure proc;
  FunctorDef fd;
  Definition def;
  Atom key;
  Module module = (Module) NULL;
  unsigned long att;
  int nodef;				/* does not define pred */

  if ( !isAtom(*what) ||
       (!isInteger(*value) || (valNum(*value) & ~1)) )
    return warning("$set_predicate_attribute/3: instantiation fault");
  key = (Atom) *what;
  if ( !(att = attribute_mask(key)) )
    return warning("$set_predicate_attribute/4: unknown key: %s",
		   stringAtom(key));

  nodef = (att & (TRACE_ANY|SPY_ME));

  pred = stripModule(pred, &module);
  if ( isAtom(*pred) )
    fd = lookupFunctorDef((Atom) *pred, 0);
  else if ( isTerm(*pred) )
    fd = functorTerm(*pred);
  else
    fail;

  proc = (nodef ? resolveProcedure(fd, module)
	        : lookupProcedureToDefine(fd, module));
  if ( !proc )
    fail;
  def = proc->definition;

  if ( *value == consNum(0) )
  { clear(def, att);
  } else
  { set(def, att);
    if ( (att == DYNAMIC || att == MULTIFILE) && SYSTEM_MODE )
    { set(def, SYSTEM|HIDE_CHILDS);
    }
/*  if ( (att == DYNAMIC) )
      clear(def, AUTOINDEX); */
  }

  succeed;
}


word
pl_default_predicate(Word d1, Word d2)
{ Procedure p1, p2;

  if ( (p1 = findProcedure(d1)) && (p2 = findProcedure(d2)) )
  { if ( p1->definition == p2->definition || !isDefinedProcedure(p1) )
      succeed;
  }

  fail;
}


void
reindexDefinition(Definition def)
{ ClauseRef cref;
  int do_hash = 0;

  DEBUG(2, if ( def->definition.clauses )
	   { Procedure proc = def->definition.clauses->clause->procedure;

	     Sdprintf("reindexDefinition(%s)\n", procedureName(proc));
	   });

  if ( true(def, AUTOINDEX) )
  { int canindex = 0;
    int cannotindex = 0;
    
    for(cref = def->definition.clauses; cref; cref = cref->next)
    { mark m;
      Word a1;
    
      Mark(m);      
      a1 = newTerm();
      decompileArg1(cref->clause, a1);
      if ( isVar(*a1) || isIndirect(*a1) )
	cannotindex++;
      else
	canindex++;
      Undo(m);
    }
    if ( canindex == 0 )
    { DEBUG(2, if ( def->definition.clauses )
	       { Procedure proc = def->definition.clauses->clause->procedure;

		 Sdprintf("not indexed: %s\n", procedureName(proc));
	       });
      def->indexPattern = 0x0;
    } else
    { def->indexPattern = 0x1;
      if ( (cannotindex * 8 < canindex) && (canindex > 5) )
	do_hash = canindex / 2;
    }
  }

  def->indexPattern &= ~NEED_REINDEX;
  def->indexCardinality = cardinalityPattern(def->indexPattern);
  for(cref = def->definition.clauses; cref; cref = cref->next)
    reindexClause(cref->clause);

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
pl_index(Word pred)
{ Procedure proc = findCreateProcedure(pred);
  Definition def = proc->definition;
  Module module = (Module) NULL;
  Word head = stripModule(pred, &module);
  Word arg;
  int arity, a;
  unsigned long pattern = 0x0;
  int card = 0;

  if (head == (Word) NULL)
    fail;

  if (!isTerm(*head) )			/* :- index(foo) */
    succeed;
  arity = def->functor->arity;
  for(a = 0; a < arity && a < 31; a++)
  { arg = argTermP(*head, a);
    deRef(arg);
    if (!isInteger(*arg) || valNum(*arg) > 1 || valNum(*arg) < 0)
      return warning("index/1: %s: illegal index specification", 
					procedureName(proc));
    if (valNum(*arg) == 1)
    { pattern |= 1 << a;
      if (++card == 4)		/* maximal 4 indexed arguments */
	break;
    }
  }

  if (true(def, FOREIGN))
    return warning("index/1: cannot index foreign predicate %s", 
		   procedureName(proc));

   
  clear(def, AUTOINDEX);
  if ( (def->indexPattern & ~NEED_REINDEX) == pattern)
    succeed;
  def->indexPattern = (pattern | NEED_REINDEX);

  succeed;
}


word
pl_get_clause_attribute(Word ref, Word att, Word value)
{ Clause clause;
  Atom a = (Atom) *att;
  word result;

  if ( !isInteger(*ref) ||
       !(clause = (Clause) numToPointer(*ref)) ||
       !inCore(clause) || !isClause(clause) )
    return warning("$clause_attribute/3: illegal reference");

  if ( a == ATOM_line_count )
  { if ( !clause->line_no )
      fail;
    else
      result = (word) consNum(clause->line_no);
  } else if ( a == ATOM_file )
  { SourceFile sf = indexToSourceFile(clause->source_no);
    
    if ( sf )
      result = (word) sf->name;
    else
      fail;
  } else
    fail;

  return unifyAtomic(value, result);
}


		/********************************
		*         SOURCE FILE           *
		*********************************/

SourceFile
lookupSourceFile(Atom name)
{ SourceFile file;
  static int index = 0;
  static Table sourceTable = NULL;
  Symbol s;

  if ( !sourceTable )
    sourceTable = newHTable(32);

  if ( (s=lookupHTable(sourceTable, name)) )
    return (SourceFile) s->value;

  file = (SourceFile) allocHeap(sizeof(struct sourceFile) );
  file->name = name;
  file->count = 0;
  file->time = 0L;
  file->index = ++index;
  file->system = status.boot;
  file->procedures = NULL;
  file->next = NULL;

  if ( sourceFileTable == NULL )
  { sourceFileTable = tailSourceFileTable = file;
  } else
  { tailSourceFileTable->next = file;
    tailSourceFileTable = file;
  }

  addHTable(sourceTable, name, file);

  return file;
}


SourceFile
isCurrentSourceFile(Atom name)
{ SourceFile file;

  for(file=sourceFileTable; file; file=file->next)
  { if (file->name == name)
      return file;
  }

  return (SourceFile) NULL;
}


SourceFile
indexToSourceFile(int index)
{ SourceFile file;

  for(file=sourceFileTable; file; file=file->next)
  { if (file->index == index)
      return file;
  }

  return NULL;
}


void
addProcedureSourceFile(SourceFile sf, Procedure proc)
{ ListCell cell;

  if ( true(proc->definition, FILE_ASSIGNED) )
  { for(cell=sf->procedures; cell; cell = cell->next)
      if ( cell->value == proc )
	return;
  }

  startCritical;
  cell = allocHeap(sizeof(struct list_cell));
  cell->value = proc;
  cell->next = sf->procedures;
  sf->procedures = cell;
  set(proc->definition, FILE_ASSIGNED);
  endCritical;
}


word
pl_make_system_source_files(void)
{ SourceFile file;

  for(file=sourceFileTable; file; file=file->next)
    file->system = TRUE;

  succeed;
}

word
pl_source_file(Word descr, Word file)
{ Procedure proc;
  ClauseRef cref;
  SourceFile sf;

  if ( !(proc = findProcedure(descr)) ||
       !proc->definition ||
       true(proc->definition, FOREIGN) ||
       !(cref = proc->definition->definition.clauses) ||
       !(sf = indexToSourceFile(cref->clause->source_no)) )
    fail;

  return unifyAtomic(file, sf->name);
}

word
pl_time_source_file(Word file, Word time, word h)
{ SourceFile fr;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      fr = sourceFileTable;
      break;
    case FRG_REDO:
      fr = (SourceFile) ForeignContextAddress(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  for(;fr != (SourceFile) NULL; fr = fr->next)
  { if ( fr->system == TRUE )
      continue;
    if ( unifyAtomic(file, fr->name) &&
         unifyTime(time, fr->time) )
    { if (fr->next != (SourceFile) NULL)
	ForeignRedo(fr->next);
      else
	succeed;
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
pl_start_consult(Word file)
{ SourceFile f;

  if (!isAtom(*file) )
    fail;
  f = lookupSourceFile((Atom)*file);
  f->time = LastModifiedFile(stringAtom(*file));

  startConsult(f);

  succeed;
}
