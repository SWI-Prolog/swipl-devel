/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Procedure (re) allocation
*/

#include "pl-incl.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
General  handling  of  procedures:  creation;  adding/removing  clauses;
finding source files, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

forwards void	resetReferencesModule P((Module));
forwards bool	attribute P((Definition, Word, short));
forwards bool	autoImport P((FunctorDef, Module));

SourceFile sourceFileTable = (SourceFile) NULL;
SourceFile tailSourceFileTable = (SourceFile) NULL;
SourceFile isCurrentSourceFile();

Procedure
lookupProcedure(f, m)
FunctorDef f;
Module m;
{ Procedure proc;
  register Definition def;
  Symbol s;
  
  if ((s = lookupHTable(m->procedures, f)) != (Symbol) NULL)
    return (Procedure) s->value;

  proc = (Procedure)  allocHeap(sizeof(struct procedure));
  def  = (Definition) allocHeap(sizeof(struct definition));
  proc->type = PROCEDURE_TYPE;
  proc->functor = f;
  proc->definition = def;
  def->module = m;
  addHTable(m->procedures, f, proc);
  statistics.predicates++;

  def->definition.clauses = (Clause) NULL;
  def->lastClause = (Clause) NULL;
#if O_PROFILE
  def->profile_ticks = 0;
  def->profile_calls = 0;
  def->profile_redos = 0;
  def->profile_fails = 0;
#endif /* O_PROFILE */
  clearFlags(def);
  resetProcedure(proc);

  return proc;
}

void
resetProcedure(proc)
Procedure proc;
{ register Definition def = proc->definition;

  def->flags ^= def->flags & ~SPY_ME;	/* Preserve the spy flag */
  def->source = (SourceFile) NULL;
  def->source_count = 0;
#if O_AUTOINDEX
  set(def, TRACE_ME|AUTOINDEX);
  def->indexPattern = 0x0;
  def->indexCardinality = 0;
  def->indexMerit = 0;
#else
  set(def, TRACE_ME);
  if ( proc->functor->arity == 0 )
  { def->indexPattern = 0x0;
    def->indexCardinality = 0;
  } else
  { def->indexPattern = 0x1;
    def->indexCardinality = 1;
  }
#endif
}

Procedure
isCurrentProcedure(f, m)
FunctorDef f;
Module m;
{ Symbol s;

  if ((s = lookupHTable(m->procedures, f)) != (Symbol) NULL)
    return (Procedure) s->value;

  return (Procedure) NULL;
}

bool
isDefinedProcedure(proc)
register Procedure proc;
{ if ( /* true(proc->definition, FOREIGN) || not needed; union */
       proc->definition->definition.clauses != (Clause) NULL ||
       true(proc->definition, DYNAMIC) )
    succeed;
  fail;
}

/*  Find a procedure from description `descr'. `descr' is one of:
    <term> or <module>:<term>. If the procedure does not exists NULL
    is returned.

 ** Tue Apr 19 16:11:25 1988  jan@swivax.UUCP (Jan Wielemaker)  */

Procedure
findProcedure(descr)
Word descr;
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
findCreateProcedure(descr)
Word descr;
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
linking from super modules and dynamoc loading from the  libraries,  the
definition  of current predicate has become a difficult issue.  Normally
it is used for meta-programming and program analysis.  I think it should
succeed  for  each  predicate  that  can   be   called.    The   current
implementation  is VERY slow due to all Prolog overhead.  This should be
reconsidered and probably a large part of this function should be  moved
to C.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_current_predicate(name, functor, h)
Word name, functor;
word h;
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
  { proc = (Procedure) symb->value;

    if (n != (Atom) NULL && n != proc->functor->name)
      continue;

    if (unifyAtomic(name, proc->functor->name) == FALSE)
      continue;
    if (unifyFunctor(functor, proc->functor) == FALSE)
      continue;

    if ((symb = nextHTable(m->procedures, symb)) != (Symbol) NULL)
      ForeignRedo(symb);

    succeed;
  }

  fail;
}


/*  Assert a clause to a procedure. Where askes to assert either at the
    head or at the tail of the clause list. It should be instantiated
    to ether 'a' or 'z'.

 ** Fri Apr 29 12:44:08 1988  jan@swivax.UUCP (Jan Wielemaker)  */

#if PROTO
bool
assertProcedure(Procedure proc, Clause clause, char where)
#else
bool
assertProcedure(proc, clause, where)
Procedure proc;
Clause clause;
char where;
#endif
{ register Definition def = proc->definition;

  startCritical;
  if (def->lastClause == (Clause) NULL)
  { def->definition.clauses = def->lastClause = clause;
  } else if (where == 'a')
  { clause->next = def->definition.clauses;
    def->definition.clauses = clause;
  } else
  { Clause last = def->lastClause;

    last->next = clause;
    def->lastClause = clause;
    
#if O_AUTOINDEX
    if ( true(def, AUTOINDEX) && def->indexPattern == 0x0 )
    { if ( true(clause, INDEXABLE) )
      { if ( true(last, INDEXABLE) )
	{ def->indexMerit += 1;		/* indexing gains on this clause */

	  if ( def->indexMerit > 1 )	/* good enough? */
	  { def->indexPattern = 0x1;
	    def->indexCardinality = 1;
	    reindexProcedure(proc);
	  }
	}
      } else
      { def->indexMerit -= 2;		/* indexing looses on this one */
      }
    }
#endif /* O_AUTOINDEX */
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
abolishProcedure(proc, module)
Procedure proc;
Module module;
{ register Definition def = proc->definition;

  if ( def->module != module )		/* imported predicate; remove link */
  { def  = (Definition) allocHeap(sizeof(struct definition));
    proc->definition = def;
    def->module = module;
    def->definition.clauses = (Clause) NULL;
    def->lastClause = (Clause) NULL;
#if O_PROFILE
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
    def->definition.clauses = def->lastClause = (Clause) NULL;
    resetProcedure(proc);
    endCritical;

    succeed;
  }

  removeClausesProcedure(proc);
  resetProcedure(proc);

  succeed;
}

void
removeClausesProcedure(proc)
Procedure proc;
{ Definition def = proc->definition;
  Clause c, next;

  startCritical;
  for(c = def->definition.clauses; c; c = next)
  { next = c->next;
    if (c->references == 0)
    { freeClause(c);
    } else
    { set(c, ERASED);
      c->next = (Clause) NULL;
    }
  }
  def->definition.clauses = def->lastClause = (Clause) NULL;

  endCritical;
}

/*  Retract a clause from a procedure.  When a clause without references
    is  retracted  it  is  actually removed from the heap, otherwise the
    clause is unlinked and marked as `erased'.  Its  next  pointer  will
    not be changed.  to avoid the follow up clause to be destroyed it is
    given an extra reference.

 ** Sun Apr 17 16:28:32 1988  jan@swivax.UUCP (Jan Wielemaker)  */

bool
retractClauseProcedure(proc, clause)
Procedure proc;
Clause clause;
{ Clause prev = (Clause) NULL;
  Clause c;
  register Definition def = proc->definition;

  for(c = def->definition.clauses; c; prev = c, c = c->next)
  { if (c == clause)
    { startCritical;
      if (prev == (Clause) NULL)
      { def->definition.clauses = c->next;
	if (c->next == (Clause) NULL)
	  def->lastClause = (Clause) NULL;
      } else
      { prev->next = c->next;
	if (c->next == (Clause) NULL)
	  def->lastClause = prev;
      }
      if (c->references == 0)
      { freeClause(c);
      } else
      { set(clause, ERASED);
	if (clause->next)
	  clause->next->references++;
      }
      endCritical;

      succeed;
    }
  }

  fail;
}

void
unallocClause(clause)
Clause clause;
{ DEBUG(1, Word w = newTerm();
	   decompile(clause, w);
	   Putf("removing clause ");
	   pl_write(w);
	   Putf(" of %s\n", procedureName(clause->procedure));
       );

  if ( clause->next &&
       --clause->next->references == 0 &&
       true(clause->next, ERASED) )
    unallocClause(clause->next);

  freeClause(clause);
}

void
freeClause(c)
Clause c;
{ if (c->XR_size)
  { freeHeap(c->externals, sizeof(word) * c->XR_size);
    statistics.externals -= c->XR_size;
  }
  statistics.codes -= c->code_size;
  freeHeap(c->codes, sizeof(code) * c->code_size);
  freeHeap(c, sizeof(struct clause));
}

/*  resetReferences() sets all clause reference counts to zero. It is
    called by abort().

 ** Fri May 27 10:36:14 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static void
resetReferencesModule(m)
Module m;
{ Definition def;
  Symbol s;
  Clause clause;

  for_table(s, m->procedures)
  { def = ((Procedure) s->value)->definition;
#if O_PROFILE
    clear(def, PROFILE_TICKED);
#endif /* O_PROFILE */
    if ( true(def, FOREIGN) )
      continue;

    for(clause=def->definition.clauses; clause; clause = clause->next)
      clause->references = 0;
  }
}

void
resetReferences()
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
resolveProcedure(f, module)
FunctorDef f;
Module module;
{ Procedure proc;
  Module m;

  for( m = module; m != (Module) NULL; m = m->super )
  { if ( (proc = isCurrentProcedure(f, m)) != (Procedure) NULL &&
       isDefinedProcedure(proc) )
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

static bool
autoImport(f, m)
FunctorDef f;
Module m;
{ Procedure proc, p;
					/* Defined: no problem */
  if ( (proc = isCurrentProcedure(f, m)) != NULL &&
       isDefinedProcedure(proc) )
    succeed;
  
  if ( m->super == (Module) NULL )	/* No super: can't import */
    fail;

  TRY( autoImport(f, m->super) );	/* Import in super */

  p = isCurrentProcedure(f, m->super);	/* Link the two */
  if ( proc == NULL )			/* Create header if not there */
    proc = lookupProcedure(f, m);
					/* save? */
  freeHeap(proc->definition, sizeof(struct definition));
  proc->definition = p->definition;

  succeed;
}

void
trapUndefined(proc)
Procedure proc;
{ int retry_times = 0;

  retry:
					/* Auto import */
  if ( autoImport(proc->functor, proc->definition->module) == TRUE )
    return;
					/* Pred/Module does not want to trap */
  if ( true(proc->definition, DYNAMIC) ||
       false(proc->definition->module, UNKNOWN) )
    return;
					/* Trap via exception/3 */
  if ( status.boot == FALSE )
  { word goal;
    mark m;
    bool rval;
  
    Mark(m);
    goal = globalFunctor(FUNCTOR_undefinterc3);
    unifyAtomic(argTermP(goal, 0), proc->definition->module->name);
    unifyAtomic(argTermP(goal, 1), proc->functor->name);
    unifyAtomic(argTermP(goal, 2), consNum(proc->functor->arity));

    debugstatus.suspendTrace++;
    rval = callGoal(MODULE_system, goal, FALSE);
    debugstatus.suspendTrace--;

    Undo(m);

    if ( rval == TRUE )
    { extern int trace_continuation;	/* from pl-trace.c */

      switch( trace_continuation )
      { case ACTION_FAIL:
	  return;
	case ACTION_RETRY:
	  if ( retry_times++ )
	  { warning("exception handler failed to define predicate %s\n",
		    procedureName(proc));
	    break;
	  }
	  goto retry;
	default:
	  warning("Illegal return value from exception handler");
      }
    }
  }

					/* No one want to intercept */
  warning("Undefined predicate: %s", procedureName(proc) );
}

		/********************************
		*            RETRACT            *
		*********************************/

word
pl_retract(term, h)
Word term;
word h;
{ Procedure proc;
  Word head, body;
  Module m = (Module) NULL;
  Clause clause;

  if ( ForeignControl(h) == FRG_CUTTED )
  { clause = (Clause) ForeignContextAddress(h);
    leaveClause(clause);			/* dereference it */

    succeed;
  }

  if ((term = stripModule(term, &m)) == (Word) NULL)
    fail;

  if (splitClause(term, &head, &body) == FALSE)
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

    if ( true(proc->definition, FOREIGN) )
      return warning("retract/1: cannot retract from a foreign predicate");
    if ( true(proc->definition, SYSTEM) && false(proc->definition, DYNAMIC) )
      return warning("retract/1: Attempt to retract from a system predicate");

    clause = proc->definition->definition.clauses;
  } else
  { Clause next;			/* dereference the old one */

    clause = (Clause) ForeignContextAddress(h);
    for( next = clause; next && true(next, ERASED); next = next->next )
      ;
    leaveClause(clause);
    clause = next;
  }

  for(; clause; clause = clause->next)
  { Clause next;
    bool det;

    if (isTerm(*head) )
    { if ((clause = findClause(clause, 
			       argTermP(*head, 0), 
			       clause->procedure->definition,
			       &det)) == (Clause) NULL)
	fail;
    } else if ( isAtom(*head) )
    { if ( true(clause, ERASED) )
	continue;
      det = (clause->next == NULL);
    } else
      return warning("retract/1: illegal clause head");

    { mark m;

      Mark(m);
      if (decompile(clause, term) == TRUE)
      { next = clause->next;
	retractClauseProcedure(clause->procedure, clause);
	unlockMark(&m);
	if ( det == TRUE )
	  succeed;
	next->references++;	/* avoid the next being deleted */

	ForeignRedo(next);
      }
      Undo(m);
    }

    continue;
  }

  fail;
}

		/********************************
		*       PROLOG PREDICATES       *
		*********************************/

word
pl_abolish(atom, arity)
Word atom, arity;
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

  if ( true(proc->definition, SYSTEM) && !SYSTEM_MODE && m == MODULE_system )
    return warning("abolish/2: attempt to abolish a system predicate");

  return abolishProcedure(proc, m);
}

word
pl_list_references(descr)
Word descr;
{ Procedure proc;
  Clause clause;

  if ((proc = findProcedure(descr)) == (Procedure) NULL)
    return warning("$list_references/1: no such predicate");

  if ( true(proc->definition, FOREIGN) )
    fail;
  for(clause=proc->definition->definition.clauses;
       clause;
       clause = clause->next)
    Putf("%d ", clause->references);

  Putf("\n");

  succeed;
}

word
pl_list_active_procedures()
{ Procedure proc;
  Module m;
  Clause clause;
  int nth;
  bool first;
  Symbol sm, sp;

  for_table(sm, moduleTable)
  { m = (Module) sm->value;
    for_table(sp, m->procedures)
    { proc = (Procedure) sp->value;

      if ( true(proc->definition, FOREIGN) ||	/* no clauses */
	   proc->definition->module != m)	/* imported */
	continue;

      first = TRUE;
      for(clause = proc->definition->definition.clauses, nth=1;
	   clause;
	   nth++, clause = clause->next)
      { if ( true(clause, ERASED) )
	  continue;
	if (clause->references != 0)
	{ if (first)
	  { Putf("%s: ", procedureName(proc) );
	    first = FALSE;
	  } else
	    Putf(", ");
	  Putf("%d: %d", nth, clause->references);
	}
      }
      if (first == FALSE)
	Putf("\n");
    }
  }

  succeed;
}

#if PROTO
static bool
attribute(Definition def, Word value, short att)
#else
static bool
attribute(def, value, att)
Definition def;
Word value;
short att;
#endif
{ if ( isVar(*value) )
    return unifyAtomic(value, consNum((def->flags & att) ? 1 : 0));

  switch((int) valNum(*value))
  { case 0:	clear(def, att);
		succeed;
    case 1:	set(def, att);
		if ( (att == DYNAMIC || att == MULTIFILE) && SYSTEM_MODE )
		{ set(def, SYSTEM);
		  set(def, HIDE_CHILDS);
		}
		succeed;
    default:	return sysError("$predicate_attribute/3: Illegal value");
  }
}

word
pl_predicate_attribute(pred, what, value)
Word pred, what, value;
{ Procedure proc;
  FunctorDef fd;
  Definition def;
  Atom key;
  Module module = (Module) NULL;

  pred = stripModule(pred, &module);
  if ( isAtom(*pred) )
    fd = lookupFunctorDef((Atom) *pred, 0);
  else if ( isTerm(*pred) )
    fd = functorTerm(*pred);
  else
    fail;

  proc = (isVar(value) ? resolveProcedure(fd, module)  /* lookup */
		       : lookupProcedure(fd, module)); /* set */
  def = proc->definition;

  if (!isAtom(*what) )
    return warning("$predicate_attribute/3: key should be an atom");
  key = (Atom) *what;

  if (key == ATOM_imported)
  { if (module == def->module)
      fail;
    return unifyAtomic(value, def->module->name);
  }
  if (key == ATOM_indexed)
  { if (def->indexPattern == 0x0)
      fail;
    return indexPatternToTerm(proc, value);
  }

  if (!isVar(*value) && (!isInteger(*value) || (valNum(*value) & ~1)))
    return warning("$predicate_attribute/3: illegal 3rd argument");

#define TRACE_ANY (TRACE_CALL|TRACE_REDO|TRACE_EXIT|TRACE_FAIL)

  if (key == ATOM_dynamic)	return attribute(def, value, DYNAMIC);
  if (key == ATOM_multifile)	return attribute(def, value, MULTIFILE);
  if (key == ATOM_system)	return attribute(def, value, SYSTEM);
  if (key == ATOM_spy)		return attribute(def, value, SPY_ME);
  if (key == ATOM_trace)	return attribute(def, value, TRACE_ME);
  if (key == ATOM_trace_call)	return attribute(def, value, TRACE_CALL);
  if (key == ATOM_trace_redo)	return attribute(def, value, TRACE_REDO);
  if (key == ATOM_trace_exit)	return attribute(def, value, TRACE_EXIT);
  if (key == ATOM_trace_fail)	return attribute(def, value, TRACE_FAIL);
  if (key == ATOM_trace_any)	return attribute(def, value, TRACE_ANY);
  if (key == ATOM_hide_childs)	return attribute(def, value, HIDE_CHILDS);
  if (key == ATOM_transparent)	return attribute(def, value, TRANSPARENT);
  if (key == ATOM_discontiguous) return attribute(def,value, DISCONTIGUOUS);
  if (key == ATOM_foreign)
    return unifyAtomic(value, consNum(true(def, FOREIGN) ? 1 : 0));
  if (key == ATOM_exported)
    return unifyAtomic(value, consNum(isPublicModule(module, proc)));
  if (key == ATOM_defined)
    return unifyAtomic(value, consNum(true(def, FOREIGN) ||
				      def->definition.clauses ? 1 : 0) );
  else
    return warning("$predicate_attribute/4: unknown key");
}


void
reindexProcedure(proc)
Procedure proc;
{ register Clause cl;

  for(cl = proc->definition->definition.clauses; cl; cl = cl->next)
    reindexClause(cl);
}


word
pl_index(pred)
Word pred;
{ Procedure proc = findCreateProcedure(pred);
  Module module = (Module) NULL;
  Word head = stripModule(pred, &module);
  Word arg;
  int arity, a;
  ulong pattern = 0x0;
  int card = 0;

  if (head == (Word) NULL)
    fail;

  if (!isTerm(*head) )			/* :- index(foo) */
    succeed;
  arity = proc->functor->arity;
  for(a = 0; a < arity; a++)
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

#if O_AUTOINDEX
  clear(proc->definition, AUTOINDEX);
#endif
  if (proc->definition->indexPattern == pattern)
    succeed;

  if (true(proc->definition, FOREIGN))
    return warning("index/1: cannot index foreign predicate %s", 
					procedureName(proc));

  proc->definition->indexPattern = pattern;
  proc->definition->indexCardinality = card;

  reindexProcedure(proc);

  succeed;
}

		/********************************
		*         SOURCE FILE           *
		*********************************/

SourceFile
lookupSourceFile(name)
Atom name;
{ SourceFile file;

  for(file=sourceFileTable; file; file=file->next)
  { if (file->name == name)
      return file;
  }
  file = (SourceFile) allocHeap(sizeof(struct sourceFile) );
  file->name = name;
  file->count = 0;
  file->time = 0L;
  file->system = status.boot;
  file->next = (SourceFile) NULL;

  if ( sourceFileTable == (SourceFile) NULL )
  { sourceFileTable = tailSourceFileTable = file;
  } else
  { tailSourceFileTable->next = file;
    tailSourceFileTable = file;
  }

  return file;
}

SourceFile
isCurrentSourceFile(name)
Atom name;
{ SourceFile file;

  for(file=sourceFileTable; file; file=file->next)
  { if (file->name == name)
      return file;
  }

  return (SourceFile) NULL;
}

word
pl_make_system_source_files()
{ SourceFile file;

  for(file=sourceFileTable; file; file=file->next)
    file->system = TRUE;

  succeed;
}

word
pl_source_file(descr, file)
Word descr, file;
{ Procedure proc;

  if ((proc = findProcedure(descr)) == (Procedure) NULL)
    fail;
  if (proc->definition->source == (SourceFile) NULL)
    fail;

  return unifyAtomic(file, proc->definition->source->name);
}

word
pl_time_source_file(file, time, h)
Word file, time;
word h;
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

word
pl_start_consult(file)
Word file;
{ SourceFile f;

  if (!isAtom(*file) )
    fail;
  f = lookupSourceFile((Atom)*file);
  f->count++;
  f->time = LastModifiedFile(stringAtom(*file));

  succeed;
}
