/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2009, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "pl-dbref.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
General  handling  of  procedures:  creation;  adding/removing  clauses;
finding source files, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define LOCK()   PL_LOCK(L_PREDICATE)
#define UNLOCK() PL_UNLOCK(L_PREDICATE)
#undef LD
#define LD LOCAL_LD

static void	resetReferencesModule(Module);
static void	resetProcedure(Procedure proc, bool isnew);
static int	removeClausesProcedure(Procedure proc, int sfindex, int file);
static atom_t	autoLoader(Definition def);
static void	registerDirtyDefinition(Definition def);
static Procedure visibleProcedure(functor_t f, Module m);
static void	detachMutexAndUnlock(Definition def);
static ClauseRef cleanDefinition(Definition def, ClauseRef garbage);

Procedure
lookupProcedure(functor_t f, Module m)
{ Procedure proc;
  Definition def;
  Symbol s;

  LOCKMODULE(m);
  if ( (s = lookupHTable(m->procedures, (void *)f)) )
  { DEBUG(3, Sdprintf("lookupProcedure() --> %s\n", procedureName(s->value)));
    proc = s->value;
  } else
  { GET_LD

    proc = (Procedure)  allocHeap(sizeof(struct procedure));
    def  = (Definition) allocHeap(sizeof(struct definition));
    proc->type = PROCEDURE_TYPE;
    proc->definition = def;

    memset(def, 0, sizeof(*def));
    def->functor = valueFunctor(f);
    def->module  = m;
    addHTable(m->procedures, (void *)f, proc);
    GD->statistics.predicates++;

    resetProcedure(proc, TRUE);
    DEBUG(3, Sdprintf("Created %s\n", procedureName(proc)));
  }
  UNLOCKMODULE(m);

  return proc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Add (import) a definition to a  module.   Used  by loadImport(). Must be
merged with pl_import().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
importDefinitionModule(Module m, Definition def)
{ functor_t functor = def->functor->functor;
  Procedure proc;
  int rc = TRUE;
  Symbol s;

  LOCKMODULE(m);
  if ( (s = lookupHTable(m->procedures, (void *)functor)) )
  { proc = s->value;

    if ( proc->definition == def )
      goto done;
    if ( !isDefinedProcedure(proc) )
    { proc->definition = def;		/* TBD: what about the old one */
      goto done;
    }
    rc = warning("Failed to import %s into %s",
		 predicateName(def), PL_atom_chars(m->name));
    goto done;
  } else
  { GET_LD

    proc = (Procedure) allocHeap(sizeof(struct procedure));
    proc->type = PROCEDURE_TYPE;
    proc->definition = def;
    addHTable(m->procedures, (void *)functor, proc);
    set(proc->definition, P_SHARED);
  }

done:
  UNLOCKMODULE(m);

  return rc;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
resetProcedure() is called  by  lookupProcedure()   for  new  ones,  and
abolishProcedure() by abolish/2. In the latter   case, abolish may leave
dirty clauses when called on a   running predicate. Hence, NEEDSCLAUSEGC
should be retained. Bug found by Paulo Moura, LogTalk developer.

There are two cases where a  complete  reset   is  safe:  if  this is an
unreferenced dynamic predicate and if this is   a  predicate that has no
clause-list. Such predicates can't be active  and can't become active as
that requires clauses which, even under  MT,   can  only  be added after
locking the L_PREDICATE mutex.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
resetProcedure(Procedure proc, bool isnew)
{ Definition def = proc->definition;

  if ( (true(def, DYNAMIC) && def->references == 0) ||
       def->definition.clauses == NULL )
    isnew = TRUE;

  def->flags ^= def->flags & ~(SPY_ME|NEEDSCLAUSEGC|P_SHARED|P_DIRTYREG);
  if ( stringAtom(def->functor->name)[0] != '$' )
    set(def, TRACE_ME);
  def->number_of_clauses = 0;

  if ( isnew )
  { def->indexCardinality = 0;
    def->indexPattern = (0x0 | NEED_REINDEX);
    set(def, AUTOINDEX);

    if ( def->hash_info )
    { unallocClauseIndexTable(def->hash_info);
      def->hash_info = NULL;
    }
  }

  freeCodesDefinition(def);		/* carefully sets to S_VIRGIN */
}


Procedure
isCurrentProcedure(functor_t f, Module m)
{ Symbol s;

  if ( (s = lookupHTable(m->procedures, (void *)f)) )
    return (Procedure) s->value;

  return NULL;
}


ClauseRef
hasClausesDefinition(Definition def)
{ if ( def->definition.clauses )
  { if ( def->erased_clauses == 0 )
      return def->definition.clauses;
    else
    { GET_LD
      ClauseRef c;
#ifdef O_LOGICAL_UPDATE
      uintptr_t generation;
      LocalFrame fr = environment_frame;
      if ( fr )
	generation = generationFrame(fr);
      else
	generation = ~0L-1;		/* any non-erased clause */
#else
#define generation (0)
#endif

      for(c = def->definition.clauses; c; c = c->next)
      { Clause cl = c->clause;

	if ( visibleClause(cl, generation) )
	  return c;
      }
    }
  }

  fail;
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
       true(proc->definition, LOCKED) &&
       false(proc->definition, DYNAMIC) )
    return proc;

  return NULL;
}


static int
checkModifySystemProc(functor_t fd)
{ Procedure proc;

  if ( (proc = isStaticSystemProcedure(fd)) &&
       true(proc->definition, P_ISO) )
    return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, proc);

  succeed;
}


Procedure
lookupProcedureToDefine(functor_t def, Module m)
{ Procedure proc;

  if ( (proc = isCurrentProcedure(def, m)) && false(proc->definition, SYSTEM) )
    return proc;

  if ( checkModifySystemProc(def) )
    return lookupProcedure(def, m);

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_functor() translates term  of  the   format  +Name/+Arity  into  the
internal functor represenation. It fails and  raises an exception on the
various possible format or represenation errors.  ISO compliant.
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

    return PL_error(NULL, 0,
		    tostr(buf, "limit is %d, request = %d",
			  maxarity, a),
		    ERR_REPRESENTATION, ATOM_max_arity);
  }

  *arity = a;

  return TRUE;
}


int
get_functor(term_t descr, functor_t *fdef, Module *m, term_t h, int how)
{ GET_LD
  term_t head = PL_new_term_ref();
  int dcgpi=FALSE;

  PL_strip_module(descr, m, head);

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
  } else if ( PL_get_functor(head, fdef) )
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
get_head_functor(term_t head, functor_t *fdef, int how ARG_LD)
{ int arity;

  if ( !PL_get_functor(head, fdef) )
  { if ( how&GP_TYPE_QUIET )
      fail;
    else
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, head);
  }

  if ( (arity=arityFunctor(*fdef)) > MAXARITY )
  { if ( how&GP_TYPE_QUIET )
    { fail;
    } else
    { char buf[100];
      return PL_error(NULL, 0,
		      tostr(buf, "limit is %d, request = %d",
			    MAXARITY, arity),
		      ERR_REPRESENTATION, ATOM_max_arity);
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

int
get_procedure(term_t descr, Procedure *proc, term_t h, int how)
{ Module m = (Module) NULL;
  functor_t fdef;
  Procedure p;

  if ( (how&GP_NAMEARITY) )
  { if ( !get_functor(descr, &fdef, &m, h,
		      GF_PROCEDURE|(how&GP_TYPE_QUIET)) )
      fail;
  } else
  { GET_LD
    term_t head = PL_new_term_ref();

    PL_strip_module(descr, &m, head);

    if ( h )
      PL_put_term(h, head);

    if ( !get_head_functor(head, &fdef, how PASS_LD) )
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
      goto notfound;
    case GP_FIND:
      if ( (p=visibleProcedure(fdef, m)) )
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
  Module	module;			/* Module to search in */
  Module	super;			/* Walking along super-chain */
  TableEnum	epred;			/* Predicate enumerator */
  TableEnum	emod;			/* Module enumerator */
} cur_enum;


static Procedure
visibleProcedure(functor_t f, Module m)
{ ListCell c;
  Procedure p;

  for(;;)
  { next:

    if ( (p = isCurrentProcedure(f, m)) && isDefinedProcedure(p) )
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


foreign_t
pl_current_predicate1(term_t spec, control_t ctx)
{ GET_LD
  cur_enum e0;
  cur_enum *e;
  Symbol sp, sm;
  int rval = FALSE;
  term_t mt = 0;			/* module-term */
  term_t nt = 0;			/* name-term */
  term_t at = 0;			/* arity-term */
  unsigned int aextra = 0;

  if ( ForeignControl(ctx) != FRG_CUTTED )
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

  switch( ForeignControl(ctx) )
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
	{ e->emod = newTableEnum(GD->tables.modules);

	  if ( (sm = advanceTableEnum(e->emod)) )
	    e->module = sm->value;
	  else
	    fail;			/* no modules!? */
	} else if ( PL_get_atom_ex(mt, &mname) )
	{ e->module = isCurrentModule(mname);
	  if ( !e->module )
	    fail;
	} else
	{ fail;
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
	  return visibleProcedure(e->functor, e->module) ? TRUE : FALSE;
      } else
      { e->epred = newTableEnum(e->module->procedures);
      }

      e = allocHeap(sizeof(*e));
      *e = e0;
      break;
    }
    case FRG_REDO:
      e = ForeignContextPtr(ctx);
      break;
    case FRG_CUTTED:
    { e = ForeignContextPtr(ctx);
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

	if ( (sm = advanceTableEnum(e->emod)) )
	{ e->module = sm->value;
	  ForeignRedoPtr(e);
	} else
	  succeed;
      }
    } else
    { while( (sp = advanceTableEnum(e->epred)) )
      { FunctorDef fd = valueFunctor((functor_t)sp->name);
	Procedure proc = sp->value;

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
    { while( (sm = advanceTableEnum(e->emod)) )
      { Module m = sm->value;

					/* skip hidden modules */
	if ( stringAtom(m->name)[0] != '$' || SYSTEM_MODE )
	  break;
      }
      if ( sm )
	e->super = e->module = sm->value;
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
      freeTableEnum(e->emod);
    freeHeap(e, sizeof(*e));
  }

  return rval;

typeerror:
  return PL_error(NULL, 0, NULL, ERR_TYPE,
		  ATOM_predicate_indicator, spec);
}


		 /*******************************
		 *	 CLAUSE REFERENCES	*
		 *******************************/


ClauseRef
newClauseRef(Clause clause ARG_LD)
{ ClauseRef cref = allocHeap(sizeof(struct clause_ref));

  cref->clause = clause;
  cref->next   = NULL;

  return cref;
}


void
freeClauseRef(ClauseRef cref ARG_LD)
{ freeHeap(cref, sizeof(struct clause_ref));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Assert a clause to a procedure. Where askes to assert either at the head
or at the tail of the clause list.

(*) This function updates the indexing information.  If we have a static
procedure, it deletes the supervisor. This is  probably a bit rough, but
deals with -for example- clauses for   term_expansion/2. After the first
definition this will be  called  and   an  S_TRUSTME  supervisor will be
installed, causing further clauses to have no effect.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ClauseRef
assertProcedure(Procedure proc, Clause clause, int where ARG_LD)
{ Definition def = getProcDefinition(proc);
  ClauseRef cref = newClauseRef(clause PASS_LD);

  if ( def->references && (debugstatus.styleCheck & DYNAMIC_STYLE) )
    printMessage(ATOM_informational,
		 PL_FUNCTOR_CHARS, "modify_active_procedure", 2,
		   PL_CHARS, "assert",
		   _PL_PREDICATE_INDICATOR, proc);

  LOCKDEF(def);
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
  PL_LOCK(L_MISC);
  clause->generation.created = ++GD->generation;
  PL_UNLOCK(L_MISC);
  clause->generation.erased  = ~0L;	/* infinite */
#endif

  if ( false(def, DYNAMIC) )		/* see (*) above */
    freeCodesDefinition(def);

  if ( def->hash_info )
  { assert(!(def->indexPattern & NEED_REINDEX));

    DEBUG(3,
	  if ( !clause->index.varmask )
	    Sdprintf("Adding non-indexed clause to %s\n", predicateName(def));
	 );

    addClauseToIndex(def, clause, where PASS_LD);
    if ( def->hash_info->size /2 > def->hash_info->buckets )
    { if ( false(def, NEEDSREHASH) )
      { set(def, NEEDSREHASH);
	DEBUG(2, Sdprintf("Asking re-hash for %s\n", predicateName(def)));
      }
      if ( true(def, DYNAMIC) && def->references == 0 )
      { gcClausesDefinitionAndUnlock(def); /* does UNLOCKDEF() */
	return cref;
      }
    }
  } else
  { if ( def->number_of_clauses == 25 &&
	 true(def, AUTOINDEX) )
    { DEBUG(2, Sdprintf("Request re-index for %s\n", predicateName(def)));
      def->indexPattern |= NEED_REINDEX;
    }
  }
  UNLOCKDEF(def);

  return cref;
}

/*  Abolish a procedure.  Referenced  clauses  are   unlinked  and left
    dangling in the dark until the procedure referencing it deletes it.

    Since we have a foreign language interface we will allow to  abolish
    foreign  predicates  as  well.  Permission testing should be done by
    the caller.

 ** Sun Apr 17 16:18:50 1988  jan@swivax.UUCP (Jan Wielemaker)  */

bool
abolishProcedure(Procedure proc, Module module)
{ GET_LD
  Definition def = proc->definition;

  DEBUG(2, Sdprintf("abolishProcedure(%s)\n", predicateName(def)));

  startCritical;
  LOCKDEF(def);
  if ( def->module != module )		/* imported predicate; remove link */
  { GET_LD
    Definition ndef	     = allocHeap(sizeof(struct definition));

    memset(ndef, 0, sizeof(*ndef));
    proc->definition         = ndef;
    ndef->functor            = def->functor; /* should be merged with */
    ndef->module             = module;	     /* lookupProcedure()!! */
    resetProcedure(proc, TRUE);
  } else if ( true(def, FOREIGN) )	/* foreign: make normal */
  { def->definition.clauses = def->lastClause = NULL;
    resetProcedure(proc, TRUE);
  } else if ( true(def, P_THREAD_LOCAL) )
  { UNLOCKDEF(def);
    if ( !endCritical )
      return FALSE;
    return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
		    ATOM_modify, ATOM_thread_local_procedure, proc);
  } else				/* normal Prolog procedure */
  { removeClausesProcedure(proc, 0, FALSE);

    if ( true(def, DYNAMIC) )
    { if ( def->references == 0 )
      { ClauseRef cref;

	resetProcedure(proc, FALSE);
	cref = cleanDefinition(def, NULL);
#ifdef O_PLMT
	detachMutexAndUnlock(def);
#endif
	if ( cref )
	  freeClauseList(cref);
      } else				/* dynamic --> static */
      { UNLOCKDYNDEF(def);		/* release private lock */
	setDynamicProcedure(proc, FALSE);
      }
      return endCritical;
    } else if ( true(def, NEEDSCLAUSEGC) )
    { registerDirtyDefinition(def);
    }

    resetProcedure(proc, FALSE);
  }
  UNLOCKDEF(def);
  return endCritical;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Remove (mark for  deletion)  all  clauses   that  come  from  the  given
source-file or any sourcefile. Note   that thread-local predicates don't
have clauses from files, so we don't   need to bother. Returns number of
clauses that is deleted.

MT: Caller must hold L_PREDICATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
removeClausesProcedure(Procedure proc, int sfindex, int fromfile)
{ Definition def = proc->definition;
  ClauseRef c;
  int deleted = 0;

#ifdef O_LOGICAL_UPDATE
  GD->generation++;
#endif

  if ( true(def, P_THREAD_LOCAL) )
    return deleted;

  for(c = def->definition.clauses; c; c = c->next)
  { Clause cl = c->clause;

    if ( (sfindex == 0 || sfindex == cl->source_no) &&
	 (!fromfile || cl->line_no > 0) &&
	 false(cl, ERASED) )
    { set(cl, ERASED);

      if ( deleted++ == 0 )
	set(def, NEEDSCLAUSEGC);

#ifdef O_LOGICAL_UPDATE
      cl->generation.erased = GD->generation;
#endif
      def->number_of_clauses--;
      def->erased_clauses++;
    }
  }
  if ( def->hash_info && deleted )
    def->hash_info->alldirty = TRUE;

  return deleted;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unlink a clause from the  definition,  both   from  the  index table and
clause-chain. The clause itself is not  deleted,   this  task is left to
retractClauseDefinition().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
unlinkClause(Definition def, Clause clause ARG_LD)
{ ClauseRef prev = NULL;
  ClauseRef c;

  startCritical;

  if ( def->hash_info )
    delClauseFromIndex(def, clause);

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


      freeClauseRef(c PASS_LD);
      def->number_of_clauses--;

      break;
    }
  }

  return endCritical;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Called from erase/1, retract/1 and retractall/1. In the latter two cases
the definition is always referenced.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
retractClauseDefinition(Definition def, Clause clause ARG_LD)
{ int rc;

  LOCKDYNDEF(def);
  assert(true(def, DYNAMIC));
  if ( true(clause, ERASED) )
  { UNLOCKDYNDEF(def);
    succeed;
  }

  set(clause, ERASED);

  if ( def->references ||
       def->number_of_clauses > 16 )
  { if ( def->hash_info )
    { markDirtyClauseIndex(def->hash_info, clause);
      if ( false(def, NEEDSREHASH) &&
	   def->hash_info->size * 4 < def->hash_info->buckets )
      { set(def, NEEDSREHASH);
      }
    }
    def->number_of_clauses--;
    def->erased_clauses++;
    if ( def->erased_clauses > def->number_of_clauses/(unsigned)16 )
    { set(def, NEEDSCLAUSEGC);
    }
#ifdef O_LOGICAL_UPDATE
    PL_LOCK(L_MISC);
    clause->generation.erased = ++GD->generation;
    PL_UNLOCK(L_MISC);
#endif
    UNLOCKDYNDEF(def);

    succeed;
  }

  rc = unlinkClause(def, clause PASS_LD);
  UNLOCKDYNDEF(def);

					/* as we do a call-back, we cannot */
					/* hold the L_PREDICATE mutex */
#if O_DEBUGGER
  if ( PROCEDURE_event_hook1 &&
       def != PROCEDURE_event_hook1->definition )
    callEventHook(PLEV_ERASED_CLAUSE, clause);
#endif

  freeClause(clause PASS_LD);

  return rc;
}


void
unallocClause(Clause c ARG_LD)
{ GD->statistics.codes -= c->code_size;
  freeHeap(c, sizeofClause(c->code_size));
}


void
freeClause(Clause c ARG_LD)
{
#if O_DEBUGGER
  if ( true(c, HAS_BREAKPOINTS) )
    clearBreakPointsClause(c);
#endif

#ifdef O_ATOMGC
  forAtomsInClause(c, PL_unregister_atom);
#endif

  if ( true(c, DBREF_CLAUSE) )
  { set(c, DBREF_ERASED_CLAUSE);
    return;
  }

  unallocClause(c PASS_LD);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
gcClausesDefinition()
    This function has two tasks. If the predicates needs to be rehashed,
    this is done and all erased clauses from the predicate are returned
    as a linked list.

    We cannot delete the clauses immediately as the debugger requires a
    call-back and we have the L_PREDICATE mutex when running this code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static ClauseRef
cleanDefinition(Definition def, ClauseRef garbage)
{ GET_LD
  ClauseRef cref, prev = NULL;
  int rehash = 0;
#if O_DEBUG
  int left = 0, removed = 0;
#endif

  DEBUG(2, Sdprintf("gcClausesDefinition(%s) --> ", predicateName(def)));

  cref = def->definition.clauses;

  if ( def->hash_info )
  { if ( false(def, NEEDSREHASH) )
      gcClauseIndex(def->hash_info PASS_LD);
    else
    { rehash = def->number_of_clauses * 2;
      unallocClauseIndexTable(def->hash_info);
      def->hash_info = NULL;
    }
  }

  while( cref && def->erased_clauses )
  { if ( true(cref->clause, ERASED) )
    { ClauseRef c = cref;

					/* Unlink from definition */
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

      DEBUG(2, removed++);
      def->erased_clauses--;

					/* re-link into garbage chain */
      c->next = garbage;
      garbage = c;
    } else
    { prev = cref;
      cref = cref->next;
      DEBUG(2, left++);
    }
  }

  DEBUG(2, if ( def->erased_clauses != 0 )
	     Sdprintf("*** %s has %d erased claused\n",
		      predicateName(def), def->erased_clauses));

  assert(def->erased_clauses == 0);

  DEBUG(2, Sdprintf("removed %d, left %d\n", removed, left));

  if ( rehash )
    hashDefinition(def, rehash);

  clear(def, NEEDSCLAUSEGC|NEEDSREHASH);

  return garbage;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Free a list of clauses as returned by gcClausesDefinition(); This may be
called from dangerous places. We detect   this by discovering that there
are no saved registers for the current query.   In that case we link the
clauses to LD->freed_clauses and raise SIG_FREECLAUSES.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
freeClauseList(ClauseRef cref)
{ GET_LD
  ClauseRef next;
#if O_DEBUGGER
  int hooked;
  int savely_hooked;

  if ( LD && LD->query &&
       PROCEDURE_event_hook1 &&
       hasClausesDefinition(PROCEDURE_event_hook1->definition) )
  { hooked = TRUE;
    savely_hooked = (LD->query->registers.fr != NULL);
  } else
  { hooked = FALSE;
    savely_hooked = FALSE;
  }

  if ( hooked && !savely_hooked )
  { if ( !LD->freed_clauses )
    { LD->freed_clauses = cref;
      PL_raise(SIG_FREECLAUSES);
      return;
    } else
    { ClauseRef ce;

      for(ce=cref; ce; ce = ce->next)
      { if ( !ce->next )
	{ ce->next = LD->freed_clauses;
	  LD->freed_clauses = cref;
	  return;
	}
      }
    }
  }
#endif

  for( ; cref; cref = next)
  { Clause cl = cref->clause;
    next = cref->next;

#if O_DEBUGGER
    if ( hooked && savely_hooked &&
	 cl->procedure->definition != PROCEDURE_event_hook1->definition )
      callEventHook(PLEV_ERASED_CLAUSE, cl);
#endif

    freeClause(cl PASS_LD);
    freeClauseRef(cref PASS_LD);
  }
}


void
gcClausesDefinition(Definition def)
{ ClauseRef cref = cleanDefinition(def, NULL);

  if ( cref )
    freeClauseList(cref);
}


void
gcClausesDefinitionAndUnlock(Definition def)
{ ClauseRef cref = cleanDefinition(def, NULL);

  UNLOCKDEF(def);

  if ( cref )
    freeClauseList(cref);
}


#ifdef O_PLMT
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Discard an entire definition. This can  only   be  used if we *know* the
definition is not  referenced  in  any  way.   It  is  used  to  discard
thread-local definitions at the end of a threads lifetime.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
destroyDefinition(Definition def)
{ GET_LD

  if ( def->hash_info )
    unallocClauseIndexTable(def->hash_info);
  if ( def->definition.clauses )
    freeClauseList(def->definition.clauses);

  freeHeap(def, sizeof(*def));
}

#endif /*O_PLMT*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
resetReferences() is called by abort() to clear all predicate references.
Erased clauses will be removed as well.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
resetReferencesModule(Module m)
{ Definition def;

  for_unlocked_table(m->procedures, s,
		     { def = ((Procedure) s->value)->definition;
		       def->references = 0;
		       if ( true(def, NEEDSCLAUSEGC|NEEDSREHASH) )
			 gcClausesDefinition(def);
		     })
}

void
resetReferences(void)
{ LOCK();
  for_unlocked_table(GD->tables.modules, s,
		     resetReferencesModule((Module) s->value));
  UNLOCK();
}


		 /*******************************
		 *	  META PREDICATE	*
		 *******************************/

/** meta_predicate :HeadList is det.

Quintus compatible declaration  for   meta-predicates.  The  declaration
fills the meta_info field of a  definition   as  well  as the P_META and
P_TRANSPARENT  flags.  P_META  indicates  that    meta_info   is  valid.
P_TRANSPARENT indicates that  the  declaration   contains  at  least one
meta-argument (: or 0..9).

@param HeadList	Comma separated list of predicates heads, where each
		predicate head has arguments 0..9, :,+,-,?
*/

static int
meta_declaration(term_t spec)
{ GET_LD
  term_t head = PL_new_term_ref();
  term_t arg = PL_new_term_ref();
  Procedure proc;
  Definition def;
  atom_t name;
  int i, arity;
  int mask = 0;
  int transparent = FALSE;

  if ( !get_procedure(spec, &proc, head, GP_DEFINE) ||
       !PL_get_name_arity(head, &name, &arity) )
    return FALSE;

  if ( arity > (int)sizeof(mask)*2 )
    return PL_error(NULL, 0, "max arity of meta predicates is 8",
		    ERR_REPRESENTATION, ATOM_max_arity);

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
      mask |= e<<(i*4);
      transparent = TRUE;
    } else if ( PL_get_atom(arg, &ma) )
    { int m;

      if      ( ma == ATOM_plus ) m = MA_NONVAR;
      else if ( ma == ATOM_minus ) m = MA_VAR;
      else if ( ma == ATOM_question_mark ) m = MA_ANY;
      else if ( ma == ATOM_colon ) m = MA_META, transparent = TRUE;
      else goto domain_error;

      mask |= m<<(i*4);
    } else
    { return PL_error(NULL, 0, "0..9",
			ERR_TYPE, ATOM_meta_argument_specifier, arg);;
    }
  }

  def = proc->definition;
  def->meta_info = mask;
  if ( transparent )
    set(def, P_TRANSPARENT);
  else
    clear(def, P_TRANSPARENT);
  set(def, P_META);

  return TRUE;
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


static int
unify_meta_argument(term_t head, Definition def, int i)
{ GET_LD
  term_t arg = PL_new_term_ref();
  int m = MA_INFO(def, i);

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
      default:		a = NULL_ATOM; assert(0);
    }

    return PL_unify_atom(arg, a);
  }
}


static int
unify_meta_pattern(Procedure proc, term_t head)
{ Definition def = proc->definition;

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


#ifdef O_CLAUSEGC
		 /*******************************
		 *	     CLAUSE-GC		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
MT: locked by caller
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
registerDirtyDefinition(Definition def)
{ if ( false(def, P_DIRTYREG) )
  { GET_LD
    DefinitionChain cell = allocHeap(sizeof(*cell));

    set(def, P_DIRTYREG);
    cell->definition = def;
    cell->next = GD->procedures.dirty;
    GD->procedures.dirty = cell;
  }
}


foreign_t
pl_garbage_collect_clauses(void)
{ GET_LD

  if ( GD->procedures.dirty && !gc_status.blocked )
  { DefinitionChain c, cell, next, last;
    sigset_t set;
    ClauseRef garbage = NULL;

    DEBUG(1, Sdprintf("pl_garbage_collect_clauses()\n"));

    LOCK();
    PL_LOCK(L_THREAD);
    blockSignals(&set);

					/* sanity-check */
    for(c=GD->procedures.dirty; c; c=c->next)
    { Definition def = c->definition;

      assert(true(def, P_DIRTYREG));
      if ( false(def, DYNAMIC) )
	assert(def->references == 0);
    }

    markPredicatesInEnvironments(LD);
#ifdef O_PLMT
    forThreadLocalData(markPredicatesInEnvironments,
		       PL_THREAD_SUSPEND_AFTER_WORK);
#endif

    DEBUG(1, Sdprintf("Marking complete; cleaning predicates\n"));

    last = NULL;
    for(cell = GD->procedures.dirty; cell; cell = next)
    { Definition def = cell->definition;

      next = cell->next;

      if ( false(def, DYNAMIC) )
      { if ( def->references )
	{ assert(def->references == 1);
	  def->references = 0;
	  last = cell;
	  continue;
	} else
	{ DEBUG(1, Sdprintf("gcClausesDefinition(%s)\n", predicateName(def)));
	  garbage = cleanDefinition(def, garbage);
	}
      }

      clear(def, P_DIRTYREG);
      freeHeap(cell, sizeof(*cell));
      if ( last )
	last->next = next;
      else
	GD->procedures.dirty = next;
    }

#ifdef O_PLMT
    resumeThreads();
#endif

    unblockSignals(&set);
    PL_UNLOCK(L_THREAD);
    UNLOCK();

    if ( garbage )
      freeClauseList(garbage);
  }

  succeed;
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

  if ( true(def, FOREIGN) )
    succeed;

  for(cref = def->definition.clauses; cref; cref = cref->next)
  { Clause clause = cref->clause;

    if ( clause->index.varmask != 0 )
      nindexable++;

    if ( false(clause, ERASED) )
      nclauses++;
    else
      nerased++;
  }

  if ( nerased != def->erased_clauses )
    Sdprintf("%s has %d erased clauses, claims %d\n",
	     predicateName(def), nerased, def->erased_clauses);

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
A dynamic call to `f' in `m' has to be made (via call/1 or from C). This
procedure returns the procedure to be run.   If no such procedure exists
an undefined procedure is created and returned. In this case interpret()
will later call trapUndefined() to generate   an  error message (or link
the procedure from the library via autoload).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Procedure
resolveProcedure(functor_t f, Module module)
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
  if ( (proc = isCurrentProcedure(f, m)) && isDefinedProcedure(proc) )
    return proc->definition;

  for(c=m->supers; c; c=c->next)
  { if ( (def = autoImport(f, c->value)) )
      goto found;
  }
  return NULL;

found:
  if ( proc == NULL )			/* Create header if not there */
    proc = lookupProcedure(f, m);
					/* Safe? See above */
					/* TBD: find something better! */
  odef = proc->definition;
  proc->definition = def;

#ifdef O_PLMT
  PL_LOCK(L_THREAD);
  if ( (GD->statistics.threads_created -
	GD->statistics.threads_finished) == 1 )
  { assert(false(proc->definition, P_DIRTYREG));
    freeHeap(odef, sizeof(struct definition));
  }
  PL_UNLOCK(L_THREAD);
#else
  freeHeap(odef, sizeof(struct definition));
#endif

  return def;
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
  atom_t sfn = source_file_name;	/* needs better solution! */
  int  sln = source_line_no;
  atom_t answer = ATOM_nil;

  if ( !GD->procedures.undefinterc4 )
    GD->procedures.undefinterc4 = PL_pred(FUNCTOR_undefinterc4,
					  MODULE_system);

  if ( !(cid  = PL_open_foreign_frame()) ||
       !(argv = PL_new_term_refs(4)) )
    return answer;

  PL_put_atom(    argv+0, def->module->name);
  PL_put_atom(    argv+1, def->functor->name);
  PL_put_integer( argv+2, def->functor->arity);

  LD->autoload_nesting++;
  if ( (qid = PL_open_query(MODULE_system, PL_Q_NODEBUG,
			    GD->procedures.undefinterc4, argv)) )
  { if ( PL_next_solution(qid) )
      PL_get_atom(argv+3, &answer);
    PL_close_query(qid);
  }
  LD->autoload_nesting--;
  source_file_name = sfn;
  source_line_no   = sln;
  PL_discard_foreign_frame(cid);

  return answer;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
According to Paulo Moura, predicates defined either dynamic, multifile or
discontiguous should not cause an undefined predicate warning.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Definition
trapUndefined_unlocked(Definition def ARG_LD)
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
  { if ( LD->autoload_nesting > 100 )
    { LD->autoload_nesting = 1;
      sysError("trapUndefined(): undefined: %s", predicateName(def));

      return def;
    } else
    { atom_t answer = autoLoader(def);

      def = lookupProcedure(functor->functor, module)->definition;

      if ( answer == ATOM_fail )
      { return def;
      } else if ( answer == ATOM_error )
      { goto error;
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
error:
  if ( GD->bootsession )
  { sysError("Undefined predicate: %s", predicateName(def));
  } else
  { createUndefSupervisor(def);
  }

  return def;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This must be  executed  holding  the   Prolog  mutex  '$load'  to  avoid
race-conditions between threads trapping undefined   code. At the moment
there is no neat way to share   mutexes  between C and Prolog, something
that should be considered.

Note that in the  multi-threaded  case,   we  first  try auto-import and
unknown=fail before locking.  This enhances concurrency during startup.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Definition
trapUndefined(Definition undef ARG_LD)
{ Definition def;

#ifdef O_PLMT
  Module module = undef->module;
  FunctorDef functor = undef->functor;
					/* Auto import */
  if ( (def = autoImport(functor->functor, module)) )
    return def;
					/* Pred/Module does not want to trap */
  if ( true(undef, PROC_DEFINED) ||
       getUnknownModule(module) == UNKNOWN_FAIL )
    return undef;

  PL_mutex_lock(GD->thread.MUTEX_load);
#endif
  def = trapUndefined_unlocked(undef PASS_LD);
#ifdef O_PLMT
  PL_mutex_unlock(GD->thread.MUTEX_load);
#endif

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
  ClauseRef  cref;
} retract_context;

static
PRED_IMPL("retract", 1, retract,
	  PL_FA_TRANSPARENT|PL_FA_NONDETERMINISTIC|PL_FA_ISO)
{ PRED_LD
  term_t term = A1;
  retract_context ctxbuf;
  retract_context *ctx;

  if ( CTX_CNTRL == FRG_CUTTED )
  { ctx = CTX_PTR;

    leaveDefinition(ctx->def);
    freeHeap(ctx, sizeof(*ctx));

    return TRUE;
  } else
  { Module m = (Module) NULL;
    term_t cl = PL_new_term_ref();
    term_t head = PL_new_term_ref();
    term_t body = PL_new_term_ref();
    Word argv;
    ClauseRef next;
    atom_t b;
    fid_t fid;

    PL_strip_module(term, &m, cl);
    get_head_and_body_clause(cl, head, body, NULL PASS_LD);
    if ( PL_get_atom(body, &b) && b == ATOM_true )
      PL_put_term(cl, head);

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
      ClauseRef cref;

      if ( !PL_get_functor(head, &fd) )
	return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, head);
      if ( !(proc = isCurrentProcedure(fd, m)) )
      { checkModifySystemProc(fd);
	fail;
      }

      def = getProcDefinition(proc);

      if ( true(def, FOREIGN) )
	return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, proc);
      if ( false(def, DYNAMIC) )
      { if ( isDefinedProcedure(proc) )
	  return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, proc);
	setDynamicProcedure(proc, TRUE); /* implicit */
	fail;				/* no clauses */
      }

      if ( def->references && (debugstatus.styleCheck & DYNAMIC_STYLE) )
	printMessage(ATOM_informational,
		     PL_FUNCTOR_CHARS, "modify_active_procedure", 2,
		       PL_CHARS, "retract",
		       _PL_PREDICATE_INDICATOR, proc);

      startCritical;
      enterDefinition(def);			/* reference the predicate */
      cref = firstClause(argv, environment_frame, def, &next PASS_LD);
      if ( !cref )
      { leaveDefinition(def);
	endCritical;
	fail;
      }

      ctx = &ctxbuf;
      ctx->def = def;
      ctx->cref = cref;
    } else
    { ctx  = CTX_PTR;
      ctx->cref = findClause(ctx->cref, argv, environment_frame,
			     ctx->def, &next PASS_LD);
      startCritical;
    }

    if ( !(fid = PL_open_foreign_frame()) )
    { leaveDefinition(ctx->def);
      if ( ctx != &ctxbuf )
	freeHeap(ctx, sizeof(*ctx));

      endCritical;
      return FALSE;
    }

    /* ctx->cref is the first candidate; next is the next one */

    while( ctx->cref )
    { if ( decompile(ctx->cref->clause, cl, 0) )
      { retractClauseDefinition(ctx->def, ctx->cref->clause PASS_LD);

	if ( !endCritical )
	{ leaveDefinition(ctx->def);
	  if ( ctx != &ctxbuf )
	    freeHeap(ctx, sizeof(*ctx));
	  PL_close_foreign_frame(fid);

	  return FALSE;
	}

	if ( !next )			/* deterministic last one */
	{ leaveDefinition(ctx->def);
	  if ( ctx != &ctxbuf )
	    freeHeap(ctx, sizeof(*ctx));
	  PL_close_foreign_frame(fid);
	  return TRUE;
	}

	if ( ctx == &ctxbuf )		/* non-determinisic; save state */
	{ ctx = allocHeap(sizeof(*ctx));
	  *ctx = ctxbuf;
	}
	ctx->cref = next;

	PL_close_foreign_frame(fid);
	ForeignRedoPtr(ctx);
      }

      PL_rewind_foreign_frame(fid);

      ctx->cref = findClause(next, argv, environment_frame,
			     ctx->def, &next PASS_LD);
    }

    PL_close_foreign_frame(fid);
    leaveDefinition(ctx->def);
    if ( ctx != &ctxbuf )
      freeHeap(ctx, sizeof(*ctx));
    endCritical;
    fail;
  }
}


static int
allVars(int argc, Word argv ARG_LD)
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


word
pl_retractall(term_t head)
{ GET_LD
  term_t thehead = PL_new_term_ref();
  Procedure proc;
  Definition def;
  ClauseRef cref;
  ClauseRef next;
  Word argv;
  int allvars = TRUE;
  fid_t fid;

  if ( !get_procedure(head, &proc, thehead, GP_CREATE) )
    succeed;

  def = getProcDefinition(proc);
  if ( true(def, FOREIGN) )
    return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, proc);
  if ( false(def, DYNAMIC) )
  { if ( isDefinedProcedure(proc) )
      return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, proc);
    if ( !setDynamicProcedure(proc, TRUE) )
      fail;
    succeed;				/* nothing to retract */
  }

  argv = valTermRef(thehead);
  deRef(argv);
  if ( isTerm(*argv) )
  { int arity = arityTerm(*argv);
    argv = argTermP(*argv, 0);

    allvars = allVars(arity, argv PASS_LD);
  } else
  { allvars = TRUE;
    argv = NULL;
  }

  startCritical;
  enterDefinition(def);
  fid = PL_open_foreign_frame();

  if ( allvars )
  { uintptr_t gen = environment_frame->generation;

    for(cref = def->definition.clauses; cref; cref = cref->next)
    { if ( visibleClause(cref->clause, gen) )
      { retractClauseDefinition(def, cref->clause PASS_LD);
      }
    }
  } else
  { if ( !(cref = firstClause(argv, environment_frame, def, &next PASS_LD)) )
    { int rc = endCritical;
      leaveDefinition(def);
      return rc;
    }

    while( cref )
    { if ( decompileHead(cref->clause, thehead) )
	retractClauseDefinition(def, cref->clause PASS_LD);

      PL_rewind_foreign_frame(fid);

      if ( !next )
      { leaveDefinition(def);
	return endCritical;
      }

      if ( argv )				/* may be shifted */
      { argv = valTermRef(thehead);
	argv = argTermP(*argv, 0);
      }

      cref = findClause(next, argv, environment_frame, def, &next PASS_LD);
    }
  }
  leaveDefinition(def);
  return endCritical;
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

  if ( truePrologFlag(PLFLAG_ISO) && false(proc->definition, DYNAMIC) )
    return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, proc);

  return abolishProcedure(proc, m);
}


word
pl_abolish(term_t name, term_t arity)	/* Name, Arity */
{ GET_LD
  Module m = NULL;

  PL_strip_module(name, &m, name);

  return do_abolish(m, name, arity);
}


word
pl_abolish1(term_t spec)		/* Name/Arity */
{ GET_LD
  term_t name  = PL_new_term_ref();
  term_t arity = PL_new_term_ref();
  Module m = NULL;

  PL_strip_module(spec, &m, spec);

  if ( !PL_is_functor(spec, FUNCTOR_divide2) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_predicate_indicator, spec);

  _PL_get_arg(1, spec, name);
  _PL_get_arg(2, spec, arity);

  return do_abolish(m, name, arity);
}


static uintptr_t
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
  if (key == ATOM_transparent)	 return P_TRANSPARENT;
  if (key == ATOM_discontiguous) return DISCONTIGUOUS;
  if (key == ATOM_volatile)	 return VOLATILE;
  if (key == ATOM_thread_local)  return P_THREAD_LOCAL;
  if (key == ATOM_noprofile)     return P_NOPROFILE;
  if (key == ATOM_iso)      	 return P_ISO;
  if (key == ATOM_public)      	 return P_PUBLIC;

  return 0;
}


word
pl_get_predicate_attribute(term_t pred,
			   term_t what, term_t value)
{ GET_LD
  Procedure proc;
  Definition def;
  functor_t fd;
  atom_t key;
  Module module = (Module) NULL;
  uintptr_t att;
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
  } else if ( key == ATOM_line_count )
  { int line;

    if ( false(def, FOREIGN|P_THREAD_LOCAL) &&
	 def->definition.clauses &&
	 (line=def->definition.clauses->clause->line_no) )
      return PL_unify_integer(value, line);
    else
      fail;
  } else if ( key == ATOM_foreign )
  { return PL_unify_integer(value, true(def, FOREIGN) ? 1 : 0);
  } else if ( key == ATOM_hashed )
  { return PL_unify_integer(value, def->hash_info?def->hash_info->buckets:0);
  } else if ( key == ATOM_references )
  { return PL_unify_integer(value, def->references);
  } else if ( key == ATOM_number_of_clauses )
  { if ( def->flags & FOREIGN )
      fail;

    def = getProcDefinition(proc);
    if ( def->number_of_clauses == 0 && false(def, DYNAMIC) )
      fail;
    return PL_unify_integer(value, def->number_of_clauses);
  } else if ( (att = attribute_mask(key)) )
  { return PL_unify_integer(value, (def->flags & att) ? 1 : 0);
  } else
  { return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    PL_new_atom("procedure_property"), what);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Toggle dynamic/static. With clause-gc this  has become harder. Basically
we can't easily make a predicate dynamic   if  it has clauses, unless we
scan the system to initialise the reference-count properly.

Static predicates are  managed  on  a   combined  mutex,  while  dynamic
predicates are locked on their own  mutex. This procedure must carefully
attach or detach the mutex.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_PLMT
static void
attachMutexDefinition(Definition def)
{ if ( !def->mutex )
    def->mutex = allocSimpleMutex(predicateName(def));
}


static void
detachMutexAndUnlock(Definition def)
{ counting_mutex *m = def->mutex;

  if ( m )
  { def->mutex = NULL;
    countingMutexUnlock(m);
    freeSimpleMutex(m);
  }
}

#else /*O_PLMT*/

#define attachMutexDefinition(def)
#define detachMutexAndUnlock(def)

#endif /*O_PLMT*/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Changing a static  procedure  to  dynamic   is  very  difficult.  If the
definition has clauses, these may be dead   clauses, so we must call the
clause garbage collector to find  out.  A   common  case  where this may
happen is abolish, followed by  dynamic.   Unfortunately  it  makes this
sequence hazardous and slow in multi-threaded environment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
setDynamicProcedure(Procedure proc, bool isdyn)
{ Definition def = proc->definition;

  LOCK();
  if ( (isdyn && true(def, DYNAMIC)) ||
       (!isdyn && false(def, DYNAMIC)) )
  { UNLOCK();
    succeed;
  }
  attachMutexDefinition(def);
  UNLOCK();

  LOCKDEF(def);

  if ( isdyn )				/* static --> dynamic */
  { char *msg;

    if ( def->definition.clauses )
    { UNLOCKDEF(def);
      if ( true(def, NEEDSCLAUSEGC) )
      { pl_garbage_collect_clauses();
	LOCKDEF(def);
	if ( !def->definition.clauses )
	  goto ok;
	UNLOCKDEF(def);
      }

      if ( isDefinedProcedure(proc) )
	msg = NULL;
      else
	msg = "procedure has active clauses";

      return PL_error(NULL, 0, msg,
		      ERR_MODIFY_STATIC_PROC, proc);
    } else if ( def->functor->arity > 0 )
    { def->indexPattern = 0x1;
      set(def, AUTOINDEX);
    }
  ok:
    freeCodesDefinition(def);		/* reset to S_VIRGIN */
    set(def, DYNAMIC);

    UNLOCKDEF(def);
  } else				/* dynamic --> static */
  { clear(def, DYNAMIC);
    if ( def->references )
    { if ( true(def, NEEDSCLAUSEGC|NEEDSREHASH) )
	registerDirtyDefinition(def);
      def->references = 0;
    }
    freeCodesDefinition(def);		/* reset to S_VIRGIN */

    detachMutexAndUnlock(def);
  }

  succeed;
}


static int
set_thread_local_procedure(Procedure proc, bool val)
{
#ifdef O_PLMT
  Definition def = proc->definition;

  LOCK();
  if ( (val && true(def, P_THREAD_LOCAL)) ||
       (!val && false(def, P_THREAD_LOCAL)) )
  { UNLOCK();
    succeed;
  }
  attachMutexDefinition(def);
  UNLOCK();

  LOCKDEF(def);

  if ( val )				/* static --> local */
  { if ( def->definition.clauses )
    { UNLOCKDEF(def);
      return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, proc);
    }
    set(def, DYNAMIC|VOLATILE|P_THREAD_LOCAL);

    def->codes = SUPERVISOR(thread_local);
    def->definition.local = new_ldef_vector();

    UNLOCKDEF(def);
    succeed;
  } else				/* local --> static */
  { UNLOCKDEF(def);
    return PL_error(NULL, 0, "TBD: better message",
		    ERR_MODIFY_STATIC_PROC, proc);
  }
#else
  setDynamicProcedure(proc, val);

  if ( val )
    set(proc->definition, VOLATILE|P_THREAD_LOCAL);
  else
    clear(proc->definition, VOLATILE|P_THREAD_LOCAL);

  succeed;
#endif
}


word
pl_set_predicate_attribute(term_t pred,
			   term_t what, term_t value)
{ GET_LD
  Procedure proc;
  Definition def;
  atom_t key;
  int val, rc;
  uintptr_t att;

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

  if ( att == DYNAMIC )
  { rc = setDynamicProcedure(proc, val);
  } else if ( att == P_THREAD_LOCAL )
  { rc = set_thread_local_procedure(proc, val);
  } else
  { if ( !val )
    { clear(def, att);
    } else
    { set(def, att);
    }

    rc = TRUE;
  }

  if ( rc && val &&
       (att & PROC_DEFINED) &&
       false(def, FILE_ASSIGNED) &&
       ReadingSource )
  { DEBUG(2, Sdprintf("Associating %s to %s (%p)\n",
		      predicateName(def), PL_atom_chars(source_file_name),
		      def));
    addProcedureSourceFile(lookupSourceFile(source_file_name, TRUE), proc);

    if ( SYSTEM_MODE )
    { set(def, SYSTEM|HIDE_CHILDS);
    } else
    { if ( truePrologFlag(PLFLAG_DEBUGINFO) )
	clear(def, HIDE_CHILDS);
      else
	set(def, HIDE_CHILDS);
    }
  }

  return rc;
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

Rebuilds the clause index for the   predicate.  This predicate is called
whenever NEED_REINDEX is set. It locks the predicate and checks the flag
again to ensure proper multi-threaded behaviour.

We cannot re-index if the  predicate   is  referenced  and hashed: other
predicates are operating on the  hashed   clauses-lists.  If  we are not
hashed there is no problem: the clause-list remains unaltered. Therefore
assertProcedure() only signals a re-index request   if  the predicate is
not yet hashed.

This is the place that  builds   the  supervisors dealing with efficient
calling conventions for the various cases.

TBD: Clear NEED_REINDEX at the end?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
reindexDefinition(Definition def)
{ ClauseRef cref;
  int do_hash = 0;
  int canindex = 0;
  int cannotindex = 0;
  unsigned long pattern = (def->indexPattern & ~NEED_REINDEX);

  LOCKDEF(def);
  if ( !(def->indexPattern & NEED_REINDEX) )
  { UNLOCKDEF(def);
    return TRUE;
  }

  assert(def->references == 1 || !def->hash_info);
  DEBUG(2, Sdprintf("reindexDefinition(%s)\n", predicateName(def)));
  def->indexPattern &= ~NEED_REINDEX;

  if ( true(def, AUTOINDEX) || pattern == 0x1 )
  { for(cref = def->definition.clauses; cref; cref = cref->next)
    { word key;

      if ( true(cref->clause, ERASED) )
	continue;

      if ( arg1Key(cref->clause, FALSE, &key) )
	canindex++;
      else
	cannotindex++;
    }
  }

  if ( true(def, AUTOINDEX) )
  { if ( canindex == 0 )
    { DEBUG(2, Sdprintf("not indexed: %s\n", predicateName(def)));
      pattern = 0x0;
    } else
    { pattern = 0x1;
    }
  }

  if ( pattern == 0x1 &&
       canindex > 5 && cannotindex <= 2 )
    do_hash = canindex / 2;

  def->indexCardinality = cardinalityPattern(pattern);
  for(cref = def->definition.clauses; cref; cref = cref->next)
  { if ( !reindexClause(cref->clause, def, pattern) )
    { UNLOCKDEF(def);
      return FALSE;			/* no space; what to do? */
    }
  }

  if ( do_hash )
  { DEBUG(3, Sdprintf("hash(%s, %d)\n", predicateName(def), do_hash));
    hashDefinition(def, do_hash);
  }

  def->indexPattern = pattern;
  UNLOCKDEF(def);

  return TRUE;
}


/* MT: Definition is locked by caller
*/

void
indexDefinition(Definition def, long pattern)
{ clear(def, AUTOINDEX);

  if ( pattern != 0x1L &&
       true(def, DYNAMIC) && def->references == 0 )
  { if ( def->hash_info )
    { unallocClauseIndexTable(def->hash_info);
      def->hash_info = NULL;
    }
  }

  def->indexPattern = (pattern | NEED_REINDEX);
}


word
pl_index(term_t pred)
{ GET_LD
  Procedure proc;
  term_t head = PL_new_term_ref();

  if ( get_procedure(pred, &proc, head, GP_CREATE) )
  { Definition def = proc->definition;
    int arity = def->functor->arity;

    if ( true(def, FOREIGN) )
      return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
		      ATOM_index, PL_new_atom("foreign_procedure"), proc);

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

      if ( def->indexPattern != pattern)
      { LOCKDEF(def);
	indexDefinition(def, pattern);
	UNLOCKDEF(def);
      }
    }

    succeed;
  }

  fail;
}


word
pl_get_clause_attribute(term_t ref, term_t att, term_t value)
{ GET_LD
  Clause clause;
  atom_t a;

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

#define source_index (GD->files._source_index)
#define sourceTable  (GD->files._source_table)

static void
registerSourceFile(SourceFile f)
{ if ( !GD->files.source_files.base )
    initBuffer(&GD->files.source_files);

  f->index = (int)entriesBuffer(&GD->files.source_files, SourceFile) + 1;

  addBuffer(&GD->files.source_files, f, SourceFile);
}


void
cleanupSourceFiles(void)
{ if ( GD->files.source_files.base )
  { discardBuffer(&GD->files.source_files);
    GD->files.source_files.base = 0;
  }
}


SourceFile
lookupSourceFile(atom_t name, int create)
{ SourceFile file;
  Symbol s;

  LOCK();
  if ( !sourceTable )
    sourceTable = newHTable(32);

  if ( (s=lookupHTable(sourceTable, (void*)name)) )
  { file = s->value;
  } else if ( create )
  { GET_LD

    file = (SourceFile) allocHeap(sizeof(struct sourceFile));
    memset(file, 0, sizeof(struct sourceFile));
    file->name = name;
    file->index = ++source_index;
    file->system = GD->bootsession;

    PL_register_atom(file->name);
    registerSourceFile(file);

    addHTable(sourceTable, (void*)name, file);
  } else
  { file = NULL;
  }
  UNLOCK();

  return file;
}


SourceFile
indexToSourceFile(int index)
{ int n = (int)entriesBuffer(&GD->files.source_files, SourceFile);

  index--;
  if ( index >= 0 && index < n )
    return fetchBuffer(&GD->files.source_files, index, SourceFile);

  return NULL;
}


static int
hasProcedureSourceFile(SourceFile sf, Procedure proc)
{ ListCell cell;

  if ( true(proc->definition, FILE_ASSIGNED) )
  { for(cell=sf->procedures; cell; cell = cell->next)
    { if ( cell->value == proc )
	succeed;
    }
  }

  fail;
}



void
addProcedureSourceFile(SourceFile sf, Procedure proc)
{ ListCell cell;

  LOCK();
  if ( hasProcedureSourceFile(sf, proc) )
  { UNLOCK();
    return;
  }

  { GET_LD

    cell = allocHeap(sizeof(struct list_cell));
    cell->value = proc;
    cell->next = sf->procedures;
    sf->procedures = cell;
    set(proc->definition, FILE_ASSIGNED);
  }

  UNLOCK();
}


int
redefineProcedure(Procedure proc, SourceFile sf, unsigned int suppress)
{ GET_LD
  Definition def = proc->definition;

  if ( true(def, FOREIGN) )
  { abolishProcedure(proc, def->module);
    printMessage(ATOM_warning,
		 PL_FUNCTOR_CHARS, "redefined_procedure", 2,
		   PL_CHARS, "foreign",
		   _PL_PREDICATE_INDICATOR, proc);
  }

  if ( false(def, MULTIFILE) )
  { ClauseRef first;

    def = getProcDefinition__LD(def PASS_LD);
    first = hasClausesDefinition(def);

    if ( first && first->clause->source_no == sf->index )
    { if ( ((debugstatus.styleCheck & ~suppress) & DISCONTIGUOUS_STYLE) &&
	   false(def, DISCONTIGUOUS) )
	printMessage(ATOM_warning,
		     PL_FUNCTOR_CHARS, "discontiguous", 1,
		       _PL_PREDICATE_INDICATOR, proc);
    } else if ( !hasProcedureSourceFile(sf, proc) )
    { if ( true(def, P_THREAD_LOCAL) )
	return PL_error(NULL, 0, NULL, ERR_MODIFY_THREAD_LOCAL_PROC, proc);

      abolishProcedure(proc, def->module);

      if ( def->references )
      { printMessage(ATOM_informational,
		     PL_FUNCTOR_CHARS, "redefined_procedure", 2,
		       PL_CHARS, "active",
		       _PL_PREDICATE_INDICATOR, proc);
      } else if ( first )
      { printMessage(ATOM_warning,
		     PL_FUNCTOR_CHARS, "redefined_procedure", 2,
		       PL_CHARS, "static",
		       _PL_PREDICATE_INDICATOR, proc);
      }
    }
  }

  return TRUE;
}


word
pl_make_system_source_files(void)
{ int i, n = (int)entriesBuffer(&GD->files.source_files, SourceFile);


  for(i=0; i<n; i++)
  { SourceFile f = fetchBuffer(&GD->files.source_files, i, SourceFile);

    f->system = TRUE;
  }

  succeed;
}


word
pl_source_file(term_t descr, term_t file, control_t h)
{ GET_LD
  Procedure proc;
  ClauseRef cref;
  SourceFile sf;
  atom_t name;
  ListCell cell;


  if ( ForeignControl(h) == FRG_FIRST_CALL )
  { if ( get_procedure(descr, &proc, 0, GP_FIND|GP_TYPE_QUIET) )
    { if ( !proc->definition ||
	   true(proc->definition, FOREIGN|P_THREAD_LOCAL) ||
	   !(cref = proc->definition->definition.clauses) ||
	   !(sf = indexToSourceFile(cref->clause->source_no)) )
	fail;

      return PL_unify_atom(file, sf->name);
    }

    if ( PL_is_variable(file) )
      return get_procedure(descr, &proc, 0, GP_FIND); /* throw exception */
  }

  if ( ForeignControl(h) == FRG_CUTTED )
    succeed;

  if ( !PL_get_atom_ex(file, &name) ||
       !(sf = lookupSourceFile(name, FALSE)) )
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

    if ( unify_definition(MODULE_user, descr, def, 0, 0) )
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

static
PRED_IMPL("$time_source_file", 3, time_source_file, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  int index;
  int mx = (int)entriesBuffer(&GD->files.source_files, SourceFile);
  term_t file = A1;
  term_t time = A2;
  term_t type = A3;			/* user or system */
  fid_t fid;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
      index = 0;
      break;
    case FRG_REDO:
      index = (int)CTX_INT;
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  fid = PL_open_foreign_frame();
  for(; index < mx; index++)
  { SourceFile f = fetchBuffer(&GD->files.source_files, index, SourceFile);

    if ( PL_unify_atom(file, f->name) &&
	 unifyTime(time, f->time) &&
	 PL_unify_atom(type, f->system ? ATOM_system : ATOM_user) )
    { PL_close_foreign_frame(fid);
      ForeignRedoInt(index+1);
    }

    PL_rewind_foreign_frame(fid);
  }

  PL_close_foreign_frame(fid);
  fail;
}


static bool
clearInitialization(SourceFile sf)
{ GET_LD
  int rc = FALSE;

  fid_t fid = PL_open_foreign_frame();
  term_t name = PL_new_term_ref();
  static predicate_t pred = NULL;

  if ( !pred )
    pred = PL_predicate("$clear_initialization", 1, "system");

  PL_put_atom(name, sf->name);
  rc = PL_call_predicate(MODULE_system, PL_Q_NORMAL, pred, name);

  PL_discard_foreign_frame(fid);

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unloadFile(SourceFile sf)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
unloadFile(SourceFile sf)
{ GET_LD
  ListCell cell, next;
  sigset_t set;
  ClauseRef garbage = NULL;

  clearInitialization(sf);

  LOCK();
  PL_LOCK(L_THREAD);
  blockSignals(&set);

  GD->procedures.active_marked = 0;
  GD->procedures.reloading = sf;
  markPredicatesInEnvironments(LD);
#ifdef O_PLMT
  forThreadLocalData(markPredicatesInEnvironments,
		     PL_THREAD_SUSPEND_AFTER_WORK);
#endif
  GD->procedures.reloading = NULL;

				      /* remove the clauses */
  for(cell = sf->procedures; cell; cell = cell->next)
  { int deleted;

    Procedure proc = cell->value;
    Definition def = proc->definition;

    DEBUG(2, Sdprintf("removeClausesProcedure(%s), refs = %d\n",
		      predicateName(def), def->references));

    deleted = removeClausesProcedure(proc,
				     true(def, MULTIFILE) ? sf->index : 0,
				     TRUE);

    if ( deleted )
    { if ( def->references == 0 )
      { freeCodesDefinition(def);
	garbage = cleanDefinition(def, garbage);
      } else if ( false(def, DYNAMIC) )
      { registerDirtyDefinition(def);
	freeCodesDefinition(def);
      }
    }

    if ( false(def, MULTIFILE) )
      clear(def, FILE_ASSIGNED);
  }

				      /* unmark the marked predicates */
  for(cell = sf->procedures; cell; cell = cell->next)
  { Procedure proc = cell->value;
    Definition def = proc->definition;

    if ( false(def, DYNAMIC) && def->references )
    { assert(def->references == 1);
      def->references = 0;
      GD->procedures.active_marked--;
    }
  }

				      /* cleanup the procedure list */
  for(cell = sf->procedures; cell; cell = next)
  { next = cell->next;
    freeHeap(cell, sizeof(struct list_cell));
  }
  sf->procedures = NULL;
  assert(GD->procedures.active_marked == 0);

#ifdef O_PLMT
  resumeThreads();
#endif

  unblockSignals(&set);
  PL_UNLOCK(L_THREAD);
  UNLOCK();

  if ( garbage )
    freeClauseList(garbage);

  return TRUE;
}


/** unload_file(+Name) is det.

Remove all traces of a loaded file.
*/

static
PRED_IMPL("unload_file", 1, unload_file, 0)
{ PRED_LD
  SourceFile sf;
  atom_t name;

  if ( !PL_get_atom_ex(A1, &name) )
    return FALSE;

  if ( (sf = lookupSourceFile(name, FALSE)) )
  { Module m;

    if ( sf->system )
      return PL_error(NULL, 0, NULL, ERR_PERMISSION,
		      ATOM_unload, ATOM_file, A1);

    if ( !unloadFile(sf) )
      return FALSE;

    if ( (m=moduleFromFile(sf)) )
    { LOCKMODULE(m);
      m->file = NULL;
      m->line_no = 0;
      sf->module_count--;
      clearHTable(m->public);
      setSuperModule(m, MODULE_user);
      UNLOCKMODULE(m);
    }

    sf->count = 0;
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startConsult(SourceFile sf)

This function is called when starting the consult a file. Its task is to
remove all clauses that come from this   file  if this is a *reconsult*.
There are two options.

    * Immediately remove the clauses from any non-referenced predicate.
    This saves space, but if there are multiple threads it may cause
    other threads to trap an undefined predicate.

    * Delay until garbage_collect_clauses/0
    This way other threads can happily keep running.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
startConsult(SourceFile f)
{ if ( f->count++ > 0 )			/* This is a re-consult */
    unloadFile(f);

  f->current_procedure = NULL;
}


word
pl_start_consult(term_t file)
{ GET_LD
  atom_t name;

  if ( PL_get_atom(file, &name) )
  { SourceFile f = lookupSourceFile(name, TRUE);

    f->time = LastModifiedFile(stringAtom(name));
    startConsult(f);
    succeed;
  }

  fail;
}

		 /*******************************
		 *       DEBUGGER SUPPORT	*
		 *******************************/

static
PRED_IMPL("$clause_from_source", 3, clause_from_source, 0)
{ PRED_LD
  atom_t name;
  SourceFile f;
  int ln;
  ListCell cell;
  Clause c = NULL;

  term_t file = A1;
  term_t line = A2;
  term_t clause = A3;

  if ( !PL_get_atom_ex(file, &name) ||
       !(f = lookupSourceFile(name, FALSE)) ||
       !PL_get_integer_ex(line, &ln) )
    fail;

  for(cell = f->procedures; cell; cell = cell->next)
  { Procedure proc = cell->value;
    Definition def = proc->definition;

    if ( def && false(def, FOREIGN) )
    { ClauseRef cref = def->definition.clauses;

      for( ; cref; cref = cref->next )
      { Clause cl = cref->clause;

	if ( cl->source_no == f->index )
	{ if ( ln >= (int)cl->line_no )
	  { if ( !c || c->line_no < cl->line_no )
	      c = cl;
	  }
	}
      }
    }
  }

  if ( c )
    return PL_unify_clref(clause, c);

  fail;
}


#ifdef O_MAINTENANCE

		 /*******************************
		 *	INTERNAL DEBUGGING	*
		 *******************************/


static void
listGenerations(Definition def)
{ GET_LD
  uintptr_t gen = environment_frame->generation;
  ClauseRef cl;

  Sdprintf("%s has %d clauses at generation %ld (%s)\n",
	   predicateName(def),
	   def->number_of_clauses, gen,
	   true(def, NEEDSCLAUSEGC) ? "needs clause-gc" : "clean");


  for(cl=def->definition.clauses; cl; cl=cl->next)
  { Clause clause = cl->clause;

    Sdprintf("%p: %8u-%10u %s\n",
	     clause,
	     clause->generation.created,
	     clause->generation.erased,
	     visibleClause(clause, gen) ? "ok" : "erased");
  }

  if ( def->hash_info )
  { int i;

    Sdprintf("Hash index (%s, %s)\n",
	     true(def, NEEDSREHASH) ? "needs rehash" : "clean",
	     def->hash_info->alldirty ? "dirty" : "clean");

    for(i=0; i<def->hash_info->buckets; i++)
    { if ( !def->hash_info->entries[i].head &&
	   !def->hash_info->entries[i].dirty )
	continue;

      Sdprintf("\nClauses at i = %d, dirty = %d:\n",
	       i, def->hash_info->entries[i].dirty);

      for(cl=def->hash_info->entries[i].head; cl; cl=cl->next)
      { Clause clause = cl->clause;

	Sdprintf("%p: %8u-%10u %s\n",
		 clause,
		 clause->generation.created,
		 clause->generation.erased,
		 visibleClause(clause, gen) ? "ok" : "erased");
      }
    }
  }
}


void
checkDefinition(Definition def)
{ unsigned int nc, indexed = 0;
  ClauseRef cl;

  for(nc=0, cl = def->definition.clauses; cl; cl=cl->next)
  { Clause clause = cl->clause;

    if ( false(clause, ERASED) )
    { if ( clause->index.varmask )
	indexed++;
      nc++;
    }
  }
  if ( nc != def->number_of_clauses )
  { listGenerations(def);
    pl_break();
  }

  if ( def->hash_info )
  { int i;

    nc = 0;
    for(i=0; i<def->hash_info->buckets; i++)
    { for(cl=def->hash_info->entries[i].head; cl; cl=cl->next)
      { Clause clause = cl->clause;

	if ( false(clause, ERASED) )
	{ if ( clause->index.varmask )
	    nc++;
	}
      }
    }

    if ( nc != indexed )
    { listGenerations(def);
      pl_break();
    }
  }
}


foreign_t
pl_check_procedure(term_t desc)
{ GET_LD
  Procedure proc;
  Definition def;

  if ( !get_procedure(desc, &proc, 0, GP_FIND) )
    fail;
  def = getProcDefinition(proc);

  if ( true(def, FOREIGN) )
    fail;

  checkDefinition(def);

  succeed;
}


foreign_t
pl_list_generations(term_t desc)
{ GET_LD
  Procedure proc;
  Definition def;

  if ( !get_procedure(desc, &proc, 0, GP_FIND) )
    fail;
  def = getProcDefinition(proc);

  if ( true(def, FOREIGN) )
    fail;				/* permission error */

  listGenerations(def);

  succeed;
}


#endif /*O_MAINTENANCE*/


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(proc)
  PRED_DEF("meta_predicate", 1, meta_predicate, PL_FA_TRANSPARENT)
  PRED_DEF("$time_source_file", 3, time_source_file, PL_FA_NONDETERMINISTIC)
  PRED_DEF("$clause_from_source", 3, clause_from_source, 0)
  PRED_DEF("retract", 1, retract,
	   PL_FA_TRANSPARENT|PL_FA_NONDETERMINISTIC|PL_FA_ISO)
  PRED_DEF("unload_file", 1, unload_file, 0)
EndPredDefs
