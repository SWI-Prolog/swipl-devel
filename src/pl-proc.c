/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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
static void	removeClausesProcedure(Procedure proc, int sfindex);
static atom_t	autoLoader(LocalFrame fr, Code PC, Definition def);
static void	registerDirtyDefinition(Definition def);
static Procedure visibleProcedure(functor_t f, Module m);

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

  def->flags ^= def->flags & ~(SPY_ME|NEEDSCLAUSEGC|P_SHARED);
  if ( stringAtom(def->functor->name)[0] != '$' )
    set(def, TRACE_ME);
  def->number_of_clauses = 0;

  if ( isnew )
  { def->indexCardinality = 0;
    if ( def->functor->arity == 0 )
    { def->indexPattern = 0x0;
    } else
    { def->indexPattern = (0x0 | NEED_REINDEX);
      set(def, AUTOINDEX);
    }
  
    if ( def->hash_info )
    { unallocClauseIndexTable(def->hash_info);
      def->hash_info = NULL;
    }
  }
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
      unsigned long generation;
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

  if ( (proc = isStaticSystemProcedure(fd)) )
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

The return value is 1 normally, -1  if no functor exists and GF_EXISTING
is defined, and 0 if an error was raised.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define GF_EXISTING	1
#define GF_PROCEDURE	2		/* check for max arity */

static int
get_arity(term_t t, int maxarity, int *arity)
{ int a;

  if ( !PL_get_integer_ex(t, &a) )
    fail;
  if ( a < 0 )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    ATOM_not_less_than_zero, a);
  if ( maxarity >= 0 && a > maxarity )
  { char buf[100];

    return PL_error(NULL, 0,
		    tostr(buf, "limit is %d, request = %d",
			  maxarity, arity),
		    ERR_REPRESENTATION, ATOM_max_arity);
  }

  *arity = a;

  return TRUE;
}


static int
get_functor(term_t descr, functor_t *fdef, Module *m, term_t h, int how)
{ GET_LD
  term_t head = PL_new_term_ref();

  PL_strip_module(descr, m, head);

  if ( PL_is_functor(head, FUNCTOR_divide2) )
  { term_t a = PL_new_term_ref();
    atom_t name;
    int arity;

    _PL_get_arg(1, head, a);
    if ( !PL_get_atom_ex(a, &name) )
      fail;
    _PL_get_arg(2, head, a);
    if ( !get_arity(a, (how&GF_PROCEDURE) ? MAXARITY : -1, &arity ) )
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
    succeed;
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

    } else if ( PL_is_variable(pi) )
    { term_t a = PL_new_term_ref();

      PL_cons_functor(a, FUNCTOR_divide2, nt, at);
      PL_unify(pi, a);
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
      if ( !PL_get_integer(at, &e->arity) )
      { if ( !PL_is_variable(at) )
	  goto typeerror;
	e->arity = -1;
      }

      if ( e->name && e->arity >= 0 )
	e->functor = PL_new_functor(e->name, e->arity);

      if ( mt )
      { atom_t mname;

	if ( PL_get_atom(mt, &mname) )
	{ e->module = isCurrentModule(mname);
	  if ( !e->module )
	    fail;
	} else if ( PL_is_variable(mt) )
	{ e->emod = newTableEnum(GD->tables.modules);

	  if ( (sm = advanceTableEnum(e->emod)) )
	    e->module = sm->value;
	  else
	    fail;			/* no modules!? */
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
	{ int retry_times = 0;

	  while(retry_times++ < 3)
	  { if ( visibleProcedure(e->functor, e->module) )
	      succeed;
	    else
	    { Procedure proc = lookupProcedure(e->functor, e->module);

	      if ( autoLoader(environment_frame, NULL,
			      proc->definition) != ATOM_retry )
		break;
	    }
	  }
	  fail;
	}
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
	     (e->arity < 0 || e->arity == fd->arity) &&
	     isDefinedProcedure(proc) )
	{ if ( mt )
	    PL_unify_atom(mt, e->module->name);
	  if ( !e->name )
	    PL_unify_atom(nt, fd->name);
	  if ( e->arity < 0 )
	    PL_unify_integer(at, fd->arity);

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


/*  Assert a clause to a procedure. Where askes to assert either at the
    head or at the tail of the clause list.

 ** Fri Apr 29 12:44:08 1988  jan@swivax.UUCP (Jan Wielemaker)  */

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
  clause->generation.created = ++GD->generation;
  clause->generation.erased  = ~0L;	/* infinite */
#endif

  if ( def->hash_info )
  { assert(!(def->indexPattern & NEED_REINDEX));

    DEBUG(3,
	  if ( !clause->index.varmask )
	    Sdprintf("Adding non-indexed clause to %s\n", predicateName(def));
	 );

    addClauseToIndex(def, clause, where PASS_LD);
    if ( def->hash_info->size /2 > def->hash_info->buckets )
    { set(def, NEEDSREHASH);
      if ( true(def, DYNAMIC) && def->references == 0 )
      { gcClausesDefinitionAndUnlock(def); /* does UNLOCKDEF() */
	return cref;
      }
    }
  } else
  { if ( def->number_of_clauses == 25 &&
	 true(def, AUTOINDEX) )
      def->indexPattern |= NEED_REINDEX;
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
{ Definition def = proc->definition;

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
    return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
		    ATOM_modify, ATOM_thread_local_procedure, def);
  } else				/* normal Prolog procedure */
  { removeClausesProcedure(proc, 0);

    if ( true(def, DYNAMIC) )
    { if ( def->references == 0 )
      { resetProcedure(proc, FALSE);
	gcClausesDefinitionAndUnlock(def);
	succeed;
      } else				/* dynamic --> static */
      { setDynamicProcedure(proc, FALSE);
	if ( true(def, NEEDSCLAUSEGC|NEEDSREHASH) )
	{ registerDirtyDefinition(def);
	  def->references = 0;
	}
      }
    } else if ( true(def, NEEDSCLAUSEGC) )
    { registerDirtyDefinition(def);
    }

    resetProcedure(proc, FALSE);
  }
  UNLOCKDEF(def);

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Remove (mark for  deletion)  all  clauses   that  come  from  the  given
source-file or any sourcefile. Note   that thread-local predicates don't
have clauses from files, so we don't need to bother.

MT: Caller must hold L_PREDICATE
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
removeClausesProcedure(Procedure proc, int sfindex)
{ Definition def = proc->definition;
  ClauseRef c;

#ifdef O_LOGICAL_UPDATE
  GD->generation++;
#endif

  if ( true(def, P_THREAD_LOCAL) )
    return;

  for(c = def->definition.clauses; c; c = c->next)
  { Clause cl = c->clause;

    if ( (sfindex == 0 || sfindex == cl->source_no) && false(cl, ERASED) )
    { set(cl, ERASED);
      set(def, NEEDSCLAUSEGC);		/* only on first */

#ifdef O_LOGICAL_UPDATE
      cl->generation.erased = GD->generation;
#endif
      def->number_of_clauses--;
      def->erased_clauses++;
    } 
  }
  if ( def->hash_info )
    def->hash_info->alldirty = TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unlink a clause from the  definition,  both   from  the  index table and
clause-chain. The clause itself is not  deleted,   this  task is left to
retractClauseProcedure().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
unlinkClause(Definition def, Clause clause ARG_LD)
{ ClauseRef prev = NULL;
  ClauseRef c;

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


      freeClauseRef(c PASS_LD);
      def->number_of_clauses--;

      break;
    }
  }

  endCritical;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Called from erase/1, retract/1 and retractall/1. In the latter two cases
the definition is always referenced.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
retractClauseProcedure(Procedure proc, Clause clause ARG_LD)
{ Definition def = getProcDefinition(proc);

  LOCKDYNDEF(def);
  assert(true(def, DYNAMIC));
  if ( true(clause, ERASED) )
  { UNLOCKDYNDEF(def);
    succeed;
  }

  if ( def->references ||
       def->number_of_clauses > 16 )
  { set(clause, ERASED);
    if ( def->hash_info )
      markDirtyClauseIndex(def->hash_info, clause);
    def->number_of_clauses--;
    def->erased_clauses++;
    if ( def->erased_clauses > def->number_of_clauses/(unsigned)16 )
    { set(def, NEEDSCLAUSEGC);
    }
#ifdef O_LOGICAL_UPDATE
    clause->generation.erased = ++GD->generation;
#endif
    UNLOCKDYNDEF(def);

    succeed;
  }

  unlinkClause(def, clause PASS_LD);
  UNLOCKDYNDEF(def);

					/* as we do a call-back, we cannot */
					/* hold the L_PREDICATE mutex */
#if O_DEBUGGER
  if ( PROCEDURE_event_hook1 &&
       def != PROCEDURE_event_hook1->definition )
    callEventHook(PLEV_ERASED, clause);
#endif

  freeClause(clause PASS_LD);

  succeed;
}


void
freeClause(Clause c ARG_LD)
{ 
#if O_DEBUGGER
  if ( true(c, HAS_BREAKPOINTS) )
    clearBreakPointsClause(c);
#endif

  GD->statistics.codes -= c->code_size;
#ifdef O_ATOMGC
  unregisterAtomsClause(c);
#endif
  freeHeap(c, sizeofClause(c->code_size));
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
    { rehash = def->hash_info->size * 2;
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
Free a list of clauses as returned by gcClausesDefinition();
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
freeClauseList(ClauseRef cref)
{ GET_LD
  ClauseRef next;
  

  for( ; cref; cref = next)
  { Clause cl = cref->clause;
    next = cref->next;
    
#if O_DEBUGGER
    if ( PROCEDURE_event_hook1 &&
	 cl->procedure->definition != PROCEDURE_event_hook1->definition )
      callEventHook(PLEV_ERASED, cl);
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


#ifdef O_CLAUSEGC
		 /*******************************
		 *	     CLAUSE-GC		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
MT: locked by caller
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
registerDirtyDefinition(Definition def)
{ GET_LD
  DefinitionChain cell = allocHeap(sizeof(*cell));

  cell->definition = def;
  cell->next = GD->procedures.dirty;
  GD->procedures.dirty = cell;
}


foreign_t
pl_garbage_collect_clauses(void)
{ GET_LD

  if ( GD->procedures.dirty && !gc_status.blocked )
  { DefinitionChain c, *cell;
    sigset_t set;

    DEBUG(2, Sdprintf("pl_garbage_collect_clauses()\n"));

    PL_LOCK(L_THREAD);
    LOCK();
    blockSignals(&set);

					/* sanity-check */
    for(c=GD->procedures.dirty; c; c=c->next)
    { Definition def = c->definition;

      assert(false(def, DYNAMIC));
      assert(def->references == 0);
    }

    markPredicatesInEnvironments(LD);
#ifdef O_PLMT
    forThreadLocalData(markPredicatesInEnvironments,
		       PL_THREAD_SUSPEND_AFTER_WORK);
#endif

    DEBUG(1, Sdprintf("Marking complete; cleaning predicates\n"));

    for( cell = &GD->procedures.dirty; *cell; )
    { Definition def = (*cell)->definition;
      
      if ( def->references )
      { def->references = 0;
	cell = &(*cell)->next;
      } else
      { DefinitionChain next = (*cell)->next;

	def->references = 0;
	DEBUG(2, Sdprintf("gcClausesDefinition(%s)\n", predicateName(def)));
	gcClausesDefinition(def);
	freeHeap(*cell, sizeof(**cell));

	*cell = next;
      }
    }

#ifdef O_PLMT
    resumeThreads();
#endif

    unblockSignals(&set);
    UNLOCK();
    PL_UNLOCK(L_THREAD);
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
{ Procedure proc;
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
A dynamic call to `f' in `m' has to be made (via call/1, apply/2 or from
C). This procedure  returns  the  procedure  to  be  run.   If  no  such
procedure  exists  an  undefined  procedure is created and returned.  In
this case interpret() will later call  trapUndefined()  to  generate  an
error message (or link the procedure from the library via autoload).
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
  Definition def;
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
#ifdef O_PLMT
  PL_LOCK(L_THREAD);
  if ( GD->statistics.threads_created > 1 )
    freeHeap(proc->definition, sizeof(struct definition));
  PL_UNLOCK(L_THREAD);
#else
  freeHeap(proc->definition, sizeof(struct definition));
#endif
  proc->definition = def;

  return def;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Call the autoloader for the given definition   that is to be executed in
the given frame. PC is the program   pointer of the current environment.
Before we call Prolog, we need to fill enough of the frame to use it for
the garbage collector to scan this frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static atom_t
autoLoader(LocalFrame fr, Code PC, Definition def)
{ GET_LD
  fid_t  cid  = PL_open_foreign_frame();
  term_t argv = PL_new_term_refs(4);
  qid_t qid;
  atom_t sfn = source_file_name;	/* needs better solution! */
  int  sln = source_line_no;
  atom_t answer = ATOM_nil;

  if ( !GD->procedures.undefinterc4 )
    GD->procedures.undefinterc4 = PL_pred(FUNCTOR_undefinterc4,
					  MODULE_system);

  PL_put_atom(    argv+0, def->module->name);
  PL_put_atom(    argv+1, def->functor->name);
  PL_put_integer( argv+2, def->functor->arity);
  
  LD->autoload_nesting++;
  if ( PC )
  { fr->parent = environment_frame;
    fr->flags = fr->parent->flags;
    fr->predicate = def;
    fr->programPointer = PC;
    fr->clause = NULL;
#ifdef O_PROFILE
    fr->prof_node = NULL;
#endif
    environment_frame = fr;
  }
  qid = PL_open_query(MODULE_system, PL_Q_NODEBUG,
		      GD->procedures.undefinterc4, argv);
  if ( PL_next_solution(qid) )
    PL_get_atom(argv+3, &answer);
  PL_close_query(qid);
  if ( PC )
    environment_frame = fr->parent;
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
trapUndefined_unlocked(LocalFrame fr, Code PC, Procedure proc ARG_LD)
{ int retry_times = 0;
  Definition newdef;
  Definition def = proc->definition;
  Module module = def->module;
  FunctorDef functor = def->functor;

  retry:
					/* Auto import */
  if ( (newdef = autoImport(functor->functor, module)) )
    return newdef;
					/* Pred/Module does not want to trap */
  if ( true(def, PROC_DEFINED) ||
       false(module, UNKNOWN_WARNING|UNKNOWN_ERROR) )
    return def;

  DEBUG(5, Sdprintf("trapUndefined(%s)\n", predicateName(def)));

					/* Trap via exception/3 */
  if ( trueFeature(AUTOLOAD_FEATURE) && !GD->bootsession )
  { if ( LD->autoload_nesting > 100 )
    { LD->autoload_nesting = 1;
      sysError("trapUndefined(): undefined: %s", predicateName(def));

      return def;
    } else
    { atom_t answer = autoLoader(fr, PC, def);

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
    sysError("Undefined predicate: %s", predicateName(def));
  else if ( true(module, UNKNOWN_ERROR) )
    PL_error(NULL, 0, NULL, ERR_UNDEFINED_PROC, def);
  else
  { fid_t fid = PL_open_foreign_frame();
    term_t pred = PL_new_term_ref();

    unify_definition(pred, def, 0, GP_NAMEARITY);

    printMessage(ATOM_warning,
		 PL_FUNCTOR, FUNCTOR_error2,
		   PL_FUNCTOR, FUNCTOR_existence_error2,
		     PL_ATOM, ATOM_procedure,
		     PL_TERM, pred,
		   PL_VARIABLE);

    PL_discard_foreign_frame(fid);
  }

  return def;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This must be  executed  holding  the   Prolog  mutex  '$load'  to  avoid
race-conditions between threads trapping undefined   code. At the moment
there is no neat way to share   mutexes  between C and Prolog, something
that should be considered.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Definition
trapUndefined(LocalFrame fr, Code PC, Procedure proc ARG_LD)
{ LocalFrame lSafe = lTop;
  Definition def;

  lTop = (LocalFrame)argFrameP(fr, proc->definition->functor->arity);
#ifdef O_PLMT
  PL_mutex_lock(GD->thread.MUTEX_load);
#endif
  def = trapUndefined_unlocked(fr, PC, proc PASS_LD);
#ifdef O_PLMT
  PL_mutex_unlock(GD->thread.MUTEX_load);
#endif
  lTop = lSafe;


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
pl_retract(term_t term, control_t h)
{ GET_LD

  if ( ForeignControl(h) == FRG_CUTTED )
  { ClauseRef cref = ForeignContextPtr(h);

    if ( cref )
    { Definition def = getProcDefinition(cref->clause->procedure);

      leaveDefinition(def);
    }

    succeed;
  } else
  { Procedure proc;
    Definition def;
    Module m = (Module) NULL;
    ClauseRef cref;
    term_t cl = PL_new_term_ref();
    term_t head = PL_new_term_ref();
    term_t body = PL_new_term_ref();
    Word argv;
    ClauseRef next;
    atom_t b;
    mark mrk;
    term_t r0;

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

    if ( ForeignControl(h) == FRG_FIRST_CALL )
    { functor_t fd;

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

      enterDefinition(def);			/* reference the predicate */
      cref = firstClause(argv, environment_frame, def, &next PASS_LD);
      if ( !cref )
      { leaveDefinition(def);
	fail;
      }
    } else
    { cref = ForeignContextPtr(h);
      proc = cref->clause->procedure;
      def  = getProcDefinition(proc);
      cref = findClause(cref, argv, environment_frame, def, &next PASS_LD);
    }

    Mark(mrk);
    r0 = PL_new_term_refs(0);
    while( cref )
    { if ( decompile(cref->clause, cl, 0) )
      { retractClauseProcedure(proc, cref->clause PASS_LD);
	if ( !next )
	{ PL_reset_term_refs(r0);
	  leaveDefinition(def);
	  succeed;
	}

	ForeignRedoPtr(next);
      }

      PL_reset_term_refs(r0);
      Undo(mrk);

      cref = findClause(next, argv, environment_frame, def, &next PASS_LD);
    }

    leaveDefinition(def);
    fail;
  }
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
  LocalFrame fr = environment_frame;
  mark m;
  term_t r0;

  if ( !get_procedure(head, &proc, thehead, GP_FINDHERE) )
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
  if ( isTerm(*argv) )			/* retract(foobar(a1, ...)) */
    argv = argTermP(*argv, 0);
  else
    argv = NULL;			/* retract(foobar) */

  Mark(m);
  enterDefinition(def);

  if ( !(cref = firstClause(argv, fr, def, &next PASS_LD)) )
  { leaveDefinition(def);
    succeed;
  }

  r0 = PL_new_term_refs(0);
  while( cref )
  { if ( decompileHead(cref->clause, thehead) )
      retractClauseProcedure(proc, cref->clause PASS_LD);

    if ( !next )
    { PL_reset_term_refs(r0);
      Undo(m);
      leaveDefinition(def);
      succeed;
    }

    PL_reset_term_refs(r0);
    Undo(m);

    cref = findClause(next, argv, fr, def, &next PASS_LD);
  }
  leaveDefinition(def);

  succeed;
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
  int a;

  if ( !PL_get_atom_ex(atom, &name) ||
       !get_arity(arity, MAXARITY, &a) )
    fail;

  if ( !(f = isCurrentFunctor(name, a)) )
    succeed;
  if ( !checkModifySystemProc(f) )
    fail;
  if ( !(proc = isCurrentProcedure(f, m)) )
    succeed;

  if ( trueFeature(ISO_FEATURE) && false(proc->definition, DYNAMIC) )
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
  if (key == ATOM_thread_local)  return P_THREAD_LOCAL;
  if (key == ATOM_noprofile)     return P_NOPROFILE;

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
attachMutexAndUnlock(Definition def)
{ if ( !def->mutex )
  { def->mutex = allocSimpleMutex(predicateName(def));
    UNLOCK();
  } else
    countingMutexUnlock(def->mutex);
}

static void
detachMutexAndUnlock(Definition def)
{ if ( def->mutex )
  { counting_mutex *m = def->mutex;
    def->mutex = NULL;
    countingMutexUnlock(m);
    freeSimpleMutex(m);
  } else
    UNLOCK();
}

#else /*O_PLMT*/

#define attachMutexAndUnlock(def)
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

  LOCKDEF(def);
  
  if ( (isdyn && true(def, DYNAMIC)) ||
       (!isdyn && false(def, DYNAMIC)) )
  { UNLOCKDEF(def);
    succeed;
  }

  if ( isdyn )				/* static --> dynamic */
  { GET_LD
    if ( def->definition.clauses )
    { UNLOCKDEF(def);
      if ( true(def, NEEDSCLAUSEGC) )
      { pl_garbage_collect_clauses();
	LOCKDEF(def);
	if ( !def->definition.clauses )
	  goto ok;
	UNLOCKDEF(def);
      }
      return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, proc);
    }
  ok:
    set(def, DYNAMIC);
    if ( SYSTEM_MODE )
      set(def, SYSTEM|HIDE_CHILDS);

    attachMutexAndUnlock(def);
  } else				/* dynamic --> static */
  { clear(def, DYNAMIC);
    if ( def->references && true(def, NEEDSCLAUSEGC|NEEDSREHASH) )
    { registerDirtyDefinition(def);
      def->references = 0;
    }

    detachMutexAndUnlock(def);
  }

  succeed;
}


static int
set_thread_local_procedure(Procedure proc, bool val)
{
#ifdef O_PLMT
  Definition def = proc->definition;

  LOCKDEF(def);

  if ( (val && true(def, P_THREAD_LOCAL)) ||
       (!val && false(def, P_THREAD_LOCAL)) )
  { UNLOCKDEF(def);
    succeed;
  }

  if ( val )				/* static --> local */
  { if ( def->definition.clauses )
    { UNLOCKDEF(def);
      return PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, proc);
    }
    set(def, DYNAMIC|VOLATILE|P_THREAD_LOCAL);

    attachMutexAndUnlock(def);
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

  if ( att == DYNAMIC )
    return setDynamicProcedure(proc, val);
  if ( att == P_THREAD_LOCAL )
    return set_thread_local_procedure(proc, val);

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

    We cannot re-index if the predicate is referenced and hashed: other
    predicates are operating on the hashed clauses-lists.  If we are not
    hashed there is no problem: the clause-list remains unaltered.  Therefore
    assertProcedure() only signals a re-index request if the predicate is
    not yet hashed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
reindexDefinition(Definition def)
{ ClauseRef cref;
  int do_hash = 0;
  int canindex = 0;
  int cannotindex = 0;

  assert(def->references == 1 || !def->hash_info);

  DEBUG(2, if ( def->definition.clauses )
	   { Procedure proc = def->definition.clauses->clause->procedure;

	     Sdprintf("reindexDefinition(%s)\n", procedureName(proc));
	   });

  enterDefinition(def);
  def->indexPattern &= ~NEED_REINDEX;

  if ( true(def, AUTOINDEX) || def->indexPattern == 0x1 )
  { for(cref = def->definition.clauses; cref; cref = cref->next)
    { word key;
      
      if ( true(cref->clause, ERASED) )
	continue;
    
      if ( arg1Key(cref->clause, &key) )
	canindex++;
      else
	cannotindex++;
    }
  }

  if ( true(def, AUTOINDEX) )
  { if ( canindex == 0 )
    { DEBUG(2, if ( def->definition.clauses )
	       { Procedure proc = def->definition.clauses->clause->procedure;

		 Sdprintf("not indexed: %s\n", procedureName(proc));
	       });
      def->indexPattern = 0x0;
    } else
    { def->indexPattern = 0x1;
    }
  }

  if ( def->indexPattern == 0x1 &&
       canindex > 5 && cannotindex <= 2 )
    do_hash = canindex / 2;

  def->indexCardinality = cardinalityPattern(def->indexPattern);
  for(cref = def->definition.clauses; cref; cref = cref->next)
    reindexClause(cref->clause, def);
  leaveDefinition(def);

  if ( do_hash )
  { DEBUG(3,
	  if ( def->definition.clauses )
	  { Procedure proc = def->definition.clauses->clause->procedure;

	    Sdprintf("hash(%s, %d)\n", procedureName(proc), do_hash);
	  });
    hashDefinition(def, do_hash);
  }
}


void
indexDefinition(Definition def, unsigned long pattern)
{ clear(def, AUTOINDEX);

  if ( (def->indexPattern & ~NEED_REINDEX) != 0x1L &&
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

  if ( !get_clause_ptr_ex(ref, &clause) )
    fail;
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

#define source_index (GD->files._source_index)
#define sourceTable  (GD->files._source_table)

static void
registerSourceFile(SourceFile f)
{ if ( !GD->files.source_files.base )
    initBuffer(&GD->files.source_files);

  f->index = entriesBuffer(&GD->files.source_files, SourceFile) + 1;
    
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
lookupSourceFile(atom_t name)
{ SourceFile file;
  Symbol s;

  LOCK();
  if ( !sourceTable )
    sourceTable = newHTable(32);

  if ( (s=lookupHTable(sourceTable, (void*)name)) )
  { file = s->value;
  } else
  { GET_LD

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
  }
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

  { GET_LD

    cell = allocHeap(sizeof(struct list_cell));
    cell->value = proc;
    cell->next = sf->procedures;
    sf->procedures = cell;
    set(proc->definition, FILE_ASSIGNED);
  }

  UNLOCK();
}


void
redefineProcedure(Procedure proc, SourceFile sf)
{ GET_LD
  Definition def = proc->definition;

  if ( true(def, FOREIGN) )
  { abolishProcedure(proc, def->module);
    printMessage(ATOM_warning,
		 PL_FUNCTOR_CHARS, "redefined_procedure", 2,
		   PL_CHARS, "foreign",
		   _PL_PREDICATE_INDICATOR, proc);
  }
  assert(false(def, P_THREAD_LOCAL));	/* what to do? */

  if ( false(def, MULTIFILE) )
  { ClauseRef first = hasClausesDefinition(def);

    if ( first && first->clause->source_no == sf->index )
    { if ( (debugstatus.styleCheck & DISCONTIGUOUS_STYLE) &&
	   false(def, DISCONTIGUOUS) )
	printMessage(ATOM_warning,
		     PL_FUNCTOR_CHARS, "discontiguous", 1,
		       _PL_PREDICATE_INDICATOR, proc);
    } else
    { abolishProcedure(proc, def->module);

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
{ GET_LD
  int index;
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
startConsult(SourceFile sf)

This function is called when starting the consult a file. Its task is to
remove all clauses that come from this   file  if this is a *reconsult*.
There are two options.

    # Immediately remove the clauses from any non-referenced predicate.
    This saves space, but if there are multiple threads it may cause
    other threads to trap an undefined predicate.

    # Delay until garbage_collect_clauses/0
    This way other threads can happily keep running.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
startConsult(SourceFile f)
{ GET_LD

  if ( f->count++ > 0 )			/* This is a re-consult */
  { ListCell cell, next;
    sigset_t set;
    int immediate;
    ClauseRef garbage = NULL;

    PL_LOCK(L_THREAD);
    LOCK();
    blockSignals(&set);

    GD->procedures.active_marked = 0;
    GD->procedures.reloading = f;
    markPredicatesInEnvironments(LD);
#ifdef O_PLMT
    forThreadLocalData(markPredicatesInEnvironments,
		       PL_THREAD_SUSPEND_AFTER_WORK);

					/* are we alone? */
    immediate = ((GD->statistics.threads_created -
		  GD->statistics.threads_finished) == 1);
#else
    immediate = TRUE;
#endif
    GD->procedures.reloading = NULL;

    for(cell = f->procedures; cell; cell = next)
    { Procedure proc = cell->value;
      Definition def = proc->definition;

      next = cell->next;
      if ( def )
      { removeClausesProcedure(proc,
			       true(def, MULTIFILE) ? f->index : 0);

	if ( true(def, NEEDSCLAUSEGC) )
	{ if ( def->references == 0 )
	    garbage = cleanDefinition(def, garbage);
	  else if ( false(def, DYNAMIC) )
	    registerDirtyDefinition(def);
	}

	if ( false(def, DYNAMIC) && def->references )
	{ def->references = 0;
	  GD->procedures.active_marked--;
	}
      }
      freeHeap(cell, sizeof(struct list_cell));
    }
    assert(GD->procedures.active_marked == 0);
    f->procedures = NULL;

#ifdef O_PLMT
    resumeThreads();
#endif

    unblockSignals(&set);
    UNLOCK();
    PL_UNLOCK(L_THREAD);

    if ( garbage )
      freeClauseList(garbage);
  }

  f->current_procedure = NULL;
}


word
pl_start_consult(term_t file)
{ GET_LD
  atom_t name;

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
{ GET_LD
  atom_t name;
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
	{ if ( ln >= (int)cl->line_no )
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


#ifdef O_MAINTENANCE

		 /*******************************
		 *	INTERNAL DEBUGGING	*
		 *******************************/

  
static void
listGenerations(Definition def)
{ GET_LD
  ulong gen = environment_frame->generation;
  ClauseRef cl;

  Sdprintf("%s has %d clauses at generation %ld (%s)\n",
	   predicateName(def),
	   def->number_of_clauses, gen,
	   true(def, NEEDSCLAUSEGC) ? "needs clause-gc" : "clean");
	   

  for(cl=def->definition.clauses; cl; cl=cl->next)
  { Clause clause = cl->clause;

    Sdprintf("%8u: %8u-%10u %s\n",
	     ((ulong)clause - heap_base)>>2,
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

	Sdprintf("%8u: %8u-%10u %s\n",
		 ((ulong)clause - heap_base)>>2,
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
{ Procedure proc;
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
{ Procedure proc;
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
