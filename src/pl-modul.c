/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2017, University of Amsterdam
                              VU University Amsterdam
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

#include "pl-incl.h"
#undef LD
#define LD LOCAL_LD

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Definition of modules.  A module consists of a  set  of  predicates.   A
predicate  can be private or public.  By default predicates are private.
A module contains two hash tables.  One that holds  all  predicates  and
one that holds the public predicates of the module.

On trapping undefined  predicates  SWI-Prolog  attempts  to  import  the
predicate  from  the  super  module  of the module.  The module `system'
holds all system predicates and has no super module.  Module  `user'  is
the  global  module  for  the  user  and imports from `system' all other
modules import from `user' (and indirect from `system').
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int	addSuperModule_no_lock(Module m, Module s, int where);
static void	unallocModule(Module m);

static void
unallocProcedureSymbol(void *name, void *value)
{ DEBUG(MSG_CLEANUP,
	Sdprintf("unallocProcedure(%s)\n", functorName((functor_t)name)));
  unallocProcedure(value);
}


static Module
_lookupModule(atom_t name ARG_LD)
{ Module m, super;

  if ( (m = lookupHTable(GD->tables.modules, (void*)name)) )
    return m;

  m = allocHeapOrHalt(sizeof(struct module));
  memset(m, 0, sizeof(*m));

  m->name = name;
#ifdef O_PLMT
  m->mutex = allocSimpleMutex(PL_atom_chars(m->name));
#endif
  set(m, M_CHARESCAPE);
  if ( !GD->options.traditional )
    set(m, DBLQ_STRING|BQ_CODES);

  if ( name == ATOM_user || name == ATOM_system )
    m->procedures = newHTable(PROCEDUREHASHSIZE);
  else
    m->procedures = newHTable(MODULEPROCEDUREHASHSIZE);
  m->procedures->free_symbol = unallocProcedureSymbol;

  m->public = newHTable(PUBLICHASHSIZE);
  m->class  = ATOM_user;

  if ( name == ATOM_user )
  { super = MODULE_system;
  } else if ( name == ATOM_system )
  { set(m, M_SYSTEM|UNKNOWN_ERROR);
    super = NULL;
    m->class = ATOM_system;
  } else if ( stringAtom(name)[0] == '$' )
  { set(m, M_SYSTEM);
    super = MODULE_system;
    m->class = ATOM_system;
  } else
  { super = MODULE_user;
  }

  if ( super )				/* TBD: Better error-handling */
  { if ( !addSuperModule_no_lock(m, super, 'A') )
      PL_warning("Could not add super-module");
  }

  addNewHTable(GD->tables.modules, (void *)name, m);
  GD->statistics.modules++;
  PL_register_atom(name);

  return m;
}


Module
lookupModule__LD(atom_t name ARG_LD)
{ Module m;

  if ( (m = lookupHTable(GD->tables.modules, (void*)name)) )
    return m;

  PL_LOCK(L_MODULE);
  m = _lookupModule(name PASS_LD);
  PL_UNLOCK(L_MODULE);

  return m;
}


Module
isCurrentModule__LD(atom_t name ARG_LD)
{ return lookupHTable(GD->tables.modules, (void*)name);
}


static void
unallocModuleSymbol(void *name, void *value)
{ unallocModule(value);
}


void
initModules(void)
{ GET_LD
  PL_LOCK(L_MODULE);
  if ( !GD->tables.modules )
  {
#ifdef O_PLMT
    initPrologThreads();
#endif
    initFunctors();

    GD->tables.modules = newHTable(MODULEHASHSIZE);
    GD->tables.modules->free_symbol = unallocModuleSymbol;
    GD->modules.system = _lookupModule(ATOM_system PASS_LD);
    GD->modules.user   = _lookupModule(ATOM_user PASS_LD);
  }
  PL_UNLOCK(L_MODULE);
}


static void
unallocList(ListCell c)
{ ListCell n;

  for(; c; c=n)
  { n = c->next;

    freeHeap(c, sizeof(*c));
  }
}


static void
freeLingeringDefinitions(ListCell c)
{ ListCell n;

  for(; c; c=n)
  { Definition def = c->value;

    n = c->next;
    freeHeap(def, sizeof(*def));
    freeHeap(c, sizeof(*c));
  }
}


static void
unallocModule(Module m)
{ if ( m->public )     destroyHTable(m->public);
  if ( m->procedures ) destroyHTable(m->procedures);
  if ( m->operators )  destroyHTable(m->operators);
  if ( m->supers )     unallocList(m->supers);
#ifdef O_PLMT
  if ( m->mutex )      freeSimpleMutex(m->mutex);
#endif
  if ( m->lingering )  freeLingeringDefinitions(m->lingering);

  freeHeap(m, sizeof(*m));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Remove all links from  the  source   file  administration  to  the given
module. Such links are added by addProcedureSourceFile(). In theory, the
relation between procedure and source file  is many-to-many, but most of
the time it is one-to-one. In that   case, proc->source_no points to the
one source file. Otherwise (multiple files), PROC_MULTISOURCE is set and
we need to scan all source files to find the references.

This is fine for the  current   schema  of destroying temporary modules,
which are typically not supposed  to   use  constructs such as multifile
anyway. The alternative  is  for  procedures   to  maintain  a  list  of
back-links to the source files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
markSourceFilesProcedure(Procedure proc, struct bit_vector *v)
{ if ( false(proc, PROC_MULTISOURCE) )
    set_bit(v, proc->source_no);
  else
    setall_bitvector(v);
}


static void
unlinkSourceFilesModule(Module m)
{ size_t i, high = highSourceFileIndex();
  struct bit_vector *vec = new_bitvector(high+1);

  for_table(m->procedures, name, value,
	    markSourceFilesProcedure(value, vec));

  for(i=1; i<=high; i++)
  { if ( true_bit(vec, i) )
    { SourceFile sf = indexToSourceFile(i);

      if ( sf )
	unlinkSourceFileModule(sf, m);
    }
  }

  free_bitvector(vec);
}


static int
destroyModule(Module m)
{ deleteHTable(GD->tables.modules, (void*)m->name);
  PL_unregister_atom(m->name);
  unlinkSourceFilesModule(m);
  GD->statistics.modules--;
  unallocModule(m);

  return TRUE;
}


static void
emptyModule(Module m)
{ DEBUG(MSG_CLEANUP, Sdprintf("emptyModule(%s)\n", PL_atom_chars(m->name)));
  if ( m->procedures ) clearHTable(m->procedures);
}


void
cleanupModules(void)
{ Table t;

  if ( (t=GD->tables.modules) )
  { for_table(t, name, value, emptyModule(value));

    GD->tables.modules = NULL;
    destroyHTable(t);
  }
}


int
isSuperModule(Module s, Module m)	/* s is a super-module of m */
{ ListCell c;

next:
  if ( m == s )
    succeed;

  for(c=m->supers; c; c=c->next)
  { if ( c->next )
    { if ( isSuperModule(s, c->value) )
	succeed;
    } else
    { m = c->value;
      goto next;
    }
  }
  fail;
}


/* MT: Must be locked by caller
*/

/* The `level' of a module is the shortest path to the root of the
   module-tree.  The level information is used by pl-arith.c.

   TBD: We should check for cycles when adding super-modules!
*/

static void
updateLevelModule(Module m)
{ int l = -1;
  ListCell c;

  for(c=m->supers; c; c=c->next)
  { Module m2 = c->value;

    if ( m2->level > l )
      l = m2->level;
  }

  m->level = l+1;
}


static int
cannotSetSuperModule(Module m, Module s)
{ GET_LD
  term_t t = PL_new_term_ref();
  (void)s;				/* would be nice to add to message */

  PL_put_atom(t, m->name);

  return PL_error(NULL, 0, "would create a cycle",
		  ERR_PERMISSION,
		    ATOM_add_import,
		    ATOM_module,
		    t);
}


static int
reachableModule(Module here, Module end)
{ if ( here != end )
  { ListCell c;

    for(c=here->supers; c; c=c->next)
    { if ( reachableModule(c->value, end) )
	succeed;
    }

    fail;
  }

  succeed;
}



static int
addSuperModule_no_lock(Module m, Module s, int where)
{ ListCell c;

  if ( reachableModule(s, m) )
    return cannotSetSuperModule(m, s);

  for(c=m->supers; c; c=c->next)
  { if ( c->value == s )
      return TRUE;			/* already a super-module */
  }

  c = allocHeapOrHalt(sizeof(*c));
  c->value = s;

  if ( where == 'A' )
  { c->next = m->supers;
    m->supers = c;
  } else
  { ListCell *p = &m->supers;

    while(*p)
    { p = &(*p)->next;
    }
    c->next = NULL;
    *p = c;
  }

  updateLevelModule(m);
  succeed;
}


int
addSuperModule(Module m, Module s, int where)
{ int rc;

  PL_LOCK(L_MODULE);
  rc = addSuperModule_no_lock(m, s, where);
  PL_UNLOCK(L_MODULE);

  return rc;
}


static int
delSuperModule(Module m, Module s)
{ ListCell *p;

  for(p = &m->supers; *p; p = &(*p)->next)
  { ListCell c = *p;

    if ( c->value == s )
    { *p = c->next;
      freeHeap(c, sizeof(*c));

      updateLevelModule(m);
      succeed;
    }
  }

  fail;
}


static void
clearSupersModule_no_lock(Module m)
{ ListCell c = m->supers;
  ListCell next;

  m->supers = NULL;
  for(; c; c=next)
  { next = c->next;
    freeHeap(c, sizeof(*c));
  }

  m->level = 0;
}

void
clearSupersModule(Module m)
{ PL_LOCK(L_MODULE);
  clearSupersModule_no_lock(m);
  PL_UNLOCK(L_MODULE);
}


int
setSuperModule(Module m, Module s)
{ if ( s == m )
    cannotSetSuperModule(m, s);

  if ( m->supers && !m->supers->next )
  { if ( (Module)m->supers->value != s )
    { m->supers->value = s;
      m->level = s->level+1;

      succeed;
    }
  }
  clearSupersModule_no_lock(m);

  return addSuperModule_no_lock(m, s, 'A');
}


static
PRED_IMPL("set_module", 1, set_module, PL_FA_TRANSPARENT)
{ PRED_LD
  Module m = MODULE_parse;
  term_t prop = PL_new_term_ref();
  atom_t pname;
  size_t arity;

  if ( !PL_strip_module(A1, &m, prop) )
    return FALSE;
  if ( PL_get_name_arity(prop, &pname, &arity) && arity == 1 )
  { term_t arg = PL_new_term_ref();

    _PL_get_arg(1, prop, arg);

    if ( pname == ATOM_base )
    { atom_t mname;
      Module super;
      int rc;

      if ( !PL_get_atom_ex(arg, &mname) )
	return FALSE;
      super = lookupModule(mname);
      PL_LOCK(L_MODULE);
      rc = setSuperModule(m, super);
      PL_UNLOCK(L_MODULE);
      return rc;
    } else if ( pname == ATOM_class )
    { atom_t class;

      if ( !PL_get_atom_ex(arg, &class) )
	return FALSE;
      if ( class == ATOM_user ||
	   class == ATOM_system ||
	   class == ATOM_library ||
	   class == ATOM_test ||
	   class == ATOM_development )
      { m->class = class;
	return TRUE;
      } else if ( class == ATOM_temporary )
      { if ( m->procedures && m->procedures->size != 0 )
	  return PL_error(NULL, 0,
			  "module is not empty",
			  ERR_PERMISSION, ATOM_module_property, ATOM_class, arg);
	m->class = class;
	return TRUE;
      } else
	return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_module_class, arg);
    } else if ( pname == ATOM_program_space )
    { size_t limit;

      if ( !PL_get_size_ex(arg, &limit) )
	return FALSE;
      if ( limit && limit < m->code_size )
      { term_t ex = PL_new_term_ref();

	PL_put_atom(ex, m->name);
	return PL_error(NULL, 0, "Used exceeds limit", ERR_PERMISSION,
			ATOM_limit, ATOM_program_space, ex);
      }
      m->code_limit = limit;
      return TRUE;
    } else
    { return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_module_property, prop);
    }
  } else
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_module_property, prop);
}


static int
inheritUnknown(Module m)
{ int u;
  ListCell c;

  if ( (u = (m->flags & UNKNOWN_MASK)) )
    return u;

  for(c = m->supers; c; c=c->next)
  { if ( (u = getUnknownModule(c->value)) )
      return u;
  }

  return 0;
}


int		/* one of UNKNOWN_ERROR, UNKNOWN_WARNING, UNKNOWN_FAIL */
getUnknownModule(Module m)
{ int u = inheritUnknown(m);

  if ( !u )
    u = UNKNOWN_ERROR;

  return u;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stripModuleName() takes an atom or term,   possible  embedded in the :/2
module term. It assigns *name  with   the  associated  module names. The
return value is the plain term or NULL if `term` is cyclic.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Word
stripModuleName(Word term, atom_t *name ARG_LD)
{ int depth = 100;
  deRef(term);
  atom_t nm = 0;

  while( hasFunctor(*term, FUNCTOR_colon2) )
  { Word mp;
    mp = argTermP(*term, 0);
    deRef(mp);
    if ( !isTextAtom(*mp) )
      break;
    nm = *mp;
    term = argTermP(*term, 1);
    deRef(term);
    if ( --depth == 0 && !is_acyclic(term PASS_LD) )
    { term_t t = pushWordAsTermRef(term);
      PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_acyclic_term, t);
      popTermRef();
      return NULL;
    }
  }

  if ( nm )
    *name = nm;

  return term;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stripModule() takes an atom or term, possible embedded in the :/2 module
term.  It will assign *module with the associated module and return  the
remaining term.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Word
stripModule(Word term, Module *module, int flags ARG_LD)
{ atom_t mname = 0;
  Word rc;

  if ( (rc=stripModuleName(term, &mname PASS_LD)) )
  { if ( mname )
    { if ( unlikely(flags&SM_NOCREATE) )
      { Module m;

	if ( (m=isCurrentModule(mname)) )
	  *module = m;
	else
	  return NULL;
      } else
      { *module = lookupModule(mname);
      }
    } else
    { *module = (environment_frame ? contextModule(environment_frame)
		                   : MODULE_user);
    }
  }

  return rc;
}


bool
isPublicModule(Module module, Procedure proc)
{ GET_LD
  if ( lookupHTable(module->public,
		    (void *)proc->definition->functor->functor) )
    succeed;

  fail;
}


		/********************************
		*       PROLOG CONNECTION       *
		*********************************/

static int
get_module(term_t t, Module *m, int create)
{ GET_LD
  atom_t name;

  if ( !PL_get_atom_ex(t, &name) )
    fail;
  if ( create )
  { *m = lookupModule(name);
    succeed;
  }
  if ( (*m = isCurrentModule(name)) )
    succeed;
  fail;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that this predicate uses integers to   avoid crashes due to changes
to the linked list while processing.  This leads to quadratic behaviour,
but given the low number of supers this shouldn't be too bad.

import_module
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("import_module", 2, import_module,
	  PL_FA_NONDETERMINISTIC)
{ PRED_LD
  int i, n;
  ListCell c;
  Module m;

  switch(ForeignControl(PL__ctx))
  { case FRG_FIRST_CALL:
      i = 0;
      break;
    case FRG_REDO:
      i = (int)ForeignContextInt(PL__ctx);
      break;
    default:
      succeed;
  }

  if ( !get_module(A1, &m, TRUE) )
    fail;

  for(n=0, c=m->supers; c; c=c->next, n++)
  { Module s = c->value;

    if ( n == i )
    { int ndet = c->next != NULL && PL_is_variable(A2);

      if ( PL_unify_atom(A2, s->name) )
      { if ( ndet )
	  ForeignRedoInt(i+1);
	else
	  succeed;
      }
    }
  }

  fail;
}


static
PRED_IMPL("add_import_module", 3, add_import_module, 0)
{ PRED_LD
  Module me, super;
  atom_t where;

  if ( !get_module(A1, &me, TRUE) ||
       !get_module(A2, &super, TRUE) ||
       !PL_get_atom_ex(A3, &where) )
    fail;

  return addSuperModule(me, super, where == ATOM_start ? 'A' : 'Z');
}


static
PRED_IMPL("delete_import_module", 2, delete_import_module, 0)
{ Module me, super;
  int rval;

  if ( !get_module(A1, &me, TRUE) ||
       !get_module(A2, &super, TRUE) )
    fail;

  PL_LOCK(L_MODULE);
  rval = delSuperModule(me, super);
  PL_UNLOCK(L_MODULE);

  return rval;
}


static int
get_existing_source_file(term_t file, SourceFile *sfp ARG_LD)
{ SourceFile sf;
  atom_t a;

  if ( PL_get_atom(file, &a) )
  { if ( (sf = lookupSourceFile(a, FALSE)) )
    { *sfp = sf;
      return TRUE;
    }

    return FALSE;
  }

  *sfp = NULL;
  return TRUE;
}


/** '$current_module'(+Module, -File) is semidet.
    '$current_module'(-ModuleOrList, +File) is semidet.
    '$current_module'(-Module, -File) is nondet.

Query module<->file association. This association  is N:1 in SWI-Prolog.
Think e.g., of test-units that are mapped  to modules. When used in mode
(-, +), this predicate unifies Module with  a non-empty list if the file
is associated to multiple modules.
*/

static
PRED_IMPL("$current_module", 2, current_module, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  TableEnum e;
  Module m;
  atom_t name;
  SourceFile sf = NULL;

  term_t module = A1;
  term_t file   = A2;

  switch(CTX_CNTRL)
  { case FRG_FIRST_CALL:
				/* deterministic case: module --> file */
      if ( PL_get_atom(module, &name) )
      { Module m;

	if ( (m=isCurrentModule(name)) )
	{ atom_t f = (!m->file ? ATOM_nil : m->file->name);
	  return PL_unify_atom(file, f);
	}

	return FALSE;
      }

      if ( !get_existing_source_file(file, &sf PASS_LD) )
	return FALSE;			/* given, but non-existing file */

      if ( sf )
      { if ( sf->modules )
	{ int rc = FALSE;

	  PL_LOCK(L_PREDICATE);
	  if ( sf->modules->next )
	  { term_t tail = PL_copy_term_ref(module);
	    term_t head = PL_new_term_ref();
	    ListCell c;

	    for(c=sf->modules; c; c=c->next)
	    { Module m = c->value;

	      if ( !(PL_unify_list(tail, head, tail) &&
		     PL_unify_atom(head, m->name)) )
		goto out;
	    }
	    rc = PL_unify_nil(tail);
	  } else
	  { Module m = sf->modules->value;
	    rc = PL_unify_atom(module, m->name);
	  }

	out:
	  PL_UNLOCK(L_PREDICATE);
	  return rc;
	}
	return FALSE;			/* source-file has no modules */
      }

      e = newTableEnum(GD->tables.modules);
      break;
    case FRG_REDO:
      e = CTX_PTR;
      get_existing_source_file(file, &sf PASS_LD);
      break;
    case FRG_CUTTED:
      e = CTX_PTR;
      freeTableEnum(e);
      succeed;
    default:
      assert(0);
      return FALSE;
  }

					/* mode (-,-) */

  while( advanceTableEnum(e, NULL, (void**)&m) )
  { atom_t f = ( !m->file ? ATOM_nil : m->file->name);

    if ( m->class == ATOM_system && m->name != ATOM_system &&
	 !SYSTEM_MODE && PL_is_variable(module) )
      continue;

    if ( PL_unify_atom(module, m->name) &&
	 PL_unify_atom(file, f) )
      ForeignRedoPtr(e);

    break;				/* must be an error */
  }

  freeTableEnum(e);
  return FALSE;
}


static
PRED_IMPL("strip_module", 3, strip_module, PL_FA_TRANSPARENT)
{ GET_LD
  Module m = (Module) NULL;
  term_t plain;

  if ( (plain = PL_new_term_ref()) &&
       PL_strip_module(A1, &m, plain) &&
       PL_unify_atom(A2, m->name) &&
       PL_unify(A3, plain) )
    succeed;

  fail;
}


static
PRED_IMPL("$current_typein_module", 1, current_typein_module, 0)
{ PRED_LD

  return PL_unify_atom(A1, LD->modules.typein->name);
}

static
PRED_IMPL("$set_typein_module", 1, set_typein_module, 0)
{ PRED_LD
  atom_t name;

  if ( !PL_get_atom_ex(A1, &name) )
    return FALSE;

  LD->modules.typein = lookupModule(name);
  return TRUE;
}

static
PRED_IMPL("$current_source_module", 1, current_source_module, 0)
{ PRED_LD

  return PL_unify_atom(A1, LD->modules.source->name);
}


PRED_IMPL("$set_source_module", 1, set_source_module, 0)
{ PRED_LD
  atom_t name;

  if ( !PL_get_atom_ex(A1, &name) )
    return FALSE;

  LD->modules.source = lookupModule(name);
  return TRUE;
}


#ifdef O_PROLOG_HOOK
word
pl_set_prolog_hook(term_t module, term_t old, term_t new)
{ Module m;
  atom_t mname;

  if ( !PL_get_atom(module, &mname) )
    PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, module);
  m = lookupModule(mname);

  if ( m->hook )
  { if ( !unify_definition(MODULE_user, old, m->hook->definition, 0, GP_HIDESYSTEM) )
      return FALSE;
  } else
  { if ( !PL_unify_nil(old) )
      return FALSE;
  }

  if ( PL_get_nil(new) )
  { m->hook = NULL;
    return TRUE;
  } else
    return get_procedure(new, &m->hook, 0, GP_NAMEARITY|GP_CREATE);
}
#endif


typedef struct defm_target
{ term_t    pi;				/* user supplied predicate indicator */
  functor_t functor;			/* functor associated to the above */
} defm_target;


static int
find_modules_with_defs(Module m, int count, defm_target targets[],
		       term_t tmp, term_t mlist,
		       int l ARG_LD)
{ ListCell c;
  int i;
  int found = FALSE;
  term_t mhead = tmp+0;
  term_t plist = tmp+1;
  term_t phead = tmp+2;

  DEBUG(9, Sdprintf("Trying %s\n", PL_atom_chars(m->name)));

  if ( l < 0 )
  { sysError("OOPS loop in default modules???\n");
    return FALSE;
  }

  for(i=0; i<count; i++)
  { Procedure proc;

    if ( (proc = isCurrentProcedure(targets[i].functor, m)) &&
	 proc->definition->impl.any )
    { if ( !found )
      { found = TRUE;
	PL_put_variable(plist);
	if ( !PL_unify_list(mlist, mhead, mlist) ||
	     !PL_unify_term(mhead, PL_FUNCTOR, FUNCTOR_minus2,
			             PL_ATOM, m->name,
				     PL_TERM, plist) )
	  return FALSE;
      }

      if ( !PL_unify_list(plist, phead, plist) ||
	   !PL_unify(phead, targets[i].pi) )
	return FALSE;
    }
  }
  if ( found && !PL_unify_nil(plist) )
    return FALSE;

  for(c = m->supers; c; c=c->next)
  { Module s = c->value;

    if ( !find_modules_with_defs(s, count, targets, tmp, mlist, l-1 PASS_LD) )
      return FALSE;
  }

  return TRUE;
}


/** '$def_modules'(:list(PI), -list(Pair)) is det.

Each Pair is a  pair  Module-list(PI),   where  Module:PI  is  a defined
predicate in the starting module or  a   default  module thereof. If the
first argument is qualified, this  is   the  starting  module. Else, the
default source module is the starting module.   Only modules in which PI
has a real definition are returned (i.e., _not_ modules where PI is only
defined as dynamic or multifile.

@see	boot/expand.pl uses this to find relevant modules that define
	term_expansion/2,4 and/or goal_expansion/2,4 definitions.
*/

#define MAX_TARGETS 10

static
PRED_IMPL("$def_modules", 2, def_modules, PL_FA_TRANSPARENT)
{ PRED_LD
  Module m = LD->modules.source;
  defm_target targets[MAX_TARGETS];
  int tcount = 0;
  term_t ttail = PL_new_term_ref();
  term_t tmp   = PL_new_term_refs(3);
  term_t tail  = PL_copy_term_ref(A2);
  term_t thead = tmp+0;
  atom_t mname = 0;
  Word mp;

  if ( !(mp=stripModuleName(valTermRef(A1), &mname PASS_LD)) )
    return FALSE;
  *valTermRef(ttail) = linkVal(mp);

  if ( mname )
  { Module m2;

    if ( (m2 = isCurrentModule(mname)) )
      m = m2;
    else if ( stringAtom(mname)[0] == '$' )
      m = MODULE_system;
    else
      m = MODULE_user;
  }

  while( PL_get_list_ex(ttail, thead, ttail) )
  { if ( tcount >= MAX_TARGETS )
      return PL_resource_error("target_predicates");
    if ( !get_functor(thead, &targets[tcount].functor,
		      NULL, 0, GF_PROCEDURE|GP_NOT_QUALIFIED) )
      return FALSE;
    targets[tcount].pi = PL_copy_term_ref(thead);
    tcount++;
  }
  if ( !PL_get_nil_ex(ttail) )
    return FALSE;

  if ( !find_modules_with_defs(m, tcount, targets, tmp, tail, 100 PASS_LD) )
    return FALSE;

  return PL_unify_nil(tail);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Declare `name' to be a module with `file' as its source  file.   If  the
module was already loaded its public table is cleared and all procedures
in it are abolished.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
declareModule(atom_t name, atom_t class, atom_t super,
	      SourceFile sf, int line,
	      int allow_newfile)
{ GET_LD
  Module module = lookupModule(name);
  term_t tmp = 0, rdef = 0, rtail = 0;
  int rc = TRUE;

  PL_LOCK(L_MODULE);
  if ( class )
    module->class = class;

  if ( !allow_newfile && module->file && module->file != sf)
  { term_t obj;
    char msg[256];
    PL_UNLOCK(L_MODULE);

    obj = PL_new_term_ref();
    PL_put_atom(obj, name);
    Ssprintf(msg, "Already loaded from %s",
	     atom_summary(module->file->name, 100));
    return PL_error("module", 2, msg, ERR_PERMISSION,
		    ATOM_redefine, ATOM_module, obj);
  }

  if ( module->file != sf )
  { module->file = sf;
    addModuleSourceFile(sf, module);
  }
  module->line_no = line;
  LD->modules.source = module;

  if ( sf->reload )
  { registerReloadModule(sf, module);
  } else
  { for_table(module->procedures, name, value,
	      { Procedure proc = value;
		Definition def = proc->definition;
		if ( !true(def, P_DYNAMIC|P_MULTIFILE|P_FOREIGN) )
		{ if ( def->module == module &&
		       hasClausesDefinition(def) )
		  { if ( !rdef )
		    { rdef = PL_new_term_ref();
		      rtail = PL_copy_term_ref(rdef);
		      tmp = PL_new_term_ref();
		    }

		    PL_unify_list(rtail, tmp, rtail);
		    unify_definition(MODULE_user, tmp, def, 0, GP_NAMEARITY);
		  }
		  abolishProcedure(proc, module);
		}
	      })
    clearHTable(module->public);
  }
  if ( super )
    setSuperModule(module, _lookupModule(super PASS_LD));

  PL_UNLOCK(L_MODULE);

  if ( rdef )
  { if ( !PL_unify_nil(rtail) )
      return FALSE;

    rc = printMessage(ATOM_warning,
		      PL_FUNCTOR_CHARS, "declare_module", 2,
		        PL_ATOM, name,
		        PL_FUNCTOR_CHARS, "abolish", 1,
		          PL_TERM, rdef);
  }

  return rc;
}


/** '$declare_module'(+Module, +Class, +Super, +File, +Line, +Redefine) is det.

Start a new (source-)module

@param	Module is the name of the module to declare
@param	File is the canonical name of the file from which the module
	is loaded
@param  Line is the line-number of the :- module/2 directive.
@param	Redefine If =true=, allow associating the module to a new file
*/

static
PRED_IMPL("$declare_module", 6, declare_module, 0)
{ PRED_LD
  SourceFile sf;
  atom_t mname, cname, sname, fname;
  int line_no, rdef;

  term_t module   = A1;
  term_t class    = A2;
  term_t super    = A3;
  term_t file     = A4;
  term_t line     = A5;
  term_t redefine = A6;

  if ( !PL_get_atom_ex(module, &mname) ||
       !PL_get_atom_ex(class, &cname) ||
       !PL_get_atom_ex(super, &sname) ||
       !PL_get_atom_ex(file, &fname) ||
       !PL_get_integer_ex(line, &line_no) ||
       !PL_get_bool_ex(redefine, &rdef) )
    fail;

  sf = lookupSourceFile(fname, TRUE);
  return declareModule(mname, cname, sname, sf, line_no, rdef);
}


static int
unify_export_list(term_t public, Module module ARG_LD)
{ term_t head = PL_new_term_ref();
  term_t list = PL_copy_term_ref(public);
  int rval = TRUE;

  LOCKMODULE(module);
  for_table(module->public, name, value,
	    { if ( !PL_unify_list(list, head, list) ||
		   !unify_functor(head, (functor_t)name, GP_NAMEARITY) )
	      { rval = FALSE;
		break;
	      }
	    })
  UNLOCKMODULE(module);
  if ( rval )
    return PL_unify_nil(list);

  fail;
}


static
PRED_IMPL("$module_property", 2, module_property, 0)
{ PRED_LD
  Module m;
  term_t a = PL_new_term_ref();
  atom_t pname;
  size_t parity;

  if ( !get_module(A1, &m, FALSE) )
    fail;

  if ( !PL_get_name_arity(A2, &pname, &parity) ||
       parity != 1 )
    return PL_error(NULL, 0, NULL, ERR_TYPE,
		    ATOM_module_property, A2);

  _PL_get_arg(1, A2, a);

  if ( pname == ATOM_line_count )
  { if ( m->line_no > 0 )
      return PL_unify_integer(a, m->line_no);
    else
      fail;
  } else if ( pname == ATOM_file )
  { if ( m->file )
      return PL_unify_atom(a, m->file->name);
    else
      fail;
  } else if ( pname == ATOM_exports )
  { return unify_export_list(a, m PASS_LD);
  } else if ( pname == ATOM_class )
  { return PL_unify_atom(a, m->class);
  } else if ( pname == ATOM_program_size )
  { return PL_unify_int64(a, m->code_size);
  } else if ( pname == ATOM_last_modified_generation )
  { return PL_unify_int64(a, m->last_modified);
  } else if ( pname == ATOM_program_space )
  { if ( m->code_limit )
      return PL_unify_int64(a, m->code_limit);
    return FALSE;
  } else
    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    ATOM_module_property, A2);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
export/1 exports a procedure specified by its name and arity or
head from the context module.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
exportProcedure(Module module, Procedure proc)
{ LOCKMODULE(module);
  updateHTable(module->public,
	       (void *)proc->definition->functor->functor,
	       proc);
  UNLOCKMODULE(module);

  return TRUE;
}

static int
export_pi1(term_t pi, Module module ARG_LD)
{ functor_t fd;
  Procedure proc;

  if ( !get_functor(pi, &fd, &module, 0, GF_PROCEDURE|GF_NAMEARITY) )
    return FALSE;

  if ( (proc = isStaticSystemProcedure(fd)) && true(proc->definition, P_ISO) )
    return TRUE;
  proc = lookupProcedure(fd, module);

  if ( ReadingSource )
  { SourceFile sf = lookupSourceFile(source_file_name, TRUE);
    return exportProcedureSource(sf, module, proc);
  } else
  { return exportProcedure(module, proc);
  }
}

static int
export_pi(term_t pi, Module module, int depth ARG_LD)
{ if ( !PL_strip_module(pi, &module, pi) )
    return FALSE;

  while ( PL_is_functor(pi, FUNCTOR_comma2) )
  { term_t a1 = PL_new_term_ref();

    if ( ++depth == 100 && !PL_is_acyclic(pi) )
      return PL_type_error("acyclic_term", pi);

    _PL_get_arg(1, pi, a1);
    if ( !export_pi(a1, module, depth PASS_LD) )
      return FALSE;
    PL_reset_term_refs(a1);
    _PL_get_arg(2, pi, pi);
  }

  return export_pi1(pi, module PASS_LD);
}



static
PRED_IMPL("export", 1, export, PL_FA_TRANSPARENT)
{ PRED_LD
  Module module = NULL;

  return export_pi(A1, module, 0 PASS_LD);
}


/** '$undefined_export'(+Module, -UndefExport:list(pi)) is det.

Unify UndefExport with predicate indicators   of undefined predicates in
Module.
*/

static
PRED_IMPL("$undefined_export", 2, undefined_export, 0)
{ PRED_LD
  atom_t mname;
  Module module;
  TableEnum e;
  Procedure proc;
  term_t tail = PL_copy_term_ref(A2);
  term_t head = PL_new_term_ref();

  if ( !PL_get_atom_ex(A1, &mname) )
    return FALSE;
  if ( !(module = isCurrentModule(mname)) )
    return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_module, A1);

  e = newTableEnum(module->public);

  while( advanceTableEnum(e, NULL, (void**)&proc) )
  { Definition def = proc->definition;
    FunctorDef fd = def->functor;

    if ( !isDefinedProcedure(proc) &&			/* not defined */
	 def->module == module &&			/* not imported */
	 !autoImport(fd->functor, module) )
    { if ( !PL_unify_list(tail, head, tail) ||
	   !unify_definition(MODULE_user, head, proc->definition,
			     0, GP_QUALIFY|GP_NAMEARITY) )
      { freeTableEnum(e);
	return FALSE;
      }
    }
  }

  freeTableEnum(e);
  return PL_unify_nil(tail);
}


word
pl_context_module(term_t module)
{ GET_LD
  return PL_unify_atom(module, contextModule(environment_frame)->name);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pl_import() imports the predicate specified with its argument  into  the
current  context  module.   If  the  predicate is already defined in the
context a warning is displayed and the predicate is  NOT  imported.   If
the  predicate  is  not  on  the  public  list of the exporting module a
warning is displayed, but the predicate is imported nevertheless.

A particulary nasty problem happens  if   a  procedure  is exported from
module A to B and then to C, while C   loads B before B loads A. In this
case C will share the definition of B, which is subsequently overwritten
when B imports A. The fixExport() stuff deals with this situation. It is
considered very rare and probably scanning  all predicate definitions is
fine.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline void
fixExportModule(Module m, Definition old, Definition new)
{ LOCKMODULE(m);

  for_table(m->procedures, name, value,
	    { Procedure proc = value;

	      if ( proc->definition == old )
	      { DEBUG(1, Sdprintf("Patched def of %s\n",
				  procedureName(proc)));
		proc->definition = new;
	      }
	    });

  UNLOCKMODULE(m);
}


static void
fixExport(Definition old, Definition new)
{ PL_LOCK(L_MODULE);
  for_table(GD->tables.modules, name, value,
	    fixExportModule(value, old, new));
  PL_UNLOCK(L_MODULE);
}


int
atomToImportStrength(atom_t a)
{ if ( a == ATOM_weak )
    return PROC_WEAK;
  else if ( a == ATOM_strong )
    return 0;
  else
    return -1;				/* domain error */
}


static int
import(term_t pred, term_t strength ARG_LD)
{ Module source = NULL;
  Module destination = contextModule(environment_frame);
  functor_t fd;
  Procedure proc, old;
  int pflags = 0;

  if ( !get_functor(pred, &fd, &source, 0, GF_PROCEDURE) )
    return FALSE;
  if ( strength )
  { atom_t a;

    if ( !PL_get_atom_ex(strength, &a) )
      return FALSE;
    if ( (pflags=atomToImportStrength(a)) < 0 )
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_import_type, strength);
  }

  if ( !(proc = lookupProcedure(fd, source)) )
    return FALSE;

  if ( !isDefinedProcedure(proc) )
    autoImport(proc->definition->functor->functor, proc->definition->module);

retry:
  if ( (old = isCurrentProcedure(proc->definition->functor->functor,
				 destination)) )
  { if ( old->definition == proc->definition )
      succeed;			/* already done this! */

    if ( !isDefinedProcedure(old) )
    { Definition odef = old->definition;

      old->definition = proc->definition;
      shareDefinition(proc->definition);
      if ( unshareDefinition(odef) > 0 )
      { fixExport(odef, proc->definition);
      } else
      { lingerDefinition(odef);
      }
      set(old, pflags);

      succeed;
    }

    if ( old->definition->module == destination )
    { if ( (pflags & PROC_WEAK) )
      { if ( truePrologFlag(PLFLAG_WARN_OVERRIDE_IMPLICIT_IMPORT) )
	{ term_t pi = PL_new_term_ref();

	  if ( !PL_unify_predicate(pi, proc, GP_NAMEARITY) )
	    return FALSE;

	  if ( !printMessage(ATOM_warning,
			     PL_FUNCTOR_CHARS, "ignored_weak_import", 2,
			       PL_ATOM, destination->name,
			       PL_TERM, pi) )
	    return FALSE;
	}

	return TRUE;
      } else
	return PL_error("import", 1, "name clash", ERR_IMPORT_PROC,
			proc, destination->name, 0);
    }

    if ( old->definition->module != source )	/* already imported */
    { return PL_error("import", 1, NULL, ERR_IMPORT_PROC,
		      proc, destination->name,
		      old->definition->module->name);
    }

    sysError("Unknown problem importing %s into module %s",
	     procedureName(proc),
	     stringAtom(destination->name));
    fail;
  }

  if ( !isPublicModule(source, proc) )
  { term_t pi = PL_new_term_ref();

    if ( !PL_unify_predicate(pi, proc, GP_NAMEARITY) )
      return FALSE;
    if ( !printMessage(ATOM_warning,
		       PL_FUNCTOR_CHARS, "import_private", 2,
		         PL_ATOM, destination->name,
		         PL_TERM, pi) )
      return FALSE;
  }

  { Procedure nproc = (Procedure)  allocHeapOrHalt(sizeof(struct procedure));
    void *old;

    nproc->flags = pflags;
    nproc->source_no = 0;
    nproc->definition = proc->definition;
    shareDefinition(proc->definition);

    LOCKMODULE(destination);
    old = addHTable(destination->procedures,
		    (void *)proc->definition->functor->functor, nproc);
    UNLOCKMODULE(destination);
    if ( old != nproc )
    { int shared = unshareDefinition(proc->definition);
      assert(shared > 0);
      freeHeap(nproc, sizeof(*nproc));
      goto retry;
    }
  }

  return TRUE;
}

static
PRED_IMPL("import", 1, import, PL_FA_TRANSPARENT)
{ PRED_LD

  return import(A1, 0 PASS_LD);
}

static
PRED_IMPL("$import", 2, import, PL_FA_TRANSPARENT)
{ PRED_LD

  return import(A1, A2 PASS_LD);
}

/** '$destroy_module'(+Module) is det.

Destroy all traces of  the  named  module.   This  is  only  safe  if no
procedure in Module is executing  and   there  are no predicates outside
this module that link to predicates of this module.
*/

static
PRED_IMPL("$destroy_module", 1, destroy_module, 0)
{ PRED_LD
  atom_t name;

  if ( PL_get_atom_ex(A1, &name) )
  { Module m;

    if ( (m=isCurrentModule(name)) )
    { if ( m->class == ATOM_temporary )
	return destroyModule(m);
      return PL_error(NULL, 0,
		      "module is not temporary",
		      ERR_PERMISSION, ATOM_destroy, ATOM_module, A1);
    }

    return TRUE;				/* non-existing */
  }

  return FALSE;
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(module)
  PRED_DEF("import_module", 2, import_module,
	   PL_FA_NONDETERMINISTIC)
  PRED_DEF("$def_modules", 2, def_modules, PL_FA_TRANSPARENT)
  PRED_DEF("$declare_module", 6, declare_module, 0)
  PRED_DEF("add_import_module", 3, add_import_module, 0)
  PRED_DEF("delete_import_module", 2, delete_import_module, 0)
  PRED_DEF("set_module", 1, set_module, PL_FA_TRANSPARENT)
  PRED_DEF("$current_module", 2, current_module, PL_FA_NONDETERMINISTIC)
  PRED_DEF("$module_property", 2, module_property, 0)
  PRED_DEF("strip_module", 3, strip_module, PL_FA_TRANSPARENT)
  PRED_DEF("import", 1, import, PL_FA_TRANSPARENT)
  PRED_DEF("$import", 2, import, PL_FA_TRANSPARENT)
  PRED_DEF("export", 1, export, PL_FA_TRANSPARENT)
  PRED_DEF("$undefined_export", 2, undefined_export, 0)
  PRED_DEF("$destroy_module", 1, destroy_module, 0)
  PRED_DEF("$current_source_module", 1, current_source_module, 0)
  PRED_DEF("$set_source_module", 1, set_source_module, 0)
  PRED_DEF("$current_typein_module", 1, current_typein_module, 0)
  PRED_DEF("$set_typein_module", 1, set_typein_module, 0)
EndPredDefs
