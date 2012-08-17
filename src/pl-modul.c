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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "pl-incl.h"
#define LOCK()   PL_LOCK(L_MODULE)
#define UNLOCK() PL_UNLOCK(L_MODULE)
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
unallocProcedureSymbol(Symbol s)
{ DEBUG(MSG_CLEANUP,
	Sdprintf("unallocProcedure(%s)\n", functorName((functor_t)s->name)));
  unallocProcedure(s->value);
}


static Module
_lookupModule(atom_t name)
{ Symbol s;
  Module m, super;

  if ((s = lookupHTable(GD->tables.modules, (void*)name)) != (Symbol) NULL)
    return (Module) s->value;

  m = allocHeapOrHalt(sizeof(struct module));
  m->name = name;
  m->file = NULL;
  m->operators = NULL;
  m->level = 0;
#ifdef O_PROLOG_HOOK
  m->hook = NULL;
#endif
#ifdef O_PLMT
  m->mutex = allocSimpleMutex(PL_atom_chars(m->name));
#endif
  clearFlags(m);
  set(m, CHARESCAPE);

  if ( name == ATOM_user || name == ATOM_system )
    m->procedures = newHTable(PROCEDUREHASHSIZE);
  else
    m->procedures = newHTable(MODULEPROCEDUREHASHSIZE);
  m->procedures->free_symbol = unallocProcedureSymbol;

  m->public = newHTable(PUBLICHASHSIZE);
  m->supers = NULL;
  m->class  = ATOM_user;

  if ( name == ATOM_user )
  { super = MODULE_system;
  } else if ( name == ATOM_system )
  { set(m, SYSTEM|UNKNOWN_ERROR);
    super = NULL;
    m->class = ATOM_system;
  } else if ( stringAtom(name)[0] == '$' )
  { set(m, SYSTEM);
    super = MODULE_system;
    m->class = ATOM_system;
  } else
  { super = MODULE_user;
  }

  if ( super )				/* TBD: Better error-handling */
  { if ( !addSuperModule_no_lock(m, super, 'A') )
      PL_warning("Could not add super-module");
  }

  addHTable(GD->tables.modules, (void *)name, m);
  GD->statistics.modules++;
  PL_register_atom(name);

  return m;
}


Module
lookupModule(atom_t name)
{ Module m;

  LOCK();
  m = _lookupModule(name);
  UNLOCK();

  return m;
}


Module
isCurrentModule(atom_t name)
{ Symbol s;

  if ( (s = lookupHTable(GD->tables.modules, (void*)name)) )
    return (Module) s->value;

  return NULL;
}


static void
unallocModuleSymbol(Symbol s)
{ unallocModule(s->value);
}


void
initModules(void)
{ LOCK();
  if ( !GD->tables.modules )
  {
#ifdef O_PLMT
    initPrologThreads();
#endif
    initTables();
    initFunctors();

    GD->tables.modules = newHTable(MODULEHASHSIZE);
    GD->tables.modules->free_symbol = unallocModuleSymbol;
    GD->modules.system = _lookupModule(ATOM_system);
    GD->modules.user   = _lookupModule(ATOM_user);
  }
  UNLOCK();
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
unallocModule(Module m)
{ if ( m->procedures ) destroyHTable(m->procedures);
  if ( m->public )     destroyHTable(m->public);
  if ( m->operators )  destroyHTable(m->operators);
  if ( m->supers )     unallocList(m->supers);
  if ( m->mutex )      freeSimpleMutex(m->mutex);

  freeHeap(m, sizeof(*m));
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
  { for_unlocked_table(t, s, emptyModule(s->value));

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

  LOCK();
  rc = addSuperModule_no_lock(m, s, where);
  UNLOCK();

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
{ LOCK();
  clearSupersModule_no_lock(m);
  UNLOCK();
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
  int arity;

  PL_strip_module(A1, &m, prop);
  if ( PL_get_name_arity(prop, &pname, &arity) && arity == 1 )
  { term_t arg = PL_new_term_ref();

    _PL_get_arg(1, prop, arg);

    if ( pname == ATOM_base )
    { atom_t mname;
      int rc;

      if ( !PL_get_atom_ex(arg, &mname) )
	return FALSE;
      LOCK();
      rc = setSuperModule(m, _lookupModule(mname));
      UNLOCK();
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
      } else
	return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_module_class, arg);
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
stripModule() takes an atom or term, possible embedded in the :/2 module
term.  It will assign *module with the associated module and return  the
remaining term.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Word
stripModule(Word term, Module *module ARG_LD)
{ deRef(term);

  while( hasFunctor(*term, FUNCTOR_colon2) )
  { Word mp;
    mp = argTermP(*term, 0);
    deRef(mp);
    if ( !isTextAtom(*mp) )
      break;
    *module = lookupModule(*mp);
    term = argTermP(*term, 1);
    deRef(term);
  }

  if ( ! *module )
    *module = (environment_frame ? contextModule(environment_frame)
				 : MODULE_user);

  return term;
}

bool
isPublicModule(Module module, Procedure proc)
{ if ( lookupHTable(module->public,
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

  LOCK();
  rval = delSuperModule(me, super);
  UNLOCK();

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
  Symbol symb;
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

  while( (symb = advanceTableEnum(e)) )
  { Module m = symb->value;
    atom_t f = ( !m->file ? ATOM_nil : m->file->name);

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
  term_t plain = PL_new_term_ref();

  PL_strip_module(A1, &m, plain);
  if ( PL_unify_atom(A2, m->name) &&
       PL_unify(A3, plain) )
    succeed;

  fail;
}


word
pl_module(term_t old, term_t new)
{ GET_LD

  if ( PL_unify_atom(old, LD->modules.typein->name) )
  { atom_t name;

    if ( !PL_get_atom(new, &name) )
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_module, new);

    LD->modules.typein = lookupModule(name);
    succeed;
  }

  fail;
}


word
pl_set_source_module(term_t old, term_t new)
{ GET_LD

  if ( PL_unify_atom(old, LD->modules.source->name) )
  { atom_t name;

    if ( !PL_get_atom(new, &name) )
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_module, new);

    LD->modules.source = lookupModule(name);
    succeed;
  }

  fail;
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


static int
find_modules_with_def(Module m, functor_t fdef,
		      term_t h, term_t t,
		      int l ARG_LD)
{ Procedure proc;
  ListCell c;

  DEBUG(9, Sdprintf("Trying %s\n", PL_atom_chars(m->name)));

  if ( l < 0 )
  { sysError("OOPS loop in default modules???\n");
    fail;
  }

  if ( (proc = isCurrentProcedure(fdef, m)) &&
       proc->definition->impl.any )
  { if ( !(PL_unify_list(t, h, t) &&
	   PL_unify_atom(h, m->name)) )
      fail;
  }

  for(c = m->supers; c; c=c->next)
  { Module s = c->value;

    if ( !find_modules_with_def(s, fdef, h, t, l-1 PASS_LD) )
      fail;
  }

  succeed;
}


/** '$def_modules'(:PI, -Modules) is det.

Modules is unified with a list of modules   that define PI and appear in
the import modules of the original module.  Search starts in the current
source module if PI is not qualified.  Only   modules  in which PI has a
real definition are returned  (i.e.,  _not_   modules  where  PI is only
defined as dynamic or multifile.

@see	boot/expand.pl uses this to find relevant modules that define
	term_expansion/2 and/or goal_expansion/2 definitions.
*/

static
PRED_IMPL("$def_modules", 2, def_modules, PL_FA_TRANSPARENT)
{ PRED_LD
  Module m = LD->modules.source;
  functor_t fdef;
  term_t head = PL_new_term_ref();
  term_t tail = PL_copy_term_ref(A2);

  if ( !get_functor(A1, &fdef, &m, 0, GF_PROCEDURE) )
    fail;

  if ( !find_modules_with_def(m, fdef, head, tail, 100 PASS_LD) )
    fail;

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
  Module module;
  term_t tmp = 0, rdef = 0, rtail = 0;

  LOCK();
  module = _lookupModule(name);
  if ( class )
    module->class = class;

  if ( !allow_newfile && module->file && module->file != sf)
  { term_t obj;
    char msg[256];
    UNLOCK();

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

  for_table(module->procedures, s,
	    { Procedure proc = s->value;
	      Definition def = proc->definition;
	      if ( /*def->module == module &&*/
		   !true(def, DYNAMIC|MULTIFILE|FOREIGN) )
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
  if ( super )
    setSuperModule(module, _lookupModule(super));

  UNLOCK();

  if ( rdef )
  { if ( !PL_unify_nil(rtail) )
      return FALSE;

    printMessage(ATOM_warning,
		 PL_FUNCTOR_CHARS, "declare_module", 2,
		   PL_ATOM, name,
		   PL_FUNCTOR_CHARS, "abolish", 1,
		     PL_TERM, rdef);
  }

  succeed;
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
  for_table(module->public, s,
	    { if ( !PL_unify_list(list, head, list) ||
		   !unify_functor(head, (functor_t)s->name, GP_NAMEARITY) )
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

  if ( !get_module(A1, &m, FALSE) )
    fail;

  if ( !PL_get_arg(1, A2, a) )
    return PL_error(NULL, 0, NULL, ERR_TYPE,
		    ATOM_module_property, A2);

  if ( PL_is_functor(A2, FUNCTOR_line_count1) )
  { if ( m->line_no > 0 )
      return PL_unify_integer(a, m->line_no);
    else
      fail;
  } else if ( PL_is_functor(A2, FUNCTOR_file1) )
  { if ( m->file )
      return PL_unify_atom(a, m->file->name);
    else
      fail;
  } else if ( PL_is_functor(A2, FUNCTOR_exports1) )
  { return unify_export_list(a, m PASS_LD);
  } else if ( PL_is_functor(A2, FUNCTOR_class1) )
  { return PL_unify_atom(a, m->class);
  } else
    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    ATOM_module_property, A2);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
export/1 exports a procedure specified by its name and arity or
head from the context module.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
export_pi(term_t pi, Module module ARG_LD)
{ functor_t fd;
  Procedure proc;

  PL_strip_module(pi, &module, pi);

  if ( PL_is_functor(pi, FUNCTOR_comma2) )
  { term_t a1 = PL_new_term_ref();
    term_t a2 = PL_new_term_ref();

    _PL_get_arg(1, pi, a1);
    _PL_get_arg(2, pi, a2);

    TRY(export_pi(a1, module PASS_LD));
    return export_pi(a2, module PASS_LD);
  }


  if ( !get_functor(pi, &fd, &module, 0, GF_PROCEDURE) )
    fail;

  if ( (proc = isStaticSystemProcedure(fd)) && true(proc->definition, P_ISO) )
    succeed;
  proc = lookupProcedure(fd, module);

  LOCKMODULE(module);
  addHTable(module->public,
	    (void *)proc->definition->functor->functor,
	    proc);
  UNLOCKMODULE(module);

  succeed;
}



static
PRED_IMPL("export", 1, export, PL_FA_TRANSPARENT)
{ PRED_LD
  Module module = NULL;

  return export_pi(A1, module PASS_LD);
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
  Symbol symb;
  term_t tail = PL_copy_term_ref(A2);
  term_t head = PL_new_term_ref();

  if ( !PL_get_atom_ex(A1, &mname) )
    return FALSE;
  if ( !(module = isCurrentModule(mname)) )
    return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_module, A1);

  e = newTableEnum(module->public);

  while( (symb = advanceTableEnum(e)) )
  { Procedure proc = (Procedure) symb->value;
    Definition def = proc->definition;
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

  for_unlocked_table(m->procedures, s,
		     { Procedure proc = s->value;

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
{ LOCK();
  for_unlocked_table(GD->tables.modules, s,
		     fixExportModule(s->value, old, new));
  UNLOCK();
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

  proc = lookupProcedure(fd, source);

  if ( !isDefinedProcedure(proc) )
    autoImport(proc->definition->functor->functor, proc->definition->module);

  if ( (old = isCurrentProcedure(proc->definition->functor->functor,
				 destination)) )
  { if ( old->definition == proc->definition )
      succeed;			/* already done this! */

    if ( !isDefinedProcedure(old) )
    { Definition odef = old->definition;

      old->definition = proc->definition;
      shareDefinition(proc->definition);
      if ( odef->shared > 1 )
	fixExport(odef, proc->definition);
      shareDefinition(odef);
      GC_LINGER(odef);
      set(old, pflags);

      succeed;
    }

    if ( old->definition->module == destination )
    { if ( (pflags & PROC_WEAK) )
      { if ( truePrologFlag(PLFLAG_WARN_OVERRIDE_IMPLICIT_IMPORT) )
	{ term_t pi = PL_new_term_ref();

	  if ( !PL_unify_predicate(pi, proc, GP_NAMEARITY) )
	    return FALSE;

	  printMessage(ATOM_warning,
		       PL_FUNCTOR_CHARS, "ignored_weak_import", 2,
		         PL_ATOM, destination->name,
		         PL_TERM, pi);
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
    printMessage(ATOM_warning,
		 PL_FUNCTOR_CHARS, "import_private", 2,
		   PL_ATOM, destination->name,
		   PL_TERM, pi);
  }

  { Procedure nproc = (Procedure)  allocHeapOrHalt(sizeof(struct procedure));

    nproc->flags = pflags;
    nproc->definition = proc->definition;
    shareDefinition(proc->definition);

    LOCKMODULE(destination);
    addHTable(destination->procedures,
	      (void *)proc->definition->functor->functor, nproc);
    UNLOCKMODULE(destination);
  }

  succeed;
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
EndPredDefs
