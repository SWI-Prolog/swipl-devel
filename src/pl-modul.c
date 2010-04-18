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

static int addSuperModule_no_lock(Module m, Module s, int where);


static Module
_lookupModule(atom_t name)
{ Symbol s;
  Module m, super;

  if ((s = lookupHTable(GD->tables.modules, (void*)name)) != (Symbol) NULL)
    return (Module) s->value;

  { GET_LD
    m = allocHeap(sizeof(struct module));
  }
  m->name = name;
  m->file = (SourceFile) NULL;
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

  m->public = newHTable(PUBLICHASHSIZE);

  m->supers = NULL;
  if ( name == ATOM_user )
  { super = MODULE_system;
  } else if ( name == ATOM_system )
  { set(m, SYSTEM|UNKNOWN_ERROR);
    super = NULL;
  } else if ( stringAtom(name)[0] == '$' )
  { set(m, SYSTEM);
    super = MODULE_system;
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
    GD->modules.system = _lookupModule(ATOM_system);
    GD->modules.user   = _lookupModule(ATOM_user);
  }
  UNLOCK();
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
{ GET_LD
  ListCell c;

  if ( reachableModule(s, m) )
    return cannotSetSuperModule(m, s);

  for(c=m->supers; c; c=c->next)
  { if ( c->value == s )
      return TRUE;			/* already a super-module */
  }

  c = allocHeap(sizeof(*c));
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
{ GET_LD
  ListCell *p;

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
clearSupersModule(Module m)
{ GET_LD
  ListCell c = m->supers;
  ListCell next;

  m->supers = NULL;
  for(; c; c=next)
  { next = c->next;
    freeHeap(c, sizeof(*c));
  }

  m->level = 0;
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
  clearSupersModule(m);

  return addSuperModule_no_lock(m, s, 'A');
}


static
PRED_IMPL("set_base_module", 1, set_base_module, PL_FA_TRANSPARENT)
{ PRED_LD
  Module m = MODULE_parse;
  atom_t mname;
  int rc;

  PL_strip_module(A1, &m, A1);
  if ( !PL_get_atom_ex(A1, &mname) )
    fail;

  LOCK();
  rc = setSuperModule(m, _lookupModule(mname));
  UNLOCK();

  return rc;
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
term.  It will assing *module with the associated module and return  the
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


Module
moduleFromFile(SourceFile sf)
{ TableEnum e;
  Symbol symb;

  for(e = newTableEnum(GD->tables.modules),
      symb = advanceTableEnum(e);
      symb;
      symb = advanceTableEnum(e))
  { Module m = symb->value;

    if ( m->file == sf )
    { freeTableEnum(e);
      return m;
    }
  }

  freeTableEnum(e);
  return NULL;
}


word
pl_current_module(term_t module, term_t file, control_t h)
{ GET_LD
  TableEnum e = NULL;
  Symbol symb;
  atom_t name;
  SourceFile sf = NULL;

  if ( ForeignControl(h) == FRG_CUTTED )
  { e = ForeignContextPtr(h);
    freeTableEnum(e);
    succeed;
  }
				/* deterministic case: module --> file */
  if ( PL_get_atom(module, &name) )
  { Module m;

    if ( (m=isCurrentModule(name)) )
    { atom_t f = (!m->file ? ATOM_nil : m->file->name);
      return PL_unify_atom(file, f);
    }

    fail;
  }

  switch(ForeignControl(h))
  { case FRG_FIRST_CALL:
      if ( !get_existing_source_file(file, &sf PASS_LD) )
	fail;				/* given, but non-existing file */
      e = newTableEnum(GD->tables.modules);
      break;
    case FRG_REDO:
      e = ForeignContextPtr(h);
      get_existing_source_file(file, &sf PASS_LD);
      break;
    case FRG_CUTTED:
      freeTableEnum(e);
      succeed;
  }

  while( (symb = advanceTableEnum(e)) )
  { Module m = symb->value;

    if ( stringAtom(m->name)[0] == '$' &&
	 !SYSTEM_MODE && PL_is_variable(module) )
      continue;

    { fid_t cid = PL_open_foreign_frame();
      atom_t f = ( !m->file ? ATOM_nil : m->file->name);

      if ( (!sf || (m->file == sf)) &&
	   PL_unify_atom(module, m->name) &&
	   PL_unify_atom(file, f) )
      { PL_close_foreign_frame(cid);

	if ( sf && sf->module_count == 1 )
	{ freeTableEnum(e);
	  succeed;
	}
	ForeignRedoPtr(e);
      }

      PL_discard_foreign_frame(cid);
    }
  }

  freeTableEnum(e);
  fail;
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
       proc->definition->definition.clauses )
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
declareModule(atom_t name, atom_t super,
	      SourceFile sf, int line,
	      int allow_newfile)
{ GET_LD
  Module module;
  term_t tmp = 0, rdef = 0, rtail = 0;

  LOCK();
  module = _lookupModule(name);

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
    sf->module_count++;		/* current determinism in $current_module/2 */
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


/** '$declare_module'(+Module, +Super, +File, +Line, +Redefine) is det.

Start a new (source-)module

@param	Module is the name of the module to declare
@param	File is the canonical name of the file from which the module
	is loaded
@param  Line is the line-number of the :- module/2 directive.
@param	Redefine If =true=, allow associating the module to a new file
*/

static
PRED_IMPL("$declare_module", 5, declare_module, 0)
{ PRED_LD
  SourceFile sf;
  atom_t mname, sname, fname;
  int line_no, rdef;

  term_t module   = A1;
  term_t super    = A2;
  term_t file     = A3;
  term_t line     = A4;
  term_t redefine = A5;

  if ( !PL_get_atom_ex(module, &mname) ||
       !PL_get_atom_ex(super, &sname) ||
       !PL_get_atom_ex(file, &fname) ||
       !PL_get_integer_ex(line, &line_no) ||
       !PL_get_bool_ex(redefine, &rdef) )
    fail;

  sf = lookupSourceFile(fname, TRUE);
  return declareModule(mname, sname, sf, line_no, rdef);
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
  { return PL_unify_integer(a, m->line_no);
  } else if ( PL_is_functor(A2, FUNCTOR_file1) )
  { if ( m->file )
      return PL_unify_atom(a, m->file->name);
    else
      fail;
  } else if ( PL_is_functor(A2, FUNCTOR_exports1) )
  { return unify_export_list(a, m PASS_LD);
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


word
pl_import(term_t pred)
{ GET_LD
  Module source = NULL;
  Module destination = contextModule(environment_frame);
  functor_t fd;
  Procedure proc, old;

  if ( !get_functor(pred, &fd, &source, 0, GF_PROCEDURE) )
    fail;

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
      if ( true(odef, P_SHARED) )
	fixExport(odef, proc->definition);
      set(proc->definition, P_SHARED);

      succeed;
    }

    if ( old->definition->module == destination )
      return warning("Cannot import %s into module %s: name clash",
		     procedureName(proc),
		     stringAtom(destination->name) );

    if ( old->definition->module != source )
    { warning("Cannot import %s into module %s: already imported from %s",
	      procedureName(proc),
	      stringAtom(destination->name),
	      stringAtom(old->definition->module->name) );
      fail;
    }

    sysError("Unknown problem importing %s into module %s",
	     procedureName(proc),
	     stringAtom(destination->name));
    fail;
  }

  if ( !isPublicModule(source, proc) )
  { warning("import/1: %s is not declared public (still imported)",
	    procedureName(proc));
  }

  { Procedure nproc = (Procedure)  allocHeap(sizeof(struct procedure));

    nproc->type = PROCEDURE_TYPE;
    nproc->definition = proc->definition;
    set(proc->definition, P_SHARED);

    LOCKMODULE(destination);
    addHTable(destination->procedures,
	      (void *)proc->definition->functor->functor, nproc);
    UNLOCKMODULE(destination);
  }

  succeed;
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(module)
  PRED_DEF("import_module", 2, import_module,
	   PL_FA_NONDETERMINISTIC)
  PRED_DEF("$def_modules", 2, def_modules, PL_FA_TRANSPARENT)
  PRED_DEF("$declare_module", 5, declare_module, 0)
  PRED_DEF("add_import_module", 3, add_import_module, 0)
  PRED_DEF("delete_import_module", 2, delete_import_module, 0)
  PRED_DEF("set_base_module", 1, set_base_module, PL_FA_TRANSPARENT)
  PRED_DEF("$module_property", 2, module_property, 0)
  PRED_DEF("strip_module", 3, strip_module, PL_FA_TRANSPARENT)
  PRED_DEF("export", 1, export, PL_FA_TRANSPARENT)
  PRED_DEF("$undefined_export", 2, undefined_export, 0)
EndPredDefs
