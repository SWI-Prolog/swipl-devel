/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include "pl-incl.h"
#define LOCK()   PL_LOCK(L_MODULE)
#define UNLOCK() PL_UNLOCK(L_MODULE)

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

static Module
_lookupModule(atom_t name)
{ Symbol s;
  Module m;

  if ((s = lookupHTable(GD->tables.modules, (void*)name)) != (Symbol) NULL)
    return (Module) s->value;

  m = allocHeap(sizeof(struct module));
  m->name = name;
  m->file = (SourceFile) NULL;
  m->operators = NULL;
  clearFlags(m);
  set(m, CHARESCAPE|UNKNOWN_ERROR);

  if ( name == ATOM_user || name == ATOM_system )
    m->procedures = newHTable(PROCEDUREHASHSIZE);
  else
    m->procedures = newHTable(MODULEPROCEDUREHASHSIZE);

  m->public = newHTable(PUBLICHASHSIZE);

  if ( name == ATOM_user || stringAtom(name)[0] == '$' )
    m->super = MODULE_system;
  else if ( name == ATOM_system )
    m->super = NULL;
  else
    m->super = MODULE_user;

  if ( name == ATOM_system || stringAtom(name)[0] == '$' )
    set(m, SYSTEM);

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
{ static int done = FALSE;

  LOCK();
  if ( !done )
  { done = TRUE;

    initTables();
    initFunctors();

    GD->tables.modules = newHTable(MODULEHASHSIZE);
    GD->modules.system = _lookupModule(ATOM_system);
    GD->modules.user   = _lookupModule(ATOM_user);
    LD->modules.typein = MODULE_user;
    LD->modules.source = MODULE_user;
  }
  UNLOCK();
}

int
isSuperModule(Module s, Module m)
{ while(m)
  { if ( m == s )
      succeed;
    m = m->super;
  }

  fail;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stripModule() takes an atom or term, possible embedded in the :/2 module
term.  It will assing *module with the associated module and return  the
remaining term.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Word
stripModule(Word term, Module *module)
{ deRef(term);

  while( hasFunctor(*term, FUNCTOR_module2) )
  { Word mp;
    mp = argTermP(*term, 0);
    deRef(mp);
    if ( !isAtom(*mp) )
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

word
pl_default_module(term_t me, term_t old, term_t new)
{ Module m, s;
  atom_t a;

  if ( PL_is_variable(me) )
  { m = contextModule(environment_frame);
    TRY(PL_unify_atom(me, m->name));
  } else if ( PL_get_atom(me, &a) )
  { m = lookupModule(a);
  } else
    return warning("super_module/2: instantiation fault");

  TRY(PL_unify_atom(old, m->super ? m->super->name : ATOM_nil));

  if ( !PL_get_atom(new, &a) )
    return warning("super_module/2: instantiation fault");

  s = (a == ATOM_nil ? NULL : lookupModule(a));
  m->super = s;

  succeed;
}


word
pl_current_module(term_t module, term_t file, word h)
{ TableEnum e = NULL;
  Symbol symb;
  atom_t name;

  if ( ForeignControl(h) == FRG_CUTTED )
  { e = ForeignContextPtr(h);
    freeTableEnum(e);
    succeed;
  }
					/* deterministic cases */
  if ( PL_get_atom(module, &name) )
  { Module m;

    if ( (m=isCurrentModule(name)) )
    { atom_t f = (!m->file ? ATOM_nil : m->file->name);
      return PL_unify_atom(file, f);
    }

    fail;
  } else if ( PL_get_atom(file, &name) )
  { int rval = FALSE;
    for_table(GD->tables.modules, s,
	      { Module m = s->value;

		if ( m->file && m->file->name == name )
		{ rval = PL_unify_atom(module, m->name);
		  break;
		}
	      })
    return rval;
  }

  switch(ForeignControl(h))
  { case FRG_FIRST_CALL:
      e = newTableEnum(GD->tables.modules);
      break;
    case FRG_REDO:
      e = ForeignContextPtr(h);
      break;
    default:
      assert(0);
  }

  while( (symb = advanceTableEnum(e)) )
  { Module m = symb->value;

    if ( stringAtom(m->name)[0] == '$' &&
	 !SYSTEM_MODE && PL_is_variable(module) )
      continue;

    { fid_t cid = PL_open_foreign_frame();
      atom_t f = ( !m->file ? ATOM_nil : m->file->name);

      if ( PL_unify_atom(module, m->name) &&
	   PL_unify_atom(file, f) )
      { ForeignRedoPtr(e);
      }

      PL_discard_foreign_frame(cid);
    }
  }

  freeTableEnum(e);
  fail;
}


word
pl_strip_module(term_t spec, term_t module, term_t term)
{ Module m = (Module) NULL;
  term_t plain = PL_new_term_ref();

  PL_strip_module(spec, &m, plain);
  if ( PL_unify_atom(module, m->name) &&
       PL_unify(term, plain) )
    succeed;

  fail;
}  


word
pl_module(term_t old, term_t new)
{ if ( PL_unify_atom(old, LD->modules.typein->name) )
  { atom_t name;

    if ( !PL_get_atom(new, &name) )
      return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		      PL_new_atom("module"), /* ATOM_module = ":" */
		      new);

    LD->modules.typein = lookupModule(name);
    succeed;
  }

  fail;
}


word
pl_set_source_module(term_t old, term_t new)
{ if ( PL_unify_atom(old, LD->modules.source->name) )
  { atom_t name;

    if ( !PL_get_atom(new, &name) )
      return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		      PL_new_atom("module"), /* ATOM_module = ":" */
		      new);

    LD->modules.source = lookupModule(name);
    succeed;
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the module in which to call   term_expansion/2. This is the current
source-module and module user, provide term_expansion/2 is defined. Note
this predicate does not generate modules for which there is a definition
that has no clauses. The predicate would fail anyhow.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static word
expansion_module(term_t name, functor_t func, word h)
{ Module m = LD->modules.source;
  Procedure proc;

  switch(ForeignControl(h))
  { case FRG_FIRST_CALL:
      m = LD->modules.source;
      break;
    case FRG_REDO:
      m = MODULE_user;
      break;
    default:
      succeed;
  }

  while(1)
  { if ( (proc = isCurrentProcedure(func, m)) &&
	 proc->definition->definition.clauses &&
	 PL_unify_atom(name, LD->modules.source->name) )
    { if ( m == MODULE_user )
	PL_succeed;
      else
	ForeignRedoInt(1);
    } else
    { if ( m == MODULE_user )
	PL_fail;
      m = MODULE_user;
    }
  }

  PL_fail;				/* should not get here */
}


word
pl_term_expansion_module(term_t name, word h)
{ return expansion_module(name, FUNCTOR_term_expansion2, h);
}


word
pl_goal_expansion_module(term_t name, word h)
{ return expansion_module(name, FUNCTOR_goal_expansion2, h);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Declare `name' to be a module with `file' as its source  file.   If  the
module was already loaded its public table is cleared and all procedures
in it are abolished.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
declareModule(atom_t name, SourceFile sf)
{ Module module;

  module = lookupModule(name);
  LOCK();
  if ( module->file && module->file != sf)
  { warning("module/2: module %s already loaded from file %s (abandoned)", 
	    stringAtom(module->name), 
	    stringAtom(module->file->name));
    fail;
  }
	    
  module->file = sf;
  LD->modules.source = module;

  for_table(module->procedures, s,
	    { Procedure proc = s->value;
	      Definition def = proc->definition;
	      if ( def->module == module &&
		   !true(def, DYNAMIC|MULTIFILE|FOREIGN) )
		abolishProcedure(proc, module);
	    })
  clearHTable(module->public);
  UNLOCK();
  
  succeed;
}


word
pl_declare_module(term_t name, term_t file)
{ SourceFile sf;
  atom_t mname, fname;

  if ( !PL_get_atom(name, &mname) ||
       !PL_get_atom(file, &fname) )
    return warning("$declare_module/2: instantiation fault");

  sf = lookupSourceFile(fname);
  return declareModule(mname, sf);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
export_list(+Module, -PublicPreds)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_export_list(term_t modulename, term_t public)
{ Module module;
  atom_t mname;

  if ( !PL_get_atom(modulename, &mname) )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    PL_new_atom("module"), /* ATOM_module = ":" */
		    modulename);
  
  if ( !(module = isCurrentModule(mname)) )
    fail;
  
  { term_t head = PL_new_term_ref();
    term_t list = PL_copy_term_ref(public);
    int rval = TRUE;

    LOCK();
    for_table(module->public, s,
	      { if ( !PL_unify_list(list, head, list) ||
		     !PL_unify_functor(head, (functor_t)s->name) )
		{ rval = FALSE;
		  break;
		}
	      })
    UNLOCK();
    if ( rval )
      return PL_unify_nil(list);

    fail;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pl_export() exports a procedure specified by its name and arity from the
context module.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_export(term_t pred)
{ Module module = NULL;
  term_t head = PL_new_term_ref();
  functor_t fd;

  PL_strip_module(pred, &module, head);
  if ( PL_get_functor(head, &fd) )
  { Procedure proc;

    LOCK();
    proc = lookupProcedure(fd, module);
    addHTable(module->public,
	      (void *)proc->definition->functor->functor,
	      proc);
    UNLOCK();
    succeed;
  }

  return warning("export/1: illegal predicate specification");
}

word
pl_check_export()
{ Module module = contextModule(environment_frame);

  for_table(module->public, s,
	    { Procedure proc = (Procedure) s->value;
	      Definition def = proc->definition;

	      if ( !isDefinedProcedure(proc) )
	      { warning("Exported procedure %s:%s/%d is not defined", 
			stringAtom(module->name), 
			stringAtom(def->functor->name), 
			def->functor->arity);
	      }
	    })

  succeed;
}

word
pl_context_module(term_t module)
{ return PL_unify_atom(module, contextModule(environment_frame)->name);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pl_import() imports the predicate specified with its argument  into  the
current  context  module.   If  the  predicate is already defined in the
context a warning is displayed and the predicate is  NOT  imported.   If
the  predicate  is  not  on  the  public  list of the exporting module a
warning is displayed, but the predicate is imported nevertheless.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_import(term_t pred)
{ Module source = NULL;
  Module destination = contextModule(environment_frame);
  term_t head = PL_new_term_ref();
  functor_t fd;
  Procedure proc, old;

  PL_strip_module(pred, &source, head);
  if ( !PL_get_functor(head, &fd) )
    return warning("import/1: instantiation fault");
  proc = lookupProcedure(fd, source);

  if ( !isDefinedProcedure(proc) )
    autoImport(proc->definition->functor->functor, proc->definition->module);

  if ( (old = isCurrentProcedure(proc->definition->functor->functor,
				 destination)) )
  { if ( old->definition == proc->definition )
      succeed;			/* already done this! */

    if ( !isDefinedProcedure(old) )
    { old->definition = proc->definition;

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
  
    LOCK();
    addHTable(destination->procedures,
	      (void *)proc->definition->functor->functor, nproc);
    UNLOCK();
  }

  succeed;
}


