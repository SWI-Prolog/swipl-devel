/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: module management
*/

#include "pl-incl.h"

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

Module
lookupModule(Atom name)
{ Symbol s;
  Module m;

  if ((s = lookupHTable(moduleTable, name)) != (Symbol) NULL)
    return (Module) s->value;

  m = (Module) allocHeap(sizeof(struct module));
  m->name = name;
  m->file = (SourceFile) NULL;
  clearFlags(m);
  set(m, UNKNOWN);

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

  addHTable(moduleTable, name, m);
  statistics.modules++;
  
  return m;
}


static Module
isCurrentModule(Atom name)
{ Symbol s;
  
  if ((s = lookupHTable(moduleTable, name)) != (Symbol) NULL)
    return (Module) s->value;

  return (Module) NULL;
}


void
initModules(void)
{ moduleTable    = newHTable(MODULEHASHSIZE);
  modules.system = lookupModule(ATOM_system);
  modules.user   = lookupModule(ATOM_user);
  modules.typein = modules.user;
  modules.source = modules.user;
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

  while(isTerm(*term) && functorTerm(*term) == FUNCTOR_module2)
  { Word mp;
    mp = argTermP(*term, 0);
    deRef(mp);
    if ( !isAtom(*mp) )
      break;
    *module = lookupModule((Atom) *mp);
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
{ return lookupHTable(module->public, proc->definition->functor) ? TRUE
								 : FALSE;
}


		/********************************
		*       PROLOG CONNECTION       *
		*********************************/

word
pl_default_module(term_t me, term_t old, term_t new)
{ Module m, s;
  Atom a;

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
{ Symbol symb = firstHTable(moduleTable);
  Atom name;

  if ( ForeignControl(h) == FRG_CUTTED )
    succeed;

					/* deterministic cases */
  if ( PL_get_atom(module, &name) )
  { for(; symb; symb = nextHTable(moduleTable, symb) )
    { Module m = (Module) symb->value;

      if ( name == m->name )
      { Atom f = (m->file == (SourceFile) NULL ? ATOM_nil : m->file->name);
	return PL_unify_atom(file, f);
      }
    }

    fail;
  } else if ( PL_get_atom(file, &name) )
  { for( ; symb; symb = nextHTable(moduleTable, symb) )
    { Module m = (Module) symb->value;

      if ( m->file && m->file->name == name )
	return PL_unify_atom(module, m->name);
    }

    fail;
  }

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      break;
    case FRG_REDO:
      symb = (Symbol) ForeignContextAddress(h);
      break;
    default:
      assert(0);
  }

  for( ; symb; symb = nextHTable(moduleTable, symb) )
  { Module m = (Module) symb->value;

    if ( stringAtom(m->name)[0] == '$' &&
	 !SYSTEM_MODE && PL_is_variable(module) )
      continue;

    { fid_t cid = PL_open_foreign_frame();
      Atom f = (m->file == (SourceFile) NULL ? ATOM_nil : m->file->name);

      if ( PL_unify_atom(module, m->name) &&
	   PL_unify_atom(file, f) )
      { if ( !(symb = nextHTable(moduleTable, symb)) )
	  succeed;

	ForeignRedo(symb);
      }

      PL_discard_foreign_frame(cid);
    }
  }

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
{ if ( PL_unify_atom(old, modules.typein->name) )
  { Atom name;

    if ( !PL_get_atom(new, &name) )
      return warning("module/2: argument should be an atom");

    modules.typein = lookupModule(name);
    succeed;
  }

  fail;
}


word
pl_set_source_module(term_t old, term_t new)
{ if ( PL_unify_atom(old, modules.source->name) )
  { Atom name;

    if ( !PL_get_atom(new, &name) )
      return warning("$source_module/2: argument should be an atom");

    modules.source = lookupModule(name);
    succeed;
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Declare `name' to be a module with `file' as its source  file.   If  the
module was already loaded its public table is cleared and all procedures
in it are abolished.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
declareModule(Atom name, SourceFile sf)
{ Module module = lookupModule(name);
  Symbol s;

  if ( module->file && module->file != sf)
  { warning("module/2: module %s already loaded from file %s (abandoned)", 
	    stringAtom(module->name), 
	    stringAtom(module->file->name));
    fail;
  }
	    
  module->file = sf;
  modules.source = module;

  for_table(s, module->procedures)
  { Procedure proc = (Procedure) s->value;
    Definition def = proc->definition;
    if ( def->module == module &&
	 false(def, DYNAMIC) &&
	 false(def, MULTIFILE) )
      abolishProcedure(proc, module);
  }
  clearHTable(module->public);
  
  succeed;
}


word
pl_declare_module(term_t name, term_t file)
{ SourceFile sf;
  Atom mname, fname;

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
  Atom mname;
  Symbol s;

  if ( !PL_get_atom(modulename, &mname) )
    return warning("export_list/2: instantiation fault");
  
  if ( !(module = isCurrentModule(mname)) )
    fail;
  
  { term_t head = PL_new_term_ref();
    term_t list = PL_copy_term_ref(public);

    for_table(s, module->public)
    { if ( !PL_unify_list(list, head, list) ||
	   !PL_unify_functor(head, (FunctorDef)s->name) )
	fail;
    }

    return PL_unify_nil(list);
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
  FunctorDef fd;

  PL_strip_module(pred, &module, head);
  if ( PL_get_functor(head, &fd) )
  { Procedure proc = lookupProcedure(fd, module);

    addHTable(module->public, proc->definition->functor, proc);
    succeed;
  }

  return warning("export/1: illegal predicate specification");
}

word
pl_check_export()
{ Module module = contextModule(environment_frame);
  Symbol s;

  for_table(s, module->public)
  { Procedure proc = (Procedure) s->value;
    Definition def = proc->definition;

    if (isDefinedProcedure(proc) == FALSE)
    { warning("Exported procedure %s:%s/%d is not defined", 
				  stringAtom(module->name), 
				  stringAtom(def->functor->name), 
				  def->functor->arity);
    }
  }

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
  FunctorDef fd;
  Procedure proc, old;

  PL_strip_module(pred, &source, head);
  if ( !PL_get_functor(head, &fd) )
    return warning("import/1: instantiation fault");
  proc = lookupProcedure(fd, source);

  if ( !isDefinedProcedure(proc) )
    autoImport(proc->definition->functor, proc->definition->module);

  if ( (old = isCurrentProcedure(proc->definition->functor, destination)) )
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
  
    addHTable(destination->procedures, proc->definition->functor, nproc);
  }

  succeed;
}
