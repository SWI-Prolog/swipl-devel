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

static void	addSuperModule(Module m, Module s, int where);


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
#ifdef O_PROLOG_HOOK
  m->hook = NULL;
#endif
#ifdef O_PLMT
  m->mutex = allocSimpleMutex(PL_atom_chars(m->name));
#endif
  clearFlags(m);
  set(m, CHARESCAPE|UNKNOWN_ERROR);

  if ( name == ATOM_user || name == ATOM_system )
    m->procedures = newHTable(PROCEDUREHASHSIZE);
  else
    m->procedures = newHTable(MODULEPROCEDUREHASHSIZE);

  m->public = newHTable(PUBLICHASHSIZE);

  m->supers = NULL;
  if ( name == ATOM_user )
  { super = MODULE_system;
  } else if ( name == ATOM_system )
  { set(m, SYSTEM);
    super = NULL;
  } else if ( stringAtom(name)[0] == '$' )
  { set(m, SYSTEM);
    super = MODULE_system;
  } else
  { super = MODULE_user;
  }

  if ( super )
  { addSuperModule(m, super, 'A');
    m->level = super->level+1;		/* TBD: check usage as this is */
  } else				/* no longer unique */
    m->level = 0;

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

static void
addSuperModule(Module m, Module s, int where)
{ GET_LD
  ListCell c;

  for(c=m->supers; c; c=c->next)
  { if ( c->value == s )
      return;				/* already a super-module */
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

      succeed;
    }
  }

  fail;
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
{ atom_t name;

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
      i = ForeignContextInt(PL__ctx);
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
{ Module me, super;
  atom_t where;

  if ( !get_module(A1, &me, TRUE) ||
       !get_module(A2, &super, TRUE) ||
       !PL_get_atom_ex(A3, &where) )
    fail;

  LOCK();
  addSuperModule(me, super, where == ATOM_start ? 'A' : 'Z');
  UNLOCK();

  succeed;
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


word
pl_current_module(term_t module, term_t file, control_t h)
{ GET_LD
  TableEnum e = NULL;
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
  { if ( !unify_definition(old, m->hook->definition, 0, GP_HIDESYSTEM) )
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the module in which to call   term_expansion/2. This is the current
source-module and module user, provide term_expansion/2 is defined. Note
this predicate does not generate modules for which there is a definition
that has no clauses. The predicate would fail anyhow.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static word
expansion_module(term_t name, functor_t func, control_t h ARG_LD)
{ Module m;
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
	 PL_unify_atom(name, m->name) )
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


static
PRED_IMPL("$term_expansion_module", 1, term_expansion_module,
	  PL_FA_NONDETERMINISTIC)
{ PRED_LD

  return expansion_module(A1, FUNCTOR_term_expansion2,
			  PL__ctx PASS_LD);
}


static
PRED_IMPL("$goal_expansion_module", 1, goal_expansion_module,
	  PL_FA_NONDETERMINISTIC)
{ PRED_LD

  return expansion_module(A1, FUNCTOR_goal_expansion2,
			  PL__ctx PASS_LD);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Declare `name' to be a module with `file' as its source  file.   If  the
module was already loaded its public table is cleared and all procedures
in it are abolished.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
declareModule(atom_t name, SourceFile sf, int line)
{ GET_LD
  Module module;
  term_t tmp = 0, rdef = 0, rtail = 0;

  LOCK();
  module = _lookupModule(name);

  if ( module->file && module->file != sf)
  { term_t obj;
    char msg[256];
    UNLOCK();

    obj = PL_new_term_ref();
    PL_put_atom(obj, name);
    Ssprintf(msg, "Alread loaded from %s",
	     atom_summary(module->file->name, 100));
    return PL_error("module", 2, msg, ERR_PERMISSION,
		    ATOM_redefine, ATOM_module, obj);
  }
	    
  module->file = sf;
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
		  unify_definition(tmp, def, 0, GP_NAMEARITY);
		}
		abolishProcedure(proc, module);
	      }
	    })
  clearHTable(module->public);
  UNLOCK();
  
  if ( rdef )
  { PL_unify_nil(rtail);

    printMessage(ATOM_warning,
		 PL_FUNCTOR_CHARS, "declare_module", 2,
		   PL_ATOM, name,
		   PL_FUNCTOR_CHARS, "abolish", 1,
		     PL_TERM, rdef);
  }

  succeed;
}


word
pl_declare_module(term_t name, term_t file, term_t line)
{ SourceFile sf;
  atom_t mname, fname;
  int line_no;

  if ( !PL_get_atom_ex(name, &mname) ||
       !PL_get_atom_ex(file, &fname) ||
       !PL_get_integer_ex(line, &line_no) )
    fail;

  sf = lookupSourceFile(fname);
  return declareModule(mname, sf, line_no);
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
  } else
    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    ATOM_module_property, A2);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
export_list(+Module, -PublicPreds)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_export_list(term_t modulename, term_t public)
{ Module module;
  atom_t mname;

  if ( !PL_get_atom_ex(modulename, &mname) )
    fail;
  
  if ( !(module = isCurrentModule(mname)) )
    fail;
  
  { GET_LD

    term_t head = PL_new_term_ref();
    term_t list = PL_copy_term_ref(public);
    int rval = TRUE;

    LOCKMODULE(module);
    for_table(module->public, s,
	      { if ( !PL_unify_list(list, head, list) ||
		     !PL_unify_functor(head, (functor_t)s->name) )
		{ rval = FALSE;
		  break;
		}
	      })
    UNLOCKMODULE(module);
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
{ GET_LD
  Module module = NULL;
  term_t head = PL_new_term_ref();
  functor_t fd;

  PL_strip_module(pred, &module, head);
  if ( PL_get_functor(head, &fd) )
  { Procedure proc;

    if ( (proc = isStaticSystemProcedure(fd)) )
      return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
		      ATOM_export, ATOM_built_in_procedure, proc->definition);
    proc = lookupProcedure(fd, module);

    LOCKMODULE(module);
    addHTable(module->public,
	      (void *)proc->definition->functor->functor,
	      proc);
    UNLOCKMODULE(module);
    succeed;
  }

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, pred);
}

word
pl_check_export()
{ GET_LD
  Module module = contextModule(environment_frame);

  for_table(module->public, s,
	    { Procedure proc = (Procedure) s->value;
	      Definition def = proc->definition;

	      if ( !isDefinedProcedure(proc) && /* not defined */
		   proc->definition->module == module ) /* not imported */
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
  PRED_DEF("$term_expansion_module", 1, term_expansion_module,
	   PL_FA_NONDETERMINISTIC)
  PRED_DEF("$goal_expansion_module", 1, goal_expansion_module,
	   PL_FA_NONDETERMINISTIC)
  PRED_DEF("import_module", 2, import_module,
	   PL_FA_NONDETERMINISTIC)
  PRED_DEF("add_import_module", 3, add_import_module, 0)
  PRED_DEF("delete_import_module", 2, delete_import_module, 0)
  PRED_DEF("$module_property", 2, module_property, 0)
  PRED_DEF("strip_module", 3, strip_module, PL_FA_TRANSPARENT)
EndPredDefs
