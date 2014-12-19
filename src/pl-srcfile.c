/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

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

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "pl-dbref.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Source administration. The core object is  SourceFile, which keeps track
of procedures that are defined by it.  Source files are identified by an
unsigned short, which is registered with clauses and procedures.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define LOCK()   PL_LOCK(L_SRCFILE)
#define UNLOCK() PL_UNLOCK(L_SRCFILE)
#undef LD
#define LD LOCAL_LD


static void
putSourceFileArray(size_t where, SourceFile sf)
{ int idx = MSB(where);

  if ( !GD->files.array.blocks[idx] )
  { PL_LOCK(L_MISC);
    if ( !GD->files.array.blocks[idx] )
    { size_t bs = (size_t)1<<idx;
      SourceFile *newblock;

      if ( !(newblock=PL_malloc_uncollectable(bs*sizeof(SourceFile))) )
	outOfCore();

      memset(newblock, 0, bs*sizeof(SourceFile));
      GD->files.array.blocks[idx] = newblock-bs;
    }
    PL_UNLOCK(L_MISC);
  }

  GD->files.array.blocks[idx][where] = sf;
}


static void
registerSourceFile(SourceFile sf)	/* locked by lookupSourceFile() */
{ size_t index;
  int i;
  int last = FALSE;

  if ( GD->files.no_hole_before == 0 )
    GD->files.no_hole_before = 1;

  for(index=GD->files.no_hole_before, i=MSB(index); !last; i++)
  { size_t upto = (size_t)2<<i;
    SourceFile *b = GD->files.array.blocks[i];

    if ( upto >= GD->files.highest )
    { upto = GD->files.highest;
      last = TRUE;
    }

    for(; index<upto; index++)
    { if ( b[index] == NULL )
      { sf->index = index;
	b[index] = sf;
	GD->files.no_hole_before = index+1;

	return;
      }
    }
  }

  GD->files.no_hole_before = index+1;
  sf->index = index;
  if ( (size_t)sf->index != index )
    fatalError("Too many (%d) source files", index);
  putSourceFileArray(index, sf);
  GD->files.highest = index+1;
}


size_t
highSourceFileIndex(void)
{ return GD->files.highest;
}


SourceFile
indexToSourceFile(int index)
{ if ( index > 0 && index < GD->files.highest )
  { int idx = MSB(index);

    return GD->files.array.blocks[idx][index];
  }

  return NULL;
}


static void
freeList(ListCell *lp)
{ ListCell c;

  if ( (c=*lp) )
  { ListCell n;

    *lp = NULL;
    for( ; c; c=n )
    { n = c->next;

      freeHeap(c, sizeof(*c));
    }
  }
}


static void
unallocSourceFile(SourceFile sf)
{ freeList(&sf->procedures);
  freeList(&sf->modules);
  if ( sf->mutex )
    freeSimpleMutex(sf->mutex);
  freeHeap(sf, sizeof(*sf));
}


static void
freeSymbolSourceFile(Symbol s)
{ SourceFile sf = s->value;

  unallocSourceFile(sf);
}


static void
cleanupSourceFileArray(void)
{ int i;
  SourceFile *ap0;

  for(i=0; (ap0=GD->files.array.blocks[i]); i++)
  { size_t bs = (size_t)1<<i;

    ap0 += bs;
    GD->files.array.blocks[i] = NULL;
    PL_free(ap0);
  }
}


void
cleanupSourceFiles(void)
{ Table t;

  if ( (t=GD->files.table) )
  { GD->files.table = NULL;

    destroyHTable(t);
  }

  cleanupSourceFileArray();
}


static bool
clearSourceAdmin(SourceFile sf)
{ GET_LD
  int rc = FALSE;

  fid_t fid = PL_open_foreign_frame();
  term_t name = PL_new_term_ref();
  static predicate_t pred = NULL;

  if ( !pred )
    pred = PL_predicate("$clear_source_admin", 1, "system");

  PL_put_atom(name, sf->name);
  rc = PL_call_predicate(MODULE_system, PL_Q_NORMAL, pred, name);

  PL_discard_foreign_frame(fid);

  return rc;
}


int
destroySourceFile(SourceFile sf)
{ Symbol s;

  DEBUG(MSG_SRCFILE,
	Sdprintf("Destroying source file %s\n", PL_atom_chars(sf->name)));

  clearSourceAdmin(sf);

  LOCK();
  s = lookupHTable(GD->files.table, (void*)sf->name);
  assert(s);
  deleteSymbolHTable(GD->files.table, s);
  PL_unregister_atom(sf->name);
  putSourceFileArray(sf->index, NULL);
  if ( GD->files.no_hole_before > sf->index )
    GD->files.no_hole_before = sf->index;
  UNLOCK();

  unallocSourceFile(sf);

  return TRUE;
}


static SourceFile
lookupSourceFile_unlocked(atom_t name, int create)
{ SourceFile file;
  Symbol s;

  if ( !GD->files.table )
  { GD->files.table = newHTable(32);
    GD->files.table->free_symbol = freeSymbolSourceFile;
    GD->files.no_hole_before = 1;
  }

  if ( (s=lookupHTable(GD->files.table, (void*)name)) )
  { file = s->value;
  } else if ( create )
  { file = allocHeapOrHalt(sizeof(*file));
    memset(file, 0, sizeof(*file));
    file->name = name;
    file->system = GD->bootsession;
#ifdef O_PLMT
    file->mutex = allocSimpleMutex(PL_atom_chars(name));
#endif
    PL_register_atom(file->name);
    registerSourceFile(file);

    addHTable(GD->files.table, (void*)name, file);
  } else
  { file = NULL;
  }

  return file;
}


SourceFile
lookupSourceFile(atom_t name, int create)
{ SourceFile sf;

  LOCK();
  sf = lookupSourceFile_unlocked(name, create);
  UNLOCK();

  return sf;
}


int
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
addProcedureSourceFile(SourceFile, Procedure) associates a  procedure to
a source file. Note that  a  procedure   may  be  associated to multiple
source files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
addProcedureSourceFile(SourceFile sf, Procedure proc)
{ LOCKSRCFILE(sf);
  if ( !hasProcedureSourceFile(sf, proc) )
  { ListCell cell;

    cell = allocHeapOrHalt(sizeof(struct list_cell));
    cell->value = proc;
    cell->next = sf->procedures;
    sf->procedures = cell;
    set(proc->definition, FILE_ASSIGNED);
    if ( !proc->source_no )
      proc->source_no = sf->index;
    else
      set(proc, PROC_MULTISOURCE);
  }
  UNLOCKSRCFILE(sf);
}


/* Add a module to the source file.  Note that we must add additional
   modules at the end because '$already_loaded'/4 assumes that the first
   module is the primary module of the file.
*/

int
addModuleSourceFile(SourceFile sf, Module m)
{ ListCell *cp, c2;
  int rc = TRUE;

  LOCKSRCFILE(sf);
  for(cp=&sf->modules; *cp; cp = &(*cp)->next)
  { ListCell cell = *cp;

    if ( cell->value == m )
      goto out;
  }

  if ( !(c2 = allocHeap(sizeof(struct list_cell))) )
  { rc = FALSE;			/* no memory */
    goto out;
  }
  c2->value = m;
  c2->next = NULL;
  *cp = c2;

out:
  UNLOCKSRCFILE(sf);
  return rc;
}


static int
delModuleSourceFile(SourceFile sf, Module m)
{ ListCell *cp, c;
  int rc = FALSE;

  LOCKSRCFILE(sf);
  for(cp=&sf->modules; (c=*cp); cp=&c->next)
  { if ( c->value == m )
    { *cp = c->next;
      freeHeap(c, sizeof(*c));

      rc = TRUE;
      break;
    }
  }
  UNLOCKSRCFILE(sf);

  return rc;
}


static void					/* requires LOCKSRCFILE(sf) */
delAllModulesSourceFile__unlocked(SourceFile sf)
{ ListCell c = sf->modules, n;

  sf->modules = NULL;

  for(; c; c = n)
  { Module m = c->value;

    n = c->next;
    if ( m->file == sf )
    { PL_LOCK(L_MODULE);
      m->file = NULL;
      m->line_no = 0;
      clearHTable(m->public);
      PL_UNLOCK(L_MODULE);
    }

    freeHeap(c, sizeof(*c));
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Remove all links from sf to  m  module.   If  sf  becomes empty, we also
delete the source file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
unlinkSourceFileModule(SourceFile sf, Module m)
{ ListCell cell, next, prev = NULL;

  LOCKSRCFILE(sf);

  DEBUG(MSG_DESTROY_MODULE,
	Sdprintf("Cleaning %s\n", PL_atom_chars(sf->name)));

  for(cell=sf->procedures; cell; cell=next)
  { Procedure proc;

    next = cell->next;
    proc = cell->value;

    if ( lookupHTable(m->procedures, (void*)proc->definition->functor->functor) )
    { if ( prev )
	prev->next = cell->next;
      else
	sf->procedures = cell->next;
      freeHeap(cell, sizeof(*cell));
    } else
      prev = cell;
  }

  UNLOCKSRCFILE(sf);

  if ( !sf->procedures && !sf->modules )
    destroySourceFile(sf);
}


static
PRED_IMPL("$make_system_source_files", 0, make_system_source_files, 0)
{ int i, n;

  LOCK();
  n = highSourceFileIndex();
  for(i=1; i<n; i++)
  { SourceFile f = indexToSourceFile(i);

    if ( f )
      f->system = TRUE;
  }
  UNLOCK();

  return TRUE;
}


/** '$source_file'(+Head, -File) is semidet.
*/

static
PRED_IMPL("$source_file", 2, source_file, 0)
{ PRED_LD
  Procedure proc;
  SourceFile sf;

  term_t descr = A1;
  term_t file  = A2;

  if ( get_procedure(descr, &proc, 0, GP_FIND) )
  { if ( isDefinedProcedure(proc) &&
	 (sf = indexToSourceFile(proc->source_no)) &&
	 sf->count > 0 )
      return PL_unify_atom(file, sf->name);
  }

  return FALSE;
}

/** '$source_file_predicates'(+File, -Heads:list(callable)) is semidet.
*/

static
PRED_IMPL("$source_file_predicates", 2, source_file_predicates, 0)
{ PRED_LD
  atom_t name;
  int rc = TRUE;
  SourceFile sf;

  term_t file = A1;

  LOCK();
  if ( PL_get_atom_ex(file, &name) &&
       (sf = lookupSourceFile_unlocked(name, FALSE)) &&
       sf->count > 0 )
  { term_t tail = PL_copy_term_ref(A2);
    term_t head = PL_new_term_ref();
    ListCell cell;

    LOCKSRCFILE(sf);
    for(cell=sf->procedures; rc && cell; cell = cell->next )
    { Procedure proc = cell->value;
      Definition def = proc->definition;

      rc = ( PL_unify_list(tail, head, tail) &&
	     unify_definition(MODULE_user, head, def, 0, GP_QUALIFY)
	   );
    }
    rc = (rc && PL_unify_nil(tail));
    UNLOCKSRCFILE(sf);
  } else
    rc = FALSE;
  UNLOCK();

  return rc;
}


static
PRED_IMPL("$time_source_file", 3, time_source_file, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  int index;
  int mx = highSourceFileIndex();
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
  { SourceFile f = indexToSourceFile(index);

    if ( f == NULL || f->count == 0 )
      continue;

    if ( PL_unify_atom(file, f->name) &&
	 PL_unify_float(time, f->mtime) &&
	 PL_unify_atom(type, f->system ? ATOM_system : ATOM_user) )
    { PL_close_foreign_frame(fid);
      ForeignRedoInt(index+1);
    }

    PL_rewind_foreign_frame(fid);
  }

  PL_close_foreign_frame(fid);
  fail;
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

  delayEvents();

  LOCKSRCFILE(sf);
  PL_LOCK(L_PREDICATE);
  PL_LOCK(L_THREAD);
  PL_LOCK(L_STOPTHEWORLD);
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

    DEBUG(MSG_UNLOAD, Sdprintf("removeClausesProcedure(%s), refs = %d\n",
			       predicateName(def), def->references));

    if ( false(def, P_FOREIGN) )
    { deleted = removeClausesProcedure(proc,
				       true(def, P_MULTIFILE) ? sf->index : 0,
				       TRUE);
    } else
      deleted = 0;

    DEBUG(MSG_UNLOAD,
	  if ( false(def, P_MULTIFILE) && def->impl.clauses.number_of_clauses )
	    Sdprintf("%s: %d clauses after unload\n",
		     predicateName(def), def->impl.clauses.number_of_clauses));

    if ( deleted )
    { if ( false(def, P_MULTIFILE|P_DYNAMIC) )
	clearTriedIndexes(def);

      if ( def->references == 0 )
      { freeCodesDefinition(def, FALSE);
	garbage = cleanDefinition(def, garbage);
      } else if ( false(def, P_DYNAMIC) )
      { registerDirtyDefinition(def);
	freeCodesDefinition(def, TRUE);
      }
    }

    if ( false(def, P_MULTIFILE) )
    { clear(def, FILE_ASSIGNED);
      clear_meta_declaration(def);
    }
  }

				      /* unmark the marked predicates */
  for(cell = sf->procedures; cell; cell = cell->next)
  { Procedure proc = cell->value;
    Definition def = proc->definition;

    if ( false(def, P_DYNAMIC) && def->references )
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
  delAllModulesSourceFile__unlocked(sf);

  unblockSignals(&set);
  PL_UNLOCK(L_STOPTHEWORLD);
  PL_UNLOCK(L_THREAD);
  PL_UNLOCK(L_PREDICATE);
  UNLOCKSRCFILE(sf);

  sendDelayedEvents();
  if ( garbage )
    freeClauseList(garbage);

  return TRUE;
}


/** '$unload_file'(+Name) is det.

Remove all traces of a loaded file.
*/

static
PRED_IMPL("$unload_file", 1, unload_file, 0)
{ PRED_LD
  SourceFile sf;
  atom_t name;

  if ( !PL_get_atom_ex(A1, &name) )
    return FALSE;

  if ( (sf = lookupSourceFile(name, FALSE)) )
  { ListCell mc, mcn;

    if ( sf->system )
      return PL_error(NULL, 0, NULL, ERR_PERMISSION,
		      ATOM_unload, ATOM_file, A1);

    if ( !unloadFile(sf) )
      return FALSE;

    for(mc=sf->modules; mc; mc=mcn)
    { Module m = mc->value;

      mcn = mc->next;
      LOCKMODULE(m);
      m->file = NULL;
      m->line_no = 0;
      delModuleSourceFile(sf, m);
      clearHTable(m->public);
      setSuperModule(m, MODULE_user);
      UNLOCKMODULE(m);
    }

    destroySourceFile(sf);
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


/** '$start_consult'(+Id, +Modified) is det.
*/

static
PRED_IMPL("$start_consult", 2, start_consult, 0)
{ PRED_LD
  atom_t name;
  double time;

  term_t file = A1;
  term_t modified = A2;

  if ( PL_get_atom_ex(file, &name) &&
       PL_get_float_ex(modified, &time) )
  { SourceFile f = lookupSourceFile(name, TRUE);

    f->mtime = time;
    startConsult(f);

    return TRUE;
  }

  return FALSE;
}


/** '$clause_from_source'(+File, +Line, -Clause) is semidet.

True when Clause is the clause that contains Line in File.
*/

static
PRED_IMPL("$clause_from_source", 3, clause_from_source, 0)
{ PRED_LD
  atom_t name;
  SourceFile sf;
  int ln;
  ListCell cell;
  Clause c = NULL;

  term_t file = A1;
  term_t line = A2;
  term_t clause = A3;

  if ( !PL_get_atom_ex(file, &name) ||
       !(sf = lookupSourceFile(name, FALSE)) ||
       !PL_get_integer_ex(line, &ln) )
    fail;

  LOCKSRCFILE(sf);
  for(cell = sf->procedures; cell; cell = cell->next)
  { Procedure proc = cell->value;
    Definition def = proc->definition;

    if ( def && false(def, P_FOREIGN) )
    { ClauseRef cref = def->impl.clauses.first_clause;

      for( ; cref; cref = cref->next )
      { Clause cl = cref->value.clause;

	if ( cl->source_no == sf->index )
	{ if ( ln >= (int)cl->line_no )
	  { if ( !c || c->line_no < cl->line_no )
	      c = cl;
	  }
	}
      }
    }
  }
  UNLOCKSRCFILE(sf);

  if ( c )
    return PL_unify_clref(clause, c);

  fail;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(srcfile)
  PRED_DEF("$source_file", 2, source_file, 0)
  PRED_DEF("$source_file_predicates", 2, source_file_predicates, 0)
  PRED_DEF("$time_source_file", 3, time_source_file, PL_FA_NONDETERMINISTIC)
  PRED_DEF("$clause_from_source", 3, clause_from_source, 0)
  PRED_DEF("$unload_file", 1, unload_file, 0)
  PRED_DEF("$start_consult", 2, start_consult, 0)
  PRED_DEF("$make_system_source_files", 0, make_system_source_files, 0)
EndPredDefs
