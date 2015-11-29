/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2015, VU University Amsterdam

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
{ if ( sf->magic == SF_MAGIC_DESTROYING )
  { sf->magic = 0;
    freeList(&sf->procedures);
    freeList(&sf->modules);
#ifdef O_PLMT
    if ( sf->mutex )
      freeSimpleMutex(sf->mutex);
#endif
    freeHeap(sf, sizeof(*sf));
  }
}


static void
freeSymbolSourceFile(void *name, void *value)
{ SourceFile sf = value;

  if ( sf->magic == SF_MAGIC )
    sf->magic = SF_MAGIC_DESTROYING;
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

  if ( sf->magic == SF_MAGIC )
  { fid_t fid = PL_open_foreign_frame();
    term_t name = PL_new_term_ref();
    static predicate_t pred = NULL;

    if ( !pred )
      pred = PL_predicate("$clear_source_admin", 1, "system");

    PL_put_atom(name, sf->name);
    rc = PL_call_predicate(MODULE_system, PL_Q_NORMAL, pred, name);

    PL_discard_foreign_frame(fid);
  }

  return rc;
}


int
destroySourceFile(SourceFile sf)
{ GET_LD

  DEBUG(MSG_SRCFILE,
	Sdprintf("Destroying source file %s\n", PL_atom_chars(sf->name)));

  clearSourceAdmin(sf);

  LOCK();
  if ( sf->magic == SF_MAGIC )
  { SourceFile f;

    sf->magic = SF_MAGIC_DESTROYING;
    f = lookupHTable(GD->files.table, (void*)sf->name);
    assert(f);
    deleteHTable(GD->files.table, (void*)sf->name);
    PL_unregister_atom(sf->name);
    putSourceFileArray(sf->index, NULL);
    if ( GD->files.no_hole_before > sf->index )
      GD->files.no_hole_before = sf->index;
  }
  UNLOCK();

  unallocSourceFile(sf);

  return TRUE;
}


static SourceFile
lookupSourceFile_unlocked(atom_t name, int create)
{ GET_LD
  SourceFile file;

  if ( !GD->files.table )
  { GD->files.table = newHTable(32);
    GD->files.table->free_symbol = freeSymbolSourceFile;
    GD->files.no_hole_before = 1;
  }

  if ( (file=lookupHTable(GD->files.table, (void*)name)) )
  { ;
  } else if ( create )
  { file = allocHeapOrHalt(sizeof(*file));
    memset(file, 0, sizeof(*file));
    file->name = name;
    file->system = GD->bootsession;
#ifdef O_PLMT
    file->mutex = allocSimpleMutex(PL_atom_chars(name));
#endif
    PL_register_atom(file->name);
    file->magic = SF_MAGIC;
    registerSourceFile(file);

    addNewHTable(GD->files.table, (void*)name, file);
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
{ GET_LD
  ListCell cell, next, prev = NULL;

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
{ ListCell cell, next;
  size_t deleted = 0;

  delayEvents();
  LOCKSRCFILE(sf);
				      /* remove the clauses */
  for(cell = sf->procedures; cell; cell = cell->next)
  { Procedure proc = cell->value;
    Definition def = proc->definition;

    if ( false(def, P_FOREIGN) )
    { deleted += removeClausesProcedure(proc,
					true(def, P_MULTIFILE) ? sf->index : 0,
					TRUE);
    }

    DEBUG(MSG_UNLOAD,
	  if ( false(def, P_MULTIFILE) && def->impl.clauses.number_of_clauses )
	    Sdprintf("%s: %d clauses after unload\n",
		     predicateName(def), def->impl.clauses.number_of_clauses));

    if ( false(def, P_MULTIFILE) )
    { clear(def, FILE_ASSIGNED);
      clear_meta_declaration(def);
    }
  }
  DEBUG(MSG_UNLOAD, Sdprintf("Removed %ld clauses\n", (long)deleted));

				      /* cleanup the procedure list */
  for(cell = sf->procedures; cell; cell = next)
  { next = cell->next;
    freeHeap(cell, sizeof(struct list_cell));
  }
  sf->procedures = NULL;

  delAllModulesSourceFile__unlocked(sf);
  UNLOCKSRCFILE(sf);

  sendDelayedEvents();
  pl_garbage_collect_clauses();

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


		 /*******************************
		 *	    RECONSULT		*
		 *******************************/

static void	fix_discontiguous(p_reload *r);
static void	fix_metapredicate(p_reload *r);

static int
startReconsultFile(SourceFile sf)
{ GET_LD
  sf_reload *r;

  DEBUG(MSG_RECONSULT, Sdprintf("Reconsult %s ...\n", sourceFileName(sf)));

  if ( (r = allocHeap(sizeof(*sf->reload))) )
  { memset(r, 0, sizeof(*r));
    r->procedures        = newHTable(16);
    r->reload_gen        = GEN_MAX-PL_thread_self();
    r->pred_access_count = popNPredicateAccess(0);
    sf->reload = r;

    return TRUE;
  }

  return PL_no_memory();
}


static ClauseRef
find_clause(ClauseRef cref, gen_t generation)
{ for(; cref; cref = cref->next)
  { if ( visibleClause(cref->value.clause, generation) )
      break;
  }

  return cref;
}


static void
advance_clause(p_reload *r ARG_LD)
{ ClauseRef cref;

  if ( (cref = r->current_clause) )
  { acquire_def(r->predicate);
    for(cref = cref->next; cref; cref = cref->next)
    { if ( visibleClause(cref->value.clause, r->generation) )
	break;
    }
    release_def(r->predicate);
    r->current_clause = cref;
  }
}


static void
copy_clause_source(Clause dest, Clause src)
{ dest->source_no = src->source_no;
  dest->owner_no  = src->owner_no;
  dest->line_no   = src->line_no;
}


static ClauseRef
keep_clause(p_reload *r, Clause clause ARG_LD)
{ ClauseRef cref = r->current_clause;
  Clause keep = cref->value.clause;

  copy_clause_source(keep, clause);
  freeClauseSilent(clause);
  advance_clause(r PASS_LD);

  return cref;
}


static int
equal_clause(Clause cl1, Clause cl2)
{ if ( cl1->code_size == cl2->code_size )
  { size_t bytes = (size_t)cl1->code_size * sizeof(code);

    return memcmp(cl1->codes, cl2->codes, bytes) == 0;
  }

  return FALSE;
}


int
reloadIsDefined(SourceFile sf, Procedure proc ARG_LD)
{ p_reload *reload;

  if ( sf->reload && (reload=lookupHTable(sf->reload->procedures, proc)) )
  { return ( true(reload, PROC_DEFINED) ||
	     reload->number_of_clauses > 0 );
  }

  return FALSE;
}


int
isDefinedProcedureSource(Procedure proc)
{ GET_LD

  if ( ReadingSource )
  { SourceFile sf = lookupSourceFile(source_file_name, FALSE);

    if ( sf && sf == indexToSourceFile(proc->source_no) && sf->reload )
      return reloadIsDefined(sf, proc PASS_LD);
  }

  return isDefinedProcedure(proc);
}


static p_reload *
reloadContext(SourceFile sf, Procedure proc ARG_LD)
{ p_reload *reload;

  if ( !(reload = lookupHTable(sf->reload->procedures, proc)) )
  { Definition def = proc->definition;

    if ( !(reload = allocHeap(sizeof(*reload))) )
    { PL_no_memory();
      return NULL;
    }
    memset(reload, 0, sizeof(*reload));
    reload->predicate = def;
    if ( true(def, P_THREAD_LOCAL|P_FOREIGN) )
    { set(reload, P_NO_CLAUSES);
    } else if ( isDefinedProcedure(proc) )
    { reload->generation = GD->generation;
      pushPredicateAccess(def, reload->generation);
      acquire_def(def);
      reload->current_clause = find_clause(def->impl.clauses.first_clause,
					   reload->generation);
      release_def(def);
    } else
    { set(reload, P_NEW);
    }
    addNewHTable(sf->reload->procedures, proc, reload);
    DEBUG(MSG_RECONSULT_PRED,
	  Sdprintf("%s %s ...\n",
		   true(reload, P_NEW)        ? "New"   :
		   true(reload, P_NO_CLAUSES) ? "Alien" :
					        "Reload",
		   predicateName(def)));
  }

  return reload;
}


ClauseRef
assertProcedureSource(SourceFile sf, Procedure proc, Clause clause ARG_LD)
{ if ( sf && sf->reload )
  { p_reload *reload;
    Definition def = proc->definition;
    ClauseRef cref;

    assert(proc == sf->current_procedure);

    if ( !(reload = reloadContext(sf, proc PASS_LD)) )
    { freeClauseSilent(clause);
      return NULL;
    }

    if ( reload->number_of_clauses++ == 0 )
      fix_discontiguous(reload);

    if ( true(reload, P_NEW|P_NO_CLAUSES) )
      return assertProcedure(proc, clause, CL_END PASS_LD);

    if ( (cref = reload->current_clause) )
    { ClauseRef cref2;

      if ( equal_clause(cref->value.clause, clause) )
      { DEBUG(MSG_RECONSULT_CLAUSE,
	      Sdprintf("  Keeping clause %d\n",
		       clauseNo(def, cref->value.clause, reload->generation)));
	return keep_clause(reload, clause PASS_LD);
      }

      set(reload, P_MODIFIED);

      acquire_def(def);
      for(cref2 = cref->next; cref2; cref2 = cref2->next)
      { Clause c2 = cref2->value.clause;

	if ( !visibleClause(c2, reload->generation) )
	  continue;
	if ( true(def, P_MULTIFILE) && c2->owner_no != sf->index )
	  continue;

	if ( equal_clause(c2, clause) )
	{ ClauseRef del;

	  for(del = cref; del != cref2; del = del->next)
	  { Clause c = del->value.clause;

	    if ( !visibleClause(c, reload->generation) ||
		 true(c, CL_ERASED) )
	      continue;
	    if ( true(def, P_MULTIFILE) && c->owner_no != sf->index )
	      continue;

	    c->generation.erased = sf->reload->reload_gen;
	    DEBUG(MSG_RECONSULT_CLAUSE,
		  Sdprintf("  Deleted clause %d\n",
			   clauseNo(def, c, reload->generation)));
	  }
	  release_def(def);

	  reload->current_clause = cref2;
	  DEBUG(MSG_RECONSULT_CLAUSE,
		Sdprintf("  Keeping clause %d\n",
			 clauseNo(def, cref2->value.clause, reload->generation)));
	  return keep_clause(reload, clause PASS_LD);
	}
      }
      release_def(def);

      DEBUG(MSG_RECONSULT_CLAUSE,
	    Sdprintf("  Inserted before clause %d\n",
		     clauseNo(def, cref->value.clause, reload->generation)));
      if ( (cref2 = assertProcedure(proc, clause, cref PASS_LD)) )
	cref2->value.clause->generation.created = sf->reload->reload_gen;

      return cref2;
    } else
    { if ( (cref = assertProcedure(proc, clause, CL_END PASS_LD)) )
	cref->value.clause->generation.created = sf->reload->reload_gen;
      DEBUG(MSG_RECONSULT_CLAUSE, Sdprintf("  Added at the end\n"));

      set(reload, P_MODIFIED);

      return cref;
    }
  }

  return assertProcedure(proc, clause, CL_END PASS_LD);
}


static void
associateSource(SourceFile sf, Procedure proc)
{ Definition def = proc->definition;

  if ( false(def, FILE_ASSIGNED) )
  { GET_LD

    DEBUG(2, Sdprintf("Associating %s to %s (%p)\n",
		      predicateName(def), PL_atom_chars(source_file_name),
		      def));
    addProcedureSourceFile(sf, proc);

    if ( SYSTEM_MODE )
    { set(def, P_LOCKED|HIDE_CHILDS);
    } else
    { if ( truePrologFlag(PLFLAG_DEBUGINFO) )
	clear(def, HIDE_CHILDS);
      else
	set(def, HIDE_CHILDS);
    }
  }
}


#define P_ATEND	(P_VOLATILE|P_PUBLIC|P_ISO|P_NOPROFILE|P_NON_TERMINAL)

int
setAttrProcedureSource(SourceFile sf, Procedure proc,
		       unsigned attr, int val ARG_LD)
{ if ( val && (attr&&PROC_DEFINED) )
    associateSource(sf, proc);

  if ( sf->reload )
  { p_reload *reload;

    if ( !(reload = reloadContext(sf, proc PASS_LD)) )
      return FALSE;

    if ( val )
      set(reload, attr);
    else
      clear(reload, attr);

    if ( (attr&(P_ATEND|P_TRANSPARENT)) )
      return TRUE;
  }

  return setAttrDefinition(proc->definition, attr, val);
}


static void
fix_attributes(SourceFile sf, Definition def, p_reload *r ARG_LD)
{ if ( false(def, P_MULTIFILE) )
  { def->flags &= ~P_ATEND;
    def->flags |= (r->flags&P_ATEND);

    if ( true(def, P_DYNAMIC) && false(r, P_DYNAMIC) )
      setDynamicDefinition(def, FALSE);
    if ( true(def, P_THREAD_LOCAL) && false(r, P_THREAD_LOCAL) )
    { if ( !setThreadLocalDefinition(def, FALSE) )
      { term_t ex = PL_exception(0);

	printMessage(ATOM_error, ex);
	PL_clear_exception();
      }
    }
  } else
  { def->flags |= (r->flags&P_ATEND);
  }

  fix_metapredicate(r);
}


static void
fix_discontiguous(p_reload *r)
{ Definition def = r->predicate;

  if ( true(def, P_DISCONTIGUOUS) && false(r, P_DISCONTIGUOUS) )
    clear(def, P_DISCONTIGUOUS);
}


int
setMetapredicateSource(SourceFile sf, Procedure proc,
		       meta_mask mask ARG_LD)
{ associateSource(sf, proc);

  if ( sf->reload )
  { p_reload *reload;

    if ( !(reload = reloadContext(sf, proc PASS_LD)) )
      return FALSE;

    reload->meta_info = mask;
    if ( isTransparentMetamask(proc->definition, mask) )
      set(reload, P_TRANSPARENT);
    else
      clear(reload, P_TRANSPARENT);
    set(reload, P_META);
  } else
  { setMetapredicateMask(proc->definition, mask);
  }

  return TRUE;
}


static void
fix_metapredicate(p_reload *r)
{ Definition def = r->predicate;

  if ( false(def, P_MULTIFILE) )
  { int mfmask = (P_META|P_TRANSPARENT);

    if ( (def->flags&mfmask) != (r->flags&mfmask) ||
	 def->meta_info != r->meta_info )
    { if ( true(def, P_META) && false(r, P_META) )
	clear_meta_declaration(def);
      else if ( true(r, P_META) )
	setMetapredicateMask(def, r->meta_info);
      clear(def, P_TRANSPARENT);
      set(def, r->flags&P_TRANSPARENT);

      freeCodesDefinition(def, FALSE);
    }
  } else if ( true(r, P_META) )
  { setMetapredicateMask(def, r->meta_info);
    freeCodesDefinition(def, FALSE);
  } else if ( true(r, P_TRANSPARENT) )
  { set(def, P_TRANSPARENT);
  }
}


void
registerReloadModule(SourceFile sf, Module module)
{ GET_LD
  m_reload *r;

  if ( sf->reload )
  { Table mt;

    if ( !(mt=sf->reload->modules) )
      mt = sf->reload->modules = newHTable(8);

    if ( !(r=lookupHTable(mt, module)) )
    { r = allocHeapOrHalt(sizeof(*r));
      memset(r, 0, sizeof(*r));
      addNewHTable(mt, module, r);
    }
  }
}


int
exportProcedureSource(SourceFile sf, Module module, Procedure proc)
{ GET_LD
  m_reload *r;

  if ( sf->reload && sf->reload->modules &&
       (r = lookupHTable(sf->reload->modules, module)) )
  { if ( !r->public )
      r->public = newHTable(8);
    updateHTable(r->public,
		 (void *)proc->definition->functor->functor,
		 proc);
  }

  return exportProcedure(module, proc);
}


static void
fix_module(Module m, m_reload *r)
{ GET_LD

  LOCKMODULE(m);
  for_table(m->public, n, v,
	    { if ( !r->public ||
		   !lookupHTable(r->public, n) )
	      { DEBUG(MSG_RECONSULT_MODULE,
		      Sdprintf("Delete export %s\n",
			       procedureName(v)));
		deleteHTable(m->public, n);
	      }
	    });
  UNLOCKMODULE(m);
}


static void
delete_old_predicates(SourceFile sf)
{ GET_LD
  ListCell cell, prev = NULL, next;
  size_t deleted = 0;

  for(cell = sf->procedures; cell; cell = next)
  { Procedure proc = cell->value;
    Definition def = proc->definition;

    next = cell->next;

    if ( false(def, P_FOREIGN) &&
	 !lookupHTable(sf->reload->procedures, proc) )
    { deleted += removeClausesProcedure(proc,
					true(def, P_MULTIFILE) ? sf->index : 0,
					TRUE);

      if ( false(def, P_MULTIFILE) )
      { clear(def, FILE_ASSIGNED);
	clear_meta_declaration(def);
	freeCodesDefinition(def, TRUE);
      }

      DEBUG(MSG_RECONSULT_PRED,
	    Sdprintf("Deleted %d clauses from predicate %s\n",
		     (long)deleted, predicateName(def)));

      if ( prev )
	prev->next = cell->next;
      else
	sf->procedures = cell->next;
    } else
    { prev = cell;
    }
  }
}


static void
delete_pending_clauses(SourceFile sf, Definition def, p_reload *r ARG_LD)
{ ClauseRef cref;
  sf_reload *rl = sf->reload;

  acquire_def(def);
  for(cref = r->current_clause; cref; cref = cref->next)
  { Clause c = cref->value.clause;

    if ( !visibleClause(c, r->generation) ||
	 true(c, CL_ERASED) )
      continue;
    if ( true(r->predicate, P_MULTIFILE) && c->owner_no != sf->index )
      continue;

    c->generation.erased = rl->reload_gen;
    DEBUG(MSG_RECONSULT_CLAUSE,
	  Sdprintf("  Deleted clause %d\n",
		   clauseNo(def, c, r->generation)));
  }
  release_def(def);
}


static int
endReconsult(SourceFile sf)
{ GET_LD
  sf_reload *reload;

  if ( (reload=sf->reload) )
  { size_t accessed_preds = reload->procedures->size;

    delete_old_predicates(sf);

    for_table(reload->procedures, n, v,
	      { Procedure proc = n;
		p_reload *r = v;

		if ( false(r, P_NEW|P_NO_CLAUSES) )
		{ Definition def = proc->definition;

		  delete_pending_clauses(sf, def, r PASS_LD);
		  fix_attributes(sf, def, r PASS_LD);
		  reconsultFinalizePredicate(reload, def, r PASS_LD);
		} else
		{ accessed_preds--;
		  if ( true(r, P_NO_CLAUSES) )
		  { Definition def = proc->definition;
		    fix_attributes(sf, def, r PASS_LD);
		  }
		}
		freeHeap(r, sizeof(*r));
	      });

    popNPredicateAccess(accessed_preds);
    assert(reload->pred_access_count == popNPredicateAccess(0));
    destroyHTable(reload->procedures);

    if ( reload->modules )
    { for_table(reload->modules, n, v,
		{ Module m = n;
		  m_reload *r = v;

		  fix_module(m, r);
		  if ( r->public )
		    destroyHTable(r->public);
		  freeHeap(r, sizeof(*r));
		});
      destroyHTable(reload->modules);
    }

    sf->reload = NULL;
    freeHeap(reload, sizeof(*reload));

    pl_garbage_collect_clauses();
  }

  return TRUE;
}


		 /*******************************
		 *	      CONSULT		*
		 *******************************/

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

int
startConsult(SourceFile f)
{ if ( f->count++ > 0 )			/* This is a re-consult */
  { if ( !startReconsultFile(f) )
      return FALSE;
  }

  f->current_procedure = NULL;
  return TRUE;
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


static
PRED_IMPL("$end_consult", 1, end_consult, 0)
{ PRED_LD
  atom_t name;

  if ( PL_get_atom_ex(A1, &name) )
  { SourceFile sf;

    if ( (sf=lookupSourceFile(name, FALSE)) )
    { sf->current_procedure = NULL;
      return endReconsult(sf);
    }

    return TRUE;
  }

  return FALSE;
}



/** '$clause_from_source'(+Owner, +File, +Line, -Clause) is semidet.

True when Clause is the clause that contains  Line in File. Owner is the
source file owning Clause. For normal  files,   Owner  and  File are the
same. This predicate can find a clause in an included file by specifying
the main file as Owner and the included file as File.
*/

static
PRED_IMPL("$clause_from_source", 4, clause_from_source, 0)
{ PRED_LD
  atom_t owner_name;
  atom_t file_name;
  SourceFile of, sf;		/* owner file, source file */
  unsigned short source_no;
  int ln;
  ListCell cell;
  Clause c = NULL;

  term_t owner = A1;
  term_t file = A2;
  term_t line = A3;
  term_t clause = A4;

  if ( !PL_get_atom_ex(owner, &owner_name) ||
       !PL_get_atom_ex(file, &file_name) ||
       !(of = lookupSourceFile(owner_name, FALSE)) ||
       !PL_get_integer_ex(line, &ln) )
    return FALSE;

  if ( file_name == owner_name ) {
    source_no = of->index;
  } else {
    if ( !(sf=lookupSourceFile(file_name, FALSE)) )
      return FALSE;
    source_no = sf->index;
  }

  LOCKSRCFILE(of);
  for(cell = of->procedures; cell; cell = cell->next)
  { Procedure proc = cell->value;
    Definition def = proc->definition;

    if ( def && false(def, P_FOREIGN) )
    { ClauseRef cref;

      acquire_def(def);
      for(cref = def->impl.clauses.first_clause; cref; cref = cref->next )
      { Clause cl = cref->value.clause;

	if ( cl->source_no == source_no )
	{ if ( ln >= (int)cl->line_no )
	  { if ( !c || c->line_no < cl->line_no )
	      c = cl;
	  }
	}
      }
      release_def(def);
    }
  }
  UNLOCKSRCFILE(of);

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
  PRED_DEF("$clause_from_source", 4, clause_from_source, 0)
  PRED_DEF("$unload_file", 1, unload_file, 0)
  PRED_DEF("$start_consult", 2, start_consult, 0)
  PRED_DEF("$end_consult", 1, end_consult, 0)
  PRED_DEF("$make_system_source_files", 0, make_system_source_files, 0)
EndPredDefs
