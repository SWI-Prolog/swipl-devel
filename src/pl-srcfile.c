/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2021, VU University Amsterdam
			      CWI, Amsterdam
			      SWI-Prolog Solutions b.v.
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

/*#define O_DEBUG 1*/
#include "pl-srcfile.h"
#include "pl-comp.h"
#include "pl-dbref.h"
#include "pl-event.h"
#include "pl-tabling.h"
#include "pl-setup.h"
#include "pl-util.h"
#include "pl-proc.h"
#include "pl-fli.h"
#include "pl-modul.h"
#include "pl-supervisor.h"
#include "pl-wam.h"

static void	fix_ssu(p_reload *r, Clause clause);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Source administration. The core object is  SourceFile, which keeps track
of procedures that are defined by it.  Source files are identified by an
unsigned int, which is registered with clauses and procedures.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#undef LD
#define LD LOCAL_LD

static inline void
LOCKSRCFILE(SourceFile sf)
{ acquireSourceFile(sf);
#ifdef O_PLMT
  countingMutexLock((sf)->mutex);
#endif
}

static inline void
UNLOCKSRCFILE(SourceFile sf)
{
#ifdef O_PLMT
  countingMutexUnlock((sf)->mutex);
#endif
  releaseSourceFile(sf);
}

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

  GD->files.highest = 0;
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This updates the dynamic predicates that  maintain the source admin. The
callback is rather dubious as it is   completely unclear what we must do
with exceptions in various conditions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
clearSourceAdmin(atom_t sf_name)
{ GET_LD
  int rc = FALSE;
  fid_t fid;
  predicate_t pred;

  if ( GD->cleaning == CLN_DATA )
    return TRUE;

  pred = _PL_predicate("$clear_source_admin", 1, "system",
		       &GD->procedures.clear_source_admin1);

  if ( (fid=PL_open_foreign_frame()) )
  { term_t name = PL_new_term_ref();

    PL_put_atom(name, sf_name);
    startCritical();			/* block signals */
    rc = PL_call_predicate(MODULE_system, PL_Q_NODEBUG, pred, name);
    rc = endCritical() && rc;

    PL_discard_foreign_frame(fid);
  }

  return rc;
}


static atom_t
destroySourceFile(SourceFile sf)
{ if ( sf->magic == SF_MAGIC )
  { atom_t name;
    SourceFile f;

    sf->magic = SF_MAGIC_DESTROYING;
    f = deleteHTable(GD->files.table, (void*)sf->name);
    assert(f);
    (void)f;
    name = sf->name;
    putSourceFileArray(sf->index, NULL);
    if ( GD->files.no_hole_before > sf->index )
      GD->files.no_hole_before = sf->index;
    unallocSourceFile(sf);

    return name;
  }

  return 0;
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

  if ( !(file=lookupHTable(GD->files.table, (void*)name)) &&
       create )
  { file = allocHeapOrHalt(sizeof(*file));
    memset(file, 0, sizeof(*file));

    file->name       = name;
    file->system     = GD->bootsession;
    file->from_state = GD->bootsession;
    file->resource   = GD->bootsession;
#ifdef O_PLMT
    file->mutex    = allocSimpleMutex(PL_atom_chars(name));
#endif
    file->magic    = SF_MAGIC;
    PL_register_atom(file->name);
    registerSourceFile(file);

    addNewHTable(GD->files.table, (void*)name, file);
  }

  return file;
}


SourceFile
lookupSourceFile(atom_t name, int create)
{ SourceFile sf;

  PL_LOCK(L_SRCFILE);
  sf = lookupSourceFile_unlocked(name, create);
  if ( sf )
    acquireSourceFile(sf);
  PL_UNLOCK(L_SRCFILE);

  return sf;
}


void
#ifdef O_DEBUG
acquireSourceFile_d(SourceFile sf, const char *file, unsigned int line)
#else
acquireSourceFile(SourceFile sf)
#endif
{ ATOMIC_INC(&sf->references);
  DEBUG(MSG_SRCFILE_REF,
	{ Sdprintf("%d: acquireSourceFile(%s) at %s:%d --> %d\n",
		   PL_thread_self(), PL_atom_chars(sf->name), file, line, sf->references);
	});
}


void
acquireSourceFileNo(int index)
{ SourceFile sf;

  if ( (sf = indexToSourceFile(index)) )
    return acquireSourceFile(sf);
}


int
#ifdef O_DEBUG
releaseSourceFile_d(SourceFile sf, const char *file, unsigned int line)
#else
releaseSourceFile(SourceFile sf)
#endif
{ DEBUG(MSG_SRCFILE_REF,
	Sdprintf("%d: releaseSourceFile(%s) at %s:%d --> %d\n",
		 PL_thread_self(),
		 PL_atom_chars(sf->name),
		 file, line,
		 sf->references-1));

  if ( sf->references <= 0 )
  { Sdprintf("Oops: %d references for sourc file %s\n", PL_atom_chars(sf->name));
    sf->references = 0x4000000;
  }
  if ( ATOMIC_DEC(&sf->references) == 0 )
  { atom_t name = 0;

    PL_LOCK(L_SRCFILE);
    if ( sf->references == 0 &&
	 !sf->system &&
	 !sf->current_procedure &&
	 !sf->procedures &&
	 !sf->modules )
    { DEBUG(MSG_DESTROY_MODULE,
	    Sdprintf("Destroying empty source file %s\n",
		     PL_atom_chars(sf->name)));
      name = destroySourceFile(sf);
    }
    PL_UNLOCK(L_SRCFILE);

    if ( name )
    { int rc = clearSourceAdmin(name);
      PL_unregister_atom(name);

      return rc;
    }
  }

  return TRUE;
}

int
releaseSourceFileNo(int index)
{ SourceFile sf;

  if ( (sf = indexToSourceFile(index)) )
    return releaseSourceFile(sf);

  return TRUE;
}

int
hasProcedureSourceFile(SourceFile sf, Procedure proc)
{ ListCell cell;

  if ( true(proc->definition, FILE_ASSIGNED) )
  { for(cell=sf->procedures; cell; cell = cell->next)
    { if ( cell->value == proc )
	return TRUE;
    }
  }

  return FALSE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
addProcedureSourceFile(SourceFile, Procedure) associates a  procedure to
a source file. Note that  a  procedure   may  be  associated to multiple
source files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
addProcedureSourceFile(SourceFile sf, Procedure proc)
{ if ( !(sf->index == proc->source_no && !sf->reload) )
  { LOCKSRCFILE(sf);
    if ( !hasProcedureSourceFile(sf, proc) )
    { ListCell cell;

      cell = allocHeapOrHalt(sizeof(struct list_cell));
      cell->value = proc;
      cell->next = sf->procedures;
      sf->procedures = cell;
      set(proc->definition, FILE_ASSIGNED);
      if ( COMPARE_AND_SWAP_UINT(&proc->source_no, 0, sf->index) )
	acquireSourceFile(sf);
      else
	set(proc, PROC_MULTISOURCE);
    }
    UNLOCKSRCFILE(sf);
  }
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

(*) Although the system:$init_goal/3 clauses belong   to the file, we'll
consider a file holding only initialization goals empty.
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
    Definition def;

    next = cell->next;
    proc = cell->value;
    def  = proc->definition;

    if ( lookupHTable(m->procedures, (void*)def->functor->functor) ||
	 PROCEDURE_dinit_goal->definition == def )	/* see (*) */
    { if ( sf->current_procedure == proc )
	sf->current_procedure = NULL;

      if ( prev )
	prev->next = cell->next;
      else
	sf->procedures = cell->next;
      freeHeap(cell, sizeof(*cell));
    } else
    { DEBUG(MSG_DESTROY_MODULE,
	    Sdprintf("  Keeping %s\n", procedureName(proc)));
      prev = cell;
    }
  }

  UNLOCKSRCFILE(sf);
}


/** '$source_file'(:Head, -File) is semidet.
*/

static
PRED_IMPL("$source_file", 2, source_file, PL_FA_TRANSPARENT)
{ PRED_LD
  Procedure proc;
  SourceFile sf;

  term_t descr = A1;
  term_t file  = A2;

  if ( get_procedure(descr, &proc, 0, GP_FINDHERE) )
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
  int rc = FALSE;
  SourceFile sf;

  term_t file = A1;

  if ( PL_get_atom_ex(file, &name) &&
       (sf = lookupSourceFile(name, FALSE)) )
  { if ( sf->count > 0 )
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
    }

    releaseSourceFile(sf);
  }

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


/** '$source_file_property'(+SrcFile, +Property, -Value) is semidet.
*/

static
PRED_IMPL("$source_file_property", 3, source_file_property, 0)
{ PRED_LD
  atom_t filename, property;

  if ( PL_get_atom_ex(A1, &filename) &&
       PL_get_atom_ex(A2, &property) )
  { SourceFile sf = lookupSourceFile(filename, FALSE);
    int rc;

    if ( property == ATOM_load_count )
      rc = PL_unify_integer(A3, sf ? sf->count : 0);
    else if ( property == ATOM_reloading )
      rc = PL_unify_bool(A3, sf ? sf->reload != NULL : 0);
    else if ( property == ATOM_number_of_clauses )
      rc = PL_unify_integer(A3, sf ? sf->number_of_clauses : 0);
    else if ( property == ATOM_resource )
      rc = PL_unify_bool(A3, sf ? sf->resource : FALSE);
    else if ( property == ATOM_from_state )
      rc = PL_unify_bool(A3, sf ? sf->from_state : FALSE);
    else
      rc = PL_domain_error("source_file_property", A2);

    if ( sf )
      releaseSourceFile(sf);

    return rc;
  }

  return FALSE;
}

static
PRED_IMPL("$set_source_file", 3, set_source_file, 0)
{ PRED_LD
  atom_t filename, property;
  SourceFile sf;

  if ( !PL_get_atom_ex(A1, &filename) ||
       !PL_get_atom_ex(A2, &property) )
    return FALSE;

  if ( (sf = lookupSourceFile(filename, FALSE)) )
  { int rc;

    if ( property == ATOM_resource )
    { int v;

      if ( PL_get_bool_ex(A3, &v) )
      { sf->resource = v;
	rc = TRUE;
      } else
	rc = FALSE;
    } else
      rc = PL_domain_error("source_file_property", A2);

    releaseSourceFile(sf);
    return rc;
  } else
    return PL_existence_error("source_file", A1);

}

static
PRED_IMPL("$set_source_files", 1, set_source_files, 0)
{ PRED_LD
  atom_t prop;

  if ( !PL_get_atom_ex(A1, &prop) )
    return FALSE;
  if ( prop == ATOM_system || prop == ATOM_from_state )
  { int i, n;

    PL_LOCK(L_SRCFILE);
    n = highSourceFileIndex();
    for(i=1; i<n; i++)
    { SourceFile f = indexToSourceFile(i);

      if ( f )
      { if ( prop == ATOM_system )
	  f->system = TRUE;
	f->from_state = TRUE;
      }
    }
    PL_UNLOCK(L_SRCFILE);

    return TRUE;
  } else
    return PL_domain_error("source_property", A1);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unloadFile(SourceFile sf)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
unloadFile(SourceFile sf)
{ ListCell cell;
  size_t deleted = 0;
  int rc;

  delayEvents();
  LOCKSRCFILE(sf);
				      /* remove the clauses */
  for(cell = sf->procedures; cell; cell = cell->next)
  { Procedure proc = cell->value;
    Definition def = proc->definition;

    if ( false(def, P_FOREIGN|P_THREAD_LOCAL) )
    { deleted += removeClausesPredicate(
		     def, true(def, P_MULTIFILE) ? sf->index : 0, TRUE);
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
  (void)deleted;

  freeList(&sf->procedures);
  delAllModulesSourceFile__unlocked(sf);
  UNLOCKSRCFILE(sf);

  rc = sendDelayedEvents(TRUE) >= 0;
  pl_garbage_collect_clauses();

  return rc;
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
    int rc;

    if ( sf->system )
    { rc = PL_error(NULL, 0, NULL, ERR_PERMISSION,
		    ATOM_unload, ATOM_file, A1);
    } else
    { if ( unloadFile(sf) )
      { for(mc=sf->modules; mc; mc=mcn)
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
	rc = TRUE;
      } else
	rc = FALSE;
    }

    releaseSourceFile(sf);
    return rc;
  }

  return TRUE;
}


		 /*******************************
		 *	    RECONSULT		*
		 *******************************/

static void	fix_metapredicate(p_reload *r);
static void	fix_det(p_reload *r);

#ifdef O_PLMT
#define GEN_RELOAD (GEN_MAX-PL_thread_self())
#else
#define GEN_RELOAD (GEN_MAX-1)
#endif

static int
startReconsultFile(SourceFile sf)
{ GET_LD
  sf_reload *r;

  DEBUG(MSG_RECONSULT, Sdprintf("Reconsult %s ...\n", sourceFileName(sf)));

  if ( (r = allocHeap(sizeof(*sf->reload))) )
  { ListCell cell;

    memset(r, 0, sizeof(*r));
    r->procedures        = newHTable(16);
    r->reload_gen        = GEN_RELOAD;
    r->pred_access_count = popNPredicateAccess(0);
    sf->reload = r;

    LD->reload.generation = r->reload_gen;
    LD->reload.nesting++;

    for(cell = sf->procedures; cell; cell = cell->next)
    { Procedure proc = cell->value;
      Definition def = proc->definition;
      ClauseRef c;

      if ( false(def, P_FOREIGN|P_THREAD_LOCAL) )
      { acquire_def(def);
	for(c = def->impl.clauses.first_clause; c; c = c->next)
	{ Clause cl = c->value.clause;

	  if ( !GLOBALLY_VISIBLE_CLAUSE(cl, global_generation()) ||
	       true(cl, CL_ERASED) )
	    continue;
	  if ( true(def, P_MULTIFILE) && cl->owner_no != sf->index )
	    continue;
	  if ( true(def, P_DYNAMIC) && cl->owner_no == 0 )
	    continue;

	  cl->generation.erased = r->reload_gen;
	}
	release_def(def);
	clear(def, P_DISCONTIGUOUS);		/* will be reinstantiated */
      }
      if ( true(def, P_AUTOLOAD) )
      { clear(def, P_AUTOLOAD);			/* should be be more selective? */
      }
    }

    return TRUE;
  }

  return PL_no_memory();
}


static ClauseRef
find_clause(ClauseRef cref, gen_t generation)
{ for(; cref; cref = cref->next)
  { if ( GLOBALLY_VISIBLE_CLAUSE(cref->value.clause, generation) )
      break;
  }

  return cref;
}


#define advance_clause(r) LDFUNC(advance_clause, r)
static void
advance_clause(DECL_LD p_reload *r)
{ ClauseRef cref;

  if ( (cref = r->current_clause) )
  { acquire_def(r->predicate);
    for(cref = cref->next; cref; cref = cref->next)
    { if ( GLOBALLY_VISIBLE_CLAUSE(cref->value.clause, r->generation) )
	break;
    }
    release_def(r->predicate);
    r->current_clause = cref;
  }
}


static void
copy_clause_source(Clause dest, Clause src)
{ dest->line_no   = src->line_no;
  if ( dest->source_no != src->source_no ||
       dest->owner_no  != src->owner_no )
  { acquireSourceFileNo(src->owner_no);
    if ( src->source_no != src->owner_no )
      acquireSourceFileNo(src->source_no);
    releaseSourceFileNo(dest->owner_no);
    if ( dest->source_no != dest->owner_no )
      releaseSourceFileNo(dest->source_no);
    dest->source_no = src->source_no;
    dest->owner_no  = src->owner_no;
  }
}


#define keep_clause(r, clause) LDFUNC(keep_clause, r, clause)
static ClauseRef
keep_clause(DECL_LD p_reload *r, Clause clause)
{ ClauseRef cref = r->current_clause;
  Clause keep = cref->value.clause;

  keep->generation.erased = GEN_MAX;
  copy_clause_source(keep, clause);
  freeClause(clause);
  advance_clause(r);

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
reloadHasClauses(DECL_LD SourceFile sf, Procedure proc)
{ p_reload *reload;

  if ( sf->reload && (reload=lookupHTable(sf->reload->procedures, proc)) )
  { return reload->number_of_clauses > 0;
  }

  return FALSE;
}


static int
isRedefinedProcedure(Procedure proc, gen_t gen)
{ GET_LD
  Definition def = proc->definition;
  ClauseRef c;
  int ret = FALSE;

  acquire_def(def);
  for(c = def->impl.clauses.first_clause; c; c = c->next)
  { Clause cl = c->value.clause;
    if ( GLOBALLY_VISIBLE_CLAUSE(cl, gen) )
    { ret = TRUE;
      break;
    }
  }
  release_def(def);

  return ret;
}


#define reloadContext(sf, proc) LDFUNC(reloadContext, sf, proc)
static p_reload *
reloadContext(DECL_LD SourceFile sf, Procedure proc)
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
    } else if ( isRedefinedProcedure(proc, global_generation()) )
    { definition_ref *dref = pushPredicateAccessObj(def);

      if ( !dref )
      { freeHeap(reload, sizeof(*reload));
	return NULL;
      }
      reload->generation = dref->generation;
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
assertProcedureSource(DECL_LD SourceFile sf, Procedure proc, Clause clause)
{ if ( sf && sf->reload )
  { p_reload *reload;
    Definition def = proc->definition;
    ClauseRef cref;

    assert(proc == sf->current_procedure);

    sf->reload->number_of_clauses++;

    if ( !(reload = reloadContext(sf, proc)) )
    { freeClause(clause);
      return NULL;
    }

    if ( reload->number_of_clauses++ == 0 )
    { fix_det(reload);
      fix_ssu(reload, clause);
    }

    if ( true(reload, P_NEW|P_NO_CLAUSES) )
      return assertProcedure(proc, clause, CL_END);

    if ( (cref = reload->current_clause) )
    { ClauseRef cref2;

      if ( equal_clause(cref->value.clause, clause) )
      { DEBUG(MSG_RECONSULT_CLAUSE,
	      Sdprintf("  Keeping clause %d\n",
		       clauseNo(cref->value.clause, reload->generation)));
	return keep_clause(reload, clause);
      }

      set(reload, P_MODIFIED);

      acquire_def(def);
      for(cref2 = cref->next; cref2; cref2 = cref2->next)
      { Clause c2 = cref2->value.clause;

	if ( !GLOBALLY_VISIBLE_CLAUSE(c2, reload->generation) )
	  continue;
	if ( true(def, P_MULTIFILE) && c2->owner_no != sf->index )
	  continue;

	if ( equal_clause(c2, clause) )
	{ ClauseRef del;

	  for(del = cref; del != cref2; del = del->next)
	  { Clause c = del->value.clause;

	    if ( !GLOBALLY_VISIBLE_CLAUSE(c, reload->generation) ||
		 true(c, CL_ERASED) )
	      continue;
	    if ( true(def, P_MULTIFILE) && c->owner_no != sf->index )
	      continue;

	    DEBUG(MSG_RECONSULT_CLAUSE,
		  Sdprintf("  Deleted clause %d\n",
			   clauseNo(c, reload->generation)));
	  }
	  release_def(def);

	  reload->current_clause = cref2;
	  DEBUG(MSG_RECONSULT_CLAUSE,
		Sdprintf("  Keeping clause %d\n",
			 clauseNo(cref2->value.clause, reload->generation)));
	  return keep_clause(reload, clause);
	}
      }
      release_def(def);

      DEBUG(MSG_RECONSULT_CLAUSE,
	    Sdprintf("  Inserted before clause %d\n",
		     clauseNo(cref->value.clause, reload->generation)));
      if ( (cref2 = assertProcedure(proc, clause, cref)) )
	cref2->value.clause->generation.created = sf->reload->reload_gen;

      return cref2;
    } else
    { if ( (cref = assertProcedure(proc, clause, CL_END)) )
	cref->value.clause->generation.created = sf->reload->reload_gen;
      DEBUG(MSG_RECONSULT_CLAUSE, Sdprintf("  Added at the end\n"));

      set(reload, P_MODIFIED);

      return cref;
    }
  } else if ( sf )
  { sf->number_of_clauses++;
  }

  return assertProcedure(proc, clause, CL_END);
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
setAttrProcedureSource(DECL_LD SourceFile sf, Procedure proc,
		       unsigned attr, int val)
{ if ( val && (attr&PROC_DEFINED) )
    associateSource(sf, proc);

  if ( sf->reload )
  { p_reload *reload;

    if ( !(reload = reloadContext(sf, proc)) )
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
check_ssu(p_reload *r)
{ GET_LD
  Definition def = r->predicate;
  ClauseRef cref;
  int errors = 0;

  acquire_def(def);
  for(cref=def->impl.clauses.first_clause; cref && !errors; cref=cref->next)
  { Clause cl = cref->value.clause;

    if ( false(cl, CL_ERASED) )
    { if ( true(def, P_SSU_DET) &&
	   false(cl, SSU_COMMIT_CLAUSE|SSU_CHOICE_CLAUSE) )
	errors++;
      if ( false(def, P_SSU_DET) &&
	   true(cl, SSU_COMMIT_CLAUSE|SSU_CHOICE_CLAUSE) )
	errors++;
    }
  }
  release_def(def);

  /* TBD: print_message/2.  Not easy as we cannot print.  So, we have
   * to use the delayed event mechanism.
   */
  if ( errors )
    Sdprintf("ERROR: Mixed SSU (=> and :-) clauses in %s\n",
	     predicateName(def));
}


#define fix_attributes(sf, def, r) LDFUNC(fix_attributes, sf, def, r)
static void
fix_attributes(DECL_LD SourceFile sf, Definition def, p_reload *r)
{ if ( false(def, P_MULTIFILE) )
    def->flags = (def->flags & ~P_ATEND) | (r->flags & P_ATEND);
  else
    def->flags |= (r->flags&P_ATEND);

  fix_metapredicate(r);
}


static void
fix_det(p_reload *r)
{ Definition def = r->predicate;

  if ( true(def, P_DET) && false(r, P_DET) )
  { clear(def, P_DET);
    freeCodesDefinition(def, TRUE);
  }
}


static void
fix_ssu(p_reload *r, Clause clause)
{ Definition def = r->predicate;

  if ( false(def, P_DYNAMIC|P_MULTIFILE) )
  { if ( true(clause, SSU_COMMIT_CLAUSE|SSU_CHOICE_CLAUSE) )
    { if ( false(def, P_SSU_DET) )
      { set(def, P_SSU_DET);
	set(r, P_CHECK_SSU);
      }
    } else
    { if ( true(def, P_SSU_DET) )
      { clear(def, P_SSU_DET);
	set(r, P_CHECK_SSU);
      }
    }
  }
}


int
setMetapredicateSource(DECL_LD SourceFile sf, Procedure proc,
		       arg_info *args)
{ associateSource(sf, proc);

  if ( sf->reload )
  { p_reload *reload;
    size_t i, arity = proc->definition->functor->arity;

    if ( !(reload = reloadContext(sf, proc)) )
      return FALSE;

    if ( !reload->args )
      reload->args = allocHeapOrHalt(sizeof(*reload->args)*arity);
    for(i=0; i<arity; i++)
      reload->args[i].meta = args[i].meta;

    if ( isTransparentMetamask(proc->definition, args) )
      set(reload, P_TRANSPARENT);
    else
      clear(reload, P_TRANSPARENT);
    set(reload, P_META);
  } else
  { setMetapredicateMask(proc->definition, args);
  }

  return TRUE;
}


static int
equal_meta(Definition def, const arg_info *args)
{ if ( def->impl.any.args && args )
  { size_t i, arity = def->functor->arity;

    for(i=0; i<arity; i++)
    { if ( def->impl.any.args[i].meta != args[i].meta )
	return FALSE;
    }

    return TRUE;
  }

  return FALSE;
}

static void
fix_metapredicate(p_reload *r)
{ Definition def = r->predicate;

  if ( false(def, P_MULTIFILE) )
  { int mfmask = (P_META|P_TRANSPARENT);

    if ( (def->flags&mfmask) != (r->flags&mfmask) ||
	 !equal_meta(def, r->args) )
    { if ( true(def, P_META) && false(r, P_META) )
	clear_meta_declaration(def);
      else if ( true(r, P_META) )
	setMetapredicateMask(def, r->args);
      clear(def, P_TRANSPARENT);
      set(def, r->flags&P_TRANSPARENT);

      freeCodesDefinition(def, FALSE);
    }
  } else if ( true(r, P_META) )
  { setMetapredicateMask(def, r->args);
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
  FOR_TABLE(m->public, n, v)
  { if ( !r->public ||
	 !lookupHTable(r->public, n) )
    { DEBUG(MSG_RECONSULT_MODULE,
	    Sdprintf("Delete export %s\n", procedureName(v)));
      deleteHTable(m->public, n);
    }
  };
  UNLOCKMODULE(m);
}


static void
delete_old_predicate(SourceFile sf, Procedure proc)
{ Definition def = proc->definition;
  size_t deleted;

  if ( def->functor->functor == FUNCTOR_dtabled2 )
  { GET_LD
    ClauseRef c;

    acquire_def(def);
    for(c = def->impl.clauses.first_clause; c; c = c->next)
    { Clause cl = c->value.clause;

      if ( false(cl, CL_ERASED) &&
	   GLOBALLY_VISIBLE_CLAUSE(cl, global_generation()) )
	untable_from_clause(cl);
    }
    release_def(def);
  }

  deleted = removeClausesPredicate(
		def,
		true(def, P_MULTIFILE) ? sf->index : 0,
		TRUE);

  if ( false(def, P_MULTIFILE) )
  { clear(def, FILE_ASSIGNED);
    clear_meta_declaration(def);
    freeCodesDefinition(def, TRUE);
  }

  DEBUG(MSG_RECONSULT_PRED,
	Sdprintf("Deleted %ld clauses from predicate %s\n",
		 (long)deleted, predicateName(def)));

  (void)deleted;
}


static void
delete_old_predicates(SourceFile sf)
{ GET_LD
  ListCell cell, prev = NULL, next;

  for(cell = sf->procedures; cell; cell = next)
  { Procedure proc = cell->value;

    next = cell->next;

    if ( false(proc->definition, P_FOREIGN) &&
	 !lookupHTable(sf->reload->procedures, proc) )
    { delete_old_predicate(sf, proc);

      if ( prev )
	prev->next = cell->next;
      else
	sf->procedures = cell->next;

      freeHeap(cell, sizeof(*cell));
    } else
    { prev = cell;
    }
  }
}


#define delete_pending_clauses(sf, def, r) LDFUNC(delete_pending_clauses, sf, def, r)
static void
delete_pending_clauses(DECL_LD SourceFile sf, Definition def, p_reload *r)
{ ClauseRef cref;
  sf_reload *rl = sf->reload;

  acquire_def(def);
  for(cref = r->current_clause; cref; cref = cref->next)
  { Clause c = cref->value.clause;

    if ( !GLOBALLY_VISIBLE_CLAUSE(c, r->generation) ||
	 true(c, CL_ERASED) )
      continue;
    if ( true(r->predicate, P_MULTIFILE|P_DYNAMIC) && c->owner_no != sf->index )
      continue;

    if ( def->functor->functor == FUNCTOR_dtabled2 )
      untable_from_clause(c);

    c->generation.erased = rl->reload_gen;
    set(r, P_MODIFIED);
    DEBUG(MSG_RECONSULT_CLAUSE,
	  Sdprintf("  %s: deleted clause %d\n",
		   predicateName(def),
		   clauseNo(c, r->generation)));
  }
  release_def(def);
}


#define end_reconsult_proc(sf, proc, r) LDFUNC(end_reconsult_proc, sf, proc, r)
static size_t
end_reconsult_proc(DECL_LD SourceFile sf, Procedure proc, p_reload *r)
{ size_t dropped_access = 0;

  DEBUG(MSG_RECONSULT_CLAUSE,
	Sdprintf("Fixup %s\n", predicateName(proc->definition)));

  if ( false(r, P_NEW|P_NO_CLAUSES) )
  { Definition def = proc->definition;

    delete_pending_clauses(sf, def, r);
    fix_attributes(sf, def, r);
    reconsultFinalizePredicate(sf->reload, def, r);
    if ( true(r, P_CHECK_SSU) )
      check_ssu(r);
  } else
  { dropped_access++;
    if ( true(r, P_NO_CLAUSES) )
    { Definition def = proc->definition;
      fix_attributes(sf, def, r);
    }
  }
  if ( r->args )
    freeHeap(r->args, 0);
  freeHeap(r, sizeof(*r));

  return dropped_access;
}


static int
endReconsult(SourceFile sf)
{ GET_LD
  sf_reload *reload;

  if ( (reload=sf->reload) )
  { size_t accessed_preds = reload->procedures->size;

    delayEvents();
    delete_old_predicates(sf);

    for_table(reload->procedures, n, v,
	      { Procedure proc = n;
		p_reload *r = v;

		accessed_preds -= end_reconsult_proc(sf, proc, r);
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

    sf->number_of_clauses = sf->reload->number_of_clauses;
    sf->reload = NULL;
    freeHeap(reload, sizeof(*reload));

    if ( --LD->reload.nesting == 0 )
      LD->reload.generation = GEN_INVALID;

    pl_garbage_collect_clauses();
    if ( sendDelayedEvents(TRUE) < 0 )
      return FALSE;
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Flush the definition of proc in the   context  of (reloading) the source
file sf.  This performs the following steps:

  - If we have not seen the predicate, remove all clauses we may have
    for it and add as P_NEW.
  - If we have seen the predicate, perform the generation sync we would
    normally do at the end of the file and mark the predicate reload
    context as P_NEW.

`proc` is a predicate indicator. If it  is not qualified, it is resolved
against M in prolog_load_context(module, M).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
flush_procedure(SourceFile sf, Procedure proc)
{ GET_LD
  sf_reload *reload;

  if ( (reload=sf->reload) )
  { p_reload *r;

    if ( (r=lookupHTable(sf->reload->procedures, proc)) )
    { if ( false(r, P_NEW|P_NO_CLAUSES) )
      { Definition def = proc->definition;

	delete_pending_clauses(sf, def, r);
	fix_attributes(sf, def, r);
	reconsultFinalizePredicate(reload, def, r);
      }
    } else
    { delete_old_predicate(sf, proc);
      (void)reloadContext(sf, proc);
    }
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
startConsult(SourceFile sf)
{ acquireSourceFile(sf);
  if ( sf->count++ > 0 )		/* This is a re-consult */
  { if ( !startReconsultFile(sf) )
    { releaseSourceFile(sf);
      return FALSE;
    }
  }

  sf->current_procedure = NULL;
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
  { SourceFile sf = lookupSourceFile(name, TRUE);

    sf->mtime = time;
    startConsult(sf);
    releaseSourceFile(sf);

    return TRUE;
  }

  return FALSE;
}


int
endConsult(SourceFile sf)
{ int rc;

  sf->current_procedure = NULL;
  rc = endReconsult(sf);
  releaseSourceFile(sf);

  return rc;
}


static
PRED_IMPL("$fixup_reconsult", 1, fixup_reconsult, 0)
{ PRED_LD
  atom_t name;
  int rc = FALSE;

  if ( PL_get_atom_ex(A1, &name) )
  { SourceFile sf;

    if ( (sf=lookupSourceFile(name, FALSE)) )
    { rc = endReconsult(sf);
      releaseSourceFile(sf);
      sf->current_procedure = NULL;
    }
  }

  return rc;
}


static
PRED_IMPL("$end_consult", 1, end_consult, 0)
{ PRED_LD
  atom_t name;
  int rc = FALSE;

  if ( PL_get_atom_ex(A1, &name) )
  { SourceFile sf;

    if ( (sf=lookupSourceFile(name, FALSE)) )
    { rc = endConsult(sf);
      releaseSourceFile(sf);
    }
  }

  return rc;
}



/** '$clause_from_source'(+Owner, +File, +Line, -Clauses) is semidet.

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
  SourceFile of=NULL, sf=NULL;		/* owner file, source file */
  unsigned int source_no;
  int ln;
  ListCell cell;
  Clause c = NULL;
  int rc = FALSE;
  tmp_buffer buf;

  term_t owner   = A1;
  term_t file    = A2;
  term_t line    = A3;
  term_t clauses = A4;

  if ( !PL_get_atom_ex(owner, &owner_name) ||
       !PL_get_atom_ex(file, &file_name) ||
       !PL_get_integer_ex(line, &ln) ||
       !(of = lookupSourceFile(owner_name, FALSE)) )
    return FALSE;

  initBuffer(&buf);

  if ( file_name == owner_name ) {
    source_no = of->index;
  } else {
    if ( !(sf=lookupSourceFile(file_name, FALSE)) )
      goto out;
    source_no = sf->index;
  }

  LOCKSRCFILE(of);
  for(cell = of->procedures; cell; cell = cell->next)
  { Procedure proc = cell->value;
    Definition def = getProcDefinition(proc);

    if ( def && false(def, P_FOREIGN) )
    { ClauseRef cref;

      acquire_def(def);
      for(cref = def->impl.clauses.first_clause; cref; cref = cref->next )
      { Clause cl = cref->value.clause;

	if ( cl->source_no == source_no )
	{ if ( ln >= (int)cl->line_no )
	  { if ( !c || c->line_no < cl->line_no )
	    { c = cl;
	      emptyBuffer(&buf, 512);
	      addBuffer(&buf, cl, Clause);
	    } else if ( c && c->line_no == cl->line_no )
	    { addBuffer(&buf, cl, Clause);
	    }
	  }
	}
      }
      release_def(def);
    }
  }
  UNLOCKSRCFILE(of);

					/* TBD: leaves clauses unreferenced */
  if ( !isEmptyBuffer(&buf) )
  { term_t tail = PL_copy_term_ref(clauses);
    term_t head = PL_new_term_ref();
    Clause *clp = baseBuffer(&buf, Clause);
    Clause *elp = topBuffer(&buf, Clause);

    for(; clp < elp; clp++)
    { if ( !PL_unify_list(tail, head, tail) ||
	   !PL_unify_clref(head, c) )
      { rc = FALSE;
	break;
      }
    }
    rc = PL_unify_nil(tail);
  }

out:
  discardBuffer(&buf);
  if ( of ) releaseSourceFile(of);
  if ( sf ) releaseSourceFile(sf);

  return rc;
}

/** '$flush_predicate'(+Predicate, +File) is det.
 *
 * Finalize the definition of Predicate wrt. File. After this,
 * subsequent changes to the predicate are _immediate_.
 */

#define flush_predicate(pred) LDFUNC(flush_predicate, pred)
static int
flush_predicate(DECL_LD term_t pred)
{ SourceFile sf;
  Procedure proc;
  Module m = LD->modules.source;
  functor_t fdef;
  int rc = FALSE;

  if ( ReadingSource )
    sf = lookupSourceFile(source_file_name, TRUE);
  else
    return TRUE;			/* not reading source; nothing to flush */

  if ( get_functor(pred, &fdef, &m, 0, GF_PROCEDURE) )
  { if ( (proc=isCurrentProcedure(fdef,m)) )
      rc = flush_procedure(sf, proc);
  }
  releaseSourceFile(sf);

  return rc;
}


static
PRED_IMPL("$flush_predicate", 1, flush_predicate, 0)
{ PRED_LD

  return flush_predicate(A1);
}


/** '$flushed_predicate'(:Head) is semidet.
 *
 * True when the finalized definition of Goal is defined.
 */

static
PRED_IMPL("$flushed_predicate", 1, flushed_predicate, 0)
{ PRED_LD
  SourceFile sf;
  term_t head = PL_new_term_ref();
  Module m = LD->modules.source;
  functor_t fdef;
  Procedure proc;
  int rc;

  if ( !PL_strip_module(A1, &m, head) )
    return FALSE;
  if ( !PL_get_functor(head, &fdef) )
    return PL_type_error("callable", A1);
  if ( !(proc=isCurrentProcedure(fdef, m)) )
    return FALSE;

  if ( ReadingSource )
    sf = lookupSourceFile(source_file_name, TRUE);
  else
    return isDefinedProcedure(proc);

  flush_procedure(sf, proc);
  rc = isDefinedProcedure(proc);
  releaseSourceFile(sf);

  return rc;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

#define  PL_FA_NONDET PL_FA_NONDETERMINISTIC

BeginPredDefs(srcfile)
  PRED_DEF("$source_file",		2, source_file,		     0)
  PRED_DEF("$source_file_predicates",	2, source_file_predicates,   0)
  PRED_DEF("$time_source_file",		3, time_source_file,    PL_FA_NONDET)
  PRED_DEF("$source_file_property",	3, source_file_property,     0)
  PRED_DEF("$set_source_file",          3, set_source_file,          0)
  PRED_DEF("$clause_from_source",	4, clause_from_source,	     0)
  PRED_DEF("$unload_file",		1, unload_file,		     0)
  PRED_DEF("$start_consult",		2, start_consult,	     0)
  PRED_DEF("$end_consult",		1, end_consult,		     0)
  PRED_DEF("$fixup_reconsult",		1, fixup_reconsult,          0)
  PRED_DEF("$set_source_files",	        1, set_source_files,	     0)
  PRED_DEF("$flush_predicate",		1, flush_predicate,          0)
  PRED_DEF("$flushed_predicate",	1, flushed_predicate,	     0)
EndPredDefs
