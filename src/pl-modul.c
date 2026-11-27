/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2024, University of Amsterdam
                              VU University Amsterdam
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

#include "pl-modul.h"
#include "pl-comp.h"
#include "pl-util.h"
#include "pl-proc.h"
#include "pl-funct.h"
#include "pl-srcfile.h"
#include "pl-fli.h"
#include "pl-prims.h"
#include "pl-wam.h"
#include "pl-op.h"
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

static bool	addSuperModule_no_lock(Module m, Module s, int where);
static void	unallocModule(Module m);
static void	unlinkSourceFilesModule(Module m);

static void
unallocProcedureSymbol(table_key_t name, table_value_t value)
{ DEBUG(MSG_CLEANUP,
	Sdprintf("unallocProcedure(%s)\n", functorName((functor_t)name)));
  unallocProcedure(val2ptr(value));
}


		 /*******************************
		 *	      MODULE GC		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Module garbage collection

Modules created at runtime, e.g., by call(M:G) for some new M, are never
removed.  Module GC reclaims modules that are empty and unreferenced.
It is modelled after procedure GC (see pl-proc.c):

  - Kernel code may keep a module pointer it found, as it always could.
    Lookups that do not count the reference, such as lookupModule() and
    isCurrentModule(), therefore pin the module (MG_PINNED): it is
    never collected.  So do PL_context(), stripModule() for the context
    module, the compiler if it needs the module in the code and all
    other ways for foreign code to get a module.
  - Code that only needs a module for a while uses a counted reference
    (module->gc_references) using acquireModuleRef() and
    releaseModuleRef().  Module enumeration (advanceModuleEnum()) does
    so for the current module.  A missed path pins the module, i.e., it
    only stops collection.
  - The meta-call (resolveVolatileModule()) protects an unpinned module
    using the thread's hazard pointer LD->modules.hazard, which remains
    until the thread resolves the next module.  By then, the frame of
    the meta-call is pushed.  This frame references the module as its
    context or through its predicate, which keeps the module alive.
  - module->definitions counts the definitions that belong to the
    module.  These are freed by clause and procedure GC.  Lingering
    definitions (see lingerDefinition()) are not counted; they are
    freed with the module.  An unpinned module is a candidate for
    collection when it is created, when its last definition is freed
    and when it is abolished using abolish_module/1.  Candidates are
    kept in GD->modules.gc.candidates (MG_CANDIDATE).
  - Source files that list a module (sf->modules) hold a pointer to it,
    counted in module->source_refs.  abolish_module/1 removes these.
  - Temporary modules (see in_temporary_module/3) are pinned while they
    are used, but these pins are soft (MG_TEMPORARY): references to
    them are counted as if they are not pinned, and '$destroy_module'/1
    abolishes the module and drops the pins of the module and its
    procedures.  See destroyTemporaryModule().
  - collectVolatileModules() runs as part of the clause GC pass after
    procedure GC, under GD->clauses.cgc_active and
    GD->modules.gc.mutex.  It sets MG_DESTROY on unreferenced empty
    candidates, then, for each thread, clears MG_DESTROY for the hazard,
    the source and typein modules, the context modules of the frames and
    the modules of lingering definitions referenced by frames.  It
    unlinks and frees the remaining ones.

The hazard or reference is set before checking MG_DESTROY, while the
collector sets MG_DESTROY before checking the hazards and references.
A lookup that sees MG_DESTROY waits for the collector by locking the
mutex and retries.

The struct module of a collected module is not freed, so a lock-free
lookup that read it from the table or its cache just before it was
unlinked does not access freed memory.  Lookups validate an unpinned
module by checking MG_DESTROY and its name (see createModule()).
Collected modules are reused for new modules from the next collection
onwards.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MODULE_GC_MIN_CREATED 1000	/* see considerModuleGC() */
#define MODULE_GC_BACKLOG     4000	/* see destroyedTemporaryModule() */
#define STORE_SEQ_CST(var, val) PUBLISH_HAZARD(var, val) /* atomic store */

typedef struct module_candidate
{ Module	module;
  struct module_candidate *next;
} module_candidate, *ModuleCandidate;

#define NEXT_FREE_MODULE(m) (*(Module*)&(m)->lingering)

static inline void
waitModuleGC(void)
{ simpleMutexLock(&GD->modules.gc.mutex);
  simpleMutexUnlock(&GD->modules.gc.mutex);
}

/* Module GC is enabled if the flag gc_code is `module`.  If it is
   disabled, modules are created pinned and temporary modules are not
   made MG_TEMPORARY, i.e., '$destroy_module'/1 only abolishes them.
*/

static inline bool
moduleGCEnabled(void)
{ return gcCodeLevel() >= GC_CODE_MODULE;
}

/* A permanent module is pinned and not temporary.  References to it
   are not counted.  The pins of a temporary module are dropped by
   '$destroy_module'/1, so references to these are counted.  As a
   module never stops being permanent, a reference counted while the
   module was not permanent may be released after it became permanent
   without decrementing: this only keeps the count positive for a
   module that is never collected anyway.
*/

static inline bool
isPermanentFlags(unsigned int flags)
{ return (flags&(MG_PINNED|MG_TEMPORARY)) == MG_PINNED;
}

static inline bool
isPermanentModule(const Module m)
{ return isPermanentFlags(LOAD_SEQ_CST(m->gc_flags));
}

static void
pushModuleCandidates(ModuleCandidate head, ModuleCandidate tail)
{ ModuleCandidate o;

  do
  { o          = LOAD_SEQ_CST(GD->modules.gc.candidates);
    tail->next = o;
  } while( !COMPARE_AND_SWAP_PTR(&GD->modules.gc.candidates, o, head) );
}

static ModuleCandidate
takeModuleCandidates(void)
{ ModuleCandidate list;

  do
  { list = LOAD_SEQ_CST(GD->modules.gc.candidates);
  } while( !COMPARE_AND_SWAP_PTR(&GD->modules.gc.candidates, list, NULL) );

  return list;
}

static void
considerModuleGC(void)
{ GET_LD
  size_t created = ATOMIC_INC(&GD->modules.gc.created);

  if ( created >= MODULE_GC_MIN_CREATED &&
       created >= (size_t)GD->statistics.modules/4 &&
       !isSignalledGCThread(SIG_CLAUSE_GC) )
    signalGCThread(SIG_CLAUSE_GC);
}

/* Called after destroying a temporary module.  Temporary modules are
   reclaimed by the clause GC pass.  If threads destroy temporary
   modules faster than the gc thread can reclaim them, memory usage
   grows without bounds.  Therefore, if many temporary modules were
   destroyed since the last pass, the destroying thread runs the pass
   itself or waits for the running pass to complete.
*/

static void
destroyedTemporaryModule(void)
{ size_t destroyed = ATOMIC_INC(&GD->modules.gc.destroyed);

  if ( destroyed >= MODULE_GC_MIN_CREATED )
  { GET_LD

    if ( !isSignalledGCThread(SIG_CLAUSE_GC) )
      signalGCThread(SIG_CLAUSE_GC);
    if ( destroyed >= MODULE_GC_BACKLOG )
    { pl_garbage_collect_clauses();
      while( LOAD_SEQ_CST(GD->clauses.cgc_active) &&
	     LOAD_SEQ_CST(GD->modules.gc.destroyed) >= MODULE_GC_BACKLOG )
	Pause(0.0002);
    }
  }
}

/* Make `m` a candidate for module GC if it is not pinned.  Called when
   an unpinned module is created, when its last definition is freed
   and by abolish_module/1.
*/

void
moduleGCCandidate(Module m)
{ unsigned int flags;

  do
  { flags = LOAD_SEQ_CST(m->gc_flags);
    if ( (flags&(MG_PINNED|MG_CANDIDATE)) )
      return;
  } while( !COMPARE_AND_SWAP_UINT(&m->gc_flags, flags, flags|MG_CANDIDATE) );

  ModuleCandidate cell = allocHeapOrHalt(sizeof(*cell));
  cell->module = m;
  pushModuleCandidates(cell, cell);
}

/* Find module `name` without protecting it.  The result may only be
   used for pinning or protecting it.
*/

#define findModule(name) LDFUNC(findModule, name)
static inline Module
findModule(DECL_LD atom_t name)
{ unsigned int i = (unsigned int)(indexAtom(name) & (MODULE_CACHE_SIZE-1));
  unsigned int gen;
  Module m;

  if ( LD->modules.cache.name[i] == name )
  { m = LD->modules.cache.module[i];
    if ( likely(LD->modules.cache.generation ==
		LOAD_SEQ_CST(GD->modules.generation)) )
      return m;
  }

  gen = LOAD_SEQ_CST(GD->modules.generation);
  if ( LD->modules.cache.generation != gen )
  { memset(LD->modules.cache.name, 0, sizeof(LD->modules.cache.name));
    LD->modules.cache.generation = gen;
  }

  if ( (m = lookupHTableWP(GD->tables.modules, name)) )
  { LD->modules.cache.name[i]   = name;
    LD->modules.cache.module[i] = m;
  }

  return m;
}

/* Pin `m`, found as `name`.  Returns the (possibly re-looked-up) module
   or NULL if it no longer exists.
*/

static Module
pinModule(atom_t name, Module m)
{ GET_LD

  for(;;)
  { unsigned int flags = LOAD_SEQ_CST(m->gc_flags);

    if ( (flags&MG_PINNED) )
    { return m;
    } else if ( (flags&MG_DESTROY) )
    { waitModuleGC();
    } else if ( COMPARE_AND_SWAP_UINT(&m->gc_flags, flags, flags|MG_PINNED) )
    { if ( lookupHTableWP(GD->tables.modules, name) == m )
	return m;
					/* stale; we pinned a reused one */
    } else
    { continue;
    }

    if ( !(m = lookupHTableWP(GD->tables.modules, name)) )
      return NULL;
  }
}

/* Pin `m`, which the caller has protected, e.g., because it is the
   context module of a frame.
*/

Module
pinModulePtr(Module m)
{ for(;;)
  { unsigned int flags = LOAD_SEQ_CST(m->gc_flags);

    if ( (flags&MG_PINNED) )
      return m;
    if ( (flags&MG_DESTROY) )
      waitModuleGC();
    else if ( COMPARE_AND_SWAP_UINT(&m->gc_flags, flags, flags|MG_PINNED) )
      return m;
  }
}

/* Find module `name` and protect it against module GC without pinning
   it:

     - MP_HAZARD: publish it in the thread's hazard pointer.
     - MP_REF: add a counted reference.  If `ref` is set, the caller
       must call releaseModuleRef(*ref).  Pinned modules get no
       reference; *ref is NULL for these.
*/

#define MP_HAZARD 0
#define MP_REF    1

#define protectModule(name, how, ref) LDFUNC(protectModule, name, how, ref)
#define protectModule_(name, how, ref) LDFUNC(protectModule_, name, how, ref)
static Module protectModule_(DECL_LD atom_t name, int how, Module *ref);

static inline Module
protectModule(DECL_LD atom_t name, int how, Module *ref)
{ Module m = findModule(name);

  if ( likely(m && isPermanentFlags(m->gc_flags)) )
  { if ( ref )
      *ref = NULL;
    return m;
  }

  return protectModule_(name, how, ref);
}

static Module
protectModule_(DECL_LD atom_t name, int how, Module *ref)
{ Module m;

  while( (m = findModule(name)) )
  { if ( isPermanentModule(m) )
    { if ( ref )
	*ref = NULL;
      return m;
    }
    if ( how == MP_HAZARD )
      PUBLISH_HAZARD(LD->modules.hazard, m);
    else
      ATOMIC_INC(&m->gc_references);	/* seq_cst; see collector */
    if ( !(LOAD_SEQ_CST(m->gc_flags)&MG_DESTROY) &&
	 m->name == name )		/* not reused; see createModule() */
    { if ( ref )
	*ref = m;
      return m;
    }
    if ( how == MP_REF )
      ATOMIC_DEC(&m->gc_references);
    waitModuleGC();
  }

  return NULL;
}

void
releaseModuleRef(Module ref)
{ if ( ref )
    ATOMIC_DEC(&ref->gc_references);
}

/* Allocate a new module.  Collected modules are reused from the next
   collection onwards (see module GC).
*/

static Module
allocModule(void)
{ Module m;

  simpleMutexLock(&GD->modules.gc.free_mutex);
  if ( (m=GD->modules.gc.free) )
    GD->modules.gc.free = NEXT_FREE_MODULE(m);
  simpleMutexUnlock(&GD->modules.gc.free_mutex);
  if ( !m )
  { m = allocHeapOrHalt(sizeof(struct module));
    m->gc_references = 0;
  }

  return m;
}

/* Create module `name`.  Must be called with L_MODULE held after
   verifying the module does not exist.  If `pinned` is false, the
   module is created unpinned and is protected as described with
   protectModule().

   (*) A thread may still hold a pointer to a reused module, read
   before it was collected.  protectModule() validates such a pointer
   by checking MG_DESTROY and the name.  We therefore keep MG_DESTROY
   set until the module is initialised.  As such a thread may also
   increment and decrement gc_references, we do not reset these.
*/

#define _lookupModule(name) LDFUNC(_lookupModule, name)
#define createModule(name, pinned, how, ref) \
	LDFUNC(createModule, name, pinned, how, ref)

static Module
createModule(DECL_LD atom_t name, unsigned int pinned, int how, Module *ref)
{ Module m, super;

  m = allocModule();			/* See (*) */
  m->gc_flags = MG_DESTROY;
  memset(m, 0, offsetof(struct module, gc_flags));
  m->definitions = 0;

  DEBUG(MSG_CREATE_MODULE,
	{ Sdprintf("Created module %s at %p\n",
		   PL_atom_chars(name), m);
	});

  m->name = name;
#ifdef O_PLMT
  m->mutex = allocSimpleMutex(PL_atom_chars(m->name));
#endif
  set(m, M_CHARESCAPE);
  if ( !GD->options.traditional )
    set(m, DBLQ_STRING|BQ_CODES|O_RATIONAL_SYNTAX);

  if ( name == ATOM_user || name == ATOM_system )
    m->procedures = newHTableWP(PROCEDUREHASHSIZE);
  else
    m->procedures = newHTableWP(MODULEPROCEDUREHASHSIZE);
  m->procedures->free_symbol = unallocProcedureSymbol;

  m->public = newHTableWP(PUBLICHASHSIZE);
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

  if ( pinned || ison(m, M_SYSTEM) )
  { STORE_SEQ_CST(m->gc_flags, MG_PINNED|(pinned&MG_TEMPORARY));
    if ( ref )
      *ref = NULL;
  } else
  { if ( how == MP_HAZARD )
    { PUBLISH_HAZARD(LD->modules.hazard, m);
    } else
    { ATOMIC_INC(&m->gc_references);
      if ( ref )
	*ref = m;
    }
    STORE_SEQ_CST(m->gc_flags, MG_CANDIDATE);
  }

  addNewHTableWP(GD->tables.modules, name, m);
  ATOMIC_INC(&GD->statistics.modules);
  PL_register_atom(name);

  if ( !(m->gc_flags&MG_PINNED) )
  { ModuleCandidate cell = allocHeapOrHalt(sizeof(*cell));

    cell->module = m;
    pushModuleCandidates(cell, cell);
    considerModuleGC();
  }

  return m;
}

/* Find or create module `name` for set_module(name:class(temporary)).
   If the module is new or not pinned, it is pinned as a temporary
   module (see destroyTemporaryModule()).  Otherwise it is returned as
   is.  Must be called with L_MODULE held.
*/

static Module _lookupModule(DECL_LD atom_t name);

#define _lookupTemporaryModule(name) LDFUNC(_lookupTemporaryModule, name)
static Module
_lookupTemporaryModule(DECL_LD atom_t name)
{ Module m;

  if ( !moduleGCEnabled() )
    return _lookupModule(name);

  while( (m = lookupHTableWP(GD->tables.modules, name)) )
  { unsigned int flags = LOAD_SEQ_CST(m->gc_flags);

    if ( (flags&MG_PINNED) )
      return m;
    if ( (flags&MG_DESTROY) )
      waitModuleGC();
    else if ( COMPARE_AND_SWAP_UINT(&m->gc_flags, flags,
				    flags|MG_PINNED|MG_TEMPORARY) )
      return m;
  }

  return createModule(name, MG_PINNED|MG_TEMPORARY, MP_REF, NULL);
}

/* Find or create module `name`, pinning it.  Must be called with
   L_MODULE held.  If the module is being collected, we wait for the
   collector, which does not use L_MODULE.
*/

static Module
_lookupModule(DECL_LD atom_t name)
{ Module m;

  while( (m = lookupHTableWP(GD->tables.modules, name)) )
  { unsigned int flags = LOAD_SEQ_CST(m->gc_flags);

    if ( (flags&MG_PINNED) )
      return m;
    if ( (flags&MG_DESTROY) )
      waitModuleGC();
    else if ( COMPARE_AND_SWAP_UINT(&m->gc_flags, flags, flags|MG_PINNED) )
      return m;
  }

  return createModule(name, MG_PINNED, MP_REF, NULL);
}


Module
lookupModule(DECL_LD atom_t name)
{ Module m;

  if ( (m = isCurrentModule(name)) )
    return m;

  PL_LOCK(L_MODULE);
  m = _lookupModule(name);
  PL_UNLOCK(L_MODULE);

  return m;
}


/* Find an existing module and pin it.  As meta calls look up modules by
 * name, each thread caches the most recent lookups.  The cache is
 * flushed if a module is collected (see destroyVolatileModule()).
 */

Module
isCurrentModule(DECL_LD atom_t name)
{ Module m = findModule(name);

  if ( m && unlikely(!(m->gc_flags&MG_PINNED)) )
    m = pinModule(name, m);

  return m;
}


/* Find module `name`, adding a counted reference if it is not pinned.
   The caller must call releaseModuleRef(*ref).  Does not create the
   module.
*/

Module
acquireModuleRef(DECL_LD atom_t name, Module *ref)
{ return protectModule(name, MP_REF, ref);
}


/* Find or create module `name` without pinning it, protected as
   described with protectModule().
*/

#define volatileModule(name, how, ref) LDFUNC(volatileModule, name, how, ref)
static inline Module
volatileModule(DECL_LD atom_t name, int how, Module *ref)
{ Module m;

  for(;;)
  { if ( (m = protectModule(name, how, ref)) )
      return m;

    PL_LOCK(L_MODULE);
    if ( !lookupHTableWP(GD->tables.modules, name) )
      m = createModule(name, moduleGCEnabled() ? 0 : MG_PINNED, how, ref);
    PL_UNLOCK(L_MODULE);
    if ( m )
      return m;
  }
}

/* Find or create module `name` for the meta-call.  An unpinned module
   is protected by the thread's hazard pointer until the thread
   resolves the next module.  The caller must push a frame that refers
   to the module before that.
*/

Module
resolveVolatileModule(DECL_LD atom_t name)
{ return volatileModule(name, MP_HAZARD, NULL);
}

/* As lookupModule(), but if the module is not pinned, add a counted
   reference rather than pinning it.  The caller must call
   releaseModuleRef(*ref).
*/

Module
lookupModuleRef(DECL_LD atom_t name, Module *ref)
{ return volatileModule(name, MP_REF, ref);
}


/* acquireModule() finds module `name` and protects it for a while
   against module GC, including temporary modules that are destroyed
   using '$destroy_module'/1.  acquireModulePtr() protects a module the
   caller already protects.  Both must be balanced by releaseModule().
   This is used by current_op/3, current_predicate/1 and the profiler.
   Unlike lookupModule(), this does not create the module.
*/

Module
acquireModule(DECL_LD atom_t name)
{ Module ref;

  return acquireModuleRef(name, &ref);
}

/* Also used for the modules in the code of clauses, e.g., from
   I_CONTEXT and I_CALLM.  See register_clause_symbols() and
   freeClause().
*/

void
acquireModulePtr(Module m)
{ if ( m && !isPermanentModule(m) )
    ATOMIC_INC(&m->gc_references);
}

void
releaseModule(Module m)
{ if ( m && !isPermanentModule(m) )
    ATOMIC_DEC(&m->gc_references);
}

/* Release the reference of a clause to `m`.  When reclaiming memory at
   halt, modules may be freed before the clauses that refer to them.
*/

void
releaseModuleCodeRef(Module m)
{ if ( GD->halt.cleaning != CLN_DATA )
    releaseModule(m);
}


ModuleEnum
newModuleEnum(int flags)
{ ModuleEnum en = malloc(sizeof(*en));

  if ( en )
  { if ( (en->tenum = newTableEnumWP(GD->tables.modules)) )
    { en->gc_ref = NULL;
      en->flags = flags;
    } else
    { free(en);
      en = NULL;
    }
  }

  return en;
}

Module
advanceModuleEnum(ModuleEnum en)
{ table_value_t v;
  Module m = NULL;

  PL_LOCK(L_MODULE);
  for(;;)
  { if ( advanceTableEnum(en->tenum, NULL, &v) )
      m = val2ptr(v);
    else
      m = NULL;

    if ( m && m->class == ATOM_temporary && !(en->flags&MENUM_TEMP) )
      continue;
					/* see module GC */
    if ( m && !isPermanentModule(m) )
    { ATOMIC_INC(&m->gc_references);
      if ( (LOAD_SEQ_CST(m->gc_flags)&MG_DESTROY) )
      { ATOMIC_DEC(&m->gc_references);
	continue;
      }
      releaseModuleRef(en->gc_ref);
      en->gc_ref = m;
    }

    break;
  }
  PL_UNLOCK(L_MODULE);

  return m;
}

void
freeModuleEnum(ModuleEnum en)
{ freeTableEnum(en->tenum);
  releaseModuleRef(en->gc_ref);
  free(en);
}


static void
unallocModuleSymbol(table_key_t name, table_value_t value)
{ (void)name;
  unallocModule(val2ptr(value));
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

    simpleMutexInit(&GD->modules.gc.mutex);
    simpleMutexInit(&GD->modules.gc.free_mutex);
    initProcedureGC();			/* before creating procedures */
    GD->tables.modules = newHTableWP(MODULEHASHSIZE);
    GD->tables.modules->free_symbol = unallocModuleSymbol;
    GD->modules.system = _lookupModule(ATOM_system);
    GD->modules.user   = _lookupModule(ATOM_user);
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
    if ( isoff(def, P_LOCALISED) && def->module )
      ATOMIC_INC(&def->module->definitions); /* see lingerDefinition() */
    unallocDefinition(def);
    freeHeap(c, sizeof(*c));
  }
}


static void
unallocModule(Module m)
{ GET_LD

  DEBUG(MSG_CREATE_MODULE, Sdprintf("unallocModule(%s) at %p\n",
				    PL_atom_chars(m->name), m));

#ifdef O_PLMT
  if ( LD )
#endif
  { if ( LD->modules.source == m ) LD->modules.source = MODULE_user;
    if ( LD->modules.typein == m ) LD->modules.typein = MODULE_user;
  }

  if ( m->public )     destroyHTableWP(m->public);
  if ( m->procedures )
  { purgeProcedureCandidates(m);
    destroyHTableWP(m->procedures);
  }
  if ( m->operators )  destroyHTableWP(m->operators);
  if ( m->supers )     unallocList(m->supers);
#ifdef O_PLMT
  if ( m->mutex )      freeSimpleMutex(m->mutex);
  if ( m->wait )       free_wait_area(m->wait);
#endif
  if ( m->lingering )  freeLingeringDefinitions(m->lingering);

  freeHeap(m, sizeof(*m));
}


/* An unpinned module is empty if it is in the state of a new module, so
   we can collect it.  Most of these properties can only be changed
   through paths that pin the module.  Lingering definitions (see
   lingerDefinition()) are not counted in m->definitions and are freed
   with the module.
*/

static bool
isEmptyModule(const Module m)
{ return ( LOAD_SEQ_CST(m->definitions) == 0 &&
	   m->procedures->size == 0 &&
	   m->public->size == 0 &&
	   !hasOperatorsModule(m) &&
	   !m->file &&
	   LOAD_SEQ_CST(m->source_refs) == 0 &&
#ifdef O_PLMT
	   !m->wait &&
#endif
#ifdef O_PROLOG_HOOK
	   !m->hook &&
#endif
	   (m->class == ATOM_user || m->class == ATOM_temporary) &&
	   isoff(m, M_SYSTEM) &&
	   m->supers && !m->supers->next &&
	   m->supers->value == MODULE_user );
}

/* `marking` maps the marked candidates and their lingering definitions
   to their ModuleCandidate.  `ptr` is either.
*/

#define keepVolatileModule(marking, ptr) LDFUNC(keepVolatileModule, marking, ptr)
static inline void
keepVolatileModule(DECL_LD TablePP marking, void *ptr)
{ ModuleCandidate c;

  if ( ptr && (c=lookupHTablePP(marking, ptr)) )
    ATOMIC_AND(&c->module->gc_flags, ~MG_DESTROY);
}

/* Clear MG_DESTROY for the modules that `ld` may use: its hazard, its
   source and typein modules, the context modules of its frames and the
   modules of lingering definitions referenced by its frames and
   predicate references.  The hazard must be scanned before the stack.
*/

static void
markVolatileModules(PL_local_data_t *ld, void *ctx)
{ GET_LD
  TablePP marking = ctx;

  if ( !ld->magic )
    return;				/* thread is exiting */

  keepVolatileModule(marking, LOAD_SEQ_CST(ld->modules.hazard));
  keepVolatileModule(marking, ld->modules.source);
  keepVolatileModule(marking, ld->modules.typein);

  Word lbase = (Word)ld->stacks.local.base;
  Word lend  = (Word)ld->stacks.local.top;	/* see markPredicatesInEnvironments() */
  if ( (Word)ld->stacks.local.max - lend > 2*ARGOFFSET/(int)sizeof(word) )
    lend++;
  for(Word current = lbase; current < lend; current++)
  { LocalFrame fr = (LocalFrame)current;

    if ( isFrame(fr) )
    { keepVolatileModule(marking, fr->context);
      keepVolatileModule(marking, fr->predicate);
    }
  }

  definition_refs *refs = &ld->predicate_references;
  for(size_t i=1; i<=refs->top; i++)
  { volatile definition_ref *drefp = &refs->blocks[MSB(i)][i];

    keepVolatileModule(marking, drefp->predicate);
  }
}

/* Unlink `m` and free its resources.  The struct is reused for new
   modules from the next collection onwards.
*/

#define destroyVolatileModule(m) LDFUNC(destroyVolatileModule, m)
static void
destroyVolatileModule(DECL_LD Module m)
{ Module old = deleteHTableWP(GD->tables.modules, m->name);

  assert(old == m);
  (void)old;
  ATOMIC_INC(&GD->modules.generation);	/* flush module caches */
  DEBUG(MSG_CREATE_MODULE, Sdprintf("Collected module %s at %p\n",
				    PL_atom_chars(m->name), m));

  assert(m->procedures->size == 0);	/* so no procedure candidates */
  destroyHTableWP(m->procedures);
  destroyHTableWP(m->public);
  if ( m->operators )			/* only cleared entries */
  { destroyHTableWP(m->operators);
    m->operators = NULL;
  }
  unallocList(m->supers);
  if ( m->lingering )
  { freeLingeringDefinitions(m->lingering);
    m->lingering = NULL;
  }
#ifdef O_PLMT
  freeSimpleMutex(m->mutex);
#endif
  ATOMIC_DEC(&GD->statistics.modules);
  PL_unregister_atom(m->name);

  m->procedures = NULL;
  m->public     = NULL;
  m->supers     = NULL;
#ifdef O_PLMT
  m->mutex      = NULL;
#endif
  NEXT_FREE_MODULE(m) = GD->modules.gc.pending;
  GD->modules.gc.pending = m;
}

/* After setting MG_DESTROY, check that no counted reference exists.
   This pairs with protectModule(), which increments the references
   before checking MG_DESTROY.  If we keep it, clear MG_DESTROY.
*/

static bool
keepReferencedModule(Module m)
{ if ( LOAD_SEQ_CST(m->gc_references) > 0 )
  { ATOMIC_AND(&m->gc_flags, ~MG_DESTROY);
    return true;
  }

  return false;
}

/* Called from pl_garbage_collect_clauses() after procedure GC while
   holding GD->clauses.cgc_active.  Returns the number of collected
   modules.

   Candidates that are pinned or have definitions are dropped.  If the
   last definition is freed, the module becomes a candidate again (see
   unallocDefinition()).  (*) If another thread freed it after we
   checked, that thread saw MG_CANDIDATE, so we re-add the module.
   Candidates that are not empty for other reasons can only become
   empty using abolish_module/1, which makes them a candidate again.
   Candidates that are referenced are kept.

   (**) A thread may have added a procedure or definition to the module
   after we tested isEmptyModule() and before we set MG_DESTROY, while
   holding a hazard or reference that it released before we checked
   them.  For example, a meta-call creates an undefined procedure and
   assertz/1 adds a clause.  Its update precedes its release, so we see
   it by testing again after marking.  Updates by threads that still
   hold a hazard or reference clear MG_DESTROY.
*/

size_t
collectVolatileModules(void)
{ GET_LD
  size_t collected = 0;
  ModuleCandidate list, keep = NULL, keep_tail = NULL, marked = NULL, c, next;
  Module m;

  STORE_SEQ_CST(GD->modules.gc.destroyed, 0);
  if ( !LOAD_SEQ_CST(GD->modules.gc.candidates) ||
       !moduleGCEnabled() )
    return 0;

  simpleMutexLock(&GD->modules.gc.mutex);
  STORE_SEQ_CST(GD->modules.gc.created, 0);
  if ( (m=GD->modules.gc.pending) )	/* make pending available */
  { Module tail = m;

    while( NEXT_FREE_MODULE(tail) )
      tail = NEXT_FREE_MODULE(tail);
    GD->modules.gc.pending = NULL;
    simpleMutexLock(&GD->modules.gc.free_mutex);
    NEXT_FREE_MODULE(tail) = GD->modules.gc.free;
    GD->modules.gc.free = m;
    simpleMutexUnlock(&GD->modules.gc.free_mutex);
  }

  list = takeModuleCandidates();
  TablePP marking = newHTablePP(64);
  for(c=list; c; c=next)
  { unsigned int flags;

    m    = c->module;
    next = c->next;
    flags = LOAD_SEQ_CST(m->gc_flags);

    if ( !(flags&MG_PINNED) &&
	 LOAD_SEQ_CST(m->gc_references) == 0 &&
	 isEmptyModule(m) &&
	 COMPARE_AND_SWAP_UINT(&m->gc_flags, flags, flags|MG_DESTROY) &&
	 !keepReferencedModule(m) )
    { addNewHTablePP(marking, m, c);
      for(ListCell l = m->lingering; l; l = l->next)
	addNewHTablePP(marking, l->value, c);
      c->next = marked;
      marked = c;
    } else if ( ( (flags=LOAD_SEQ_CST(m->gc_flags))&MG_PINNED ||
		  LOAD_SEQ_CST(m->definitions) > 0 ||
		  !isEmptyModule(m) ) &&
		COMPARE_AND_SWAP_UINT(&m->gc_flags, flags,
				      flags&~MG_CANDIDATE) )
    { freeHeap(c, sizeof(*c));		/* pinned or not empty */
      if ( isEmptyModule(m) )
	moduleGCCandidate(m);		/* last definition was freed (*) */
    } else
    { if ( !keep_tail ) keep_tail = c;
      c->next = keep;
      keep = c;
    }
  }

  if ( marking->size > 0 )
  { MEMORY_BARRIER();
    markVolatileModules(LD, marking);
#ifdef O_PLMT
    forThreadLocalDataUnsuspended(markVolatileModules, marking);
#endif

    for(c=marked; c; c=next)
    { m    = c->module;
      next = c->next;

      if ( (LOAD_SEQ_CST(m->gc_flags)&MG_DESTROY) &&
	   isEmptyModule(m) )		/* see (**) */
      { destroyVolatileModule(m);
	freeHeap(c, sizeof(*c));
	collected++;
      } else
      { ATOMIC_AND(&m->gc_flags, ~MG_DESTROY);
	if ( !keep_tail ) keep_tail = c;
	c->next = keep;
	keep = c;
      }
    }
  }
  destroyHTablePP(marking);

  if ( keep )
    pushModuleCandidates(keep, keep_tail);

  GD->modules.gc.collected += collected;
  simpleMutexUnlock(&GD->modules.gc.mutex);

  DEBUG(MSG_CGC, Sdprintf("Module GC: collected %zd\n", collected));

  return collected;
}

/* Called by cleanupModules() at halt */

static void
cleanupModuleGC(void)
{ ModuleCandidate c, next;
  Module m;

  for(c=takeModuleCandidates(); c; c=next)
  { next = c->next;
    freeHeap(c, sizeof(*c));
  }
  while( (m=GD->modules.gc.pending) )
  { GD->modules.gc.pending = NEXT_FREE_MODULE(m);
    freeHeap(m, sizeof(*m));
  }
  while( (m=GD->modules.gc.free) )
  { GD->modules.gc.free = NEXT_FREE_MODULE(m);
    freeHeap(m, sizeof(*m));
  }
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

We also remove `m` from the modules of the source files.  As these hold
a pointer to the module, module GC does not collect modules that are
listed by a source file (m->source_refs).

The procedures are enumerated using acquireTableProcedure(): procedure
GC reuses the `source_no` field of a collected procedure to link it into
its free list.  A source file may be registered after we computed
`high`; in that case we mark all files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
markSourceFilesProcedure(Procedure proc, struct bit_vector *v)
{ size_t no = proc->source_no;

  if ( isoff(proc, PROC_MULTISOURCE) && no < v->size )
    set_bit(v, no);
  else
    setall_bitvector(v);
}


static void
unlinkSourceFilesModule(Module m)
{ size_t i, high = highSourceFileIndex();
  struct bit_vector *vec = new_bitvector(high+1);
  SourceFile sf;
  TableEnum e = newTableEnumWP(m->procedures);
  table_key_t name;
  table_value_t value;

  while( advanceTableEnum(e, &name, &value) )
  { Procedure proc = val2ptr(value);

    if ( acquireTableProcedure(m, (functor_t)name, proc) )
    { markSourceFilesProcedure(proc, vec);
      releaseProcedureRef(proc);
    }
  }
  freeTableEnum(e);

  for(i=1; i<=high; i++)
  { if ( true_bit(vec, i) )
    { SourceFile sf = acquireIndexSourceFile(i);

      if ( sf )
      { unlinkSourceFileModule(sf, m);
	releaseSourceFile(sf);
      }
    }
  }

  free_bitvector(vec);
					/* files without procedures for m */
  for(i=1; i<=high && LOAD_SEQ_CST(m->source_refs) > 0; i++)
  { SourceFile sf = acquireIndexSourceFile(i);

    if ( sf )
    { if ( sf->modules )
	unlinkSourceFileModule(sf, m);
      releaseSourceFile(sf);
    }
  }

  if ( (sf = m->file) )
  { m->file = NULL;
    releaseSourceFile(sf);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
abolishModule() implements abolish_module/1.  It resets the module to
the state of a new module:

  - All predicates are abolished, including thread-local predicates.
    Their clauses are reclaimed by clause GC, and unpinned procedures
    by procedure GC.  Import links are unpinned and replaced by
    undefined stubs, so procedure GC reclaims them and their functors.
    Predicates imported into other modules become undefined.
  - Exports and local operators are removed.
  - The import modules are reset to `user`.
  - Links from source files are removed.
  - Class and flags are reset.

The module keeps its identity.  This is safe while code of the module
is running and if clauses of other modules refer to its predicates.

If `unpin` is true, we are called by '$destroy_module'/1 for a temporary
module and also drop the pins of its procedures (see unpinProcedure()).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define abolishProceduresModule(m, unpin) \
	LDFUNC(abolishProceduresModule, m, unpin)
static void
abolishProceduresModule(DECL_LD Module m, bool unpin)
{ TableEnum e = newTableEnumWP(m->procedures);
  table_key_t name;
  table_value_t value;

  while( advanceTableEnum(e, &name, &value) )
  { Procedure proc = val2ptr(value);

    if ( isVolatileProcedure(proc) ||
	 !acquireTableProcedure(m, (functor_t)name, proc) )
      continue;
    /* Import links are created pinned.  abolish/1 leaves that pin, so
       the undefined predicate stays.  Abolishing the module drops it:
       an unpinned module can then become empty and be collected. */
    if ( unpin || proc->definition->module != m )
      unpinProcedure(proc, m);
    /* Thread-local predicates are skipped by abolish/1.  A module
       that is going away takes their clauses with it, in every engine,
       and leaves a stub procedure GC can collect once unpinned. */
    if ( proc->definition->module == m &&
	 ison(proc->definition, P_THREAD_LOCAL) )
      abolishThreadLocalProcedure(proc);
    else
      abolishProcedure(proc, m);
    releaseProcedureRef(proc);
  }
  freeTableEnum(e);
}


#define MODULE_RESET_FLAGS (M_WAITED_FOR)

#define abolishModule(m, unpin) LDFUNC(abolishModule, m, unpin)
static bool
abolishModule(DECL_LD Module m, bool unpin)
{ DEBUG(MSG_DESTROY_MODULE,
	Sdprintf("Abolishing module %s at %p\n", PL_atom_chars(m->name), m));

  unlinkSourceFilesModule(m);		/* before procedure GC may */
  abolishProceduresModule(m, unpin);	/* collect the procedures */
  clearHTableWP(m->public);
  clearOperatorsModule(m);

  PL_LOCK(L_MODULE);
  m->line_no = 0;
  if ( m->class != ATOM_temporary )
    m->class = ATOM_user;
  unsigned int flags = (m->flags&MODULE_RESET_FLAGS)|M_CHARESCAPE;
  if ( !GD->options.traditional )
    flags |= DBLQ_STRING|BQ_CODES|O_RATIONAL_SYNTAX;
  m->flags = flags;
  m->var_prefix = 0;
#ifdef O_PROLOG_HOOK
  m->hook = NULL;
#endif
  bool rc = setSuperModule(m, MODULE_user);
  m->last_modified = global_generation();
  PL_UNLOCK(L_MODULE);

  return rc;
}


static void
empty_module(table_key_t key, table_value_t value)
{ Module m = val2ptr(value);
  (void)key;

  unallocModule(m);
}


void
cleanupModules(void)
{ TableWP t;

  if ( (t=GD->tables.modules) )
  { GD->tables.modules = NULL;
    t->free_symbol = empty_module;
    destroyHTableWP(t);
  }
  cleanupModuleGC();
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


static bool
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


static bool
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



static bool
addSuperModule_no_lock(Module m, Module s, int where)
{ ListCell c;

  if ( reachableModule(s, m) )
    return cannotSetSuperModule(m, s);

  for(c=m->supers; c; c=c->next)
  { if ( c->value == s )
      return true;			/* already a super-module */
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


static bool
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


/* Make `s` the only import module of `m`.  Caller holds L_MODULE.

   (*) Other threads walk m->supers without locking, e.g., to resolve
   an undefined predicate.  If `m` has a single import module, we
   update it in place rather than freeing the cell, notably if `s` is
   already the import module, which happens for each (re)load of a
   module file.
*/

int
setSuperModule(Module m, Module s)
{ if ( s == m )
    return cannotSetSuperModule(m, s);

  if ( m->supers && !m->supers->next )	/* See (*) */
  { if ( (Module)m->supers->value != s )
    { m->supers->value = s;
      m->level = s->level+1;
    }

    succeed;
  }
  clearSupersModule_no_lock(m);

  return addSuperModule_no_lock(m, s, 'A');
}


/* True if `m` has defined procedures.  A destroyed temporary module
   only has undefined (abolished) procedures.
*/

#define hasProceduresModule(m) LDFUNC(hasProceduresModule, m)
static bool
hasProceduresModule(DECL_LD Module m)
{ if ( m->procedures->size == 0 )
    return false;

  FOR_TABLE(m->procedures, name, value)
  { Procedure proc = val2ptr(value);

    if ( !isVolatileProcedure(proc) && isDefinedProcedure(proc) )
      return true;
  }

  return false;
}

#define set_module(m, prop) LDFUNC(set_module, m, prop)
static int
set_module(DECL_LD Module m, term_t prop)
{ atom_t pname;
  size_t arity;

  if ( PL_get_name_arity(prop, &pname, &arity) && arity == 1 )
  { term_t arg = PL_new_term_ref();

    _PL_get_arg(1, prop, arg);

    if ( pname == ATOM_base )
    { atom_t mname;

      if ( !PL_get_atom_ex(arg, &mname) )
	return false;

      return setSuperModule(m, _lookupModule(mname));
    } else if ( pname == ATOM_class )
    { atom_t class;

      if ( !PL_get_atom_ex(arg, &class) )
	return false;
      if ( class == ATOM_user ||
	   class == ATOM_system ||
	   class == ATOM_library ||
	   class == ATOM_test ||
	   class == ATOM_development )
      { m->class = class;
	return true;
      } else if ( class == ATOM_temporary )
      { if ( ( m->class == ATOM_user ||
	       m->class == ATOM_temporary ) && /* destroyed, not collected */
	     !hasProceduresModule(m) )
	{ m->class = class;
	} else
	{ return PL_error(NULL, 0,
			  m->class != ATOM_user ? "Not a user module" :
						  "module is not empty",
			  ERR_PERMISSION, ATOM_module_property, ATOM_class, arg);
	}
	return true;
      } else
	return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_module_class, arg);
    } else if ( pname == ATOM_program_space )
    { size_t limit;

      if ( !PL_get_size_ex(arg, &limit) )
	return false;
      if ( limit && limit < m->code_size )
      { term_t ex = PL_new_term_ref();

	PL_put_atom(ex, m->name);
	return PL_error(NULL, 0, "Used exceeds limit", ERR_PERMISSION,
			ATOM_limit, ATOM_program_space, ex);
      }
      m->code_limit = limit;
      return true;
    } else
    { return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_module_property, prop);
    }
  } else
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_module_property, prop);
}


static bool
isTemporaryClassProperty(term_t prop)
{ GET_LD
  Word p = valTermRef(prop);

  deRef(p);
  if ( hasFunctor(*p, FUNCTOR_class1) )
  { p = argTermP(*p, 0);
    deRef(p);
    return *p == ATOM_temporary;
  }

  return false;
}

static
PRED_IMPL("set_module", 1, set_module, PL_FA_TRANSPARENT)
{ PRED_LD
  Module m;
  term_t prop = PL_new_term_ref();
  atom_t mname = 0;
  Word p;
  int rc;

  if ( !(p=stripModuleName(valTermRef(A1), &mname)) )
    return false;
  *valTermRef(prop) = linkValNoG(p);

  PL_LOCK(L_MODULE);
  if ( mname )
    m = ( isTemporaryClassProperty(prop) ? _lookupTemporaryModule(mname)
					 : _lookupModule(mname) );
  else
    m = MODULE_parse;
  rc = set_module(m, prop);
  PL_UNLOCK(L_MODULE);

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
stripModuleName() takes an atom or term,   possible  embedded in the :/2
module term. It assigns *name  with   the  associated  module names. The
return value is the plain term or NULL if `term` is cyclic.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Word
stripModuleName(DECL_LD Word term, atom_t *name)
{ int depth = 100;
  deRef(term);
  atom_t nm = 0;

  while( hasFunctor(*term, FUNCTOR_colon2) )
  { Word mp;
    mp = argTermP(*term, 0);
    deRef(mp);
    if ( !isTextAtom(*mp) )
      break;
    nm = word2atom(*mp);
    term = argTermP(*term, 1);
    deRef(term);
    if ( --depth == 0 && !is_acyclic(term) )
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
remaining term.  The module is pinned (see module GC), unless `flags`
contains SM_VOLATILE, which is used by the meta-call.  In that case the
module is protected by the thread's hazard pointer or by the frame from
which we take the context module.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Word
stripModule(DECL_LD Word term, Module *module, int flags)
{ atom_t mname = 0;
  Word rc;

  if ( (rc=stripModuleName(term, &mname)) )
  { if ( mname )
    { if ( unlikely(flags&SM_NOCREATE) )
      { Module m;

	if ( (m=isCurrentModule(mname)) )
	  *module = m;
	else
	  return NULL;
      } else if ( (flags&SM_VOLATILE) )
      { *module = resolveVolatileModule(mname);
      } else
      { Module m = findModule(mname);	/* inline fast path */

	if ( likely(m && (m->gc_flags&MG_PINNED)) )
	  *module = m;
	else
	  *module = lookupModule(mname);
      }
    } else
    { Module m = (environment_frame ? contextModule(environment_frame)
				    : MODULE_user);

      if ( unlikely(!(flags&SM_VOLATILE) &&
		    !(m->gc_flags&MG_PINNED)) )
	m = pinModulePtr(m);		/* see module GC */
      *module = m;
    }
  }

  return rc;
}


/* As stripModule(), but do not pin the module.  If the module is not
   pinned, *ref is set to the module and the caller must call
   releaseModuleRef(*ref) when done.  Otherwise *ref is NULL.  The
   context module needs no reference as it is protected by the frame.
*/

Word
stripModuleRef(DECL_LD Word term, Module *module, Module *ref, int flags)
{ atom_t mname = 0;
  Word rc;

  *ref = NULL;
  if ( (rc=stripModuleName(term, &mname)) )
  { if ( mname )
    { Module m = findModule(mname);

      if ( likely(m && isPermanentFlags(m->gc_flags)) )
      { *module = m;
      } else if ( unlikely(flags&SM_NOCREATE) )
      { if ( !(*module = acquireModuleRef(mname, ref)) )
	  return NULL;
      } else
      { *module = lookupModuleRef(mname, ref);
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
  return !!lookupHTableWP(module->public,
			  proc->definition->functor->functor);
}


		/********************************
		*       PROLOG CONNECTION       *
		*********************************/

static bool
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

  if ( !get_module(A1, &m, true) )
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

  if ( !get_module(A1, &me, true) ||
       !get_module(A2, &super, true) ||
       !PL_get_atom_ex(A3, &where) )
    fail;

  return addSuperModule(me, super, where == ATOM_start ? 'A' : 'Z');
}


static
PRED_IMPL("delete_import_module", 2, delete_import_module, 0)
{ Module me, super;
  int rval;

  if ( !get_module(A1, &me, true) ||
       !get_module(A2, &super, true) )
    fail;

  PL_LOCK(L_MODULE);
  rval = delSuperModule(me, super);
  PL_UNLOCK(L_MODULE);

  return rval;
}


#define get_existing_source_file(file, sfp) LDFUNC(get_existing_source_file, file, sfp)
static bool
get_existing_source_file(DECL_LD term_t file, SourceFile *sfp)
{ SourceFile sf;
  atom_t a;

  if ( PL_get_atom(file, &a) )
  { if ( (sf = lookupSourceFile(a, false)) )
    { *sfp = sf;
      return true;
    }

    return false;
  }

  *sfp = NULL;
  return true;
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
  ModuleEnum e;
  Module m;
  atom_t name;
  SourceFile sf = NULL;

  term_t module = A1;
  term_t file   = A2;

  switch(CTX_CNTRL)
  { case FRG_FIRST_CALL:
				/* deterministic case: module --> file */
      if ( PL_get_atom(module, &name) )
      { Module m, ref;

	if ( (m=acquireModuleRef(name, &ref)) )
	{ atom_t f = (!m->file ? ATOM_nil : m->file->name);
	  releaseModuleRef(ref);
	  return PL_unify_atom(file, f);
	}

	return false;
      }

      if ( !get_existing_source_file(file, &sf) )
	return false;			/* given, but non-existing file */

      if ( sf )
      { int rc = false;

	if ( sf->modules )
	{ PL_LOCK(L_PREDICATE);
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
	}
	releaseSourceFile(sf);

	return rc;			/* source-file has no modules */
      }

      if ( !(e = newModuleEnum(0)) )
	return PL_no_memory();
      break;
    case FRG_REDO:
      e = CTX_PTR;
      break;
    case FRG_CUTTED:
      e = CTX_PTR;
      freeModuleEnum(e);
      succeed;
    default:
      assert(0);
      return false;
  }

					/* mode (-,-) */

  while( (m=advanceModuleEnum(e)) )
  { atom_t f = ( !m->file ? ATOM_nil : m->file->name);

    if ( m->class == ATOM_system && m->name != ATOM_system &&
	 !SYSTEM_MODE && PL_is_variable(module) )
      continue;

    if ( PL_unify_atom(module, m->name) &&
	 PL_unify_atom(file, f) )
      ForeignRedoPtr(e);

    break;				/* must be an error */
  }

  freeModuleEnum(e);
  return false;
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
    return false;

  LD->modules.typein = lookupModule(name);
  return true;
}

static
PRED_IMPL("$current_source_module", 1, current_source_module, 0)
{ PRED_LD

  return PL_unify_atom(A1, LD->modules.source->name);
}

static
PRED_IMPL("$set_source_module", 1, set_source_module, 0)
{ PRED_LD
  atom_t name;

  if ( !PL_get_atom_ex(A1, &name) )
    return false;

  LD->modules.source = lookupModule(name);
  return true;
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
      return false;
  } else
  { if ( !PL_unify_nil(old) )
      return false;
  }

  if ( PL_get_nil(new) )
  { m->hook = NULL;
    return true;
  } else
    return get_procedure(new, &m->hook, 0, GP_NAMEARITY|GP_CREATE);
}
#endif


typedef struct defm_target
{ term_t    pi;				/* user supplied predicate indicator */
  functor_t functor;			/* functor associated to the above */
} defm_target;


#define find_modules_with_defs(m, count, targets, tmp, mlist, l) LDFUNC(find_modules_with_defs, m, count, targets, tmp, mlist, l)
static bool
find_modules_with_defs(DECL_LD Module m, int count, defm_target targets[],
		       term_t tmp, term_t mlist,
		       int l)
{ ListCell c;
  int i;
  int found = false;
  term_t mhead = tmp+0;
  term_t plist = tmp+1;
  term_t phead = tmp+2;

  DEBUG(9, Sdprintf("Trying %s\n", PL_atom_chars(m->name)));

  if ( l < 0 )
  { sysError("OOPS loop in default modules???\n");
    return false;
  }

  for(i=0; i<count; i++)
  { Procedure proc;

    if ( (proc = isCurrentProcedure(targets[i].functor, m)) &&
	 proc->definition->impl.any.defined )
    { if ( !found )
      { found = true;
	PL_put_variable(plist);
	if ( !PL_unify_list(mlist, mhead, mlist) ||
	     !PL_unify_term(mhead, PL_FUNCTOR, FUNCTOR_minus2,
				     PL_ATOM, m->name,
				     PL_TERM, plist) )
	  return false;
      }

      if ( !PL_unify_list(plist, phead, plist) ||
	   !PL_unify(phead, targets[i].pi) )
	return false;
    }
  }
  if ( found && !PL_unify_nil(plist) )
    return false;

  for(c = m->supers; c; c=c->next)
  { Module s = c->value;

    if ( !find_modules_with_defs(s, count, targets, tmp, mlist, l-1) )
      return false;
  }

  return true;
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

  if ( !(mp=stripModuleName(valTermRef(A1), &mname)) )
    return false;
  *valTermRef(ttail) = linkValNoG(mp);

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
      return false;
    targets[tcount].pi = PL_copy_term_ref(thead);
    tcount++;
  }
  if ( !PL_get_nil_ex(ttail) )
    return false;

  if ( !find_modules_with_defs(m, tcount, targets, tmp, tail, 100) )
    return false;

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
	      bool allow_newfile)
{ GET_LD
  Module module = lookupModule(name);
  term_t tmp = 0, rdef = 0, rtail = 0;
  int rc = true;

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
  { FOR_TABLE(module->procedures, name, value)
    { Procedure proc = val2ptr(value);
      Definition def;

      if ( isVolatileProcedure(proc) ||
	   !acquireTableProcedure(module, (functor_t)name, proc) )
	continue;
      def = proc->definition;
      if ( !ison(def, P_DYNAMIC|P_MULTIFILE|P_FOREIGN) )
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
      releaseProcedureRef(proc);
    }
    clearHTableWP(module->public);
  }
  if ( super )
    rc = setSuperModule(module, _lookupModule(super));

  PL_UNLOCK(L_MODULE);

  if ( rdef )
  { if ( !PL_unify_nil(rtail) )
      return false;

    if ( rc )
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
  int line_no;
  bool rdef;

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
       !PL_get_stdbool_ex(redefine, &rdef) )
    fail;

  sf = lookupSourceFile(fname, true);
  return declareModule(mname, cname, sname, sf, line_no, rdef);
}


#define unify_export_list(public, module) LDFUNC(unify_export_list, public, module)
static bool
unify_export_list(DECL_LD term_t public, Module module)
{ term_t head = PL_new_term_ref();
  term_t list = PL_copy_term_ref(public);
  int rval = true;

  FOR_TABLE(module->public, name, value)
  { if ( !PL_unify_list(list, head, list) ||
	 !unify_functor(head, (functor_t)name, GP_NAMEARITY) )
    { rval = false;
      break;
    }
  }
  if ( rval )
    return PL_unify_nil(list);

  fail;
}


static size_t
sizeof_module(Module m)
{ GET_LD
  size_t size = sizeof(*m);

  if ( m->public)     size += sizeofTableWP(m->public);
  if ( m->procedures) size += sizeofTableWP(m->procedures);
  if ( m->operators)  size += sizeofTableWP(m->operators);

  FOR_TABLE(m->procedures, name, value)
  { Procedure proc = val2ptr(value);
    Definition def;

    size += sizeof(*proc);
    if ( isVolatileProcedure(proc) ||
	 !acquireTableProcedure(m, (functor_t)name, proc) )
      continue;
    def = proc->definition;

    if ( def->module == m && isoff(def, P_FOREIGN) )
    { Definition def = getProcDefinition(proc);
      size += sizeof_predicate(def);
    }
    releaseProcedureRef(proc);
  }

  return size;
}



static
PRED_IMPL("$module_property", 2, module_property, 0)
{ PRED_LD
  Module m;
  term_t a = PL_new_term_ref();
  atom_t pname;
  size_t parity;

  if ( !get_module(A1, &m, false) )
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
  { return unify_export_list(a, m);
  } else if ( pname == ATOM_class )
  { return PL_unify_atom(a, m->class);
  } else if ( pname == ATOM_size )
  { return PL_unify_int64(a, sizeof_module(m));
  } else if ( pname == ATOM_program_size )
  { return PL_unify_int64(a, m->code_size);
  } else if ( pname == ATOM_last_modified_generation )
  { return PL_unify_int64(a, m->last_modified);
  } else if ( pname == ATOM_program_space )
  { if ( m->code_limit )
      return PL_unify_int64(a, m->code_limit);
    return false;
  } else
    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    ATOM_module_property, A2);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
export/1 exports a procedure specified by its name and arity or
head from the context module.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
exportProcedure(DECL_LD const Module module, const Procedure proc)
{ updateHTableWP(module->public,
		 proc->definition->functor->functor,
		 proc);

  return true;
}

#define export_pi1(pi, module) LDFUNC(export_pi1, pi, module)
static bool
export_pi1(DECL_LD term_t pi, Module module)
{ functor_t fd;
  Procedure proc;

  if ( !get_functor(pi, &fd, &module, 0, GF_PROCEDURE|GF_NAMEARITY) )
    return false;

  if ( (proc = isStaticSystemProcedure(fd)) && ison(proc->definition, P_ISO) )
    return true;
  proc = lookupProcedure(fd, module);

  if ( ReadingSource )
  { SourceFile sf = lookupSourceFile(source_file_name, true);
    bool rc = exportProcedureSource(sf, module, proc);
    releaseSourceFile(sf);
    return rc;
  } else
  { return exportProcedure(module, proc);
  }
}

#define export_pi(pi, module, depth) LDFUNC(export_pi, pi, module, depth)
static bool
export_pi(DECL_LD term_t pi, Module module, int depth)
{ if ( !PL_strip_module(pi, &module, pi) )
    return false;

  while ( PL_is_functor(pi, FUNCTOR_comma2) )
  { term_t a1 = PL_new_term_ref();

    if ( ++depth == 100 && !PL_is_acyclic(pi) )
      return PL_type_error("acyclic_term", pi);

    _PL_get_arg(1, pi, a1);
    if ( !export_pi(a1, module, depth) )
      return false;
    PL_reset_term_refs(a1);
    _PL_get_arg(2, pi, pi);
  }

  return export_pi1(pi, module);
}



static
PRED_IMPL("export", 1, export, PL_FA_TRANSPARENT)
{ PRED_LD
  Module module = NULL;

  return export_pi(A1, module, 0);
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
  table_value_t tv;
  term_t tail = PL_copy_term_ref(A2);
  term_t head = PL_new_term_ref();

  if ( !PL_get_atom_ex(A1, &mname) )
    return false;
  if ( !(module = isCurrentModule(mname)) )
    return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_module, A1);

  e = newTableEnumWP(module->public);

  while( advanceTableEnum(e, NULL, &tv) )
  { Procedure proc = val2ptr(tv);
    Definition def = proc->definition;
    FunctorDef fd = def->functor;

    if ( !isDefinedProcedure(proc) &&			/* not defined */
	 def->module == module &&			/* not imported */
	 !autoImport(fd->functor, module) )
    { if ( !PL_unify_list(tail, head, tail) ||
	   !unify_definition(MODULE_user, head, proc->definition,
			     0, GP_QUALIFY|GP_NAMEARITY) )
      { freeTableEnum(e);
	return false;
      }
    }
  }

  freeTableEnum(e);
  return PL_unify_nil(tail);
}


foreign_t
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

  FOR_TABLE(m->procedures, name, value)
  { Procedure proc = val2ptr(value);

    if ( proc->definition == old )
    { DEBUG(1, Sdprintf("Patched def of %s\n", procedureName(proc)));
      shareDefinition(new);
      proc->definition = new;
      if ( unshareDefinition(old) == 0 )
	lingerDefinition(old);
    }
  }

  UNLOCKMODULE(m);
}


static void
fixExport(Definition old, Definition new)
{ PL_LOCK(L_MODULE);		/* Otherwise tmp modules may disappear */
  FOR_TABLE(GD->tables.modules, name, value)
  { Module m = val2ptr(value);

    if ( isPermanentModule(m) )
    { fixExportModule(m, old, new);
    } else				/* see module GC */
    { ATOMIC_INC(&m->gc_references);
      if ( !(LOAD_SEQ_CST(m->gc_flags)&MG_DESTROY) )
	fixExportModule(m, old, new);
      ATOMIC_DEC(&m->gc_references);
    }
  }
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


#define import(pred, strength) LDFUNC(import, pred, strength)
static int
import(DECL_LD term_t pred, term_t strength)
{ Module source = NULL;
  Module destination = contextModule(environment_frame);
  functor_t fd;
  Procedure proc, old;
  int pflags = 0;

  if ( !get_functor(pred, &fd, &source, 0, GF_PROCEDURE) )
    return false;
  if ( strength )
  { atom_t a;

    if ( !PL_get_atom_ex(strength, &a) )
      return false;
    if ( (pflags=atomToImportStrength(a)) < 0 )
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_import_type, strength);
  }

  if ( !(proc = lookupProcedure(fd, source)) )
    return false;

  if ( !isDefinedProcedure(proc) )
    autoImport(proc->definition->functor->functor, proc->definition->module);

retry:
  if ( (old = pinCurrentProcedure(proc->definition->functor->functor,
				 destination)) )
  { LOCKMODULE(destination);
    if ( old->definition == proc->definition )
    { UNLOCKMODULE(destination);
      return true;			/* already done this! */
    }

    if ( !isDefinedProcedure(old) )
    { Definition odef = old->definition;
      int fixup = false;

      old->definition = proc->definition;
      shareDefinition(proc->definition);
      if ( unshareDefinition(odef) > 0 )
      { fixup = true;			/* delay to avoid a deadlock */
      } else
      { lingerDefinition(odef);
      }
      set(old, pflags|PROC_IMPORTED);

      UNLOCKMODULE(destination);
      if ( fixup )
	fixExport(odef, proc->definition);

      return true;
    }

    if ( old->definition->module == destination )
    { UNLOCKMODULE(destination);

      if ( (pflags & PROC_WEAK) )
      { if ( truePrologFlag(PLFLAG_WARN_OVERRIDE_IMPLICIT_IMPORT) )
	{ term_t pi = PL_new_term_ref();

	  if ( !PL_unify_predicate(pi, proc, GP_NAMEARITY) )
	    return false;

	  if ( !printMessage(ATOM_warning,
			     PL_FUNCTOR_CHARS, "ignored_weak_import", 2,
			       PL_ATOM, destination->name,
			       PL_TERM, pi) )
	    return false;
	}

	return true;
      } else
	return PL_error("import", 1, "name clash", ERR_IMPORT_PROC,
			proc, destination->name, 0);
    }

    if ( old->definition->module != source )	/* already imported */
    { UNLOCKMODULE(destination);

      return PL_error("import", 1, NULL, ERR_IMPORT_PROC,
		      proc, destination->name,
		      old->definition->module->name);
    }

    UNLOCKMODULE(destination);
    sysError("Unknown problem importing %s into module %s",
	     procedureName(proc),
	     stringAtom(destination->name));
    fail;
  }

  if ( !isPublicModule(source, proc) )
  { term_t pi = PL_new_term_ref();

    if ( !PL_unify_predicate(pi, proc, GP_NAMEARITY) )
      return false;
    if ( !printMessage(ATOM_warning,
		       PL_FUNCTOR_CHARS, "import_private", 2,
			 PL_ATOM, destination->name,
			 PL_TERM, pi) )
      return false;
  }

  { Procedure nproc = allocProcedure();	/* see procedure GC */
    Procedure old;

    nproc->flags = pflags|PROC_PINNED;
    nproc->references = 0;
    nproc->source_no = 0;
    shareDefinition(proc->definition);
    nproc->definition = proc->definition;

    old = addHTableWP(destination->procedures,
		      proc->definition->functor->functor,
		      nproc);
    if ( old != nproc )
    { int shared = unshareDefinition(proc->definition);
      assert(shared > 0);
      (void)shared;
      freeHeap(nproc, sizeof(*nproc));
      goto retry;
    }
    DEBUG(MSG_PROC_COUNT, Sdprintf("Created %s at %p\n",
				   procedureName(nproc), nproc));
  }

  return true;
}

static
PRED_IMPL("import", 1, import, PL_FA_TRANSPARENT)
{ PRED_LD

  return import(A1, 0);
}

static
PRED_IMPL("$import", 2, import, PL_FA_TRANSPARENT)
{ PRED_LD

  return import(A1, A2);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destroyTemporaryModule() implements '$destroy_module'/1, which is used by
in_temporary_module/3.  It abolishes the module (see abolishModule()) and
leaves it to procedure and module GC to reclaim the procedures and the
module when they are no longer referenced.

A temporary module and its procedures are pinned as usual while the
module is used, e.g., while loading code into it.  If the module was a
new or unpinned module when it became temporary (MG_TEMPORARY, see
set_module/1), references to it are counted as if it is not pinned and
destroying it drops the pins of the module and its procedures.  This
assumes that, after it is destroyed, the module is no longer used
through a pointer obtained by a path that pins it.  Pins after
destroying the module are permanent.  Clauses that refer to the module
hold a counted reference (see acquireClauseModule()).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define destroyTemporaryModule(m) LDFUNC(destroyTemporaryModule, m)
static bool
destroyTemporaryModule(DECL_LD Module m)
{ bool soft = !!(LOAD_SEQ_CST(m->gc_flags)&MG_TEMPORARY);
  bool rc = abolishModule(m, soft);

  if ( soft )
  { ATOMIC_AND(&m->gc_flags, ~(MG_PINNED|MG_TEMPORARY));
    moduleGCCandidate(m);
    destroyedTemporaryModule();
  }

  return rc;
}


/** '$destroy_module'(+Module) is det.
 *
 * Destroy the temporary module Module.  See destroyTemporaryModule().
 */

static
PRED_IMPL("$destroy_module", 1, destroy_module, 0)
{ PRED_LD
  atom_t name;

  if ( PL_get_atom_ex(A1, &name) )
  { Module m;

    if ( (m=acquireModule(name)) )
    { if ( m->class == ATOM_temporary )
      { bool rc = destroyTemporaryModule(m);

	releaseModule(m);
	return rc;
      } else
      { releaseModule(m);
	return PL_error(NULL, 0,
			"module is not temporary",
			ERR_PERMISSION, ATOM_destroy, ATOM_module, A1);
      }
    }

    return true;				/* non-existing */
  }

  return false;
}

/** '$abolish_module'(+Module) is det.
 *
 * Abolish Module.  See abolishModule().  Does nothing if Module does
 * not exist.
 */

static
PRED_IMPL("$abolish_module", 1, abolish_module, 0)
{ PRED_LD
  atom_t name;
  Module m;

  Module ref;
  bool rc;

  if ( !PL_get_atom_ex(A1, &name) )
    return false;
  if ( !(m=acquireModuleRef(name, &ref)) )
    return true;
  if ( name == ATOM_user || name == ATOM_system ||
       ison(m, M_SYSTEM) || m->class == ATOM_system )
  { releaseModuleRef(ref);
    return PL_error("abolish_module", 1, "system module",
		    ERR_PERMISSION, ATOM_abolish, ATOM_module, A1);
  }

  rc = abolishModule(m, false);
  moduleGCCandidate(m);
  releaseModuleRef(ref);

  return rc;
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
  PRED_DEF("$abolish_module", 1, abolish_module, 0)
  PRED_DEF("$current_source_module", 1, current_source_module, 0)
  PRED_DEF("$set_source_module", 1, set_source_module, 0)
  PRED_DEF("$current_typein_module", 1, current_typein_module, 0)
  PRED_DEF("$set_typein_module", 1, set_typein_module, 0)
EndPredDefs
