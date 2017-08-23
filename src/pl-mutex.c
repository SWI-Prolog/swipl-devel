/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, University of Amsterdam
                         VU University Amsterdam
		         CWI, Amsterdam
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
#include "pl-thread.h"

#undef LD
#define LD LOCAL_LD

#ifdef O_PLMT
static void	unalloc_mutex(pl_mutex *m);


		 /*******************************
		 *	    USER MUTEXES	*
		 *******************************/

typedef struct mutexref
{ pl_mutex	*mutex;
} mutexref;

static int try_really_destroy_mutex(pl_mutex *m);

static int
write_mutexref(IOSTREAM *s, atom_t aref, int flags)
{ mutexref *ref = PL_blob_data(aref, NULL, NULL);
  (void)flags;

  Sfprintf(s, "<mutex>(%p)", ref->mutex);
  return TRUE;
}


static int
release_mutexref(atom_t aref)
{ mutexref *ref = PL_blob_data(aref, NULL, NULL);
  pl_mutex *m;

  DEBUG(MSG_MUTEX_GC,
	Sdprintf("GC mutex %p\n", ref->mutex));

  if ( (m=ref->mutex) )
  { if ( !m->destroyed )
      deleteHTable(GD->thread.mutexTable, (void *)m->id);

    if ( m->owner )
    { Sdprintf("WARNING: <mutex>(%p) garbage collected "
	       "while owned by thread %d\n",
	       m, m->owner);

      if ( m->owner == PL_thread_self() )
	pthread_mutex_unlock(&m->mutex);
      else
	return TRUE;
    }

    if ( m->initialized )
      pthread_mutex_destroy(&m->mutex);
    unalloc_mutex(m);
  }

  return TRUE;
}


static int
save_mutexref(atom_t aref, IOSTREAM *fd)
{ mutexref *ref = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <mutex>(%p)", ref->mutex);
}


static atom_t
load_mutexref(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<saved-mutex-ref>");
}


static PL_blob_t mutex_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  "mutex",
  release_mutexref,
  NULL,
  write_mutexref,
  NULL,
  save_mutexref,
  load_mutexref
};


static void
initMutexRef(void)
{ mutex_blob.atom_name = ATOM_mutex;	/* avoid early initAtoms() */
  PL_register_blob_type(&mutex_blob);
}


static void
unalloc_mutex(pl_mutex *m)
{ freeHeap(m, sizeof(*m));
}


static void
destroy_mutex(pl_mutex *m)
{ if ( m->initialized )
  { m->initialized = FALSE;
    pthread_mutex_destroy(&m->mutex);
  }
  if ( !m->anonymous )
    unalloc_mutex(m);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
User-level mutexes. On Windows we can't   use  critical sections here as
TryEnterCriticalSection() is only defined on NT 4, not on Windows 95 and
friends.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
unify_mutex(term_t t, pl_mutex *m)
{ GET_LD

  return PL_unify_atom(t, m->id);
}


static int
unify_mutex_owner(term_t t, int owner)
{ if ( owner )
    return unify_thread_id(t, GD->thread.threads[owner]);
  else
    return PL_unify_nil(t);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) We must lock the atom, but   threads are initialised before the atom
infrastructure :-( Note that this is hacky, but safe: m->id is an ATOM_*
built-in atom and  needs  not  to  be   locked.  Putting  this  test  in
PL_register_atom() would be cleaner, but that  routine is much more time
critical.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static pl_mutex *
mutexCreate(atom_t name)
{ pl_mutex *m;

  if ( (m=allocHeap(sizeof(*m))) )
  { memset(m, 0, sizeof(*m));
    pthread_mutex_init(&m->mutex, NULL);
    m->initialized = TRUE;

    if ( name == NULL_ATOM )
    { mutexref ref;
      int new;

      ref.mutex = m;
      m->id = lookupBlob((void*)&ref, sizeof(ref), &mutex_blob, &new);
      m->anonymous = TRUE;
    } else
    { m->id = name;
    }

    addNewHTable(GD->thread.mutexTable, (void *)m->id, m);
    if ( m->anonymous )
      PL_unregister_atom(m->id);		/* reclaim on GC */
    else if ( GD->atoms.builtin )		/* (*) */
      PL_register_atom(m->id);
  } else
    PL_no_memory();

  return m;
}


static pl_mutex *
unlocked_pl_mutex_create(term_t mutex)
{ GET_LD
  atom_t name = NULL_ATOM;
  pl_mutex *m;
  word id;

  if ( PL_get_atom(mutex, &name) )
  { if ( lookupHTable(GD->thread.mutexTable, (void *)name) )
    { PL_error("mutex_create", 1, NULL, ERR_PERMISSION,
	       ATOM_create, ATOM_mutex, mutex);
      return NULL;
    }
    id = name;
  } else if ( PL_is_variable(mutex) )
  { id = NULL_ATOM;
  } else
  { PL_error("mutex_create", 1, NULL, ERR_TYPE, ATOM_mutex, mutex);
    return NULL;
  }

  if ( (m=mutexCreate(id)) )
  { if ( !unify_mutex(mutex, m) )
    { destroy_mutex(m);
      m = NULL;
    }
  }

  return m;
}


static
PRED_IMPL("mutex_create", 1, mutex_create1, 0)
{ int rval;

  PL_LOCK(L_UMUTEX);
  rval = (unlocked_pl_mutex_create(A1) ? TRUE : FALSE);
  PL_UNLOCK(L_UMUTEX);

  return rval;
}


static const opt_spec mutex_options[] =
{ { ATOM_alias,		OPT_ATOM },
  { NULL_ATOM,		0 }
};


static
PRED_IMPL("mutex_create", 2, mutex_create2, 0)
{ PRED_LD
  int rval;
  atom_t alias = 0;

  if ( !scan_options(A2, 0,
		     ATOM_mutex_option, mutex_options,
		     &alias) )
    fail;

  if ( alias )
  { if ( !PL_unify_atom(A1, alias) )
      return PL_error("mutex_create", 2, NULL, ERR_UNINSTANTIATION, 1, A1);
  }

  PL_LOCK(L_UMUTEX);
  rval = (unlocked_pl_mutex_create(A1) ? TRUE : FALSE);
  PL_UNLOCK(L_UMUTEX);

  return rval;
}


static int
get_mutex(term_t t, pl_mutex **mutex, int create)
{ GET_LD
  atom_t name;
  word id = 0;
  pl_mutex *m = NULL;

  if ( PL_get_atom(t, &name) )
  { PL_blob_t *type;
    mutexref *ref = PL_blob_data(name, NULL, &type);

    if ( type == &mutex_blob )
    { m = ref->mutex;
      goto out;
    } else if ( isTextAtom(name) )
    { id = name;
    }
  }

  if ( !id )
  { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_mutex, t);
    return FALSE;
  }

  PL_LOCK(L_UMUTEX);
  if ( GD->thread.mutexTable &&
       (m = lookupHTable(GD->thread.mutexTable, (void *)id)) )
  { ;
  } else if ( create )
  { m = unlocked_pl_mutex_create(t);
  } else
  { PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_mutex, t);
  }
  PL_UNLOCK(L_UMUTEX);

out:
  if ( m )
  { if ( !m->destroyed )
    { *mutex = m;
      return TRUE;
    }
    PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_mutex, t);
  }

  return FALSE;
}



int
PL_mutex_lock(struct pl_mutex *m)
{ int self = PL_thread_self();

  if ( self == m->owner )
  { m->count++;
  } else
  { int rc;
#ifdef HAVE_PTHREAD_MUTEX_TIMEDLOCK
    for(;;)
    { struct timespec deadline;

      get_current_timespec(&deadline);
      deadline.tv_nsec += 250000000;
      carry_timespec_nanos(&deadline);

      if ( (rc=pthread_mutex_timedlock(&m->mutex, &deadline)) == ETIMEDOUT )
      { if ( PL_handle_signals() < 0 )
	  return FALSE;
      } else
	break;
    }
#else
    rc = pthread_mutex_lock(&m->mutex);
#endif
    assert(rc == 0);
    m->count = 1;
    m->owner = self;
  }

  return TRUE;
}


static
PRED_IMPL("mutex_lock", 1, mutex_lock, 0)
{ pl_mutex *m;

  if ( !get_mutex(A1, &m, TRUE) )
    return FALSE;

  return  PL_mutex_lock(m);
}


static int
PL_mutex_trylock(struct pl_mutex *m)
{ int self = PL_thread_self();
  int rc;

  if ( self == m->owner )
  { m->count++;
  } else if ( (rc = pthread_mutex_trylock(&m->mutex)) == 0 )
  { m->count = 1;
    m->owner = self;
  } else
  { assert(rc == EBUSY);
    return FALSE;
  }

  return TRUE;
}


static
PRED_IMPL("mutex_trylock", 1, mutex_trylock, 0)
{ pl_mutex *m;

  if ( !get_mutex(A1, &m, TRUE) )
    return FALSE;

  return  PL_mutex_trylock(m);
}


int
PL_mutex_unlock(struct pl_mutex *m)
{ int self = PL_thread_self();

  if ( self == m->owner )
  { if ( --m->count == 0 )
    { m->owner = 0;

      pthread_mutex_unlock(&m->mutex);
    }

    return TRUE;
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The error message of this  predicate  is   not  thread-safe.  I.e. it is
possible the message is wrong. This can   only be fixed by modifying the
API of PL_mutex_unlock(), which is asking a  bit too much for this small
error.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("mutex_unlock", 1, mutex_unlock, 0)
{ pl_mutex *m;

  if ( !get_mutex(A1, &m, FALSE) )
    return FALSE;

  if ( PL_mutex_unlock(m) )
  { if ( m->auto_destroy )
    { PL_LOCK(L_UMUTEX);
      try_really_destroy_mutex(m);
      PL_UNLOCK(L_UMUTEX);
    }

    return TRUE;
  } else
  { char *msg = m->owner ? "not owner" : "not locked";

    return PL_error("mutex_unlock", 1, msg, ERR_PERMISSION,
		    ATOM_unlock, ATOM_mutex, A1);
  }
}


static
PRED_IMPL("mutex_unlock_all", 0, mutex_unlock_all, 0)
{ TableEnum e;
  pl_mutex *m;
  int tid = PL_thread_self();

  e = newTableEnum(GD->thread.mutexTable);
  while( advanceTableEnum(e, NULL, (void**)&m) )
  { if ( m->owner == tid )
    { m->count = 0;
      m->owner = 0;
      pthread_mutex_unlock(&m->mutex);
    }
  }
  freeTableEnum(e);
  return TRUE;
}


static int
try_really_destroy_mutex(pl_mutex *m)
{ if ( PL_mutex_trylock(m) )
  { if ( m->count == 1 )
    { m->destroyed = TRUE;
      deleteHTable(GD->thread.mutexTable, (void *)m->id);
      if ( !m->anonymous )
	PL_unregister_atom(m->id);
      m->count = 0;
      m->owner = 0;
      pthread_mutex_unlock(&m->mutex);
      destroy_mutex(m);
      return TRUE;
    } else
      PL_mutex_unlock(m);
  }

  return FALSE;
}


static
PRED_IMPL("mutex_destroy", 1, mutex_destroy, 0)
{ pl_mutex *m;

  if ( !get_mutex(A1, &m, FALSE) )
    return FALSE;

  PL_LOCK(L_UMUTEX);
  if ( !try_really_destroy_mutex(m) )
    m->auto_destroy = TRUE;
  PL_UNLOCK(L_UMUTEX);

  return TRUE;
}


		 /*******************************
		 *	  MUTEX_PROPERTY	*
		 *******************************/

static int		/* mutex_property(Mutex, alias(Name)) */
mutex_alias_property(pl_mutex *m, term_t prop ARG_LD)
{ if ( !m->anonymous )
    return PL_unify_atom(prop, m->id);

  fail;
}


static int		/* mutex_property(Mutex, status(locked(By, Count))) */
mutex_status_property(pl_mutex *m, term_t prop ARG_LD)
{ if ( m->owner )
  { int owner = m->owner;
    int count = m->count;
    term_t owner_term = PL_new_term_ref();

    return (PL_unify_term(prop, PL_FUNCTOR, FUNCTOR_locked2,
			    PL_TERM, owner_term,
			    PL_INT, count) &&
	    unify_mutex_owner(owner_term, owner));
  } else
  { return PL_unify_atom(prop, ATOM_unlocked);
  }

  fail;
}


static const tprop mprop_list [] =
{ { FUNCTOR_alias1,	    mutex_alias_property },
  { FUNCTOR_status1,	    mutex_status_property },
  { 0,			    NULL }
};


typedef struct
{ TableEnum e;				/* Enumerator on mutex-table */
  pl_mutex *m;				/* current mutex */
  const tprop *p;			/* Pointer in properties */
  int enum_properties;			/* Enumerate the properties */
} mprop_enum;


static int
advance_mstate(mprop_enum *state)
{ if ( state->enum_properties )
  { state->p++;
    if ( state->p->functor )
      succeed;

    state->p = mprop_list;
  }
  if ( state->e )
  { pl_mutex *m;

    if ( advanceTableEnum(state->e, NULL, (void**)&m) )
    { state->m = m;

      succeed;
    }
  }

  fail;
}


static void
free_mstate(mprop_enum *state)
{ if ( state->e )
    freeTableEnum(state->e);

  freeForeignState(state, sizeof(*state));
}


static
PRED_IMPL("mutex_property", 2, mutex_property, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  term_t mutex = A1;
  term_t property = A2;
  mprop_enum statebuf;
  mprop_enum *state;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { memset(&statebuf, 0, sizeof(statebuf));
      state = &statebuf;

      if ( PL_is_variable(mutex) )
      { switch( get_prop_def(property, ATOM_mutex_property,
			     mprop_list, &state->p) )
	{ case 1:
	    state->e = newTableEnum(GD->thread.mutexTable);
	    goto enumerate;
	  case 0:
	    state->e = newTableEnum(GD->thread.mutexTable);
	    state->p = mprop_list;
	    state->enum_properties = TRUE;
	    goto enumerate;
	  case -1:
	    fail;
	}
      } else if ( get_mutex(mutex, &state->m, FALSE) )
      { switch( get_prop_def(property, ATOM_mutex_property,
			     mprop_list, &state->p) )
	{ case 1:
	    goto enumerate;
	  case 0:
	    state->p = mprop_list;
	    state->enum_properties = TRUE;
	    goto enumerate;
	  case -1:
	    fail;
	}
      } else
      { fail;
      }
    }
    case FRG_REDO:
      state = CTX_PTR;
      break;
    case FRG_CUTTED:
      state = CTX_PTR;
      free_mstate(state);
      succeed;
    default:
      assert(0);
      fail;
  }

enumerate:
  if ( !state->m )			/* first time, enumerating mutexes */
  { pl_mutex *m;

    assert(state->e);
    if ( advanceTableEnum(state->e, NULL, (void**)&m) )
    { state->m = m;
    } else
    { freeTableEnum(state->e);
      assert(state != &statebuf);
      fail;
    }
  }


  { term_t arg = PL_new_term_ref();

    if ( !state->enum_properties )
      _PL_get_arg(1, property, arg);

    for(;;)
    { if ( (*state->p->function)(state->m, arg PASS_LD) )
      { if ( state->enum_properties )
	{ if ( !PL_unify_term(property,
			      PL_FUNCTOR, state->p->functor,
			        PL_TERM, arg) )
	    goto error;
	}
	if ( state->e )
	{ if ( !unify_mutex(mutex, state->m) )
	    goto error;
	}

	if ( advance_mstate(state) )
	{ if ( state == &statebuf )
	  { mprop_enum *copy = allocForeignState(sizeof(*copy));

	    *copy = *state;
	    state = copy;
	  }

	  ForeignRedoPtr(state);
	}

	if ( state != &statebuf )
	  free_mstate(state);
	succeed;
      }

      if ( !advance_mstate(state) )
      { error:
	if ( state != &statebuf )
	  free_mstate(state);
	fail;
      }
    }
  }
}

		 /*******************************
		 *	  INITIALIZATION	*
		 *******************************/

static void
unalloc_mutex_symbol(void *name, void *value)
{ unalloc_mutex(value);
}

void
initMutexes(void)
{ GD->thread.mutexTable = newHTable(16);
  GD->thread.mutexTable->free_symbol = unalloc_mutex_symbol;
  initMutexRef();
}

#endif /*O_PLMT*/

		 /*******************************
		 *	    WITH-MUTEX		*
		 *******************************/

foreign_t
pl_with_mutex(term_t mutex, term_t goal)
{ term_t ex = 0;
  int rval;

#ifdef O_PLMT
  pl_mutex *m;

  if ( !get_mutex(mutex, &m, TRUE) )
    return FALSE;
  PL_mutex_lock(m);
  rval = callProlog(NULL, goal, PL_Q_CATCH_EXCEPTION, &ex);
  PL_mutex_unlock(m);
#else
  rval = callProlog(NULL, goal, PL_Q_CATCH_EXCEPTION, &ex);
#endif

  if ( !rval && ex )
  { DEBUG(CHK_SECURE,
	  { GET_LD
	    checkData(valTermRef(ex));
	  });
    PL_raise_exception(ex);
  }

  return rval;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

#define NDET PL_FA_NONDETERMINISTIC

BeginPredDefs(mutex)
#ifdef O_PLMT
  PRED_DEF("mutex_create",	     1,	mutex_create1,	       0)
  PRED_DEF("mutex_create",	     2,	mutex_create2,	       PL_FA_ISO)
  PRED_DEF("mutex_destroy",	     1,	mutex_destroy,	       PL_FA_ISO)
  PRED_DEF("mutex_lock",	     1,	mutex_lock,	       PL_FA_ISO)
  PRED_DEF("mutex_trylock",	     1,	mutex_trylock,	       PL_FA_ISO)
  PRED_DEF("mutex_unlock",	     1,	mutex_unlock,	       PL_FA_ISO)
  PRED_DEF("mutex_unlock_all",	     0,	mutex_unlock_all,      0)
  PRED_DEF("mutex_property",	     2,	mutex_property,	       NDET|PL_FA_ISO)
#endif
EndPredDefs
