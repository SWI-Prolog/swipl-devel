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

#define WITH_MD5 1
#define WITH_PL_MUTEX 1
#define _GNU_SOURCE 1			/* get rwlocks from glibc */

#ifdef _REENTRANT
#ifdef WIN32
#include <windows.h>
#else
#include <pthread.h>
#include <errno.h>
#endif
#endif

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include "rdf_db.h"
#include <assert.h>
#include <string.h>
#include <ctype.h>
#include "atom_set.h"
#ifdef WITH_MD5
#include "md5.h"

static void md5_triple(triple *t, md5_byte_t *digest);
static void sum_digest(md5_byte_t *digest, md5_byte_t *add);
static void dec_digest(md5_byte_t *digest, md5_byte_t *add);
#endif

#define O_DEBUG 1

#ifdef O_DEBUG
static int debuglevel = 0;
#define DEBUG(n, g) if ( debuglevel >= (n) ) { g; }
#else
#define DEBUG(n, g) ((void)0);
#endif

#ifdef DIRECT_MALLOC
#define PL_malloc malloc
#define PL_free free
#define PL_realloc realloc
#endif

static functor_t FUNCTOR_literal1;
static functor_t FUNCTOR_literal2;
static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_type_error2;
static functor_t FUNCTOR_domain_error2;
static functor_t FUNCTOR_colon2;

static functor_t FUNCTOR_triples1;
static functor_t FUNCTOR_triples2;
static functor_t FUNCTOR_subjects1;
static functor_t FUNCTOR_predicates1;
static functor_t FUNCTOR_duplicates1;
static functor_t FUNCTOR_subject1;
static functor_t FUNCTOR_predicate1;
static functor_t FUNCTOR_object1;
static functor_t FUNCTOR_source1;
static functor_t FUNCTOR_indexed8;

static functor_t FUNCTOR_exact1;
static functor_t FUNCTOR_substring1;
static functor_t FUNCTOR_word1;
static functor_t FUNCTOR_prefix1;
static functor_t FUNCTOR_like1;

static functor_t FUNCTOR_symmetric1;
static functor_t FUNCTOR_inverse_of1;
static functor_t FUNCTOR_transitive1;
static functor_t FUNCTOR_rdf_subject_branch_factor1;    /* S --> BF*O */
static functor_t FUNCTOR_rdf_object_branch_factor1;	/* O --> BF*S */
static functor_t FUNCTOR_rdfs_subject_branch_factor1;	/* S --> BF*O */
static functor_t FUNCTOR_rdfs_object_branch_factor1;	/* O --> BF*S */

static functor_t FUNCTOR_searched_nodes1;
static functor_t FUNCTOR_lang2;
static functor_t FUNCTOR_type2;

static atom_t   ATOM_user;
static atom_t	ATOM_exact;
static atom_t	ATOM_prefix;
static atom_t	ATOM_substring;
static atom_t	ATOM_word;
static atom_t	ATOM_like;

static atom_t	ATOM_subPropertyOf;

static predicate_t PRED_call1;

#define MATCH_EXACT 		0x1	/* exact triple match */
#define MATCH_SUBPROPERTY	0x2	/* Use subPropertyOf relations */
#define MATCH_SRC		0x4	/* Match source location */
#define MATCH_INVERSE		0x8	/* use symmetric match too */

static int match(int how, atom_t search, atom_t label);
static int update_duplicates_add(rdf_db *db, triple *t);
static void update_duplicates_del(rdf_db *db, triple *t);
static void unlock_atoms(triple *t);
static int  update_hash(rdf_db *db);
static int  triple_hash(rdf_db *db, triple *t, int which);
static unsigned long object_hash(triple *t);
static int	permission_error(const char *op, const char *type,
				 const char *obj, const char *msg);
static void	reset_db(rdf_db *db);

static void	record_transaction(rdf_db *db,
				   tr_type type, triple *t);
static void	record_md5_transaction(rdf_db *db,
				       source *src, md5_byte_t *digest);


		 /*******************************
		 *	       LOCKING		*
		 *******************************/

#ifdef _REENTRANT


		 /*******************************
		 *	 WINDOWS VERSION	*
		 *******************************/

#ifdef WIN32

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This  is  a  slightly  simplified  version  of  the  condition  variable
emulation used in SWI-Prolog. We have   deleted the broadcast facilities
of the CVs as this is not used in this code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int 
win32_cond_init(win32_cond_t *cv)
{ cv->events[SIGNAL]    = CreateEvent(NULL, FALSE, FALSE, NULL);
  cv->waiters = 0;

  return 0;
}


static int
win32_cond_destroy(win32_cond_t *cv)
{ CloseHandle(cv->events[SIGNAL]);

  return 0;
}

#define WAIT_INTR (-1)

static int 
win32_cond_wait(win32_cond_t *cv,
		CRITICAL_SECTION *external_mutex)
{ int rc;

  cv->waiters++;

  LeaveCriticalSection(external_mutex);
  rc = MsgWaitForMultipleObjects(1,
				 cv->events,
				 FALSE,	/* wait for either event */
				 INFINITE,
				 QS_ALLINPUT);
  if ( rc == WAIT_OBJECT_0+1 )
  { MSG msg;

    while( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
    { TranslateMessage(&msg);
      DispatchMessage(&msg);
    }

    if ( PL_handle_signals() < 0 )
    { EnterCriticalSection(external_mutex);
      return WAIT_INTR;
    }
  }

  EnterCriticalSection(external_mutex);

  cv->waiters--;

  return 0;
}


static int 
win32_cond_signal(win32_cond_t *cv)	/* must be holding associated mutex */
{ if ( cv->waiters > 0 )
    SetEvent(cv->events[SIGNAL]);

  return 0;
}


static int
RDLOCK(rdf_db *db)
{ EnterCriticalSection(&db->mutex);

  if ( db->allow_readers == TRUE )
  { ok:

    db->readers++;
    db->read_by_thread[PL_thread_self()]++;
    LeaveCriticalSection(&db->mutex);

    return TRUE;
  }
  
  db->waiting_readers++;

  for(;;)
  { int rc = win32_cond_wait(&db->rdcondvar, &db->mutex);

    if ( rc == WAIT_INTR )
    { LeaveCriticalSection(&db->mutex);
      return FALSE;
    } else if ( rc == 0 )
    { if ( db->allow_readers == TRUE )
      { db->waiting_readers--;
	goto ok;
      }
    } else
    { assert(0);			/* TBD: OS errors */
    }
  }
}


static int
WRLOCK(rdf_db *db, int allow_readers)
{ int self = PL_thread_self();

  if ( db->writer == self )		/* recursive write lock, used for */
  { db->lock_level++;			/* nested transactions */

    return TRUE;
  }

  EnterCriticalSection(&db->mutex);
  
  if ( db->writer == -1 && db->readers == 0 )
  { ok:

    db->writer = self;
    db->lock_level = 1;
    db->allow_readers = allow_readers;
    LeaveCriticalSection(&db->mutex);
    DEBUG(3, Sdprintf("WRLOCK(%d): OK\n", self));

    return TRUE;
  }

  if ( db->read_by_thread[self] > 0 )
  { LeaveCriticalSection(&db->mutex);
    return permission_error("write", "rdf_db", "default",
			    "Operation would deadlock");
  }

  db->waiting_writers++;
  DEBUG(3, Sdprintf("WRLOCK(%d): waiting ...\n", self));

  for(;;)
  { int rc = win32_cond_wait(&db->wrcondvar, &db->mutex);

    if ( rc == WAIT_INTR )
    { LeaveCriticalSection(&db->mutex);
      return FALSE;
    } else if ( rc == 0 )
    { if ( db->writer == -1 && db->readers == 0 )
      { db->waiting_writers--;
	goto ok;
      }
    } else
    { assert(0);			/* TBD: OS errors */
    }
  }     
}


static int
LOCKOUT_READERS(rdf_db *db)
{ EnterCriticalSection(&db->mutex);
  
  if ( db->readers == 0 )
  { ok:

    db->allow_readers = FALSE;
    LeaveCriticalSection(&db->mutex);

    return TRUE;
  }

  db->waiting_upgrade++;

  for(;;)
  { int rc = win32_cond_wait(&db->upcondvar, &db->mutex);

    if ( rc == WAIT_INTR )
    { LeaveCriticalSection(&db->mutex);
      return FALSE;
    } else if ( rc == 0 )
    { if ( db->readers == 0 )
      { db->waiting_upgrade--;
	goto ok;
      }
    } else
    { assert(0);			/* TBD: OS errors */
    }
  }
}


static int
UNLOCK(rdf_db *db, int rd)
{ int self = PL_thread_self();
  int signal;

  if ( db->writer == self && db->lock_level > 1 )
  { db->lock_level--;
    return TRUE;
  }

  EnterCriticalSection(&db->mutex);
  if ( rd )				/* must be a read lock */
  { db->readers--;
    db->read_by_thread[self]--;
    signal = (db->readers == 0);
  } else
  { db->writer = -1;
    db->allow_readers = TRUE;
    signal = TRUE;
  }

  if ( signal )
  { enum { NONE, READ, WRITE, UPGRADE } waiting;

    waiting = (db->waiting_upgrade ? UPGRADE :
	       db->waiting_writers ? WRITE :
	       db->waiting_readers ? READ : NONE);
  
    switch(waiting)
    { case UPGRADE:
	win32_cond_signal(&db->upcondvar);
	break;
      case WRITE:
	win32_cond_signal(&db->wrcondvar);
	break;
      case READ:
	win32_cond_signal(&db->rdcondvar);
	break;
      default:
	;
    }
  }

  LeaveCriticalSection(&db->mutex);	/* In our WIN32 emulation we */
					/* must hold the associated mutex */
  return TRUE;
}


static int
LOCK_MISC(rdf_db *db)
{ EnterCriticalSection(&db->hash_mutex);

  return TRUE;
}


static int
UNLOCK_MISC(rdf_db *db)
{ LeaveCriticalSection(&db->hash_mutex);
  
  return TRUE;
}


static int
INIT_LOCK(rdf_db *db)
{ int bytes;

  InitializeCriticalSection(&db->mutex);
  InitializeCriticalSection(&db->hash_mutex);

  if ( !win32_cond_init(&db->wrcondvar) == 0 ||
       !win32_cond_init(&db->rdcondvar) == 0 ||
       !win32_cond_init(&db->upcondvar) == 0 )
  {					/* TBD: System error */
    return FALSE;
  }

  db->writer          = -1;
  db->allow_readers   = TRUE;
  db->readers         = 0;
  db->waiting_readers = 0;
  db->waiting_writers = 0;
  db->waiting_upgrade = 0;
  db->lock_level      = 0;

  bytes = sizeof(int)*PL_query(PL_QUERY_MAX_THREADS);

  db->read_by_thread = PL_malloc(bytes);
  memset(db->read_by_thread, 0, bytes);

  return TRUE;
}

#else /*WIN32*/

		 /*******************************
		 *	   POSIX VERSION	*
		 *******************************/

static int
RDLOCK(rdf_db *db)
{ int self = PL_thread_self();

  pthread_mutex_lock(&db->mutex);

  if ( db->allow_readers == TRUE )
  { ok:

    db->readers++;
    db->read_by_thread[self]++;
    pthread_mutex_unlock(&db->mutex);

    return TRUE;
  }
  
  db->waiting_readers++;

  for(;;)
  { int rc = pthread_cond_wait(&db->rdcondvar, &db->mutex);

    if ( rc == EINTR )
    { if ( PL_handle_signals() < 0 )
	return FALSE;
      continue;
    } else if ( rc == 0 )
    { if ( db->allow_readers == TRUE )
      { db->waiting_readers--;
	goto ok;
      }
    } else
    { assert(0);			/* TBD: OS errors */
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
WRLOCK() and LOCKOUT_READERS() can be  used   in  two ways. Conventional
write locks are established using   WRLOCK(db,  FALSE) ... WRUNLOCK(db).
For transactions, we allow concurrent  readers   until  we  are ready to
commit, in which case we use  WRLOCK(db, TRUE) ... LOCKOUT_READERS() ...
WRUNLOCK(db)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
WRLOCK(rdf_db *db, int allow_readers)
{ int self = PL_thread_self();

  if ( db->writer == self )		/* recursive write lock, used for */
  { db->lock_level++;			/* nested transactions */

    return TRUE;
  }

  pthread_mutex_lock(&db->mutex);
  
  if ( db->writer == -1 && db->readers == 0 )
  { ok:

    db->writer = self;
    db->lock_level = 1;
    db->allow_readers = allow_readers;
    pthread_mutex_unlock(&db->mutex);
    DEBUG(3, Sdprintf("WRLOCK(%d): OK\n", self));

    return TRUE;
  }

  if ( db->read_by_thread[self] > 0 )
  { DEBUG(1, Sdprintf("SELF(%d) has %d readers\n",
		      self, db->read_by_thread[self]));
    pthread_mutex_unlock(&db->mutex);
    return permission_error("write", "rdf_db", "default",
			    "Operation would deadlock");
  }

  db->waiting_writers++;
  DEBUG(3, Sdprintf("WRLOCK(%d): waiting ...\n", self));

  for(;;)
  { int rc = pthread_cond_wait(&db->wrcondvar, &db->mutex);

    if ( rc == EINTR )
    { if ( PL_handle_signals() < 0 )
	return FALSE;
      continue;
    } else if ( rc == 0 )
    { if ( db->writer == -1 && db->readers == 0 )
      { db->waiting_writers--;
	goto ok;
      }
    } else
    { assert(0);			/* TBD: OS errors */
    }
  }     
}


static int
LOCKOUT_READERS(rdf_db *db)
{ pthread_mutex_lock(&db->mutex);
  
  if ( db->readers == 0 )
  { ok:

    db->allow_readers = FALSE;
    pthread_mutex_unlock(&db->mutex);

    return TRUE;
  }

  db->waiting_upgrade++;

  for(;;)
  { int rc = pthread_cond_wait(&db->upcondvar, &db->mutex);

    if ( rc == EINTR )
    { if ( PL_handle_signals() < 0 )
	return FALSE;
      continue;
    } else if ( rc == 0 )
    { if ( db->readers == 0 )
      { db->waiting_upgrade--;
	goto ok;
      }
    } else
    { assert(0);			/* TBD: OS errors */
    }
  }
}


static int
UNLOCK(rdf_db *db, int rd)		/* TRUE: read lock */
{ int self = PL_thread_self();
  int signal;

  if ( db->writer == self && db->lock_level > 1 )
  { db->lock_level--;
    return TRUE;
  }

  pthread_mutex_lock(&db->mutex);
  if ( rd )				/* read lock */
  { db->readers--;
    db->read_by_thread[self]--;
    signal = (db->readers == 0);
  } else
  { db->writer = -1;
    db->allow_readers = TRUE;
    signal = TRUE;
  }

  if ( signal )
  { enum { NONE, READ, WRITE, UPGRADE } waiting;

    waiting = (db->waiting_upgrade ? UPGRADE :
	       db->waiting_writers ? WRITE :
	       db->waiting_readers ? READ : NONE);
    pthread_mutex_unlock(&db->mutex);
  
    switch(waiting)
    { case UPGRADE:
	pthread_cond_signal(&db->upcondvar);
	break;
      case WRITE:
	pthread_cond_signal(&db->wrcondvar);
	break;
      case READ:
	pthread_cond_signal(&db->rdcondvar);
	break;
      default:
	;
    }
  } else
  { pthread_mutex_unlock(&db->mutex);
  }
  
  return TRUE;
}


static int
LOCK_MISC(rdf_db *db)
{ return pthread_mutex_lock(&db->hash_mutex) == 0;
}


static int
UNLOCK_MISC(rdf_db *db)
{ return pthread_mutex_unlock(&db->hash_mutex) == 0;
}


static int
INIT_LOCK(rdf_db *db)
{ int bytes;
  int maxthreads;

  if ( !pthread_mutex_init(&db->mutex, NULL) == 0 ||
       !pthread_mutex_init(&db->hash_mutex, NULL) == 0 ||
       !pthread_cond_init(&db->wrcondvar, NULL) == 0 ||
       !pthread_cond_init(&db->rdcondvar, NULL) == 0 ||
       !pthread_cond_init(&db->upcondvar, NULL) == 0 )
  {					/* TBD: System error */
    return FALSE;
  }

  db->writer          = -1;
  db->readers	      = 0;
  db->allow_readers   = TRUE;
  db->waiting_readers = 0;
  db->waiting_writers = 0;
  db->waiting_upgrade = 0;
  db->lock_level      = 0;

  maxthreads = PL_query(PL_QUERY_MAX_THREADS);
  bytes = sizeof(int)*maxthreads;
  DEBUG(1, Sdprintf("MAX_THREADS = %d\n", maxthreads));
  db->read_by_thread = PL_malloc(bytes);
  memset(db->read_by_thread, 0, bytes);

  return TRUE;
}

#endif /*WIN32*/


#else /*_REENTRANT*/

static int
RDLOCK(rdf_db *db)
{ db->readers++;

  return TRUE;
}

static int
WRLOCK(rdf_db *db, int allow_readers)
{ if ( db->readers )
    return permission_error("write", "rdf_db", "default",
			    "Operation would deadlock");

  db->writer = 0;

  return TRUE;
}

static int
UNLOCK(rdf_db *db)
{ if ( db->writer == -1 )
  { db->readers--;
  } else
  { db->writer = -1;
  }

  return TRUE;
}


static int
LOCK_MISC(rdf_db *db)
{ return TRUE;
}


static int
UNLOCK_MISC(rdf_db *db)
{ return TRUE;
}


static int
INIT_LOCK(rdf_db *db)
{ db->writer = -1;
  db->readers = 0;

  return TRUE;
}

#endif /*_REENTRANT*/

#define WRUNLOCK(db)	UNLOCK(db, FALSE)
#define RDUNLOCK(db)	UNLOCK(db, TRUE)


		 /*******************************
		 *	       ERRORS		*
		 *******************************/

static int
instantiation_error(term_t actual)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_CHARS, "instantiation_error",
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
type_error(term_t actual, const char *expected)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_type_error2,
		        PL_CHARS, expected,
		        PL_TERM, actual,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
domain_error(term_t actual, const char *expected)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_domain_error2,
		        PL_CHARS, expected,
		        PL_TERM, actual,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
permission_error(const char *op, const char *type, const char *obj,
		 const char *msg)
{ term_t ex = PL_new_term_ref();
  term_t ctx = PL_new_term_ref();

  if ( msg )
    PL_unify_term(ctx, PL_FUNCTOR_CHARS, "context", 2,
		         PL_VARIABLE,
		         PL_CHARS, msg);

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR_CHARS, "permission_error", 3,
		        PL_CHARS, op,
		        PL_CHARS, type,
		        PL_CHARS, obj,
		      PL_TERM, ctx);

  return PL_raise_exception(ex);
}


static int
get_atom_ex(term_t t, atom_t *a)
{ if ( PL_get_atom(t, a) )
    return TRUE;

  return type_error(t, "atom");
}


static int
get_long_ex(term_t t, long *v)
{ if ( PL_get_long(t, v) )
    return TRUE;

  return type_error(t, "integer");
}


static int
get_atom_or_var_ex(term_t t, atom_t *a)
{ if ( PL_get_atom(t, a) )
    return TRUE;
  if ( PL_is_variable(t) )
  { *a = 0L;
    return TRUE;
  }

  return type_error(t, "atom");
}


static int
get_bool_arg_ex(int a, term_t t, int *val)
{ term_t arg = PL_new_term_ref();

  if ( !PL_get_arg(a, t, arg) )
    return type_error(t, "compound");
  if ( !PL_get_bool(arg, val) )
    return type_error(arg, "bool");

  return TRUE;
}



		 /*******************************
		 *	   DEBUG SUPPORT	*
		 *******************************/

#ifdef O_DEBUG

#define PRT_SRC	0x1

static void
print_object(triple *t)
{ switch(t->objtype)
  { case OBJ_RESOURCE:
      Sdprintf("%s", PL_atom_chars(t->object.resource));
      break;
    case OBJ_STRING:
      Sdprintf("\"%s\"", PL_atom_chars(t->object.string));
      break;
    case OBJ_INTEGER:
      Sdprintf("%ld", t->object.integer);
      break;
    case OBJ_DOUBLE:
      Sdprintf("%f", t->object.real);
      break;
    case OBJ_TERM:
    { fid_t fid = PL_open_foreign_frame();
      term_t term = PL_new_term_ref();

      PL_recorded_external(t->object.term.record, term);
      PL_write_term(Serror, term, 1200,
		    PL_WRT_QUOTED|PL_WRT_NUMBERVARS|PL_WRT_PORTRAY);
      break;
      PL_discard_foreign_frame(fid);
      break;
    }
    default:
      assert(0);
  }
}


static void
print_triple(triple *t, int flags)
{ Sdprintf("<%s %s ",
	   PL_atom_chars(t->subject),
	   PL_atom_chars(t->predicate->name));
  print_object(t);
  if ( (flags & PRT_SRC) )
  { if ( t->line == NO_LINE )
      Sdprintf(" @%s", PL_atom_chars(t->source));
    else
      Sdprintf(" @%s:%ld", PL_atom_chars(t->source), t->line);
  }
  Sdprintf(">");
}

#endif

		 /*******************************
		 *	     STORAGE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Our one and only database (for the time being).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static rdf_db *DB;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SWI-Prolog note: Atoms are integers shifted by LMASK_BITS (7)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define atom_hash(a) (((unsigned long)a)>>7)


		 /*******************************
		 *	      LISTS		*
		 *******************************/

static int
add_list(list *list, void *value)
{ cell *c;

  for(c=list->head; c; c=c->next)
  { if ( c->value == value )
      return FALSE;			/* already a member */
  }

  c = PL_malloc(sizeof(*c));
  c->value = value;
  c->next = NULL;

  if ( list->tail )
    list->tail->next = c;
  else
    list->head = c;

  list->tail = c;

  return TRUE;
}


static int
del_list(list *list, void *value)
{ cell *c, *p = NULL;

  for(c=list->head; c; p=c, c=c->next)
  { if ( c->value == value )
    { if ( p )
	p->next = c->next;
      else
	list->head = c->next;

      if ( !c->next )
	list->tail = p;

      PL_free(c);

      return TRUE;
    }
  }

  return FALSE;				/* not a member */
}


static void
free_list(list *list)
{ cell *c, *n;

  for(c=list->head; c; c=n)
  { n = c->next;
    PL_free(c);
  }

  list->head = list->tail = NULL;
}


		 /*******************************
		 *	    PREDICATES		*
		 *******************************/

static void
init_pred_table(rdf_db *db)
{ int bytes = sizeof(predicate*)*INITIAL_PREDICATE_TABLE_SIZE;

  db->pred_table = PL_malloc(bytes);
  memset(db->pred_table, 0, bytes);
  db->pred_table_size = INITIAL_PREDICATE_TABLE_SIZE;
}


static predicate *
lookup_predicate(rdf_db *db, atom_t name)
{ int hash = atom_hash(name) % db->pred_table_size;
  predicate *p;

  LOCK_MISC(db);
  for(p=db->pred_table[hash]; p; p = p->next)
  { if ( p->name == name )
    { UNLOCK_MISC(db);
      return p;
    }
  }
  p = PL_malloc(sizeof(*p));
  memset(p, 0, sizeof(*p));
  p->name = name;
  p->root = p;
  PL_register_atom(name);
  p->next = db->pred_table[hash];
  db->pred_table[hash] = p;
  db->pred_count++;
  UNLOCK_MISC(db);

  return p;
}


static predicate *
alloc_dummy_root_predicate()
{ predicate *p;

  p = PL_malloc(sizeof(*p));
  memset(p, 0, sizeof(*p));
  p->name = 0;				/* dummy roots have no name */
  p->root = p;
  p->oldroot = NULL;

  return p;
}

#if 0
/* We do not yet have a safe mechanism to avoid multiple destruction.  As
   it is anticipated to be very infrequent we'll forget about freeing
   predicates for the time being.
*/

static void				/* currently only frees dummy root */
free_predicate(predicate *p) { assert(!p->name);

  free_list(&p->siblings);
  free_list(&p->subPropertyOf);
  PL_free(p);
}
#endif


static __inline int
is_dummy_root(predicate *p)
{ return p && !p->name;			/* no name --> dummy root */
}


static __inline int
is_virgin_dummy_root(predicate *p)
{ return is_dummy_root(p) && !p->siblings.head;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Change the root if we put a new root   on  top of a hierarchy. We should
*not* assign to predicates that already have a root!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void				/* make root the root of p */
set_dummy_root_r(predicate *p, predicate *root)
{ if ( p->root && p->root != root )
  { cell *c;

    if ( p->root && p->root != (predicate*)1 && is_dummy_root(p->root) )
      del_list(&p->root->siblings, p);
    p->root = root;
    add_list(&root->siblings, p);

    for(c=p->siblings.head; c; c=c->next)
    { set_dummy_root_r(c->value, root);
    }
  }
}


static void				/* make root the root of p */
set_dummy_root(predicate *p, predicate *root)
{ p->root = (predicate*)1;		/* not-null */
  set_dummy_root_r(p, root);
}



static const char *
pname(predicate *p)
{ if ( p->name )
    return PL_atom_chars(p->name);
  else
  { static char *ring[10];
    static int ri = 0;
    char buf[25];
    char *r;

    Ssprintf(buf, "__D%p", p);
    ring[ri++] = r = strdup(buf);
    if ( ri == 10 )
    { ri = 0;
      free(ring[ri]);
    }

    return (const char*)r;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
predicate *root_predicate(predicate *p)

Find the root of a  predicate.  This   function  finds  the one and only
origin of the sub-property graph, dealing   with possible cycles. If the
graph has multiple roots it constructs an artificial parent of all roots
and returns this parent.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static predicate*
root_predicate(predicate *p0, int vindex)
{ predicate *p = p0;

  DEBUG(3, Sdprintf("root_predicate(%s) ...", pname(p)));

  for(;;)
  { cell *c;

    if ( p->root )
    { DEBUG(3, Sdprintf("%s (old)\n", pname(p->root)));
      return p->root;
    }
    if ( p->visited == vindex )
    { DEBUG(3, Sdprintf("%s (cycle)\n", pname(p)));
      return p;				/* cycle */
    }

    p->visited = vindex;
    if ( !p->subPropertyOf.head )
    { DEBUG(3, Sdprintf("%s (root)\n", pname(p)));
      return p;				/* no super */
    }
    if ( !p->subPropertyOf.head->next )
      p = p->subPropertyOf.head->value;	/* exactly one super */
    else				/* multiple supers */
    { predicate *root = root_predicate(p->subPropertyOf.head->value, vindex);
      
      DEBUG(2, Sdprintf("%s has multiple roots\n", pname(p)));

      for(c=p->subPropertyOf.head->next; c; c=c->next)
      { predicate *r2 = root_predicate(c->value, vindex);

	if ( r2 != root )		/* multiple roots */
	{ if ( is_dummy_root(root) )
	  { if ( is_dummy_root(r2) )
	    { cell *c;

	      for(c=r2->siblings.head; c; c=c->next)
		set_dummy_root(c->value, root);
#if 0
	      if ( !r2->siblings.head )
		free_predicate(r2);
#endif
	    } else
	    { set_dummy_root(r2, root);
	    }
	  } else if ( is_dummy_root(r2) )
	  { set_dummy_root(root, r2);
	    root = r2;
	  } else
	  { predicate *nr;

	    if ( is_virgin_dummy_root(root->oldroot) )
	      nr = root->oldroot;
	    else if ( is_virgin_dummy_root(r2->oldroot) )
	      nr = r2->oldroot;
	    else
	      nr = alloc_dummy_root_predicate();

	    set_dummy_root(root, nr);
	    set_dummy_root(r2, nr);

	    DEBUG(1, Sdprintf("New virtual root %s for %s and %s\n",
			      pname(nr),
			      pname(root),
			      pname(r2)));
	    root = nr;
	  }
	}
      }

      return root;
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int organise_predicates()

Assign each predicate its root and return the number of predicates who's
root has changed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
organise_predicates(rdf_db *db)
{ predicate **ht;
  int i;
  int changed = 0;
  int seen = 0;

  for(i=0,ht = db->pred_table; i<db->pred_table_size; i++, ht++)
  { predicate *p;

    for( p = *ht; p; p = p->next )
    { p->oldroot = p->root;
      if ( is_dummy_root(p->oldroot) )
	free_list(&p->oldroot->siblings);
      p->root = NULL;
      p->visited = -1;
    }
  }

  for(i=0,ht = db->pred_table; i<db->pred_table_size; i++, ht++)
  { predicate *p;

    for( p = *ht; p; p = p->next )
    { predicate *root = root_predicate(p, seen);

      p->root = root;
      seen++;
    }
  }

  for(i=0,ht = db->pred_table; i<db->pred_table_size; i++, ht++)
  { predicate *p;

    for( p = *ht; p; p = p->next )
    { if ( p->oldroot != p->root )
	changed++;
#if 0					/* may be referenced multiple */
      if ( is_virgin_dummy_root(p->oldroot) )
	free_predicate(p->oldroot);	/* has not been reused: discard */
#endif
      p->oldroot = NULL;
      DEBUG(1,
	    if ( p->root != p )
	    { Sdprintf("Root of %s = %s\n", pname(p), pname(p->root));
	    })
    }
  }

  assert(seen == db->pred_count);

  return changed;
}


static unsigned long
predicate_hash(predicate *p)
{ return (unsigned long)p >> 2;
}


static void
addSubPropertyOf(rdf_db *db, predicate *sub, predicate *super)
{ if ( add_list(&sub->subPropertyOf, super) )
  { add_list(&super->siblings, sub);
    db->need_update++;
  }
}


static void
delSubPropertyOf(rdf_db *db, predicate *sub, predicate *super)
{ if ( del_list(&sub->subPropertyOf, super) )
  { del_list(&super->siblings, sub);
    db->need_update++;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
See whether sub is p or a subproperty of p.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
is_sub_property_of(predicate *sub, predicate *p, avl_tree *seen)
{ for(;;)
  { cell *c;

    if ( sub == p )
      return TRUE;
    if ( avl_insert(seen, p->name) != 1 )
      return FALSE;

    c = sub->subPropertyOf.head;
    for(c=sub->subPropertyOf.head; c && c->next; c=c->next)
    { if ( is_sub_property_of(c->value, p, seen) )
	return TRUE;
    }
    if ( c )
    { sub = c->value;
    }
  }
}


static int
isSubPropertyOf(predicate *sub, predicate *p)
{ avl_tree seen;
  int rc;

  DEBUG(2, Sdprintf("isSubPropertyOf(%s, %s)\n",
		    PL_atom_chars(sub->name),
		    PL_atom_chars(p->name)));

  if ( sub->root == p )
    return TRUE;

  avl_init(&seen);
  rc = is_sub_property_of(sub, p, &seen);
  avl_destroy(&seen);

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Branching  factors  are  crucial  in  ordering    the  statements  of  a
conjunction. These functions compute  the   average  branching factor in
both directions ("subject --> P  -->  object"   and  "object  -->  P -->
subject") by determining the number of unique   values at either side of
the predicate. This number  is  only   recomputed  if  it  is considered
`dirty'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
update_predicate_counts(rdf_db *db, predicate *p, int which)
{ long total = 0;

  if ( !update_hash(db) )
    return FALSE;

  if ( which == DISTINCT_DIRECT )
  { long changed = abs(p->triple_count - p->distinct_updated[DISTINCT_DIRECT]);

    if ( changed < p->distinct_updated[DISTINCT_DIRECT] )
      return TRUE;

    if ( p->triple_count == 0 )
    { p->distinct_count[which]    = 0;
      p->distinct_subjects[which] = 0;
      p->distinct_objects[which]  = 0;

      return TRUE;
    }
  } else
  { long changed = db->generation - p->distinct_updated[DISTINCT_SUB];

    if ( changed < p->distinct_count[DISTINCT_SUB] )
      return TRUE;
  }

  { avl_tree subject_set;
    avl_tree object_set;
    triple t;
    triple *byp;

    memset(&t, 0, sizeof(t));
    t.predicate = p;
    t.indexed |= BY_P;

    avl_init(&subject_set);
    avl_init(&object_set);
    for(byp = db->table[t.indexed][triple_hash(db, &t, t.indexed)];
	byp;
	byp = byp->next[t.indexed])
    { if ( !byp->erased && !byp->is_duplicate )
      { if ( (which == DISTINCT_DIRECT && byp->predicate == p) ||
	     (which != DISTINCT_DIRECT && isSubPropertyOf(byp->predicate, p)) )
	{ total++;
	  avl_insert(&subject_set, byp->subject);
	  avl_insert(&object_set, object_hash(byp)); /* NOTE: not exact! */
	}
      }
    }

    avl_destroy(&subject_set);
    avl_destroy(&object_set);

    p->distinct_count[which]    = total;
    p->distinct_subjects[which] = subject_set.size;
    p->distinct_objects[which]  = object_set.size;

    if ( which == DISTINCT_DIRECT )
      p->distinct_updated[DISTINCT_DIRECT] = total;
    else
      p->distinct_updated[DISTINCT_SUB] = db->generation;

    DEBUG(1, Sdprintf("%s: distinct subjects (%s): %ld, objects: %ld\n",
		      PL_atom_chars(p->name),
		      (which == DISTINCT_DIRECT ? "rdf" : "rdfs"),
		      p->distinct_subjects[which],
		      p->distinct_objects[which]));
  }

  return TRUE;
}


static void
invalidate_distinct_counts(rdf_db *db)
{ predicate **ht;
  int i;

  for(i=0,ht = db->pred_table; i<db->pred_table_size; i++, ht++)
  { predicate *p;

    for( p = *ht; p; p = p->next )
    { p->distinct_updated[DISTINCT_SUB] = 0;
      p->distinct_count[DISTINCT_SUB] = 0;
      p->distinct_subjects[DISTINCT_SUB] = 0;
      p->distinct_objects[DISTINCT_SUB] = 0;
    }
  }
}


static double
subject_branch_factor(rdf_db *db, predicate *p, int which)
{ if ( !update_predicate_counts(db, p, which) )
    return FALSE;

  if ( p->distinct_subjects[which] == 0 )
    return 0.0;				/* 0 --> 0 */

  return (double)p->distinct_count[which] /
         (double)p->distinct_subjects[which];
}


static double
object_branch_factor(rdf_db *db, predicate *p, int which)
{ if ( !update_predicate_counts(db, p, which) )
    return FALSE;

  if ( p->distinct_objects[which] == 0 )
    return 0.0;				/* 0 --> 0 */

  return (double)p->distinct_count[which] /
         (double)p->distinct_objects[which];
}




		 /*******************************
		 *	     SOURCE FILES	*
		 *******************************/

/* MT: all calls must be locked
*/

static void
init_source_table(rdf_db *db)
{ int bytes = sizeof(predicate*)*INITIAL_SOURCE_TABLE_SIZE;

  db->source_table = PL_malloc(bytes);
  memset(db->source_table, 0, bytes);
  db->source_table_size = INITIAL_SOURCE_TABLE_SIZE;
}


static source *
lookup_source(rdf_db *db, atom_t name, int create)
{ int hash = atom_hash(name) % db->source_table_size;
  source *src;

  LOCK_MISC(db);
  for(src=db->source_table[hash]; src; src = src->next)
  { if ( src->name == name )
    { UNLOCK_MISC(db);
      return src;
    }
  }

  if ( !create )
  { UNLOCK_MISC(db);
    return NULL;
  }

  src = PL_malloc(sizeof(*src));
  memset(src, 0, sizeof(*src));
  src->name = name;
  src->md5 = TRUE;
  PL_register_atom(name);
  src->next = db->source_table[hash];
  db->source_table[hash] = src;
  UNLOCK_MISC(db);

  return src;
}


static void
erase_sources(rdf_db *db)
{ source **ht;
  int i;

  for(i=0,ht = db->source_table; i<db->source_table_size; i++, ht++)
  { source *src, *n;

    for( src = *ht; src; src = n )
    { n = src->next;

      PL_unregister_atom(src->name);
      PL_free(src);
    }

    *ht = NULL;
  }

  db->last_source = NULL;
}


static void
register_source(rdf_db *db, triple *t)
{ source *src;

  if ( !t->source )
    return;

  if ( db->last_source && db->last_source->name == t->source )
  { src = db->last_source;
  } else
  { src = lookup_source(db, t->source, TRUE);
    db->last_source = src;
  } 

  src->triple_count++;
#ifdef WITH_MD5
  if ( src->md5 )
  { md5_byte_t digest[16];
    md5_triple(t, digest);
    sum_digest(src->digest, digest);
  }
#endif
}


static void
unregister_source(rdf_db *db, triple *t)
{ source *src;

  if ( !t->source )
    return;

  if ( db->last_source && db->last_source->name == t->source )
  { src = db->last_source;
  } else
  { src = lookup_source(db, t->source, TRUE);
    db->last_source = src;
  } 

  src->triple_count--;
#ifdef WITH_MD5
  if ( src->md5 )
  { md5_byte_t digest[16];
    md5_triple(t, digest);
    dec_digest(src->digest, digest);
  }
#endif
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rdf_sources_(-ListOfSources)

Return a list holding the names  of   all  currently defined sources. We
return a list to avoid the need for complicated long locks.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
rdf_sources(term_t list)
{ int i;
  term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();
  rdf_db *db = DB;

  if ( !RDLOCK(db) )
    return FALSE;
  for(i=0; i<db->source_table_size; i++)
  { source *src;

    for(src=db->source_table[i]; src; src = src->next)
    { if ( !PL_unify_list(tail, head, tail) ||
	   !PL_unify_atom(head, src->name) )
      { RDUNLOCK(db);
	return FALSE;
      }
    }
  }
  RDUNLOCK(db);

  return PL_unify_nil(tail);
}


		 /*******************************
		 *	      TRIPLES		*
		 *******************************/

static void
init_tables(rdf_db *db)
{ int i;
  int bytes = sizeof(triple*)*INITIAL_TABLE_SIZE;
  int cbytes = sizeof(int)*INITIAL_TABLE_SIZE;

  db->table[0] = &db->by_none;
  db->tail[0]  = &db->by_none_tail;

  for(i=BY_S; i<=BY_OP; i++)
  { if ( i == BY_SO )
      continue;

    db->table[i] = PL_malloc(bytes);
    memset(db->table[i], 0, bytes);
    db->tail[i] = PL_malloc(bytes);
    memset(db->tail[i], 0, bytes);
    db->counts[i] = PL_malloc(cbytes);
    memset(db->counts[i], 0, cbytes);
    db->table_size[i] = INITIAL_TABLE_SIZE;
  }

  init_pred_table(db);
  init_source_table(db);
}


static rdf_db *
new_db()
{ rdf_db *db = PL_malloc(sizeof(*db));

  memset(db, 0, sizeof(*db));
  INIT_LOCK(db);
  init_tables(db);

  return db;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Save a triple in the permanent heap.  Right   now  this is only used for
backtracking context. There is no need to register the atoms here as the
Prolog backtracking context references them.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static triple *
save_triple(triple *t)
{ triple *copy = PL_malloc(sizeof(*copy));

  *copy = *t;

  return copy;
}


static unsigned long
string_hash(const char *t, unsigned int len)
{ unsigned int value = 0;
  unsigned int shift = 5;

  while(len-- != 0)
  { unsigned int c = *t++;
    
    c = tolower(c);			/* case insensitive */
    c -= 'a';
    value ^= c << (shift & 0xf);
    shift ^= c;
  }

  return value ^ (value >> 16);
}


static unsigned long
case_insensitive_atom_hash(atom_t a)
{ const char *s;
  unsigned len;

  s = PL_atom_nchars(a, &len);

  return string_hash(s, len);
}


static unsigned long
object_hash(triple *t)
{ switch(t->objtype)
  { case OBJ_RESOURCE:
      return t->object.resource;
    case OBJ_STRING:
      return case_insensitive_atom_hash(t->object.string);
    case OBJ_INTEGER:
      return t->object.integer;
    case OBJ_DOUBLE:
      return t->object.integer;		/* TBD: get all bits */
    case OBJ_TERM:
      return string_hash((const char*)t->object.term.record,
			 t->object.term.len);
    default:
      assert(0);
      return 0;
  }
}


static int
triple_hash(rdf_db *db, triple *t, int which)
{ unsigned long v;

  switch(which)
  { case BY_NONE:
      return 0;
    case BY_S:
      v = atom_hash(t->subject);
      break;
    case BY_P:
      v = predicate_hash(t->predicate->root);
      break;
    case BY_O:
      v = atom_hash(object_hash(t));
      break;
    case BY_SP:
      v = atom_hash(t->subject) ^ predicate_hash(t->predicate->root);
      break;
    case BY_OP:
      v = predicate_hash(t->predicate->root) ^ object_hash(t);
      break;
    default:
      assert(0);
  }

  return (int)(v % (long)db->table_size[which]);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
by_inverse[] returns the index key to use   for inverse search as needed
to realise symmetric and inverse predicates.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int by_inverse[8] =
{ BY_NONE,				/* BY_NONE = 0 */
  BY_O,					/* BY_S    = 1 */
  BY_P,					/* BY_P    = 2 */
  BY_OP,				/* BY_SP   = 3 */
  BY_S,					/* BY_O    = 4 */
  BY_SO,				/* BY_SO   = 5 */
  BY_SP,				/* BY_OP   = 6 */
  BY_SPO,				/* BY_SPO  = 7 */
};


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
triple *first(atom_t subject)
    Find the first triple on subject.  The first is marked to generate a
    unique subjects quickly;
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static triple *
first(rdf_db *db, atom_t subject)
{ triple *t, tmp;
  int hash;

  tmp.subject = subject;
  hash = triple_hash(db, &tmp, BY_S);
  
  for(t=db->table[BY_S][hash]; t; t = t->next[BY_S])
  { if ( t->subject == subject && !t->erased )
      return t;
  }

  return NULL;
}


static void
link_triple_hash(rdf_db *db, triple *t)
{ int i;

  for(i=1; i<=BY_OP; i++)
  { if ( db->table[i] )
    { int hash = triple_hash(db, t, i);

      if ( db->tail[i][hash] )
      { db->tail[i][hash]->next[i] = t;
      } else
      { db->table[i][hash] = t;
      }
      db->tail[i][hash] = t;
      db->counts[i][hash]++;
    }
  }
}


/* MT: must be locked by caller */

static void
link_triple(rdf_db *db, triple *t)
{ triple *one;

  if ( db->by_none_tail )
    db->by_none_tail->next[BY_NONE] = t;
  else
    db->by_none = t;
  db->by_none_tail = t;

  link_triple_hash(db, t);

  if ( update_duplicates_add(db, t)	)
    goto ok;				/* is a duplicate */

					/* keep track of subjects */
  one = first(db, t->subject);
  if ( !one->first )
  { one->first = TRUE;
    db->subjects++;
  }

					/* keep track of subPropertyOf */
  if ( t->predicate->name == ATOM_subPropertyOf &&
       t->objtype == OBJ_RESOURCE )
  { predicate *me    = lookup_predicate(db, t->subject);
    predicate *super = lookup_predicate(db, t->object.resource);

    addSubPropertyOf(db, me, super);
  }

ok:
  db->created++;
  t->predicate->triple_count++;
  register_source(db, t);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rehash_triples()

Relink the triples in the hash-chains after the hash-keys for properties
have changed or the tables have  been   resized.  The caller must ensure
there are no active queries and the tables are of the proper size.

At the same time, this predicate actually removes erased triples.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static long
tbl_size(long triples)
{ long s0 = 1024;

  while(s0 < triples)
    s0 *= 2;

  return s0;
}



static void
rehash_triples(rdf_db *db)
{ int i;
  triple *t, *t2;
  long count = db->created - db->freed;
  long tsize = tbl_size(count);

  for(i=1; i<INDEX_TABLES; i++)
  { if ( db->table[i] )
    { long bytes  = sizeof(triple*) * tsize;
      long cbytes = sizeof(int)    * tsize;

      db->table_size[i] = tsize;
      db->table[i]  = PL_realloc(db->table[i], bytes);
      db->tail[i]   = PL_realloc(db->tail[i], bytes);
      db->counts[i] = PL_realloc(db->counts[i], cbytes);

      memset(db->table[i], 0, bytes);
      memset(db->tail[i], 0, bytes);
      memset(db->counts[i], 0, cbytes);
    }
  }

					/* delete leading erased triples */
  for(t=db->by_none; t && t->erased; t=t2)
  { t2 = t->next[BY_NONE];

    unlock_atoms(t);
    PL_free(t);
    db->freed++;

    db->by_none = t2;
  }

  for(t=db->by_none; t; t = t2)
  { triple *t3;

    t2 = t->next[BY_NONE];

    for(i=1; i<INDEX_TABLES; i++)
      t->next[i] = NULL;

    assert(t->erased == FALSE);
    link_triple_hash(db, t);

    for( ; t2 && t2->erased; t2=t3 )
    { t3 = t2->next[BY_NONE];

      unlock_atoms(t2);
      PL_free(t2);
      db->freed++;
    }

    t->next[BY_NONE] = t2;
    if ( !t2 )
      db->by_none_tail = t;
  }

  if ( db->by_none == NULL )
    db->by_none_tail = NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
update_hash(). Note this may be called by  readers and writers, but must
be done only onces and certainly   not concurrently by multiple readers.
Hence we need a seperate lock.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
WANT_GC(rdf_db *db)
{ long dirty = db->erased - db->freed;
  long count = db->created - db->erased;

  if ( dirty > 1000 && dirty > count )
    return TRUE;
  if ( count > db->table_size[1]*4 )
    return TRUE;

  return FALSE;
}


static int
update_hash(rdf_db *db)
{ int want_gc = WANT_GC(db);

  if ( want_gc )
    DEBUG(1, Sdprintf("rdf_db: want GC\n"));

  if ( db->need_update || want_gc )
  { LOCK_MISC(db);

    if ( db->need_update )		/* check again */
    { if ( organise_predicates(db) )
      { DEBUG(1, Sdprintf("Re-hash ..."));
	invalidate_distinct_counts(db);
	rehash_triples(db);
	db->generation += (db->created-db->erased);
	DEBUG(1, Sdprintf("ok\n"));
      }
      db->need_update = 0;
    } else if ( WANT_GC(db) )
    { DEBUG(1, Sdprintf("rdf_db: GC ..."));
      rehash_triples(db);
      DEBUG(1, Sdprintf("ok\n"));
    }

    UNLOCK_MISC(db);
  }

  return TRUE;
}


/* MT: Must be locked */

static void
erase_triple(rdf_db *db, triple *t)
{ if ( !t->erased )
  { t->erased = TRUE;

    update_duplicates_del(db, t);

    if ( t->predicate->name == ATOM_subPropertyOf &&
	 t->objtype == OBJ_RESOURCE )
    { predicate *me    = lookup_predicate(db, t->subject);
      predicate *super = lookup_predicate(db, t->object.resource);

      delSubPropertyOf(db, me, super);
    }

    if ( t->first )
    { triple *one = first(db, t->subject);

      if ( one )
	one->first = TRUE;
      else
	db->subjects--;
    }
    db->erased++;
    t->predicate->triple_count--;
    unregister_source(db, t);
  }
}


static int
match_object(triple *t, triple *p)
{ if ( p->objtype )			/* something is filled in */
  { if ( p->objtype != t->objtype )
      return FALSE;

    switch( p->objtype )
    { case OBJ_RESOURCE:
	return t->object.resource == p->object.resource;
      case OBJ_STRING:
        if ( p->qualifier && t->qualifier && t->qualifier != p->qualifier )
	  return FALSE;
	if ( p->type_or_lang && t->type_or_lang != p->type_or_lang )
	  return FALSE;
	if ( p->object.string )
	{ if ( t->object.string != p->object.string )
	  { if ( p->match )
	      return match(p->match, p->object.string, t->object.string);
	    else
	      return FALSE;
	  }
	}
	return TRUE;
      case OBJ_INTEGER:
	return t->object.integer == p->object.integer;
      case OBJ_DOUBLE:
	return t->object.real == p->object.real;
      case OBJ_TERM:
	if ( p->object.term.len != t->object.term.len )
	  return FALSE;
	return memcmp(t->object.term.record, p->object.term.record,
		      p->object.term.len) == 0;
      default:
	assert(0);
    }

    return FALSE;
  }

  return TRUE;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Match triple t to pattern p.  Erased triples are always skipped.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
match_triples(triple *t, triple *p, unsigned flags)
{ DEBUG(3, Sdprintf("match_triple(");
	   print_triple(t, 0);
	   Sdprintf(")\n"));

  if ( t->erased )
    return FALSE;
  if ( p->subject && t->subject != p->subject )
    return FALSE;
  if ( !match_object(t, p) )
    return FALSE;
  if ( flags & MATCH_SRC )
  { if ( p->source && t->source != p->source )
      return FALSE;
    if ( p->line && t->line != p->line )
      return FALSE;
  }
					/* last; may be expensive */
  if ( p->predicate && t->predicate != p->predicate )
  { if ( (flags & MATCH_SUBPROPERTY) )
    { if ( !isSubPropertyOf(t->predicate, p->predicate) )
	return FALSE;
    } else
      return FALSE;
  }
  return TRUE;
}


		 /*******************************
		 *	      SAVE/LOAD		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The RDF triple format.  This format is intended for quick save and load
and not for readability or exchange.  Parts are based on the SWI-Prolog
Quick Load Format (implemented in pl-wic.c).

	<file> 		::= <magic>
			    <version>
			    [<source-file>]
			    [<md5>]
			    {<triple>}
			    'E'

	<magic> 	::= "RDF-dump\n"
	<version> 	::= <integer>

	<md5>		::= 'M'
			    <byte>* 		(16 bytes digest)

	<triple>	::= 'T'
	                    <subject>
			    <predicate>
			    <object>
			    <source>

	<subject>	::= <resource>
	<predicate>	::= <resource>

	<object>	::= "R" <resource>
			  | "L" <atom>

	<resource>	::= <atom>

	<atom>		::= "X" <integer>
			    "A" <string>

	<string>	::= <integer><bytes>

	<source-file>	::= <atom>

	<source>	::= <source-file>
			    <line>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define SAVE_MAGIC "RDF-dump\n"
#define SAVE_VERSION 1

typedef struct saved
{ atom_t name;
  long   as;
  struct saved *next;
} saved;


typedef struct save_context
{ saved ** saved_table;
  long     saved_size;
  long     saved_id;
} save_context;


long
next_table_size(long s0)
{ long size = 2;

  while(size < s0)
    size *= 2;

  return size;
}

static void
init_saved(rdf_db *db, save_context *ctx)
{ long size = next_table_size((db->created - db->erased)/8);
  long bytes = size * sizeof(*ctx->saved_table);

  ctx->saved_table = PL_malloc(bytes);
  memset(ctx->saved_table, 0, bytes);
  ctx->saved_size = size;
  ctx->saved_id = 0;
}

static void
destroy_saved(save_context *ctx)
{ if ( ctx->saved_table )
    PL_free(ctx->saved_table);
}

#define LONGBITSIZE (sizeof(long)*8)
#define PLMINLONG   ((long)(1L<<(LONGBITSIZE-1)))

static void
save_int(IOSTREAM *fd, long n)
{ int m;
  long absn = (n >= 0 ? n : -n);

  if ( n != PLMINLONG )
  { if ( absn < (1L << 5) )
    { Sputc((n & 0x3f), fd);
      return;
    } else if ( absn < (1L << 13) )
    { Sputc((((n >> 8) & 0x3f) | (1 << 6)), fd);
      Sputc((n & 0xff), fd);
      return;
    } else if ( absn < (1L << 21) )
    { Sputc((((n >> 16) & 0x3f) | (2 << 6)), fd);
      Sputc(((n >> 8) & 0xff), fd);
      Sputc((n & 0xff), fd);
      return;
    }
  }

  for(m = sizeof(n); ; m--)
  { int b = (absn >> (((m-1)*8)-1)) & 0x1ff;

    if ( b == 0 )
      continue;
    break;
  }

  Sputc(m | (3 << 6), fd);

  for( ; m > 0; m--)
  { int b = (n >> ((m-1)*8)) & 0xff;
    
    Sputc(b, fd);
  }
}


static void
save_atom(IOSTREAM *out, atom_t a, save_context *ctx)
{ int hash = atom_hash(a) % ctx->saved_size;
  saved *s;
  unsigned int len;
  const char *chars;
  unsigned int i;

  for(s=ctx->saved_table[hash]; s; s= s->next)
  { if ( s->name == a )
    { Sputc('X', out);
      save_int(out, s->as);

      return;
    }
  }

  s = PL_malloc(sizeof(*s));
  s->name = a;
  s->as = ctx->saved_id++;
  s->next = ctx->saved_table[hash];
  ctx->saved_table[hash] = s;

  Sputc('A', out);
  chars = PL_atom_nchars(a, &len);
  save_int(out, len);
  for(i=0; i<len; i++, chars++)
    Sputc(*chars, out);
}


#ifdef WORDS_BIGENDIAN
static const int double_byte_order[] = { 7,6,5,4,3,2,1,0 };
#else
static const int double_byte_order[] = { 0,1,2,3,4,5,6,7 };
#endif


static void
write_triple(IOSTREAM *out, triple *t, save_context *ctx)
{ Sputc('T', out);

  save_atom(out, t->subject, ctx);
  save_atom(out, t->predicate->name, ctx);

  if ( t->qualifier )
  { assert(t->type_or_lang);
    Sputc(t->qualifier == Q_LANG ? 'l' : 't', out);
    save_atom(out, t->type_or_lang, ctx);
  }

  switch(t->objtype)
  { case OBJ_RESOURCE:
      Sputc('R', out);
      save_atom(out, t->object.resource, ctx);
      break;
    case OBJ_STRING:
      Sputc('L', out);
      save_atom(out, t->object.string, ctx);
      break;
    case OBJ_INTEGER:
      Sputc('I', out);
      save_int(out, t->object.integer);
      break;
    case OBJ_DOUBLE:
    { double f = t->object.real;
      unsigned char *cl = (unsigned char *)&f;
      unsigned int i;
      
      Sputc('F', out);
      for(i=0; i<sizeof(double); i++)
	Sputc(cl[double_byte_order[i]], out);

      break;
    }
    case OBJ_TERM:
    { const char *s = t->object.term.record;
      int len = t->object.term.len;
      
      Sputc('T', out);
      save_int(out, len);
      while(--len >= 0)
	Sputc(*s++, out);

      break;
    }
    default:
      assert(0);
  }

  save_atom(out, t->source, ctx);
  save_int(out, t->line);
}


static void
write_md5(rdf_db *db, IOSTREAM *out, atom_t src)
{ source *s = lookup_source(db, src, FALSE);

  if ( s )
  { md5_byte_t *p = s->digest;
    int i;

    Sputc('M', out);
    for(i=0; i<16; i++)
      Sputc(*p++, out);
  }
}


static int
save_db(rdf_db *db, IOSTREAM *out, atom_t src)
{ triple *t;
  save_context ctx;

  if ( !RDLOCK(db) )
    return FALSE;
  init_saved(db, &ctx);

  Sfprintf(out, "%s", SAVE_MAGIC);
  save_int(out, SAVE_VERSION);
  if ( src )
  { Sputc('S', out);
    save_atom(out, src, &ctx);
    write_md5(db, out, src);
  }
  if ( Sferror(out) )
  { RDUNLOCK(db);
    return FALSE;
  }

  for(t = db->by_none; t; t = t->next[BY_NONE])
  { if ( !t->erased &&
	 (!src || t->source == src) )
    { write_triple(out, t, &ctx);
      if ( Sferror(out) )
	return FALSE;
    }
  }
  Sputc('E', out);
  if ( Sferror(out) )
  { RDUNLOCK(db);
    return FALSE;
  }

  destroy_saved(&ctx);
  RDUNLOCK(db);

  return TRUE;
}


static foreign_t
rdf_save_db(term_t stream, term_t source)
{ IOSTREAM *out;
  atom_t src;

  if ( !PL_get_stream_handle(stream, &out) )
    return type_error(stream, "stream");
  if ( !get_atom_or_var_ex(source, &src) )
    return FALSE;

  return save_db(DB, out, src);
}


static long
load_int(IOSTREAM *fd)
{ long first = Sgetc(fd);
  int bytes, shift, b;

  if ( !(first & 0xc0) )		/* 99% of them: speed up a bit */    
  { first <<= (LONGBITSIZE-6);
    first >>= (LONGBITSIZE-6);

    return first;
  }

  bytes = (int) ((first >> 6) & 0x3);
  first &= 0x3f;

  if ( bytes <= 2 )
  { for( b = 0; b < bytes; b++ )
    { first <<= 8;
      first |= Sgetc(fd) & 0xff;
    }

    shift = (sizeof(long)-1-bytes)*8 + 2;
  } else
  { int m;

    bytes = first;
    first = 0L;

    for(m=0; m<bytes; m++)
    { first <<= 8;
      first |= Sgetc(fd) & 0xff;
    }
    shift = (sizeof(long)-bytes)*8;
  }

  first <<= shift;
  first >>= shift;
  
  return first;
}

typedef struct ld_context
{ long		loaded_id;
  atom_t       *loaded_atoms;
  long		atoms_size;
  source       *source;
  md5_byte_t    digest[16];
  int		md5;
} ld_context;


static void
add_atom(atom_t a, ld_context *ctx)
{ if ( ctx->loaded_id >= ctx->atoms_size )
  { if ( ctx->atoms_size == 0 )
    { ctx->atoms_size = 1024;
      ctx->loaded_atoms = PL_malloc(sizeof(atom_t)*ctx->atoms_size);
    } else
    { ctx->atoms_size *= 2;
      ctx->loaded_atoms = PL_realloc(ctx->loaded_atoms,
				     sizeof(atom_t)*ctx->atoms_size);
    }
  }

  ctx->loaded_atoms[ctx->loaded_id++] = a;
}


static atom_t
load_atom(IOSTREAM *in, ld_context *ctx)
{ switch(Sgetc(in))
  { case 'X':
      return ctx->loaded_atoms[load_int(in)];
    case 'A':
    { int len = load_int(in);
      atom_t a;

      if ( len < 1024 )
      { char buf[1024];
	Sfread(buf, 1, len, in);
	a = PL_new_atom_nchars(len, buf);
      } else
      { char *buf = PL_malloc(len);
	Sfread(buf, 1, len, in);
	a = PL_new_atom_nchars(len, buf);
	PL_free(buf);
      }

      add_atom(a, ctx);
      return a;
    }
    default:
      assert(0);
  }
}


static double
load_double(IOSTREAM *fd)
{ double f;
  unsigned char *cl = (unsigned char *)&f;
  unsigned int i;

  for(i=0; i<sizeof(double); i++)
  { int c = Sgetc(fd);
    
    assert(c != EOF);
      
    cl[double_byte_order[i]] = c;
  }
  
  return f;
}


static int
load_triple(rdf_db *db, IOSTREAM *in, ld_context *ctx)
{ triple *t = PL_malloc(sizeof(*t));

  memset(t, 0, sizeof(*t));
  t->subject   = load_atom(in, ctx);
  t->predicate = lookup_predicate(db, load_atom(in, ctx));
value:
  switch(Sgetc(in))
  { case 'R':
      t->objtype = OBJ_RESOURCE;
      t->object.resource = load_atom(in, ctx);
      break;
    case 'L':
      t->objtype = OBJ_STRING;
      t->object.resource = load_atom(in, ctx);
      break;
    case 'I':
      t->objtype = OBJ_INTEGER;
      t->object.integer = load_int(in);
      break;
    case 'F':
      t->objtype = OBJ_DOUBLE;
      t->object.real = load_double(in);
      break;
    case 'T':
    { int i;
      char *s;

      t->objtype = OBJ_TERM;
      t->object.term.len = load_int(in);
      t->object.term.record = PL_malloc(t->object.term.len);
      s = (char *)t->object.term.record;

      for(i=0; i<t->object.term.len; i++)
	s[i] = Sgetc(in);

      break;
    }
    case 'l':
      t->qualifier = Q_LANG;
      t->type_or_lang = load_atom(in, ctx);
      goto value;
    case 't':
      t->qualifier = Q_TYPE;
      t->type_or_lang = load_atom(in, ctx);
      goto value;
    default:
      assert(0);
  }
  t->source = load_atom(in, ctx);
  t->line   = load_int(in);

  if ( db->tr_first )
  { record_transaction(db, TR_ASSERT, t);     
  } else
  { link_triple(db, t);
  }

  return TRUE;
}


static int
load_magic(IOSTREAM *in)
{ char *s = SAVE_MAGIC;

  for( ; *s; s++)
  { if ( Sgetc(in) != *s )
      return FALSE;
  }

  return TRUE;
}


static int
load_db(rdf_db *db, IOSTREAM *in)
{ ld_context ctx;
  int version;
  int c;
  long created0 = db->created;

  if ( !load_magic(in) )
    return FALSE;
  version = load_int(in);
  
  memset(&ctx, 0, sizeof(ctx));

  if ( !WRLOCK(db, FALSE) )
    return FALSE;

  while((c=Sgetc(in)) != EOF)
  { switch(c)
    { case 'T':
	if ( !load_triple(db, in, &ctx) )
	{ WRUNLOCK(db);
	  return FALSE;
	}
        break;
      case 'S':
	ctx.source = lookup_source(db, load_atom(in, &ctx), TRUE);
        break;
      case 'M':
      { int i;

	for(i=0; i<16; i++)
	  ctx.digest[i] = Sgetc(in);

	if ( ctx.source && ctx.source->md5 )
	{ ctx.md5 = ctx.source->md5;
	  if ( db->tr_first )
	  { record_md5_transaction(db, ctx.source, NULL);
	  } else
	  { ctx.source->md5 = FALSE;
	  }
	}

	break;
      }
      case 'E':
	if ( ctx.loaded_atoms )
	  PL_free(ctx.loaded_atoms);

        if ( ctx.md5 )
	{ if ( db->tr_first )
	  { md5_byte_t *d = PL_malloc(sizeof(ctx.digest));
	    memcpy(d, ctx.digest, sizeof(ctx.digest));
	    record_md5_transaction(db, ctx.source, d);
	  } else
	  { sum_digest(ctx.source->digest, ctx.digest);
	    ctx.source->md5 = ctx.md5;
	  }
	}

	db->generation += (db->created-created0);
	WRUNLOCK(db);
	return TRUE;
      default:
	break;
    }
  }
  
  WRUNLOCK(db);
  return PL_warning("Illegal RDF triple file");
}


static foreign_t
rdf_load_db(term_t stream)
{ IOSTREAM *in;

  if ( !PL_get_stream_handle(stream, &in) )
    return type_error(stream, "stream");

  return load_db(DB, in);
}


#ifdef WITH_MD5
		 /*******************************
		 *	     MD5 SUPPORT	*
		 *******************************/

static void
md5_triple(triple *t, md5_byte_t *digest)
{ md5_state_t state;
  unsigned int len;
  char tmp[2];
  const char *s;

  md5_init(&state);
  s = PL_atom_nchars(t->subject, &len);
  md5_append(&state, (const md5_byte_t *)s, len);
  md5_append(&state, "P", 1);
  s = PL_atom_nchars(t->predicate->name, &len);
  md5_append(&state, (const md5_byte_t *)s, len);
  tmp[0] = 'O';
  tmp[1] = (char)t->objtype;
  md5_append(&state, tmp, 2);
  switch(t->objtype)
  { case OBJ_RESOURCE:
      s = PL_atom_nchars(t->object.resource, &len);
      break;
    case OBJ_STRING:
      s = PL_atom_nchars(t->object.string, &len);
      break;
    case OBJ_INTEGER:			/* TBD: byte order issues */
      s = (const char *)&t->object.integer;
      len = sizeof(t->object.integer);
      break;
    case OBJ_DOUBLE:
      s = (const char *)&t->object.real;
      len = sizeof(t->object.real);
      break;
    case OBJ_TERM:
      s = (const char *)t->object.term.record;
      len = t->object.term.len;
      break;
    default:
      assert(0);
  }
  md5_append(&state, (const md5_byte_t *)s, len);
  if ( t->qualifier )
  { assert(t->type_or_lang);
    md5_append(&state, t->qualifier == Q_LANG ? "l" : "t", 1);
    s = PL_atom_nchars(t->type_or_lang, &len);
    md5_append(&state, (const md5_byte_t *)s, len);
  }
  if ( t->source )
  { md5_append(&state, "S", 1);
    s = PL_atom_nchars(t->source, &len);
    md5_append(&state, (const md5_byte_t *)s, len);
  }
  
  md5_finish(&state, digest);
}


static void
sum_digest(md5_byte_t *digest, md5_byte_t *add)
{ md5_byte_t *p, *q;
  int n;

  for(p=digest, q=add, n=16; --n>=0; )
    *p++ += *q++;
}


static void
dec_digest(md5_byte_t *digest, md5_byte_t *add)
{ md5_byte_t *p, *q;
  int n;

  for(p=digest, q=add, n=16; --n>=0; )
    *p++ -= *q++;
}


static int
md5_unify_digest(term_t t, md5_byte_t digest[16])
{ char hex_output[16*2];
  int di;
  char *pi;
  static char hexd[] = "0123456789abcdef";

  for(pi=hex_output, di = 0; di < 16; ++di)
  { *pi++ = hexd[(digest[di] >> 4) & 0x0f];
    *pi++ = hexd[digest[di] & 0x0f];
  }

  return PL_unify_atom_nchars(t, 16*2, hex_output);
}


static foreign_t
rdf_md5(term_t file, term_t md5)
{ atom_t src;
  int rc;
  rdf_db *db = DB;

  if ( !get_atom_or_var_ex(file, &src) )
    return FALSE;

  if ( src )
  { source *s;

    if ( !RDLOCK(db) )
      return FALSE;
    if ( (s = lookup_source(db, src, FALSE)) )
    { rc = md5_unify_digest(md5, s->digest);
    } else
    { md5_byte_t digest[16];

      memset(digest, 0, sizeof(digest));
      rc = md5_unify_digest(md5, digest);
    }
    RDUNLOCK(db);
  } else
  { md5_byte_t digest[16];
    source **ht;
    int i;
    
    memset(&digest, 0, sizeof(digest));

    if ( !RDLOCK(db) )
      return FALSE;

    for(i=0,ht = db->source_table; i<db->source_table_size; i++, ht++)
    { source *s;
      
      for( s = *ht; s; s = s->next )
	sum_digest(digest, s->digest);
    }

    rc = md5_unify_digest(md5, digest);
    RDUNLOCK(db);
  }

  return rc;
}


static foreign_t
rdf_atom_md5(term_t text, term_t times, term_t md5)
{ char *s;
  int n, i, len;
  md5_byte_t digest[16];

  if ( !PL_get_nchars(text, &len, &s, CVT_ALL) )
    return type_error(text, "text");
  if ( !PL_get_integer(times, &n) )
    return type_error(times, "integer");
  if ( n < 1 )
    return domain_error(times, "positive_integer");

  for(i=0; i<n; i++)
  { md5_state_t state;
    md5_init(&state);
    md5_append(&state, (const md5_byte_t *)s, len);
    md5_finish(&state, digest);
    s = digest;
    len = sizeof(digest);
  }

  return md5_unify_digest(md5, digest);
}



#endif /*WITH_MD5*/


		 /*******************************
		 *	       ATOMS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Lock atoms in triple against AGC. Note that the predicate name is locked
in the predicate structure.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
lock_atoms(triple *t)
{ PL_register_atom(t->subject);
  switch(t->objtype)
  { case OBJ_RESOURCE:
      PL_register_atom(t->object.resource);
      break;
    case OBJ_STRING:
      PL_register_atom(t->object.string);
      break;
  }
}


static void
unlock_atoms(triple *t)
{ PL_unregister_atom(t->subject);
  switch(t->objtype)
  { case OBJ_RESOURCE:
      PL_unregister_atom(t->object.resource);
      break;
    case OBJ_STRING:
      PL_unregister_atom(t->object.string);
      break;
  }
}


		 /*******************************
		 *      PROLOG CONVERSION	*
		 *******************************/

#define LIT_TYPED	0x1
#define LIT_NOERROR	0x2
#define LIT_PARTIAL	0x4

static int
get_lit_atom_ex(term_t t, atom_t *a, int flags)
{ if ( PL_get_atom(t, a) )
    return TRUE;
  if ( (flags & LIT_PARTIAL) && PL_is_variable(t) )
  { *a = 0L;
    return TRUE;
  }

  return type_error(t, "atom");
}


static int
get_literal(term_t lit, triple *t, int flags)
{ if ( PL_get_atom(lit, &t->object.string) )
  { t->objtype = OBJ_STRING;
  } else if ( PL_is_integer(lit) && PL_get_long(lit, &t->object.integer) )
  { t->objtype = OBJ_INTEGER;
  } else if ( PL_get_float(lit, &t->object.real) )
  { t->objtype = OBJ_DOUBLE;
  } else if ( PL_is_functor(lit, FUNCTOR_lang2) )
  { term_t a = PL_new_term_ref();
    
    PL_get_arg(1, lit, a);
    if ( !get_lit_atom_ex(a, &t->type_or_lang, flags) )
      return FALSE;
    PL_get_arg(2, lit, a);
    if ( !get_lit_atom_ex(a, &t->object.string, flags) )
      return FALSE;

    t->qualifier = Q_LANG;
    t->objtype = OBJ_STRING;
  } else if ( PL_is_functor(lit, FUNCTOR_type2) &&
	      !(flags & LIT_TYPED) )	/* avoid recursion */
  { term_t a = PL_new_term_ref();
    
    PL_get_arg(1, lit, a);
    if ( !get_lit_atom_ex(a, &t->type_or_lang, flags) )
      return FALSE;
    t->qualifier = Q_TYPE;
    PL_get_arg(2, lit, a);

    return get_literal(a, t, LIT_TYPED|flags);
  } else if ( !PL_is_ground(lit) )
  { if ( !(flags & LIT_PARTIAL) )
      return type_error(lit, "rdf_object");
  } else
  { t->object.term.record = PL_record_external(lit, &t->object.term.len);
    t->objtype = OBJ_TERM;
  }

  return TRUE;
}


static int
get_object(term_t object, triple *t)
{ if ( PL_get_atom(object, &t->object.resource) )
    t->objtype = OBJ_RESOURCE;
  else if ( PL_is_functor(object, FUNCTOR_literal1) )
  { term_t a = PL_new_term_ref();
    
    PL_get_arg(1, object, a);
    return get_literal(a, t, 0);
  } else
    return type_error(object, "rdf_object");

  return TRUE;
}


static int
get_src(term_t src, triple *t)
{ if ( src && !PL_is_variable(src) )
  { if ( PL_get_atom(src, &t->source) )
    { t->line = NO_LINE;
    } else if ( PL_is_functor(src, FUNCTOR_colon2) )
    { term_t a = PL_new_term_ref();
      long line;
      
      PL_get_arg(1, src, a);
      if ( !get_atom_or_var_ex(a, &t->source) )
	return FALSE;
      PL_get_arg(2, src, a);
      if ( PL_get_long(a, &line) )
	t->line = line;
      else if ( !PL_is_variable(a) )
	return type_error(a, "integer");
    } else
      return type_error(src, "rdf_source");
  }

  return TRUE;
}


static int
get_predicate(rdf_db *db, term_t t, predicate **p)
{ atom_t name;

  if ( !get_atom_ex(t, &name ) )
    return FALSE;

  *p = lookup_predicate(db, name);
  return TRUE;
}


static int
get_triple(rdf_db *db,
	   term_t subject, term_t predicate, term_t object,
	   triple *t)
{ if ( !get_atom_ex(subject, &t->subject) ||
       !get_predicate(db, predicate, &t->predicate) ||
       !get_object(object, t) )
    return FALSE;

  return TRUE;
}


static int
get_partial_triple(rdf_db *db,
		   term_t subject, term_t predicate, term_t object,
		   term_t src, triple *t)
{ if ( subject && !get_atom_or_var_ex(subject, &t->subject) )
    return FALSE;
  if ( !PL_is_variable(predicate) &&
       !get_predicate(db, predicate, &t->predicate) )
    return FALSE;
					/* the object */
  if ( object && !PL_is_variable(object) )
  { if ( PL_get_atom(object, &t->object.resource) )
      t->objtype = OBJ_RESOURCE;
    else if ( PL_is_functor(object, FUNCTOR_literal1) )
    { term_t a = PL_new_term_ref();
      
      PL_get_arg(1, object, a);
      if ( !get_literal(a, t, LIT_PARTIAL) )
	return FALSE;
    } else if ( PL_is_functor(object, FUNCTOR_literal2) )
    { term_t a = PL_new_term_ref();
      
      PL_get_arg(1, object, a);
      if ( PL_is_functor(a, FUNCTOR_exact1) )
	t->match = STR_MATCH_EXACT;
      else if ( PL_is_functor(a, FUNCTOR_substring1) )
	t->match = STR_MATCH_SUBSTRING;
      else if ( PL_is_functor(a, FUNCTOR_word1) )
	t->match = STR_MATCH_WORD;
      else if ( PL_is_functor(a, FUNCTOR_prefix1) )
	t->match = STR_MATCH_PREFIX;
      else if ( PL_is_functor(a, FUNCTOR_like1) )
	t->match = STR_MATCH_LIKE;
      else 
	return domain_error(a, "match_type");
      PL_get_arg(1, a, a);
      if ( !get_atom_or_var_ex(a, &t->object.string) )
	return FALSE;
      t->objtype = OBJ_STRING;
    } else
      return type_error(object, "rdf_object");
  }
					/* the source */
  if ( !get_src(src, t) )
    return FALSE;

  if ( t->subject )
    t->indexed |= BY_S;
  if ( t->predicate )
    t->indexed |= BY_P;
  if ( t->objtype == OBJ_STRING && t->match <= STR_MATCH_EXACT )
    t->indexed |= BY_O;
  else if ( t->objtype == OBJ_RESOURCE )
    t->indexed |= BY_O;

  db->indexed[t->indexed]++;		/* statistics */

  switch(t->indexed)
  { case BY_SPO:
      t->indexed = BY_SP;
      break;
    case BY_SO:
      t->indexed = BY_S;
      break;
  }

  return TRUE;
}


static int
inverse_partial_triple(triple *t)
{ predicate *i = 0;

  if ( !t->inversed &&
       (!t->predicate || (i=t->predicate->inverse_of)) &&
       t->objtype <= OBJ_RESOURCE )	/* also allow OBJ_UNTYPED */
  { atom_t o = t->object.resource;

    if ( (t->object.resource = t->subject) )
      t->objtype = OBJ_RESOURCE;
    t->subject = o;

    if ( t->predicate )
    { if ( i == t->predicate )		/* symmetric */
      { 
      } else
      { t->predicate = i;
      }
    }

    t->indexed  = by_inverse[t->indexed];
    t->inversed = TRUE;

    return TRUE;
  }

  return FALSE;
}


static int
get_source(term_t src, triple *t)
{ if ( PL_get_atom(src, &t->source) )
  { t->line = NO_LINE;
    return TRUE;
  }

  if ( PL_is_functor(src, FUNCTOR_colon2) )
  { term_t a = PL_new_term_ref();
    long line;

    PL_get_arg(1, src, a);
    if ( !get_atom_ex(a, &t->source) )
      return FALSE;
    PL_get_arg(2, src, a);
    if ( !get_long_ex(a, &line) )
      return FALSE;
    t->line = line;

    return TRUE;
  }

  return type_error(src, "rdf_source");
}


static int
unify_source(term_t src, triple *t)
{ if ( t->line == NO_LINE )
  { if ( !PL_unify_atom(src, t->source) )
      return PL_unify_term(src,
			   PL_FUNCTOR, FUNCTOR_colon2,
			     PL_ATOM, t->source,
			     PL_VARIABLE);  
    return TRUE;
  } else
    return PL_unify_term(src,
			 PL_FUNCTOR, FUNCTOR_colon2,
			   PL_ATOM, t->source,
			   PL_INTEGER, t->line);
}


static int
put_value(term_t v, triple *t)
{ switch(t->objtype)
  { case OBJ_STRING:
      PL_put_atom(v, t->object.string);
      break;
    case OBJ_INTEGER:
      PL_put_integer(v, t->object.integer);
      break;
    case OBJ_DOUBLE:
      PL_put_float(v, t->object.real);
      break;
    case OBJ_TERM:
      PL_recorded_external(t->object.term.record, v);
      break;
    default:
      assert(0);
      return FALSE;
  }

  return TRUE;
}



static int
unify_object(term_t object, triple *t)
{ if ( t->objtype == OBJ_RESOURCE )
  { return PL_unify_atom(object, t->object.resource);
  } else
  { term_t v = PL_new_term_ref();
    term_t lit = PL_new_term_ref();

    put_value(v, t);

    if ( PL_unify_functor(object, FUNCTOR_literal1) )
      PL_get_arg(1, object, lit);
    else if ( PL_is_functor(object, FUNCTOR_literal2) )
      PL_get_arg(2, object, lit);

    if ( t->qualifier )
    { functor_t qf;

      assert(t->type_or_lang);

      if ( t->qualifier == Q_LANG )
	qf = FUNCTOR_lang2;
      else
	qf = FUNCTOR_type2;

      if ( PL_unify_term(lit, PL_FUNCTOR, qf,
			   PL_ATOM, t->type_or_lang,
			   PL_TERM, v) )
	return TRUE;

      return PL_unify(lit, v);		/* allow rdf(X, Y, literal(foo)) */
    } else if ( PL_unify(lit, v) )
    { return TRUE;
    } else if ( PL_is_functor(lit, FUNCTOR_lang2) &&
		t->objtype == OBJ_STRING )
    { term_t a = PL_new_term_ref();
      PL_get_arg(2, lit, a);
      return PL_unify(a, v);
    } else if ( PL_is_functor(lit, FUNCTOR_type2) )
    { term_t a = PL_new_term_ref();
      PL_get_arg(2, lit, a);
      return PL_unify(a, v);
    } else
      return FALSE;
  }
}


static int
unify_triple(term_t subject, term_t pred, term_t object,
	     term_t src, triple *t, int inversed)
{ predicate *p = t->predicate;
  fid_t fid;

  if ( inversed )
  { term_t tmp = object;
    object = subject;
    subject = tmp;

    if ( !(p = p->inverse_of) )
      return FALSE;
  }

  fid = PL_open_foreign_frame();

  if ( !PL_unify_atom(subject, t->subject) ||
       !PL_unify_atom(pred, p->name) ||
       !unify_object(object, t) ||
       (src && !unify_source(src, t)) )
  { PL_discard_foreign_frame(fid);
    return FALSE;
  } else
  { PL_close_foreign_frame(fid);
    return TRUE;
  }
}


		 /*******************************
		 *	DUBLICATE HANDLING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
According to the RDF specs, duplicate triples  have no meaning, but they
slow down search and often produce   duplicate results in search. Worse,
some coding styles proposed in the  OWL documents introduce huge amounts
of duplicate triples. We cannot  simply  ignore   a  triple  if  it is a
duplicate as a subsequent retract  would   delete  the final triple. For
example, after loading two  files  that   contain  the  same  triple and
unloading one of these files the database would be left without triples.

In our solution, if a triple is added as a duplicate, it is flagged such
using  the  flag  is_duplicate.  The  `principal'  triple  has  a  count
`duplicates',  indicating  the  number  of   duplicate  triples  in  the
database.

It might make sense to  introduce  the   BY_SPO  table  as fully indexed
lookups are frequent with the introduction of duplicate detection.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static int
update_duplicates_add(rdf_db *db, triple *t)
{ triple *d;
  const int indexed = BY_SP;

  assert(t->is_duplicate == FALSE);
  assert(t->duplicates == 0);

  d = db->table[indexed][triple_hash(db, t, indexed)];
  for( ; d && d != t; d = d->next[indexed] )
  { if ( match_triples(d, t, MATCH_EXACT) )
    { t->is_duplicate = TRUE;
      assert( !d->is_duplicate );

      d->duplicates++;

      DEBUG(1,
	    print_triple(t, PRT_SRC);
	    Sdprintf(" %p: %d-th duplicate: ", t, d->duplicates);
	    Sdprintf("Location of first (%p)\n", d));
      
      assert(d->duplicates);		/* check overflow */
      db->duplicates++;
      return TRUE;
    }
  }

  return FALSE;
}


static void				/* t is about to be deleted */
update_duplicates_del(rdf_db *db, triple *t)
{ const int indexed = BY_SP;

  db->duplicates--;

  if ( t->duplicates )			/* I am the principal one */
  { triple *d;
      
    DEBUG(1,
	  print_triple(t, PRT_SRC);
	  Sdprintf(": Deleting %p, %d duplicates: ", t, t->duplicates));

    d = db->table[indexed][triple_hash(db, t, indexed)];
    for( ; d; d = d->next[indexed] )
    { if ( d != t && match_triples(d, t, MATCH_EXACT) )
      { assert(d->is_duplicate);
	d->is_duplicate = FALSE;
	d->duplicates = t->duplicates-1;
	DEBUG(1, Sdprintf("New principal: %p\n", d));

	return;
      }
    }
    assert(0);
  } else if ( t->is_duplicate )		/* I am a duplicate */
  { triple *d;
      
    DEBUG(1,
	  print_triple(t, PRT_SRC);
	  Sdprintf(": Deleting, is a duplicate: "));

    d = db->table[indexed][triple_hash(db, t, indexed)];
    for( ; d; d = d->next[indexed] )
    { if ( d != t && match_triples(d, t, MATCH_EXACT) )
      { if ( d->duplicates )
	{ d->duplicates--;
	  DEBUG(1, Sdprintf("Principal %p has %d duplicates\n",
			    d, d->duplicates));
	  return;
	}
      }
    }
    Sdprintf("FATAL\n");
    PL_halt(1);
    assert(0);
  }
}


		 /*******************************
		 *	    TRANSACTIONS	*
		 *******************************/

static void
append_transaction(rdf_db *db, transaction_record *tr)
{ if ( db->tr_last )
  { tr->next = NULL;
    tr->previous = db->tr_last;
    db->tr_last->next = tr;
    db->tr_last = tr;
  } else
  { tr->next = tr->previous = NULL;
    db->tr_first = db->tr_last = tr;
  }
}


static void
open_transaction(rdf_db *db)
{ transaction_record *tr = PL_malloc(sizeof(*tr));

  memset(tr, 0, sizeof(*tr));
  tr->type = TR_MARK;

  if ( db->tr_first )
    db->tr_nesting++;
  else
    db->tr_nesting = 0;

  append_transaction(db, tr);
}


static void
record_transaction(rdf_db *db, tr_type type, triple *t)
{ transaction_record *tr = PL_malloc(sizeof(*tr));

  memset(tr, 0, sizeof(*tr));
  tr->type = type;
  tr->triple = t;

  append_transaction(db, tr);
}


static void
record_md5_transaction(rdf_db *db, source *src, md5_byte_t *digest)
{ transaction_record *tr = PL_malloc(sizeof(*tr));

  memset(tr, 0, sizeof(*tr));
  tr->type = TR_UPDATE_MD5,
  tr->update.md5.source = src;
  tr->update.md5.digest = digest;

  append_transaction(db, tr);
}


static void
record_update_transaction(rdf_db *db, triple *t, triple *new)
{ transaction_record *tr = PL_malloc(sizeof(*tr));

  memset(tr, 0, sizeof(*tr));
  tr->type = TR_UPDATE,
  tr->triple = t;
  tr->update.triple = new;

  append_transaction(db, tr);
}


static void
record_update_src_transaction(rdf_db *db, triple *t,
			      atom_t src, unsigned long line)
{ transaction_record *tr = PL_malloc(sizeof(*tr));

  memset(tr, 0, sizeof(*tr));
  tr->type = TR_UPDATE_SRC,
  tr->triple = t;
  tr->update.src.atom = src;
  tr->update.src.line = line;

  append_transaction(db, tr);
}


static void
free_transaction(transaction_record *tr)
{ switch(tr->type)
  { case TR_ASSERT:
      unlock_atoms(tr->triple);
      PL_free(tr->triple);
      break;
    case TR_UPDATE:
      unlock_atoms(tr->update.triple);
      PL_free(tr->update.triple);
      break;
    case TR_UPDATE_MD5:
      if ( tr->update.md5.digest )
	PL_free(tr->update.md5.digest);
      break;
    default:
      break;
  }

  PL_free(tr);
}


static void
truncate_transaction(rdf_db *db, transaction_record *last)
{ db->tr_last = last;
  if ( last )
  { db->tr_last->next = NULL;
  } else
  { db->tr_first = NULL;
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
discard_transaction()  simply  destroys  all   actions    in   the  last
transaction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
discard_transaction(rdf_db *db)
{ transaction_record *tr, *prev;

  for(tr=db->tr_last; tr; tr = prev)
  { prev = tr->previous;

    if ( tr->type == TR_MARK )
    { PL_free(tr);
      truncate_transaction(db, prev);
      db->tr_nesting--;
      return;
    }
    
    free_transaction(tr);
  }
}


static int
commit_transaction(rdf_db *db)
{ transaction_record *tr, *next;

  if ( db->tr_nesting > 0 )		/* commit nested transaction */
  { tr=db->tr_last;

    if ( tr->type == TR_MARK )		/* empty nested transaction */
    { truncate_transaction(db, tr->previous);
      PL_free(tr);
      db->tr_nesting--;

      return TRUE;
    }

    for(; tr; tr = tr->previous)	/* not the last (tested above) */
    {					/* not the first (we are nested) */
      if ( tr->type == TR_MARK )
      { tr->previous->next = tr->next;
	tr->next->previous = tr->previous;
	PL_free(tr);
	db->tr_nesting--;

	return TRUE;
      }
    }

    assert(0);
    return FALSE;
  }

					/* real commit */
  for(tr=db->tr_first; tr; tr = next)
  { next = tr->next;

    switch(tr->type)
    { case TR_MARK:
	break;
      case TR_ASSERT:
	link_triple(db, tr->triple);
        db->generation++;
	break;
      case TR_RETRACT:
	if ( !tr->triple->erased )	/* already erased */
	{ erase_triple(db, tr->triple);
	  db->generation++;
	}
	break;
      case TR_UPDATE:
	erase_triple(db, tr->triple);
        link_triple(db, tr->update.triple);
	db->generation++;
	break;
      case TR_UPDATE_SRC:
        if ( tr->triple->source != tr->update.src.atom )
	{ if ( tr->triple->source )
	    unregister_source(db, tr->triple);
	  tr->triple->source = tr->update.src.atom;
	  if ( tr->triple->source )
	    register_source(db, tr->triple);
	}
        tr->triple->line = tr->update.src.line;
	db->generation++;
	break;
      case TR_UPDATE_MD5:
      { source *src = tr->update.md5.source;
	md5_byte_t *digest = tr->update.md5.digest;
	if ( digest )
	{ sum_digest(digest, src->digest);
	  src->md5 = TRUE;
	  PL_free(digest);
	} else
	{ src->md5 = FALSE;
	}
	break;
      }
      case TR_RESET:
	reset_db(db);
        break;
      default:
	assert(0);
    }

    PL_free(tr);
  }

  db->tr_first = db->tr_last = NULL;

  return TRUE;
}


static foreign_t
rdf_transaction(term_t goal)
{ int rc;
  rdf_db *db = DB;

  if ( !WRLOCK(db, TRUE) )
    return FALSE;

  open_transaction(db);

  rc = PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, PRED_call1, goal);

  if ( rc )
  { if ( db->tr_nesting == 0 )
    { if ( db->tr_last && db->tr_last->type != TR_MARK && /* not empty */
	   !LOCKOUT_READERS(db) )
	goto discard;
    }

    commit_transaction(db);
  } else
  { discard:
    discard_transaction(db);
  }
  WRUNLOCK(db);

  return rc;
}




		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

static foreign_t
rdf_assert4(term_t subject, term_t predicate, term_t object, term_t src)
{ triple *t = PL_malloc(sizeof(*t));
  rdf_db *db = DB;

  memset(t, 0, sizeof(*t));
  if ( !get_triple(db, subject, predicate, object, t) )
  { PL_free(t);
    return FALSE;
  }
  if ( src )
  { if ( !get_source(src, t) )
    { PL_free(t);
      return FALSE;
    }
  } else
  { t->source = ATOM_user;
    t->line = NO_LINE;
  }

  lock_atoms(t);
  if ( !WRLOCK(db, FALSE) )
  { unlock_atoms(t);
    PL_free(t);
    return FALSE;
  }

  if ( db->tr_first )
  { record_transaction(db, TR_ASSERT, t);
  } else
  { link_triple(db, t);
    db->generation++;
  }
  WRUNLOCK(db);

  return TRUE;
}


static foreign_t
rdf_assert3(term_t subject, term_t predicate, term_t object)
{ return rdf_assert4(subject, predicate, object, 0);
}


static foreign_t
rdf(term_t subject, term_t predicate, term_t object,
    term_t src, term_t realpred, control_t h, unsigned flags)
{ term_t retpred = realpred ? realpred : predicate;
  rdf_db *db = DB;

  switch(PL_foreign_control(h))
  { case PL_FIRST_CALL:
    { triple t, *p;
      
      memset(&t, 0, sizeof(t));
      if ( !get_partial_triple(db, subject, predicate, object, src, &t) )
	return FALSE;

      if ( !RDLOCK(db) )
	return FALSE;
      if ( !update_hash(db) )
      { RDUNLOCK(db);
	return FALSE;
      }

    inverse:
      p = db->table[t.indexed][triple_hash(db, &t, t.indexed)];
      for( ; p; p = p->next[t.indexed])
      { if ( match_triples(p, &t, flags) )
	{ if ( !unify_triple(subject, retpred, object, src, p, t.inversed) )
	    continue;
	  if ( realpred && PL_is_variable(predicate) )
	    PL_unify(predicate, retpred);

	  p=p->next[t.indexed];
	inv_alt:
	  for(; p; p = p->next[t.indexed])
	  { if ( p->is_duplicate && !src )
	      continue;

	    if ( match_triples(p, &t, flags) )
	    { t.next[0] = p;
	      
	      db->active_queries++;
	      PL_retry_address(save_triple(&t));
	    }
	  }

	  if ( (flags & MATCH_INVERSE) && inverse_partial_triple(&t) )
	  { p = db->table[t.indexed][triple_hash(db, &t, t.indexed)];
	    goto inv_alt;
	  }
	  RDUNLOCK(db);
          return TRUE;
	}
      }

      if ( (flags & MATCH_INVERSE) && inverse_partial_triple(&t) )
	goto inverse;
      RDUNLOCK(db);
      return FALSE;
    }
    case PL_REDO:
    { triple *p, *t = PL_foreign_context_address(h);

      p = t->next[0];
    retry_inv:
      for( ; p; p = p->next[t->indexed])
      { if ( p->is_duplicate && !src )
	  continue;

	if ( match_triples(p, t, flags) )
	{ if ( !unify_triple(subject, retpred, object, src, p, t->inversed) )
	    continue;
	  if ( realpred && PL_is_variable(predicate) )
	    PL_unify(predicate, retpred);

	  p=p->next[t->indexed];
	retry_inv_alt:
	  for(; p; p = p->next[t->indexed])
	  { if ( match_triples(p, t, flags) )
	    { t->next[0] = p;
	      
	      PL_retry_address(t);
	    }
	  }

	  if ( (flags & MATCH_INVERSE) && inverse_partial_triple(t) )
	  { p = db->table[t->indexed][triple_hash(db, t, t->indexed)];
	    goto retry_inv_alt;
	  }

          PL_free(t);
	  db->active_queries--;
	  RDUNLOCK(db);
          return TRUE;
	}
      }
      if ( (flags & MATCH_INVERSE) && inverse_partial_triple(t) )
      { p = db->table[t->indexed][triple_hash(db, t, t->indexed)];
	goto retry_inv;
      }
      PL_free(t);
      db->active_queries--;
      RDUNLOCK(db);
      return FALSE;
    }
    case PL_CUTTED:
    { triple *t = PL_foreign_context_address(h);

      db->active_queries--;
      PL_free(t);
      RDUNLOCK(db);
      return TRUE;
    }
    default:
      RDUNLOCK(db);
      assert(0);
      return FALSE;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rdf(Subject, Predicate, Object)

Search specifications:

	Predicate:

		subPropertyOf(X) = P

	Object:

		literal(substring(X), L)
		literal(word(X), L)
		literal(exact(X), L)
		literal(prefix(X), L)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static foreign_t
rdf3(term_t subject, term_t predicate, term_t object, control_t h)
{ return rdf(subject, predicate, object, 0, 0, h,
	     MATCH_EXACT);
}


static foreign_t
rdf4(term_t subject, term_t predicate, term_t object,
     term_t src, control_t h)
{ return rdf(subject, predicate, object, src, 0, h,
	     MATCH_EXACT|MATCH_SRC);
}


static foreign_t
rdf_has(term_t subject, term_t predicate, term_t object,
	term_t realpred, control_t h)
{ return rdf(subject, predicate, object, 0, realpred, h,
	     MATCH_SUBPROPERTY|MATCH_INVERSE);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rdf_estimate_complexity(+S,+P,+O,-C)

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
rdf_estimate_complexity(term_t subject, term_t predicate, term_t object,
		        term_t complexity)
{ triple t;
  long c;
  rdf_db *db = DB;
  int rc;

  memset(&t, 0, sizeof(t));
  if ( !get_partial_triple(db, subject, predicate, object, 0, &t) )
    return FALSE;
  
  if ( !RDLOCK(db) )
    return FALSE;
  if ( !update_hash(db) )			/* or ignore this problem? */
  { RDUNLOCK(db);
    return FALSE;
  }

  if ( t.indexed == BY_NONE )
  { c = db->created - db->erased;		/* = totale triple count */
  } else
  { c = db->counts[t.indexed][triple_hash(db, &t, t.indexed)];
  }

  rc = PL_unify_integer(complexity, c);
  RDUNLOCK(db);

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rdf_update(+Subject, +Predicate, +Object, +Action)

Update a triple. Please note this is actually erase+assert as the triple
needs to be updated in  the  linked   lists  while  erase simply flags a
triple as `erases' without deleting it   to support queries which active
choicepoints.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
update_triple(rdf_db *db, term_t action, triple *t)
{ term_t a = PL_new_term_ref();
  triple tmp = *t;
  triple *new;
  int i;

  if ( !PL_get_arg(1, action, a) )
    return type_error(action, "rdf_action");

  if ( PL_is_functor(action, FUNCTOR_subject1) )
  { atom_t s;

    if ( !get_atom_ex(a, &s) )
      return FALSE;
    if ( tmp.subject == s )
      return TRUE;			/* no change */

    tmp.subject = s;
  } else if ( PL_is_functor(action, FUNCTOR_predicate1) )
  { predicate *p;

    if ( !get_predicate(db, a, &p) )
      return FALSE;
    if ( tmp.predicate == p )
      return TRUE;			/* no change */

    tmp.predicate = p;
  } else if ( PL_is_functor(action, FUNCTOR_object1) )
  { triple t2;

    memset(&t2, 0, sizeof(t2));
    if ( !get_object(a, &t2) )
      return FALSE;
    if ( match_object(&t2, &tmp) && t2.qualifier == tmp.qualifier )
      return TRUE;

    tmp.objtype = t2.objtype;
    tmp.object = t2.object;		/* Union copy.  Portable? */
    tmp.type_or_lang = t2.type_or_lang;
    tmp.qualifier = t2.qualifier;
  } else if ( PL_is_functor(action, FUNCTOR_source1) )
  { triple t2;

    if ( !get_source(a, &t2) )
      return FALSE;
    if ( t2.source == t->source && t2.line == t->line )
      return TRUE;
    if ( db->tr_first )
    { record_update_src_transaction(db, t, t2.source, t2.line);
    } else
    { if ( t->source )
	unregister_source(db, t);
      t->source = t2.source;
      t->line = t2.line;
      if ( t->source )
	register_source(db, t);
    }

    return TRUE;			/* considered no change */
  } else
    return domain_error(action, "rdf_action");

  for(i=0; i<INDEX_TABLES; i++)
    tmp.next[i] = NULL;

  new = PL_malloc(sizeof(*new));
  memset(new, 0, sizeof(*new));
  new->subject	    = tmp.subject;
  new->predicate    = tmp.predicate;
  new->object	    = tmp.object;
  new->objtype	    = tmp.objtype;
  new->type_or_lang = tmp.type_or_lang;
  new->qualifier    = tmp.qualifier;
  new->source	    = tmp.source;
  new->line	    = tmp.line;
  lock_atoms(new);

  if ( db->tr_first )
  { record_update_transaction(db, t, new);
  } else
  { erase_triple(db, t);
    link_triple(db, new);
    db->generation++;
  }

  return TRUE;
}



static foreign_t
rdf_update5(term_t subject, term_t predicate, term_t object, term_t src,
	    term_t action)
{ triple t, *p;
  int indexed = BY_SP;
  int done = 0;
  rdf_db *db = DB;
    
  memset(&t, 0, sizeof(t));
  if ( !get_triple(db, subject, predicate, object, &t) ||
       !get_src(src, &t) )
    return FALSE;
  
  if ( !WRLOCK(db, FALSE) )
    return FALSE;
  if ( !update_hash(db) )
  { WRUNLOCK(db);
    return FALSE;
  }
  p = db->table[indexed][triple_hash(db, &t, indexed)];
  for( ; p; p = p->next[indexed])
  { if ( match_triples(p, &t, MATCH_EXACT) )
    { if ( !update_triple(db, action, p) )
	return FALSE;			/* type errors */
      done++;
    }
  }
  WRUNLOCK(db);

  return done ? TRUE : FALSE;
}


static foreign_t
rdf_update(term_t subject, term_t predicate, term_t object, term_t action)
{ return rdf_update5(subject, predicate, object, 0, action);
}


static foreign_t
rdf_retractall4(term_t subject, term_t predicate, term_t object, term_t src)
{ triple t, *p;
  rdf_db *db = DB;
      
  memset(&t, 0, sizeof(t));
  if ( !get_partial_triple(db, subject, predicate, object, src, &t) )
    return FALSE;

  if ( !WRLOCK(db, FALSE) )
    return FALSE;
  if ( !update_hash(db) )
  { WRUNLOCK(db);
    return FALSE;
  }
  p = db->table[t.indexed][triple_hash(db, &t, t.indexed)];
  for( ; p; p = p->next[t.indexed])
  { if ( match_triples(p, &t, MATCH_EXACT|MATCH_SRC) )
    { if ( db->tr_first )
      { record_transaction(db, TR_RETRACT, p);
      } else
      { erase_triple(db, p);
	db->generation++;
      }
    }
  }

  WRUNLOCK(db);

  return TRUE;
}


static foreign_t
rdf_retractall3(term_t subject, term_t predicate, term_t object)
{ return rdf_retractall4(subject, predicate, object, 0);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Enumerate the known subjects. This uses the   `first' flag on triples to
avoid returning the same resource multiple   times.  As the `by_none' is
never re-hashed, we don't mark this query in the `active_queries'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
rdf_subject(term_t subject, control_t h)
{ triple *t;
  rdf_db *db = DB;

  switch(PL_foreign_control(h))
  { case PL_FIRST_CALL:
    { if ( PL_is_variable(subject) )
      { t = db->table[BY_NONE][0];
	goto next;
      } else
      { atom_t a;

	if ( get_atom_ex(subject, &a) )
	{ if ( first(db, a) )
	    return TRUE;
	  return FALSE;
	}

	return FALSE;
      }
    }
    case PL_REDO:
      t = PL_foreign_context_address(h);
    next:
      for(; t; t = t->next[BY_NONE])
      { if ( t->first && !t->erased )
	{ PL_unify_atom(subject, t->subject);
	  
	  t = t->next[BY_NONE];
	  if ( t )
	    PL_retry_address(t);
	  return TRUE;
	}
      }
      return FALSE;
    case PL_CUTTED:
      return TRUE;
    default:
      assert(0);
      return FALSE;
  }
}


static foreign_t
rdf_set_predicate(term_t pred, term_t option)
{ predicate *p;
  rdf_db *db = DB;

  if ( !get_predicate(db, pred, &p) )
    return FALSE;

  if ( PL_is_functor(option, FUNCTOR_symmetric1) )
  { int val;

    if ( !get_bool_arg_ex(1, option, &val) )
      return FALSE;

    p->inverse_of = p;
    return TRUE;
  } else if ( PL_is_functor(option, FUNCTOR_inverse_of1) )
  { term_t a = PL_new_term_ref();
    predicate *i;

    PL_get_arg(1, option, a);
    if ( !get_predicate(db, a, &i) )
      return FALSE;

    p->inverse_of = i;
    i->inverse_of = p;
    return TRUE;
  } else if ( PL_is_functor(option, FUNCTOR_transitive1) )
  { int val;

    if ( !get_bool_arg_ex(1, option, &val) )
      return FALSE;

    p->transitive = val;

    return TRUE;
  } else
    return type_error(option, "predicate_option");
}


#define PRED_PROPERTY_COUNT 9
static functor_t predicate_key[PRED_PROPERTY_COUNT];

static int
unify_predicate_property(rdf_db *db, predicate *p, term_t option, functor_t f)
{ if ( f == FUNCTOR_symmetric1 )
    return PL_unify_term(option, PL_FUNCTOR, f,
			 PL_BOOL, p->inverse_of == p ? TRUE : FALSE);
  else if ( f == FUNCTOR_inverse_of1 )
  { if ( p->inverse_of )
      return PL_unify_term(option, PL_FUNCTOR, f,
			   PL_ATOM, p->inverse_of->name);
    else
      return FALSE;
  } else if ( f == FUNCTOR_transitive1 )
  { return PL_unify_term(option, PL_FUNCTOR, f,
			 PL_BOOL, p->transitive);
  } else if ( f == FUNCTOR_triples1 )
  { return PL_unify_term(option, PL_FUNCTOR, f,
			 PL_INTEGER, p->triple_count);
  } else if ( f == FUNCTOR_rdf_subject_branch_factor1 )
  { return PL_unify_term(option, PL_FUNCTOR, f,
		 PL_FLOAT, subject_branch_factor(db, p, DISTINCT_DIRECT));
  } else if ( f == FUNCTOR_rdf_object_branch_factor1 )
  { return PL_unify_term(option, PL_FUNCTOR, f,
		 PL_FLOAT, object_branch_factor(db, p, DISTINCT_DIRECT));
  } else if ( f == FUNCTOR_rdfs_subject_branch_factor1 )
  { return PL_unify_term(option, PL_FUNCTOR, f,
		 PL_FLOAT, subject_branch_factor(db, p, DISTINCT_SUB));
  } else if ( f == FUNCTOR_rdfs_object_branch_factor1 )
  { return PL_unify_term(option, PL_FUNCTOR, f,
		 PL_FLOAT, object_branch_factor(db, p, DISTINCT_SUB));
  } else
  { assert(0);
    return FALSE;
  }
}


static foreign_t
rdf_predicate_property(term_t pred, term_t option, control_t h)
{ int n;
  predicate *p;
  rdf_db *db = DB;

  if ( !predicate_key[0] )
  { int i = 0;

    predicate_key[i++] = FUNCTOR_symmetric1;
    predicate_key[i++] = FUNCTOR_inverse_of1;
    predicate_key[i++] = FUNCTOR_transitive1;
    predicate_key[i++] = FUNCTOR_triples1;
    predicate_key[i++] = FUNCTOR_rdf_subject_branch_factor1;
    predicate_key[i++] = FUNCTOR_rdf_object_branch_factor1;
    predicate_key[i++] = FUNCTOR_rdfs_subject_branch_factor1;
    predicate_key[i++] = FUNCTOR_rdfs_object_branch_factor1;
    assert(i < PRED_PROPERTY_COUNT);
  }

  switch(PL_foreign_control(h))
  { case PL_FIRST_CALL:
    { functor_t f;

      if ( PL_is_variable(option) )
      { n = 0;
	goto redo;
      } else if ( PL_get_functor(option, &f) )
      { for(n=0; predicate_key[n]; n++)
	{ if ( predicate_key[n] == f ) 
	  { if ( !get_predicate(db, pred, &p) )
	      return FALSE;
	    return unify_predicate_property(db, p, option, f);
	  }
	}
	return domain_error(option, "rdf_predicate_property");
      } else
	return type_error(option, "rdf_predicate_property");
    }
    case PL_REDO:
      n = PL_foreign_context(h);
    redo:
      if ( !get_predicate(db, pred, &p) )
	return FALSE;
      for( ; predicate_key[n]; n++ )
      { if ( unify_predicate_property(db, p, option, predicate_key[n]) )
	{ n++;
	  if ( predicate_key[n] )
	    PL_retry(n);
	  return TRUE;
	}
      }
      return FALSE;
    case PL_CUTTED:
      return TRUE;
    default:
      assert(0);
      return TRUE;
  }
}


		 /*******************************
		 *     TRANSITIVE RELATIONS	*
		 *******************************/

typedef struct visited
{ struct visited *next;			/* next in list */
  struct visited *hash_link;		/* next in hashed link */
  atom_t resource;			/* visited resource */
} visited;


typedef struct agenda
{ visited *head;			/* visited list */
  visited *tail;			/* tail of visited list */
  visited *to_expand;			/* next to expand */
  visited *to_return;			/* next to return */
  visited **hash;			/* hash-table for cycle detection */
  int	  hash_size;
  int     size;				/* size of the agenda */
  triple  pattern;			/* partial triple used as pattern */
  atom_t  target;			/* resource we are seaching for */
  struct chunk  *chunk;			/* node-allocation chunks */
} agenda;

#ifndef offsetof
#define offsetof(structure, field) ((int) &(((structure *)NULL)->field))
#endif
#define CHUNK_SIZE(n) offsetof(chunk, nodes[n])

typedef struct chunk
{ struct chunk *next;
  int	 used;				/* # used elements */
  int	 size;				/* size of the chunk */
  struct visited nodes[1];		/* nodes in the chunk */
} chunk;


static visited *
alloc_node_agenda(agenda *a)
{ chunk *c;
  int size;

  if ( (c=a->chunk) )
  { if ( c->used < c->size )
    { visited *v = &c->nodes[c->used++];

      return v;
    }
  }

  size = (a->size == 0 ? 8 : 1024);
  c = PL_malloc(CHUNK_SIZE(size));
  c->size = size;
  c->used = 1;
  c->next = a->chunk;
  a->chunk = c;

  return &c->nodes[0];
}


static void
empty_agenda(agenda *a)
{ chunk *c, *n;

  for(c=a->chunk; c; c = n)
  { n = c->next;
    PL_free(c);
  }
  if ( a->hash )
    PL_free(a->hash);
}


static agenda *
save_agenda(agenda *a)
{ agenda *r = PL_malloc(sizeof(*r));

  *r = *a;

  return r;
}


static void
hash_agenda(agenda *a, int size)
{ if ( a->hash )
    PL_free(a->hash);
  if ( size > 0 )
  { visited *v;

    a->hash = PL_malloc(sizeof(visited*)*size);
    memset(a->hash, 0, sizeof(visited*)*size);
    a->hash_size = size;
    
    for(v=a->head; v; v = v->next)
    { int key = atom_hash(v->resource)&(size-1);

      v->hash_link = a->hash[key];
      a->hash[key] = v;
    }
  }
}


static int
in_aganda(agenda *a, atom_t resource)
{ visited *v;

  if ( a->hash )
  { int key = atom_hash(resource)&(a->hash_size-1);
    v = a->hash[key];

    for( ; v; v = v->hash_link )
    { if ( v->resource == resource )
	return TRUE;
    }
  } else
  { v = a->head;

    for( ; v; v = v->next )
    { if ( v->resource == resource )
	return TRUE;
    }
  }

  return FALSE;
}


static visited *
append_agenda(rdf_db *db, agenda *a, atom_t res)
{ visited *v = a->head;

  if ( in_aganda(a, res) )
    return NULL;

  db->agenda_created++;			/* statistics */

  a->size++; 
  if ( !a->hash_size && a->size > 32 )
    hash_agenda(a, 64);
  else if ( a->size > a->hash_size * 4 )
    hash_agenda(a, a->hash_size * 4);

  v = alloc_node_agenda(a);
  v->resource = res;
  v->next = NULL;
  if ( a->tail )
  { a->tail->next = v;
    a->tail = v;
  } else
  { a->head = a->tail = v;
  }

  if ( a->hash_size )
  { int key = atom_hash(res)&(a->hash_size-1);
   
    v->hash_link = a->hash[key];
    a->hash[key] = v;
  }

  return v;
}


static int
can_reach_target(rdf_db *db, agenda *a)
{ int indexed = a->pattern.indexed;
  int rc = FALSE;
  triple *p;

  if ( indexed & BY_S )			/* subj ---> */
  { a->pattern.object.resource = a->target;
    indexed |= BY_O;
  } else
  { a->pattern.subject = a->target;
    indexed |= BY_S;
  }

  p = db->table[indexed][triple_hash(db, &a->pattern, indexed)];
  for( ; p; p = p->next[indexed])
  { if ( match_triples(p, &a->pattern, MATCH_SUBPROPERTY) )
    { rc = TRUE;
      break;
    }
  }

  if ( a->pattern.indexed & BY_S )	
  { a->pattern.object.resource = 0;
  } else
  { a->pattern.subject = 0;
  }

  return rc;
}



static visited *
bf_expand(rdf_db *db, agenda *a, atom_t resource)
{ triple *p;
  int indexed = a->pattern.indexed;
  visited *rc = NULL;

  if ( indexed & BY_S )			/* subj ---> */
  { a->pattern.subject = resource;
  } else
  { a->pattern.object.resource = resource;
  }

  if ( a->target && can_reach_target(db, a) )
  { return append_agenda(db, a, a->target);
  }

  p = db->table[indexed][triple_hash(db, &a->pattern, indexed)];
  for( ; p; p = p->next[indexed])
  { if ( match_triples(p, &a->pattern, MATCH_SUBPROPERTY) )
    { atom_t found = (indexed & BY_S) ? p->object.resource : p->subject;
      visited *v;

      v = append_agenda(db, a, found);
      if ( !rc )
	rc = v;
      if ( found == a->target )
	break;
    }
  }
					/* TBD: handle owl:inverseOf */
					/* TBD: handle owl:sameAs */
  return rc;
}


static int
next_agenda(rdf_db *db, agenda *a, atom_t *next)
{ if ( a->to_return )
  { ok:

    *next = a->to_return->resource;
    a->to_return = a->to_return->next;
  
    return TRUE;
  }

  while( a->to_expand )
  { a->to_return = bf_expand(db, a, a->to_expand->resource);
    a->to_expand = a->to_expand->next;

    if ( a->to_return )
      goto ok;
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rdf_reachable(+Subject, +Predicate, -Object)
rdf_reachable(-Subject, +Predicate, ?Object)
    Examine transitive relations, reporting all `Object' that can be
    reached from `Subject' using Predicate without going into a loop
    if the relation is cyclic.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
rdf_reachable(term_t subj, term_t pred, term_t obj, control_t h)
{ rdf_db *db = DB;

  switch(PL_foreign_control(h))
  { case PL_FIRST_CALL:
    { agenda a;
      atom_t r;
      term_t target_term;

      if ( PL_is_variable(pred) )
	return instantiation_error(pred);

      memset(&a, 0, sizeof(a));
      if ( !PL_is_variable(subj) )		/* subj .... obj */
      { if ( !get_partial_triple(db, subj, pred, 0, 0, &a.pattern) )
	  return FALSE;
	a.target = a.pattern.object.resource;
	target_term = obj;
      } else if ( !PL_is_variable(obj) ) 	/* obj .... subj */
      {	if ( !get_partial_triple(db, 0, pred, obj, 0, &a.pattern) )
	  return FALSE;
	a.target = a.pattern.subject;
	target_term = subj;
      } else
	return instantiation_error(subj);

      if ( !update_hash(db) )
	return FALSE;
      if ( (a.pattern.indexed & BY_S) ) 	/* subj ... */
	append_agenda(db, &a, a.pattern.subject);
      else
	append_agenda(db, &a, a.pattern.object.resource);
      a.to_return = a.head;
      a.to_expand = a.head;

      while(next_agenda(db, &a, &r))
      { if ( PL_unify_atom(target_term, r) )
	{ if ( a.target )		/* mode(+, +, +) */
	  { empty_agenda(&a);
	    return TRUE;
	  } else			/* mode(+, +, -) or mode(-, +, +) */
	  { db->active_queries++;
	    PL_retry_address(save_agenda(&a));
	  }
	}
      }
      empty_agenda(&a);
      return FALSE;
    }
    case PL_REDO:
    { agenda *a = PL_foreign_context_address(h);
      term_t target_term;
      atom_t r;

      if ( !PL_is_variable(subj) )
	target_term = obj;
      else
	target_term = subj;

      while(next_agenda(db, a, &r))
      { if ( PL_unify_atom(target_term, r) )
	{ if ( a->target )		/* +, +, + */
	  { empty_agenda(a);
	    return TRUE;
	  } else
	  { PL_retry_address(a);
	  }
	}
      }

      db->active_queries--;
      empty_agenda(a);
      PL_free(a);
      return FALSE;
    }
    case PL_CUTTED:
    { agenda *a = PL_foreign_context_address(h);

      db->active_queries--;
      empty_agenda(a);
      PL_free(a);
      return TRUE;
    }
    default:
      assert(0);
      return FALSE;
  }
}


		 /*******************************
		 *	     STATISTICS		*
		 *******************************/

static functor_t keys[10];

static int
unify_statistics(rdf_db *db, term_t key, functor_t f)
{ long v;

  if ( f == FUNCTOR_triples1 )
  { v = db->created - db->erased;
  } else if ( f == FUNCTOR_subjects1 )
  { v = db->subjects;
  } else if ( f == FUNCTOR_predicates1 )
  { v = db->pred_count;
  } else if ( f == FUNCTOR_indexed8 )
  { int i;
    term_t a = PL_new_term_ref();

    PL_unify_functor(key, FUNCTOR_indexed8);
    for(i=0; i<8; i++)
    { PL_get_arg(i+1, key, a);
      PL_unify_integer(a, db->indexed[i]);
    }

    return TRUE;
  } else if ( f == FUNCTOR_searched_nodes1 )
  { v = db->agenda_created;
  } else if ( f == FUNCTOR_duplicates1 )
  { v = db->duplicates;
  } else if ( f == FUNCTOR_triples2 && PL_is_functor(key, f) )
  { source *src;
    term_t a = PL_new_term_ref();
    atom_t name;

    PL_get_arg(1, key, a);
    if ( !PL_get_atom(a, &name) )
      return type_error(a, "atom");
    if ( (src = lookup_source(db, name, FALSE)) )
      v = src->triple_count;
    else
      v = 0;

    PL_get_arg(2, key, a);
    return PL_unify_integer(a, v);
  } else
    assert(0);

  return PL_unify_term(key, PL_FUNCTOR, f, PL_INTEGER, v);
}

static foreign_t
rdf_statistics(term_t key, control_t h)
{ int n;
  rdf_db *db = DB;

  switch(PL_foreign_control(h))
  { case PL_FIRST_CALL:
    { functor_t f;

      if ( PL_is_variable(key) )
      { n = 0;
	goto redo;
      } else if ( PL_get_functor(key, &f) )
      { for(n=0; keys[n]; n++)
	{ if ( keys[n] == f ) 
	    return unify_statistics(db, key, f);
	}
	return domain_error(key, "rdf_statistics");
      } else
	return type_error(key, "rdf_statistics");
    }
    case PL_REDO:
      n = PL_foreign_context(h);
    redo:
      unify_statistics(db, key, keys[n]);
      n++;
      if ( keys[n] )
	PL_retry(n);
    case PL_CUTTED:
      return TRUE;
    default:
      assert(0);
      return TRUE;
  }
}


static foreign_t
rdf_generation(term_t t)
{ rdf_db *db = DB;

  return PL_unify_integer(t, db->generation);
}


		 /*******************************
		 *	       RESET		*
		 *******************************/

static void
erase_triples(rdf_db *db)
{ triple *t, *n;
  int i;

  for(t=db->by_none; t; t=n)
  { n = t->next[BY_NONE];

    unlock_atoms(t);
    PL_free(t);
    db->freed++;
  }
  db->by_none = db->by_none_tail = NULL;

  for(i=BY_S; i<=BY_OP; i++)
  { if ( db->table[i] )
    { int bytes = sizeof(triple*) * db->table_size[i];
      
      memset(db->table[i], 0, bytes);
      memset(db->tail[i], 0, bytes);
    }
  }

  db->created = 0;
  db->erased = 0;
  db->subjects = 0;
  memset(db->indexed, 0, sizeof(db->indexed));
  db->duplicates = 0;
  db->generation = 0;
}


static void				/* TBD: get rid of virtual roots */
erase_predicates(rdf_db *db)
{ predicate **ht;
  int i;

  for(i=0,ht = db->pred_table; i<db->pred_table_size; i++, ht++)
  { predicate *p, *n;

    for( p = *ht; p; p = n )
    { n = p->next;

      free_list(&p->siblings);
      free_list(&p->subPropertyOf);

      PL_free(p);
    }

    *ht = NULL;
  }

  db->pred_count = 0;
}


static void
reset_db(rdf_db *db)
{ erase_triples(db);
  erase_predicates(db);
  erase_sources(db);
  db->need_update = FALSE;
  db->agenda_created = 0;
}


static foreign_t
rdf_reset_db()
{ rdf_db *db = DB;
  
  if ( !WRLOCK(db, FALSE) )
    return FALSE;

  if ( db->tr_first )
    record_transaction(db, TR_RESET, NULL);
  else
    reset_db(db);

  WRUNLOCK(db);

  return TRUE;
}



		 /*******************************
		 *	       MATCH		*
		 *******************************/

static const char *
nextword(const char *s)
{ while(*s && isalnum(*s))
    s++;
  while(*s && !isalnum(*s))
    s++;

  return s;
}


static int
match(int how, atom_t search, atom_t label)
{ const char *l = PL_atom_chars(label);
  const char *f = PL_atom_chars(search);

  switch(how)
  { case STR_MATCH_EXACT:
    { for( ; *l && *f; l++, f++ )
      { if ( tolower(*l) != tolower(*f) )
	  return FALSE;
      }
      if ( *l == '\0' && *f == '\0' )
	return TRUE;
  
      return FALSE;
    }
    case STR_MATCH_PREFIX:
    { for( ; *l && *f; l++, f++ )
      { if ( tolower(*l) != tolower(*f) )
	  return FALSE;
      }
      if ( *f == '\0' )
	return TRUE;
  
      return FALSE;
    }
    case STR_MATCH_SUBSTRING:		/* use Boyle-More! */
    { const char *h;
      const char *f0 = f;
  
      for(h=l; *h; h++)
      { for( l=h,f=f0; *l && *f; l++, f++ )
	{ if ( tolower(*l) != tolower(*f) )
	    break;
	}
	if ( *f == '\0' )
	  return TRUE;
	if ( *h == '\0' )
	  return FALSE;
      }
  
      return FALSE;
    }
    case STR_MATCH_WORD:
    { const char *h;
      const char *f0 = f;
  
      for(h=l; *h; h = nextword(h))
      { for( l=h,f=f0; *l && *f; l++, f++ )
	{ if ( tolower(*l) != tolower(*f) )
	    break;
	}
	if ( *f == '\0' )
	{ if ( *l == '\0' || !isalnum(*l) )
	    return TRUE;
	}
	if ( *l == '\0' )
	  return FALSE;
      }
  
      return FALSE;
    }
    case STR_MATCH_LIKE:		/* SeRQL like: * --> wildcart */
    { typedef struct chp { const char *pattern;
			   const char *label; } chp;
      chp chps[MAX_LIKE_CHOICES];
      int chn=0;

      for( ; *l && *f; l++, f++ )
      { if ( *f == '*' )
	{ f++;

	  if ( *f == '\0' )		/* foo* */
	    return TRUE;

	search_like:
	  while ( *l && tolower(*l) != tolower(*f) )
	    l++;

	  if ( *l )
	  { if ( chn >= MAX_LIKE_CHOICES )
	    { Sdprintf("rdf_db: too many * in `like' expression (>%d)",
		       MAX_LIKE_CHOICES);
	      return FALSE;
	    }
	    chps[chn].pattern = f;
	    chps[chn].label   = l+1;
	    chn++;

	    continue;
	  } else
	    goto retry_like;
	}

	if ( tolower(*l) != tolower(*f) )
	  goto retry_like;
      }
      if ( *l == '\0' && (*f == '\0' ||
			 (*f == '*' && f[1] == '\0')) )
	return TRUE;
  
retry_like:
      if ( chn > 0 )
      { chn--;
	f = chps[chn].pattern;
	l = chps[chn].label;
	goto search_like;
      }

      return FALSE;
    }
    default:
      assert(0);
      return FALSE;
  }
}


static foreign_t
match_label(term_t how, term_t search, term_t label)
{ atom_t h, f, l;
  int type;

  if ( !get_atom_ex(how, &h) ||
       !get_atom_ex(search, &f) ||
       !get_atom_ex(label, &l) )
    return FALSE;

  if ( h == ATOM_exact )
    type = STR_MATCH_EXACT;
  else if ( h == ATOM_substring )
    type = STR_MATCH_SUBSTRING;
  else if ( h == ATOM_word )
    type = STR_MATCH_WORD;
  else if ( h == ATOM_prefix )
    type = STR_MATCH_PREFIX;
  else if ( h == ATOM_like )
    type = STR_MATCH_LIKE;
  else
    return domain_error(how, "search_method");

  return match(type, f, l);
}


static foreign_t
split_url(term_t base, term_t local, term_t url)
{ char *b, *l, *u;
  unsigned int bl, ll;

  if ( PL_get_atom_nchars(base, &bl, &b) &&
       PL_get_atom_nchars(local, &ll, &l) )
  { if ( bl+ll < 1024 )
    { char buf[1024];

      memcpy(buf, b, bl);
      memcpy(buf+bl, l, ll);

      return PL_unify_atom_nchars(url, bl+ll, buf);
    } else
    { char *buf = PL_malloc(bl+ll);
      int rc;

      memcpy(buf, b, bl);
      memcpy(buf+bl, l, ll);

      rc = PL_unify_atom_nchars(url, bl+ll, buf);
      PL_free(buf);
      return rc;
    }
  } else if ( PL_get_atom_chars(url, &u) )
  { char *s, *last = NULL;

    for(s=u; *s; s++)
    { if ( *s == '#' || *s == '/' )
	last = s;
    }
    if ( last )
    { if ( PL_unify_atom_chars(local, last+1) &&
	   PL_unify_atom_nchars(base, last+1-u, u) )
	return TRUE;
      else
	return FALSE;
    } else
    { if ( PL_unify(local, url) &&
	   PL_unify_atom_chars(base, "") )
	return TRUE;
      else
	return FALSE;
    }
  } else
    return type_error(url, "atom");
}


		 /*******************************
		 *	       DEBUG		*
		 *******************************/

#ifdef O_DEBUG

static foreign_t
rdf_debug(term_t level)
{ long v;

  if ( !get_long_ex(level, &v) )
    return FALSE;

  debuglevel = v;
  return TRUE;
}

#endif


		 /*******************************
		 *	       VERSION		*
		 *******************************/

static foreign_t
rdf_version(term_t v)
{ return PL_unify_integer(v, RDF_VERSION);
}


		 /*******************************
		 *	     REGISTER		*
		 *******************************/

#define MKFUNCTOR(n, a) \
	FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)
#define NDET PL_FA_NONDETERMINISTIC
#define META PL_FA_TRANSPARENT

install_t
install_rdf_db()
{ int i=0;
  MKFUNCTOR(literal, 1);
  MKFUNCTOR(error, 2);
  MKFUNCTOR(type_error, 2);
  MKFUNCTOR(domain_error, 2);
  MKFUNCTOR(triples, 1);
  MKFUNCTOR(triples, 2);
  MKFUNCTOR(subjects, 1);
  MKFUNCTOR(predicates, 1);
  MKFUNCTOR(subject, 1);
  MKFUNCTOR(predicate, 1);
  MKFUNCTOR(object, 1);
  MKFUNCTOR(source, 1);
  MKFUNCTOR(indexed, 8);
  MKFUNCTOR(exact, 1);
  MKFUNCTOR(substring, 1);
  MKFUNCTOR(word, 1);
  MKFUNCTOR(prefix, 1);
  MKFUNCTOR(like, 1);
  MKFUNCTOR(literal, 2);
  MKFUNCTOR(searched_nodes, 1);
  MKFUNCTOR(duplicates, 1);
  MKFUNCTOR(symmetric, 1);
  MKFUNCTOR(transitive, 1);
  MKFUNCTOR(inverse_of, 1);
  MKFUNCTOR(lang, 2);
  MKFUNCTOR(type, 2);
  MKFUNCTOR(rdf_subject_branch_factor, 1);
  MKFUNCTOR(rdf_object_branch_factor, 1);
  MKFUNCTOR(rdfs_subject_branch_factor, 1);
  MKFUNCTOR(rdfs_object_branch_factor, 1);

  FUNCTOR_colon2 = PL_new_functor(PL_new_atom(":"), 2);

  ATOM_user	     = PL_new_atom("user");
  ATOM_exact	     = PL_new_atom("exact");
  ATOM_prefix	     = PL_new_atom("prefix");
  ATOM_like	     = PL_new_atom("like");
  ATOM_substring     = PL_new_atom("substring");
  ATOM_word	     = PL_new_atom("word");
  ATOM_subPropertyOf = PL_new_atom(URL_subPropertyOf);

  PRED_call1         = PL_predicate("call", 1, "user");

					/* statistics */
  keys[i++] = FUNCTOR_triples1;
  keys[i++] = FUNCTOR_subjects1;
  keys[i++] = FUNCTOR_indexed8;
  keys[i++] = FUNCTOR_predicates1;
  keys[i++] = FUNCTOR_searched_nodes1;
  keys[i++] = FUNCTOR_duplicates1;
  keys[i++] = FUNCTOR_triples2;
  keys[i++] = 0;

					/* setup the database */
  DB = new_db();

  PL_register_foreign("rdf_version",    1, rdf_version,     0);
  PL_register_foreign("rdf_assert",	3, rdf_assert3,	    0);
  PL_register_foreign("rdf_assert",	4, rdf_assert4,	    0);
  PL_register_foreign("rdf_update",	4, rdf_update,      0);
  PL_register_foreign("rdf_update",	5, rdf_update5,     0);
  PL_register_foreign("rdf_retractall",	3, rdf_retractall3, 0);
  PL_register_foreign("rdf_retractall",	4, rdf_retractall4, 0);
  PL_register_foreign("rdf_subject",	1, rdf_subject,	    NDET);
  PL_register_foreign("rdf",		3, rdf3,	    NDET);
  PL_register_foreign("rdf",		4, rdf4,	    NDET);
  PL_register_foreign("rdf_has",	4, rdf_has,	    NDET);
  PL_register_foreign("rdf_statistics_",1, rdf_statistics,  NDET);
  PL_register_foreign("rdf_generation", 1, rdf_generation,  0);
  PL_register_foreign("rdf_match_label",3, match_label,     0);
  PL_register_foreign("rdf_split_url",  3, split_url,       0);
  PL_register_foreign("rdf_save_db_",   2, rdf_save_db,     0);
  PL_register_foreign("rdf_load_db_",   1, rdf_load_db,     0);
  PL_register_foreign("rdf_reachable",  3, rdf_reachable,   NDET);
  PL_register_foreign("rdf_reset_db_",  0, rdf_reset_db,    0);
  PL_register_foreign("rdf_set_predicate",
					2, rdf_set_predicate, 0);
  PL_register_foreign("rdf_predicate_property",
					2, rdf_predicate_property, NDET);
  PL_register_foreign("rdf_sources_",   1, rdf_sources,     0);
  PL_register_foreign("rdf_estimate_complexity",
					4, rdf_estimate_complexity, 0);
  PL_register_foreign("rdf_transaction",1, rdf_transaction, META);
#ifdef O_DEBUG
  PL_register_foreign("rdf_debug",      1, rdf_debug,       0);
#endif
#ifdef WITH_MD5
  PL_register_foreign("rdf_md5",	2, rdf_md5,	    0);
  PL_register_foreign("rdf_atom_md5",	3, rdf_atom_md5,    0);
#endif
}
