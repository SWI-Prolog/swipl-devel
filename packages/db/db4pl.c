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

#include <SWI-Stream.h>
#include "db4pl.h"
#include <sys/types.h>
#include <limits.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "error.h"
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <signal.h>

					/* <  4.0: set_server */
					/* >= 4.0: set_rpc_server */
#ifndef HAVE_SET_RPC_SERVER
#define set_rpc_server set_server
#endif

#define DEBUG(g) (void)0

static atom_t ATOM_read;
static atom_t ATOM_update;
static atom_t ATOM_true;
static atom_t ATOM_false;
static atom_t ATOM_btree;
static atom_t ATOM_hash;
static atom_t ATOM_recno;
static atom_t ATOM_unknown;
static atom_t ATOM_duplicates;
static atom_t ATOM_mp_mmapsize;
static atom_t ATOM_mp_size;
static atom_t ATOM_home;
static atom_t ATOM_config;
static atom_t ATOM_locking;
static atom_t ATOM_logging;
static atom_t ATOM_transactions;
static atom_t ATOM_create;
static atom_t ATOM_database;
static atom_t ATOM_key;
static atom_t ATOM_value;
static atom_t ATOM_term;
static atom_t ATOM_atom;
static atom_t ATOM_c_string;
static atom_t ATOM_c_long;
static atom_t ATOM_server;
static atom_t ATOM_server_timeout;
static atom_t ATOM_client_timeout;

static functor_t FUNCTOR_db1;
static functor_t FUNCTOR_type1;

DB_ENV *db_env;				/* default environment */

#define mkfunctor(n, a) PL_new_functor(PL_new_atom(n), a)

#define NOSIG(code) \
	{ sigset_t new, old; \
	  sigemptyset(&new); \
	  sigaddset(&new, SIGINT); \
	  sigprocmask(SIG_BLOCK, &new, &old); \
	  code; \
	  sigprocmask(SIG_SETMASK, &old, NULL); \
	}

#define TheTXN current_transaction()

static void
initConstants()
{
  ATOM_read	      =	PL_new_atom("read");
  ATOM_update	      =	PL_new_atom("update");
  ATOM_true	      =	PL_new_atom("true");
  ATOM_false	      =	PL_new_atom("false");
  ATOM_btree	      =	PL_new_atom("btree");
  ATOM_hash	      =	PL_new_atom("hash");
  ATOM_recno	      =	PL_new_atom("recno");
  ATOM_unknown	      =	PL_new_atom("unknown");
  ATOM_duplicates     =	PL_new_atom("duplicates");
  ATOM_mp_size	      =	PL_new_atom("mp_size");
  ATOM_mp_mmapsize    =	PL_new_atom("mp_mmapsize");
  ATOM_home	      =	PL_new_atom("home");
  ATOM_config	      =	PL_new_atom("config");
  ATOM_locking	      =	PL_new_atom("locking");
  ATOM_logging	      =	PL_new_atom("logging");
  ATOM_transactions   =	PL_new_atom("transactions");
  ATOM_create	      =	PL_new_atom("create");
  ATOM_database	      =	PL_new_atom("database");
  ATOM_key	      =	PL_new_atom("key");
  ATOM_value	      =	PL_new_atom("value");
  ATOM_term	      =	PL_new_atom("term");
  ATOM_atom	      =	PL_new_atom("atom");
  ATOM_c_string	      =	PL_new_atom("c_string");
  ATOM_c_long	      =	PL_new_atom("c_long");
  ATOM_server	      =	PL_new_atom("server");
  ATOM_server_timeout =	PL_new_atom("server_timeout");
  ATOM_client_timeout =	PL_new_atom("client_timeout");

  FUNCTOR_db1	      =	mkfunctor("$db", 1);
  FUNCTOR_type1	      =	mkfunctor("type", 1);
}

static void	cleanup();

typedef struct _db_list
{ dbh	*db;
  struct _db_list *next;
} db_list;

static db_list *dbs;			/* open DB's */

static void
register_db(dbh *db)
{ db_list *l = calloc(1, sizeof(*l));
  
  l->db = db;
  l->next = dbs;
  dbs = l;
}


static void
unregister_db(dbh *db)
{ db_list **p = &dbs;

  for(; *p; p = &(*p)->next)
  { if ( (*p)->db == db )
    { db_list *l = *p;

      *p = l->next;
      free(l);
      return;
    }
  }
}


static int
unify_db(term_t t, dbh *db)
{ return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_db1,
		         PL_POINTER, db);
}


static int
get_db(term_t t, dbh **db)
{ if ( PL_is_functor(t, FUNCTOR_db1) )
  { term_t a = PL_new_term_ref();
    void *ptr;

    _PL_get_arg(1, t, a);
    if ( PL_get_pointer(a, &ptr) )
    { if ( ((dbh *)ptr)->magic != DBH_MAGIC )
	return pl_error(ERR_EXISTENCE, "db", t);

      *db = ptr;
      return TRUE;
    }
  }

  return pl_error(ERR_TYPE, "db", t);
}


static int
unify_dbt(term_t t, dtype type, DBT *dbt)
{ switch( type )
  { case D_TERM:
    { term_t r = PL_new_term_ref();

      PL_recorded_external(dbt->data, r);
      return PL_unify(t, r);
    }
    case D_ATOM:
      return PL_unify_atom_nchars(t, dbt->size, dbt->data);
    case D_CSTRING:
      return PL_unify_atom_chars(t, dbt->data);
    case D_CLONG:
    { long *v = dbt->data;
      return PL_unify_integer(t, *v);
    }
  }
  assert(0);
  return FALSE;
}


static int
get_dbt(term_t t, dtype type, DBT *dbt)
{ memset(dbt, 0, sizeof(*dbt));

  switch(type)
  { case D_TERM:
    { unsigned int len;
  
      dbt->data = PL_record_external(t, &len);
      dbt->size = len;
      return TRUE;
    }
    case D_ATOM:
    { unsigned int len;
      char *s;

      if ( PL_get_atom_nchars(t, &len, &s) )
      { dbt->data = s;
	dbt->size = len;

	return TRUE;
      }
      return pl_error(ERR_TYPE, "atom", t);
    }
    case D_CSTRING:
    { unsigned int len;
      char *s;

      if ( PL_get_atom_nchars(t, &len, &s) )
      { dbt->data = s;
	dbt->size = len+1;		/* account for terminator */

	return TRUE;
      }
      return pl_error(ERR_TYPE, "atom", t);
    }
    case D_CLONG:
    { long v;

      if ( PL_get_long(t, &v) )
      {	long *d = malloc(sizeof(long));

	*d = v;
	dbt->data = d;
	dbt->size = sizeof(long);

	return TRUE;
      }
      return pl_error(ERR_TYPE, "integer", t);
    }
  }
  assert(0);
  return FALSE;
}


static void
free_dbt(DBT *dbt, dtype type)
{ switch ( type )
  { case D_TERM:
      PL_erase_external(dbt->data);
      return;
    case D_ATOM:
    case D_CSTRING:
      return;
    case D_CLONG:
      free(dbt->data);
  }
}


static int
get_bool_ex(term_t t, int *v)
{ atom_t a;

  if ( PL_get_atom(t, &a) )
  { if ( a == ATOM_true )
    { *v = TRUE;
      return TRUE;
    }
    if ( a == ATOM_false )
    { *v = FALSE;
      return TRUE;
    }
  }

  return pl_error(ERR_TYPE, "bool", t);
}


static int
get_long_ex(term_t t, long *v)
{ if ( PL_get_long(t, v) )
    return TRUE;

  return pl_error(ERR_TYPE, "integer", t);
}


static int
get_size_ex(term_t t, long *v)
{ if ( get_long_ex(t, v) )
  { if ( v >= 0 )
      return TRUE;

    return pl_error(ERR_DOMAIN, "not_less_than_zero", t);
  }

  return FALSE;
}


static int
get_chars_ex(term_t t, char **s)
{ if ( PL_get_atom_chars(t, s) )
    return TRUE;

  return pl_error(ERR_TYPE, "atom", t);
}


int
db_status(int rval)
{ switch( rval )
  { case 0:
      return TRUE;
    case DB_LOCK_DEADLOCK:
      Sdprintf("Throwing deadlock exception\n");
      return pl_error(ERR_PACKAGE_ID, "db", "deadlock", db_strerror(rval));
    case DB_RUNRECOVERY:
      Sdprintf("Need recovery\n");
      return pl_error(ERR_PACKAGE_ID, "db", "run_recovery", db_strerror(rval));
  }

  if ( rval < 0 )
  { DEBUG(Sdprintf("DB error: %s\n", db_strerror(rval)));
    return FALSE;			/* normal failure */
  }

  Sdprintf("Throwing error: %s\n", db_strerror(rval));
  return pl_error(ERR_PACKAGE_INT, "db", rval, db_strerror(rval));
}


static int
db_type(term_t t, int *type)
{ term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();

  while( PL_get_list(tail, head, tail) )
  { if ( PL_is_functor(head, FUNCTOR_type1) )
    { term_t a0 = PL_new_term_ref();
      atom_t tp;

      _PL_get_arg(1, head, a0);

      if ( !PL_get_atom(a0, &tp) )
	return pl_error(ERR_TYPE, "atom", a0);
      if ( tp == ATOM_btree )
	*type = DB_BTREE;
      else if ( tp == ATOM_hash )
	*type = DB_HASH;
      else if ( tp == ATOM_recno )
	*type = DB_RECNO;
      else if ( tp == ATOM_unknown )
	*type = DB_UNKNOWN;
      else
	return pl_error(ERR_DOMAIN, "db_type", a0);

      return TRUE;
    }
  }

  return TRUE;
}


static int
get_dtype(term_t t, dtype *type)
{ atom_t a;

  if ( !PL_get_atom(t, &a) )
    return pl_error(ERR_TYPE, "atom", t);
  if ( a == ATOM_term )
    *type = D_TERM;
  else if ( a == ATOM_atom )
    *type = D_ATOM;
  else if ( a == ATOM_c_string )
    *type = D_CSTRING;
  else if ( a == ATOM_c_long )
    *type = D_CLONG;
  else
    return pl_error(ERR_DOMAIN, "type", t);

  return TRUE;
}


static int
db_options(term_t t, dbh *dbh, char **subdb)
{ term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();
  int flags = 0;

  dbh->key_type   = D_TERM;
  dbh->value_type = D_TERM;

  while( PL_get_list(tail, head, tail) )
  { atom_t name;
    int arity;

    if ( PL_get_name_arity(head, &name, &arity) )
    { if ( arity == 1 )
      { term_t a0 = PL_new_term_ref();

	_PL_get_arg(1, head, a0);
	if ( name == ATOM_duplicates )
	{ int v;

	  if ( !get_bool_ex(a0, &v) )
	    return FALSE;

	  if ( v )
	  { flags |= DB_DUP;
	    dbh->duplicates = TRUE;
	  }
	} else if ( name == ATOM_database )
	{ if ( !get_chars_ex(a0, subdb) )
	    return FALSE;
	} else if ( name == ATOM_key )
	{ if ( !get_dtype(a0, &dbh->key_type) )
	    return FALSE;
	} else if ( name == ATOM_value )
	{ if ( !get_dtype(a0, &dbh->value_type) )
	    return FALSE;
	} else
	  return pl_error(ERR_DOMAIN, "db_option", head);
      } else
	return pl_error(ERR_DOMAIN, "db_option", head);
    }
  }

  if ( !PL_get_nil(tail) )
    return pl_error(ERR_TYPE, "list", t);

  if ( flags )
  { int rval;

    if ( (rval=dbh->db->set_flags(dbh->db, flags)) )
      return db_status(rval);
  }


  return TRUE;
}


static foreign_t
pl_db_open(term_t file, term_t mode, term_t handle, term_t options)
{ char *fname;
  int flags;
  int m = 0666;
  int type = DB_BTREE;
  dbh *dbh;
  atom_t a;
  int rval;
  char *subdb = NULL;

  if ( !PL_get_atom_chars(file, &fname) )
    return pl_error(ERR_TYPE, "atom", file);

  if ( !PL_get_atom(mode, &a) )		/* process mode */
    return pl_error(ERR_TYPE, "atom", mode);
  if ( a == ATOM_read )
    flags = DB_RDONLY;
  else if ( a == ATOM_update )
    flags = DB_CREATE;
  else
    return pl_error(ERR_DOMAIN, "io_mode", mode);
  
  dbh = calloc(1, sizeof(*dbh));
  dbh->magic = DBH_MAGIC;
  NOSIG(rval=db_create(&dbh->db, db_env, 0));
  if ( rval )
    return db_status(rval);

  DEBUG(Sdprintf("New DB at %p\n", dbh->db));

  if ( !db_type(options, &type) ||
       !db_options(options, dbh, &subdb) )
    return FALSE;
  
  NOSIG(rval=dbh->db->open(dbh->db, fname, subdb, type, flags, m));
  if ( rval )
    return db_status(rval);

  register_db(dbh);
  return unify_db(handle, dbh);
}


static foreign_t
pl_db_close(term_t handle)
{ dbh *db;

  if ( get_db(handle, &db) )
  { int rval;

    DEBUG(Sdprintf("Close DB at %p\n", db->db));
    NOSIG(rval = db->db->close(db->db, 0);
	  unregister_db(db);
	  db->magic = 0;
	  free(db));

    return db_status(rval);
  }

  return FALSE;
}


static foreign_t
pl_db_closeall()
{ db_list *l, *n;

  for(l=dbs; l; l=n)
  { int rval;

    n = l->next;

    NOSIG(rval = l->db->db->close(l->db->db, 0);
	  l->db->magic = 0;
	  unregister_db(l->db));
    if ( rval )
      return db_status(rval);
  }

  assert(dbs == NULL);

  cleanup();
  return TRUE;
}


		 /*******************************
		 *	   TRANSACTIONS		*
		 *******************************/

typedef struct _transaction
{ DB_TXN *tid;				/* transaction id */
  struct _transaction *parent;		/* parent id */
} transaction;

static transaction *transaction_stack;

static int
begin_transaction()
{ if ( db_env )
  { int rval;
    DB_TXN *pid, *tid;
    transaction *t;
  
    if ( transaction_stack )
      pid = transaction_stack->tid;
    else
      pid = NULL;
  
    if ( (rval=txn_begin(db_env, pid, &tid, 0)) )
      return db_status(rval);
  
    t = malloc(sizeof(*t));
    t->parent = transaction_stack;
    t->tid = tid;
    transaction_stack = t;
  
    return TRUE;
  }

  return pl_error(ERR_PACKAGE_INT, "db", 0,
		  "Not initialized for transactions");
}


static int
commit_transaction()
{ transaction *t;

  if ( (t=transaction_stack) )
  { DB_TXN *tid = t->tid;
    int rval;
    
    transaction_stack = t->parent;
    free(t);

    if ( (rval=txn_commit(tid, 0)) )
      return db_status(rval);

    return TRUE;
  }

  return pl_error(ERR_PACKAGE_INT, "db", 0, "No transactions");
}


static int
abort_transaction()
{ transaction *t;

  if ( (t=transaction_stack) )
  { DB_TXN *tid = t->tid;
    int rval;
    
    transaction_stack = t->parent;
    free(t);

    if ( (rval=txn_abort(tid)) )
      return db_status(rval);

    return TRUE;
  }

  return pl_error(ERR_PACKAGE_INT, "db", 0, "No transactions");
}


static DB_TXN *
current_transaction()
{ if ( transaction_stack )
    return transaction_stack->tid;

  return NULL;
}


static foreign_t
pl_db_transaction(term_t goal)
{ static predicate_t call1;
  qid_t qid;
  int rval;

  if ( !call1 )
    call1 = PL_predicate("call", 1, "user");
  
  NOSIG(rval=begin_transaction());
  if ( !rval )
    return FALSE;

  qid = PL_open_query(NULL, PL_Q_CATCH_EXCEPTION, call1, goal);
  rval = PL_next_solution(qid);
  if ( rval )
  { PL_cut_query(qid);
    NOSIG(rval=commit_transaction());
    return rval;
  } else
  { term_t ex = PL_exception(qid);

    PL_cut_query(qid);

    NOSIG(rval=abort_transaction());
    if ( !rval )
      return FALSE;

    if ( ex )
      return PL_raise_exception(ex);

    return FALSE;
  }
}


		 /*******************************
		 *	     DB ACCESS		*
		 *******************************/

static foreign_t
pl_db_put(term_t handle, term_t key, term_t value)
{ DBT k, v;
  dbh *db;
  int flags = 0;
  int rval;

  if ( !get_db(handle, &db) )
    return FALSE;

  if ( !get_dbt(key, db->key_type, &k) ||
       !get_dbt(value, db->value_type, &v) )
    return FALSE;

  NOSIG(rval = db_status(db->db->put(db->db, TheTXN, &k, &v, flags)));
  free_dbt(&k, db->key_type);
  free_dbt(&v, db->value_type);

  return rval;
}


static foreign_t
pl_db_del2(term_t handle, term_t key)
{ DBT k;
  dbh *db;
  int flags = 0;			/* current no flags in DB */
  int rval;

  if ( !get_db(handle, &db) )
    return FALSE;

  if ( !get_dbt(key, db->key_type, &k) )
    return FALSE;

  NOSIG(rval = db_status(db->db->del(db->db, TheTXN, &k, flags)));
  free_dbt(&k, db->key_type);

  return rval;
}


static int
equal_dbt(DBT *a, DBT *b)
{ if ( a->size == b->size )
  { if ( a->data == b->data )
      return TRUE;
    if ( memcmp(a->data, b->data, a->size) == 0 )
      return TRUE;
  }

  return FALSE;
}


static foreign_t
pl_db_getall(term_t handle, term_t key, term_t value)
{ DBT k, v;
  dbh *db;
  int rval;

  if ( !get_db(handle, &db) )
    return FALSE;

  if ( !get_dbt(key, db->key_type, &k) )
    return FALSE;
  memset(&v, 0, sizeof(v));

  if ( db->duplicates )			/* must use a cursor */
  { DBC *cursor;
    term_t tail = PL_copy_term_ref(value);
    term_t head = PL_new_term_ref();

    NOSIG(rval=db->db->cursor(db->db, TheTXN, &cursor, 0));
    if ( rval )
      return db_status(rval);

    NOSIG(rval=cursor->c_get(cursor, &k, &v, DB_SET));
    if ( rval == 0 )
    { DBT k2;

      if ( !PL_unify_list(tail, head, tail) ||
	   !unify_dbt(head, db->value_type, &v) )
      { cursor->c_close(cursor);
	return FALSE;
      }

      memset(&k2, 0, sizeof(k2));
      for(;;)
      { NOSIG(rval=cursor->c_get(cursor, &k2, &v, DB_NEXT));

	if ( rval == 0 && equal_dbt(&k, &k2) )
	{ if ( PL_unify_list(tail, head, tail) &&
	       unify_dbt(head, db->value_type, &v) )
	    continue;
	}

	NOSIG(cursor->c_close(cursor);
	      free_dbt(&k, db->key_type));
	
	if ( rval <= 0 )		/* normal failure */
	{ return PL_unify_nil(tail);
	} else				/* error failure */
	{ return db_status(rval);
	}
      }
    } else if ( rval == DB_NOTFOUND )
    { free_dbt(&k, db->key_type);
      return FALSE;
    } else
    { free_dbt(&k, db->key_type);
      return db_status(rval);
    }
  } else
  { NOSIG(rval=db->db->get(db->db, TheTXN, &k, &v, 0));

    if ( !rval )
    { term_t t = PL_new_term_ref();
      term_t tail = PL_copy_term_ref(value);
      term_t head = PL_new_term_ref();

      free_dbt(&k, db->key_type);
      PL_recorded_external(v.data, t);
      if ( PL_unify_list(tail, head, tail) &&
	   PL_unify(head, t) &&
	   PL_unify_nil(tail) )
	return TRUE;

      return FALSE;
    } else
      return db_status(rval);
  }
}


typedef struct _dbget_ctx
{ dbh *db;				/* the database */
  DBC *cursor;				/* the cursor */
  DBT key;				/* the key */
  DBT k2;				/* secondary key */
  DBT value;				/* the value */
} dbget_ctx;


static foreign_t
pl_db_enum(term_t handle, term_t key, term_t value, control_t ctx)
{ DBT k, v;
  dbh *db;
  int rval = 0;
  dbget_ctx *c = NULL;
  fid_t fid = 0;

  memset(&k, 0, sizeof(k));
  memset(&v, 0, sizeof(v));

  switch( PL_foreign_control(ctx) )
  { case PL_FIRST_CALL:
      if ( !get_db(handle, &db) )
	return FALSE;
      c = calloc(1, sizeof(*c));

      c->db = db;
      if ( (rval=db->db->cursor(db->db, TheTXN, &c->cursor, 0)) )
      { free(c);
	return db_status(rval);
      }
      DEBUG(Sdprintf("Created cursor at %p\n", c->cursor));

      rval = c->cursor->c_get(c->cursor, &c->key, &c->value, DB_FIRST);
      if ( rval == 0 )
      { fid = PL_open_foreign_frame();

	if ( unify_dbt(key, db->key_type, &c->key) &&
	     unify_dbt(value, db->value_type, &c->value) )
	{ PL_close_foreign_frame(fid);
	  PL_retry_address(c);
	}

	PL_rewind_foreign_frame(fid);
	goto retry;
      }
      goto out;
    case PL_REDO:
      c = PL_foreign_context_address(ctx);
      db = c->db;

    retry:
      for(;;)
      { rval = c->cursor->c_get(c->cursor, &c->k2, &c->value, DB_NEXT);

	if ( rval == 0 )
	{ if ( !fid )
	    fid = PL_open_foreign_frame();
	  if ( unify_dbt(key, db->key_type, &c->k2) &&
	       unify_dbt(value, db->value_type, &c->value) )
	  { PL_close_foreign_frame(fid);
	    PL_retry_address(c);
	  }
	  PL_rewind_foreign_frame(fid);
	  continue;
	}
	break;
      }
      break;
    case PL_CUTTED:
      c = PL_foreign_context_address(ctx);
      db = c->db;
      break;
  }

out:
  if ( c )
  { if ( rval == 0 )
      rval = c->cursor->c_close(c->cursor);
    else
      c->cursor->c_close(c->cursor);
    free(c);
  }
  if ( fid )
    PL_close_foreign_frame(fid);

  db_status(rval);
  return FALSE;				/* also on rval = 0! */
}


#define DO_DEL \
	if ( del ) \
	{ do \
	  { if ( (rval=c->cursor->c_del(c->cursor, 0)) != 0 ) \
	      return db_status(rval); \
	  } while(0); \
	}


static foreign_t
pl_db_getdel(term_t handle, term_t key, term_t value, control_t ctx, int del)
{ dbh *db;
  int rval = 0;
  dbget_ctx *c = NULL;
  fid_t fid = 0;

  switch( PL_foreign_control(ctx) )
  { case PL_FIRST_CALL:
      if ( !get_db(handle, &db) )
	return FALSE;

      if ( db->duplicates )		/* DB with duplicates */
      { c = calloc(1, sizeof(*c));

	c->db = db;
	if ( (rval=db->db->cursor(db->db, TheTXN, &c->cursor, 0)) )
	{ free(c);
	  return db_status(rval);
	}
	DEBUG(Sdprintf("Created cursor at %p\n", c->cursor));
	if ( !get_dbt(key, db->key_type, &c->key) )
	  return FALSE;

	rval = c->cursor->c_get(c->cursor, &c->key, &c->value, DB_SET);
	if ( rval == 0 )
	{ fid = PL_open_foreign_frame();

	  if ( unify_dbt(value, db->value_type, &c->value) )
	  { DO_DEL;

	    PL_close_foreign_frame(fid);
	    PL_retry_address(c);
	  }

	  PL_rewind_foreign_frame(fid);
	  goto retry;
	}
	goto out;
      } else				/* Unique DB */
      { DBT k, v;

	if ( !get_dbt(key, db->key_type, &k) )
	  return FALSE;
	memset(&v, 0, sizeof(v));

	if ( (rval=db->db->get(db->db, TheTXN, &k, &v, 0)) == 0 )
	  return unify_dbt(value, db->value_type, &v);

	free_dbt(&k, db->key_type);
	return db_status(rval);
      }
    case PL_REDO:
      c = PL_foreign_context_address(ctx);
      db = c->db;

    retry:
      for(;;)
      { rval = c->cursor->c_get(c->cursor, &c->k2, &c->value, DB_NEXT);

	if ( rval == 0 && equal_dbt(&c->key, &c->k2) )
	{ if ( !fid )
	    fid = PL_open_foreign_frame();
	  if ( unify_dbt(value, db->value_type, &c->value) )
	  { DO_DEL;
	    PL_close_foreign_frame(fid);
	    PL_retry_address(c);
	  }
	  PL_rewind_foreign_frame(fid);
	  continue;
	}
	break;
      }
      break;
    case PL_CUTTED:
      c = PL_foreign_context_address(ctx);
      db = c->db;
      break;
  }

out:
  if ( c )
  { if ( rval == 0 )
      rval = c->cursor->c_close(c->cursor);
    else
      c->cursor->c_close(c->cursor);
    DEBUG(Sdprintf("Destroyed cursor at %p\n", c->cursor));
    free_dbt(&c->key, db->key_type);
    free(c);
  }
  if ( fid )
    PL_close_foreign_frame(fid);

  db_status(rval);
  return FALSE;				/* also on rval = 0! */
}


static foreign_t
pl_db_get(term_t handle, term_t key, term_t value, control_t ctx)
{ int rval;

  NOSIG(rval = pl_db_getdel(handle, key, value, ctx, FALSE));

  return rval;
}


static foreign_t
pl_db_del3(term_t handle, term_t key, term_t value, control_t ctx)
{ int rval;

  NOSIG(rval=pl_db_getdel(handle, key, value, ctx, TRUE));

  return rval;
}


static void
cleanup()
{ if ( db_env )
  { int rval;

    if ( (rval=db_env->close(db_env, 0)) )
      Sdprintf("DB: close failed: %s\n", db_strerror(rval));
    db_env = NULL;
  }
}


		 /*******************************
		 *	     APPINIT		*
		 *******************************/

typedef struct _server_info
{ char *host;
  long cl_timeout;
  long sv_timeout;
  u_int32_t flags;
} server_info;


static void
pl_db_error(const char *prefix, char *buffer)
{ Sdprintf("%s%s\n", prefix, buffer);
}


static int
get_server(term_t options, server_info *info)
{ term_t l = PL_copy_term_ref(options);
  term_t h = PL_new_term_ref();
  
  while( PL_get_list(l, h, l) )
  { atom_t name;
    int arity;

    if ( PL_get_name_arity(h, &name, &arity) && name == ATOM_server )
    { info->cl_timeout = 0;
      info->sv_timeout = 0;
      info->flags      = 0;

      if ( arity >= 1 )			/* server(host) */
      { term_t a = PL_new_term_ref();

	PL_get_arg(1, h, a);
	if ( !PL_get_atom_chars(a, &info->host) )
	  return pl_error(ERR_TYPE, "atom", a);
      }
      if ( arity == 2 )			/* server(host, options) */
      { term_t a = PL_new_term_ref();

	PL_get_arg(2, h, l);
	while( PL_get_list(l, h, l) )
	{ atom_t name;
	  int arity;

	  if ( PL_get_name_arity(h, &name, &arity) && arity == 1 )
	  { PL_get_arg(1, h, a);
	    
	    if ( name == ATOM_server_timeout )
	    { if ( !get_long_ex(a, &info->sv_timeout) )
		return FALSE;
	    } else if ( name == ATOM_client_timeout )
	    { if ( !get_long_ex(a, &info->cl_timeout) )
		return FALSE;
	    } else
	      return pl_error(ERR_DOMAIN, "server_option", a);
	  } else
	    return pl_error(ERR_DOMAIN, "server_option", a);
	}
	if ( !PL_get_nil(l) )
	  return pl_error(ERR_TYPE, "list", a);
      }

      return TRUE;
    }
  }

  return FALSE;
}


#define MAXCONFIG 20

static foreign_t
pl_db_init(term_t option_list)
{ int rval;
  term_t options = PL_copy_term_ref(option_list);
  u_int32_t flags = 0;
  term_t head = PL_new_term_ref();
  term_t a    = PL_new_term_ref();
  server_info si;
  char *home = NULL;
  char *config[MAXCONFIG];
  int nconf = 0;

  if ( db_env )
    return pl_error(ERR_PACKAGE_INT, "db", 0, "Already initialized");

  config[0] = NULL;

  if ( get_server(option_list, &si) )
  { if ( (rval=db_env_create(&db_env, DB_CLIENT)) )
      return db_status(rval);
    rval = db_env->set_rpc_server(db_env, si.host,
				  si.cl_timeout, si.sv_timeout, si.flags);
    if ( rval )
      return db_status(rval);
  } else
  { if ( (rval=db_env_create(&db_env, 0)) )
      return db_status(rval);
  }

  db_env->set_errpfx(db_env, "db4pl: ");
  db_env->set_errcall(db_env, pl_db_error);

  flags |= DB_INIT_MPOOL;		/* always needed? */

  while(PL_get_list(options, head, options))
  { atom_t name;
    int arity;

    if ( !PL_get_name_arity(head, &name, &arity) )
      return pl_error(ERR_TYPE, "option", head);
    if ( arity == 1 )
    { long v;

      PL_get_arg(1, head, a);
      
      if ( name == ATOM_mp_mmapsize )	/* mp_mmapsize */
      { if ( !get_size_ex(a, &v) )
	  return FALSE;
	db_env->set_mp_mmapsize(db_env, v);
	flags |= DB_INIT_MPOOL;
      } else if ( name == ATOM_mp_size ) /* mp_size */
      { if ( !get_size_ex(a, &v) )
	  return FALSE;
	db_env->set_cachesize(db_env, 0, v, 0);
	flags |= DB_INIT_MPOOL;
      } else if ( name == ATOM_home )	/* db_home */
      {	if ( !get_chars_ex(a, &home) )
	  return FALSE;
      } else if ( name == ATOM_locking ) /* locking */
      {	int v;

	if ( !get_bool_ex(a, &v) )
	  return FALSE;
	if ( v )
	  flags |= DB_INIT_LOCK;
      } else if ( name == ATOM_logging ) /* logging */
      {	int v;

	if ( !get_bool_ex(a, &v) )
	  return FALSE;
	if ( v )
	  flags |= DB_INIT_LOG;
      } else if ( name == ATOM_transactions )	/* transactions */
      {	int v;

	if ( !get_bool_ex(a, &v) )
	  return FALSE;
	if ( v )
	  flags |= (DB_INIT_TXN|DB_INIT_MPOOL|DB_INIT_LOCK|DB_INIT_LOG);
      } else if ( name == ATOM_create )	/* Create files */
      {	int v;

	if ( !get_bool_ex(a, &v) )
	  return FALSE;
	if ( v )
	  flags |= DB_CREATE;
      } else if ( name == ATOM_config )	/* db_config */
      { term_t h = PL_new_term_ref();
	term_t a2 = PL_new_term_ref();

	while(PL_get_list(a, h, a))
	{ atom_t nm;
	  int ar;
	  const char *n;
	  char *v;

	  if ( !PL_get_name_arity(h, &nm, &ar) || ar !=	1 )
	    return pl_error(ERR_TYPE, "db_config", h);
	  PL_get_arg(1, h, a2);
	  if ( !get_chars_ex(a2, &v) )
	    return FALSE;
	  n = PL_atom_chars(nm);
	  config[nconf] = malloc(strlen(n)+strlen(v)+2);
	  strcpy(config[nconf], n);
	  strcat(config[nconf], " ");
	  strcat(config[nconf], v);
	  config[++nconf] = NULL;
	}
	if ( !PL_get_nil(a) )
	  return pl_error(ERR_TYPE, "list", a);
      } else
	return pl_error(ERR_DOMAIN, "db_option", v);
    } else
      return pl_error(ERR_TYPE, "option", head);
  }

  if ( !PL_get_nil(options) )
    return pl_error(ERR_TYPE, "list", options);

  if ( (rval=db_env->open(db_env, home, flags, 0666)) != 0 )
    return db_status(rval);

  atexit(cleanup);

  return TRUE;
}



		 /*******************************
		 *     TMP ATOM PROLOG STUFF	*
		 *******************************/

static foreign_t
pl_db_atom(term_t handle, term_t atom, term_t id)
{ dbh *db;
  atom_t a;
  atomid_t aid;
    
  if ( !get_db(handle, &db) )
    return FALSE;

  if ( PL_get_atom(atom, &a) )
  { if ( !db_atom_id(db, a, &aid, DB4PL_ATOM_CREATE) )
      return FALSE;

    return PL_unify_integer(id, aid);
  } else if ( PL_get_long(id, &aid) )
  { if ( !pl_atom_from_db(db, aid, &a) )
      return FALSE;

    return PL_unify_atom(atom, a);
  } else
    return pl_error(ERR_TYPE, "atom", atom);
}


install_t
install()
{ initConstants();

  PL_register_foreign("db_open",   4, pl_db_open,   0);
  PL_register_foreign("db_close",  1, pl_db_close,  0);
  PL_register_foreign("db_closeall", 0, pl_db_closeall,  0);
  PL_register_foreign("db_put",    3, pl_db_put,    0);
  PL_register_foreign("db_del",    2, pl_db_del2,   0);
  PL_register_foreign("db_del",    3, pl_db_del3,   PL_FA_NONDETERMINISTIC);
  PL_register_foreign("db_getall", 3, pl_db_getall, 0);
  PL_register_foreign("db_get",    3, pl_db_get,    PL_FA_NONDETERMINISTIC);
  PL_register_foreign("db_enum",   3, pl_db_enum,   PL_FA_NONDETERMINISTIC);
  PL_register_foreign("db_init",   1, pl_db_init,   0);
  PL_register_foreign("db_transaction", 1, pl_db_transaction,
						    PL_FA_TRANSPARENT);

  PL_register_foreign("db_atom",  3, pl_db_atom,  0);
}


install_t
uninstall()
{ cleanup();
}
