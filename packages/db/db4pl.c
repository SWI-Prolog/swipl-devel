/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
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

static functor_t FUNCTOR_db1;
static functor_t FUNCTOR_type1;

DB_ENV db_env;				/* default environment */

#define mkfunctor(n, a) PL_new_functor(PL_new_atom(n), a)

static void
initConstants()
{
  ATOM_read	    = PL_new_atom("read");
  ATOM_update	    = PL_new_atom("update");
  ATOM_true	    = PL_new_atom("true");
  ATOM_false	    = PL_new_atom("false");
  ATOM_btree	    = PL_new_atom("btree");
  ATOM_hash	    = PL_new_atom("hash");
  ATOM_recno	    = PL_new_atom("recno");
  ATOM_unknown	    = PL_new_atom("unknown");
  ATOM_duplicates   = PL_new_atom("duplicates");
  ATOM_mp_size	    = PL_new_atom("mp_size");
  ATOM_mp_mmapsize  = PL_new_atom("mp_mmapsize");
  ATOM_home	    = PL_new_atom("home");
  ATOM_config	    = PL_new_atom("config");
  ATOM_locking	    = PL_new_atom("locking");
  ATOM_logging	    = PL_new_atom("logging");
  ATOM_transactions = PL_new_atom("transactions");
  ATOM_create       = PL_new_atom("create");

  FUNCTOR_db1	    = mkfunctor("$db", 1);
  FUNCTOR_type1	    = mkfunctor("type", 1);
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
unify_dbt(term_t t, DBT *dbt)
{ term_t r = PL_new_term_ref();

  PL_recorded_external(dbt->data, r);
  
  return PL_unify(t, r);
}


static int
get_dbt(term_t t, DBT *dbt)
{ unsigned int len;

  memset(dbt, 0, sizeof(*dbt));
  dbt->data = PL_record_external(t, &len);
  dbt->size = len;

  return TRUE;
}


static void
free_dbt(DBT *dbt)
{ PL_erase_external(dbt->data);
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
{ if ( rval == 0 )
    return TRUE;

  if ( rval < 0 )
    return FALSE;			/* normal failure */

  return pl_error(ERR_ERRNO, rval, "?"); /* system error */
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
db_options(term_t t, int type, DB_INFO *info)
{ term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();

  while( PL_get_list(tail, head, tail) )
  { atom_t name;
    int arity;

    if ( PL_get_name_arity(head, &name, &arity) )
    { if ( arity == 1 )
      { term_t a0 = PL_new_term_ref();

	_PL_get_arg(1, head, a0);
	if ( name == ATOM_duplicates && type == DB_BTREE )
	{ int v;

	  if ( !get_bool_ex(a0, &v) )
	    return FALSE;

	  if ( v )
	    info->flags |= DB_DUP;
	} else
	  return pl_error(ERR_DOMAIN, "db_option", head);
      }
    }
  }

  if ( !PL_get_nil(tail) )
    return pl_error(ERR_TYPE, "list", t);

  return TRUE;
}


static foreign_t
pl_db_open(term_t file, term_t mode, term_t handle, term_t options)
{ char *fname;
  int flags;
  int m = 0666;
  int type = DB_BTREE;
  DB_INFO info;
  DB *db;
  atom_t a;
  int rval;

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
  
  memset(&info, 0, sizeof(info));
  if ( !db_type(options, &type) ||
       !db_options(options, type, &info) )
    return FALSE;
  
  if ( (rval=db_open(fname, type, flags, m, &db_env, &info, &db)) == 0 )
  { dbh *dbh = calloc(1, sizeof(*dbh));

    dbh->magic = DBH_MAGIC;
    dbh->db = db;

    if ( type == DB_BTREE && (info.flags & DB_DUP) )
      dbh->duplicates = TRUE;

    return unify_db(handle, dbh);
  }

  return pl_error(ERR_ERRNO, rval);
}


static foreign_t
pl_db_close(term_t handle)
{ dbh *db;

  if ( get_db(handle, &db) )
  { int rval;

    rval = db->db->close(db->db, 0);
    db->magic = 0;
    free(db);

    return db_status(rval);
  }

  return FALSE;
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
{ if ( db_env.tx_info )
  { int rval;
    DB_TXN *pid, *tid;
    transaction *t;

    if ( transaction_stack )
      pid = transaction_stack->tid;
    else
      pid = NULL;

    if ( (rval=txn_begin(db_env.tx_info, pid, &tid)) )
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

    if ( (rval=txn_commit(tid)) )
      return db_status(rval);
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

  if ( !call1 )
    call1 = PL_predicate("call", 1, "user");
  
  if ( !begin_transaction() )
    return FALSE;

  if ( PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, call1, goal) )
    return commit_transaction();

  if ( !abort_transaction() )
    return FALSE;

  return FALSE;
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

  get_dbt(key, &k);
  get_dbt(value, &v);

  rval = db_status(db->db->put(db->db, current_transaction(), &k, &v, flags));
  free_dbt(&k);
  free_dbt(&v);

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

  get_dbt(key, &k);

  rval = db_status(db->db->del(db->db, current_transaction(), &k, flags));
  free_dbt(&k);

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

  get_dbt(key, &k);
  memset(&v, 0, sizeof(v));

  if ( db->duplicates )			/* must use a cursor */
  { DBC *cursor;
    term_t tail = PL_copy_term_ref(value);
    term_t head = PL_new_term_ref();

    if ( (rval = db->db->cursor(db->db, current_transaction(), &cursor)) != 0 )
      return db_status(rval);

    if ( (rval=cursor->c_get(cursor, &k, &v, DB_SET)) == 0 )
    { DBT k2;

      if ( !PL_unify_list(tail, head, tail) ||
	   !unify_dbt(head, &v) )
      { cursor->c_close(cursor);
	return FALSE;
      }

      memset(&k2, 0, sizeof(k2));
      for(;;)
      { if ( (rval=cursor->c_get(cursor, &k2, &v, DB_NEXT)) == 0 &&
	     equal_dbt(&k, &k2) )
	{ if ( PL_unify_list(tail, head, tail) &&
	       unify_dbt(head, &v) )
	    continue;
	}
	if ( rval <= 0 )		/* normal failure */
	{ cursor->c_close(cursor);
	  free_dbt(&k);
	  return PL_unify_nil(tail);
	} else				/* error failure */
	{ cursor->c_close(cursor);
	  free_dbt(&k);
	  return db_status(rval);
	}
      }
    } else if ( rval == DB_NOTFOUND )
    { free_dbt(&k);
      return FALSE;
    } else
    { free_dbt(&k);
      return db_status(rval);
    }
  } else
  { if ( (rval=db->db->get(db->db, current_transaction(), &k, &v, 0)) == 0 )
    { term_t t = PL_new_term_ref();
      term_t tail = PL_copy_term_ref(value);
      term_t head = PL_new_term_ref();

      free_dbt(&k);
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
  int do_enum;				/* enumerate the DB */
} dbget_ctx;

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

      if ( PL_is_variable(key) )	/* enumerate the DB */
      { c = calloc(1, sizeof(*c));

	c->db = db;
	if ( (rval = db->db->cursor(db->db, NULL, &c->cursor)) != 0 )
	{ free(c);
	  return db_status(rval);
	}

	c->do_enum = TRUE;
	rval = c->cursor->c_get(c->cursor, &c->key, &c->value, DB_FIRST);
	if ( rval == 0 )
	{ fid = PL_open_foreign_frame();

	  if ( unify_dbt(key, &c->key) &&
	       unify_dbt(value, &c->value) )
	  { DO_DEL;
	      
	    PL_close_foreign_frame(fid);
	    PL_retry_address(c);
	  }

	  PL_rewind_foreign_frame(fid);
	  goto retry;
	}
	goto out;
      }

      if ( db->duplicates )		/* DB with duplicates */
      { c = calloc(1, sizeof(*c));

	c->db = db;
	if ( (rval = db->db->cursor(db->db, NULL, &c->cursor)) != 0 )
	{ free(c);
	  return db_status(rval);
	}
	get_dbt(key, &c->key);

	rval = c->cursor->c_get(c->cursor, &c->key, &c->value, DB_SET);
	if ( rval == 0 )
	{ fid = PL_open_foreign_frame();

	  if ( unify_dbt(value, &c->value) )
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

	get_dbt(key, &k);
	memset(&v, 0, sizeof(v));

	if ( (rval=db->db->get(db->db, NULL, &k, &v, 0)) == 0 )
	  return unify_dbt(value, &v);

	free_dbt(&k);
	return db_status(rval);
      }
    case PL_REDO:
      c = PL_foreign_context_address(ctx);

    retry:
      for(;;)
      { rval = c->cursor->c_get(c->cursor, &c->k2, &c->value, DB_NEXT);

	if ( rval == 0 && (equal_dbt(&c->key, &c->k2) || c->do_enum) )
	{ if ( !fid )
	    fid = PL_open_foreign_frame();
	  if ( (!c->do_enum || unify_dbt(key, &c->k2)) &&
	       unify_dbt(value, &c->value) )
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
      break;
  }

out:
  if ( c )
  { c->cursor->c_close(c->cursor);
    free_dbt(&c->key);
    free(c);
  }
  if ( fid )
    PL_close_foreign_frame(fid);

  db_status(rval);
  return FALSE;				/* also on rval = 0! */
}


static foreign_t
pl_db_get(term_t handle, term_t key, term_t value, control_t ctx)
{ return pl_db_getdel(handle, key, value, ctx, FALSE);
}


static foreign_t
pl_db_del3(term_t handle, term_t key, term_t value, control_t ctx)
{ return pl_db_getdel(handle, key, value, ctx, TRUE);
}


static int app_initted = FALSE;

static void
cleanup()
{ if ( app_initted )
  { db_appexit(&db_env);
    app_initted = FALSE;
  }
}


		 /*******************************
		 *	     APPINIT		*
		 *******************************/

static void
pl_db_error(const char *prefix, char *buffer)
{ Sdprintf("%s%s\n", prefix, buffer);
}


#define MAXCONFIG 20

static foreign_t
pl_db_init(term_t option_list)
{ int rval;
  term_t options = PL_copy_term_ref(option_list);
  u_int32_t flags = 0;
  term_t head = PL_new_term_ref();
  term_t a    = PL_new_term_ref();
  char *home = NULL;
  char *config[MAXCONFIG];
  int nconf = 0;

  if ( app_initted )
    return pl_error(ERR_PACKAGE_INT, "db", 0, "Already initialized");

  config[0] = NULL;

  db_env.db_errpfx = "db4pl: ";
  db_env.db_errcall = pl_db_error;

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
	db_env.mp_mmapsize = v;
	flags |= DB_INIT_MPOOL;
      } else if ( name == ATOM_mp_size ) /* mp_size */
      { if ( !get_size_ex(a, &v) )
	  return FALSE;
	db_env.mp_size = v;
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
	  flags |= DB_INIT_TXN;
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

  if ( (rval=db_appinit(home, config, &db_env, flags)) != 0 )
    return db_status(rval);

  app_initted = TRUE;
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
  PL_register_foreign("db_put",    3, pl_db_put,    0);
  PL_register_foreign("db_del",    2, pl_db_del2,   0);
  PL_register_foreign("db_del",    3, pl_db_del3,   PL_FA_NONDETERMINISTIC);
  PL_register_foreign("db_getall", 3, pl_db_getall, 0);
  PL_register_foreign("db_get",    3, pl_db_get,    PL_FA_NONDETERMINISTIC);
  PL_register_foreign("db_init",   1, pl_db_init,   0);
  PL_register_foreign("db_transaction", 1, pl_db_transaction,
						    PL_FA_TRANSPARENT);

  PL_register_foreign("db_atom",  3, pl_db_atom,  0);
}


install_t
uninstall()
{ cleanup();
}
