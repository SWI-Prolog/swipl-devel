/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module is based on pl_odbc.{c,pl},   a  read-only ODBC interface by
Stefano  De  Giorgi  (s.degiorgi@tin.it).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef WIN32
#include <windows.h>
#define HAVE_MKTIME 1
#define HAVE_GMTIME 1
#else
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#endif

#include <sql.h>
#include <sqlext.h>
#include <time.h>
#include <limits.h>			/* LONG_MAX, etc. */

#ifdef WIN32
typedef DWORD SQLLEN;
#endif

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#ifndef NULL
#define NULL 0
#endif
#define MAX_NAME_LEN 50
#define MAX_STMT_LEN 100
#define STRICT

#define NameBufferLength 256
#define CVNERR -1			/* conversion error */

static atom_t    ATOM_row;		/* "row" */
static atom_t    ATOM_informational;	/* "informational" */
static atom_t	 ATOM_default;		/* "default" */
static atom_t	 ATOM_once;		/* "once" */
static atom_t	 ATOM_multiple;		/* "multiple" */
static atom_t	 ATOM_commit;		/* "commit" */
static atom_t	 ATOM_rollback;		/* "rollback" */
static atom_t	 ATOM_atom;
static atom_t	 ATOM_string;
static atom_t	 ATOM_codes;
static atom_t	 ATOM_float;
static atom_t	 ATOM_integer;
static atom_t	 ATOM_time;
static atom_t	 ATOM_date;
static atom_t	 ATOM_timestamp;
static atom_t	 ATOM_all_types;
static atom_t	 ATOM_null;		/* default null atom */

static functor_t FUNCTOR_timestamp7;	/* timestamp/7 */
static functor_t FUNCTOR_time3;		/* time/7 */
static functor_t FUNCTOR_date3;		/* date/3 */
static functor_t FUNCTOR_odbc3;		/* odbc(state, code, message) */
static functor_t FUNCTOR_error2;	/* error(Formal, Context) */
static functor_t FUNCTOR_type_error2;	/* type_error(Term, Expected) */
static functor_t FUNCTOR_domain_error2;	/* domain_error(Term, Expected) */
static functor_t FUNCTOR_existence_error2; /* existence_error(Term, Expected) */
static functor_t FUNCTOR_representation_error1; /* representation_error(What) */
static functor_t FUNCTOR_resource_error1; /* resource_error(Error) */
static functor_t FUNCTOR_odbc_statement1; /* $odbc_statement(Id) */
static functor_t FUNCTOR_odbc_connection1;
static functor_t FUNCTOR_user1;
static functor_t FUNCTOR_password1;
static functor_t FUNCTOR_alias1;
static functor_t FUNCTOR_open1;
static functor_t FUNCTOR_auto_commit1;
static functor_t FUNCTOR_types1;
static functor_t FUNCTOR_minus2;
static functor_t FUNCTOR_gt2;
static functor_t FUNCTOR_context_error3;
static functor_t FUNCTOR_data_source2;
static functor_t FUNCTOR_null1;

#define SQL_PL_DEFAULT  0		/* don't change! */
#define SQL_PL_ATOM	1		/* return as atom */
#define SQL_PL_CODES	2		/* return as code-list */
#define SQL_PL_STRING	3		/* return as string */
#define SQL_PL_INTEGER	4		/* return as integer */
#define SQL_PL_FLOAT	5		/* return as float */
#define SQL_PL_TIME	6		/* return as time/3 structure */
#define SQL_PL_DATE	7		/* return as date/3 structure */
#define SQL_PL_TIMESTAMP 8		/* return as timestamp/7 structure */

#define PARAM_BUFSIZE sizeof(double)

typedef struct
{ SWORD        cTypeID;			/* C type of value */
  SWORD	       plTypeID;		/* Prolog type of value */
  SWORD	       sqlTypeID;		/* Sql type of value */
  SWORD	       scale;			/* Scale */
  SQLPOINTER  *ptr_value;		/* ptr to value */
  SQLLEN       length_ind;		/* length/indicator of value */
  SQLLEN       len_value;		/* length of value (as parameter)  */
  char	       buf[PARAM_BUFSIZE];	/* Small buffer for simple cols */
} parameter;

typedef struct
{ enum
  { NULL_VAR,				/* represent as variable */
    NULL_ATOM,				/* some atom */
    NULL_FUNCTOR,			/* e.g. null(_) */
    NULL_RECORD				/* an arbitrary term */
  } nulltype;
  union
  { atom_t atom;			/* as atom */
    functor_t functor;			/* as functor */
    record_t record;			/* as term */
  } nullvalue;
} nulldef;				/* Prolog's representation of NULL */

typedef struct connection
{ long	       magic;			/* magic code */
  atom_t       alias;			/* alias name of the connection */
  atom_t       dsn;			/* DSN name of the connection */
  HDBC	       hdbc;			/* ODBC handle */
  nulldef     *null;			/* Prolog null value */
  struct connection *next;		/* next in chain */
} connection;

typedef struct
{ long	       magic;			/* magic code */
  connection  *connection;		/* connection used */
  HENV	       henv;			/* ODBC environment */
  HSTMT	       hstmt;			/* ODBC statement handle */
  RETCODE      rc;			/* status of last operation */
  parameter   *params;			/* Input parameters */
  parameter   *result;			/* Outputs (row descriptions) */
  SQLSMALLINT  NumCols;			/* # columns */
  SQLSMALLINT  NumParams;		/* # parameters */
  functor_t    db_row;			/* Functor for row */
  unsigned int sqllen;			/* length of statement */
  char        *sqltext;			/* statement text */
  unsigned     flags;			/* general flags */
  nulldef     *null;			/* Prolog null value */
  struct context *clones;		/* chain of clones */
} context;

static struct
{ long	statements_created;		/* # created statements */
  long  statements_freed;		/* # destroyed statements */
} statistics;


#define CON_MAGIC      0x7c42b620	/* magic code */
#define CTX_MAGIC      0x7c42b621	/* magic code */

#define CTX_PERSISTENT  0x0001		/* persistent statement handle */
#define CTX_BOUND       0x0002		/* result-columns are bound */
#define	CTX_SQLMALLOCED 0x0004		/* sqltext is malloced */
#define CTX_INUSE	0x0008		/* statement is running */
#define CTX_OWNNULL	0x0010		/* null-definition is not shared */

#define true(s, f)	((s)->flags & (f))
#define false(s, f)	!true(s, f)
#define set(s, f)	((s)->flags |= (f))
#define clear(s, f)	((s)->flags &= ~(f))

static  HENV henv;			/* environment handle (ODBC) */


/* Prototypes */
static int pl_put_row(term_t, context *);
static SWORD CvtSqlToCType(SQLSMALLINT, SQLSMALLINT);
static void free_context(context *ctx);
static void close_context(context *ctx);
static foreign_t odbc_set_connection(term_t con, term_t option);
static int get_pltype(term_t t, SWORD *type);
static SWORD get_sqltype_from_atom(atom_t name, SWORD *type);


		 /*******************************
		 *	      ERRORS		*
		 *******************************/

static int
odbc_report(HENV henv, HDBC hdbc, HSTMT hstmt, RETCODE rc)
{ SQLCHAR state[16];			/* Normally 5-character ID */
  DWORD   native;
  SQLCHAR message[SQL_MAX_MESSAGE_LENGTH];
  SWORD   msglen;
  term_t  msg = PL_new_term_ref();

  if ( SQLError(henv, hdbc, hstmt, state, &native, message,
		sizeof(message), &msglen) == SQL_SUCCESS )
  { PL_unify_term(msg, PL_FUNCTOR, FUNCTOR_odbc3,
			 PL_CHARS,   state,
		         PL_INTEGER, (long)native,
		         PL_NCHARS,  msglen, message);
  } else if ( rc != SQL_ERROR )
    return TRUE;

  switch(rc)
  { case SQL_SUCCESS_WITH_INFO:
    { fid_t fid = PL_open_foreign_frame();
      predicate_t pred = PL_predicate("print_message", 2, "user");
      term_t av = PL_new_term_refs(2);
      
      PL_put_atom(av+0, ATOM_informational);
      PL_put_term(av+1, msg);
	
      PL_call_predicate(NULL, PL_Q_NORMAL, pred, av);
      PL_discard_foreign_frame(fid);
	
      return TRUE;
    }
    case SQL_ERROR:
    { term_t ex = PL_new_term_ref();
      
      PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		    PL_TERM, msg,
		    PL_VARIABLE);

      return PL_raise_exception(ex);
    }
    default:
      return PL_warning("Statement returned %d\n", rc);
  }
}

#define TRY(ctxt, stmt) \
	{ ctxt->rc = (stmt); \
	  if ( !report_status(ctxt) ) \
	    return FALSE; \
	}

static int
report_status(context *ctxt)
{ int rval;

  switch(ctxt->rc)
  { case SQL_SUCCESS:
      return TRUE;
    case SQL_NO_DATA_FOUND:
      close_context(ctxt);
      return FALSE;
    case SQL_INVALID_HANDLE:
    { free_context(ctxt);
      return PL_warning("Invalid handle: %p", ctxt->hstmt);
    }
  }

  if ( !(rval=odbc_report(ctxt->henv, ctxt->connection->hdbc,
			  ctxt->hstmt, ctxt->rc)) )
    close_context(ctxt);

  return rval;
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
existence_error(term_t actual, const char *expected)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_existence_error2,
		        PL_CHARS, expected,
		        PL_TERM, actual,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}

static int
resource_error(const char *error)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_resource_error1,
		        PL_CHARS, error,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
representation_error(term_t t, const char *error)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_representation_error1,
		        PL_CHARS, error,
		      PL_TERM, t);

  return PL_raise_exception(ex);
}


static int
context_error(term_t term, const char *error, const char *what)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_context_error3,
			PL_TERM, term,
		        PL_CHARS, error,
			PL_CHARS, what,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


		 /*******************************
		 *	     PRIMITIVES		*
		 *******************************/

#define get_name_arg_ex(i, t, n)  \
	PL_get_typed_arg_ex(i, t, PL_get_atom_chars, "atom", n)
#define get_atom_arg_ex(i, t, n)  \
	PL_get_typed_arg_ex(i, t, PL_get_atom, "atom", n)
#define get_int_arg_ex(i, t, n)   \
	PL_get_typed_arg_ex(i, t, PL_get_integer, "integer", n)
#define get_bool_arg_ex(i, t, n)   \
	PL_get_typed_arg_ex(i, t, PL_get_bool, "boolean", n)
#define get_float_arg_ex(i, t, n) \
	PL_get_typed_arg_ex(i, t, PL_get_float, "float", n)

static int
PL_get_typed_arg_ex(int i, term_t t, int (*func)(), const char *ex, void *ap)
{ term_t a = PL_new_term_ref();

  if ( !PL_get_arg(i, t, a) ) 
    return type_error(t, "compound");
  if ( !(*func)(a, ap) )
    return type_error(a, ex);

  return TRUE;
}

#define get_int_arg(i, t, n)   \
	PL_get_typed_arg(i, t, PL_get_integer, n)

static int
PL_get_typed_arg(int i, term_t t, int (*func)(), void *ap)
{ term_t a = PL_new_term_ref();

  if ( !PL_get_arg(i, t, a) ) 
    return FALSE;
  return (*func)(a, ap);
}


static int
list_length(term_t list)
{ term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();
  int n = 0;

  while(PL_get_list(tail, head, tail))
    n++;

  if ( !PL_get_nil(tail) )
  { type_error(tail, "list");
    return -1;
  }

  return n;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int formatted_string(+Fmt-[Arg...], unsigned *len, char **out)
    Much like sformat, but this approach avoids avoids creating
    intermediate Prolog data.  Maybe we should publish pl_format()?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
formatted_string(term_t in, unsigned int *len, char **out)
{ term_t av = PL_new_term_refs(3);
  static predicate_t format;
  IOSTREAM *fd = Sopenmem(out, len, "w");

  if ( !format )
    format = PL_predicate("format", 3, "user");
  PL_unify_stream(av+0, fd);
  PL_get_arg(1, in, av+1);
  PL_get_arg(2, in, av+2);
					/* TBD: call natively */
  if ( !PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, format, av) )
  { Sclose(fd);
    if ( *out )
      free(*out);
    return FALSE;
  }
  
  Sclose(fd);
  return TRUE;
}


		 /*******************************
		 *	    NULL VALUES		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
There are many ways one  may  wish   to  handle  SQL  null-values. These
functions deal with the three common ways   specially  and can deal with
arbitrary representations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static nulldef *
nulldef_spec(term_t t)
{ atom_t a;
  functor_t f;
  nulldef *nd = malloc(sizeof(*nd));
  
  memset(nd, 0, sizeof(*nd));

  if ( PL_get_atom(t, &a) )
  { if ( a == ATOM_null )
    { free(nd);				/* TBD: not very elegant */
      return NULL;			/* default specifier */
    }
    nd->nulltype  = NULL_ATOM;
    nd->nullvalue.atom = a;
    PL_register_atom(a);		/* avoid atom-gc */
  } else if ( PL_is_variable(t) )
  { nd->nulltype = NULL_VAR;
  } else if ( PL_get_functor(t, &f) &&
	      PL_functor_arity(f) == 1 )
  { term_t a1 = PL_new_term_ref();

    PL_get_arg(1, t, a1);
    if ( PL_is_variable(a1) )
    { nd->nulltype = NULL_FUNCTOR;
      nd->nullvalue.functor = f;
    } else
      goto term;
  } else
  { term:
    nd->nulltype = NULL_RECORD;
    nd->nullvalue.record = PL_record(t);  
  }

  return nd;
}


static void
free_nulldef(nulldef *nd)
{ if ( nd )
  { switch(nd->nulltype)
    { case NULL_ATOM:
	PL_unregister_atom(nd->nullvalue.atom);
        break;
      case NULL_RECORD:
	PL_erase(nd->nullvalue.record);
        break;
      default:
	break;
    }

    free(nd);
  }
}


static void
put_sql_null(term_t t, nulldef *nd)
{ if ( nd )
  { switch(nd->nulltype)
    { case NULL_VAR:
	break;
      case NULL_ATOM:
	PL_put_atom(t, nd->nullvalue.atom);
        break;
      case NULL_FUNCTOR:
	PL_put_functor(t, nd->nullvalue.functor);
        break;
      case NULL_RECORD:
	PL_recorded(nd->nullvalue.record, t);
    }
  } else
    PL_put_atom(t, ATOM_null);
}


static int
is_sql_null(term_t t, nulldef *nd)
{ if ( nd )
  { switch(nd->nulltype)
    { case NULL_VAR:
	return PL_is_variable(t);
      case NULL_ATOM:
      { atom_t a;

	return PL_get_atom(t, &a) && a == nd->nullvalue.atom;
      }
      case NULL_FUNCTOR:
	return PL_is_functor(t, nd->nullvalue.functor);
      case NULL_RECORD:			/* TBD: Provide PL_unify_record */
      { term_t rec = PL_new_term_ref();
	PL_recorded(nd->nullvalue.record, rec);
	return PL_unify(t, rec);
      }
      default:				/* should not happen */
	assert(0);
        return FALSE;
    }
  } else
  { atom_t a;

    return PL_get_atom(t, &a) && a == ATOM_null;
  }
}


		 /*******************************
		 *	    CONNECTION		*
		 *******************************/

static connection *connections;

static connection *
find_connection(atom_t alias)
{ connection *c;

  for(c=connections; c; c=c->next)
  { if ( c->alias == alias )
      return c;
  }

  return NULL;
}


static connection *
find_connection_from_dsn(atom_t dsn)
{ connection *c;

  for(c=connections; c; c=c->next)
  { if ( c->dsn == dsn )
      return c;
  }

  return NULL;
}


static connection *
alloc_connection(atom_t alias, atom_t dsn)
{ connection *c;

  if ( alias && find_connection(alias) )
    return NULL;			/* already existenting */
  
  if ( !(c = malloc(sizeof(*c))) )
  { resource_error("memory");
    return NULL;
  }
  memset(c, 0, sizeof(*c));
  c->alias = alias;
  if ( alias )
    PL_register_atom(alias);
  c->dsn = dsn;
  PL_register_atom(dsn);

  c->next = connections;
  connections = c;

  return c;
}


static void
free_connection(connection *c)
{ if ( c == connections )
    connections = c->next;
  else
  { connection *c2;

    for(c2 = connections; c2; c2 = c2->next)
    { if ( c2->next == c )
      { c2->next = c->next;
	break;
      }
    }
  }

  if ( c->alias )
    PL_unregister_atom(c->alias);
  if ( c->dsn )
    PL_unregister_atom(c->dsn);
  free_nulldef(c->null);

  free(c);
}


static int
get_connection(term_t tdsn, connection **cn)
{ atom_t dsn;
  connection *c;

  if ( PL_is_functor(tdsn, FUNCTOR_odbc_connection1) )
  { term_t a = PL_new_term_ref();

    PL_get_arg(1, tdsn, a);
    if ( !PL_get_pointer(a, (void **)&c) )
      return type_error(tdsn, "odbc_connection");

    if ( !c->magic == CON_MAGIC )
      return existence_error(tdsn, "odbc_connection");
  } else
  { if ( !PL_get_atom(tdsn, &dsn) )
      return type_error(tdsn, "odbc_connection");
    if ( !(c=find_connection(dsn)) )
      return existence_error(tdsn, "odbc_connection");
  }
  
  *cn = c;

  return TRUE;
}


static int
unify_connection(term_t t, connection *cn)
{ if ( cn->alias )
    return PL_unify_atom(t, cn->alias);

  return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_odbc_connection1,
		            PL_POINTER, cn);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
odbc_connect(+DSN, -Connection, +Options)
    Create a new connection. Option is a list of options with the
    following standards:

	user(User)

	password(Password)

	alias(Name)
	    Alias-name for the connection.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAX_AFTER_OPTIONS 10

static foreign_t
pl_odbc_connect(term_t tdsource, term_t cid, term_t options)
{  atom_t dsn;
   const char *dsource;			/* odbc data source */
   char *uid = "";			/* user id */
   char *pwd = "";			/* password */
   atom_t alias = 0;			/* alias-name */
   atom_t open = 0;			/* open next connection */
   RETCODE rc;				/* result code for ODBC functions */
   HDBC hdbc;
   connection *cn;
   term_t tail = PL_copy_term_ref(options);
   term_t head = PL_new_term_ref();
   term_t after_open = PL_new_term_refs(MAX_AFTER_OPTIONS);
   int i, nafter = 0;

   /* Read parameters from terms. */
   if ( !PL_get_atom(tdsource, &dsn) )
     return type_error(tdsource, "atom");

   while(PL_get_list(tail, head, tail))
   { if ( PL_is_functor(head, FUNCTOR_user1) )
     { if ( !get_name_arg_ex(1, head, &uid) )
	 return FALSE;
     } else if ( PL_is_functor(head, FUNCTOR_password1) )
     { if ( !get_name_arg_ex(1, head, &pwd) )
	 return FALSE;
     } else if ( PL_is_functor(head, FUNCTOR_alias1) )
     { if ( !get_atom_arg_ex(1, head, &alias) )
	 return FALSE;
     } else if ( PL_is_functor(head, FUNCTOR_open1) )
     { if ( !get_atom_arg_ex(1, head, &open) )
	 return FALSE;
       if ( !(open == ATOM_once ||
	      open == ATOM_multiple) )
	 return domain_error(head, "open_mode");
     } else if ( PL_is_functor(head, FUNCTOR_auto_commit1) ||
		 PL_is_functor(head, FUNCTOR_null1) )
     { if ( nafter < MAX_AFTER_OPTIONS )
	 PL_put_term(after_open+nafter++, head);
       else
	 return PL_warning("Too many options"); /* shouldn't happen */
     } else
       return domain_error(head, "odbc_option");
   }
   if ( !PL_get_nil(tail) )
     return type_error(tail, "list");

   if ( !open )
     open = alias ? ATOM_once : ATOM_multiple;
   if ( open == ATOM_once && (cn = find_connection_from_dsn(dsn)) )
   { if ( alias && cn->alias != alias )
     { if ( !cn->alias )
       { if ( !find_connection(alias) )
	 { cn->alias = alias;
	   PL_register_atom(alias);
	 } else
	   return PL_warning("Alias already in use");
       } else
	 return PL_warning("Cannot redefined connection alias");
     }
     return unify_connection(cid, cn);
   }

   dsource = PL_atom_chars(dsn);

   if ( !henv )
     SQLAllocEnv(&henv);		/* Allocate an environment handle */

   SQLAllocConnect(henv, &hdbc);	/* Allocate a connection handle */
   /* Connect to a data source. */
   rc = SQLConnect(hdbc, (SQLCHAR *)dsource, SQL_NTS,
                         (SQLCHAR *)uid,     SQL_NTS,
                         (SQLCHAR *)pwd,     SQL_NTS);
   if ( rc != SQL_SUCCESS && !odbc_report(henv, hdbc, NULL, rc) )
     return FALSE;

   if ( !(cn=alloc_connection(alias, dsn)) )
     return FALSE;

   cn->hdbc = hdbc;

   if ( !unify_connection(cid, cn) )
   { free_connection(cn);
     return FALSE;
   }

   for(i=0; i<nafter; i++)
   { if ( !odbc_set_connection(cid, after_open+i) )
     { free_connection(cn);
       return FALSE;
     }
   }

   return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
odbc_disconnect/0
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
pl_odbc_disconnect(term_t dsn)
{ connection *cn;
  
  if ( get_connection(dsn, &cn) )
    return FALSE;

  SQLDisconnect(cn->hdbc);  /* Disconnect from the data source */
  SQLFreeConnect(cn->hdbc); /* Free the connection handle */
  free_connection(cn);

  return TRUE;
}


static foreign_t
odbc_current_connection(term_t cid, term_t dsn, control_t h)
{ connection *cn;

  switch(PL_foreign_control(h))
  { case PL_FIRST_CALL:
      if ( !PL_is_variable(cid) )
      { if ( get_connection(cid, &cn) &&
	     PL_unify_atom(dsn, cn->dsn) )
	  return TRUE;
	
	return FALSE;
      }

      cn = connections;
      goto next;

    case PL_REDO:
      cn = PL_foreign_context_address(h);
    next:
    { fid_t fid = PL_open_foreign_frame();

      for(; cn; cn = cn->next, PL_rewind_foreign_frame(fid))
      { if ( unify_connection(cid, cn) &&
	     PL_unify_atom(dsn, cn->dsn) )
	{ PL_close_foreign_frame(fid);
	  if ( cn->next )
	    PL_retry_address(cn);
	  return TRUE;
	}
      }

      PL_close_foreign_frame(fid);
      return FALSE;
    }

    default:
      return FALSE;
  }
}


static foreign_t
odbc_set_connection(term_t con, term_t option)
{ connection *cn;
  RETCODE rc;
  UWORD opt;
  UDWORD optval;

  if ( !get_connection(con, &cn) )
    return FALSE;

  if ( PL_is_functor(option, FUNCTOR_auto_commit1) )
  { int val;

    if ( !get_bool_arg_ex(1, option, &val) )
      return FALSE;
    opt = SQL_AUTOCOMMIT;
    optval = (val ? SQL_AUTOCOMMIT_ON : SQL_AUTOCOMMIT_OFF);
  } else if ( PL_is_functor(option, FUNCTOR_null1) )
  { term_t a = PL_new_term_ref();

    PL_get_arg(1, option, a);
    cn->null = nulldef_spec(a);

    return TRUE;
  } else
    return domain_error(option, "odbc_option");

  if ( (rc=SQLSetConnectOption(cn->hdbc, opt, optval)) != SQL_SUCCESS )
    return odbc_report(henv, cn->hdbc, NULL, rc);

  return TRUE;
}


static foreign_t
odbc_end_transaction(term_t conn, term_t action)
{ connection *cn;
  RETCODE rc;
  UWORD opt;
  atom_t a;
  

  if ( !get_connection(conn, &cn) )
    return FALSE;

  if ( !PL_get_atom(action, &a) )
    return type_error(action, "atom");
  if ( a == ATOM_commit )
  { opt = SQL_COMMIT;
  } else if ( a == ATOM_rollback )
  { opt = SQL_ROLLBACK;
  } else
    return domain_error(action, "transaction");
  
  if ( (rc=SQLTransact(henv, cn->hdbc, opt)) != SQL_SUCCESS )
    return odbc_report(henv, cn->hdbc, NULL, rc);

  return TRUE;
}


		 /*******************************
		 *	CONTEXT (STATEMENTS)	*
		 *******************************/

static context *
new_context(connection *cn)
{ context *ctxt = (context *) malloc(sizeof(context));

  memset(ctxt, 0, sizeof(*ctxt));
  ctxt->magic = CTX_MAGIC;
  ctxt->henv  = henv;
  ctxt->connection = cn;
  ctxt->null = cn->null;
  SQLAllocStmt(cn->hdbc, &ctxt->hstmt);
  statistics.statements_created++;

  return ctxt;
}


static void
close_context(context *ctxt)
{ clear(ctxt, CTX_INUSE);

  if ( ctxt->flags & CTX_PERSISTENT )
  { if ( ctxt->hstmt )
      SQLFreeStmt(ctxt->hstmt, SQL_CLOSE);
  } else
    free_context(ctxt);
}


static void
free_parameters(int n, parameter *params)
{ if ( n && params )
  { parameter *p = params;
    int i;

    for (i=0; i<n; i++, p++)
    { if ( p->ptr_value && p->ptr_value != (void *)p->buf )
	free(p->ptr_value);
    }

    free(params);
  }
}


static void
free_context(context *ctx)
{ ctx->magic = 0;

  if ( ctx->hstmt )
    SQLFreeStmt(ctx->hstmt, SQL_DROP);

  free_parameters(ctx->NumCols,   ctx->result);
  free_parameters(ctx->NumParams, ctx->params);
  if ( true(ctx, CTX_SQLMALLOCED) )
    free(ctx->sqltext);
  if ( true(ctx, CTX_OWNNULL) )
    free_nulldef(ctx->null);
  free(ctx);

  statistics.statements_freed++;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
clone_context()

Create a clone of a context, so we   can have the same statement running
multiple times. Is there really no better   way  to handle this? Can't I
have multiple cursors on a statement?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static context *
clone_context(context *in)
{ context *new = new_context(in->connection);

					/* Copy SQL statement */
  new->sqltext = malloc(in->sqllen+1);
  new->sqllen = in->sqllen;
  memcpy(new->sqltext, in->sqltext, in->sqllen+1);
  set(new, CTX_SQLMALLOCED);

					/* Prepare the statement */
  TRY(new, SQLPrepare(new->hstmt, new->sqltext, new->sqllen));

					/* Copy parameter declarations */
  if ( (new->NumParams = in->NumParams) > 0 )
  { int pn;
    parameter *p;

    new->params = malloc(sizeof(parameter)*new->NumParams);
    memcpy(new->params, in->params, sizeof(parameter)*new->NumParams);

    for(p=new->params, pn=1; pn<=new->NumParams; pn++, p++)
    { SQLLEN *vlenptr = NULL;

      switch(p->cTypeID)
      { case SQL_C_CHAR:
	case SQL_C_BINARY:
	  p->ptr_value = malloc(p->length_ind);
	  vlenptr = &p->len_value;
	  break;
      }

      TRY(new, SQLBindParameter(new->hstmt,		/* hstmt */
				(SWORD)pn,			/* ipar */
				SQL_PARAM_INPUT, 	/* fParamType */
				p->cTypeID, 		/* fCType */
				p->sqlTypeID,		/* fSqlType */
				p->length_ind,		/* cbColDef */
				p->scale,		/* ibScale */
				p->ptr_value,		/* rgbValue */
				0,			/* cbValueMax */
				vlenptr));		/* pcbValue */
    }
  }

					/* Copy result columns */
  new->db_row = in->db_row;		/* the row/N functor */

  if ( in->result )
  { new->NumCols = in->NumCols;
    new->result  = malloc(in->NumCols*sizeof(parameter));
    memcpy(new->result, in->result, in->NumCols*sizeof(parameter));

    if ( true(in, CTX_BOUND) )
    { parameter *p = new->result;
      int i;

      for(i = 1; i <= new->NumCols; i++, p++)
      { if ( p->len_value > PARAM_BUFSIZE )
	  p->ptr_value = malloc(p->len_value);
	else
	  p->ptr_value = (SQLPOINTER)p->buf;

	TRY(new, SQLBindCol(new->hstmt, (SWORD)i,
			    p->cTypeID,
			    p->ptr_value,
			    p->len_value,
			    &p->length_ind));
      }

      set(new, CTX_BOUND);
    }
  }


  return new;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Oops. The string is malloced by Prolog  and this probably poses problems
when using on Windows, where each DLL has  its own memory pool. Guess we
need PL_malloc() and PL_free() to get access   to the memory malloced by
Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
get_sql_text(context *ctxt, term_t tquery)
{ int qlen;
  char *q;

  if ( PL_is_functor(tquery, FUNCTOR_minus2) )
  { qlen = 0;
    q = NULL;

    if ( !formatted_string(tquery, &qlen, &q) )
      return FALSE;
    ctxt->sqltext = q;
    ctxt->sqllen = qlen;
    set(ctxt, CTX_SQLMALLOCED);
  } else if ( PL_get_nchars(tquery, &qlen, &q, CVT_ATOM|CVT_STRING|BUF_MALLOC))
  { ctxt->sqltext = q;
    ctxt->sqllen = qlen;
    set(ctxt, CTX_SQLMALLOCED);
  } else
    return type_error(tquery, "atom_or_format");

  return TRUE;
}


static int
prepare_result(context *ctxt)
{ SQLSMALLINT i;
  SQLCHAR nameBuffer[NameBufferLength];
  SQLSMALLINT nameLength, dataType, decimalDigits, nullable;
  SQLUINTEGER columnSize;
  parameter *ptr_result;
  SQLSMALLINT ncol;

  SQLNumResultCols(ctxt->hstmt, &ncol);
  if ( ncol == 0 )
    return TRUE;			/* no results */

  if ( ctxt->result )			/* specified types */
  { if ( ncol != ctxt->NumCols )
      return PL_warning("# columns mismatch"); /* TBD: exception */
  } else
  { ctxt->NumCols = ncol;
    ctxt->db_row = PL_new_functor(ATOM_row, ctxt->NumCols);
    ctxt->result = malloc(sizeof(parameter)*ctxt->NumCols);
    memset(ctxt->result, 0, sizeof(parameter)*ctxt->NumCols);
  }

  ptr_result = ctxt->result;
  for(i = 1; i <= ctxt->NumCols; i++)
  { SQLDescribeCol(ctxt->hstmt, i,
		   nameBuffer, NameBufferLength, &nameLength,
		   &dataType, &columnSize, &decimalDigits,
		   &nullable);
	
    ptr_result->cTypeID = CvtSqlToCType(dataType, ptr_result->plTypeID);
    if (ptr_result->cTypeID == CVNERR)
    { free_context(ctxt);
      return PL_warning("odbc_query/2: column type not managed");
    }

    switch (ptr_result->cTypeID)
    { case SQL_C_CHAR:
	ptr_result->len_value = sizeof(char)*columnSize+1;
	break;
      case SQL_C_SLONG:
	ptr_result->len_value = sizeof(SQLINTEGER);
	break;
      case SQL_C_DOUBLE:
	ptr_result->len_value = sizeof(SQLDOUBLE);
	break;
      case SQL_C_TYPE_DATE:
	ptr_result->len_value = sizeof(DATE_STRUCT);
	break;
      case SQL_C_TYPE_TIME:
	ptr_result->len_value = sizeof(TIME_STRUCT);
	break;
      case SQL_C_TIMESTAMP:
	ptr_result->len_value = sizeof(SQL_TIMESTAMP_STRUCT);
	break;
      default:
	assert(0);
        return FALSE;			/* make compiler happy */
    }
	
    if ( ptr_result->len_value <= PARAM_BUFSIZE )
      ptr_result->ptr_value = (void *)ptr_result->buf;
    else
      ptr_result->ptr_value = malloc(ptr_result->len_value);

    TRY(ctxt, SQLBindCol(ctxt->hstmt, i,
			 ptr_result->cTypeID,
			 ptr_result->ptr_value,
			 ptr_result->len_value,
			 &ptr_result->length_ind));
    
    ++ptr_result;
  }

  return TRUE;
}


static foreign_t
odbc_row(context *ctxt, term_t trow)
{ term_t local_trow = PL_new_term_ref();
  fid_t fid = PL_open_foreign_frame();

  if ( !true(ctxt, CTX_BOUND) )
  { if ( !prepare_result(ctxt) )
      return FALSE;
    set(ctxt, CTX_BOUND);
  }
      
  if ( !ctxt->result )
  { close_context(ctxt);
    return TRUE;			/* no results defined */
  }

  for(;;)
  { TRY(ctxt, SQLFetch(ctxt->hstmt));

    if ( !pl_put_row(local_trow, ctxt) )
    { close_context(ctxt);
      return FALSE;		/* with pending exception */
    }
      
    if ( !PL_unify(trow, local_trow) )
    { PL_rewind_foreign_frame(fid);
      continue;
    }
      
    PL_retry_address(ctxt);
  }
}


static int
set_column_types(context *ctxt, term_t option)
{ term_t tail = PL_new_term_ref();
  term_t head = PL_new_term_ref();
  parameter *p;
  int ntypes;

  PL_get_arg(1, option, tail);
  if ( (ntypes = list_length(tail)) < 0 )
    return FALSE;			/* not a proper list */

  ctxt->NumCols = ntypes;
  ctxt->db_row = PL_new_functor(ATOM_row, ctxt->NumCols);
  ctxt->result = malloc(sizeof(parameter)*ctxt->NumCols);
  memset(ctxt->result, 0, sizeof(parameter)*ctxt->NumCols);

  for(p = ctxt->result; PL_get_list(tail, head, tail); p++)
  { if ( !get_pltype(head, &p->plTypeID) )
      return FALSE;
  }
  if ( !PL_get_nil(tail) )
    return type_error(tail, "list");

  return TRUE;
}


static int
set_statement_options(context *ctxt, term_t options)
{ if ( !PL_get_nil(options) )
  { term_t tail = PL_copy_term_ref(options);
    term_t head = PL_new_term_ref();

    while(PL_get_list(tail, head, tail))
    { if ( PL_is_functor(head, FUNCTOR_types1) )
      { if ( !set_column_types(ctxt, head) )
	  return FALSE;
      } else if ( PL_is_functor(head, FUNCTOR_null1) )
      { term_t arg = PL_new_term_ref();

	PL_get_arg(1, head, arg);
	ctxt->null = nulldef_spec(arg);
	set(ctxt, CTX_OWNNULL);
      } else
	return domain_error(head, "odbc_option");
    }
    if ( !PL_get_nil(tail) )
      return type_error(tail, "list");
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
odbc_query(+DSN, +SQL, -Row)
    Execute an SQL query, returning the result-rows 1-by-1 on
    backtracking
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
pl_odbc_query(term_t dsn, term_t tquery, term_t trow, term_t options,
	      control_t handle)
{ context *ctxt;

  switch( PL_foreign_control(handle) )
  { case PL_FIRST_CALL:   
    { connection *cn;
  
      if ( !get_connection(dsn, &cn) )
	return FALSE;
         
      ctxt = new_context(cn);
      if ( !get_sql_text(ctxt, tquery) )
      { free_context(ctxt);
	return FALSE;
      }

      if ( !set_statement_options(ctxt, options) )
      { free_context(ctxt);
	return FALSE;
      }
      set(ctxt, CTX_INUSE);
      TRY(ctxt, SQLExecDirect(ctxt->hstmt, ctxt->sqltext, ctxt->sqllen));

      return odbc_row(ctxt, trow);
    }
    case PL_REDO:
      return odbc_row(PL_foreign_context_address(handle), trow);

    default:
    case PL_CUTTED:
      free_context(PL_foreign_context_address(handle));
      return TRUE;
  }
}


		 /*******************************
		 *	DICTIONARY SUPPORT	*
		 *******************************/

static foreign_t
odbc_tables(term_t dsn, term_t row, control_t handle)
{ switch( PL_foreign_control(handle) )
  { case PL_FIRST_CALL:
    { connection *cn;
      context *ctxt;

      if ( !get_connection(dsn, &cn) )
	return FALSE;

      ctxt = new_context(cn);
      ctxt->null = NULL;		/* use default $null$ */
      TRY(ctxt, SQLTables(ctxt->hstmt, NULL,0,NULL,0,NULL,0,NULL,0));

      return odbc_row(ctxt, row);
    }
    case PL_REDO:
      return odbc_row(PL_foreign_context_address(handle), row);

    case PL_CUTTED:
      free_context(PL_foreign_context_address(handle));
      return TRUE;

    default:
      assert(0);
      return FALSE;
  }
}


static foreign_t
pl_odbc_column(term_t dsn, term_t db, term_t row, control_t handle)
{ switch( PL_foreign_control(handle) )
  { case PL_FIRST_CALL:
    { connection *cn;
      context *ctxt;
      unsigned int len;
      char *s;

      if ( !PL_get_nchars(db, &len, &s, CVT_ATOM|CVT_STRING) )
	return type_error(db, "atom");

      if ( !get_connection(dsn, &cn) )
	return FALSE;
      ctxt = new_context(cn);
      ctxt->null = NULL;		/* use default $null$ */
      TRY(ctxt, SQLColumns(ctxt->hstmt, NULL, 0, NULL, 0,
			   (SQLCHAR*)s, (SWORD)len, NULL, 0));
      
      return odbc_row(ctxt, row);
    }
    case PL_REDO:
      return odbc_row(PL_foreign_context_address(handle), row);

    case PL_CUTTED:
      free_context(PL_foreign_context_address(handle));
      return TRUE;

    default:
      assert(0);
      return FALSE;
  }
}


static foreign_t
odbc_types(term_t dsn, term_t sqltype, term_t row, control_t handle)
{ switch( PL_foreign_control(handle) )
  { case PL_FIRST_CALL:
    { connection *cn;
      context *ctxt;
      atom_t tname;
      SWORD type;
      int v;

      if ( PL_get_integer(sqltype, &v) )
      { type = v;
      } else
      { if ( !PL_get_atom(sqltype, &tname) )
	  return type_error(sqltype, "sql_type");
	if ( tname == ATOM_all_types )
	  type = SQL_ALL_TYPES;
	else if ( !get_sqltype_from_atom(tname, &type) )
	  return domain_error(sqltype, "sql_type");
      }

      if ( !get_connection(dsn, &cn) )
	return FALSE;
      ctxt = new_context(cn);
      ctxt->null = NULL;		/* use default $null$ */
      TRY(ctxt, SQLGetTypeInfo(ctxt->hstmt, type));
      
      return odbc_row(ctxt, row);
    }
    case PL_REDO:
      return odbc_row(PL_foreign_context_address(handle), row);

    case PL_CUTTED:
      free_context(PL_foreign_context_address(handle));
      return TRUE;

    default:
      assert(0);
      return FALSE;
  }
}


static foreign_t
odbc_data_sources(term_t list)
{ UCHAR dsn[SQL_MAX_DSN_LENGTH];
  UCHAR description[1024];
  SWORD dsnlen, dlen;
  UWORD dir = SQL_FETCH_FIRST;
  RETCODE rc;
  term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();
  
  if ( !henv )
    SQLAllocEnv(&henv);		/* Allocate an environment handle */

  for(;; dir=SQL_FETCH_NEXT)
  { rc = SQLDataSources(henv,
			dir,
			dsn, sizeof(dsn)-1, &dsnlen,
			description, sizeof(description)-1, &dlen);
    switch(rc)
    { case SQL_SUCCESS:
      { if ( PL_unify_list(tail, head, tail) &&
	     PL_unify_term(head, PL_FUNCTOR, FUNCTOR_data_source2,
			           PL_NCHARS, dsnlen, dsn,
			           PL_NCHARS, dlen, description) )
	  continue;

	return FALSE;
      }
      case SQL_NO_DATA_FOUND:
	return PL_unify_nil(tail);
      default:
	odbc_report(henv, NULL, NULL, rc);
        return FALSE;
    }
  }
}


		 /*******************************
		 *	COMPILE STATEMENTS	*
		 *******************************/

static int
unifyStmt(term_t id, context *ctxt)
{ return PL_unify_term(id, PL_FUNCTOR, FUNCTOR_odbc_statement1,
		             PL_POINTER, ctxt);
}


static int
getStmt(term_t id, context **ctxt)
{ if ( PL_is_functor(id, FUNCTOR_odbc_statement1) )
  { term_t a = PL_new_term_ref();
    void *ptr;

    PL_get_arg(1, id, a);
    if ( PL_get_pointer(a, &ptr) )
    { *ctxt = ptr;

      if ( (*ctxt)->magic != CTX_MAGIC )
	return existence_error(id, "odbc_statement_handle");

      return TRUE;
    }
  }

  return type_error(id, "odbc_statement_handle");
}


typedef struct
{ SWORD  type;				/* SQL_* */
  const char *text;			/* same as text */
  atom_t name;				/* Prolog name */
} sqltypedef;

static sqltypedef sqltypes[] = 
{ { SQL_BIGINT,	       "bigint" },
  { SQL_BINARY,	       "binary" },
  { SQL_BIT,	       "bit" },
  { SQL_CHAR,	       "char" },
  { SQL_DATE,	       "date" },
  { SQL_DECIMAL,       "decimal" },
  { SQL_DOUBLE,	       "double" },
  { SQL_FLOAT,	       "float" },
  { SQL_INTEGER,       "integer" },
  { SQL_LONGVARBINARY, "longvarbinary" },
  { SQL_LONGVARCHAR,   "longvarchar" },
  { SQL_NUMERIC,       "numeric" },
  { SQL_REAL,	       "real" },
  { SQL_SMALLINT,      "smallint" },
  { SQL_TIME,	       "time" },
  { SQL_TIMESTAMP,     "timestamp" },
  { SQL_TINYINT,       "tinyint" },
  { SQL_VARBINARY,     "varbinary" },
  { SQL_VARCHAR,       "varchar" },
  { 0,		       NULL }
};


static SWORD
get_sqltype_from_atom(atom_t name, SWORD *type)
{ sqltypedef *def;

  for(def=sqltypes; def->text; def++)
  { if ( !def->name )
      def->name = PL_new_atom(def->text);
    if ( def->name == name )
    { *type = def->type;
      return TRUE;
    }
  }

  return FALSE;
}

static sqltypedef pltypes[] = 
{ { SQL_PL_ATOM,       "atom" },
  { SQL_PL_STRING,     "string" },
  { SQL_PL_CODES,      "codes" },
  { SQL_PL_INTEGER,    "integer" },
  { SQL_PL_FLOAT,      "float" },
  { SQL_PL_TIME,       "time" },
  { SQL_PL_DATE,       "date" },
  { SQL_PL_TIMESTAMP,  "timestamp" },
  { 0,		       NULL }
};


static int
get_pltype(term_t t, SWORD *type)
{ atom_t name;

  if ( PL_get_atom(t, &name) )
  { sqltypedef *def;

    for(def=pltypes; def->text; def++)
    { if ( !def->name )
	def->name = PL_new_atom(def->text);

      if ( def->name == name )
      { *type = def->type;
        return TRUE;
      }
    }

    return domain_error(t, "sql_prolog_type");
  }

  return type_error(t, "atom");
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Declare parameters for prepared statements.

odbc_prepare(DSN, 'select * from product where price < ?',
	     [ integer
	     ],
	     Qid,
	     Options).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
declare_parameters(context *ctxt, term_t parms)
{ int nparams;
  term_t tail = PL_copy_term_ref(parms);
  term_t head = PL_new_term_ref();
  parameter *params;
  int pn;

  if ( (nparams=list_length(parms)) < 0 )
    return FALSE;

  ctxt->NumParams = nparams;
  if ( nparams == 0 )
    return TRUE;			/* no parameters */

  ctxt->params = malloc(sizeof(parameter)*nparams);
  memset(ctxt->params, 0, sizeof(parameter)*nparams);
  params = ctxt->params;

  for(params = ctxt->params, pn = 1;
      PL_get_list(tail, head, tail);
      params++, pn++)
  { atom_t name;
    int arity;
    SWORD sqlType, fNullable;
    UDWORD cbColDef = 0;
    SWORD plType = SQL_PL_DEFAULT;
    SQLLEN *vlenptr = NULL;		/* pointer to length */

    if ( PL_is_functor(head, FUNCTOR_gt2) )
    { term_t a = PL_new_term_ref();

      PL_get_arg(1, head, a);
      if ( !get_pltype(a, &plType) )
	return FALSE;

      PL_get_arg(2, head, head);
    }

    if ( !PL_get_name_arity(head, &name, &arity) )
      return type_error(head, "parameter_type");

    if ( name != ATOM_default )
    { int val;

      if ( !get_sqltype_from_atom(name, &sqlType) )
	return domain_error(head, "parameter_type");

					/* char(N) --> cbColDef */
      if ( get_int_arg(1, head, &val) )	/* TBD: incomplete */
	cbColDef = val;
    } else
    { TRY(ctxt, SQLDescribeParam(ctxt->hstmt,		/* hstmt */
				 (SWORD)pn,		/* ipar */
				 &sqlType,
				 &cbColDef,
				 &params->scale,
				 &fNullable));
    }

    params->sqlTypeID = sqlType;
    params->plTypeID  = plType;
    params->cTypeID   = CvtSqlToCType(params->sqlTypeID, plType);
    params->ptr_value = (SQLPOINTER)params->buf;

    switch(params->cTypeID)
    { case SQL_C_CHAR:
      case SQL_C_BINARY:
	if ( cbColDef > 0 )
	{ if ( cbColDef+1 > PARAM_BUFSIZE )
	    params->ptr_value = malloc(cbColDef+1);
	  params->length_ind = cbColDef;
	} else
	{ params->ptr_value = malloc(256);
	  params->length_ind = 255;
	}
        vlenptr = &params->len_value;
	break;
      case SQL_C_SLONG:
	params->len_value = sizeof(long);
        vlenptr = &params->len_value;
	break;
      case SQL_C_DOUBLE:
	params->len_value = sizeof(double);
        vlenptr = &params->len_value;
	break;
      case SQL_C_DATE:
	params->ptr_value = malloc(sizeof(DATE_STRUCT));
        params->len_value = sizeof(DATE_STRUCT);
	vlenptr = &params->len_value;
        break;
      case SQL_C_TIME:
	params->ptr_value = malloc(sizeof(TIME_STRUCT));
        params->len_value = sizeof(TIME_STRUCT);
	vlenptr = &params->len_value;
        break;
      case SQL_C_TIMESTAMP:
	params->ptr_value = malloc(sizeof(SQL_TIMESTAMP_STRUCT));
        params->len_value = sizeof(SQL_TIMESTAMP_STRUCT);
	vlenptr = &params->len_value;
        break;
    }


    TRY(ctxt, SQLBindParameter(ctxt->hstmt,		/* hstmt */
			       (SWORD)pn,		/* ipar */
			       SQL_PARAM_INPUT,		/* fParamType */
			       params->cTypeID,		/* fCType */
			       params->sqlTypeID,	/* fSqlType */
			       params->length_ind,	/* cbColDef */
			       params->scale,		/* ibScale */
			       params->ptr_value,	/* rgbValue */
			       0,			/* cbValueMax */
			       vlenptr));		/* pcbValue */
  }

  return TRUE;
}


static foreign_t
odbc_prepare(term_t dsn, term_t sql, term_t parms, term_t qid, term_t options)
{ connection *cn;
  context *ctxt;

  if ( !get_connection(dsn, &cn) )
    return FALSE;

  ctxt = new_context(cn);
  if ( !get_sql_text(ctxt, sql) )
  { free_context(ctxt);
    return FALSE;
  }

  TRY(ctxt, SQLPrepare(ctxt->hstmt, ctxt->sqltext, ctxt->sqllen));

  if ( !declare_parameters(ctxt, parms) )
  { free_context(ctxt);
    return FALSE;
  }

  if ( !set_statement_options(ctxt, options) )
  { free_context(ctxt);
    return FALSE;
  }

  ctxt->flags |= CTX_PERSISTENT;

  return unifyStmt(qid, ctxt);
}


static foreign_t
odbc_clone_statement(term_t qid, term_t cloneqid)
{ context *ctxt, *clone;

  if ( !getStmt(qid, &ctxt) )
    return FALSE;
  if ( !(clone = clone_context(ctxt)) )
    return FALSE;

  clone->flags |= CTX_PERSISTENT;

  return unifyStmt(cloneqid, clone);
}


static foreign_t
odbc_free_statement(term_t qid)
{ context *ctxt;

  if ( !getStmt(qid, &ctxt) )
    return FALSE;

  if ( true(ctxt, CTX_INUSE) )
    clear(ctxt, CTX_PERSISTENT);	/* oops, delay! */
  else
    free_context(ctxt);

  return TRUE;
}


static int
get_date(term_t head, DATE_STRUCT* date)
{ if ( PL_is_functor(head, FUNCTOR_date3) )
  { int v;

    if ( !get_int_arg(1, head, &v) ) return FALSE;
    date->year = v;
    if ( !get_int_arg(2, head, &v) ) return FALSE;
    date->month = v;
    if ( !get_int_arg(3, head, &v) ) return FALSE;
    date->day = v;

    return TRUE;
  }

  return FALSE;
}


static int
get_time(term_t head, TIME_STRUCT* time)
{ if ( PL_is_functor(head, FUNCTOR_time3) )
  { int v;

    if ( !get_int_arg(1, head, &v) ) return FALSE;
    time->hour = v;
    if ( !get_int_arg(2, head, &v) ) return FALSE;
    time->minute = v;
    if ( !get_int_arg(3, head, &v) ) return FALSE;
    time->second = v;

    return TRUE;
  }

  return FALSE;
}


static int
get_timestamp(term_t t, SQL_TIMESTAMP_STRUCT* stamp)
{
#if defined(HAVE_LOCALTIME) || defined(HAVE_GMTIME)
  double tf;
#endif

  if ( PL_is_functor(t, FUNCTOR_timestamp7) )
  { int v;

    if ( !get_int_arg(1, t, &v) ) return FALSE;
    stamp->year = v;
    if ( !get_int_arg(2, t, &v) ) return FALSE;
    stamp->month = v;
    if ( !get_int_arg(3, t, &v) ) return FALSE;
    stamp->day = v;
    if ( !get_int_arg(4, t, &v) ) return FALSE;
    stamp->hour = v;
    if ( !get_int_arg(5, t, &v) ) return FALSE;
    stamp->minute = v;
    if ( !get_int_arg(6, t, &v) ) return FALSE;
    stamp->second = v;
    if ( !get_int_arg(7, t, &v) ) return FALSE;
    stamp->fraction = v;

    return TRUE;
#if defined(HAVE_LOCALTIME) || defined(HAVE_GMTIME)
  } else if ( PL_get_float(t, &tf) && tf <= LONG_MAX && tf >= LONG_MIN )
  { time_t t = (time_t) tf;
    long  us = (long)((tf - (double) t) * 1000.0);
#ifdef HAVE_GMTIME
    struct tm *tm = localtime(&t);
#else
    struct tm *tm = gmtime(&t);
#endif

    stamp->year	    = tm->tm_year + 1900;
    stamp->month    = tm->tm_mon + 1;
    stamp->day	    = tm->tm_mday;
    stamp->hour	    = tm->tm_hour;
    stamp->minute   = tm->tm_min;
    stamp->second   = tm->tm_sec;
    stamp->fraction = us;

    return TRUE;
#endif
  } else
    return FALSE;
}


static int
try_null(context *ctxt, parameter *prm, term_t val, const char *expected)
{ if ( is_sql_null(val, ctxt->null) )
  { prm->len_value = SQL_NULL_DATA;

    return TRUE;
  } else
    return type_error(val, expected);
}


static int
bind_parameters(context *ctxt, term_t parms)
{ term_t tail = PL_copy_term_ref(parms);
  term_t head = PL_new_term_ref();
  parameter *prm;

  for(prm = ctxt->params; PL_get_list(tail, head, tail); prm++)
  { switch(prm->cTypeID)
    { case SQL_C_SLONG:
	if ( !PL_get_long(head, (long *)prm->ptr_value) &&
	     !try_null(ctxt, prm, head, "integer") )
	  return FALSE;
        prm->len_value = sizeof(long);
        break;
      case SQL_C_DOUBLE:
	if ( !PL_get_float(head, (double *)prm->ptr_value) &&
	     !try_null(ctxt, prm, head, "float") )
	  return FALSE;
        prm->len_value = sizeof(double);
        break;
      case SQL_C_CHAR:
      case SQL_C_BINARY:
      { unsigned int len;
	unsigned int flags = CVT_ATOM|CVT_STRING;
	const char *expected = "text";
	char *s;

					/* check for NULL */
	if ( is_sql_null(head, ctxt->null) )
	{ prm->len_value = SQL_NULL_DATA;
	  break;
	}

	switch(prm->plTypeID)
	{ case SQL_PL_DEFAULT:
	    flags = CVT_ATOM|CVT_STRING;
	    expected = "text";
	    break;
	  case SQL_PL_ATOM:
	    flags = CVT_ATOM;
	    expected = "atom";
	    break;
	  case SQL_PL_STRING:
	    flags = CVT_STRING;
	    expected = "string";
	    break;
	  case SQL_PL_CODES:
	    flags = CVT_LIST;
	    expected = "code_list";
	    break;
	  default:
	    assert(0);
	}

	if ( !PL_get_nchars(head, &len, &s, flags) )
	  return type_error(head, expected);

	if ( len > prm->length_ind )
	  return representation_error(head, "column_width");
	memcpy(prm->ptr_value, s, len+1);
	prm->len_value = len;
	break;
      }
      case SQL_C_TYPE_DATE:
      { if ( !get_date(head, (DATE_STRUCT*)prm->ptr_value) &&
	     !try_null(ctxt, prm, head, "date") )
	  return FALSE;
	prm->len_value = sizeof(DATE_STRUCT);
	break;
      }
      case SQL_C_TYPE_TIME:
      { if ( !get_time(head, (TIME_STRUCT*)prm->ptr_value) &&
	     !try_null(ctxt, prm, head, "time") )
	  return FALSE;
	prm->len_value = sizeof(TIME_STRUCT);
	break;
      }
      case SQL_C_TIMESTAMP:
      { if ( !get_timestamp(head, (SQL_TIMESTAMP_STRUCT*)prm->ptr_value) &&
	     !try_null(ctxt, prm, head, "timestamp") )
	  return FALSE;
	prm->len_value = sizeof(SQL_TIMESTAMP_STRUCT);
	break;
      }
      default:
	return PL_warning("Unknown parameter type: %d", prm->cTypeID);
    }
  }
  if ( !PL_get_nil(tail) )
    return type_error(tail, "list");

  return TRUE;
}

static foreign_t
odbc_execute(term_t qid, term_t args, term_t row, control_t handle)
{ switch( PL_foreign_control(handle) )
  { case PL_FIRST_CALL:
    { context *ctxt;

      if ( !getStmt(qid, &ctxt) )
	return FALSE;
      if ( true(ctxt, CTX_INUSE) )
      { context *clone;

	if ( !(clone = clone_context(ctxt)) )
	  return context_error(qid, "in_use", "statement");
	else
	  ctxt = clone;
      }

      if ( !bind_parameters(ctxt, args) )
	return FALSE;

      set(ctxt, CTX_INUSE);
      TRY(ctxt, SQLExecute(ctxt->hstmt));

      return odbc_row(ctxt, row);
    }

    case PL_REDO:
      return odbc_row(PL_foreign_context_address(handle), row);

    case PL_CUTTED:
      close_context(PL_foreign_context_address(handle));
      return TRUE;

    default:
      assert(0);
      return FALSE;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
$odbc_statistics/1

	NOTE: enumeration of available statistics is done in Prolog
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static functor_t FUNCTOR_statements2;	/* statements(created,freed) */

static int
unify_int_arg(int pos, term_t t, long val)
{ term_t a = PL_new_term_ref();

  if ( PL_get_arg(pos, t, a) )
    return PL_unify_integer(a, val);

  return FALSE;
}


static foreign_t
odbc_statistics(term_t what)
{ if ( !PL_is_compound(what) )
    return type_error(what, "compound");

  if ( PL_is_functor(what, FUNCTOR_statements2) )
  { if ( unify_int_arg(1, what, statistics.statements_created) &&
	 unify_int_arg(2, what, statistics.statements_freed) )
      return TRUE;
  } else
    return domain_error(what, "odbc_statistics");

  return FALSE;
}


#define MKFUNCTOR(name, arity) PL_new_functor(PL_new_atom(name), arity)
#define NDET(name, arity, func) PL_register_foreign(name, arity, func, \
						    PL_FA_NONDETERMINISTIC)
#define DET(name, arity, func) PL_register_foreign(name, arity, func, 0)

install_t
install_odbc4pl()
{  ATOM_row	      =	PL_new_atom("row");
   ATOM_informational =	PL_new_atom("informational");
   ATOM_default	      =	PL_new_atom("default");
   ATOM_once	      =	PL_new_atom("once");
   ATOM_multiple      =	PL_new_atom("multiple");
   ATOM_commit	      =	PL_new_atom("commit");
   ATOM_rollback      =	PL_new_atom("rollback");
   ATOM_atom	      =	PL_new_atom("atom");
   ATOM_string	      =	PL_new_atom("string");
   ATOM_codes	      =	PL_new_atom("codes");
   ATOM_integer	      =	PL_new_atom("integer");
   ATOM_float	      =	PL_new_atom("float");
   ATOM_time	      =	PL_new_atom("time");
   ATOM_date	      =	PL_new_atom("date");
   ATOM_timestamp     =	PL_new_atom("timestamp");
   ATOM_all_types     = PL_new_atom("all_types");
   ATOM_null          = PL_new_atom("$null$");

   FUNCTOR_timestamp7		 = MKFUNCTOR("timestamp", 7);
   FUNCTOR_time3		 = MKFUNCTOR("time", 3);
   FUNCTOR_date3		 = MKFUNCTOR("date", 3);
   FUNCTOR_odbc3		 = MKFUNCTOR("odbc", 3);
   FUNCTOR_error2		 = MKFUNCTOR("error", 2);
   FUNCTOR_type_error2		 = MKFUNCTOR("type_error", 2);
   FUNCTOR_domain_error2	 = MKFUNCTOR("domain_error", 2);
   FUNCTOR_existence_error2	 = MKFUNCTOR("existence_error", 2);
   FUNCTOR_resource_error1	 = MKFUNCTOR("resource_error", 1);
   FUNCTOR_representation_error1 = MKFUNCTOR("representation_error", 1);
   FUNCTOR_odbc_statement1	 = MKFUNCTOR("$odbc_statement", 1);
   FUNCTOR_odbc_connection1	 = MKFUNCTOR("$odbc_connection", 1);
   FUNCTOR_user1		 = MKFUNCTOR("user", 1);
   FUNCTOR_password1		 = MKFUNCTOR("password", 1);
   FUNCTOR_alias1		 = MKFUNCTOR("alias", 1);
   FUNCTOR_open1		 = MKFUNCTOR("open", 1);
   FUNCTOR_auto_commit1		 = MKFUNCTOR("auto_commit", 1);
   FUNCTOR_types1		 = MKFUNCTOR("types", 1);
   FUNCTOR_minus2		 = MKFUNCTOR("-", 2);
   FUNCTOR_gt2			 = MKFUNCTOR(">", 2);
   FUNCTOR_context_error3	 = MKFUNCTOR("context_error", 3);
   FUNCTOR_statements2		 = MKFUNCTOR("statements", 2);
   FUNCTOR_data_source2		 = MKFUNCTOR("data_source", 2);
   FUNCTOR_null1		 = MKFUNCTOR("null", 1);

   DET("odbc_connect",		   3, pl_odbc_connect);
   DET("odbc_disconnect",	   1, pl_odbc_disconnect);
   NDET("odbc_current_connection", 2, odbc_current_connection);
   DET("odbc_set_connection",	   2, odbc_set_connection);
   DET("odbc_end_transaction",	   2, odbc_end_transaction);

   DET("odbc_prepare",		   5, odbc_prepare);
   DET("odbc_clone_statement",	   2, odbc_clone_statement);
   DET("odbc_free_statement",	   1, odbc_free_statement);
   NDET("odbc_execute",		   3, odbc_execute);

   NDET("odbc_query",		   4, pl_odbc_query);
   NDET("odbc_tables",	           2, odbc_tables);
   NDET("odbc_column",		   3, pl_odbc_column);
   NDET("odbc_types",		   3, odbc_types);
   DET("odbc_data_sources",	   1, odbc_data_sources);

   DET("$odbc_statistics",	   1, odbc_statistics);
}


install_t
uninstall_odbc()			/* TBD: make sure the library is */
{ if ( henv )				/* not in use! */
  { SQLFreeEnv(henv);
    henv = NULL;
  }
}


		 /*******************************
		 *	      TYPES		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
MS SQL Server seems to store the   dictionary  in UNICODE, returning the
types SQL_WCHAR, etc. As current SWI-Prolog   doesn't do unicode, we ask
them as normal SQL_C_CHAR, assuming the   driver  will convert this. One
day this should become SQL_C_WCHAR
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static SWORD
CvtSqlToCType(SQLSMALLINT fSqlType, SQLSMALLINT plTypeID)
{ switch(plTypeID)
  { case SQL_PL_DEFAULT:
      switch (fSqlType)
      { case SQL_CHAR:
	case SQL_VARCHAR:
	case SQL_LONGVARCHAR:
#ifdef SQL_WCHAR
	case SQL_WCHAR:			/* see note above */
	case SQL_WVARCHAR:
	case SQL_WLONGVARCHAR:
#endif
	  return SQL_C_CHAR;
    
	case SQL_DECIMAL:
	case SQL_NUMERIC:
	  return SQL_C_CHAR;
  
	case SQL_REAL:
	case SQL_FLOAT:
	case SQL_DOUBLE:
	  return SQL_C_DOUBLE;
       
	case SQL_BIT:
	case SQL_TINYINT:
	case SQL_SMALLINT:
	case SQL_INTEGER:
	  return SQL_C_SLONG;
    
	case SQL_TYPE_DATE:
	  return SQL_C_TYPE_DATE;
	case SQL_TYPE_TIME:
	  return SQL_C_TYPE_TIME;
	case SQL_TIMESTAMP:
	  return SQL_C_TIMESTAMP;
	default:
	  PL_warning("fSqlType %d not supported", fSqlType);
	  return CVNERR;  
      }
    case SQL_PL_ATOM:
    case SQL_PL_STRING:
    case SQL_PL_CODES:
      return SQL_C_CHAR;
    case SQL_PL_INTEGER:
      switch(fSqlType)
      { case SQL_TIMESTAMP:
	  return SQL_C_TIMESTAMP;
	default:
	  return SQL_C_SLONG;
      }
    case SQL_PL_FLOAT:
      switch(fSqlType)
      { case SQL_TIMESTAMP:
	  return SQL_C_TIMESTAMP;
	default:
	  return SQL_C_DOUBLE;
      }
    case SQL_PL_DATE:
      return SQL_C_TYPE_DATE;
    case SQL_PL_TIME:
      return SQL_C_TYPE_TIME;
    case SQL_PL_TIMESTAMP:
      return SQL_C_TIMESTAMP;
    default:
      assert(0);
      return CVNERR;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Store a row
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
pl_put_row(term_t row, context *c)
{ term_t columns = PL_new_term_refs(c->NumCols);
  parameter *p;
  SQLSMALLINT i;
   
  for (i=0, p=c->result; i<c->NumCols; i++, p++)
  { if ( p->length_ind == SQL_NULL_DATA )
    { put_sql_null(columns+i, c->null);
    } else
    { switch( p->cTypeID )
      { case SQL_C_CHAR:
	case SQL_C_BINARY:
	  switch( p->plTypeID )
	  { case SQL_PL_DEFAULT:
	    case SQL_PL_ATOM:
	      PL_put_atom_nchars(columns+i,
				 p->length_ind,
				 (SQLCHAR *)p->ptr_value);
	      break;
	    case SQL_PL_STRING:
	      PL_put_string_nchars(columns+i,
				   p->length_ind,
				   (SQLCHAR *)p->ptr_value);
	      break;
	    case SQL_PL_CODES:
	      PL_put_list_ncodes(columns,
				 p->length_ind,
				 (SQLCHAR *)p->ptr_value);
	      break;
	    default:
	      assert(0);
	  }
	  break;
	case SQL_C_SLONG:
	  PL_put_integer(columns+i,*(SQLINTEGER *)p->ptr_value);
	  break;
	case SQL_C_DOUBLE:
	  PL_put_float(columns+i,*(SQLDOUBLE *)p->ptr_value);
	  break;
	case SQL_C_TYPE_DATE:
	{ DATE_STRUCT* ds = (DATE_STRUCT*)p->ptr_value;
	  term_t av = PL_new_term_refs(3);

	  PL_put_integer(av+0, ds->year);
	  PL_put_integer(av+1, ds->month);
	  PL_put_integer(av+2, ds->day);
	  
	  PL_cons_functor_v(columns+i, FUNCTOR_date3, av);
	  break;
	}
	case SQL_C_TYPE_TIME:
	{ TIME_STRUCT* ts = (TIME_STRUCT*)p->ptr_value;
	  term_t av = PL_new_term_refs(3);

	  PL_put_integer(av+0, ts->hour);
	  PL_put_integer(av+1, ts->minute);
	  PL_put_integer(av+2, ts->second);
	  
	  PL_cons_functor_v(columns+i, FUNCTOR_time3, av);
	  break;
	}
	case SQL_C_TIMESTAMP: 
	{ SQL_TIMESTAMP_STRUCT* ts = (SQL_TIMESTAMP_STRUCT*)p->ptr_value;

	  switch( p->plTypeID )
	  { case SQL_PL_DEFAULT:
	    case SQL_PL_TIMESTAMP:
	    { term_t av = PL_new_term_refs(7);

	      PL_put_integer(av+0, ts->year);
	      PL_put_integer(av+1, ts->month);
	      PL_put_integer(av+2, ts->day);
	      PL_put_integer(av+3, ts->hour);
	      PL_put_integer(av+4, ts->minute);
	      PL_put_integer(av+5, ts->second);
	      PL_put_integer(av+6, ts->fraction);
	  
	      PL_cons_functor_v(columns+i, FUNCTOR_timestamp7, av);
	      break;
	    }
	    case SQL_PL_INTEGER:
	    case SQL_PL_FLOAT:
#ifdef HAVE_MKTIME
	    { struct tm tm;
	      time_t time;

	      memset(&tm, 0, sizeof(tm));
	      tm.tm_year  = ts->year - 1900;
	      tm.tm_mon   = ts->month-1;
	      tm.tm_mday  = ts->day;
	      tm.tm_hour  = ts->hour;
	      tm.tm_min   = ts->minute;
	      tm.tm_sec   = ts->second;
	      tm.tm_isdst = -1;		/* do not know */

	      time = mktime(&tm);

	      if ( p->plTypeID == SQL_PL_INTEGER )
		PL_put_integer(columns+1, time);
	      else
		PL_put_float(columns+1, (double)time); /* TBD: fraction */
	    }
#else
	      return PL_warning("System doesn't support mktime()");
#endif
	      break;
	    default:
	      assert(0);
	  }
	  break;
	}
	default:
	  return PL_warning("ODBC: Unknown cTypeID: %d",
			    p->cTypeID);
      }
    }
  }

  PL_cons_functor_v(row, c->db_row, columns);

  return TRUE;
}
