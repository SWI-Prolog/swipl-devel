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

#include <SWI-Stream.h>			/* for printing messages */
#include <SWI-Prolog.h>
#include <codqlhdr.h>
#include "error.h"
#include <assert.h>

static functor_t FUNCTOR_odb1;
static functor_t FUNCTOR_decimal3;
static functor_t FUNCTOR_odb_object1;
static atom_t ATOM_tuple;
static atom_t ATOM_true;
static atom_t ATOM_false;

#define mkfunc(n, a) PL_new_functor(PL_new_atom(n), a)

static void
initConstants()
{ FUNCTOR_odb1 = mkfunc("odb", 1);
  FUNCTOR_decimal3 = mkfunc("decimal", 3);
  FUNCTOR_odb_object1 = mkfunc("odb_object", 1);
  ATOM_tuple = PL_new_atom("tuple");
  ATOM_true  = PL_new_atom("true");
  ATOM_false = PL_new_atom("false");
}


static int
unify_session(term_t t, odbSessH h)
{ return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_odb1, PL_POINTER, h);
}


static int
get_session(term_t t, odbSessH *h)
{ if ( PL_is_functor(t, FUNCTOR_odb1) )
  { term_t a = PL_new_term_ref();
    void *ptr;

    PL_get_arg(1, t, a);
    if ( PL_get_pointer(a, &ptr) )
    { *h = ptr;
      return TRUE;
    }
  }

  return pl_error(ERR_TYPE, "odb_handle", t);
}

static int
get_chars_or_null_ex(term_t t, char **s)
{ if ( PL_get_atom_chars(t, s) )
    return TRUE;
  if ( PL_is_variable(t) )
  { *s = NULL;
    return TRUE;
  }
  
  return pl_error(ERR_TYPE, "atom_or_variable", t);
}


static int
get_chars_ex(term_t t, char **s)
{ if ( PL_get_chars(t, s, CVT_ATOMIC) )
    return TRUE;
  if ( PL_is_variable(t) )
  { *s = NULL;
    return TRUE;
  }
  
  return pl_error(ERR_TYPE, "atom_or_variable", t);
}


static int
report_status(odbSessH sh, odbStatus status)
{ if ( status == ODB_NORMAL )
    return TRUE;
  else
  { char msg[1000];
    int len;
    long code;

    odbGetError(sh, &code, msg, sizeof(msg), &len);
    return pl_error(ERR_PACKAGE_INT, "jasmine", status, "%s", msg);
  }
}


		 /*******************************
		 *	     SESSION		*
		 *******************************/

static foreign_t
odb_ses_start(term_t h, term_t node, term_t user, term_t pwd, term_t env)
{ char *n, *u, *p, *e;
  odbSessH sh;
  odbStatus rval;

  if ( !get_chars_or_null_ex(node, &n) ||
       !get_chars_or_null_ex(user, &u) ||
       !get_chars_or_null_ex(pwd, &p) ||
       !get_chars_or_null_ex(env, &e) )
    return FALSE;
  
  if ( (rval=odbSesStart(&sh, n, u, p, e)) != ODB_NORMAL )
    return report_status(sh, rval);
  
  return unify_session(h, sh);  
}


static foreign_t
odb_ses_end(term_t h)
{ odbSessH sh;

  if ( !get_session(h, &sh) )
    return FALSE;
  
  odbSesEnd(sh);
  return TRUE;
}

		 /*******************************
		 *	       ODQL		*
		 *******************************/

static foreign_t
odb_exec_odql(term_t h, term_t command)
{ odbSessH sh;
  char *cmd;

  if ( !get_session(h, &sh) )
    return FALSE;
  if ( !get_chars_ex(command, &cmd) )
    return FALSE;

  return report_status(sh, odbExecODQL(sh, cmd, 0, NULL));
}

		 /*******************************
		 *	       VALUES		*
		 *******************************/

#include <time.h>			/* struct tm */

#ifndef HAVE_TIMELOCAL
#define MINUTE	60
#define HOUR	(60 * MINUTE)
#define DAY	(24 * HOUR)

#define leapYear(y)	 ((y % 4) && (!(y % 100) || y % 400))
static int monthsize[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

static time_t
timelocal(struct tm *tm)
{ time_t sec;
  int n;

  sec  = tm->tm_sec;
  sec += tm->tm_min * MINUTE;
  sec += (tm->tm_hour - 1) * HOUR;		/* why - 1 ?? */
  sec += (tm->tm_mday - 1)  * DAY;
  for( n=0; n<tm->tm_mon; n++ )
    sec += (n == 1 && leapYear(tm->tm_year) ? 29 : monthsize[n]) * DAY;
  for( n=70; n < tm->tm_year; n++ )
    sec += (leapYear(n) ? 365 : 366) * DAY;

  return sec;
}
#endif /*HAVE_TIMELOCAL*/

static int
unify_value(term_t t, odbData data)
{ switch(odbGetType(data))
  { case ODBBOOL:
      if ( odbGetBool(data) )
	return PL_unify_atom(t, ATOM_true);
      else
	return PL_unify_atom(t, ATOM_false);
    case ODBBSEQ:
    { char *bytes = odbGetBseq(data);
      long len = odbGetDataLen(data);

      return PL_unify_atom_nchars(t, len, bytes);
    }
    case ODBDATE:
    { struct tm *tm = odbGetDate(data);
      time_t posix = timelocal(tm);

      return PL_unify_float(t, (double)posix);
    }
    case ODBDEC:
    { char *dec = odbGetDec(data);
      int scale = odbGetDecScale(data);
      int prec  = odbGetDecPrecision(data);

      return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_decimal3,
			   PL_CHARS, dec,
			   PL_INT, prec,
			   PL_INT, scale);
    }
    case ODBINT:
      return PL_unify_integer(t, odbGetInt(data));
    case ODBNIL:
      return PL_unify_nil(t);
    case ODBOID:
      return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_odb_object1,
			   PL_CHARS, odbGetOid(data));
    case ODBREAL:
      return PL_unify_float(t, odbGetReal(data));
    case ODBSTR:
      return PL_unify_atom_chars(t, odbGetStr(data));
    case ODBTPL:
    { long fields = odbNoOfFields(data);
      odbData elem;
      term_t tuple = PL_new_term_ref();
      term_t av = PL_new_term_refs(fields);
      long i;

      for(i=0; i<fields; i++)
      { elem = odbFieldAt(data, i);
	unify_value(av+i, elem);
      }

      PL_cons_functor_v(tuple, PL_new_functor(ATOM_tuple, fields), av);
      return PL_unify(t, tuple);
    }
    default:
      assert(0);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
put_value(term_t value, odbData data)
    Store a Prolog value into an ODB data handle.  The current value is in
    data and tells us the requested type.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
put_value(odbSessH sh, term_t value, odbData data)
{ switch(odbGetType(data))
  { case ODBBOOL:
    { atom_t a;
      int v;

      if ( PL_get_atom(value, &a) )
      { if ( a == ATOM_true )
	  v = TRUE;
	else if ( a == ATOM_false )
	  v = FALSE;
	else
	  goto nobool;

	return report_status(sh, odbSetBool(data, v));
      }
    nobool:
      return pl_error(ERR_TYPE, "bool", value);
    }
    case ODBBSEQ:
    { char *v;
      int len;

      if ( PL_get_nchars(value, &len, &v, CVT_ALL) )
	return report_status(sh, odbSetBseq(data, v, (long)len));
      return pl_error(ERR_TYPE, "bseq", value);
    }
    case ODBDATE:
    { struct tm *tm;
      double t;
      time_t tt;

      if ( !PL_get_float(value, &t) )
	return pl_error(ERR_TYPE, "time", value);
      tt = (time_t)t;
      tm = localtime(&tt);
      
      return report_status(sh, odbSetDate(data, tm));
    }
    case ODBDEC:
    { char *v;
      int scale;
      int prec;

      if ( PL_is_functor(value, FUNCTOR_decimal3) )
      { term_t a = PL_new_term_ref();

	if ( PL_get_arg(1, value, a) &&
	     PL_get_chars(a, &v, CVT_ATOMIC) &&
	     PL_get_arg(2, value, a) &&
	     PL_get_integer(a, &prec) &&
	     PL_get_arg(3, value, a) &&
	     PL_get_integer(a, &scale) )
	  return report_status(sh, odbSetDec(data, v, prec, scale));
      }

      return pl_error(ERR_TYPE, "decimal", value);
    }
    case ODBINT:
    { long v;

      if ( !PL_get_long(value, &v) )
	return pl_error(ERR_TYPE, "integer", value);

      return report_status(sh, odbSetInt(data, v));
    }
    case ODBNIL:
    { long int_v;
      double float_v;
      char *str_v;

      if ( PL_is_functor(value, FUNCTOR_odb_object1) )
	goto oid;
      if ( PL_get_long(value, &int_v) )
	return report_status(sh, odbSetInt(data, int_v));
      if ( PL_get_float(value, &float_v) )
	return report_status(sh, odbSetReal(data, float_v));
      if ( PL_get_chars(value, &str_v, CVT_ALL) )
	return report_status(sh, odbSetStr(data, str_v));

      return pl_error(ERR_TYPE, "odb value", value);
    }
    case ODBOID:
    { if ( PL_is_functor(value, FUNCTOR_odb_object1) )
      { term_t a; 
	char *oid;

      oid:
	a = PL_new_term_ref();
        PL_get_arg(1, value, a);
	if ( PL_get_atom_chars(a, &oid) )
	  return report_status(sh, odbSetOid(data, oid));
      }
      return pl_error(ERR_TYPE, "oid", value);
    }
    case ODBREAL:
    { double v;

      if ( !PL_get_float(value, &v) )
	return pl_error(ERR_TYPE, "float", value);
      return report_status(sh, odbSetReal(data, v));
    }
    case ODBSTR:
    { char *v;

      if ( !PL_get_chars(value, &v, CVT_ALL) )
	return pl_error(ERR_TYPE, "text", value);
      return report_status(sh, odbSetStr(data, v));
    }
    case ODBTPL:
      return PL_warning("Cannot set var to a tuple");
    default:
      assert(0);
  }
}

		 /*******************************
		 *	    VARIABLES		*
		 *******************************/

static foreign_t
odb_get_var(term_t h, term_t name, term_t value)
{ odbSessH sh;
  char *vname;
  odbStatus rval;
  odbData data;

  if ( !get_session(h, &sh) )
    return FALSE;
  if ( !get_chars_ex(name, &vname) )
    return FALSE;

  data = odbAllocData();
  if ( (rval=odbGetVar(sh, vname, data)) == ODB_NORMAL )
  { int r = unify_value(value, data);
    odbFreeData(data);
    
    return r;
  }

  odbFreeData(data);			/* does this clobber error status? */
  return report_status(sh, rval);
}


static foreign_t
odb_set_var(term_t h, term_t name, term_t value)
{ odbSessH sh;
  char *vname;
  odbStatus rval;
  odbData data;

  if ( !get_session(h, &sh) )
    return FALSE;
  if ( !get_chars_ex(name, &vname) )
    return FALSE;
  
  data = odbAllocData();
  if ( (rval=odbGetVar(sh, vname, data)) != ODB_NORMAL )
    return report_status(sh, rval);
  if ( !put_value(sh, value, data) )
    return FALSE;
  if ( (rval=odbSetVar(sh, vname, data)) == ODB_NORMAL )
  { odbFreeData(data);
    return TRUE;
  }

  odbFreeData(data);			/* does this clobber error status? */
  return report_status(sh, rval);
}


		 /*******************************
		 *	   COLLECTIONS		*
		 *******************************/

static foreign_t
odb_collection_to_list(term_t h, term_t id, term_t list)
{ odbSessH sh;
  odbStatus odbrc;
  odbScanId scanId;
  char *var;

  if ( !get_session(h, &sh) )
    return FALSE;
  if ( PL_get_atom_chars(id, &var) )
  { odbrc = odbScanOpen(sh, var, &scanId, 0);
    goto ok;
  } else if ( PL_is_functor(id, FUNCTOR_odb_object1) )
  { term_t a = PL_new_term_ref();
    char *oid;

    PL_get_arg(1, id, a);
    if ( PL_get_atom_chars(a, &oid) )
    { odbData obj = odbAllocData();
      odbSetOid(obj, oid);
      odbrc = odbScanOpenOnOID(sh, obj, &scanId, 0);
      odbFreeData(obj);			/* can this already be done!? */
      goto ok;
    }
  }
  return pl_error(ERR_TYPE, "var_or_obj", id);

ok:
  if ( odbrc != ODB_NORMAL )
    return report_status(sh, odbrc);	/* not really */

  { term_t tail = PL_copy_term_ref(list);
    term_t head = PL_new_term_ref();
    odbData e = odbAllocData();
    int rval = FALSE;
  
    while( odbScanNext(sh, scanId, e) == ODB_NORMAL )
    { if ( !PL_unify_list(tail, head, tail) ||
	   !unify_value(head, e) )
	goto out;
    }
    if ( PL_unify_nil(tail) )
      rval = TRUE;
  
  out:
    odbFreeData(e);
    odbScanClose(sh, scanId);
    return rval;
  }
}

		 /*******************************
		 *	   INSTALLATION		*
		 *******************************/


install_t install()
{ initConstants();

  PL_register_foreign("odb_ses_start", 5, odb_ses_start, 0);
  PL_register_foreign("odb_ses_end",   1, odb_ses_end,   0);
  PL_register_foreign("odb_exec_odql", 2, odb_exec_odql, 0);
  PL_register_foreign("odb_get_var",   3, odb_get_var,   0);
  PL_register_foreign("odb_set_var",   3, odb_set_var,   0);
  PL_register_foreign("odb_collection_to_list", 3, odb_collection_to_list, 0);

  odbPrcInit();
}

install_t uninstall()
{ odbPrcExit();
}
