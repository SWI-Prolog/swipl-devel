/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2010, VU University Amsterdam

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This include file emulates <sicstus.h> for SWI-Prolog.

This  version  was   written   to   get    the   Alpino   parser   suite
(http://www.let.rug.nl/vannoord/alp/Alpino/) to run on SWI-Prolog. It is
by no means complete and intended  as   a  `living document'. So, please
contribute   your   changes.    See     also    library(qpforeign)   and
library(dialect/sicstus).

Most should be(come) fully compatible. Some  issues are hard to emulate.
Please checks the notes for:

	* SP_to_os()
	* SP_from_os()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef SICSTUS_H_INCLUDED
#define SICSTUS_H_INCLUDED
#include <SWI-Prolog.h>
#include <assert.h>

typedef term_t SP_term_ref;
typedef atom_t SP_atom;
typedef predicate_t SP_pred_ref;

#define SP_ERROR  -1
#define SP_FAILURE 0
#define SP_SUCCESS 1

#define SP_WHEN_RESTORE 1		/* Note: these are not supported yet */
#define SP_WHEN_SAVE 2

#define SP_TYPE_ATOM     PL_ATOM
#define SP_TYPE_COMPOUND PL_TERM
#define SP_TYPE_FLOAT    PL_FLOAT
#define SP_TYPE_INTEGER  PL_INTEGER
#define SP_TYPE_VARIABLE PL_VARIABLE


		 /*******************************
		 *  READING AND WRITING TERMS	*
		 *******************************/

#define REP_SP PL_cvt_encoding()

#define SP_new_term_ref() PL_new_nil_ref()

#define SP_is_list(t) PL_is_list(t)

#define SP_cons_list(l,h,t) PL_cons_list(l,h,t)

#define SP_atom_from_string(s) PL_new_atom(s)
#define SP_string_from_atom(a) PL_atom_chars(a)

#define SP_put_variable(t) PL_put_variable(t)
#define SP_put_atom(t,a) PL_put_atom(t,a)
#define SP_put_integer(t,i) PL_put_integer(t,i)
#define SP_put_float(t,f) PL_put_float(t,f)
#define SP_put_list(t) PL_put_list(t)
#define SP_put_term(t1,t2) PL_put_term(t1,t2)

#define SP_get_float(t,f) PL_get_float(t,f)
#define SP_get_functor(t,n,a) PL_get_name_arity(t,n,a)
#define SP_get_list(l,h,t) PL_get_list(l,h,t)
#define SP_get_string(t,s) PL_get_chars(t,(char**)(s),CVT_ATOM|REP_SP)
#define SP_get_integer(t,pi) PL_get_long(t, pi)
#define SP_get_arg(i,t,a) PL_get_arg(i,t,a)

#define SP_unify(x,y) PL_unify(x,y)
#define SP_term_type(t) PL_term_type(t)

static __inline int
SP_put_string(term_t t, const char *s)
{ PL_put_variable(t);

  return PL_unify_chars(t, PL_ATOM|REP_SP, (size_t)-1, s);
}


static __inline int
SP_put_list_n_bytes(SP_term_ref list, SP_term_ref tail,
		    size_t len, unsigned char const *buf)
{ term_t t0 = PL_new_term_refs(2);
  int rc = PL_unify_chars(t0, PL_CODE_LIST|PL_DIFF_LIST|REP_ISO_LATIN_1,
			  len, (const char*)buf);

  if ( rc )
  { PL_put_term(list, t0);
    rc = PL_unify(tail, t0+1);
  }

  return rc;
}


static __inline int
SP_put_list_chars(SP_term_ref list, SP_term_ref tail, const char *s)
{ term_t t0 = PL_new_term_refs(2);
  int rc = PL_unify_chars(t0, PL_CODE_LIST|PL_DIFF_LIST|REP_SP,
			  (size_t)-1, s);

  if ( rc )
  { PL_put_term(list, t0);
    rc = PL_unify(tail, t0+1);
  }

  return rc;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Copies into the byte array s the  initial elements of term, which should
hold a list of integers in the range   [0,255],  so that at most n bytes
are used. The number of bytes actually   written is assigned to *w. tail
is set to the remainder of the list. The   array s must have room for at
least n bytes.

TBD: This implementation is a bit slow, but   it should do the trick for
now.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static __inline int
SP_get_list_n_bytes(SP_term_ref term,
		    SP_term_ref tail,
		    size_t n,
		    size_t *w,
		    unsigned char *s)
{ size_t written = 0;
  term_t head = PL_new_term_ref();

  PL_put_term(tail, term);
  while( written < n && PL_get_list(tail, head, tail) )
  { int i;

    if ( PL_get_integer(head, &i) && i >= 0 && i <= 255 )
    { s[written++] = i;
    } else
    { *w = written;
      return SP_ERROR;			/* Is this ok? */
    }
  }

  *w = written;
  return SP_SUCCESS;
}


static __inline int
SP_get_number_codes(SP_term_ref term, char const **s)
{ char *tmp;

  if ( PL_get_chars(term, &tmp, CVT_NUMBER) )
  { *s = (const char*)tmp;
    return TRUE;
  }

  return FALSE;
}


static __inline int
SP_put_number_codes(SP_term_ref term, char const *s)
{ term_t t = PL_new_term_ref();

  if ( PL_chars_to_term(s, t) &&
       PL_is_number(t) )
  { PL_put_term(term, t);
    return TRUE;
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If native is zero, buf  consists  of   the  buf_size  bytes  of the twos
complement representation of the integer. Less  significant bytes are at
lower indices
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static __inline int
SP_put_integer_bytes(SP_term_ref term,
		     void *buf, size_t buf_size,
		     int native)
{ if ( native )
  { int64_t val;

    switch(buf_size)
    { case 1:
	{ char *p = (char*)buf;
	val = *p;
	break;
      }
      case 2:
	{ short *p = (short*)buf;
	val = *p;
	break;
      }
      case 4:
	{ int *p = (int*)buf;
	val = *p;
	break;
      }
      case 8:
	{ int64_t *p = (int64_t*)buf;
	val = *p;
	break;
      }
    default:
      return FALSE;
    }

    return PL_put_int64(term, val);
  } else					/* see above */
  {
#ifdef __GNU_MP__
    mpz_t mpz;
    int rc;

    mpz_init(mpz);
    mpz_import(mpz,
	       buf_size,			/* COUNT */
	       1,				/* ORDER */
	       1,				/* SIZE */
	       0,				/* ENDIAN (native) */
	       0,				/* NAILS */
	       buf);				/* OP */
    PL_put_variable(term);
    rc = PL_unify_mpz(term, mpz);
    mpz_clear(mpz);

    return rc;
#else
    assert(0);
#endif
  }
}



static __inline int
SP_cons_functor_array(SP_term_ref term, SP_atom name, int arity,
		      SP_term_ref *arg)
{ functor_t f = PL_new_functor(name, arity);
  term_t argv;

  if ( (argv=PL_new_term_refs(arity)) )
  { int i;

    for(i=0; i<arity; i++)
      PL_put_term(argv+i, arg[i]);

    return PL_cons_functor_v(term, f, argv);
  }

  return FALSE;
}



		 /*******************************
		 * RETURN CODES AND EXCEPTIONS	*
		 *******************************/

#define SP_raise_exception(t) do { PL_raise_exception(t); \
				   SP_set_state(SP_ERROR); \
				 } while(0)
#define SP_fail()	      do { SP_set_state(SP_FAILURE); \
				 } while(0)
#define SP_WRAP_INIT() \
	SP_set_state(SP_SUCCESS)
#define SP_WRAP_CHECK_STATE() \
	if ( SP_get_state() != SP_SUCCESS ) \
	  return FALSE


		 /*******************************
		 *	 C CALLING PROLOG	*
		 *******************************/

#define SP_predicate(name,arity,module) PL_predicate(name,arity,module)

static __inline int
SP_query(SP_pred_ref predicate, ...)
{ atom_t name;
  int i, arity;
  module_t module;
  fid_t fid;
  qid_t qid;
  term_t t0;
  va_list args;

  if ( !(fid = PL_open_foreign_frame()) )
    return SP_ERROR;

  PL_predicate_info(predicate, &name, &arity, &module);

  if ( !(t0 = PL_new_term_refs(arity)) )
  { PL_close_foreign_frame(fid);
    return SP_ERROR;
  }

  va_start(args, predicate);
  for(i=0; i<arity; i++)
  { term_t a = va_arg(args, term_t);
    PL_put_term(t0+i, a);
  }
  va_end(args);

  if ( !(qid=PL_open_query(NULL, PL_Q_CATCH_EXCEPTION, predicate, t0)) )
    return SP_ERROR;
  if ( !PL_next_solution(qid) )
  { term_t ex = PL_exception(qid);

    PL_cut_query(qid);
    PL_close_foreign_frame(fid);
    if ( ex )
      return SP_ERROR;
    return SP_FAILURE;
  }
  PL_cut_query(qid);
  PL_close_foreign_frame(fid);

  return SP_SUCCESS;
}


static __inline int
SP_query_cut_fail(SP_pred_ref predicate, ...)
{ atom_t name;
  int i, arity;
  module_t module;
  fid_t fid;
  qid_t qid;
  term_t t0;
  va_list args;

  if ( !(fid = PL_open_foreign_frame()) )
    return SP_ERROR;

  PL_predicate_info(predicate, &name, &arity, &module);

  if ( !(t0 = PL_new_term_refs(arity)) )
  { PL_close_foreign_frame(fid);
    return SP_ERROR;
  }

  va_start(args, predicate);
  for(i=0; i<arity; i++)
  { term_t a = va_arg(args, term_t);
    PL_put_term(t0+i, a);
  }
  va_end(args);

  if ( !(qid=PL_open_query(NULL, PL_Q_CATCH_EXCEPTION, predicate, t0)) )
    return SP_ERROR;
  if ( !PL_next_solution(qid) )
  { term_t ex = PL_exception(qid);

    PL_cut_query(qid);
    if ( ex )
    { PL_close_foreign_frame(fid);
      return SP_ERROR;
    }
    PL_discard_foreign_frame(fid);
    return SP_FAILURE;
  }
  PL_cut_query(qid);
  PL_discard_foreign_frame(fid);

  return SP_SUCCESS;
}


#define SP_malloc(n) PL_malloc(n)
#define SP_realloc(p,n) PL_realloc(p,n)
#define SP_free(p) PL_free(p)


/* These functions perform string-encoding conversion between Prolog and
   the OS-native representation.  This is done using the conversion-flag
   in SWI-Prolog's PL_get_chars() and PL_unify_chars() routines. I'm not
   sure how we should deal with this.
*/

#define SP_to_os(s,c) (s)
#define SP_from_os(s,c) (s)

#endif /*SICSTUS_H_INCLUDED*/
