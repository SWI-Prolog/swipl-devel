/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2024, University of Amsterdam
			      VU University Amsterdam
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

#include "../pl-incl.h"
#include "../pl-dict.h"
#include "../pl-fli.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Option list (or dict) processing.  See PL_scan_options() for details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAXOPTIONS 64
#define HAS_OPT_MODE(f,m) (((f)&OPT_UNKNOWN_MASK) == (m))

typedef union
{ int      *b;				/* boolean value */
  int      *i;				/* integer value */
  int64_t  *i64;			/* 64 bit integer */
  uint64_t *ui64;			/* 64 bit unsigned integer */
  size_t   *sz;				/* size_t value */
  double   *f;				/* double value */
  char    **s;				/* string value */
  atom_t   *a;				/* atom value */
  term_t   *t;				/* term-reference */
  void	   *ptr;			/* anonymous pointer */
} optvalue;


#define get_optval(valp, spec, val) LDFUNC(get_optval, valp, spec, val)
static int
get_optval(DECL_LD optvalue valp, const PL_option_t *spec, term_t val)
{ switch((spec->type & OPT_TYPE_MASK))
  { case OPT_BOOL:
    { int bval;

      if ( !PL_get_bool_ex(val, &bval) )
	return false;
      *valp.b = bval;

      return true;
    }
    case OPT_INT:
    { if ( !PL_get_integer_ex(val, valp.i) )
	return false;

      return true;
    }
    case OPT_INT64:
    { if ( (spec->type & OPT_INF) && PL_is_inf(val) )
	*valp.i64 = INT64_MAX;
      else if ( !PL_get_int64_ex(val, valp.i64) )
	return false;

      return true;
    }
    case OPT_UINT64:
    { if ( (spec->type & OPT_INF) && PL_is_inf(val) )
	*valp.ui64 = (uint64_t)-1;
      if ( !PL_get_uint64_ex(val, valp.ui64) )
	return false;

      return true;
    }
    case OPT_SIZE:
    { if ( (spec->type & OPT_INF) && PL_is_inf(val) )
	*valp.sz = (size_t)-1;
      else if ( !PL_get_size_ex(val, valp.sz) )
	return false;

      return true;
    }
    case OPT_DOUBLE:
    { if ( !PL_get_float_ex(val, valp.f) )
	return false;

      return true;
    }
    case OPT_STRING:
    { char *str;

      if ( !PL_get_chars(val, &str, CVT_ALL|REP_UTF8|BUF_STACK|CVT_EXCEPTION) )
	return false;
      *valp.s = str;

      return true;
    }
    case OPT_ATOM:
    { atom_t a;

      if ( !PL_get_atom_ex(val, &a) )
	return false;
      *valp.a = a;

      return true;
    }
#ifdef O_LOCALE
    case OPT_LOCALE:
    { PL_locale *l;
      PL_locale **lp = valp.ptr;

      if ( !getLocaleEx(val, &l) )
	return false;
      *lp = l;

      return true;
    }
#endif
    case OPT_TERM:
    { *valp.t = PL_copy_term_ref(val); /* can't reuse anymore */

      return true;
    }
    default:
      assert(0);
  }

  return false;
}


typedef struct dictopt_ctx
{ const PL_option_t    *specs;		/* specifications */
  optvalue	       *values;		/* value pointers */
  const char           *opttype;
  int			flags;
} dictopt_ctx;

#define dict_option(key, value, last, closure) \
	LDFUNC(dict_option, key, value, last, closure)

static int
dict_option(DECL_LD term_t key, term_t value, int last, void *closure)
{ dictopt_ctx *ctx = closure;
  atom_t name;
  int n;
  const PL_option_t *s;

  if ( !PL_get_atom_ex(key, &name) )
    return -1;

  for( n=0, s = ctx->specs; s->name; n++, s++ )
  { if ( s->name == name )
    { if ( !get_optval(ctx->values[n], s, value) )
	return -1;
      return 0;
    }
  }

  if ( !HAS_OPT_MODE(ctx->flags, OPT_UNKNOWN_IGNORE) )
  { term_t kv;

    if ( !((kv=PL_new_term_ref()) &&
	   PL_cons_functor(kv, FUNCTOR_colon2, key, value)) )
      return -1;

    if ( HAS_OPT_MODE(ctx->flags, OPT_UNKNOWN_ERROR) )
    { if ( !PL_domain_error(ctx->opttype, kv) )
	return -1;
      return -1;
    }
    if ( !printMessage(ATOM_warning,
			 PL_FUNCTOR, FUNCTOR_unknown_option2,
			   PL_CHARS, ctx->opttype,
			   PL_TERM, kv) )
      return -1;
  }

  return 0;				/* unprocessed key */
}


/* Process options from a dict.  Note that this can be more efficient by
   sorting the option specification, after which we can perform a linear
   scan.  I think that the best way to do that is to associate the
   option specification with a writeable structure that is lazily
   initialized to an array of PL_option_t pointers using the ordering of
   dicts.

   An alternative is to process the opt-specs in order and use the
   dicts binary search to find the values (or not).  As far as I know,
   option processing of built-ins is not a bottleneck, so there is no
   need to worry right now.
*/

#define dict_options(dict, flags, opttype, specs, values) \
	LDFUNC(dict_options, dict, flags, opttype, specs, values)

static int
dict_options(DECL_LD term_t dict, int flags, const char *opttype,
	     const PL_option_t *specs, optvalue *values)
{ dictopt_ctx ctx;

  ctx.specs   = specs;
  ctx.values  = values;
  ctx.flags   = flags;
  ctx.opttype = opttype;

  return _PL_for_dict(dict, dict_option, &ctx, 0) == 0;
}

#define vscan_options(list, flags, name, specs, args) \
	LDFUNC(vscan_options, list, flags, name, specs, args)

static bool
vscan_options(DECL_LD term_t options, int flags, const char *opttype,
	      const PL_option_t *specs, va_list args)
{ const PL_option_t *s;
  optvalue values[MAXOPTIONS];
  term_t list;
  term_t av, head, tmp, val;
  int n;
  int candiscard = true;
  int count = 0;
  (void)opttype;

  if ( flags == OPT_UNKNOWN_DEFAULT )
    flags = LD->prolog_flag.unknown_option;

  for( n=0, s = specs; s->name; s++, n++ )
  { if ( n >= MAXOPTIONS )
      fatalError("PL_scan_options(): more than %d options", MAXOPTIONS);
    values[n].ptr = va_arg(args, void *);
  }

  if ( PL_is_dict(options) )
    return dict_options(options, flags, opttype, specs, values);

  list = PL_copy_term_ref(options);
  av = PL_new_term_refs(3);
  head = av+0;
  tmp  = av+1;
  val  = av+2;

  while ( PL_get_list(list, head, list) )
  { atom_t name;
    size_t arity;
    int implicit_true = false;

    if ( count++ == 1000 )
    { if ( !PL_is_acyclic(list) )
	return PL_type_error("list", options);
    }

    if ( PL_get_name_arity(head, &name, &arity) )
    { if ( name == ATOM_equals && arity == 2 )
      { _PL_get_arg(1, head, tmp);

	if ( !PL_get_atom(tmp, &name) )
	  goto itemerror;
	_PL_get_arg(2, head, val);
      } else if ( arity == 1 )
      { _PL_get_arg(1, head, val);
      } else if ( arity == 0 && !truePrologFlag(PLFLAG_ISO) )
      { implicit_true = true;
      } else
      { goto itemerror;
      }
    } else
    { itemerror:
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_option, head);
    }

    for( n=0, s = specs; s->name; n++, s++ )
    { if ( s->name == name )
      { if ( implicit_true )
	{ if ( (s->type&OPT_TYPE_MASK) == OPT_BOOL )
	  { *(values[n].b) = true;
	    break;
	  }
	  goto itemerror;
	}
	if ( !get_optval(values[n], s, val) )
	  return false;
	if ( (s->type&OPT_TYPE_MASK) == OPT_TERM )
	  candiscard = false;
	break;
      }
    }

    if ( !s->name && !HAS_OPT_MODE(flags, OPT_UNKNOWN_IGNORE) )
    { if ( HAS_OPT_MODE(flags, OPT_UNKNOWN_ERROR) )
	return PL_domain_error(opttype, head);
      if ( !printMessage(ATOM_warning,
			 PL_FUNCTOR, FUNCTOR_unknown_option2,
			   PL_CHARS, opttype,
			   PL_TERM, head) )
	return false;
    }
  }

  if ( !PL_get_nil(list) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, list);

  if ( candiscard )
    PL_reset_term_refs(list);

  succeed;
}


bool
PL_scan_options(DECL_LD term_t options, int flags, const char *opttype,
		const PL_option_t *specs, ...)
{ bool rc;
  va_list args;

  va_start(args, specs);
  rc = vscan_options(options, flags, opttype, specs, args);
  va_end(args);

  return rc;
}

API_STUB(bool)
(PL_scan_options)(term_t options, int flags, const char *opttype,
		  PL_option_t *specs, ...)
( bool rc;
  va_list args;

  valid_term_t(options);
  for(PL_option_t *s = specs; s->name || s->string; s++)
  { if ( !s->name && s->string )
      s->name = PL_new_atom(s->string);
  }

  va_start(args, specs);
  rc = vscan_options(options, flags, opttype, specs, args);
  va_end(args);

  return rc;
)
