/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2015, University of Amsterdam
                              VU University Amsterdam
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
#include "pl-dict.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable argument list:

	atom_t	name
	int	type	OPT_ATOM, OPT_STRING, OPT_BOOL, OPT_INT, OPT_LONG
	pointer	value
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAXOPTIONS 32

typedef union
{ bool *b;				/* boolean value */
  long *l;				/* long value */
  int  *i;				/* integer value */
  uintptr_t *sz;			/* size_t value */
  double *f;				/* double value */
  char **s;				/* string value */
  word *a;				/* atom value */
  term_t *t;				/* term-reference */
  void *ptr;				/* anonymous pointer */
} optvalue;


static int
get_optval(optvalue valp, const opt_spec *spec, term_t val ARG_LD)
{ switch((spec->type & OPT_TYPE_MASK))
  { case OPT_BOOL:
    { int bval;

      if ( !PL_get_bool_ex(val, &bval) )
	return FALSE;
      *valp.b = bval;

      return TRUE;
    }
    case OPT_INT:
    { if ( !PL_get_integer_ex(val, valp.i) )
	return FALSE;

      return TRUE;
    }
    case OPT_LONG:
    { if ( (spec->type & OPT_INF) && PL_is_inf(val) )
	*valp.l = LONG_MAX;
      else if ( !PL_get_long_ex(val, valp.l) )
	return FALSE;

      return TRUE;
    }
    case OPT_NATLONG:
    { if ( !PL_get_long_ex(val, valp.l) )
	return FALSE;
      if ( *(valp.l) <= 0 )
	return PL_error(NULL, 0, NULL, ERR_DOMAIN,
			ATOM_not_less_than_one, val);

      return TRUE;
    }
    case OPT_SIZE:
    { if ( (spec->type & OPT_INF) && PL_is_inf(val) )
	*valp.sz = (size_t)-1;
      else if ( !PL_get_size_ex(val, valp.sz) )
	return FALSE;

      return TRUE;
    }
    case OPT_DOUBLE:
    { if ( !PL_get_float_ex(val, valp.f) )
	return FALSE;

      return TRUE;
    }
    case OPT_STRING:
    { char *str;

      if ( !PL_get_chars(val, &str, CVT_ALL|CVT_EXCEPTION) ) /* copy? */
	return FALSE;
      *valp.s = str;

      return TRUE;
    }
    case OPT_ATOM:
    { atom_t a;

      if ( !PL_get_atom_ex(val, &a) )
	return FALSE;
      *valp.a = a;

      return TRUE;
    }
#ifdef O_LOCALE
    case OPT_LOCALE:
    { PL_locale *l;
      PL_locale **lp = valp.ptr;

      if ( !getLocaleEx(val, &l) )
	return FALSE;
      *lp = l;

      return TRUE;
    }
#endif
    case OPT_TERM:
    { *valp.t = PL_copy_term_ref(val); /* can't reuse anymore */

      return TRUE;
    }
    default:
      assert(0);
  }

  return FALSE;
}


typedef struct dictopt_ctx
{ const opt_spec       *specs;		/* specifications */
  optvalue	       *values;		/* value pointers */
  PL_local_data_t      *ld;		/* the engine */
} dictopt_ctx;


static int
dict_option(term_t key, term_t value, int last, void *closure)
{ dictopt_ctx *ctx = closure;
#if defined(O_PLMT) || defined(O_MULTIPLE_ENGINES)
  PL_local_data_t *__PL_ld = ctx->ld;
#endif
  atom_t name;
  int n;
  const opt_spec *s;

  if ( !PL_get_atom_ex(key, &name) )
    return -1;

  for( n=0, s = ctx->specs; s->name; n++, s++ )
  { if ( s->name == name )
    { if ( !get_optval(ctx->values[n], s, value PASS_LD) )
	return -1;
      return 0;
    }
  }

  return 0;				/* unprocessed key */
}


/* Process options from a dict.  Note that this can be more efficient by
   sorting the option specification, after which we can perform a linear
   scan.  I think that the best way to do that is to associate the
   option specification with a writeable structure that is lazily
   initialized to an array of opt_spec pointers using the ordering of
   dicts.

   An alternative is to process the opt-specs in order and use the
   dicts binary search to find the values (or not).  As far as I know,
   option processing of built-ins is not a bottleneck, so there is no
   need to worry right now.
*/

static int
dict_options(term_t dict, int flags, const opt_spec *specs, optvalue *values ARG_LD)
{ dictopt_ctx ctx;

  ctx.specs  = specs;
  ctx.values = values;
  ctx.ld     = LD;

  return PL_for_dict(dict, dict_option, &ctx, 0) == 0 ? TRUE : FALSE;
}


int
scan_options(term_t options, int flags, atom_t optype,
	     const opt_spec *specs, ...)
{ GET_LD
  va_list args;
  const opt_spec *s;
  optvalue values[MAXOPTIONS];
  term_t list;
  term_t av, head, tmp, val;
  int n;
  int candiscard = TRUE;

  if ( truePrologFlag(PLFLAG_ISO) )
    flags |= OPT_ALL;

  va_start(args, specs);
  for( n=0, s = specs; s->name; s++, n++ )
    values[n].ptr = va_arg(args, void *);
  va_end(args);

  if ( PL_is_dict(options) )
    return dict_options(options, flags, specs, values PASS_LD);

  list = PL_copy_term_ref(options);
  av = PL_new_term_refs(3);
  head = av+0;
  tmp  = av+1;
  val  = av+2;

  while ( PL_get_list(list, head, list) )
  { atom_t name;
    size_t arity;
    int implicit_true = FALSE;

    if ( PL_get_name_arity(head, &name, &arity) )
    { if ( name == ATOM_equals && arity == 2 )
      { _PL_get_arg(1, head, tmp);

	if ( !PL_get_atom(tmp, &name) )
	  goto itemerror;
	_PL_get_arg(2, head, val);
      } else if ( arity == 1 )
      { _PL_get_arg(1, head, val);
      } else if ( arity == 0 )
      { implicit_true = TRUE;
      } else
      { goto itemerror;
      }
    } else if ( PL_is_variable(head) )
    { return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
    } else
    { itemerror:
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, optype, head);
    }

    for( n=0, s = specs; s->name; n++, s++ )
    { if ( s->name == name )
      { if ( implicit_true )
	{ if ( (s->type&OPT_TYPE_MASK) == OPT_BOOL )
	  { *(values[n].b) = TRUE;
	    break;
	  }
	  goto itemerror;
	}
	if ( !get_optval(values[n], s, val PASS_LD) )
	  return FALSE;
	if ( (s->type&OPT_TYPE_MASK) == OPT_TERM )
	  candiscard = FALSE;
	break;
      }
    }

    if ( !s->name && (implicit_true || (flags & OPT_ALL)) )
      goto itemerror;
  }

  if ( !PL_get_nil(list) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, list);

  if ( candiscard )
    PL_reset_term_refs(list);

  succeed;
}
