/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2013, University of Amsterdam
			      VU University Amsterdam

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

#include "pl-incl.h"

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


bool
scan_options(term_t options, int flags, atom_t optype,
	     const opt_spec *specs, ...)
{ GET_LD
  va_list args;
  const opt_spec *s;
  optvalue values[MAXOPTIONS];
  term_t list = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  term_t tmp  = PL_new_term_ref();
  term_t val  = PL_new_term_ref();
  int n;

  if ( truePrologFlag(PLFLAG_ISO) )
    flags |= OPT_ALL;

  va_start(args, specs);
  for( n=0, s = specs; s->name; s++, n++ )
    values[n].ptr = va_arg(args, void *);
  va_end(args);

  while ( PL_get_list(list, head, list) )
  { atom_t name;
    int arity;

    if ( PL_get_name_arity(head, &name, &arity) )
    { if ( name == ATOM_equals && arity == 2 )
      { _PL_get_arg(1, head, tmp);

	if ( !PL_get_atom(tmp, &name) )
	  goto itemerror;
	_PL_get_arg(2, head, val);
      } else if ( arity == 1 )
      { _PL_get_arg(1, head, val);
      } else if ( arity == 0 )
	PL_put_atom(val, ATOM_true);
    } else if ( PL_is_variable(head) )
    { return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
    } else
    { itemerror:
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, optype, head);
    }

    for( n=0, s = specs; s->name; n++, s++ )
    { if ( s->name == name )
      { if ( !get_optval(values[n], s, val PASS_LD) )
	  return FALSE;
	break;
      }
    }

    if ( !s->name && (flags & OPT_ALL) )
      goto itemerror;
  }

  if ( !PL_get_nil(list) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, list);

  succeed;
}
