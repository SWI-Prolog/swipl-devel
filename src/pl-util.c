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

#include "pl-incl.h"
#include "pl-ctype.h"

static bool	isUserSystemPredicate(Definition def);

/*  Return the character representing some digit.

 ** Fri Jun 10 10:45:40 1988  jan@swivax.UUCP (Jan Wielemaker)  */

char
digitName(int n, bool small)
{ if (n <= 9)
    return n + '0';
  return n + (small ? 'a' : 'A') - 10;
}

/*  Return the value of a digit when transforming a number of base 'b'.
    Return '-1' if it is an illegal digit.

 ** Fri Jun 10 10:46:40 1988  jan@swivax.UUCP (Jan Wielemaker)  */

int
digitValue(int b, int c)
{ int v;

  if ( b == 0 )
    return c;				/* 0'c */
  if ( b == 1 )
    return -1;
  if ( b <= 10 )
  { v = c - '0';
    if ( v < b )
      return v;
    return -1;
  }
  if ( c <= '9' )
    return c - '0';
  if (isUpper(c))
    c = toLower(c);
  c = c - 'a' + 10;
  if ( c < b && c >= 10 )
    return c;
  return -1;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These  functions  return  a  user-printable  name   of  a  predicate  as
name/arity or module:name/arity. The result  is   stored  in the foreign
buffer ring, so we are thread-safe, but   the  result needs to be copied
before the ring is exhausted. See buffer_string() for details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
procedureName(Procedure proc)
{ return predicateName(proc->definition);
}


char *
predicateName(Definition def)
{ char tmp[256];

  if ( def->module == MODULE_user || isUserSystemPredicate(def) )
    Ssprintf(tmp, "%s/%d",
	     stringAtom(def->functor->name), 
	     def->functor->arity);
  else
    Ssprintf(tmp, "%s:%s/%d",
	     stringAtom(def->module->name), 
	     stringAtom(def->functor->name), 
	     def->functor->arity);

  return buffer_string(tmp, BUF_RING);
}

/*  succeeds if proc is a system predicate exported to the public module.

 ** Fri Sep  2 17:03:43 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static bool
isUserSystemPredicate(Definition def)
{ if ( true(def, SYSTEM) &&
       isCurrentProcedure(def->functor->functor, MODULE_user) )
    succeed;

  fail;
}


word
notImplemented(char *name, int arity)
{ return PL_error(NULL, 0, NULL, ERR_NOT_IMPLEMENTED_PROC, name, arity);
}


word
setBoolean(int *flag, term_t old, term_t new)
{ if ( !PL_unify_bool_ex(old, *flag) ||
       !PL_get_bool_ex(new, flag) )
    fail;

  succeed;
}


word
setInteger(int *flag, term_t old, term_t new)
{ if ( !PL_unify_integer(old, *flag) ||
       !PL_get_integer_ex(new, flag) )
    fail;

  succeed;
}


word
setLong(long *flag, term_t old, term_t new)
{ if ( !PL_unify_integer(old, *flag) ||
       !PL_get_long_ex(new, flag) )
    fail;

  succeed;
}


		 /*******************************
		 *	       OPTIONS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable argument list:

	atom_t	name
	int	type	OPT_ATOM, OPT_STRING, OPT_BOOL, OPT_INT, OPT_LONG
	pointer	value
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAXOPTIONS 32

typedef union
{ bool *b;				/* boolean value */
  long *l;				/* integer value */
  int  *i;				/* integer value */
  char **s;				/* string value */
  word *a;				/* atom value */
  term_t *t;				/* term-reference */
  void *ptr;				/* anonymous pointer */
} optvalue;

bool
scan_options(term_t options, int flags, atom_t optype,
	     const opt_spec *specs, ...)
{ va_list args;
  const opt_spec *s;
  optvalue values[MAXOPTIONS];
  term_t list = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  term_t tmp  = PL_new_term_ref();
  term_t val  = PL_new_term_ref();
  int n;

  if ( trueFeature(ISO_FEATURE) )
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
      { PL_get_arg(1, head, tmp);

	if ( !PL_get_atom(tmp, &name) )
	  goto itemerror;
	PL_get_arg(2, head, val);
      } else if ( arity == 1 )
      { PL_get_arg(1, head, val);
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
      { switch(s->type)
	{ case OPT_BOOL:
	  { atom_t aval;

	    if ( !PL_get_atom(val, &aval) )
	      fail;
	    if ( aval == ATOM_true || aval == ATOM_on )
	      *values[n].b = TRUE;
	    else if ( aval == ATOM_false || aval == ATOM_off )
	      *values[n].b = FALSE;
	    else
	      goto itemerror;
	    break;
	  }
	  case OPT_INT:
	  { if ( !PL_get_integer(val, values[n].i) )
	      goto itemerror;

	    break;
	  }
	  case OPT_LONG:
	  { if ( !PL_get_long(val, values[n].l) )
	      goto itemerror;

	    break;
	  }
	  case OPT_STRING:
	  { char *str;

	    if ( !PL_get_chars(val, &str, CVT_ALL) ) /* copy? */
	      goto itemerror;
	    *values[n].s = str;
	    break;
	  }
	  case OPT_ATOM:
	  { atom_t a;

	    if ( !PL_get_atom(val, &a) )
	      goto itemerror;
	    *values[n].a = a;
	    break;
	  }
	  case OPT_TERM:
	  { *values[n].t = val;
	    val = PL_new_term_ref();	/* can't reuse anymore */
	    break;
	  }
	  default:
	    assert(0);
	    fail;
	}
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



		/********************************
		*             STRING            *
		*********************************/


bool
strprefix(const char *string, const char *prefix)
{ while(*prefix && *string == *prefix)
    prefix++, string++;
  if (*prefix == EOS )
    succeed;
  fail;
}


bool
strpostfix(const char *string, const char *postfix)
{ long offset = strlen(string) - strlen(postfix);

  if ( offset < 0 )
    fail;

  return streq(&string[offset], postfix);
}


#ifndef HAVE_STRICMP
int
stricmp(const char *s1, const char *s2)
{ while(*s1 && makeLower(*s1) == makeLower(*s2))
    s1++, s2++;
  
  return makeLower(*s1) - makeLower(*s2);
}
#endif

#ifndef HAVE_STRLWR
char *
strlwr(char *s)
{ char *q;

  for(q=s; *q; q++)
    *q = makeLower(*q);

  return s;
}
#endif


bool
stripostfix(const char *s, const char *e)
{ int ls = strlen(s);
  int le = strlen(e);

  if ( ls >= le )
    return stricmp(&s[ls-le], e) == 0;

  return FALSE;
} 


