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
#include "pl-utf8.h"

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
before the ring is exhausted. See buffer_string() for details.  For wide
character versions, we use UTF-8 encoding.  This isn't very elegant, but
these functions are for debugging only.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
procedureName(Procedure proc)
{ return predicateName(proc->definition);
}


#define fetch_text(s, i) \
	((s)->encoding == ENC_ISO_LATIN_1 ? (s)->text.t[i]&0xff \
					  : (s)->text.w[i])

const char *
atom_summary(atom_t name, unsigned int maxlen)
{ PL_chars_t txt;
  Buffer b;
  unsigned int i;

  if ( !get_atom_text(name, &txt) )
    return NULL;

  if ( txt.encoding == ENC_ISO_LATIN_1 && txt.length < maxlen )
  { const unsigned char *s = (const unsigned char*) txt.text.t;
    const unsigned char *e = &s[txt.length];

    for( ; s<e; s++ )
    { if ( *s >= 0x80 )
	break;
    }
    if ( s == e )
      return txt.text.t;
  }

  b = findBuffer(BUF_RING);
  for(i=0; i<txt.length; i++)
  { char buf[6];
    char *e;

    e = utf8_put_char(buf, fetch_text(&txt, i));
    addMultipleBuffer(b, buf, e-buf, char);
    if ( i == maxlen - 6 )
    { addMultipleBuffer(b, "...", 3, char);
      i = txt.length - 4;
      maxlen = 0;			/* make sure not to trap again */
    }
  }
  addBuffer(b, 0, char);

  return baseBuffer(b, char);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
predicateName() returns an UTF-8  representation  of   the  name  of the
predicate. Note that we need for the buffer 6*max summary length,
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
predicateName(Definition def)
{ char tmp[650];
  char *e = tmp;

  if ( def->module != MODULE_user && !isUserSystemPredicate(def) )
  { strcpy(e, atom_summary(def->module->name, 50));
    e += strlen(e);
    *e++ = ':';
  }
  strcpy(e, atom_summary(def->functor->name, 50));
  e += strlen(e);
  *e++ = '/';
  Ssprintf(e, "%d", def->functor->arity);

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
      { switch((s->type & OPT_TYPE_MASK))
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
	    { if ( (s->type & OPT_INF) && PL_is_inf(val) )
		*values[n].l = LONG_MAX;
	      else
		goto itemerror;
	    }

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


#ifndef HAVE_STRCASECMP
int
strcasecmp(const char *s1, const char *s2)
{
#ifdef HAVE_STRICMP
  return stricmp(s1, s2);
#else
  while(*s1 && makeLower(*s1) == makeLower(*s2))
    s1++, s2++;
  
  return makeLower(*s1) - makeLower(*s2);
#endif
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
    return strcasecmp(&s[ls-le], e) == 0;

  return FALSE;
} 


		 /*******************************
		 *	MULTIBYTE STRINGS	*
		 *******************************/

typedef struct 
{ wchar_t *wcp;
  int	   len;
  int	   malloced;
} wbuf;


#if !defined(HAVE_MBSCOLL) || !defined(HAVE_MBCASESCOLL)
static void
wstolower(wchar_t *w, int len)
{ wchar_t *e = &w[len];

  for( ; w<e; w++ )
    *w = towlower(*w);
}

static int
int_mbscoll(const char *s1, const char *s2, int icase)
{ int l1 = strlen(s1);
  int l2 = strlen(s2);
  wchar_t *w1;
  wchar_t *w2;
  int ml1, ml2;
  mbstate_t mbs;
  int rc;

  if ( l1 < 1024 && (w1 = alloca(sizeof(wchar_t)*(l1+1))) )
  { ml1 = FALSE;
  } else
  { w1 = PL_malloc(sizeof(wchar_t)*(l1+1));
    ml1 = TRUE;
  }
  if ( l2 < 1024 && (w2 = alloca(sizeof(wchar_t)*(l2+1))) )
  { ml2 = FALSE;
  } else
  { w2 = PL_malloc(sizeof(wchar_t)*(l2+1));
    ml2 = TRUE;
  }

  memset(&mbs, 0, sizeof(mbs));
  if ( mbsrtowcs(w1, &s1, l1, &mbs) < 0 )
  { rc = -2;
    goto out;
  }
  if ( mbsrtowcs(w2, &s2, l2, &mbs) < 0 )
  { rc = 2;
    goto out;
  }
  if ( icase )
  { wstolower(w1, l1);
    wstolower(w2, l2);
  }

  rc = wcscoll(w1, w2);

out:
  if ( ml1 ) PL_free(w1);
  if ( ml2 ) PL_free(w2);

  return rc;
}
#endif


#ifndef HAVE_MBSCOLL
int
mbscoll(const char *s1, const char *s2)
{ return int_mbscoll(s1, s2, FALSE);
}
#endif


#ifndef HAVE_MBSCASECOLL
int
mbscasecoll(const char *s1, const char *s2)
{ return int_mbscoll(s1, s2, TRUE);
}
#endif

