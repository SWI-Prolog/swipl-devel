/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: asorted handy functions
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

/*  return the name of a procedure as a string.  The result is stored in
    static  area and should be copied away before the next call if it is
    to be preserved.

 ** Sun Aug 28 13:21:07 1988  jan@swivax.UUCP (Jan Wielemaker)  */

char *
procedureName(Procedure proc)
{ return predicateName(proc->definition);
}


char *
predicateName(Definition def)
{ static char tmp[256];

  if ( def->module == MODULE_user || isUserSystemPredicate(def) )
    Ssprintf(tmp, "%s/%d",
	     stringAtom(def->functor->name), 
	     def->functor->arity);
  else
    Ssprintf(tmp, "%s:%s/%d",
	     stringAtom(def->module->name), 
	     stringAtom(def->functor->name), 
	     def->functor->arity);

  return tmp;
}

/*  succeeds if proc is a system predicate exported to the public module.

 ** Fri Sep  2 17:03:43 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static bool
isUserSystemPredicate(Definition def)
{ if ( true(def, SYSTEM) &&
       isCurrentProcedure(def->functor, MODULE_user) )
    succeed;

  fail;
}

word
notImplemented(char *name, int arity)
{ return warning("%s/%d is not implemented in this version", name, arity);
}

word
setBoolean(int *flag, const char *name, term_t old, term_t new)
{ atom_t n;

  if ( !PL_unify_atom(old, *flag ? ATOM_on : ATOM_off) )
    fail;

  if ( PL_get_atom(new, &n) )
  { if ( n == ATOM_on )
    { *flag = TRUE;
      succeed;
    } else if ( n == ATOM_off )
    { *flag = FALSE;
      succeed;
    }
  }

  return warning("%s/2: instantiation fault", name);
}

word
setInteger(int *flag, const char *name, term_t old, term_t new)
{ if ( !PL_unify_integer(old, *flag) )
    fail;
  if ( PL_get_integer(new, flag) )
    succeed;

  return warning("%s/2: instantiation fault", name);
}


word
setLong(long *flag, const char *name, term_t old, term_t new)
{ if ( !PL_unify_integer(old, *flag) )
    fail;
  if ( PL_get_long(new, flag) )
    succeed;

  return warning("%s: instantiation fault", name);
}


		 /*******************************
		 *	       OPTIONS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable argument list:

	atom_t	name
	int	type	OPT_ATOM, OPT_STRING, OPT_BOOL, OPT_INT
	pointer	value
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
scan_options(term_t options, int flags, OptSpec specs, ...)
{ va_list args;
  OptSpec s;
  term_t list = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  term_t tmp  = PL_new_term_ref();
  term_t val  = PL_new_term_ref();

  va_start(args, specs);
  for( s = specs; s->name; s++ )
    s->value.ptr = va_arg(args, void *);
  va_end(args);

  while ( PL_get_list(list, head, list) )
  { atom_t name;
    int arity;
    
    if ( PL_get_name_arity(head, &name, &arity) )
    { if ( name == ATOM_equals && arity == 2 )
      { PL_get_arg(1, head, tmp);

	if ( !PL_get_atom(tmp, &name) )
	  fail;
	PL_get_arg(2, head, val);
      } else if ( arity == 1 )
      { PL_get_arg(1, head, val);
      } else if ( arity == 0 )
	PL_put_atom(val, ATOM_true);
    } else
      fail;

    for( s = specs; s->name; s++ )
    { if ( s->name == name )
      { switch(s->type)
	{ case OPT_BOOL:
	  { atom_t aval;

	    if ( !PL_get_atom(val, &aval) )
	      fail;
	    if ( aval == ATOM_true || aval == ATOM_on )
	      *s->value.b = TRUE;
	    else if ( aval == ATOM_false || aval == ATOM_off )
	      *s->value.b = FALSE;
	    else
	      fail;
	    break;
	  }
	  case OPT_INT:
	  { if ( !PL_get_long(val, s->value.i) )
	      fail;

	    break;
	  }
	  case OPT_STRING:
	  { char *str;

	    if ( !PL_get_chars(val, &str, CVT_ALL) ) /* copy? */
	      fail;
	    *s->value.s = str;
	    break;
	  }
	  case OPT_ATOM:
	  { atom_t a;

	    if ( !PL_get_atom(val, &a) )
	      fail;
	    *s->value.a = a;
	    break;
	  }
	  default:
	    fail;
	}
      }
    }
    
    if ( !s->name && (flags & OPT_ALL) )
      fail;
  }

  return PL_get_nil(list);		/* tail must now be [] */
}



		/********************************
		*             STRING            *
		*********************************/


bool
strprefix(register char *string, register char *prefix)
{ while(*prefix && *string == *prefix)
    prefix++, string++;
  if (*prefix == EOS )
    succeed;
  fail;
}


bool
strpostfix(char *string, char *postfix)
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
stripostfix(char *s, char *e)
{ int ls = strlen(s);
  int le = strlen(e);

  if ( ls >= le )
    return stricmp(&s[ls-le], e) == 0;

  return FALSE;
} 


		/********************************
		*        CHARACTER TYPES        *
		*********************************/

char char_type[] = {
/* ^@  ^A  ^B  ^C  ^D  ^E  ^F  ^G  ^H  ^I  ^J  ^K  ^L  ^M  ^N  ^O    0-15 */
   SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, 
/* ^P  ^Q  ^R  ^S  ^T  ^U  ^V  ^W  ^X  ^Y  ^Z  ^[  ^\  ^]  ^^  ^_   16-31 */
   SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, SP, 
/* sp   !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /   32-47 */
   SP, SO, DQ, SY, SY, SO, SY, SQ, PU, PU, SY, SY, PU, SY, SY, SY, 
/*  0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?   48-63 */
   DI, DI, DI, DI, DI, DI, DI, DI, DI, DI, SY, SO, SY, SY, SY, SY, 
/*  @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   64-79 */
   SY, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, 
/*  P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _   80-95 */
   UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, UC, PU, SY, PU, SY, UC, 
/*  `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o   96-111 */
   SY, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
/*  p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~  ^?   112-127 */
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, PU, PU, PU, SY, SP, 
			  /* 128-255 */
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, 
   LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC, LC };

void
systemMode(bool accept)
{ char_type[(int)'$'] = (accept ? LC : SY);
  if ( accept )
    debugstatus.styleCheck |= DOLLAR_STYLE;
  else
    debugstatus.styleCheck &= ~DOLLAR_STYLE;
}

