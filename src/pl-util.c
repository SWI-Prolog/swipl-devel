/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: asorted handy functions
*/

#include "pl-incl.h"
#include "pl-ctype.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Transform a Prolog word into an integer.   Accepts  integers  and  reals
that are by accident integer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
wordToInteger(word w, long *n)
{ real f;

  if (isInteger(w) )
  { *n = valNum(w);
    succeed;
  }
  if (isReal(w) )
  { f = valReal(w);
    if (f == (real)((long)f))
    { *n = (long) f;
      succeed;
    }      
  }
  fail;
}  

/*  Transform a Prolog word into a real.  Accepts integers and reals.

 ** Fri Jun 10 10:45:18 1988  jan@swivax.UUCP (Jan Wielemaker)  */

bool
wordToReal(word w, real *f)
{ if (isInteger(w) )
  { *f = (real) valNum(w);
    succeed;
  }
  if (isReal(w) )
  { *f = valReal(w);
    succeed;
  }
  fail;
}  

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

bool
isUserSystemPredicate(Definition def)
{ if ( true(def, SYSTEM) &&
       isCurrentProcedure(def->functor, MODULE_user) )
    succeed;

  fail;
}

bool
isUserSystemProcedure(Procedure proc)
{ return isUserSystemPredicate(proc->definition);
}

word
notImplemented(char *name, int arity)
{ return warning("%s/%d is not implemented in this version");
}

		 /*******************************
		 *	       OPTIONS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable argument list:

	Atom	name
	int	type	OPT_ATOM, OPT_STRING, OPT_BOOL, OPT_INT
	pointer	value
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
scan_options(word list, int flags, OptSpec specs, ...)
{ va_list args;
  OptSpec s;

  va_start(args, specs);
  for( s = specs; s->name; s++ )
    s->value.ptr = va_arg(args, void *);
  va_end(args);

  while ( isList(list) )
  { Atom name;
    word value;
    word a = argTerm(list, 0);

    if ( isTerm(a) )
    { FunctorDef f = functorTerm(a);

      if ( f == FUNCTOR_equals2 )
      { word a1 = argTerm(a, 0);

        if ( isAtom(a1) )
	{ name = (Atom)a1;
	  value = argTerm(a, 1);
	} else
	  fail;
      } else if ( f->arity == 1 )
      { name = f->name;
        value = argTerm(a, 0);
      } else
	fail;
    } else if ( isAtom(a) )
    { name = (Atom)a;
      value = (word) ATOM_true;
    } else
      fail;

    for( s = specs; s->name; s++ )
    { if ( s->name == name )
      { switch(s->type)
	{ case OPT_BOOL:
	    if ( value == (word)ATOM_true ||
		 value == (word)ATOM_on )
	      *s->value.b = TRUE;
	    else if ( value == (word)ATOM_false ||
		      value == (word)ATOM_off )
	      *s->value.b = FALSE;
	    else
	      fail;
	    break;
	  case OPT_INT:
	    if ( !wordToInteger(value, s->value.i) )
	      fail;
	    break;
	  case OPT_STRING:
	  { char *str;

	    if ( (str = toString(value)) )
	      *s->value.s = str;
	    else
	      fail;
	    break;
	  }
	  case OPT_ATOM:
	  { if ( isAtom(value) )
	      *s->value.a = (Atom) value;
	    else
	      fail;
	  }
	  break;
	  default:
	    fail;
	}
      }
    }
    
    if ( !s->name && (flags & OPT_ALL) )
      fail;

    list = argTerm(list, 1);
  }

  if ( list == (word)ATOM_nil )
    succeed;

  fail;
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


bool
strsub(register char *string, register char *sub)
{ register char *s, *sb;

  while( *(s = string++) )
  { for(sb=sub; *sb && *s == *sb; )
      s++, sb++;
    if ( *sb == EOS )
      succeed;
  }
  fail;
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

