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
wordToInteger(word w, long int *n)
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
digitValue(int b, char c)
{ DEBUG(9, printf("digitValue(%d, %c)\n", b, c));
  if (b == 0)
  { if (c & 0x80)
      return -1;
    return c;
  }
  if (b == 1)
    return -1;
  if (b <= 10)
  { if (c - '0' < b)
      return c - '0';
    return -1;
  }
  if (c <= '9')
    return c - '0';
  if (isUpper(c))
    c = toLower(c);
  c = c - 'a' + 10;
  if (c < b)
    return c;
  return -1;
}

/*  return the name of a procedure as a string.  The result is stored in
    static  area and should be copied away before the next call if it is
    to be preserved.

 ** Sun Aug 28 13:21:07 1988  jan@swivax.UUCP (Jan Wielemaker)  */

char *
procedureName(Procedure proc)
{ static char tmp[256];

  if ( proc->definition->module == MODULE_user ||
       isUserSystemProcedure(proc) )
    sprintf(tmp, "%s/%d", stringAtom(proc->functor->name), 
			  proc->functor->arity);
  else
    sprintf(tmp, "%s:%s/%d", stringAtom(proc->definition->module->name), 
			     stringAtom(proc->functor->name), 
			     proc->functor->arity);

  return tmp;
}

/*  succeeds if proc is a system predicate exported to the public module.

 ** Fri Sep  2 17:03:43 1988  jan@swivax.UUCP (Jan Wielemaker)  */

bool
isUserSystemProcedure(Procedure proc)
{ if ( true(proc->definition, SYSTEM) &&
       isCurrentProcedure(proc->functor, MODULE_user) != (Procedure) NULL)
    succeed;

  fail;
}

word
notImplemented(char *name, int arity)
{ return warning("%s/%d is not implemented in this version");
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

