/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2012, University of Amsterdam
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
#include "pl-string.h"
#include "pl-ctype.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
String operations that are needed for the shared IO library.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	     ALLOCATION		*
		 *******************************/

char *
store_string(const char *s)
{ if ( s )
  { char *copy = (char *)allocHeapOrHalt(strlen(s)+1);

    strcpy(copy, s);
    return copy;
  } else
  { return NULL;
  }
}


void
remove_string(char *s)
{ if ( s )
    freeHeap(s, strlen(s)+1);
}

		 /*******************************
		 *	     NUMBERS		*
		 *******************************/

/*  Return the character representing some digit.

 ** Fri Jun 10 10:45:40 1988  jan@swivax.UUCP (Jan Wielemaker)  */

char
digitName(int n, int smll)
{ if (n <= 9)
    return n + '0';
  return n + (smll ? 'a' : 'A') - 10;
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


		/********************************
		*  LESS COMMON BASIC FUNCTIONS  *
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
{ intptr_t offset = strlen(string) - strlen(postfix);

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
{ size_t ls = strlen(s);
  size_t le = strlen(e);

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
wstolower(wchar_t *w, size_t len)
{ wchar_t *e = &w[len];

  for( ; w<e; w++ )
    *w = towlower(*w);
}

static int
int_mbscoll(const char *s1, const char *s2, int icase)
{ size_t l1 = strlen(s1);
  size_t l2 = strlen(s2);
  wchar_t *w1;
  wchar_t *w2;
  int ml1, ml2;
  mbstate_t mbs;
  int rc;

  if ( l1 < 1024 && (w1 = alloca(sizeof(wchar_t)*(l1+1))) )
  { ml1 = FALSE;
  } else
  { w1 = PL_malloc_atomic(sizeof(wchar_t)*(l1+1));
    ml1 = TRUE;
  }
  if ( l2 < 1024 && (w2 = alloca(sizeof(wchar_t)*(l2+1))) )
  { ml2 = FALSE;
  } else
  { w2 = PL_malloc_atomic(sizeof(wchar_t)*(l2+1));
    ml2 = TRUE;
  }

  memset(&mbs, 0, sizeof(mbs));
  if ( mbsrtowcs(w1, &s1, l1+1, &mbs) == (size_t)-1 )
  { rc = -2;
    goto out;
  }
  if ( mbsrtowcs(w2, &s2, l2+1, &mbs) == (size_t)-1 )
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
