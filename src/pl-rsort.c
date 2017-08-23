/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, University of Amsterdam
                         VU University Amsterdam
		         CWI, Amsterdam
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

#define _GNU_SOURCE 1				/* get qsort_r() */
#include "pl-incl.h"
#include "pl-rsort.h"


		 /*******************************
		 *	REENTRANT SORTING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sort_r(void *base, size_t nel, size_t width,
       int (*compar)(const void *a1, const void *a2, void *aarg), void *arg)

This library provides sort_r(), which is   compatible  to qsort_r() from
glibc.       The       implementation       is         inspired       by
https://github.com/noporpoise/sort_r, but has been extended since to use
autoconf rather than hard-wired #ifdef and  fallback to a locked version
if all fails.  The four versions tried are:

  - GNU qsort_r (direct mapping)
  - BSD qsort_r (different argument order)
  - MS  qsort_s (yet another different argument order)
  - GCC nested functions (does not work if stack is execute-protected)
  - A locket version.

Eventually it might be better to roll our own or use the sorting routine
from pl-list.c.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if !defined(HAVE_QSORT_R) && !defined(HAVE_QSORT_S)

#ifdef QSORT_R_WITH_NESTED_FUNCTIONS

void
sort_r(void *base, size_t nel, size_t width,
       int (*compar)(const void *a1, const void *a2, void *aarg), void *arg)
{ int nested_cmp(const void *a, const void *b)
  {
    return compar(a, b, arg);
  }

  qsort(base, nel, width, nested_cmp);
}

#else

static void *sort_r_ctx;
static int (*sort_r_compar)(const void *a1, const void *a2, void *aarg);

int
nested_cmp(const void *a, const void *b)
{ return (*sort_r_compar)(a, b, sort_r_ctx);
}

void
sort_r(void *base, size_t nel, size_t width,
       int (*compar)(const void *a1, const void *a2, void *aarg), void *arg)
{ PL_LOCK(L_SORTR);
  sort_r_ctx = arg;
  sort_r_compar = compar;

  qsort(base, nel, width, nested_cmp);
  PL_UNLOCK(L_SORTR);
}

#endif

#else /*HAVE_QSORT_R|HAVE_QSORT_S*/

#ifndef QSORT_R_GNU
struct sort_r_data
{ void *arg;
  int (*compar)(const void *a1, const void *a2, void *aarg);
};

static int
sort_r_arg_swap(void *s, const void *aa, const void *bb)
{ struct sort_r_data *ss = (struct sort_r_data*)s;
  return (ss->compar)(aa, bb, ss->arg);
}
#endif

void
sort_r(void *base, size_t nel, size_t width,
       int (*compar)(const void *a1, const void *a2, void *aarg), void *arg)
{
#ifdef HAVE_QSORT_R
#ifdef QSORT_R_GNU
    qsort_r(base, nel, width, compar, arg);
#else
    struct sort_r_data tmp = {arg, compar};
    qsort_r(base, nel, width, &tmp, &sort_r_arg_swap);
#endif
#else
    struct sort_r_data tmp = {arg, compar};
    qsort_s(base, nel, width, &sort_r_arg_swap, &tmp);
#endif
}

#endif /*HAVE_QSORT_R|HAVE_QSORT_S*/

