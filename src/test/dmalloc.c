/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009, University of Amsterdam
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

#include <SWI-Prolog.h>
#include <dmalloc.h>

static foreign_t
pl_dmalloc_mark(term_t t)
{ unsigned long mark = dmalloc_mark();

  return PL_unify_int64(t, mark);
}


static foreign_t
pl_dmalloc_log_changed(term_t mark,
		       term_t not_freed, term_t freed, term_t details)
{ unsigned long m;
  int64_t i;
  int nf, f, d;

  if ( !PL_get_int64(mark, &i) ||
       !PL_get_bool(not_freed, &nf) ||
       !PL_get_bool(freed, &f) ||
       !PL_get_bool(details, &d) )
    return FALSE;

  m = (unsigned long)i;
  dmalloc_log_changed(m, nf, f, d);

  return TRUE;
}


static foreign_t
pl_dmalloc_message(term_t msg)
{ char *s;

  if ( PL_get_chars(msg, &s, CVT_ALL|CVT_EXCEPTION|REP_MB) )
  { dmalloc_message("%s", s);
    return TRUE;
  }

  return FALSE;
}


install_t
install_dmalloc()
{ PL_register_foreign("dmalloc_mark",	     1, pl_dmalloc_mark,        0);
  PL_register_foreign("dmalloc_log_changed", 4, pl_dmalloc_log_changed, 0);
  PL_register_foreign("_dmalloc_message",    1, pl_dmalloc_message,     0);
}
