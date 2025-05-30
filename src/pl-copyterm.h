/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2024, VU University Amsterdam
			 CWI, Amsterdam
			 SWI-Prolog Solutions b.v.
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

#ifndef _PL_COPYTERM_H
#define _PL_COPYTERM_H

typedef struct fastheap_term
{ size_t data_len;
  unsigned int *relocations;			/* Relocation points */
  Word data;					/* the actual data */
} fastheap_term;

#if USE_LD_MACROS
#define	term_to_fastheap(t)			LDFUNC(term_to_fastheap, t)
#define	put_fastheap(fht, t)			LDFUNC(put_fastheap, fht, t)
#define	duplicate_term(in, copy, nshare, share) \
	LDFUNC(duplicate_term, in, copy, nshare, share)
#define	size_abstract_term(in, copy, abstract) \
	LDFUNC(size_abstract_term, in, copy, abstract)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

fastheap_term*	term_to_fastheap(term_t t);
void		free_fastheap(fastheap_term *fht);
bool		put_fastheap(fastheap_term *fht, term_t t);
bool		duplicate_term(term_t in, term_t copy,
			       size_t nshare, term_t share);
bool		size_abstract_term(term_t in, term_t copy,
				   size_t abstract);

#undef LDFUNC_DECLARATIONS

#endif /*_PL_COPYTERM_H*/
