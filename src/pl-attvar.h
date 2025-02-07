/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2025, University of Amsterdam
                              VU University Amsterdam
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

#include "pl-incl.h"
#include "pl-wam.h"

#ifndef _PL_ATTVAR_H
#define _PL_ATTVAR_H

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

#if USE_LD_MACROS
#define	assignAttVar(av, value)		LDFUNC(assignAttVar, av, value)
#define	bind_attvar_const(av, c)	LDFUNC(bind_attvar_const, av, c)
#define	saveWakeup(state, forceframe)	LDFUNC(saveWakeup, state, forceframe)
#define wakeup_state_exception(state)	LDFUNC(wakeup_state_exception, state)
#define	restoreWakeup(state)		LDFUNC(restoreWakeup, state)
#define	PL_get_attr(t, a)		LDFUNC(PL_get_attr, t, a)
#define	alloc_attvar(_)			LDFUNC(alloc_attvar, _)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

void		assignAttVar(Word av, Word value);
bool		bind_attvar_const(Word p, word c);
bool		saveWakeup(wakeup_state *state, bool forceframe);
term_t		wakeup_state_exception(const wakeup_state *state);
void		restoreWakeup(wakeup_state *state);
bool		PL_get_attr(term_t t, term_t a);
bool		on_attvar_chain(Word avp);
Word		alloc_attvar(void);

#undef LDFUNC_DECLARATIONS

		 /*******************************
		 *	INLINE DEFINITIONS	*
		 *******************************/

#define varBindConst(p, c) LDFUNC(varBindConst, p, c)
static inline int
varBindConst(DECL_LD Word p, word c)
{ *p = (c);
  if ( unlikely((void*)p >= (void*)lBase || p < LD->mark_bar) )
    return trail_ptr(p);

  return true;
}

#define bindConst(p, c) LDFUNC(bindConst, p, c)
static inline int
bindConst(DECL_LD Word p, word c)
{
#ifdef O_ATTVAR
  if ( isVar(*p) )
    return varBindConst(p, c);
  else
    return bind_attvar_const(p, c);
#else
  return varBindConst(p, c);
#endif
}

#endif /*_PL_ATTVAR_H*/
