/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2020, University of Amsterdam
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

#include "pl-incl.h"

#ifndef _PL_TRACE_H
#define _PL_TRACE_H

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

#if USE_LD_MACROS
#define	tracePort(frame, bfr, port, PC)		LDFUNC(tracePort, frame, bfr, port, PC)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

void		suspendTrace(int suspend);	/* suspend/resume tracing */
int		isDebugFrame(LocalFrame FR);
int		tracePort(LocalFrame frame, Choice bfr,
			  int port, Code PC);
void		initTracer(void);
int		enable_debug_on_interrupt(int enable);
void		resetTracer(void);
int		tracemode(int new, int *old);
int		debugmode(debug_type new, debug_type *old);
int		trace_if_space(void);
int		put_frame_goal(term_t goal, LocalFrame frame);
word		pl_trace(void);
word		pl_notrace(void);
word		pl_tracing(void);
word		pl_spy(term_t p);
word		pl_nospy(term_t p);
word		pl_leash(term_t old, term_t new);
word		pl_visible(term_t old, term_t new);
word		pl_debuglevel(term_t old, term_t new);
word		pl_prolog_current_frame(term_t fr);
int		PL_put_frame(term_t t, LocalFrame fr);
void		PL_put_choice(term_t t, Choice ch);

#undef LDFUNC_DECLARATIONS

#endif /*_PL_TRACE_H*/			