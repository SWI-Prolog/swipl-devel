/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2025, University of Amsterdam
                              VU University Amsterdam
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

#include "pl-incl.h"

#ifndef _PL_TRACE_H
#define _PL_TRACE_H

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

#if USE_LD_MACROS
#define	tracePort(frame, bfr, port, PC)		LDFUNC(tracePort, frame, bfr, port, PC)
#define initTracer(_)				LDFUNC(initTracer, _)
#define enable_debug_on_interrupt(enable)	LDFUNC(enable_debug_on_interrupt, enable)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

void		suspendTrace(int suspend);	/* suspend/resume tracing */
bool		isDebugFrame(const LocalFrame FR, int port);
int		tracePort(LocalFrame frame, Choice bfr,
			  int port, Code PC);
void		initTracer(void);
bool		enable_debug_on_interrupt(bool enable);
void		resetTracer(void);
bool		tracemode(bool new, bool *old);
bool		debugmode(debug_type new, debug_type *old);
bool		trace_if_space(void);
bool		put_frame_goal(term_t goal, LocalFrame frame);
foreign_t	pl_trace(void);
foreign_t	pl_notrace(void);
foreign_t	pl_tracing(void);
foreign_t	pl_spy(term_t p);
foreign_t	pl_nospy(term_t p);
foreign_t	pl_leash(term_t old, term_t new);
foreign_t	pl_visible(term_t old, term_t new);
foreign_t	pl_debuglevel(term_t old, term_t new);
foreign_t	pl_prolog_current_frame(term_t fr);
bool		PL_put_frame(term_t t, LocalFrame fr);
bool		PL_put_choice(term_t t, Choice ch);

#undef LDFUNC_DECLARATIONS

#endif /*_PL_TRACE_H*/
