/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013, VU University Amsterdam
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

#ifndef PL_PROF_H_INCLUDED
#define PL_PROF_H_INCLUDED

typedef enum
{ PROF_INACTIVE = 0,		/* Profiler is inactive */
  PROF_CPU,			/* Profile CPU time */
  PROF_WALL			/* Profile wall time */
} prof_status;

#if USE_LD_MACROS
#define	activateProfiler(status)	LDFUNC(activateProfiler, status)
#define	resetProfiler(_)		LDFUNC(resetProfiler, _)
#define	profCall(def)			LDFUNC(profCall, def)
#define	profResumeParent(node)		LDFUNC(profResumeParent, node)
#define	profExit(node)			LDFUNC(profExit, node)
#define	profRedo(node)			LDFUNC(profRedo, node)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

void		stopItimer(void);
int		activateProfiler(prof_status status);
bool		resetProfiler(void);
struct call_node* profCall(Definition def);
void		profResumeParent(struct call_node *node);
void		profExit(struct call_node *node);
void		profRedo(struct call_node *node);
void		profSetHandle(struct call_node *node, void *handle);

#undef LDFUNC_DECLARATIONS

#endif /*PL_PROF_H_INCLUDED*/
