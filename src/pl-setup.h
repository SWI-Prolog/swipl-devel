/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2021, University of Amsterdam
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

#ifndef _PL_SETUP_H
#define _PL_SETUP_H

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

#if USE_LD_MACROS
#define	startCritical(_)	LDFUNC(startCritical, _)
#define	endCritical(_)		LDFUNC(endCritical, _)
#define	handleSignals(_)	LDFUNC(handleSignals, _)
#define	initPrologLocalData(_)	LDFUNC(initPrologLocalData, _)
#define	trimStacks(resize)	LDFUNC(trimStacks, resize)
#define	freeStacks(_)		LDFUNC(freeStacks, _)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

int		setupProlog(void);
void		startCritical(void);
int		endCritical(void);
void		dispatch_signal(int sig, int sync);
handler_t	set_sighandler(int sig, handler_t func);
void		blockSignals(sigset_t *mask);
void		allSignalMask(sigset_t *set);
void		unblockSignals(sigset_t *mask);
void		unblockSignal(int sig);
void		blockSignal(int sig);
void		resetSignals(void);
void		cleanupSignals(void);
int		handleSignals(void);
void		terminate_on_signal(int signo);
int		initGuardCStack(void);

int		initPrologStacks(size_t limit);
void		initPrologLocalData(void);
void		deallocateStacks(void);
bool		restoreStack(Stack s);
void		trimStacks(int resize);
void		emptyStacks(void);
void		freeStacks(void);
void		freePrologLocalData(PL_local_data_t *ld);
int		ensure_room_stack(Stack s, size_t n, int ex);
int		trim_stack(Stack s);
int		set_stack_limit(size_t limit);
const char *	signal_name(int sig);

#undef LDFUNC_DECLARATIONS

#endif /*_PL_SETUP_H*/
