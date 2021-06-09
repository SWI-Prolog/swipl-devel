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

#ifndef _PL_WAM_H
#define _PL_WAM_H

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

word		pl_count(void);
void		TrailAssignment__LD(Word p ARG_LD);
void		do_undo(mark *m);
Definition	getProcDefinition__LD(Definition def ARG_LD);
Definition	getProcDefinitionForThread(Definition def, unsigned int tid);
void		destroyLocalDefinition(Definition def, unsigned int tid);
void		fix_term_ref_count(void);
fid_t		PL_open_foreign_frame__LD(ARG1_LD);
void		PL_close_foreign_frame__LD(fid_t id ARG_LD);
fid_t		PL_open_signal_foreign_frame(int sync);
int		foreignWakeup(term_t ex ARG_LD);
void		resumeAfterException(int clear, Stack outofstack);
void		updateAlerted(PL_local_data_t *ld);
int		raiseSignal(PL_local_data_t *ld, int sig);
int		pendingSignal(PL_local_data_t *ld, int sig);
Module		contextModule(LocalFrame fr);
void		setContextModule(LocalFrame fr, Module context);
int		existingChoice(Choice ch ARG_LD);
#ifdef O_DEBUG
char *		chp_chars(Choice ch);
#endif

		 /*******************************
		 *	LD-USING FUNCTIONS	*
		 *******************************/

#define TrailAssignment(p)	TrailAssignment__LD(p PASS_LD)
#define getProcDefinition(proc)	getProcDefinition__LD(proc->definition PASS_LD)
#define PL_open_foreign_frame() PL_open_foreign_frame__LD(PASS_LD1)
#define PL_close_foreign_frame(id) PL_close_foreign_frame__LD(id PASS_LD)

#endif /*_PL_WAM_H*/