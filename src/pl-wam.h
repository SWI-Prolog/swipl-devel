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

#ifndef _PL_WAM_H
#define _PL_WAM_H

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

#if USE_LD_MACROS
#define	TrailAssignment(p)		LDFUNC(TrailAssignment, p)
#define	getLocalProcDefinition(def)	LDFUNC(getLocalProcDefinition, def)
#define	PL_open_foreign_frame(_)	LDFUNC(PL_open_foreign_frame, _)
#define	PL_close_foreign_frame(id)	LDFUNC(PL_close_foreign_frame, id)
#define	PL_next_solution(qid)		LDFUNC(PL_next_solution, qid)
#define	foreignWakeup(ex)		LDFUNC(foreignWakeup, ex)
#define	existingChoice(ch)		LDFUNC(existingChoice, ch)
#define	existingFrame(fr)		LDFUNC(existingFrame, fr)
#define grow_trail_ptr(p)		LDFUNC(grow_trail_ptr, p)
#define handles_unwind(qid, flags)	LDFUNC(handles_unwind, qid, flags)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

word		pl_count(void);
void		TrailAssignment(Word p);
void		do_undo(mark *m);
Definition	getLocalProcDefinition(Definition def);
Definition	getProcDefinitionForThread(Definition def, unsigned int tid);
void		destroyLocalDefinition(Definition def, unsigned int tid);
void		fix_term_ref_count(void);
fid_t		PL_open_foreign_frame(void);
void		PL_close_foreign_frame(fid_t id);
fid_t		PL_open_signal_foreign_frame(int sync);
int		PL_next_solution(qid_t qid);
bool		foreignWakeup(term_t ex);
void		resumeAfterException(int clear, Stack outofstack);
void		updateAlerted(PL_local_data_t *ld);
bool		raiseSignal(PL_local_data_t *ld, int sig);
int		pendingSignal(PL_local_data_t *ld, int sig);
Module		contextModule(LocalFrame fr);
void		setContextModule(LocalFrame fr, Module context);
bool		existingChoice(Choice ch);
bool		existingFrame(LocalFrame fr);
bool		grow_trail_ptr(Word p);
bool		handles_unwind(qid_t qid, unsigned int flags);
#ifdef O_DEBUG
char *		chp_chars(Choice ch);
#endif
void		initVM(void);

#undef LDFUNC_DECLARATIONS

#define getProcDefinition(proc)	getLocalProcDefinition(proc->definition)

#define trail_ptr(p) LDFUNC(trail_ptr, p)
static inline bool
trail_ptr(DECL_LD Word p)
{ if ( hasTrailSpace(1) )
  { (tTop++)->address = p;
    return true;
  }

  return grow_trail_ptr(p);
}

#endif /*_PL_WAM_H*/
