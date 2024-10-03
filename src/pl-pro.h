/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2024, University of Amsterdam
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

#ifndef _PL_PRO_H
#define _PL_PRO_H

#if USE_LD_MACROS
#define	raise_halt_exception(code, force) \
	LDFUNC(raise_halt_exception, code, force)
#endif /*USE_LD_MACROS*/

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

#define LDFUNC_DECLARATIONS
foreign_t	pl_break(void);
int		currentBreakLevel(void);
bool		callProlog(Module module, term_t goal, int flags, term_t *ex);
bool		abortProlog(void);
bool		raise_halt_exception(int code, bool force);
bool		prologToplevel(atom_t toplevel);
int		query_loop(atom_t goal, bool loop);
foreign_t	pl_metacut(void);
word		checkDataEx(Word p, int flags);
int		getAccessLevelMask(atom_t a, access_level_t *val);
atom_t		accessLevel(void);
#undef LDFUNC_DECLARATIONS

#endif /*_PL_PRO_H*/
