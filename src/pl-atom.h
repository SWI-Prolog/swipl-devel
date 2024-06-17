/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2024, University of Amsterdam
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

#ifndef _PL_ATOM_H
#define _PL_ATOM_H

#if USE_LD_MACROS
#define lookupBlob(s, len, type, new) LDFUNC(lookupBlob, s, len, type, new)
#define lookupAtom(s, len) LDFUNC(lookupAtom, s, len)
#endif


		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

#define LDFUNC_DECLARATIONS
#define checkAtoms()	checkAtoms_src(__FILE__, __LINE__)
atom_t		lookupAtom(const char *s, size_t len);
atom_t		lookupBlob(const char *s, size_t len,
			   PL_blob_t *type, int *new);
foreign_t	pl_atom_hashstat(term_t i, term_t n);
void		do_init_atoms(void);
int		resetListAtoms(void);
void		cleanupAtoms(void);
void		markAtom(atom_t a);
foreign_t	pl_garbage_collect_atoms(void);
void		resetAtoms(void);
int		checkAtoms_src(const char *file, int line);
int		is_volatile_atom(atom_t a);
size_t		atom_space(void);
#undef LDFUNC_DECLARATIONS

static inline int
isBuiltInAtom(atom_t a)
{ size_t index = indexAtom(a);
  return index < GD->atoms.builtin;
}

#endif /*_PL_ATOM_H*/
