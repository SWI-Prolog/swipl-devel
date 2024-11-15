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

#ifndef _PL_WRITE_H
#define _PL_WRITE_H

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

#if USE_LD_MACROS
#define	var_name_ptr(p, name)	LDFUNC(var_name_ptr, p, name)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

char *		var_name_ptr(Word p, char *name);
char *		varName(term_t var, char *buf);
foreign_t	pl_write_canonical(term_t term);
foreign_t	pl_write_canonical2(term_t stream, term_t term);
foreign_t	pl_write_term(term_t term, term_t options);
foreign_t	pl_write_term3(term_t stream,
			       term_t term, term_t options);
foreign_t	pl_write(term_t term);
foreign_t	pl_writeln(term_t term);
foreign_t	pl_writeq(term_t term);
foreign_t	pl_print(term_t term);
foreign_t	pl_write2(term_t stream, term_t term);
foreign_t	pl_writeln2(term_t stream, term_t term);
foreign_t	pl_writeq2(term_t stream, term_t term);
foreign_t	pl_print2(term_t stream, term_t term);
int		writeAttributeMask(atom_t name);
bool		writeUCSAtom(IOSTREAM *fd, atom_t atom, int flags);
bool		writeReservedSymbol(IOSTREAM *fd, atom_t atom, int flags);
bool		writeAtomToStream(IOSTREAM *s, atom_t atom);
size_t		format_float(char *buf, size_t size, double f, int N, char E);
int		unquoted_atom(atom_t a);
strnumstat	make_nan(double *f);
double		NaN_value(double f);

#undef LDFUNC_DECLARATIONS

#endif /*_PL_WRITE_H*/
