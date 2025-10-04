/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2023, University of Amsterdam
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

#ifndef _PL_READ_H
#define _PL_READ_H
#include <wchar.h>

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

#if USE_LD_MACROS
#define	read_clause(s, term, options)	LDFUNC(read_clause, s, term, options)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

void		resetRead(void);
int		f_is_prolog_var_start(int c);
int		f_is_prolog_atom_start(int c);
int		f_is_prolog_identifier_continue(int c);
int		f_is_prolog_symbol(int c);
int		f_is_decimal(int c);
int		decimal_weight(int code);
int		unicode_separator(int c);
int		unicode_quoted_escape(int c);
bool		atom_varnameW(const pl_wchar_t *s, size_t len);
int		atom_is_named_var(atom_t name);
strnumstat	str_number(const unsigned char *string,
			   unsigned char **end,
			   Number value, int flags);
const char *	str_number_error(strnumstat rc);
foreign_t	pl_raw_read(term_t term);
foreign_t	pl_raw_read2(term_t stream, term_t term);
foreign_t	pl_read(term_t term);
foreign_t	pl_read2(term_t stream, term_t term);
void		initCharConversion(void);
foreign_t	pl_char_conversion(term_t in, term_t out);
foreign_t	pl_current_char_conversion(term_t in, term_t out, control_t h);
bool		read_clause(IOSTREAM *s, term_t term, term_t options);

#undef LDFUNC_DECLARATIONS

const char *	utf8_skip_blanks(const char *in);

#endif /*_PL_READ_H*/
