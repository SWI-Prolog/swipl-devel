/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, University of Amsterdam
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


#ifndef _PL_TRANSACTION_H
#define _PL_TRANSACTION_H

#define TR_TRANSACTION		0x0001
#define TR_SNAPSHOT		0x0002
#define TR_BULK			0x0004

#define GEN_TR_ASSERT_ERASE		2
#define GEN_TR_DISCARD_ASSERT		3
#define GEN_TR_DISCARD_ASSERT_ERASE	4

#if USE_LD_MACROS
#define	transaction_retract_clause(clause)		LDFUNC(transaction_retract_clause, clause)
#define	transaction_assert_clause(clause, where)	LDFUNC(transaction_assert_clause, clause, where)
#define	transaction_visible_clause(cl, gen)		LDFUNC(transaction_visible_clause, cl, gen)
#define	transaction_last_modified_predicate(def)	LDFUNC(transaction_last_modified_predicate, def)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

int	transaction_retract_clause(Clause clause);
int	transaction_assert_clause(Clause clause, ClauseRef where);
int	transaction_visible_clause(Clause cl, gen_t gen);
gen_t	transaction_last_modified_predicate(Definition def);
void	transaction_set_last_modified(Definition def,
				      gen_t gen, int flags);

#undef LDFUNC_DECLARATIONS

#endif /*_PL_TRANSACTION_H*/
