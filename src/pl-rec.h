/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2020, University of Amsterdam
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

#include "pl-incl.h"

#ifndef _PL_REC_H
#define _PL_REC_H

typedef enum record_az
{ RECORDA,
  RECORDZ
} record_az;

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

#if USE_LD_MACROS
#define	compileTermToHeap_ex(term, allocate, ctx, flags)	LDFUNC(compileTermToHeap_ex, term, allocate, ctx, flags)
#define	copyRecordToGlobal(copy, term, flags)			LDFUNC(copyRecordToGlobal, copy, term, flags)
#define	getKeyEx(key, k)					LDFUNC(getKeyEx, key, k)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

void		initRecords(void);
void		cleanupRecords(void);
Record		compileTermToHeap_ex(term_t term,
				     void* (*allocate)(void *ctx, size_t size),
				     void* ctx,
				     int flags);
int		copyRecordToGlobal(term_t copy, Record term,
				   int flags);
int		variantRecords(const Record r1, const Record r2);
bool		freeRecord(Record record);
void		unallocRecordRef(RecordRef r);
bool		unifyKey(term_t key, word val);
int		getKeyEx(term_t key, word *k);
word		pl_term_complexity(term_t t, term_t mx, term_t count);
void		markAtomsRecord(Record record);
int		PL_record_az(word k, term_t term, term_t ref, record_az az);

#undef LDFUNC_DECLARATIONS

#define compileTermToHeap(t, f)	\
	compileTermToHeap_ex(t, NULL, NULL, f)

#endif /*_PL_REC_H*/
