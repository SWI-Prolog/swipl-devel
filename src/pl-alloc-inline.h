/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021-2024, VU University Amsterdam
			      CWI, Amsterdam
			      SWI-Prolog Soutions b.v.
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

#ifndef _PL_ALLOC_INLINE_H
#define _PL_ALLOC_INLINE_H
#include "pl-comp.h"

ALLOC_INLINE size_t					/* size in cells */
gsizeIndirectFromCode(Code pc)
{ Word wpc = (Word)pc;
  return wsizeofInd(wpc[0]) + 2;
}

/* The VM_ alternatives of these functions pass and return pc, to avoid needing to
 * store it in a memory address */
ALLOC_INLINE struct word_and_Code
VM_globalIndirectFromCode(DECL_LD Code pc)
{ word m;
  Word data;
  pc = code_get_indirect(pc, &m, &data);
  size_t n = wsizeofInd(m);
  Word p = allocGlobal(n+2);

  if ( p )
  { word r = consPtr(p, tag(m)|STG_GLOBAL);

    *p++ = m;
    memcpy(p, data, sizeof(*data)*n);
    p += n;
    *p = m;

    return WORD_AND_CODE(r, pc);
  } else
    return WORD_AND_CODE(0, pc);
}

ALLOC_INLINE struct word_and_Code				/* used in pl-wam.c */
VM_equalIndirectFromCode(DECL_LD word a, Code pc)
{ word m;
  Word data;
  pc = code_get_indirect(pc, &m, &data);
  Word pa = addressIndirect(a);

  if ( m == *pa )
  { size_t n = wsizeofInd(m);

    pa++;
    return WORD_AND_CODE(memcmp(data, pa, sizeof(*data)*n) == 0, pc);
  }

  return WORD_AND_CODE(false, pc);
}

#endif
