#ifndef _PL_ALLOC_INLINE_H
#define _PL_ALLOC_INLINE_H

ALLOC_INLINE size_t					/* size in cells */
gsizeIndirectFromCode(Code pc)
{ return wsizeofInd(pc[0]) + 2;
}

/* The VM_ alternatives of these functions pass and return pc, to avoid needing to
 * store it in a memory address */
ALLOC_INLINE struct word_and_Code
VM_globalIndirectFromCode(DECL_LD Code pc)
{ word m = *pc++;
  size_t n = wsizeofInd(m);
  Word p = allocGlobal(n+2);

  if ( p )
  { word r = consPtr(p, tag(m)|STG_GLOBAL);

    *p++ = m;
    while(n-- > 0)
      *p++ = *pc++;
    *p++ = m;

    return WORD_AND_CODE(r, pc);
  } else
    return WORD_AND_CODE(0, pc);
}

ALLOC_INLINE struct word_and_Code				/* used in pl-wam.c */
VM_equalIndirectFromCode(DECL_LD word a, Code pc)
{ Code orig_pc = pc;
  Word pa = addressIndirect(a);

  if ( *pc == *pa )
  { size_t n = wsizeofInd(*pc);

    while(n-- > 0)
    { if ( *++pc != *++pa )
	return WORD_AND_CODE(FALSE, orig_pc);
    }
    pc++;
    return WORD_AND_CODE(TRUE, pc);
  }

  return WORD_AND_CODE(FALSE, orig_pc);
}

#endif