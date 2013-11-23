/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2008, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef PL_INLINE_H_INCLUDED
#define PL_INLINE_H_INCLUDED
#undef LD
#define LD LOCAL_LD


		 /*******************************
		 *	 LOCK-FREE SUPPORT	*
		 *******************************/

#ifdef _MSC_VER				/* Windows MSVC version */

/* MSB(0) = undefined
   MSB(1) = 0
   MSB(2) = 1
   ...
*/

#define HAVE_MSB 1
static inline int
MSB(size_t i)
{ unsigned long index;
#if SIZEOF_VOIDP == 8
  unsigned __int64 mask = i;
  _BitScanReverse64(&index, mask);
#else
  unsigned long mask = i;
  _BitScanReverse(&index, mask);
#endif

  return index;
}

#define HAVE_MEMORY_BARRIER 1
#ifndef MemoryBarrier
#define MemoryBarrier() (void)0
#endif

#endif /*_MSC_VER*/

#if !defined(HAVE_MSB) && defined(HAVE__BUILTIN_CLZ)
#define HAVE_MSB 1
#define MSB(i) (sizeof(long)*8 - 1 - __builtin_clzl(i)) /* GCC builtin */
#endif

#if !defined(HAVE_MEMORY_BARRIER) && defined(HAVE__SYNC_SYNCHRONIZE)
#define HAVE_MEMORY_BARRIER 1
#ifndef MemoryBarrier
#define MemoryBarrier()			__sync_synchronize()
#endif
#define ATOMIC_ADD(ptr, v)		__sync_add_and_fetch(ptr, v)
#define ATOMIC_SUB(ptr, v)		__sync_sub_and_fetch(ptr, v)
#define ATOMIC_INC(ptr)			ATOMIC_ADD(ptr, 1) /* ++(*ptr) */
#define ATOMIC_DEC(ptr)			ATOMIC_SUB(ptr, 1) /* --(*ptr) */
#define ATOMIC_OR(ptr, v)		__sync_fetch_and_or(ptr, v)
#define ATOMIC_AND(ptr, v)		__sync_fetch_and_and(ptr, v)
#define COMPARE_AND_SWAP(ptr,o,n)	__sync_bool_compare_and_swap(ptr,o,n)
#endif

#ifndef HAVE_MSB
#define HAVE_MSB 1
static inline int
MSB(size_t i)
{ int j = 0;

#if SIZEOF_VOIDP == 8
  if (i >= 0x100000000) {i >>= 32; j += 32;}
#endif
  if (i >=     0x10000) {i >>= 16; j += 16;}
  if (i >=       0x100) {i >>=  8; j +=  8;}
  if (i >=        0x10) {i >>=  4; j +=  4;}
  if (i >=         0x4) {i >>=  2; j +=  2;}
  if (i >=         0x2) j++;

  return j;
}
#endif

#ifndef HAVE_MEMORY_BARRIER
#define HAVE_MEMORY_BARRIER 1
#define MemoryBarrier() (void)0
#endif

		 /*******************************
		 *	 ATOMS/FUNCTORS		*
		 *******************************/

static inline Atom
fetchAtomArray(size_t index)
{ int idx = MSB(index);

  return GD->atoms.array.blocks[idx][index];
}


static inline FunctorDef
fetchFunctorArray(size_t index)
{ int idx = MSB(index);

  return GD->functors.array.blocks[idx][index];
}


		 /*******************************
		 *	     BITVECTOR		*
		 *******************************/

typedef uintptr_t bitv_chunk;
typedef struct bit_vector
{ size_t size;
  bitv_chunk chunk[1];				/* bits */
} bit_vector;
#define BITSPERE (sizeof(bitv_chunk)*8)

#ifndef offset
#define offset(s, f) ((size_t)(&((struct s *)NULL)->f))
#endif

static inline size_t
sizeof_bitvector(size_t bits)
{ return offset(bit_vector, chunk[(bits+BITSPERE-1)/BITSPERE]);
}

static inline void
init_bitvector(bit_vector *v, size_t bits)
{ size_t bytes = offset(bit_vector, chunk[(bits+BITSPERE-1)/BITSPERE]);

  memset(v, 0, bytes);
  v->size = bits;
}

static inline bit_vector *
new_bitvector(size_t size)
{ size_t bytes = offset(bit_vector, chunk[(size+BITSPERE-1)/BITSPERE]);
  bit_vector *v = allocHeapOrHalt(bytes);

  memset(v, 0, bytes);
  v->size = size;
  return v;
}

static inline void
free_bitvector(bit_vector *v)
{ size_t bytes = offset(bit_vector, chunk[(v->size+BITSPERE-1)/BITSPERE]);

  freeHeap(v, bytes);
}

static inline void
clear_bitvector(bit_vector *v)
{ size_t chunks = (v->size+BITSPERE-1)/BITSPERE;

  memset(v->chunk, 0, chunks*sizeof(bitv_chunk));
}

static inline void
set_bit(bit_vector *v, int which)
{ int e = which/BITSPERE;
  int b = which%BITSPERE;

  v->chunk[e] |= ((uintptr_t)1<<b);
}

static inline void
clear_bit(bit_vector *v, int which)
{ int e = which/BITSPERE;
  int b = which%BITSPERE;

  v->chunk[e] &= ~((uintptr_t)1<<b);
}

static inline int
true_bit(bit_vector *v, int which)
{ int e = which/BITSPERE;
  int b = which%BITSPERE;

  return (v->chunk[e]&((uintptr_t)1<<b)) != 0;
}


		 /*******************************
		 *	     MISC STUFF		*
		 *******************************/

static inline code
fetchop(Code PC)
{ code op = decode(*PC);

  if ( unlikely(op == D_BREAK) )
    op = decode(replacedBreak(PC));

  return op;
}


static inline Code
stepPC(Code PC)
{ code op = fetchop(PC++);

  if ( unlikely(codeTable[op].arguments == VM_DYNARGC) )
    return stepDynPC(PC, &codeTable[op]);
  else
    return PC + codeTable[op].arguments;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Mark() sets LD->mark_bar, indicating  that   any  assignment  above this
value need not be trailed.

Note that the local stack is always _above_ the global stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline void
Trail__LD(Word p, word v ARG_LD)
{ DEBUG(CHK_SECURE, assert(tTop+1 <= tMax));

  if ( (void*)p >= (void*)lBase || p < LD->mark_bar )
    (tTop++)->address = p;
  *p = v;
}


static inline void
bindConst__LD(Word p, word c ARG_LD)
{ DEBUG(CHK_SECURE, assert(hasGlobalSpace(0)));

#ifdef O_ATTVAR
  if ( isVar(*p) )
  { *p = (c);
    if ( (void*)p >= (void*)lBase || p < LD->mark_bar )
      (tTop++)->address = p;
  } else
  { assignAttVar(p, &(c) PASS_LD);
  }
#else
  *p = (c);
  if ( (void*)p >= (void*)lBase || p < LD->mark_bar )
    (tTop++)->address = p;
#endif
}


static inline word
consPtr__LD(void *p, word ts ARG_LD)
{ uintptr_t v = (uintptr_t) p;

  v -= LD->bases[ts&STG_MASK];
  DEBUG(CHK_SECURE, assert(v < MAXTAGGEDPTR && !(v&0x3)));
  return (v<<5)|ts;
}


#if ALIGNOF_DOUBLE != ALIGNOF_VOIDP
static inline double
valFloat__LD(word w ARG_LD)
{ Word p = valIndirectP(w);
  double d;

  memcpy(&d, p, sizeof(d));
  return d;
}
#endif


#endif /*PL_INLINE_H_INCLUDED*/
