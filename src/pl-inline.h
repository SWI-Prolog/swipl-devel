/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2024, University of Amsterdam
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

#ifndef PL_INLINE_H_INCLUDED
#define PL_INLINE_H_INCLUDED

#ifdef __WINDOWS__
  #include <windows.h>
  #undef small
  #include <intrin.h>
  #ifdef _MSC_VER
    #if SIZEOF_VOIDP == 8
      #pragma intrinsic(_BitScanReverse64)
    #endif
      #pragma intrinsic(_BitScanReverse)
    #endif
#endif

#include "pl-transaction.h"
#include "pl-atom.h"


		 /*******************************
		 *	 LOCK-FREE SUPPORT	*
		 *******************************/

#ifdef _MSC_VER
#define LL(x) x ## i64
#else
#define LL(x) x ## LL
#endif

#ifdef _MSC_VER				/* Windows MSVC version */

/* MSB(0) = undefined
   MSB(1) = 0
   MSB(2) = 1
   ...
*/

#define HAVE_MSB 1
static inline unsigned int
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

#if SIZEOF_VOIDP == 8
#define HAVE_MSB64 1
static inline unsigned int
MSB64(int64_t i)
{ unsigned long index;
  _BitScanReverse64(&index, i);
  return index;
}
#endif

#define MEMORY_ACQUIRE() MemoryBarrier()
#define MEMORY_RELEASE() MemoryBarrier()
#define MEMORY_BARRIER() MemoryBarrier()

static inline size_t
__builtin_popcount(size_t sz)
{
#if SIZEOF_VOIDP == 4
  return __popcnt(sz);
#else
  return __popcnt64(sz);
#endif
}

static inline int
__builtin_saddll_overflow(long long int a, long long int b, long long int *res)
{ long long int r = a + b;
  if ( (r > 0 && a < 0 && b < 0) ||
       (r < 0 && a > 0 && b > 0) )
    return true;

  *res = r;
  return false;
}

#endif /*_MSC_VER*/

#if !defined(HAVE_MSB) && defined(HAVE__BUILTIN_CLZ)
#define HAVE_MSB 1
#define HAVE_MSB64 1

static inline unsigned int
MSB(size_t i)
{
#if SIZEOF_VOIDP == SIZEOF_LONG
  return (unsigned int)sizeof(long)*8-1-__builtin_clzl(i);
#elif SIZEOF_VOIDP == SIZEOF_LONG_LONG
  return (unsigned int)sizeof(long long)*8-1-__builtin_clzll(i);
#else
#error "No MSB";
#endif
}

static inline unsigned int
MSB64(int64_t i)
{ return (unsigned int)sizeof(long long)*8-1-__builtin_clzll(i);
}
#endif

#ifdef HAVE_GCC_ATOMIC
#define MEMORY_ACQUIRE()	__atomic_thread_fence(__ATOMIC_ACQUIRE)
#define MEMORY_RELEASE()	__atomic_thread_fence(__ATOMIC_RELEASE)
#define MEMORY_BARRIER()	__atomic_thread_fence(__ATOMIC_SEQ_CST)
#endif

#if !defined(__ATOMIC_ACQUIRE) && defined _MSC_VER
/* Only used to read size_t under acquire semantics */
#define __ATOMIC_ACQUIRE 0
static inline size_t
__atomic_load_n(size_t *ptr, int memorder)
{ assert(memorder == __ATOMIC_ACQUIRE);
#if SIZEOF_VOIDP == 8
  return InterlockedOr64Acquire(ptr, 0LL);
#else
  return InterlockedOrAcquire(ptr, 0L);
#endif
}
#endif

#ifdef O_PLMT
#ifdef _MSC_VER
#define ATOMIC_ADD(ptr, v)	_InterlockedExchangeAdd64(ptr, v)
#define ATOMIC_SUB(ptr, v)	ATOMIC_ADD(ptr, -(v))
#define ATOMIC_INC(ptr)		_Generic((*ptr), \
					 int: _InterlockedIncrement((long*)ptr), \
					 unsigned int: _InterlockedIncrement((long*)ptr), \
					 size_t: _InterlockedIncrement64((__int64*)ptr), \
					 __int64: _InterlockedIncrement64((__int64*)ptr))
#define ATOMIC_DEC(ptr)		_Generic((*ptr), \
					 int: _InterlockedDecrement((long*)ptr), \
					 unsigned int: _InterlockedDecrement((long*)ptr), \
					 size_t:  _InterlockedDecrement64((__int64*)ptr), \
					 __int64: _InterlockedDecrement64((__int64*)ptr))
#define ATOMIC_OR(ptr, v)	_Generic((*ptr), \
					 unsigned short: _InterlockedOr16((short*)ptr, (short)(v)), \
					 unsigned int: _InterlockedOr((long*)ptr, (long)(v)), \
					 unsigned __int64: _InterlockedOr64((__int64*)ptr, (__int64)(v)))
#define ATOMIC_AND(ptr, v)	_Generic((*ptr), \
					 unsigned short: _InterlockedAnd16((short*)ptr, (short)(v)), \
					 unsigned int: _InterlockedAnd((long*)ptr, (long)(v)), \
					 unsigned __int64: _InterlockedAnd64((__int64*)ptr, (__int64)(v)))
#else
#define ATOMIC_ADD(ptr, v)	__atomic_add_fetch(ptr, v, __ATOMIC_SEQ_CST)
#define ATOMIC_SUB(ptr, v)	__atomic_sub_fetch(ptr, v, __ATOMIC_SEQ_CST)
#define ATOMIC_INC(ptr)		ATOMIC_ADD(ptr, 1) /* ++(*ptr) */
#define ATOMIC_DEC(ptr)		ATOMIC_SUB(ptr, 1) /* --(*ptr) */
#define ATOMIC_OR(ptr, v)	__atomic_fetch_or(ptr, v, __ATOMIC_SEQ_CST)
#define ATOMIC_AND(ptr, v)	__atomic_fetch_and(ptr, v, __ATOMIC_SEQ_CST)
#endif

#define __COMPARE_AND_SWAP(at, from, to) \
	__atomic_compare_exchange_n(at, &(from), to, false, \
				    __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)

static inline int
COMPARE_AND_SWAP_PTR(void *at, void *from, void *to)
{
#ifdef _MSC_VER
# if SIZEOF_VOIDP == 4
  return _InterlockedCompareExchange(at, (long)to, (long)from) == (long)from;
# else
  return _InterlockedCompareExchange64(at, (int64_t)to, (int64_t)from) == (int64_t)from;
#endif
#else
  void **ptr = at;
  return __COMPARE_AND_SWAP(ptr, from, to);
#endif
}

static inline int
COMPARE_AND_SWAP_INT64(int64_t *at, int64_t from, int64_t to)
{
#ifdef _MSC_VER
  return _InterlockedCompareExchange64(at, to, from) == from;
#else
  return __COMPARE_AND_SWAP(at, from, to);
#endif
}

static inline int
COMPARE_AND_SWAP_UINT64(uint64_t *at, uint64_t from, uint64_t to)
{
#ifdef _MSC_VER
  return _InterlockedCompareExchange64((int64_t *)at, (int64_t)to, (int64_t)from) == from;
#else
  return __COMPARE_AND_SWAP(at, from, to);
#endif
}

static inline int
COMPARE_AND_SWAP_INT(int *at, int from, int to)
{
#ifdef _MSC_VER /* sizeof(int) == sizeof(long) */
  return _InterlockedCompareExchange(at, to, from) == from;
#else
  return __COMPARE_AND_SWAP(at, from, to);
#endif
}

static inline int
COMPARE_AND_SWAP_UINT(unsigned int *at, unsigned int from, unsigned int to)
{
#ifdef _MSC_VER /* sizeof(int) == sizeof(long) */
  return _InterlockedCompareExchange((long*)at, (long)to, (long)from) == (long)from;
#else
  return __COMPARE_AND_SWAP(at, from, to);
#endif
}

static inline int
COMPARE_AND_SWAP_SIZE(size_t *at, size_t from, size_t to)
{
#ifdef _MSC_VER
# if SIZEOF_VOIDP == 4
  return _InterlockedCompareExchange(at, to, from) == from;
# else
  return _InterlockedCompareExchange64(at, to, from) == from;
#endif
#else
  return __COMPARE_AND_SWAP(at, from, to);
#endif
}

static inline int
COMPARE_AND_SWAP_WORD(word *at, word from, word to)
{
#ifdef _MSC_VER
# if SIZEOF_VOIDP == 4
  return _InterlockedCompareExchange(at, to, from) == from;
# else
  return _InterlockedCompareExchange64(at, to, from) == from;
#endif
#else
  return __COMPARE_AND_SWAP(at, from, to);
#endif
}

static inline int
COMPARE_AND_SWAP_ATOM(atom_t *at, atom_t from, atom_t to)
{
#ifdef _MSC_VER
# if SIZEOF_ATOM == 4
  return _InterlockedCompareExchange(at, to, from) == from;
# else
  return _InterlockedCompareExchange64(at, to, from) == from;
#endif
#else
  return __COMPARE_AND_SWAP(at, from, to);
#endif
}

#else
#define ATOMIC_ADD(ptr, v)		(*ptr += v)
#define ATOMIC_SUB(ptr, v)		(*ptr -= v)
#define ATOMIC_INC(ptr)			(++(*ptr))
#define ATOMIC_DEC(ptr)			(--(*ptr))
#define ATOMIC_OR(ptr, v)		(*ptr |= v)
#define ATOMIC_AND(ptr, v)		(*ptr &= v)
#define COMPARE_AND_SWAP(ptr,o,n)	(*ptr == o ? (*ptr = n), 1 : 0)
#define COMPARE_AND_SWAP_PTR(ptr,o,n)	COMPARE_AND_SWAP(ptr,o,n)
#define COMPARE_AND_SWAP_INT64(ptr,o,n)	COMPARE_AND_SWAP(ptr,o,n)
#define COMPARE_AND_SWAP_UINT64(ptr,o,n) COMPARE_AND_SWAP(ptr,o,n)
#define COMPARE_AND_SWAP_INT(ptr,o,n)	COMPARE_AND_SWAP(ptr,o,n)
#define COMPARE_AND_SWAP_UINT(ptr,o,n)	COMPARE_AND_SWAP(ptr,o,n)
#define COMPARE_AND_SWAP_SIZE(ptr,o,n)	COMPARE_AND_SWAP(ptr,o,n)
#define COMPARE_AND_SWAP_WORD(ptr,o,n)	COMPARE_AND_SWAP(ptr,o,n)
#define COMPARE_AND_SWAP_ATOM(ptr,o,n)	COMPARE_AND_SWAP(ptr,o,n)
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


#ifndef HAVE_MSB64
#define HAVE_MSB64 1
static inline int
MSB64(int64_t i)
{ int j = 0;

  if (i >= LL(0x100000000)) {i >>= 32; j += 32;}
  if (i >=     LL(0x10000)) {i >>= 16; j += 16;}
  if (i >=       LL(0x100)) {i >>=  8; j +=  8;}
  if (i >=	  LL(0x10)) {i >>=  4; j +=  4;}
  if (i >=         LL(0x4)) {i >>=  2; j +=  2;}
  if (i >=         LL(0x2)) j++;

  return j;
}
#endif


#ifndef MEMORY_BARRIER
#define MEMORY_BARRIER() (void)0
#define MEMORY_ACQUIRE() (void)0
#define MEMORY_RELEASE() (void)0
#endif

#ifndef max
#define max(x,y) ( \
	{ __auto_type __x = (x); __auto_type __y = (y); \
          __x > __y ? __x : __y; \
	})
#define min(x,y) ( \
	{ __auto_type __x = (x); __auto_type __y = (y); \
          __x < __y ? __x : __y; \
	})
#endif

		 /*******************************
		 *	 ATOMS/FUNCTORS		*
		 *******************************/

static inline void
initAtoms(void)
{ if ( !likely(GD->atoms.initialised) )
    do_init_atoms();
}

static inline Atom
fetchAtomArray(size_t index)
{ int idx = MSB(index);

  return &GD->atoms.array.blocks[idx][index];
}


static inline FunctorDef
fetchFunctorArray(size_t index)
{ int idx = MSB(index);

  return GD->functors.array.blocks[idx][index];
}

#define pushVolatileAtom(a) LDFUNC(pushVolatileAtom, a)
static inline void
pushVolatileAtom(DECL_LD atom_t a)
{ LD->atoms.unregistering = a;
  if ( GD->atoms.gc_active )
    markAtom(a);
}



		 /*******************************
		 *	     BITVECTOR		*
		 *******************************/

typedef unsigned int bitv_chunk;
typedef struct bit_vector
{ size_t size;
  bitv_chunk chunk[];				/* bits */
} bit_vector;

#define BITSPERE (sizeof(bitv_chunk)*8)
#define CHUNKS_BITVECTOR(bits) (((bits)+BITSPERE-1)/BITSPERE)
#define SIZEOF_BITVECTOR(bits) offsetof(struct bit_vector, \
					chunk[CHUNKS_BITVECTOR(bits)])

static inline size_t
sizeof_bitvector(size_t bits)
{ return offsetof(struct bit_vector, chunk[(bits+BITSPERE-1)/BITSPERE]);
}

static inline void
init_bitvector(bit_vector *v, size_t bits)
{ size_t bytes = SIZEOF_BITVECTOR(bits);

  memset(v, 0, bytes);
  v->size = bits;
}

#define local_bitvector(name, bits) \
  bit_vector *name = alloca(SIZEOF_BITVECTOR(bits)); \
  memset(name->chunk, 0, CHUNKS_BITVECTOR(bits)*sizeof(*name->chunk)); \
  name->size = bits;

static inline bit_vector *
new_bitvector(size_t size)
{ size_t bytes = SIZEOF_BITVECTOR(size);
  bit_vector *v = allocHeapOrHalt(bytes);

  memset(v, 0, bytes);
  v->size = size;
  return v;
}

static inline void
free_bitvector(bit_vector *v)
{ size_t bytes = SIZEOF_BITVECTOR(v->size);

  freeHeap(v, bytes);
}

static inline void
clear_bitvector(bit_vector *v)
{ size_t chunks = CHUNKS_BITVECTOR(v->size);

  memset(v->chunk, 0, chunks*sizeof(bitv_chunk));
}

static inline void
setall_bitvector(bit_vector *v)
{ size_t chunks = CHUNKS_BITVECTOR(v->size);

  memset(v->chunk, 0xff, chunks*sizeof(bitv_chunk));
}

static inline bool		/* true when set, false when already set */
set_bit(bit_vector *v, size_t which)
{ size_t e = which/BITSPERE;
  size_t b = which%BITSPERE;

  bool rc = (v->chunk[e]&((bitv_chunk)1<<b)) == 0;
  v->chunk[e] |= ((bitv_chunk)1<<b);
  return rc;
}

static inline void
clear_bit(bit_vector *v, size_t which)
{ size_t e = which/BITSPERE;
  size_t b = which%BITSPERE;

  v->chunk[e] &= ~((bitv_chunk)1<<b);
}

static inline bool
true_bit(bit_vector *v, size_t which)
{ size_t e = which/BITSPERE;
  size_t b = which%BITSPERE;

  return (v->chunk[e]&((bitv_chunk)1<<b)) != 0;
}

static inline size_t
popcount_bitvector(const bit_vector *v)
{ const bitv_chunk *p = v->chunk;
  int cnt = (int)(v->size+BITSPERE-1)/BITSPERE;
  size_t bits = 0;

  while( cnt-- > 0 )
    bits += __builtin_popcount(*p++);

  return bits;
}


		 /*******************************
		 *	     MISC STUFF		*
		 *******************************/

static int	  same_type_numbers(Number n1, Number n2) WUNUSED;

static inline int
same_type_numbers(Number n1, Number n2)
{ if ( n1->type == n2->type )
    return true;
  return make_same_type_numbers(n1, n2);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Mark() sets LD->mark_bar, indicating  that   any  assignment  above this
value need not be trailed.

Note that the local stack is always _above_ the global stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define Trail(p, v) LDFUNC(Trail, p, v)
static inline void
Trail(DECL_LD Word p, word v)
{ DEBUG(CHK_SECURE, assert(tTop+1 <= tMax));

  if ( (void*)p >= (void*)lBase || p < LD->mark_bar )
    (tTop++)->address = p;
  *p = v;
}


static inline word
consPtr(void *p, word ts)
{ word v = ptr2word(p);

  return (v<<LMASK_BITS)|ts;
}

static inline Word
valPtr(word w)
{ return word2ptr(Word, w>>LMASK_BITS);
}

#if ALIGNOF_DOUBLE == ALIGNOF_VOIDP
#define valFloat(w) (*(double *)valIndirectP(w))
#else
#define valFloat(w) LDFUNC(valFloat, w)
static inline double
valFloat(DECL_LD word w)
{ Word p = valIndirectP(w);
  double d;

  memcpy(&d, p, sizeof(d));
  return d;
}
#endif


#define valHandle(r) LDFUNC(valHandle, r)
static inline word
valHandle(DECL_LD term_t r)
{ Word p = valTermRef(r);

  deRef(p);
  return *p;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
linkValI(p)  is  an  inlined  version  of  linkVal()  that  assumes  the
dereferenced value of `p` lives on  the   global  stack. It has no error
checking (unless compiled for debugging).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline word
linkValI(Word p)
{ word w = *p;

  while(isRef(w))
  { p = unRef(w);
    w = *p;
  }

  DEBUG(CHK_ATOM_GARBAGE_COLLECTED, assert(w != ATOM_garbage_collected));

  if ( !needsRef(w) )
  { return w;
  } else
  { DEBUG(0, assert(p<(Word)lBase));
    return makeRefG(p);
  }
}

#define is_signalled(_) LDFUNC(is_signalled, _)
static inline bool
is_signalled(DECL_LD)
{ sigmask_t msk = 0;

  if ( HAS_LD )
  { for (int i = 0; i < SIGMASK_WORDS; i++)
      msk |= LD->signal.pending[i];
  }

  return !!msk;
}

#define register_attvar(gp) LDFUNC(register_attvar, gp)
static inline void
register_attvar(DECL_LD Word gp)
{ if ( LD->attvar.attvars )
  { *gp = makeRefG(LD->attvar.attvars);
    DEBUG(MSG_ATTVAR_LINK,
	  Sdprintf("Linking %p -> %p\n", gp, LD->attvar.attvars));
  } else
  { DEBUG(MSG_ATTVAR_LINK,
	  Sdprintf("Attvar chain head at %p\n", gp));
    setVar(*gp);
  }

  LD->attvar.attvars = gp;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
True when clause  is visible at generation.  This notably  needs to be
critically aligned with committing  or discarding transactions.  These
operations       update      cl->generation.erased       and      next
cl->generation.created.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define visibleClause(cl, gen) LDFUNC(visibleClause, cl, gen)
static inline bool
visibleClause(DECL_LD Clause cl, gen_t gen)
{ gen_t c, e;

  c = cl->generation.created;
  MEMORY_ACQUIRE();
  e = cl->generation.erased;

  if ( unlikely(e == LD->reload.generation) )
    return false;
  if ( unlikely(c == LD->reload.generation) )
    return true;

  if ( c <= gen && e > gen )
    return true;

  if ( unlikely(LD->transaction.gen_base && gen >= LD->transaction.gen_base) &&
       ison(cl->predicate, P_TRANSACT) )
    return transaction_visible_clause(cl, gen);

  return false;
}

#define visibleClauseCNT(cl, gen) LDFUNC(visibleClauseCNT, cl, gen)
static inline bool
visibleClauseCNT(DECL_LD Clause cl, gen_t gen)
{ if ( likely(visibleClause(cl, gen)) )
    return true;
  LD->clauses.erased_skipped++;
  return false;
}

static inline gen_t
global_generation(void)
{ return GD->_generation;
}

#define current_generation(def) LDFUNC(current_generation, def)
static inline gen_t
current_generation(DECL_LD Definition def)
{ if ( unlikely(!!LD->transaction.generation) && def && ison(def, P_TRANSACT) )
  { return LD->transaction.generation;
  } else
  { return GD->_generation;
  }
}

#define next_generation(def) LDFUNC(next_generation, def)
static inline gen_t
next_generation(DECL_LD Definition def)
{ if ( unlikely(!!LD->transaction.generation) && def && ison(def, P_TRANSACT) )
  { if ( LD->transaction.generation < LD->transaction.gen_max )
      return ++LD->transaction.generation;
    return 0;
  } else
  { gen_t gen;

    PL_LOCK(L_GENERATION);
    gen = ++GD->_generation;
    PL_UNLOCK(L_GENERATION);

    return gen;
  }
}

#define max_generation(def) LDFUNC(max_generation, def)
static inline gen_t
max_generation(DECL_LD Definition def)
{ if ( unlikely(!!LD->transaction.generation) && def && ison(def, P_TRANSACT) )
    return LD->transaction.gen_max;
  else
    return GEN_MAX;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We must ensure that cleanDefinition() does   not remove clauses that are
valid   for   the   generation   in   the   frame.   This   means   that
pl_garbage_collect_clauses() must either pick  up   the  generation from
this frame using markPredicatesInEnvironments() or  the start generation
of pl_garbage_collect_clauses() is older than  what   is  stored in this
frame.  This  loop  ensure  that  if    CGC  has  been  running  between
global_generation()  and  storing  the  generation  in  our  frame,  our
generation is updated and thus no harm is done.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define setGenerationFrame(fr) LDFUNC(setGenerationFrame, fr)
static inline void
setGenerationFrame(DECL_LD LocalFrame fr)
{ if ( unlikely(LD->transaction.generation &&
		ison(fr->predicate, P_TRANSACT)) )
  { setGenerationFrameVal(fr, LD->transaction.generation);
  } else
  { gen_t gen;

    do
    { gen = global_generation();
      setGenerationFrameVal(fr, gen);
    } while(gen != global_generation());
  }
}

static inline QueryFrame
QueryFromQid(QueryRef qid)
{ return (QueryFrame) (&((Word)qid->engine->stacks.local.base)[qid->offset]);
}

		 /*******************************
		 *	      INDEXING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
murmur_key() is used to quickly compute  a   key  from indirect data for
clause indexing. ptr is the start of the  data. It is word aligned and n
is the size in bytes, a multiple of sizeof(word). If the indirect is too
long we has based on the start, end and length.

The hash should not conflict  with   a  functor_t  (hence the STG_GLOBAL
mask) and may never be 0.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define KEY_INDEX_MAX 4

static inline word
clean_index_key(word key)
{ key &= ~((word)STG_GLOBAL);
  if ( !key ) key = 1;
  return key;
}

static inline word
murmur_key(const void *ptr, size_t n)
{ word k;

  DEBUG(0, assert(n%sizeof(word) == 0));

  if ( n > sizeof(word)*KEY_INDEX_MAX )
  { word data[KEY_INDEX_MAX];
    const Word in = (const Word)ptr;

    for(size_t i=0; i<KEY_INDEX_MAX-2; i++)
      data[i] = in[i];
    data[KEY_INDEX_MAX-2] = in[n/sizeof(word)-1];
    data[KEY_INDEX_MAX-1] = n;

    k = MurmurHashAligned2(data, sizeof(word)*4, MURMUR_SEED);
  } else
  { k = MurmurHashAligned2(ptr, n, MURMUR_SEED);
  }

  return clean_index_key(k);
}

		 /*******************************
		 *	      THREADS		*
		 *******************************/

#ifdef O_PLMT
#define acquire_ldata(info) LDFUNC(acquire_ldata, info)
static inline PL_local_data_t *
acquire_ldata(DECL_LD PL_thread_info_t *info)
{ PL_local_data_t *ld = info->thread_data;
  LD->thread.info->access.ldata = ld;
  if ( ld && ld->magic == LD_MAGIC )
    return ld;
  LD->thread.info->access.ldata = NULL;
  return NULL;
}
#endif


		 /*******************************
		 *     POINTER <-> PROLOG INT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Pointers are not a special type in Prolog. Instead, they are represented
by an integer. The funtions below convert   integers  such that they can
normally be expressed as a tagged  integer: the heap_base is subtracted,
it is divided by 4 and the low 2   bits  are placed at the top (they are
normally 0). longToPointer() does the inverse operation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline uintptr_t
pointerToInt(void *ptr)
{
#if SIZEOF_VOIDP == 8
  return (uintptr_t) ptr;
#else
  uintptr_t p   = (uintptr_t) ptr;
  uintptr_t low = p & 0x3L;

  p >>= 2;
  p |= low<<(sizeof(uintptr_t)*8-2);

  return p;
#endif
}


static inline void *
intToPointer(uintptr_t p)
{
#if SIZEOF_VOIDP == 8
  return (void*) p;
#else
  uintptr_t low = p >> (sizeof(uintptr_t)*8-2);

  p <<= 2;
  p |= low;

  return (void *) p;
#endif
}

#endif /*PL_INLINE_H_INCLUDED*/
