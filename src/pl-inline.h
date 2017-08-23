/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2017, University of Amsterdam
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
#undef LD
#define LD LOCAL_LD


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

#define HAVE_MSB64 1
static inline int
MSB64(int64_t i)
{ unsigned long index;
  _BitScanReverse64(&index, i);
  return index;
}


#define HAVE_MEMORY_BARRIER 1
#ifndef MemoryBarrier
#define MemoryBarrier() (void)0
#endif

#endif /*_MSC_VER*/

#if !defined(HAVE_MSB) && defined(HAVE__BUILTIN_CLZ)
#if SIZEOF_VOIDP == SIZEOF_LONG
#define MSB(i) ((int)sizeof(long)*8-1-__builtin_clzl(i)) /* GCC builtin */
#define HAVE_MSB 1
#elif SIZEOF_VOIDP == SIZEOF_LONG_LONG
#define MSB(i) ((int)sizeof(long long)*8-1-__builtin_clzll(i)) /* GCC builtin */
#define HAVE_MSB 1
#endif
#define HAVE_MSB64 1
#define MSB64(i) ((int)sizeof(long long)*8-1-__builtin_clzll(i))
#endif

#if !defined(HAVE_MEMORY_BARRIER) && defined(HAVE__SYNC_SYNCHRONIZE)
#define HAVE_MEMORY_BARRIER 1
#ifndef MemoryBarrier
#define MemoryBarrier()			__sync_synchronize()
#endif
#endif

#ifdef O_PLMT
#define ATOMIC_ADD(ptr, v)		__sync_add_and_fetch(ptr, v)
#define ATOMIC_SUB(ptr, v)		__sync_sub_and_fetch(ptr, v)
#define ATOMIC_INC(ptr)			ATOMIC_ADD(ptr, 1) /* ++(*ptr) */
#define ATOMIC_DEC(ptr)			ATOMIC_SUB(ptr, 1) /* --(*ptr) */
#define ATOMIC_OR(ptr, v)		__sync_fetch_and_or(ptr, v)
#define ATOMIC_AND(ptr, v)		__sync_fetch_and_and(ptr, v)
#define COMPARE_AND_SWAP(ptr,o,n)	__sync_bool_compare_and_swap(ptr,o,n)
#else
#define ATOMIC_ADD(ptr, v)		(*ptr += v)
#define ATOMIC_SUB(ptr, v)		(*ptr -= v)
#define ATOMIC_INC(ptr)			(++(*ptr))
#define ATOMIC_DEC(ptr)			(--(*ptr))
#define ATOMIC_OR(ptr, v)		(*ptr |= v)
#define ATOMIC_AND(ptr, v)		(*ptr &= v)
#define COMPARE_AND_SWAP(ptr,o,n)	(*ptr == o ? (*ptr = n), 1 : 0)
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


#ifndef HAVE_MEMORY_BARRIER
#define HAVE_MEMORY_BARRIER 1
#define MemoryBarrier() (void)0
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

static inline void
pushVolatileAtom__LD(atom_t a ARG_LD)
{ LD->atoms.unregistering = a;
  if ( GD->atoms.gc_active )
    markAtom(a);
}

#define pushVolatileAtom(a) pushVolatileAtom__LD(a PASS_LD)


		 /*******************************
		 *	     BITVECTOR		*
		 *******************************/

typedef unsigned int bitv_chunk;
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
setall_bitvector(bit_vector *v)
{ size_t chunks = (v->size+BITSPERE-1)/BITSPERE;

  memset(v->chunk, 0xff, chunks*sizeof(bitv_chunk));
}

static inline void
set_bit(bit_vector *v, size_t which)
{ size_t e = which/BITSPERE;
  size_t b = which%BITSPERE;

  v->chunk[e] |= ((bitv_chunk)1<<b);
}

static inline void
clear_bit(bit_vector *v, size_t which)
{ size_t e = which/BITSPERE;
  size_t b = which%BITSPERE;

  v->chunk[e] &= ~((bitv_chunk)1<<b);
}

static inline int
true_bit(bit_vector *v, size_t which)
{ size_t e = which/BITSPERE;
  size_t b = which%BITSPERE;

  return (v->chunk[e]&((bitv_chunk)1<<b)) != 0;
}

static inline size_t
popcount_bitvector(const bit_vector *v)
{ const bitv_chunk *p = v->chunk;
  int cnt = (v->size+BITSPERE-1)/BITSPERE;
  size_t bits = 0;

  while( cnt-- > 0 )
    bits += __builtin_popcount(*p++);

  return bits;
}


		 /*******************************
		 *	     MISC STUFF		*
		 *******************************/

static int	  same_type_numbers(Number n1, Number n2) WUNUSED;
static Definition lookupDefinition(functor_t f, Module m) WUNUSED;

static inline int
same_type_numbers(Number n1, Number n2)
{ if ( n1->type == n2->type )
    return TRUE;
  return make_same_type_numbers(n1, n2);
}


static inline Definition
lookupDefinition(functor_t f, Module m)
{ Procedure proc = lookupProcedure(f, m);

  return proc ? proc->definition : NULL;
}


static inline code
fetchop(Code PC)
{ code op = decode(*PC);

  if ( unlikely(op == D_BREAK) )
    op = decode(replacedBreak(PC));

  return op;
}


static inline code			/* caller must hold the L_BREAK lock */
fetchop_unlocked(Code PC)
{ code op = decode(*PC);

  if ( unlikely(op == D_BREAK) )
    op = decode(replacedBreakUnlocked(PC));

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


static inline Code
stepPC_unlocked(Code PC)
{ code op = fetchop_unlocked(PC++);

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

static inline int
is_signalled(ARG1_LD)
{ return HAS_LD && unlikely((LD->signal.pending[0]|LD->signal.pending[1]) != 0);
}

static inline void
register_attvar(Word gp ARG_LD)
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

static inline int
visibleClause__LD(Clause cl, gen_t gen ARG_LD)
{ return VISIBLE_CLAUSE(cl, gen);
}

static inline int
visibleClauseCNT__LD(Clause cl, gen_t gen ARG_LD)
{ if ( likely(visibleClause__LD(cl, gen PASS_LD)) )
    return TRUE;
  LD->clauses.erased_skipped++;
  return FALSE;
}

#ifdef ATOMIC_GENERATION_HACK
/* Work around lacking 64-bit atomic operations.  These are designed to
   be safe if we assume that read and increment complete before other
   threads incremented 4G generations.
*/

static inline gen_t
global_generation(void)
{ gen_t g;
  gen_t last;

  do
  { last = GD->_last_generation;
    g = (gen_t)GD->_generation.gen_u<<32 | GD->_generation.gen_l;
  } while ( unlikely(g < last) );

  if ( unlikely(last != g) )
    GD->_last_generation = g;

  return g;
}

static inline gen_t
next_global_generation(void)
{ uint32_t u = GD->_generation.gen_u;
  uint32_t l;

  if ( unlikely((l=ATOMIC_INC(&GD->_generation.gen_l)) == 0) )
    u = ATOMIC_INC(&GD->_generation.gen_u);

  return (gen_t)u<<32|l;
}
#endif /*ATOMIC_GENERATION_HACK*/

static inline int WUNUSED
callEventHook(pl_event_type ev, ...)
{
#ifdef O_DEBUGGER
  if ( PROCEDURE_event_hook1->definition->impl.any )
  { va_list args;
    int rc;

    va_start(args, ev);
    rc = PL_call_event_hook_va(ev, args);
    va_end(args);

    return rc;
  }
#endif

  return TRUE;
}

static inline int
ensureLocalSpace__LD(size_t bytes ARG_LD)
{ int rc;

  if ( likely(addPointer(lTop, bytes) <= (void*)lMax) )
    return TRUE;

  if ( (rc=growLocalSpace__LD(bytes, ALLOW_SHIFT PASS_LD)) == TRUE )
    return TRUE;

  return raiseStackOverflow(rc);
}


		 /*******************************
		 *	     ARITHMETIC		*
		 *******************************/

static inline Number
allocArithStack(ARG1_LD)
{ if ( unlikely(LD->arith.stack.top == LD->arith.stack.max) )
    return growArithStack(PASS_LD1);

  return LD->arith.stack.top++;
}

static inline void
pushArithStack(Number n ARG_LD)
{ Number np = allocArithStack(PASS_LD1);

  *np = *n;				/* structure copy */
}

static inline void
resetArithStack(ARG1_LD)
{ LD->arith.stack.top = LD->arith.stack.base;
}

static inline Number
argvArithStack(int n ARG_LD)
{ DEBUG(0, assert(LD->arith.stack.top - n >= LD->arith.stack.base));

  return LD->arith.stack.top - n;
}

static inline void
popArgvArithStack(int n ARG_LD)
{ DEBUG(0, assert(LD->arith.stack.top - n >= LD->arith.stack.base));

  for(; n>0; n--)
  { LD->arith.stack.top--;
    clearNumber(LD->arith.stack.top);
  }
}

		 /*******************************
		 *	      THREADS		*
		 *******************************/

#ifdef O_PLMT
static inline PL_local_data_t *
acquire_ldata__LD(PL_thread_info_t *info ARG_LD)
{ PL_local_data_t *ld = info->thread_data;
  LD->thread.info->access.ldata = ld;
  if ( ld && ld->magic == LD_MAGIC )
    return ld;
  LD->thread.info->access.ldata = NULL;
  return NULL;
}
#endif

#endif /*PL_INLINE_H_INCLUDED*/
