/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: memory allocation
*/

#include "pl-incl.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines memory allocation for the heap (the  program  space)
and  the  various  stacks.   Memory  allocation below ALLOCFAST bytes is
based entirely on a perfect fit algorithm.  Above ALLOCFAST  the  system
memory  allocation  function  (typically malloc() is used to optimise on
space.  Perfect fit memory allocation is fast and because  most  of  the
memory  is allocated in small segments and these segments normally occur
in similar relative frequencies it does not waste much memory.

The prolog machinery using these memory allocation functions always know
how  much  memory  is  allocated  and  provides  this  argument  to  the
corresponding  unalloc()  call if memory need to be freed.  This saves a
word to store the size of the memory segment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct chunk *	Chunk;
typedef long		align_type;

struct chunk
{ Chunk		next;		/* next of chain */
};

forwards Chunk	allocate P((alloc_t size));

#define ALLOCSIZE	10240	/* size of allocation chunks */
#define ALLOCFAST	512	/* big enough for all structures */

static char   *spaceptr;	/* alloc: pointer to first free byte */
static alloc_t spacefree;	/* number of free bytes left */

static Chunk  freeChains[ALLOCFAST/sizeof(Chunk)+1];

#define ALLOCROUND(n) ( (n) < sizeof(struct chunk) ? sizeof(struct chunk) \
						   : ROUND(n, sizeof(align_type)) )
			   
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Allocate n bytes from the heap.  The amount returned is n rounded up to
a multiple of words.  Allocated memory always starts at a word boundary.

below ALLOCFAST we use a special purpose fast allocation scheme.  Above
(which is very rare) we use Unix malloc()/free() mechanism.

The rest of the code uses the macro allocHeap() to access this function
to avoid problems with 16-bit machines not supporting an ANSI compiler.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Void
alloc_heap(n)
register alloc_t n;
{ register Chunk f;
  register alloc_t m;
  
  DEBUG(9, printf("allocated %ld bytes at ", n));
  n = ALLOCROUND(n);
  statistics.heap += n;

  if (n <= ALLOCFAST)
  { m = n / (int) sizeof(align_type);
    if ((f = freeChains[m]) != NULL)
    { freeChains[m] = f->next;
      f->next = (Chunk) NULL;
      DEBUG(9, printf("(r) %ld (0x%lx)\n", f, f));
      return (Word) f;			/* perfect fit */
    }
    f = allocate(n);			/* allocate from core */

    if ((char *)f < hBase) hBase = (char *)f;
    if ((char *)f > hTop)  hTop  = (char *)f;

    DEBUG(9, printf("(n) %ld (0x%lx)\n", f, f));
    return f;
  }

  f = (Chunk) Malloc(n);
  DEBUG(9, printf("(b) %ld\n", f));
  return f;
}

void
free_heap(mem, n)
register Void mem;
register alloc_t n;
{ Chunk p = (Chunk) mem;

  n = ALLOCROUND(n);
  statistics.heap -= n;
  DEBUG(9, printf("freed %d bytes at %ld\n", n, p));

  if (n <= ALLOCFAST)
  { n /= sizeof(align_type);
    p->next = freeChains[n];
    freeChains[n] = p;
  } else
  { Free(p);
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
No perfect fit is available.  We pick memory from the big chunk  we  are
working  on.   If this is not big enough we will free the remaining part
of it.  Next we check whether any areas are  assigned  to  be  used  for
allocation.   If  all  this fails we allocate new core using Allocate(),
which normally calls Malloc(). Early  versions  of  this  module  called
sbrk(),  but  many systems get very upset by using sbrk() in combination
with other memory allocation functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
Chunk
allocate(n)
register alloc_t n;
{ char *p;

  if (n <= spacefree)
  { p = spaceptr;
    spaceptr += n;
    spacefree -= n;
    return (Chunk) p;
  }

  if ( spacefree >= sizeof(struct chunk) )
    freeHeap(spaceptr, (alloc_t) (spacefree/sizeof(align_type))*sizeof(align_type));

  if ((p = (char *) Allocate(ALLOCSIZE)) <= (char *)NULL)
    fatalError("Not enough core");

  spacefree = ALLOCSIZE;
  spaceptr = p + n;
  spacefree -= n;

  return (Chunk) p;
}

		/********************************
		*             STACKS            *
		*********************************/

volatile void
outOf(s)
Stack s;
{ warning("Out of %s stack", s->name);

  pl_abort();
}

		/********************************
		*        GLOBAL STACK           *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
alloc_global() allocates on the global stack.  Many  functions  do  this
inline  as  it is simple and usualy very time critical.  The rest of the
system should call the macro allocGlobal() to ensure the type  is  right
on 16-bit machines not supporting ANSI.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Void
alloc_global(n)
register alloc_t n;
{ register Word result = gTop;

  gTop += (n + sizeof(word)-1) / sizeof(word);
  verifyStack(global);

  return result;
}

word
globalFunctor(def)
register FunctorDef def;
{ register Functor f = (Functor) gTop;
  register int arity = def->arity;
  register Word a;

  gTop = (Word)((char *)gTop + sizeof(FunctorDef) + sizeof(word) * arity);
  verifyStack(global);
  f->definition = def;
  for(a = argTermP(f, 0); arity > 0; a++, arity--)
    setVar(*a);

  return (word) f;
}

#if O_STRING
word
globalString(s)
register char *s;
{ register ulong l = strlen(s) + 1;
  register Word gt = gTop;
  register long chars = ROUND(l, sizeof(word));

  gTop = (Word) addPointer(gTop, 2*sizeof(word) + chars);
  verifyStack(global);
  gt[0] = gt[1+chars/sizeof(word)] = (((l-1)<<LMASK_BITS) | STRING_MASK);
  strcpy((char *)(gt+1), s);

  return ((word)gt | INDIRECT_MASK);
}

word
heapString(s)
char *s;
{ ulong l = strlen(s) + 1;
  register long chars = ROUND(l, sizeof(word));
  Word gt = (Word)allocHeap(2*sizeof(word) + chars);

  gt[0] = gt[1+chars/sizeof(word)] = (((l-1)<<LMASK_BITS) | STRING_MASK);
  strcpy((char *)(gt+1), s);

  return (word)gt | INDIRECT_MASK;
}

#endif /* O_STRING */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
To allow for garbage collection,  reals  are  packed  into  two  tagged
words.   The  4  top  bits  are  REAL_MASK  and the two bottom bits are
reserved for garbage collection.  This leaves us with 52 bits  to  store
the  real.   As  a  consequence,  SWI-Prolog  now uses a kind of `small
doubles', increasing arithmetic accuracy.

This code is very hacky and needs to be rewritten for  systems  that  do
not  have  IEEE  floating  point format.  Luckily almost all systems use
IEEE these days.

Fixed for GCC 2.2 with the help of Giovanni Malnati.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef union
{ real f;
  struct
  { ulong e;
    ulong f;
  } bits;
} fconvert;

forwards void	pack_real P((real f, fconvert *r));

#define IEEE 1

#if IEEE
static void
pack_real(f, r)
real f;
register fconvert *r;
{ fconvert b;

  b.f = f;
  b.bits.f >>= 10;
  b.bits.f &= ~0xffc00003L;
  b.bits.f |= (b.bits.e & 0x3f) << 22;
  b.bits.e >>= 4;
  b.bits.e &= ~0xf0000003L;
#if O_16_BITS
  b.bits.e >>= 1;
  b.bits.f >>= 1;
#endif
  b.bits.f |= REAL_MASK;
  b.bits.e |= REAL_MASK;

  (*r).bits = b.bits;
}

double
unpack_real(p)
Word p;
{ fconvert b;

  b.bits = ((fconvert *)p)->bits;

#if O_16_BITS
  b.bits.e <<= 1;
  b.bits.f <<= 1;
#endif
  b.bits.e <<= 4;
  b.bits.e &= ~0x0000003fL;
  b.bits.e |= (b.bits.f & 0x0fc00000L) >> 22;
  b.bits.f <<= 10;
  b.bits.f &= ~0x000003ffL;

  return b.f;
}
#endif /* IEEE */

void
setReal(w, f)
word w;
real f;
{ fconvert *b = (fconvert *)unMask(w);
  pack_real(f, b);
}

word
globalReal(f)
real f;
{ fconvert *b = (fconvert *)gTop;

  gTop += sizeof(fconvert) / sizeof(word);
  verifyStack(global);
  pack_real(f, b);

  DEBUG(4, printf("Put REAL on global stack at 0x%x\n", b));
  return (word)b | INDIRECT_MASK;
}

word
heapReal(f)
real f;
{ fconvert *b = (fconvert *)allocHeap(sizeof(fconvert));

  pack_real(f, b);
  return (word)b | INDIRECT_MASK;
}


		/********************************
		*         LOCAL STACK           *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Allocation on the local stack is used by many foreign language functions
that needs scratch memory.  The area normally is large and it  need  not
be  deallocated  as  it  vanishes  after  quiting  the  foreign language
function anyway.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *scratchBase;			/* base for scratching */
#if !O_DYNAMIC_STACKS
static char *scratchTop;
#endif

void
initAllocLocal()
{ if (scratchBase == (char *)NULL)
  { scratchBase = (char *) lTop;
#if !O_DYNAMIC_STACKS
    scratchTop  = (char *) lMax;
#endif
  }
}

Void
alloc_local(n)
register alloc_t n;
{ register char *mem = scratchBase;

  scratchBase += ROUND(n, sizeof(word));
#if !O_DYNAMIC_STACKS
  STACKVERIFY( if ( scratchBase >= scratchTop )
		 outOf((Stack) &stacks.local) );
#endif

  return mem;
}

void
stopAllocLocal()
{ scratchBase = (char *)NULL;
}

char *
store_string_local(s)
register char *s;
{ register char *copy = (char *)allocLocal(strlen(s)+1);

  strcpy(copy, s);
  return copy;
}

		/********************************
		*            STRINGS            *
		*********************************/

char *
store_string(s)
char *s;
{ char *copy = (char *)allocHeap(strlen(s)+1);

  strcpy(copy, s);
  return copy;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Hash function for strings.  This function has been evaluated on Shelley,
which defines about 5000 Prolog atoms.  It produces a very nice  uniform
distribution over these atoms.  Note that size equals 2^n.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
stringHashValue(s, size)
register char *s;
register int size;
{ register int value = 0;
  register int shift = 0;

  while(*s)
    value += (((int)(*s++)) << ((++shift) & 0x7));

  return value & (size-1);
}
