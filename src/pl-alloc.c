/*  pl-alloc.c,v 1.18 1995/02/07 12:12:16 jan Exp

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: memory allocation
*/

#include "pl-incl.h"

#ifndef ALLOC_DEBUG
#define ALLOC_DEBUG 0
#endif
#define ALLOC_MAGIC 0xbf
#define ALLOC_FREE_MAGIC 0x5f

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

forwards Chunk	allocate(alloc_t size);

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

void *
alloc_heap(register size_t n)
{ register Chunk f;
  register alloc_t m;
  
  DEBUG(9, Sdprintf("allocated %ld bytes at ", (unsigned long)n));
  n = ALLOCROUND(n);
  statistics.heap += n;

  if (n <= ALLOCFAST)
  { m = n / (int) sizeof(align_type);
    if ((f = freeChains[m]) != NULL)
    { freeChains[m] = f->next;
      f->next = (Chunk) NULL;
      DEBUG(9, Sdprintf("(r) %ld (0x%lx)\n",
		      (unsigned long) f, (unsigned long) f));
#if ALLOC_DEBUG
      { int i;
	char *s = (char *) f;

	for(i=sizeof(struct chunk); i<n; i++)
	  assert(s[i] == ALLOC_FREE_MAGIC);

	memset((char *) f, ALLOC_MAGIC, n);
      }
#endif
      return (Word) f;			/* perfect fit */
    }
    f = allocate(n);			/* allocate from core */

    SetHBase(f);
    SetHTop((char *)f + n);

    DEBUG(9, Sdprintf("(n) %ld (0x%lx)\n", (unsigned long)f, (unsigned long)f));
#if ALLOC_DEBUG
    memset((char *) f, ALLOC_MAGIC, n);
#endif
    return f;
  }

  f = (Chunk) Malloc(n);
  DEBUG(9, Sdprintf("(b) %ld\n", (unsigned long)f));
#if ALLOC_DEBUG
  memset((char *) f, ALLOC_MAGIC, n);
#endif
  return f;
}

void
free_heap(register void * mem, register size_t n)
{ Chunk p = (Chunk) mem;

  n = ALLOCROUND(n);
#if ALLOC_DEBUG
  memset((char *) mem, ALLOC_FREE_MAGIC, n);
#endif
  statistics.heap -= n;
  DEBUG(9, Sdprintf("freed %ld bytes at %ld\n",
		    (unsigned long)n, (unsigned long)p));

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
allocate(register size_t n)
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
outOf(Stack s)
{ status.outofstack = TRUE;		/* will be reset by abort() */

  warning("Out of %s stack", s->name);

  pl_abort();
  exit(2);				/* should not happen */
}

		 /*******************************
		 *	REFS AND POINTERS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
__consPtr() is inlined for this module (including pl-wam.c), but external
for the other modules, where it is far less fime-critical.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef consPtr
inline word
__consPtr(void *p, int ts)
{ unsigned long v = (unsigned long) p;

  v -= base_addresses[ts&STG_MASK];
  assert(v < MAXTAGGEDPTR);
  return (v<<5)|ts;
}

word
consPtr(void *p, int ts)
{ return __consPtr(p, ts);
}

#define consPtr(p, s) __consPtr(p, s)
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
makeRef() and makeRefLG().   Make  a   reference  pointer.   The  second
version is used by the WAM-interpreter to  exploit the fact that we know
the pointer is either to the local or global stack.  See also TrailLG().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline word
makeRefLG(Word p)
{ if ( p >= (Word) lBase )
    return consPtr(p, TAG_REFERENCE|STG_LOCAL);
  else
    return consPtr(p, TAG_REFERENCE|STG_GLOBAL);
}


word
makeRef(Word p)
{ int s;
  word ref;

  if ( onStackArea(local, p) )
    s = TAG_REFERENCE|STG_LOCAL;
  else if ( onStackArea(global, p) )
    s = TAG_REFERENCE|STG_GLOBAL;
  else
    s = TAG_REFERENCE|STG_HEAP;

  ref = consPtr(p, s);
  return ref;
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

#if O_SHIFT_STACKS
void *
alloc_global(int n)
{ Word result;

  if ( roomStack(global)/sizeof(word) < (long) n )
  { growStacks(NULL, NULL, FALSE, TRUE, FALSE);

    if ( roomStack(global)/sizeof(word) < (long) n )
      outOf((Stack) &stacks.global);
  }

  result = gTop;
  gTop += n;

  return result;
}

#else

void *
alloc_global(int n)
{ Word result = gTop;

  requireStack(global, n * sizeof(word));
  gTop += n;

  return result;
}

#endif

word
globalFunctor(register FunctorDef def)
{ register int arity = def->arity;
  register Functor f = allocGlobal(1 + arity);
  register Word a;

  f->definition = def->functor;
  for(a = &f->arguments[0]; arity > 0; a++, arity--)
    setVar(*a);

  return consPtr(f, TAG_COMPOUND|STG_GLOBAL);
}


Word
newTerm(void)
{ Word t = allocGlobal(1);

  setVar(*t);

  return t;
}

		 /*******************************
		 *      OPERATIONS ON LONGS	*
		 *******************************/

word
globalLong(long l)
{ Word p = allocGlobal(3);
  word r = consPtr(p, TAG_INTEGER|STG_GLOBAL);
  word m = mkIndHdr(1, TAG_INTEGER);

  *p++ = m;
  *p++ = l;
  *p   = m;
  
  return r;
}


word
heapLong(long l)
{ Word p = allocHeap(3 * sizeof(word));
  word r = consPtr(p, TAG_INTEGER|STG_HEAP);
  word m = mkIndHdr(1, TAG_INTEGER);

  *p++ = m;
  *p++ = l;
  *p   = m;
  
  return r;
}


		 /*******************************
		 *    OPERATIONS ON STRINGS	*
		 *******************************/

int
sizeString(word w)
{ word m  = *((Word)addressIndirect(w));
  int wn  = wsizeofInd(m);
  int pad = padHdr(m);

  return wn*sizeof(word) - pad;
}


word
globalNString(long l, const char *s)
{ int lw = (l+sizeof(word))/sizeof(word);
  int pad = (lw*sizeof(word) - l);
  Word p = allocGlobal(2 + lw);
  word r = consPtr(p, TAG_STRING|STG_GLOBAL);
  word m = mkStrHdr(lw, pad);

  *p++ = m;
  p[lw-1] = 0L;				/* write zero's for padding */
  memcpy(p, s, l);
  p += lw;
  *p = m;
  
  return r;
}


word
globalString(const char *s)
{ return globalNString(strlen(s), s);
}


word
heapString(const char *s)
{ int l = strlen(s);
  int lw = (l+sizeof(word))/sizeof(word);
  int pad = (lw*sizeof(word) - l);
  Word p = allocHeap((2 + lw) * sizeof(word));
  word r = consPtr(p, TAG_STRING|STG_HEAP);
  word m = mkStrHdr(lw, pad);

  *p++ = m;
  p[lw-1] = 0L;				/* padding word */
  memcpy(p, s, l);
  p += lw;
  *p = m;
  
  return r;
}


		 /*******************************
		 *     OPERATIONS ON DOUBLES	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Storage of floats (doubles) on the  stacks   and  heap.  Such values are
packed into two `guards words'.  An   intermediate  structure is used to
ensure the possibility of  word-aligned  copy   of  the  data. Structure
assignment is used here  to  avoid  a   loop  for  different  values  of
WORDS_PER_DOUBLE.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define WORDS_PER_DOUBLE ((sizeof(double)+sizeof(word)-1)/sizeof(word))

typedef struct
{ word w[WORDS_PER_DOUBLE];
} fword;

double					/* take care of alignment! */
valReal(word w)
{ fword *v = (fword *)valIndirectP(w);
  union
  { double d;
    fword  l;
  } val;
  
  val.l = *v;

  return val.d;
}


word
globalReal(double d)
{ Word p = allocGlobal(2+WORDS_PER_DOUBLE);
  word r = consPtr(p, TAG_FLOAT|STG_GLOBAL);
  word m = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
  union
  { double d;
    fword  l;
  } val;
  fword *v;

  val.d = d;
  *p++ = m;
  v = (fword *)p;
  *v++ = val.l;
  p = (Word) v;
  *p   = m;

  return r;
}


word
heapReal(double d)
{ Word p = allocHeap((2+WORDS_PER_DOUBLE)*sizeof(word));
  word r = consPtr(p, TAG_FLOAT|STG_HEAP);
  word m = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
  union
  { double d;
    fword  l;
  } val;
  fword *v;

  val.d = d;
  *p++ = m;
  v = (fword *)p;
  *v++ = val.l;
  p = (Word) v;
  *p   = m;

  return r;
}


		 /*******************************
		 *  GENERIC INDIRECT OPERATIONS	*
		 *******************************/

int
equalIndirect(word w1, word w2)
{ Word p1 = addressIndirect(w1);
  Word p2 = addressIndirect(w2);
  
  if ( *p1 == *p2 )
  { int n = wsizeofInd(*p1);
    
    while( --n >= 0 )
    { if ( *++p1 != *++p2 )
	fail;
    }

    succeed;
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Copy an indirect data object to the heap.  The type is not of importance,
neither is the length or original location.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
heapIndirect(word w)
{ Word p = addressIndirect(w);
  word t = *p;
  int  n = wsizeofInd(t);
  Word h = allocHeap((n+2) * sizeof(word));
  Word hp = h;
  
  *hp = t;
  while(--n >= 0)
    *++hp = *++p;
  *++hp = t;

  return consPtr(h, tag(w)|STG_HEAP);
}


void
freeHeapIndirect(word w)
{ Word p = addressIndirect(w);
  int  n = wsizeofInd(*p);

  freeHeap(p, (n+2) * sizeof(word));
}


word
globalIndirect(word w)
{ Word p = addressIndirect(w);
  word t = *p;
  int  n = wsizeofInd(t);
  Word h = allocGlobal((n+2));
  Word hp = h;
  
  *hp = t;
  while(--n >= 0)
    *++hp = *++p;
  *++hp = t;

  return consPtr(h, tag(w)|STG_GLOBAL);
}


word
globalIndirectFromCode(Code *PC)
{ Code pc = *PC;
  word m = *pc++;
  int  n = wsizeofInd(m);
  Word p = allocGlobal(n+2);
  word r = consPtr(p, tag(m)|STG_GLOBAL);

  *p++ = m;
  while(--n >= 0)
    *p++ = *pc++;
  *p++ = m;

  *PC = pc;
  return r;
}


int
equalIndirectFromCode(word a, Code *PC)
{ Word pc = *PC;
  Word pa = addressIndirect(a);

  if ( *pc == *pa )
  { int  n = wsizeofInd(*pc);

    while(--n >= 0)
    { if ( *++pc != *++pa )
	fail;
    }
    pc++;
    *PC = pc;
    succeed;
  }

  fail;
}


		/********************************
		*            STRINGS            *
		*********************************/

char *
store_string(const char *s)
{ char *copy = (char *)allocHeap(strlen(s)+1);

  strcpy(copy, s);
  return copy;
}


void
remove_string(char *s)
{ if ( s )
    freeHeap(s, strlen(s)+1);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Hash function for strings.  This function has been evaluated on Shelley,
which defines about 5000 Prolog atoms.  It produces a very nice  uniform
distribution over these atoms.  Note that size equals 2^n.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
unboundStringHashValue(const char *t)
{ unsigned int value = 0;
  unsigned int shift = 5;

  while(*t)
  { unsigned int c = *t++;
    
    c -= 'a';
    value ^= c << (shift & 0xf);
    shift ^= c;
  }

  return value ^ (value >> 16);
}


		 /*******************************
		 *	     GNU MALLOC		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These functions are used by various GNU-libraries and -when not linked
with the GNU C-library lead to undefined symbols.  Therefore we define
them in SWI-Prolog so that we can also give consistent warnings.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void *
xmalloc(size_t size)
{ void *mem;

  if ( (mem = malloc(size)) )
    return mem;

  fatalError("Not enough core");
  return NULL;				/* NOTREACHED */
}


void *
xrealloc(void *mem, size_t size)
{ void *newmem;

  newmem = mem ? realloc(mem, size) : malloc(size);
  if ( newmem )
    return newmem;
  if ( size )
    fatalError("Not enough core");
  return NULL;				/* NOTREACHED */
}
