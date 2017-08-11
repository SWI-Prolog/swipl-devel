/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2017, University of Amsterdam
                              VU University Amsterdam
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
#include "os/pl-cstack.h"

#undef LD
#define LD LOCAL_LD

#if ALLOC_DEBUG
#define ALLOC_FREE_MAGIC 0xFB
#define ALLOC_NEW_MAGIC  0xF9
#endif


		 /*******************************
		 *	    USE BOEHM GC	*
		 *******************************/

#if !defined(PL_ALLOC_DONE) && defined(HAVE_BOEHM_GC)
#define PL_ALLOC_DONE 1
#undef HAVE_MTRACE

void *
allocHeap(size_t n)
{ void *mem = GC_MALLOC(n);

#if ALLOC_DEBUG
  if ( mem )
    memset(mem, ALLOC_NEW_MAGIC, n);
#endif

  return mem;
}


void *
allocHeapOrHalt(size_t n)
{ void *mem = allocHeap(n);

  if ( !mem )
    outOfCore();

  return mem;
}


void
freeHeap(void *mem, size_t n)
{
#if ALLOC_DEBUG
  if ( mem )
    memset(mem, ALLOC_FREE_MAGIC, n);
#endif

  GC_FREE(mem);
}


#ifdef GC_DEBUG
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
To debug the  interaction  between  Boehm-GC   and  Prolog,  we  run the
collector in leak-detection mode.  Reported leaks can have three causes:

  - They are real leaks. We would like to fix these, at least for the
    non-GC version.
  - They are caused by lacking traceable pointers.  This must be fixed
    to run reliably under Boehm-GC.
  - The are place that can currently not be safely removed.  We call
    GC_LINGER() on such pointers.  These will be left to GC, but in
    leak-detection mode we give them a reference to silence the leak
    detector.

GC_linger() is called to keep track of objects we would like to leave to
GC because we are not sure they can be reclaimed safely now. We use this
as a debugging aid if GC_DEBUG is enabled.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct linger
{ struct linger *next;
  void	        *object;
} linger;

linger *GC_lingering = NULL;

void
GC_linger(void *ptr)
{ linger *l = GC_MALLOC_UNCOLLECTABLE(sizeof(*l));

  l->object = ptr;
  PL_LOCK(L_ALLOC);
  l->next = GC_lingering;
  GC_lingering = l->next;
  PL_UNLOCK(L_ALLOC);
}

#endif /*GC_DEBUG*/
#endif /*HAVE_BOEHM_GC*/


		 /*******************************
		 *   USE PLAIN SYSTEM MALLOC	*
		 *******************************/

#ifndef PL_ALLOC_DONE
#if defined(HAVE_MTRACE) && defined(O_MAINTENANCE)
#include <mcheck.h>
#endif

void *
allocHeap(size_t n)
{ void *mem = malloc(n);

#if ALLOC_DEBUG
  if ( mem )
    memset((char *) mem, ALLOC_NEW_MAGIC, n);
#endif

  return mem;
}


void *
allocHeapOrHalt(size_t n)
{ if ( n )
  { void *mem = allocHeap(n);

    if ( !mem )
      outOfCore();

    return mem;
  }

  return NULL;
}


void
freeHeap(void *mem, size_t n)
{
#if ALLOC_DEBUG
  memset((char *) mem, ALLOC_FREE_MAGIC, n);
#else
  (void)n;
#endif

  free(mem);
}

#endif /*PL_ALLOC_DONE*/


		 /*******************************
		 *	 LINGERING OBJECTS	*
		 *******************************/

void
linger(linger_list** list, void (*unalloc)(void *), void *object)
{ linger_list *c = allocHeapOrHalt(sizeof(*c));
  linger_list *o;

  c->object  = object;
  c->unalloc = unalloc;

  do
  { o = *list;
    c->next = o;
  } while( !COMPARE_AND_SWAP(list, o, c) );
}

void
free_lingering(linger_list **list)
{ linger_list *c, *n;

  do
  { if ( !(c = *list) )
      return;
  } while( !COMPARE_AND_SWAP(list, c, NULL) );

  for(; c; c=n)
  { n = c->next;
    (*c->unalloc)(c->object);
    freeHeap(c, sizeof(*c));
  }
}

		/********************************
		*             STACKS            *
		*********************************/

int
enableSpareStack(Stack s)
{ if ( s->spare )
  { s->max = addPointer(s->max, s->spare);
    s->spare = 0;
    return TRUE;
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*)  outOfStack(stack,  how)  is  called  to   raise  a  stack  overflow
exception. This can happen from two  placed:   the  VM and foreign code.
When the error is thrown from the VM,  we have to be careful because the
state of the VM is unknown. Notably, we   might be in `body write' mode,
which implies we are writing terms to   the  local stack above lTop. For
this reason, we cannot use  PL_open_foreign_frame().   So,  we build the
error term using low-level primitives that   only touch the global stack
with  a  few  cells   and   also    avoid   the   term   duplication  of
PL_raise_exception().

FIXME: We could consider reserving some space   on  the global stack for
resource exceptions near the bottom. That would   also avoid the need to
freeze the global stack. One  problem  is   that  the  user  migh keep a
reference to this reserved exception term,  which makes it impossible to
reuse.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
outOfStack(void *stack, stack_overflow_action how)
{ GET_LD
  Stack s = stack;
  const char *msg = "out-of-stack";

  if ( LD->outofstack == stack )
  { Sdprintf("[Thread %d]: failed to recover from %s-overflow\n",
	     PL_thread_self(), s->name);
    print_backtrace_named(msg);
    save_backtrace("crash");
    print_backtrace_named("crash");
    fatalError("Sorry, cannot continue");

    return FALSE;				/* NOTREACHED */
  }

  save_backtrace(msg);

  if ( s->spare < s->def_spare/4 )
  { Sdprintf("[Thread %d]: %s-overflow: spare=%ld (def=%ld)\n"
	     "Last resource exception:\n",
	     PL_thread_self(), s->name, (long)s->spare, (long)s->def_spare);
    print_backtrace_named("exception");
  }

  enableSpareStack(s);
  LD->trim_stack_requested = TRUE;
  LD->exception.processing = TRUE;
  LD->outofstack = stack;

  switch(how)
  { case STACK_OVERFLOW_THROW:
    case STACK_OVERFLOW_RAISE:
    { if ( gTop+5 < gMax )
      { Word p = gTop;

	p[0] = FUNCTOR_error2;			/* see (*) above */
	p[1] = consPtr(&p[3], TAG_COMPOUND|STG_GLOBAL);
	p[2] = PL_new_atom(s->name);
	p[3] = FUNCTOR_resource_error1;
	p[4] = ATOM_stack;
	gTop += 5;
	PL_unregister_atom(p[2]);

	*valTermRef(LD->exception.bin) = consPtr(p, TAG_COMPOUND|STG_GLOBAL);
	freezeGlobal(PASS_LD1);
      } else
      { Sdprintf("Out of %s-stack.  No room for exception term.  Aborting.\n", s->name);
	*valTermRef(LD->exception.bin) = ATOM_aborted;
      }
      exception_term = exception_bin;

      if ( how == STACK_OVERFLOW_THROW &&
	   LD->exception.throw_environment )
      {						/* see PL_throw() */
	longjmp(LD->exception.throw_environment->exception_jmp_env, 1);
      }

      return FALSE;
    }
    default:
      assert(0);
      fail;
  }
}


int
raiseStackOverflow(int overflow)
{ GET_LD
  Stack s;

  switch(overflow)
  { case LOCAL_OVERFLOW:    s = (Stack)&LD->stacks.local;    break;
    case GLOBAL_OVERFLOW:   s = (Stack)&LD->stacks.global;   break;
    case TRAIL_OVERFLOW:    s = (Stack)&LD->stacks.trail;    break;
    case ARGUMENT_OVERFLOW: s = (Stack)&LD->stacks.argument; break;
    case MEMORY_OVERFLOW:
      return PL_error(NULL, 0, NULL, ERR_NOMEM);
    case FALSE:				/* some other error is pending */
      return FALSE;
    default:
      s = NULL;
      assert(0);
  }

  return outOfStack(s, STACK_OVERFLOW_RAISE);
}


void
pushArgumentStack__LD(Word p ARG_LD)
{ Word *newbase;
  size_t newsize = nextStackSize((Stack)&LD->stacks.argument, 1);

  if ( newsize && (newbase = stack_realloc(aBase, newsize)) )
  { intptr_t as = newbase - aBase;

    if ( as )
    { QueryFrame qf;

      aTop += as;
      aBase = newbase;

      for(qf=LD->query; qf; qf = qf->parent)
	qf->aSave += as;
    }
    aMax  = addPointer(newbase,  newsize);
    *aTop++ = p;
  } else
    outOfStack((Stack)&LD->stacks.argument, STACK_OVERFLOW_THROW);
}


void
outOfCore(void)
{ fatalError("Could not allocate memory: %s", OsError());
}


		/********************************
		*        GLOBAL STACK           *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
allocGlobal() allocates on the global stack.  Many  functions  do  this
inline  as  it is simple and usualy very time critical.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Word
allocGlobal__LD(size_t n ARG_LD)
{ Word result;

  if ( !hasGlobalSpace(n) )
  { int rc;

    if ( (rc=ensureGlobalSpace(n, ALLOW_GC)) != TRUE )
    { raiseStackOverflow(rc);
      return NULL;
    }
  }

  result = gTop;
  gTop += n;

  return result;
}

Word
allocGlobalNoShift__LD(size_t n ARG_LD)
{ Word result;

  if ( gTop+n > gMax )
    return NULL;

  result = gTop;
  gTop += n;

  return result;
}


Word
newTerm(void)
{ GET_LD
  Word t = allocGlobal(1);

  setVar(*t);

  return t;
}

		 /*******************************
		 *    OPERATIONS ON INTEGERS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translate  a  64-bit  integer  into   a    Prolog   cell.   Uses  tagged
representation if possible or allocates 64-bits on the global stack.

Return is one of:

	TRUE:		 Success
	FALSE:		 Interrupt
	GLOBAL_OVERFLOW: Stack overflow
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
put_int64(Word at, int64_t l, int flags ARG_LD)
{ Word p;
  word r, m;
  int req;

  r = consInt(l);
  if ( valInt(r) == l )
  { *at = r;
    return TRUE;
  }

#if SIZEOF_VOIDP == 8
  req = 3;
#elif SIZEOF_VOIDP == 4
  req = 4;
#else
#error "FIXME: Unsupported sizeof word"
#endif

  if ( !hasGlobalSpace(req) )
  { int rc = ensureGlobalSpace(req, flags);

    if ( rc != TRUE )
      return rc;
  }
  p = gTop;
  gTop += req;

#if SIZEOF_VOIDP == 8
  r = consPtr(p, TAG_INTEGER|STG_GLOBAL);
  m = mkIndHdr(1, TAG_INTEGER);

  *p++ = m;
  *p++ = l;
  *p   = m;
#else
#if SIZEOF_VOIDP == 4
  r = consPtr(p, TAG_INTEGER|STG_GLOBAL);
  m = mkIndHdr(2, TAG_INTEGER);

  *p++ = m;
#ifdef WORDS_BIGENDIAN
  *p++ = (word)(l>>32);
  *p++ = (word)l;
#else
  *p++ = (word)l;
  *p++ = (word)(l>>32);
#endif
  *p   = m;
#else
#error "FIXME: Unsupported sizeof intptr_t."
#endif
#endif

  *at = r;
  return TRUE;
}


		 /*******************************
		 *    OPERATIONS ON STRINGS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
To distinguish between byte and wide strings,   the system adds a 'B' or
'W' in front of the real string. For   a  'W', the following 3 bytes are
ignored to avoid alignment restriction problems.

Note that these functions can trigger GC
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Word
allocString(size_t len ARG_LD)
{ size_t lw = (len+sizeof(word))/sizeof(word);
  int pad = (int)(lw*sizeof(word) - len);
  Word p = allocGlobal(2 + lw);
  word m = mkStrHdr(lw, pad);

  if ( !p )
    return NULL;

  p[0]    = m;
  p[lw]   = 0L;				/* zero the pad bytes */
  p[lw+1] = m;

  return p;
}


word
globalString(size_t len, const char *s)
{ GET_LD
  Word p = allocString(len+1 PASS_LD);

  if ( p )
  { char *q = (char *)&p[1];

    *q++ = 'B';
    memcpy(q, s, len);

    return consPtr(p, TAG_STRING|STG_GLOBAL);
  }

  return 0;
}


word
globalWString(size_t len, const pl_wchar_t *s)
{ GET_LD
  const pl_wchar_t *e = &s[len];
  const pl_wchar_t *p;
  Word g;

  for(p=s; p<e; p++)
  { if ( *p > 0xff )
      break;
  }

  if ( p == e )				/* 8-bit string */
  { unsigned char *t;

    if ( !(g = allocString(len+1 PASS_LD)) )
      return 0;
    t = (unsigned char *)&g[1];
    *t++ = 'B';
    for(p=s; p<e; )
      *t++ = (unsigned char)(*p++ & 0xff);
  } else				/* wide string */
  { char *t;
    pl_wchar_t *w;

    if ( !(g = allocString((len+1)*sizeof(pl_wchar_t) PASS_LD)) )
      return 0;
    t = (char *)&g[1];
    w = (pl_wchar_t*)t;
    w[0] = 0;
    *t = 'W';
    memcpy(&w[1], s, len*sizeof(pl_wchar_t));
  }

  return consPtr(g, TAG_STRING|STG_GLOBAL);
}


char *
getCharsString__LD(word w, size_t *len ARG_LD)
{ Word p = valPtr(w);
  word m = *p;
  size_t wn  = wsizeofInd(m);
  size_t pad = padHdr(m);
  char *s;

  if ( len )
    *len = wn*sizeof(word) - pad - 1;	/* -1 for the 'B' */

  s = (char *)&p[1];

  if ( *s == 'B' )
    return s+1;

  assert(*s == 'W');
  return NULL;
}


pl_wchar_t *
getCharsWString__LD(word w, size_t *len ARG_LD)
{ Word p = valPtr(w);
  word m = *p;
  size_t wn  = wsizeofInd(m);
  size_t pad = padHdr(m);
  char *s;
  pl_wchar_t *ws;

  s = (char *)&p[1];
  if ( *s != 'W' )
    return NULL;

  if ( len )
    *len = ((wn*sizeof(word) - pad)/sizeof(pl_wchar_t)) - 1;

  ws = (pl_wchar_t *)&p[1];
  return ws+1;
}



		 /*******************************
		 *     OPERATIONS ON DOUBLES	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Storage of floats (doubles) on the  stacks   and  heap.  Such values are
packed into two `guards words'. We  cannot   just  copy the double as it
might not be properly aligned.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
put_double(Word at, double d, int flags ARG_LD)
{ Word p;
  word m = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);

  if ( flags != ALLOW_CHECKED && !hasGlobalSpace(2+WORDS_PER_DOUBLE) )
  { int rc = ensureGlobalSpace(2+WORDS_PER_DOUBLE, flags);

    if ( rc != TRUE )
      return rc;
  }
  p = gTop;
  gTop += 2+WORDS_PER_DOUBLE;

  *at = consPtr(p, TAG_FLOAT|STG_GLOBAL);

  *p++ = m;
  memcpy(p, &d, sizeof(d));
  p += WORDS_PER_DOUBLE;
  *p = m;

  return TRUE;
}


		 /*******************************
		 *	  64-BIT INTEGERS	*
		 *******************************/

#if ALIGNOF_INT64_T != ALIGNOF_VOIDP

int64_t					/* take care of alignment! */
valBignum__LD(word w ARG_LD)
{ Word p = valIndirectP(w);
  union
  { int64_t i;
    word w[WORDS_PER_INT64];
  } val;

#if ( SIZEOF_VOIDP == 4 )
  val.w[0] = p[0];
  val.w[1] = p[1];
#else
#error "Unsupported int64_t alignment conversion"
#endif

  return val.i;
}

#endif

		 /*******************************
		 *  GENERIC INDIRECT OPERATIONS	*
		 *******************************/

int
equalIndirect(word w1, word w2)
{ GET_LD
  Word p1 = addressIndirect(w1);
  Word p2 = addressIndirect(w2);

  if ( *p1 == *p2 )
  { size_t n = wsizeofInd(*p1);

    while( n-- > 0 )
    { if ( *++p1 != *++p2 )
	fail;
    }

    succeed;
  }

  fail;
}


size_t					/* size in cells */
gsizeIndirectFromCode(Code pc)
{ return wsizeofInd(pc[0]) + 2;
}


word
globalIndirectFromCode(Code *PC)
{ GET_LD
  Code pc = *PC;
  word m = *pc++;
  size_t n = wsizeofInd(m);
  Word p = allocGlobal(n+2);

  if ( p )
  { word r = consPtr(p, tag(m)|STG_GLOBAL);

    *p++ = m;
    while(n-- > 0)
      *p++ = *pc++;
    *p++ = m;

    *PC = pc;
    return r;
  } else
    return 0;
}


static int				/* used in pl-wam.c */
equalIndirectFromCode(word a, Code *PC)
{ GET_LD
  Word pc = *PC;
  Word pa = addressIndirect(a);

  if ( *pc == *pa )
  { size_t n = wsizeofInd(*pc);

    while(n-- > 0)
    { if ( *++pc != *++pa )
	fail;
    }
    pc++;
    *PC = pc;
    succeed;
  }

  fail;
}


		 /*******************************
		 *	     GNU MALLOC		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These functions are used by various   GNU-libraries and -when not linked
with the GNU C-library lead to   undefined  symbols. Therefore we define
them in SWI-Prolog so that we can   also  give consistent warnings. Note
that we must call plain system malloc as the library will call free() on
it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if !defined(xmalloc) && defined(O_XMALLOC)

void *
xmalloc(size_t size)
{ void *mem;

  if ( (mem = malloc(size)) )
    return mem;
  if ( size )
    outOfCore();

  return NULL;
}


void *
xrealloc(void *mem, size_t size)
{ void *newmem;

  newmem = mem ? realloc(mem, size) : malloc(size);
  if ( newmem )
    return newmem;
  if ( size )
    outOfCore();

  return NULL;
}

#endif /*xmalloc*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Allocation on behalf of foreign code. There  is generally no need to use
this, unless malloced data is returned by Prolog and the foreign routine
wants to free it (e.g. using BUF_MALLOC).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void *
PL_malloc(size_t size)
{ void *mem;

  if ( (mem = GC_MALLOC(size)) )
    return mem;

  outOfCore();

  return NULL;
}


void *
PL_malloc_atomic(size_t size)
{ void *mem;

  if ( (mem = GC_MALLOC_ATOMIC(size)) )
    return mem;

  outOfCore();

  return NULL;
}


void *
PL_malloc_uncollectable(size_t size)
{ void *mem;

  if ( (mem = GC_MALLOC_UNCOLLECTABLE(size)) )
    return mem;

  outOfCore();

  return NULL;
}


void *
PL_malloc_atomic_uncollectable(size_t size)
{ void *mem;

  if ( (mem = GC_MALLOC_ATOMIC_UNCOLLECTABLE(size)) )
    return mem;

  outOfCore();

  return NULL;
}


void *
PL_malloc_unmanaged(size_t size)
{ void *mem;

  if ( (mem = GC_MALLOC(size)) )
  {
#if defined(HAVE_BOEHM_GC) && defined(GC_FLAG_UNCOLLECTABLE)
    GC_SET_FLAGS(mem, GC_FLAG_UNCOLLECTABLE);
#endif
    return mem;
  }

  outOfCore();

  return NULL;
}


void *
PL_malloc_atomic_unmanaged(size_t size)
{ void *mem;

  if ( (mem = GC_MALLOC_ATOMIC(size)) )
  {
#if defined(HAVE_BOEHM_GC) && defined(GC_FLAG_UNCOLLECTABLE)
    GC_SET_FLAGS(mem, GC_FLAG_UNCOLLECTABLE);
#endif
    return mem;
  }

  outOfCore();

  return NULL;
}


void *
PL_realloc(void *mem, size_t size)
{ void *newmem;

  if ( !(newmem = GC_REALLOC(mem, size)) )
    outOfCore();

  return newmem;
}


void
PL_free(void *mem)
{ GC_FREE(mem);
}


int
PL_linger(void *mem)
{
#if defined(HAVE_BOEHM_GC) && defined(GC_FLAG_UNCOLLECTABLE)
  if ( mem )
  { GC_CLEAR_FLAGS(mem, GC_FLAG_UNCOLLECTABLE);
#ifdef GC_DEBUG
    GC_linger(mem);
#endif
  }
  return TRUE;
#else
  return FALSE;
#endif
}


		 /*******************************
		 *	       INIT		*
		 *******************************/

#ifdef HAVE_BOEHM_GC
static void
heap_gc_warn_proc(char *msg, GC_word arg)
{
#if ALLOC_DEBUG
  Sdprintf(msg, arg);
  save_backtrace("heap-gc-warning");
  print_backtrace_named("heap-gc-warning");
#endif
}
#endif

static void
initHBase(void)
{ void *p = GC_MALLOC(sizeof(void*));
  uintptr_t base = (uintptr_t)p;

  GC_FREE(p);				/* Keep leak-detection happy */
  base &= ~0xfffff;			/* round down 1m */
  GD->heap_base = base;			/* for pointer <-> int conversion */
}


void
initAlloc(void)
{ static int done = FALSE;

  if ( done )
    return;
  done = TRUE;

#if defined(_DEBUG) && defined(__WINDOWS__) && 0
  _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF|
		 _CRTDBG_CHECK_CRT_DF|
		 //_CRTDBG_CHECK_ALWAYS_DF|	/* very expensive */
		 //_CRTDBG_DELAY_FREE_MEM_DF|   /* does not reuse freed mem */
		 //_CRTDBG_LEAK_CHECK_DF|
		 0);
#endif

#ifdef HAVE_BOEHM_GC
  GC_INIT();
  GC_set_warn_proc(heap_gc_warn_proc);
#endif

#if defined(HAVE_MTRACE) && defined(O_MAINTENANCE)
  if ( getenv("MALLOC_TRACE") )		/* glibc malloc tracer */
    mtrace();
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FORCED_MALLOC_BASE is a debugging aid for  me   to  force  the system to
allocate memory starting from a specific   address.  Probably only works
properly on Linux. Don't bother with it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef FORCED_MALLOC_BASE
  start_memory((void *)FORCED_MALLOC_BASE);
  Sdprintf("FORCED_MALLOC_BASE at 0x%08x\n", FORCED_MALLOC_BASE);
#endif
#if O_MALLOC_DEBUG
  malloc_debug(O_MALLOC_DEBUG);
#endif

  initHBase();
}


		 /*******************************
		 *	      PREDICATES	*
		 *******************************/

#ifdef HAVE_BOEHM_GC
static
PRED_IMPL("garbage_collect_heap", 0, garbage_collect_heap, 0)
{ GC_gcollect();

  return TRUE;
}
#endif

BeginPredDefs(alloc)
#ifdef HAVE_BOEHM_GC
  PRED_DEF("garbage_collect_heap", 0, garbage_collect_heap, 0)
#endif
EndPredDefs
