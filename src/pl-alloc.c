/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
			      VU University Amsterdam

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

#include "pl-incl.h"
#include "os/pl-cstack.h"

#define LOCK()   PL_LOCK(L_ALLOC)
#define UNLOCK() PL_UNLOCK(L_ALLOC)
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
  LOCK();
  l->next = GC_lingering;
  GC_lingering = l->next;
  UNLOCK();
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


int
outOfStack(void *stack, stack_overflow_action how)
{ GET_LD
  Stack s = stack;
  const char *msg = "unhandled stack overflow";

  if ( LD->outofstack )
  { Sdprintf("[Thread %d]: failed to recover from %s-overflow\n",
	     PL_thread_self(), s->name);
    print_backtrace_named(msg);
    save_backtrace("crash");
    print_backtrace_named("crash");
    fatalError("Sorry, cannot continue");

    return FALSE;				/* NOTREACHED */
  }

  save_backtrace(msg);

  LD->trim_stack_requested = TRUE;
  LD->exception.processing = TRUE;
  LD->outofstack = stack;

  switch(how)
  { case STACK_OVERFLOW_THROW:
    case STACK_OVERFLOW_RAISE:
    { fid_t fid;

      blockGC(0 PASS_LD);

      if ( (fid=PL_open_foreign_frame()) )
      { PL_clearsig(SIG_GC);
	s->gced_size = 0;			/* after handling, all is new */
	if ( !PL_unify_term(LD->exception.tmp,
			    PL_FUNCTOR, FUNCTOR_error2,
			      PL_FUNCTOR, FUNCTOR_resource_error1,
			        PL_ATOM, ATOM_stack,
			      PL_CHARS, s->name) )
	  fatalError("Out of stack inside out-of-stack handler");

	if ( how == STACK_OVERFLOW_THROW )
	{ PL_close_foreign_frame(fid);
	  unblockGC(0 PASS_LD);
	  PL_throw(LD->exception.tmp);
	  warning("Out of %s stack while not in Prolog!?", s->name);
	  assert(0);
	} else
	{ PL_raise_exception(LD->exception.tmp);
	}

	PL_close_foreign_frame(fid);
      }

      unblockGC(0 PASS_LD);
      fail;
    }
  }
  assert(0);
  fail;
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

static Word
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
      *t++ = *p++ & 0xff;
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
packed into two `guards words'.  An   intermediate  structure is used to
ensure the possibility of  word-aligned  copy   of  the  data. Structure
assignment is used here  to  avoid  a   loop  for  different  values  of
WORDS_PER_DOUBLE.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ word w[WORDS_PER_DOUBLE];
} fword;


void
doublecpy(void *to, void *from)
{ fword *t = to;
  fword *f = from;

  *t = *f;
}


double					/* take care of alignment! */
valFloat__LD(word w ARG_LD)
{ fword *v = (fword *)valIndirectP(w);
  union
  { double d;
    fword  l;
  } val;

  val.l = *v;

  return val.d;
}


int
put_double(Word at, double d, int flags ARG_LD)
{ Word p;
  word m = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
  union
  { double d;
    fword  l;
  } val;
  fword *v;

  if ( flags != ALLOW_CHECKED && !hasGlobalSpace(2+WORDS_PER_DOUBLE) )
  { int rc = ensureGlobalSpace(2+WORDS_PER_DOUBLE, flags);

    if ( rc != TRUE )
      return rc;
  }
  p = gTop;
  gTop += 2+WORDS_PER_DOUBLE;

  *at = consPtr(p, TAG_FLOAT|STG_GLOBAL);

  val.d = d;
  *p++ = m;
  v = (fword *)p;
  *v++ = val.l;
  p = (Word) v;
  *p = m;

  return TRUE;
}


		 /*******************************
		 *	  64-BIT INTEGERS	*
		 *******************************/

#ifdef INT64_ALIGNMENT

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
{
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


#undef LOCK
#undef UNLOCK

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
