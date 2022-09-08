/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2021, University of Amsterdam
                              VU University Amsterdam
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

#define EMIT_ALLOC_INLINES 1
#include "pl-incl.h"
#include "os/pl-cstack.h"
#include "pl-dict.h"
#include "pl-arith.h"
#include "pl-variant.h"
#include "pl-prims.h"
#include "pl-gvar.h"
#include "pl-gc.h"
#include "pl-fli.h"
#include "pl-setup.h"
#include "pl-pro.h"
#include <math.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#ifdef HAVE_TCMALLOC_EXTENSION_C_H
/* Provides error checking for the weak declarations below */
#include <gperftools/malloc_extension_c.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#define MMAP_STACK 1
#include <sys/mman.h>
#include <unistd.h>
#ifndef MAP_ANONYMOUS
#ifdef MAP_ANON
#define MAP_ANONYMOUS MAP_ANON
#else
#define MAP_ANONYMOUS 0
#endif
#endif
#endif

#undef LD
#define LD LOCAL_LD

#if ALLOC_DEBUG
#define ALLOC_FREE_MAGIC 0xFB
#define ALLOC_NEW_MAGIC  0xF9
#endif

/* Emit the non-inline definitions here */
#include "pl-alloc-inline.h"

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
{ if ( GD->cleaning != CLN_DATA )
  { linger_list *c = allocHeapOrHalt(sizeof(*c));
    linger_list *o;

    c->generation = global_generation();
    c->object	  = object;
    c->unalloc	  = unalloc;

    do
    { o = *list;
      c->next = o;
    } while( !COMPARE_AND_SWAP_PTR(list, o, c) );
  } else
  { (*unalloc)(object);
  }
}

void
free_lingering(linger_list **list, gen_t generation)
{ linger_list **p = list;
  linger_list *c = *list;

  while ( c )
  { if ( c->generation < generation )
    { while ( !COMPARE_AND_SWAP_PTR(p, c, c->next) )
      { p = &(*p)->next;
      }
      (*c->unalloc)(c->object);
      freeHeap(c, sizeof(*c));
    } else
    { p = &(*p)->next;
    }
    c = *p;
  }
}

		/********************************
		*             STACKS            *
		*********************************/

int
enableSpareStack(Stack s, int always)
{ if ( s->spare && (roomStackP(s) < s->def_spare || always) )
  { DEBUG(MSG_SPARE_STACK,
	  Sdprintf("Enabling spare on %s: %zd bytes\n", s->name, s->spare));
    s->max = addPointer(s->max, s->spare);
    s->spare = 0;
    return TRUE;
  }

  return FALSE;
}


void
enableSpareStacks(void)
{ GET_LD

  enableSpareStack((Stack)&LD->stacks.local,  FALSE);
  enableSpareStack((Stack)&LD->stacks.global, FALSE);
  enableSpareStack((Stack)&LD->stacks.trail,  FALSE);
}


static intptr_t
env_frames(LocalFrame fr)
{ intptr_t count = 0;

  while(fr)
  { count++;
    fr = parentFrame(fr);
  }

  return count;
}


static intptr_t
choice_points(Choice chp)
{ GET_LD

  intptr_t count = 0;
  QueryFrame qfr = LD->query;

  while( chp )
  { count++;

    if ( chp->parent )
    { chp = chp->parent;
    } else if ( qfr )
    { assert(qfr->magic == QID_MAGIC);
      chp = qfr->saved_bfr;
      qfr = qfr->parent;
    }
  }

  return count;
}


#define MAX_CYCLE     20
#define CYCLE_CTX      1
#define MAX_PRE_LOOP  20
#define MIN_REPEAT   100

typedef struct cycle_entry
{ LocalFrame frame;
} cycle_entry;

#define is_variant_frame(fr1, fr2) LDFUNC(is_variant_frame, fr1, fr2)
static int
is_variant_frame(DECL_LD LocalFrame fr1, LocalFrame fr2)
{ if ( fr1->predicate == fr2->predicate )
  { size_t arity = fr1->predicate->functor->arity;
    size_t i;

    for(i=0; i<arity; i++)
    { if ( !is_variant_ptr(argFrameP(fr1, i), argFrameP(fr2, i)) )
	return FALSE;
    }

    return TRUE;
  }

  return FALSE;
}


#define non_terminating_recursion(fr0, ce, is_cycle) LDFUNC(non_terminating_recursion, fr0, ce, is_cycle)
static int
non_terminating_recursion(DECL_LD LocalFrame fr0,
			  cycle_entry ce[MAX_CYCLE],
			  int *is_cycle)
{ int depth, mindepth = 1, repeat;
  LocalFrame fr, ctx;

  ce[0].frame = fr0;

again:
  for( fr=parentFrame(fr0), depth=1;
       fr && depth<MAX_CYCLE;
       depth++, fr = parentFrame(fr) )
  { if ( fr->predicate == fr0->predicate && depth >= mindepth )
      break;
    ce[depth].frame = fr;
  }

  if ( !fr || depth >= MAX_CYCLE )
    return 0;

  *is_cycle = is_variant_frame(fr0, fr);
  ctx = fr;

  for(repeat=MIN_REPEAT; fr && --repeat > 0; )
  { int i;

    for(i=0; fr && i<depth; i++, fr = parentFrame(fr))
    { if ( fr->predicate != ce[i].frame->predicate )
      { mindepth = depth+1;
	if ( mindepth > MAX_CYCLE )
	  return 0;
	// Sdprintf("Cycle not repeated at %d\n", i);
	goto again;
      }
    }
  }

  if ( repeat == 0 )
  { int nctx = CYCLE_CTX;

    for(fr=ctx; fr && nctx-- > 0; fr = parentFrame(fr))
      ce[depth++].frame = fr;

    return depth;
  }

  return 0;
}

#define find_non_terminating_recursion(fr, ce, is_cycle) LDFUNC(find_non_terminating_recursion, fr, ce, is_cycle)
static int
find_non_terminating_recursion(DECL_LD LocalFrame fr, cycle_entry ce[MAX_CYCLE],
			       int *is_cycle)
{ int max_pre_loop = MAX_PRE_LOOP;

  for(; fr && max_pre_loop; fr = parentFrame(fr), max_pre_loop--)
  { int len;

    if ( (len=non_terminating_recursion(fr, ce, is_cycle)) )
      return len;
  }

  return 0;
}


#define top_of_stack(fr, ce, maxdepth) LDFUNC(top_of_stack, fr, ce, maxdepth)
static int
top_of_stack(DECL_LD LocalFrame fr, cycle_entry ce[MAX_CYCLE], int maxdepth)
{ int depth;

  for(depth = 0; fr && depth < maxdepth; fr = parentFrame(fr), depth++)
  { ce[depth].frame = fr;
  }

  return depth;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Push a goal to the stack. This   code uses low-level primitives to avoid
stack shifts. The goal is a term `Module:Head`, where each Head argument
is a primitive (var, atom, number, string), a term `[Length]` for a list
of length Length, a term `[cyclic_term]` if the list is cyclic otherwise
a term `Name/Arity` to indicate the principal functor.

Returns `0` if there is no enough space to store this term.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static size_t
size_frame_term(LocalFrame fr)
{ GET_LD
  size_t arity = fr->predicate->functor->arity;
  size_t size = 4 + 3 + arity+1;
  size_t i;

  for(i=0; i<arity; i++)
  { Word p = argFrameP(fr, i);
    deRef(p);

    if ( isTerm(*p) )
      size += 3;				/* one of f/n, [Len] or [c] */
  }

  return size;
}


static word
push_goal(LocalFrame fr)
{ GET_LD
  size_t arity = fr->predicate->functor->arity;
  size_t i;
  Word p = gTop;
  word r = consPtr(p, STG_GLOBAL|TAG_COMPOUND);

  p[0] = FUNCTOR_frame3;
  p[1] = consInt(fr->level);
  p[2] = consPtr(&p[4], STG_GLOBAL|TAG_COMPOUND);
  p[3] = ATOM_nil;				/* reserved */
  p += 4;

  p[0] = FUNCTOR_colon2;
  p[1] = fr->predicate->module->name;
  if ( arity > 0 )
  { Word ad;					/* argument descriptions */

    p[2] = consPtr(&p[3], STG_GLOBAL|TAG_COMPOUND);
    p += 3;
    p[0] = fr->predicate->functor->functor;
    p++;
    ad = p+arity;
    for(i=0; i<arity; i++)
    { Word a = argFrameP(fr, i);

      deRef(a);
      if ( isTerm(*a) )
      { *p++ = consPtr(ad, STG_GLOBAL|TAG_COMPOUND);

	if ( isList(*a) )
	{ Word tail;
	  intptr_t len = skip_list(a, &tail);

	  *ad++ = FUNCTOR_dot2;
	  deRef(tail);
	  if ( isList(*tail) )
	  { *ad++ = ATOM_cyclic_term;
	    *ad++ = ATOM_nil;
	  } else
	  { *ad++ = consInt(len);
	    if ( isTerm(*tail) )
	      *ad++ = ATOM_compound;
	    else
	      *ad++ = *tail;
	  }
	} else
	{ FunctorDef f = valueFunctor(functorTerm(*a));

	  *ad++ = FUNCTOR_divide2;
	  *ad++ = f->name;
	  *ad++ = consInt(f->arity);
	}
      } else
      { *p++ = *a;
      }
    }
    gTop = ad;
  } else
  { p[2] = fr->predicate->functor->name;
    gTop = &p[3];
  }

  return r;
}


static word
push_cycle(cycle_entry ce[MAX_CYCLE], int depth)
{ GET_LD
  size_t size = depth*3;
  int i;

  for(i=0; i<depth; i++)
  { size += size_frame_term(ce[i].frame);
  }

  if ( gTop+size < gMax )
  { Word p  = gTop;
    word r  = consPtr(p, STG_GLOBAL|TAG_COMPOUND);

    gTop = p+depth*3;
    for(i=0; i<depth; i++, p+=3)
    { p[0] = FUNCTOR_dot2;
      p[1] = push_goal(ce[i].frame);
      if ( i+1 < depth )
	p[2] = consPtr(&p[3], STG_GLOBAL|TAG_COMPOUND);
      else
	p[2] = ATOM_nil;
    }

    return r;
  } else
    return 0;
}


#define push_stack(ce, depth, name, pp) LDFUNC(push_stack, ce, depth, name, pp)
static void
push_stack(DECL_LD cycle_entry ce[MAX_CYCLE], int depth, atom_t name, Word *pp)
{ word w;
  Word p = *pp;

  gTop = p+2;
  if ( (w=push_cycle(ce, depth)) )
  { *p++ = w;
    *p++ = name;
  } else
  { gTop = p;
  }

  *pp = p;
}



static word
push_overflow_context(Stack stack, int extra)
{ GET_LD
  int keys = 7;

  if ( gTop+2*keys+extra < gMax )
  { Word p = gTop;
    Word dict = p;
    cycle_entry ce[MAX_CYCLE+CYCLE_CTX];
    int depth;

    *p++ = dict_functor(1);
    *p++ = ATOM_stack_overflow;			/* dict tag */
    *p++ = consInt(LD->stacks.limit/1024);
    *p++ = ATOM_stack_limit;			/* overflow */
    *p++ = consInt(usedStack(local)/1024);	/* K-bytes to avoid small int */
    *p++ = ATOM_localused;
    *p++ = consInt(usedStack(global)/1024);
    *p++ = ATOM_globalused;
    *p++ = consInt(usedStack(trail)/1024);
    *p++ = ATOM_trailused;
    if ( environment_frame )
    { *p++ = consUInt(environment_frame->level);
      *p++ = ATOM_depth;
    }
    *p++ = consInt(env_frames(environment_frame));
    *p++ = ATOM_environments;
    *p++ = consInt(choice_points(LD->choicepoints));
    *p++ = ATOM_choicepoints;
    gTop = p;

    if ( roomStack(local) < LD->stacks.local.def_spare + LOCAL_MARGIN )
    { int is_cycle;

      if ( (depth=find_non_terminating_recursion(environment_frame, ce,
						 &is_cycle)) )
      { push_stack(ce, depth, is_cycle ? ATOM_cycle : ATOM_non_terminating,
		   &p);
      } else if ( (depth=top_of_stack(environment_frame, ce, 5)) )
      { push_stack(ce, depth, ATOM_stack, &p);
      }
    } else if ( (depth=top_of_stack(environment_frame, ce, 5)) )
    { push_stack(ce, depth, ATOM_stack, &p);
    }

    *dict = dict_functor((p-dict-2)/2);		/* final functor */

    dict_order(dict, NULL);

    return consPtr(dict, STG_GLOBAL|TAG_COMPOUND);
  } else
    return PL_new_atom(stack->name); /* The stack names are built-in atoms */
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

Out of stack exception context:
  - Stack sizes (Local, Global, Trail)
  - Goal stack depth
  - Ratio choice points/stack frames?
  - Is there unbound recursion?
  - Ratio global data reachable through environments and
    choice points (requires running GC)
  - Global storage only reachable through choice points
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
    print_backtrace_named(msg);
  }

  enableSpareStacks();
  LD->trim_stack_requested = TRUE;
  LD->exception.processing = TRUE;
  LD->outofstack = stack;

  switch(how)
  { case STACK_OVERFLOW_THROW:
    case STACK_OVERFLOW_RAISE:
    { word ctx = push_overflow_context(s, 6);

      if ( gTop+5 < gMax )
      { Word p = gTop;

	p[0] = FUNCTOR_error2;			/* see (*) above */
	p[1] = consPtr(&p[3], TAG_COMPOUND|STG_GLOBAL);
	p[2] = ctx;
	p[3] = FUNCTOR_resource_error1;
	p[4] = ATOM_stack;
	gTop += 5;

	*valTermRef(LD->exception.bin) = consPtr(p, TAG_COMPOUND|STG_GLOBAL);
	freezeGlobal();
      } else
      { Sdprintf("ERROR: Out of global-stack.\n"
		 "ERROR: No room for exception term.  Aborting.\n");
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
    case STACK_OVERFLOW:    s = &GD->combined_stack;         break;
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
f_pushArgumentStack(DECL_LD Word p)
{ Word *newbase;
  size_t newsize = nextStackSize((Stack)&LD->stacks.argument, 1);

  if ( newsize &&
       (newsize = stack_nalloc(newsize)) &&
       (newbase = stack_realloc(aBase, newsize)) )
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
allocGlobal(DECL_LD size_t n)
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
allocGlobalNoShift(DECL_LD size_t n)
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
put_int64(DECL_LD Word at, int64_t l, int flags)
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
allocString(DECL_LD size_t len)
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
  Word p = allocString(len+1);

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

    if ( !(g = allocString(len+1)) )
      return 0;
    t = (unsigned char *)&g[1];
    *t++ = 'B';
    for(p=s; p<e; )
      *t++ = (unsigned char)(*p++ & 0xff);
  } else				/* wide string */
  { char *t;
    pl_wchar_t *w;

    if ( !(g = allocString((len+1)*sizeof(pl_wchar_t))) )
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
getCharsString(DECL_LD word w, size_t *len)
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
getCharsWString(DECL_LD word w, size_t *len)
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
put_double(DECL_LD Word at, double d, int flags)
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

  if ( isnan(d) )
    d = PL_nan();			/* SWI-Prolog canonical 1.5NaN */

  *p++ = m;
  memcpy(p, &d, sizeof(d));
  p += WORDS_PER_DOUBLE;
  *p = m;

  return TRUE;
}


/* valBignum(DECL_LD word w) moved to pl-inline.h */

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

word
globalIndirectFromCode(Code *PC)
{ GET_LD
  struct word_and_Code retval = VM_globalIndirectFromCode(*PC);
  *PC = retval.code;
  return retval.word;
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
}

		 /*******************************
		 *	    MMAP STACKS		*
		 *******************************/

#ifdef  MMAP_STACK
#define MMAP_THRESHOLD 32768

typedef struct
{ size_t size;				/* Size (including header) */
  int	 mmapped;			/* Is mmapped? */
  double data[1];			/* ensure alignment */
} map_region;

#define SA_OFFSET offsetof(map_region, data)

static size_t
pgsize(void)
{ static size_t sz = 0;

  if ( !sz )
    sz = sysconf(_SC_PAGESIZE);

  return sz;
}

static inline size_t
roundpgsize(size_t sz)
{ size_t r = pgsize();

  return ((sz+r-1)/r)*r;
}

size_t
tmp_nalloc(size_t req)
{ if ( req < MMAP_THRESHOLD-SA_OFFSET )
    return req;

  return roundpgsize(req+SA_OFFSET)-SA_OFFSET;
}

size_t
tmp_nrealloc(void *mem, size_t req)
{ if ( mem )
  { map_region *reg = (map_region *)((char*)mem-SA_OFFSET);

    if ( !reg->mmapped && req < MMAP_THRESHOLD-SA_OFFSET )
      return req;

    return roundpgsize(req+SA_OFFSET)-SA_OFFSET;
  }

  return tmp_nalloc(req);
}


static size_t
tmp_malloc_size(void *mem)
{ if ( mem )
  { map_region *reg = (map_region *)((char*)mem-SA_OFFSET);
    return reg->size-SA_OFFSET;
  }

  return 0;
}

void *
tmp_malloc(size_t req)
{ map_region *reg;
  int mmapped;

  req += SA_OFFSET;
  if ( req < MMAP_THRESHOLD )
  { reg = malloc(req);
    mmapped = FALSE;
  } else
  { req = roundpgsize(req);

    reg = mmap(NULL, req,
	       (PROT_READ|PROT_WRITE),
	       (MAP_PRIVATE|MAP_ANONYMOUS),
	       -1, 0);
    if ( reg == MAP_FAILED )
      reg = NULL;
    mmapped = TRUE;
  }

  if ( reg )
  { reg->size    = req;
    reg->mmapped = mmapped;
#ifdef O_DEBUG
    memset(reg->data, 0xFB, req-SA_OFFSET);
#endif

    return reg->data;
  }

  return NULL;
}


void *
tmp_realloc(void *mem, size_t req)
{ if ( mem )
  { map_region *reg = (map_region *)((char*)mem-SA_OFFSET);

    req += SA_OFFSET;
    if ( !reg->mmapped )
    { if ( req < MMAP_THRESHOLD )
      { map_region *nw = realloc(reg, req);
	if ( nw )
	{ nw->size = req;
	  return nw->data;
	}
	return NULL;
      } else				/* malloc --> mmap */
      { void *nw = tmp_malloc(req-SA_OFFSET);
	if ( nw )
	{ size_t copy = reg->size;

	  if ( copy > req )
	    copy = req;

	  memcpy(nw, mem, copy-SA_OFFSET);
	  free(reg);
	}
	return nw;
      }
    } else
    { req = roundpgsize(req);

      if ( reg->size != req )
      { if ( reg->size > req )
	{ size_t trunk = reg->size-req;

	  munmap((char*)reg+req, trunk);
	  reg->size = req;

	  return reg->data;
	} else
	{ void *ra = tmp_malloc(req);

	  if ( ra )
	  { memcpy(ra, mem, reg->size-SA_OFFSET);
#ifdef O_DEBUG
	    memset((char*)ra+reg->size-SA_OFFSET, 0xFB,
		   req-(reg->size-SA_OFFSET));
#endif
	    tmp_free(mem);
	  }

	  return ra;
	}
      } else
      { return mem;
      }
    }
  } else
  { return tmp_malloc(req);
  }
}


void
tmp_free(void *mem)
{ if ( mem )
  { map_region *reg = (map_region *)((char*)mem-SA_OFFSET);

    if ( reg->mmapped )
      munmap(reg, reg->size);
    else
      free(reg);
  }
}

#else /*MMAP_STACK*/

size_t
tmp_nalloc(size_t req)
{ return req;
}

size_t
tmp_nrealloc(void *mem, size_t req)
{ (void)mem;

  return req;
}

size_t
tmp_malloc_size(void *mem)
{ if ( mem )
  { size_t *sp = mem;
    return sp[-1];
  }

  return 0;
}

void *
tmp_malloc(size_t size)
{ void *mem = malloc(size+sizeof(size_t));

  if ( mem )
  { size_t *sp = mem;
    *sp++ = size;
#ifdef O_DEBUG
    memset(sp, 0xFB, size);
#endif

    return sp;
  }

  return NULL;
}

void *
tmp_realloc(void *old, size_t size)
{ size_t *sp = old;
  size_t osize = *--sp;
  void *mem;

#ifdef O_DEBUG
  if ( (mem = tmp_malloc(size)) )
  { memcpy(mem, old, (size>osize?osize:size));
    tmp_free(old);
    return mem;
  }
#else
  (void)osize;
  if ( (mem = realloc(sp, size+sizeof(size_t))) )
  { sp = mem;
    *sp++ = size;
    return sp;
  }
#endif

  return NULL;
}

void
tmp_free(void *mem)
{ size_t *sp = mem;
  size_t osize = *--sp;

#ifdef O_DEBUG
  memset(sp, 0xFB, osize+sizeof(size_t));
#else
  (void)osize;
#endif
  free(sp);
}

#endif /*MMAP_STACK*/

void *
stack_malloc(size_t size)
{ void *ptr = tmp_malloc(size);

  if ( ptr )
    ATOMIC_ADD(&GD->statistics.stack_space, tmp_malloc_size(ptr));

  return ptr;
}

void *
stack_realloc(void *mem, size_t size)
{ size_t osize = tmp_malloc_size(mem);
  void *ptr ;
  if(mem)
    ptr = tmp_realloc(mem, size);
  else
    ptr = tmp_malloc(size);

  if ( ptr )
  { size = tmp_malloc_size(ptr);

    if ( osize > size )
      ATOMIC_SUB(&GD->statistics.stack_space, osize-size);
    else
      ATOMIC_ADD(&GD->statistics.stack_space, size-osize);
  }

  return ptr;
}

void
stack_free(void *mem)
{ size_t size = tmp_malloc_size(mem);

  ATOMIC_SUB(&GD->statistics.stack_space, size);
  tmp_free(mem);
}

size_t
stack_nalloc(size_t req)
{ return tmp_nalloc(req);
}

size_t
stack_nrealloc(void *mem, size_t req)
{ return tmp_nrealloc(mem, req);
}


		 /*******************************
		 *	       TCMALLOC		*
		 *******************************/

WEAK_DECLARE(int, MallocExtension_GetNumericProperty, (const char *, size_t *));
WEAK_DECLARE(int, MallocExtension_SetNumericProperty, (const char *, size_t));
WEAK_DECLARE(void, MallocExtension_MarkThreadIdle, (void));
WEAK_DECLARE(void, MallocExtension_MarkThreadTemporarilyIdle, (void));
WEAK_DECLARE(void, MallocExtension_MarkThreadBusy, (void));
WEAK_DECLARE(void, MallocExtension_ReleaseFreeMemory, (void));

static const char* tcmalloc_properties[] =
{ "generic.current_allocated_bytes",
  "generic.heap_size",
  "tcmalloc.max_total_thread_cache_bytes",
  "tcmalloc.current_total_thread_cache_bytes",
  "tcmalloc.central_cache_free_bytes",
  "tcmalloc.transfer_cache_free_bytes",
  "tcmalloc.thread_cache_free_bytes",
  "tcmalloc.pageheap_free_bytes",
  "tcmalloc.pageheap_unmapped_bytes",
  NULL
};

static foreign_t
malloc_property(term_t prop, control_t handle)
{ GET_LD
  const char **pname;

  switch( PL_foreign_control(handle) )
  { case PL_FIRST_CALL:
    { atom_t name;
      size_t arity;

      if ( PL_get_name_arity(prop, &name, &arity) && arity == 1 )
      { const char *s = PL_atom_nchars(name, NULL);

	if ( s )
	{ pname = tcmalloc_properties;

	  for(; *pname; pname++)
	  { if ( streq(s, *pname) )
	    { size_t val;

	      if ( WEAK_FUNC(MallocExtension_GetNumericProperty)(*pname, &val) )
	      { term_t a = PL_new_term_ref();
		_PL_get_arg(1, prop, a);
		return PL_unify_uint64(a, val);
	      }
	    }
	  }
	}

	return FALSE;
      } else if ( PL_is_variable(prop) )
      { pname = tcmalloc_properties;
	goto enumerate;
      }
    }
    case PL_REDO:
    { fid_t fid;

      pname = PL_foreign_context_address(handle);
    enumerate:

      fid = PL_open_foreign_frame();
      for(; *pname; pname++)
      { size_t val;

	if ( WEAK_FUNC(MallocExtension_GetNumericProperty)(*pname, &val) )
	{ if ( PL_unify_term(prop, PL_FUNCTOR_CHARS, *pname, 1,
			             PL_INT64, val) )
	  { PL_close_foreign_frame(fid);
	    pname++;
	    if ( *pname )
	      PL_retry_address(pname);
	    else
	      return TRUE;
	  }
	}

	if ( PL_exception(0) )
	  return FALSE;
	PL_rewind_foreign_frame(fid);
      }
      PL_close_foreign_frame(fid);

      return FALSE;
    }
    case PL_CUTTED:
    { return TRUE;
    }
    default:
    { assert(0);
      return FALSE;
    }
  }
}

static foreign_t
set_malloc(term_t prop)
{ GET_LD
  atom_t name;
  size_t arity;

  if ( PL_get_name_arity(prop, &name, &arity) && arity == 1 )
  { const char *s = PL_atom_nchars(name, NULL);
    term_t a = PL_new_term_ref();
    size_t val;

    if ( !PL_get_arg(1, prop, a) ||
	 !PL_get_size_ex(a, &val) )
      return FALSE;

    if ( s )
    { const char **pname = tcmalloc_properties;

      for(; *pname; pname++)
      { if ( streq(s, *pname) )
	{ if ( WEAK_FUNC(MallocExtension_SetNumericProperty)(*pname, val) )
	    return TRUE;
	  else
	    return PL_permission_error("set", "malloc_property", prop);
	}
      }

      return PL_domain_error("malloc_property", prop);
    }
  }

  return PL_type_error("malloc_property", prop);
}


size_t
heapUsed(void)
{ size_t val;

  if (WEAK_TRY_CALL(MallocExtension_GetNumericProperty, "generic.current_allocated_bytes", &val))
  {
#ifdef MMAP_STACK
    val += GD->statistics.stack_space;
#endif

    return val;
  }

  return 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Try to initialize tcmalloc(). Note that we   get all the functions using
dlsym() rather than as static symbols  because   we  do not know whether
tcmalloc is really there. Even if the   symbols  are present the library
may be overruled.

Returns 0 if tcmalloc is not present or not enabled.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int is_tcmalloc = FALSE;

static int
initTCMalloc(void)
{ static int done = FALSE;
  int set = 0;

  if ( done )
    return is_tcmalloc;
  done = TRUE;

  if ( WEAK_IMPORT(MallocExtension_GetNumericProperty) )
  { size_t in_use;

    if ( WEAK_FUNC(MallocExtension_GetNumericProperty)("generic.current_allocated_bytes", &in_use) &&
	 in_use > 100000 )
    { is_tcmalloc = TRUE;
      PL_set_prolog_flag("malloc", PL_ATOM, "tcmalloc");
    } else
    { return 0;
    }

    PL_register_foreign_in_module("system", "malloc_property", 1, malloc_property,
			PL_FA_NONDETERMINISTIC);
    set++;
  }
  if ( WEAK_IMPORT(MallocExtension_SetNumericProperty) )
  { PL_register_foreign_in_module("system", "set_malloc", 1, set_malloc, 0);
    set++;
  }

  WEAK_IMPORT(MallocExtension_MarkThreadIdle);
  WEAK_IMPORT(MallocExtension_MarkThreadTemporarilyIdle);
  WEAK_IMPORT(MallocExtension_MarkThreadBusy);
  WEAK_IMPORT(MallocExtension_ReleaseFreeMemory);

  return set;
}

static int is_ptmalloc = FALSE;
#ifdef HAVE_MALLINFO2
WEAK_DECLARE(struct mallinfo2, mallinfo2, (void));
#elif defined(HAVE_MALLINFO)
WEAK_DECLARE(struct mallinfo, mallinfo, (void));
#endif
WEAK_DECLARE(int, malloc_trim, (size_t pad));

static int
initPTMalloc(void)
{ static int done = FALSE;

  if ( done )
    return is_ptmalloc;
  done = TRUE;

  size_t uordblks = 0;
#ifdef HAVE_MALLINFO2
  if ( WEAK_IMPORT(mallinfo2) )
  { uordblks = WEAK_FUNC(mallinfo2)().uordblks;
  }
#elif defined(HAVE_MALLINFO)
  if ( WEAK_IMPORT(mallinfo) && uordblks < 100000 )
  { uordblks = WEAK_FUNC(mallinfo)().uordblks;
  }
#endif

  if ( uordblks > 100000 )
  { PL_set_prolog_flag("malloc", PL_ATOM, "ptmalloc");
    is_ptmalloc = TRUE;
  }

  WEAK_IMPORT(malloc_trim); /* hope to have trim but it doesn't change the test */

  return is_ptmalloc;
}


int
initMalloc(void)
{ return ( initTCMalloc() ||
	   initPTMalloc() ||
	   FALSE
	 );
}


/** trim_heap
 *
 * Release as much as possible memory to the system.
 */

static
PRED_IMPL("trim_heap", 0, trim_heap, 0)
{ if ( is_tcmalloc )
    WEAK_TRY_CALL(MallocExtension_ReleaseFreeMemory);
  else
    WEAK_TRY_CALL(malloc_trim, 0);

  return TRUE;
}


/** thread_idle(:Goal, +How)
 *
 */

static
PRED_IMPL("thread_idle", 2, thread_idle, PL_FA_TRANSPARENT)
{ PRED_LD
  int rc;
  atom_t how;

  if ( !PL_get_atom_ex(A2, &how) )
    return FALSE;

  if ( how == ATOM_short )
  { trimStacks(TRUE);
    WEAK_TRY_CALL(MallocExtension_MarkThreadTemporarilyIdle);
  } else if ( how == ATOM_long )
  { LD->trim_stack_requested = TRUE;
    garbageCollect(GC_USER);
    LD->trim_stack_requested = FALSE;
    WEAK_TRY_CALL(MallocExtension_MarkThreadIdle);
  }

  rc = callProlog(NULL, A1, PL_Q_PASS_EXCEPTION, NULL);

  WEAK_TRY_CALL(MallocExtension_MarkThreadBusy);

  return rc;
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
  PRED_DEF("thread_idle", 2, thread_idle, PL_FA_TRANSPARENT)
  PRED_DEF("trim_heap",   0, trim_heap,   0)
EndPredDefs
