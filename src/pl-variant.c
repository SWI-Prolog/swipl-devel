/*  Part of SWI-Prolog

    Author:        Kuniaki Mukai Wielemaker
    E-mail:        mukai@sfc.keio.ac.jp
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2016, Kuniaki Mukai
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

/*#define O_DEBUG 1*/
#include "pl-variant.h"
#include "pl-gc.h"
#include "pl-setup.h"


		/********************************
		*	     VARIANT		*
		*********************************/

#define consVar(w) (((word)(w)<<LMASK_BITS) | TAG_VAR | FIRST_MASK)
#define valVar(w)  ((size_t)((word)(w) >> LMASK_BITS))

#define TAG_COMPOUND_x    (STG_STATIC|TAG_COMPOUND)
#define isCompound_x(w) \
	(( (word)(w) & (STG_MASK|TAG_MASK) ) == TAG_COMPOUND_x )
#define valCompound_x(w)  ((size_t)((word)(w) >> LMASK_BITS))
#define consCompound_x(i) (((word)(i)<<LMASK_BITS) | TAG_COMPOUND_x)

#define node_bp(p)	((p)->bp)
#define node_orig(p)	((p)->orig)
#define node_variant(p) ((p)->a)
#define node_isom(p)	((p)->b)

typedef struct aWork
{ Word		left;			/* left term arguments */
  Word		right;			/* right term arguments */
  size_t	arg;
  size_t	arity;
} aWork;


typedef struct argPairs
{ aWork	work;				/* current work */
  segstack	stack;
  aWork		first_chunk[16];
} argPairs;


typedef struct node
{ Word  bp;			/* pointer to the original term */
  word  orig;			/* saved word */
  int	a;			/* variant at left */
  int	b;			/* link to isomophic node */
} node;


static void
init_agenda(argPairs *a)
{ initSegStack(&a->stack, sizeof(aWork),
	       sizeof(a->first_chunk), a->first_chunk);
}


static inline bool
push_start_args(argPairs *a, Word left, Word right, size_t arity)  /* plural */
{ a->work.left  = left;
  a->work.right = right;
  a->work.arg   = 0;
  a->work.arity = arity;

  return true;
}

static inline bool
push_args(argPairs *a, Word left, Word right, size_t arity)  /* plural */
{ if ( !pushSegStack(&a->stack, a->work, aWork) )
    return false;

  return push_start_args(a, left, right, arity);
}

static inline bool
next_arg(argPairs *a, Word *lp, Word *rp)   /* singular (not plural !) */
{ while( a->work.arg >= a->work.arity)
  { if ( !popSegStack(&a->stack, &a->work, aWork) )
      return false;
  }

  *lp = a->work.left;
  *rp = a->work.right;

  a->work.arg++;
  a->work.left++;
  a->work.right++;

  return true;
}

static inline node *
Node(size_t i, Buffer buf)
{ return  &fetchBuffer(buf, i, node);
}

static inline bool
add_node_buffer(Buffer b, node *obj)
{ if ( ((b->top) + sizeof(node)) > (b->max) )
  { if ( !growBuffer(b, sizeof(node)) )
      return false;
  }

  *((node *)(b)->top) = *obj;
  b->top += sizeof(node);

  return true;
}

static size_t
var_id(Word p, Buffer buf)
{ word w = *p;

  if ( (w&FIRST_MASK) )
  { return valVar(w);		/* node id truncated to int: */
  } else				/* < 2^31 nodes */
  { size_t n = entriesBuffer(buf, node);
    node new = {p, w, 0, 0};

    if ( !add_node_buffer((Buffer)buf, &new) )
      return MEMORY_OVERFLOW;

    *p = consVar(n);
    return n;
  }
}

static size_t
term_id(Word p, Buffer buf)
{ word w = *p;

  if ( isCompound_x(w) )
  { return valCompound_x(w);
  } else
  { size_t n = entriesBuffer(buf, node);
    node new = {p, w, 0, 0};

    if ( !add_node_buffer((Buffer)buf, &new) )
      return MEMORY_OVERFLOW;

    *p = consCompound_x(n);
    return n;
  }
}

static inline int
Root(int i, node **r, Buffer buf)
{ int k;
  node * n;

  do
  { k = i;
    n = Node(i, buf);
    DEBUG(CHK_SECURE, assert(n != NULL));
    i = node_isom(n);
  } while ( i != 0 );

  *r = n;

  return k;
}

#define univ(t, d, a) LDFUNC(univ, t, d, a)
static inline void
univ(DECL_LD word t, Word d, Word *a)
{ Functor f;

  f = valueTerm(t);
  while ( isRef(f->definition) )
    f = (Functor)unRef(f->definition);
  *d = f->definition;
  *a = f->arguments;
}

static inline void
reset_terms(node *r)
{ IS_WORD_ALIGNED(r->bp);
  *r->bp = r->orig;
}

/* isomorphic (==) */

#define isomorphic(a, i, j, buf) LDFUNC(isomorphic, a, i, j, buf)
static int
isomorphic(DECL_LD argPairs *a, int i, int j, Buffer buf)
{ Word l = NULL, r = NULL, lm, ln;
  word dm, dn;
  Word dummy = NULL;

  if ( i == j )
    return true;

  if ( !push_args(a, dummy, dummy, 1) )
    return MEMORY_OVERFLOW;

  univ(node_orig(Node(i, buf)), &dm, &lm);
  univ(node_orig(Node(j, buf)), &dn, &ln);

  if ( dm != dn )
    return false;

  if ( !push_args(a, lm, ln, arityFunctor(dm)) )
    return MEMORY_OVERFLOW;

  while( next_arg(a, &l, &r) )
  { word wl, wr;
    if ( l == NULL )
      return true;

  attvar:
    deRef(l);
    deRef(r);

    wl = *l;
    wr = *r;

    if ( tag(wl) != tag(wr) )
      return false;

    if ( tag(wl) == TAG_VAR )
    { if ( l != r )		/* identity test on variables */
	return false;
      continue;
    }

    if ( tag(wl) == TAG_ATTVAR )
    { l = valPAttVar(wl);
      r = valPAttVar(wr);
      goto attvar;
    }

    if ( wl == wr && !isTerm(wl) )
      continue;

    switch(tag(wl))
    { case TAG_ATOM:
	return false;
      case TAG_INTEGER:
	if ( storage(wl) == STG_INLINE ||
	     storage(wr) == STG_INLINE )
	  return false;
      case TAG_STRING:
      case TAG_FLOAT:
	if ( equalIndirect(wl, wr) )
	  continue;
        return false;
      case TAG_COMPOUND:
      { Word lm, ln;
	word dm, dn;
	size_t i, j;
	node  *m,  *n;

	if ( (i = term_id(l, buf)) < 0 )
	  return MEMORY_OVERFLOW;
	i = Root(i, &m, buf);

	if ( (j = term_id(r, buf)) < 0 )
	  return MEMORY_OVERFLOW;
	j = Root(j, &n, buf);

	if ( i==j )
	  continue;

	univ(node_orig(m), &dm, &lm);
	univ(node_orig(n), &dn, &ln);

	if ( dm != dn )
	  return false;

	if ( i <= j )
	  node_isom(m) = j;		/* union */
	else
	  node_isom(n) = i;

	if ( !push_args(a, lm, ln, arityFunctor(dm)) )
	  return MEMORY_OVERFLOW;

	continue;
      }
      default:
	assert(0);
    }
  }

  return true;
}

/* t =@= u */
/* returns true, false or MEMORY_OVERFLOW */

#define variant(agenda, buf) LDFUNC(variant, agenda, buf)
static int
variant(DECL_LD argPairs *agenda, Buffer buf)
{ Word l = NULL, r =NULL;

  while( next_arg(agenda, &l, &r) )
  { word wl, wr;

 attvar:
    IS_WORD_ALIGNED(l);
    IS_WORD_ALIGNED(r);
    deRef(l);
    deRef(r);

    wl = *l;
    wr = *r;

    if ( tag(wl) != tag(wr) )
      return false;

   if ( needsRef(wl) )		/* var or attvar */
   { size_t i, j;
     int m, n;
     node *vl, *vr;
     Word al, ar;

     if ( tag(wl) == TAG_ATTVAR )
     { al = valPAttVar(wl);
       ar = valPAttVar(wr);
     } else
     { al = ar = NULL;
     }

     if ((i = var_id(l, buf)) < 0)
       return MEMORY_OVERFLOW;
     if ((j = var_id(r, buf)) < 0)
       return MEMORY_OVERFLOW;

     vl = Node(i, buf);
     vr = Node(j, buf);

     m = vl->a;
     n = vr->b;

     if ( (m==0) && (n==0) )
     { vl->a = j;
       vr->b = i;
       if ( al )
       { l = al; r = ar;
	 goto attvar;
       }
       continue;
     }
     if ( (m != 0) && (n != 0) )
     { if ( (m == j) && (n == i) )
       { if ( al )
	 { l = al; r = ar;
	   goto attvar;
	 }
	 continue;
       }
     }
     return false;
    }

    if ( wl == wr && !isTerm(wl) )
      continue;

    switch(tag(wl))
    { case TAG_ATOM:
	return false;
      case TAG_INTEGER:
	if ( storage(wl) == STG_INLINE ||
	     storage(wr) == STG_INLINE )
	  return false;
      case TAG_STRING:
      case TAG_FLOAT:
	if ( equalIndirect(wl, wr) )
	  continue;
        return false;
      case TAG_COMPOUND:
      {   size_t i, j;
	  int k, h;
	  node *m;

	  word dm, dn;			/* definition (= functor/arity) */
	  Word lm, ln;			/* arguments list */

	  if ( (i = term_id(l, buf)) < 0 )
	    return MEMORY_OVERFLOW;
	  if ( (j = term_id(r, buf)) < 0 )
	    return MEMORY_OVERFLOW;

	  m = Node(i, buf);
	  k = node_variant(m);

	  if ( 0 != k )
	  { if ( ( h = isomorphic(agenda, k, j, buf) ) <= 0 )
	      return  h;
	    continue;
	  }

	  univ(node_orig(m), &dm, &lm);
	  univ(node_orig(Node(j,buf)), &dn, &ln);

	  if ( dm != dn )
	    return false;

	  node_variant(m) = j;

	  if ( !push_args(agenda, lm, ln, arityFunctor(dm)) )
	    return MEMORY_OVERFLOW;

	  continue;
	}
    }
  }

  return true;
}


/* returns true, false or false+exception */
int
is_variant_ptr(DECL_LD Word p1, Word p2)
{ argPairs agenda;
  tmp_buffer buf;
  Buffer VARIANT_BUFFER = (Buffer)&buf;
  int rval;
  node *r;
  node new = {NULL, 0, 0, 0};   /* dummy node as 0-th element*/

  deRef(p1);
  deRef(p2);

  if ( *p1 == *p2 )                     /* same term */
    return true;
  if ( tag(*p1) != tag(*p2) )           /* different type */
    return false;
again:
  switch(tag(*p1))                      /* quick tests */
  { case TAG_VAR:
      return true;
    case TAG_ATTVAR:
      p1 = valPAttVar(*p1);
      p2 = valPAttVar(*p2);
      goto again;
    case TAG_ATOM:
      return false;
    case TAG_INTEGER:
      if ( !(isIndirect(*p1) && isIndirect(*p2)) )
        return false;
      /*FALLTHROUGH*/
    case TAG_FLOAT:
    case TAG_STRING:
      return equalIndirect(*p1, *p2);
    case TAG_COMPOUND:
    { Functor t1 = valueTerm(*p1);
      Functor t2 = valueTerm(*p2);

      if ( t1->definition != t2->definition )
        return false;
      break;
    }
    default:
      assert(0);
      return false;
  }

  initBuffer(&buf);	/* can be faster! */
  init_agenda(&agenda);

  if ( add_node_buffer(VARIANT_BUFFER, &new) &&
       push_start_args(&agenda, p1, p2, 1) )
    rval = variant(&agenda, VARIANT_BUFFER);
  else
    rval = MEMORY_OVERFLOW;

  for(r = baseBuffer(VARIANT_BUFFER, node) + 1;
      r < topBuffer(VARIANT_BUFFER, node); r++ )
    reset_terms(r);

  discardBuffer(VARIANT_BUFFER);
  clearSegStack(&agenda.stack);

  DEBUG(CHK_SECURE, checkStacks(NULL));

  if ( rval >= 0 )
    return rval;

  return PL_error(NULL, 0, NULL, ERR_NOMEM);
}

static
PRED_IMPL("=@=", 2, variant, 0)
{ PRED_LD

  return is_variant_ptr(valTermRef(A1), valTermRef(A2));
}

static
PRED_IMPL("\\=@=", 2, not_variant, 0)
{ PRED_LD

  return !is_variant_ptr(valTermRef(A1), valTermRef(A2));
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(variant)
  PRED_DEF("=@=", 2, variant, 0)
  PRED_DEF("\\=@=", 2, not_variant, 0)
EndPredDefs
