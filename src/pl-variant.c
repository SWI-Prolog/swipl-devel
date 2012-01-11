/*  $Id$

    Part of SWI-Prolog

    Author:        Kuniaki Mukai Wielemaker
    E-mail:        mukai@sfc.keio.ac.jp
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2011, Kuniaki Mukai

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

/*#define O_DEBUG 1*/
#include "pl-incl.h"


		/********************************
		*	     VARIANT	        *
		*********************************/

#define consVar(w) (((intptr_t)(w)<<LMASK_BITS) | TAG_VAR | FIRST_MASK)
#define valVar(w)  ((intptr_t)(w) >> LMASK_BITS)

#define TAG_COMPOUND_x    (STG_STATIC|TAG_COMPOUND)
#define isCompound_x(w) \
  (( (intptr_t)(w) & (STG_MASK|TAG_MASK) ) == TAG_COMPOUND_x )
#define valCompound_x(w)  ((intptr_t)(w) >> LMASK_BITS)
#define consCompound_x(i)  (((i)<<LMASK_BITS) | TAG_COMPOUND_x)

#define node_bp(p)	((p)->bp)
#define node_orig(p)	((p)->orig)
#define node_variant(p) ((p)->a)
#define node_isom(p)	((p)->b)

typedef struct aWork
{ word *	left;			/* left term arguments */
  word *	right;			/* right term arguments */
  int		arg;
  int		arity;
} aWork;


typedef struct argPairs
{ aWork	work;				/* current work */
  segstack	stack;
  char		first_chunk[sizeof(aWork)*25];
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
push_args(argPairs *a, Word left, Word right, int arity)  /* plural */
{ if ( !pushSegStack(&a->stack, a->work, aWork) )
    return FALSE;

  a->work.left  = left;
  a->work.right = right;
  a->work.arg   = 0;
  a->work.arity = arity;

  return TRUE;
}

static inline bool
next_arg(argPairs *a, Word *lp, Word *rp)   /* singular (not plural !) */
{ if ( emptySegStack(&a->stack) )
    return FALSE;

  *lp = a->work.left;
  *rp = a->work.right;

  a->work.arg++ ;
  a->work.left++ ;
  a->work.right++ ;

  if ( a->work.arg >= a->work.arity )
  { if ( !popSegStack(&a->stack, &a->work, aWork) )
      return FALSE;
  }

  return TRUE;
}

static inline node *
Node(int i, Buffer buf)
{ return  &fetchBuffer(buf, i, node);
}

static inline bool
add_node_buffer(Buffer b, node *obj)
{ if ( ((b->top) + sizeof(node)) > (b->max) )
  { if ( !growBuffer(b, sizeof(node)) )
      return FALSE;
  }

  *((node *)(b)->top) = *obj;
  b->top += sizeof(node);

  return TRUE;
}

static inline int
var_id(Word p, Buffer buf)
{ word w = *p;

  if ( w )
  { return (int)valVar(w);		/* node id truncated to int: */
  } else				/* < 2^31 nodes */
  { int n = (int)entriesBuffer(buf, node);
    node new = {p, w, 0, 0};

    if ( !add_node_buffer((Buffer)buf, &new) )
      return MEMORY_OVERFLOW;

    *p = (word)consVar(n);
    return n;
  }
}

static inline int
term_id(Word p, Buffer buf)
{ word w = *p;

  if ( isCompound_x(w) )
  { return (int)valCompound_x(w);
  } else
  { int n = (int)entriesBuffer(buf, node);
    node new = {p, w, 0, 0};

    if ( !add_node_buffer((Buffer)buf, &new) )
      return MEMORY_OVERFLOW;

    *p = (word)consCompound_x(n);
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
    DEBUG(CHK_SECURE, assert(n > 0));
    i = node_isom(n);
  } while ( i != 0 );

  *r = n;

  return k;
}

static inline void
univ(word t, Word d, Word *a ARG_LD)
{ Functor f;

  f = valueTerm(t);
  while ( isRef(f->definition) )
    f = (Functor)unRef(f->definition);
  *d = f->definition;
  *a = f->arguments;
}

static inline void
reset_terms(node * r)
{ *(r->bp)  =  r->orig;
}

/* isomorphic (==) */

static int
isomorphic(argPairs *a, int i, int j, Buffer buf ARG_LD)
{ Word l = NULL, r = NULL, lm, ln;
  word dm, dn;
  Word dummy = NULL;

  if ( i == j )
    return TRUE;

  if ( !push_args(a, dummy, dummy, 1) )
    return MEMORY_OVERFLOW;

  univ(node_orig(Node(i, buf)), &dm, &lm PASS_LD);
  univ(node_orig(Node(j, buf)), &dn, &ln PASS_LD);

  if ( dm != dn )
    return FALSE;

  if ( !push_args(a,  lm, ln, arityFunctor(dm)) )
    return MEMORY_OVERFLOW;

  while( next_arg(a, &l, &r) )
  { word wl, wr;
    if ( l == NULL )
      return TRUE;

  attvar:
    deRef(l);
    deRef(r);

    wl = *l;
    wr = *r;

    if ( tag(wl) != tag(wr) )
      return FALSE;

    if ( tag(wl) == TAG_VAR )
    { if ( l != r )		/* identity test on variables */
	return FALSE;
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
	return FALSE;
      case TAG_INTEGER:
	if ( storage(wl) == STG_INLINE ||
	     storage(wr) == STG_INLINE )
	  return FALSE;
      case TAG_STRING:
      case TAG_FLOAT:
	if ( equalIndirect(wl, wr) )
	  continue;
        return FALSE;
      case TAG_COMPOUND:
      { Word lm, ln;
	word dm, dn;
	int i, j;
	node  *m,  *n;

	if ( (i = term_id(l, buf)) < 0 )
	  return MEMORY_OVERFLOW;
	i = Root(i, &m, buf);

	if ( (j = term_id(r, buf)) < 0 )
	  return MEMORY_OVERFLOW;
	j = Root(j, &n, buf);

	if ( i==j )
	  continue;

	univ(node_orig(m), &dm, &lm PASS_LD);
	univ(node_orig(n), &dn, &ln PASS_LD);

	if ( dm != dn )
	  return FALSE;

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

  return TRUE;
}

/* t =@= u */
static int
variant(argPairs *agenda, Buffer buf ARG_LD)
{ Word l = NULL, r =NULL;

  while( next_arg(agenda, &l, &r) )
  { word wl, wr;

 attvar:
   deRef(l);
   deRef(r);

   wl = *l;
   wr = *r;

   if ( tag(wl) != tag(wr) )
     return FALSE;

   if ( tag(wl) == TAG_VAR )
   { int i, j, m, n;
     node *vl, *vr;

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
       continue;
     }
     if ( (m != 0) && (n != 0) )
     { if ( (m == j) && (n == i) )
	 continue;
     }
     return FALSE;
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
	return FALSE;
      case TAG_INTEGER:
	if ( storage(wl) == STG_INLINE ||
	     storage(wr) == STG_INLINE )
	  return FALSE;
      case TAG_STRING:
      case TAG_FLOAT:
	if ( equalIndirect(wl, wr) )
	  continue;
        return FALSE;
      case TAG_COMPOUND:
	{ int i, j, k, h;
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
	  { if ( ( h = isomorphic(agenda, k, j, buf PASS_LD) ) <= 0 )
	      return  h;
	    continue;
	  }

	  univ(node_orig(m), &dm, &lm PASS_LD);
	  univ(node_orig(Node(j,buf)), &dn, &ln PASS_LD);

	  if ( dm != dn )
	    return FALSE;

	  node_variant(m) = j;

	  if ( !push_args(agenda, lm, ln, arityFunctor(dm)) )
	    return MEMORY_OVERFLOW;

	  continue;
	}
    }
  }

  return TRUE;
}


static
PRED_IMPL("=@=", 2, variant, 0)
{ PRED_LD
  argPairs agenda;
  tmp_buffer buf;
  Buffer VARIANT_BUFFER = (Buffer)&buf;
  bool rval;
  node *r;
  Word p1 = valTermRef(A1);
  Word p2 = valTermRef(A2);
  node new = {NULL, 0, 0, 0};   /* dummy node as 0-th element*/

  deRef(p1);
  deRef(p2);

  if ( *p1 == *p2 )                     /* same term */
    return TRUE;
  if ( tag(*p1) != tag(*p2) )           /* different type */
    return FALSE;
again:
  switch(tag(*p1))                      /* quick tests */
  { case TAG_VAR:
      return TRUE;
    case TAG_ATTVAR:
      p1 = valPAttVar(*p1);
      p2 = valPAttVar(*p2);
      goto again;
    case TAG_ATOM:
      return FALSE;
    case TAG_INTEGER:
      if ( !(isIndirect(*p1) && isIndirect(*p2)) )
        return FALSE;
      /*FALLTHROUGH*/
    case TAG_FLOAT:
    case TAG_STRING:
      return equalIndirect(*p1, *p2);
    case TAG_COMPOUND:
    { Functor t1 = valueTerm(*p1);
      Functor t2 = valueTerm(*p2);

      if ( t1->definition != t2->definition )
        return FALSE;
      break;
    }
    default:
      assert(0);
      return FALSE;
  }

  startCritical;
  initBuffer(&buf);	/* can be faster! */
  init_agenda(&agenda);

  if ( (add_node_buffer(VARIANT_BUFFER, &new) >= 0) &&
       (push_args(&agenda, p1, p2, 1) >=0) )
    rval = variant(&agenda, VARIANT_BUFFER PASS_LD);
  else
    rval = MEMORY_OVERFLOW;

  for(r = baseBuffer(VARIANT_BUFFER, node) + 1;
      r < topBuffer(VARIANT_BUFFER, node); r++ )
    reset_terms(r);

  discardBuffer(VARIANT_BUFFER);
  clearSegStack(&agenda.stack);

  DEBUG(CHK_SECURE, checkStacks(NULL));

  if ( !endCritical )
    return FALSE;

  if ( rval >= 0 )
    return rval;

  return PL_error(NULL, 0, NULL, ERR_NOMEM);
}


static
PRED_IMPL("\\=@=", 2, not_variant, 0)
{ return pl_variant2_va(PL__t0, PL__ac, PL__ctx) ? FALSE : TRUE;
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(variant)
  PRED_DEF("=@=", 2, variant, 0)
  PRED_DEF("\\=@=", 2, not_variant, 0)
EndPredDefs
