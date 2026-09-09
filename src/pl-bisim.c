/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Kuniaki Mukai
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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
#include "pl-incl.h"
#include "pl-fli.h"
#include "pl-prims.h"
#include "pl-gc.h"
#include "pl-bisim.h"
#include "pl-rsort.h"
#include <limits.h>
#include "pl-termwalk.c"		/* plain term_agenda */

#undef LD
#define LD LOCAL_LD

/* Minimising a term graph under bisimulation.

   A term in the store is a graph, and a cyclic term is a finite graph
   denoting an infinite tree.  Two terms denote the same tree exactly when
   their graphs are bisimilar, which is the equivalence ==/2 already decides
   (see do_compare() in pl-prims.c).  What is missing is a canonical
   *representative*: the smallest graph denoting the same tree.  That is what
   this file computes.

   The graph is built by walking the term with a term_agenda and giving each
   compound cell a number, which is stored in the cell's own functor word as
   a tagged integer.  Meeting a cell whose functor word is already an integer
   is how sharing and cycles are detected, and it means there is no node
   table to consult: the number *is* the functor word.  The original functor
   words are kept in a tmp_buffer and put back by restore_graph().

   While the numbers are in place the term is malformed, so nothing may be
   allocated on the Prolog stacks and no GC may run until restore_graph()
   has completed.  This is the same discipline as scan_shared() in
   pl-prims.c and termHashValue() in pl-termhash.c.

   This code is based on a prototype by Kuniaki Mukai using the public C
   API.
*/


typedef struct
{ Functor	term;			/* the cell */
  word		functor;		/* its functor word before numbering */
} bs_node;

/* The graph, once the functor words have been put back.  From here on it is
   all integers: node ids, class ids, and the edges between them.

   An edge holds the id of the cell it points at, or -(leaf+1) when the
   argument is not a compound.  Leaves are numbered so that two arguments get
   the same number exactly when they are ==, which is what lets two parents
   come out equivalent because their arguments are.
*/

typedef struct
{ size_t	nodes;			/* number of cells */
  size_t	edges;			/* sum of the arities */
  size_t	leaves;			/* distinct non-compound arguments */
  bs_node      *node;			/* [nodes], owned by the caller */
  size_t       *arg;			/* [nodes+1] index into child */
  int	       *child;			/* [edges], see setup_graph() */
  size_t       *pred;			/* [edges] reverse edges */
  size_t       *pred_pos;		/* [edges] which argument each came from */
  size_t       *pred_at;		/* [nodes+1] index into pred */
  size_t	arity;			/* largest arity in the graph */
  size_t       *class;			/* [nodes] class of each node */
  size_t       *order;			/* [nodes] node ids grouped by class */
  size_t       *at;			/* [nodes] index of a node in order */
  size_t       *class_start;		/* [nodes] */
  size_t       *class_len;		/* [nodes] */
  size_t	classes;
  size_t	vars;			/* distinct variables among the leaves */
  bool		attvars;		/* one of them is attributed */
  size_t       *canon;			/* [classes] canonical class number */
  size_t       *vnum;			/* [vars] canonical variable number */
  size_t       *uses;			/* [classes] how often a class is used */
  size_t	named;			/* classes that need a name */
					/* scratch for the refinement */
  size_t       *head;			/* [nodes+leaves] bucket chains */
  size_t       *next;			/* [nodes] */
  size_t       *touch;			/* [nodes] buckets used */
  size_t       *scratch;		/* [nodes] */
  size_t       *grp;			/* [nodes+1] group boundaries */
  size_t       *grp2;			/* [nodes+1] */
  size_t       *marked;			/* [nodes] marked members per class */
  size_t       *pos_head;		/* [arity] in-edges by argument */
  size_t       *pos_next;		/* [edges] */
  size_t       *queue;			/* [nodes] classes to re-examine */
  char	       *queued;			/* [nodes] */
  size_t	qhead;
  size_t	qtail;
  void	       *pool;			/* the arrays, in one block */
  void	       *pool2;			/* those sized after refining */
} bs_graph;

#define NO_NODE ((size_t)-1)


		 /*******************************
		 *	   BUILD THE GRAPH	*
		 *******************************/

/* build_graph() numbers every physically distinct compound cell reachable
   from p.  Returns true, or MEMORY_OVERFLOW if the buffer cannot grow.  The
   caller must call restore_graph() either way.
*/

#define build_graph(p, b, opaque) LDFUNC(build_graph, p, b, opaque)

static boolex_t
build_graph(DECL_LD Word p, Buffer b, word opaque)
{ term_agenda agenda;
  boolex_t rc = true;

  deRef(p);
  if ( !isTerm(*p) )
    return true;			/* no compound cells at all */

  initTermAgenda(&agenda, 1, p);
  while( (p=nextTermAgenda(&agenda)) )
  { Functor f;
    bs_node *n;

    if ( !isTerm(*p) )
      continue;				/* a leaf: no cell to number */

    f = valueTerm(*p);
    if ( isTaggedInt(f->definition) )
      continue;				/* seen: shared cell or a cycle */
    if ( f->definition == opaque )
      continue;				/* not to be taken apart */

    if ( !(n=allocFromBuffer(b, sizeof(*n))) )
    { rc = MEMORY_OVERFLOW;
      break;
    }
    n->term    = f;
    n->functor = f->definition;
    f->definition = consInt(entriesBuffer(b, bs_node)-1);

    if ( !pushWorkAgenda(&agenda, arityFunctor(n->functor), f->arguments) )
    { rc = MEMORY_OVERFLOW;
      break;
    }
  }
  clearTermAgenda(&agenda);

  return rc;
}


/* restore_graph() puts the functor words back.  It must run before anything
   can allocate or garbage collect, and it must run on every exit path.
*/

static void
restore_graph(Buffer b)
{ bs_node *n   = baseBuffer(b, bs_node);
  bs_node *end = n + entriesBuffer(b, bs_node);

  for(; n<end; n++)
    n->term->definition = n->functor;
}


		 /*******************************
		 *	     LEAF NUMBERING	*
		 *******************************/

/* A leaf is identified by ==.  For a variable that means its address, as
   every unbound cell holds the same word; for anything else it means its
   value, which needs compareStandard() because two equal strings or bignums
   are separate cells.  Ordering variables ahead of the rest completes the
   order, which is all that is needed to number them.
*/

/* leaf_order carries LD into the comparison and takes note if
   compareStandard() gives up: sort_r() has nowhere to report that, and
   treating the failure as "equal" would quietly merge two leaves that are
   not the same.
*/

typedef struct
{ PL_local_data_t *ld;
  bool		   failed;		/* compareStandard() raised */
} leaf_order;

#define cmp_leaf(ctx, a, b) LDFUNC(cmp_leaf, ctx, a, b)

static cmp_t
cmp_leaf(DECL_LD leaf_order *ctx, Word a, Word b)
{ bool va = isVar(*a) || isAttVar(*a);
  bool vb = isVar(*b) || isAttVar(*b);

  if ( va != vb )
    return va ? CMP_LESS : CMP_GREATER;
  if ( va )				/* a variable is its own address */
    return SCALAR_TO_CMP(a, b);
  if ( *a == *b )			/* same word: certainly == */
    return CMP_EQUAL;

  switch( compareStandard(a, b, false) )
  { case CMPEX_LESS:	 return CMP_LESS;
    case CMPEX_GREATER:	 return CMP_GREATER;
    case CMPEX_EQUAL:	 return CMP_EQUAL;
    default:				/* CMP_ERROR: exception pending */
      ctx->failed = true;
      return SCALAR_TO_CMP(a, b);	/* keep sort_r() well behaved */
  }
}

static int	/* cmp_t, but that does not satisfy sort_r() */
cmp_leaf_r(const void *p1, const void *p2, void *arg)
{ leaf_order *ctx = arg;

  return cmp_leaf(PASS_AS_LD(ctx->ld) ctx, *(Word *)p1, *(Word *)p2);
}


/* number_leaves() turns the placeholders left in g->child into leaf numbers.
   leaf[i] is the argument that placeholder -(i+1) stands for.
*/

#define number_leaves(g, leaf, nleaf) LDFUNC(number_leaves, g, leaf, nleaf)

static boolex_t
number_leaves(DECL_LD bs_graph *g, Word *leaf, size_t nleaf)
{ leaf_order ctx = { .ld = LD, .failed = false };
  Word *sorted;
  size_t i, n;

  g->leaves = 0;
  if ( nleaf == 0 )
    return true;

  if ( !(sorted=tmp_malloc(nleaf*sizeof(*sorted))) )
    return MEMORY_OVERFLOW;
  memcpy(sorted, leaf, nleaf*sizeof(*sorted));
  sort_r(sorted, nleaf, sizeof(*sorted), cmp_leaf_r, &ctx);

  for(i=0, n=0; i<nleaf; i++)		/* squeeze out the duplicates */
  { if ( i == 0 || cmp_leaf(&ctx, sorted[i-1], sorted[i]) != CMP_EQUAL )
      sorted[n++] = sorted[i];
  }
  g->leaves = n;
  for(i=0; i<n; i++)			/* cmp_leaf() sorts variables first */
  { if ( !(isVar(*sorted[i]) || isAttVar(*sorted[i])) )
      break;
  }
  g->vars = i;

  for(i=0; i<g->edges; i++)		/* rewrite the placeholders */
  { if ( g->child[i] < 0 )
    { Word w = leaf[-g->child[i]-1];
      size_t lo = 0, hi = n;

      while(lo < hi)			/* binary search */
      { size_t mid = (lo+hi)/2;

	if ( cmp_leaf(&ctx, sorted[mid], w) == CMP_LESS )
	  lo = mid+1;
	else
	  hi = mid;
      }
      g->child[i] = -(int)(lo+1);
    }
  }

  tmp_free(sorted);

  return ctx.failed ? false : true;	/* false: exception already raised */
}


		 /*******************************
		 *	     THE QUOTIENT	*
		 *******************************/

/* The class an edge points at.  Node classes change as the partition is
   refined; leaves never do, so they are numbered above every node class and
   stay put.
*/

#define child_class(g, e) \
	((g)->child[e] >= 0 ? (g)->class[(g)->child[e]] \
			    : (g)->nodes + (size_t)(-(g)->child[e]-1))

typedef struct
{ word		functor;
  size_t	node;
} bs_colour;

static int	/* cmp_t, but that does not satisfy qsort() */
cmp_colour(const void *p1, const void *p2)
{ const bs_colour *a = p1;
  const bs_colour *b = p2;

  return SCALAR_TO_CMP(a->functor, b->functor);
}


/* colour_nodes() makes the initial partition.  Cells that share a functor
   may still turn out equivalent; cells that do not never will, so this is
   the coarsest partition to start refining from.
*/

static bool
colour_nodes(bs_graph *g)
{ bs_colour *c;
  size_t i, class = 0;

  if ( !(c=tmp_malloc(g->nodes*sizeof(*c))) )
    return false;

  for(i=0; i<g->nodes; i++)
  { c[i].functor = g->node[i].functor;
    c[i].node    = i;
  }
  qsort(c, g->nodes, sizeof(*c), cmp_colour);

  g->class_start[0] = 0;
  for(i=0; i<g->nodes; i++)
  { if ( i > 0 && c[i-1].functor != c[i].functor )
    { g->class_len[class] = i - g->class_start[class];
      class++;
      g->class_start[class] = i;
    }
    g->order[i]         = c[i].node;
    g->at[c[i].node]    = i;
    g->class[c[i].node] = class;
  }
  g->class_len[class] = g->nodes - g->class_start[class];
  g->classes          = class+1;

  tmp_free(c);

  return true;
}


/* group_signature() reorders order[start..start+len) so that members with
   equal signatures become contiguous, and leaves the group boundaries in
   g->grp[0..n].  It groups on one argument at a time; grouping needs no
   order on the keys, so this costs the members times the arity rather than
   sorting them.

   Only used to finish the initial partition, where the key is the leaf an
   argument holds, or one shared value for every argument that is a compound.
   Leaves never change class, so every distinction they make can be made once
   and for all, and refine() below then only has to deal with the arguments
   that point at other cells.
*/

#define LEAF_SAME ((size_t)0)		/* every compound argument alike */

static size_t
group_signature(bs_graph *g, size_t start, size_t len, size_t arity)
{ size_t ngrp = 1;
  size_t j;

  g->grp[0] = start;
  g->grp[1] = start+len;

  for(j=0; j<arity && ngrp < len; j++)
  { size_t out = 0;
    size_t b;

    g->grp2[0] = start;
    for(b=0; b<ngrp; b++)
    { size_t s = g->grp[b];
      size_t e = g->grp[b+1];
      size_t i, ntouch = 0, p = s;

      if ( e-s <= 1 )
      { g->grp2[++out] = e;
	continue;
      }

      for(i=s; i<e; i++)		/* chain the members per key */
      { size_t n = g->order[i];
	int c = g->child[g->arg[n]+j];
	size_t k = c >= 0 ? LEAF_SAME : (size_t)(-c);

	if ( g->head[k] == NO_NODE )
	  g->touch[ntouch++] = k;
	g->next[n] = g->head[k];
	g->head[k] = n;
      }

      for(i=0; i<ntouch; i++)		/* write the chains back out */
      { size_t k = g->touch[i];
	size_t n;

	for(n=g->head[k]; n != NO_NODE; n=g->next[n])
	  g->scratch[p++] = n;
	g->head[k] = NO_NODE;
	g->grp2[++out] = p;
      }
      memcpy(&g->order[s], &g->scratch[s], (e-s)*sizeof(size_t));
    }

    ngrp = out;
    memcpy(g->grp, g->grp2, (ngrp+1)*sizeof(size_t));
  }

  return ngrp;
}


/* new_class() turns order[start..start+len) into a class of its own. */

static size_t
new_class(bs_graph *g, size_t start, size_t len)
{ size_t c = g->classes++;
  size_t i;

  g->class_start[c] = start;
  g->class_len[c]   = len;
  g->queued[c]      = false;
  for(i=start; i<start+len; i++)
  { g->class[g->order[i]] = c;
    g->at[g->order[i]]    = i;
  }

  return c;
}


/* split_colours() finishes the initial partition by separating cells that
   share a functor but hold different leaves.
*/

static void
split_colours(bs_graph *g)
{ size_t c;

  for(c=0; c<g->classes; c++)		/* g->classes grows as we go */
  { size_t start = g->class_start[c];
    size_t len   = g->class_len[c];
    size_t arity, ngrp, b;

    if ( len <= 1 )
      continue;
    arity = arityFunctor(g->node[g->order[start]].functor);
    if ( arity == 0 )
      continue;

    ngrp = group_signature(g, start, len, arity);
    if ( ngrp <= 1 )
    { size_t i;					/* order may have moved */

      for(i=start; i<start+len; i++)
	g->at[g->order[i]] = i;
      continue;
    }

    g->class_len[c] = g->grp[1] - g->grp[0];
    { size_t i;

      for(i=g->grp[0]; i<g->grp[1]; i++)
	g->at[g->order[i]] = i;
    }
    for(b=1; b<ngrp; b++)
      new_class(g, g->grp[b], g->grp[b+1]-g->grp[b]);
  }
}


		 /*******************************
		 *	    REFINEMENT		*
		 *******************************/

static void
enqueue(bs_graph *g, size_t class)
{ if ( !g->queued[class] )
  { g->queued[class] = true;
    g->queue[g->qtail++] = class;
    if ( g->qtail == g->nodes+1 )
      g->qtail = 0;
  }
}


/* mark_member() moves n to the front of its class, in among the members
   marked so far, so that a class can be split in time proportional to the
   number of marked members rather than to the size of the class.  That is
   what keeps a chain of N cells linear: peeling one cell off a class of N
   must not cost N.
*/

static void
mark_member(bs_graph *g, size_t n, size_t *touch, size_t *ntouch)
{ size_t c = g->class[n];
  size_t to, other;

  if ( g->marked[c] == 0 )
    touch[(*ntouch)++] = c;

  to = g->class_start[c] + g->marked[c];
  if ( g->at[n] != to )
  { other = g->order[to];
    g->order[to]        = n;
    g->order[g->at[n]]  = other;
    g->at[other]        = g->at[n];
    g->at[n]            = to;
  }
  g->marked[c]++;
}


/* refine() splits until nothing splits any more.  A class B can only become
   splittable once some class S it points into has split, and then only into
   the members whose argument lands in S and the rest.  So the work for a
   splitter is proportional to the edges entering it, not to the sizes of the
   classes it splits.

   Enqueueing the smaller of the two halves is what bounds the total work:
   splitting by both halves is the same as splitting by one of them plus the
   split that produced them, so the larger half can be skipped unless the
   class it came from was still waiting to be used as a splitter.
*/

static void
refine(bs_graph *g)
{ size_t i;

  g->qhead = g->qtail = 0;
  for(i=0; i<g->classes; i++)
    g->queued[i] = false;
  for(i=0; i<g->classes; i++)
    enqueue(g, i);
  for(i=0; i<g->nodes; i++)
    g->marked[i] = 0;
  for(i=0; i<g->arity; i++)
    g->pos_head[i] = NO_NODE;

  while( g->qhead != g->qtail )
  { size_t sp = g->queue[g->qhead++];
    size_t s, npos = 0, j;

    if ( g->qhead == g->nodes+1 )
      g->qhead = 0;
    g->queued[sp] = false;

    /* Sort the edges entering the splitter by the argument they come from;
       each argument is a separate split.
    */
    for(s=g->class_start[sp]; s<g->class_start[sp]+g->class_len[sp]; s++)
    { size_t m = g->order[s];
      size_t e;

      for(e=g->pred_at[m]; e<g->pred_at[m+1]; e++)
      { size_t pos = g->pred_pos[e];

	if ( g->pos_head[pos] == NO_NODE )
	  g->touch[npos++] = pos;
	g->pos_next[e]   = g->pos_head[pos];
	g->pos_head[pos] = e;
      }
    }

    for(j=0; j<npos; j++)
    { size_t pos = g->touch[j];
      size_t ntouch = 0;
      size_t e, k;

      for(e=g->pos_head[pos]; e != NO_NODE; e=g->pos_next[e])
	mark_member(g, g->pred[e], g->scratch, &ntouch);
      g->pos_head[pos] = NO_NODE;

      for(k=0; k<ntouch; k++)
      { size_t c = g->scratch[k];
	size_t nmarked = g->marked[c];
	size_t len = g->class_len[c];

	g->marked[c] = 0;
	if ( nmarked == len )		/* the whole class: no split */
	  continue;

	{ size_t start = g->class_start[c];
	  size_t nc;

	  g->class_start[c] = start + nmarked;
	  g->class_len[c]   = len - nmarked;
	  nc = new_class(g, start, nmarked);

	  if ( g->queued[c] )		/* still to be used as a splitter */
	    enqueue(g, nc);
	  else if ( nmarked <= len-nmarked )
	    enqueue(g, nc);
	  else
	    enqueue(g, c);
	}
      }
    }
  }
}

/* identity_partition() leaves every cell in a class of its own, which is
   what term_factorized/4 asks for with minimal(false): share what is
   physically shared and nothing else.
*/

static void
identity_partition(bs_graph *g)
{ size_t i;

  for(i=0; i<g->nodes; i++)
  { g->class[i]       = i;
    g->order[i]       = i;
    g->at[i]          = i;
    g->class_start[i] = i;
    g->class_len[i]   = 1;
  }
  g->classes = g->nodes;
}


		 /*******************************
		 *	    THE ARRAY POOL	*
		 *******************************/

/* The graph is a couple of dozen arrays, all sized from the number of nodes
   and edges.  Allocating them one by one leaves many blocks that are each
   too small for tmp_malloc() to mmap() and that the allocator therefore
   never hands back to the OS: at some 210 bytes per node the whole set
   passes the 32Kb threshold at around 160 nodes, while the largest single
   array does not until around 2,000.  So sum the sizes, do one
   tmp_malloc(), and cut it into the arrays.

   With no block yet, pool_alloc() only counts.  That lets the layout be
   written once and run twice: see setup_graph().
*/

typedef struct
{ char	       *base;			/* the block; NULL while counting */
  size_t	used;			/* bytes handed out so far */
} bs_pool;

static void *
pool_alloc(bs_pool *p, size_t n, size_t esize)
{ size_t at = p->used;

  p->used += ((n*esize)+sizeof(double)-1)&~(sizeof(double)-1);

  return p->base ? p->base+at : NULL;
}

/* Allocate the block the counting pass asked for and start handing out.
   tmp_malloc() returns memory aligned for a double, and pool_alloc() keeps
   every piece a multiple of that, so each array is aligned for its type.
*/

static bool
pool_create(bs_pool *p)
{ if ( (p->base=tmp_malloc(p->used)) )
  { p->used = 0;
    return true;
  }

  return false;
}

#define pool_array(p, a, n) ((a) = pool_alloc(p, n, sizeof(*(a))))


		 /*******************************
		 *	      SET UP		*
		 *******************************/

static void
free_graph(bs_graph *g)
{ tmp_free(g->pool);
  tmp_free(g->pool2);
  tmp_free(g->uses);
}


/* fill_edges() must run while the functor words still hold the node
   numbers: that is how an argument that is a compound becomes the id of the
   cell it points at.  A leaf gets a placeholder that number_leaves()
   resolves once the walk is over.
*/

#define fill_edges(g, leaf, nleafp) LDFUNC(fill_edges, g, leaf, nleafp)

static void
fill_edges(DECL_LD bs_graph *g, Word *leaf, size_t *nleafp)
{ size_t i, e = 0, nleaf = 0;

  for(i=0; i<g->nodes; i++)
  { Functor f = g->node[i].term;
    size_t arity = arityFunctor(g->node[i].functor);
    size_t j;

    g->arg[i] = e;
    for(j=0; j<arity; j++)
    { Word a = &f->arguments[j];

      deRef(a);
      /* A compound that was not numbered is one build_graph() was told to
	 leave alone, e.g. '$VAR'/1.  It counts as a leaf.
      */
      if ( isTerm(*a) && isTaggedInt(valueTerm(*a)->definition) )
      { g->child[e++] = (int)valInt(valueTerm(*a)->definition);
      } else
      { if ( isAttVar(*a) )
	  g->attvars = true;
	leaf[nleaf] = a;
	g->child[e++] = -(int)(nleaf+1);
	nleaf++;
      }
    }
  }
  g->arg[g->nodes] = e;
  *nleafp = nleaf;
}


static void
fill_pred(bs_graph *g)
{ size_t i, e;

  for(i=0; i<=g->nodes; i++)
    g->pred_at[i] = 0;
  for(e=0; e<g->edges; e++)
  { if ( g->child[e] >= 0 )
      g->pred_at[g->child[e]+1]++;
  }
  for(i=0; i<g->nodes; i++)
    g->pred_at[i+1] += g->pred_at[i];

  memcpy(g->scratch, g->pred_at, g->nodes*sizeof(size_t));
  for(i=0; i<g->nodes; i++)
  { size_t j;

    for(j=g->arg[i]; j<g->arg[i+1]; j++)
    { if ( g->child[j] >= 0 )
      { size_t at = g->scratch[g->child[j]]++;

	g->pred[at]     = i;
	g->pred_pos[at] = j - g->arg[i];
      }
    }
  }
}


/* setup_graph() sizes and allocates everything and fills the edges.  It runs
   with the functor words still replaced, so it may not touch the Prolog
   stacks and cannot raise: allocation is all it does.  It answers true,
   MEMORY_OVERFLOW or GRAPH_OVERFLOW, and term_quotient() turns the last two
   into an exception once the term is whole again.
*/

#define setup_graph(g, b, leafp, nleafp) LDFUNC(setup_graph, g, b, leafp, nleafp)

static boolex_t
setup_graph(DECL_LD bs_graph *g, Buffer b, Word **leafp, size_t *nleafp)
{ bs_pool pool = {0};
  size_t i, buckets;

  memset(g, 0, sizeof(*g));
  g->node  = baseBuffer(b, bs_node);
  g->nodes = entriesBuffer(b, bs_node);
  if ( g->nodes == 0 )
    return true;

  for(i=0; i<g->nodes; i++)
  { size_t arity = arityFunctor(g->node[i].functor);

    g->edges += arity;
    if ( arity > g->arity )
      g->arity = arity;
  }

  /* An edge is held as an int: the id of the cell it points at, or minus the
     number of the leaf, which is why it is signed.  Refuse a graph that does
     not fit rather than truncate one silently.  This is not a shortage of
     memory but a term this implementation cannot hold as a graph, so it
     becomes representation_error(int).  Reaching it needs a term of some
     2**31 cells, so tens of gigabytes of global stack.
  */
  if ( g->nodes > INT_MAX || g->edges > INT_MAX )
    return GRAPH_OVERFLOW;

  buckets = g->nodes + g->edges + 2;

  /* Lay the arrays out in one block: the first pass has no block to hand
     out from and only counts the bytes, pool_create() allocates them, and
     the second pass hands out the pieces.
  */
  for(;;)
  { pool_array(&pool, g->arg,         g->nodes+1);
    pool_array(&pool, g->pred,        g->edges+1);
    pool_array(&pool, g->pred_pos,    g->edges+1);
    pool_array(&pool, g->pred_at,     g->nodes+1);
    pool_array(&pool, g->class,       g->nodes);
    pool_array(&pool, g->order,       g->nodes);
    pool_array(&pool, g->at,          g->nodes);
    pool_array(&pool, g->class_start, g->nodes);
    pool_array(&pool, g->class_len,   g->nodes);
    pool_array(&pool, g->head,        buckets);
    pool_array(&pool, g->next,        g->nodes);
    pool_array(&pool, g->touch,       buckets);
    pool_array(&pool, g->scratch,     g->nodes+1);
    pool_array(&pool, g->grp,         g->nodes+1);
    pool_array(&pool, g->grp2,        g->nodes+1);
    pool_array(&pool, g->marked,      g->nodes);
    pool_array(&pool, g->pos_head,    g->arity+1);
    pool_array(&pool, g->pos_next,    g->edges+1);
    pool_array(&pool, g->queue,       g->nodes+1);
    pool_array(&pool, g->child,       g->edges+1);
    pool_array(&pool, g->queued,      g->nodes);

    if ( pool.base )
      break;
    if ( !pool_create(&pool) )
      return MEMORY_OVERFLOW;
  }
  g->pool = pool.base;

  /* leaf[] dies before the refinement starts, so it is not in the pool.
  */
  if ( !(*leafp=tmp_malloc((g->edges+1)*sizeof(Word))) )
    return MEMORY_OVERFLOW;

  for(i=0; i<buckets; i++)
    g->head[i] = NO_NODE;

  fill_edges(g, *leafp, nleafp);

  return true;
}


/* term_quotient() leaves g holding the coarsest bisimulation of the graph of
   the term at p.  Returns true, false with an exception, or MEMORY_OVERFLOW.
*/

#define term_quotient(p, g, b, opaque, minimal) \
	LDFUNC(term_quotient, p, g, b, opaque, minimal)

static boolex_t
term_quotient(DECL_LD Word p, bs_graph *g, Buffer b, word opaque, bool minimal)
{ Word *leaf = NULL;
  size_t nleaf = 0;
  boolex_t rc;

  memset(g, 0, sizeof(*g));

  rc = build_graph(p, b, opaque);
  if ( rc == true )
    rc = setup_graph(g, b, &leaf, &nleaf);
  restore_graph(b);			/* the term is usable again */

  /* Now the stacks may be touched, so a graph that does not fit in an int
     can be reported as what it is.  PL_error() answers false, which is this
     file's "an exception has been raised".
  */
  if ( rc == GRAPH_OVERFLOW )
    rc = PL_error(NULL, 0, NULL, ERR_REPRESENTATION, ATOM_int);

  if ( rc == true && g->nodes > 0 )
  { /* number_leaves() answers boolex_t: false means compareStandard()
       raised, which must not be turned into a resource error here.
    */
    rc = number_leaves(g, leaf, nleaf);
    tmp_free(leaf);			/* nothing uses it after this, and the */
    leaf = NULL;			/* refinement is where memory peaks */

    if ( rc == true )
    { if ( !minimal )
      { identity_partition(g);
      } else if ( colour_nodes(g) )
      { fill_pred(g);
	split_colours(g);
	refine(g);
      } else
	rc = MEMORY_OVERFLOW;
    }
  }

  tmp_free(leaf);			/* if we never got that far */

  return rc;
}


		 /*******************************
		 *	  CANONICAL NUMBERING	*
		 *******************************/

/* The quotient is unique but the numbers on it are not: they fall out of the
   order the refinement happened to split things in, which depends on the
   input's physical sharing and on the order the atoms were created.  Two
   terms that are =@= must come out with the same numbers, or the answer is
   no use as a key.

   So renumber: walk the quotient depth first from the root, arguments left
   to right, and number each class the first time it is reached.  Variables
   are numbered the first time the class holding them is reached, left to
   right.  Both depend on nothing but the shape of the quotient.

   Which member of a class is used to read the arguments does not matter.
   Members of a class have equivalent successors by construction, and they
   hold the very same leaves, because the leaves are settled before the
   refinement starts.
*/

static bool
canonicalise(bs_graph *g)
{ bs_pool pool = {0};
  size_t *stack;
  size_t top = 0, next = 1, vnext = 1;
  size_t i;

  for(;;)				/* count, then hand out */
  { pool_array(&pool, g->canon, g->classes);
    pool_array(&pool, g->vnum,  g->vars+1);

    if ( pool.base )
      break;
    if ( !pool_create(&pool) )
      return false;
  }
  g->pool2 = pool.base;

  if ( !(stack=tmp_malloc((g->edges+2)*sizeof(*stack))) )
    return false;

  for(i=0; i<g->classes; i++)
    g->canon[i] = 0;
  for(i=0; i<=g->vars; i++)
    g->vnum[i] = 0;

  stack[top++] = g->class[0];		/* the root cell is node 0 */
  while(top > 0)
  { size_t c = stack[--top];
    size_t rep, arity, j;

    if ( g->canon[c] )
      continue;
    g->canon[c] = next++;

    rep   = g->order[g->class_start[c]];
    arity = arityFunctor(g->node[rep].functor);

    for(j=0; j<arity; j++)		/* variables, left to right */
    { int ch = g->child[g->arg[rep]+j];

      if ( ch < 0 )
      { size_t leaf = (size_t)(-ch-1);

	if ( leaf < g->vars && g->vnum[leaf] == 0 )
	  g->vnum[leaf] = vnext++;
      }
    }
    for(j=arity; j-- > 0; )		/* push right to left, so they come */
    { int ch = g->child[g->arg[rep]+j];	/* off the stack left to right */

      if ( ch >= 0 )
	stack[top++] = g->class[ch];
    }
  }

  tmp_free(stack);

  return true;
}


		 /*******************************
		 *	   BUILD THE RESULT	*
		 *******************************/

/* canonical_size() and build_canonical() write the quotient out as a ground,
   acyclic term: a list of the classes in canonical order, each of them the
   class's functor applied to its arguments, where an argument that is a cell
   is the canonical number of its class, a variable is '$VAR'(N) with N its
   canonical number, and anything else is the leaf itself.

   Two terms are =@= exactly when this comes out ==, which is what makes it
   usable as a key: for a cyclic term that is the "canonical cycle" the
   variant_sha1/2 documentation asks for and does not have.
*/

static size_t
canonical_size(bs_graph *g)
{ size_t c, need = 3*g->classes + 2*g->vars;

  for(c=0; c<g->classes; c++)
  { size_t rep = g->order[g->class_start[c]];

    need += 1 + arityFunctor(g->node[rep].functor);
  }

  return need;
}


#define build_canonical(g, base, form) LDFUNC(build_canonical, g, base, form)

static bool
build_canonical(DECL_LD bs_graph *g, Word base, word *form)
{ bs_pool pool = {0};
  Word *ccell, *vcell;
  Word p = base;
  size_t *bycanon;
  size_t i;

  *form = ATOM_nil;
  for(;;)				/* count, then hand out */
  { pool_array(&pool, ccell,   g->classes);
    pool_array(&pool, bycanon, g->classes);
    pool_array(&pool, vcell,   g->vars);

    if ( pool.base )
      break;
    if ( !pool_create(&pool) )
      return false;
  }

  for(i=0; i<g->classes; i++)
    bycanon[g->canon[i]-1] = i;

  for(i=0; i<g->classes; i++)		/* hand out the cells */
  { size_t rep = g->order[g->class_start[bycanon[i]]];

    ccell[i] = p;
    p += 1 + arityFunctor(g->node[rep].functor);
  }
  for(i=0; i<g->vars; i++)
  { vcell[i] = p;
    p += 2;
  }

  for(i=0; i<g->classes; i++)
  { size_t rep   = g->order[g->class_start[bycanon[i]]];
    size_t arity = arityFunctor(g->node[rep].functor);
    size_t j;

    ccell[i][0] = g->node[rep].functor;
    for(j=0; j<arity; j++)
    { int ch = g->child[g->arg[rep]+j];

      if ( ch >= 0 )
      { ccell[i][1+j] = consInt(g->canon[g->class[ch]]);
      } else
      { size_t leaf = (size_t)(-ch-1);

	if ( leaf < g->vars )
	{ ccell[i][1+j] = consPtr(vcell[g->vnum[leaf]-1],
				  TAG_COMPOUND|STG_GLOBAL);
	} else
	{ Word a = &g->node[rep].term->arguments[j];

	  deRef(a);
	  ccell[i][1+j] = *a;
	}
      }
    }
  }

  for(i=0; i<g->vars; i++)
  { vcell[i][0] = FUNCTOR_isovar1;
    vcell[i][1] = consInt(i+1);
  }

  *form = g->classes > 0 ? consPtr(p, TAG_COMPOUND|STG_GLOBAL) : ATOM_nil;
  for(i=0; i<g->classes; i++)		/* the list holding them together */
  { p[0] = FUNCTOR_dot2;
    p[1] = consPtr(ccell[i], TAG_COMPOUND|STG_GLOBAL);
    p[2] = i+1 < g->classes ? consPtr(p+3, TAG_COMPOUND|STG_GLOBAL) : ATOM_nil;
    p += 3;
  }

  tmp_free(pool.base);

  return true;
}


/* minimal_size() is the number of global stack cells the quotient needs:
   one functor plus its arguments for every class.
*/

static size_t
minimal_size(bs_graph *g)
{ size_t c, need = 0;

  for(c=0; c<g->classes; c++)
  { size_t rep = g->order[g->class_start[c]];

    need += 1 + arityFunctor(g->node[rep].functor);
  }

  return need;
}


/* build_minimal() writes the quotient onto the global stack, one cell per
   class, and answers the word denoting the class of the root.  The caller
   has checked there is room, so nothing here can shift the stacks: the
   Functor and Word pointers held in the graph stay valid throughout.
*/

#define build_minimal(g, base, minimal) LDFUNC(build_minimal, g, base, minimal)

static bool
build_minimal(DECL_LD bs_graph *g, Word base, word *minimal)
{ Word *cell;
  Word p = base;
  size_t c;

  *minimal = ATOM_nil;
  if ( !(cell=tmp_malloc(g->classes*sizeof(*cell))) )
    return false;

  for(c=0; c<g->classes; c++)		/* hand out the cells */
  { size_t rep = g->order[g->class_start[c]];

    cell[c] = p;
    p += 1 + arityFunctor(g->node[rep].functor);
  }

  for(c=0; c<g->classes; c++)
  { size_t rep   = g->order[g->class_start[c]];
    size_t arity = arityFunctor(g->node[rep].functor);
    size_t j;

    cell[c][0] = g->node[rep].functor;
    for(j=0; j<arity; j++)
    { int ch = g->child[g->arg[rep]+j];

      if ( ch >= 0 )			/* a cell: point at its class */
      { cell[c][1+j] = consPtr(cell[g->class[ch]], TAG_COMPOUND|STG_GLOBAL);
      } else				/* a leaf: take it as it stands */
      { Word a = &g->node[rep].term->arguments[j];

	deRef(a);
	cell[c][1+j] = isVar(*a) ? makeRefG(a) : *a;
      }
    }
  }

  *minimal = consPtr(cell[g->class[0]], TAG_COMPOUND|STG_GLOBAL);
  tmp_free(cell);

  return true;
}


		 /*******************************
		 *	      PREDICATES	*
		 *******************************/

/** '$term_graph_size'(+Term, -Cells) is det.

Cells is the number of physically distinct compound cells in Term, i.e. the
size of its term graph rather than of its unfolding.  A term of N cells may
denote a tree of 2**N nodes, or an infinite one.
*/

static
PRED_IMPL("$term_graph_size", 2, term_graph_size, 0)
{ PRED_LD
  tmp_buffer tmp;
  Buffer b = (Buffer)&tmp;
  size_t cells;
  boolex_t rc;

  initBuffer(&tmp);
  rc = build_graph(valTermRef(A1), b, 0);
  cells = entriesBuffer(b, bs_node);
  restore_graph(b);
  discardBuffer(b);

  if ( rc == MEMORY_OVERFLOW )
    return PL_error(NULL, 0, NULL, ERR_NOMEM);
  if ( rc != true )
    return false;			/* exception already raised */

  return PL_unify_int64(A2, cells);
}


/** '$term_graph_classes'(+Term, -Classes) is det.

Classes is the number of compound cells the graph of Term has after
collapsing cells that denote the same tree.  It is the number of cells
term_minimal/2 will produce.
*/

static
PRED_IMPL("$term_graph_classes", 2, term_graph_classes, 0)
{ PRED_LD
  tmp_buffer tmp;
  Buffer b = (Buffer)&tmp;
  bs_graph g;
  size_t classes;
  boolex_t rc;

  initBuffer(&tmp);
  rc = term_quotient(valTermRef(A1), &g, b, 0, true);
  classes = g.classes;
  free_graph(&g);
  discardBuffer(b);

  if ( rc == MEMORY_OVERFLOW )
    return PL_error(NULL, 0, NULL, ERR_NOMEM);
  if ( rc != true )
    return false;

  return PL_unify_int64(A2, classes);
}


/** term_minimal(+Term, -Minimal) is det.

Minimal == Term, using the least possible number of compound cells.  Cells
denoting the same tree are shared, so a term that is cyclic or has repeated
subterms comes back smaller, and one that has neither comes back unchanged.
Because ==/2 on rational trees is bisimulation, Minimal is the smallest term
there is that is still == to Term.
*/

static
PRED_IMPL("term_minimal", 2, term_minimal, 0)
{ PRED_LD
  term_t result = PL_new_term_ref();

  if ( !result )
    return false;

  for(;;)
  { tmp_buffer tmp;
    Buffer b = (Buffer)&tmp;
    bs_graph g;
    size_t need;
    boolex_t rc;

    initBuffer(&tmp);
    rc = term_quotient(valTermRef(A1), &g, b, 0, true);

    if ( rc == true && g.nodes == 0 )	/* nothing to share */
    { free_graph(&g);
      discardBuffer(b);
      return PL_unify(A2, A1);
    }

    if ( rc == true )
    { need = minimal_size(&g);
      if ( gTop + need > gMax )
	rc = GLOBAL_OVERFLOW;
      else
      { word w;

	if ( build_minimal(&g, gTop, &w) )
	{ gTop += need;
	  *valTermRef(result) = w;
	} else
	  rc = MEMORY_OVERFLOW;
      }
    }

    free_graph(&g);
    discardBuffer(b);

    if ( rc == true )
      break;
    if ( rc == MEMORY_OVERFLOW )
      return PL_error(NULL, 0, NULL, ERR_NOMEM);
    if ( rc != GLOBAL_OVERFLOW )
      return false;
    if ( !makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
      return false;
  }

  return PL_unify(A2, result);
}


/* term_canonical_form() answers a ground acyclic term that is == for two
   terms exactly when they are =@=, including when they are cyclic.  It is
   the quotient of the term graph written out in canonical order.

   pl-termhash.c uses it to hash a cyclic term: the digest of the form is a
   digest of the term up to =@=, which is what variant_hash/2 promises and
   what it cannot do by walking a term that has no end.
*/

bool
term_canonical_form(DECL_LD term_t t, term_t form, bool *attvars)
{ term_t result = PL_new_term_ref();

  if ( attvars )
    *attvars = false;

  if ( !result )
    return false;

  for(;;)
  { tmp_buffer tmp;
    Buffer b = (Buffer)&tmp;
    bs_graph g;
    size_t need;
    boolex_t rc;

    initBuffer(&tmp);
    rc = term_quotient(valTermRef(t), &g, b, 0, true);

    if ( rc == true && attvars )
      *attvars = g.attvars;

    if ( rc == true && g.nodes == 0 )	/* no cells: the term is its own form */
    { free_graph(&g);
      discardBuffer(b);
      return PL_unify(form, t);
    }

    if ( rc == true && !canonicalise(&g) )
      rc = MEMORY_OVERFLOW;

    if ( rc == true )
    { need = canonical_size(&g);
      if ( gTop + need > gMax )
	rc = GLOBAL_OVERFLOW;
      else
      { word w;

	if ( build_canonical(&g, gTop, &w) )
	{ gTop += need;
	  *valTermRef(result) = w;
	} else
	  rc = MEMORY_OVERFLOW;
      }
    }

    free_graph(&g);
    discardBuffer(b);

    if ( rc == true )
      break;
    if ( rc == MEMORY_OVERFLOW )
      return PL_error(NULL, 0, NULL, ERR_NOMEM);
    if ( rc != GLOBAL_OVERFLOW )
      return false;
    if ( !makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
      return false;
  }

  return PL_unify(form, result);
}


/** '$term_canonical_form'(+Term, -Form) is det.

Form is a ground, acyclic term with Form1 == Form2 exactly when Term1 and
Term2 are =@=, including when they are cyclic.
*/

static
PRED_IMPL("$term_canonical_form", 2, term_canonical_form_pred, 0)
{ PRED_LD

  return term_canonical_form(A1, A2, NULL);
}


		 /*******************************
		 *	   FACTORIZED FORM	*
		 *******************************/

/* A class needs a name when it is used more than once, counting the root as
   a use.  That is what term_factorized/3 has always done -- its rbtree
   counts how often each subterm is reached in the graph -- and it is also
   exactly enough to break every cycle, so the skeleton stays finite: a cycle
   reachable from the root either contains the root, which is a use on top of
   the one from the cycle, or is entered from outside, which gives its entry
   class a second use.

   A class used once needs no name: it is written where it is used.
*/

static bool
name_classes(bs_graph *g)
{ size_t c;

  if ( !(g->uses=tmp_malloc(g->classes*sizeof(*g->uses))) )
    return false;

  for(c=0; c<g->classes; c++)
    g->uses[c] = 0;
  g->uses[g->class[0]]++;		/* the root is a use */

  for(c=0; c<g->classes; c++)
  { size_t rep   = g->order[g->class_start[c]];
    size_t arity = arityFunctor(g->node[rep].functor);
    size_t j;

    for(j=0; j<arity; j++)
    { int ch = g->child[g->arg[rep]+j];

      if ( ch >= 0 )
	g->uses[g->class[ch]]++;
    }
  }

  g->named = 0;
  for(c=0; c<g->classes; c++)
  { if ( g->uses[c] > 1 )
      g->named++;
  }

  return true;
}


static size_t
factorized_size(bs_graph *g)
{ size_t c, need = g->named * (1+3+3);	/* variable, =/2, list cell */

  for(c=0; c<g->classes; c++)
  { size_t rep = g->order[g->class_start[c]];

    need += 1 + arityFunctor(g->node[rep].functor);
  }

  return need;
}


/* build_factorized() writes one cell per class.  A class used once is
   referred to by a pointer to its cell, which is the same as writing it out
   there, as it is written nowhere else.  A class that needs a name gets a
   fresh variable, and its cell becomes the right hand side of an equation.
   The skeleton is then acyclic, because every cycle runs through a named
   class and stops at its variable.

   The equations come out in canonical order, so that terms that are =@= give
   substitutions that are =@= -- which is what nb_set and
   solution_sequences:trieable/2 need of them, and what ordering by the
   standard order of the values fails to give, that being variable age.
*/

#define build_factorized(g, base, skel, subst) \
	LDFUNC(build_factorized, g, base, skel, subst)

static bool
build_factorized(DECL_LD bs_graph *g, Word base, word *skel, word *subst)
{ bs_pool pool = {0};
  Word *cell, *var;
  Word p = base;
  size_t *bycanon;
  size_t c, i;
  size_t root = g->class[0];

  *skel  = ATOM_nil;			/* never used; keeps gcc quiet */
  *subst = ATOM_nil;

  if ( g->classes == 0 )
    return false;

  for(;;)				/* count, then hand out */
  { pool_array(&pool, cell,    g->classes);
    pool_array(&pool, bycanon, g->classes);
    pool_array(&pool, var,     g->classes);

    if ( pool.base )
      break;
    if ( !pool_create(&pool) )
      return false;
  }

  for(c=0; c<g->classes; c++)
  { bycanon[g->canon[c]-1] = c;
    var[c] = NULL;
  }

  for(c=0; c<g->classes; c++)		/* hand out the cells */
  { size_t rep = g->order[g->class_start[c]];

    cell[c] = p;
    p += 1 + arityFunctor(g->node[rep].functor);
  }
  for(i=0; i<g->classes; i++)		/* and a variable per named class */
  { size_t cc = bycanon[i];

    if ( g->uses[cc] > 1 )
    { var[cc] = p++;
      setVar(*var[cc]);
    }
  }

  for(c=0; c<g->classes; c++)
  { size_t rep   = g->order[g->class_start[c]];
    size_t arity = arityFunctor(g->node[rep].functor);
    size_t j;

    cell[c][0] = g->node[rep].functor;
    for(j=0; j<arity; j++)
    { int ch = g->child[g->arg[rep]+j];

      if ( ch >= 0 )
      { size_t d = g->class[ch];

	cell[c][1+j] = var[d] ? makeRefG(var[d])
			      : consPtr(cell[d], TAG_COMPOUND|STG_GLOBAL);
      } else
      { Word a = &g->node[rep].term->arguments[j];

	deRef(a);
	cell[c][1+j] = isVar(*a) ? makeRefG(a) : *a;
      }
    }
  }

  *skel = var[root] ? makeRefG(var[root])
		    : consPtr(cell[root], TAG_COMPOUND|STG_GLOBAL);

  *subst = ATOM_nil;			/* the equations, canonical order */
  for(i=g->classes; i-- > 0; )
  { size_t cc = bycanon[i];

    if ( var[cc] )
    { Word eq = p;

      eq[0] = FUNCTOR_equals2;
      eq[1] = makeRefG(var[cc]);
      eq[2] = consPtr(cell[cc], TAG_COMPOUND|STG_GLOBAL);
      p += 3;

      p[0] = FUNCTOR_dot2;
      p[1] = consPtr(eq, TAG_COMPOUND|STG_GLOBAL);
      p[2] = *subst;
      *subst = consPtr(p, TAG_COMPOUND|STG_GLOBAL);
      p += 3;
    }
  }

  tmp_free(pool.base);

  return true;
}


/** term_factorized(+Term, -Skeleton, -Substitution) is det.

Skeleton is Term with every subterm that is used more than once replaced by a
variable, and Substitution is a list of Var=Value giving those subterms, such
that binding them all makes Skeleton == Term.  Term may be cyclic; the cycles
come back through the substitutions, so Skeleton itself is acyclic.

Subterms are the same when they are ==, so for a rational tree they are the
same when they denote the same infinite tree, and Term is factorized as far
as it can be.  '$VAR'/1 is left alone, as it stands for a variable rather
than being an ordinary compound.
*/

#define do_term_factorized(t, skeleton, substitution, opaque, minimal) \
	LDFUNC(do_term_factorized, t, skeleton, substitution, opaque, minimal)

static bool
do_term_factorized(DECL_LD term_t t, term_t skeleton, term_t substitution,
		   word opaque, bool minimal)
{ term_t skel  = PL_new_term_ref();
  term_t subst = PL_new_term_ref();

  if ( !skel || !subst )
    return false;

  for(;;)
  { tmp_buffer tmp;
    Buffer b = (Buffer)&tmp;
    bs_graph g;
    size_t need;
    boolex_t rc;

    initBuffer(&tmp);
    rc = term_quotient(valTermRef(t), &g, b, opaque, minimal);

    if ( rc == true && g.nodes == 0 )	/* no cells: nothing to factor out */
    { free_graph(&g);
      discardBuffer(b);
      return ( PL_unify(skeleton, t) &&
	       PL_unify_nil(substitution) );
    }

    if ( rc == true && (!canonicalise(&g) || !name_classes(&g)) )
      rc = MEMORY_OVERFLOW;

    if ( rc == true )
    { need = factorized_size(&g);
      if ( gTop + need > gMax )
	rc = GLOBAL_OVERFLOW;
      else
      { word s, e;

	if ( build_factorized(&g, gTop, &s, &e) )
	{ gTop += need;
	  *valTermRef(skel)  = s;
	  *valTermRef(subst) = e;
	} else
	  rc = MEMORY_OVERFLOW;
      }
    }

    free_graph(&g);
    discardBuffer(b);

    if ( rc == true )
      break;
    if ( rc == MEMORY_OVERFLOW )
      return PL_error(NULL, 0, NULL, ERR_NOMEM);
    if ( rc != GLOBAL_OVERFLOW )
      return false;
    if ( !makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
      return false;
  }

  return ( PL_unify(skeleton, skel) &&
	   PL_unify(substitution, subst) );
}


static const PL_option_t factorized_options[] =
{ { ATOM_minimal,	OPT_BOOL },
  { ATOM_dollar_var,	OPT_BOOL },
  { NULL_ATOM,		0 }
};


static
PRED_IMPL("term_factorized", 3, term_factorized, 0)
{ PRED_LD

  return do_term_factorized(A1, A2, A3, FUNCTOR_isovar1, true);
}


/** term_factorized(+Term, -Skeleton, -Substitution, +Options) is det.

As term_factorized/3, with

  - minimal(Bool)
    When `true` (default), subterms are the same when they are ==, so for a
    rational tree when they denote the same infinite tree.  When `false`,
    only cells that are literally the same cell are shared.
  - dollar_var(Bool)
    When `true` (default), '$VAR'/1 is left alone rather than taken apart.
*/

static
PRED_IMPL("term_factorized", 4, term_factorized4, 0)
{ PRED_LD
  int minimal = true;
  int dollar_var = true;

  if ( !PL_scan_options(A4, 0, "factorize_option", factorized_options,
			&minimal, &dollar_var) )
    return false;

  return do_term_factorized(A1, A2, A3,
			    dollar_var ? FUNCTOR_isovar1 : 0,
			    minimal);
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(bisim)
  PRED_DEF("$term_graph_size",    2, term_graph_size,    0)
  PRED_DEF("$term_graph_classes", 2, term_graph_classes, 0)
  PRED_DEF("term_minimal",        2, term_minimal,        0)
  PRED_DEF("$term_canonical_form", 2, term_canonical_form_pred, 0)
  PRED_DEF("term_factorized",     3, term_factorized,     0)
  PRED_DEF("term_factorized",     4, term_factorized4,    0)
EndPredDefs
