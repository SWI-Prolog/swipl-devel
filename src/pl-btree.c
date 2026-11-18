/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2018, University of Amsterdam
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
#include "pl-prims.h"
#include "pl-fli.h"
#include "pl-gc.h"
#include "pl-attvar.h"

#undef LD
#define LD LOCAL_LD


/** '$btree_find_node'(+Key, +Tree, +Pos, -Node, -Arg)

Assuming Tree is a term x(...)  with   Value,  Left and Right defined by
Pos, find a (sub) node for operating  on   Value.  If a node with Key ==
Value is found Arg is unified with =. If such a node is not found Arg
is < if the tree must get a new left-node and > if it must get a new
right-node.

@arg Pos is 256*256*KeyPos + 256*LeftPos + RightPos
*/


static
PRED_IMPL("$btree_find_node", 5, btree_find_node, 0)
{ PRED_LD
  Word t, k;
  Functor f;
  functor_t fd;
  size_t arity;
  unsigned int p, kp, lp, rp;

  if ( !PL_cvt_i_uint(A3, &p) )
    return false;
  rp = (p       & 0xff)-1;
  lp = ((p>>8)  & 0xff)-1;
  kp = ((p>>16) & 0xff)-1;

  k = valTermRef(A1);
  t = valTermRef(A2);

  deRef(k);
  deRef(t);

  if ( !isTerm(*t) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_btree, A2);
  f = valueTerm(*t);
  fd = word2functor(f->definition);
  arity = arityFunctor(fd);
  if ( arity < kp || arity < lp || arity < rp )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_btree, A2);

  for(;;)
  { Word a = &f->arguments[kp];
    Word n;
    int d = compareStandard(k, a, false);
    int arg;

    if ( d == CMP_ERROR )
      return false;
    if ( d == CMP_EQUAL )
    { if ( unify_ptrs(t, valTermRef(A4), ALLOW_GC|ALLOW_SHIFT) &&
	   PL_unify_atom(A5, ATOM_equals) )
	return true;
      return false;
    }

    arg = (d == CMP_LESS ? lp : rp);
    n = &f->arguments[arg];
    deRef(n);
    DEBUG(1, Sdprintf("Taking %s\n", arg == lp ? "left" : "right"));

    if ( !isTerm(*n) )
    { nomatch:

      if ( unify_ptrs(t, valTermRef(A4), ALLOW_GC|ALLOW_SHIFT) &&
	   PL_unify_atom(A5, arg == lp ? ATOM_smaller : ATOM_larger ) )
	return true;
      return false;
    }
    f = valueTerm(*n);
    if ( f->definition != fd )
      goto nomatch;

    t = n;
  }
}


/** '$btree_avl_put'(+Key, +Tree0, +Value, -Tree) is semidet.

Fast path for put_assoc/4 from library(assoc).   Tree0 is an AVL tree as
used by that library: the atom `t` or t(Key,Value,Balance,Left,Right),
where Balance is one of `<`, `-` or `>`.

This succeeds only if it does exactly what the Prolog definition does.
It fails, without error, if it finds anything else.  The caller must
then run the Prolog definition.  This is the case if

  - something on the path to Key is not a node,
  - a node that gets deeper has a balance that cannot result from
    inserting into an AVL tree,
  - the path is longer than AVL_MAX_DEPTH, or
  - the `occurs_check` flag is not `false`.

We first walk down to find the place for  Key, remembering the path.  As
each node on the path is copied once and we need at most one new leaf we
then know how much space we need.   Rotations modify the copies, which
is safe because nothing else can see them yet.
*/

#define AVL_MAX_DEPTH 128		/* no AVL tree gets this deep */

#define AVL_KEY	      1			/* offsets in a node */
#define AVL_VALUE     2
#define AVL_BALANCE   3
#define AVL_LEFT      4
#define AVL_RIGHT     5
#define AVL_NODE_SIZE 6

#define AVL_HEAVY(side) ((side) == AVL_LEFT ? ATOM_smaller : ATOM_larger)
#define AVL_OTHER(side) (AVL_LEFT+AVL_RIGHT-(side))

typedef struct
{ Word node;				/* node in Tree0 */
  int  dir;				/* AVL_LEFT or AVL_RIGHT */
} avl_path;

static atom_t	 ATOM_avl_t;		/* t */
static functor_t FUNCTOR_avl_t5;	/* t/5 */

static
PRED_IMPL("$btree_avl_put", 4, btree_avl_put, 0)
{ PRED_LD
  avl_path path[AVL_MAX_DEPTH];
  int depth;
  bool found, changed;
  Word k, t, g, cur, kcell, vcell, out;
  size_t need;
  term_t tmp = 0;
  word tree;

  if ( LD->prolog_flag.occurs_check != OCCURS_CHECK_FALSE )
    return false;

  if ( unlikely(!FUNCTOR_avl_t5) )
  { ATOM_avl_t     = PL_new_atom("t");
    FUNCTOR_avl_t5 = PL_new_functor(ATOM_avl_t, 5);
  }

  out = valTermRef(A4);
  deRef(out);
  if ( !isVar(*out) &&			/* may shift, so before we */
       !(tmp = PL_new_term_ref()) )	/* take pointers */
    return false;

retry:
  k = valTermRef(A1);
  t = valTermRef(A2);
  deRef(k);
  deRef(t);
  depth = 0;
  found = false;

  while( *t != ATOM_avl_t )
  { Word n;
    cmpex_t d;

    if ( !hasFunctor(*t, FUNCTOR_avl_t5) || depth == AVL_MAX_DEPTH )
      return false;

    n = (Word)valueTerm(*t);
    path[depth].node = n;
    d = compareStandard(k, &n[AVL_KEY], false);
    if ( d == CMP_ERROR )
      return false;
    if ( d == CMPEX_EQUAL )
    { found = true;
      break;
    }
    path[depth].dir = (d < 0 ? AVL_LEFT : AVL_RIGHT);
    t = &n[path[depth].dir];
    deRef(t);
    depth++;
  }

  need = (size_t)(depth+1)*AVL_NODE_SIZE;
  if ( !hasStackSpace(need, 3) )
  { if ( !ensureStackSpace(need, 3) )
      return false;
    goto retry;				/* the stacks may have moved */
  }

  g = gTop;
  cur = g;
  g += AVL_NODE_SIZE;
  cur[0] = FUNCTOR_avl_t5;
  vcell = &cur[AVL_VALUE];
  if ( found )
  { Word o = path[depth].node;

    kcell = NULL;
    cur[AVL_KEY]     = linkValI(&o[AVL_KEY]);
    cur[AVL_BALANCE] = linkValI(&o[AVL_BALANCE]);
    cur[AVL_LEFT]    = linkValI(&o[AVL_LEFT]);
    cur[AVL_RIGHT]   = linkValI(&o[AVL_RIGHT]);
    changed = false;
  } else
  { kcell = &cur[AVL_KEY];
    cur[AVL_BALANCE] = ATOM_minus;
    cur[AVL_LEFT]    = ATOM_avl_t;
    cur[AVL_RIGHT]   = ATOM_avl_t;
    changed = true;			/* the subtree got deeper */
  }

  for(int i=depth-1; i>=0; i--)
  { Word o    = path[i].node;
    int dir   = path[i].dir;
    int other = AVL_OTHER(dir);
    Word n    = g;

    g += AVL_NODE_SIZE;
    n[0]         = FUNCTOR_avl_t5;
    n[AVL_KEY]   = linkValI(&o[AVL_KEY]);
    n[AVL_VALUE] = linkValI(&o[AVL_VALUE]);
    n[other]     = linkValI(&o[other]);

    if ( !changed )
    { n[AVL_BALANCE] = linkValI(&o[AVL_BALANCE]);
      n[dir] = consCompound(cur);
      cur = n;
    } else
    { Word bp = &o[AVL_BALANCE];
      word b0;

      deRef(bp);
      b0 = *bp;
      if ( b0 == ATOM_minus )
      { n[AVL_BALANCE] = AVL_HEAVY(dir);
	n[dir] = consCompound(cur);
	cur = n;
      } else if ( b0 == AVL_HEAVY(other) )
      { n[AVL_BALANCE] = ATOM_minus;
	n[dir] = consCompound(cur);
	cur = n;
	changed = false;
      } else if ( b0 == AVL_HEAVY(dir) )
      { word cb = cur[AVL_BALANCE];

	if ( cb == b0 )			/* single rotation */
	{ n[AVL_BALANCE]   = ATOM_minus;
	  n[dir]           = cur[other];
	  cur[AVL_BALANCE] = ATOM_minus;
	  cur[other]       = consCompound(n);
	} else if ( cb == AVL_HEAVY(other) ) /* double rotation */
	{ Word x  = valPtr(cur[other]);
	  word b1 = x[AVL_BALANCE];

	  n[dir]           = x[other];
	  cur[other]       = x[dir];
	  n[AVL_BALANCE]   = (b1 == AVL_HEAVY(dir)   ? AVL_HEAVY(other)
						     : ATOM_minus);
	  cur[AVL_BALANCE] = (b1 == AVL_HEAVY(other) ? AVL_HEAVY(dir)
						     : ATOM_minus);
	  x[AVL_BALANCE]   = ATOM_minus;
	  x[other]         = consCompound(n);
	  x[dir]           = consCompound(cur);
	  cur = x;
	} else				/* not an AVL tree */
	{ return false;
	}
	changed = false;
      } else				/* not a balance */
      { return false;
      }
    }
  }

  if ( kcell )
    bArgVar(kcell, k);
  bArgVar(vcell, valTermRef(A3));
  gTop = g;
  tree = consCompound(cur);

  if ( tmp )
  { *valTermRef(tmp) = tree;
    return PL_unify(A4, tmp);
  }

  out = valTermRef(A4);
  deRef(out);
  return varBindConst(out, tree);
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(btree)
  PRED_DEF("$btree_find_node", 5, btree_find_node, 0)
  PRED_DEF("$btree_avl_put",   4, btree_avl_put,   0)
EndPredDefs
