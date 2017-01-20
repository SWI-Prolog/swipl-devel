/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2011, University of Amsterdam
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

#undef LD
#define LD LOCAL_LD


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
$btree_find_node(+Value, +Tree, -Node, -Arg)

Assuming Tree is a term x(Key, Left, Right,  ...), find a (sub) node for
operating on Value. If a node with Key  == Value is found Arg is unified
to 1. If such a node is not found Arg   is  2 if the tree must get a new
left-node and 3 if it must get a new right-node.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static
PRED_IMPL("$btree_find_node", 4, btree_find_node, 0)
{ PRED_LD
  Word t, k;
  Functor f;
  functor_t fd;
  int arity;

  k = valTermRef(A1);
  t = valTermRef(A2);

  deRef(k);
  deRef(t);

  if ( !isTerm(*t) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_btree, A2);
  f = valueTerm(*t);
  fd = f->definition;
  arity = arityFunctor(fd);
  if ( arity < 3 )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_btree, A2);

  for(;;)
  { Word a = &f->arguments[0];
    Word n;
    int d = compareStandard(k, a, FALSE PASS_LD);
    int arg;

    if ( d == CMP_ERROR )
      return FALSE;
    if ( d == CMP_EQUAL )
    { if ( unify_ptrs(t, valTermRef(A3), ALLOW_GC|ALLOW_SHIFT PASS_LD) &&
	   PL_unify_integer(A4, 1) )
	succeed;
      fail;
    }

    arg = (d == CMP_LESS ? 1 : 2);
    n = a+arg;
    deRef(n);
    DEBUG(1, Sdprintf("Taking %s\n", arg == 1 ? "left" : "right"));

    if ( !isTerm(*n) )
    { nomatch:

      if ( unify_ptrs(t, valTermRef(A3), ALLOW_GC|ALLOW_SHIFT PASS_LD) &&
	   PL_unify_integer(A4, arg+1) )
	succeed;
      fail;
    }
    f = valueTerm(*n);
    if ( f->definition != fd )
      goto nomatch;

    t = n;
  }

  succeed;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(btree)
  PRED_DEF("$btree_find_node", 4, btree_find_node, 0)
EndPredDefs
