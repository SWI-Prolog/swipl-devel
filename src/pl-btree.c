/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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
