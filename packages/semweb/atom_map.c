/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

#include <SWI-Prolog.h>
#include "atom_set.h"
#include <string.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file realises the low-level support   for  indexing literals in the
semantic web library. The idea is to   make a map from abstracted tokens
from each literal to  the  exact   literals.  Abstraction  introduces  a
certain amount of ambiguity that  makes   fuzzy  matching possible. Good
abstraction candidates are the Porter  Stem   algorithm  and  the Double
Metaphone algorithm. Both are provide by the SWI-Prolog NLP package.

Basic query provides a  set  of   abstracted  terms  and  requests those
literals containing all of  them.  Ideally   we  would  like to maintain
ordered sets of literals and do set-intersection on them to achieve good
linear performance.

Some current artchive statistics (porter stem)

	  # stems: 0.4 million
	  # literals: 0.9 million
	  # stem->literal relations: 3.1 million

Av. literals/stem: about 8.

Searching is done using

	atom_map_query(SetOfAbstract, -Literals)

TBD: destroy nodes when destroying the tree

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	     BASIC STUFF	*
		 *******************************/

static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_type_error2;
static functor_t FUNCTOR_resource_error1;
static functor_t FUNCTOR_atom_map1;

#define MKFUNCTOR(n,a) \
	FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)

static void
init_functors()
{ MKFUNCTOR(error, 2);
  MKFUNCTOR(type_error, 2);
  MKFUNCTOR(resource_error, 1);
  FUNCTOR_atom_map1 = PL_new_functor(PL_new_atom("$atom_map"), 1);
}


static int
type_error(term_t actual, const char *expected)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_type_error2,
		        PL_CHARS, expected,
		        PL_TERM, actual,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
resource_error(const char *what)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_resource_error1,
		        PL_CHARS, what,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}


static int
get_atom_map(term_t t, avl_tree **tree)
{ if ( PL_is_functor(t, FUNCTOR_atom_map1) )
  { term_t a = PL_new_term_ref();
    void *ptr;

    _PL_get_arg(1, t, a);
    if ( PL_get_pointer(a, &ptr) )
    { avl_tree *avl = ptr;

      if ( avl->magic == AVL_MAGIC )
      { *tree = avl;
        return TRUE;
      }
    }
  }

  return type_error(t, "atom_map");
}


static int
unify_atom_map(term_t t, avl_tree *tree)
{ return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_atom_map1,
		            PL_POINTER, tree);
}


static int
get_atom_ex(term_t t, atom_t *a)
{ if ( !PL_get_atom(t, a) )
    return type_error(t, "atom");

  return TRUE;
}


		 /*******************************
		 *	     ATOM SETS		*
		 *******************************/

#define AS_INITIAL_SIZE 4

typedef struct atom_set
{ size_t  size;				/* # cells in use */
  size_t  allocated;			/* # cells allocated */
  atom_t *atoms;			/* allocated cells */
} atom_set;


static atom_set *
new_atom_set(atom_t a0)
{ atom_set *as;

  if ( (as = malloc(sizeof(*as))) &&
       (as->atoms = malloc(sizeof(atom_t)*AS_INITIAL_SIZE)) )
  { as->size = 1;
    as->allocated = AS_INITIAL_SIZE;
    as->atoms[0] = a0;
  }
  
  return as;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
find_in_atom_set(atom_set *as, atom_t  a)  returns   a  pointer  to  the
location of the atom or, if the atom  isn't there, to the first location
*after* the atom
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static atom_t *
find_in_atom_set(atom_set *as, atom_t a)
{ const atom_t *ap = (const atom_t *)as->atoms;
  const atom_t *ep = &ap[as->size];

  for(;;)
  { const atom_t *cp = ap+(ep-ap)/2;
    
    if ( a < *cp )
    { ep = cp;
    } else if ( a > *cp )
    { ap = cp;
    } else
    { return (atom_t*)cp;
    }

    if ( ap == ep )
    { if ( a > *ap )
	ap++;
      return (atom_t*)ap;
    }
  }
}


static int
in_atom_set(atom_set *as, atom_t a)
{ atom_t *ap = find_in_atom_set(as, a);

  return *ap == a;
}


#define ptr_diff(p1, p2) ((char *)(p1) - (char *)(p2))

static int
insert_atom_set(atom_set *as, atom_t a)
{ atom_t *ap = find_in_atom_set(as, a);
  
  if ( *ap != a )
  { PL_register_atom(a);

    if ( ++as->size > as->allocated )
    { atom_t *na;
      size_t newsize = as->allocated*2;

      if ( (na = malloc(sizeof(atom_t)*newsize)) )
      { size_t i = ap-as->atoms;
	memcpy(na, as->atoms, ptr_diff(ap, as->atoms));
	na[i++] = a;
	memcpy(&na[i], ap, ptr_diff(&as->atoms[as->size], ap));
      } else
	return FALSE;
    } else
    { memmove(ap+1, ap, ptr_diff(&as->atoms[as->size], ap));
      *ap = a;
    }
  }

  return TRUE;
}


static void
destroy_atom_set(atom_set *as)
{ int i;

  for(i=0; i<as->size; i++)
    PL_unregister_atom(as->atoms[i]);

  free(as->atoms);
  free(as);
}



		 /*******************************
		 *	   TREE INTERFACE	*
		 *******************************/

static foreign_t
new_atom_map(term_t handle)
{ avl_tree *t;

  if ( !(t=malloc(sizeof(*t))) )
    return resource_error("memory");

  avl_init(t);

  return unify_atom_map(handle, t);
}


static foreign_t
destroy_atom_map(term_t handle)
{ avl_tree *tree;

  if ( !get_atom_map(handle, &tree) )
    return FALSE;

  avl_destroy(tree);

  return TRUE;
}


static foreign_t
insert_atom_map(term_t handle, term_t from, term_t to)
{ avl_tree *tree;
  avl_node *node;
  atom_t a1, a2;

  if ( !get_atom_map(handle, &tree) ||
       !get_atom_ex(from, &a1) ||
       !get_atom_ex(to, &a2) )
    return FALSE;
  
  avl_insert(tree, a1, &node);
  if ( !node->value )
  { PL_register_atom(a1);

    if ( !(node->value = new_atom_set(a2)) )
      return resource_error("memory");
  } else
  { if ( !(insert_atom_set(node->value, a2)) )
      return resource_error("memory");
  }

  return TRUE;
}


static int
cmp_atom_set_size(const void *p1, const void *p2)
{ const atom_set *ap1 = p1;
  const atom_set *ap2 = p2;

  return ap1->size - ap2->size;
}


#define MAX_SETS 100

static foreign_t
find_atom_map(term_t handle, term_t keys, term_t literals)
{ avl_tree  *tree;
  atom_set  *as[MAX_SETS];		/* TBD */
  int ns = 0;
  term_t tail = PL_copy_term_ref(keys);
  term_t head = PL_new_term_ref();
  atom_set *s0;
  int ca;

  if ( !get_atom_map(handle, &tree) )
    return FALSE;

  while(PL_get_list(tail, head, tail))
  { atom_t a;
    avl_node *node;
  
    if ( !get_atom_ex(head, &a) )
      return FALSE;
    
    if ( (node = avl_find_node(tree, a)) )
    { if ( ns+1 >= MAX_SETS )
	return resource_error("max_search_atoms");

      as[ns++] = node->value;
    } else
    { return FALSE;
    }
  }
  if ( !PL_get_nil(tail) )
    return type_error(tail, "list");

  qsort(as, ns, sizeof(*as), cmp_atom_set_size);
  s0 = as[0];

  PL_put_term(tail, literals);

  for(ca=0; ca<s0->size; ca++)
  { atom_t a = s0->atoms[ca];
    int i;

    for(i=1; i<ns; i++)
    { if ( !in_atom_set(as[i], a) ) 
      { if ( a > as[i]->atoms[as[i]->size-1] )
	  return FALSE;
	goto next;
      }
    }
    
    if ( !PL_unify_list(tail, head, tail) ||
	 !PL_unify_atom(head, a) )
      return FALSE;
next:;
  }

  return PL_unify_nil(tail);
}


install_t
install_atom_map()
{ init_functors();

  PL_register_foreign("rdf_new_literal_map",     1, new_atom_map,     0);
  PL_register_foreign("rdf_destroy_literal_map", 1, destroy_atom_map, 0);
  PL_register_foreign("rdf_insert_literal_map",  3, insert_atom_map,  0);
  PL_register_foreign("rdf_find_literal_map",    3, find_atom_map,    0);
}
