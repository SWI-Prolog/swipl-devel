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
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include "atom_set.h"
#include "lock.h"
#include "atom.h"
#include "debug.h"
#include <string.h>
#include <assert.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file realises the low-level support   for  indexing literals in the
semantic web library. The idea is to   make a map from abstracted tokens
from each literal to  the  exact   literals.  Abstraction  introduces  a
certain amount of ambiguity that  makes   fuzzy  matching possible. Good
abstraction candidates are the Porter Stem  or Snowbal algorithm and the
Double Metaphone algorithm. Both  are  provide   by  the  SWI-Prolog NLP
package.

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

#define AM_MAGIC	0x6ab19e8e

typedef struct atom_map
{ long		magic;			/* AM_MAGIC */
  rwlock	lock;			/* Multi-threaded access */
  avl_tree	tree;			/* AVL tree */
} atom_map;


#define RDLOCK(map)			rdlock(&map->lock)
#define WRLOCK(map, allowreaders)	wrlock(&map->lock, allowreaders)
#define WRUNLOCK(map)			unlock(&map->lock, FALSE)
#define RDUNLOCK(map)			unlock(&map->lock, TRUE)

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
  FUNCTOR_atom_map1 = PL_new_functor(PL_new_atom("$literal_map"), 1);
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
get_atom_map(term_t t, atom_map **map)
{ if ( PL_is_functor(t, FUNCTOR_atom_map1) )
  { term_t a = PL_new_term_ref();
    void *ptr;

    _PL_get_arg(1, t, a);
    if ( PL_get_pointer(a, &ptr) )
    { atom_map *am = ptr;

      if ( am->magic == AM_MAGIC )
      { *map = am;
        return TRUE;
      }
    }
  }

  return type_error(t, "atom_map");
}


static int
unify_atom_map(term_t t, atom_map *map)
{ return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_atom_map1,
		            PL_POINTER, map);
}


		 /*******************************
		 *	       DATUM		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Datum is either an atom or a 31-bit  signed integer. Atoms are shifted 7
bits
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef void *datum;

#define ATOM_TAG_BITS 7
#define ATOM_TAG 0x1

#define tag(d)		((long)(d)&0x1)
#define isAtomDatum(d)  ((long)(d)&ATOM_TAG)

static int atom_mask;

static void
init_datum_store()
{ atom_t a = PL_new_atom("hdowefho");
  
  atom_mask = a & ((1<<(ATOM_TAG_BITS-1))-1);
}


static inline atom_t
atom_from_datum(datum d)
{ unsigned long v = (unsigned long)d;
  atom_t a;

  a  = ((v&~0x1)<<(ATOM_TAG_BITS-1))|atom_mask;
  DEBUG(9, Sdprintf("0x%lx --> %s\n", v, PL_atom_chars(a)));
  return a;
}


static inline long
long_from_datum(datum d)
{ unsigned long v = (unsigned long)d;

  return (v>>1);
}


static int
get_datum(term_t t, datum* d)
{ atom_t a;
  long l;

  if ( PL_get_atom(t, &a) )
  { unsigned long v = (a>>(ATOM_TAG_BITS-1))|ATOM_TAG;

    SECURE(assert(atom_from_datum((datum)v) == a));

    *d = (datum)v;
    DEBUG(9, Sdprintf("Atom %s --> 0x%lx\n", PL_atom_chars(a), v));
    return TRUE;
  } else if ( PL_get_long(t, &l) )
  { *d = (datum)(l<<1);			/* TBD: verify range */
    return TRUE;
  }

  return type_error(t, "atom or integer");
}


static int
unify_datum(term_t t, datum d)
{ unsigned long v = (unsigned long)d;

  if ( isAtomDatum(v) )
    return PL_unify_atom(t, atom_from_datum(d));
  else
    return PL_unify_integer(t, long_from_datum(d));
}


static void
lock_datum(datum d)
{ unsigned long v = (unsigned long)d;

  if ( isAtomDatum(v) )
    return PL_register_atom(atom_from_datum(d));
}


static void
unlock_datum(datum d)
{ unsigned long v = (unsigned long)d;

  if ( isAtomDatum(v) )
    return PL_unregister_atom(atom_from_datum(d));
}


static int
cmp_datum(void *p1, void *p2)
{ datum d1 = p1;
  datum d2 = p2;
  int d;

  if ( (d=(tag(d1)-tag(d2))) == 0 )
  { if ( isAtomDatum(d1) )
    { return cmp_atoms(atom_from_datum(d1), atom_from_datum(d2));
    } else
    { long l1 = long_from_datum(d1);
      long l2 = long_from_datum(d2);
      
      return l1 > l2 ? 1 : l1 < l2 ? -1 : 0;
    }
  }

  return d;
}


		 /*******************************
		 *	     ATOM SETS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A set of atoms (literals) is a   sorted  array of atom-handles. They are
sorted simply by handle as we are  not   interested  in the value in the
actual atom.  Search is implemeted as binary search.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define AS_INITIAL_SIZE 4

typedef struct atom_set
{ size_t  size;				/* # cells in use */
  size_t  allocated;			/* # cells allocated */
  datum *atoms;			/* allocated cells */
} atom_set;


static atom_set *
new_atom_set(datum a0)
{ atom_set *as;

  if ( (as = malloc(sizeof(*as))) &&
       (as->atoms = malloc(sizeof(datum)*AS_INITIAL_SIZE)) )
  { as->size = 1;
    as->allocated = AS_INITIAL_SIZE;
    as->atoms[0] = a0;
  }
  
  return as;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
find_in_atom_set(atom_set *as, datum  a)  returns   a  pointer  to  the
location of the atom or, if the atom  isn't there, to the first location
*after* the atom
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static datum *
find_in_atom_set(atom_set *as, datum a)
{ const datum *ap = (const datum *)as->atoms;
  const datum *ep = &ap[as->size];

  for(;;)
  { const datum *cp = ap+(ep-ap)/2;
    
    if ( a < *cp )
    { if ( ep == cp )
	return (datum*)cp;
      ep = cp;
    } else if ( a > *cp )
    { if ( ap == cp )
      { cp++;
	return (datum*)cp;
      }
      ap = cp;
    } else
    { return (datum*)cp;
    }
  }
}


static int
in_atom_set(atom_set *as, datum a)
{ datum *ap = find_in_atom_set(as, a);

  return *ap == a;
}


#define ptr_diff(p1, p2) ((char *)(p1) - (char *)(p2))

static int
insert_atom_set(atom_set *as, datum a)
{ datum *ap = find_in_atom_set(as, a);
  
  if ( *ap != a )
  { lock_datum(a);

    if ( ++as->size > as->allocated )
    { datum *na;
      size_t newsize = as->allocated*2;

      if ( !(na = realloc(as->atoms, sizeof(datum)*newsize)) )
	return FALSE;
      ap += na-as->atoms;
      as->atoms = na;
      as->allocated = newsize;
    }

    memmove(ap+1, ap, ptr_diff(&as->atoms[as->size], ap));
    *ap = a;
  }

  return TRUE;
}


static void
destroy_atom_set(atom_set *as)
{ size_t i;

  for(i=0; i<as->size; i++)
    unlock_datum(as->atoms[i]);

  free(as->atoms);
  free(as);
}


static void
destroy_map_node(avl_node *node)
{ assert(node->value);

  unlock_datum(node->key);
  destroy_atom_set(node->value);
}


		 /*******************************
		 *	   TREE INTERFACE	*
		 *******************************/

static foreign_t
new_atom_map(term_t handle)
{ atom_map *m;

  if ( !(m=malloc(sizeof(*m))) )
    return resource_error("memory");

  init_lock(&m->lock);
  avl_init(&m->tree);
  m->magic = AM_MAGIC;
  m->tree.destroy_node = destroy_map_node;
  m->tree.compare = cmp_datum;

  return unify_atom_map(handle, m);
}


static foreign_t
destroy_atom_map(term_t handle)
{ atom_map *m;

  if ( !get_atom_map(handle, &m) )
    return FALSE;

  m->magic = 0;
  destroy_lock(&m->lock);
  avl_destroy(&m->tree);

  return TRUE;
}


static foreign_t
insert_atom_map(term_t handle, term_t from, term_t to)
{ atom_map *map;
  avl_node *node;
  datum a1, a2;

  if ( !get_atom_map(handle, &map) ||
       !get_datum(from, &a1) ||
       !get_datum(to, &a2) )
    return FALSE;
  
  if ( !WRLOCK(map, FALSE) )
    return FALSE;

  avl_insert_atom(&map->tree, a1, &node);
  if ( !node->value )
  { lock_datum(a1);

    if ( !(node->value = new_atom_set(a2)) )
      return resource_error("memory");
  } else
  { if ( !(insert_atom_set(node->value, a2)) )
      return resource_error("memory");
  }

  WRUNLOCK(map);

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
{ atom_map  *map;
  atom_set  *as[MAX_SETS];		/* TBD */
  int ns = 0;
  term_t tail = PL_copy_term_ref(keys);
  term_t head = PL_new_term_ref();
  atom_set *s0;
  size_t ca;

  if ( !get_atom_map(handle, &map) )
    return FALSE;

  if ( !RDLOCK(map) )
    return FALSE;

  while(PL_get_list(tail, head, tail))
  { datum a;
    avl_node *node;
  
    if ( !get_datum(head, &a) )
      goto failure;
    
    if ( (node = avl_find_node_atom(&map->tree, a)) )
    { if ( ns+1 >= MAX_SETS )
	return resource_error("max_search_atoms");

      as[ns++] = node->value;
    } else
    { goto failure;
    }
  }
  if ( !PL_get_nil(tail) )
  { type_error(tail, "list");
    goto failure;
  }

  qsort(as, ns, sizeof(*as), cmp_atom_set_size);
  s0 = as[0];

  PL_put_term(tail, literals);

  for(ca=0; ca<s0->size; ca++)
  { datum a = s0->atoms[ca];
    int i;

    for(i=1; i<ns; i++)
    { if ( !in_atom_set(as[i], a) ) 
      { if ( a > as[i]->atoms[as[i]->size-1] )
	  goto failure;
	goto next;
      }
    }
    
    if ( !PL_unify_list(tail, head, tail) ||
	 !unify_datum(head, a) )
      goto failure;
next:;
  }

  RDUNLOCK(map);
  return PL_unify_nil(tail);

failure:
  RDUNLOCK(map);
  return FALSE;
}


install_t
install_atom_map()
{ init_functors();
  init_datum_store();

  PL_register_foreign("rdf_new_literal_map",     1, new_atom_map,     0);
  PL_register_foreign("rdf_destroy_literal_map", 1, destroy_atom_map, 0);
  PL_register_foreign("rdf_insert_literal_map",  3, insert_atom_map,  0);
  PL_register_foreign("rdf_find_literal_map",    3, find_atom_map,    0);
}
