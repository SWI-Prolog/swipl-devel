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
#include "avl.h"
#include "lock.h"
#include "atom.h"
#include "debug.h"
#include <string.h>
#include <assert.h>
#ifdef WIN32
#define inline __inline
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file realises the low-level support   for  indexing literals in the
semantic web library. The idea is to   make a map from abstracted tokens
from each literal to  the  exact   literals.  Abstraction  introduces  a
certain amount of ambiguity that  makes   fuzzy  matching possible. Good
abstraction candidates are the Porter Stem  or Snowbal algorithm and the
Double Metaphone algorithm. Both  are  provide   by  the  SWI-Prolog NLP
package.

Basic query provides a  set  of   abstracted  terms  and  requests those
literals containing all of them. We   maintain  ordered sets of literals
and do set-intersection on them to achieve good linear performance.

Some current E-culture project statistics (porter stem)

	  # stems: 0.4 million
	  # literals: 0.9 million
	  # stem->literal relations: 3.1 million

Av. literals/stem: about 8.

Searching is done using

	rdf_find_literal_map(Map, SetOfAbstract, -Literals)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define AM_MAGIC	0x6ab19e8e

typedef struct atom_map
{ long		magic;			/* AM_MAGIC */
  long		value_count;		/* total # values */
  rwlock	lock;			/* Multi-threaded access */
  avl_tree	tree;			/* AVL tree */
} atom_map;

typedef void *datum;

#define S_MAGIC 0x8734abcd

typedef struct atom_set
{ size_t  size;				/* # cells in use */
  size_t  allocated;			/* # cells allocated */
  datum *atoms;			/* allocated cells */
#ifdef O_SECURE
  long	  magic;
#endif
} atom_set;


#define ND_MAGIC 0x67b49a23

typedef struct node_data
{ datum		key;
  atom_set     *values;
#ifdef O_SECURE
  long		magic;
#endif
} node_data;


#define RDLOCK(map)			rdlock(&map->lock)
#define WRLOCK(map, allowreaders)	wrlock(&map->lock, allowreaders)
#define LOCKOUT_READERS(map)		lockout_readers(&map->lock)
#define REALLOW_READERS(map)		reallow_readers(&map->lock)
#define WRUNLOCK(map)			unlock(&map->lock, FALSE)
#define RDUNLOCK(map)			unlock(&map->lock, TRUE)

		 /*******************************
		 *	     BASIC STUFF	*
		 *******************************/

static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_type_error2;
static functor_t FUNCTOR_domain_error2;
static functor_t FUNCTOR_resource_error1;
static functor_t FUNCTOR_atom_map1;
static functor_t FUNCTOR_size2;
static functor_t FUNCTOR_not1;
static atom_t	 ATOM_all;
static atom_t	 ATOM_case;
static atom_t	 ATOM_prefix;
static atom_t	 ATOM_le;
static atom_t	 ATOM_ge;
static atom_t	 ATOM_between;
static atom_t	 ATOM_key;

#define MKFUNCTOR(n,a) \
	FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)
#define MKATOM(n) \
	ATOM_ ## n = PL_new_atom(#n)

static void
init_functors()
{ FUNCTOR_atom_map1 = PL_new_functor(PL_new_atom("$literal_map"), 1);

  MKFUNCTOR(error, 2);
  MKFUNCTOR(type_error, 2);
  MKFUNCTOR(domain_error, 2);
  MKFUNCTOR(resource_error, 1);
  MKFUNCTOR(size, 2);
  MKFUNCTOR(not, 1);

  MKATOM(all);
  MKATOM(case);
  MKATOM(prefix);
  MKATOM(le);
  MKATOM(ge);
  MKATOM(between);
  MKATOM(key);
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
domain_error(term_t actual, const char *expected)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR, FUNCTOR_domain_error2,
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
representation_error(const char *what)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_FUNCTOR_CHARS, "representation_error", 1,
		        PL_CHARS, what,
		      PL_VARIABLE);

  return PL_raise_exception(ex);
}



static int
get_atom_ex(term_t t, atom_t *a)
{ if ( PL_get_atom(t, a) )
    return TRUE;

  return type_error(t, "atom");
}


static int
get_long_ex(term_t t, long *v)
{ if ( PL_get_long(t, v) )
    return TRUE;

  return type_error(t, "integer");
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

#define ATOM_TAG_BITS 7
#define ATOM_TAG 0x1

#define tag(d)		((long)(d)&0x1)
#define isAtomDatum(d)  ((long)(d)&ATOM_TAG)
#define isIntDatum(d)	!isAtomDatum(d)

#define MAP_MIN_INT	(-(long)(1L<<(sizeof(long)*8 - 1 - 1)))
#define MAP_MAX_INT	(-MAP_MIN_INT - 1)

static int atom_mask;

static void
init_datum_store()
{ atom_t a = PL_new_atom("[]");
  
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
{ long v = (long)d;

  return (v>>1);
}


static inline datum
atom_to_datum(atom_t a)
{ unsigned long v = (a>>(ATOM_TAG_BITS-1))|ATOM_TAG;

  SECURE(assert(atom_from_datum((datum)v) == a));
  DEBUG(9, Sdprintf("Atom %s --> 0x%lx\n", PL_atom_chars(a), v));

  return (datum)v;
}


static inline datum
long_to_datum(long v)
{ return (datum)(v<<1);
}


static int
get_datum(term_t t, datum* d)
{ atom_t a;
  long l;

  if ( PL_get_atom(t, &a) )
  { *d = atom_to_datum(a);
    return TRUE;
  } else if ( PL_get_long(t, &l) )
  { if ( l < MAP_MIN_INT || l > MAP_MAX_INT )
      return representation_error("integer_range");

    *d = long_to_datum(l);			/* TBD: verify range */
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
    PL_register_atom(atom_from_datum(d));
}


static void
unlock_datum(datum d)
{ unsigned long v = (unsigned long)d;

  if ( isAtomDatum(v) )
    PL_unregister_atom(atom_from_datum(d));
}


static inline int
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
      
      DEBUG(2, Sdprintf("cmp_datum(%ld, %ld)\n", l1, l2));

      return l1 > l2 ? 1 : l1 < l2 ? -1 : 0;
    }
  }

  return d;
}


static const char *
format_datum(datum d, char *buf)
{ static char tmp[20];

  if ( isAtomDatum(d) )
    return PL_atom_chars(atom_from_datum(d));

  if ( !buf )
    buf = tmp;
  Ssprintf(buf, "%ld", long_from_datum(d));

  return buf;
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

static atom_set *
new_atom_set(datum a0)
{ atom_set *as;

  if ( (as = malloc(sizeof(*as))) &&
       (as->atoms = malloc(sizeof(datum)*AS_INITIAL_SIZE)) )
  { as->size = 1;
    as->allocated = AS_INITIAL_SIZE;
    as->atoms[0] = a0;
    SECURE(as->magic = S_MAGIC);
  }
  
  return as;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
find_in_atom_set(atom_set *as, datum  a)  returns   a  pointer  to  the
location of the atom or, if the atom  isn't there, to the first location
*after* the atom
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static datum *
find_in_atom_set(atom_set *as, datum a, int *found)
{ const datum *ap = (const datum *)as->atoms;
  const datum *ep = &ap[as->size];

  SECURE(assert(as->magic == S_MAGIC));

  for(;;)
  { const datum *cp = ap+(ep-ap)/2;
    
    if ( a < *cp )
    { if ( ep == cp )
      { *found = FALSE;
	return (datum*)cp;
      }
      ep = cp;
    } else if ( a > *cp )
    { if ( ap == cp )
      { cp++;
	*found = FALSE;
	return (datum*)cp;
      }
      ap = cp;
    } else
    { *found = TRUE;
      return (datum*)cp;
    }
  }
}


static int
in_atom_set(atom_set *as, datum a)
{ int found;

  find_in_atom_set(as, a, &found);

  return found;
}


#define ptr_diff(p1, p2) ((char *)(p1) - (char *)(p2))

static int
insert_atom_set(atom_set *as, datum a)
{ int found;
  datum *ap = find_in_atom_set(as, a, &found);
  
  if ( !found )
  { lock_datum(a);

    if ( as->size == as->allocated )
    { datum *na;
      size_t newsize = as->allocated*2;

      if ( !(na = realloc(as->atoms, sizeof(datum)*newsize)) )
	return -1;
      ap += na-as->atoms;
      as->atoms = na;
      as->allocated = newsize;
    }
    assert(as->size < as->allocated);

    memmove(ap+1, ap, ptr_diff(&as->atoms[as->size], ap));
    as->size++;
    *ap = a;

    return 1;
  }

  return 0;
}


static int
delete_atom_set(atom_set *as, datum a)
{ int found;
  datum *ap = find_in_atom_set(as, a, &found);

  if ( found )
  { unlock_datum(a);
    as->size--;
    memmove(ap, ap+1, ptr_diff(&as->atoms[as->size], ap));
  }

  return found;
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
free_node_data(void *ptr)
{ node_data *data = ptr;

  DEBUG(2,
	char b[20];
	Sdprintf("Destroying node with key = %s\n",
		 format_datum(data->key, b)));

  unlock_datum(data->key);
  destroy_atom_set(data->values);
}


		 /*******************************
		 *	   TREE INTERFACE	*
		 *******************************/

static int
cmp_node_data(void *l, void *r, NODE type)
{ node_data *d1 = l;
  node_data *d2 = r;

  return cmp_datum(d1->key, d2->key);
}


static void
init_tree_map(atom_map *m)
{ avlinit(&m->tree,
	  NULL, sizeof(node_data),
	  cmp_node_data,
	  free_node_data,		/* destroy */
	  NULL,				/* alloc */
	  NULL);			/* free */
}


static foreign_t
new_atom_map(term_t handle)
{ atom_map *m;

  if ( !(m=malloc(sizeof(*m))) )
    return resource_error("memory");

  memset(m, 0, sizeof(*m));
  init_lock(&m->lock);
  init_tree_map(m);
  m->magic = AM_MAGIC;

  return unify_atom_map(handle, m);
}


static foreign_t
destroy_atom_map(term_t handle)
{ atom_map *m;

  if ( !get_atom_map(handle, &m) )
    return FALSE;

  WRLOCK(m, FALSE);
  avlfree(&m->tree);
  m->magic = 0;
  WRUNLOCK(m);
  destroy_lock(&m->lock);
  free(m);

  return TRUE;
}


		 /*******************************
		 *	       INSERT		*
		 *******************************/


static foreign_t
insert_atom_map(term_t handle, term_t from, term_t to)
{ atom_map *map;
  datum a2;
  node_data search, *data;


  if ( !get_atom_map(handle, &map) ||
       !get_datum(from, &search.key) ||
       !get_datum(to, &a2) )
    return FALSE;
  
  if ( !WRLOCK(map, FALSE) )
    return FALSE;

  search.values = NULL;
  if ( (data=avlfind(&map->tree, &search)) )
  { int rc;

    SECURE(assert(data->magic == ND_MAGIC));

    if ( (rc=insert_atom_set(data->values, a2)) < 0 )
      return resource_error("memory");

    if ( rc )
      map->value_count++;
  } else
  { if ( !(search.values = new_atom_set(a2)) )
      return resource_error("memory");
    lock_datum(search.key);
    SECURE(search.magic = ND_MAGIC);

    data = avlins(&map->tree, &search);
    assert(!data);
    map->value_count++;
  }

  WRUNLOCK(map);

  return TRUE;
}


		 /*******************************
		 *	       DELETE		*
		 *******************************/

static foreign_t
delete_atom_map2(term_t handle, term_t from)
{ atom_map *map;
  node_data search;
  node_data *data;

  if ( !get_atom_map(handle, &map) ||
       !get_datum(from, &search.key) )
    return FALSE;
  
  if ( !WRLOCK(map, TRUE) )
    return FALSE;

  search.values = NULL;
  if ( (data = avlfind(&map->tree, &search)) )
  { LOCKOUT_READERS(map);
    map->value_count -= data->values->size;
    avldel(&map->tree, data);
    REALLOW_READERS(map);
  }

  WRUNLOCK(map);

  return TRUE;
}


static foreign_t
delete_atom_map3(term_t handle, term_t from, term_t to)
{ atom_map *map;
  node_data *data, search;
  datum a2;

  if ( !get_atom_map(handle, &map) ||
       !get_datum(from, &search.key) ||
       !get_datum(to, &a2) )
    return FALSE;
  
  if ( !WRLOCK(map, TRUE) )
    return FALSE;

  if ( (data = avlfind(&map->tree, &search)) &&
       in_atom_set(data->values, a2) )
  { atom_set *as = data->values;

    LOCKOUT_READERS(map);
    if ( delete_atom_set(as, a2) )
    { map->value_count--;
      if ( as->size == 0 )
      { avldel(&map->tree, data);
      }
    }
    REALLOW_READERS(map);
  }

  WRUNLOCK(map);

  return TRUE;
}


		 /*******************************
		 *	      SEARCH		*
		 *******************************/

typedef struct
{ atom_set *set;
  int      neg;				/* not(Lit) */
} pn_set;


static int
cmp_atom_set_size(const void *p1, const void *p2)
{ const pn_set *ap1 = p1;
  const pn_set *ap2 = p2;

  if ( ap1->neg != ap2->neg )
    return ap1->neg ? 1 : -1;		/* all negatives at the end */

  return ap1->set->size - ap2->set->size;
}


#define MAX_SETS 100

static foreign_t
find_atom_map(term_t handle, term_t keys, term_t literals)
{ atom_map  *map;
  pn_set    as[MAX_SETS];		/* TBD */
  int ns = 0;
  term_t tmp = PL_new_term_ref();
  term_t tail = PL_copy_term_ref(keys);
  term_t head = PL_new_term_ref();
  atom_set *s0;
  size_t ca;

  if ( !get_atom_map(handle, &map) )
    return FALSE;

  if ( !RDLOCK(map) )
    return FALSE;

  while(PL_get_list(tail, head, tail))
  { node_data *data, search;
    int neg = FALSE;

    if ( PL_is_functor(head, FUNCTOR_not1) )
    { PL_get_arg(1, head, tmp);
      if ( !get_datum(tmp, &search.key) )
	goto failure;
      neg = TRUE;
    } else
    { if ( !get_datum(head, &search.key) )
	goto failure;
    }
    
    if ( (data = avlfind(&map->tree, &search)) )
    { if ( ns+1 >= MAX_SETS )
	return resource_error("max_search_atoms");

      as[ns].set = data->values;
      as[ns].neg = neg;
      DEBUG(2, Sdprintf("Found atom-set of size %d\n", as[ns].set->size));
      ns++;
    } else if ( !neg )
    { goto empty;
    }
  }
  if ( !PL_get_nil(tail) )
  { type_error(tail, "list");
    goto failure;
  }

  qsort(as, ns, sizeof(*as), cmp_atom_set_size);
  if ( ns==0 || as[0].neg )
  { domain_error(keys, "keywords");
    goto failure;
  }
    
  s0 = as[0].set;

  PL_put_term(tail, literals);

  for(ca=0; ca<s0->size; ca++)
  { datum a = s0->atoms[ca];
    int i;

    for(i=1; i<ns; i++)
    { if ( !as[i].neg )
      { if ( !in_atom_set(as[i].set, a) ) 
	{ if ( a > as[i].set->atoms[as[i].set->size-1] )
	    goto empty;
	  goto next;
	}
      } else
      { if ( in_atom_set(as[i].set, a) )
	  goto next;
      }
    }
    
    if ( !PL_unify_list(tail, head, tail) ||
	 !unify_datum(head, a) )
      goto failure;
next:;
  }

empty:
  RDUNLOCK(map);
  return PL_unify_nil(tail);

failure:
  RDUNLOCK(map);
  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rdf_keys_in_literal_map(+Map, +Spec, -Keys)

Spec is one of

	* all
	* prefix(Text)			atoms only
	* ge(Low)			integers only
	* le(High)
	* between(Low, High)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* TBD: should use avlwalk(), but this isn't fine as it does not allow a
   failure return and does not allow passing our term-handles
*/

static int unify_keys(term_t head, term_t tail, AVLnode *node)
{ node_data *data;

  if ( node->subtree[LEFT] )
  { if ( !unify_keys(head, tail, node->subtree[LEFT]) )
      return FALSE;
  }
  
  data = (node_data*)node->data;
  if ( !PL_unify_list(tail, head, tail) ||
       !unify_datum(head, data->key) )
    return FALSE;

  if ( node->subtree[RIGHT] )
    return unify_keys(head, tail, node->subtree[RIGHT]);

  return TRUE;
}


static int
between_keys(atom_map *map, long min, long max, term_t head, term_t tail) 
{ avl_enum state;
  node_data *data, search;

  DEBUG(2, Sdprintf("between %ld .. %ld\n", min, max));

  search.key = long_to_datum(min);
  search.values = NULL;
  if ( (data = avlfindfirst(&map->tree, &search, &state)) &&
       isIntDatum(data->key) )
  { for(;;)
    { if ( long_from_datum(data->key) > max )
	break;

      if ( !PL_unify_list(tail, head, tail) ||
	   !unify_datum(head, data->key) )
      { avlfinddestroy(&state);
	return FALSE;
      }

      if ( !(data = avlfindnext(&state)) ||
	   !isIntDatum(data->key) )
	break;
    }

    avlfinddestroy(&state);
  }

  return TRUE;
}


static foreign_t
rdf_keys_in_literal_map(term_t handle, term_t spec, term_t keys)
{ atom_map *map;
  term_t tail = PL_copy_term_ref(keys);
  term_t head = PL_new_term_ref();
  atom_t name;
  int arity;

  if ( !get_atom_map(handle, &map) )
    return FALSE;

  if ( !RDLOCK(map) )
    return FALSE;

  if ( !PL_get_name_arity(spec, &name, &arity) )
    type_error(spec, "key-specifier");

  if ( name == ATOM_all )
  { AVLnode *node = map->tree.root;

    if ( !unify_keys(head, tail, node) )
      goto failure;
  } else if ( name == ATOM_key && arity == 1 )
  { term_t a = PL_new_term_ref();
    node_data *data, search;

    PL_get_arg(1, spec, a);
    if ( !get_datum(a, &search.key) )
      goto failure;

    search.values = NULL;
    if ( (data = avlfind(&map->tree, &search)) )
    { int size = data->values->size;

      RDUNLOCK(map);
      assert(size > 0);

      return PL_unify_integer(keys, size);
    }
    goto failure;
  } else if ( (name == ATOM_prefix || name == ATOM_case) && arity == 1 )
  { term_t a = PL_new_term_ref();
    atom_t prefix, first_a;
    avl_enum state;
    node_data *data, search;
    int match = (name == ATOM_prefix ? STR_MATCH_PREFIX : STR_MATCH_EXACT);

    PL_get_arg(1, spec, a);
    if ( !get_atom_ex(a, &prefix) )
      goto failure;
    first_a = first_atom(prefix, STR_MATCH_PREFIX);

    search.key = atom_to_datum(first_a);
    search.values = NULL;

    for(data = avlfindfirst(&map->tree, &search, &state);
	data;
	data=avlfindnext(&state))
    { assert(isAtomDatum(data->key));

      if ( !match_atoms(match,
			first_a, atom_from_datum(data->key)) )
	break;

      if ( !PL_unify_list(tail, head, tail) ||
	   !unify_datum(head, data->key) )
      { avlfinddestroy(&state);
	goto failure;
      }
    }
    avlfinddestroy(&state);
  } else if ( (name == ATOM_ge || name == ATOM_le) && arity == 1 )
  { term_t a = PL_new_term_ref();
    long val, min, max;

    PL_get_arg(1, spec, a);
    if ( !get_long_ex(a, &val) )
      goto failure;

    if ( name == ATOM_ge )
      min = val, max = MAP_MAX_INT;
    else
      max = val, min = MAP_MIN_INT;
    
    if ( !between_keys(map, min, max, head, tail) )
      goto failure;
  } else if ( name == ATOM_between && arity == 2 )
  { term_t a = PL_new_term_ref();
    long min, max;

    PL_get_arg(1, spec, a);
    if ( !get_long_ex(a, &min) )
      goto failure;
    PL_get_arg(2, spec, a);
    if ( !get_long_ex(a, &max) )
      goto failure;

    if ( !between_keys(map, min, max, head, tail) )
      goto failure;
  } else
  { type_error(spec, "key-specifier");
    goto failure;
  }

  RDUNLOCK(map);

  return PL_unify_nil(tail);

failure:
  RDUNLOCK(map);
  return FALSE;
}


		 /*******************************
		 *	      RESET		*
		 *******************************/

static foreign_t
rdf_reset_literal_map(term_t handle)
{ atom_map *map;

  if ( !get_atom_map(handle, &map) )
    return FALSE;

  if ( !WRLOCK(map, FALSE) )
    return FALSE;
  avlfree(&map->tree);
  init_tree_map(map);
  map->value_count = 0;
  WRUNLOCK(map);

  return TRUE;
}



		 /*******************************
		 *	    STATISTICS		*
		 *******************************/


term_t
rdf_statistics_literal_map(term_t map, term_t key)
{ atom_map *m;

  if ( !get_atom_map(map, &m) )
    return FALSE;
  
  if ( PL_is_functor(key, FUNCTOR_size2) )
  { term_t a = PL_new_term_ref();

    PL_get_arg(1, key, a);
    if ( !PL_unify_integer(a, m->tree.count) )
      return FALSE;
    PL_get_arg(2, key, a);

    return PL_unify_integer(a, m->value_count);
  }

  return type_error(key, "statistics_key");
}




		 /*******************************
		 *	     REGISTER		*
		 *******************************/

#define PRED(n,a,f,o) PL_register_foreign(n,a,f,o)

install_t
install_atom_map()
{ init_functors();
  init_datum_store();

  PRED("rdf_new_literal_map",	     1,	new_atom_map,		    0);
  PRED("rdf_destroy_literal_map",    1,	destroy_atom_map,	    0);
  PRED("rdf_reset_literal_map",	     1, rdf_reset_literal_map,	    0);
  PRED("rdf_insert_literal_map",     3,	insert_atom_map,	    0);
  PRED("rdf_delete_literal_map",     3,	delete_atom_map3,	    0);
  PRED("rdf_delete_literal_map",     2,	delete_atom_map2,	    0);
  PRED("rdf_find_literal_map",	     3,	find_atom_map,		    0);
  PRED("rdf_keys_in_literal_map",    3,	rdf_keys_in_literal_map,    0);
  PRED("rdf_statistics_literal_map", 2,	rdf_statistics_literal_map, 0);
}
