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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include "rdf_db.h"
#include <assert.h>
#include <string.h>
#include <ctype.h>

#ifdef _REENTRANT
#include <pthread.h>

static pthread_mutex_t rdf_db_mutex = PTHREAD_MUTEX_INITIALIZER;
#define LOCK() pthread_mutex_lock(&rdf_db_mutex)
#define UNLOCK() pthread_mutex_unlock(&rdf_db_mutex)
#else
#define LOCK()
#define UNLOCK()
#endif

#define O_DEBUG 1

#ifdef O_DEBUG
static int debuglevel = 0;
#define DEBUG(n, g) if ( debuglevel >= (n) ) { g; }
#else
#define DEBUG(n, g) ((void)0);
#endif

#ifdef DIRECT_MALLOC
#define PL_malloc malloc
#define PL_free free
#define PL_realloc realloc
#endif

static functor_t FUNCTOR_literal1;
static functor_t FUNCTOR_literal2;
static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_type_error2;
static functor_t FUNCTOR_domain_error2;
static functor_t FUNCTOR_colon2;

static functor_t FUNCTOR_triples1;
static functor_t FUNCTOR_subjects1;
static functor_t FUNCTOR_predicates1;
static functor_t FUNCTOR_duplicates1;

static functor_t FUNCTOR_subject1;
static functor_t FUNCTOR_predicate1;
static functor_t FUNCTOR_object1;
static functor_t FUNCTOR_source1;
static functor_t FUNCTOR_indexed8;

static functor_t FUNCTOR_exact1;
static functor_t FUNCTOR_substring1;
static functor_t FUNCTOR_word1;
static functor_t FUNCTOR_prefix1;

static functor_t FUNCTOR_searched_nodes1;

static atom_t   ATOM_user;
static atom_t	ATOM_exact;
static atom_t	ATOM_prefix;
static atom_t	ATOM_substring;
static atom_t	ATOM_word;

static atom_t	ATOM_subPropertyOf;

#define MATCH_EXACT 		0x1	/* exact triple match */
#define MATCH_SUBPROPERTY	0x2	/* Use subPropertyOf relations */
#define MATCH_SRC		0x4	/* Match source location */

static int match(int how, atom_t search, atom_t label);
static int update_duplicates_add(triple *t);
static void update_duplicates_del(triple *t);

		 /*******************************
		 *	       ERRORS		*
		 *******************************/

static int
instantiation_error(term_t actual)
{ term_t ex = PL_new_term_ref();

  PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
		      PL_CHARS, "instantiation_error",
		      PL_VARIABLE);

  return PL_raise_exception(ex);
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
get_atom_or_var_ex(term_t t, atom_t *a)
{ if ( PL_get_atom(t, a) )
    return TRUE;
  if ( PL_is_variable(t) )
  { *a = 0L;
    return TRUE;
  }

  return type_error(t, "atom");
}

		 /*******************************
		 *	   DEBUG SUPPORT	*
		 *******************************/

#ifdef O_DEBUG

static void
print_triple_source(triple *t)
{ if ( t->line == NO_LINE )
    Sdprintf("[%s]: ", PL_atom_chars(t->source));
  else
    Sdprintf("%s:%ld: ", PL_atom_chars(t->source), t->line);
}
  
static void
print_triple(triple *t)
{ char *sep = t->objtype == OBJ_RESOURCE ? "" : "\"";

  Sdprintf("<%s %s %s%s%s>",
	   PL_atom_chars(t->subject),
	   PL_atom_chars(t->predicate->name),
	   sep, PL_atom_chars(t->object), sep);
}

#endif

		 /*******************************
		 *	     STORAGE		*
		 *******************************/

static triple  *by_none, *by_none_tail;
static triple **table[INDEX_TABLES];
static triple **tail[INDEX_TABLES];
static int	table_size[INDEX_TABLES];
static long	created;		/* #triples created */
static long	erased;			/* #triples erased */
static long	subjects;		/* subjects (unique first) */
static long	indexed[8];		/* Count calls */
static predicate **pred_table;		/* Hash-table of predicates */
static int	pred_table_size;	/* #entries in the table */
static int	pred_count;		/* #predicates */
static int	active_queries;		/* Calls with choicepoints */
static int	need_update;		/* We need to update */
static long	agenda_created;		/* #visited nodes in agenda */
static long	duplicates;		/* #duplicate triples */
static long	generation;		/* generation-id of the database */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SWI-Prolog note: Atoms are integers shifted by LMASK_BITS (7)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define atom_hash(a) (((unsigned long)a)>>7)


		 /*******************************
		 *	      LISTS		*
		 *******************************/

static int
add_list(list *list, void *value)
{ cell *c;

  for(c=list->head; c; c=c->next)
  { if ( c->value == value )
      return FALSE;			/* already a member */
  }

  c = PL_malloc(sizeof(*c));
  c->value = value;
  c->next = NULL;

  if ( list->tail )
    list->tail->next = c;
  else
    list->head = c;

  list->tail = c;

  return TRUE;
}


static int
del_list(list *list, void *value)
{ cell *c, *p = NULL;

  for(c=list->head; c; p=c, c=c->next)
  { if ( c->value == value )
    { if ( p )
	p->next = c->next;
      else
	list->head = c->next;

      if ( !c->next )
	list->tail = p;

      PL_free(c);

      return TRUE;
    }
  }

  return FALSE;				/* not a member */
}


static void
free_list(list *list)
{ cell *c, *n;

  for(c=list->head; c; c=n)
  { n = c->next;
    PL_free(c);
  }

  list->head = list->tail = NULL;
}


		 /*******************************
		 *	    PREDICATES		*
		 *******************************/

static void
init_pred_table()
{ int bytes = sizeof(predicate*)*INITIAL_PREDICATE_TABLE_SIZE;

  pred_table = PL_malloc(bytes);
  memset(pred_table, 0, bytes);
  pred_table_size = INITIAL_PREDICATE_TABLE_SIZE;
}


static predicate *
lookup_predicate(atom_t name)
{ int hash = atom_hash(name) % pred_table_size;
  predicate *p;

  for(p=pred_table[hash]; p; p = p->next)
  { if ( p->name == name )
      return p;
  }
  p = PL_malloc(sizeof(*p));
  memset(p, 0, sizeof(*p));
  p->name = name;
  p->root = p;
  p->oldroot = NULL;
  PL_register_atom(name);
  p->next = pred_table[hash];
  pred_table[hash] = p;
  pred_count++;

  return p;
}


static predicate *
alloc_dummy_root_predicate()
{ predicate *p;

  p = PL_malloc(sizeof(*p));
  memset(p, 0, sizeof(*p));
  p->name = 0;				/* dummy roots have no name */
  p->root = p;
  p->oldroot = NULL;

  return p;
}

#if 0
/* We do not yet have a safe mechanism to avoid multiple destruction.  As
   it is anticipated to be very infrequent we'll forget about freeing
   predicates for the time being.
*/

static void				/* currently only frees dummy root */
free_predicate(predicate *p) { assert(!p->name);

  free_list(&p->siblings);
  free_list(&p->subPropertyOf);
  PL_free(p);
}
#endif


static __inline int
is_dummy_root(predicate *p)
{ return p && !p->name;			/* no name --> dummy root */
}


static __inline int
is_virgin_dummy_root(predicate *p)
{ return is_dummy_root(p) && !p->siblings.head;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Change the root if we put a new root   on  top of a hierarchy. We should
*not* assign to predicates that already have a root!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void				/* make root the root of p */
set_dummy_root_r(predicate *p, predicate *root)
{ if ( p->root && p->root != root )
  { cell *c;

    if ( p->root && p->root != (predicate*)1 && is_dummy_root(p->root) )
      del_list(&p->root->siblings, p);
    p->root = root;
    add_list(&root->siblings, p);

    for(c=p->siblings.head; c; c=c->next)
    { set_dummy_root_r(c->value, root);
    }
  }
}


static void				/* make root the root of p */
set_dummy_root(predicate *p, predicate *root)
{ p->root = (predicate*)1;		/* not-null */
  set_dummy_root_r(p, root);
}



static const char *
pname(predicate *p)
{ if ( p->name )
    return PL_atom_chars(p->name);
  else
  { static char *ring[10];
    static int ri = 0;
    char buf[25];
    char *r;

    Ssprintf(buf, "__D%p", p);
    ring[ri++] = r = strdup(buf);
    if ( ri == 10 )
    { ri = 0;
      free(ring[ri]);
    }

    return (const char*)r;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
predicate *root_predicate(predicate *p)

Find the root of a  predicate.  This   function  finds  the one and only
origin of the sub-property graph, dealing   with possible cycles. If the
graph has multiple roots it constructs an artificial parent of all roots
and returns this parent.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static predicate*
root_predicate(predicate *p0, int vindex)
{ predicate *p = p0;

  DEBUG(3, Sdprintf("root_predicate(%s) ...", pname(p)));

  for(;;)
  { cell *c;

    if ( p->root )
    { DEBUG(3, Sdprintf("%s (old)\n", pname(p->root)));
      return p->root;
    }
    if ( p->visited == vindex )
    { DEBUG(3, Sdprintf("%s (cycle)\n", pname(p)));
      return p;				/* cycle */
    }

    p->visited = vindex;
    if ( !p->subPropertyOf.head )
    { DEBUG(3, Sdprintf("%s (root)\n", pname(p)));
      return p;				/* no super */
    }
    if ( !p->subPropertyOf.head->next )
      p = p->subPropertyOf.head->value;	/* exactly one super */
    else				/* multiple supers */
    { predicate *root = root_predicate(p->subPropertyOf.head->value, vindex);
      
      DEBUG(2, Sdprintf("%s has multiple roots\n", pname(p)));

      for(c=p->subPropertyOf.head->next; c; c=c->next)
      { predicate *r2 = root_predicate(c->value, vindex);

	if ( r2 != root )		/* multiple roots */
	{ if ( is_dummy_root(root) )
	  { if ( is_dummy_root(r2) )
	    { cell *c;

	      for(c=r2->siblings.head; c; c=c->next)
		set_dummy_root(c->value, root);
#if 0
	      if ( !r2->siblings.head )
		free_predicate(r2);
#endif
	    } else
	    { set_dummy_root(r2, root);
	    }
	  } else if ( is_dummy_root(r2) )
	  { set_dummy_root(root, r2);
	    root = r2;
	  } else
	  { predicate *nr;

	    if ( is_virgin_dummy_root(root->oldroot) )
	      nr = root->oldroot;
	    else if ( is_virgin_dummy_root(r2->oldroot) )
	      nr = r2->oldroot;
	    else
	      nr = alloc_dummy_root_predicate();

	    set_dummy_root(root, nr);
	    set_dummy_root(r2, nr);

	    DEBUG(1, Sdprintf("New virtual root %s for %s and %s\n",
			      pname(nr),
			      pname(root),
			      pname(r2)));
	    root = nr;
	  }
	}
      }

      return root;
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int organise_predicates()

Assign each predicate its root and return the number of predicates who's
root has changed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
organise_predicates()
{ predicate **ht;
  int i;
  int changed = 0;
  int seen = 0;

  for(i=0,ht = pred_table; i<pred_table_size; i++, ht++)
  { predicate *p;

    for( p = *ht; p; p = p->next )
    { p->oldroot = p->root;
      if ( is_dummy_root(p->oldroot) )
	free_list(&p->oldroot->siblings);
      p->root = NULL;
      p->visited = -1;
    }
  }

  for(i=0,ht = pred_table; i<pred_table_size; i++, ht++)
  { predicate *p;

    for( p = *ht; p; p = p->next )
    { predicate *root = root_predicate(p, seen);

      p->root = root;
      seen++;
    }
  }

  for(i=0,ht = pred_table; i<pred_table_size; i++, ht++)
  { predicate *p;

    for( p = *ht; p; p = p->next )
    { if ( p->oldroot != p->root )
	changed++;
#if 0					/* may be referenced multiple */
      if ( is_virgin_dummy_root(p->oldroot) )
	free_predicate(p->oldroot);	/* has not been reused: discard */
#endif
      p->oldroot = NULL;
      DEBUG(1,
	    if ( p->root != p )
	    { Sdprintf("Root of %s = %s\n", pname(p), pname(p->root));
	    })
    }
  }

  assert(seen == pred_count);

  return changed;
}


static unsigned long
predicate_hash(predicate *p)
{ return (unsigned long)p >> 2;
}


static void
addSubPropertyOf(predicate *sub, predicate *super)
{ if ( add_list(&sub->subPropertyOf, super) )
  { add_list(&super->siblings, sub);
    need_update++;
  }
}


static void
delSubPropertyOf(predicate *sub, predicate *super)
{ if ( del_list(&sub->subPropertyOf, super) )
  { del_list(&super->siblings, sub);
    need_update++;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
See whether sub is p or a subproperty   of  p. This predicate deals with
cycles and multiple parents. A cycle is detected if the we pass the root
(which has p->root == p) for the  second time without finding the target
predicate.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
isSubPropertyOf(predicate *sub, predicate *p)
{ int seen_root = 0;

  DEBUG(2, Sdprintf("isSubPropertyOf(%s, %s)\n",
		    PL_atom_chars(sub->name),
		    PL_atom_chars(p->name)));

  if ( sub->root == p )
    return TRUE;

  for(;;)
  { cell *c;

    if ( sub == p )
      return TRUE;

    for(c=sub->subPropertyOf.head; c; c=c->next)
    { if ( !c->next )			/* tail-recursion optimisation */
      { sub = c->value;
	goto next;
      }

      if ( isSubPropertyOf(c->value, p) )
	return TRUE;
    }
    return FALSE;

  next:
    DEBUG(2, Sdprintf("Trying %s%s\n",
		      PL_atom_chars(sub->name),
		      sub->root == sub ? " (root)" : ""));

    if ( sub->root == sub && ++seen_root == 2 )
      return FALSE;			/* made a full turn */
  }
}


		 /*******************************
		 *	      TRIPLES		*
		 *******************************/

static void
init_tables()
{ int i;
  int bytes = sizeof(triple*)*INITIAL_TABLE_SIZE;

  table[0] = &by_none;
  tail[0]  = &by_none_tail;

  for(i=BY_S; i<=BY_OP; i++)
  { if ( i == BY_SO )
      continue;

    table[i] = PL_malloc(bytes);
    memset(table[i], 0, bytes);
    tail[i] = PL_malloc(bytes);
    memset(tail[i], 0, bytes);
    table_size[i] = INITIAL_TABLE_SIZE;
  }

  init_pred_table();
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Save a triple in the permanent heap.  Right   now  this is only used for
backtracking context. There is no need to register the atoms here as the
Prolog backtracking context references them.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static triple *
save_triple(triple *t)
{ triple *copy = PL_malloc(sizeof(*copy));

  *copy = *t;

  return copy;
}


static unsigned long
string_hash(const char *t, unsigned int len)
{ unsigned int value = 0;
  unsigned int shift = 5;

  while(len-- != 0)
  { unsigned int c = *t++;
    
    c = tolower(c);			/* case insensitive */
    c -= 'a';
    value ^= c << (shift & 0xf);
    shift ^= c;
  }

  return value ^ (value >> 16);
}


static unsigned long
case_insensitive_atom_hash(atom_t a)
{ const char *s;
  unsigned len;

  s = PL_atom_nchars(a, &len);

  return string_hash(s, len);
}


static unsigned long
object_hash(triple *t)
{ switch(t->objtype)
  { case OBJ_RESOURCE:
      return t->object;
    case OBJ_LITERAL:
      return case_insensitive_atom_hash(t->object);
    default:
      assert(0);
      return 0;
  }
}


static int
triple_hash(triple *t, int which)
{ unsigned long v;

  switch(which)
  { case BY_NONE:
      return 0;
    case BY_S:
      v = atom_hash(t->subject);
      break;
    case BY_P:
      v = predicate_hash(t->predicate);
      break;
    case BY_O:
      v = atom_hash(object_hash(t));
      break;
    case BY_SP:
      v = atom_hash(t->subject) ^ predicate_hash(t->predicate->root);
      break;
    case BY_OP:
      v = predicate_hash(t->predicate->root) ^ object_hash(t);
      break;
    default:
      assert(0);
  }

  return (int)(v % (long)table_size[which]);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
triple *first(atom_t subject)
    Find the first triple on subject.  The first is marked to generate a
    unique subjects quickly;
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static triple *
first(atom_t subject)
{ triple *t, tmp;
  int hash;

  tmp.subject = subject;
  hash = triple_hash(&tmp, BY_S);
  
  for(t=table[BY_S][hash]; t; t = t->next[BY_S])
  { if ( t->subject == subject && !t->erased )
      return t;
  }

  return NULL;
}


static void
link_triple_hash(triple *t)
{ int i;

  for(i=1; i<=BY_OP; i++)
  { if ( table[i] )
    { int hash = triple_hash(t, i);

      if ( tail[i][hash] )
      { tail[i][hash]->next[i] = t;
      } else
      { table[i][hash] = t;
      }
      tail[i][hash] = t;
    }
  }
}


/* MT: must be locked by caller */

static void
link_triple(triple *t)
{ triple *one;

  if ( by_none_tail )
    by_none_tail->next[BY_NONE] = t;
  else
    by_none = t;
  by_none_tail = t;

  link_triple_hash(t);

  if ( update_duplicates_add(t)	)
    goto ok;				/* is a duplicate */

					/* keep track of subjects */
  one = first(t->subject);
  if ( !one->first )
  { one->first = TRUE;
    subjects++;
  }

					/* keep track of subPropertyOf */
  if ( t->predicate->name == ATOM_subPropertyOf &&
       t->objtype == OBJ_RESOURCE )
  { predicate *me    = lookup_predicate(t->subject);
    predicate *super = lookup_predicate(t->object);

    addSubPropertyOf(me, super);
  }

ok:
  created++;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rehash_triples()

Relink the triples in the hash-chains after the hash-keys for properties
have changed or the tables have  been   resized.  The caller must ensure
there are no active queries and the tables are of the proper size.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
rehash_triples()
{ int i;
  triple *t;

  for(i=1; i<INDEX_TABLES; i++)
  { if ( table[i] )
    { long bytes = sizeof(triple*) * table_size[i];

      memset(table[i], 0, bytes);
      memset(tail[i], 0, bytes);
    }
  }

  for(t=by_none; t; t = t->next[BY_NONE])
  { for(i=1; i<INDEX_TABLES; i++)
      t->next[i] = NULL;

    if ( t->erased == FALSE )
      link_triple_hash(t);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
update_hash()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
update_hash()
{ if ( need_update )
  { if ( active_queries )
    { Sdprintf("Problem: %d active queries\n", active_queries);
      return FALSE;
    }

    LOCK();
    if ( need_update )			/* check again */
    { if ( organise_predicates() )
      { Sdprintf("Re-hash ...");
	rehash_triples();
	Sdprintf("ok\n");
      }
      need_update = 0;
    }
    UNLOCK();
  }

  return TRUE;
}


/* MT: Must be locked */

static void
erase_triple(triple *t)
{ if ( !t->erased )
  { t->erased = TRUE;

    update_duplicates_del(t);

    if ( t->predicate->name == ATOM_subPropertyOf &&
	 t->objtype == OBJ_RESOURCE )
    { predicate *me    = lookup_predicate(t->subject);
      predicate *super = lookup_predicate(t->object);

      delSubPropertyOf(me, super);
    }

    if ( t->first )
    { triple *one = first(t->subject);

      if ( one )
	one->first = TRUE;
      else
	subjects--;
    }
    erased++;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Match triple t to pattern p.  Erased triples are always skipped.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
match_triples(triple *t, triple *p, unsigned flags)
{ if ( t->erased )
    return FALSE;
  if ( p->subject && t->subject != p->subject )
    return FALSE;
  if ( p->object && t->object != p->object )
  { if ( p->match && match(p->match, p->object, t->object) )
      return TRUE;

    return FALSE;
  }
  if ( p->objtype && t->objtype != p->objtype )
    return FALSE;
  if ( flags & MATCH_SRC )
  { if ( p->source && t->source != p->source )
      return FALSE;
    if ( p->line && t->line != p->line )
      return FALSE;
  }
					/* last; may be expensive */
  if ( p->predicate && t->predicate != p->predicate )
  { if ( (flags & MATCH_SUBPROPERTY) )
    { if ( !isSubPropertyOf(t->predicate, p->predicate) )
	return FALSE;
    } else
      return FALSE;
  }
  return TRUE;
}


		 /*******************************
		 *	      SAVE/LOAD		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The RDF triple format.  This format is intended for quick save and load
and not for readability or exchange.  Parts are based on the SWI-Prolog
Quick Load Format (implemented in pl-wic.c).

	<file> 		::= <magic>
			    <version>
			    {<triple>}
			    'E'

	<magic> 	::= "RDF-dump\n"
	<version> 	::= <integer>

	<triple>	::= 'T'
	                    <subject>
			    <predicate>
			    <object>
			    <source>

	<subject>	::= <resource>
	<predicate>	::= <resource>

	<object>	::= "R" <resource>
			  | "L" <atom>

	<resource>	::= <atom>

	<atom>		::= "X" <integer>
			    "A" <string>

	<string>	::= <integer><bytes>

	<source>	::= <atom>
			    <line>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define SAVE_MAGIC "RDF-dump\n"
#define SAVE_VERSION 1

typedef struct saved
{ atom_t name;
  long   as;
  struct saved *next;
} saved;

static saved ** saved_table;
static long     saved_size;
static long     saved_id;

long
next_table_size(long s0)
{ long size = 2;

  while(size < s0)
    size *= 2;

  return size;
}

static void
init_saved()
{ long size = next_table_size((created - erased)/8);
  long bytes = size * sizeof(*saved_table);

  saved_table = PL_malloc(bytes);
  memset(saved_table, 0, bytes);
  saved_size = size;
  saved_id = 0;
}

static void
destroy_saved()
{ if ( saved_table )
    PL_free(saved_table);

  saved_size = 0;
  saved_id = 0;
}

#define LONGBITSIZE (sizeof(long)*8)
#define PLMINLONG   ((long)(1L<<(LONGBITSIZE-1)))

static void
save_int(IOSTREAM *fd, long n)
{ int m;
  long absn = (n >= 0 ? n : -n);

  if ( n != PLMINLONG )
  { if ( absn < (1L << 5) )
    { Sputc((n & 0x3f), fd);
      return;
    } else if ( absn < (1L << 13) )
    { Sputc((((n >> 8) & 0x3f) | (1 << 6)), fd);
      Sputc((n & 0xff), fd);
      return;
    } else if ( absn < (1L << 21) )
    { Sputc((((n >> 16) & 0x3f) | (2 << 6)), fd);
      Sputc(((n >> 8) & 0xff), fd);
      Sputc((n & 0xff), fd);
      return;
    }
  }

  for(m = sizeof(n); ; m--)
  { int b = (absn >> (((m-1)*8)-1)) & 0x1ff;

    if ( b == 0 )
      continue;
    break;
  }

  Sputc(m | (3 << 6), fd);

  for( ; m > 0; m--)
  { int b = (n >> ((m-1)*8)) & 0xff;
    
    Sputc(b, fd);
  }
}


static void
save_atom(IOSTREAM *out, atom_t a)
{ int hash = atom_hash(a) % saved_size;
  saved *s;
  unsigned int len;
  const char *chars;
  unsigned int i;

  for(s=saved_table[hash]; s; s= s->next)
  { if ( s->name == a )
    { Sputc('X', out);
      save_int(out, s->as);

      return;
    }
  }

  s = PL_malloc(sizeof(*s));
  s->name = a;
  s->as = saved_id++;
  s->next = saved_table[hash];
  saved_table[hash] = s;

  Sputc('A', out);
  chars = PL_atom_nchars(a, &len);
  save_int(out, len);
  for(i=0; i<len; i++, chars++)
    Sputc(*chars, out);
}


static void
write_triple(IOSTREAM *out, triple *t)
{ Sputc('T', out);

  save_atom(out, t->subject);
  save_atom(out, t->predicate->name);

  switch(t->objtype)
  { case OBJ_RESOURCE:
      Sputc('R', out);
      break;
    case OBJ_LITERAL:
      Sputc('L', out);
      break;
    default:
      assert(0);
  }
  save_atom(out, t->object);

  save_atom(out, t->source);
  save_int(out, t->line);
}


static int
save_db(IOSTREAM *out, atom_t src)
{ triple *t;

  LOCK();
  init_saved();

  Sfprintf(out, "%s", SAVE_MAGIC);
  save_int(out, SAVE_VERSION);
  
  for(t = by_none; t; t = t->next[BY_NONE])
  { if ( !t->erased &&
	 (!src || t->source == src) )
    { write_triple(out, t);
      if ( Sferror(out) )
	return FALSE;
    }
  }
  Sputc('E', out);
  if ( Sferror(out) )
    return FALSE;

  destroy_saved();
  UNLOCK();

  return TRUE;
}


static foreign_t
rdf_save_db(term_t stream, term_t source)
{ IOSTREAM *out;
  atom_t src;

  if ( !PL_get_stream_handle(stream, &out) )
    return type_error(stream, "stream");
  if ( !get_atom_or_var_ex(source, &src) )
    return FALSE;

  return save_db(out, src);
}


static long
load_int(IOSTREAM *fd)
{ long first = Sgetc(fd);
  int bytes, shift, b;

  if ( !(first & 0xc0) )		/* 99% of them: speed up a bit */    
  { first <<= (LONGBITSIZE-6);
    first >>= (LONGBITSIZE-6);

    return first;
  }

  bytes = (int) ((first >> 6) & 0x3);
  first &= 0x3f;

  if ( bytes <= 2 )
  { for( b = 0; b < bytes; b++ )
    { first <<= 8;
      first |= Sgetc(fd) & 0xff;
    }

    shift = (sizeof(long)-1-bytes)*8 + 2;
  } else
  { int m;

    bytes = first;
    first = 0L;

    for(m=0; m<bytes; m++)
    { first <<= 8;
      first |= Sgetc(fd) & 0xff;
    }
    shift = (sizeof(long)-bytes)*8;
  }

  first <<= shift;
  first >>= shift;
  
  return first;
}

typedef struct ld_context
{ long		loaded_id;
  atom_t       *loaded_atoms;
  long		atoms_size;
} ld_context;


static void
add_atom(atom_t a, ld_context *ctx)
{ if ( ctx->loaded_id >= ctx->atoms_size )
  { if ( ctx->atoms_size == 0 )
    { ctx->atoms_size = 1024;
      ctx->loaded_atoms = PL_malloc(sizeof(atom_t)*ctx->atoms_size);
    } else
    { ctx->atoms_size *= 2;
      ctx->loaded_atoms = PL_realloc(ctx->loaded_atoms,
				     sizeof(atom_t)*ctx->atoms_size);
    }
  }

  ctx->loaded_atoms[ctx->loaded_id++] = a;
}


static atom_t
load_atom(IOSTREAM *in, ld_context *ctx)
{ switch(Sgetc(in))
  { case 'X':
      return ctx->loaded_atoms[load_int(in)];
    case 'A':
    { int len = load_int(in);
      atom_t a;

      if ( len < 1024 )
      { char buf[1024];
	Sfread(buf, 1, len, in);
	a = PL_new_atom_nchars(len, buf);
      } else
      { char *buf = PL_malloc(len);
	Sfread(buf, 1, len, in);
	a = PL_new_atom_nchars(len, buf);
	PL_free(buf);
      }

      add_atom(a, ctx);
      return a;
    }
    default:
      assert(0);
  }
}


static int
load_triple(IOSTREAM *in, ld_context *ctx)
{ triple *t = PL_malloc(sizeof(*t));

  memset(t, 0, sizeof(*t));
  t->subject   = load_atom(in, ctx);
  t->predicate = lookup_predicate(load_atom(in, ctx));
  switch(Sgetc(in))
  { case 'R':
      t->objtype = OBJ_RESOURCE;
      break;
    case 'L':
      t->objtype = OBJ_LITERAL;
      break;
    default:
      assert(0);
  }
  t->object = load_atom(in, ctx);
  t->source = load_atom(in, ctx);
  t->line   = load_int(in);
  LOCK();
  link_triple(t);
  UNLOCK();

  return TRUE;
}


static int
load_magic(IOSTREAM *in)
{ char *s = SAVE_MAGIC;

  for( ; *s; s++)
  { if ( Sgetc(in) != *s )
      return FALSE;
  }

  return TRUE;
}


static int
load_db(IOSTREAM *in)
{ ld_context ctx;
  int version;
  int c;

  if ( !load_magic(in) )
    return FALSE;
  version = load_int(in);
  
  memset(&ctx, 0, sizeof(ctx));

  while((c=Sgetc(in)) != EOF)
  { switch(c)
    { case 'T':
	if ( !load_triple(in, &ctx) )
	  return FALSE;
        break;
      case 'E':
	if ( ctx.loaded_atoms )
	  PL_free(ctx.loaded_atoms);
        generation++;
	return TRUE;
      default:
	break;
    }
  }
  
  return PL_warning("Illegal RDF triple file");
}


static foreign_t
rdf_load_db(term_t stream)
{ IOSTREAM *in;

  if ( !PL_get_stream_handle(stream, &in) )
    return type_error(stream, "stream");

  return load_db(in);
}


		 /*******************************
		 *	       ATOMS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Lock atoms in triple against AGC. Note that the predicate name is locked
in the predicate structure.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
lock_atoms(triple *t)
{ PL_register_atom(t->subject);
  PL_register_atom(t->object);
}


		 /*******************************
		 *      PROLOG CONVERSION	*
		 *******************************/

static int
get_object(term_t object, triple *t)
{ if ( PL_get_atom(object, &t->object) )
    t->objtype = OBJ_RESOURCE;
  else if ( PL_is_functor(object, FUNCTOR_literal1) )
  { term_t a = PL_new_term_ref();
    
    PL_get_arg(1, object, a);
    if ( !get_atom_ex(a, &t->object) )
      return FALSE;
    t->objtype = OBJ_LITERAL;
  } else
    return type_error(object, "rdf_object");

  return TRUE;
}


static int
get_src(term_t src, triple *t)
{ if ( src && !PL_is_variable(src) )
  { if ( PL_get_atom(src, &t->source) )
    { t->line = NO_LINE;
    } else if ( PL_is_functor(src, FUNCTOR_colon2) )
    { term_t a = PL_new_term_ref();
      long line;
      
      PL_get_arg(1, src, a);
      if ( !get_atom_or_var_ex(a, &t->source) )
	return FALSE;
      PL_get_arg(2, src, a);
      if ( PL_get_long(a, &line) )
	t->line = line;
      else if ( !PL_is_variable(a) )
	return type_error(a, "integer");
    } else
      return type_error(src, "rdf_source");
  }

  return TRUE;
}


static int
get_predicate(term_t t, predicate **p)
{ atom_t name;

  if ( !get_atom_ex(t, &name ) )
    return FALSE;

  *p = lookup_predicate(name);
  return TRUE;
}


static int
get_triple(term_t subject, term_t predicate, term_t object, triple *t)
{ if ( !get_atom_ex(subject, &t->subject) ||
       !get_predicate(predicate, &t->predicate) ||
       !get_object(object, t) )
    return FALSE;

  return TRUE;
}


static int
get_partial_triple(term_t subject, term_t predicate, term_t object,
		   term_t src, triple *t)
{ if ( subject && !get_atom_or_var_ex(subject, &t->subject) )
    return FALSE;
  if ( !PL_is_variable(predicate) &&
       !get_predicate(predicate, &t->predicate) )
    return FALSE;
					/* the object */
  if ( object && !PL_is_variable(object) )
  { if ( PL_get_atom(object, &t->object) )
      t->objtype = OBJ_RESOURCE;
    else if ( PL_is_functor(object, FUNCTOR_literal1) )
    { term_t a = PL_new_term_ref();
      
      PL_get_arg(1, object, a);
      if ( !get_atom_or_var_ex(a, &t->object) )
	return FALSE;
      t->objtype = OBJ_LITERAL;
    } else if ( PL_is_functor(object, FUNCTOR_literal2) )
    { term_t a = PL_new_term_ref();
      
      PL_get_arg(1, object, a);
      if ( PL_is_functor(a, FUNCTOR_exact1) )
	t->match = MATCH_EXACT;
      else if ( PL_is_functor(a, FUNCTOR_substring1) )
	t->match = MATCH_SUBSTRING;
      else if ( PL_is_functor(a, FUNCTOR_word1) )
	t->match = MATCH_WORD;
      else if ( PL_is_functor(a, FUNCTOR_prefix1) )
	t->match = MATCH_PREFIX;
      else
	return domain_error(a, "match_type");
      PL_get_arg(1, a, a);
      if ( !get_atom_or_var_ex(a, &t->object) )
	return FALSE;
      t->objtype = OBJ_LITERAL;
    } else
      return type_error(object, "rdf_object");
  }
					/* the source */
  if ( !get_src(src, t) )
    return FALSE;

  if ( t->subject )
    t->indexed |= BY_S;
  if ( t->predicate )
    t->indexed |= BY_P;
  if ( t->object && t->match <= MATCH_EXACT )
    t->indexed |= BY_O;

  indexed[t->indexed]++;		/* statistics */

  switch(t->indexed)
  { case BY_SPO:
      t->indexed = BY_SP;
      break;
    case BY_SO:
      t->indexed = BY_S;
      break;
  }

  return TRUE;
}


static int
get_source(term_t src, triple *t)
{ if ( PL_get_atom(src, &t->source) )
  { t->line = NO_LINE;
    return TRUE;
  }

  if ( PL_is_functor(src, FUNCTOR_colon2) )
  { term_t a = PL_new_term_ref();
    long line;

    PL_get_arg(1, src, a);
    if ( !get_atom_ex(a, &t->source) )
      return FALSE;
    PL_get_arg(2, src, a);
    if ( !get_long_ex(a, &line) )
      return FALSE;
    t->line = line;

    return TRUE;
  }

  return type_error(src, "rdf_source");
}


static int
unify_source(term_t src, triple *t)
{ if ( t->line == NO_LINE )
  { if ( !PL_unify_atom(src, t->source) )
      return PL_unify_term(src,
			   PL_FUNCTOR, FUNCTOR_colon2,
			     PL_ATOM, t->source,
			     PL_VARIABLE);  
  } else
    return PL_unify_term(src,
			 PL_FUNCTOR, FUNCTOR_colon2,
			   PL_ATOM, t->source,
			   PL_INTEGER, t->line);

  return FALSE;				/* make compiler happy */
}


static int
unify_triple(term_t subject, term_t predicate, term_t object,
	     term_t src, triple *t)
{ if ( !PL_unify_atom(subject, t->subject) ||
       !PL_unify_atom(predicate, t->predicate->name) )
    return FALSE;

  switch(t->objtype)
  { case OBJ_RESOURCE:
      if ( !PL_unify_atom(object, t->object) )
	return FALSE;
      break;
    case OBJ_LITERAL:
      if ( PL_is_functor(object, FUNCTOR_literal2) )
      { term_t a = PL_new_term_ref();

	PL_get_arg(2, object, a);
	if ( !PL_unify_atom(a, t->object) )
	  return FALSE;
      } else
      { if ( !PL_unify_term(object,
			    PL_FUNCTOR, FUNCTOR_literal1,
			      PL_ATOM, t->object) )
	  return FALSE;
      }
      break;
    default:
      assert(0);
  }

  if ( src )
    return unify_source(src, t);

  return TRUE;
}


		 /*******************************
		 *	DUBLICATE HANDLING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
According to the RDF specs, duplicate triples  have no meaning, but they
slow down search and often produce   duplicate results in search. Worse,
some coding styles proposed in the  OWL documents introduce huge amounts
of duplicate triples. We cannot  simply  ignore   a  triple  if  it is a
duplicate as a subsequent retract  would   delete  the final triple. For
example, after loading two  files  that   contain  the  same  triple and
unloading one of these files the database would be left without triples.

In our solution, if a triple is added as a duplicate, it is flagged such
using  the  flag  is_duplicate.  The  `principal'  triple  has  a  count
`duplicates',  indicating  the  number  of   duplicate  triples  in  the
database.

It might make sense to  introduce  the   BY_SPO  table  as fully indexed
lookups are frequent with the introduction of duplicate detection.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static int
update_duplicates_add(triple *t)
{ triple *d;
  const int indexed = BY_SP;

  assert(t->is_duplicate == FALSE);
  assert(t->duplicates == 0);

  d = table[indexed][triple_hash(t, indexed)];
  for( ; d; d = d->next[indexed] )
  { if ( d != t && match_triples(d, t, MATCH_EXACT) )
    { t->is_duplicate = TRUE;
      if ( !d->is_duplicate )
      { d->duplicates++;

	DEBUG(1,
	      print_triple_source(t);
	      Sdprintf("%p: %d-th duplicate: ", t, d->duplicates);
	      print_triple(t);
	      print_triple_source(d);
	      Sdprintf("Location of first (%p)\n", d));

	assert(d->duplicates);		/* check overflow */
	duplicates++;
	return TRUE;
      }
    }
  }

  return FALSE;
}


static void				/* t is about to be deleted */
update_duplicates_del(triple *t)
{ const int indexed = BY_SP;

  duplicates--;

  if ( t->duplicates )			/* I am the principal one */
  { triple *d;
      
    DEBUG(1,
	  print_triple(t);
	  Sdprintf(": Deleting %p, %d duplicates\n", t, t->duplicates));

    d = table[indexed][triple_hash(t, indexed)];
    for( ; d; d = d->next[indexed] )
    { if ( d != t && match_triples(d, t, MATCH_EXACT) )
      { assert(d->is_duplicate);
	d->is_duplicate = FALSE;
	d->duplicates = t->duplicates-1;
	DEBUG(1, Sdprintf("New principal: %p\n", d));

	return;
      }
    }
    assert(0);
  } else if ( t->is_duplicate )		/* I am a duplicate */
  { triple *d;
      
    DEBUG(1,
	  print_triple(t);
	  Sdprintf(": Deleting, is a duplicate\n"));

    d = table[indexed][triple_hash(t, indexed)];
    for( ; d; d = d->next[indexed] )
    { if ( d != t && match_triples(d, t, MATCH_EXACT) )
      { if ( d->duplicates )
	{ d->duplicates--;
	  return;
	}
      }
    }
    assert(0);
  }
}


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

static foreign_t
rdf_assert4(term_t subject, term_t predicate, term_t object, term_t src)
{ triple *t = PL_malloc(sizeof(*t));

  memset(t, 0, sizeof(*t));
  if ( !get_triple(subject, predicate, object, t) )
  { PL_free(t);
    return FALSE;
  }
  if ( src )
  { if ( !get_source(src, t) )
    { PL_free(t);
      return FALSE;
    }
  } else
  { t->source = ATOM_user;
    t->line = NO_LINE;
  }

  lock_atoms(t);
  LOCK();
  link_triple(t);
  generation++;
  UNLOCK();

  return TRUE;
}


static foreign_t
rdf_assert3(term_t subject, term_t predicate, term_t object)
{ return rdf_assert4(subject, predicate, object, 0);
}


static foreign_t
rdf(term_t subject, term_t predicate, term_t object,
    term_t src, term_t realpred, control_t h)
{ unsigned flags = realpred ? MATCH_SUBPROPERTY : MATCH_EXACT;
  term_t retpred = realpred ? realpred : predicate;

  if ( src )
    flags |= MATCH_SRC;

  switch(PL_foreign_control(h))
  { case PL_FIRST_CALL:
    { triple t, *p;
      
      memset(&t, 0, sizeof(t));
      if ( !get_partial_triple(subject, predicate, object, src, &t) )
	return FALSE;

      if ( !update_hash() )
	return FALSE;
      p = table[t.indexed][triple_hash(&t, t.indexed)];
      for( ; p; p = p->next[t.indexed])
      { if ( match_triples(p, &t, flags) )
	{ if ( !unify_triple(subject, retpred, object, src, p) )
	    continue;
	  if ( realpred && PL_is_variable(predicate) )
	    PL_unify(predicate, retpred);

	  for(p=p->next[t.indexed]; p; p = p->next[t.indexed])
	  { if ( p->is_duplicate && !src )
	      continue;

	    if ( match_triples(p, &t, flags) )
	    { t.next[0] = p;
	      
	      active_queries++;
	      PL_retry_address(save_triple(&t));
	    }
	  }

          return TRUE;
	}
      }
      return FALSE;
    }
    case PL_REDO:
    { triple *p, *t = PL_foreign_context_address(h);

      p = t->next[0];
      for( ; p; p = p->next[t->indexed])
      { if ( p->is_duplicate && !src )
	  continue;

	if ( match_triples(p, t, flags) )
	{ if ( !unify_triple(subject, retpred, object, src, p) )
	    continue;
	  if ( realpred && PL_is_variable(predicate) )
	    PL_unify(predicate, retpred);

	  for(p=p->next[t->indexed]; p; p = p->next[t->indexed])
	  { if ( match_triples(p, t, flags) )
	    { t->next[0] = p;
	      
	      PL_retry_address(t);
	    }
	  }

          PL_free(t);
	  active_queries--;
          return TRUE;
	}
      }
      PL_free(t);
      active_queries--;
      return FALSE;
    }
    case PL_CUTTED:
    { triple *t = PL_foreign_context_address(h);

      active_queries--;
      PL_free(t);
      return TRUE;
    }
    default:
      assert(0);
      return FALSE;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rdf(Subject, Predicate, Object)

Search specifications:

	Predicate:

		subPropertyOf(X) = P

	Object:

		literal(substring(X), L)
		literal(word(X), L)
		literal(exact(X), L)
		literal(prefix(X), L)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static foreign_t
rdf3(term_t subject, term_t predicate, term_t object, control_t h)
{ return rdf(subject, predicate, object, 0, 0, h);
}


static foreign_t
rdf4(term_t subject, term_t predicate, term_t object,
     term_t src, control_t h)
{ return rdf(subject, predicate, object, src, 0, h);
}


static foreign_t
rdf_has(term_t subject, term_t predicate, term_t object,
	term_t realpred, control_t h)
{ return rdf(subject, predicate, object, 0, realpred, h);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rdf_update(+Subject, +Predicate, +Object, +Action)

Update a triple. Please note this is actually erase+assert as the triple
needs to be updated in  the  linked   lists  while  erase simply flags a
triple as `erases' without deleting it   to support queries which active
choicepoints.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
update_triple(term_t action, triple *t)
{ term_t a = PL_new_term_ref();
  triple tmp = *t;
  triple *new;
  int i;

  if ( !PL_get_arg(1, action, a) )
    return type_error(action, "rdf_action");

  if ( PL_is_functor(action, FUNCTOR_subject1) )
  { atom_t s;

    if ( !get_atom_ex(a, &s) )
      return FALSE;
    if ( tmp.subject == s )
      return TRUE;			/* no change */

    tmp.subject = s;
  } else if ( PL_is_functor(action, FUNCTOR_predicate1) )
  { predicate *p;

    if ( !get_predicate(a, &p) )
      return FALSE;
    if ( tmp.predicate == p )
      return TRUE;			/* no change */

    tmp.predicate = p;
  } else if ( PL_is_functor(action, FUNCTOR_object1) )
  { triple t2;

    if ( !get_object(a, &t2) )
      return FALSE;
    if ( t2.object == tmp.object && t2.objtype == tmp.objtype )
      return TRUE;

    tmp.objtype = t2.objtype;
    tmp.object = t2.object;
  } else if ( PL_is_functor(action, FUNCTOR_source1) )
  { triple t2;

    if ( !get_source(a, &t2) )
      return FALSE;
    if ( t2.source == t->source && t2.line == t->line )
      return TRUE;
    t->source = t2.source;
    t->line = t2.line;

    return TRUE;			/* considered no change */
  } else
    return domain_error(action, "rdf_action");

  for(i=0; i<INDEX_TABLES; i++)
    tmp.next[i] = NULL;

  LOCK();
  erase_triple(t);
  new = PL_malloc(sizeof(*new));
  *new = tmp;

  lock_atoms(new);
  link_triple(new);
  generation++;
  UNLOCK();

  return TRUE;
}



static foreign_t
rdf_update5(term_t subject, term_t predicate, term_t object, term_t src,
	    term_t action)
{ triple t, *p;
  int indexed = BY_SP;
  int done = 0;
    
  memset(&t, 0, sizeof(t));
  if ( !get_triple(subject, predicate, object, &t) ||
       !get_src(src, &t) )
    return FALSE;
  
  if ( !update_hash() )
    return FALSE;
  p = table[indexed][triple_hash(&t, indexed)];
  for( ; p; p = p->next[indexed])
  { if ( match_triples(p, &t, MATCH_EXACT) )
    { if ( !update_triple(action, p) )
	return FALSE;			/* type errors */
      done++;
    }
  }

  return done ? TRUE : FALSE;
}


static foreign_t
rdf_update(term_t subject, term_t predicate, term_t object, term_t action)
{ return rdf_update5(subject, predicate, object, 0, action);
}


static foreign_t
rdf_retractall4(term_t subject, term_t predicate, term_t object, term_t src)
{ triple t, *p;
  int erased = 0;
      
  memset(&t, 0, sizeof(t));
  if ( !get_partial_triple(subject, predicate, object, src, &t) )
    return FALSE;

  if ( !update_hash() )
    return FALSE;
  p = table[t.indexed][triple_hash(&t, t.indexed)];
  for( ; p; p = p->next[t.indexed])
  { if ( match_triples(p, &t, MATCH_EXACT|MATCH_SRC) )
    { LOCK();
      erase_triple(p);
      erased++;
      UNLOCK();
    }
  }

  if ( erased )
    generation++;

  return TRUE;
}


static foreign_t
rdf_retractall3(term_t subject, term_t predicate, term_t object)
{ return rdf_retractall4(subject, predicate, object, 0);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Enumerate the known subjects. This uses the   `first' flag on triples to
avoid returning the same resource multiple   times.  As the `by_none' is
never re-hashed, we don't mark this query in the `active_queries'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
rdf_subject(term_t subject, control_t h)
{ triple *t;

  switch(PL_foreign_control(h))
  { case PL_FIRST_CALL:
    { if ( PL_is_variable(subject) )
      { t = table[BY_NONE][0];
	goto next;
      } else
      { atom_t a;

	if ( get_atom_ex(subject, &a) )
	{ if ( first(a) )
	    return TRUE;
	  return FALSE;
	}

	return FALSE;
      }
    }
    case PL_REDO:
      t = PL_foreign_context_address(h);
    next:
      for(; t; t = t->next[BY_NONE])
      { if ( t->first && !t->erased )
	{ PL_unify_atom(subject, t->subject);
	  
	  t = t->next[BY_NONE];
	  if ( t )
	    PL_retry_address(t);
	  return TRUE;
	}
      }
      return FALSE;
    case PL_CUTTED:
      return TRUE;
    default:
      assert(0);
      return FALSE;
  }
}

		 /*******************************
		 *     TRANSITIVE RELATIONS	*
		 *******************************/

typedef struct visited
{ struct visited *next;			/* next in list */
  struct visited *hash_link;		/* next in hashed link */
  atom_t resource;			/* visited resource */
} visited;


typedef struct agenda
{ visited *head;			/* visited list */
  visited *tail;			/* tail of visited list */
  visited *to_expand;			/* next to expand */
  visited *to_return;			/* next to return */
  visited **hash;			/* hash-table for cycle detection */
  int	  hash_size;
  int     size;				/* size of the agenda */
  triple  pattern;			/* partial triple used as pattern */
  atom_t  target;			/* resource we are seaching for */
  struct chunk  *chunk;			/* node-allocation chunks */
} agenda;

#ifndef offsetof
#define offsetof(structure, field) ((int) &(((structure *)NULL)->field))
#endif
#define CHUNK_SIZE(n) offsetof(chunk, nodes[n])

typedef struct chunk
{ struct chunk *next;
  int	 used;				/* # used elements */
  int	 size;				/* size of the chunk */
  struct visited nodes[1];		/* nodes in the chunk */
} chunk;


static visited *
alloc_node_agenda(agenda *a)
{ chunk *c;
  int size;

  if ( (c=a->chunk) )
  { if ( c->used < c->size )
    { visited *v = &c->nodes[c->used++];

      return v;
    }
  }

  size = (a->size == 0 ? 8 : 1024);
  c = PL_malloc(CHUNK_SIZE(size));
  c->size = size;
  c->used = 1;
  c->next = a->chunk;
  a->chunk = c;

  return &c->nodes[0];
}


static void
empty_agenda(agenda *a)
{ chunk *c, *n;

  for(c=a->chunk; c; c = n)
  { n = c->next;
    PL_free(c);
  }
  if ( a->hash )
    PL_free(a->hash);
}


static agenda *
save_agenda(agenda *a)
{ agenda *r = PL_malloc(sizeof(*r));

  *r = *a;

  return r;
}


static void
hash_agenda(agenda *a, int size)
{ if ( a->hash )
    PL_free(a->hash);
  if ( size > 0 )
  { visited *v;

    a->hash = PL_malloc(sizeof(visited*)*size);
    memset(a->hash, 0, sizeof(visited*)*size);
    a->hash_size = size;
    
    for(v=a->head; v; v = v->next)
    { int key = atom_hash(v->resource)&(size-1);

      v->hash_link = a->hash[key];
      a->hash[key] = v;
    }
  }
}


static int
in_aganda(agenda *a, atom_t resource)
{ visited *v;

  if ( a->hash )
  { int key = atom_hash(resource)&(a->hash_size-1);
    v = a->hash[key];

    for( ; v; v = v->hash_link )
    { if ( v->resource == resource )
	return TRUE;
    }
  } else
  { v = a->head;

    for( ; v; v = v->next )
    { if ( v->resource == resource )
	return TRUE;
    }
  }

  return FALSE;
}


static visited *
append_agenda(agenda *a, atom_t res)
{ visited *v = a->head;

  if ( in_aganda(a, res) )
    return NULL;

  agenda_created++;			/* statistics */

  a->size++; 
  if ( !a->hash_size && a->size > 32 )
    hash_agenda(a, 64);
  else if ( a->size > a->hash_size * 4 )
    hash_agenda(a, a->hash_size * 4);

  v = alloc_node_agenda(a);
  v->resource = res;
  v->next = NULL;
  if ( a->tail )
  { a->tail->next = v;
    a->tail = v;
  } else
  { a->head = a->tail = v;
  }

  if ( a->hash_size )
  { int key = atom_hash(res)&(a->hash_size-1);
   
    v->hash_link = a->hash[key];
    a->hash[key] = v;
  }

  return v;
}


static int
can_reach_target(agenda *a)
{ int indexed = a->pattern.indexed;
  int rc = FALSE;
  triple *p;

  if ( indexed & BY_S )			/* subj ---> */
  { a->pattern.object = a->target;
    indexed |= BY_O;
  } else
  { a->pattern.subject = a->target;
    indexed |= BY_S;
  }

  p = table[indexed][triple_hash(&a->pattern, indexed)];
  for( ; p; p = p->next[indexed])
  { if ( match_triples(p, &a->pattern, MATCH_SUBPROPERTY) )
    { rc = TRUE;
      break;
    }
  }

  if ( a->pattern.indexed & BY_S )	
  { a->pattern.object = 0;
  } else
  { a->pattern.subject = 0;
  }

  return rc;
}



static visited *
bf_expand(agenda *a, atom_t resource)
{ triple *p;
  int indexed = a->pattern.indexed;
  visited *rc = NULL;

  if ( indexed & BY_S )			/* subj ---> */
  { a->pattern.subject = resource;
  } else
  { a->pattern.object = resource;
  }

  if ( a->target && can_reach_target(a) )
  { return append_agenda(a, a->target);
  }

  p = table[indexed][triple_hash(&a->pattern, indexed)];
  for( ; p; p = p->next[indexed])
  { if ( match_triples(p, &a->pattern, MATCH_SUBPROPERTY) )
    { atom_t found = (indexed & BY_S) ? p->object : p->subject;
      visited *v;

      v = append_agenda(a, found);
      if ( !rc )
	rc = v;
      if ( found == a->target )
	break;
    }
  }

  return rc;
}


static int
next_agenda(agenda *a, atom_t *next)
{ if ( a->to_return )
  { ok:

    *next = a->to_return->resource;
    a->to_return = a->to_return->next;
  
    return TRUE;
  }

  while( a->to_expand )
  { a->to_return = bf_expand(a, a->to_expand->resource);
    a->to_expand = a->to_expand->next;

    if ( a->to_return )
      goto ok;
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rdf_reachable(+Subject, +Predicate, -Object)
rdf_reachable(-Subject, +Predicate, ?Object)
    Examine transitive relations, reporting all `Object' that can be
    reached from `Subject' using Predicate without going into a loop
    if the relation is cyclic.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
rdf_reachable(term_t subj, term_t pred, term_t obj, control_t h)
{ switch(PL_foreign_control(h))
  { case PL_FIRST_CALL:
    { agenda a;
      atom_t r;
      term_t target_term;

      if ( PL_is_variable(pred) )
	return instantiation_error(pred);

      memset(&a, 0, sizeof(a));
      if ( !PL_is_variable(subj) )		/* subj .... obj */
      { if ( !get_partial_triple(subj, pred, 0, 0, &a.pattern) )
	  return FALSE;
	a.target = a.pattern.object;
	target_term = obj;
      } else if ( !PL_is_variable(obj) ) 	/* obj .... subj */
      {	if ( !get_partial_triple(0, pred, obj, 0, &a.pattern) )
	  return FALSE;
	a.target = a.pattern.subject;
	target_term = subj;
      } else
	return instantiation_error(subj);

      if ( !update_hash() )
	return FALSE;
      if ( (a.pattern.indexed & BY_S) ) 	/* subj ... */
	append_agenda(&a, a.pattern.subject);
      else
	append_agenda(&a, a.pattern.object);
      a.to_return = a.head;
      a.to_expand = a.head;

      while(next_agenda(&a, &r))
      { if ( PL_unify_atom(target_term, r) )
	{ if ( a.target )		/* mode(+, +, +) */
	  { empty_agenda(&a);
	    return TRUE;
	  } else			/* mode(+, +, -) or mode(-, +, +) */
	  { active_queries++;
	    PL_retry_address(save_agenda(&a));
	  }
	}
      }
      empty_agenda(&a);
      return FALSE;
    }
    case PL_REDO:
    { agenda *a = PL_foreign_context_address(h);
      term_t target_term;
      atom_t r;

      if ( !PL_is_variable(subj) )
	target_term = obj;
      else
	target_term = subj;

      while(next_agenda(a, &r))
      { if ( PL_unify_atom(target_term, r) )
	{ if ( a->target )		/* +, +, + */
	  { empty_agenda(a);
	    return TRUE;
	  } else
	  { PL_retry_address(a);
	  }
	}
      }

      active_queries--;
      empty_agenda(a);
      PL_free(a);
      return FALSE;
    }
    case PL_CUTTED:
    { agenda *a = PL_foreign_context_address(h);

      active_queries--;
      empty_agenda(a);
      PL_free(a);
      return TRUE;
    }
    default:
      assert(0);
      return FALSE;
  }
}


		 /*******************************
		 *	     STATISTICS		*
		 *******************************/

static functor_t keys[10];

static int
unify_statistics(term_t key, functor_t f)
{ long v;

  if ( f == FUNCTOR_triples1 )
  { v = created - erased;
  } else if ( f == FUNCTOR_subjects1 )
  { v = subjects;
  } else if ( f == FUNCTOR_predicates1 )
  { v = pred_count;
  } else if ( f == FUNCTOR_indexed8 )
  { int i;
    term_t a = PL_new_term_ref();

    PL_unify_functor(key, FUNCTOR_indexed8);
    for(i=0; i<8; i++)
    { PL_get_arg(i+1, key, a);
      PL_unify_integer(a, indexed[i]);
    }

    return TRUE;
  } else if ( f == FUNCTOR_searched_nodes1 )
  { v = agenda_created;
  } else if ( f == FUNCTOR_duplicates1 )
  { v = duplicates;
  } else
    assert(0);

  return PL_unify_term(key, PL_FUNCTOR, f, PL_INTEGER, v);
}

static foreign_t
rdf_statistics(term_t key, control_t h)
{ int n;

  switch(PL_foreign_control(h))
  { case PL_FIRST_CALL:
    { functor_t f;

      if ( PL_is_variable(key) )
      { n = 0;
	goto redo;
      } else if ( PL_get_functor(key, &f) )
      { for(n=0; keys[n]; n++)
	{ if ( keys[n] == f ) 
	    return unify_statistics(key, f);
	}
	return domain_error(key, "rdf_statistics");
      } else
	return type_error(key, "rdf_statistics");
    }
    case PL_REDO:
      n = PL_foreign_context(h);
    redo:
      unify_statistics(key, keys[n]);
      n++;
      if ( keys[n] )
	PL_retry(n);
    case PL_CUTTED:
      return TRUE;
    default:
      assert(0);
      return TRUE;
  }
}


static foreign_t
rdf_generation(term_t t)
{ return PL_unify_integer(t, generation);
}


		 /*******************************
		 *	       MATCH		*
		 *******************************/

static const char *
nextword(const char *s)
{ while(*s && isalnum(*s))
    s++;
  while(*s && !isalnum(*s))
    s++;

  return s;
}


static int
match(int how, atom_t search, atom_t label)
{ const char *l = PL_atom_chars(label);
  const char *f = PL_atom_chars(search);

  switch(how)
  { case MATCH_EXACT:
    { for( ; *l && *f; l++, f++ )
      { if ( tolower(*l) != tolower(*f) )
	  return FALSE;
      }
      if ( *l == '\0' && *f == '\0' )
	return TRUE;
  
      return FALSE;
    }
    case MATCH_PREFIX:
    { for( ; *l && *f; l++, f++ )
      { if ( tolower(*l) != tolower(*f) )
	  return FALSE;
      }
      if ( *f == '\0' )
	return TRUE;
  
      return FALSE;
    }
    case MATCH_SUBSTRING:		/* use Boyle-More! */
    { const char *h;
      const char *f0 = f;
  
      for(h=l; *h; h++)
      { for( l=h,f=f0; *l && *f; l++, f++ )
	{ if ( tolower(*l) != tolower(*f) )
	    break;
	}
	if ( *f == '\0' )
	  return TRUE;
	if ( *h == '\0' )
	  return FALSE;
      }
  
      return FALSE;
    }
    case MATCH_WORD:
    { const char *h;
      const char *f0 = f;
  
      for(h=l; *h; h = nextword(h))
      { for( l=h,f=f0; *l && *f; l++, f++ )
	{ if ( tolower(*l) != tolower(*f) )
	    break;
	}
	if ( *f == '\0' )
	{ if ( *l == '\0' || !isalnum(*l) )
	    return TRUE;
	}
	if ( *l == '\0' )
	  return FALSE;
      }
  
      return FALSE;
    }
    default:
      assert(0);
      return FALSE;
  }
}


static foreign_t
match_label(term_t how, term_t search, term_t label)
{ atom_t h, f, l;
  int type;

  if ( !get_atom_ex(how, &h) ||
       !get_atom_ex(search, &f) ||
       !get_atom_ex(label, &l) )
    return FALSE;

  if ( h == ATOM_exact )
    type = MATCH_EXACT;
  else if ( h == ATOM_substring )
    type = MATCH_SUBSTRING;
  else if ( h == ATOM_word )
    type = MATCH_WORD;
  else if ( h == ATOM_prefix )
    type = MATCH_PREFIX;
  else
    return domain_error(how, "search_method");

  return match(type, f, l);
}


static foreign_t
split_url(term_t base, term_t local, term_t url)
{ char *b, *l, *u;
  unsigned int bl, ll;

  if ( PL_get_atom_nchars(base, &bl, &b) &&
       PL_get_atom_nchars(local, &ll, &l) )
  { if ( bl+ll < 1024 )
    { char buf[1024];

      memcpy(buf, b, bl);
      memcpy(buf, l, ll);

      return PL_unify_atom_nchars(url, bl+ll, buf);
    } else
    { char *buf = PL_malloc(bl+ll);
      int rc;

      memcpy(buf, b, bl);
      memcpy(buf, l, ll);

      rc = PL_unify_atom_nchars(url, bl+ll, buf);
      PL_free(buf);
      return rc;
    }
  } else if ( PL_get_atom_chars(url, &u) )
  { char *s, *last = NULL;

    for(s=u; *s; s++)
    { if ( *s == '#' || *s == '/' )
	last = s;
    }
    if ( last )
    { if ( PL_unify_atom_chars(local, last+1) &&
	   PL_unify_atom_nchars(base, last+1-u, u) )
	return TRUE;
      else
	return FALSE;
    } else
    { if ( PL_unify(local, url) &&
	   PL_unify_atom_chars(base, "") )
	return TRUE;
      else
	return FALSE;
    }
  } else
    return type_error(url, "atom");
}


		 /*******************************
		 *	       DEBUG		*
		 *******************************/

#ifdef O_DEBUG

static foreign_t
rdf_debug(term_t level)
{ long v;

  if ( !get_long_ex(level, &v) )
    return FALSE;

  debuglevel = v;
  return TRUE;
}

#endif

		 /*******************************
		 *	     REGISTER		*
		 *******************************/

#define MKFUNCTOR(n, a) \
	FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)
#define NDET PL_FA_NONDETERMINISTIC

install_t
install_rdf_db()
{ int i=0;
  MKFUNCTOR(literal, 1);
  MKFUNCTOR(error, 2);
  MKFUNCTOR(type_error, 2);
  MKFUNCTOR(domain_error, 2);
  MKFUNCTOR(triples, 1);
  MKFUNCTOR(subjects, 1);
  MKFUNCTOR(predicates, 1);
  MKFUNCTOR(subject, 1);
  MKFUNCTOR(predicate, 1);
  MKFUNCTOR(object, 1);
  MKFUNCTOR(source, 1);
  MKFUNCTOR(indexed, 8);
  MKFUNCTOR(exact, 1);
  MKFUNCTOR(substring, 1);
  MKFUNCTOR(word, 1);
  MKFUNCTOR(prefix, 1);
  MKFUNCTOR(literal, 2);
  MKFUNCTOR(searched_nodes, 1);
  MKFUNCTOR(duplicates, 1);

  FUNCTOR_colon2 = PL_new_functor(PL_new_atom(":"), 2);

  ATOM_user	     = PL_new_atom("user");
  ATOM_exact	     = PL_new_atom("exact");
  ATOM_prefix	     = PL_new_atom("prefix");
  ATOM_substring     = PL_new_atom("substring");
  ATOM_word	     = PL_new_atom("word");
  ATOM_subPropertyOf = PL_new_atom(URL_subPropertyOf);

					/* statistics */
  keys[i++] = FUNCTOR_triples1;
  keys[i++] = FUNCTOR_subjects1;
  keys[i++] = FUNCTOR_indexed8;
  keys[i++] = FUNCTOR_predicates1;
  keys[i++] = FUNCTOR_searched_nodes1;
  keys[i++] = FUNCTOR_duplicates1;
  keys[i++] = 0;

					/* setup database */
  init_tables();

  PL_register_foreign("rdf_assert",	3, rdf_assert3,	    0);
  PL_register_foreign("rdf_assert",	4, rdf_assert4,	    0);
  PL_register_foreign("rdf_update",	4, rdf_update,      0);
  PL_register_foreign("rdf_update",	5, rdf_update5,     0);
  PL_register_foreign("rdf_retractall",	3, rdf_retractall3, 0);
  PL_register_foreign("rdf_retractall",	4, rdf_retractall4, 0);
  PL_register_foreign("rdf_subject",	1, rdf_subject,	    NDET);
  PL_register_foreign("rdf",		3, rdf3,	    NDET);
  PL_register_foreign("rdf",		4, rdf4,	    NDET);
  PL_register_foreign("rdf_has",	4, rdf_has,	    NDET);
  PL_register_foreign("rdf_statistics_",1, rdf_statistics,  NDET);
  PL_register_foreign("rdf_generation", 1, rdf_generation,  0);
  PL_register_foreign("rdf_match_label",3, match_label,     0);
  PL_register_foreign("rdf_split_url",  3, split_url,       0);
  PL_register_foreign("rdf_save_db_",   2, rdf_save_db,     0);
  PL_register_foreign("rdf_load_db_",   1, rdf_load_db,     0);
  PL_register_foreign("rdf_reachable",  3, rdf_reachable,   NDET);
#ifdef O_DEBUG
  PL_register_foreign("rdf_debug",      1, rdf_debug,       0);
#endif
}
