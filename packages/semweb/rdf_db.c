/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define WITH_MD5 1
#define WITH_PL_MUTEX 1
#define _GNU_SOURCE 1			/* get rwlocks from glibc */

#ifdef _REENTRANT
#ifdef __WINDOWS__
#include <malloc.h>			/* alloca() */
#define inline __inline
#ifndef SIZEOF_LONG
#define SIZEOF_LONG 4
#endif
#else
#if (!defined(__GNUC__) || defined(__hpux)) && defined(HAVE_ALLOCA_H)
#include <alloca.h>
#endif
#include <errno.h>
#endif
#endif

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include "rdf_db.h"
#include <assert.h>
#include <string.h>
#include <wchar.h>
#include <wctype.h>
#include <ctype.h>
#include "avl.h"
#ifdef WITH_MD5
#include "md5.h"
#include "atom.h"
#include "debug.h"
#include "hash.h"
#include "murmur.h"

#undef UNLOCK

static void md5_triple(triple *t, md5_byte_t *digest);
static void sum_digest(md5_byte_t *digest, md5_byte_t *add);
static void dec_digest(md5_byte_t *digest, md5_byte_t *add);
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The ids form a mask. This must be kept consistent with monitor_mask/2 in
rdf_db.pl!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum
{ EV_ASSERT      = 0x0001,		/* triple */
  EV_ASSERT_LOAD = 0x0002,		/* triple */
  EV_RETRACT     = 0x0004,		/* triple */
  EV_UPDATE      = 0x0008,		/* old, new */
  EV_NEW_LITERAL = 0x0010,		/* literal */
  EV_OLD_LITERAL = 0x0020,		/* literal */
  EV_TRANSACTION = 0x0040,		/* id, begin/end */
  EV_LOAD	 = 0x0080,		/* id, begin/end */
  EV_REHASH	 = 0x0100		/* begin/end */
} broadcast_id;

static void broadcast(broadcast_id id, void *a1, void *a2);


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We now use malloc/free/realloc  calls  with   explicit  sizes  to  allow
maintaining statistics as well as to   prepare  for dealing with special
memory  pools  associated  with  databases.  Using  -DDIRECT_MALLOC  the
library uses plain malloc to facilitate malloc debuggers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef DIRECT_MALLOC

#define rdf_malloc(db, size)		malloc(size)
#define rdf_free(db, ptr, size)     	free(ptr)
#define rdf_realloc(db, ptr, old, new)  realloc(ptr, new)

#else /*DIRECT_MALLOC*/

#if CHECK_MALLOC_SIZES
static void *
rdf_malloc(rdf_db *db, size_t size)
{ size_t bytes = size + sizeof(size_t);
  size_t *ptr = PL_malloc(bytes);

  *ptr++ = size;
  if ( db )
    db->core += size;

  return ptr;
}

static void
rdf_free(rdf_db *db, void *ptr, size_t size)
{ size_t *p = ptr;

  assert(p[-1] == size);

  db->core -= size;
  PL_free(&p[-1]);
}


static void *
rdf_realloc(rdf_db *db, void *ptr, size_t old, size_t new)
{ size_t *p = ptr;
  size_t bytes = new + sizeof(size_t);

  assert(p[-1] == old);
  p = PL_realloc(&p[-1], bytes);
  *p++ = new;
  db->core< += new-old;

  return p;
}

#else /*CHECK_MALLOC_SIZES*/

static void *
rdf_malloc(rdf_db *db, size_t size)
{ if ( db )
    db->core += size;

  return PL_malloc(size);
}

static void
rdf_free(rdf_db *db, void *ptr, size_t size)
{ db->core -= size;

  PL_free(ptr);
}


static void *
rdf_realloc(rdf_db *db, void *ptr, size_t old, size_t new)
{ db->core += new-old;

  return PL_realloc(ptr, new);
}

#endif /*CHECK_MALLOC_SIZES*/
#endif /*DIRECT_MALLOC*/

static functor_t FUNCTOR_literal1;
static functor_t FUNCTOR_literal2;
static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_type_error2;
static functor_t FUNCTOR_domain_error2;
static functor_t FUNCTOR_colon2;

static functor_t FUNCTOR_triples1;
static functor_t FUNCTOR_triples2;
static functor_t FUNCTOR_subjects1;
static functor_t FUNCTOR_predicates1;
static functor_t FUNCTOR_duplicates1;
static functor_t FUNCTOR_literals1;
static functor_t FUNCTOR_subject1;
static functor_t FUNCTOR_predicate1;
static functor_t FUNCTOR_object1;
static functor_t FUNCTOR_graph1;
static functor_t FUNCTOR_indexed8;

static functor_t FUNCTOR_exact1;
static functor_t FUNCTOR_substring1;
static functor_t FUNCTOR_word1;
static functor_t FUNCTOR_prefix1;
static functor_t FUNCTOR_like1;

static functor_t FUNCTOR_symmetric1;
static functor_t FUNCTOR_inverse_of1;
static functor_t FUNCTOR_transitive1;
static functor_t FUNCTOR_rdf_subject_branch_factor1;    /* S --> BF*O */
static functor_t FUNCTOR_rdf_object_branch_factor1;	/* O --> BF*S */
static functor_t FUNCTOR_rdfs_subject_branch_factor1;	/* S --> BF*O */
static functor_t FUNCTOR_rdfs_object_branch_factor1;	/* O --> BF*S */

static functor_t FUNCTOR_searched_nodes1;
static functor_t FUNCTOR_lang2;
static functor_t FUNCTOR_type2;

static functor_t FUNCTOR_gc2;
static functor_t FUNCTOR_rehash2;
static functor_t FUNCTOR_core1;

static functor_t FUNCTOR_assert4;
static functor_t FUNCTOR_retract4;
static functor_t FUNCTOR_update5;
static functor_t FUNCTOR_new_literal1;
static functor_t FUNCTOR_old_literal1;
static functor_t FUNCTOR_transaction2;
static functor_t FUNCTOR_load2;
static functor_t FUNCTOR_rehash1;
static functor_t FUNCTOR_begin1;
static functor_t FUNCTOR_end1;

static atom_t   ATOM_user;
static atom_t	ATOM_exact;
static atom_t	ATOM_prefix;
static atom_t	ATOM_substring;
static atom_t	ATOM_word;
static atom_t	ATOM_like;
static atom_t	ATOM_error;
static atom_t	ATOM_begin;
static atom_t	ATOM_end;

static atom_t	ATOM_subPropertyOf;

static predicate_t PRED_call1;

#define MATCH_EXACT 		0x01	/* exact triple match */
#define MATCH_SUBPROPERTY	0x02	/* Use subPropertyOf relations */
#define MATCH_SRC		0x04	/* Match graph location */
#define MATCH_INVERSE		0x08	/* use symmetric match too */
#define MATCH_QUAL		0x10	/* Match qualifiers too */
#define MATCH_DUPLICATE		(MATCH_EXACT|MATCH_QUAL)

static int update_duplicates_add(rdf_db *db, triple *t);
static void update_duplicates_del(rdf_db *db, triple *t);
static void unlock_atoms(triple *t);
static void lock_atoms(triple *t);
static void unlock_atoms_literal(literal *lit);
static int  update_hash(rdf_db *db);
static int  triple_hash(rdf_db *db, triple *t, int which);
static unsigned long object_hash(triple *t);
static void	reset_db(rdf_db *db);

static void	record_transaction(rdf_db *db,
				   tr_type type, triple *t);
static void	record_md5_transaction(rdf_db *db,
				       graph *src, md5_byte_t *digest);
static void	create_reachability_matrix(rdf_db *db, predicate_cloud *cloud);
static int	get_predicate(rdf_db *db, term_t t, predicate **p);
static predicate_cloud *new_predicate_cloud(rdf_db *db, predicate **p, size_t count);
static int	unify_literal(term_t lit, literal *l);


		 /*******************************
		 *	       LOCKING		*
		 *******************************/

#define RDLOCK(db)			rdlock(&db->lock)
#define WRLOCK(db, allowreaders)	wrlock(&db->lock, allowreaders)
#define LOCKOUT_READERS(db)		lockout_readers(&db->lock)
#define REALLOW_READERS(db)		reallow_readers(&db->lock)
#define WRUNLOCK(db)			unlock(&db->lock, FALSE)
#define RDUNLOCK(db)			unlock(&db->lock, TRUE)
#define LOCK_MISC(db)			lock_misc(&db->lock)
#define UNLOCK_MISC(db)			unlock_misc(&db->lock)
#define INIT_LOCK(db)			init_lock(&db->lock)


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
permission_error(const char *op, const char *type, const char *obj,
		 const char *msg)
{ term_t ex = PL_new_term_ref();
  term_t ctx = PL_new_term_ref();

  if ( msg )
    PL_unify_term(ctx, PL_FUNCTOR_CHARS, "context", 2,
		         PL_VARIABLE,
		         PL_CHARS, msg);

  PL_unify_term(ex, PL_FUNCTOR_CHARS, "error", 2,
		      PL_FUNCTOR_CHARS, "permission_error", 3,
		        PL_CHARS, op,
		        PL_CHARS, type,
		        PL_CHARS, obj,
		      PL_TERM, ctx);

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


static int
get_resource_or_var_ex(term_t t, atom_t *a)
{ if ( PL_get_atom(t, a) )
    return TRUE;
  if ( PL_is_variable(t) )
  { *a = 0L;
    return TRUE;
  }
  if ( PL_is_functor(t, FUNCTOR_literal1) )
    return FALSE;			/* fail on rdf(literal(_), ...) */

  return type_error(t, "atom");
}


static int
get_bool_arg_ex(int a, term_t t, int *val)
{ term_t arg = PL_new_term_ref();

  if ( !PL_get_arg(a, t, arg) )
    return type_error(t, "compound");
  if ( !PL_get_bool(arg, val) )
    return type_error(arg, "bool");

  return TRUE;
}



		 /*******************************
		 *	   DEBUG SUPPORT	*
		 *******************************/

#ifdef O_DEBUG

#define PRT_SRC	0x1

static void
print_literal(literal *lit)
{ switch(lit->objtype)
  { case OBJ_STRING:
      switch(lit->qualifier)
      { case Q_TYPE:
	  Sdprintf("%s^^\"%s\"",
		   PL_atom_chars(lit->value.string),
		   PL_atom_chars(lit->type_or_lang));
	  break;
	case Q_LANG:
	  Sdprintf("%s@\"%s\"",
		   PL_atom_chars(lit->value.string),
		   PL_atom_chars(lit->type_or_lang));
	  break;
	default:
	{ size_t len;
	  const char *s;
	  const wchar_t *w;

	  if ( (s = PL_atom_nchars(lit->value.string, &len)) )
	  { if ( strlen(s) == len )
	      Sdprintf("\"%s\"", s);
	    else
	      Sdprintf("\"%s\" (len=%d)", s, len);
	  } else if ( (w = PL_atom_wchars(lit->value.string, &len)) )
	  { unsigned int i;
	    Sputc('L', Serror);
	    Sputc('"', Serror);
	    for(i=0; i<len; i++)
	    { if ( w[i] < 0x7f )
		Sputc(w[i], Serror);
	      else
		Sfprintf(Serror, "\\\\u%04x", w[i]);
	    }
	    Sputc('"', Serror);
	  }
	  break;
	}
      }
      break;
    case OBJ_INTEGER:
      Sdprintf("%ld", lit->value.integer);
      break;
    case OBJ_DOUBLE:
      Sdprintf("%f", lit->value.real);
      break;
    case OBJ_TERM:
    { fid_t fid = PL_open_foreign_frame();
      term_t term = PL_new_term_ref();

      PL_recorded_external(lit->value.term.record, term);
      PL_write_term(Serror, term, 1200,
		    PL_WRT_QUOTED|PL_WRT_NUMBERVARS|PL_WRT_PORTRAY);
      PL_discard_foreign_frame(fid);
      break;
    }
    default:
      assert(0);
  }
}


static void
print_object(triple *t)
{ if ( t->object_is_literal )
  { print_literal(t->object.literal);
  } else
  { Sdprintf("%s", PL_atom_chars(t->object.resource));
  }
}


static void
print_src(triple *t)
{ if ( t->line == NO_LINE )
    Sdprintf(" [%s]", PL_atom_chars(t->graph));
  else
    Sdprintf(" [%s:%ld]", PL_atom_chars(t->graph), t->line);
}


static void
print_triple(triple *t, int flags)
{ Sdprintf("<%s %s ",
	   PL_atom_chars(t->subject),
	   PL_atom_chars(t->predicate.r->name));
  print_object(t);
  if ( (flags & PRT_SRC) )
    print_src(t);
  Sdprintf(">");
}

#endif

		 /*******************************
		 *	     STORAGE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Our one and only database (for the time being).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static rdf_db *DB;


		 /*******************************
		 *	      LISTS		*
		 *******************************/

static int
add_list(rdf_db *db, list *list, void *value)
{ cell *c;

  for(c=list->head; c; c=c->next)
  { if ( c->value == value )
      return FALSE;			/* already a member */
  }

  c = rdf_malloc(db, sizeof(*c));
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
del_list(rdf_db *db, list *list, void *value)
{ cell *c, *p = NULL;

  for(c=list->head; c; p=c, c=c->next)
  { if ( c->value == value )
    { if ( p )
	p->next = c->next;
      else
	list->head = c->next;

      if ( !c->next )
	list->tail = p;

      rdf_free(db, c, sizeof(*c));

      return TRUE;
    }
  }

  return FALSE;				/* not a member */
}


static void
free_list(rdf_db *db, list *list)
{ cell *c, *n;

  for(c=list->head; c; c=n)
  { n = c->next;
    rdf_free(db, c, sizeof(*c));
  }

  list->head = list->tail = NULL;
}


		 /*******************************
		 *	     ATOM SETS		*
		 *******************************/


#define CHUNKSIZE 1024

typedef struct mchunk
{ struct mchunk *next;
  size_t used;
  char buf[CHUNKSIZE];
} mchunk;

typedef struct
{ avl_tree tree;
  mchunk *node_store;
  mchunk store0;
} atomset;


static void *
alloc_node_atomset(void *ptr, size_t size)
{ void *p;
  atomset *as = ptr;

  assert(size < CHUNKSIZE);

  if ( as->node_store->used + size > CHUNKSIZE )
  { mchunk *ch = malloc(sizeof(mchunk));

    ch->used = 0;
    ch->next = as->node_store;
    as->node_store = ch;
  }

  p = &as->node_store->buf[as->node_store->used];
  as->node_store->used += size;

  return p;
}


static void
free_node_atomset(void *ptr, void *data, size_t size)
{ assert(0);
}


static int
cmp_long_ptr(void *p1, void *p2, NODE type)
{ long *l1 = p1;
  long *l2 = p2;

  return *l1 < *l2 ? -1 : *l1 > *l2 ? 1 : 0;
}


static void
init_atomset(atomset *as)
{ avlinit(&as->tree, as, sizeof(atom_t),
	  cmp_long_ptr,
	  NULL,
	  alloc_node_atomset,
	  free_node_atomset);
  
  as->node_store = &as->store0;
  as->node_store->next = NULL;
  as->node_store->used = 0;
}


static void
destroy_atomset(atomset *as)
{ mchunk *ch, *next;

  for(ch=as->node_store; ch != &as->store0; ch = next)
  { next = ch->next;
    free(ch);
  }
}


static int
add_atomset(atomset *as, atom_t atom)
{ return avlins(&as->tree, &atom) ? FALSE : TRUE;
}


		 /*******************************
		 *	    PREDICATES		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Predicates are represented as first class   citizens  for three reasons:
quickly  answer  on  the  transitive   rdfs:subPropertyOf  relation  for
rdf_hash/3,  keep  track  of  statistics  that   are  useful  for  query
optimization  (#triples,  branching   factor)    and   keep   properties
(inverse/transitive).

To answer the rdfs:subPropertyOf quickly,   predicates  are organised in
`clouds', where a cloud defines a   set  of predicates connected through
rdfs:subPropertyOf triples. The cloud numbers  its members and maintains
a bit-matrix that contains the closure  of the reachability. Initially a
predicate has a simple cloud of size 1. merge_clouds() and split_cloud()
deals with adding  and  deleting   rdfs:subPropertyOf  relations.  These
operations try to modify the clouds that have   no triples, so it can be
done without a rehash. If this fails, the predicates keep their own hash
to make search without rdfs:subPropertyOf  still   possible  (so  we can
avoid frequent updates while loading triples),   sets  the cloud `dirty'
flag and the DB's need_update flag. Queries that need rdfs:subPropertyOf
find the need_update flag,  which   calls  organise_predicates(),  which
cause a rehash if some predicates  have   changed  hash-code  to the new
cloud they have become part of.

TBD: We can do a partial re-hash in that case!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static void
init_pred_table(rdf_db *db)
{ int bytes = sizeof(predicate*)*INITIAL_PREDICATE_TABLE_SIZE;

  db->pred_table = rdf_malloc(db, bytes);
  memset(db->pred_table, 0, bytes);
  db->pred_table_size = INITIAL_PREDICATE_TABLE_SIZE;
}


static predicate *
existing_predicate(rdf_db *db, atom_t name)
{ int hash = atom_hash(name) % db->pred_table_size;
  predicate *p;

  LOCK_MISC(db);
  for(p=db->pred_table[hash]; p; p = p->next)
  { if ( p->name == name )
    { UNLOCK_MISC(db);
      return p;
    }
  }

  UNLOCK_MISC(db);
  return NULL;
}


static predicate *
lookup_predicate(rdf_db *db, atom_t name)
{ int hash = atom_hash(name) % db->pred_table_size;
  predicate *p;
  predicate_cloud *cp;

  LOCK_MISC(db);
  for(p=db->pred_table[hash]; p; p = p->next)
  { if ( p->name == name )
    { UNLOCK_MISC(db);
      return p;
    }
  }
  p = rdf_malloc(db, sizeof(*p));
  memset(p, 0, sizeof(*p));
  p->name = name;
  cp = new_predicate_cloud(db, &p, 1);
  p->hash = cp->hash;
  PL_register_atom(name);
  p->next = db->pred_table[hash];
  db->pred_table[hash] = p;
  db->pred_count++;
  DEBUG(5, Sdprintf("Pred %s (count = %d)\n",
		    PL_atom_chars(name), db->pred_count));
  UNLOCK_MISC(db);

  return p;
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


static int
organise_predicates(rdf_db *db)		/* TBD: rename&move */
{ predicate **ht;
  int i;
  int changed = 0;

  DEBUG(2, Sdprintf("rdf_db: fixing predicate clouds\n"));

  for(i=0,ht = db->pred_table; i<db->pred_table_size; i++, ht++)
  { predicate *p;

    for( p = *ht; p; p = p->next )
    { predicate_cloud *cloud = p->cloud;

      if ( cloud->dirty )
      { predicate **cp;
	int i2;

	for(i2=0, cp = cloud->members; i2 < cloud->size; i2++, cp++)
	{ if ( (*cp)->hash != cloud->hash )
	  { (*cp)->hash = cloud->hash;
	    if ( (*cp)->triple_count > 0 )
	      changed++;
	  }
	}
	cloud->dirty = FALSE;
      }
    }
  }

  return changed;
}


		 /*******************************
		 *	 PREDICATE CLOUDS	*
		 *******************************/

static predicate_cloud *
new_predicate_cloud(rdf_db *db, predicate **p, size_t count)
{ predicate_cloud *cloud = rdf_malloc(db, sizeof(*cloud));
  
  memset(cloud, 0, sizeof(*cloud));
  cloud->hash = db->next_hash++;
  if ( count )
  { int i;
    predicate **p2;

    cloud->size = count;
    cloud->members = rdf_malloc(db, sizeof(predicate*)*count);
    memcpy(cloud->members, p, sizeof(predicate*)*count);
    
    for(i=0, p2=cloud->members; i<cloud->size; i++, p2++)
      (*p2)->cloud = cloud;
  }
  create_reachability_matrix(db, cloud);

  return cloud;
}


static void
free_predicate_cloud(rdf_db *db, predicate_cloud *cloud)
{ if ( cloud->members )
  { rdf_free(db, cloud->members, sizeof(predicate*)*cloud->size);
  }

  rdf_free(db, cloud, sizeof(*cloud));
}


static long
triples_in_predicate_cloud(predicate_cloud *cloud)
{ long triples = 0;
  predicate **p;
  int i;

  for(i=0, p=cloud->members; i<cloud->size; i++, p++)
    triples += (*p)->triple_count;
  
  return triples;
}


/* Add the predicates of c2 to c1 and destroy c2.  Returns c1 */

static predicate_cloud *
append_clouds(rdf_db *db, predicate_cloud *c1, predicate_cloud *c2, int update_hash)
{ predicate **p;
  int i;
    
  for(i=0, p=c2->members; i<c2->size; i++, p++)
  { (*p)->cloud = c1;
    if ( update_hash )
      (*p)->hash = c1->hash;
  }

  if ( c1->size > 0 && c2->size > 0 )
  { c1->members = rdf_realloc(db, c1->members,
			      c1->size*sizeof(predicate*),
			      (c1->size+c2->size)*sizeof(predicate*));
    memcpy(&c1->members[c1->size], c2->members, c2->size*sizeof(predicate*));
    c1->size += c2->size;
    free_predicate_cloud(db, c2);
  } else if ( c2->size > 0 )
  { c1->members = c2->members;
    c1->size = c2->size;
    c2->members = NULL;
    free_predicate_cloud(db, c2);
  } else
  { free_predicate_cloud(db, c2);
  }

  return c1;
}


/* merge two predicate clouds.  If either of them has no triples we
   can do the merge without rehashing the database.  Note that this
   code is only called from addSubPropertyOf().  If c1==c2, we added
   an rdfs:subPropertyOf between two predicates in the same cloud.
   we must still update the matrix, though we could do it a bit more
   efficient.  I doubt this is worth the trouble though.
*/

static predicate_cloud *
merge_clouds(rdf_db *db, predicate_cloud *c1, predicate_cloud *c2)
{ predicate_cloud *cloud;

  if ( c1 != c2 )
  { if ( triples_in_predicate_cloud(c1) == 0 )
    { cloud = append_clouds(db, c2, c1, TRUE);
    } else if ( triples_in_predicate_cloud(c2) == 0 )
    { cloud = append_clouds(db, c1, c2, TRUE);
    } else
    { cloud = append_clouds(db, c1, c2, FALSE);
      db->need_update++;
    }
  } else
  { cloud = c1;
  }

  create_reachability_matrix(db, cloud);
  
  return cloud;
}


/* split a cloud into multiple disjoint clouds.  The first cloud is 
   given the hash of the original, so we only need to update if new
   clouds are created.  Ideally we should se whether it is possible
   to give the orginal hash to the one and only non-empty cloud to
   avoid re-hashing alltogether.
*/

static void
pred_reachable(predicate *start, char *visited, predicate **nodes, int *size)
{ if ( !visited[start->label] )
  { cell *c;

    visited[start->label] = TRUE;
    nodes[(*size)++] = start;
    for(c=start->subPropertyOf.head; c; c=c->next)
      pred_reachable(c->value, visited, nodes, size);
    for(c=start->siblings.head; c; c=c->next)
      pred_reachable(c->value, visited, nodes, size);
  }
}


static int
split_cloud(rdf_db *db, predicate_cloud *cloud,
	    predicate_cloud **parts, int size)
{ char *done        = alloca(cloud->size*sizeof(char));
  predicate **graph = alloca(cloud->size*sizeof(predicate*));
  int found = 0;
  int i;
  
  memset(done, 0, cloud->size*sizeof(char));
  for(i=0; i<cloud->size; i++)
  { if ( !done[i] )
    { predicate *start = cloud->members[i];
      predicate_cloud *new_cloud;
      int gsize = 0;

      pred_reachable(start, done, graph, &gsize);
      new_cloud = new_predicate_cloud(db, graph, gsize);
      if ( found == 0 )
      { new_cloud->hash = cloud->hash;
      } else
      { new_cloud->dirty = TRUE;	/* preds come from another cloud */
	db->need_update++;
      }
      parts[found++] = new_cloud;
    }
  }

  free_predicate_cloud(db, cloud);

  return found;
}


static unsigned long
predicate_hash(predicate *p)
{ return p->hash;
}


static void
addSubPropertyOf(rdf_db *db, predicate *sub, predicate *super)
{ /*DEBUG(2, Sdprintf("addSubPropertyOf(%s, %s)\n", pname(sub), pname(super)));*/

  if ( add_list(db, &sub->subPropertyOf, super) )
  { add_list(db, &super->siblings, sub);
    merge_clouds(db, sub->cloud, super->cloud);
  }
}


/* deleting an rdfs:subPropertyOf.  This is a bit naughty.  If the
   cloud is still connected we only need to refresh the reachability
   matrix.  Otherwise the cloud breaks in maximum two clusters.  We
   can decide to leave it as is, which saves a re-hash of the triples
   but harms indexing.  Alternative we can create a new cloud for one
   of the clusters and re-hash.
*/

static void
delSubPropertyOf(rdf_db *db, predicate *sub, predicate *super)
{ if ( del_list(db, &sub->subPropertyOf, super) )
  { del_list(db, &super->siblings, sub);
 /* if ( not worth the trouble )
      create_reachability_matrix(db, sub->cloud);
    else */
    { predicate_cloud *parts[2];
      split_cloud(db, sub->cloud, parts, 2);
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Reachability matrix.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define WBITSIZE (sizeof(int)*8)

static int
byte_size_bitmatrix(int w, int h)
{ int wsize = ((w*h)+WBITSIZE-1)/WBITSIZE;

  return (int)(long)&((bitmatrix*)NULL)->bits[wsize];
}


static bitmatrix *
alloc_bitmatrix(rdf_db *db, int w, int h)
{ int size = byte_size_bitmatrix(w, h);
  bitmatrix *m = PL_malloc(size);

  memset(m, 0, size);
  m->width = w;
  m->heigth = h;

  return m;
}


static void
free_bitmatrix(rdf_db *db, bitmatrix *bm)
{ int size = byte_size_bitmatrix(bm->width, bm->heigth);

  rdf_free(db, bm, size);
}


#undef setbit				/* conflict in HPUX 11.23 */

static void
setbit(bitmatrix *m, int i, int j)
{ int ij = m->width*i+j;
  int word = ij/WBITSIZE;
  int bit  = ij%WBITSIZE;

  m->bits[word] |= 1<<bit;
}


static int
testbit(bitmatrix *m, int i, int j)
{ int ij = m->width*i+j;
  int word = ij/WBITSIZE;
  int bit  = ij%WBITSIZE;

  return ((m->bits[word] & (1<<bit)) != 0);
}


static int
label_predicate_cloud(predicate_cloud *cloud)
{ predicate **p;
  int i;

  for(i=0, p=cloud->members; i<cloud->size; i++, p++)
    (*p)->label = i;

  return i;
}


static void
fill_reachable(bitmatrix *bm, predicate *p0, predicate *p)
{ if ( !testbit(bm, p0->label, p->label) )
  { cell *c;

    DEBUG(1, Sdprintf("    Reachable [%s (%d)]\n", pname(p), p->label));
    setbit(bm, p0->label, p->label);
    for(c = p->subPropertyOf.head; c; c=c->next)
      fill_reachable(bm, p0, c->value);
  }
}


static void
create_reachability_matrix(rdf_db *db, predicate_cloud *cloud)
{ bitmatrix *m = alloc_bitmatrix(db, cloud->size, cloud->size);
  predicate **p;
  int i;

  label_predicate_cloud(cloud);
  for(i=0, p=cloud->members; i<cloud->size; i++, p++)
  { DEBUG(1, Sdprintf("Reachability for %s (%d)\n", pname(*p), (*p)->label));

    fill_reachable(m, *p, *p);
  }

  if ( cloud->reachable )
    free_bitmatrix(db, cloud->reachable);

  cloud->reachable = m;
}


static int
isSubPropertyOf(predicate *sub, predicate *p)
{ if ( sub->cloud == p->cloud )
    return testbit(sub->cloud->reachable, sub->label, p->label);

  return FALSE;
}

		 /*******************************
		 *   PRINT PREDICATE HIERARCHY	*
		 *******************************/

static void
print_reachability_cloud(predicate *p)
{ int x, y;
  predicate_cloud *cloud = p->cloud;

  Sdprintf("Reachability matrix:\n");
  for(x=0; x<cloud->reachable->width; x++)
    Sdprintf("%d", x%10);
  Sdprintf("\n");
  for(y=0; y<cloud->reachable->heigth; y++)
  { for(x=0; x<cloud->reachable->width; x++)
    { if ( testbit(cloud->reachable, x, y) )
	Sdprintf("X");
      else
	Sdprintf(".");
    }

    Sdprintf(" %2d %s\n", y, PL_atom_chars(cloud->members[y]->name));
    assert(cloud->members[y]->label == y);
  }
}


static foreign_t
rdf_print_predicate_cloud(term_t t)
{ predicate *p;
  rdf_db *db = DB;

  if ( !get_predicate(db, t, &p) )
    return FALSE;

  print_reachability_cloud(p);

  return TRUE;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Branching  factors  are  crucial  in  ordering    the  statements  of  a
conjunction. These functions compute  the   average  branching factor in
both directions ("subject --> P  -->  object"   and  "object  -->  P -->
subject") by determining the number of unique   values at either side of
the predicate. This number  is  only   recomputed  if  it  is considered
`dirty'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
update_predicate_counts(rdf_db *db, predicate *p, int which)
{ long total = 0;

  if ( which == DISTINCT_DIRECT )
  { long changed = abs(p->triple_count - p->distinct_updated[DISTINCT_DIRECT]);

    if ( changed < p->distinct_updated[DISTINCT_DIRECT] )
      return TRUE;

    if ( p->triple_count == 0 )
    { p->distinct_count[which]    = 0;
      p->distinct_subjects[which] = 0;
      p->distinct_objects[which]  = 0;

      return TRUE;
    }
  } else
  { long changed = db->generation - p->distinct_updated[DISTINCT_SUB];

    if ( changed < p->distinct_count[DISTINCT_SUB] )
      return TRUE;
  }

  if ( !update_hash(db) )
    return FALSE;

  { atomset subject_set;
    atomset object_set;
    triple t;
    triple *byp;

    memset(&t, 0, sizeof(t));
    t.predicate.r = p;
    t.indexed |= BY_P;

    init_atomset(&subject_set);
    init_atomset(&object_set);
    for(byp = db->table[t.indexed][triple_hash(db, &t, t.indexed)];
	byp;
	byp = byp->next[t.indexed])
    { if ( !byp->erased && !byp->is_duplicate )
      { if ( (which == DISTINCT_DIRECT && byp->predicate.r == p) ||
	     (which != DISTINCT_DIRECT && isSubPropertyOf(byp->predicate.r, p)) )
	{ total++;
	  add_atomset(&subject_set, byp->subject);
	  add_atomset(&object_set, object_hash(byp)); /* NOTE: not exact! */
	}
      }
    }

    p->distinct_count[which]    = total;
    p->distinct_subjects[which] = subject_set.tree.count;
    p->distinct_objects[which]  = object_set.tree.count;

    destroy_atomset(&subject_set);
    destroy_atomset(&object_set);

    if ( which == DISTINCT_DIRECT )
      p->distinct_updated[DISTINCT_DIRECT] = total;
    else
      p->distinct_updated[DISTINCT_SUB] = db->generation;

    DEBUG(1, Sdprintf("%s: distinct subjects (%s): %ld, objects: %ld\n",
		      PL_atom_chars(p->name),
		      (which == DISTINCT_DIRECT ? "rdf" : "rdfs"),
		      p->distinct_subjects[which],
		      p->distinct_objects[which]));
  }

  return TRUE;
}


static void
invalidate_distinct_counts(rdf_db *db)
{ predicate **ht;
  int i;

  for(i=0,ht = db->pred_table; i<db->pred_table_size; i++, ht++)
  { predicate *p;

    for( p = *ht; p; p = p->next )
    { p->distinct_updated[DISTINCT_SUB] = 0;
      p->distinct_count[DISTINCT_SUB] = 0;
      p->distinct_subjects[DISTINCT_SUB] = 0;
      p->distinct_objects[DISTINCT_SUB] = 0;
    }
  }
}


static double
subject_branch_factor(rdf_db *db, predicate *p, int which)
{ if ( !update_predicate_counts(db, p, which) )
    return FALSE;

  if ( p->distinct_subjects[which] == 0 )
    return 0.0;				/* 0 --> 0 */

  return (double)p->distinct_count[which] /
         (double)p->distinct_subjects[which];
}


static double
object_branch_factor(rdf_db *db, predicate *p, int which)
{ if ( !update_predicate_counts(db, p, which) )
    return FALSE;

  if ( p->distinct_objects[which] == 0 )
    return 0.0;				/* 0 --> 0 */

  return (double)p->distinct_count[which] /
         (double)p->distinct_objects[which];
}




		 /*******************************
		 *	   NAMED GRAPHS		*
		 *******************************/

/* MT: all calls must be locked
*/

static void
init_graph_table(rdf_db *db)
{ int bytes = sizeof(predicate*)*INITIAL_GRAPH_TABLE_SIZE;

  db->graph_table = rdf_malloc(db, bytes);
  memset(db->graph_table, 0, bytes);
  db->graph_table_size = INITIAL_GRAPH_TABLE_SIZE;
}


static graph *
lookup_graph(rdf_db *db, atom_t name, int create)
{ int hash = atom_hash(name) % db->graph_table_size;
  graph *src;

  LOCK_MISC(db);
  for(src=db->graph_table[hash]; src; src = src->next)
  { if ( src->name == name )
    { UNLOCK_MISC(db);
      return src;
    }
  }

  if ( !create )
  { UNLOCK_MISC(db);
    return NULL;
  }

  src = rdf_malloc(db, sizeof(*src));
  memset(src, 0, sizeof(*src));
  src->name = name;
  src->md5 = TRUE;
  PL_register_atom(name);
  src->next = db->graph_table[hash];
  db->graph_table[hash] = src;
  UNLOCK_MISC(db);

  return src;
}


static void
erase_graphs(rdf_db *db)
{ graph **ht;
  int i;

  for(i=0,ht = db->graph_table; i<db->graph_table_size; i++, ht++)
  { graph *src, *n;

    for( src = *ht; src; src = n )
    { n = src->next;

      PL_unregister_atom(src->name);
      if ( src->source )
	PL_unregister_atom(src->source);
      rdf_free(db, src, sizeof(*src));
    }

    *ht = NULL;
  }

  db->last_graph = NULL;
}


static void
register_graph(rdf_db *db, triple *t)
{ graph *src;

  if ( !t->graph )
    return;

  if ( db->last_graph && db->last_graph->name == t->graph )
  { src = db->last_graph;
  } else
  { src = lookup_graph(db, t->graph, TRUE);
    db->last_graph = src;
  } 

  src->triple_count++;
#ifdef WITH_MD5
  if ( src->md5 )
  { md5_byte_t digest[16];
    md5_triple(t, digest);
    sum_digest(src->digest, digest);
  }
#endif
}


static void
unregister_graph(rdf_db *db, triple *t)
{ graph *src;

  if ( !t->graph )
    return;

  if ( db->last_graph && db->last_graph->name == t->graph )
  { src = db->last_graph;
  } else
  { src = lookup_graph(db, t->graph, TRUE);
    db->last_graph = src;
  } 

  src->triple_count--;
#ifdef WITH_MD5
  if ( src->md5 )
  { md5_byte_t digest[16];
    md5_triple(t, digest);
    dec_digest(src->digest, digest);
  }
#endif
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rdf_graphs_(-ListOfGraphs)

Return a list holding the names  of   all  currently defined graphs. We
return a list to avoid the need for complicated long locks.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
rdf_graphs(term_t list)
{ int i;
  term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();
  rdf_db *db = DB;

  if ( !RDLOCK(db) )
    return FALSE;
  for(i=0; i<db->graph_table_size; i++)
  { graph *src;

    for(src=db->graph_table[i]; src; src = src->next)
    { if ( !PL_unify_list(tail, head, tail) ||
	   !PL_unify_atom(head, src->name) )
      { RDUNLOCK(db);
	return FALSE;
      }
    }
  }
  RDUNLOCK(db);

  return PL_unify_nil(tail);
}


static foreign_t
rdf_graph_source(term_t graph_name, term_t source)
{ atom_t gn;
  int rc = FALSE;
  rdf_db *db = DB;

  if ( !get_atom_or_var_ex(graph_name, &gn) )
    return FALSE;

  if ( gn )
  { graph *s;

    if ( !RDLOCK(db) )
      return FALSE;
    if ( (s = lookup_graph(db, gn, FALSE)) && s->source)
    { rc = PL_unify_atom(source, s->source);
    }
    RDUNLOCK(db);
  } else
  { atom_t src;

    if ( get_atom_ex(source, &src) )
    { int i;
      graph **ht;

      if ( !RDLOCK(db) )
	return FALSE;

      for(i=0,ht = db->graph_table; i<db->graph_table_size; i++, ht++)
      { graph *s;
      
	for( s = *ht; s; s = s->next )
	{ if ( s->source == src )
	    rc = PL_unify_atom(graph_name, s->name);
	}
      }

      RDUNLOCK(db);
    }
  }

  return rc;
}


static foreign_t
rdf_set_graph_source(term_t graph_name, term_t source)
{ atom_t gn, src;
  int rc = FALSE;
  rdf_db *db = DB;
  graph *s;

  if ( !get_atom_ex(graph_name, &gn) ||
       !get_atom_ex(source, &src) )
    return FALSE;

  if ( !RDLOCK(db) )
    return FALSE;
  if ( (s = lookup_graph(db, gn, TRUE)) )
  { if ( s->source != src )
    { if ( s->source )
	PL_unregister_atom(s->source);
      s->source = src;
      PL_register_atom(s->source);
    }
    rc = TRUE;
  }
  RDUNLOCK(db);

  return rc;
}

		 /*******************************
		 *	     LITERALS		*
		 *******************************/

#define LITERAL_EX_MAGIC 0x2b97e881

typedef struct literal_ex
{ literal  *literal;
  atom_info atom;
#ifdef O_SECURE
  long	    magic;
#endif
} literal_ex;


static inline void
prepare_literal_ex(literal_ex *lex)
{ SECURE(lex->magic = 0x2b97e881);

  if ( lex->literal->objtype == OBJ_STRING )
  { lex->atom.handle = lex->literal->value.string;
    lex->atom.resolved = FALSE;
  }
}


static literal *
new_literal(rdf_db *db)
{ literal *lit = rdf_malloc(db, sizeof(*lit));
  memset(lit, 0, sizeof(*lit));
  lit->references = 1;

  return lit;
}


static void
free_literal(rdf_db *db, literal *lit)
{ if ( --lit->references == 0 )
  { unlock_atoms_literal(lit);

    if ( lit->shared && !db->resetting )
    { literal_ex lex;

      lit->shared = FALSE;
      broadcast(EV_OLD_LITERAL, lit, NULL);
      DEBUG(2,
	    Sdprintf("Delete %p from literal table: ", lit);
	    print_literal(lit);
	    Sdprintf("\n"));

      lex.literal = lit;
      prepare_literal_ex(&lex);

      if ( !avldel(&db->literals, &lex) )
      { Sdprintf("Failed to delete %p (size=%ld): ", lit, db->literals.count);
	print_literal(lit);
	Sdprintf("\n");
	assert(0);
      }
    }

    if ( lit->objtype == OBJ_TERM )
    { if ( lit->term_loaded )
	rdf_free(db, lit->value.term.record, lit->value.term.len);
      else
	PL_erase_external(lit->value.term.record);
    }
    rdf_free(db, lit, sizeof(*lit));
  }
}


static literal *
copy_literal(rdf_db *db, literal *lit)
{ lit->references++;
  return lit;
}


static void
alloc_literal_triple(rdf_db *db, triple *t)
{ if ( !t->object_is_literal )
  { t->object.literal = new_literal(db);
    t->object_is_literal = TRUE;
  }
}


static void
lock_atoms_literal(literal *lit)
{ if ( !lit->atoms_locked )
  { lit->atoms_locked = TRUE;

    switch(lit->objtype)
    { case OBJ_STRING:
	PL_register_atom(lit->value.string);
	if ( lit->qualifier )
	  PL_register_atom(lit->type_or_lang);
	break;
    }
  }
}


static void
unlock_atoms_literal(literal *lit)
{ if ( lit->atoms_locked )
  { lit->atoms_locked = FALSE;

    switch(lit->objtype)
    { case OBJ_STRING:
	PL_unregister_atom(lit->value.string);
	if ( lit->qualifier )
	  PL_unregister_atom(lit->type_or_lang);
	break;
    }
  }
}


		 /*******************************
		 *	     LITERAL DB		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
compare_literals() sorts literals.  Ordering is defined as:

	* Numeric literals < string literals < term literals
	* Numeric literals (int and float) are sorted by value
	* String literals are sorted alhabetically
		- case independent, but uppercase before lowercase
		- locale (strcoll) sorting?
		- delete dyadrics
		- first on string, then on type, then on language
	* Terms are sorted on Prolog standard order of terms
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
compare_literals(void *p1, void *p2, NODE type)
{ literal_ex *lex = p1;
  literal *l1 = lex->literal;
  literal *l2 = *(literal**)p2;

  SECURE(assert(lex->magic == LITERAL_EX_MAGIC));

  if ( l1->objtype == l2->objtype )
  { switch(l1->objtype)
    { case OBJ_INTEGER:
      { int64_t v1 = l1->value.integer;
	int64_t v2 = l2->value.integer;
	return v1 < v2 ? -1 : v1 > v2 ? 1 : 0;
      }
      case OBJ_DOUBLE:
      { double v1 = l1->value.real;
	double v2 = l2->value.real;
	return v1 < v2 ? -1 : v1 > v2 ? 1 : 0;
      }
      case OBJ_STRING:
      { int rc = cmp_atom_info(&lex->atom, l2->value.string);
	
	if ( rc == 0 )
	{ if ( l1->qualifier == l2->qualifier )
	    return cmp_atoms(l1->type_or_lang, l2->type_or_lang);
	  return l1->qualifier - l2->qualifier;
	}
	return rc;
      }
      case OBJ_TERM:
      { fid_t fid = PL_open_foreign_frame();
	term_t t1 = PL_new_term_ref();
	term_t t2 = PL_new_term_ref();
	int rc;

	PL_recorded_external(l1->value.term.record, t1); /* can also be handled in literal_ex */
	PL_recorded_external(l2->value.term.record, t2);
	rc = PL_compare(t1, t2);

	PL_discard_foreign_frame(fid);
	return rc;
      }
      default:
	assert(0);
        return 0;
    }
  } else if ( l1->objtype == OBJ_INTEGER && l2->objtype == OBJ_DOUBLE )
  { double v1 = (double)l1->value.integer;
    double v2 = l2->value.real;
    return v1 < v2 ? -1 : v1 > v2 ? 1 : -1;
  } else if ( l1->objtype == OBJ_DOUBLE && l2->objtype == OBJ_INTEGER )
  { double v1 = l1->value.real;
    double v2 = (double)l2->value.integer;
    return v1 < v2 ? -1 : v1 > v2 ? 1 : 1;
  } else
  { return l1->objtype - l2->objtype;
  }
}


static void*
avl_malloc(void *ptr, size_t size)
{ return rdf_malloc(ptr, size);
}


static void
avl_free(void *ptr, void *data, size_t size)
{ rdf_free(ptr, data, size);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create the sorted literal tree. Note  that   we  do  not register a free
handler  for  the  tree  as  nodes   are  either  already  destroyed  by
free_literal() or by rdf_reset_db().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
init_literal_table(rdf_db *db)
{ avlinit(&db->literals,
	  db, sizeof(literal*),
	  compare_literals,
	  NULL,
	  avl_malloc,
	  avl_free);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
share_literal() takes a literal  and  replaces   it  with  one  from the
literal database if there is a match.   On a match, the argument literal
is destroyed. Without a match it adds   the  literal to the database and
returns it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static literal *
share_literal(rdf_db *db, literal *from)
{ literal **data;
  literal_ex lex;

  lex.literal = from;
  prepare_literal_ex(&lex);

  if ( (data = avlins(&db->literals, &lex)) )
  { literal *l2 = *data;

    DEBUG(2,
	  Sdprintf("Replace %p by %p:\n", from, l2);
	  Sdprintf("\tfrom: "); print_literal(from);
	  Sdprintf("\n\tto: "); print_literal(l2);
	  Sdprintf("\n"));
    
    l2->references++;
    free_literal(db, from);

    return l2;
  } else
  { DEBUG(2,
	  Sdprintf("Insert %p into literal table: ", from);
	  print_literal(from);
	  Sdprintf("\n"));

    from->shared = TRUE;
    broadcast(EV_NEW_LITERAL, from, NULL);
    return from;
  }
}


#ifdef O_SECURE
static literal **
add_literals(AVLtree node, literal **p)
{ literal **litp;

  if ( node->subtree[LEFT] )
    p = add_literals(node->subtree[LEFT], p);
  litp = (literal**)node->data;
  *p++ = *litp;
  if ( node->subtree[RIGHT] )
    p = add_literals(node->subtree[RIGHT], p);

  return p;
}


static foreign_t
check_transitivity()
{ rdf_db *db = DB;
  literal **array = malloc(sizeof(literal*)*db->literals.count);
  literal **p = array;
  int i,j;

  add_literals(db->literals.root, p);
  Sdprintf("Checking %ld literals ...\n", db->literals.count);

  for(i=0; i<db->literals.count; i++)
  { int end;

    Sdprintf("\r%6ld", i);
    end = i+100;
    if ( end > db->literals.count )
      end = db->literals.count;

    for(j=i+1; j<end; j++)
    { literal_ex lex;

      lex.literal = &array[i];
      prepare_literal_ex(&lex);

      if ( compare_literals(&lex, &array[j], IS_NULL) >= 0 )
      { Sdprintf("\nERROR: i,j=%d,%d: ", i, j);
	print_literal(array[i]);
	Sdprintf(" >= ");
	print_literal(array[j]);
	Sdprintf("\n");
      }
    }
  }

  free(array);

  return TRUE;
}


static void
dump_lnode(AVLtree node)
{ literal **litp;

  if ( node->subtree[LEFT] )
    dump_lnode(node->subtree[LEFT]);
  litp = (literal**)node->data;
  print_literal(*litp);
  Sdprintf("\n");
  if ( node->subtree[RIGHT] )
    dump_lnode(node->subtree[RIGHT]);
}

static foreign_t
dump_literals()
{ rdf_db *db = DB;
  
  dump_lnode(db->literals.root);
  return TRUE;
}
#endif



		 /*******************************
		 *	      TRIPLES		*
		 *******************************/

static void
init_tables(rdf_db *db)
{ int i;
  int bytes = sizeof(triple*)*INITIAL_TABLE_SIZE;
  int cbytes = sizeof(int)*INITIAL_TABLE_SIZE;

  db->table[0] = &db->by_none;
  db->tail[0]  = &db->by_none_tail;

  for(i=BY_S; i<=BY_OP; i++)
  { if ( i == BY_SO )
      continue;

    db->table[i] = rdf_malloc(db, bytes);
    memset(db->table[i], 0, bytes);
    db->tail[i] = rdf_malloc(db, bytes);
    memset(db->tail[i], 0, bytes);
    db->counts[i] = rdf_malloc(db, cbytes);
    memset(db->counts[i], 0, cbytes);
    db->table_size[i] = INITIAL_TABLE_SIZE;
  }

  init_pred_table(db);
  init_graph_table(db);
  init_literal_table(db);
}


static rdf_db *
new_db()
{ rdf_db *db = rdf_malloc(NULL, sizeof(*db));

  memset(db, 0, sizeof(*db));
  INIT_LOCK(db);
  init_tables(db);

  return db;
}


static triple *
new_triple(rdf_db *db)
{ triple *t = rdf_malloc(db, sizeof(*t));
  memset(t, 0, sizeof(*t));
  t->allocated = TRUE;

  return t;
}


static void
free_triple(rdf_db *db, triple *t)
{ unlock_atoms(t);

  if ( t->object_is_literal && t->object.literal ) 
    free_literal(db, t->object.literal);

  if ( t->allocated )
    rdf_free(db, t, sizeof(*t));
}


#define HASHED 0x80000000

static unsigned int
literal_hash(literal *lit)
{ if ( lit->hash & HASHED )
  { return lit->hash;
  } else
  { unsigned int hash;

    switch(lit->objtype)
    { case OBJ_STRING:
	hash = atom_hash_case(lit->value.string);
        break;
      case OBJ_INTEGER:
      case OBJ_DOUBLE:
	hash = rdf_murmer_hash(&lit->value.integer,
			       sizeof(lit->value.integer),
			       MURMUR_SEED);
        break;
      case OBJ_TERM:
	hash = rdf_murmer_hash(lit->value.term.record,
			       lit->value.term.len,
			       MURMUR_SEED);
	break;
      default:
	assert(0);
	return 0;
    }

    lit->hash = (hash | HASHED);
    return lit->hash;
  }
}


static unsigned long
object_hash(triple *t)
{ if ( t->object_is_literal )
  { return literal_hash(t->object.literal);
  } else
  { return atom_hash(t->object.resource);
  }
}


static int
triple_hash(rdf_db *db, triple *t, int which)
{ unsigned long v;

  switch(which)
  { case BY_NONE:
      return 0;
    case BY_S:
      v = atom_hash(t->subject);
      break;
    case BY_P:
      v = predicate_hash(t->predicate.r);
      break;
    case BY_O:
      v = object_hash(t);
      break;
    case BY_SP:
      v = atom_hash(t->subject) ^ predicate_hash(t->predicate.r);
      break;
    case BY_OP:
      v = predicate_hash(t->predicate.r) ^ object_hash(t);
      break;
    default:
      v = 0;				/* make compiler silent */
      assert(0);
  }

  return (int)(v % (long)db->table_size[which]);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
by_inverse[] returns the index key to use   for inverse search as needed
to realise symmetric and inverse predicates.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int by_inverse[8] =
{ BY_NONE,				/* BY_NONE = 0 */
  BY_O,					/* BY_S    = 1 */
  BY_P,					/* BY_P    = 2 */
  BY_OP,				/* BY_SP   = 3 */
  BY_S,					/* BY_O    = 4 */
  BY_SO,				/* BY_SO   = 5 */
  BY_SP,				/* BY_OP   = 6 */
  BY_SPO,				/* BY_SPO  = 7 */
};


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
triple *first(atom_t subject)
    Find the first triple on subject.  The first is marked to generate a
    unique subjects quickly;
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static triple *
first(rdf_db *db, atom_t subject)
{ triple *t, tmp;
  int hash;

  tmp.subject = subject;
  hash = triple_hash(db, &tmp, BY_S);
  
  for(t=db->table[BY_S][hash]; t; t = t->next[BY_S])
  { if ( t->subject == subject && !t->erased )
      return t;
  }

  return NULL;
}


static void
link_triple_hash(rdf_db *db, triple *t)
{ int i;

  for(i=1; i<=BY_OP; i++)
  { if ( db->table[i] )
    { int hash = triple_hash(db, t, i);

      if ( db->tail[i][hash] )
      { db->tail[i][hash]->next[i] = t;
      } else
      { db->table[i][hash] = t;
      }
      db->tail[i][hash] = t;
      db->counts[i][hash]++;
    }
  }
}


/* MT: must be locked by caller */

static void
link_triple_silent(rdf_db *db, triple *t)
{ triple *one;

  if ( t->resolve_pred )
  { t->predicate.r = lookup_predicate(db, t->predicate.u);
    t->resolve_pred = FALSE;
  }

  if ( db->by_none_tail )
    db->by_none_tail->next[BY_NONE] = t;
  else
    db->by_none = t;
  db->by_none_tail = t;

  link_triple_hash(db, t);
  if ( t->object_is_literal )
    t->object.literal = share_literal(db, t->object.literal);

  if ( update_duplicates_add(db, t) )
    goto ok;				/* is a duplicate */

					/* keep track of subjects */
  one = first(db, t->subject);
  if ( !one->first )
  { one->first = TRUE;
    db->subjects++;
  }

					/* keep track of subPropertyOf */
  if ( t->predicate.r->name == ATOM_subPropertyOf &&
       t->object_is_literal == FALSE )
  { predicate *me    = lookup_predicate(db, t->subject);
    predicate *super = lookup_predicate(db, t->object.resource);

    addSubPropertyOf(db, me, super);
  }

ok:
  db->created++;
  t->predicate.r->triple_count++;
  register_graph(db, t);
}


static inline void
link_triple(rdf_db *db, triple *t)
{ link_triple_silent(db, t);
  broadcast(EV_ASSERT, t, NULL);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rehash_triples()

Relink the triples in the hash-chains after the hash-keys for properties
have changed or the tables have  been   resized.  The caller must ensure
there are no active queries and the tables are of the proper size.

At the same time, this predicate actually removes erased triples.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static long
tbl_size(long triples)
{ long s0 = 1024;

  triples /= MIN_HASH_FACTOR;

  while(s0 < triples)
    s0 *= 2;

  return s0;
}


static void
rehash_triples(rdf_db *db)
{ int i;
  triple *t, *t2;
  long count = db->created - db->freed;
  long tsize = tbl_size(count);

  DEBUG(1, Sdprintf("(%ld triples; %ld entries) ...", count, tsize));
  broadcast(EV_REHASH, (void*)ATOM_begin, NULL);

  for(i=1; i<INDEX_TABLES; i++)
  { if ( db->table[i] )
    { long bytes   = sizeof(triple*) * tsize;
      long cbytes  = sizeof(int)     * tsize;
      long obytes  = sizeof(triple*) * db->table_size[i];
      long ocbytes = sizeof(int)     * db->table_size[i];

      db->table[i]  = rdf_realloc(db, db->table[i],  obytes,  bytes);
      db->tail[i]   = rdf_realloc(db, db->tail[i],   obytes,  bytes);
      db->counts[i] = rdf_realloc(db, db->counts[i], ocbytes, cbytes);
      db->table_size[i] = tsize;

      memset(db->table[i],  0, bytes);
      memset(db->tail[i],   0, bytes);
      memset(db->counts[i], 0, cbytes);
    }
  }

					/* delete leading erased triples */
  for(t=db->by_none; t && t->erased; t=t2)
  { t2 = t->next[BY_NONE];

    free_triple(db, t);
    db->freed++;

    db->by_none = t2;
  }

  for(t=db->by_none; t; t = t2)
  { triple *t3;

    t2 = t->next[BY_NONE];

    for(i=1; i<INDEX_TABLES; i++)
      t->next[i] = NULL;

    assert(t->erased == FALSE);
    link_triple_hash(db, t);

    for( ; t2 && t2->erased; t2=t3 )
    { t3 = t2->next[BY_NONE];

      free_triple(db, t2);
      db->freed++;
    }

    t->next[BY_NONE] = t2;
    if ( !t2 )
      db->by_none_tail = t;
  }

  if ( db->by_none == NULL )
    db->by_none_tail = NULL;

  broadcast(EV_REHASH, (void*)ATOM_end, NULL);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
update_hash(). Note this may be called by  readers and writers, but must
be done only onces and certainly   not concurrently by multiple readers.
Hence we need a seperate lock.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
WANT_GC(rdf_db *db)
{ long dirty = db->erased - db->freed;
  long count = db->created - db->erased;

  if ( dirty > 1000 && dirty > count )
    return TRUE;
  if ( count > db->table_size[1]*MAX_HASH_FACTOR )
    return TRUE;

  return FALSE;
}


static int
update_hash(rdf_db *db)
{ int want_gc = WANT_GC(db);

  if ( want_gc )
    DEBUG(1, Sdprintf("rdf_db: want GC\n"));

  if ( db->need_update || want_gc )
  { LOCK_MISC(db);

    if ( db->need_update )		/* check again */
    { if ( organise_predicates(db) )
      { long t0 = (long)PL_query(PL_QUERY_USER_CPU);

	DEBUG(1, Sdprintf("Re-hash ..."));
	invalidate_distinct_counts(db);
	rehash_triples(db);
	db->generation += (db->created-db->erased);
	db->rehash_count++;
	db->rehash_time += ((double)(PL_query(PL_QUERY_USER_CPU)-t0))/1000.0;
	DEBUG(1, Sdprintf("ok\n"));
      }
      db->need_update = 0;
    } else if ( WANT_GC(db) )
    { long t0 = (long)PL_query(PL_QUERY_USER_CPU);

      DEBUG(1, Sdprintf("rdf_db: GC ..."));
      rehash_triples(db);
      db->gc_count++;
      db->gc_time += ((double)(PL_query(PL_QUERY_USER_CPU)-t0))/1000.0;
      DEBUG(1, Sdprintf("ok\n"));
    }

    UNLOCK_MISC(db);
  }

  return TRUE;
}


/* MT: Must be locked */

static void
erase_triple_silent(rdf_db *db, triple *t)
{ if ( !t->erased )
  { t->erased = TRUE;

    update_duplicates_del(db, t);

    if ( t->predicate.r->name == ATOM_subPropertyOf &&
	 t->object_is_literal == FALSE )
    { predicate *me    = lookup_predicate(db, t->subject);
      predicate *super = lookup_predicate(db, t->object.resource);

      delSubPropertyOf(db, me, super);
    }

    if ( t->first )
    { triple *one = first(db, t->subject);

      if ( one )
	one->first = TRUE;
      else
	db->subjects--;
    }
    db->erased++;
    t->predicate.r->triple_count--;
    unregister_graph(db, t);

    if ( t->object_is_literal )
    { literal *lit = t->object.literal;

      t->object.literal = NULL;
      free_literal(db, lit);		/* TBD: thread-safe? */
    }
  }
}


static inline void
erase_triple(rdf_db *db, triple *t)
{ broadcast(EV_RETRACT, t, NULL);
  erase_triple_silent(db, t);
}


static int
match_object(triple *t, triple *p, unsigned flags)
{ if ( p->object_is_literal )
  { if ( t->object_is_literal )
    { literal *plit = p->object.literal;
      literal *tlit = t->object.literal;

      if ( !plit->objtype )
	return TRUE;

      if ( plit->objtype != tlit->objtype )
	return FALSE;
    
      switch( plit->objtype )
      { case OBJ_STRING:
	  if ( (flags & MATCH_QUAL) )
	  { if ( tlit->qualifier != plit->qualifier )
	      return FALSE;
	  } else
	  { if ( plit->qualifier && tlit->qualifier &&
		 tlit->qualifier != plit->qualifier )
	      return FALSE;
	  }
	  if ( plit->type_or_lang && 
	       tlit->type_or_lang != plit->type_or_lang )
	    return FALSE;
	  if ( plit->value.string )
	  { if ( tlit->value.string != plit->value.string )
	    { if ( p->match )
	      { return match_atoms(p->match,
				   plit->value.string, tlit->value.string);
	      } else
	      { return FALSE;
	      }
	    }
	  }
	  return TRUE;
	case OBJ_INTEGER:
	  return tlit->value.integer == plit->value.integer;
	case OBJ_DOUBLE:
	  return tlit->value.real == plit->value.real;
	case OBJ_TERM:
	  if ( plit->value.term.len != tlit->value.term.len )
	    return FALSE;
	  return memcmp(tlit->value.term.record, plit->value.term.record,
			plit->value.term.len) == 0;
	default:
	  assert(0);
      }
    }
    return FALSE;
  } else
  { if ( p->object.resource )
    { if ( t->object_is_literal || 
	   (p->object.resource != t->object.resource) )
	return FALSE;
    }
  }

  return TRUE;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Match triple t to pattern p.  Erased triples are always skipped.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
match_triples(triple *t, triple *p, unsigned flags)
{ /* DEBUG(3, Sdprintf("match_triple(");
	   print_triple(t, 0);
	   Sdprintf(")\n"));
  */

  if ( t->erased )
    return FALSE;
  if ( p->subject && t->subject != p->subject )
    return FALSE;
  if ( !match_object(t, p, flags) )
    return FALSE;
  if ( flags & MATCH_SRC )
  { if ( p->graph && t->graph != p->graph )
      return FALSE;
    if ( p->line && t->line != p->line )
      return FALSE;
  }
					/* last; may be expensive */
  if ( p->predicate.r && t->predicate.r != p->predicate.r )
  { if ( (flags & MATCH_SUBPROPERTY) )
      return isSubPropertyOf(t->predicate.r, p->predicate.r);
    else
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
			    ['S' <graph-name>]
			    ['F' <graph-source>]
			    ['M' <md5>]
			    {<triple>}
			    'E'

	<magic> 	::= "RDF-dump\n"
	<version> 	::= <integer>

	<md5>		::= <byte>* 		(16 bytes digest)

	<triple>	::= 'T'
	                    <subject>
			    <predicate>
			    <object>
			    <graph>

	<subject>	::= <resource>
	<predicate>	::= <resource>

	<object>	::= "R" <resource>
			  | "L" <atom>

	<resource>	::= <atom>

	<atom>		::= "X" <integer>
			    "A" <string>
			    "W" <utf-8 string>

	<string>	::= <integer><bytes>

	<graph-name>	::= <atom>
	<graph-source>	::= <atom>

	<graph>	::= <graph-file>
			    <line>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define SAVE_MAGIC "RDF-dump\n"
#define SAVE_VERSION 2

typedef struct saved
{ atom_t name;
  long   as;
  struct saved *next;
} saved;


typedef struct save_context
{ saved ** saved_table;
  long     saved_size;
  long     saved_id;
} save_context;


long
next_table_size(long s0)
{ long size = 2;

  while(size < s0)
    size *= 2;

  return size;
}

static void
init_saved(rdf_db *db, save_context *ctx)
{ long size = next_table_size((db->created - db->erased)/8);
  long bytes = size * sizeof(*ctx->saved_table);

  ctx->saved_table = rdf_malloc(db, bytes);
  memset(ctx->saved_table, 0, bytes);
  ctx->saved_size = size;
  ctx->saved_id = 0;
}

static void
destroy_saved(rdf_db *db, save_context *ctx)
{ if ( ctx->saved_table )
  { saved **s = ctx->saved_table;
    int i;

    for(i=0; i<ctx->saved_size; i++, s++)
    { saved *c, *n;

      for(c=*s; c; c = n)
      { n = c->next;
	free(c);
      }
    }
  
    rdf_free(db, ctx->saved_table, ctx->saved_size*sizeof(*ctx->saved_table));
  }
}

#define INT64BITSIZE (sizeof(int64_t)*8)
#define PLMINLONG   ((int64_t)((uint64_t)1<<(INT64BITSIZE-1)))

static void
save_int(IOSTREAM *fd, int64_t n)
{ int m;
  int64_t absn = (n >= 0 ? n : -n);

  if ( n != PLMINLONG )
  { if ( absn < ((intptr_t)1 << 5) )
    { Sputc((int)(n & 0x3f), fd);
      return;
    } else if ( absn < ((intptr_t)1 << 13) )
    { Sputc((int)(((n >> 8) & 0x3f) | (1 << 6)), fd);
      Sputc((int)(n & 0xff), fd);
      return;
    } else if ( absn < ((intptr_t)1 << 21) )
    { Sputc((int)(((n >> 16) & 0x3f) | (2 << 6)), fd);
      Sputc((int)((n >> 8) & 0xff), fd);
      Sputc((int)(n & 0xff), fd);
      return;
    }
  }

  for(m = sizeof(n); ; m--)
  { int b = (int)((absn >> (((m-1)*8)-1)) & 0x1ff);

    if ( b == 0 )
      continue;
    break;
  }

  Sputc(m | (3 << 6), fd);

  for( ; m > 0; m--)
  { int b = (int)((n >> ((m-1)*8)) & 0xff);
    
    Sputc(b, fd);
  }
}


static int
save_atom(rdf_db *db, IOSTREAM *out, atom_t a, save_context *ctx)
{ int hash = atom_hash(a) % ctx->saved_size;
  saved *s;
  size_t len;
  const char *chars;
  unsigned int i;
  const wchar_t *wchars;

  for(s=ctx->saved_table[hash]; s; s= s->next)
  { if ( s->name == a )
    { Sputc('X', out);
      save_int(out, s->as);

      return TRUE;
    }
  }

  s = rdf_malloc(db, sizeof(*s));
  s->name = a;
  s->as = ctx->saved_id++;
  s->next = ctx->saved_table[hash];
  ctx->saved_table[hash] = s;

  if ( (chars = PL_atom_nchars(a, &len)) )
  { Sputc('A', out);
    save_int(out, len);
    for(i=0; i<len; i++, chars++)
      Sputc(*chars&0xff, out);
  } else if ( (wchars = PL_atom_wchars(a, &len)) )
  { IOENC enc = out->encoding;

    Sputc('W', out);
    save_int(out, len);
    out->encoding = ENC_UTF8;
    for(i=0; i<len; i++, wchars++)
    { wint_t c = *wchars;

      SECURE(assert(c>=0 && c <= 0x10ffff));
      Sputcode(c, out);
    } 
    out->encoding = enc;
  } else
    return FALSE;

  return TRUE;
}


#ifdef WORDS_BIGENDIAN
static const int double_byte_order[] = { 7,6,5,4,3,2,1,0 };
#else
static const int double_byte_order[] = { 0,1,2,3,4,5,6,7 };
#endif


static void
write_triple(rdf_db *db, IOSTREAM *out, triple *t, save_context *ctx)
{ Sputc('T', out);

  save_atom(db, out, t->subject, ctx);
  save_atom(db, out, t->predicate.r->name, ctx);

  if ( t->object_is_literal )
  { literal *lit = t->object.literal;

    if ( lit->qualifier )
    { assert(lit->type_or_lang);
      Sputc(lit->qualifier == Q_LANG ? 'l' : 't', out);
      save_atom(db, out, lit->type_or_lang, ctx);
    }

    switch(lit->objtype)
    { case OBJ_STRING:
	Sputc('L', out);
	save_atom(db, out, lit->value.string, ctx);
	break;
      case OBJ_INTEGER:
	Sputc('I', out);
	save_int(out, lit->value.integer); /* TBD: 64-bit int */
	break;
      case OBJ_DOUBLE:
      { double f = lit->value.real;
	unsigned char *cl = (unsigned char *)&f;
	unsigned int i;
	
	Sputc('F', out);
	for(i=0; i<sizeof(double); i++)
	  Sputc(cl[double_byte_order[i]], out);
  
	break;
      }
      case OBJ_TERM:
      { const char *s = lit->value.term.record;
	size_t len = lit->value.term.len;
	
	Sputc('T', out);
	save_int(out, len);
	while(len-- > 0)
	  Sputc(*s++, out);
  
	break;
      }
      default:
	assert(0);
    }
  } else
  { Sputc('R', out);
    save_atom(db, out, t->object.resource, ctx);
  }

  save_atom(db, out, t->graph, ctx);
  save_int(out, t->line);
}


static void
write_source(rdf_db *db, IOSTREAM *out, atom_t src, save_context *ctx)
{ graph *s = lookup_graph(db, src, FALSE);

  if ( s && s->source )
  { Sputc('F', out);
    save_atom(db, out, s->source, ctx);
  }
}


static void
write_md5(rdf_db *db, IOSTREAM *out, atom_t src)
{ graph *s = lookup_graph(db, src, FALSE);

  if ( s )
  { md5_byte_t *p = s->digest;
    int i;

    Sputc('M', out);
    for(i=0; i<16; i++)
      Sputc(*p++, out);
  }
}


static int
save_db(rdf_db *db, IOSTREAM *out, atom_t src)
{ triple *t;
  save_context ctx;

  if ( !RDLOCK(db) )
    return FALSE;
  init_saved(db, &ctx);

  Sfprintf(out, "%s", SAVE_MAGIC);
  save_int(out, SAVE_VERSION);
  if ( src )
  { Sputc('S', out);			/* start of graph header */
    save_atom(db, out, src, &ctx);
    write_source(db, out, src, &ctx);
    write_md5(db, out, src);
  }
  if ( Sferror(out) )
  { RDUNLOCK(db);
    return FALSE;
  }

  for(t = db->by_none; t; t = t->next[BY_NONE])
  { if ( !t->erased &&
	 (!src || t->graph == src) )
    { write_triple(db, out, t, &ctx);
      if ( Sferror(out) )
	return FALSE;
    }
  }
  Sputc('E', out);
  if ( Sferror(out) )
  { RDUNLOCK(db);
    return FALSE;
  }

  destroy_saved(db, &ctx);
  RDUNLOCK(db);

  return TRUE;
}


static foreign_t
rdf_save_db(term_t stream, term_t graph)
{ IOSTREAM *out;
  atom_t src;

  if ( !PL_get_stream_handle(stream, &out) )
    return type_error(stream, "stream");
  if ( !get_atom_or_var_ex(graph, &src) )
    return FALSE;

  return save_db(DB, out, src);
}


static int64_t
load_int(IOSTREAM *fd)
{ int64_t first = Sgetc(fd);
  int bytes, shift, b;

  if ( !(first & 0xc0) )		/* 99% of them: speed up a bit */    
  { first <<= (INT64BITSIZE-6);
    first >>= (INT64BITSIZE-6);

    return first;
  }

  bytes = (int) ((first >> 6) & 0x3);
  first &= 0x3f;

  if ( bytes <= 2 )
  { for( b = 0; b < bytes; b++ )
    { first <<= 8;
      first |= Sgetc(fd) & 0xff;
    }

    shift = (sizeof(first)-1-bytes)*8 + 2;
  } else
  { int m;

    bytes = (int)first;
    first = 0L;

    for(m=0; m<bytes; m++)
    { first <<= 8;
      first |= Sgetc(fd) & 0xff;
    }
    shift = (sizeof(first)-bytes)*8;
  }

  first <<= shift;
  first >>= shift;
  
  return first;
}

typedef struct ld_context
{ long		loaded_id;		/* keep track of atoms */
  atom_t       *loaded_atoms;
  long		atoms_size;
  atom_t	graph;			/* for single-graph files */
  atom_t	graph_source;
  int		has_digest;
  md5_byte_t    digest[16];
  atom_hash    *graph_table;		/* multi-graph file */
} ld_context;


static void
add_atom(rdf_db *db, atom_t a, ld_context *ctx)
{ if ( ctx->loaded_id >= ctx->atoms_size )
  { if ( ctx->atoms_size == 0 )
    { ctx->atoms_size = 1024;
      ctx->loaded_atoms = rdf_malloc(db, sizeof(atom_t)*ctx->atoms_size);
    } else
    { long obytes = sizeof(atom_t)*ctx->atoms_size;
      long  bytes;

      ctx->atoms_size *= 2;
      bytes = sizeof(atom_t)*ctx->atoms_size;
      ctx->loaded_atoms = rdf_realloc(db, ctx->loaded_atoms, obytes, bytes);
    }
  }

  ctx->loaded_atoms[ctx->loaded_id++] = a;
}


static atom_t
load_atom(rdf_db *db, IOSTREAM *in, ld_context *ctx)
{ switch(Sgetc(in))
  { case 'X':
    { intptr_t idx = (intptr_t)load_int(in);
      return ctx->loaded_atoms[idx];
    }
    case 'A':
    { size_t len = (size_t)load_int(in);
      atom_t a;

      if ( len < 1024 )
      { char buf[1024];
	Sfread(buf, 1, len, in);
	a = PL_new_atom_nchars(len, buf);
      } else
      { char *buf = rdf_malloc(db, len);
	Sfread(buf, 1, len, in);
	a = PL_new_atom_nchars(len, buf);
	rdf_free(db, buf, len);
      }

      add_atom(db, a, ctx);
      return a;
    }
    case 'W':
    { int len = (int)load_int(in);
      atom_t a;
      wchar_t buf[1024];
      wchar_t *w;
      IOENC enc = in->encoding;
      int i;

      if ( len < 1024 )
	w = buf;
      else
	w = rdf_malloc(db, len*sizeof(wchar_t));

      in->encoding = ENC_UTF8;
      for(i=0; i<len; i++)
      { w[i] = Sgetcode(in);
	SECURE(assert(w[i]>=0 && w[i] <= 0x10ffff));
      }
      in->encoding = enc;
	  
      a = PL_new_atom_wchars(len, w);
      if ( w != buf )
	rdf_free(db, w, len*sizeof(wchar_t));

      add_atom(db, a, ctx);
      return a;
    }
    default:
    { assert(0);
      return 0;
    }
  }
}


static double
load_double(IOSTREAM *fd)
{ double f;
  unsigned char *cl = (unsigned char *)&f;
  unsigned int i;

  for(i=0; i<sizeof(double); i++)
  { int c = Sgetc(fd);
    
    assert(c != EOF);
      
    cl[double_byte_order[i]] = c;
  }
  
  return f;
}


static triple *
load_triple(rdf_db *db, IOSTREAM *in, ld_context *ctx)
{ triple *t = new_triple(db);
  int c;

  t->subject   = load_atom(db, in, ctx);
  t->predicate.u = load_atom(db, in, ctx);
  t->resolve_pred = TRUE;
  if ( (c=Sgetc(in)) == 'R' )
  { t->object.resource = load_atom(db, in, ctx);
  } else
  { literal *lit = new_literal(db);

    t->object_is_literal = TRUE;
    t->object.literal = lit;

  value:
    switch(c)
    { case 'L':
	lit->objtype = OBJ_STRING;
	lit->value.string = load_atom(db, in, ctx);
	break;
      case 'I':
	lit->objtype = OBJ_INTEGER;
	lit->value.integer = load_int(in);
	break;
      case 'F':
	lit->objtype = OBJ_DOUBLE;
	lit->value.real = load_double(in);
	break;
      case 'T':
      { unsigned int i;
	char *s;
  
	lit->objtype = OBJ_TERM;
	lit->value.term.len = (size_t)load_int(in);
	lit->value.term.record = rdf_malloc(db, lit->value.term.len);
	lit->term_loaded = TRUE;	/* see free_literal() */
	s = (char *)lit->value.term.record;
  
	for(i=0; i<lit->value.term.len; i++)
	  s[i] = Sgetc(in);
  
	break;
      }
      case 'l':
	lit->qualifier = Q_LANG;
	lit->type_or_lang = load_atom(db, in, ctx);
	c = Sgetc(in);
	goto value;
      case 't':
	lit->qualifier = Q_TYPE;
	lit->type_or_lang = load_atom(db, in, ctx);
	c = Sgetc(in);
	goto value;
      default:
	assert(0);
        return NULL;
    }
  }
  t->graph = load_atom(db, in, ctx);
  t->line  = (unsigned long)load_int(in);
  if ( !ctx->graph )
  { if ( !ctx->graph_table )
      ctx->graph_table = new_atom_hash(64);
    add_atom_hash(ctx->graph_table, t->graph);
  }

  return t;
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that we have two types  of   saved  states.  One holding many named
graphs and one holding the content of exactly one named graph.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define LOAD_ERROR ((triple*)(intptr_t)-1)

static triple *
load_db(rdf_db *db, IOSTREAM *in, ld_context *ctx)
{ int version;
  int c;
  triple *list = NULL, *tail = NULL;

  if ( !load_magic(in) )
    return LOAD_ERROR;
  version = (int)load_int(in);

  while((c=Sgetc(in)) != EOF)
  { switch(c)
    { case 'T':
      { triple *t;

	if ( !(t=load_triple(db, in, ctx)) )
	  return FALSE;

	if ( tail )
	{ tail->next[BY_NONE] = t;
	  tail = t;
	} else
	{ list = tail = t;
	}

        break;
      }
					/* file holding exactly one graph */
      case 'S':				/* name of the graph */
      { ctx->graph = load_atom(db, in, ctx);
        break;
      }
      case 'M':				/* MD5 of the graph */
      { int i;

	for(i=0; i<16; i++)
	  ctx->digest[i] = Sgetc(in);
	ctx->has_digest = TRUE;

	break;
      }
      case 'F':				/* file of the graph */
	ctx->graph_source = load_atom(db, in, ctx);
	break;				/* end of one-graph handling */
      case 'E':				/* end of file */
	return list;
      default:
	break;
    }
  }
  
  PL_warning("Illegal RDF triple file");

  return LOAD_ERROR;
}


static int
link_loaded_triples(rdf_db *db, triple *t, ld_context *ctx)
{ long created0 = db->created;
  graph *graph;

  if ( ctx->graph )			/* lookup named graph */
  { graph = lookup_graph(db, ctx->graph, TRUE);
    if ( ctx->graph_source && graph->source != ctx->graph_source )
    { if ( graph->source )
	PL_unregister_atom(graph->source);
      graph->source = ctx->graph_source;
      PL_register_atom(graph->source);
    }

    if ( ctx->has_digest )
    { if ( graph->md5 )
      { if ( db->tr_first )
	{ record_md5_transaction(db, graph, NULL);
	} else
	{ graph->md5 = FALSE;		/* kill repetitive MD5 update */
	}
      } else
      { ctx->has_digest = FALSE;
      }
    }
  } else
  { graph = NULL;
  }


  if ( db->tr_first )			/* loading in a transaction */
  { triple *next;

    for( ; t; t = next )
    { next = t->next[BY_NONE];
      
      t->next[BY_NONE] = NULL;
      lock_atoms(t);
      record_transaction(db, TR_ASSERT, t);
    }
  } else
  { triple *next;

    for( ; t; t = next )
    { next = t->next[BY_NONE];
      
      t->next[BY_NONE] = NULL;
      lock_atoms(t);
      link_triple_silent(db, t);
      broadcast(EV_ASSERT_LOAD, t, NULL);
    }
  }

					/* update the graph info */
  if ( ctx->has_digest )
  { if ( db->tr_first )
    { md5_byte_t *d = rdf_malloc(db, sizeof(ctx->digest));
      memcpy(d, ctx->digest, sizeof(ctx->digest));
      record_md5_transaction(db, graph, d);
    } else
    { sum_digest(graph->digest, ctx->digest);
    }
    graph->md5 = TRUE;
  }

  db->generation += (db->created-created0);

  return TRUE;
}


static int
append_graph_to_list(ptr_hash_node *node, void *closure)
{ atom_t graph = (atom_t)node->value;
  term_t tail  = (term_t)closure;
  term_t head  = PL_new_term_ref();
  int rc;

  rc = (PL_unify_list(tail, head, tail) &&
	PL_unify_atom(head, graph));
  PL_reset_term_refs(head);

  return rc;
}


static foreign_t
rdf_load_db(term_t stream, term_t id, term_t graphs)
{ ld_context ctx;
  rdf_db *db = DB;
  IOSTREAM *in;
  triple *list;
  int rc;

  if ( !PL_get_stream_handle(stream, &in) )
    return type_error(stream, "stream");

  memset(&ctx, 0, sizeof(ctx));
  if ( (list=load_db(db, in, &ctx)) == LOAD_ERROR )
    return FALSE;

  if ( !WRLOCK(db, FALSE) )
    return FALSE;
  broadcast(EV_LOAD, (void*)id, (void*)ATOM_begin);

  if ( (rc=link_loaded_triples(db, list, &ctx)) )
  { if ( ctx.graph_table )
    { term_t tail = PL_copy_term_ref(graphs);

      rc = ( for_atom_hash(ctx.graph_table, append_graph_to_list, (void*)tail) &&
	     PL_unify_nil(tail) );

      destroy_atom_hash(ctx.graph_table);
    } else
    { rc = PL_unify_atom(graphs, ctx.graph);
    }
  }
  broadcast(EV_LOAD, (void*)id, (void*)ATOM_end);
  WRUNLOCK(db);

  PL_release_stream(in);
  if ( ctx.loaded_atoms )
  { atom_t *ap, *ep;

    for(ap=ctx.loaded_atoms, ep=ap+ctx.loaded_id; ap<ep; ap++)
      PL_unregister_atom(*ap);

    rdf_free(db, ctx.loaded_atoms, sizeof(atom_t)*ctx.atoms_size);
  }

  return rc;
}


#ifdef WITH_MD5
		 /*******************************
		 *	     MD5 SUPPORT	*
		 *******************************/

/* md5_type is used to keep the MD5 independent from the internal
   numbers
*/
static const char md5_type[] =
{ 0x0,					/* OBJ_UNKNOWN */
  0x3,					/* OBJ_INTEGER */
  0x4,					/* OBJ_DOUBLE */
  0x2,					/* OBJ_STRING */
  0x5					/* OBJ_TERM */
};

static void
md5_triple(triple *t, md5_byte_t *digest)
{ md5_state_t state;
  size_t len;
  md5_byte_t tmp[2];
  const char *s;
  literal *lit;

  md5_init(&state);
  s = PL_blob_data(t->subject, &len, NULL);
  md5_append(&state, (const md5_byte_t *)s, (int)len);
  md5_append(&state, (const md5_byte_t *)"P", 1);
  s = PL_blob_data(t->predicate.r->name, &len, NULL);
  md5_append(&state, (const md5_byte_t *)s, (int)len);
  tmp[0] = 'O';
  if ( t->object_is_literal )
  { lit = t->object.literal;
    tmp[1] = md5_type[lit->objtype];

    switch(lit->objtype)
    { case OBJ_STRING:
	s = PL_blob_data(lit->value.string, &len, NULL);
	break;
      case OBJ_INTEGER:			/* TBD: byte order issues */
	s = (const char *)&lit->value.integer;
	len = sizeof(lit->value.integer);
	break;
      case OBJ_DOUBLE:
	s = (const char *)&lit->value.real;
	len = sizeof(lit->value.real);
	break;
      case OBJ_TERM:
	s = (const char *)lit->value.term.record;
	len = lit->value.term.len;
	break;
      default:
	assert(0);
    }
  } else
  { s = PL_blob_data(t->object.resource, &len, NULL);
    tmp[1] = 0x1;			/* old OBJ_RESOURCE */
    lit = NULL;
  }
  md5_append(&state, tmp, 2);
  md5_append(&state, (const md5_byte_t *)s, (int)len);
  if ( lit && lit->qualifier )
  { assert(lit->type_or_lang);
    md5_append(&state,
	       (const md5_byte_t *)(lit->qualifier == Q_LANG ? "l" : "t"),
	       1);
    s = PL_blob_data(lit->type_or_lang, &len, NULL);
    md5_append(&state, (const md5_byte_t *)s, (int)len);
  }
  if ( t->graph )
  { md5_append(&state, (const md5_byte_t *)"S", 1);
    s = PL_blob_data(t->graph, &len, NULL);
    md5_append(&state, (const md5_byte_t *)s, (int)len);
  }
  
  md5_finish(&state, digest);
}


static void
sum_digest(md5_byte_t *digest, md5_byte_t *add)
{ md5_byte_t *p, *q;
  int n;

  for(p=digest, q=add, n=16; --n>=0; )
    *p++ += *q++;
}


static void
dec_digest(md5_byte_t *digest, md5_byte_t *add)
{ md5_byte_t *p, *q;
  int n;

  for(p=digest, q=add, n=16; --n>=0; )
    *p++ -= *q++;
}


static int
md5_unify_digest(term_t t, md5_byte_t digest[16])
{ char hex_output[16*2];
  int di;
  char *pi;
  static char hexd[] = "0123456789abcdef";

  for(pi=hex_output, di = 0; di < 16; ++di)
  { *pi++ = hexd[(digest[di] >> 4) & 0x0f];
    *pi++ = hexd[digest[di] & 0x0f];
  }

  return PL_unify_atom_nchars(t, 16*2, hex_output);
}


static foreign_t
rdf_md5(term_t graph_name, term_t md5)
{ atom_t src;
  int rc;
  rdf_db *db = DB;

  if ( !get_atom_or_var_ex(graph_name, &src) )
    return FALSE;

  if ( src )
  { graph *s;

    if ( !RDLOCK(db) )
      return FALSE;
    if ( (s = lookup_graph(db, src, FALSE)) )
    { rc = md5_unify_digest(md5, s->digest);
    } else
    { md5_byte_t digest[16];

      memset(digest, 0, sizeof(digest));
      rc = md5_unify_digest(md5, digest);
    }
    RDUNLOCK(db);
  } else
  { md5_byte_t digest[16];
    graph **ht;
    int i;
    
    memset(&digest, 0, sizeof(digest));

    if ( !RDLOCK(db) )
      return FALSE;

    for(i=0,ht = db->graph_table; i<db->graph_table_size; i++, ht++)
    { graph *s;
      
      for( s = *ht; s; s = s->next )
	sum_digest(digest, s->digest);
    }

    rc = md5_unify_digest(md5, digest);
    RDUNLOCK(db);
  }

  return rc;
}


static foreign_t
rdf_atom_md5(term_t text, term_t times, term_t md5)
{ char *s;
  int n, i;
  size_t len;
  md5_byte_t digest[16];

  if ( !PL_get_nchars(text, &len, &s, CVT_ALL) )
    return type_error(text, "text");
  if ( !PL_get_integer(times, &n) )
    return type_error(times, "integer");
  if ( n < 1 )
    return domain_error(times, "positive_integer");

  for(i=0; i<n; i++)
  { md5_state_t state;
    md5_init(&state);
    md5_append(&state, (const md5_byte_t *)s, (int)len);
    md5_finish(&state, digest);
    s = (char *)digest;
    len = sizeof(digest);
  }

  return md5_unify_digest(md5, digest);
}



#endif /*WITH_MD5*/


		 /*******************************
		 *	       ATOMS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Lock atoms in triple against AGC. Note that the predicate name is locked
in the predicate structure.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
lock_atoms(triple *t)
{ if ( !t->atoms_locked )
  { t->atoms_locked = TRUE;

    PL_register_atom(t->subject);
    if ( t->object_is_literal )
    { lock_atoms_literal(t->object.literal);
    } else
    { PL_register_atom(t->object.resource);
    }
  }
}


static void
unlock_atoms(triple *t)
{ if ( t->atoms_locked )
  { t->atoms_locked = FALSE;

    PL_unregister_atom(t->subject);
    if ( !t->object_is_literal )
    { PL_unregister_atom(t->object.resource);
    }
  }
}


		 /*******************************
		 *      PROLOG CONVERSION	*
		 *******************************/

#define LIT_TYPED	0x1
#define LIT_NOERROR	0x2
#define LIT_PARTIAL	0x4

static int
get_lit_atom_ex(term_t t, atom_t *a, int flags)
{ if ( PL_get_atom(t, a) )
    return TRUE;
  if ( (flags & LIT_PARTIAL) && PL_is_variable(t) )
  { *a = 0L;
    return TRUE;
  }

  return type_error(t, "atom");
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_literal() processes the argument  of  a   literal/1  term  passes as
object.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
get_literal(rdf_db *db, term_t litt, triple *t, int flags)
{ literal *lit;

  alloc_literal_triple(db, t);
  lit = t->object.literal;

  if ( PL_get_atom(litt, &lit->value.string) )
  { lit->objtype = OBJ_STRING;
  } else if ( PL_is_integer(litt) && PL_get_int64(litt, &lit->value.integer) )
  { lit->objtype = OBJ_INTEGER;
  } else if ( PL_get_float(litt, &lit->value.real) )
  { lit->objtype = OBJ_DOUBLE;
  } else if ( PL_is_functor(litt, FUNCTOR_lang2) )
  { term_t a = PL_new_term_ref();
    
    PL_get_arg(1, litt, a);
    if ( !get_lit_atom_ex(a, &lit->type_or_lang, flags) )
      return FALSE;
    PL_get_arg(2, litt, a);
    if ( !get_lit_atom_ex(a, &lit->value.string, flags) )
      return FALSE;

    lit->qualifier = Q_LANG;
    lit->objtype = OBJ_STRING;
  } else if ( PL_is_functor(litt, FUNCTOR_type2) &&
	      !(flags & LIT_TYPED) )	/* avoid recursion */
  { term_t a = PL_new_term_ref();
    
    PL_get_arg(1, litt, a);
    if ( !get_lit_atom_ex(a, &lit->type_or_lang, flags) )
      return FALSE;
    lit->qualifier = Q_TYPE;
    PL_get_arg(2, litt, a);

    return get_literal(db, a, t, LIT_TYPED|flags);
  } else if ( !PL_is_ground(litt) )
  { if ( !(flags & LIT_PARTIAL) )
      return type_error(litt, "rdf_object");
  } else
  { lit->value.term.record = PL_record_external(litt, &lit->value.term.len);
    lit->objtype = OBJ_TERM;
  }

  return TRUE;
}


static int
get_object(rdf_db *db, term_t object, triple *t)
{ if ( PL_get_atom(object, &t->object.resource) )
  { assert(!t->object_is_literal);
  } else if ( PL_is_functor(object, FUNCTOR_literal1) )
  { term_t a = PL_new_term_ref();
    
    PL_get_arg(1, object, a);
    return get_literal(db, a, t, 0);
  } else
    return type_error(object, "rdf_object");

  return TRUE;
}


static int
get_src(term_t src, triple *t)
{ if ( src && !PL_is_variable(src) )
  { if ( PL_get_atom(src, &t->graph) )
    { t->line = NO_LINE;
    } else if ( PL_is_functor(src, FUNCTOR_colon2) )
    { term_t a = PL_new_term_ref();
      long line;
      
      PL_get_arg(1, src, a);
      if ( !get_atom_or_var_ex(a, &t->graph) )
	return FALSE;
      PL_get_arg(2, src, a);
      if ( PL_get_long(a, &line) )
	t->line = line;
      else if ( !PL_is_variable(a) )
	return type_error(a, "integer");
    } else
      return type_error(src, "rdf_graph");
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Return values:
	-1: exception
	 0: no predicate
	 1: the predicate
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
get_existing_predicate(rdf_db *db, term_t t, predicate **p)
{ atom_t name;

  if ( !PL_get_atom(t, &name ) )
  { if ( PL_is_functor(t, FUNCTOR_literal1) )
      return 0;				/* rdf(_, literal(_), _) */
    return type_error(t, "atom");
  }

  if ( (*p = existing_predicate(db, name)) )
    return 1;

  DEBUG(5, Sdprintf("No predicate %s\n", PL_atom_chars(name)));
  return 0;				/* no predicate */
}


static int
get_predicate(rdf_db *db, term_t t, predicate **p)
{ atom_t name;

  if ( !get_atom_ex(t, &name ) )
    return FALSE;

  *p = lookup_predicate(db, name);
  return TRUE;
}


static int
get_triple(rdf_db *db,
	   term_t subject, term_t predicate, term_t object,
	   triple *t)
{ if ( !get_atom_ex(subject, &t->subject) ||
       !get_predicate(db, predicate, &t->predicate.r) ||
       !get_object(db, object, t) )
    return FALSE;

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_partial_triple() creates a triple  for   matching  purposes.  It can
return FALSE for  two  reasons.  Mostly   (type)  errors,  but  also  if
resources are accessed that do not   exist  and therefore the subsequent
matching will always fail. This  is   notably  the  case for predicates,
which are first class citizens to this library.

Return values:
	1: ok
	0: no predicate
       -1: error
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
get_partial_triple(rdf_db *db,
		   term_t subject, term_t predicate, term_t object,
		   term_t src, triple *t)
{ int rc;

  if ( subject && !get_resource_or_var_ex(subject, &t->subject) )
    return FALSE;
  if ( !PL_is_variable(predicate) &&
       (rc=get_existing_predicate(db, predicate, &t->predicate.r)) != 1 )
    return rc;
					/* the object */
  if ( object && !PL_is_variable(object) )
  { if ( PL_get_atom(object, &t->object.resource) )
    { assert(!t->object_is_literal);
    } else if ( PL_is_functor(object, FUNCTOR_literal1) )
    { term_t a = PL_new_term_ref();
      
      PL_get_arg(1, object, a);
      if ( !get_literal(db, a, t, LIT_PARTIAL) )
	return FALSE;
    } else if ( PL_is_functor(object, FUNCTOR_literal2) )
    { term_t a = PL_new_term_ref();
      literal *lit;

      alloc_literal_triple(db, t);
      lit = t->object.literal;
      
      PL_get_arg(1, object, a);
      if ( PL_is_functor(a, FUNCTOR_exact1) )
	t->match = STR_MATCH_EXACT;
      else if ( PL_is_functor(a, FUNCTOR_substring1) )
	t->match = STR_MATCH_SUBSTRING;
      else if ( PL_is_functor(a, FUNCTOR_word1) )
	t->match = STR_MATCH_WORD;
      else if ( PL_is_functor(a, FUNCTOR_prefix1) )
	t->match = STR_MATCH_PREFIX;
      else if ( PL_is_functor(a, FUNCTOR_like1) )
	t->match = STR_MATCH_LIKE;
      else 
	return domain_error(a, "match_type");
      PL_get_arg(1, a, a);
      if ( !get_atom_or_var_ex(a, &lit->value.string) )
	return FALSE;
      lit->objtype = OBJ_STRING;
    } else
      return type_error(object, "rdf_object");
  }
					/* the graph */
  if ( !get_src(src, t) )
    return FALSE;

  if ( t->subject )
    t->indexed |= BY_S;
  if ( t->predicate.r )
    t->indexed |= BY_P;
  if ( t->object_is_literal )
  { literal *lit = t->object.literal;

    if ( lit->objtype == OBJ_STRING && 
	 lit->value.string &&
	 t->match <= STR_MATCH_EXACT )
      t->indexed |= BY_O;
  } else if ( t->object.resource )
    t->indexed |= BY_O;

  db->indexed[t->indexed]++;		/* statistics */

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
inverse_partial_triple(triple *t)
{ predicate *i = 0;

  if ( !t->inversed &&
       (!t->predicate.r || (i=t->predicate.r->inverse_of)) &&
       !t->object_is_literal )
  { atom_t o = t->object.resource;

    t->object.resource = t->subject;
    t->subject = o;

    if ( t->predicate.r )
      t->predicate.r = i;

    t->indexed  = by_inverse[t->indexed];
    t->inversed = TRUE;

    return TRUE;
  }

  return FALSE;
}


static int
get_graph(term_t src, triple *t)
{ if ( PL_get_atom(src, &t->graph) )
  { t->line = NO_LINE;
    return TRUE;
  }

  if ( PL_is_functor(src, FUNCTOR_colon2) )
  { term_t a = PL_new_term_ref();
    long line;

    PL_get_arg(1, src, a);
    if ( !get_atom_ex(a, &t->graph) )
      return FALSE;
    PL_get_arg(2, src, a);
    if ( !get_long_ex(a, &line) )
      return FALSE;
    t->line = line;

    return TRUE;
  }

  return type_error(src, "rdf_graph");
}


static int
unify_graph(term_t src, triple *t)
{ if ( t->line == NO_LINE )
  { if ( !PL_unify_atom(src, t->graph) )
      return PL_unify_term(src,
			   PL_FUNCTOR, FUNCTOR_colon2,
			     PL_ATOM, t->graph,
			     PL_VARIABLE);  
    return TRUE;
  } else
    return PL_unify_term(src,
			 PL_FUNCTOR, FUNCTOR_colon2,
			   PL_ATOM, t->graph,
			   PL_LONG, t->line);
}


static int
same_graph(triple *t1, triple *t2)
{ return t1->line   == t2->line &&
         t1->graph == t2->graph;
}



static int
put_literal_value(term_t v, literal *lit)
{ switch(lit->objtype)
  { case OBJ_STRING:
      PL_put_atom(v, lit->value.string);
      break;
    case OBJ_INTEGER:
      PL_put_variable(v);
      PL_unify_int64(v, lit->value.integer);
      break;
    case OBJ_DOUBLE:
      PL_put_float(v, lit->value.real);
      break;
    case OBJ_TERM:
      PL_recorded_external(lit->value.term.record, v);
      break;
    default:
      assert(0);
      return FALSE;
  }

  return TRUE;
}


static int
unify_literal(term_t lit, literal *l)
{ term_t v = PL_new_term_ref();

  put_literal_value(v, l);

  if ( l->qualifier )
  { functor_t qf;

    assert(l->type_or_lang);

    if ( l->qualifier == Q_LANG )
      qf = FUNCTOR_lang2;
    else
      qf = FUNCTOR_type2;

    if ( PL_unify_term(lit, PL_FUNCTOR, qf,
			 PL_ATOM, l->type_or_lang,
			 PL_TERM, v) )
      return TRUE;

    return PL_unify(lit, v);		/* allow rdf(X, Y, literal(foo)) */
  } else if ( PL_unify(lit, v) )
  { return TRUE;
  } else if ( PL_is_functor(lit, FUNCTOR_lang2) &&
	      l->objtype == OBJ_STRING )
  { term_t a = PL_new_term_ref();
    PL_get_arg(2, lit, a);
    return PL_unify(a, v);
  } else if ( PL_is_functor(lit, FUNCTOR_type2) )
  { term_t a = PL_new_term_ref();
    PL_get_arg(2, lit, a);
    return PL_unify(a, v);
  } else
    return FALSE;
}



static int
unify_object(term_t object, triple *t)
{ if ( t->object_is_literal )
  { term_t lit = PL_new_term_ref();

    if ( PL_unify_functor(object, FUNCTOR_literal1) )
      PL_get_arg(1, object, lit);
    else if ( PL_is_functor(object, FUNCTOR_literal2) )
      PL_get_arg(2, object, lit);
    else
      return FALSE;

    return unify_literal(lit, t->object.literal);
  } else
  { return PL_unify_atom(object, t->object.resource);
  }
}


static int
unify_triple(term_t subject, term_t pred, term_t object,
	     term_t src, triple *t, int inversed)
{ predicate *p = t->predicate.r;
  fid_t fid;

  if ( inversed )
  { term_t tmp = object;
    object = subject;
    subject = tmp;

    if ( !(p = p->inverse_of) )
      return FALSE;
  }

  fid = PL_open_foreign_frame();

  if ( !PL_unify_atom(subject, t->subject) ||
       !PL_unify_atom(pred, p->name) ||
       !unify_object(object, t) ||
       (src && !unify_graph(src, t)) )
  { PL_discard_foreign_frame(fid);
    return FALSE;
  } else
  { PL_close_foreign_frame(fid);
    return TRUE;
  }
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

(*) Iff too many triples are  added,  it   may  be  time  to enlarge the
hashtable. Note that we do not call  update_hash() blindly as this would
cause each triple that  modifies  the   predicate  hierarchy  to force a
rehash. As we are not searching using subPropertyOf semantics during the
duplicate update, there is no point updating. If it is incorrect it will
be updated on the first real query.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static int
update_duplicates_add(rdf_db *db, triple *t)
{ triple *d;
  const int indexed = BY_SP;

  assert(t->is_duplicate == FALSE);
  assert(t->duplicates == 0);

  if ( WANT_GC(db) )			/* (*) See above */
    update_hash(db);
  d = db->table[indexed][triple_hash(db, t, indexed)];
  for( ; d && d != t; d = d->next[indexed] )
  { if ( match_triples(d, t, MATCH_DUPLICATE) )
    { t->is_duplicate = TRUE;
      assert( !d->is_duplicate );

      d->duplicates++;

      DEBUG(2,
	    print_triple(t, PRT_SRC);
	    Sdprintf(" %p: %d-th duplicate: ", t, d->duplicates);
	    Sdprintf("Principal: %p at", d);
	    print_src(d);
	    Sdprintf("\n"));
      
      assert(d->duplicates);		/* check overflow */
      db->duplicates++;
      return TRUE;
    }
  }

  return FALSE;
}


static void				/* t is about to be deleted */
update_duplicates_del(rdf_db *db, triple *t)
{ const int indexed = BY_SP;

  if ( t->duplicates )			/* I am the principal one */
  { triple *d;
      
    DEBUG(2,
	  print_triple(t, PRT_SRC);
	  Sdprintf(": DEL principal %p, %d duplicates: ", t, t->duplicates));

    db->duplicates--;
    d = db->table[indexed][triple_hash(db, t, indexed)];
    for( ; d; d = d->next[indexed] )
    { if ( d != t && match_triples(d, t, MATCH_DUPLICATE) )
      { assert(d->is_duplicate);
	d->is_duplicate = FALSE;
	d->duplicates = t->duplicates-1;
	DEBUG(2,
	      Sdprintf("New principal: %p at", d);
	      print_src(d);
	      Sdprintf("\n"));

	return;
      }
    }
    assert(0);
  } else if ( t->is_duplicate )		/* I am a duplicate */
  { triple *d;
      
    DEBUG(2,
	  print_triple(t, PRT_SRC);
	  Sdprintf(": DEL: is a duplicate: "));

    db->duplicates--;
    d = db->table[indexed][triple_hash(db, t, indexed)];
    for( ; d; d = d->next[indexed] )
    { if ( d != t && match_triples(d, t, MATCH_DUPLICATE) )
      { if ( d->duplicates )
	{ d->duplicates--;
	  DEBUG(2,
		Sdprintf("Principal %p at ", d);
		print_src(d);
		Sdprintf(" has %d duplicates\n", d->duplicates));
	  return;
	}
      }
    }
    Sdprintf("FATAL\n");
    PL_halt(1);
    assert(0);
  }
}


		 /*******************************
		 *	    TRANSACTIONS	*
		 *******************************/

static void
append_transaction(rdf_db *db, transaction_record *tr)
{ if ( db->tr_last )
  { tr->next = NULL;
    tr->previous = db->tr_last;
    db->tr_last->next = tr;
    db->tr_last = tr;
  } else
  { tr->next = tr->previous = NULL;
    db->tr_first = db->tr_last = tr;
  }
}


static void
open_transaction(rdf_db *db)
{ transaction_record *tr = rdf_malloc(db, sizeof(*tr));

  memset(tr, 0, sizeof(*tr));
  tr->type = TR_MARK;

  if ( db->tr_first )
    db->tr_nesting++;
  else
    db->tr_nesting = 0;

  append_transaction(db, tr);
}


static void
record_transaction(rdf_db *db, tr_type type, triple *t)
{ transaction_record *tr = rdf_malloc(db, sizeof(*tr));

  memset(tr, 0, sizeof(*tr));
  tr->type = type;
  tr->triple = t;

  append_transaction(db, tr);
}


static void
record_md5_transaction(rdf_db *db, graph *src, md5_byte_t *digest)
{ transaction_record *tr = rdf_malloc(db, sizeof(*tr));

  memset(tr, 0, sizeof(*tr));
  tr->type = TR_UPDATE_MD5,
  tr->update.md5.graph = src;
  tr->update.md5.digest = digest;

  append_transaction(db, tr);
}


static void
record_update_transaction(rdf_db *db, triple *t, triple *new)
{ transaction_record *tr = rdf_malloc(db, sizeof(*tr));

  memset(tr, 0, sizeof(*tr));
  tr->type = TR_UPDATE,
  tr->triple = t;
  tr->update.triple = new;

  append_transaction(db, tr);
}


static void
record_update_src_transaction(rdf_db *db, triple *t,
			      atom_t src, unsigned long line)
{ transaction_record *tr = rdf_malloc(db, sizeof(*tr));

  memset(tr, 0, sizeof(*tr));
  tr->type = TR_UPDATE_SRC,
  tr->triple = t;
  tr->update.src.atom = src;
  tr->update.src.line = line;

  append_transaction(db, tr);
}


static void
free_transaction(rdf_db *db, transaction_record *tr)
{ switch(tr->type)
  { case TR_ASSERT:
      free_triple(db, tr->triple);
      break;
    case TR_UPDATE:
      free_triple(db, tr->update.triple);
      break;
    case TR_UPDATE_MD5:
      if ( tr->update.md5.digest )
	rdf_free(db, tr->update.md5.digest, sizeof(*tr->update.md5.digest));
      break;
    default:
      break;
  }

  rdf_free(db, tr, sizeof(*tr));
}


static void
truncate_transaction(rdf_db *db, transaction_record *last)
{ db->tr_last = last;
  if ( last )
  { db->tr_last->next = NULL;
  } else
  { db->tr_first = NULL;
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
discard_transaction()  simply  destroys  all   actions    in   the  last
transaction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
discard_transaction(rdf_db *db)
{ transaction_record *tr, *prev;

  for(tr=db->tr_last; tr; tr = prev)
  { prev = tr->previous;

    if ( tr->type == TR_SUB_END )
    { if ( tr->update.transaction_id )
	PL_erase(tr->update.transaction_id);
    }

    if ( tr->type == TR_MARK )
    { rdf_free(db, tr, sizeof(*tr));
      truncate_transaction(db, prev);
      db->tr_nesting--;
      return;
    }
    
    free_transaction(db, tr);
  }
}


void
put_begin_end(term_t t, functor_t be, int level)
{ term_t av = PL_new_term_ref();

  PL_put_integer(av, level);
  PL_cons_functor_v(t, be, av);
}




/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note  (*)  rdf-monitors  can  modify  the    database   by  opening  new
transactions. Therefore we first close the  transaction to allow opening
new ones. TBD: get  this  clear.   Monitors  have  only  restricted read
access?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
commit_transaction(rdf_db *db, term_t id)
{ transaction_record *tr, *next;
  int tr_level = 0;			/* nesting level */

  if ( db->tr_nesting > 0 )		/* commit nested transaction */
  { tr=db->tr_last;

    if ( tr->type == TR_MARK )		/* empty nested transaction */
    { truncate_transaction(db, tr->previous);
      rdf_free(db, tr, sizeof(*tr));
      db->tr_nesting--;

      return TRUE;
    }

    for(; tr; tr = tr->previous)	/* not the last (tested above) */
    {					/* not the first (we are nested) */
      if ( tr->type == TR_MARK )
      { transaction_record *end = rdf_malloc(db, sizeof(*end));

	memset(end, 0, sizeof(*end));
	end->type = TR_SUB_END;
	end->update.transaction_id = PL_record(id);
	append_transaction(db, end);

	tr->type = TR_SUB_START;
	tr->update.transaction_id = end->update.transaction_id;
	db->tr_nesting--;

	return TRUE;
      }
    }

    assert(0);
    return FALSE;
  }

  while( (tr=db->tr_first) )		/* See above (*) */
  { db->tr_first = db->tr_last = NULL;

					/* real commit */
    for(; tr; tr = next)
    { next = tr->next;
  
      switch(tr->type)
      { case TR_MARK:
	  break;
	case TR_SUB_START:
	{ term_t id = PL_new_term_ref();
	  term_t be = PL_new_term_ref();
	  PL_recorded(tr->update.transaction_id, id);
	  put_begin_end(be, FUNCTOR_begin1, ++tr_level);
	  broadcast(EV_TRANSACTION, (void*)id, (void*)be);
	  break;
	}
	case TR_SUB_END:
	{ term_t id = PL_new_term_ref();
	  term_t be = PL_new_term_ref();
	  PL_recorded(tr->update.transaction_id, id);
	  PL_erase(tr->update.transaction_id);
	  put_begin_end(be, FUNCTOR_end1, tr_level--);
	  broadcast(EV_TRANSACTION, (void*)id, (void*)be);
	  break;
	}
	case TR_ASSERT:
	  link_triple(db, tr->triple);
	  db->generation++;
	  break;
	case TR_RETRACT:
	  if ( !tr->triple->erased )	/* already erased */
	  { erase_triple(db, tr->triple);
	    db->generation++;
	  }
	  break;
	case TR_UPDATE:
	  broadcast(EV_UPDATE, tr->triple, tr->update.triple);
	  erase_triple_silent(db, tr->triple);
	  link_triple_silent(db, tr->update.triple);
	  db->generation++;
	  break;
	case TR_UPDATE_SRC:
	  if ( tr->triple->graph != tr->update.src.atom )
	  { if ( tr->triple->graph )
	      unregister_graph(db, tr->triple);
	    tr->triple->graph = tr->update.src.atom;
	    if ( tr->triple->graph )
	      register_graph(db, tr->triple);
	  }
	  tr->triple->line = tr->update.src.line;
	  db->generation++;
	  break;
	case TR_UPDATE_MD5:
	{ graph *src = tr->update.md5.graph;
	  md5_byte_t *digest = tr->update.md5.digest;
	  if ( digest )
	  { sum_digest(digest, src->digest);
	    src->md5 = TRUE;
	    rdf_free(db, digest, sizeof(md5_byte_t)*16);
	  } else
	  { src->md5 = FALSE;
	  }
	  break;
	}
	case TR_RESET:
	  db->tr_reset = FALSE;
	  reset_db(db);
	  break;
	default:
	  assert(0);
      }
  
      rdf_free(db, tr, sizeof(*tr));
    }
  }

  return TRUE;
}


static foreign_t
rdf_transaction(term_t goal, term_t id)
{ int rc;
  rdf_db *db = DB;
  active_transaction me;

  if ( !WRLOCK(db, TRUE) )
    return FALSE;

  open_transaction(db);
  me.parent = db->tr_active;
  me.id = id;
  db->tr_active = &me;

  rc = PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, PRED_call1, goal);

  if ( rc )
  { int empty = (db->tr_last == NULL || db->tr_last->type == TR_MARK);

    if ( empty || db->tr_nesting > 0 )
    { commit_transaction(db, id);
    } else
    { term_t be = PL_new_term_ref();
      put_begin_end(be, FUNCTOR_begin1, 0);
      broadcast(EV_TRANSACTION, (void*)id, (void*)be);
      put_begin_end(be, FUNCTOR_end1, 0);
      if ( !LOCKOUT_READERS(db) )	/* interrupt, timeout */
      { broadcast(EV_TRANSACTION, (void*)id, (void*)be);
	rc = FALSE;
	goto discard;
      }
      commit_transaction(db, id);
      REALLOW_READERS(db);
      broadcast(EV_TRANSACTION, (void*)id, (void*)be);
    }
  } else
  { discard:
    discard_transaction(db);
  }
  db->tr_active = me.parent;
  WRUNLOCK(db);

  return rc;
}


static foreign_t
rdf_active_transactions(term_t list)
{ rdf_db *db = DB;
  term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();
  active_transaction *ot;

  for(ot = db->tr_active; ot; ot=ot->parent)
  { if ( !PL_unify_list(tail, head, tail) ||
	 !PL_unify(head, ot->id) )
      return FALSE;
  }  

  return PL_unify_nil(tail);
}


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

static foreign_t
rdf_assert4(term_t subject, term_t predicate, term_t object, term_t src)
{ rdf_db *db = DB;
  triple *t = new_triple(db);
  
  if ( !get_triple(db, subject, predicate, object, t) )
  { free_triple(db, t);
    return FALSE;
  }
  if ( src )
  { if ( !get_graph(src, t) )
    { free_triple(db, t);
      return FALSE;
    }
  } else
  { t->graph = ATOM_user;
    t->line = NO_LINE;
  }

  lock_atoms(t);
  if ( !WRLOCK(db, FALSE) )
  { free_triple(db, t);
    return FALSE;
  }

  if ( db->tr_first )
  { record_transaction(db, TR_ASSERT, t);
  } else
  { link_triple(db, t);
    db->generation++;
  }
  WRUNLOCK(db);

  return TRUE;
}


static foreign_t
rdf_assert3(term_t subject, term_t predicate, term_t object)
{ return rdf_assert4(subject, predicate, object, 0);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
inc_active_queries(rdf_db *db);
dec_active_queries(rdf_db *db);

TBD: Either delete this or use atomic inc/dec.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
inc_active_queries(rdf_db *db)
{ LOCK_MISC(db);
  db->active_queries++;
  UNLOCK_MISC(db);
}


static void
dec_active_queries(rdf_db *db)
{ LOCK_MISC(db);
  db->active_queries--;
  assert(db->active_queries>=0);
  UNLOCK_MISC(db);
}


typedef struct search_state
{ rdf_db       *db;			/* our database */
  term_t	subject;		/* Prolog term references */
  term_t 	object;
  term_t 	predicate;
  term_t 	src;
  term_t 	realpred;
  unsigned 	locked : 1;		/* State has been locked */
  unsigned	allocated : 1;		/* State has been allocated */
  unsigned	flags;			/* Misc flags controlling search */
  atom_t	prefix;			/* prefix and like search */
  avl_enum     *literal_state;		/* Literal search state */
  literal      *literal_cursor;		/* pointer in current literal */
  literal_ex    lit_ex;			/* extended literal for fast compare */
  triple       *cursor;			/* Pointer in triple DB */
  triple	pattern;		/* Pattern triple */
} search_state;


static void	free_search_state(search_state *state);

static void
init_cursor_from_literal(search_state *state, literal *cursor)
{ triple *p = &state->pattern;
  unsigned long iv;
  int i;

  DEBUG(3,
	Sdprintf("Trying literal search for ");
	print_literal(cursor);
	Sdprintf("\n"));
  
  p->indexed |= BY_O;
  p->indexed &= ~BY_S;			/* we do not have index BY_SO */
  switch(p->indexed)
  { case BY_O:
      iv = literal_hash(cursor);
      break;
    case BY_OP:
      iv = predicate_hash(p->predicate.r) ^ literal_hash(cursor);
      break;
    default:
      iv = 0;				/* make compiler silent */
      assert(0);
  }

  i = (int)(iv % (long)state->db->table_size[p->indexed]);
  state->cursor = state->db->table[p->indexed][i];
  state->literal_cursor = cursor;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) update_hash() is there to update  the   hash  after  a change to the
predicate organization. If we do  not  have   a  predicate  or we do not
search using rdf_has/3, this is not needed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
init_search_state(search_state *state)
{ triple *p = &state->pattern;

  if ( get_partial_triple(state->db,
			  state->subject, state->predicate, state->object,
			  state->src, p) != TRUE )	
  { free_triple(state->db, p);
    return FALSE;
  }
  
  if ( !RDLOCK(state->db) )
  { free_triple(state->db, p);
    return FALSE;
  }
  state->locked = TRUE;
  if ( p->predicate.r && (state->flags & MATCH_SUBPROPERTY) ) /* See (*) */
  { if ( !update_hash(state->db) )
    { free_search_state(state);
      return FALSE;
    }
  }

  if ( (p->match == STR_MATCH_PREFIX ||	p->match == STR_MATCH_LIKE) &&
       p->indexed != BY_SP &&
       (state->prefix = first_atom(p->object.literal->value.string, p->match)))
  { literal lit;
    literal **rlitp;
    
    lit = *p->object.literal;
    lit.value.string = state->prefix;
    state->literal_state = rdf_malloc(state->db,
				      sizeof(*state->literal_state));
    state->lit_ex.literal = &lit;
    prepare_literal_ex(&state->lit_ex);
    rlitp = avlfindfirst(&state->db->literals, &state->lit_ex, state->literal_state);
    if ( rlitp )
    { init_cursor_from_literal(state, *rlitp);
    } else
    { free_search_state(state);
      return FALSE;
    }
  } else
  { state->cursor = state->db->table[p->indexed]
    				    [triple_hash(state->db, p, p->indexed)];
  }

  return TRUE;
}


static void
free_search_state(search_state *state)
{ if ( state->locked )
  { RDUNLOCK(state->db);
  }

  free_triple(state->db, &state->pattern);
  if ( state->prefix )
    PL_unregister_atom(state->prefix);
  if ( state->literal_state )
    rdf_free(state->db, state->literal_state, sizeof(*state->literal_state));
  if ( state->allocated )		/* also means redo! */
  { dec_active_queries(state->db);
    rdf_free(state->db, state, sizeof(*state));
  }  
}


static foreign_t
allow_retry_state(search_state *state)
{ if ( !state->allocated )
  { search_state *copy = rdf_malloc(state->db, sizeof(*copy));
    *copy = *state;
    copy->allocated = TRUE;
    inc_active_queries(state->db);

    state = copy;
  }

  PL_retry_address(state);
}


/* TBD: simplify.   Maybe split for resource and literal search, as
   both involve mutual exclusive complications to this routine,
*/

static int
next_search_state(search_state *state)
{ triple *t = state->cursor;
  triple *p = &state->pattern;

retry:
  for( ; t; t = t->next[p->indexed])
  { if ( t->is_duplicate && !state->src )
      continue;

					/* hash-collision, skip */
    if ( state->literal_state )
    { if ( !(t->object_is_literal &&
	     t->object.literal == state->literal_cursor) )
	continue;
    }

    if ( match_triples(t, p, state->flags) )
    { term_t retpred = state->realpred ? state->realpred : state->predicate;
      if ( !unify_triple(state->subject, retpred, state->object,
			 state->src, t, p->inversed) )
	continue;
      if ( state->realpred && PL_is_variable(state->predicate) )
	PL_unify(state->predicate, retpred);

      t=t->next[p->indexed];
    inv_alt:
      for(; t; t = t->next[p->indexed])
      { if ( state->literal_state )
	{ if ( !(t->object_is_literal &&
		 t->object.literal == state->literal_cursor) )
	    continue;
	}

	if ( match_triples(t, p, state->flags) )
	{ state->cursor = t;
	  
	  return TRUE;			/* non-deterministic */
	}
      }

      if ( (state->flags & MATCH_INVERSE) && inverse_partial_triple(p) )
      { t = state->db->table[p->indexed][triple_hash(state->db, p, p->indexed)];
	goto inv_alt;
      }

      state->cursor = NULL;		/* deterministic */
      return TRUE;
    }
  }

  if ( (state->flags & MATCH_INVERSE) && inverse_partial_triple(p) )
  { t = state->db->table[p->indexed][triple_hash(state->db, p, p->indexed)];
    goto retry;
  }

  if ( state->literal_state )
  { literal **litp;

    if ( (litp = avlfindnext(state->literal_state)) )
    { if ( state->prefix )
      { literal *lit = *litp;

	if ( !match_atoms(STR_MATCH_PREFIX, state->prefix, lit->value.string) )
	{ DEBUG(1,
		Sdprintf("Terminated literal iteration from ");
		print_literal(lit);
		Sdprintf("\n"));
	  return FALSE;			/* no longer a prefix */
	}
      }

      init_cursor_from_literal(state, *litp);
      t = state->cursor;

      goto retry;
    }
  }

  return FALSE;
}



static foreign_t
rdf(term_t subject, term_t predicate, term_t object,
    term_t src, term_t realpred, control_t h, unsigned flags)
{ rdf_db *db = DB;
  search_state *state;

  switch(PL_foreign_control(h))
  { case PL_FIRST_CALL:
    { search_state buf;

      state = &buf;
      memset(state, 0, sizeof(*state));
      state->db	       = db;
      state->subject   = subject;
      state->object    = object;
      state->predicate = predicate;
      state->src       = src;
      state->realpred  = realpred;
      state->flags     = flags;

      if ( !init_search_state(state) )
	return FALSE;

      goto search;
    }
    case PL_REDO:
    { int rc;

      state = PL_foreign_context_address(h);
      assert(state->subject == subject);

    search:
      if ( (rc=next_search_state(state)) )
      { if ( state->cursor || state->literal_state )
	  return allow_retry_state(state);
      }

      free_search_state(state);
      return rc;
    }
    case PL_CUTTED:
    { search_state *state = PL_foreign_context_address(h);

      free_search_state(state);
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
{ return rdf(subject, predicate, object, 0, 0, h,
	     MATCH_EXACT);
}


static foreign_t
rdf4(term_t subject, term_t predicate, term_t object,
     term_t src, control_t h)
{ return rdf(subject, predicate, object, src, 0, h,
	     MATCH_EXACT|MATCH_SRC);
}


static foreign_t
rdf_has(term_t subject, term_t predicate, term_t object,
	term_t realpred, control_t h)
{ return rdf(subject, predicate, object, 0, realpred, h,
	     MATCH_SUBPROPERTY|MATCH_INVERSE);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rdf_estimate_complexity(+S,+P,+O,-C)

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
rdf_estimate_complexity(term_t subject, term_t predicate, term_t object,
		        term_t complexity)
{ triple t;
  long c;
  rdf_db *db = DB;
  int rc;

  memset(&t, 0, sizeof(t));
  if ( (rc=get_partial_triple(db, subject, predicate, object, 0, &t)) != TRUE )
  { if ( rc == -1 )
    { return FALSE;			/* error */
    } else
    { return PL_unify_integer(complexity, 0); 	/* no predicate */
    }
  }
  
  if ( !RDLOCK(db) )
    return FALSE;
  if ( !update_hash(db) )			/* or ignore this problem? */
  { RDUNLOCK(db);
    free_triple(db, &t);
    return FALSE;
  }

  if ( t.indexed == BY_NONE )
  { c = db->created - db->erased;		/* = totale triple count */
  } else
  { c = db->counts[t.indexed][triple_hash(db, &t, t.indexed)];
  }

  rc = PL_unify_integer(complexity, c);
  RDUNLOCK(db);
  free_triple(db, &t);

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
current_literal(?Literals)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
rdf_current_literal(term_t t, control_t h)
{ rdf_db *db = DB;
  literal **data;
  avl_enum *state;
  int rc;

  switch(PL_foreign_control(h))
  { case PL_FIRST_CALL:
      if ( PL_is_variable(t) )
      { state = rdf_malloc(db, sizeof(*state));

	RDLOCK(db);
	inc_active_queries(db);
	data = avlfindfirst(&db->literals, NULL, state);
	goto next;
      } else
      { return FALSE;			/* TBD */
      }
    case PL_REDO:
      state = PL_foreign_context_address(h);
      data = avlfindnext(state);
    next:
      for(; data; data=avlfindnext(state))
      { literal *lit = *data;

	if ( unify_literal(t, lit) )
	{ PL_retry_address(state);
	}
      }

      rc = FALSE;
      goto cleanup;
    case PL_CUTTED:
      rc = TRUE;

    cleanup:
      state = PL_foreign_context_address(h);
      avlfinddestroy(state);
      rdf_free(db, state, sizeof(*state));
      RDUNLOCK(db);
      dec_active_queries(db);

      return rc;
    default:
      assert(0);
      return FALSE;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rdf_update(+Subject, +Predicate, +Object, +Action)

Update a triple. Please note this is actually erase+assert as the triple
needs to be updated in  the  linked   lists  while  erase simply flags a
triple as `erases' without deleting it   to support queries which active
choicepoints.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
update_triple(rdf_db *db, term_t action, triple *t)
{ term_t a = PL_new_term_ref();
  triple tmp, *new;
  int i;
					/* Create copy in local memory */
  tmp = *t;
  tmp.allocated = FALSE;
  tmp.atoms_locked = FALSE;
  if ( t->object_is_literal )
    tmp.object.literal = copy_literal(db, t->object.literal);

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

    if ( !get_predicate(db, a, &p) )
      return FALSE;
    if ( tmp.predicate.r == p )
      return TRUE;			/* no change */

    tmp.predicate.r = p;
  } else if ( PL_is_functor(action, FUNCTOR_object1) )
  { triple t2;

    memset(&t2, 0, sizeof(t2));

    if ( !get_object(db, a, &t2) )
    { free_triple(db, &t2);
      return FALSE;
    }
    if ( match_object(&t2, &tmp, MATCH_QUAL) )
    { free_triple(db, &t2);
      return TRUE;
    }

    if ( tmp.object_is_literal )
      free_literal(db, tmp.object.literal);
    if ( (tmp.object_is_literal = t2.object_is_literal) )
    { tmp.object.literal = t2.object.literal;
    } else
    { tmp.object.resource = t2.object.resource;
    }
  } else if ( PL_is_functor(action, FUNCTOR_graph1) )
  { triple t2;

    if ( !get_graph(a, &t2) )
      return FALSE;
    if ( t2.graph == t->graph && t2.line == t->line )
      return TRUE;
    if ( db->tr_first )
    { record_update_src_transaction(db, t, t2.graph, t2.line);
    } else
    { if ( t->graph )
	unregister_graph(db, t);
      t->graph = t2.graph;
      t->line = t2.line;
      if ( t->graph )
	register_graph(db, t);
    }

    return TRUE;			/* considered no change */
  } else
    return domain_error(action, "rdf_action");

  for(i=0; i<INDEX_TABLES; i++)
    tmp.next[i] = NULL;

  new = new_triple(db);
  new->subject		 = tmp.subject;
  new->predicate.r	 = tmp.predicate.r;
  if ( (new->object_is_literal = tmp.object_is_literal) )
  { new->object.literal = copy_literal(db, tmp.object.literal);
  } else
  { new->object.resource = tmp.object.resource;
  }
  new->graph		 = tmp.graph;
  new->line		 = tmp.line;

  free_triple(db, &tmp);
  lock_atoms(new);

  if ( db->tr_first )
  { record_update_transaction(db, t, new);
  } else
  { broadcast(EV_UPDATE, t, new);
    erase_triple_silent(db, t);
    link_triple_silent(db, new);
    db->generation++;
  }

  return TRUE;
}



static foreign_t
rdf_update5(term_t subject, term_t predicate, term_t object, term_t src,
	    term_t action)
{ triple t, *p;
  int indexed = BY_SP;
  int done = 0;
  rdf_db *db = DB;
    
  memset(&t, 0, sizeof(t));

  if ( !get_src(src, &t) ||
       !get_triple(db, subject, predicate, object, &t) )
    return FALSE;
  
  if ( !WRLOCK(db, FALSE) )
  { free_triple(db, &t);
    return FALSE;
  }
  if ( !update_hash(db) )
  { WRUNLOCK(db);
    free_triple(db, &t);
    return FALSE;
  }
  p = db->table[indexed][triple_hash(db, &t, indexed)];
  for( ; p; p = p->next[indexed])
  { if ( match_triples(p, &t, MATCH_EXACT) )
    { if ( !update_triple(db, action, p) )
      { WRUNLOCK(db);
	free_triple(db, &t);
	return FALSE;			/* type errors */
      }
      done++;
    }
  }
  free_triple(db, &t);
  WRUNLOCK(db);

  return done ? TRUE : FALSE;
}


static foreign_t
rdf_update(term_t subject, term_t predicate, term_t object, term_t action)
{ return rdf_update5(subject, predicate, object, 0, action);
}


static foreign_t
rdf_retractall4(term_t subject, term_t predicate, term_t object, term_t src)
{ triple t, *p;
  rdf_db *db = DB;
      
  memset(&t, 0, sizeof(t));
  switch( get_partial_triple(db, subject, predicate, object, src, &t) )
  { case 0:				/* no such predicate */
      return TRUE;
    case -1:				/* error */
      return FALSE;
  }

  if ( t.graph	)		/* speedup for rdf_retractall(_,_,_,DB) */
  { graph *gr = lookup_graph(db, t.graph, FALSE);

    if ( !gr || gr->triple_count == 0 )
      return TRUE;
  }

  if ( !WRLOCK(db, FALSE) )
    return FALSE;
/*			No need, as we do not search with subPropertyOf
  if ( !update_hash(db) )
  { WRUNLOCK(db);
    return FALSE;
  }
*/
  p = db->table[t.indexed][triple_hash(db, &t, t.indexed)];
  for( ; p; p = p->next[t.indexed])
  { if ( match_triples(p, &t, MATCH_EXACT|MATCH_SRC) )
    { if ( db->tr_first )
      { if ( db->tr_reset )
	{ WRUNLOCK(db);
	  return permission_error("retract", "triple", "",
				  "rdf_retractall cannot follow "
				  "rdf_reset_db in one transaction");
	}
	record_transaction(db, TR_RETRACT, p);
      } else
      { erase_triple(db, p);
	db->generation++;
      }
    }
  }

  WRUNLOCK(db);
  free_triple(db, &t);

  return TRUE;
}


static foreign_t
rdf_retractall3(term_t subject, term_t predicate, term_t object)
{ return rdf_retractall4(subject, predicate, object, 0);
}


		 /*******************************
		 *	     MONITOR		*
		 *******************************/

typedef struct broadcast_callback
{ struct broadcast_callback *next;
  predicate_t		     pred;
  long			     mask;
} broadcast_callback;

static long joined_mask = 0L;
static broadcast_callback *callback_list;
static broadcast_callback *callback_tail;

static void
do_broadcast(term_t term, long mask)
{ if ( callback_list )
  { broadcast_callback *cb;

    for(cb = callback_list; cb; cb = cb->next)
    { qid_t qid;
      term_t ex;

      if ( !(cb->mask & mask) )
	continue;

      qid = PL_open_query(NULL, PL_Q_CATCH_EXCEPTION, cb->pred, term);
      if ( !PL_next_solution(qid) && (ex = PL_exception(qid)) )
      { term_t av = PL_new_term_refs(2);

	PL_cut_query(qid);

	PL_put_atom(av+0, ATOM_error);
	PL_put_term(av+1, ex);

	PL_call_predicate(NULL, PL_Q_NORMAL, 
			  PL_predicate("print_message", 2, "user"),
			  av);
      } else
      { PL_close_query(qid);
      }
    }
  }
}


/* No longer used, but we keep it for if we need it again
static foreign_t
rdf_broadcast(term_t term, term_t mask)
{ long msk;

  if ( !get_long_ex(mask, &msk) )
    return FALSE;

  do_broadcast(term, msk);
  return TRUE;
}
*/

static void
broadcast(broadcast_id id, void *a1, void *a2)
{ if ( (joined_mask & id) )
  { fid_t fid;
    term_t term;
    functor_t funct;
    
    fid = PL_open_foreign_frame();
    term = PL_new_term_ref();

    switch(id)
    { case EV_ASSERT:
      case EV_ASSERT_LOAD:
	funct = FUNCTOR_assert4;
        goto assert_retract;
      case EV_RETRACT:
	funct = FUNCTOR_retract4;
      assert_retract:
      { triple *t = a1;
	term_t tmp = PL_new_term_refs(4);
	
	PL_put_atom(tmp+0, t->subject);
	PL_put_atom(tmp+1, t->predicate.r->name);
	unify_object(tmp+2, t);
	unify_graph(tmp+3, t);

	PL_cons_functor_v(term, funct, tmp);
	break;
      }
      case EV_UPDATE:
      { triple *t = a1;
	triple *new = a2;
	term_t tmp = PL_new_term_refs(5);
	term_t a = PL_new_term_ref();
	functor_t action;

	PL_put_atom(tmp+0, t->subject);
	PL_put_atom(tmp+1, t->predicate.r->name);
	unify_object(tmp+2, t);
	unify_graph(tmp+3, t);

	if ( t->subject != new->subject )
	{ action = FUNCTOR_subject1;
	  PL_put_atom(a, new->subject);
	} else if ( t->predicate.r != new->predicate.r )
	{ action = FUNCTOR_predicate1;
	  PL_put_atom(a, new->predicate.r->name);
	} else if ( !match_object(t, new, MATCH_QUAL) )
	{ action = FUNCTOR_object1;
	  unify_object(a, new);
	} else if ( !same_graph(t, new) )
	{ action = FUNCTOR_graph1;
	  unify_graph(a, new);
	} else
	{ return;			/* no change */
	}
	  
	PL_cons_functor_v(tmp+4, action, a);
	PL_cons_functor_v(term, FUNCTOR_update5, tmp);
	break;
      }
      case EV_NEW_LITERAL:
      { literal *lit = a1;
	term_t tmp = PL_new_term_refs(1);
	
	unify_literal(tmp, lit);
	PL_cons_functor_v(term, FUNCTOR_new_literal1, tmp);
	break;
      }
      case EV_OLD_LITERAL:
      { literal *lit = a1;
	term_t tmp = PL_new_term_refs(1);
	
	unify_literal(tmp, lit);
	PL_cons_functor_v(term, FUNCTOR_old_literal1, tmp);
	break;
      }
      case EV_LOAD:
      { term_t ctx = (term_t)a1;
	atom_t be  = (atom_t)a2;
	term_t tmp = PL_new_term_refs(2);
	
	PL_put_atom(tmp+0, be);		/* begin/end */
	PL_put_term(tmp+1, ctx);

	PL_cons_functor_v(term, FUNCTOR_load2, tmp);
	break;
      }
      case EV_TRANSACTION:
      { term_t ctx = (term_t)a1;
	term_t be  = (term_t)a2;
	term_t tmp = PL_new_term_refs(2);
	
	PL_put_term(tmp+0, be);		/* begin/end */
	PL_put_term(tmp+1, ctx);

	PL_cons_functor_v(term, FUNCTOR_transaction2, tmp);
	break;
      }
      case EV_REHASH:
      { term_t tmp = PL_new_term_refs(1);
	atom_t be = (atom_t)a1;
	
	PL_put_atom(tmp+0, be);	
	PL_cons_functor_v(term, FUNCTOR_rehash1, tmp);
	break;
      }
      default:
	assert(0);
    }

    do_broadcast(term, id);

    PL_discard_foreign_frame(fid);
  }
}


static foreign_t
rdf_monitor(term_t goal, term_t mask)
{ atom_t name;
  broadcast_callback *cb;
  predicate_t p;
  long msk;

  if ( !get_atom_ex(goal, &name) ||
       !get_long_ex(mask, &msk) )
    return FALSE;
    
  p = PL_pred(PL_new_functor(name, 1), NULL);

  for(cb=callback_list; cb; cb = cb->next)
  { if ( cb->pred == p )
    { broadcast_callback *cb2;
      cb->mask = msk;

      joined_mask = 0L;
      for(cb2=callback_list; cb2; cb2 = cb2->next)
	joined_mask |= cb2->mask;
      DEBUG(2, Sdprintf("Set mask to 0x%x\n", joined_mask));

      return TRUE;
    }
  }

  cb = PL_malloc(sizeof(*cb));
  cb->next = NULL;
  cb->mask = msk;
  cb->pred = p;
  if ( callback_list )
  { callback_tail->next = cb;
    callback_tail = cb;
  } else
  { callback_list = callback_tail = cb;
  }
  joined_mask |= msk;

  return TRUE;
}



		 /*******************************
		 *	       QUERY		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Enumerate the known subjects. This uses the   `first' flag on triples to
avoid returning the same resource multiple   times.  As the `by_none' is
never re-hashed, we don't mark this query in the `active_queries'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
rdf_subject(term_t subject, control_t h)
{ triple *t;
  rdf_db *db = DB;

  switch(PL_foreign_control(h))
  { case PL_FIRST_CALL:
    { if ( PL_is_variable(subject) )
      { t = db->table[BY_NONE][0];
	goto next;
      } else
      { atom_t a;

	if ( get_atom_ex(subject, &a) )
	{ if ( first(db, a) )
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


static foreign_t
rdf_set_predicate(term_t pred, term_t option)
{ predicate *p;
  rdf_db *db = DB;

  if ( !get_predicate(db, pred, &p) )
    return FALSE;

  if ( PL_is_functor(option, FUNCTOR_symmetric1) )
  { int val;

    if ( !get_bool_arg_ex(1, option, &val) )
      return FALSE;

    p->inverse_of = p;
    return TRUE;
  } else if ( PL_is_functor(option, FUNCTOR_inverse_of1) )
  { term_t a = PL_new_term_ref();
    predicate *i;

    PL_get_arg(1, option, a);
    if ( PL_get_nil(a) )
    { if ( p->inverse_of )
      { p->inverse_of->inverse_of = NULL;
	p->inverse_of = NULL;
      }
    } else
    { if ( !get_predicate(db, a, &i) )
	return FALSE;

      p->inverse_of = i;
      i->inverse_of = p;
    }
    return TRUE;
  } else if ( PL_is_functor(option, FUNCTOR_transitive1) )
  { int val;

    if ( !get_bool_arg_ex(1, option, &val) )
      return FALSE;

    p->transitive = val;

    return TRUE;
  } else
    return type_error(option, "predicate_option");
}


#define PRED_PROPERTY_COUNT 9
static functor_t predicate_key[PRED_PROPERTY_COUNT];

static int
unify_predicate_property(rdf_db *db, predicate *p, term_t option, functor_t f)
{ if ( f == FUNCTOR_symmetric1 )
    return PL_unify_term(option, PL_FUNCTOR, f,
			 PL_BOOL, p->inverse_of == p ? TRUE : FALSE);
  else if ( f == FUNCTOR_inverse_of1 )
  { if ( p->inverse_of )
      return PL_unify_term(option, PL_FUNCTOR, f,
			   PL_ATOM, p->inverse_of->name);
    else
      return FALSE;
  } else if ( f == FUNCTOR_transitive1 )
  { return PL_unify_term(option, PL_FUNCTOR, f,
			 PL_BOOL, p->transitive);
  } else if ( f == FUNCTOR_triples1 )
  { return PL_unify_term(option, PL_FUNCTOR, f,
			 PL_LONG, p->triple_count);
  } else if ( f == FUNCTOR_rdf_subject_branch_factor1 )
  { return PL_unify_term(option, PL_FUNCTOR, f,
		 PL_FLOAT, subject_branch_factor(db, p, DISTINCT_DIRECT));
  } else if ( f == FUNCTOR_rdf_object_branch_factor1 )
  { return PL_unify_term(option, PL_FUNCTOR, f,
		 PL_FLOAT, object_branch_factor(db, p, DISTINCT_DIRECT));
  } else if ( f == FUNCTOR_rdfs_subject_branch_factor1 )
  { return PL_unify_term(option, PL_FUNCTOR, f,
		 PL_FLOAT, subject_branch_factor(db, p, DISTINCT_SUB));
  } else if ( f == FUNCTOR_rdfs_object_branch_factor1 )
  { return PL_unify_term(option, PL_FUNCTOR, f,
		 PL_FLOAT, object_branch_factor(db, p, DISTINCT_SUB));
  } else
  { assert(0);
    return FALSE;
  }
}


static foreign_t
rdf_current_predicates(term_t preds)
{ rdf_db *db = DB;
  int i;
  term_t head = PL_new_term_ref();
  term_t tail = PL_copy_term_ref(preds);

  LOCK_MISC(db);
  for(i=0; i<db->pred_table_size; i++)
  { predicate *p;

    for(p=db->pred_table[i]; p; p = p->next)
    { if ( !PL_unify_list(tail, head, tail) ||
	   !PL_unify_atom(head, p->name) )
      { UNLOCK_MISC(db);
	return FALSE;
      }
    }
  }
  UNLOCK_MISC(db);

  return PL_unify_nil(tail);
}  


static foreign_t
rdf_predicate_property(term_t pred, term_t option, control_t h)
{ int n;
  predicate *p;
  rdf_db *db = DB;

  if ( !predicate_key[0] )
  { int i = 0;

    predicate_key[i++] = FUNCTOR_symmetric1;
    predicate_key[i++] = FUNCTOR_inverse_of1;
    predicate_key[i++] = FUNCTOR_transitive1;
    predicate_key[i++] = FUNCTOR_triples1;
    predicate_key[i++] = FUNCTOR_rdf_subject_branch_factor1;
    predicate_key[i++] = FUNCTOR_rdf_object_branch_factor1;
    predicate_key[i++] = FUNCTOR_rdfs_subject_branch_factor1;
    predicate_key[i++] = FUNCTOR_rdfs_object_branch_factor1;
    assert(i < PRED_PROPERTY_COUNT);
  }

  switch(PL_foreign_control(h))
  { case PL_FIRST_CALL:
    { functor_t f;

      if ( PL_is_variable(option) )
      { n = 0;
	goto redo;
      } else if ( PL_get_functor(option, &f) )
      { for(n=0; predicate_key[n]; n++)
	{ if ( predicate_key[n] == f ) 
	  { if ( !get_predicate(db, pred, &p) )
	      return FALSE;
	    return unify_predicate_property(db, p, option, f);
	  }
	}
	return domain_error(option, "rdf_predicate_property");
      } else
	return type_error(option, "rdf_predicate_property");
    }
    case PL_REDO:
      n = (int)PL_foreign_context(h);
    redo:
      if ( !get_predicate(db, pred, &p) )
	return FALSE;
      for( ; predicate_key[n]; n++ )
      { if ( unify_predicate_property(db, p, option, predicate_key[n]) )
	{ n++;
	  if ( predicate_key[n] )
	    PL_retry(n);
	  return TRUE;
	}
      }
      return FALSE;
    case PL_CUTTED:
      return TRUE;
    default:
      assert(0);
      return TRUE;
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


#define AGENDA_LOCAL_MAGIC 742736360
#define AGENDA_SAVED_MAGIC 742736362

typedef struct agenda
{ visited *head;			/* visited list */
  visited *tail;			/* tail of visited list */
  visited *to_expand;			/* next to expand */
  visited *to_return;			/* next to return */
  visited **hash;			/* hash-table for cycle detection */
  int	  magic;			/* AGENDA_*_MAGIC */
  int	  hash_size;
  int     size;				/* size of the agenda */
  triple  pattern;			/* partial triple used as pattern */
  atom_t  target;			/* resource we are seaching for */
  struct chunk  *chunk;			/* node-allocation chunks */
} agenda;

#ifndef offsetof
#define offsetof(structure, field) ((size_t) &(((structure *)NULL)->field))
#endif
#define CHUNK_SIZE(n) offsetof(chunk, nodes[n])

typedef struct chunk
{ struct chunk *next;
  int	 used;				/* # used elements */
  int	 size;				/* size of the chunk */
  struct visited nodes[1];		/* nodes in the chunk */
} chunk;


static visited *
alloc_node_agenda(rdf_db *db, agenda *a)
{ chunk *c;
  int size;

  if ( (c=a->chunk) )
  { if ( c->used < c->size )
    { visited *v = &c->nodes[c->used++];

      return v;
    }
  }

  size = (a->size == 0 ? 8 : 1024);
  c = rdf_malloc(db, CHUNK_SIZE(size));
  c->size = size;
  c->used = 1;
  c->next = a->chunk;
  a->chunk = c;

  return &c->nodes[0];
}


static void
empty_agenda(rdf_db *db, agenda *a)
{ chunk *c, *n;

  for(c=a->chunk; c; c = n)
  { n = c->next;
    rdf_free(db, c, CHUNK_SIZE(c->size));
  }
  if ( a->hash )
    rdf_free(db, a->hash, sizeof(visited*)*a->hash_size);

  if ( a->magic == AGENDA_SAVED_MAGIC )
  {  a->magic = 0;
     rdf_free(db, a, sizeof(*a));
  } else
  { a->magic = 0;
  }
}


static void
unlock_and_empty_agenda(rdf_db *db, agenda *a)
{ RDUNLOCK(db);
  empty_agenda(db, a);
}


static agenda *
save_agenda(rdf_db *db, agenda *a)
{ agenda *r = rdf_malloc(db, sizeof(*r));

  assert(a->magic == AGENDA_LOCAL_MAGIC);
  *r = *a;
  r->magic = AGENDA_SAVED_MAGIC;

  return r;
}


static void
hash_agenda(rdf_db *db, agenda *a, int size)
{ if ( a->hash )
    rdf_free(db, a->hash, sizeof(*a->hash));
  if ( size > 0 )
  { visited *v;

    a->hash = rdf_malloc(db, sizeof(visited*)*size);
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
append_agenda(rdf_db *db, agenda *a, atom_t res)
{ visited *v = a->head;

  if ( in_aganda(a, res) )
    return NULL;

  db->agenda_created++;			/* statistics */

  a->size++; 
  if ( !a->hash_size && a->size > 32 )
    hash_agenda(db, a, 64);
  else if ( a->size > a->hash_size * 4 )
    hash_agenda(db, a, a->hash_size * 4);

  v = alloc_node_agenda(db, a);
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
can_reach_target(rdf_db *db, agenda *a)
{ int indexed = a->pattern.indexed;
  int rc = FALSE;
  triple *p;

  if ( indexed & BY_S )			/* subj ---> */
  { a->pattern.object.resource = a->target;
    indexed |= BY_O;
  } else
  { a->pattern.subject = a->target;
    indexed |= BY_S;
  }

  p = db->table[indexed][triple_hash(db, &a->pattern, indexed)];
  for( ; p; p = p->next[indexed])
  { if ( match_triples(p, &a->pattern, MATCH_SUBPROPERTY) )
    { rc = TRUE;
      break;
    }
  }

  if ( a->pattern.indexed & BY_S )	
  { a->pattern.object.resource = 0;
  } else
  { a->pattern.subject = 0;
  }

  return rc;
}



static visited *
bf_expand(rdf_db *db, agenda *a, atom_t resource)
{ triple *p;
  int indexed = a->pattern.indexed;
  visited *rc = NULL;

  if ( indexed & BY_S )			/* subj ---> */
  { a->pattern.subject = resource;
  } else
  { a->pattern.object.resource = resource;
  }

  if ( a->target && can_reach_target(db, a) )
  { return append_agenda(db, a, a->target);
  }

  p = db->table[indexed][triple_hash(db, &a->pattern, indexed)];
  for( ; p; p = p->next[indexed])
  { if ( match_triples(p, &a->pattern, MATCH_SUBPROPERTY) )
    { atom_t found = (indexed & BY_S) ? p->object.resource : p->subject;
      visited *v;

      v = append_agenda(db, a, found);
      if ( !rc )
	rc = v;
      if ( found == a->target )
	break;
    }
  }
					/* TBD: handle owl:inverseOf */
					/* TBD: handle owl:sameAs */
  return rc;
}


static int
next_agenda(rdf_db *db, agenda *a, atom_t *next)
{ if ( a->to_return )
  { ok:

    *next = a->to_return->resource;
    a->to_return = a->to_return->next;
  
    return TRUE;
  }

  while( a->to_expand )
  { a->to_return = bf_expand(db, a, a->to_expand->resource);
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

directly_attached() deals with the posibility that  the predicate is not
defined and Subject and Object are  the   same.  Should  use clean error
handling, but that means a lot of changes. For now this will do.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
directly_attached(term_t pred, term_t from, term_t to)
{ if ( PL_is_atom(pred) && PL_is_atom(from) )
    return PL_unify(to, from);

  return FALSE;
}


static foreign_t
rdf_reachable(term_t subj, term_t pred, term_t obj, control_t h)
{ rdf_db *db = DB;

  switch(PL_foreign_control(h))
  { case PL_FIRST_CALL:
    { agenda a;
      atom_t r;
      term_t target_term;
      int is_det = FALSE;

      if ( PL_is_variable(pred) )
	return instantiation_error(pred);

      memset(&a, 0, sizeof(a));
      a.magic = AGENDA_LOCAL_MAGIC;

      if ( !PL_is_variable(subj) )		/* subj .... obj */
      { switch(get_partial_triple(db, subj, pred, 0, 0, &a.pattern))
	{ case 0:
	    return directly_attached(pred, subj, obj);
	  case -1:
	    return FALSE;
	}
	is_det = PL_is_ground(obj);
	target_term = obj;
      } else if ( PL_is_atom(obj) )		/* obj .... subj */
      {	switch(get_partial_triple(db, 0, pred, obj, 0, &a.pattern))
	{ case 0:
	    return directly_attached(pred, obj, subj);
	  case -1:
	    return FALSE;
	}
	target_term = subj;
      } else
	return instantiation_error(subj);

      if ( !RDLOCK(db) )
	return FALSE;
      if ( !update_hash(db) )
	return FALSE;
      if ( (a.pattern.indexed & BY_S) ) 	/* subj ... */
	append_agenda(db, &a, a.pattern.subject);
      else
	append_agenda(db, &a, a.pattern.object.resource);
      a.to_return = a.head;
      a.to_expand = a.head;

      while(next_agenda(db, &a, &r))
      { if ( PL_unify_atom(target_term, r) )
	{ if ( is_det )		/* mode(+, +, +) */
	  { unlock_and_empty_agenda(db, &a);
	    return TRUE;
	  } else			/* mode(+, +, -) or mode(-, +, +) */
	  { agenda *ra = save_agenda(db, &a);
	    inc_active_queries(db);
	    DEBUG(9, Sdprintf("Saved agenta to %p\n", ra));
	    PL_retry_address(ra);
	  }
	}
      }
      unlock_and_empty_agenda(db, &a);
      return FALSE;
    }
    case PL_REDO:
    { agenda *a = PL_foreign_context_address(h);
      term_t target_term;
      atom_t r;

      assert(a->magic == AGENDA_SAVED_MAGIC);

      if ( !PL_is_variable(subj) )	/* +, +, - */
	target_term = obj;
      else
	target_term = subj;		/* -, +, + */

      while(next_agenda(db, a, &r))
      { if ( PL_unify_atom(target_term, r) )
	{ assert(a->magic == AGENDA_SAVED_MAGIC);
	  PL_retry_address(a);
	}
      }

      dec_active_queries(db);
      unlock_and_empty_agenda(db, a);
      return FALSE;
    }
    case PL_CUTTED:
    { agenda *a = PL_foreign_context_address(h);

      DEBUG(9, Sdprintf("Cutted; agenda = %p\n", a));

      assert(a->magic == AGENDA_SAVED_MAGIC);

      dec_active_queries(db);
      unlock_and_empty_agenda(db, a);
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

static functor_t keys[16];		/* initialised in install_rdf_db() */

static int
unify_statistics(rdf_db *db, term_t key, functor_t f)
{ int64_t v;

  if ( f == FUNCTOR_triples1 )
  { v = db->created - db->erased;
  } else if ( f == FUNCTOR_subjects1 )
  { v = db->subjects;
  } else if ( f == FUNCTOR_predicates1 )
  { v = db->pred_count;
  } else if ( f == FUNCTOR_core1 )
  { v = db->core;
  } else if ( f == FUNCTOR_indexed8 )
  { int i;
    term_t a = PL_new_term_ref();

    PL_unify_functor(key, FUNCTOR_indexed8);
    for(i=0; i<8; i++)
    { PL_get_arg(i+1, key, a);
      PL_unify_integer(a, db->indexed[i]);
    }

    return TRUE;
  } else if ( f == FUNCTOR_searched_nodes1 )
  { v = db->agenda_created;
  } else if ( f == FUNCTOR_duplicates1 )
  { v = db->duplicates;
  } else if ( f == FUNCTOR_literals1 )
  { v = db->literals.count;
  } else if ( f == FUNCTOR_triples2 && PL_is_functor(key, f) )
  { graph *src;
    term_t a = PL_new_term_ref();
    atom_t name;

    PL_get_arg(1, key, a);
    if ( !PL_get_atom(a, &name) )
      return type_error(a, "atom");
    if ( (src = lookup_graph(db, name, FALSE)) )
      v = src->triple_count;
    else
      v = 0;

    PL_get_arg(2, key, a);
    return PL_unify_int64(a, v);
  } else if ( f == FUNCTOR_gc2 )
  { return PL_unify_term(key,
			 PL_FUNCTOR, f,
			   PL_INT, db->gc_count,
			   PL_FLOAT, db->gc_time); 	/* time spent */
  } else if ( f == FUNCTOR_rehash2 )
  { return PL_unify_term(key,
			 PL_FUNCTOR, f,
			   PL_INT, db->rehash_count,
			   PL_FLOAT, db->rehash_time);
  } else
    assert(0);

  return PL_unify_term(key, PL_FUNCTOR, f, PL_INT64, v);
}

static foreign_t
rdf_statistics(term_t key, control_t h)
{ int n;
  rdf_db *db = DB;

  switch(PL_foreign_control(h))
  { case PL_FIRST_CALL:
    { functor_t f;

      if ( PL_is_variable(key) )
      { n = 0;
	goto redo;
      } else if ( PL_get_functor(key, &f) )
      { for(n=0; keys[n]; n++)
	{ if ( keys[n] == f ) 
	    return unify_statistics(db, key, f);
	}
	return domain_error(key, "rdf_statistics");
      } else
	return type_error(key, "rdf_statistics");
    }
    case PL_REDO:
      n = (int)PL_foreign_context(h);
    redo:
      unify_statistics(db, key, keys[n]);
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
{ rdf_db *db = DB;

  return PL_unify_integer(t, db->generation);
}


		 /*******************************
		 *	       RESET		*
		 *******************************/

static void
erase_triples(rdf_db *db)
{ triple *t, *n;
  int i;

  for(t=db->by_none; t; t=n)
  { n = t->next[BY_NONE];

    free_triple(db, t);
    db->freed++;
  }
  db->by_none = db->by_none_tail = NULL;

  for(i=BY_S; i<=BY_OP; i++)
  { if ( db->table[i] )
    { int bytes = sizeof(triple*) * db->table_size[i];
      
      memset(db->table[i], 0, bytes);
      memset(db->tail[i], 0, bytes);
    }
  }

  db->created = 0;
  db->erased = 0;
  db->freed = 0;
  db->erased = 0;
  db->subjects = 0;
  db->rehash_count = 0;
  memset(db->indexed, 0, sizeof(db->indexed));
  db->duplicates = 0;
  db->generation = 0;
}


static void
erase_predicates(rdf_db *db)
{ predicate **ht;
  int i;

  for(i=0,ht = db->pred_table; i<db->pred_table_size; i++, ht++)
  { predicate *p, *n;

    for( p = *ht; p; p = n )
    { n = p->next;

      free_list(db, &p->subPropertyOf);
      free_list(db, &p->siblings);
      if ( ++p->cloud->deleted == p->cloud->size )
	free_predicate_cloud(db, p->cloud);

      rdf_free(db, p, sizeof(*p));
    }

    *ht = NULL;
  }

  db->pred_count = 0;
  db->next_hash = 0;
}


static void
reset_db(rdf_db *db)
{ db->resetting = TRUE;

  erase_triples(db);
  erase_predicates(db);
  erase_graphs(db);
  db->need_update = FALSE;
  db->agenda_created = 0;
  avlfree(&db->literals);
  init_literal_table(db);

  db->resetting = FALSE;
}


static foreign_t
rdf_reset_db()
{ rdf_db *db = DB;
  
  if ( !WRLOCK(db, FALSE) )
    return FALSE;

  if ( db->tr_first )
  { record_transaction(db, TR_RESET, NULL);
    db->tr_reset = TRUE;
  } else
    reset_db(db);

  WRUNLOCK(db);

  return TRUE;
}


		 /*******************************
		 *	       MATCH		*
		 *******************************/


static foreign_t
match_label(term_t how, term_t search, term_t label)
{ atom_t h, f, l;
  int type;

  if ( !get_atom_ex(how, &h) ||
       !get_atom_ex(search, &f) ||
       !get_atom_ex(label, &l) )
    return FALSE;

  if ( h == ATOM_exact )
    type = STR_MATCH_EXACT;
  else if ( h == ATOM_substring )
    type = STR_MATCH_SUBSTRING;
  else if ( h == ATOM_word )
    type = STR_MATCH_WORD;
  else if ( h == ATOM_prefix )
    type = STR_MATCH_PREFIX;
  else if ( h == ATOM_like )
    type = STR_MATCH_LIKE;
  else
    return domain_error(how, "search_method");

  return match_atoms(type, f, l);
}


static char url_special[128] = {0};
static int  url_special_done = FALSE;

static void
fill_special()
{ if ( !url_special_done )
  { url_special['#'] = TRUE;
    url_special['/'] = TRUE;
    url_special['?'] = TRUE;
    url_special[':'] = TRUE;
    url_special['='] = TRUE;
    url_special['&'] = TRUE;

    url_special_done = TRUE;
  }
}


static foreign_t
split_url(term_t base, term_t local, term_t url)
{ char *b, *l, *u;
  size_t bl, ll;

  if ( local &&
       PL_get_atom_nchars(base, &bl, &b) &&
       PL_get_atom_nchars(local, &ll, &l) )
  { if ( bl+ll < 1024 )
    { char buf[1024];

      memcpy(buf, b, bl);
      memcpy(buf+bl, l, ll);

      return PL_unify_atom_nchars(url, bl+ll, buf);
    } else
    { char *buf = PL_malloc(bl+ll);
      int rc;

      memcpy(buf, b, bl);
      memcpy(buf+bl, l, ll);

      rc = PL_unify_atom_nchars(url, bl+ll, buf);
      PL_free(buf);
      return rc;
    }
  } else if ( PL_get_atom_chars(url, &u) )
  { const unsigned char *s, *last = NULL;

    fill_special();

    for(s = (unsigned char*)u; *s; s++)
    { int c = *s;

      if ( c < 128 && url_special[c] )
	last = s;
    }
    if ( last )
    { const char *l1 = (const char*)last+1;

      if ( (!local || PL_unify_atom_chars(local, l1)) &&
	   PL_unify_atom_nchars(base, l1-u, u) )
	return TRUE;
      else
	return FALSE;
    } else
    { if ( (!local || PL_unify(local, url)) &&
	   PL_unify_atom_chars(base, "") )
	return TRUE;
      else
	return FALSE;
    }
  } else
    return type_error(url, "atom");
}


static foreign_t
url_namespace(term_t url, term_t namespace)
{ return split_url(namespace, 0, url);
}



		 /*******************************
		 *	       VERSION		*
		 *******************************/

static foreign_t
rdf_version(term_t v)
{ return PL_unify_integer(v, RDF_VERSION);
}


		 /*******************************
		 *	     MORE STUFF		*
		 *******************************/

#include "quote.c"

		 /*******************************
		 *	     REGISTER		*
		 *******************************/

#define MKFUNCTOR(n, a) \
	FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)
#define NDET PL_FA_NONDETERMINISTIC
#define META PL_FA_TRANSPARENT

install_t
install_rdf_db()
{ int i=0;
  extern install_t install_atom_map(void);

  MKFUNCTOR(literal, 1);
  MKFUNCTOR(error, 2);
  MKFUNCTOR(type_error, 2);
  MKFUNCTOR(domain_error, 2);
  MKFUNCTOR(triples, 1);
  MKFUNCTOR(triples, 2);
  MKFUNCTOR(subjects, 1);
  MKFUNCTOR(predicates, 1);
  MKFUNCTOR(subject, 1);
  MKFUNCTOR(predicate, 1);
  MKFUNCTOR(object, 1);
  MKFUNCTOR(graph, 1);
  MKFUNCTOR(indexed, 8);
  MKFUNCTOR(exact, 1);
  MKFUNCTOR(substring, 1);
  MKFUNCTOR(word, 1);
  MKFUNCTOR(prefix, 1);
  MKFUNCTOR(like, 1);
  MKFUNCTOR(literal, 2);
  MKFUNCTOR(searched_nodes, 1);
  MKFUNCTOR(duplicates, 1);
  MKFUNCTOR(literals, 1);
  MKFUNCTOR(symmetric, 1);
  MKFUNCTOR(transitive, 1);
  MKFUNCTOR(inverse_of, 1);
  MKFUNCTOR(lang, 2);
  MKFUNCTOR(type, 2);
  MKFUNCTOR(rdf_subject_branch_factor, 1);
  MKFUNCTOR(rdf_object_branch_factor, 1);
  MKFUNCTOR(rdfs_subject_branch_factor, 1);
  MKFUNCTOR(rdfs_object_branch_factor, 1);
  MKFUNCTOR(gc, 2);
  MKFUNCTOR(rehash, 2);
  MKFUNCTOR(core, 1);
  MKFUNCTOR(assert, 4);
  MKFUNCTOR(retract, 4);
  MKFUNCTOR(update, 5);
  MKFUNCTOR(new_literal, 1);
  MKFUNCTOR(old_literal, 1);
  MKFUNCTOR(transaction, 2);
  MKFUNCTOR(load, 2);
  MKFUNCTOR(rehash, 1);
  MKFUNCTOR(begin, 1);
  MKFUNCTOR(end, 1);

  FUNCTOR_colon2 = PL_new_functor(PL_new_atom(":"), 2);

  ATOM_user	     = PL_new_atom("user");
  ATOM_exact	     = PL_new_atom("exact");
  ATOM_prefix	     = PL_new_atom("prefix");
  ATOM_like	     = PL_new_atom("like");
  ATOM_substring     = PL_new_atom("substring");
  ATOM_word	     = PL_new_atom("word");
  ATOM_subPropertyOf = PL_new_atom(URL_subPropertyOf);
  ATOM_error	     = PL_new_atom("error");
  ATOM_begin	     = PL_new_atom("begin");
  ATOM_end	     = PL_new_atom("end");

  PRED_call1         = PL_predicate("call", 1, "user");

					/* statistics */
  keys[i++] = FUNCTOR_triples1;
  keys[i++] = FUNCTOR_subjects1;
  keys[i++] = FUNCTOR_indexed8;
  keys[i++] = FUNCTOR_predicates1;
  keys[i++] = FUNCTOR_searched_nodes1;
  keys[i++] = FUNCTOR_duplicates1;
  keys[i++] = FUNCTOR_literals1;
  keys[i++] = FUNCTOR_triples2;
  keys[i++] = FUNCTOR_gc2;
  keys[i++] = FUNCTOR_rehash2;
  keys[i++] = FUNCTOR_core1;
  keys[i++] = 0;

					/* setup the database */
  DB = new_db();

  PL_register_foreign("rdf_version",    1, rdf_version,     0);
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
  PL_register_foreign("rdf_url_namespace", 2, url_namespace,0);
  PL_register_foreign("rdf_save_db_",   2, rdf_save_db,     0);
  PL_register_foreign("rdf_load_db_",   3, rdf_load_db,     0);
  PL_register_foreign("rdf_reachable",  3, rdf_reachable,   NDET);
  PL_register_foreign("rdf_reset_db_",  0, rdf_reset_db,    0);
  PL_register_foreign("rdf_set_predicate",
					2, rdf_set_predicate, 0);
  PL_register_foreign("rdf_predicate_property_",
					2, rdf_predicate_property, NDET);
  PL_register_foreign("rdf_current_predicates",
					1, rdf_current_predicates, 0);
  PL_register_foreign("rdf_current_literal",
					1, rdf_current_literal, NDET);
  PL_register_foreign("rdf_graphs_",    1, rdf_graphs,      0);
  PL_register_foreign("rdf_set_graph_source", 2, rdf_set_graph_source, 0);
  PL_register_foreign("rdf_graph_source_", 2, rdf_graph_source, 0);
  PL_register_foreign("rdf_estimate_complexity",
					4, rdf_estimate_complexity, 0);
  PL_register_foreign("rdf_transaction_",2, rdf_transaction, META);
  PL_register_foreign("rdf_active_transactions_",
					1, rdf_active_transactions, 0);
  PL_register_foreign("rdf_monitor_",   2, rdf_monitor,     META);
/*PL_register_foreign("rdf_broadcast_", 2, rdf_broadcast,   0);*/
#ifdef WITH_MD5
  PL_register_foreign("rdf_md5",	2, rdf_md5,	    0);
  PL_register_foreign("rdf_atom_md5",	3, rdf_atom_md5,    0);
#endif
  PL_register_foreign("rdf_quote_uri",	2, rdf_quote_uri,   0);

#ifdef O_DEBUG
  PL_register_foreign("rdf_debug",      1, rdf_debug,       0);
  PL_register_foreign("rdf_print_predicate_cloud", 1, rdf_print_predicate_cloud, 0);
#endif
#ifdef O_SECURE
  PL_register_foreign("rdf_dump_literals", 0, dump_literals, 0);
  PL_register_foreign("rdf_check_literals", 0, check_transitivity, 0);
#endif

  install_atom_map();
}
