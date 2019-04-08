/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017-2019, VU University Amsterdam
			      CWI, Amsterdam
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

#include "pl-incl.h"
#include "pl-tabling.h"
#include "pl-copyterm.h"

#define record_t fastheap_term *
#define PL_record(t)      term_to_fastheap(t PASS_LD)
#define PL_recorded(r, t) put_fastheap(r, t PASS_LD)
#define PL_erase(r)	  free_fastheap(r)

static void	free_worklist(worklist *wl);
static void	free_worklist_set(worklist_set *wls, int freewl);
static void	add_global_worklist(worklist *wl);
static cluster *new_answer_cluster(trie_node *first);
static void	wkl_append_left(worklist *wl, cluster *c);
static int	tbl_put_trie_value(term_t t, trie_node *node ARG_LD);
static void	del_child_component(tbl_component *parent, tbl_component *child);
static void	free_components_set(component_set *cs, int destroy);
#ifdef O_DEBUG
static void	print_worklist(const char *prefix, worklist *wl);
#endif

		 /*******************************
		 *	     COMPONENTS		*
		 *******************************/

static tbl_component *
new_component(void)
{ tbl_component *c = PL_malloc(sizeof(*c));

  memset(c, 0, sizeof(*c));
  c->magic = COMPONENT_MAGIC;

  return c;
}

#define FC_DESTROY	0x0001
#define FC_CHILD	0x0002

static void
free_component(tbl_component *c, int flags)
{ GET_LD
  assert(c->magic == COMPONENT_MAGIC);
  c->magic = 0;

  if ( c == LD->tabling.component )
  { LD->tabling.component = c->parent;
    if ( !c->parent && LD->tabling.has_scheduling_component )
      LD->tabling.has_scheduling_component = FALSE;
  }

  if ( !(flags&FC_CHILD) && c->parent )
    del_child_component(c->parent, c);
  if ( c->worklist )
    free_worklist_set(c->worklist, FALSE);
  if ( c->created_worklists )
    free_worklist_set(c->created_worklists, TRUE);
  if ( c->children )
    free_components_set(c->children, flags|FC_CHILD);

  PL_free(c);
}


static void
add_child_component(tbl_component *parent, tbl_component *child)
{ component_set *cs;

  if ( !(cs=parent->children) )
  { cs = PL_malloc(sizeof(*cs));
    initBuffer(&cs->members);
    parent->children = cs;
  }

  addBuffer(&cs->members, child, tbl_component*);
}

/*
static void
print_component_set(char *why, component_set *cs)
{ tbl_component **bp = baseBuffer(&cs->members, tbl_component*);
  tbl_component **tp = topBuffer(&cs->members, tbl_component*);

  Sdprintf("%s:", why);
  for(;	bp < tp; bp++)
  { tbl_component *c = *bp;
    Sdprintf(" %p", c);
  }
  Sdprintf("\n");
}
*/

static void
del_child_component(tbl_component *parent, tbl_component *child)
{ component_set *cs = parent->children;
  tbl_component **bp = baseBuffer(&cs->members, tbl_component*);
  tbl_component **tp = topBuffer(&cs->members, tbl_component*);

  for(; *bp != child && bp < tp; bp++)
    ;
  assert(bp < tp);
  memmove(bp, bp+1, (tp-bp-1)*sizeof(*bp));
  (void)popBuffer(&cs->members, tbl_component*);
}

static void
free_components_set(component_set *cs, int flags)
{ if ( (flags & FC_DESTROY) )
  { tbl_component **bp = baseBuffer(&cs->members, tbl_component*);
    tbl_component **tp = topBuffer(&cs->members, tbl_component*);

    for(; bp < tp; bp++)
      free_component(*bp, flags);
  }

  discardBuffer(&cs->members);
  PL_free(cs);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Merge all subcomponets of c into c.   The properties of the subcomponets
are destroyed and .status is set to SCC_MERGED.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void merge_children(tbl_component *c, component_set *cs);
static void merge_one_component(tbl_component *c, tbl_component *m);
static void wls_set_component(worklist_set *wls, tbl_component *c);

static void
merge_component(tbl_component *c)
{ if ( c->children )
    merge_children(c, c->children);

  wls_set_component(c->worklist, c);
  wls_set_component(c->created_worklists, c);
}

static void
wls_set_component(worklist_set *wls, tbl_component *c)
{ worklist **base = baseBuffer(&wls->members, worklist*);
  worklist **top  = topBuffer(&wls->members, worklist*);

  for(; base < top; base++)
  { worklist *wl = *base;

    wl->component = c;
    wl->executing = FALSE;
    if ( !wl->in_global_wl )
      add_global_worklist(wl);
  }
}

static void
merge_children(tbl_component *c, component_set *cs)
{ tbl_component **bp = baseBuffer(&cs->members, tbl_component*);
  tbl_component **tp = topBuffer(&cs->members, tbl_component*);

  for(;	bp < tp; bp++)
    merge_one_component(c, *bp);
}


static void
merge_worklists(worklist_set **into, worklist_set **from)
{ typedef worklist* Worklist;

  if ( *into && *from )
  { worklist **s = baseBuffer(&(*from)->members, worklist*);
    size_t   cnt = entriesBuffer(&(*from)->members, worklist*);
    Buffer     b = &(*into)->members;

    addMultipleBuffer(b, s, cnt, Worklist);
    free_worklist_set(*from, FALSE);
    *from = NULL;
  } else if ( *from )
  { *into = *from;
    *from = NULL;
  }
}


static void
merge_one_component(tbl_component *c, tbl_component *m)
{ assert(m->magic == COMPONENT_MAGIC);

  if ( m->status != SCC_ACTIVE )
    return;

  if ( m->children )
    merge_children(c, m->children);

  DEBUG(MSG_TABLING_WORK,
	Sdprintf("Merged %p into %p, %zd worklists, %zd created\n",
		 m, c,
		 entriesBuffer(&m->worklist->members, worklist*),
		 entriesBuffer(&m->created_worklists->members, worklist*)));

  merge_worklists(&c->worklist, &m->worklist);
  merge_worklists(&c->created_worklists, &m->created_worklists);
  m->status = SCC_MERGED;
}

		 /*******************************
		 *           WORKLISTS		*
		 *******************************/

static void
add_global_worklist(worklist *wl)
{ tbl_component *c = wl->component;
  worklist_set *wls;

  if ( !(wls=c->worklist) )
  { wls	= c->worklist = PL_malloc(sizeof(*c->worklist));
    initBuffer(&wls->members);
  }

  addBuffer(&wls->members, wl, worklist*);
  wl->in_global_wl = TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Normal completion is done. There  may   be  worklists that are suspended
using negation_suspend/3. We wake  these  up   by  adding  a  new answer
cluster with a NULL node.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static worklist *
negative_worklist(tbl_component *c)
{ worklist **wlp = baseBuffer(&c->created_worklists->members, worklist*);
  worklist **top = topBuffer(&c->created_worklists->members, worklist*);

  for(; wlp < top; wlp++)
  { worklist *wl = *wlp;

    if ( wl->negative &&
	 !wl->neg_complete &&
	 !wl->has_answers )		/* FIXME: could also use neg_complete? */
    { cluster *c;

      wl->neg_complete = TRUE;
      DEBUG(MSG_TABLING_NEG,
	    Sdprintf("Resume negative node %zd\n", pointerToInt(wl)));

      c = new_answer_cluster(NULL);
      wkl_append_left(wl, c);
      if ( !wl->riac )
	wl->riac = c;

      return wl;
    }
  }

  return NULL;
}


static worklist *
pop_worklist(tbl_component *c)
{ worklist_set *wls;

  if ( (wls=c->worklist) && !isEmptyBuffer(&wls->members) )
  { worklist *wl = popBuffer(&wls->members, worklist*);
    wl->in_global_wl = FALSE;

    return wl;
  }

  return negative_worklist(c);
}


static void
reset_global_worklist(tbl_component *c)
{ worklist_set *wls;

  if ( c && (wls = c->worklist) )
  { c->worklist = NULL;
    free_worklist_set(wls, FALSE);
  }
}


static void
add_newly_created_worklist(worklist *wl)
{ tbl_component *c = wl->component;
  worklist_set *wls;

  if ( !(wls=c->created_worklists) )
  { wls	= c->created_worklists = PL_malloc(sizeof(*c->created_worklists));
    initBuffer(&wls->members);
  }

  addBuffer(&wls->members, wl, worklist*);
}

static void
reset_newly_created_worklists(tbl_component *c)
{ worklist_set *wls;

  if ( c && (wls = c->created_worklists) )
  { c->created_worklists = NULL;
    free_worklist_set(wls, TRUE);
  }
}

static size_t
worklist_set_to_array(worklist_set *wls, worklist ***wlp)
{ if ( wls )
  { *wlp = (worklist**)baseBuffer(&wls->members, worklist*);
    return entriesBuffer(&wls->members, worklist*);
  } else
  { *wlp = NULL;
    return 0;
  }
}

static void
free_worklist_set(worklist_set *wls, int freewl)
{ if ( freewl )
  { worklist **wlp = (worklist**)baseBuffer(&wls->members, worklist*);
    size_t i, nwpl = entriesBuffer(&wls->members, worklist*);

    for(i=0; i<nwpl; i++)
    { worklist *wl = wlp[i];

      free_worklist(wl);
    }
  }

  discardBuffer(&wls->members);
  PL_free(wls);
}


		 /*******************************
		 *     THREAD VARIANT TABLE	*
		 *******************************/

static void release_variant_table_node(trie *trie, trie_node *node);

static trie *
thread_variant_table(ARG1_LD)
{ if ( !LD->tabling.variant_table )
  { LD->tabling.variant_table = trie_create();
    trie_symbol(LD->tabling.variant_table);
    LD->tabling.variant_table->release_node = release_variant_table_node;
  }

  return LD->tabling.variant_table;
}


static void
release_variant_table_node(trie *variant_table, trie_node *node)
{ (void)variant_table;

  if ( node->value )
  { trie *vtrie = symbol_trie(node->value);

    assert(vtrie->data.variant == node);
    vtrie->data.variant = NULL;
    vtrie->data.worklist = NULL;
    trie_empty(vtrie);
  }
}


static void
clear_variant_table(PL_local_data_t *ld)
{ if ( ld->tabling.variant_table )
  { trie_empty(ld->tabling.variant_table);
    PL_unregister_atom(ld->tabling.variant_table->symbol);
    ld->tabling.variant_table = NULL;
  }
}


static trie *
get_variant_table(term_t t, int create ARG_LD)
{ trie *variants = thread_variant_table(PASS_LD1);
  trie_node *node;
  int rc;
  Word v = valTermRef(t);

  if ( (rc=trie_lookup(variants, &node, v, create PASS_LD)) == TRUE )
  { if ( node->value )
    { return symbol_trie(node->value);
    } else if ( create )
    { trie *vt = trie_create();
      node->value = trie_symbol(vt);
      vt->data.variant = node;
      vt->alloc_pool = &LD->tabling.node_pool;
      return vt;
    } else
      return NULL;
  }

  trie_error(rc, t);
  return NULL;
}


void
clearThreadTablingData(PL_local_data_t *ld)
{ reset_global_worklist(ld->tabling.component);
  reset_newly_created_worklists(ld->tabling.component);
  clear_variant_table(ld);
}



		 /*******************************
		 *  ANSWER/SUSPENSION CLUSTERS	*
		 *******************************/

static cluster *
new_answer_cluster(trie_node *first)
{ cluster *c;

  c = PL_malloc(sizeof(*c));
  c->type = CLUSTER_ANSWERS;
  initBuffer(&c->members);
  addBuffer(&c->members, first, trie_node*);

  return c;
}

static void
free_answer_cluster(cluster *c)
{ discardBuffer(&c->members);
  PL_free(c);
}

static void
add_to_answer_cluster(cluster *c, trie_node *answer)
{ addBuffer(&c->members, answer, trie_node*);
}

static trie_node*
get_answer_from_cluster(cluster *c, size_t index)
{ if ( index < entriesBuffer(&c->members, trie_node*) )
    return fetchBuffer(&c->members, index, trie_node*);
  return NULL;
}

static cluster *
new_suspension_cluster(term_t first ARG_LD)
{ cluster *c;
  record_t r;

  if ( !(r=PL_record(first)) )
    return NULL;

  c = PL_malloc(sizeof(*c));
  c->type = CLUSTER_SUSPENSIONS;
  initBuffer(&c->members);
  addBuffer(&c->members, r, record_t);

  return c;
}

static void
free_suspension_cluster(cluster *c)
{ record_t *base = baseBuffer(&c->members, record_t);
  size_t entries = entriesBuffer(&c->members, record_t);
  size_t i;

  for(i=0; i<entries; i++)
    PL_erase(base[i]);

  discardBuffer(&c->members);
  PL_free(c);
}

static int
add_to_suspension_cluster(cluster *c, term_t suspension ARG_LD)
{ record_t r;

  if ( (r=PL_record(suspension)) )
  { addBuffer(&c->members, r, record_t);
    return TRUE;
  }

  return FALSE;
}

static record_t
get_suspension_from_cluster(cluster *c, size_t index)
{ if ( index < entriesBuffer(&c->members, record_t) )
    return fetchBuffer(&c->members, index, record_t);
  return 0;
}

static void
free_cluster(cluster *c)
{ if ( c->type == CLUSTER_ANSWERS )
    free_answer_cluster(c);
  else
    free_suspension_cluster(c);
}

static int
acp_size(cluster *c)
{ return entriesBuffer(&c->members, trie_node*);
}

static int
scp_size(cluster *c)
{ return entriesBuffer(&c->members, record_t);
}

		 /*******************************
		 *	   TABLE WORKLIST	*
		 *******************************/

static worklist *
new_worklist(trie *trie)
{ worklist *wl;

  wl = PL_malloc(sizeof(*wl));
  memset(wl, 0, sizeof(*wl));
  wl->magic = WORKLIST_MAGIC;
  wl->table = trie;
  trie->data.worklist = wl;

  return wl;
}


static void
free_worklist(worklist *wl)
{ cluster *c, *next;

  assert(wl->magic == WORKLIST_MAGIC);
  wl->magic = 0;

  for(c=wl->head; c; c = next)
  { next = c->next;

    free_cluster(c);
  }

  PL_free(wl);
}


static int
worklist_negative(worklist *wl)
{ wl->negative = TRUE;

  return TRUE;
}

static int
worklist_is_false(worklist *wl)
{ assert(wl->negative);
  return wl->neg_complete && !wl->has_answers;
}


/* The work is done if there is no answer cluster or there is
   no suspension right of the answer cluster
*/

static int
worklist_work_done(worklist *wl)
{ return !wl->riac || !wl->riac->next;
}


static void
wkl_append_left(worklist *wl, cluster *c)
{ if ( wl->head )
  { c->prev = NULL;
    c->next = wl->head;
    wl->head->prev = c;
    wl->head = c;
  } else
  { c->next = c->prev = NULL;
    wl->head = wl->tail = c;
  }
}


static void
wkl_append_right(worklist *wl, cluster *c)
{ if ( wl->tail )
  { c->next = NULL;
    c->prev = wl->tail;
    wl->tail->next = c;
    wl->tail = c;
  } else
  { c->next = c->prev = NULL;
    wl->head = wl->tail = c;
  }
}


static void
update_riac(worklist *wl, cluster *acp)
{ cluster *c;

  if ( !acp->next ||
       acp->next->type == CLUSTER_ANSWERS )
  { for(c=acp->prev; c; c = c->prev)
    { if ( c->type == CLUSTER_ANSWERS )
      { wl->riac = c;
	return;
      }
    }

    wl->riac = NULL;
  }
}


static void
wkl_swap_clusters(worklist *wl, cluster *acp, cluster *scp)
{ cluster *a = acp->prev;		/* before the couple */
  cluster *z = scp->next;		/* after the couple */

  assert(acp->next == scp);

  if ( a ) a->next = scp; else wl->head = scp;
  if ( z ) z->prev = acp; else wl->tail = acp;
  scp->prev = a;
  acp->next = z;
  scp->next = acp;
  acp->prev = scp;

  update_riac(wl, acp);

  DEBUG(MSG_TABLING_WORK, print_worklist("Swapped: ", wl));
}


static void
potentially_add_to_global_worklist(worklist *wl ARG_LD)
{ if ( !wl->in_global_wl && !wl->executing )
    add_global_worklist(wl);
}


static int
wkl_add_answer(worklist *wl, trie_node *node ARG_LD)
{ potentially_add_to_global_worklist(wl PASS_LD);
  wl->has_answers = TRUE;
  if ( wl->head && wl->head->type == CLUSTER_ANSWERS )
  { add_to_answer_cluster(wl->head, node);
  } else
  { cluster *c = new_answer_cluster(node);
    wkl_append_left(wl, c);
    if ( !wl->riac )
      wl->riac = c;
  }
  DEBUG(MSG_TABLING_WORK, print_worklist("Added answer: ", wl));

  return TRUE;
}


static int
wkl_add_suspension(worklist *wl, term_t suspension ARG_LD)
{ potentially_add_to_global_worklist(wl PASS_LD);
  if ( wl->tail && wl->tail->type == CLUSTER_SUSPENSIONS )
  { if ( !add_to_suspension_cluster(wl->tail, suspension PASS_LD) )
      return FALSE;
  } else
  { cluster *c = new_suspension_cluster(suspension PASS_LD);
    if ( !c )
      return FALSE;
    wkl_append_right(wl, c);
    if ( c->prev && c->prev->type == CLUSTER_ANSWERS )
      wl->riac = c->prev;
  }
  DEBUG(MSG_TABLING_WORK, print_worklist("Added suspension: ", wl));

  return TRUE;
}


#ifdef O_DEBUG
static void
print_worklist(const char *prefix, worklist *wl)
{ cluster *c;

  Sdprintf("%s", prefix);
  for(c=wl->head; c; c=c->next)
  { if ( c->type == CLUSTER_ANSWERS )
    { Sdprintf("ACP(%d)%s ", acp_size(c), c == wl->riac ? "[RIAC]" : "");
    } else
    { Sdprintf("SCP(%d) ", scp_size(c));
    }
  }
  Sdprintf("\n");
}
#endif



		 /*******************************
		 *	PROLOG CONNECTION	*
		 *******************************/

#define WL_IS_SPECIAL(wl)  (((intptr_t)(wl)) & 0x1)
#define WL_IS_WORKLIST(wl) ((wl) && !WL_IS_SPECIAL(wl))

#define WL_COMPLETE ((worklist *)0x11)

static int
unify_table_status(term_t t, trie *trie ARG_LD)
{ worklist *wl = trie->data.worklist;

  if ( WL_IS_WORKLIST(wl) )
  { if ( wl->component != LD->tabling.component )
    { DEBUG(MSG_TABLING_WORK,
	    Sdprintf("Merging into %p (current = %p)\n",
		     wl->component, LD->tabling.component));
      merge_component(wl->component);
      LD->tabling.component = wl->component;
    }
    return PL_unify_pointer(t, wl);
  }
  if ( !wl )
    return PL_unify_atom(t, ATOM_fresh);
  if ( wl == WL_COMPLETE )
    return PL_unify_atom(t, ATOM_complete);

  assert(0);
  return FALSE;
}

static int
unify_skeleton(trie *trie, term_t wrapper, term_t skeleton ARG_LD)
{ if ( trie->data.skeleton )
  { term_t av;

    return ( (av=PL_new_term_refs(2)) &&
	     PL_recorded(trie->data.skeleton, av+0) &&
	     (_PL_get_arg(1, av+0, av+1),TRUE) &&
	     PL_unify(wrapper, av+1) &&
	     (_PL_get_arg(2, av+0, av+1),TRUE) &&
	     PL_unify(skeleton, av+1) );
  } else
  { term_t av;

    if ( (av = PL_new_term_ref()) &&
	 term_var_skeleton(wrapper, av PASS_LD) &&
	 PL_unify(av, skeleton) &&
	 PL_cons_functor(av, FUNCTOR_minus2, wrapper, av) )
    { trie->data.skeleton = PL_record(av);
      return TRUE;
    }

    return FALSE;
  }
}


static int
get_scc(term_t t, tbl_component **cp)
{ void *ptr;

  if ( PL_get_pointer_ex(t, &ptr) )
  { tbl_component *c = ptr;

    if ( c->magic != COMPONENT_MAGIC )
      return PL_existence_error("table component", t);

    *cp = c;

    return TRUE;
  }

  return FALSE;
}

static int
get_worklist(term_t t, worklist **wlp)
{ GET_LD
  void *ptr;

  if ( PL_get_pointer(t, &ptr) )
  { worklist *wl = ptr;
    assert(wl->magic == WORKLIST_MAGIC);
    *wlp = wl;
    return TRUE;
  }

  PL_type_error("worklist", t);
  return FALSE;
}

/*
static int
get_trie_node(term_t t, trie_node **np)
{ GET_LD
  void *ptr;

  if ( PL_get_pointer(t, &ptr) )
  { trie_node *n = ptr;
    *np = n;
    return TRUE;
  }

  return PL_type_error("trie_node", t);
}
*/

/** '$tbl_new_worklist'(-Worklist, +Trie) is det.
 *
 * Create a new worklist for Trie add add it to the global worklist
 * set.
 */

static
PRED_IMPL("$tbl_new_worklist", 2, tbl_new_worklist, 0)
{ PRED_LD
  trie *trie;

  if ( get_trie(A2, &trie) )
  { worklist *wl = new_worklist(trie);

    wl->component = LD->tabling.component;
    add_global_worklist(wl);
    add_newly_created_worklist(wl);
    return PL_unify_pointer(A1, wl);
  }

  return FALSE;
}


/** '$tbl_destroy_table'(+Trie)
 *
 * Destroy a single trie table.
 */

static
PRED_IMPL("$tbl_destroy_table", 1, tbl_destroy_table, 0)
{ PRED_LD
  trie *table;

  if ( get_trie(A1, &table) )
  { if ( table->data.variant )
    { trie *vtrie = get_trie_form_node(table->data.variant);

      if ( vtrie == LD->tabling.variant_table )
      { prune_node(vtrie, table->data.variant);
	return TRUE;
      }

      return PL_type_error("table", A1);
    }
  }

  return FALSE;
}


/** '$tbl_pop_worklist'(+SCC, -Worklist) is semidet.
 *
 * Pop next worklist from the component.
 */

static
PRED_IMPL("$tbl_pop_worklist", 2, tbl_pop_worklist, 0)
{ PRED_LD
  tbl_component *scc;

  if ( get_scc(A1, &scc) )
  { worklist *wl;

    if ( (wl=pop_worklist(scc)) )
      return PL_unify_pointer(A2, wl);
  }

  return FALSE;
}

/** '$tbl_wkl_add_answer'(+Worklist, +Term) is semidet.
 *
 * Add an answer to the worklist's trie  and the worklist answer cluster
 * using trie_insert_new/3. Fails if a  variant   of  Term is already in
 * Worklist.
 */

static
PRED_IMPL("$tbl_wkl_add_answer", 2, tbl_wkl_add_answer, 0)
{ PRED_LD
  worklist *wl;

  if ( get_worklist(A1, &wl) )
  { Word kp;
    trie_node *node;
    int rc;

    kp = valTermRef(A2);

    if ( (rc=trie_lookup(wl->table, &node, kp, TRUE PASS_LD)) == TRUE )
    { if ( node->value )
      { if ( node->value == ATOM_trienode )
	  return FALSE;				/* already in trie */
	return PL_permission_error("modify", "trie_key", A2);
      }
      node->value = ATOM_trienode;

      return wkl_add_answer(wl, node PASS_LD);
    }

    return trie_error(rc, A2);
  }

  return FALSE;
}

/** '$tbl_wkl_mode_add_answer'(+Worklist, +TermNoModes, +Args, +Term) is semidet.
 *
 * Add an answer Args for moded arguments to the worklist's trie and the
 * worklist answer cluster using  trie_insert_new/3   and  mode directed
 * tabling.
 *
 * @arg TermNoModes is the call variant without moded arguments
 * @arg Args is a term holding the moded arguments.  If there is
 * only one moded argument, this is the value.  Otherwise it is a
 * term s(V1,V2,...).  See extract_modes/5.
 * @arg Term is the full tabled goal, including moded
 * arguments. This is is passed to update/4 to find the correct
 * update clause.
 */

static
PRED_IMPL("$tbl_wkl_mode_add_answer", 4, tbl_wkl_mode_add_answer, 0)
{ PRED_LD
  worklist *wl;

  if ( get_worklist(A1, &wl) )
  { Word kp;
    trie_node *node;
    int rc;

    kp = valTermRef(A2);
    DEBUG(MSG_TABLING_MODED,
	  { PL_write_term(Serror, A2, 1200, 0);
	    Sdprintf(": ");
	  });

    if ( (rc=trie_lookup(wl->table, &node, kp, TRUE PASS_LD)) == TRUE )
    { if ( node->value )
      { static predicate_t PRED_update4 = 0;
	term_t av;

	if ( !PRED_update4 )
	  PRED_update4 = PL_predicate("update", 4, "$tabling");

	if ( !((av=PL_new_term_refs(4)) &&
	       PL_put_term(av+0, A4) &&
	       tbl_put_trie_value(av+1, node PASS_LD) &&
	       PL_put_term(av+2, A3) &&
	       PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, PRED_update4, av) &&
	       set_trie_value(node, av+3 PASS_LD)) )
	{ DEBUG(MSG_TABLING_MODED, Sdprintf("No change!\n"));
	  return FALSE;
	}

	DEBUG(MSG_TABLING_MODED,
	      { Sdprintf("Updated answer to: ");
		PL_write_term(Serror, av+3, 1200, PL_WRT_NEWLINE);
	      });
	return wkl_add_answer(wl, node PASS_LD);
      } else
      { if ( !set_trie_value(node, A3 PASS_LD) )
	  return FALSE;

	DEBUG(MSG_TABLING_MODED,
	      { Sdprintf("Set first answer: ");
		PL_write_term(Serror, A3, 1200, PL_WRT_NEWLINE);
	      });
	return wkl_add_answer(wl, node PASS_LD);
      }
    }

    return trie_error(rc, A2);
  }

  return FALSE;
}


/** '$tbl_wkl_add_suspension'(+Worklist, +Suspension) is det.
 *
 * Add a suspension to the worklist.
 */

static
PRED_IMPL("$tbl_wkl_add_suspension", 2, tbl_wkl_add_suspension, 0)
{ PRED_LD
  worklist *wl;

  if ( get_worklist(A1, &wl) )
    return wkl_add_suspension(wl, A2 PASS_LD);

  return FALSE;
}

/** '$tbl_wkl_done'(+Worklist) is semidet.
 *
 * True if the worklist is complete
 */

static
PRED_IMPL("$tbl_wkl_done", 1, tbl_wkl_done, 0)
{ worklist *wl;

  return get_worklist(A1, &wl) && worklist_work_done(wl);
}

/** '$tbl_wkl_negative'(+Worklist) is semidet.
 *
 * True if the worklist is complete
 */

static
PRED_IMPL("$tbl_wkl_negative", 1, tbl_wkl_negative, 0)
{ worklist *wl;

  return get_worklist(A1, &wl) && worklist_negative(wl);
}


/** '$tbl_tbl_wkl_is_false'(+Worklist) is semidet.
 *
 * True if the worklist is is a negative node that is true (has no
 * solutions)
 */

static
PRED_IMPL("$tbl_wkl_is_false", 1, tbl_wkl_is_false, 0)
{ worklist *wl;

  return get_worklist(A1, &wl) && worklist_is_false(wl);
}


/** '$tbl_wkl_work'(+Worklist, -Answer, -ModeArgs,
 *		    -Goal, -Continuation, -Wrapper, -TargetTable) is nondet.
 *
 * True when Answer must be tried on Suspension.  Backtracking
 * basically does
 *
 *   ==
 *   member(Answer, RIAC),
 *   member(Suspension, LastSuspensionCluster)
 *   ==
 *
 * If the carthesian product is exhausted it tries to re-start using the
 * possible new RIAC and SCP.  During its execution, worklist->executing
 * is TRUE to avoid the worklist to   become part of the global worklist
 * again.
 *
 * This replaces table_get_work/3 from the pure Prolog implementation.
 */

typedef struct
{ worklist *list;
  cluster *acp;
  cluster *scp;
  int acp_size;
  int scp_size;
  int acp_index;
  int scp_index;
  int iteration;
  int next_step;
} wkl_step_state;


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unify the 4 arguments  of  the   dependecy  structure  with subsequent 4
output arguments.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline void
unify_arg_term(term_t a, Word v ARG_LD)
{ Word p = valTermRef(a);

  deRef(p);
  DEBUG(CHK_SECURE, assert(isVar(*p)));
  Trail(p, linkVal(v));
}

static int
unify_dependency(term_t a0, term_t dependency ARG_LD)
{ if ( tTop + 4 < tMax ||
       makeMoreStackSpace(TRAIL_OVERFLOW, ALLOW_GC|ALLOW_SHIFT) )
  { Word dp = valTermRef(dependency);
    Functor f;

    deRef(dp);
    if ( unlikely(!isTerm(*dp)) )
      return FALSE;
    f = (Functor)valPtr(*dp);

    unify_arg_term(a0+0, &f->arguments[0] PASS_LD);
    unify_arg_term(a0+1, &f->arguments[1] PASS_LD);
    unify_arg_term(a0+2, &f->arguments[2] PASS_LD);
    unify_arg_term(a0+3, &f->arguments[3] PASS_LD);

    return TRUE;
  }

  return FALSE;
}


static int
tbl_unify_trie_term(trie_node *node, term_t term ARG_LD)
{ if ( node )
    return unify_trie_term(node, term PASS_LD);

  return TRUE;				/* for negative dummy solutions */
}

static int
tbl_put_trie_value(term_t t, trie_node *node ARG_LD)
{ if ( node )
  { return put_trie_value(t, node PASS_LD);
  } else
  { *valTermRef(t) = ATOM_trienode;
    return TRUE;
  }
}


static
PRED_IMPL("$tbl_wkl_work", 7, tbl_wkl_work, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  wkl_step_state *state;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { worklist *wl;

      if ( get_worklist(A1, &wl) )
      { cluster *acp, *scp;

	if ( (acp=wl->riac) && (scp=acp->next) )
	{ DEBUG(MSG_TABLING_WORK,
		print_worklist("First step: ", wl));
	  wkl_swap_clusters(wl, acp, scp);
	  state = allocForeignState(sizeof(*state));
	  memset(state, 0, sizeof(*state));
	  state->list	   = wl;
	  state->acp	   = acp;
	  state->scp	   = scp;
	  state->acp_index = state->acp_size = acp_size(acp);
	  state->scp_index = state->scp_size = scp_size(scp);
	  wl->executing    = TRUE;

	  break;
	}
      }

      return FALSE;
    }
    case FRG_REDO:
      state = CTX_PTR;
      break;
    case FRG_CUTTED:
      state = CTX_PTR;
      state->list->executing = FALSE;
      freeForeignState(state, sizeof(*state));
      return TRUE;
    default:
      assert(0);
      return FALSE;
  }

  if ( state->next_step )
  { cluster *acp, *scp;

    if ( (acp=state->list->riac) && (scp=acp->next) )
    { DEBUG(MSG_TABLING_WORK,
	    print_worklist("Next step: ", state->list));
      assert(acp->type == CLUSTER_ANSWERS);
      assert(scp->type == CLUSTER_SUSPENSIONS);
      wkl_swap_clusters(state->list, acp, scp);
      state->acp       = acp;
      state->scp       = scp;
      state->acp_index = state->acp_size = acp_size(acp);
      state->scp_index = state->scp_size = scp_size(scp);
      state->next_step = FALSE;
    } else
    { DEBUG(MSG_TABLING_WORK,
	    Sdprintf("No more work in worklist\n"));
    }
  }

  if ( state->next_step == FALSE && state->acp_index > 0 )
  { trie_node *an = get_answer_from_cluster(state->acp, state->acp_index-1);

    if ( state->scp_index > 0 )
    { record_t sr       = get_suspension_from_cluster(state->scp,
						      state->scp_index-1);
      term_t av         = PL_new_term_refs(2);
      term_t suspension = av+0;
      term_t modeargs   = av+1;

      if ( !( tbl_unify_trie_term(an, A2 PASS_LD) &&
	      tbl_put_trie_value(modeargs, an PASS_LD) &&
	      PL_recorded(sr, suspension) &&
	      PL_unify_output(A3, modeargs) &&
	      unify_dependency(A4, suspension PASS_LD)
         ) )
      { freeForeignState(state, sizeof(*state));
	return FALSE;			/* resource error */
      }

      DEBUG(MSG_TABLING_WORK,
	    { Sdprintf("Work: %d %d\n\t",
		       (int)state->acp_index, (int)state->scp_index);
	      PL_write_term(Serror, A2, 1200, PL_WRT_NEWLINE);
	      Sdprintf("\t");
	      PL_write_term(Serror, suspension, 1200, PL_WRT_NEWLINE);
	    });

      if ( --state->scp_index == 0 )
      { state->scp_index = state->scp_size;
	if ( --state->acp_index == 0 )
	  state->next_step = TRUE;
      }

      ForeignRedoPtr(state);
    }
  }

  state->list->executing = FALSE;
  freeForeignState(state, sizeof(*state));
  return FALSE;
}


/** '$tbl_variant_table'(+Variant, -Trie, -Status, -Skeleton) is det.
 *
 * Retrieve the table for Variant. Status is one of
 *
 *   - `fresh` if the table is new
 *   - `complete` if the table is completed
 *   - A worklist pointer
 */

static
PRED_IMPL("$tbl_variant_table", 4, tbl_variant_table, 0)
{ PRED_LD
  trie *trie;

  if ( (trie=get_variant_table(A1, TRUE PASS_LD)) )
  { return ( _PL_unify_atomic(A2, trie->symbol) &&
	     unify_table_status(A3, trie PASS_LD)  &&
	     unify_skeleton(trie, A1, A4 PASS_LD) );
  }

  return FALSE;
}


static
PRED_IMPL("$tbl_variant_table", 1, tbl_variant_table, 0)
{ PRED_LD
  trie *trie = LD->tabling.variant_table;

  if ( trie )
    return _PL_unify_atomic(A1, trie->symbol);

  return FALSE;
}


/** '$tbl_table_status'(+Trie, -Status, -Wrapper, -Skeleton)
 *
 * Get the status of Trie as well as its wrapper and Skeleton */

static
PRED_IMPL("$tbl_table_status", 4, tbl_table_status, 0)
{ PRED_LD
  trie *trie;

  return ( get_trie(A1, &trie) &&
	   unify_table_status(A2, trie PASS_LD) &&
	   (!trie->data.skeleton ||
	    unify_skeleton(trie, A3, A4 PASS_LD)) );
}

/** '$tbl_table_complete_all'(+SCC)
 *
 * Complete and reset all newly created tables.
 */

static
PRED_IMPL("$tbl_table_complete_all", 1, tbl_table_complete_all, 0)
{ PRED_LD
  tbl_component *c;

  if ( !get_scc(A1, &c) )
    return FALSE;

  if ( c->status == SCC_ACTIVE )
  { worklist **wls;
    size_t ntables = worklist_set_to_array(c->created_worklists, &wls);
    size_t i;

    for(i=0; i<ntables; i++)
    { worklist *wl = wls[i];
      trie *trie = wl->table;

      trie->data.worklist = WL_COMPLETE;
    }
    reset_newly_created_worklists(c);
    c->status = SCC_COMPLETED;

    if ( c->parent && LD->tabling.component == c )
      LD->tabling.component = c->parent;
    if ( !c->parent )
      LD->tabling.has_scheduling_component = FALSE;
// FIXME: Leave destruction to GC
  }

  return TRUE;
}


/** '$tbl_free_component'(+SCC)
 *
 * Destroy a component and all subcomponents
 */

static
PRED_IMPL("$tbl_free_component", 1, tbl_free_component, 0)
{ PRED_LD
  tbl_component *c;

  if ( get_scc(A1, &c) )
  { assert(!c->parent);
    if ( LD->tabling.component == c )
      free_component(c, FC_DESTROY);
    assert(LD->tabling.component == NULL);
    return TRUE;
  }

  return FALSE;
}


/** '$tbl_table_discard_all'(+SCC)
 *
 * Discard all newly created tables and the worklists. This is used if
 * an exception happens during tabling.
 */

static
PRED_IMPL("$tbl_table_discard_all", 1, tbl_table_discard_all, 0)
{ PRED_LD
  tbl_component *c;

  if ( get_scc(A1, &c) )
  { if ( c->created_worklists )
    { size_t i, ntables;
      worklist **wls;

      /* Remove the associated variant tables */
      ntables = worklist_set_to_array(c->created_worklists, &wls);
      for(i=0; i<ntables; i++)
      { worklist *wl = wls[i];
	trie *trie = wl->table;

	prune_node(LD->tabling.variant_table, trie->data.variant);
      }
      reset_newly_created_worklists(c);
    }
    reset_global_worklist(c);
    // FIXME: just pop?
    LD->tabling.has_scheduling_component = FALSE;
  }

  return TRUE;
}



static
PRED_IMPL("$tbl_create_component", 1, tbl_create_component, 0)
{ PRED_LD

  if ( !LD->tabling.has_scheduling_component )
  { LD->tabling.has_scheduling_component = TRUE;
    if ( !LD->tabling.component )
      LD->tabling.component = new_component();
    else
      LD->tabling.component->status = SCC_ACTIVE;
    return PL_unify_pointer(A1, LD->tabling.component);
  }

  return FALSE;
}


static
PRED_IMPL("$tbl_create_subcomponent", 1, tbl_create_subcomponent, 0)
{ PRED_LD
  tbl_component *c, *p;

					/* no component; create main */
  if ( !LD->tabling.has_scheduling_component )
  { LD->tabling.has_scheduling_component = TRUE;
    if ( !LD->tabling.component )
      LD->tabling.component = new_component();
    else
      LD->tabling.component->status = SCC_ACTIVE;
    return PL_unify_pointer(A1, LD->tabling.component);
  }

  c = new_component();
  c->parent = (p=LD->tabling.component);
  LD->tabling.component = c;
  add_child_component(p, c);

  return PL_unify_pointer(A1, c);
}

static int
unify_component_status(term_t t, tbl_component *scc ARG_LD)
{ atom_t status;

  switch(scc->status)
  { case SCC_ACTIVE:	status = ATOM_active; break;
    case SCC_MERGED:	status = ATOM_merged; break;
    case SCC_COMPLETED:
    { if ( scc->parent )
	status = ATOM_complete;
      else
	status = ATOM_final;
      break;
    }
    default:
      assert(0);
      return FALSE;
  }

  return PL_unify_atom(t, status);
}

static
PRED_IMPL("$tbl_component_status", 2, tbl_component_status, 0)
{ PRED_LD
  tbl_component *c;

  if ( get_scc(A1, &c) )
    return unify_component_status(A2, c PASS_LD);

  return FALSE;
}


/** '$tbl_abolish_all_tables' is det.
 *
 * Clear the thread table data.
 */

static
PRED_IMPL("$tbl_abolish_all_tables", 0, tbl_abolish_all_tables, 0)
{ PRED_LD

  if ( !LD->tabling.has_scheduling_component )
  { clearThreadTablingData(LD);
    return TRUE;
  } else
  { term_t ex = PL_new_term_ref();

    PL_put_atom(ex, ATOM_all);
    return PL_permission_error("abolish", "tables", ex);
  }
}

/** '$tbl_trienode'(-X) is det.
 *
 * X is the reserved node value for non-moded arguments.
 */

static
PRED_IMPL("$tbl_trienode", 1, tbl_trienode, 0)
{ PRED_LD

  return PL_unify_atom(A1, ATOM_trienode);
}

		 /*******************************
		 *     INSPECT TABLING DATA	*
		 *******************************/

static
PRED_IMPL("$tbl_scc", 1, tbl_scc, 0)
{ PRED_LD

  if ( LD->tabling.component )
    return PL_unify_pointer(A1, LD->tabling.component);

  return FALSE;
}



static int
unify_wl_set(term_t l, worklist_set *wls)
{ GET_LD
  worklist **p;
  size_t i, n = worklist_set_to_array(wls, &p);

  term_t tail = PL_copy_term_ref(l);
  term_t head = PL_new_term_ref();
  for(i=0; i<n; i++)
  { if ( !PL_unify_list(tail, head, tail) ||
	 !PL_unify_pointer(head, p[i]) )
      return FALSE;
  }

  return PL_unify_nil(tail);
}

static int
unify_scc_set(term_t l, component_set *cs)
{ GET_LD
  term_t tail = PL_copy_term_ref(l);

  if ( cs )
  { tbl_component **c = baseBuffer(&cs->members, tbl_component*);
    tbl_component **top = topBuffer(&cs->members, tbl_component*);
    term_t head = PL_new_term_ref();

    for(; c < top; c++)
    { if ( !PL_unify_list(tail, head, tail) ||
	   !PL_unify_pointer(head, *c) )
	return FALSE;
    }
  }

  return PL_unify_nil(tail);
}

static int
unify_pointer_or_nil(term_t t, void *ptr)
{ GET_LD

  if ( ptr )
    return PL_unify_pointer(t, ptr);
  else
    return PL_unify_atom_chars(t, "null");
}

static
PRED_IMPL("$tbl_scc_data", 2, tbl_scc_data, 0)
{ PRED_LD
  tbl_component *scc;

  if ( get_scc(A1, &scc) )
  { term_t av = PL_new_term_refs(5);
    term_t t = PL_new_term_ref();
    static functor_t f = 0;

    if ( !f ) f = PL_new_functor(PL_new_atom("scc"),5);

    return ( unify_pointer_or_nil(av+0, scc->parent) &&
	     unify_scc_set(av+1, scc->children) &&
	     unify_component_status(av+2, scc PASS_LD) &&
	     unify_wl_set(av+3, scc->worklist) &&
	     unify_wl_set(av+4, scc->created_worklists) &&
	     PL_cons_functor_v(t, f, av) &&
	     PL_unify(t, A2) );
  }

  return FALSE;
}


static int
unify_cluster(term_t t, cluster *c, int is_riac)
{ GET_LD

  if ( is_riac )
  { term_t a = PL_new_term_ref();
    if ( !PL_unify_term(t, PL_FUNCTOR_CHARS, "riac", 1,
			     PL_TERM, a) )
      return FALSE;
    t = a;
  }

  term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();

  if ( c->type == CLUSTER_ANSWERS )
  { trie_node **ap  = baseBuffer(&c->members, trie_node*);
    trie_node **top = topBuffer(&c->members, trie_node*);
    term_t answer = PL_new_term_ref();
    term_t modeav = PL_new_term_ref();

    for(; ap < top; ap++)
    { if ( !PL_put_variable(answer) ||
	   !PL_unify_list(tail, head, tail) ||
	   !tbl_unify_trie_term(*ap, answer PASS_LD) ||
	   !tbl_put_trie_value(modeav, *ap PASS_LD) ||
	   !PL_unify_term(head, PL_FUNCTOR_CHARS, "-", 2,
			          PL_TERM, answer, PL_TERM, modeav) )
	return FALSE;
    }
    return PL_unify_nil(tail);
  } else
  { record_t *sp  = baseBuffer(&c->members, record_t);
    record_t *top = topBuffer(&c->members, record_t);
    term_t tmp = PL_new_term_ref();

    assert(c->type == CLUSTER_SUSPENSIONS);

    for(; sp < top; sp++)
    { if ( !PL_unify_list(tail, head, tail) ||
	   !PL_recorded(*sp, tmp) ||
	   !PL_unify(tmp, head) )
	return FALSE;
    }
    return PL_unify_nil(tail);
  }
}


static int
unify_clusters(term_t t, worklist *wl)
{ GET_LD
  cluster *c;
  term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();

  for(c=wl->head; c; c=c->next)
  { if ( !PL_unify_list(tail, head, tail) ||
	 !unify_cluster(head, c, c==wl->riac) )
      return FALSE;
  }

  return PL_unify_nil(tail);
}


static
PRED_IMPL("$tbl_worklist_data", 2, tbl_worklist_data, 0)
{ PRED_LD
  worklist *wl;

  if ( get_worklist(A1, &wl) )
  { term_t av = PL_new_term_refs(5);
    term_t t = PL_new_term_ref();
    static functor_t f = 0;

    if ( !f ) f = PL_new_functor(PL_new_atom("worklist"),5);

    return ( PL_unify_pointer(av+0, wl->component) &&
	     _PL_unify_atomic(av+1, wl->table->symbol) &&
	     PL_unify_bool(av+2, wl->in_global_wl) &&
	     PL_unify_bool(av+3, wl->executing) &&
	     unify_clusters(av+4, wl) &&
	     PL_cons_functor_v(t, f, av) &&
	     PL_unify(t, A2)
	   );
  }

  return FALSE;
}



		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(tabling)
  PRED_DEF("$tbl_new_worklist",		2, tbl_new_worklist,	     0)
  PRED_DEF("$tbl_pop_worklist",		2, tbl_pop_worklist,	     0)
  PRED_DEF("$tbl_wkl_add_answer",	2, tbl_wkl_add_answer,	     0)
  PRED_DEF("$tbl_wkl_mode_add_answer",	4, tbl_wkl_mode_add_answer,  0)
  PRED_DEF("$tbl_wkl_add_suspension",	2, tbl_wkl_add_suspension,   0)
  PRED_DEF("$tbl_wkl_done",		1, tbl_wkl_done,	     0)
  PRED_DEF("$tbl_wkl_negative",		1, tbl_wkl_negative,	     0)
  PRED_DEF("$tbl_wkl_is_false",		1, tbl_wkl_is_false,	     0)
  PRED_DEF("$tbl_wkl_work",		7, tbl_wkl_work, PL_FA_NONDETERMINISTIC)
  PRED_DEF("$tbl_variant_table",	4, tbl_variant_table,	     0)
  PRED_DEF("$tbl_variant_table",        1, tbl_variant_table,        0)
  PRED_DEF("$tbl_table_status",		4, tbl_table_status,	     0)
  PRED_DEF("$tbl_table_complete_all",	1, tbl_table_complete_all,   0)
  PRED_DEF("$tbl_free_component",       1, tbl_free_component,       0)
  PRED_DEF("$tbl_table_discard_all",    1, tbl_table_discard_all,    0)
  PRED_DEF("$tbl_create_component",	1, tbl_create_component,     0)
  PRED_DEF("$tbl_create_subcomponent",  1, tbl_create_subcomponent,  0)
  PRED_DEF("$tbl_component_status",     2, tbl_component_status,     0)
  PRED_DEF("$tbl_abolish_all_tables",   0, tbl_abolish_all_tables,   0)
  PRED_DEF("$tbl_destroy_table",        1, tbl_destroy_table,        0)
  PRED_DEF("$tbl_trienode",             1, tbl_trienode,             0)

  PRED_DEF("$tbl_scc",                  1, tbl_scc,                  0)
  PRED_DEF("$tbl_scc_data",             2, tbl_scc_data,             0)
  PRED_DEF("$tbl_worklist_data",        2, tbl_worklist_data,        0)
EndPredDefs
