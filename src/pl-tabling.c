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
#include "pl-wrap.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We provide two answer completion strategies:

  - Eager AC: #define O_AC_EAGER 1
    Complete each component fully before continuing to the next.
  - Lazy AC:  #undef O_AC_EAGER
    Only complete the leader of a component.  This is what XSB is
    doing.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define O_AC_EAGER 1
// #undef O_AC_EAGER

#define record_t fastheap_term *
#define PL_record(t)      term_to_fastheap(t PASS_LD)
#define PL_recorded(r, t) put_fastheap(r, t PASS_LD)
#define PL_erase(r)	  free_fastheap(r)

static int	destroy_answer_trie(trie *atrie);
static void	free_worklist(worklist *wl);
static void	clean_worklist(worklist *wl);
static void	destroy_depending_worklists(worklist *wl0);
static void	free_worklist_set(worklist_set *wls, int freewl);
static void	add_global_worklist(worklist *wl);
static int	wl_has_work(const worklist *wl);
static cluster *new_answer_cluster(worklist *wl, trie_node *first);
static void	wkl_append_left(worklist *wl, cluster *c);
static int	wkl_add_answer(worklist *wl, trie_node *node ARG_LD);
static int	tbl_put_trie_value(term_t t, trie_node *node ARG_LD);
static void	del_child_component(tbl_component *parent, tbl_component *child);
static void	free_components_set(component_set *cs, int destroy);
static int	unify_skeleton(trie *trie, term_t wrapper, term_t skel ARG_LD);
#ifdef O_DEBUG
static void	print_worklist(const char *prefix, worklist *wl);
static void	print_delay(const char *msg,
			    trie_node *variant, trie_node *answer);
static void	print_answer(const char *msg, trie_node *answer);
static int	put_delay_info(term_t t, trie_node *answer);
#endif
static int	simplify_component(tbl_component *scc);
static void	idg_destroy(idg_node *node);
static int	idg_init_variant(trie *atrie, term_t variant ARG_LD);
static void	reeval_complete(trie *atrie);
static int	simplify_answer(worklist *wl, trie_node *answer, int truth);
static int	table_is_incomplete(trie *trie);

#define WL_IS_SPECIAL(wl)  (((intptr_t)(wl)) & 0x1)
#define WL_IS_WORKLIST(wl) ((wl) && !WL_IS_SPECIAL(wl))

#define WL_COMPLETE ((worklist *)0x11)
#define WL_GROUND   ((worklist *)0x21)
#define WL_DYNAMIC  ((worklist *)0x41)

#define WLFS_FREE_NONE		0x0000
#define WLFS_KEEP_COMPLETE	0x0001
#define WLFS_FREE_ALL		0x0002
#define WLFS_DISCARD_INCOMPLETE	0x0004

#define DV_DELETED		((trie*)0x1)
#define DL_UNDEFINED		((delay_info*)0x1)

#define DL_IS_DELAY_LIST(dl)	((dl) && (dl) != DL_UNDEFINED)


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
    free_worklist_set(c->worklist, WLFS_FREE_NONE);
  if ( c->delay_worklists )
    free_worklist_set(c->delay_worklists, WLFS_FREE_NONE);
  if ( c->created_worklists )
    free_worklist_set(c->created_worklists, WLFS_FREE_ALL);
  if ( c->children )
    free_components_set(c->children, flags|FC_CHILD);
  if ( c->merged )
    free_components_set(c->merged, flags|FC_CHILD);

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

static void merge_children(tbl_component *c, tbl_component *m);
static void merge_one_component(tbl_component *c, tbl_component *m);
static void wls_set_component(worklist_set *wls, size_t size0, tbl_component *c);

static size_t
wls_size(const worklist_set *wls)
{ return wls ? entriesBuffer(&wls->members, worklist*) : 0;
}


static void
merge_component(tbl_component *c)
{ size_t s_global  = wls_size(c->worklist);
  size_t s_created = wls_size(c->created_worklists);

  if ( c->children )
    merge_children(c, c);

  wls_set_component(c->worklist, s_global, c);
  wls_set_component(c->created_worklists, s_created, c);

  DEBUG(MSG_TABLING_MERGE,
	Sdprintf("Grown SCC %p from %zd to %zd worklists\n",
		 c, s_created, wls_size(c->created_worklists)));
}


static void
wl_set_component(worklist *wl, tbl_component *c)
{ wl->component = c;
  wl->executing = FALSE;
  if ( !wl->in_global_wl && wl_has_work(wl) )
    add_global_worklist(wl);
  if ( wl->negative )
  { DEBUG(MSG_TABLING_MERGE,
	  Sdprintf("Merging negative literal into SCC %zd\n",
		   pointerToInt(c)));
    if ( c->neg_status == SCC_NEG_NONE )
      c->neg_status = SCC_NEG_DELAY;
    else if ( c->neg_status == SCC_NEG_SIMPLIFY )
      c->neg_status = SCC_NEG_DELAY;
  }
}


static void
wls_set_component(worklist_set *wls, size_t size0, tbl_component *c)
{ worklist **base = baseBuffer(&wls->members, worklist*);
  worklist **top  = topBuffer(&wls->members, worklist*);

#ifdef O_DEBUG
  size_t old = 0;
  for(; base < top; base++, old++)
  { if ( old < size0 )
      assert((*base)->component == c);
    else
      wl_set_component(*base, c);
  }
#else
  base += size0;			/* skip old ones */
  for(; base < top; base++)
    wl_set_component(*base, c);
#endif
}

static void
merge_component_sets(component_set **into, component_set **from)
{ typedef tbl_component* Component;

  if ( *into && *from )
  { tbl_component **s = baseBuffer(&(*from)->members, tbl_component*);
    size_t        cnt = entriesBuffer(&(*from)->members, tbl_component*);
    Buffer	    b = &(*into)->members;

    addMultipleBuffer(b, s, cnt, Component);
    free_components_set(*from, 0);
    *from = NULL;
  } else if ( *from )
  { *into = *from;
    *from = NULL;
  }
}


/* Merge all components of cs into c */

static void
merge_children(tbl_component *c, tbl_component *m)
{ component_set *cs;

  if ( (cs=m->children) )
  { tbl_component **bp = baseBuffer(&cs->members, tbl_component*);
    tbl_component **tp = topBuffer(&cs->members, tbl_component*);

    for( ; bp < tp; bp++)
      merge_one_component(c, *bp);

    merge_component_sets(&m->merged, &m->children);
  }
}


static void
merge_worklists(worklist_set **into, worklist_set **from)
{ typedef worklist* Worklist;

  if ( *into && *from )
  { worklist **s = baseBuffer(&(*from)->members, worklist*);
    size_t   cnt = entriesBuffer(&(*from)->members, worklist*);
    Buffer     b = &(*into)->members;

    addMultipleBuffer(b, s, cnt, Worklist);
    free_worklist_set(*from, WLFS_FREE_NONE);
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

  merge_children(c, m);

  DEBUG(MSG_TABLING_MERGE,
	Sdprintf("Merged %p into %p, %zd worklists, %zd created\n",
		 m, c,
		 entriesBuffer(&m->worklist->members, worklist*),
		 entriesBuffer(&m->created_worklists->members, worklist*)));

  merge_worklists(&c->worklist, &m->worklist);
  merge_worklists(&c->created_worklists, &m->created_worklists);
  merge_worklists(&c->delay_worklists, &m->delay_worklists);

  m->status = SCC_MERGED;
}

		 /*******************************
		 *           WORKLISTS		*
		 *******************************/

static worklist_set *
new_worklist_set(worklist *wl)
{ worklist_set *wls = PL_malloc(sizeof(*wls));

  initBuffer(&wls->members);
  addBuffer(&wls->members, wl, worklist*);

  return wls;
}


static void
add_global_worklist(worklist *wl)
{ tbl_component *c = wl->component;
  worklist_set *wls;

  if ( !(wls=c->worklist) )
    c->worklist = new_worklist_set(wl);
  else
    addBuffer(&wls->members, wl, worklist*);

  wl->in_global_wl = TRUE;
}


static void
add_delay_worklist(worklist *wl)
{ tbl_component *c = wl->component;
  worklist_set *wls;

  if ( !(wls=c->delay_worklists) )
    c->delay_worklists = new_worklist_set(wl);
  else
    addBuffer(&wls->members, wl, worklist*);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Normal completion is done. There  may   be  worklists that are suspended
using negation_suspend/3. We wake  these  up   by  adding  a  new answer
cluster with a NULL node.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static worklist *
negative_worklist(tbl_component *scc ARG_LD)
{ if ( scc->delay_worklists )
  { while( !isEmptyBuffer(&scc->delay_worklists->members) )
    { worklist *wl = popBuffer(&scc->delay_worklists->members, worklist*);

      if ( !wl->has_answers )	/* we have an unconditional answers, so no delay */
      { cluster *c;

	wl->neg_delayed = TRUE;
	DEBUG(MSG_TABLING_NEG,
	      { term_t t = PL_new_term_ref();
		unify_trie_term(wl->table->data.variant, t PASS_LD);
		Sdprintf("Resuming negative node with delay list %zd: ",
			 pointerToInt(wl));
		PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
	      });

	c = new_answer_cluster(wl, NULL);
	wkl_append_left(wl, c);
	if ( !wl->riac )
	  wl->riac = c;

	return wl;
      }
    }

    scc->neg_status = SCC_NEG_SIMPLIFY;
  }

#ifdef O_DEBUG
  if ( !DEBUGGING(TABLING_NO_SIMPLIFY) )
  { simplify_component(scc);
  } else
  { Sdprintf("Skipping (TABLING_NO_SIMPLIFY) simplifiation for SCC %zd\n",
	     pointerToInt(scc));
  }
#else
  simplify_component(scc);
#endif

  return NULL;
}


static int
wl_has_work(const worklist *wl)
{ return wl->riac && wl->riac->next;
}

static worklist *
pop_worklist(tbl_component *c ARG_LD)
{ worklist_set *wls = c->worklist;

  if ( wls )
  { while( !isEmptyBuffer(&wls->members) )
    { worklist *wl = popBuffer(&wls->members, worklist*);
      wl->in_global_wl = FALSE;

      if ( wl_has_work(wl) )
	return wl;
    }
  }

  return NULL;
}


static void
reset_global_worklist(tbl_component *c)
{ worklist_set *wls;

  if ( c && (wls = c->worklist) )
  { c->worklist = NULL;
    free_worklist_set(wls, WLFS_FREE_NONE);
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
reset_newly_created_worklists(tbl_component *c, int flags)
{ worklist_set *wls;

  if ( c && (wls = c->created_worklists) )
  { c->created_worklists = NULL;
    free_worklist_set(wls, flags);
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
      trie *atrie = wl->table;

      if ( (freewl&WLFS_FREE_ALL) ||
	   wl->table->data.worklist == WL_COMPLETE )
	free_worklist(wl);
      if ( (freewl&WLFS_DISCARD_INCOMPLETE) && table_is_incomplete(atrie) )
      { DEBUG(MSG_TABLING_EXCEPTION,
	      { GET_LD
		term_t tab = PL_new_term_ref();
		unify_trie_term(atrie->data.variant, tab PASS_LD);
		Sdprintf("Deleting incomplete answer table ");
		PL_write_term(Serror, tab, 999, PL_WRT_NEWLINE);
	      });
	destroy_answer_trie(atrie);
      }
    }
  }

  discardBuffer(&wls->members);
  PL_free(wls);
}


		 /*******************************
		 *	 TABLE DELAY LISTS	*
		 *******************************/

#ifdef O_DEBUG
static void print_dl_dependency(trie *from, trie *to);
#endif

static inline trie_node *
REC_DELAY(record_t r)
{ return (trie_node*)(((uintptr_t)r)|0x1);
}

static inline record_t
UNREC_DELAY(trie_node *r)
{ return (record_t)(((uintptr_t)r)&~(uintptr_t)1);
}

static int
IS_REC_DELAY(trie_node *r)
{ return (uintptr_t)r & 0x1;
}

int
answer_is_conditional(trie_node *answer)
{ delay_info *di;

  return ( (di=answer->data.delayinfo) &&
	   (di == DL_UNDEFINED || !isEmptyBuffer(&di->delay_sets)) );
}

static delay_info *
answer_delay_info(worklist *wl, trie_node *answer, int create)
{ delay_info *di;

  if ( (di=answer->data.delayinfo) )
  { return di;
  } else if ( !create )
  { return NULL;
  } else if ( (di=malloc(sizeof(*di))) )
  { di->variant = wl->table->data.variant;
    di->has_share_records = FALSE;
    initBuffer(&di->delay_sets);
    initBuffer(&di->delays);
    answer->data.delayinfo = di;
    wl->undefined++;

    return di;
  } else
  { return NULL;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destroy_delay_info(trie_node  *answer,  int  propagate)    removes   and
deallocates the delay info  that  may   be  associated  to  `answer`. If
`propagate` is TRUE, it also removes   the backpointers to `answer` from
the worklist `delay` buffer of worklists   that  are references from the
delay elements.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
delete_answer(Buffer ab,  trie_node *answer)
{ trie_node **ap = baseBuffer(ab, trie_node*);
  trie_node **ep = topBuffer(ab, trie_node*);
  trie_node **op = ap;

  for(; ap < ep; ap++)
  { if ( *ap != answer )
      *op++ = *ap;
  }

  ab->top = (char*)op;
}

static void
destroy_delay_info(trie *atrie, trie_node *answer, int propagate)
{ delay_info *di = answer->data.delayinfo;

  if ( DL_IS_DELAY_LIST(di) )
  { answer->data.delayinfo = NULL;
    if ( di->has_share_records )
    { delay *d = baseBuffer(&di->delays, delay);
      delay *z = topBuffer(&di->delays, delay);

      for(; d < z; d++)		/* keep a flag to see whether we have these */
      { if ( IS_REC_DELAY(d->answer) )
	  PL_erase(UNREC_DELAY(d->answer));
      }
    }

    if ( propagate )
    { delay *db = baseBuffer(&di->delays, delay);
      delay *dt = topBuffer(&di->delays, delay);
      delay *d;

      for(d=db; d < dt; d++)
      { trie *at;

	if ( (at=d->variant) && at != DV_DELETED )
	{ worklist *wl = at->data.worklist;

	  if ( WL_IS_WORKLIST(wl) && !isEmptyBuffer(&wl->delays) )
	  { DEBUG(MSG_TABLING_VTRIE_DEPENDENCIES,
		  { GET_LD
		    term_t tab = PL_new_term_ref();
		    term_t dep = PL_new_term_ref();
		    unify_trie_term(atrie->data.variant, tab PASS_LD);
		    unify_trie_term(wl->table->data.variant, dep PASS_LD);
		    Sdprintf("  Deleting answer from table ");
		    PL_write_term(Serror, tab, 999, 0);
		    Sdprintf(" <-- ");
		    PL_write_term(Serror, dep, 999, PL_WRT_NEWLINE);
		  });

	    delete_answer(&wl->delays, answer);
	  }
	}
      }
    }

    discardBuffer(&di->delay_sets);
    discardBuffer(&di->delays);
    free(di);
  }
}


static void
answer_set_general_undefined(trie *atrie, trie_node *answer)
{ destroy_delay_info(atrie, answer, TRUE);
  answer->data.delayinfo = DL_UNDEFINED;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Delete the undefined answers that depend on this worklist
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
delete_depending_answers(worklist *wl, TmpBuffer wlset)
{ DEBUG(MSG_TABLING_VTRIE_DEPENDENCIES,
	{ GET_LD
	  term_t t = PL_new_term_ref();
	  unify_trie_term(wl->table->data.variant, t PASS_LD);
	  Sdprintf("delete_depending_answers for ");
	  PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
	});

  while( !isEmptyBuffer(&wl->delays) )
  { trie_node **top = topBuffer(&wl->delays, trie_node*);
    trie_node *answer = top[-1];
    delay_info *di;

    assert(wl->depend_abolish);

    if ( DL_IS_DELAY_LIST(di=answer->data.delayinfo) )
    { trie *at = symbol_trie(di->variant->value);
      worklist *dwl;

      if ( WL_IS_WORKLIST((dwl=at->data.worklist)) &&
	   !dwl->depend_abolish )
      { assert(dwl != wl);
	assert(dwl->table != wl->table);
	DEBUG(MSG_TABLING_VTRIE_DEPENDENCIES,
	      print_dl_dependency(wl->table, dwl->table));
	dwl->depend_abolish = TRUE;
	addBuffer(wlset, dwl, worklist *);
      }
      answer_set_general_undefined(at, answer);
    } else
    { (void)popBufferP(&wl->delays, trie_node *);
    }
  }

  discardBuffer(&wl->delays);
  initBuffer(&wl->delays);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destroy_depending_worklists(worklist *wl) destroys worklists that have
answers pointing to this worklist and its answers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
destroy_depending_worklists(worklist *wl0)
{ tmp_buffer wlset;

  initBuffer(&wlset);
  wl0->depend_abolish = TRUE;
  delete_depending_answers(wl0, &wlset);
  while( !isEmptyBuffer(&wlset) )
  { worklist *wl = popBuffer(&wlset, worklist *);

    delete_depending_answers(wl, &wlset);
    destroy_answer_trie(wl->table);
  }
  discardBuffer(&wlset);
}


static void *
destroy_delay_info_answer(trie_node *answer, void *ctx)
{ trie *atrie = ctx;

  if ( DL_IS_DELAY_LIST(answer->data.delayinfo) )
  { answer_set_general_undefined(atrie, answer);
  }

  return NULL;
}


static void
destroy_delay_info_worklist(worklist *wl)
{ map_trie_node(&wl->table->root, destroy_delay_info_answer, wl->table);
}


static delay_set *
create_delay_set(delay_info *di)
{ delay_set *ds;

  if ( di &&
       (ds=allocFromBuffer(&di->delay_sets, sizeof(*ds))) )
  { ds->offset = entriesBuffer(&di->delays, delay);
    ds->size   = 0;
    ds->active = 0;

    return ds;
  }

  return NULL;
}

static int
add_to_delay_set(delay_info *di, delay_set *ds,
		 trie *variant, trie_node *answer)
{ delay *d;

  if ( (d=allocFromBuffer(&di->delays, sizeof(*d))) )
  { d->variant = variant;
    d->answer  = answer;
    if ( variant )
      ds->active++;
    return ++ds->size;
  } else
  { return 0;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Register a conditional answer with  the   worklist  associated  with the
variant that contributes to the condition. The argument is a node in the
variant table. If this node is not associated   with  a worklist it is a
completed node and we are just propagating an undefined literal.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
add_to_wl_delays(trie *at, trie_node *answer, worklist *wla)
{ worklist *wl = at->data.worklist;

  if ( WL_IS_WORKLIST(wl) )
  { DEBUG(MSG_TABLING_SIMPLIFY,
	  { GET_LD
	    term_t t = PL_new_term_ref();
	    term_t v = PL_new_term_ref();
	    term_t vt = PL_new_term_ref();
	    unify_trie_term(at->data.variant, t PASS_LD);
	    unify_trie_term(answer, v PASS_LD);
	    unify_trie_term(wla->table->data.variant, vt PASS_LD);
	    Sdprintf("Adding propagation to worklist for ");
	    PL_write_term(Serror, t, 999, 0);
	    Sdprintf(" to answer ");
	    PL_write_term(Serror, v, 999, 0);
	    Sdprintf(" of table ");
	    PL_write_term(Serror, vt, 999, PL_WRT_NEWLINE);
	  });
    addBuffer(&wl->delays, answer, trie_node *);
  } else
  { /* see '$tbl_table_complete_all'/1 */
    DEBUG(MSG_TABLING_VTRIE_DEPENDENCIES,
	  print_dl_dependency(wla->table, at));
    assert(0);
  }

  return TRUE;
}


static void
add_to_wl_pos_undefined(worklist *wl, trie_node *answer)
{ addBuffer(&wl->pos_undefined, answer, trie_node *);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Simplify a delay set after adding ds. This  pops the new delay set if it
is a duplicate.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
equal_delay(const delay *a, const delay *b)
{ return ( a->variant == b->variant &&
	   a->answer  == b->answer );
}

static int
equal_delay_set(const delay *delays, const delay_set *a, const delay_set *b)
{ if ( a->size == b->size )
  { unsigned int ia = a->offset;
    unsigned int ib = b->offset;
    unsigned int ea = a->offset + a->size;

    for( ; ia < ea; ia++, ib++)
    { if ( !equal_delay(&delays[ia], &delays[ib]) )
	return FALSE;
    }

    return TRUE;
  }

  return FALSE;
}


static int
simplify_delay_set(delay_info *di, delay_set *ds)
{ delay     *delays = baseBuffer(&di->delays, delay);
  delay_set *base   = baseBuffer(&di->delay_sets, delay_set);

  for(; base < ds; base++)
  { if ( equal_delay_set(delays, base, ds) )
    { seekBuffer(&di->delays, ds->offset, delay);
      popBufferP(&di->delay_sets, delay_set);
      return TRUE;
    }
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The delay list (`delays`) is a  list   of  delayed positive and negative
literals:

  - Negative literal ---> answer-trie ptr
  - Positive literal ---> answer-trie ptr + answer-node ptr

TBD: If we make an answer unconditional,  should we propagate this? Note
that the worklists still  point  to   this  answer.  That should trigger
propagation?

FIXME: delete variable sharing record
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum
{ UDL_FALSE = 0,
  UDL_TRUE,
  UDL_COMPLETE
} udl_status;

static int
update_delay_list(worklist *wl, trie_node *answer,
		  term_t skel, term_t delays ARG_LD)
{ Word ldlp;
  Word gdlp;

retry:
  deRef2(valTermRef(LD->tabling.delay_list), gdlp);
  gdlp = argTermP(*gdlp, 0);
  deRef(gdlp);
  deRef2(valTermRef(delays), ldlp);

  if ( isNil(*ldlp) && isNil(*gdlp) )
  { delay_info *di;

    if ( (di=answer->data.delayinfo) )
    { destroy_delay_info(wl->table, answer, TRUE);
      answer->data.delayinfo = NULL;
      wl->undefined--;
      DEBUG(MSG_TABLING_SIMPLIFY,
	    Sdprintf("Unconditional answer after conditional\n"));
      simplify_answer(wl, answer, TRUE);
    }
					/* Incremental tabling */
    if ( wl->table->data.IDG && wl->table->data.IDG->reevaluating )
    { if ( false(answer, TN_IDG_UNCONDITIONAL) )
      { set(answer, TN_IDG_UNCONDITIONAL);
	simplify_answer(wl, answer, TRUE);
      }
    }

    DEBUG(TABLING_NO_EARLY_COMPLETION,
	  return UDL_TRUE);

    if ( wl->ground )				/* early completion */
      return UDL_COMPLETE;
    return UDL_TRUE;
  } else
  { delay_info *di = answer_delay_info(wl, answer, TRUE);
    delay_set  *ds = create_delay_set(di);
    size_t count;
    Word tail;

    count = skip_list(ldlp, &tail PASS_LD);
    if ( !isNil(*tail) )
      return PL_type_error("delay_list", delays);
    count += skip_list(gdlp, &tail PASS_LD);
    if ( !isNil(*tail) )
      return PL_type_error("delay_list", LD->tabling.delay_list);

    if ( !hasGlobalSpace(count+2) )
    { int rc;

      if ( (rc = ensureGlobalSpace(count+2, ALLOW_GC)) != TRUE )
	return raiseStackOverflow(rc);
      goto retry;
    }

    if ( ds )
    { int pass = 0;
      Word dlp;
      Word   tshare = NULL;
      size_t nshare = 0;

      for(pass = 0; pass <= 1; pass++)
      {	dlp = pass ? ldlp : gdlp;

	for(; !isNil(*dlp); dlp = TailList(dlp))
	{ Word h;
	  trie *at;
	  trie_node *an;

	  deRef(dlp);
	  if ( !isList(*dlp) )
	  { PL_type_error("list", delays);
	    return UDL_FALSE;
	  }

	  h = HeadList(dlp);
	  deRef(h);
	  if ( isAtom(*h) )		/* Answer trie symbol */
	  { if ( (at=symbol_trie(*h)) )
	    { an = NULL;
	    } else			/* deleted trie or 'undefined' */
	    { undef:
	      destroy_delay_info(wl->table, answer, TRUE);
	      answer->data.delayinfo = DL_UNDEFINED;
	      return UDL_TRUE;
	    }
	  } else if ( isTerm(*h) )
	  { Functor f = valueTerm(*h);
	    Word p;

	    if ( f->definition == FUNCTOR_plus2 )
	    { deRef2(&f->arguments[0], p);
	      assert(isAtom(*p));
	      if ( !(at=symbol_trie(*p)) )
	      { goto undef;
	      }
	      deRef2(&f->arguments[1], p);
	      if ( isInteger(*p) )
	      { assert(isTaggedInt(*p));
		an = intToPointer(valInt(*p));
		assert(is_ground_trie_node(an));
	      } else
	      { int rc;

		/* ground__LD() returns first var */
		if ( ground__LD(p PASS_LD) != NULL )
		{ if ( !tshare )
		  { tshare = allocGlobalNoShift(3);
		    assert(tshare);
		    tshare[1] = linkVal(valTermRef(skel));
		    tshare[2] = *p;
		    nshare = 1;
		  } else
		  { Word s = allocGlobalNoShift(1);
		    assert(s);
		    s[0] = *p;
		    nshare++;
		  }
		}

		if ( (rc=trie_lookup(at, &an, p, TRUE, NULL PASS_LD)) == TRUE )
		{ // TBD: can we immediately simplify if this already has a value?
		  DEBUG(MSG_TABLING_DELAY_VAR,
			print_delay("Waiting for instantiated",
				    at->data.variant, an));
		  // TBD: at->data.worklist?
		  add_to_wl_pos_undefined(wl, an);
		} else
		{ term_t trie = PL_new_term_ref();

		  PL_put_atom(trie, wl->table->symbol);
		  return trie_error(rc, trie);
		}
	      }
	    } else
	    { PL_type_error("delay_list", delays);
	      return UDL_FALSE;
	    }
	  } else
	  { PL_type_error("delay_list", delays);
	    return UDL_FALSE;
	  }

	  assert(at->magic == TRIE_MAGIC);

	  if ( !add_to_delay_set(di, ds, at, an) )
	    goto nomem;
	} /*for list*/
      } /*for pass*/

      if ( tshare )
      { word w = consPtr(tshare, TAG_COMPOUND|STG_GLOBAL);
	record_t r;

	tshare[0] = PL_new_functor(ATOM_v, nshare+1);
	r = PL_record(pushWordAsTermRef(&w));
	popTermRef();
	if ( r )
	{ if ( !add_to_delay_set(di, ds, NULL, REC_DELAY(r)) )
	    goto nomem;
	  di->has_share_records = TRUE;
	} else
	{ return UDL_FALSE;
	}
      }

      if ( tshare || !simplify_delay_set(di, ds) )
      { delay *d = baseBuffer(&di->delays, delay);
	unsigned int i, e = ds->offset+ds->size;

	for(i=ds->offset; i<e; i++)
	{ if ( d[i].variant )
	  { if ( !add_to_wl_delays(d[i].variant, answer, wl) )
	      return UDL_FALSE;
	  }
	}
      }

      return UDL_TRUE;
    }

  nomem:
    PL_resource_error("memory");
    return UDL_FALSE;
  }
}


static void
delay_sets(delay_info *di, delay_set **base, delay_set **top)
{ *base = baseBuffer(&di->delay_sets, delay_set);
  *top  = topBuffer(&di->delay_sets, delay_set);
}

static void
get_delay_set(delay_info *di, delay_set *set, delay **base, delay **top)
{ *base = baseBuffer(&di->delays, delay) + set->offset;
  *top  = (*base) + set->size;
}


term_t
init_delay_list(void)
{ GET_LD
  term_t t = PL_new_term_ref();
  Word p = allocGlobal(2);

  p[0] = FUNCTOR_minus1;
  p[1] = ATOM_nil;

  *valTermRef(t) = consPtr(p, TAG_COMPOUND|STG_GLOBAL);

  return t;
}

static
PRED_IMPL("$tbl_delay_list", 1, tbl_delay_list, 0)
{ PRED_LD;
  term_t dl = LD->tabling.delay_list;
  term_t a = PL_new_term_ref();

  return ( _PL_get_arg(1, dl, a) &&
	   PL_unify(A1, a) );
}

static
PRED_IMPL("$tbl_set_delay_list", 1, tbl_set_delay_list, 0)
{ PRED_LD;
  term_t dl = LD->tabling.delay_list;
  Word p;

  if ( !hasGlobalSpace(0) )
  { int rc;

    if ( (rc=ensureGlobalSpace(0, ALLOW_GC)) != TRUE )
      return raiseStackOverflow(rc);
  }

  p = valTermRef(dl);
  if ( isTerm(*p) )
  { p = argTermP(*p, 0);

    TrailAssignment(p);
    unify_vp(p, valTermRef(A1) PASS_LD);
  }

  return TRUE;
}

static void
push_delay_list(Word p ARG_LD)
{ Word dl = valTermRef(LD->tabling.delay_list);

  assert(isTerm(*dl));
  dl = argTermP(*dl, 0);
  p[2] = *dl;
  TrailAssignment(dl);
  *dl = consPtr(p, TAG_COMPOUND|STG_GLOBAL);
}

/* Push a positive delay node.  If the answer is ground this is a
 * term atrie+answer, else it is a term atrie+wrapper.
 */

void
tbl_push_delay(atom_t atrie, Word wrapper, trie_node *answer ARG_LD)
{ Word p;

  if ( (p = allocGlobalNoShift(6)) )
  { p[0] = FUNCTOR_dot2;
    p[1] = consPtr(&p[3], TAG_COMPOUND|STG_GLOBAL);
    p[3] = FUNCTOR_plus2;
    p[4] = atrie;
    if ( unlikely(answer == NULL) )
    { p[5] = consInt(0);
    } else if ( is_ground_trie_node(answer) )
    { p[5] = consInt(pointerToInt(answer));
    } else
    { p[5] = linkVal(wrapper);
    }

    push_delay_list(p PASS_LD);
  } else
  { assert(0);
  }
}


/** '$tbl_add_global_delays'(+Delays0, -Delays) is det.
 *
 *  Delays is the result of appending the  global delay list to Delays0.
 *  This is a highly time  critical   operation  and might eventually be
 *  merged into '$tbl_wkl_add_answer'/4 and '$tbl_wkl_add_suspension'/2.
 */

static
PRED_IMPL("$tbl_add_global_delays", 2, tbl_add_global_delays, 0)
{ PRED_LD
  term_t dl = PL_new_term_ref();

  _PL_get_arg(1, LD->tabling.delay_list, dl);

  if ( PL_get_nil(dl) )
  { return PL_unify(A1, A2);
  } else if ( PL_get_nil(A1) )
  { return PL_unify(A2, dl);
  } else
  { intptr_t len;
    Word tailp;
    Word dlp, p;
    word l;

    len = skip_list(valTermRef(dl), &tailp PASS_LD);
    assert(isNil(*tailp));

    if ( !(p=allocGlobal(3*len)) )
      return FALSE;
    l = consPtr(p, TAG_COMPOUND|STG_GLOBAL);

    dlp = valTermRef(dl);
    deRef(dlp);

    for(;;)
    { *p++ = FUNCTOR_dot2;
      *p++ = linkVal(HeadList(dlp));
      dlp = TailList(dlp);
      deRef(dlp);
      if ( isNil(*dlp) )
      { *p = linkVal(valTermRef(A1));
	return _PL_unify_atomic(A2, l);
      }
      *p   = consPtr(&p[1], TAG_COMPOUND|STG_GLOBAL);
      p++;
    }
  }
}

		 /*******************************
		 *	  SIMPLIFICATION	*
		 *******************************/

static int	answer_completion(tbl_component *scc);

typedef struct propagate
{ worklist  *worklist;
  trie_node *answer;
  int	     result;
} propagate;

typedef struct agenda
{ size_t	done;
  tmp_buffer	buffer;
} spf_agenda;

static void
init_spf_agenda(spf_agenda *a)
{ a->done = 0;
  initBuffer(&a->buffer);
}

static void
exit_spf_agenda(spf_agenda *a)
{ discardBuffer(&a->buffer);
}

static int
push_propagate(spf_agenda *a, worklist *wl, trie_node *answer, int result)
{ propagate *p = allocFromBuffer(&a->buffer, sizeof(*p));

  p->worklist = wl;
  p->answer   = answer;
  p->result   = result;

  return TRUE;
}

propagate *
pop_propagate(spf_agenda *a)
{ if ( isEmptyBuffer(&a->buffer) )
    return NULL;

  return popBufferP(&a->buffer, propagate);
}


static int propagate_result(spf_agenda *agenda,
			    worklist *wl, trie_node *answer, int result);
#ifdef O_DEBUG
static void print_delay(const char *msg, trie_node *variant, trie_node *answer);
#endif

static int
make_answer_unconditional(spf_agenda *agenda, trie_node *answer)
{ delay_info *di = answer->data.delayinfo;

  if ( DL_IS_DELAY_LIST(di) )
  { trie *at = symbol_trie(di->variant->value);
    worklist *wl = at->data.worklist;
    assert(wl->magic == WORKLIST_MAGIC);

    DEBUG(MSG_TABLING_SIMPLIFY,
	  print_delay("   Making answer unconditional", di->variant, answer));

    destroy_delay_info(at, answer, TRUE);
    agenda->done++;
    wl->undefined--;

    if ( !isEmptyBuffer(&wl->delays) )
      push_propagate(agenda, wl, answer, TRUE);
  } else
  { assert(0);
  }

  return TRUE;
}


static int
remove_conditional_answer(spf_agenda *agenda, trie_node *answer)
{ delay_info *di = answer->data.delayinfo;

  if ( DL_IS_DELAY_LIST(di) )
  { trie *at = symbol_trie(di->variant->value);
    worklist *wl = at->data.worklist;

    assert(wl->magic == WORKLIST_MAGIC);

    DEBUG(MSG_TABLING_SIMPLIFY,
	  print_delay("    Removing conditional answer", di->variant, answer));

    destroy_delay_info(at, answer, TRUE);
    trie_delete(at, answer, TRUE);		/* cannot prune as may be */
    agenda->done++;				/* in worklist delay lists */
    wl->undefined--;

    if ( !isEmptyBuffer(&wl->delays) )
      push_propagate(agenda, wl, answer, FALSE);
  }

  return TRUE;
}



static void
answer_delay_sets(delay_info *di, delay_set **base, delay_set **top)
{ *base = baseBuffer(&di->delay_sets, delay_set);
  *top  = topBuffer(&di->delay_sets, delay_set);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Propagate a result in worklist wl. For  negative nodes `answer` is NULL.
The `result` indicates whether the  answer   is  satisfied (TRUE) or not
(FALSE). If an answer is satisfied it is removed from the delay list and
if  the  resulting  delay  list  becomes    empty  the  answer  is  made
unconditional. Otherwise the delay list can   no longer become satisfied
and we remove the delay list. If this was the last delay list the answer
is definitely invalid and can be removed from the answer trie.

Answer to propagate is <wl,panswer> with truth result.
This answer is propagate to `answer`
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
propagate_to_answer(spf_agenda *agenda, worklist *wl,
		    trie_node *panswer, int result, trie_node *answer)
{ delay_info *di;
  trie *variant = wl->table;
  int found = FALSE;

  DEBUG(MSG_TABLING_SIMPLIFY, print_answer("  to", answer));

  if ( (di=answer_delay_info(NULL, answer, FALSE)) )
  { delay_set *ds, *dz;
    delay *db = baseBuffer(&di->delays, delay);

    for(answer_delay_sets(di, &ds, &dz); ds < dz; ds++)
    { unsigned o;
      unsigned oe = ds->offset+ds->size;

      for(o=ds->offset; o<oe; o++)
      { delay *d = &db[o];

	if ( d->variant == variant )
	{ if ( d->answer == panswer || d->answer == NULL )
	  { int res;

	    DEBUG(MSG_TABLING_SIMPLIFY,
		  Sdprintf("   found (SCC=%zd, simplifications = %zd)\n",
			   pointerToInt(wl->component),
			   wl->component->simplifications));

	    if ( d->answer == NULL )
	    { if ( result == FALSE &&
		   (wl->has_answers || wl->undefined) )
		continue;
	      res = !result;
	    } else
	    { res = result;
	    }

	    found = TRUE;
	    wl->component->simplifications++;

	    if ( res )			/* remove member from conjunction */
	    { d->variant = DV_DELETED;
	      if ( --ds->active == 0 )
	      { make_answer_unconditional(agenda, answer);
		return found;
	      }
	    } else			/* remove the conjunction */
	    { memmove(ds, ds+1, sizeof(*ds)*(dz-ds-1));
	      (void)popBufferP(&di->delay_sets, delay_set);
	      ds--;			/* compensate for(;;ds++) */
	      dz--;
	      break;
	    }
	  }
	}
      }

      if ( isEmptyBuffer(&di->delay_sets) )
      { remove_conditional_answer(agenda, answer);
	return found;
      }
    }
  }

  if ( found )
    DEBUG(MSG_TABLING_SIMPLIFY, print_answer("  now", answer));

  return found;
}


static int
propagate_result(spf_agenda *agenda,
		 worklist *wl, trie_node *panswer, int result)
{ DEBUG(MSG_TABLING_SIMPLIFY,
	{ print_delay(result ? "Propagating TRUE" : "Propagating FALSE",
		      wl->table->data.variant, panswer);
	  Sdprintf("  %zd dependent answers\n",
		   entriesBuffer(&wl->delays, trie_node*));
	});

  while( !isEmptyBuffer(&wl->delays) )
  { trie_node *answer = popBuffer(&wl->delays, trie_node*);

    propagate_to_answer(agenda, wl, panswer, result, answer);
  }

  return TRUE;
}


static int
simplify_answer(worklist *wl, trie_node *answer, int truth)
{ spf_agenda agenda;
  propagate *p;

  init_spf_agenda(&agenda);
  push_propagate(&agenda, wl, answer, truth);
  while( (p=pop_propagate(&agenda)) )
    propagate_result(&agenda, p->worklist, p->answer, p->result);
  exit_spf_agenda(&agenda);

  return TRUE;
}


static int
simplify_component(tbl_component *scc)
{ spf_agenda agenda;
  propagate *p;
  worklist **wlp0 = baseBuffer(&scc->created_worklists->members, worklist*);
  worklist **top  = topBuffer(&scc->created_worklists->members, worklist*);
  worklist **wlp;
  int undefined, pass;
#ifndef O_AC_EAGER
  size_t simplified0 = scc->simplifications;
#endif

  DEBUG(MSG_TABLING_SIMPLIFY,
	{ GET_LD
	  term_t t = PL_new_term_ref();
	  unify_trie_term(scc->leader->data.variant, t PASS_LD);
	  Sdprintf("Simplifying SCC %zd; leader = ", pointerToInt(scc));
	  PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
	});

  init_spf_agenda(&agenda);

  for(pass=0; ;pass++)
  { int count = 0;

    undefined = 0;

    for(wlp = wlp0; wlp < top; wlp++)
    { worklist *wl = *wlp;

      if ( pass == 0 )
	clean_worklist(wl);

      if ( wl->negative &&
	   wl->neg_delayed &&
	   wl->table->value_count == 0 &&
	   !isEmptyBuffer(&wl->delays) )
      { DEBUG(MSG_TABLING_SIMPLIFY,
	      { GET_LD
		term_t t = PL_new_term_ref();
		unify_trie_term(wl->table->data.variant, t PASS_LD);
		Sdprintf("No conditional answers for %zd: ", pointerToInt(wl));
		PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
	      });

	count++;
	push_propagate(&agenda, wl, NULL, FALSE);
	while( (p=pop_propagate(&agenda)) )
	  propagate_result(&agenda, p->worklist, p->answer, p->result);
      }

      if ( !isEmptyBuffer(&wl->pos_undefined) )
      { trie_node **bn = baseBuffer(&wl->pos_undefined, trie_node *);
	trie_node **en = topBuffer(&wl->pos_undefined, trie_node *);
	trie_node **on = bn;

	for(; bn < en; bn++)
	{ trie_node *an = *bn;

	  if ( !answer_is_conditional(an) )
	  { DEBUG(MSG_TABLING_SIMPLIFY,
		  Sdprintf("Propagating instantiated answer\n"));
	    count++;
	    push_propagate(&agenda, wl, an, an->value != 0);
	    while( (p=pop_propagate(&agenda)) )
	      propagate_result(&agenda, p->worklist, p->answer, p->result);
	  } else
	  { *on++ = an;
	  }
	}
	wl->pos_undefined.top = (char*)on;
      }

      if ( wl->undefined )
	undefined++;
    }

    if ( count == 0 || undefined == 0 )
      break;
  }

  exit_spf_agenda(&agenda);

#ifndef O_AC_EAGER
  if ( (simplified0 != scc->simplifications) )
  { size_t cnt = scc->simplifications - simplified0 ;
    tbl_component *c;

    for(c = scc->parent; c; c = c->parent)
      c->simplifications += cnt;
  }
#endif

  /* DSW: there cannot be any "uncovering" of a positive loop if there
   * was no simplification
   */

  DEBUG(MSG_TABLING_SIMPLIFY,
	Sdprintf("Simplified SCC %zd; undefined = %d; simplifications: %zd\n",
		 pointerToInt(scc), undefined, scc->simplifications));

  if ( undefined && scc->simplifications )
    return answer_completion(scc);
  else
    return TRUE;
}

#ifdef O_DEBUG
static void
print_dl_dependency(trie *from, trie *to)
{ GET_LD
  term_t From = PL_new_term_ref();
  term_t To   = PL_new_term_ref();

  unify_trie_term(from->data.variant, From PASS_LD);
  unify_trie_term(to->data.variant, To PASS_LD);
  Sdprintf("Delay list dep from %p (", from);
  PL_write_term(Serror, From, 999, 0);
  Sdprintf(") -> %p (", to);
  PL_write_term(Serror, To, 999, 0);
  Sdprintf(")\n");
}
#endif

		 /*******************************
		 *	ANSWER COMPLETION	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The role of answer completion  is  to   remove  positive  loops from the
remaining delay lists. We do   this by calling answer_completion(+ATrie)
in  boot/tabling.pl.  This  predicate  uses  recursive  tabling  on  the
residual program that involves the given  ATrie and removing all answers
deduced as false and marking those deduced as true as `answer_completed`

We search for a candidate worklist as one that is undefined (e.g., has a
residual program) and has at  least  one   depending  answer  that has a
positive delay element because a positive loop needs to include at least
one positive dependency.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
call_answer_completion(trie *atrie ARG_LD)
{ fid_t fid;

  if ( (fid = PL_open_foreign_frame()) )
  { static predicate_t pred = NULL;
    term_t av = PL_new_term_refs(2);
    int rc;
    tbl_component *scc_old = LD->tabling.component;
    int hsc = LD->tabling.has_scheduling_component;

    if ( !pred )
      pred = PL_predicate("answer_completion", 2, "$tabling");

    DEBUG(MSG_TABLING_AC,
	  { term_t t = PL_new_term_ref();
	    unify_trie_term(atrie->data.variant, t PASS_LD);
	    Sdprintf("Calling answer completion for: ");
	    PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
	  });

    LD->tabling.component = NULL;
    LD->tabling.has_scheduling_component = FALSE;
    LD->tabling.in_answer_completion = TRUE;
    rc = ( PL_put_atom(av+0, atrie->symbol) &&
	   unify_skeleton(atrie, 0, av+1 PASS_LD) &&
	   PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, pred, av) );
    LD->tabling.in_answer_completion = FALSE;
    LD->tabling.has_scheduling_component = hsc;
    LD->tabling.component = scc_old;

    PL_close_foreign_frame(fid);
    return rc;
  } else
    return FALSE;				/* stack overflow */
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A variant can only be subject to answer   completion  if it has at least
one answer that  is  undefined  and   has  a  condition  containing only
positive delay elements.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_AC_EAGER
static int
has_positive_dl(trie_node *n)
{ delay_info *di;

  if ( n->value && DL_IS_DELAY_LIST(di=n->data.delayinfo) )
  { delay_set *ds, *dz;
    delay *db = baseBuffer(&di->delays, delay);

    for(answer_delay_sets(di, &ds, &dz); ds < dz; ds++)
    { unsigned o;
      unsigned oe = ds->offset+ds->size;

      for(o=ds->offset; o<oe; o++)
      { delay *d = &db[o];

	if ( d->variant && d->variant != DV_DELETED )
	{ if ( d->answer )
	    return TRUE;
	}
      }
    }
  }

  return FALSE;
}

static int
is_ac_candidate_wl(worklist *wl)
{ if ( wl->undefined && !wl->answer_completed &&
       !isEmptyBuffer(&wl->delays) )
  { trie_node **n = baseBuffer(&wl->delays, trie_node*);
    trie_node **z =  topBuffer(&wl->delays, trie_node*);

    for( ; n < z; n++)
    { if ( has_positive_dl(*n) )
	return TRUE;
    }
  }

  return FALSE;
}
#endif /*O_AC_EAGER*/


static int
answer_completion(tbl_component *scc)
{ GET_LD

#ifdef O_DEBUG
  if ( DEBUGGING(TABLING_NO_AC) )
    return TRUE;
#endif

  if ( LD->tabling.in_answer_completion )
    return TRUE;			/* not recursive! */

#ifdef O_AC_EAGER
  worklist **wlp = baseBuffer(&scc->created_worklists->members, worklist*);
  worklist **top = topBuffer(&scc->created_worklists->members, worklist*);

  for(; wlp < top; wlp++)
  { worklist *wl = *wlp;

    if ( is_ac_candidate_wl(wl) )
    { if ( !call_answer_completion(wl->table PASS_LD) )
	return FALSE;
    }
  }

  return TRUE;
#else
  return call_answer_completion(scc->leader PASS_LD);
#endif /*O_AC_EAGER*/
}

/** '$tbl_force_truth_value'(+AnswerNode, +Value, -Count)
 *
 * Force AnswerNode to have truth value Value.  Count returns the
 * number of answer nodes that have been changed, which may be more
 * than one due to propagation.
 */

static
PRED_IMPL("$tbl_force_truth_value", 3, tbl_force_truth_value, 0)
{ PRED_LD
  void *ptr;
  int truth;
  int rc = FALSE;

  if ( PL_get_pointer_ex(A1, &ptr) &&
       PL_get_bool_ex(A2, &truth) )
  { trie_node *answer = ptr;
    delay_info *di = answer->data.delayinfo;
    spf_agenda agenda;
    propagate *p;

    init_spf_agenda(&agenda);

    if ( DL_IS_DELAY_LIST(di) )
    { trie *at = symbol_trie(di->variant->value);
      worklist *wl = at->data.worklist;

      DEBUG(MSG_TABLING_AC,
	    { term_t v = PL_new_term_ref();
	      term_t a = PL_new_term_ref();

	      unify_trie_term(at->data.variant, v PASS_LD);
	      unify_trie_term(answer, a PASS_LD);
	      Sdprintf("Forcing answer ");
	      PL_write_term(Serror, a, 999, 0);
	      Sdprintf(" for ");
	      PL_write_term(Serror, v, 999, 0);
	      Sdprintf(" to FALSE\n");
	    });

      if ( WL_IS_WORKLIST(wl) )
      { if ( truth )
	  rc = make_answer_unconditional(&agenda, answer);
	else
	  rc = remove_conditional_answer(&agenda, answer);
      } else
      { rc = PL_permission_error("force_truth_value", "answer", A1);
      }
    } else					/* answer is not conditional */
    { if ( !truth )
      { trie *at = get_trie_from_node(answer);

	trie_delete(at, answer, FALSE);		/* TBD: propagate? */
      }
      rc = TRUE;
    }

    while( rc && (p=pop_propagate(&agenda)) )
      rc = propagate_result(&agenda, p->worklist, p->answer, p->result);

    rc = rc && PL_unify_integer(A3, agenda.done);

    exit_spf_agenda(&agenda);
  }

  return rc;
}


static
PRED_IMPL("$tbl_set_answer_completed", 1, tbl_set_answer_completed, 0)
{ trie *trie;

  if ( get_trie(A1, &trie) )
  { worklist *wl;

    if ( WL_IS_WORKLIST((wl=trie->data.worklist)) )
    { wl->answer_completed = TRUE;

      return TRUE;
    }

    if ( wl == WL_COMPLETE )
      return TRUE;

    return PL_permission_error("set_answer_complete", "trie", A1);
  }

  return FALSE;
}

static
PRED_IMPL("$tbl_is_answer_completed", 1, tbl_is_answer_completed, 0)
{ trie *trie;

  if ( get_trie(A1, &trie) )
  { worklist *wl;

    if ( WL_IS_WORKLIST((wl=trie->data.worklist)) )
      return wl->answer_completed;

    return wl == WL_COMPLETE;
  }

  return FALSE;
}


#ifdef O_DEBUG
static void
print_delay(const char *msg, trie_node *variant, trie_node *answer)
{ GET_LD
  term_t t = PL_new_term_ref();

  unify_trie_term(variant, t PASS_LD);
  Sdprintf("%s: %s", msg, answer ? "" : "~");
  PL_write_term(Serror, t, 999, answer ? 0 : PL_WRT_NEWLINE);
  if ( answer )
  { PL_put_variable(t);
    unify_trie_term(answer, t PASS_LD);
    Sdprintf(", answer: ");
    PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
  }
}

static void
print_answer(const char *msg, trie_node *answer)
{ GET_LD
  trie *at = get_trie_from_node(answer);
  term_t t = PL_new_term_ref();

  unify_trie_term(at->data.variant, t PASS_LD);
  Sdprintf("%s: variant ", msg);
  PL_write_term(Serror, t, 999, 0);
  PL_put_variable(t);
  unify_trie_term(answer, t PASS_LD);
  Sdprintf(", answer: ");
  PL_write_term(Serror, t, 999, 0);
  if ( !answer->value )
    Sdprintf(" (NULL)");
  if ( answer_is_conditional(answer) )
  { put_delay_info(t, answer);
    Sdprintf(" (IF ");
    PL_write_term(Serror, t, 999, 0);
    Sdprintf(")\n");
  } else
    Sdprintf("\n");
}

#endif


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
    worklist *wl;

    if ( WL_IS_WORKLIST(wl=vtrie->data.worklist) )
    { if ( !isEmptyBuffer(&wl->delays) &&
	   variant_table->magic == TRIE_MAGIC )	/* not in final destruction */
	destroy_depending_worklists(wl);
      if ( wl->undefined )
	destroy_delay_info_worklist(wl);
      vtrie->data.worklist = NULL;
      free_worklist(wl);
    }

    assert(vtrie->data.variant == node);
    trie_empty(vtrie);
    vtrie->data.variant = NULL;
    if ( vtrie->data.IDG )
    { idg_destroy(vtrie->data.IDG);
      vtrie->data.IDG = NULL;
    }
  }
}


static int
is_variant_trie(trie *trie)
{ return trie->release_node == release_variant_table_node;
}


static void
clear_variant_table(PL_local_data_t *ld)
{ if ( ld->tabling.variant_table )
  { trie_empty(ld->tabling.variant_table);
    PL_unregister_atom(ld->tabling.variant_table->symbol);
    ld->tabling.variant_table = NULL;
  }
}


#define VAR_SKEL_FAST 8

static int
unify_trie_ret(term_t ret, TmpBuffer vars ARG_LD)
{ Word *pp = baseBuffer(vars, Word);
  Word *ep = topBuffer(vars, Word);
  static functor_t fast[VAR_SKEL_FAST] = {0};
  size_t arity = ep-pp;
  functor_t vf;

  assert(arity > 0);
  if ( arity < VAR_SKEL_FAST )
  { if ( !(vf=fast[arity]) )
      fast[arity] = vf = PL_new_functor(ATOM_ret, arity);
  } else
  { vf = PL_new_functor(ATOM_ret, arity);
  }

  if ( hasGlobalSpace(arity+1) )
  { Word p = allocGlobalNoShift(arity+1);
    word w = consPtr(p, TAG_COMPOUND|STG_GLOBAL);
    *p++ = vf;

    for(; pp < ep; pp++)
    { Word ap = *pp;
      *p++ = makeRefG(ap);
    }

    return _PL_unify_atomic(ret, w);
  }

  return GLOBAL_OVERFLOW;
}


static void
release_answer_node(trie *atrie, trie_node *node)
{ if ( DL_IS_DELAY_LIST(node->data.delayinfo) )
    destroy_delay_info(atrie, node, TRUE);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_answer_table(+Variant, -Return, int flags)

Find the answer table for  Variant  and   its  return  template  (a term
ret/N). If `create` is TRUE, create the table if it does not exist.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define AT_CREATE		0x0001
#define AT_MODED		0x0002

static trie *
get_answer_table(term_t t, term_t ret, int flags ARG_LD)
{ trie *variants = thread_variant_table(PASS_LD1);
  trie *atrie;
  trie_node *node;
  int rc;
  Word v;
  tmp_buffer vars;
  mark m;

  initBuffer(&vars);

retry:
  Mark(m);
  v = valTermRef(t);
  rc = trie_lookup(variants, &node, v, (flags&AT_CREATE), &vars PASS_LD);

  if ( rc == TRUE )
  { if ( node->value )
    { atrie = symbol_trie(node->value);
    } else if ( (flags&AT_CREATE) )
    { atrie = trie_create();
      set(atrie, (flags&AT_MODED) ? TRIE_ISMAP : TRIE_ISSET);
      atrie->release_node = release_answer_node;
      node->value = trie_symbol(atrie);
      atrie->data.variant = node;
      atrie->alloc_pool = &LD->tabling.node_pool;
      ATOMIC_INC(&variants->value_count);
    } else
    { discardBuffer(&vars);
      return NULL;
    }

    if ( ret )
    { if ( isEmptyBuffer(&vars) )		/* TBD: only needed first time */
      { if ( WL_IS_WORKLIST(atrie->data.worklist) )
	{ atrie->data.worklist->ground = TRUE;
	} else if ( !atrie->data.worklist )
	{ atrie->data.worklist = WL_GROUND;
	}
	if ( !PL_unify_atom(ret, ATOM_ret) )
	  atrie = NULL;
      } else
      { int rc;

	if ( (rc=unify_trie_ret(ret, &vars PASS_LD)) != TRUE )
	{ if ( rc < 0 )
	  { Undo(m);
	    emptyBuffer(&vars);
	    if ( makeMoreStackSpace(rc, ALLOW_GC) )
	      goto retry;
	  }
	  atrie = NULL;
	}
      }
    }
    discardBuffer(&vars);
    return atrie;
  } else
  { discardBuffer(&vars);
  }

  trie_error(rc, t);
  return NULL;
}


void
clearThreadTablingData(PL_local_data_t *ld)
{ reset_global_worklist(ld->tabling.component);
  reset_newly_created_worklists(ld->tabling.component, WLFS_KEEP_COMPLETE);
  clear_variant_table(ld);
}



		 /*******************************
		 *  ANSWER/SUSPENSION CLUSTERS	*
		 *******************************/

static cluster *
new_answer_cluster(worklist *wl, trie_node *first)
{ cluster *c;

  if ( (c=wl->free_clusters) )
  { wl->free_clusters = c->next;
    c->type = CLUSTER_ANSWERS;
  } else
  { c = PL_malloc(sizeof(*c));
    c->type = CLUSTER_ANSWERS;
    initBuffer(&c->members);
  }
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

static void
merge_answer_clusters(cluster *to, cluster *from)
{ typedef trie_node* TrieNode;

  addMultipleBuffer(&to->members,
		    baseBuffer(&from->members, trie_node*),
		    entriesBuffer(&from->members, trie_node*),
		    TrieNode);
}

static trie_node*
get_answer_from_cluster(cluster *c, size_t index)
{ if ( index < entriesBuffer(&c->members, trie_node*) )
    return fetchBuffer(&c->members, index, trie_node*);
  return NULL;
}

static inline record_t
TNOT(record_t r, int is_tnot)
{ return (record_t)(((uintptr_t)r)|is_tnot);
}

static inline record_t
UNTNOT(record_t r)
{ return (record_t)(((uintptr_t)r)&~(uintptr_t)1);
}

static int
IS_TNOT(record_t r)
{ return (uintptr_t)r & 0x1;
}

static cluster *
new_suspension_cluster(worklist *wl, term_t first, int is_tnot ARG_LD)
{ cluster *c;
  record_t r;

  if ( !(r=PL_record(first)) )
    return NULL;

  if ( (c=wl->free_clusters) )
  { wl->free_clusters = c->next;
    c->type = CLUSTER_SUSPENSIONS;
  } else
  { c = PL_malloc(sizeof(*c));
    c->type = CLUSTER_SUSPENSIONS;
    initBuffer(&c->members);
  }
  addBuffer(&c->members, TNOT(r, is_tnot), record_t);

  return c;
}

static void
free_suspension_cluster(cluster *c)
{ record_t *base = baseBuffer(&c->members, record_t);
  size_t entries = entriesBuffer(&c->members, record_t);
  size_t i;

  for(i=0; i<entries; i++)
    PL_erase(UNTNOT(base[i]));

  discardBuffer(&c->members);
  PL_free(c);
}

static int
add_to_suspension_cluster(cluster *c, term_t suspension, int is_tnot ARG_LD)
{ record_t r;

  if ( (r=PL_record(suspension)) )
  { addBuffer(&c->members, TNOT(r, is_tnot), record_t);
    return TRUE;
  }

  return FALSE;
}

static void
merge_suspension_cluster(cluster *to, cluster *from, int do_free)
{ typedef record_t* Record;

  addMultipleBuffer(&to->members,
		    baseBuffer(&from->members, record_t*),
		    entriesBuffer(&from->members, record_t*),
		    Record);
  if ( do_free )
  { discardBuffer(&from->members);
    PL_free(from);
  }
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
  if ( trie->data.worklist == WL_GROUND )
    wl->ground = TRUE;
  trie->data.worklist = wl;
  initBuffer(&wl->delays);
  initBuffer(&wl->pos_undefined);

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
  for(c=wl->free_clusters; c; c = next)
  { next = c->next;
    free_cluster(c);
  }
  discardBuffer(&wl->delays);
  discardBuffer(&wl->pos_undefined);

  if ( wl->abolish_on_complete )		/* abolished while incomplete */
    destroy_answer_trie(wl->table);

  PL_free(wl);
}


static void
clean_worklist(worklist *wl)
{ cluster *c, *next;

  wl->riac = NULL;
  if ( wl->head )
  { for(c=wl->head; c; c = next)
    { next = c->next;
      free_cluster(c);
    }
    wl->head = wl->tail = NULL;
  }

  if ( wl->free_clusters )
  { for(c=wl->free_clusters; c; c = next)
    { next = c->next;
      free_cluster(c);
    }
    wl->free_clusters = NULL;
  }
}


static void
complete_worklist(worklist *wl)
{ clean_worklist(wl);

  wl->completed = TRUE;
}


static int
worklist_negative(worklist *wl)
{ if ( !wl->negative )
  { wl->negative = TRUE;
    add_delay_worklist(wl);
    if ( wl->component->neg_status == SCC_NEG_NONE )
      wl->component->neg_status = SCC_NEG_DELAY;
  }

  return TRUE;
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

  if ( !answer_is_conditional(node) )
    wl->has_answers = TRUE;

  if ( wl->head && wl->head->type == CLUSTER_ANSWERS )
  { add_to_answer_cluster(wl->head, node);
  } else
  { cluster *c = new_answer_cluster(wl, node);
    wkl_append_left(wl, c);
    if ( !wl->riac )
      wl->riac = c;
  }
  DEBUG(MSG_TABLING_WORK,
	{ print_worklist("Added answer: ", wl);
	});

  return TRUE;
}


static int
wkl_add_suspension(worklist *wl, term_t suspension, int is_tnot ARG_LD)
{ potentially_add_to_global_worklist(wl PASS_LD);
  if ( wl->tail && wl->tail->type == CLUSTER_SUSPENSIONS )
  { if ( !add_to_suspension_cluster(wl->tail, suspension, is_tnot PASS_LD) )
      return FALSE;
  } else
  { cluster *c = new_suspension_cluster(wl, suspension, is_tnot PASS_LD);
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

static int
unify_complete_or_invalid(term_t t, trie *atrie ARG_LD)
{ idg_node *n;

  if ( (n=atrie->data.IDG) )
  { if ( n->falsecount > 0 )
      return PL_unify_atom(t, ATOM_invalid);
    if ( n->reevaluating )
      return PL_unify_atom(t, ATOM_fresh);
  }

  return PL_unify_atom(t, ATOM_complete);
}


static int
unify_table_status(term_t t, trie *trie, int merge ARG_LD)
{ worklist *wl = trie->data.worklist;

  if ( WL_IS_WORKLIST(wl) )
  { if ( wl->completed )
      return unify_complete_or_invalid(t, trie PASS_LD);

    if ( merge && wl->component != LD->tabling.component )
    { DEBUG(MSG_TABLING_WORK,
	    Sdprintf("Merging into %p (current = %p)\n",
		     wl->component, LD->tabling.component));
      merge_component(wl->component);
      LD->tabling.component = wl->component;
    }

    return PL_unify_pointer(t, wl);
  }
  if ( wl == WL_COMPLETE )
    return unify_complete_or_invalid(t, trie PASS_LD);
  if ( wl == WL_DYNAMIC )
    return PL_unify_atom(t, ATOM_dynamic);

  assert(!wl || wl == WL_GROUND);
  return PL_unify_atom(t, ATOM_fresh);
}


static int
table_is_incomplete(trie *trie)
{ worklist *wl = trie->data.worklist;

  if ( WL_IS_WORKLIST(wl) && !wl->completed )
    return TRUE;

  return FALSE;
}


static int
unify_skeleton(trie *atrie, term_t wrapper, term_t skeleton ARG_LD)
{ if ( !wrapper )
    wrapper = PL_new_term_ref();

  if ( unify_trie_term(atrie->data.variant, wrapper PASS_LD) )
  { get_answer_table(wrapper, skeleton, 0 PASS_LD);

    return TRUE;
  }

  return FALSE;
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
get_worklist(term_t t, worklist **wlp ARG_LD)
{ void *ptr;

  if ( PL_get_pointer(t, &ptr) )
  { worklist *wl = ptr;
    assert(wl->magic == WORKLIST_MAGIC);
    *wlp = wl;
    return TRUE;
  }

  PL_type_error("worklist", t);
  return FALSE;
}


static int
tnot_get_worklist(term_t t, worklist **wlp, int *is_tnot)
{ GET_LD
  void *ptr;

  if ( PL_get_pointer(t, &ptr) )
  { worklist *wl = ptr;
    assert(wl->magic == WORKLIST_MAGIC);
    *wlp = wl;
    *is_tnot = FALSE;
    return TRUE;
  }

  if ( PL_is_functor(t, FUNCTOR_tnot1) )
  { term_t a = PL_new_term_ref();
    _PL_get_arg(1, t, a);
    if ( PL_get_pointer(a, &ptr) )
    { worklist *wl = ptr;
      assert(wl->magic == WORKLIST_MAGIC);
      *wlp = wl;
      *is_tnot = TRUE;
      return TRUE;
    }
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
  { worklist *wl;

    if ( WL_IS_WORKLIST(wl=trie->data.worklist) )
      wl->completed = FALSE;
    else
      wl = new_worklist(trie);

    wl->component = LD->tabling.component;
    add_global_worklist(wl);
    add_newly_created_worklist(wl);
    return PL_unify_pointer(A1, wl);
  }

  return FALSE;
}


static int
destroy_answer_trie(trie *atrie)
{ if ( atrie->data.variant)
  { trie *vtrie = get_trie_from_node(atrie->data.variant);

    if ( is_variant_trie(vtrie) )
    { DEBUG(MSG_TABLING_VTRIE_DEPENDENCIES,
	    { GET_LD
	      term_t t = PL_new_term_ref();
	      unify_trie_term(atrie->data.variant, t PASS_LD);
	      Sdprintf("Deleting answer trie for ");
	      PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
	    });

      trie_delete(vtrie, atrie->data.variant, TRUE);
      return TRUE;
    }
  }

  return FALSE;
}


/** '$tbl_destroy_table'(+Trie)
 *
 * Destroy a single trie table.
 */

static
PRED_IMPL("$tbl_destroy_table", 1, tbl_destroy_table, 0)
{ trie *table;

  if ( get_trie(A1, &table) )
  { worklist *wl;

    if ( WL_IS_WORKLIST(wl=table->data.worklist) &&
	 !wl->completed )
    { wl->abolish_on_complete = TRUE;
    } else if ( destroy_answer_trie(table) )
    { return TRUE;
    } else
    { return PL_type_error("table", A1);
    }
  }

  return FALSE;
}


/** '$tbl_pop_worklist'(+SCC, -Worklist, -Delays) is semidet.
 *
 * Pop next worklist from the component.
 */

static
PRED_IMPL("$tbl_pop_worklist", 2, tbl_pop_worklist, 0)
{ PRED_LD
  tbl_component *scc;

  if ( get_scc(A1, &scc) )
  { worklist *wl;

    if ( (wl=pop_worklist(scc PASS_LD)) )
      return PL_unify_pointer(A2, wl);

    if (
#ifndef O_AC_EAGER
	  scc->simplifications ||
#endif
	  scc->neg_status != SCC_NEG_NONE )
    { if ( (wl=negative_worklist(scc PASS_LD)) )
	return PL_unify_pointer(A2, wl);
    }
  }

  return FALSE;
}

/** '$tbl_wkl_add_answer'(+Worklist, +Term, +Delays, -Complete) is semidet.
 *
 * Add an answer to the worklist's trie  and the worklist answer cluster
 * using trie_insert_new/3. Fails if a  variant   of  Term is already in
 * Worklist.
 */

static
PRED_IMPL("$tbl_wkl_add_answer", 4, tbl_wkl_add_answer, 0)
{ PRED_LD
  worklist *wl;

  if ( get_worklist(A1, &wl PASS_LD) )
  { Word kp;
    trie_node *node;
    int rc;

    kp = valTermRef(A2);

    if ( (rc=trie_lookup(wl->table, &node, kp, TRUE, NULL PASS_LD)) == TRUE )
    { idg_node *idg;

      if ( node->value )
      { if ( node->value == ATOM_trienode )
	{ if ( true(node, TN_IDG_DELETED) )
	  { clear(node, TN_IDG_DELETED);
	    goto update_dl;
	  } else
	  { if ( answer_is_conditional(node) )
	    { if ( update_delay_list(wl, node, A2, A3 PASS_LD) == UDL_COMPLETE )
		return PL_unify_atom(A4, ATOM_cut);
	    }
	  }

	  return FALSE;				/* already in trie */
	}
	return PL_permission_error("modify", "trie_key", A2);
      } else
      { set_trie_value_word(wl->table, node, ATOM_trienode);
	if ( (idg=wl->table->data.IDG) )
	  idg->new_answer = TRUE;

      update_dl:
	rc = update_delay_list(wl, node, A2, A3 PASS_LD);

	switch(rc)
	{ case UDL_FALSE:
	    return FALSE;
	  case UDL_COMPLETE:
	    if ( !PL_unify_atom(A4, ATOM_cut) )
	      return FALSE;
	  default:
	    ;
	}

	return wkl_add_answer(wl, node PASS_LD);
      }
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

  if ( get_worklist(A1, &wl PASS_LD) )
  { Word kp;
    trie_node *node;
    int rc;

    kp = valTermRef(A2);
    DEBUG(MSG_TABLING_MODED,
	  { PL_write_term(Serror, A2, 1200, 0);
	    Sdprintf(": ");
	  });

    if ( (rc=trie_lookup(wl->table, &node, kp, TRUE, NULL PASS_LD)) == TRUE )
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
	       set_trie_value(wl->table, node, av+3 PASS_LD)) )
	{ DEBUG(MSG_TABLING_MODED, Sdprintf("No change!\n"));
	  return FALSE;
	}

	DEBUG(MSG_TABLING_MODED,
	      { Sdprintf("Updated answer to: ");
		PL_write_term(Serror, av+3, 1200, PL_WRT_NEWLINE);
	      });
	return wkl_add_answer(wl, node PASS_LD);
      } else
      { if ( !set_trie_value(wl->table, node, A3 PASS_LD) )
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
  int is_tnot;

  if ( tnot_get_worklist(A1, &wl, &is_tnot) )
    return wkl_add_suspension(wl, A2, is_tnot PASS_LD);

  return FALSE;
}

/** '$tbl_wkl_make_follower'(+Worklist) is det.
 *
 * Turn a worklist that used  to  be   a  leader  into  a follower after
 * merging it with a parent SCC. This   implies  we must move the answer
 * clusters in front of the dependency clusters.
 */

static
PRED_IMPL("$tbl_wkl_make_follower", 1, tbl_wkl_make_follower, 0)
{ PRED_LD
  worklist *wl;

  if ( get_worklist(A1, &wl PASS_LD) )
  { cluster *scp = NULL;
    cluster *acp = NULL;
    cluster *c, *next;

    for(c=wl->head; c; c=next)
    { next = c->next;

      if ( c->type == CLUSTER_ANSWERS )
      { if ( acp )
	{ merge_answer_clusters(acp, c);
	  free_answer_cluster(c);
	} else
	{ acp = c;
	  acp->prev = acp->next = NULL;
	}
      } else
      { if ( scp )
	{ merge_suspension_cluster(scp, c, TRUE);
	} else
	{ scp = c;
	  scp->prev = scp->next = NULL;
	}
      }
    }

    wl->head = wl->tail = NULL;
    if ( acp )
    { wkl_append_left(wl, acp);
      wl->riac = acp;
    } else
    { wl->riac = NULL;
    }
    if ( scp )
      wkl_append_right(wl, scp);

    if ( acp && scp )
      add_global_worklist(wl);

    return TRUE;
  }

  return FALSE;
}


/** '$tbl_wkl_done'(+Worklist) is semidet.
 *
 * True if the worklist is complete
 */

static
PRED_IMPL("$tbl_wkl_done", 1, tbl_wkl_done, 0)
{ PRED_LD
  worklist *wl;

  return get_worklist(A1, &wl PASS_LD) && worklist_work_done(wl);
}

/** '$tbl_wkl_negative'(+Worklist) is semidet.
 *
 * True if the worklist is complete
 */

static
PRED_IMPL("$tbl_wkl_negative", 1, tbl_wkl_negative, 0)
{ PRED_LD
  worklist *wl;

  return get_worklist(A1, &wl PASS_LD) && worklist_negative(wl);
}


/** '$tbl_tbl_wkl_is_false'(+Worklist) is semidet.
 *
 * True if the worklist is is  a  negative   node  that  is true (has no
 * definite solutions). This is used at   the end of negation_suspend/4,
 * after we delayed the negation. This  means   we  must fail if we have
 * definite answers, but succeed if  there   are  conditional answers as
 * simplification has not yet been done.
 */

static
PRED_IMPL("$tbl_wkl_is_false", 1, tbl_wkl_is_false, 0)
{ PRED_LD
  worklist *wl;

  if ( get_worklist(A1, &wl PASS_LD) )
  { assert(wl->negative);

    return wl->neg_delayed && !wl->has_answers;
  }

  return FALSE;
}

/** '$tbl_wkl_answer_trie'(+Worklist, -Trie) is det.
 *
 * True when Trie is the answer trie associated with Worklist
 */

static
PRED_IMPL("$tbl_wkl_answer_trie", 2, tbl_wkl_answer_trie, 0)
{ GET_LD
  worklist *wl;

  return ( get_worklist(A1, &wl PASS_LD) &&
	   PL_unify_atom(A2, wl->table->symbol) );
}


/** '$tbl_wkl_work'(+Worklist, -Answer, -ModeArgs,
 *		    -Goal, -Continuation, -Wrapper, -TargetTable,
 *		    -Delays) is nondet.
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
unify_dependency(term_t a0, term_t dependency,
		 worklist *wl, trie_node *answer ARG_LD)
{ if ( likely(ensureStackSpace__LD(6, 5, ALLOW_GC PASS_LD)) )
  { Word dp = valTermRef(dependency);
    Functor f;

    deRef(dp);
    if ( unlikely(!isTerm(*dp)) )
      return FALSE;
    f = valueTerm(*dp);

    unify_arg_term(a0+0, &f->arguments[0] PASS_LD);
    unify_arg_term(a0+1, &f->arguments[1] PASS_LD);
    unify_arg_term(a0+2, &f->arguments[2] PASS_LD);
    unify_arg_term(a0+3, &f->arguments[3] PASS_LD);
    unify_arg_term(a0+4, &f->arguments[4] PASS_LD);

    if ( !answer )				/* negative delay */
    { Word p = allocGlobalNoShift(3);

      assert(p);
      p[0] = FUNCTOR_dot2;
      p[1] = wl->table->symbol;

      push_delay_list(p PASS_LD);
    } else if ( unlikely(answer_is_conditional(answer)) )
    { Word p = allocGlobalNoShift(6);
      assert(p);

      p[0] = FUNCTOR_dot2;
      p[1] = consPtr(&p[3], TAG_COMPOUND|STG_GLOBAL);
      p[3] = FUNCTOR_plus2;
      p[4] = wl->table->symbol;
      if ( is_ground_trie_node(answer) )
      { p[5] = consInt(pointerToInt(answer));
      } else
      { p[5] = linkVal(valTermRef(a0+0));
      }

      push_delay_list(p PASS_LD);
    }

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


static int
advance_wkl_state(wkl_step_state *state)
{ if ( --state->scp_index == 0 )
  { state->scp_index = scp_size(state->scp);
    if ( --state->acp_index == 0 )
    { cluster *acp, *scp;

      if ( (scp=state->scp)->prev && scp->prev->type == CLUSTER_SUSPENSIONS )
      { scp->prev->next = scp->next;
	scp->next->prev = scp->prev;
	merge_suspension_cluster(scp->prev, scp, FALSE);
	seekBuffer(&scp->members, 0, record_t);
	scp->next = state->list->free_clusters;
	state->list->free_clusters = scp;
      }

      if ( (acp=state->acp)->next && acp->next->type == CLUSTER_ANSWERS )
      { acp->prev->next = acp->next;
	acp->next->prev = acp->prev;
	merge_answer_clusters(acp->next, acp);
	seekBuffer(&acp->members, 0, trie_node*);
	acp->next = state->list->free_clusters;
	state->list->free_clusters = acp;
      }

      state->next_step = TRUE;
      return ((acp=state->list->riac) && acp->next);
    }
  }

  return TRUE;
}


/**
 * '$tbl_wkl_work'(+WorkList,
 *		   -Answer, -ModeArgs,
 *		   -Goal, -Continuation, -Wrapper, -TargetWorklist,
 *		   -Delays)
 */

static
PRED_IMPL("$tbl_wkl_work", 8, tbl_wkl_work, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  wkl_step_state *state;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { worklist *wl;

      if ( get_worklist(A1, &wl PASS_LD) )
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
	  state->acp_index = acp_size(acp);
	  state->scp_index = scp_size(scp);
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

next:
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
      state->acp_index = acp_size(acp);
      state->scp_index = scp_size(scp);
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

      /* Ignore (1) dummy restart for delayed negation,
       *        (2) conditional answer for tnot
       *        (3) removed answer due to simplification
       */

      if ( (an == NULL && !IS_TNOT(sr)) ||
	   (an != NULL && IS_TNOT(sr) && answer_is_conditional(an)) ||
	   (an != NULL && an->value == 0) )
      { if ( advance_wkl_state(state) )
	  goto next;
	goto out_fail;
      }

      /* WFS: need to add a positive node to the delay list if `an`
       * is conditional.  The positive node contains the variant
       * we continue and `an`, but is _independant_ from the
       * condition on `an`.
       */

      if ( !( tbl_unify_trie_term(an, A2 PASS_LD) &&
	      tbl_put_trie_value(modeargs, an PASS_LD) &&
	      PL_recorded(UNTNOT(sr), suspension) &&
	      PL_unify_output(A3, modeargs) &&
					/* unifies A4..A8 */
	      unify_dependency(A4, suspension, state->list, an PASS_LD)
         ) )
      { state->list->executing = FALSE;
	freeForeignState(state, sizeof(*state));
	return FALSE;			/* resource error */
      }

      DEBUG(MSG_TABLING_WORK,
	    { Sdprintf("Work: %d %d\n\t",
		       (int)state->acp_index, (int)state->scp_index);
	      PL_write_term(Serror, A2, 1200, PL_WRT_NEWLINE);
	      Sdprintf("\t");
	      PL_write_term(Serror, suspension, 1200, PL_WRT_NEWLINE);
	    });

      if ( advance_wkl_state(state) )
      { ForeignRedoPtr(state);
      } else
      { state->list->executing = FALSE;
	freeForeignState(state, sizeof(*state));
	return TRUE;
      }
    }
  }

out_fail:
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

static int
tbl_variant_table(term_t closure, term_t variant, term_t Trie, term_t status, term_t ret,
		  int flags ARG_LD)
{ trie *atrie;
  Definition def = NULL;

  get_closure_predicate(closure, &def);
  (void) def;

  if ( (atrie=get_answer_table(variant, ret, flags PASS_LD)) )
  { return ( idg_init_variant(atrie, variant PASS_LD) &&
	     _PL_unify_atomic(Trie, atrie->symbol) &&
	     unify_table_status(status, atrie, TRUE PASS_LD) );
  }

  return FALSE;
}

static
PRED_IMPL("$tbl_variant_table", 5, tbl_variant_table, 0)
{ PRED_LD

  return tbl_variant_table(A1, A2, A3, A4, A5, AT_CREATE PASS_LD);
}


static
PRED_IMPL("$tbl_moded_variant_table", 5, tbl_moded_variant_table, 0)
{ PRED_LD

  return tbl_variant_table(A1, A2, A3, A4, A5, AT_CREATE|AT_MODED PASS_LD);
}


static
PRED_IMPL("$tbl_existing_variant_table", 5, tbl_existing_variant_table, 0)
{ PRED_LD
  trie *trie;

  if ( (trie=get_answer_table(A2, A5, FALSE PASS_LD)) )
  { return ( _PL_unify_atomic(A3, trie->symbol) &&
	     unify_table_status(A4, trie, TRUE PASS_LD) );
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
 * Get the status of Trie as well as its wrapper and Skeleton.
 */

static
PRED_IMPL("$tbl_table_status", 4, tbl_table_status, 0)
{ PRED_LD
  trie *trie;
  term_t wv = PL_new_term_ref();

  return ( get_trie(A1, &trie) &&
	   unify_table_status(A2, trie, FALSE PASS_LD) &&
	   unify_skeleton(trie, wv, A4 PASS_LD) &&
	   PL_unify(A3, wv)
	 );
}

/** '$tbl_table_complete_all'(+SCC)
 *
 * Complete and reset all newly created tables.
 *
 * (*) currently we keep worklists that play a role on a network
 * of undefined answers.  That is needed for lazy answer completion
 * (see O_AC_EAGER).  If we do not do so, we still must keep track
 * of dependencies when abolishing tries.
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
      trie *atrie = wl->table;
      idg_node *n;
      int reeval, reeval_this;

      // I think we either nee to reevaluate all or none
      reeval_this = ((n=atrie->data.IDG) && n->reevaluating);
      if ( i==0 )
        reeval = reeval_this;
      else
	assert(reeval == reeval_this);

      reeval_complete(atrie);		/* incremental tabling */
    }

    for(i=0; i<ntables; i++)
    { worklist *wl = wls[i];
      trie *trie = wl->table;

      DEBUG(MSG_TABLING_WORK,
	    { term_t t = PL_new_term_ref();
	      unify_trie_term(trie->data.variant, t PASS_LD);
	      Sdprintf("Setting wl %zd in scc %zd to WL_COMPLETE.  Variant: ",
		       pointerToInt(wl), pointerToInt(c));
	      PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
	    });

						/* see (*) */
      if ( !wl->undefined && isEmptyBuffer(&wl->delays) )
      { free_worklist(wl);
	trie->data.worklist = WL_COMPLETE;
      } else
      { complete_worklist(wl);
      }
    }
    reset_newly_created_worklists(c, WLFS_FREE_NONE);
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
  { tbl_component *parent = c->parent;

    if ( c->created_worklists )
      reset_newly_created_worklists(c, WLFS_DISCARD_INCOMPLETE);
    reset_global_worklist(c);

    LD->tabling.component = parent;
    free_component(c, FC_DESTROY);

    if ( !parent )
      LD->tabling.has_scheduling_component = FALSE;
  }

  return TRUE;
}


static
PRED_IMPL("$tbl_create_subcomponent", 2, tbl_create_subcomponent, 0)
{ PRED_LD
  tbl_component *c, *p;
  trie *leader;

  if ( !get_trie(A2, &leader) )
    return FALSE;
					/* no component; create main */
  if ( !LD->tabling.has_scheduling_component )
  { LD->tabling.has_scheduling_component = TRUE;
    if ( !LD->tabling.component || LD->tabling.in_answer_completion )
      LD->tabling.component = new_component();
    else
      LD->tabling.component->status = SCC_ACTIVE;
    LD->tabling.component->leader = leader;
    return PL_unify_pointer(A1, LD->tabling.component);
  }

  c = new_component();
  c->leader = leader;
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
		 *	RECURSIVE TABLING	*
		 *******************************/

static
PRED_IMPL("$tbl_scc_save", 1, tbl_scc_save, 0)
{ PRED_LD

  if ( PL_unify_term(A1, PL_FUNCTOR, FUNCTOR_table_state3,
		     PL_POINTER, LD->tabling.component,
		     PL_BOOL,    LD->tabling.has_scheduling_component,
		     PL_BOOL,    LD->tabling.in_answer_completion) )
  { LD->tabling.component = NULL;
    LD->tabling.has_scheduling_component = FALSE;
    LD->tabling.in_answer_completion = FALSE;

    return TRUE;
  }

  return FALSE;
}


static
PRED_IMPL("$tbl_scc_restore", 1, tbl_scc_restore, 0)
{ PRED_LD

  if ( PL_is_functor(A1, FUNCTOR_table_state3) )
  { void *ptr;
    int hsc, iac;
    term_t arg;

    if ( (arg=PL_new_term_ref()) &&
	 _PL_get_arg(1, A1, arg) &&
	 PL_get_pointer(arg, &ptr) &&
	  _PL_get_arg(2, A1, arg) &&
	 PL_get_bool_ex(arg, &hsc) &&
	  _PL_get_arg(3, A1, arg) &&
	 PL_get_bool_ex(arg, &iac) )
    { LD->tabling.component = ptr;
      LD->tabling.has_scheduling_component = hsc;
      LD->tabling.in_answer_completion = iac;

      return TRUE;
    }
  }

  return FALSE;
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
uc_put_trie_value(term_t t, trie_node *an ARG_LD)
{ static atom_t anull;

  if ( !anull )
    anull = PL_new_atom("NULL");

  if ( !an || an->value )
    return tbl_put_trie_value(t, an PASS_LD);
  else
    return PL_put_atom(t, anull);
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
    { trie_node *an = *ap;

      if ( !PL_put_variable(answer) ||
	   !PL_unify_list(tail, head, tail) ||
	   !tbl_unify_trie_term(an, answer PASS_LD) ||
	   !uc_put_trie_value(modeav, an PASS_LD) ||
	   !PL_unify_term(head, PL_FUNCTOR, FUNCTOR_minus2,
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
	   !PL_recorded(UNTNOT(*sp), tmp) ||
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

  if ( get_worklist(A1, &wl PASS_LD) )
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


typedef struct
{ term_t skel;
} answer_ctx;

/** '$tbl_answer'(+Trie, -Answer, -Condition) is nondet.
 */

static int
put_delay_set(term_t cond, delay_info *di, delay_set *set,
	      answer_ctx *ctx ARG_LD)
{ delay *base, *top;
  term_t av = PL_new_term_refs(4);
  int count = 0;
  term_t gshare = 0;
  term_t gskel = 0;
  size_t arity = 0;

  get_delay_set(di, set, &base, &top);

  /* Get the variable sharing term */
  if ( top > base && top[-1].answer && !top[-1].variant )
  { record_t r;

    assert(IS_REC_DELAY(top[-1].answer));
    r = UNREC_DELAY(top[-1].answer);
    if ( !(gshare = PL_new_term_refs(2)) ||
	 !PL_recorded(r, gshare) )
      return FALSE;
    PL_get_name_arity(gshare, NULL, &arity);
    gskel = gshare+1;
    _PL_get_arg(1, gshare, gskel);
    if ( !PL_unify(gskel, ctx->skel) )
    { DEBUG(0, Sdprintf("Oops, global skeleton doesn't unify\n"));
      return FALSE;
    }

    top--;

    DEBUG(MSG_TABLING_DELAY_VAR,
	  { Sdprintf("Got sharing skeleton of size %zd\n", arity);
	    pl_writeln(gshare);
	  });
  }

  for(--top; top >= base; top--)
  { term_t c1 = count == 0 ? cond : av+0;

    if ( top->variant == DV_DELETED )
    { if ( top->answer && !is_ground_trie_node(top->answer) )
	arity--;
      continue;
    }
    if ( top->answer )
    { term_t ans  = av+1;

      PL_put_variable(c1);
      PL_put_variable(ans);

      if ( !unify_trie_term(top->variant->data.variant, c1 PASS_LD) )
	return FALSE;
      if ( !get_answer_table(c1, ans, FALSE PASS_LD) )
      { Sdprintf("OOPS! could not find variant table\n");
	return FALSE;
      }
      if ( !unify_trie_term(top->answer, ans PASS_LD) )
	return FALSE;

      if ( !is_ground_trie_node(top->answer) )
      { assert(gshare);

	_PL_get_arg(arity, gshare, gskel);
	if ( !PL_unify(gskel, ans) )
	{ DEBUG(0, Sdprintf("Oops, skeleton %zd does not unify\n", arity));
	  pl_writeln(gskel);
	  pl_writeln(ans);
	  return FALSE;
	}
	arity--;
      }
    } else
    { PL_put_variable(c1);
      if ( !unify_trie_term(top->variant->data.variant, c1 PASS_LD) ||
	   !PL_cons_functor(c1, FUNCTOR_tnot1, c1) )
	return FALSE;
    }

    if ( count++ > 0 )
    { if ( !PL_cons_functor(cond, FUNCTOR_comma2, c1, cond) )
	return FALSE;
    }
  }

  PL_reset_term_refs(av);

  return TRUE;
}

static int
unify_delay_info(term_t t, trie_node *answer, void *ctxp ARG_LD)
{ delay_info *di;

  if ( (di=answer_delay_info(NULL, answer, FALSE)) )
  { if ( DL_IS_DELAY_LIST(di) )
    { term_t av = PL_new_term_refs(2);
      term_t cond = av+1;
      delay_set *base, *top;
      int count = 0;
      answer_ctx *ctx = ctxp;

      delay_sets(di, &base, &top);
      for(; base < top; base++)
      { term_t c1 = count == 0 ? cond : av+0;

	if ( isEmptyBuffer(&di->delay_sets) )
	  continue;

	if ( !put_delay_set(c1, di, base, ctx PASS_LD) )
	  return FALSE;

	if ( count++ > 0 )
	{ if ( !PL_cons_functor_v(cond, FUNCTOR_semicolon2, av) )
	    return FALSE;
	}
      }

      return PL_unify(t, cond);
    } else
    { return PL_unify_atom(t, ATOM_undefined);
    }
  } else
  { return PL_unify_atom(t, ATOM_true);
  }
}

#if O_DEBUG
static int
put_delay_info(term_t t, trie_node *answer)
{ GET_LD
  answer_ctx ctx;

  ctx.skel = PL_new_term_ref();			/* TBD */
  PL_put_variable(t);
  return unify_delay_info(t, answer, &ctx PASS_LD);
}
#endif

/** '$tbl_answer'(+Trie, ?Skeleton, -Condition) is nondet.
 */

static
PRED_IMPL("$tbl_answer", 3, tbl_answer, PL_FA_NONDETERMINISTIC)
{ answer_ctx ctx;

  ctx.skel = A2;
  return trie_gen(A1, A2, 0, A3, unify_delay_info, &ctx, PL__ctx);
}

static int
unify_delay_info_dl(term_t t, trie_node *answer, void *ctx ARG_LD)
{ (void) ctx;

  if ( answer_is_conditional(answer) )
  { if ( is_ground_trie_node(answer) )
      return PL_unify_pointer(t, answer);
    else
      return PL_unify_atom(t, ATOM_nonground);
  } else
  { return PL_unify_atom(t, ATOM_true);
  }
}

static
PRED_IMPL("$tbl_answer_dl", 3, tbl_answer_dl, PL_FA_NONDETERMINISTIC)
{ return trie_gen(A1, A2, 0, A3, unify_delay_info_dl, NULL, PL__ctx);
}


/** '$tbl_answer_update_dl'(+ATrie, -Skeleton) is nondet.
 *
 * Obtain an answer from ATrie.  If the answer is conditional, update
 * the global delay list.  If the answer is ground with a term
 * ATrie+ANode and otherwise ATrie+Wrapper
 */

typedef struct
{ term_t atrie;
} update_dl_ctx;

static int
answer_update_delay_list(term_t wrapper, trie_node *answer, void *vctx ARG_LD)
{ update_dl_ctx *ctx = vctx;

  if ( answer_is_conditional(answer) )
  { Word p;

    if ( !ensureStackSpace(6, 2) )
      return FALSE;
    p = valTermRef(ctx->atrie);
    deRef(p);
    assert(isAtom(*p));

   tbl_push_delay(*p, valTermRef(wrapper), answer PASS_LD);
  }

  return TRUE;
}

static
PRED_IMPL("$tbl_answer_update_dl", 2, tbl_answer_update_dl,
	  PL_FA_NONDETERMINISTIC)
{ update_dl_ctx ctx;

  ctx.atrie = A1;

  return trie_gen(A1, A2, 0, A2, answer_update_delay_list, &ctx, PL__ctx);
}


/** '$tbl_implementation'(:G0, -G) is det.
 *
 * Find location where G is actually defined and raise an error of the
 * predicate is not tabled.
 */

static int
tbl_implementation(term_t g0, term_t g, int must_be_tabled ARG_LD)
{ Module m = NULL;
  term_t t = PL_new_term_ref();
  functor_t f = 0;
  Procedure proc;
  Definition def;

  if ( !PL_strip_module(g0, &m, t) )
    return FALSE;
  if ( !PL_get_functor(t, &f) )
    return PL_type_error("callable", g0);
  if ( !(proc = resolveProcedure(f, m)) )
    return FALSE;				/* should not happen */

  if ( !isDefinedProcedure(proc) )
    trapUndefined(getProcDefinition(proc) PASS_LD);
  def = getProcDefinition(proc);

  if ( must_be_tabled && false(def, P_TABLED) )
  { return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
		    ATOM_tnot, ATOM_non_tabled_procedure, proc);
  }

  if ( def->module == m )
  { return PL_unify(g, g0);
  } else
  { return PL_unify_term(g, PL_FUNCTOR, FUNCTOR_colon2,
			      PL_ATOM, def->module->name,
			      PL_TERM, t);
  }
}


static
PRED_IMPL("$tnot_implementation", 2, tnot_implementation, PL_FA_TRANSPARENT)
{ PRED_LD

  return tbl_implementation(A1, A2, TRUE PASS_LD);
}

static
PRED_IMPL("$tbl_implementation", 2, tbl_implementation, PL_FA_TRANSPARENT)
{ PRED_LD

  return tbl_implementation(A1, A2, FALSE PASS_LD);
}

/**
 * '$is_answer_trie'(@Trie) is semidet
 *
 * True if Trie is an answer trie, possible already destroyed.  This
 * is used to find remaining tables for gc_tables/1.
 */

static
PRED_IMPL("$is_answer_trie", 1, is_answer_trie, 0)
{ trie *trie;

  if ( get_trie_noex(A1, &trie) )
    return trie->release_node == release_answer_node;

  return FALSE;
}

		 /*******************************
		 *	 IDG CONSTRUCTION	*
		 *******************************/

/** '$idg_add_edge'(+Trie)
 *
 * Add Trie to the IDG
 */

static idg_node *
idg_new(trie *atrie)
{ idg_node *n = PL_malloc(sizeof(*n));

  memset(n, 0, sizeof(*n));
  n->atrie = atrie;

  return n;
}

static void
idg_clean_affected(idg_node *node)
{ Table table;

  if ( (table=node->affected) )
  { node->affected = NULL;
    destroyHTable(table);
  }
}

static void
idg_clean_dependent(idg_node *node)
{ Table table;

  if ( (table=node->dependent) )
  { node->dependent = NULL;
    destroyHTable(table);
  }
}


static void
idg_destroy(idg_node *node)
{ idg_clean_affected(node);
  idg_clean_dependent(node);

  PL_free(node);
}

static void
idg_free_affected(void *n, void *v)
{ idg_node *child  = v;
  idg_node *parent = n;

  assert(parent->dependent);
  if ( !deleteHTable(parent->dependent, child) )
    Sdprintf("OOPS: idg_free_affected() failed to delete backlink\n");
}

static void
idg_free_dependent(void *n, void *v)
{ idg_node *parent = v;
  idg_node *child  = n;

  assert(child->affected);
  if ( !deleteHTable(child->affected, parent) )
    Sdprintf("OOPS: idg_free_dependent() failed to delete backlink\n");
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a bi-directional link between parent and   child node. We use the
hash tables as sets only, but we use   the _other side_ as entry _value_
to recover the full link and allow   ->free_symbol()  to delete the back
pointer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
idg_add_child(idg_node *parent, idg_node *child ARG_LD)
{ if ( !child->affected )
  { child->affected = newHTable(4);
    child->affected->free_symbol = idg_free_affected;
  }
  addHTable(child->affected, parent, child);

  if ( !parent->dependent )
  { parent->dependent = newHTable(4);
    parent->dependent->free_symbol = idg_free_dependent;
  }
  addHTable(parent->dependent, child, parent);
}


static int
idg_init_variant(trie *atrie, term_t variant ARG_LD)
{ if ( !atrie->data.IDG )
  { Procedure proc;

    if ( get_procedure(variant, &proc, 0, GP_RESOLVE|GP_EXISTENCE_ERROR) )
    { if ( true(proc->definition, P_INCREMENTAL) )
	atrie->data.IDG = idg_new(atrie);
    } else
      return FALSE;
  }

  return TRUE;
}


static int
set_idg_current(trie *atrie ARG_LD)
{ int rc;
  Word p;

  if ( (rc=ensureGlobalSpace(0, ALLOW_GC)) != TRUE )
    return raiseStackOverflow(rc);
  p = valTermRef(LD->tabling.idg_current);
  TrailAssignment(p);
  if ( atrie )
    *p = trie_symbol(atrie);
  else
    setVar(*p);

  return TRUE;
}

static trie *
idg_add_edge(trie *atrie ARG_LD)
{ atom_t current;
  trie *ctrie;

  if ( atrie->data.IDG &&
       (current = *valTermRef(LD->tabling.idg_current)) &&
       (ctrie = symbol_trie(current)) )
  { if ( ctrie->data.IDG )
    { DEBUG(MSG_TABLING_IDG,
	    { term_t f = PL_new_term_ref();
	      term_t t = PL_new_term_ref();
	      unify_trie_term(ctrie->data.variant, f PASS_LD);
	      unify_trie_term(atrie->data.variant, t PASS_LD);
	      Sdprintf("IDG: Edge ");
	      PL_write_term(Serror, f, 999, 0);
	      Sdprintf(" -> ");
	      PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
	    });

      idg_add_child(ctrie->data.IDG, atrie->data.IDG PASS_LD);
      return ctrie;
    }
  }

  return NULL;
}

/** '$idg_add_edge'(+ATrie)
 *
 * Create an edge from ATrie to the current node.   The notion of
 * current is NOT UPDATED.
 */

static
PRED_IMPL("$idg_add_edge", 1, idg_add_edge, 0)
{ PRED_LD
  trie *atrie;

  if ( get_trie(A1, &atrie) )
  { idg_add_edge(atrie PASS_LD);
    return TRUE;
  }

  return FALSE;
}


/** '$idg_add_edge'(-OldCurrent, +ATrie)
 *
 * Create an edge from ATrie to the current node, set the current to
 * Atrie and return the old current.
 */

static
PRED_IMPL("$idg_add_edge", 2, idg_add_edge, 0)
{ PRED_LD
  trie *atrie;

  if ( get_trie(A2, &atrie) )
  { atom_t current;

    if ( (current = *valTermRef(LD->tabling.idg_current)) )
    { if ( !PL_unify_atom(A1, current) )
	return FALSE;
    }
    idg_add_edge(atrie PASS_LD);

    return set_idg_current(atrie PASS_LD);
  }

  return FALSE;
}


static
PRED_IMPL("$idg_add_dyncall", 1, idg_add_dyncall, 0)
{ PRED_LD
  trie *atrie;

  if ( (atrie=get_answer_table(A1, 0, TRUE PASS_LD)) )
  { trie *ctrie;

    if ( !atrie->data.IDG )
    { assert(!atrie->data.worklist || atrie->data.worklist == WL_GROUND);
      atrie->data.worklist = WL_DYNAMIC;
      atrie->data.IDG = idg_new(atrie);
    }
    if ( (ctrie=idg_add_edge(atrie PASS_LD)) )	/* Does not update current. */
    { if ( ctrie->data.IDG->reevaluating )	/* Should it? */
	atrie->data.IDG->falsecount = 0;
    }

    return TRUE;
  }

  return FALSE;
}

static
PRED_IMPL("$idg_set_current_wl", 1, idg_set_current_wl, 0)
{ PRED_LD
  worklist *wl;

  if ( get_worklist(A1, &wl PASS_LD) )
  { trie *atrie = wl->table;

    DEBUG(MSG_TABLING_IDG,
	  { term_t t = PL_new_term_ref();
	    unify_trie_term(atrie->data.variant, t PASS_LD);
	    Sdprintf("IDG: Set current to ");
	    PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
	  });

    return set_idg_current(atrie PASS_LD);
  }

  return FALSE;
}

static
PRED_IMPL("$idg_set_current", 1, idg_set_current, 0)
{ PRED_LD
  trie *atrie;

  if ( get_trie_noex(A1, &atrie) )
  { DEBUG(MSG_TABLING_IDG,
	  { term_t t = PL_new_term_ref();
	    unify_trie_term(atrie->data.variant, t PASS_LD);
	    Sdprintf("IDG: Set current to ");
	    PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
	  });

    return set_idg_current(atrie PASS_LD);
  } else
  { return set_idg_current(NULL PASS_LD);
  }

  return TRUE;
}


static
PRED_IMPL("$idg_reset_current", 0, idg_reset_current, 0)
{ PRED_LD

  return set_idg_current(NULL PASS_LD);
}


		 /*******************************
		 *	    IDG QUERYING	*
		 *******************************/

/** '$idg_edge'(+ATrie, ?Direction, ?Node)
 *
 * Enumerate over the edges of the dependency graph
 */

typedef struct idg_edge_state
{ trie *	atrie;
  Table		table;
  TableEnum	tenum;
  atom_t	dir;
  int		fixed_dir;
  int		allocated;
  atom_t	deptrie_symbol;
} idg_edge_state;


static int
advance_idg_edge_state(idg_edge_state *state)
{ void *k, *v;

retry:
  if ( advanceTableEnum(state->tenum, &k, &v) )
  { idg_node *n = k;

    state->deptrie_symbol = trie_symbol(n->atrie);
    return TRUE;
  } else
  { freeTableEnum(state->tenum);
    state->tenum = NULL;

    if ( !state->fixed_dir && state->dir == ATOM_affected )
    { if ( (state->table = state->atrie->data.IDG->dependent) )
      { state->dir = ATOM_dependent;
	state->tenum = newTableEnum(state->table);
	goto retry;
      }
    }
  }

  return FALSE;
}

static void
free_idg_edge_state(idg_edge_state *state)
{ if ( state->tenum )
    freeTableEnum(state->tenum);
  if ( state->allocated )
    freeForeignState(state, sizeof(*state));
}

static idg_edge_state *
save_idg_edge_state(idg_edge_state *state)
{ if ( !state->allocated )
  { idg_edge_state *n = allocForeignState(sizeof(*n));

    *n = *state;
    n->allocated = TRUE;
    return n;
  }

  return state;
}


static
PRED_IMPL("$idg_edge", 3, idg_edge, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  idg_edge_state sbuf;
  idg_edge_state *state;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { trie *to;

      state = &sbuf;
      memset(state, 0, sizeof(*state));

      if ( !get_trie(A1, &state->atrie) )
	return FALSE;
      if ( !state->atrie->data.IDG )
	return FALSE;

      if ( PL_is_variable(A2) )
      { if ( (state->table = state->atrie->data.IDG->affected) )
	{ state->dir = ATOM_affected;
	} else if ( (state->table = state->atrie->data.IDG->dependent) )
	{ state->dir = ATOM_dependent;
	  if ( !PL_unify_atom(A2, ATOM_dependent) )
	    return FALSE;
	  state->fixed_dir = TRUE;
	} else
	  return FALSE;
      } else if ( PL_get_atom_ex(A2, &state->dir) )
      { state->fixed_dir = TRUE;
	if ( state->dir == ATOM_affected )
	  state->table = state->atrie->data.IDG->affected;
	else if ( state->dir == ATOM_dependent )
	  state->table = state->atrie->data.IDG->dependent;
	else
	  return PL_domain_error("idg_edge_dir", A2);
      }

      if ( !state->table )
	return FALSE;

      if ( PL_is_variable(A3) )
      { state->tenum = newTableEnum(state->table);
	if ( advance_idg_edge_state(state) )
	  break;
	free_idg_edge_state(state);
	return FALSE;
      } else if ( get_trie(A3, &to) )
      { return lookupHTable(state->table, to) != NULL;
      }
    }
    case FRG_REDO:
      state = CTX_PTR;
      break;
    case FRG_CUTTED:
      state = CTX_PTR;
      free_idg_edge_state(state);
      return TRUE;
    default:
      assert(0);
      return FALSE;
  }

  Mark(fli_context->mark);
  do
  { if ( PL_unify_atom(A3, state->deptrie_symbol) )
    { if ( state->fixed_dir ||
	   PL_unify_atom(A2, state->dir) )
      { if ( advance_idg_edge_state(state) )
	  ForeignRedoPtr(save_idg_edge_state(state));
	free_idg_edge_state(state);
	return TRUE;
      }
    }

    if ( PL_exception(0) )
    { free_idg_edge_state(state);
      return FALSE;
    }

    Undo(fli_context->mark);
  } while(advance_idg_edge_state(state));

  free_idg_edge_state(state);
  return FALSE;
}


typedef struct idg_propagate_state
{ size_t modified;
  trie *incomplete;				/* hit an incomplete trie */
  TableEnum en;
  segstack  stack;
  idg_node  *buf[100];
} idg_propagate_state;


static void
idg_changed_loop(idg_propagate_state *state, int changed)
{ typedef struct idg_node *IDGNode;

  for(;;)
  { void *k, *v;
    idg_node *next;

    while( advanceTableEnum(state->en, &k, &v) )
    { idg_node *n = k;

      DEBUG(MSG_TABLING_IDG_CHANGED,
	    { GET_LD
		term_t v = PL_new_term_ref();

	      unify_trie_term(n->atrie->data.variant, v PASS_LD);
	      Sdprintf("IDG: propagate falsecount (re-eval=%d, falsecount=%d) to: ",
		       n->reevaluating, n->falsecount);
	      PL_write_term(Serror, v, 999, PL_WRT_NEWLINE);
	    });

      if ( n->reevaluating )
	continue;

      if ( changed )
      { if ( table_is_incomplete(n->atrie) )
	  state->incomplete = n->atrie;		/* return? */
	if ( n->falsecount++ == 0 )
	{ if ( n->affected )
	    pushSegStack(&state->stack, n, IDGNode);
	}
      } else
      { if ( --n->falsecount == 0 )
	{ if ( n->affected )
	    pushSegStack(&state->stack, n, IDGNode);
	}
      }
    }
    freeTableEnum(state->en);

    if ( popSegStack(&state->stack, &next, IDGNode) )
    { assert(next->affected);
      state->en = newTableEnum(next->affected);
    } else
      break;
  }
}


static trie *
idg_propagate_change(idg_node *n, int changed)
{ if ( n->affected )
  { idg_propagate_state state;

    state.modified = 0;
    state.incomplete = NULL;
    initSegStack(&state.stack, sizeof(idg_node*), sizeof(state.buf), state.buf);
    state.en = newTableEnum(n->affected);
    idg_changed_loop(&state, changed);
    clearSegStack(&state.stack);

    return state.incomplete;
  }

  return NULL;
}


static int
change_incomplete_error(trie *atrie)
{ GET_LD
  term_t v;

  return ( (v=PL_new_term_ref()) &&
	   unify_trie_term(atrie->data.variant, v PASS_LD) &&
	   PL_permission_error("update", "variant", v) );
}


static int
idg_changed(trie *atrie)
{ idg_node *n;

  DEBUG(MSG_TABLING_IDG_CHANGED,
	{ GET_LD
	  term_t v = PL_new_term_ref();

	  unify_trie_term(atrie->data.variant, v PASS_LD);
	  Sdprintf("IDG: dynamic change: ");
	  PL_write_term(Serror, v, 999, 0);
	});

  if ( (n=atrie->data.IDG) && n->falsecount == 0 )
  { trie *incomplete;

    DEBUG(MSG_TABLING_IDG_CHANGED, Sdprintf(" (propagating)\n"));

    if ( table_is_incomplete(atrie) )
      return change_incomplete_error(atrie);
    n->falsecount = 1;

    if ( (incomplete=idg_propagate_change(n, TRUE)) )
    { n->falsecount = 0;
      idg_propagate_change(n, FALSE);
      return change_incomplete_error(incomplete);
    }
  } else
  { DEBUG(MSG_TABLING_IDG_CHANGED,
	  if ( n ) Sdprintf(" (already changed (%d))\n", n->falsecount);
	  else Sdprintf(" (no IDG)\n"));
  }

  return TRUE;
}


static
PRED_IMPL("$idg_changed", 1, idg_changed, 0)
{ trie *atrie;

  if ( get_trie(A1, &atrie) )
    return idg_changed(atrie);

  return FALSE;
}


static
PRED_IMPL("$idg_falsecount", 2, idg_falsecount, 0)
{ GET_LD
  trie *atrie;

  if ( get_trie(A1, &atrie) )
  { idg_node *n;

    if ( (n=atrie->data.IDG) )
      return PL_unify_integer(A2, n->falsecount);

    return FALSE;
  }

  return FALSE;
}


static
PRED_IMPL("$idg_set_falsecount", 2, idg_set_falsecount, 0)
{ trie *atrie;

  if ( get_trie(A1, &atrie) )
  { idg_node *n;

    if ( (n=atrie->data.IDG) )
      return PL_get_integer_ex(A2, &n->falsecount);

    return FALSE;
  }

  return FALSE;
}


		 /*******************************
		 *  INCREMENTAL RE-EVALUATION	*
		 *******************************/

static void *
reeval_prep_node(trie_node *n, void *ctx)
{ trie *atrie = ctx;

  if ( n->value )
  { set(n, TN_IDG_DELETED);
    if ( answer_is_conditional(n) )
    { destroy_delay_info(atrie, n, TRUE);
      n->data.delayinfo = NULL;
    } else
      set(n, TN_IDG_UNCONDITIONAL);
  }

  return NULL;
}


static
PRED_IMPL("$tbl_reeval_prepare", 1, tbl_reeval_prepare, 0)
{ trie *atrie;

  if ( get_trie(A1, &atrie) )
  { idg_node *idg = atrie->data.IDG;

    DEBUG(MSG_TABLING_IDG_REEVAL,
	  { GET_LD
	    term_t t = PL_new_term_ref();
	    unify_trie_term(atrie->data.variant, t PASS_LD);
	    Sdprintf("Preparing re-evaluation of ");
	    PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
	  });

    idg->answer_count = atrie->value_count;
    idg->new_answer = FALSE;
    idg->falsecount = 0;
    idg_clean_dependent(idg);
    map_trie_node(&atrie->root, reeval_prep_node, atrie);
    idg->reevaluating = TRUE;

    return TRUE;
  }

  return FALSE;
}


static void *
reeval_complete_node(trie_node *n, void *ctx)
{ trie *atrie = ctx;

  if ( true(n, TN_IDG_DELETED) )
  { clear(n, TN_IDG_DELETED);		/* not used by trie admin */
    trie_delete(atrie, n, FALSE);	/* TBD: can we prune? */
    if ( false(n, TN_IDG_UNCONDITIONAL) )
      simplify_answer(atrie->data.worklist, n, FALSE);
  } else if ( true(n, TN_IDG_UNCONDITIONAL) &&
	      answer_is_conditional(n) )
  { atrie->data.IDG->new_answer = TRUE;
  }

  return NULL;
}


static void
reeval_complete(trie *atrie)
{ idg_node *n;

  if ( (n=atrie->data.IDG) && n->reevaluating )
  { map_trie_node(&atrie->root, reeval_complete_node, atrie);

    DEBUG(MSG_TABLING_IDG_REEVAL,
	  { GET_LD
	    term_t t = PL_new_term_ref();
	    unify_trie_term(atrie->data.variant, t PASS_LD);
	    Sdprintf("Re-evaluation of ");
	    PL_write_term(Serror, t, 999, 0);
	  });

    if ( n->new_answer == FALSE &&
	 n->answer_count == atrie->value_count )
    { DEBUG(MSG_TABLING_IDG_REEVAL, Sdprintf(": same answers\n"));
      idg_propagate_change(n, FALSE);
    } else
    { DEBUG(MSG_TABLING_IDG_REEVAL,
	    Sdprintf(": modified (new=%d, count %zd -> %zd)\n",
		     n->new_answer, n->answer_count, atrie->value_count));
    }

    n->reevaluating = FALSE;
  }
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

#define NDET PL_FA_NONDETERMINISTIC
#define META PL_FA_TRANSPARENT

BeginPredDefs(tabling)
  PRED_DEF("$tbl_new_worklist",		2, tbl_new_worklist,	     0)
  PRED_DEF("$tbl_pop_worklist",		2, tbl_pop_worklist,	     0)
  PRED_DEF("$tbl_wkl_add_answer",	4, tbl_wkl_add_answer,	     0)
  PRED_DEF("$tbl_wkl_mode_add_answer",	4, tbl_wkl_mode_add_answer,  0)
  PRED_DEF("$tbl_wkl_make_follower",    1, tbl_wkl_make_follower,    0)
  PRED_DEF("$tbl_wkl_add_suspension",	2, tbl_wkl_add_suspension,   0)
  PRED_DEF("$tbl_wkl_done",		1, tbl_wkl_done,	     0)
  PRED_DEF("$tbl_wkl_negative",		1, tbl_wkl_negative,	     0)
  PRED_DEF("$tbl_wkl_is_false",		1, tbl_wkl_is_false,	     0)
  PRED_DEF("$tbl_wkl_answer_trie",	2, tbl_wkl_answer_trie,      0)
  PRED_DEF("$tbl_wkl_work",		8, tbl_wkl_work,          NDET)
  PRED_DEF("$tbl_variant_table",	5, tbl_variant_table,	     0)
  PRED_DEF("$tbl_existing_variant_table", 5, tbl_existing_variant_table, 0)
  PRED_DEF("$tbl_moded_variant_table",	5, tbl_moded_variant_table,  0)
  PRED_DEF("$tbl_variant_table",        1, tbl_variant_table,        0)
  PRED_DEF("$tbl_table_status",		4, tbl_table_status,	     0)
  PRED_DEF("$tbl_table_complete_all",	1, tbl_table_complete_all,   0)
  PRED_DEF("$tbl_free_component",       1, tbl_free_component,       0)
  PRED_DEF("$tbl_table_discard_all",    1, tbl_table_discard_all,    0)
  PRED_DEF("$tbl_create_subcomponent",  2, tbl_create_subcomponent,  0)
  PRED_DEF("$tbl_component_status",     2, tbl_component_status,     0)
  PRED_DEF("$tbl_abolish_all_tables",   0, tbl_abolish_all_tables,   0)
  PRED_DEF("$tbl_destroy_table",        1, tbl_destroy_table,        0)
  PRED_DEF("$tbl_trienode",             1, tbl_trienode,             0)
  PRED_DEF("$tbl_delay_list",           1, tbl_delay_list,           0)
  PRED_DEF("$tbl_set_delay_list",       1, tbl_set_delay_list,       0)
  PRED_DEF("$tbl_add_global_delays",    2, tbl_add_global_delays,    0)

  PRED_DEF("$tbl_scc_save",             1, tbl_scc_save,             0)
  PRED_DEF("$tbl_scc_restore",          1, tbl_scc_restore,          0)

  PRED_DEF("$tbl_scc",                  1, tbl_scc,                  0)
  PRED_DEF("$tbl_scc_data",             2, tbl_scc_data,             0)
  PRED_DEF("$tbl_worklist_data",        2, tbl_worklist_data,        0)
  PRED_DEF("$tbl_answer",               3, tbl_answer,            NDET)
  PRED_DEF("$tbl_answer_dl",		3, tbl_answer_dl,         NDET)
  PRED_DEF("$tbl_answer_update_dl",     2, tbl_answer_update_dl,  NDET)
  PRED_DEF("$tbl_force_truth_value",    3, tbl_force_truth_value,    0)
  PRED_DEF("$tbl_set_answer_completed", 1, tbl_set_answer_completed, 0)
  PRED_DEF("$tbl_is_answer_completed",  1, tbl_is_answer_completed,  0)
  PRED_DEF("$tnot_implementation",      2, tnot_implementation,   META)
  PRED_DEF("$tbl_implementation",       2, tbl_implementation,    META)
  PRED_DEF("$is_answer_trie",           1, is_answer_trie,           0)

  PRED_DEF("$idg_add_edge",             1, idg_add_edge,             0)
  PRED_DEF("$idg_add_edge",             2, idg_add_edge,             0)
  PRED_DEF("$idg_add_dyncall",          1, idg_add_dyncall,          0)
  PRED_DEF("$idg_set_current_wl",       1, idg_set_current_wl,       0)
  PRED_DEF("$idg_set_current",          1, idg_set_current,          0)
  PRED_DEF("$idg_reset_current",        0, idg_reset_current,        0)
  PRED_DEF("$idg_edge",                 3, idg_edge,              NDET)
  PRED_DEF("$idg_changed",              1, idg_changed,              0)
  PRED_DEF("$idg_falsecount",           2, idg_falsecount,           0)
  PRED_DEF("$idg_set_falsecount",       2, idg_set_falsecount,       0)

  PRED_DEF("$tbl_reeval_prepare",       1, tbl_reeval_prepare,	     0)
EndPredDefs
