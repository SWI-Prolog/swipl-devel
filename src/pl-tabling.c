/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017-2020, VU University Amsterdam
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
#include "pl-comp.h"
#include "pl-arith.h"
#include "pl-tabling.h"
#include "pl-copyterm.h"
#include "pl-wrap.h"
#include "pl-event.h"
#include "pl-allocpool.h"

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

#define SINDEX_MAX	32

typedef struct sindex_key
{ unsigned	argn;
  unsigned	key;
} sindex_key;

typedef struct suspension
{ record_t	term;		/* dependency/5 term with TNOT flag */
  record_t	instance;	/* Filter for call subsumption */
  sindex_key   *keys;		/* Index to find filter candidates */
} suspension;

typedef struct answer
{ trie_node    *node;		/* Point into answer trie */
} answer;

typedef struct
{ worklist     *list;		/* worklist we enumerate */
  cluster      *acp;		/* Current answer cluster */
  cluster      *scp;		/* Current suspension cluster */
  answer       *answer;		/* Current answer */
  int		acp_index;	/* Index in anser cluster */
  struct
  { suspension *base;
    suspension *top;
    suspension *here;
  } suspensions;
  int		keys_inited;	/* #Initialized keys */
  sindex_key	keys[SINDEX_MAX]; /* suspension matching */
} wkl_step_state;

static int	destroy_answer_trie(trie *atrie);
static void	free_worklist(worklist *wl);
static void	clean_worklist(worklist *wl);
static void	destroy_depending_worklists(worklist *wl0);
static void	free_worklist_set(worklist_set *wls, int freewl);
static void	add_global_worklist(worklist *wl);
static tbl_component *tbl_create_subcomponent(trie *leader ARG_LD);
static worklist *tbl_add_worklist(trie *atrie, tbl_component *scc);
static int	wl_has_work(const worklist *wl);
static cluster *new_answer_cluster(worklist *wl, answer *first);
static void	wkl_append_left(worklist *wl, cluster *c);
static int	wkl_add_answer(worklist *wl, trie_node *node ARG_LD);
static int	wkl_mode_add_answer(worklist *wl, term_t answer,
				    term_t delays ARG_LD);
static int	tbl_put_moded_args(term_t t, trie_node *node ARG_LD);
static void	del_child_component(tbl_component *parent, tbl_component *child);
static void	free_components_set(component_set *cs, int destroy);
static int	unify_skeleton(trie *trie, term_t wrapper, term_t skel ARG_LD);
#ifdef O_DEBUG
static void	print_worklist(const char *prefix, worklist *wl);
static void	print_delay(const char *msg,
			    trie_node *variant, trie_node *answer);
static void	print_answer(const char *msg, trie_node *answer);
static int	put_delay_info(term_t t, trie_node *answer);
static void	print_answer_table(trie *atrie, const char *msg, ...);
#endif
static int	simplify_component(tbl_component *scc);
static void	idg_destroy(idg_node *node);
static void	idg_reset(idg_node *node);
static int	idg_init_variant(trie *atrie, Definition def, term_t variant
				 ARG_LD);
static void	reeval_complete(trie *atrie);
static void	reset_reevaluation(trie *atrie);
static int	unify_component_status(term_t t, tbl_component *scc ARG_LD);
static int	simplify_answer(worklist *wl, trie_node *answer, int truth);
static int	table_is_incomplete(trie *trie);
static int	idg_add_edge(trie *atrie, trie *ctrie ARG_LD);
static int	idg_set_current_wl(term_t wlref ARG_LD);
#ifdef O_PLMT
static int	claim_answer_table(trie *atrie, atom_t *clrefp,
				   int flags ARG_LD);
#endif
static atom_t	tripwire_answers_for_subgoal(worklist *wl ARG_LD);
static int	generalise_answer_substitution(term_t spec, term_t gen ARG_LD);
static int	add_answer_count_restraint(void);
static int	add_radial_restraint(void);
static int	tbl_wl_tripwire(worklist *wl, atom_t action, atom_t wire);
static int	tbl_pred_tripwire(Definition def, atom_t action, atom_t wire);

#define WL_IS_SPECIAL(wl)  (((intptr_t)(wl)) & 0x1)
#define WL_IS_WORKLIST(wl) ((wl) && !WL_IS_SPECIAL(wl))

#define WL_GROUND   ((worklist *)0x21)
#define WL_DYNAMIC  ((worklist *)0x41)

#define WLFS_FREE_NONE		0x0000
#define WLFS_KEEP_COMPLETE	0x0001
#define WLFS_FREE_ALL		0x0002
#define WLFS_DISCARD_INCOMPLETE	0x0004

#define DV_DELETED		((trie*)0x1)
#define DL_UNDEFINED		((delay_info*)0x1)

#define DL_IS_DELAY_LIST(dl)	((dl) && (dl) != DL_UNDEFINED)


#ifdef O_PLMT
#define	LOCK_SHARED_TABLE(t)	countingMutexLock(&GD->tabling.mutex);
#define	UNLOCK_SHARED_TABLE(t)	countingMutexUnlock(&GD->tabling.mutex);

static inline void
drop_trie(trie *atrie)
{
#ifdef O_DEBUG
  int mytid = PL_thread_self();
  assert(mytid == atrie->tid);
  int rc = COMPARE_AND_SWAP_INT(&atrie->tid, mytid, 0);
  assert(rc);
#else
  atrie->tid = 0;
#endif
}

static inline void
take_trie(trie *atrie, int tid)
{ assert(atrie->data.worklist != WL_DYNAMIC);
#ifdef O_DEBUG
  int rc = COMPARE_AND_SWAP_INT(&atrie->tid, 0, tid);
  assert(rc);
#else
  atrie->tid = tid;
#endif
}

#define COMPLETE_WORKLIST(__trie, __code) \
	do \
	{ LOCK_SHARED_TABLE(__trie); \
	  if ( __trie->tid ) \
	  { DEBUG(0, assert(__trie->tid == PL_thread_self())); \
	  } else \
	  { take_trie(__trie, PL_thread_self()); \
	  } \
	  __code; \
	  drop_trie(__trie); \
	  cv_broadcast(&GD->tabling.cvar); \
	  UNLOCK_SHARED_TABLE(__trie); \
	} while(0)

static int	wait_for_table_to_complete(trie *atrie);
static int	table_needs_work(trie *atrie);
static void	register_waiting(int tid, trie *atrie);
static void	unregister_waiting(int tid, trie *atrie);
static int	is_deadlock(trie *atrie);

#else /*O_PLMT*/

#define COMPLETE_WORKLIST(__trie, __code) \
	do { __code; } while(0)

#endif /*O_PLMT*/


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
push_component_set(segstack *stack, component_set *cs)
{ tbl_component **bp = baseBuffer(&cs->members, tbl_component*);
  tbl_component **tp = topBuffer(&cs->members, tbl_component*);
  typedef struct tbl_component *Component;

  for(; bp < tp; bp++)
    pushSegStack(stack, *bp, Component);

  discardBuffer(&cs->members);
  PL_free(cs);
}

static void
free_component(tbl_component *c, int flags)
{ GET_LD
  assert(c->magic == COMPONENT_MAGIC);
  c->magic = 0;
  segstack stack;
  typedef struct tbl_component *Component;
  Component buf[100];

  if ( c == LD->tabling.component )
  { LD->tabling.component = c->parent;
    if ( !c->parent && LD->tabling.has_scheduling_component )
      LD->tabling.has_scheduling_component = FALSE;
  }

  initSegStack(&stack, sizeof(Component), sizeof(buf), buf);
  pushSegStack(&stack, c, Component);

  while( popSegStack(&stack, &c, Component) )
  { if ( !(flags&FC_CHILD) && c->parent )
      del_child_component(c->parent, c);
    flags |= FC_CHILD;				/* only for the first */
    if ( c->worklist )
      free_worklist_set(c->worklist, WLFS_FREE_NONE);
    if ( c->delay_worklists )
      free_worklist_set(c->delay_worklists, WLFS_FREE_NONE);
    if ( c->created_worklists )
      free_worklist_set(c->created_worklists, WLFS_FREE_ALL);
    if ( c->children )
      push_component_set(&stack, c->children);
    if ( c->merged )
      push_component_set(&stack, c->merged);

    PL_free(c);
  }

  clearSegStack(&stack);
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
{ component_set *cs;

  if ( (cs=parent->children) )			/* can be merged */
  { tbl_component **bp = baseBuffer(&cs->members, tbl_component*);
    tbl_component **tp = topBuffer(&cs->members, tbl_component*);

    for(; *bp != child && bp < tp; bp++)
      ;
    if ( bp < tp )
    { memmove(bp, bp+1, (tp-bp-1)*sizeof(*bp));
      (void)popBuffer(&cs->members, tbl_component*);
    }
  }
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
	Sdprintf("Merged %zd into %zd, %zd worklists, %zd created\n",
		 pointerToInt(m), pointerToInt(c),
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
	answer ans = {NULL};

	wl->neg_delayed = TRUE;
	DEBUG(MSG_TABLING_NEG,
	      { term_t t = PL_new_term_ref();
		unify_trie_term(wl->table->data.variant, NULL, t PASS_LD);
		Sdprintf("Resuming negative node with delay list %zd: ",
			 pointerToInt(wl));
		PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
	      });

	c = new_answer_cluster(wl, &ans);
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

      if ( (freewl&WLFS_FREE_ALL) || true(wl->table, TRIE_COMPLETE) )
      { free_worklist(wl);
      } else if ( (freewl&WLFS_DISCARD_INCOMPLETE) )
      { trie *atrie = wl->table;

	if ( atrie->data.IDG && atrie->data.IDG->reevaluating )
	{ atrie->data.worklist = NULL;
	  free_worklist(wl);
	  reset_reevaluation(atrie);
	} else
	{ if ( table_is_incomplete(atrie) )
	  { DEBUG(MSG_TABLING_EXCEPTION,
		  print_answer_table(atrie, "Deleting incomplete answer table"));
	    destroy_answer_trie(atrie);
	  }
	}
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
		    unify_trie_term(atrie->data.variant, NULL, tab PASS_LD);
		    unify_trie_term(wl->table->data.variant, NULL, dep PASS_LD);
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
	  unify_trie_term(wl->table->data.variant, NULL, t PASS_LD);
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
	    unify_trie_term(at->data.variant, NULL, t PASS_LD);
	    unify_trie_term(answer, NULL, v PASS_LD);
	    unify_trie_term(wla->table->data.variant, NULL, vt PASS_LD);
	    Sdprintf("Adding propagation to worklist for ");
	    PL_write_term(Serror, t, 999, 0);
	    Sdprintf(" to answer ");
	    PL_write_term(Serror, v, 999, 0);
	    Sdprintf(" of table ");
	    PL_write_term(Serror, vt, 999, PL_WRT_NEWLINE);
	  });
    addBuffer(&wl->delays, answer, trie_node *);
  } else
  { /* see '$tbl_table_complete_all'/3 */
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

		if ( true(at, TRIE_ISMAP) )	/* answer subsumption */
		{ Word rp;
		  trie_node *root;

		  assert(hasFunctor(*p, FUNCTOR_divide2));
		  rp = argTermP(*p, 0);
		  rc = trie_lookup(at, NULL, &root, rp+0, TRUE, NULL PASS_LD);
		  if ( rc == TRUE )
		  { rc = trie_lookup(at, root, &an, rp+1, TRUE, NULL PASS_LD);
		  }
		} else
		{ rc = trie_lookup(at, NULL, &an, p, TRUE, NULL PASS_LD);
		}

		if ( rc == TRUE )
		{ // TBD: can we immediately simplify if this already has a value?
		  DEBUG(MSG_TABLING_DELAY_VAR,
			print_delay("Waiting for instantiated",
				    at->data.variant, an));
		  // TBD: at->data.worklist?
		  add_to_wl_pos_undefined(wl, an);
		} else
		{ return trie_trie_error(rc, at);
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
	  unify_trie_term(scc->leader->data.variant, NULL, t PASS_LD);
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
		unify_trie_term(wl->table->data.variant, NULL, t PASS_LD);
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
		  print_answer("Propagating now unconditional answer", an));
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

  unify_trie_term(from->data.variant, NULL, From PASS_LD);
  unify_trie_term(to->data.variant, NULL, To PASS_LD);
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
	    unify_trie_term(atrie->data.variant, NULL, t PASS_LD);
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

	      unify_trie_term(at->data.variant, NULL, v PASS_LD);
	      unify_trie_term(answer, NULL, a PASS_LD);
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

    if ( true(trie, TRIE_COMPLETE) )
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

    return !!true(trie, TRIE_COMPLETE);
  }

  return FALSE;
}


#ifdef O_DEBUG
static void
print_delay(const char *msg, trie_node *variant, trie_node *answer)
{ GET_LD
  term_t t = PL_new_term_ref();

  unify_trie_term(variant, NULL, t PASS_LD);
  Sdprintf("%s: %s", msg, answer ? "" : "~");
  PL_write_term(Serror, t, 999, answer ? 0 : PL_WRT_NEWLINE);
  if ( answer )
  { PL_put_variable(t);
    unify_trie_term(answer, NULL, t PASS_LD);
    Sdprintf(", answer: ");
    PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
  }
}

static void
print_answer(const char *msg, trie_node *answer)
{ GET_LD
  trie *at = get_trie_from_node(answer);
  term_t t = PL_new_term_ref();

  unify_trie_term(at->data.variant, NULL, t PASS_LD);
  Sdprintf("%s: variant ", msg);
  PL_write_term(Serror, t, 999, 0);
  PL_put_variable(t);
  unify_trie_term(answer, NULL, t PASS_LD);
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

static void
print_answer_table(trie *atrie, const char *msg, ...)
{ GET_LD
  va_list args;
  term_t t = PL_new_term_ref();

  va_start(args, msg);
  unify_trie_term(atrie->data.variant, NULL, t PASS_LD);
  if ( msg )
  { if ( true(atrie, TRIE_ISSHARED) )
      Sdprintf("Thread [%d]: ", PL_thread_self());

    Svdprintf(msg, args);
    Sdprintf(": <trie>(%p) for ", atrie);
  }
  va_end(args);
  PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
}

#endif


		 /*******************************
		 *     THREAD VARIANT TABLE	*
		 *******************************/

static void release_variant_table_node(trie *trie, trie_node *node);

static trie *
variant_table(int shared ARG_LD)
{ trie **tp;
  alloc_pool *pool;

#ifdef O_PLMT
  if ( shared )
  { tp   = &GD->tabling.variant_table;
    if ( !(pool = GD->tabling.node_pool) )
    { if ( (pool = new_alloc_pool("shared_table_space",
				  GD->options.sharedTableSpace)) )
      { if ( !COMPARE_AND_SWAP_PTR(&GD->tabling.node_pool, NULL, pool) )
	{ free_alloc_pool(pool);
	  pool = GD->tabling.node_pool;
	}
      } else
      { return NULL;
      }
    }
  } else
#endif
  { tp   = &LD->tabling.variant_table;
    if ( !(pool = LD->tabling.node_pool) )
    { pool = LD->tabling.node_pool = new_alloc_pool("private_table_space",
						    GD->options.tableSpace);
      if ( !pool )
	return NULL;
    }
  }

  if ( *tp == NULL )
  { trie *t;

    if ( (t = trie_create(pool)) )
    { atom_t symb;

      t->release_node = release_variant_table_node;
      symb = trie_symbol(t);

      if ( COMPARE_AND_SWAP_PTR(tp, NULL, t) )
      { if ( shared )
	{ set(t, TRIE_ISSHARED);
	  acquire_trie(t);			/* bit misuse */
	}
      } else
      { PL_unregister_atom(symb);			/* destroyed by atom-GC */
      }
    }
  }

  return *tp;
}


static void
reset_answer_table(trie *atrie, int cleanup)
{ worklist *wl;
  idg_node *n;

  if ( WL_IS_WORKLIST(wl=atrie->data.worklist) )
  { if ( !isEmptyBuffer(&wl->delays) && !cleanup )
      destroy_depending_worklists(wl);
    if ( wl->undefined )
      destroy_delay_info_worklist(wl);
    atrie->data.worklist = NULL;
    clear(atrie, TRIE_ABOLISH_ON_COMPLETE);
    free_worklist(wl);
  } else if ( wl )
  { atrie->data.worklist = NULL;		/* make fresh again */
  }
  clear(atrie, TRIE_COMPLETE);

  if ( (n=atrie->data.IDG) )
  { if ( true(atrie, TRIE_ISSHARED) )
      idg_reset(n);
    else
      idg_destroy(n);
  }

  trie_empty(atrie);
}


static void
release_variant_table_node(trie *variant_table, trie_node *node)
{ (void)variant_table;

  if ( node->value )
  { trie *atrie = symbol_trie(node->value);

    reset_answer_table(atrie, variant_table->magic == TRIE_CMAGIC);
    assert(atrie->data.variant == node);
    atrie->data.variant = NULL;
  }
}


static int
is_variant_trie(trie *trie)
{ return trie->release_node == release_variant_table_node;
}


static void
clear_variant_table(PL_local_data_t *ld)
{ trie *vtrie;

  if ( (vtrie=ld->tabling.variant_table) )
  { vtrie->magic = TRIE_CMAGIC;
    trie_empty(vtrie);
    PL_unregister_atom(vtrie->symbol);
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

      if ( isVar(*ap) )
	*p++ = makeRefG(ap);
      else
	*p++ = *ap;
    }

    if ( PL_is_variable(ret) )
      return _PL_unify_atomic(ret, w);
    else
      return unify_ptrs(valTermRef(ret), &w, ALLOW_RETCODE PASS_LD);
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

(*) We must avoid a race  with   trie_discard_clause().  As  long as the
(atom) clause reference is not garbage collected  the clause is safe. We
first check there is an atom, then push  it as a volatile atom and check
it again. If the atom is still valid it is not protected against atom-gc
by pushVolatileAtom() and will  be  unified   before  anything  else  in
'$variant_table'/5, so it remains valid.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define AT_CREATE		0x0001
#define AT_MODED		0x0002	/* moded tabling: trie has values */
#define AT_SHARED		0x0004	/* find a shared table */
#define AT_PRIVATE		0x0008	/* find a private table */
#define AT_NOCLAIM		0x0010	/* Do not claim ownership */


#define AT_ABSTRACT		0x0020	/* subgoal_abstract(N) tabling */
#define AT_SCOPE_MASK (AT_SHARED|AT_PRIVATE)

static inline size_t
pred_max_table_subgoal_size(const Definition def ARG_LD)
{ size_t limit;

  limit = def->tabling ? def->tabling->subgoal_abstract : (size_t)-1;
  if ( limit == (size_t)-1 )
    limit = LD->tabling.restraint.max_table_subgoal_size;

  return limit;
}


static trie *
get_answer_table(Definition def, term_t t, term_t ret, atom_t *clrefp,
		 int flags ARG_LD)
{ trie *variants;
  trie *atrie;
  trie_node *node;
  int rc;
  Word v;
  tmp_buffer vars;
  mark m;
  int shared;
  size_abstract sa = {.from_depth = 2, .size = (size_t)-1};

#ifdef O_PLMT
  if ( (flags & AT_SCOPE_MASK) )
  { shared = !!(flags&AT_SHARED);
  } else
  { if ( !def )					/* we should avoid these */
    { Procedure proc;

      if ( get_procedure(t, &proc, 0, GP_RESOLVE) )
      { def = proc->definition;
      } else
      { assert(0);
	return NULL;
      }
    }
    shared = !!true(def, P_TSHARED);
  }
#else
  shared = FALSE;
#endif

  if ( def )			/* otherwise we don't need it anyway */
    sa.size = pred_max_table_subgoal_size(def PASS_LD);
  variants = variant_table(shared PASS_LD);
  initBuffer(&vars);

retry:
  Mark(m);
  v = valTermRef(t);
  rc = trie_lookup_abstract(variants, NULL, &node, v, (flags&AT_CREATE),
			    &sa, &vars PASS_LD);

  if ( rc > 0 )
  { if ( rc == TRIE_ABSTRACTED )
    { atom_t action = LD->tabling.restraint.max_table_subgoal_size_action;

      DEBUG(MSG_TABLING_RESTRAINT,
	    Sdprintf("Trapped by subgoal size restraint\n"));
      if ( action == ATOM_abstract && !(flags&AT_ABSTRACT) )
	action = ATOM_error;

      if ( action != ATOM_abstract )
      { if ( tbl_pred_tripwire(def, action, ATOM_max_table_subgoal_size) )
	{ sa.size = (size_t)-1;
	  emptyBuffer(&vars, (size_t)-1);
	  goto retry;
	} else
	{ discardBuffer(&vars);
	  return NULL;
	}
      }
    }

    if ( node->value )
    { atrie = symbol_trie(node->value);
    } else if ( (flags&AT_CREATE) )
    { atom_t symb;
#ifdef O_PLMT
      alloc_pool *pool = (shared ? GD->tabling.node_pool
				 : LD->tabling.node_pool);
#else
      alloc_pool *pool = LD->tabling.node_pool;
#endif

      if ( !(atrie = trie_create(pool)) )
	return NULL;
      set(atrie, (flags&AT_MODED) ? TRIE_ISMAP : TRIE_ISSET);
      atrie->release_node = release_answer_node;
      atrie->data.variant = node;
      symb = trie_symbol(atrie);

#ifdef O_PLMT
      if ( shared )
      { set(atrie, TRIE_ISSHARED);
	if ( COMPARE_AND_SWAP_WORD(&node->value, 0, symb) )
	{ set(node, TN_PRIMARY);
	  ATOMIC_INC(&variants->value_count);
	} else
	{ PL_unregister_atom(symb);
	  trie_destroy(atrie);
	  atrie = symbol_trie(node->value);
	}
      } else
#endif
      { set(node, TN_PRIMARY);
	node->value = symb;
	ATOMIC_INC(&variants->value_count);
      }
    } else
    { discardBuffer(&vars);
      return NULL;
    }

#ifdef O_PLMT
    if ( !claim_answer_table(atrie, clrefp, flags PASS_LD) )
    { discardBuffer(&vars);
      return NULL;
    }
#endif

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
	    emptyBuffer(&vars, (size_t)-1);
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
		 *   CALL SUBSUPTION INDEXING	*
		 *******************************/

/* TBD: Share with pl-index.c */

static word
indexOfWord(word w ARG_LD)
{ for(;;)
  { switch(tag(w))
    { case TAG_VAR:
      case TAG_ATTVAR:
	return 0;
      case TAG_ATOM:
	break;				/* atom_t */
      case TAG_INTEGER:
	if ( storage(w) == STG_INLINE )
	  break;
      /*FALLTHROUGH*/
      case TAG_STRING:
      case TAG_FLOAT:
      { Word p = addressIndirect(w);
	size_t n = wsizeofInd(*p);
	word k;

	k = MurmurHashAligned2(p+1, n*sizeof(*p), MURMUR_SEED);
	k &= ~((word)STG_GLOBAL);	/* avoid confusion with functor_t */
	if ( !k ) k = 1;		/* avoid no-key */
	return k;
      }
      case TAG_COMPOUND:
	w = *valPtr(w);			/* functor_t */
	break;
      case TAG_REFERENCE:
	w = *unRef(w);
	continue;
    }

    return w;
  }
}


static sindex_key *
suspension_keys(term_t instance ARG_LD)
{ Word p = valTermRef(instance);

  deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);
    size_t i, arity = arityFunctor(f->definition);
    sindex_key keys[SINDEX_MAX];
    sindex_key *k = keys;

    if ( arity > SINDEX_MAX )
      arity = SINDEX_MAX;

    for(i=0; i<arity; i++)
    { unsigned int ki = indexOfWord(f->arguments[i] PASS_LD);

      if ( ki )
      { k->argn = i+1;
	k->key  = ki;
	if ( ++k >= &keys[SINDEX_MAX-1] )
	  break;
      }
    }

    if ( k > keys )
    { k->argn = 0;
      k->key  = 0;
      k++;

      size_t bytes = (char*)k - (char*)keys;
      sindex_key *gk = malloc(bytes);

      if ( gk )
	memcpy(gk, keys, bytes);
      return gk;
    }
  }

  return NULL;
}


static int
suspension_matches_index(const suspension *susp, const sindex_key *skeys)
{ if ( likely(susp->keys!=NULL) )
  { sindex_key *k;

    for(k=susp->keys; k->argn; k++)
    { const sindex_key *sk = &skeys[k->argn];

      if ( unlikely(k->key != sk->key) && likely(!!sk->key) )
	return FALSE;
    }
  }

  return TRUE;
}


static int
suspension_matches(term_t answer, const suspension *susp ARG_LD)
{ fid_t fid;
  term_t tmp;

  if ( (fid=PL_open_foreign_frame()) &&
       (tmp = PL_new_term_ref()) &&
       PL_recorded(susp->instance, tmp) )
  { int ok;

    DEBUG(MSG_TABLING_CALL_SUBSUMPTION,
	  Sdprintf("Skeleton: ");
	  PL_write_term(Serror, tmp, 1200, 0);
	  Sdprintf(", instance: ");
	  PL_write_term(Serror, answer, 1200, PL_WRT_NEWLINE));

    ok = PL_unify(tmp, answer);
    PL_discard_foreign_frame(fid);

    return ok ? TRUE : PL_exception(0) ? -1 : FALSE;
  } else
    return -1;
}

		 /*******************************
		 *  ANSWER/SUSPENSION CLUSTERS	*
		 *******************************/

static cluster *
new_answer_cluster(worklist *wl, answer *ans)
{ cluster *c;

  if ( (c=wl->free_clusters) )
  { wl->free_clusters = c->next;
    c->type = CLUSTER_ANSWERS;
  } else
  { c = PL_malloc(sizeof(*c));
    c->type = CLUSTER_ANSWERS;
    initBuffer(&c->members);
  }
  addBuffer(&c->members, *ans, answer);

  return c;
}

static void
free_answer_cluster(cluster *c)
{ discardBuffer(&c->members);
  PL_free(c);
}

static void
add_to_answer_cluster(cluster *c, answer *ans)
{ addBuffer(&c->members, *ans, answer);
}

static void
merge_answer_clusters(cluster *to, cluster *from)
{ typedef answer* Answer;

  addMultipleBuffer(&to->members,
		    baseBuffer(&from->members, answer),
		    entriesBuffer(&from->members, answer),
		    Answer);
}

static answer *
get_answer_from_cluster(cluster *c, size_t index)
{ return &fetchBuffer(&c->members, index, answer);
}

static size_t
prune_answer_cluster(cluster *c)
{ answer *base = baseBuffer(&c->members, answer);
  answer *top  = topBuffer(&c->members, answer);
  answer *out  = base;

  for( ; base < top; base++)
  { trie_node *n = base->node;
    if ( n->value )
      *out++ = *base;
  }

  c->members.top = (char*)out;

  return top-out;
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

static int
new_suspension(suspension *sp, term_t term, int is_tnot,
	       term_t instance ARG_LD)
{ if ( !(sp->term=PL_record(term)) )
    return FALSE;

  if ( unlikely(instance) )
  { if ( !(sp->instance=PL_record(instance)) )
    { PL_erase(sp->term);
      return FALSE;
    }
    sp->keys = suspension_keys(instance PASS_LD);
  } else
  { sp->instance = 0;
    sp->keys = NULL;
  }

  sp->term = TNOT(sp->term, is_tnot);

  return TRUE;
}


static cluster *
new_suspension_cluster(worklist *wl, term_t first, int is_tnot,
		       term_t instance ARG_LD)
{ cluster *c;
  suspension s;

  if ( !new_suspension(&s, first, is_tnot, instance PASS_LD) )
    return NULL;

  if ( (c=wl->free_clusters) )
  { wl->free_clusters = c->next;
    c->type = CLUSTER_SUSPENSIONS;
  } else
  { c = PL_malloc(sizeof(*c));
    c->type = CLUSTER_SUSPENSIONS;
    initBuffer(&c->members);
  }
  addBuffer(&c->members, s, suspension);

  return c;
}

static void
free_suspension_cluster(cluster *c)
{ suspension *base = baseBuffer(&c->members, suspension);
  size_t entries = entriesBuffer(&c->members, suspension);
  size_t i;

  for(i=0; i<entries; i++)
  { suspension *s = &base[i];

    PL_erase(UNTNOT(s->term));
    if ( s->instance )
      PL_erase(s->instance);
    if ( s->keys )
      free(s->keys);
  }

  discardBuffer(&c->members);
  PL_free(c);
}

static int
add_to_suspension_cluster(cluster *c, term_t sterm, int is_tnot,
			  term_t instance ARG_LD)
{ suspension s;

  if ( !new_suspension(&s, sterm, is_tnot, instance PASS_LD) )
    return FALSE;
  addBuffer(&c->members, s, suspension);

  return TRUE;
}

static void
merge_suspension_cluster(cluster *to, cluster *from, int do_free)
{ typedef suspension* Suspension;

  addMultipleBuffer(&to->members,
		    baseBuffer(&from->members, suspension*),
		    entriesBuffer(&from->members, suspension*),
		    Suspension);
  if ( do_free )
  { discardBuffer(&from->members);
    PL_free(from);
  }
}


static suspension *
get_suspension_from_cluster(cluster *c, size_t index)
{ DEBUG(CHK_SECURE, assert(index < entriesBuffer(&c->members, suspension)));
  return &fetchBuffer(&c->members, index, suspension);
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
{ return entriesBuffer(&c->members, answer);
}

static int
scp_size(cluster *c)
{ return entriesBuffer(&c->members, suspension);
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
  initBuffer(&wl->delays);
  initBuffer(&wl->pos_undefined);
  trie->data.worklist = wl;

  return wl;
}


static void
free_worklist(worklist *wl)
{ cluster *c, *next;
  trie *atrie;

  assert(wl->magic == WORKLIST_MAGIC);
  wl->magic = 0;

  if ( (atrie=wl->table) && atrie->data.worklist )
  { if ( atrie->data.worklist == wl )
      atrie->data.worklist = NULL;
    else
      Sdprintf("Oops, worklist trie doesn't point back at me!\n");
  }

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

  COMPLETE_WORKLIST(wl->table, set(wl->table, TRIE_COMPLETE));
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


static size_t
prune_answers_worklist(worklist *wl)
{ cluster *c;
  size_t gained = 0;

  for(c=wl->head; c; c=c->next)
  { if ( c->type == CLUSTER_ANSWERS )
      gained += prune_answer_cluster(c);
  }

  return gained;
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
wkl_add_answer(worklist *wl, trie_node *an ARG_LD)
{ potentially_add_to_global_worklist(wl PASS_LD);
  answer ans = {an};

  if ( !answer_is_conditional(an) )
    wl->has_answers = TRUE;

  if ( wl->head && wl->head->type == CLUSTER_ANSWERS )
  { add_to_answer_cluster(wl->head, &ans);
  } else
  { cluster *c = new_answer_cluster(wl, &ans);
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
wkl_add_suspension(worklist *wl, term_t suspension, int is_tnot,
		   term_t inst ARG_LD)
{ potentially_add_to_global_worklist(wl PASS_LD);
  if ( wl->tail && wl->tail->type == CLUSTER_SUSPENSIONS )
  { if ( !add_to_suspension_cluster(wl->tail, suspension, is_tnot, inst PASS_LD) )
      return FALSE;
  } else
  { cluster *c = new_suspension_cluster(wl, suspension, is_tnot, inst PASS_LD);
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
unify_fresh(term_t t, trie *atrie, Definition def, int create ARG_LD)
{ if ( create )
  { tbl_component *scc = tbl_create_subcomponent(atrie PASS_LD);
    worklist *wl = tbl_add_worklist(atrie, scc);

    if ( wl )
    { wl->predicate = def;
      return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_fresh2,
			         PL_POINTER, scc,
			         PL_POINTER, wl);
    } else
    { return FALSE;
    }
  } else
  { return PL_unify_atom(t, ATOM_fresh);
  }
}


static int
unify_complete_or_invalid(term_t t, trie *atrie,
			  Definition def, int create ARG_LD)
{ idg_node *n;

  if ( (n=atrie->data.IDG) )
  { if ( n->falsecount > 0 )
      return PL_unify_atom(t, ATOM_invalid);
    if ( n->reevaluating )
      return unify_fresh(t, atrie, def, create PASS_LD);
  }

  return PL_unify_atom(t, ATOM_complete);
}


/** unify_table_status(term_t t, trie *trie, Definition def, int create ARG_LD)
 *
 * @param `t` is unified with the status of `trie`.  Possible values
 * are:
 *
 *   - A worklist (if the trie is incomplete)
 *   - `complete`
 *   - `dynamic` (for pseudo answer tries representing an incremental
 *     dynamic predicate)
 *   - `fresh` (if `create` is `FALSE`)
 *   - `fresh(SCC, WL)` (if `create` is `TRUE`)
 *
 * @param `create` If `TRUE`, we are going to use this worklist for
 * filling the trie.  This is used by '$tbl_variant_table'/5 and
 * friends.
 */

static int
unify_table_status(term_t t, trie *trie, Definition def, int create ARG_LD)
{ if ( true(trie, TRIE_COMPLETE) )
  { return unify_complete_or_invalid(t, trie, def, create PASS_LD);
  } else
  { worklist *wl = trie->data.worklist;

    if ( WL_IS_WORKLIST(wl) )
    { if ( create && wl->component != LD->tabling.component )
      { DEBUG(MSG_TABLING_WORK,
	      Sdprintf("Merging into %p (current = %p)\n",
		       wl->component, LD->tabling.component));
	merge_component(wl->component);
	LD->tabling.component = wl->component;
      }

      return PL_unify_pointer(t, wl);
    }

    if ( wl == WL_DYNAMIC )
      return PL_unify_atom(t, ATOM_dynamic);

    assert(!wl || wl == WL_GROUND);
    return unify_fresh(t, trie, def, create PASS_LD);
  }
}


static int
table_is_incomplete(trie *trie)
{ return ( WL_IS_WORKLIST(trie->data.worklist) &&
	   false(trie, TRIE_COMPLETE) );
}


static int
unify_skeleton(trie *atrie, term_t wrapper, term_t skeleton ARG_LD)
{ if ( !wrapper )
    wrapper = PL_new_term_ref();

  if ( atrie->data.variant && wrapper &&
       unify_trie_term(atrie->data.variant, NULL, wrapper PASS_LD) )
  { worklist *wl = atrie->data.worklist;
    Definition def = WL_IS_WORKLIST(wl) ? wl->predicate : NULL;
    int flags = true(atrie, TRIE_ISSHARED) ? AT_SHARED : AT_PRIVATE;
    return ( get_answer_table(def, wrapper, skeleton,
			      NULL, flags|AT_NOCLAIM PASS_LD) != NULL);
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


static worklist *
tbl_add_worklist(trie *atrie, tbl_component *scc)
{ worklist *wl;

  if ( !WL_IS_WORKLIST(wl=atrie->data.worklist) )
    wl = new_worklist(atrie);

  wl->component = scc;
  add_global_worklist(wl);
  add_newly_created_worklist(wl);
  clear(atrie, TRIE_COMPLETE);

  return wl;
}


static int
destroy_answer_trie(trie *atrie)
{ if ( atrie->data.variant)
  { trie *vtrie = get_trie_from_node(atrie->data.variant);

    if ( is_variant_trie(vtrie) )
    { DEBUG(MSG_TABLING_VTRIE_DEPENDENCIES,
	    print_answer_table(atrie, "Delete answer trie for"));

      if ( true(atrie, TRIE_ISSHARED) )
      { COMPLETE_WORKLIST(atrie,		/* lock might be overkill */
			  reset_answer_table(atrie, FALSE));
      } else
	trie_delete(vtrie, atrie->data.variant, TRUE);

      return TRUE;
    }
  }

  return FALSE;
}


static int
delayed_destroy_table(trie *atrie)
{ if ( table_is_incomplete(atrie) )
  { set(atrie, TRIE_ABOLISH_ON_COMPLETE);
    DEBUG(MSG_TABLING_ABOLISH,
	  print_answer_table(atrie, "Scheduling for delayed abolish"));
    return TRUE;
  }

  return FALSE;
}


/** '$tbl_destroy_table'(+Trie)
 *
 * Destroy a single trie table.
 */

static
PRED_IMPL("$tbl_destroy_table", 1, tbl_destroy_table, 0)
{ trie *atrie;

  if ( get_trie(A1, &atrie) )
  { if ( atrie->data.worklist == WL_DYNAMIC )
      return TRUE;		/* quickly ignore dynamic pseudo tables */

    if ( atrie->data.variant)
    { trie *vtrie = get_trie_from_node(atrie->data.variant);

      if ( is_variant_trie(vtrie) )
      {
#ifdef O_PLMT
        int mytid = PL_thread_self();

	if ( true(atrie, TRIE_ISSHARED) )
	{ LOCK_SHARED_TABLE(atrie);
	  if ( !atrie->tid )			/* no owner */
	  { take_trie(atrie, mytid);
	    assert(!table_is_incomplete(atrie));
	    reset_answer_table(atrie, FALSE);
	    drop_trie(atrie);
	  } else if ( atrie->tid == mytid )	/* I am the owner */
	  { if ( !delayed_destroy_table(atrie) )
	    { reset_answer_table(atrie, FALSE);
	      drop_trie(atrie);
	      cv_broadcast(&GD->tabling.cvar);
	    }
	  } else
	  { set(atrie, TRIE_ABOLISH_ON_COMPLETE);
	    DEBUG(MSG_TABLING_ABOLISH,
		  print_answer_table(atrie, "Scheduling for delayed abolish"));
	  }
	  UNLOCK_SHARED_TABLE(atrie);
	} else
#endif
	{ if ( !delayed_destroy_table(atrie) )
	    trie_delete(vtrie, atrie->data.variant, TRUE);
	}

	return TRUE;
      }
    }

    return PL_type_error("table", A1);
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
  { if ( scc->status == SCC_ACTIVE )
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
  }

  return FALSE;
}

/** '$tbl_wkl_add_answer'(+Worklist, +Answer, +Delays, -Complete) is semidet.
 *
 * Add an answer to the worklist's trie  and the worklist answer cluster
 * using trie_insert_new/3. Fails if a  variant   of  Term is already in
 * Worklist.
 *
 * @arg Answer is either a ret/N (normal tabling) or a (ret/N)/ModeArgs
 * term (answer subsumption).
 */

static inline size_t
pred_max_table_answer_size(const Definition def ARG_LD)
{ size_t limit;

  limit = def->tabling ? def->tabling->answer_abstract : (size_t)-1;
  if ( limit == (size_t)-1 )
    limit = LD->tabling.restraint.max_table_answer_size;

  return limit;
}


static
PRED_IMPL("$tbl_wkl_add_answer", 4, tbl_wkl_add_answer, 0)
{ PRED_LD
  worklist *wl;

  if ( get_worklist(A1, &wl PASS_LD) )
  { Word kp;
    trie_node *node;
    atom_t action;
    size_abstract sa = {.from_depth = 2};
    int rc;

#ifdef O_PLMT
    DEBUG(0, assert(false(wl->table, TRIE_ISSHARED) || wl->table->tid));
#endif

    kp = valTermRef(A2);
    if ( true(wl->table, TRIE_ISMAP) )
      return wkl_mode_add_answer(wl, A2, A3 PASS_LD);

    sa.size = pred_max_table_answer_size(wl->predicate PASS_LD);
    rc = trie_lookup_abstract(wl->table, NULL, &node, kp,
			      TRUE, &sa, NULL PASS_LD);
    if ( rc > 0 )				/* ok or abstracted */
    { idg_node *idg;

      if ( rc == TRIE_ABSTRACTED )
      { atom_t action = LD->tabling.restraint.max_table_answer_size_action;

	DEBUG(MSG_TABLING_RESTRAINT,
	      print_answer_table(wl->table, "Max answer size exceeded"));

	if ( action == ATOM_bounded_rationality )
	{ if ( !add_radial_restraint() )
	    return FALSE;
	} else if ( action == ATOM_fail ||
		    !tbl_wl_tripwire(wl, action, ATOM_max_table_answer_size) )
	{ trie_delete(wl->table, node, TRUE);
	  return FALSE;
	}
      }

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
      } else if ( (action=tripwire_answers_for_subgoal(wl PASS_LD)) )
      { DEBUG(MSG_TABLING_RESTRAINT,
	      print_answer_table(wl->table, "Answer count exceeded"));
	if ( action == ATOM_bounded_rationality )
	{ term_t gen;

	  trie_delete(wl->table, node, TRUE);

	  if ( !(gen = PL_new_term_ref()) ||
	       !generalise_answer_substitution(A2, gen PASS_LD) ||
	       !add_answer_count_restraint() )
	    return FALSE;

	  kp = valTermRef(gen);
	  rc = trie_lookup(wl->table, NULL, &node, kp, TRUE, NULL PASS_LD);
	  if ( rc == TRUE )
	  { if ( !PL_unify_atom(A4, ATOM_cut) )
	      return FALSE;
	    set_trie_value_word(wl->table, node, ATOM_trienode);
	    if ( update_delay_list(wl, node, A2, A3 PASS_LD) == UDL_FALSE )
	      return FALSE;
	    return TRUE;
	  } else
	  { return trie_error(rc, gen);
	  }
	} else
	{ if ( tbl_wl_tripwire(wl, action, ATOM_max_answers_for_subgoal) )
	  { goto add_anyway;
	  } else
	  { trie_delete(wl->table, node, TRUE);
	    return FALSE;
	  }
	}
      } else
      { add_anyway:
	set_trie_value_word(wl->table, node, ATOM_trienode);
	if ( (idg=wl->table->data.IDG) && idg->reevaluating )
	{ set(node, TN_IDG_ADDED);
	  idg->new_answer = TRUE;
	}

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

/** wkl_mode_add_answer(worklist *wl, term_t answer, term_t delays ARG_LD)
 *
 * Add an answer Args for moded arguments to the worklist's trie and the
 * worklist answer cluster using  trie_insert_new/3   and  mode directed
 * tabling.
 *
 * @param answer is a term Return/ModedArgs
 */

#define AS_NEW_DEFINED 0x1
#define AS_OLD_DEFINED 0x2

typedef struct sa_context
{ worklist  *wl;
  trie_node *root;
  term_t     skel;
  term_t     argv;			/* Arguments for '$tabling':update/8 */
  term_t     delays;
  int	     flags;			/* AS_*_DEFINED */
  int	     garbage;			/* # not pruned dummy nodes */
} sa_context;

#define TRIE_MAP_FALSE ((void*)1)	/* error while mapping */
#define TRIE_MAP_DONE  ((void*)2)	/* found existing answer subsuming new */
#define TRIE_MAP_TRUE  NULL		/* mapped all nodes */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
add_subsuming_answer() adds a new aggregate value   as  a secondary node
below `root`. If `old` is  present,  the   new  value  replaces  the one
pointer to by `old`. This poses two problems.

  - We must remove `old` from its answer cluster.  We have little clue
    about which answer cluster holds the `old` answer though.  Adding
    data in the node to help finding the cluster is not that useful
    as clusters can be merged and the answer array may be relocated.

  - We must prune `old` from the trie. If we do not, we waste a lot
    of memory and update_subsuming_answers(), using map_trie_node()
    becomes slow, resulting in quadratic complexity.

These issues are currently resolved  by calling prune_answers_worklist()
and prune_trie() if there are more than   10 deleted answers. The number
10 should depend on

  - The ratio (deleted answers/real answers) in the worklist's answer
    clusters as the prune time is proportional to the total number
    of deleted plus real answers in these clusters.
  - The ration (prunable trie nodes/total trie nodes) in the answer
    trie as this refers wasted memory and map_trie_node() walking over
    deleted nodes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
add_subsuming_answer(worklist *wl, trie_node *root, term_t skel,
		     trie_node *old, term_t margs, term_t delays ARG_LD)
{ trie_node *node;
  Word vp;
  int rc;

  vp = valTermRef(margs);
  rc = trie_lookup(wl->table, root, &node, vp, TRUE, NULL PASS_LD);
  if ( rc == TRUE )
  { if ( false(node, TN_SECONDARY) )
    { node->value = ATOM_trienode;
      set(node, TN_SECONDARY);

      if ( old )
	trie_delete(wl->table, old, FALSE);

      if ( update_delay_list(wl, node, skel, delays PASS_LD) == UDL_FALSE )
	return FALSE;

      wkl_add_answer(wl, node PASS_LD);
      return TRUE;
    } else if ( answer_is_conditional(node) )
    { update_delay_list(wl, node, skel, delays PASS_LD);
    }

    return FALSE;			/* no change */
  } else
  { term_t trie;

    return ( (trie = PL_new_term_ref()) &&
	     _PL_unify_atomic(trie, wl->table->symbol) &&
	     trie_error(rc, trie) );
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Calls

    '$tabling':update(+Flags, +Head, +Module, +Old, +New, -Agg, -Action)
		      av+0,    av+1,  av+2,    av+3, av+4, av+5, av+6
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void *
update_subsuming_answer(trie_node *node, void *ptr)
{ sa_context *ctx = ptr;

  if ( true(node, TN_SECONDARY) )
  { GET_LD
    static predicate_t PRED_update7 = 0;
    term_t av = ctx->argv;
    term_t agg = av+5;
    term_t action = av+6;
    atom_t conditional = answer_is_conditional(node) ? 0
						     : AS_OLD_DEFINED;

    if ( !PRED_update7 )
	  PRED_update7 = PL_predicate("update", 7, "$tabling");

    if ( tbl_put_moded_args(av+3, node PASS_LD) &&
	 PL_put_integer(av+0, ctx->flags|conditional) &&
	 PL_put_variable(agg) &&
	 PL_put_variable(action) &&
	 PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, PRED_update7, av) )
    { trie_node *del;
      atom_t action;

      DEBUG(MSG_TABLING_MODED,
	    { Sdprintf("Updated answer: ");
	      PL_write_term(Serror, av+3, 1200, 0);
	      Sdprintf(" to ");
	      PL_write_term(Serror, agg, 1200, PL_WRT_NEWLINE);
	    });

      if ( !PL_get_atom_ex(av+6, &action) )
	return TRIE_MAP_FALSE;

      if ( action == ATOM_done )
	return TRIE_MAP_DONE;
      else if ( action == ATOM_keep )
	del = NULL;
      else
	del = node;

      if ( add_subsuming_answer(ctx->wl, ctx->root, ctx->skel, del, agg,
				ctx->delays PASS_LD) )
	return TRIE_MAP_TRUE;

      return TRIE_MAP_FALSE;
    } else
    { if ( PL_exception(0) )
	return TRIE_MAP_FALSE;
      DEBUG(MSG_TABLING_MODED, Sdprintf("No change!\n"));
      return TRIE_MAP_TRUE;
    }
  } else
  { if ( !node->children.any )
      ctx->garbage++;

    return TRIE_MAP_TRUE;
  }
}


static int
update_subsuming_answers(worklist *wl, trie_node *root, term_t skel,
			 term_t margs, term_t delays ARG_LD)
{ Word ldlp;
  Word gdlp;

  sa_context ctx = { .wl      = wl,
		     .root    = root,
		     .skel    = skel,
		     .delays  = delays,
		     .flags   = 0,
		     .garbage = 0
		   };

  if ( !( (ctx.argv = PL_new_term_refs(7)) &&
	  PL_put_functor(ctx.argv+1, wl->predicate->functor->functor) &&
	  PL_put_atom(ctx.argv+2,    wl->predicate->module->name) &&
	  PL_put_term(ctx.argv+4,    margs) ) )
    return FALSE;

  deRef2(valTermRef(LD->tabling.delay_list), gdlp);
  gdlp = argTermP(*gdlp, 0);
  deRef(gdlp);
  deRef2(valTermRef(delays), ldlp);

  if ( isNil(*ldlp) && isNil(*gdlp) )
    ctx.flags = AS_NEW_DEFINED;

  if ( map_trie_node(root, update_subsuming_answer, &ctx) == TRIE_MAP_FALSE )
    return FALSE;

  if ( ctx.garbage > 10 )
  { size_t gained = prune_answers_worklist(wl);
    DEBUG(MSG_TABLING_MODED,
	  Sdprintf("Pruned %zd answers\n", gained));
    (void)gained;
    prune_trie(wl->table, root, NULL, NULL);
  }

  return TRUE;
}


static int
wkl_mode_add_answer(worklist *wl, term_t answer, term_t delays ARG_LD)
{ Word kp;
  trie_node *root;
  int rc;
  term_t av = PL_new_term_refs(2);
  term_t skel  = av+0;
  term_t margs = av+1;

  DEBUG(MSG_TABLING_MODED,
	{ print_answer_table(wl->table, "");
	});

  kp = valTermRef(answer);
  deRef(kp);
  if ( hasFunctor(*kp, FUNCTOR_divide2) )
  { kp = argTermP(*kp, 0);
    *valTermRef(skel)  = linkVal(kp);
    *valTermRef(margs) = linkVal(kp+1);
  } else
  { return PL_domain_error("moded_answer", answer);
  }

  rc = trie_lookup(wl->table, NULL, &root, kp, TRUE, NULL PASS_LD);
  if ( rc == TRUE )
  { if ( true(root, TN_PRIMARY) )
    { return update_subsuming_answers(wl, root, skel, margs, delays PASS_LD);
    } else
    { DEBUG(MSG_TABLING_MODED,
	    { Sdprintf("First answer: ");
	      PL_write_term(Serror, margs, 1200, PL_WRT_NEWLINE);
	    });

      set_trie_value_word(wl->table, root, ATOM_trienode);
      return add_subsuming_answer(wl, root, skel, NULL, margs, delays PASS_LD);
    }
  } else
  { return trie_error(rc, skel);
  }
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
    return wkl_add_suspension(wl, A2, is_tnot, 0 PASS_LD);

  return FALSE;
}

/** '$tbl_wkl_add_suspension'(+Worklist, +Instance, +Suspension) is det.
 *
 * Add a suspension to the worklist for	call subsumtive tabling.  Only
 * answers that unify with Instance must be passed down to Suspension.
 */

static
PRED_IMPL("$tbl_wkl_add_suspension", 3, tbl_wkl_add_suspension, 0)
{ PRED_LD
  worklist *wl;
  int is_tnot;

  if ( tnot_get_worklist(A1, &wl, &is_tnot) )
    return wkl_add_suspension(wl, A3, is_tnot, A2 PASS_LD);

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
 *   ```
 *   member(Answer, RIAC),
 *   member(Suspension, LastSuspensionCluster)
 *   ```
 *
 * If the carthesian product is exhausted it tries to re-start using the
 * possible new RIAC and SCP.  During its execution, worklist->executing
 * is TRUE to avoid the worklist to   become part of the global worklist
 * again.
 *
 * This replaces table_get_work/3 from the pure Prolog implementation.
 */

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
  { term_t srcskel = PL_new_term_ref();
    Word dp = valTermRef(dependency);
    Functor f;

    deRef(dp);
    if ( unlikely(!isTerm(*dp)) )
      return FALSE;
    f = valueTerm(*dp);

    unify_arg_term(srcskel, &f->arguments[0] PASS_LD); /* SrcSkeleton */
    unify_arg_term(a0+1,    &f->arguments[1] PASS_LD); /* Continuation */
    unify_arg_term(a0+2,    &f->arguments[2] PASS_LD); /* TargetSkeleton */
    unify_arg_term(a0+3,    &f->arguments[3] PASS_LD); /* TargetWL */
    unify_arg_term(a0+4,    &f->arguments[4] PASS_LD); /* Delays */

    if ( !PL_unify(srcskel, a0+0) )
      return FALSE;
    if ( !idg_set_current_wl(a0+3 PASS_LD) )
      return FALSE;

    if ( unlikely(!answer) )			    /* negative delay */
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


/* Unify `term` with the Prolog term represented by `node`.  Note that
 * the node can be a secondary value in the case of answer subsumption,
 * in which case a term Ret/ModeArgs is created.
 */

static int
tbl_unify_answer(trie_node *node, term_t term ARG_LD)
{ if ( node )
  { if ( unlikely(true(node, TN_SECONDARY)) )
    { term_t av = PL_new_term_refs(2);

      return ( unify_trie_term(node, &node, av+1 PASS_LD) &&
	       unify_trie_term(node, NULL,  av+0 PASS_LD) &&
	       PL_cons_functor_v(av+0, FUNCTOR_divide2, av) &&
	       PL_unify_output(term, av+0) );
    } else
    { return unify_trie_term(node, NULL, term PASS_LD);
    }
  }

  return TRUE;				/* for negative dummy solutions */
}


static int
tbl_put_moded_args(term_t t, trie_node *node ARG_LD)
{ if ( node )
  { if ( unlikely(true(node, TN_SECONDARY)) )
      return unify_trie_term(node, NULL, t PASS_LD);
    else
      return put_trie_value(t, node PASS_LD); /* TBD: Can become ATOM_trienode */
  } else				/* negative dummy solution */
  { *valTermRef(t) = ATOM_trienode;
    return TRUE;
  }
}


static int
advance_wkl_state(wkl_step_state *state)
{ next:

  if ( --state->suspensions.here < state->suspensions.base )
  { state->suspensions.here = state->suspensions.top;
    if ( --state->acp_index == 0 )
    { cluster *acp, *scp;

      /* Merge adjacent suspension clusters */
      if ( (scp=state->scp)->prev && scp->prev->type == CLUSTER_SUSPENSIONS )
      { scp->prev->next = scp->next;
	scp->next->prev = scp->prev;
	merge_suspension_cluster(scp->prev, scp, FALSE);
	seekBuffer(&scp->members, 0, record_t);
	scp->next = state->list->free_clusters;
	state->list->free_clusters = scp;
      }

      /* Merge adjacent answer clusters */
      if ( (acp=state->acp)->next && acp->next->type == CLUSTER_ANSWERS )
      { acp->prev->next = acp->next;
	acp->next->prev = acp->prev;
	merge_answer_clusters(acp->next, acp);
	seekBuffer(&acp->members, 0, answer);
	acp->next = state->list->free_clusters;
	state->list->free_clusters = acp;
      }

      /* If more work, re-initialize */
      if ( (acp=state->list->riac) && (scp=acp->next) )
      { DEBUG(MSG_TABLING_WORK,
	      print_worklist("Next step: ", state->list));
	assert(acp->type == CLUSTER_ANSWERS);
	assert(scp->type == CLUSTER_SUSPENSIONS);
	wkl_swap_clusters(state->list, acp, scp);
	state->acp       = acp;
	state->scp       = scp;
	state->acp_index = acp_size(acp);

	if ( state->acp_index > 0 && scp_size(scp) > 0 )
	{ state->suspensions.base = get_suspension_from_cluster(scp, 0);
	  state->suspensions.top  = get_suspension_from_cluster(scp, scp_size(scp)-1);
	  state->suspensions.here = state->suspensions.top;
	  goto next_answer;
	} else
	{ goto next;
	}
      }

      return FALSE;
    } else
    { next_answer:
      state->answer = get_answer_from_cluster(state->acp, state->acp_index-1);
      state->keys_inited = 0;
    }
  }

  return TRUE;
}


/**
 * '$tbl_wkl_work'(+WorkList,
 *		   -Answer,
 *		   -Continuation, -TargetSkeleton, -TargetWorklist,
 *		   -Delays)
 */

static
PRED_IMPL("$tbl_wkl_work", 6, tbl_wkl_work, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  wkl_step_state *state;
  trie_node *can = NULL;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { worklist *wl;

      if ( get_worklist(A1, &wl PASS_LD) )
      { cluster *acp, *scp;

	if ( (acp=wl->riac) && (scp=acp->next) )
	{ int sz_acp = acp_size(acp);
	  int sz_scp = scp_size(scp);

	  wkl_swap_clusters(wl, acp, scp);

	  if ( sz_acp > 0 && sz_scp > 0 )
	  { DEBUG(MSG_TABLING_WORK,
		  print_worklist("First step: ", wl));
	    state = allocForeignState(sizeof(*state));
	    state->list	            = wl;
	    state->acp	            = acp;
	    state->scp		    = scp;
	    state->acp_index        = sz_acp;
	    state->answer           = get_answer_from_cluster(acp, sz_acp-1);
	    state->suspensions.base = get_suspension_from_cluster(scp, 0);
	    state->suspensions.top  = get_suspension_from_cluster(scp, sz_scp-1);
	    state->suspensions.here = state->suspensions.top;
	    state->keys_inited      = 0;
	    wl->executing	    = TRUE;

	    break;
	  }
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

  Mark(fli_context->mark);

  do
  { const suspension *sp;
    term_t susp;
    trie_node *an = state->answer->node;

    /* Ignore (1) removed answer due to simplification
     *        (2) dummy restart for delayed negation,
     *        (3) conditional answer for tnot
     */

    if ( (an != NULL && an->value == 0) )	/* removed answer */
    { state->suspensions.here = state->suspensions.base;/* skip all suspensions */
      continue;
    }

    sp = state->suspensions.here;

    if ( (an == NULL && !IS_TNOT(sp->term)) ||
	 (an != NULL && IS_TNOT(sp->term) && answer_is_conditional(an)) )
      continue;

    /* We got an answer we want to pass the suspension cluster.
     * Unify A2 with it and get the first suspension.
     */

    if ( can != an )				/* reuse the answer */
    { if ( can )
	Undo(fli_context->mark);
      if ( !tbl_unify_answer(an, A2 PASS_LD) )
	break;					/* resource error */
      can = an;
    }

    /* WFS: need to add a positive node to the delay list if `an`
     * is conditional.  The positive node contains the variant
     * we continue and `an`, but is _independant_ from the
     * condition on `an`.
     */

    /* Call subsumption: filter out suspensions for which the
     * answer does not unify with the answer skeleton for the
     * subsumed table.  Note that this implies that we often
     * hold the same answer (an) against multiple suspensions
     * and therefore we avoid unifying A2 with `an` multiple
     * times.  This block may (1) just be traversed without
     * side effects, (2) `break` on resource errors or
     * (3) `continue`, calling advance_wkl_state() to skip
     * if this suspension is not applicable to this answer
     * instance.
     */

    if ( sp->instance && an )
    { int rc;

      if ( !state->keys_inited )
      { Word p = valTermRef(A2);
	Functor f;
	size_t arity, i;

	deRef(p);
	assert(isTerm(*p));
	f = valueTerm(*p);
	arity = arityFunctor(f->definition);

	if ( arity > SINDEX_MAX )
	  arity = SINDEX_MAX;

	for(i=0; i<arity; i++)
	  state->keys[i].key = indexOfWord(f->arguments[i] PASS_LD);

	state->keys_inited = TRUE;
      }

      const sindex_key *skeys = state->keys;
      skeys--;					/* key args are one based */

      for(;;)
      { if ( unlikely((suspension_matches_index(sp, skeys))) )
	{ rc = suspension_matches(A2, sp PASS_LD);

	  if ( rc == TRUE )  goto match;
	  if ( rc != FALSE ) goto out_fail;
	}

	if ( likely((sp = --state->suspensions.here) >= state->suspensions.base) )
	{ if ( unlikely(!sp->instance) )
	    goto match;
	} else
	{ goto next;
	}
      }
    }

    /* Found real work to do.  If we get here we can only fail due to
     * resource errors.  Normally we succeed with or without a choice
     * point.
     */

  match:
    if ( !( (susp=PL_new_term_ref()) &&
	    PL_recorded(UNTNOT(sp->term), susp) &&
				      /* unifies A4..A8 */
	    unify_dependency(A2, susp, state->list, an PASS_LD)
	  ) )
      break;			/* resource errors */

    DEBUG(MSG_TABLING_WORK,
	  { Sdprintf("Work: %d %d\n\t",
		     (int)state->acp_index,
		     (int)(state->suspensions.here - state->suspensions.base));
	    PL_write_term(Serror, A2, 1200, PL_WRT_NEWLINE);
	    Sdprintf("\t");
	    PL_write_term(Serror, susp, 1200, PL_WRT_NEWLINE);
	  });

    if ( advance_wkl_state(state) )
    { ForeignRedoPtr(state);
    } else
    { state->list->executing = FALSE;
      freeForeignState(state, sizeof(*state));
      return TRUE;
    }
  next:
    ;
  } while ( advance_wkl_state(state) );

out_fail:
  state->list->executing = FALSE;
  freeForeignState(state, sizeof(*state));
  return FALSE;
}


/** '$tbl_variant_table'(+Closure, +Variant, -Trie, -Status, -Skeleton) is det.
 *
 * Retrieve the table for Variant. Status is one of
 *
 *   - `fresh` if the table is new
 *   - `complete` if the table is completed
 *   - A worklist pointer
 */

static int
tbl_variant_table(term_t closure, term_t variant, term_t Trie,
		  term_t abstract, term_t status, term_t ret, int flags ARG_LD)
{ trie *atrie;
  Definition def = NULL;
  atom_t clref = 0;

  get_closure_predicate(closure, &def);

  if ( (atrie=get_answer_table(def, variant, ret, &clref, flags PASS_LD)) )
  { if ( !idg_init_variant(atrie, def, variant PASS_LD)  ||
	 !idg_add_edge(atrie, NULL PASS_LD) )
      return FALSE;

    if ( clref )
    { TRIE_STAT_INC(atrie, gen_call);
      return ( _PL_unify_atomic(Trie, clref) &&
	       _PL_unify_atomic(status, ATOM_complete) );
    } else
    { return ( _PL_unify_atomic(Trie, atrie->symbol) &&
	       unify_table_status(status, atrie, def, TRUE PASS_LD) );
    }
  }

  return FALSE;
}

static
PRED_IMPL("$tbl_variant_table", 5, tbl_variant_table, 0)
{ PRED_LD

  return tbl_variant_table(A1, A2, A3, 0, A4, A5, AT_CREATE PASS_LD);
}


/** '$tbl_abstract_table'(+Closure, :Wrapper, -Trie,
			  -Abstract, -Status, -Skeleton)

Abstract is one of `0` or a generalization of Wrapper
*/

static
PRED_IMPL("$tbl_abstract_table", 6, tbl_abstract_table, 0)
{ PRED_LD

  return tbl_variant_table(A1, A2, A3, A4, A5, A6, AT_CREATE|AT_ABSTRACT PASS_LD);
}


static
PRED_IMPL("$tbl_moded_variant_table", 5, tbl_moded_variant_table, 0)
{ PRED_LD

  return tbl_variant_table(A1, A2, A3, 0, A4, A5, AT_CREATE|AT_MODED PASS_LD);
}


static
PRED_IMPL("$tbl_existing_variant_table", 5, tbl_existing_variant_table, 0)
{ PRED_LD
  trie *trie;
  Definition def = NULL;
  atom_t clref = 0;

  get_closure_predicate(A1, &def);

  if ( (trie=get_answer_table(def, A2, A5, &clref, FALSE PASS_LD)) )
  { return ( _PL_unify_atomic(A3, trie->symbol) &&
	     unify_table_status(A4, trie, def, TRUE PASS_LD) );
  }

  return FALSE;
}


static
PRED_IMPL("$tbl_local_variant_table", 1, tbl_local_variant_table, 0)
{ PRED_LD
  trie *trie = LD->tabling.variant_table;

  if ( trie )
    return _PL_unify_atomic(A1, trie->symbol);

  return FALSE;
}


static
PRED_IMPL("$tbl_global_variant_table", 1, tbl_global_variant_table, 0)
{
#ifdef O_PLMT
  PRED_LD
  trie *trie = GD->tabling.variant_table;

  if ( trie )
    return _PL_unify_atomic(A1, trie->symbol);
#endif

  return FALSE;
}


/** '$tbl_variant_table'(?Table) is nondet.
 *
 *  True when Table is a variant table. If there is both a local and
 *  global table it first returns the local one and then the global one.
 *  '$tbl_local_variant_table'/1 and '$tbl_global_variant_table'/1 fetch
 *  a specific table.
 *
 *  This predicate is in C to make it easy to be deterministic in case
 *  there is no global table.
 */

#ifdef O_PLMT
static
PRED_IMPL("$tbl_variant_table", 1, tbl_variant_table, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  trie *trie;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
      if ( (trie=LD->tabling.variant_table) )
      { if ( _PL_unify_atomic(A1, trie->symbol) )
	{ if ( GD->tabling.variant_table )
	    ForeignRedoInt(1);
	  else
	    return TRUE;
	}
      }
    /*FALLTHROUGH*/
    case FRG_REDO:
      if ( (trie=GD->tabling.variant_table) )
	return _PL_unify_atomic(A1, trie->symbol);
      return FALSE;
    case FRG_CUTTED:
      return TRUE;
        default:
      assert(0);
      return FALSE;
  }
}
#endif

/** '$tbl_table_status'(+Trie, -Status)
 *
 * Get the status of an answer table.
 */

static
PRED_IMPL("$tbl_table_status", 2, tbl_table_status, 0)
{ PRED_LD
  trie *trie;

  return ( get_trie(A1, &trie) &&
	   unify_table_status(A2, trie, NULL, FALSE PASS_LD)
	 );
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
	   unify_table_status(A2, trie, NULL, FALSE PASS_LD) &&
	   unify_skeleton(trie, wv, A4 PASS_LD) &&
	   PL_unify(A3, wv)
	 );
}


/** '$tbl_table_pi'(+ATrie, -PredicateIndicator)
 *
 * Get the predicate indicator that is associated with an answer trie.
 * This is used for e.g., abstracting the IDG to predicates.
 */

static
PRED_IMPL("$tbl_table_pi", 2, tbl_table_pi, 0)
{ PRED_LD
  trie *atrie;

  if ( get_trie(A1, &atrie) )
  { term_t av = PL_new_term_refs(3);
    term_t wrapper = av+0;
    term_t module  = av+1;
    term_t head	   = av+2;

    if ( unify_trie_term(atrie->data.variant, NULL, wrapper PASS_LD) )
    { atom_t name;
      size_t arity;

      assert(PL_is_functor(wrapper, FUNCTOR_colon2));
      _PL_get_arg(1, wrapper, module);
      _PL_get_arg(2, wrapper, head);
      if ( PL_get_name_arity(head, &name, &arity) )
      { return PL_unify_term(A2, PL_FUNCTOR, FUNCTOR_colon2,
				   PL_TERM, module,
				   PL_FUNCTOR, FUNCTOR_divide2,
				     PL_ATOM, name,
				     PL_INTPTR, (intptr_t)arity);
      }
    }
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Finish re-evaluation of a component by  running reeval_complete() on all
re-evaluated answer tables. Typically either none   of the answer tables
is a re-evaluated one or all are.  In   some  cases though, there may be
some non-reevaluated tables, either because the   table was abolished or
because a new table gets involved into the SCC.

We could maintain a flag on the   SCC telling re-evaluated tables may be
involved. Not sure it is worth while.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
wls_reeval_complete(worklist **wls, size_t ntables)
{ size_t i;

  for(i=0; i<ntables; i++)
  { worklist *wl = wls[i];
    trie *atrie = wl->table;
    idg_node *n;

    if ( (n=atrie->data.IDG) && n->reevaluating )
      reeval_complete(atrie);
  }
}


static int
unify_leader_clause(tbl_component *scc, term_t cl ARG_LD)
{ trie *atrie = scc->leader;
  Procedure proc = (true(atrie, TRIE_ISMAP)
			     ? GD->procedures.trie_gen_compiled3
			     : GD->procedures.trie_gen_compiled2);
  atom_t clref = compile_trie(proc->definition, atrie PASS_LD);

  TRIE_STAT_INC(atrie, gen_call);
  return _PL_unify_atomic(cl, clref);
}


/** '$tbl_table_complete_all'(+SCC, -Status, -Clause)
 *
 * Complete and reset all newly created tables.
 *
 * (*) currently we keep worklists that play a role on a network
 * of undefined answers.  That is needed for lazy answer completion
 * (see O_AC_EAGER).  If we do not do so, we still must keep track
 * of dependencies when abolishing tries.
 */

static
PRED_IMPL("$tbl_table_complete_all", 3, tbl_table_complete_all, 0)
{ PRED_LD
  tbl_component *c;

  if ( !get_scc(A1, &c) )
    return FALSE;

  if ( c->status == SCC_ACTIVE )
  { worklist **wls;
    size_t ntables = worklist_set_to_array(c->created_worklists, &wls);
    size_t i;
    int rc;

    wls_reeval_complete(wls, ntables);
    rc = unify_leader_clause(c, A3 PASS_LD);

    for(i=0; i<ntables; i++)
    { worklist *wl = wls[i];
      trie *atrie = wl->table;

      DEBUG(MSG_TABLING_WORK,
	    { term_t t = PL_new_term_ref();
	      unify_trie_term(atrie->data.variant, NULL, t PASS_LD);
	      Sdprintf("Setting wl %zd in scc %zd to COMPLETE.  Variant: ",
		       pointerToInt(wl), pointerToInt(c));
	      PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
	    });

      if ( true(atrie, TRIE_ABOLISH_ON_COMPLETE) )
      { atrie->data.worklist = NULL;
	destroy_answer_trie(atrie);
	free_worklist(wl);
      } else if ( !wl->undefined && isEmptyBuffer(&wl->delays) ) /* see (*) */
      { free_worklist(wl);
	COMPLETE_WORKLIST(atrie,
			  { atrie->data.worklist = NULL;
			    set(atrie, TRIE_COMPLETE);
			  });
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

    if ( !rc )
      return FALSE;
  }

  return unify_component_status(A2, c PASS_LD);
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
  { if ( c->status != SCC_MERGED )
    { tbl_component *parent = c->parent;

      if ( c->created_worklists )
	reset_newly_created_worklists(c, WLFS_DISCARD_INCOMPLETE);
      reset_global_worklist(c);

      LD->tabling.component = parent;
      free_component(c, FC_DESTROY);

      if ( !parent )
	LD->tabling.has_scheduling_component = FALSE;
    }
  }

  return TRUE;
}


static tbl_component *
tbl_create_subcomponent(trie *leader ARG_LD)
{ if ( !LD->tabling.has_scheduling_component )
  { LD->tabling.has_scheduling_component = TRUE;
    if ( !LD->tabling.component || LD->tabling.in_answer_completion )
      LD->tabling.component = new_component();
    else
      LD->tabling.component->status = SCC_ACTIVE;
    LD->tabling.component->leader = leader;
  } else
  { tbl_component *c = new_component();
    tbl_component *p;

    c->leader = leader;
    c->parent = (p=LD->tabling.component);
    LD->tabling.component = c;
    add_child_component(p, c);
  }

  return LD->tabling.component;
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

/** '$tbl_abolish_local_tables' is det.
 *
 * Clear the thread table data.  Fails silently if tabling is in
 * progress.
 */

static
PRED_IMPL("$tbl_abolish_local_tables", 0, tbl_abolish_local_tables, 0)
{ PRED_LD

  if ( !LD->tabling.has_scheduling_component )
  { clearThreadTablingData(LD);
    return TRUE;
  }

  return FALSE;
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

/** '$tbl_is_trienode'(@X) is det.
 *
 * True if X is the reserved trie node.
 */

static
PRED_IMPL("$tbl_is_trienode", 1, tbl_is_trienode, 0)
{ PRED_LD
  Word p = valTermRef(A1);

  deRef(p);
  return *p == ATOM_trienode;
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

    for(; ap < top; ap++)
    { trie_node *an = *ap;

      if ( !PL_unify_list(tail, head, tail) ||
	   !tbl_unify_answer(an, head PASS_LD)  )
	return FALSE;
    }
    return PL_unify_nil(tail);
  } else
  { suspension *sp  = baseBuffer(&c->members, suspension);
    suspension *top = topBuffer(&c->members, suspension);
    term_t tmp = PL_new_term_ref();

    assert(c->type == CLUSTER_SUSPENSIONS);

    for(; sp < top; sp++)
    { if ( !PL_unify_list(tail, head, tail) ||
	   !PL_recorded(UNTNOT(sp->term), tmp) ||
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


static
PRED_IMPL("$tbl_wkl_table", 2, tbl_wkl_table, 0)
{ PRED_LD
  worklist *wl;

  return ( get_worklist(A1, &wl PASS_LD) &&
	   _PL_unify_atomic(A2, wl->table->symbol) );
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
  term_t av = PL_new_term_refs(6);
  int count = 0;
  term_t gshare = 0;
  term_t gskel  = 0;
  size_t arity  = 0;
  term_t tmp    = av+4;
  term_t ret    = av+5;

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
    if ( top->answer )				/* positive delay */
    { term_t ans  = av+1;
      term_t uans;

      PL_put_variable(c1);
      PL_put_variable(ans);
      PL_put_variable(ret);

      if ( !unify_trie_term(top->variant->data.variant, NULL, c1 PASS_LD) )
	return FALSE;
      if ( !get_answer_table(NULL, c1, ret, NULL, FALSE PASS_LD) )
      { Sdprintf("OOPS! could not find variant table\n");
	return FALSE;
      }

      if ( true(top->answer, TN_SECONDARY) ) /* Ret/ModeArgs */
      { if ( !tbl_unify_answer(top->answer, ans PASS_LD) ||
	     !PL_get_arg(1, ans, tmp) ||
	     !PL_unify(tmp, ret) ||
	     !PL_get_arg(2, ans, tmp) ||
	     !PL_cons_functor(c1, FUNCTOR_divide2, c1, tmp) )
	  return FALSE;
	uans = ans;
      } else
      { if ( !unify_trie_term(top->answer, NULL, ret PASS_LD) )
	  return FALSE;
	uans = ret;
      }

      if ( !is_ground_trie_node(top->answer) )
      { assert(gshare);

	_PL_get_arg(arity, gshare, gskel);
	if ( !PL_unify(gskel, uans) )
	{ DEBUG(0, Sdprintf("Oops, skeleton %zd does not unify\n", arity));
	  pl_writeln(gskel);
	  pl_writeln(ans);
	  return FALSE;
	}
	arity--;
      }
    } else					/* negative delay */
    { PL_put_variable(c1);
      if ( !unify_trie_term(top->variant->data.variant, NULL, c1 PASS_LD) ||
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
  return trie_gen(A1, 0, A2, 0, A3, unify_delay_info, &ctx, PL__ctx);
}

/** '$tbl_answer_c'(+Trie, +Skeleton, -ModedArgs, -Condition) is nondet.
 */

static
PRED_IMPL("$tbl_answer_c", 4, tbl_answer_c, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  trie *trie;

  if ( get_trie(A1, &trie) )
  { Word kp;
    trie_node *root;
    int rc;

    kp = valTermRef(A2);
    rc = trie_lookup(trie, NULL, &root, kp, FALSE, NULL PASS_LD);
    if ( rc == TRUE )
    { answer_ctx ctx;

      ctx.skel = A2;
      return trie_gen_raw(trie, root, A3, 0, A4, unify_delay_info, &ctx, PL__ctx);
    } else
    { rc = trie_error(rc, A1);
    }

    return rc;
  }

  return FALSE;
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
{ return trie_gen(A1, 0, A2, 0, A3, unify_delay_info_dl, NULL, PL__ctx);
}

/** '$tbl_answer_dl'(+ATrie, +Skeleton, -Sumbsuming, -DL) is nondet.
*/

static
PRED_IMPL("$tbl_answer_dl", 4, tbl_answer_dl, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  trie *trie;

  if ( get_trie(A1, &trie) )
  { Word kp;
    trie_node *root;
    int rc;

    kp = valTermRef(A2);
    rc = trie_lookup(trie, NULL, &root, kp, FALSE, NULL PASS_LD);
    if ( rc == TRUE )
    { return trie_gen_raw(trie, root, A3, 0, A4, unify_delay_info_dl,
			  NULL, PL__ctx);
    } else
    { rc = trie_error(rc, A1);
    }

    return rc;
  }

  return FALSE;
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

  return trie_gen(A1, 0, A2, 0, A2, answer_update_delay_list, &ctx, PL__ctx);
}


/** '$tbl_answer_update_dl'(+Trie, +Skeleton, -ModeArgs) is nondet.
*/

static
PRED_IMPL("$tbl_answer_update_dl", 3, tbl_answer_update_dl,
	  PL_FA_NONDETERMINISTIC)
{ PRED_LD
  trie *trie;

  if ( get_trie(A1, &trie) )
  { Word kp;
    trie_node *root;
    int rc;

    kp = valTermRef(A2);
    rc = trie_lookup(trie, NULL, &root, kp, FALSE, NULL PASS_LD);
    if ( rc == TRUE )
    { update_dl_ctx ctx;

      ctx.atrie = A1;
      return trie_gen_raw(trie, root, A3, 0, A3,
			  answer_update_delay_list, &ctx, PL__ctx);
    } else
    { rc = trie_error(rc, A1);
    }

    return rc;
  }

  return FALSE;
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
    clearHTable(table);
}

static void
idg_clean_dependent(idg_node *node)
{ Table table;

  if ( (table=node->dependent) )
    clearHTable(table);
}


static void
idg_reset(idg_node *node)
{ idg_clean_affected(node);
  idg_clean_dependent(node);
  node->answer_count = 0;
  node->new_answer   = FALSE;
  node->reevaluating = FALSE;
  node->falsecount   = 0;
}

static void
idg_destroy(idg_node *node)
{ Table table;

  if ( (table=node->affected) )
  { node->affected = NULL;
    destroyHTable(table);
  }
  if ( (table=node->dependent) )
  { node->dependent = NULL;
    destroyHTable(table);
  }

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


/**
 * Throw error(idg_dependency_error(Parent, Child), _)
 */

static int
idg_dependency_error(idg_node *parent, idg_node *child ARG_LD)
{ term_t av;

  return ( (av=PL_new_term_refs(3)) &&
	   unify_trie_term(parent->atrie->data.variant, NULL, av+0 PASS_LD) &&
	   unify_trie_term(child->atrie->data.variant,  NULL, av+1 PASS_LD) &&
	   PL_unify_term(av+2,
			 PL_FUNCTOR, FUNCTOR_error2,
		           PL_FUNCTOR_CHARS, "idg_dependency_error", 2,
			     PL_TERM, av+0,
		             PL_TERM, av+1,
		           PL_VARIABLE) &&
	   PL_raise_exception(av+2));
}


static int
idg_dependency_error_dyncall(idg_node *parent, term_t call ARG_LD)
{ term_t av;

  return ( (av=PL_new_term_refs(2)) &&
	   unify_trie_term(parent->atrie->data.variant, NULL, av+0 PASS_LD) &&
	   PL_unify_term(av+1,
			 PL_FUNCTOR, FUNCTOR_error2,
		           PL_FUNCTOR_CHARS, "idg_dependency_error", 2,
			     PL_TERM, av+0,
		             PL_TERM, call,
		           PL_VARIABLE) &&
	   PL_raise_exception(av+1));
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a bi-directional link between parent and   child node. We use the
hash tables as sets only, but we use   the _other side_ as entry _value_
to recover the full link and allow   ->free_symbol()  to delete the back
pointer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
idg_add_child(idg_node *parent, idg_node *child ARG_LD)
{ volatile Table t;

  if ( true(parent->atrie, TRIE_ISSHARED) &&
       false(child->atrie, TRIE_ISSHARED) )
    return idg_dependency_error(parent, child PASS_LD);

  if ( !(t=child->affected) )
  { t = newHTable(4);
    t->free_symbol = idg_free_affected;
    if ( !COMPARE_AND_SWAP_PTR(&child->affected, NULL, t) )
      destroyHTable(t);
  }
  addHTable(t, parent, child);

  if ( !(t=parent->dependent) )
  { t = newHTable(4);
    t->free_symbol = idg_free_dependent;
    if ( !COMPARE_AND_SWAP_PTR(&parent->dependent, NULL, t) )
      destroyHTable(t);
  }
  addHTable(t, child, parent);

  return TRUE;
}


static int
idg_init_variant(trie *atrie, Definition def, term_t variant ARG_LD)
{ if ( !atrie->data.IDG )
  { if ( unlikely(!def) )
    { Procedure proc;

      if ( get_procedure(variant, &proc, 0, GP_RESOLVE|GP_EXISTENCE_ERROR) )
	def = proc->definition;
      else
	return FALSE;
    }

    if ( true(def, P_INCREMENTAL) )
    { idg_node *n = idg_new(atrie);

      if ( !COMPARE_AND_SWAP_PTR(&atrie->data.IDG, NULL, n) )
	idg_destroy(n);
    }
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
idg_current(ARG1_LD)
{ atom_t current = *valTermRef(LD->tabling.idg_current);

  if ( current )
    return symbol_trie(current);

  return NULL;
}


/** Add an edge from the current node to the new child represented
 * by `atrie`.  If `ctrie` is given it is used.  Otherwise idg_current()
 * is used.
 *
 *   - TRUE:  created a dependency
 *   - FALSE: something is wrong
 *   - -1:    there is no current node
 */

static int
idg_add_edge(trie *atrie, trie *ctrie ARG_LD)
{ if ( atrie->data.IDG )
  { if ( !ctrie )
      ctrie = idg_current(PASS_LD1);

    if ( ctrie && ctrie->data.IDG )
    { DEBUG(MSG_TABLING_IDG,
	    { term_t f = PL_new_term_ref();
	      term_t t = PL_new_term_ref();
	      unify_trie_term(ctrie->data.variant, NULL, f PASS_LD);
	      unify_trie_term(atrie->data.variant, NULL, t PASS_LD);
	      Sdprintf("IDG: Edge ");
	      PL_write_term(Serror, f, 999, 0);
	      Sdprintf(" -> ");
	      PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
	    });

      return idg_add_child(ctrie->data.IDG, atrie->data.IDG PASS_LD);
    }
  }

  return -1;
}

/** '$idg_set_current'(-OldCurrent, +ATrie)
 *
 * Set the current to Atrie and return the old current.
 */

static
PRED_IMPL("$idg_set_current", 2, idg_set_current, 0)
{ PRED_LD
  trie *atrie;

  if ( get_trie(A2, &atrie) )
  { atom_t current;

    if ( (current = *valTermRef(LD->tabling.idg_current)) )
    { if ( !PL_unify_atom(A1, current) )
	return FALSE;
    }
    return set_idg_current(atrie PASS_LD);
  }

  return FALSE;
}


/** '$idg_add_dyncall'(+Variant)
 *
 * Called on a call to an incremental dynamic predicate.
 *
 * (*) If we make a call we should reset the `falsecount` to 0 as this
 * may have added a new dependency.  Ideally we should keep track of the
 * edges we have signalled, although we need to lookup the dynamic trie
 * from the assert/removed variant anyway and propagation to node that
 * have already been triggered stop quickly.  Setting the `falsecount`
 * to zero should be considered similar to re-evaluating an incremental
 * tabled predicate when it is called.
 */

int
idg_add_dyncall(Definition def, trie *ctrie, term_t variant ARG_LD)
{ trie *atrie;
  int flags = (AT_CREATE|AT_NOCLAIM);

  if ( true(ctrie, TRIE_ISSHARED) )
  { flags |= AT_SHARED;

    /* a shared table cannot depend on a thread-local predicate */
    /* TBD: Avoid the procedure lookup! */
    if ( !def )
    { Procedure proc;

      if ( get_procedure(variant, &proc, 0, GP_RESOLVE) &&
	   true(proc->definition, P_THREAD_LOCAL) )
      { return idg_dependency_error_dyncall(ctrie->data.IDG, variant PASS_LD);
      }
    } else if ( true(def, P_THREAD_LOCAL) )
    { return idg_dependency_error_dyncall(ctrie->data.IDG, variant PASS_LD);
    }
  } else
  { flags |= AT_PRIVATE;
  }

  if ( (atrie=get_answer_table(NULL, variant, 0, NULL, flags PASS_LD)) )
  { if ( !atrie->data.IDG )
    { idg_node *n;

      assert(!atrie->data.worklist || atrie->data.worklist == WL_GROUND);
      atrie->data.worklist = WL_DYNAMIC;
      n = idg_new(atrie);
      if ( !COMPARE_AND_SWAP_PTR(&atrie->data.IDG, NULL, n) )
	idg_destroy(n);
    }

    idg_add_edge(atrie, ctrie PASS_LD);
    atrie->data.IDG->falsecount = 0;	/* see (*) above */

    return TRUE;
  }

  return FALSE;
}


static
PRED_IMPL("$idg_add_dyncall", 1, idg_add_dyncall, 0)
{ PRED_LD
  trie *ctrie = idg_current(PASS_LD1);

  if ( ctrie && ctrie->data.IDG )
    return idg_add_dyncall(NULL, ctrie, A1 PASS_LD);

  return TRUE;
}


static int
idg_set_current_wl(term_t wlref ARG_LD)
{ worklist *wl;

  if ( get_worklist(wlref, &wl PASS_LD) )
  { trie *atrie = wl->table;

    DEBUG(MSG_TABLING_IDG,
	  { term_t t = PL_new_term_ref();
	    unify_trie_term(atrie->data.variant, NULL, t PASS_LD);
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
	    unify_trie_term(atrie->data.variant, NULL, t PASS_LD);
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) If not-changed propagation re-validates  the   table  someone may be
waiting for it and we must release   ownership  for the table and signal
possible waiters.

It is probably possible we re-validate a   table that is claimed by some
other thread. What should we do in this case?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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
	    print_answer_table(
		n->atrie,
		"IDG: propagate falsecount (re-eval=%d, falsecount=%d)",
		n->reevaluating, n->falsecount));

      if ( n->reevaluating )
	continue;

      if ( changed )
      { if ( table_is_incomplete(n->atrie) )
	  state->incomplete = n->atrie;		/* return? */
	if ( ATOMIC_INC(&n->falsecount) == 1 )
	{ TRIE_STAT_INC(n, invalidated);
	  if ( n->affected )
	    pushSegStack(&state->stack, n, IDGNode);
	}
      } else
      { if ( ATOMIC_DEC(&n->falsecount) == 0 )
	{
#ifdef O_PLMT
	  if ( true(n->atrie, TRIE_ISSHARED) && n->atrie->tid )
	  { if ( n->atrie->tid == PL_thread_self() ) /* See (*) */
	      COMPLETE_WORKLIST(n->atrie, (void)0);
	    else
	      Sdprintf("IDG falsecount propagation re-validated a table "
		       "from another thread\n");
	  }
#endif
	  if ( n->affected )
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
	   unify_trie_term(atrie->data.variant, NULL, v PASS_LD) &&
	   PL_permission_error("update", "variant", v) );
}


static int
idg_changed(trie *atrie)
{ idg_node *n;

  DEBUG(MSG_TABLING_IDG_CHANGED,
	print_answer_table(atrie, "IDG: dynamic change"));

  if ( (n=atrie->data.IDG) && n->falsecount == 0 )
  { trie *incomplete;

    DEBUG(MSG_TABLING_IDG_CHANGED, Sdprintf(" (propagating)\n"));

    if ( table_is_incomplete(atrie) )
      return change_incomplete_error(atrie);
    if ( ATOMIC_INC(&n->falsecount) == 1 )
    { TRIE_STAT_INC(n, invalidated);
      if ( (incomplete=idg_propagate_change(n, TRUE)) )
      { n->falsecount = 0;
	idg_propagate_change(n, FALSE);
	return change_incomplete_error(incomplete);
      }
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

/** '$tbl_reeval_wait'(+Trie, -Status) is det.
 *
 * Get the status for Trie as one of `complete` or `invalid`, but wait
 * if another thread is evaluating the table.
 *
 * @error `deadlock` if claiming the table would cause a deadlock.
 */

static
PRED_IMPL("$tbl_reeval_wait", 2, tbl_reeval_wait, 0)
{ GET_LD
  trie *atrie;

  if ( get_trie(A1, &atrie) )
  { if ( atrie->data.worklist == WL_DYNAMIC )
    { return PL_unify_atom(A2, ATOM_dynamic);
    } else
    { int rc;
#ifdef O_PLMT
      int tid = PL_thread_self();

      if ( atrie->tid == tid )			/* remain owner */
      { rc = unify_table_status(A2, atrie, NULL, FALSE PASS_LD);
      } else
      { if ( !claim_answer_table(atrie, NULL, 0 PASS_LD) )
	  return FALSE;				/* deadlock */
	rc = unify_table_status(A2, atrie, NULL, FALSE PASS_LD);
	COMPLETE_WORKLIST(atrie, (void)0);
      }
#else
      rc = unify_table_status(A2, atrie, NULL, FALSE PASS_LD);
#endif

      return rc;
    }
  }

  return FALSE;
}


/** '$tbl_reeval_prepare'(+Trie, -Variant, -Clause) is det.
 *
 * Prepare Trie for re-evaluation. If Trie is invalid, it is claimed and
 * prepared for re-evaluation and Variant is   unified  with the goal to
 * re-evaluate. If the Trie is (already)   valid,  unify Clause with its
 * answer set.
 *
 * @error `deadlock` if claiming the table would cause a deadlock.
 */

static void *
reeval_prep_node(trie_node *n, void *ctx)
{ trie *atrie = ctx;

  if ( n->value )
  { set(n, TN_IDG_DELETED);
    clear(n, TN_IDG_ADDED);

    if ( answer_is_conditional(n) )
    { destroy_delay_info(atrie, n, TRUE);
      n->data.delayinfo = NULL;
      clear(n, TN_IDG_UNCONDITIONAL);
    } else
    { set(n, TN_IDG_UNCONDITIONAL);
    }
  }

  return NULL;
}


static
PRED_IMPL("$tbl_reeval_prepare", 3, tbl_reeval_prepare, 0)
{ PRED_LD
  trie *atrie;

  if ( get_trie(A1, &atrie) )
  { idg_node *idg = atrie->data.IDG;

#ifdef O_PLMT
    if ( true(atrie, TRIE_ISSHARED) )
    { atom_t cref = 0;

      if ( !claim_answer_table(atrie, &cref, 0 PASS_LD) )
	return FALSE;				/* deadlock */
      if ( cref )
	return PL_unify_atom(A3, cref);
    }
#endif
    if ( idg->falsecount == 0 )			/* someone else re-evaluated it */
      return PL_unify_atom(A3, trie_symbol(atrie));

    if ( !unify_trie_term(atrie->data.variant, NULL, A2 PASS_LD) )
      return FALSE;

    DEBUG(MSG_TABLING_IDG_REEVAL,
	  print_answer_table(atrie, "Preparing re-evaluation of"));

    idg->answer_count = atrie->value_count;
    idg->new_answer = FALSE;
    idg->falsecount = 0;
    idg_clean_dependent(idg);
    if ( !idg->aborted )
      map_trie_node(&atrie->root, reeval_prep_node, atrie);
    idg->reevaluating = TRUE;

    return TRUE;
  }

  return FALSE;
}


/** '$tbl_reeval_abandon'(+ATrie)
 *
 * Release ownership of ATrie if we cannot re-evaluate it
 */

static
PRED_IMPL("$tbl_reeval_abandon", 1, tbl_reeval_abandon, 0)
{ trie *atrie;

  if ( get_trie(A1, &atrie) )
  { DEBUG(MSG_TABLING_SHARED,
	  print_answer_table(atrie, "Abondon re-evaluation"));
    COMPLETE_WORKLIST(atrie, (void)0);

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

  clear(n, TN_IDG_MASK);

  return NULL;
}


static void
reeval_complete(trie *atrie)
{ idg_node *n;

  if ( (n=atrie->data.IDG) && n->reevaluating )
  { map_trie_node(&atrie->root, reeval_complete_node, atrie);

    DEBUG(MSG_TABLING_IDG_REEVAL,
	  print_answer_table(atrie, "Re-evaluation of"));

    if ( n->new_answer == FALSE &&
	 n->answer_count == atrie->value_count )
    { DEBUG(MSG_TABLING_IDG_REEVAL, Sdprintf(": same answers\n"));
      idg_propagate_change(n, FALSE);
    } else
    { DEBUG(MSG_TABLING_IDG_REEVAL,
	    Sdprintf(": modified (new=%d, count %zd -> %zd)\n",
		     n->new_answer, n->answer_count, atrie->value_count));
    }

    TRIE_STAT_INC(n, reevaluated);

    n->reevaluating = FALSE;
    n->aborted = FALSE;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
reset_reevaluation(trie *atrie)

Reset a table that is  being   reevaluated  while an exception happened.
This must ensure that a subsequent call   on  the table will restart the
re-evaluation and forward a _not-changed_ to   the affected nodes if the
table evaluates to the same values.

We set the nodes back to the   state  after the initial preparation. The
flag `aborted` is used by  the  subsequent   prepare  to  keep  the node
states.

TBD: the dependency links  to  dependent   nodes  (and  back)  have been
removed during preparation. This is ok for restoring the state. It poses
problems for propagating the falsecounts from our dependent nodes to our
affected nodes though if there are  modifications to our dependent nodes
while re-evaluation is in progress.  In-progress   here  means  the time
between the node was prepared and completed.  This period can be long if
the re-evaluation has been aborted.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void *
reset_evaluate_node(trie_node *n, void *ctx)
{ trie *atrie = ctx;

  if ( answer_is_conditional(n) )
  { destroy_delay_info(atrie, n, TRUE);
    n->data.delayinfo = NULL;
  }

  if ( true(n, TN_IDG_ADDED) )
  { trie_delete(atrie, n, TRUE);
  } else
  { set(n, TN_IDG_DELETED);
    if ( true(n, TN_IDG_SAVED_UNCONDITIONAL) )
      set(n, TN_IDG_UNCONDITIONAL);
    else
      clear(n, TN_IDG_UNCONDITIONAL);
  }

  return NULL;
}


static void
reset_reevaluation(trie *atrie)
{ idg_node *n = atrie->data.IDG;

  DEBUG(MSG_TABLING_EXCEPTION,
	print_answer_table(atrie, "Abort reevaluation of (%zd answers)",
			   atrie->value_count));

  map_trie_node(&atrie->root, reset_evaluate_node, atrie);
  assert(n->answer_count == atrie->value_count);

  n->new_answer = FALSE;
  n->aborted = TRUE;
  n->falsecount = 1;
  set(atrie, TRIE_COMPLETE);
  COMPLETE_WORKLIST(atrie, n->reevaluating = FALSE);
}


		 /*******************************
		 *	     RESTRAINTS		*
		 *******************************/

int
tbl_is_predicate_attribute(atom_t key)
{ return ( key == ATOM_abstract ||
	   key == ATOM_subgoal_abstract ||
	   key == ATOM_answer_abstract ||
	   key == ATOM_max_answers
	 );
}


static void
clear_table_props(table_props *p)
{ p->abstract         = (size_t)-1;
  p->subgoal_abstract = (size_t)-1;
  p->answer_abstract  = (size_t)-1;
  p->max_answers      = (size_t)-1;
}


void
tbl_reset_tabling_attributes(Definition def)
{ table_props *p;

  if ( (p=def->tabling) )
    clear_table_props(p);
}


int
tbl_get_predicate_attribute(Definition def, atom_t att, size_t *value)
{ table_props *p;

  if ( (p=def->tabling) )
  { size_t v0;

    if ( att == ATOM_abstract )
      v0 = p->abstract;
    else if ( att == ATOM_subgoal_abstract )
      v0 = p->subgoal_abstract;
    else if ( att == ATOM_answer_abstract )
      v0 = p->answer_abstract;
    else if ( att == ATOM_max_answers )
      v0 = p->max_answers;
    else
      return -1;

    if ( v0 != (size_t)-1 )
    { *value = v0;
      return TRUE;
    }
  }

  return FALSE;
}


int
tbl_set_predicate_attribute(Definition def, atom_t att, size_t value)
{ table_props *p;

  if ( !(p=def->tabling) )
  { p = allocHeapOrHalt(sizeof(*p));

    clear_table_props(p);
    if ( !COMPARE_AND_SWAP_PTR(&def->tabling, NULL, p) )
    { p = def->tabling;
      freeHeap(p, sizeof(*p));
    }
  }

  if ( att == ATOM_abstract )
    p->abstract = value;
  else if ( att == ATOM_subgoal_abstract )
    p->subgoal_abstract = value;
  else if ( att == ATOM_answer_abstract )
    p->answer_abstract = value;
  else if ( att == ATOM_max_answers )
    p->max_answers = value;
  else
    return -1;

  return TRUE;
}


int
tbl_is_restraint_flag(atom_t key)
{ return ( key == ATOM_max_table_subgoal_size_action ||
	   key == ATOM_max_table_subgoal_size ||
	   key == ATOM_max_table_answer_size_action ||
	   key == ATOM_max_table_answer_size ||
	   key == ATOM_max_answers_for_subgoal_action ||
	   key == ATOM_max_answers_for_subgoal );
}


static int
unify_restraint(term_t t, size_t val)
{ if ( val == (size_t)-1 )
    return FALSE;
  else
    return PL_unify_uint64(t, val);
}


int
tbl_get_restraint_flag(term_t t, atom_t key ARG_LD)
{ if ( key == ATOM_max_table_subgoal_size_action )
    return PL_unify_atom(t, LD->tabling.restraint.max_table_subgoal_size_action);
  else if ( key == ATOM_max_table_answer_size_action )
    return PL_unify_atom(t, LD->tabling.restraint.max_table_answer_size_action);
  else if ( key == ATOM_max_answers_for_subgoal_action )
    return PL_unify_atom(t, LD->tabling.restraint.max_answers_for_subgoal_action);
  else if ( key == ATOM_max_table_subgoal_size )
    return unify_restraint(t, LD->tabling.restraint.max_table_subgoal_size);
  else if ( key == ATOM_max_table_answer_size )
    return unify_restraint(t, LD->tabling.restraint.max_table_answer_size);
  else if ( key == ATOM_max_answers_for_subgoal )
    return unify_restraint(t, LD->tabling.restraint.max_answers_for_subgoal);
  else
    return -1;
}


static int
set_restraint_action(term_t t, atom_t key, atom_t *valp ARG_LD)
{ atom_t act;

  if ( PL_get_atom_ex(t, &act) )
  { if ( act == ATOM_error || act == ATOM_warning || act == ATOM_suspend )
    { ok:
      *valp = act;
      return TRUE;
    }

    if ( act == ATOM_complete_soundly )	/* XSB compatibility */
      act = ATOM_bounded_rationality;

    if ( key == ATOM_max_table_subgoal_size_action &&
	 ( act == ATOM_abstract ) )
      goto ok;
    if ( key == ATOM_max_table_answer_size_action &&
	 ( act == ATOM_bounded_rationality ||
	   act == ATOM_fail) )
      goto ok;
    if ( key == ATOM_max_answers_for_subgoal_action &&
	 ( act == ATOM_bounded_rationality ) )
      goto ok;

    return PL_domain_error("restraint_action", t);
  }

  return FALSE;
}


static int
set_restraint(term_t t, size_t *valp)
{ GET_LD
  atom_t inf;

  if ( PL_get_atom(t, &inf) && inf == ATOM_infinite )
  { *valp = (size_t)-1;
    return TRUE;
  }
  return PL_get_size_ex(t, valp);
}


int
tbl_set_restraint_flag(term_t t, atom_t key ARG_LD)
{ if ( key == ATOM_max_table_subgoal_size_action )
    return set_restraint_action(
	       t, key,
	       &LD->tabling.restraint.max_table_subgoal_size_action PASS_LD);
  else if ( key == ATOM_max_table_answer_size_action )
    return set_restraint_action(
	       t, key,
	       &LD->tabling.restraint.max_table_answer_size_action PASS_LD);
  else if ( key == ATOM_max_answers_for_subgoal_action )
    return set_restraint_action(
	       t, key,
	       &LD->tabling.restraint.max_answers_for_subgoal_action PASS_LD);
  else if ( key == ATOM_max_table_subgoal_size )
    return set_restraint(t, &LD->tabling.restraint.max_table_subgoal_size);
  else if ( key == ATOM_max_table_answer_size )
    return set_restraint(t, &LD->tabling.restraint.max_table_answer_size);
  else if ( key == ATOM_max_answers_for_subgoal )
    return set_restraint(t, &LD->tabling.restraint.max_answers_for_subgoal);
  else
    return -1;
}


static atom_t
tripwire_answers_for_subgoal(worklist *wl ARG_LD)
{ table_props *ps;
  size_t limit;

  if ( ((ps=wl->predicate->tabling) &&
	(limit=ps->max_answers) != (size_t)-1) )
  { if ( wl->table->value_count >= limit )
      return ATOM_bounded_rationality;
    return NULL_ATOM;
  }

  if ( (limit=LD->tabling.restraint.max_answers_for_subgoal) != (size_t)-1 )
  { if ( wl->table->value_count == limit )
      return LD->tabling.restraint.max_answers_for_subgoal_action;
  }

  return NULL_ATOM;
}


/* Create the most general ret/N term compliant with `spec`.  We need
 * this term when the answer count restraint is exceeded.
 */

static int
generalise_answer_substitution(term_t spec, term_t gen ARG_LD)
{ Word p = valTermRef(spec);

  deRef(p);
  if ( isTerm(*p) )
    return PL_unify_functor(gen, functorTerm(*p));
  if ( *p == ATOM_ret )
    return PL_unify_atom(gen, ATOM_ret);

  return PL_type_error("answer_substitution", spec);
}


/* Add the condition `answer_count_restraint` to the current delay list.
 * We can simply call the predicate as the constraint will be added to
 * the global delay list as a result.
 */


static int
add_answer_count_restraint(void)
{ static predicate_t pred = NULL;

  if ( !pred )
    pred = PL_predicate("answer_count_restraint", 0, "system");

  DEBUG(MSG_TABLING_RESTRAINT,
	Sdprintf("Calling %s\n", procedureName(pred)));

  return PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, pred, 0);
}


static int
add_radial_restraint(void)
{ static predicate_t pred = NULL;

  if ( !pred )
    pred = PL_predicate("radial_restraint", 0, "system");

  DEBUG(MSG_TABLING_RESTRAINT,
	Sdprintf("Calling %s\n", procedureName(pred)));

  return PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, pred, 0);
}


static int
tbl_wl_tripwire(worklist *wl, atom_t action, atom_t wire)
{ GET_LD
  static predicate_t pred = NULL;
  term_t av;

  if ( !pred )
    pred = PL_predicate("tripwire", 3, "$tabling");

  DEBUG(MSG_TABLING_RESTRAINT,
	Sdprintf("Calling %s\n", procedureName(pred)));

  return ( (av = PL_new_term_refs(3)) &&
	   PL_put_atom(av+0, wire) &&
	   PL_put_atom(av+1, action) &&
	   PL_put_atom(av+2, trie_symbol(wl->table)) &&
	   PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, pred, av) );
}


static int
tbl_pred_tripwire(Definition def, atom_t action, atom_t wire)
{ GET_LD
  static predicate_t pred = NULL;
  term_t av;

  if ( !pred )
    pred = PL_predicate("tripwire", 3, "$tabling");

  DEBUG(MSG_TABLING_RESTRAINT,
	Sdprintf("Calling %s\n", procedureName(pred)));

  return ( (av = PL_new_term_refs(3)) &&
	   PL_put_atom(av+0, wire) &&
	   PL_put_atom(av+1, action) &&
	   unify_definition(MODULE_user, av+2, def, 0, GP_QUALIFY) &&
	   PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, pred, av) );
}

		 /*******************************
		 *	    CONCURRENCY		*
		 *******************************/

#if O_PLMT

static int
table_needs_work(trie *atrie)
{ if ( true(atrie, TRIE_COMPLETE) )
  { idg_node *n;

    if ( (n=atrie->data.IDG) )
    { if ( n->falsecount > 0 ||		/* invalid */
	   n->reevaluating )		/* fresh (re-evaluating) */
	return TRUE;
    }

    return FALSE;
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Claim ownership for an answer table if the table is incomplete/invalid:

  - If we alread own the table, fine
  - Else
    - If the table is incomplete, someone else is completing it
      (otherwise we were the owner).  Either throw a `deadlock`
      exception or wait.
    - If the table needs work (fresh, invalid), try to claim it.
    - If the table is complete, return its compiled trie.  As
      we are in a locked region we can do so safely.

Note that this code uses  a   mutex/condition  variable  pair. Currently
there is a single mutex. Future versions could  use an array of these to
reduce contention.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
claim_answer_table(trie *atrie, atom_t *clrefp, int flags ARG_LD)
{ if ( true(atrie, TRIE_ISSHARED) && !(flags&AT_NOCLAIM) )
  { int mytid = PL_thread_self();
    volatile atom_t clref = 0;

    if ( atrie->tid != mytid )
    { LOCK_SHARED_TABLE(atrie);
    retry_shared:
      if ( atrie->tid )
      { register_waiting(mytid, atrie);
	if ( is_deadlock(atrie) )
	{ term_t ex;

	  DEBUG(MSG_TABLING_SHARED,
		print_answer_table(atrie, "DEADLOCK"));
	  unregister_waiting(mytid, atrie);
	  if ( (ex = PL_new_term_ref()) &&
	       PL_put_atom(ex, ATOM_deadlock) )
	    PL_raise_exception(ex);
	  UNLOCK_SHARED_TABLE(atrie);
	  return FALSE;
	}
	TRIE_STAT_INC(atrie, wait);
	if ( !wait_for_table_to_complete(atrie) )
	{ UNLOCK_SHARED_TABLE(atrie);
	  return FALSE;
	}
	unregister_waiting(mytid, atrie);
	if ( !atrie->tid && table_needs_work(atrie) )
	{ DEBUG(MSG_TABLING_SHARED,
		print_answer_table(atrie, "stealing abandonned trie"));
	  take_trie(atrie, mytid);
	} else
	{ goto complete;
	}
      } else if ( table_needs_work(atrie) )
      { DEBUG(MSG_TABLING_SHARED,
	      print_answer_table(atrie, "claiming"));
	take_trie(atrie, mytid);
      } else					/* complete and valid */
      { complete:
	if ( clrefp )
	{ if ( !(clref=atrie->clause) )
	  { Procedure proc = ((flags&AT_MODED)
				      ? GD->procedures.trie_gen_compiled3
				      : GD->procedures.trie_gen_compiled2);

	    clref = compile_trie(proc->definition, atrie PASS_LD);
	  }
	  pushVolatileAtom(clref);		/* see (*) above */
	  if ( clref != atrie->clause )
	    goto retry_shared;
	  *clrefp = clref;
	}
      }
      UNLOCK_SHARED_TABLE(atrie);
    }
  }

  return TRUE;
}


static trie_array *
new_trie_array(void)
{ trie_array *a = allocHeapOrHalt(sizeof(*a));

  memset(a, 0, sizeof(*a));
  a->blocks[0] = a->preallocated - 1;
  a->blocks[1] = a->preallocated - 1;
  a->blocks[2] = a->preallocated - 1;

  return a;
}


static void
register_waiting(int tid, trie *atrie)
{ trie_array *ta;
  size_t idx = MSB(tid);

  if ( !(ta=GD->tabling.waiting) )
  { ta = new_trie_array();
    if ( !COMPARE_AND_SWAP_PTR(&GD->tabling.waiting, NULL, ta) )
    { freeHeap(ta, sizeof(*ta));
      ta = GD->tabling.waiting;
    }
  }

  if ( !ta->blocks[idx] )
  { if ( !ta->blocks[idx] )
    { size_t bs = (size_t)1<<idx;
      trie **newblock;

      if ( !(newblock=PL_malloc_uncollectable(bs*sizeof(trie*))) )
	outOfCore();

      memset(newblock, 0, bs*sizeof(trie*));
      if ( !COMPARE_AND_SWAP_PTR(&ta->blocks[idx], NULL, newblock-bs) )
	PL_free(newblock);
    }
  }

  ta->blocks[idx][tid] = atrie;
}


static void
unregister_waiting(int tid, trie *atrie)
{ size_t idx = MSB(tid);

  GD->tabling.waiting->blocks[idx][tid] = NULL;
}


static trie *
thread_waits_for_trie(int tid)
{ trie_array *ta;

  if ( (ta=GD->tabling.waiting) )
  { size_t idx = MSB(tid);

    if ( ta->blocks[idx] )
      return ta->blocks[idx][tid];
  }

  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
is_deadlock() succeeds if  the  proposed  situation   would  lead  to  a
deadlock.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
stat_deadlock(trie *atrie)
{
#ifdef O_TRIE_STATS
  int mytid = atrie->tid;
  trie *t = NULL;
  int tid = mytid;

  TRIE_STAT_INC(atrie, deadlock);

  for(;;)
  { t = thread_waits_for_trie(tid);
    if ( t )
    { TRIE_STAT_INC(t, deadlock);
      tid = t->tid;
    }

    if ( !t || !tid )
      return;
    if ( tid == mytid )
      return;
  }
#endif
}


static int
is_deadlock(trie *atrie)
{ int mytid = atrie->tid;
  trie *t = NULL;
  int tid = mytid;

  for(;;)
  { t = thread_waits_for_trie(tid);
    if ( t )
      tid = t->tid;

    if ( !t || !tid )
      return FALSE;
    if ( tid == mytid )
    { stat_deadlock(atrie);
      return TRUE;
    }
  }
}


static int
wait_for_table_to_complete(trie *atrie)
{ DEBUG(MSG_TABLING_SHARED,
	print_answer_table(atrie, "waiting for %d to complete", atrie->tid));

  do
  { if ( cv_wait(&GD->tabling.cvar, &GD->tabling.mutex.mutex) == EINTR )
    { if ( PL_handle_signals() < 0 )
      { DEBUG(MSG_TABLING_SHARED,
	      print_answer_table(atrie, "Ready (interrupted"));
	return FALSE;
      }
    }
  } while( atrie->tid != 0 );

  DEBUG(MSG_TABLING_SHARED,
	print_answer_table(atrie,
			   table_needs_work(atrie) ? "Ready (abandonned)"
			                           : "Ready (completed)"));

  return TRUE;
}
#endif /*O_PLMT*/


		 /*******************************
		 *	     UNTABLE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This predicate is called for clauses   from  M:'$tabled'(Head, Mode) are
deleted during reconsult. It is called from the fixup phase of reloading
a file (see pl-srcfile.c) and we may not use any error condition.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
untable_from_clause(Clause cl)
{ if ( cl->codes[0] == encode(H_FUNCTOR) )
  { GET_LD
    functor_t f = (functor_t)cl->codes[1];
    Module m = cl->predicate->module;
    Procedure proc = isCurrentProcedure(f, m);

    if ( proc && !callEventHook(PLEV_UNTABLE, proc) )
      fatalError("Failed to register untable event\n");
  } else
  { Sdprintf("WARNING: untable_from_clause(): unexpected clause\n");
  }
}

		 /*******************************
		 *	       INIT		*
		 *******************************/

void
initTabling(void)
{ GET_LD

#ifdef O_PLMT
  initSimpleMutex(&GD->tabling.mutex, "L_SHARED_TABLING");
  cv_init(&GD->tabling.cvar, NULL);
#endif

  LD->tabling.restraint.max_table_subgoal_size_action  = ATOM_error;
  LD->tabling.restraint.max_table_subgoal_size	       = (size_t)-1;
  LD->tabling.restraint.max_table_answer_size_action   = ATOM_error;
  LD->tabling.restraint.max_table_answer_size	       = (size_t)-1;
  LD->tabling.restraint.max_answers_for_subgoal_action = ATOM_error;
  LD->tabling.restraint.max_answers_for_subgoal	       = (size_t)-1;

  setPrologFlag("max_table_subgoal_size_action",  FT_ATOM,    "error");
  setPrologFlag("max_table_answer_size_action",	  FT_ATOM,    "error");
  setPrologFlag("max_answers_for_subgoal_action", FT_ATOM,    "error");
  setPrologFlag("max_table_subgoal_size",	  FT_INTEGER, -1);
  setPrologFlag("max_table_answer_size",	  FT_INTEGER, -1);
  setPrologFlag("max_answers_for_subgoal",	  FT_INTEGER, -1);
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

#define NDET PL_FA_NONDETERMINISTIC
#define META PL_FA_TRANSPARENT

BeginPredDefs(tabling)
  PRED_DEF("$tbl_pop_worklist",		2, tbl_pop_worklist,	     0)
  PRED_DEF("$tbl_wkl_add_answer",	4, tbl_wkl_add_answer,	     0)
  PRED_DEF("$tbl_wkl_make_follower",    1, tbl_wkl_make_follower,    0)
  PRED_DEF("$tbl_wkl_add_suspension",	2, tbl_wkl_add_suspension,   0)
  PRED_DEF("$tbl_wkl_add_suspension",	3, tbl_wkl_add_suspension,   0)
  PRED_DEF("$tbl_wkl_done",		1, tbl_wkl_done,	     0)
  PRED_DEF("$tbl_wkl_negative",		1, tbl_wkl_negative,	     0)
  PRED_DEF("$tbl_wkl_is_false",		1, tbl_wkl_is_false,	     0)
  PRED_DEF("$tbl_wkl_answer_trie",	2, tbl_wkl_answer_trie,      0)
  PRED_DEF("$tbl_wkl_work",		6, tbl_wkl_work,          NDET)
  PRED_DEF("$tbl_variant_table",	5, tbl_variant_table,	     0)
  PRED_DEF("$tbl_abstract_table",       6, tbl_abstract_table,       0)
  PRED_DEF("$tbl_existing_variant_table", 5, tbl_existing_variant_table, 0)
  PRED_DEF("$tbl_moded_variant_table",	5, tbl_moded_variant_table,  0)
#ifdef O_PLMT
  PRED_DEF("$tbl_variant_table",        1, tbl_variant_table,	  NDET)
#else
  PRED_DEF("$tbl_variant_table",        1, tbl_local_variant_table,  0)
#endif
  PRED_DEF("$tbl_local_variant_table",  1, tbl_local_variant_table,  0)
  PRED_DEF("$tbl_global_variant_table", 1, tbl_global_variant_table, 0)
  PRED_DEF("$tbl_table_status",         2, tbl_table_status,         0)
  PRED_DEF("$tbl_table_status",		4, tbl_table_status,	     0)
  PRED_DEF("$tbl_table_pi",             2, tbl_table_pi,	     0)
  PRED_DEF("$tbl_table_complete_all",	3, tbl_table_complete_all,   0)
  PRED_DEF("$tbl_free_component",       1, tbl_free_component,       0)
  PRED_DEF("$tbl_table_discard_all",    1, tbl_table_discard_all,    0)
  PRED_DEF("$tbl_abolish_local_tables", 0, tbl_abolish_local_tables, 0)
  PRED_DEF("$tbl_destroy_table",        1, tbl_destroy_table,        0)
  PRED_DEF("$tbl_trienode",             1, tbl_trienode,             0)
  PRED_DEF("$tbl_is_trienode",          1, tbl_is_trienode,          0)
  PRED_DEF("$tbl_delay_list",           1, tbl_delay_list,           0)
  PRED_DEF("$tbl_set_delay_list",       1, tbl_set_delay_list,       0)
  PRED_DEF("$tbl_add_global_delays",    2, tbl_add_global_delays,    0)

  PRED_DEF("$tbl_scc",                  1, tbl_scc,                  0)
  PRED_DEF("$tbl_scc_data",             2, tbl_scc_data,             0)
  PRED_DEF("$tbl_worklist_data",        2, tbl_worklist_data,        0)
  PRED_DEF("$tbl_wkl_table",            2, tbl_wkl_table,	     0)
  PRED_DEF("$tbl_answer",               3, tbl_answer,            NDET)
  PRED_DEF("$tbl_answer_c",             4, tbl_answer_c,          NDET)
  PRED_DEF("$tbl_answer_dl",		3, tbl_answer_dl,         NDET)
  PRED_DEF("$tbl_answer_dl",            4, tbl_answer_dl,	  NDET)
  PRED_DEF("$tbl_answer_update_dl",     2, tbl_answer_update_dl,  NDET)
  PRED_DEF("$tbl_answer_update_dl",     3, tbl_answer_update_dl,  NDET)
  PRED_DEF("$tbl_force_truth_value",    3, tbl_force_truth_value,    0)
  PRED_DEF("$tbl_set_answer_completed", 1, tbl_set_answer_completed, 0)
  PRED_DEF("$tbl_is_answer_completed",  1, tbl_is_answer_completed,  0)
  PRED_DEF("$tnot_implementation",      2, tnot_implementation,   META)
  PRED_DEF("$tbl_implementation",       2, tbl_implementation,    META)
  PRED_DEF("$is_answer_trie",           1, is_answer_trie,           0)

  PRED_DEF("$idg_add_dyncall",          1, idg_add_dyncall,          0)
  PRED_DEF("$idg_set_current",          1, idg_set_current,          0)
  PRED_DEF("$idg_set_current",          2, idg_set_current,          0)
  PRED_DEF("$idg_reset_current",        0, idg_reset_current,        0)
  PRED_DEF("$idg_edge",                 3, idg_edge,              NDET)
  PRED_DEF("$idg_changed",              1, idg_changed,              0)
  PRED_DEF("$idg_falsecount",           2, idg_falsecount,           0)
  PRED_DEF("$idg_set_falsecount",       2, idg_set_falsecount,       0)

  PRED_DEF("$tbl_reeval_prepare",       3, tbl_reeval_prepare,	     0)
  PRED_DEF("$tbl_reeval_abandon",       1, tbl_reeval_abandon,       0)
  PRED_DEF("$tbl_reeval_wait",          2, tbl_reeval_wait,          0)
EndPredDefs
