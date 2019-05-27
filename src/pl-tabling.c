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
static cluster *new_answer_cluster(worklist *wl, trie_node *first);
static void	wkl_append_left(worklist *wl, cluster *c);
static int	wkl_add_answer(worklist *wl, trie_node *node ARG_LD);
static int	tbl_put_trie_value(term_t t, trie_node *node ARG_LD);
static void	del_child_component(tbl_component *parent, tbl_component *child);
static void	free_components_set(component_set *cs, int destroy);
#ifdef O_DEBUG
static void	print_worklist(const char *prefix, worklist *wl);
#endif
static int	simplify_component(tbl_component *scc);

#define WL_IS_SPECIAL(wl)  (((intptr_t)(wl)) & 0x1)
#define WL_IS_WORKLIST(wl) ((wl) && !WL_IS_SPECIAL(wl))

#define WL_COMPLETE ((worklist *)0x11)
#define WL_GROUND   ((worklist *)0x21)

#define WLFS_FREE_NONE		0x0000
#define WLFS_KEEP_COMPLETE	0x0001
#define WLFS_FREE_ALL		0x0002


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
  if ( c->created_worklists )
    free_worklist_set(c->created_worklists, WLFS_FREE_ALL);
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
wl_set_component(worklist *wl, tbl_component *c)
{ wl->component = c;
  wl->executing = FALSE;
  if ( !wl->in_global_wl )
    add_global_worklist(wl);
  if ( wl->negative )
  { DEBUG(MSG_TABLING_NEG,
	  Sdprintf("Merging negative literal into SCC %zd\n",
		   pointerToInt(c)));
    if ( c->neg_status == SCC_NEG_NONE )
      c->neg_status = SCC_NEG_DELAY;
    else if ( c->neg_status == SCC_NEG_SIMPLIFY &&
	      !wl->neg_completed )
      c->neg_status = SCC_NEG_DELAY;
  }
}


static void
wls_set_component(worklist_set *wls, tbl_component *c)
{ worklist **base = baseBuffer(&wls->members, worklist*);
  worklist **top  = topBuffer(&wls->members, worklist*);

  for(; base < top; base++)
    wl_set_component(*base, c);
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


#ifdef O_DEBUG
static int
is_in_global_worklist(worklist *wl)
{ tbl_component *c = wl->component;
  worklist_set *wls;

  if ( (wls = c->worklist) )
  { worklist **wlp = baseBuffer(&wls->members, worklist*);
    worklist **elp = topBuffer(&wls->members, worklist*);

    for( ; wlp < elp; wlp++)
    { if ( *wlp == wl )
	return TRUE;
    }
  }

  return FALSE;
}
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Normal completion is done. There  may   be  worklists that are suspended
using negation_suspend/3. We wake  these  up   by  adding  a  new answer
cluster with a NULL node.

For WFS we need to pass the condition we represent in the delay list.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static worklist *
negative_worklist(tbl_component *scc ARG_LD)
{ worklist **wlp = baseBuffer(&scc->created_worklists->members, worklist*);
  worklist **top = topBuffer(&scc->created_worklists->members, worklist*);

  if ( scc->neg_status == SCC_NEG_DELAY )
  { DEBUG(MSG_TABLING_NEG,
	  Sdprintf("Searching delayed negative literals in SCC %zd\n",
		   pointerToInt(scc)));

    wlp = baseBuffer(&scc->created_worklists->members, worklist*);
    for(; wlp < top; wlp++)
    { worklist *wl = *wlp;

      if ( wl->negative &&
	   !wl->neg_completed &&
	   !wl->neg_delayed &&
	   !wl->has_answers )
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


static worklist *
pop_worklist(tbl_component *c ARG_LD)
{ worklist_set *wls;

  if ( (wls=c->worklist) && !isEmptyBuffer(&wls->members) )
  { worklist *wl = popBuffer(&wls->members, worklist*);
    wl->in_global_wl = FALSE;

    return wl;
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
reset_newly_created_worklists(tbl_component *c)
{ worklist_set *wls;

  if ( c && (wls = c->created_worklists) )
  { c->created_worklists = NULL;
    free_worklist_set(wls, WLFS_KEEP_COMPLETE);
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

      if ( freewl == WLFS_FREE_ALL ||
	   wl->table->data.worklist == WL_COMPLETE )
	free_worklist(wl);
    }
  }

  discardBuffer(&wls->members);
  PL_free(wls);
}

#if 0
static void *
pop_add_answer(trie_node *n, void *ctx)
{ worklist *wl = ctx;

  if ( n->value )
  { GET_LD

    wkl_add_answer(wl, n PASS_LD);
  }

  return NULL;
}

static void
populate_answers(worklist *wl)
{ map_trie_node(&wl->table->root, pop_add_answer, wl);
}
#endif


		 /*******************************
		 *	 TABLE DELAY LISTS	*
		 *******************************/

static int
answer_is_conditional(trie_node *answer)
{ delay_info *di;

  return ( (di=answer->data.delayinfo) &&
	   !isEmptyBuffer(&di->delay_sets) );
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
    initBuffer(&di->delay_sets);
    initBuffer(&di->delays);
    answer->data.delayinfo = di;

    return di;
  } else
  { return NULL;
  }
}


static void
destroy_delay_info(delay_info *di)
{ discardBuffer(&di->delay_sets);
  discardBuffer(&di->delays);
  free(di);
}


static delay_set *
create_delay_set(delay_info *di)
{ delay_set *ds;

  if ( di &&
       (ds=allocFromBuffer(&di->delay_sets, sizeof(*ds))) )
  { ds->offset = entriesBuffer(&di->delays, delay);
    ds->size   = 0;

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
add_to_wl_delays(trie *at, trie_node *answer)
{ worklist *wl = at->data.worklist;

  if ( WL_IS_WORKLIST(wl) )
  { DEBUG(MSG_TABLING_SIMPLIFY,
	  { GET_LD
	    term_t t = PL_new_term_ref();
	    unify_trie_term(at->data.variant, t PASS_LD);
	    Sdprintf("Adding propagation to worklist for: ");
	    PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
	  });
    addBuffer(&wl->delays, answer, trie_node *);
  }

  return TRUE;
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

static inline trie_node *
REC_DELAY(record_t r)
{ return (trie_node*)(((uintptr_t)r)|0x1);
}

static inline record_t
UNREC_DELAY(trie_node *r)
{ return (record_t)(((uintptr_t)r)&~(uintptr_t)1);
}

#ifndef NDEBUG					/* only used in assert() */
static int
IS_REC_DELAY(trie_node *r)
{ return (uintptr_t)r & 0x1;
}
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The delay list (`delays`) is a  list   of  delayed positive and negative
literals:

  - Negative literal ---> answer-trie ptr
  - Positive literal ---> answer-trie ptr + answer-node ptr

TBD: If we make an answer unconditional,  should we propagate this? Note
that the worklists still  point  to   this  answer.  That should trigger
propagation?

FIXME: delete record delays
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum
{ UDL_FALSE = 0,
  UDL_TRUE,
  UDL_COMPLETE
} udl_status;

static int
update_delay_list(worklist *wl, trie_node *answer,
		  term_t skel, term_t delays ARG_LD)
{ if ( delays )
  { Word dlp = valTermRef(delays);

    if ( isNil(*dlp) )
    { delay_info *di;

      if ( (di=answer->data.delayinfo) )
      { answer->data.delayinfo = NULL;
	destroy_delay_info(di);
      }

      if ( wl->ground )
	return UDL_COMPLETE;
      return UDL_TRUE;
    } else
    { delay_info *di;
      delay_set  *ds;
      intptr_t len;
      Word tail;

      len = skip_list(dlp, &tail PASS_LD);
      if ( !isNil(*tail) )
      { PL_type_error("list", delays);
	return UDL_FALSE;
      }
      if ( !hasGlobalSpace(3+len*3) )
      { int rc;

	if ( (rc=ensureGlobalSpace(3+len*3, ALLOW_GC)) == TRUE )
	{ dlp = valTermRef(delays);
	  deRef(dlp);
	} else
	{ raiseStackOverflow(rc);
	  return UDL_FALSE;
	}
      }

      di = answer_delay_info(wl, answer, TRUE);
      ds = create_delay_set(di);

      if ( ds )
      { word conj = 0;

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
	  { at = symbol_trie(*h);
	    an = NULL;
	  } else if ( isTerm(*h) )
	  { Functor f = valueTerm(*h);
	    Word p;

	    if ( f->definition == FUNCTOR_plus2 )
	    { deRef2(&f->arguments[0], p);
	      assert(isAtom(*p));
	      at = symbol_trie(*p);
	      deRef2(&f->arguments[1], p);
	      if ( isInteger(*p) )
	      { assert(isTaggedInt(*p));
		an = intToPointer(valInt(*p));
	      } else
	      { DEBUG(MSG_TABLING_DELAY,
		      { Sdprintf("Instantiated DL: ");
			pl_writeln(pushWordAsTermRef(p));
			popTermRef();
		      });

		if ( !conj )
		{ conj = *p;
		} else
		{ Word c = allocGlobalNoShift(3);

		  assert(c);
		  c[0] = FUNCTOR_comma2;
		  c[1] = *p;
		  c[2] = conj;
		  conj = consPtr(c, TAG_COMPOUND|STG_GLOBAL);
		}
		continue;
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

	if ( conj )
	{ record_t r;
	  Word vt = allocGlobalNoShift(3);
	  word rt;

	  assert(vt);
	  vt[0] = FUNCTOR_plus2;
	  vt[1] = linkVal(valTermRef(skel));
	  vt[2] = conj;
	  rt    = consPtr(vt, TAG_COMPOUND|STG_GLOBAL);

	  r = PL_record(pushWordAsTermRef(&rt));
	  popTermRef();
	  if ( r )
	  { if ( !add_to_delay_set(di, ds, NULL, REC_DELAY(r)) )
	      goto nomem;
	  } else
	  { return UDL_FALSE;
	  }
	}

	if ( !simplify_delay_set(di, ds) )
	{ delay *d = baseBuffer(&di->delays, delay);
	  unsigned int i, e = ds->offset+ds->size;

	  for(i=ds->offset; i<e; i++)
	  { if ( d[i].variant )
	    { if ( !add_to_wl_delays(d[i].variant, answer) )
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

  return UDL_TRUE;
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

static int
push_delay_list(Word p ARG_LD)
{ Word dl = valTermRef(LD->tabling.delay_list);

  assert(isTerm(*dl));
  dl = argTermP(*dl, 0);
  p[2] = *dl;
  TrailAssignment(dl);
  *dl = consPtr(p, TAG_COMPOUND|STG_GLOBAL);

  return TRUE;
}

/* Push a positive delay node.  If the answer is ground this is a
 * term atrie+answer, else it is a term atrie+wrapper.
 */

static int
push_delay(term_t atrie, term_t wrapper, trie_node *answer ARG_LD)
{ Word p;

  if ( (p = allocGlobal(6)) )
  { p[0] = FUNCTOR_dot2;
    p[1] = consPtr(&p[3], TAG_COMPOUND|STG_GLOBAL);
    p[3] = FUNCTOR_plus2;
    p[4] = linkVal(valTermRef(atrie));
    if ( is_ground_trie_node(answer) )
    { p[5] = consInt(pointerToInt(answer));
    } else
    { p[5] = linkVal(valTermRef(wrapper));
    }

    return push_delay_list(p PASS_LD);
  }

  return FALSE;
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
  trie *at = symbol_trie(di->variant->value);
  worklist *wl = at->data.worklist;
  assert(wl->magic == WORKLIST_MAGIC);

  DEBUG(MSG_TABLING_SIMPLIFY,
	print_delay("Making answer unconditional", di->variant, answer));

  answer->data.delayinfo = NULL;
  destroy_delay_info(di);
  agenda->done++;
  wl->undefined--;

  if ( !isEmptyBuffer(&wl->delays) )
    push_propagate(agenda, wl, answer, TRUE);

  return TRUE;
}


static int
remove_conditional_answer(spf_agenda *agenda, trie_node *answer)
{ delay_info *di = answer->data.delayinfo;
  trie *at = symbol_trie(di->variant->value);
  worklist *wl = at->data.worklist;
  assert(wl->magic == WORKLIST_MAGIC);

  DEBUG(MSG_TABLING_SIMPLIFY,
	print_delay("Removing conditional answer", di->variant, answer));

  answer->data.delayinfo = NULL;
  trie_delete(at, answer, FALSE);		/* cannot prune as may be */
  agenda->done++;				/* in worklist delay lists */
  wl->undefined--;

  if ( !isEmptyBuffer(&wl->delays) )
    push_propagate(agenda, wl, answer, FALSE);

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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
propagate_to_answer(spf_agenda *agenda, worklist *wl,
		    trie_node *panswer, int result, trie_node *answer)
{ delay_info *di;
  trie *variant = wl->table;
  int found = FALSE;

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

	    if ( d->answer == NULL )
	    { if ( !wl->negative )
		continue;
	      if ( result == FALSE &&
		   wl->table->value_count > 0 )
		continue;
	      res = !result;
	    } else
	    { res = result;
	    }

	    found = TRUE;

	    if ( res )
	    { memmove(d, d+1, sizeof(*d)*(oe-o-1));
	      if ( --ds->size == 0 )
	      { make_answer_unconditional(agenda, answer);
		return found;
	      }
	    } else
	    { memmove(ds, ds+1, sizeof(*ds)*(dz-ds-1));
	      (void)popBufferP(&di->delay_sets, delay_set);
	      ds--;				/* compensafe for(;;ds++) */
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

  return found;
}


static int
propagate_result(spf_agenda *agenda,
		 worklist *wl, trie_node *panswer, int result)
{ DEBUG(MSG_TABLING_SIMPLIFY,
	print_delay(result ? "Propagating TRUE" : "Propagating FALSE",
		    wl->table->data.variant, panswer));

  while( !isEmptyBuffer(&wl->delays) )
  { trie_node *answer = popBuffer(&wl->delays, trie_node*);

    propagate_to_answer(agenda, wl, panswer, result, answer);
  }

#if 0
  trie_node **base = baseBuffer(&wl->delays, trie_node*);
  trie_node **top  = topBuffer(&wl->delays, trie_node*);
  trie_node **to   = base;

  for( ; base < top; base++)
  { trie_node *answer = *base;

    if ( !propagate_to_answer(agenda, wl, panswer, result, answer) )
      *to++ = answer;
  }

  wl->delays.top = (char*)to;
#endif

  return TRUE;
}


static int
simplify_component(tbl_component *scc)
{ spf_agenda agenda;
  propagate *p;
  worklist **wlp0 = baseBuffer(&scc->created_worklists->members, worklist*);
  worklist **top  = topBuffer(&scc->created_worklists->members, worklist*);
  worklist **wlp;
  int undefined;

  DEBUG(MSG_TABLING_SIMPLIFY,
	Sdprintf("Simplifying SCC %zd\n", pointerToInt(scc)));

  init_spf_agenda(&agenda);

  for(;;)
  { int count = 0;

    undefined = 0;

    for(wlp = wlp0; wlp < top; wlp++)
    { worklist *wl = *wlp;

      if ( wl->negative &&
	   wl->neg_delayed &&
	   !wl->neg_completed &&
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

      if ( wl->undefined )
	undefined++;
    }

    if ( count == 0 || undefined == 0 )
      break;
  }

  exit_spf_agenda(&agenda);

  if ( undefined )
    return answer_completion(scc);
  else
    return TRUE;
}

static int
answer_completion(tbl_component *scc)
{ GET_LD
  worklist **wlp = baseBuffer(&scc->created_worklists->members, worklist*);
  worklist **top = topBuffer(&scc->created_worklists->members, worklist*);

#ifdef O_DEBUG
  if ( DEBUGGING(TABLING_NO_AC) )
    return TRUE;
#endif

  if ( LD->tabling.in_answer_completion )
    return TRUE;				/* Warn? */

  for(; wlp < top; wlp++)
  { worklist *wl = *wlp;

    if ( wl->undefined && !(wl->table->data.flags & AT_ANSWER_COMPLETED) )
    { fid_t fid;

      if ( (fid = PL_open_foreign_frame()) )
      { static predicate_t pred = NULL;
	term_t av = PL_new_term_refs(1);
	int rc;
	tbl_component *scc_old = LD->tabling.component;
	int hsc = LD->tabling.has_scheduling_component;

	if ( !pred )
	  pred = PL_predicate("answer_completion", 1, "$tabling");

	DEBUG(MSG_TABLING_AC,
	      { term_t t = PL_new_term_ref();
		unify_trie_term(wl->table->data.variant, t PASS_LD);
		Sdprintf("Calling answer completion for: ");
		PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
	      });

	LD->tabling.component = NULL;
	LD->tabling.has_scheduling_component = FALSE;
	LD->tabling.in_answer_completion = TRUE;
	rc = ( PL_put_atom(av+0, wl->table->symbol) &&
	       PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, pred, av) );
	LD->tabling.in_answer_completion = FALSE;
	LD->tabling.has_scheduling_component = hsc;
	LD->tabling.component = scc_old;

	PL_close_foreign_frame(fid);
	if ( !rc )
	  return FALSE;
      } else
	return FALSE;				/* stack overflow */
    }
  }

  return TRUE;
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

    if ( di )
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
      { trie *at = get_trie_form_node(answer);

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
  { trie->data.flags |= AT_ANSWER_COMPLETED;

    return TRUE;
  }

  return FALSE;
}

static
PRED_IMPL("$tbl_is_answer_completed", 1, tbl_is_answer_completed, 0)
{ trie *trie;

  if ( get_trie(A1, &trie) )
  { return (trie->data.flags & AT_ANSWER_COMPLETED) != 0;
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

  PL_free(wl);
}


static int
worklist_negative(worklist *wl)
{ wl->negative = TRUE;
  if ( wl->component->neg_status == SCC_NEG_NONE )
    wl->component->neg_status = SCC_NEG_DELAY;

  return TRUE;
}

static int
worklist_is_false(worklist *wl)
{ assert(wl->negative);
  return (wl->neg_completed || wl->neg_delayed) && !wl->has_answers;
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

  if ( answer_is_conditional(node) )
    wl->undefined++;
  else
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
unify_table_status(term_t t, trie *trie, int merge ARG_LD)
{ worklist *wl = trie->data.worklist;

  if ( WL_IS_WORKLIST(wl) )
  { if ( merge && wl->component != LD->tabling.component )
    { DEBUG(MSG_TABLING_WORK,
	    Sdprintf("Merging into %p (current = %p)\n",
		     wl->component, LD->tabling.component));
      merge_component(wl->component);
      LD->tabling.component = wl->component;
    }
    return PL_unify_pointer(t, wl);
  }
  if ( wl == WL_COMPLETE )
    return PL_unify_atom(t, ATOM_complete);

  assert(!wl || wl == WL_GROUND);
  return PL_unify_atom(t, ATOM_fresh);
}

static int
unify_skeleton(trie *trie, term_t wrapper, term_t skeleton ARG_LD)
{ if ( trie->data.skeleton )
  { term_t av;

    return ( (av=PL_new_term_refs(2)) &&
	     PL_recorded(trie->data.skeleton, av+0) &&
	     _PL_get_arg(1, av+0, av+1) &&
	     PL_unify(wrapper, av+1) &&
	     _PL_get_arg(2, av+0, av+1) &&
	     PL_unify(skeleton, av+1) );
  } else
  { term_t av;
    ssize_t vars;

    if ( (av = PL_new_term_ref()) &&
	 (vars=term_var_skeleton(wrapper, av PASS_LD)) >= 0 &&
	 PL_unify(av, skeleton) &&
	 PL_cons_functor(av, FUNCTOR_minus2, wrapper, av) )
    { trie->data.skeleton = PL_record(av);
      if ( vars == 0 )
      { if ( WL_IS_WORKLIST(trie->data.worklist) )
	{ trie->data.worklist->ground = TRUE;
	} else
	{ assert(!trie->data.worklist);
	  trie->data.worklist = WL_GROUND;
	}
      }
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

    if ( scc->neg_status != SCC_NEG_NONE &&
	 (wl=negative_worklist(scc PASS_LD)) )
      return PL_unify_pointer(A2, wl);
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

  if ( get_worklist(A1, &wl) )
  { Word kp;
    trie_node *node;
    int rc;

    kp = valTermRef(A2);

    if ( (rc=trie_lookup(wl->table, &node, kp, TRUE PASS_LD)) == TRUE )
    { if ( node->value )
      { if ( node->value == ATOM_trienode )
	{ if ( answer_is_conditional(node) )
	  { if ( update_delay_list(wl, node, A2, A3 PASS_LD) == UDL_COMPLETE )
	      return PL_unify_atom(A4, ATOM_cut);
	  }
	  return FALSE;				/* already in trie */
	}
	return PL_permission_error("modify", "trie_key", A2);
      }
      set_trie_value_word(wl->table, node, ATOM_trienode);

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
{ worklist *wl;

  if ( get_worklist(A1, &wl) )
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

    DEBUG(0, assert(is_in_global_worklist(wl)));

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

/** '$tbl_wkl_answer_trie'(+Worklist, -Trie) is det.
 *
 * True when Trie is the answer trie associated with Worklist
 */

static
PRED_IMPL("$tbl_wkl_answer_trie", 2, tbl_wkl_answer_trie, 0)
{ GET_LD
  worklist *wl;

  return ( get_worklist(A1, &wl) &&
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
{ if ( ensureTrailSpace(4) &&
       ensureGlobalSpace(6, ALLOW_GC) )
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

      return push_delay_list(p PASS_LD);
    } else if ( unlikely(answer_is_conditional(answer)) )
    { term_t av = PL_new_term_refs(3);
      Word p;

      if ( !PL_recorded(wl->table->data.skeleton, av+0) )
	return FALSE;
      _PL_get_arg(1, av+0, av+1);	/* wrapper */
      _PL_get_arg(2, av+0, av+2);	/* skeleton */
      if ( !PL_unify(a0+0, av+2) )
	return FALSE;

      p = allocGlobalNoShift(6);
      assert(p);

      p[0] = FUNCTOR_dot2;
      p[1] = consPtr(&p[3], TAG_COMPOUND|STG_GLOBAL);
      p[3] = FUNCTOR_plus2;
      p[4] = wl->table->symbol;
      if ( is_ground_trie_node(answer) )
      { p[5] = consInt(pointerToInt(answer));
      } else
      { p[5] = linkVal(valTermRef(av+1));
      }

      return push_delay_list(p PASS_LD);
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

      if ( an == NULL && !IS_TNOT(sr) )
      { if ( advance_wkl_state(state) )
	  goto next;
	goto out_fail;
      }
      if ( an != NULL && IS_TNOT(sr) &&
	   answer_is_conditional(an) )
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

static
PRED_IMPL("$tbl_variant_table", 4, tbl_variant_table, 0)
{ PRED_LD
  trie *trie;

  if ( (trie=get_variant_table(A1, TRUE PASS_LD)) )
  { return ( _PL_unify_atomic(A2, trie->symbol) &&
	     unify_table_status(A3, trie, TRUE PASS_LD)  &&
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
 * Get the status of Trie as well as its wrapper and Skeleton.
 */

static
PRED_IMPL("$tbl_table_status", 4, tbl_table_status, 0)
{ PRED_LD
  trie *trie;

  return ( get_trie(A1, &trie) &&
	   unify_table_status(A2, trie, FALSE PASS_LD) &&
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

      DEBUG(MSG_TABLING_WORK,
	    { term_t t = PL_new_term_ref();
	      unify_trie_term(trie->data.variant, t PASS_LD);
	      Sdprintf("Setting wl %zd in scc %zd to WL_COMPLETE.  Variant: ",
		       pointerToInt(wl), pointerToInt(c));
	      PL_write_term(Serror, t, 999, PL_WRT_NEWLINE);
	    });

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
    if ( !LD->tabling.component || LD->tabling.in_answer_completion )
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

  get_delay_set(di, set, &base, &top);
  for(--top; top >= base; top--, count++)
  { term_t c1 = count == 0 ? cond : av+0;

    if ( top->answer )
    { if ( top->variant )
      { term_t skel = av+1;
	term_t ans  = av+2;
	term_t tmp  = av+3;

	PL_put_variable(c1);
	PL_put_variable(ans);
	PL_put_variable(skel);

	if ( !unify_trie_term(top->variant->data.variant, c1 PASS_LD) )
	  return FALSE;
	if ( !( unify_trie_term(top->answer, ans PASS_LD) &&
		PL_recorded(top->variant->data.skeleton, skel) &&
		_PL_get_arg(1, skel, tmp) &&
		PL_unify(c1, tmp) &&
		_PL_get_arg(2, skel, tmp) &&
		PL_unify(ans, tmp) ) )
	  return FALSE;
      } else
      { term_t rd  = av+1;
	term_t tmp = av+2;

	assert(IS_REC_DELAY(top->answer));
	if ( !( PL_recorded(UNREC_DELAY(top->answer), rd) &&
		_PL_get_arg(1, rd, tmp) &&
		PL_unify(tmp, ctx->skel) &&
		_PL_get_arg(2, rd, c1) ) )
	  return FALSE;
      }
    } else
    { PL_put_variable(c1);
      if ( !unify_trie_term(top->variant->data.variant, c1 PASS_LD) ||
	   !PL_cons_functor(c1, FUNCTOR_tnot1, c1) )
	return FALSE;
    }

    if ( count > 0 )
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
  { return PL_unify_atom(t, ATOM_true);
  }
}

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


/** '$tbl_answer_update_dl'(+ATrie, +Wrapper, -Skeleton) is nondet.
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
  { return push_delay(ctx->atrie, wrapper, answer PASS_LD);
  } else
  { return TRUE;
  }
}

static
PRED_IMPL("$tbl_answer_update_dl", 3, tbl_answer_update_dl,
	  PL_FA_NONDETERMINISTIC)
{ update_dl_ctx ctx;

  ctx.atrie   = A1;

  return trie_gen(A1, A3, 0, A2, answer_update_delay_list, &ctx, PL__ctx);
}




		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

#define NDET PL_FA_NONDETERMINISTIC

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
  PRED_DEF("$tbl_delay_list",           1, tbl_delay_list,           0)
  PRED_DEF("$tbl_set_delay_list",       1, tbl_set_delay_list,       0)
  PRED_DEF("$tbl_add_global_delays",    2, tbl_add_global_delays,    0)

  PRED_DEF("$tbl_scc",                  1, tbl_scc,                  0)
  PRED_DEF("$tbl_scc_data",             2, tbl_scc_data,             0)
  PRED_DEF("$tbl_worklist_data",        2, tbl_worklist_data,        0)
  PRED_DEF("$tbl_answer",               3, tbl_answer,            NDET)
  PRED_DEF("$tbl_answer_dl",		3, tbl_answer_dl,         NDET)
  PRED_DEF("$tbl_answer_update_dl",     3, tbl_answer_update_dl,  NDET)
  PRED_DEF("$tbl_force_truth_value",    3, tbl_force_truth_value,    0)
  PRED_DEF("$tbl_set_answer_completed", 1, tbl_set_answer_completed, 0)
  PRED_DEF("$tbl_is_answer_completed",  1, tbl_is_answer_completed,  0)
EndPredDefs
