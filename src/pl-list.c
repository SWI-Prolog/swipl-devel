/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2015, University of Amsterdam
                              VU University Amsterdam
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
#include "pl-dict.h"

#undef LD
#define LD LOCAL_LD

static
PRED_IMPL("is_list", 1, is_list, 0)
{ if ( lengthList(A1, FALSE) >= 0 )
    succeed;

  fail;
}


/** $length(-List, +Len) is semidet.

Implements `known-length' generation path of length/2. Fails if Len < 0.
*/

static
PRED_IMPL("$length", 2, dlength, 0)
{ PRED_LD
  intptr_t len;

  if ( PL_get_intptr(A2, &len) )
  { if ( len > 0 )
    { Word p;
      term_t list = PL_new_term_ref();

      if ( !hasGlobalSpace(len*3) )
      { int rc;

	if ( (rc=ensureGlobalSpace(len*3, ALLOW_GC)) != TRUE )
	  return raiseStackOverflow(rc);
      }

      p = gTop;
      *valTermRef(list) = consPtr(p, TAG_COMPOUND|STG_GLOBAL);
      while(len-- > 0)
      { p[0] = FUNCTOR_dot2;
	setVar(p[1]);
	p[2] = consPtr(&p[3], TAG_COMPOUND|STG_GLOBAL);
	p += 3;
      }
      p[-1] = ATOM_nil;
      gTop = p;

      return PL_unify(A1, list);
    } else if ( len == 0 )
    { return PL_unify_nil(A1);
    } else
    { return FALSE;
    }
  } else if ( PL_is_integer(A2) )
  { number i;
    Word p =  valTermRef(A2);

    deRef(p);
    get_integer(*p, &i);
    if ( ar_sign_i(&i) < 0 )
      return FALSE;

    return outOfStack((Stack)&LD->stacks.global, STACK_OVERFLOW_RAISE);
  }

  return PL_error("length", 2, NULL, ERR_TYPE, ATOM_integer, A2);
}


static
PRED_IMPL("memberchk", 2, memberchk, 0)
{ GET_LD
  term_t ex = PL_new_term_ref();
  term_t h = PL_new_term_ref();
  term_t l = PL_copy_term_ref(A2);
  size_t done = 0;
  fid_t fid;

  if ( !(fid=PL_open_foreign_frame()) )
    return FALSE;

  for(;;)
  { if ( ++done % 10000 == 0 )
    { if ( PL_handle_signals() < 0 )
	return FALSE;
      if ( done > usedStack(global)/(sizeof(word)*2) )
	return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, A2);
    }

    if ( !PL_unify_list(l, h, l) )
    { PL_close_foreign_frame(fid);
      PL_unify_nil_ex(l);
      return FALSE;
    }

    if ( PL_unify(A1, h) )
    { if ( foreignWakeup(ex PASS_LD) )
      { PL_close_foreign_frame(fid);
	succeed;
      } else
      { if ( !isVar(*valTermRef(ex)) )
	  return PL_raise_exception(ex);
	PL_rewind_foreign_frame(fid);
      }
    } else
    { PL_rewind_foreign_frame(fid);
    }
  }
}


		 /*******************************
		 *	      SORTING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Natural merge sort. Code contributed by   Richard O'Keefe and integrated
into SWI-Prolog by Jan Wielemaker. The  nice   point  about this code is
that it uses no  extra  space  and   is  pretty  stable  in performance.
Richards claim it that many  qsort()   implementations  in libc are very
slow. This isn't the case for glibc   2.2, where this performs about the
same  as  the  previous  qsort()    based  implementation.  However,  it
integrated keysort/2 in the set and here the difference is huge.

Here is C code implementing a bottom-up  natural merge sort on lists; it
has remove_dups and compare_keys options.   (Actually  I wouldn't handle
the compare_keys option quite like this.)   The  difference between this
and sam-sort is the way runs are built:

    natural merge:
        add new node r after last node q of run if item(q) <= item(r)
        otherwise end this run.

    sam-sort:
        add new node r after last node q of run if item(q) <= item(r)
        otherwise
            add new new r before first node p of run if item(r) < item(p)
            otherwise end this run.

The natural merge has the nice  property   that  if  the list is already
sorted it takes O(N) time. In  general  if   you  have  a list made of M
already sorted pieces S1++S2++...++SM it will  take no more than O(N.log
M). Sam-sort (for "Smooth Applicative Merge sort") has the nice property
that it likes the reverse order almost as much as forward order, so \ /\
and \/ patterns are sorted (nearly) as  fast   as  /  // and // patterns
respectively.

I've been using a variant of this code  in a sorting utility since about
1988. It leaves the UNIX sort(1) program in   the dust. As you may know,
sort(1) breaks the input into  blocks  that   fit  in  memory, sorts the
blocks using qsort(), and writes the blocks out to disc, then merges the
blocks. For files that fit into memory,   the  variant of this code runs
about twice as fast as sort(1). Part of  that is better I/O, but part is
just plain not using qsort().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum
{ SORT_ASC = 0,
  SORT_DESC = 1
} sort_order;

/*  Things in capital letters should be replaced for different applications  */

/*  ITEM	The type of an individual item.
    COMPARE	Compares two items given their addresses (allows ITEM to be
		large and avoids pass by copy).  Return <0, =0, or >0.
    COMPARE_KEY	Compares the keys of two items given the addresses of the
		entire items.
    FREE	Frees a List_Record including its ITEM.
*/

typedef struct
{ Word term;
  Word key;
} ITEM;

					/* TBD: handle CMP_ERROR */
#ifndef COMPARE_KEY
#define COMPARE_KEY(x,y) compareStandard((x)->key, (y)->key, FALSE PASS_LD)
#endif
#ifndef FREE
#define FREE(x) \
	{ x->next = NULL; \
	  x->item.term = NULL; \
	  x->item.key = NULL; \
	}
#endif

typedef struct List_Record *list;
struct List_Record {
    list next;
    ITEM item;
};

#define NIL (list)0

#define compare(c, x, y) \
	int c = COMPARE_KEY(&(x)->item, &(y)->item); \
	if ( order == SORT_DESC ) c = -c


static list
nat_sort(list data, int remove_dups, sort_order order)
{ GET_LD
  list stack[64];			/* enough for biggest machine */
  list *sp = stack;
  int runs = 0;				/* total number of runs processed */
  list p, q, r, s;
  struct List_Record header;
  int k;

  remove_dups = !remove_dups;		/* 0 -> do, 1 -> don't */
  while ((p = data) != NIL)
  { /* pick up a run from the front of data, setting */
    /* p = (pointer to beginning of run), data = (rest of data) */
    if ((q = p->next) != NIL)
    { compare(c, p, q);

      data = q->next;
      if (c > 0)
      { r = q, q = p, p = r;
	p->next = q;
      } else if (c == remove_dups)
      {	/* c < 0 or = 0, so c = 1 impossible */
	p->next = q->next;
	FREE(q);
	q = p;
      }

      for (r = data; r != NIL; )
      { compare(c, q, r);

	if (c > 0)
	  break;
	if (c == remove_dups)
	{ s = r->next;
	  FREE(r);
	  r = s;
	} else
	{ q->next = r, q = r, r = r->next;
	}
      }

      q->next = NIL;
      data = r;
    } else
    { data = NIL;
    }

    runs++;
    /* merge this run with 0 or more runs off the top of the stack */
    for (k = runs; 1 &~ k; k >>= 1)
    { q = *--sp;
      r = &header;
      while (q && p)
      {	/* q precedes p */
	compare(c, q, p);

	if (c <= 0)
	{ r->next = q, r = q, q = q->next;
	  if (c == remove_dups)
	  { s = p->next;
	    FREE(p);
	    p = s;
	  }
	} else
	{ r->next = p, r = p, p = p->next;
	}
      }
      r->next = q ? q : p;
      p = header.next;
    }

	 /* push the merged run onto the stack */
    *sp++ = p;
  }

  if (sp == stack)
    return NIL;

  /* merge all the runs on the stack */
  p = *--sp;
  while (sp != stack)
  { q = *--sp;
    r = &header;
    while (q && p)
    {	/* q precedes p */
      compare(c, q, p);

      if (c <= 0)
      { r->next = q, r = q, q = q->next;
	if (c == remove_dups)
	{ s = p->next;
	  FREE(p);
	  p = s;
	}
      } else
      { r->next = p, r = p, p = p->next;
      }
    }
    r->next = q ? q : p;
    p = header.next;
  }

  return p;
}


static Word
extract_key(Word p1, int argc, const word *argv, int pair ARG_LD)
{ if ( pair )
  { if ( hasFunctor(*p1, FUNCTOR_minus2) )
    { p1 = argTermP(*p1, 0);
      deRef(p1);
    } else
    { term_t err_t = pushWordAsTermRef(p1);

      PL_error("keysort", 2, NULL, ERR_TYPE, ATOM_pair, err_t);
      popTermRef();
      return NULL;
    }
  } else
  { for(; --argc >= 0; argv++)
    { term_t err_t, ant;
      const char *expected = "compound";
      atom_t existence = ATOM_argument;

      if ( isTerm(*p1) )
      { if ( termIsDict(*p1) )
	{ Word vp;

	  if ( (vp = dict_lookup_ptr(*p1, argv[0] PASS_LD)) )
	  { p1 = vp;
	    goto next;
	  }
	  existence = ATOM_key;
	  goto err_exists;
	} else if ( isInteger(argv[0]) )
	{ int arity = arityTerm(*p1);
	  int an = valInt(argv[0]);

	  if ( an <= arity )
	  { p1 = argTermP(*p1, an-1);
	  next:
	    deRef(p1);
	    continue;
	  }

	err_exists:
	  err_t = pushWordAsTermRef(p1);
	  ant = PL_new_term_ref();
	  *valTermRef(ant) = argv[0];
	  PL_error(NULL, 0, NULL, ERR_EXISTENCE3, existence, ant, err_t);
	  popTermRef();
	  return NULL;
	} else					/* no dict, atom key */
	{ expected = "dict";
	}
      }

      err_t = pushWordAsTermRef(p1);
      PL_type_error(expected, err_t);
      popTermRef();
      return NULL;
    }
  }

  return p1;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a list on the global stack, just   at  the place the final result
will be.  Return: 0: error, 1: sort, 2: do not sort (len < 2)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef enum
{ SORT_ERR,
  SORT_SORT,
  SORT_NIL,
  SORT_NOSORT
} list_sort;

static list_sort
prolog_list_to_sort_list(term_t t,		/* input list */
			 int remove_dups,	/* allow to be cyclic */
			 int argc, const word *argv, int pair, /* find key */
			 list *lp, Word *end ARG_LD)	/* result list */
{ Word l, tail;
  list p;
  intptr_t len;
  int rc;

  l = valTermRef(t);
  len = skip_list(l, &tail PASS_LD);
  if ( !(isNil(*tail) ||			/* proper list */
	 (isList(*tail) && remove_dups)) )	/* sort/2 on cyclic list */
  { if ( isVar(*tail) )
      PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
    else
      PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, t);

    return SORT_ERR;
  }

  if ( len == 0 )
    return SORT_NIL;
  if ( len == 1 && !pair && argc == 0 && !isList(*tail) )
    return SORT_NOSORT;

  if ( !hasGlobalSpace(len*3) )
  { if ( (rc=ensureGlobalSpace(len*3, ALLOW_GC)) != TRUE )
    { raiseStackOverflow(rc);
      return SORT_ERR;
    }
    l = valTermRef(t);			/* may be shifted */
    deRef(l);
  }

  p = (list)gTop;
  *lp = p;

  while(len-- > 0)
  { p->item.term = HeadList(l);
    deRef(p->item.term);
    p->item.key = extract_key(p->item.term, argc, argv, pair PASS_LD);

    if ( unlikely(!p->item.key) )
      return SORT_ERR;

    l = TailList(l);
    deRef(l);
    if ( len > 0 )
    { assert(isList(*l));
      p->next = p+1;
      p++;
    }
  }

  p->next = NULL;
  *end = (Word)(p+1);

  return SORT_SORT;
}


static void
put_sort_list(term_t l, list sl)
{ GET_LD

  *valTermRef(l) = consPtr(sl, TAG_COMPOUND|STG_GLOBAL);

  for(;;)
  { list n = sl->next;
    Word p = (Word)sl;

    n = sl->next;
					/* see also linkVal() */
    p[1] = (needsRef(*sl->item.term) ? makeRef(sl->item.term)
				     : *sl->item.term);
    p[0] = FUNCTOR_dot2;
    if ( n )
    { p[2] = consPtr(n, TAG_COMPOUND|STG_GLOBAL);
      sl = n;
    } else
    { p[2] = ATOM_nil;
      return;
    }
  }
}


static int
pl_nat_sort(term_t in, term_t out,
	    int remove_dups, sort_order order,
	    int argc, const word *argv, int pair
	    ARG_LD)
{ list l = 0;
  Word top = NULL;

  if ( !ensureLocalSpace(sizeof(word)) )
    return FALSE;

  switch( prolog_list_to_sort_list(in, remove_dups,
				   argc, argv, pair,
				   &l, &top PASS_LD) )
  { case SORT_ERR:
      return FALSE;
    case SORT_NIL:
      return PL_unify_nil(out);
    case SORT_NOSORT:
      return PL_unify(in, out);
    case SORT_SORT:
    default:
    { term_t tmp = PL_new_term_ref();
      l = nat_sort(l, remove_dups, order);
      put_sort_list(tmp, l);
      gTop = top;

      return PL_unify(out, tmp);
    }
  }
}


static
PRED_IMPL("sort", 2, sort, PL_FA_ISO)
{ PRED_LD

  return pl_nat_sort(A1, A2,
		     TRUE, SORT_ASC,
		     0, NULL, FALSE PASS_LD);
}


static
PRED_IMPL("msort", 2, msort, 0)
{ PRED_LD

  return pl_nat_sort(A1, A2,
		     FALSE, SORT_ASC,
		     0, NULL, FALSE PASS_LD);
}


static
PRED_IMPL("keysort", 2, keysort, PL_FA_ISO)
{ PRED_LD

  return pl_nat_sort(A1, A2,
		     FALSE, SORT_ASC,
		     0, NULL, TRUE PASS_LD);
}

/** sort(+Key, +Order, +Random, -Sorted)

ECLiPSe compatible sort.
*/

#define FAST_ARGV 10

static int
get_key_arg_ex(term_t t, word *k, int zero_ok ARG_LD)
{ Word p = valTermRef(t);

  deRef(p);
  if ( isTaggedInt(*p) )
  { intptr_t v = valInt(*p);

    if ( v > 0 )
    { *k = *p;
      return TRUE;
    }
    if ( v == 0 )
    { *k = *p;
      if ( zero_ok )
	return TRUE;
    }
  }

  if ( isAtom(*p) )
  { *k = *p;
    return TRUE;
  }

  if ( isInteger(*p) )
  { number n;

    get_integer(*p, &n);
    if ( ar_sign_i(&n) <= 0 )
      PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_not_less_than_one, t);

    return FALSE;
  }

  return -1;
}

typedef struct order_def
{ atom_t     name;
  sort_order order;
  int	     remove_dups;
} order_def;

static const order_def order_defs[] =
{ { ATOM_smaller,	SORT_ASC,  TRUE	 },
  { ATOM_at_smaller,	SORT_ASC,  TRUE	 },
  { ATOM_smaller_equal,	SORT_ASC,  FALSE },
  { ATOM_at_smaller_eq,	SORT_ASC,  FALSE },
  { ATOM_larger,	SORT_DESC, TRUE	 },
  { ATOM_at_larger,	SORT_DESC, TRUE	 },
  { ATOM_larger_equal,	SORT_DESC, FALSE },
  { ATOM_at_larger_eq,	SORT_DESC, FALSE },
  { 0 }
};


static
PRED_IMPL("sort", 4, sort, 0)
{ PRED_LD
  word tmp[FAST_ARGV];
  word *argv = tmp;
  int argc;
  int rc;
  atom_t order_name;
  const order_def *od;

  if ( (rc=get_key_arg_ex(A1, argv, TRUE PASS_LD)) == FALSE )
    return FALSE;
  if ( rc == TRUE )				/* Key is integer */
  { if ( argv[0] == consInt(0) )
    { argc = 0;
      argv = NULL;
    } else
    { argc = 1;
    }
  } else
  { size_t len;

    switch(PL_skip_list(A1, 0, &len))
    { case PL_LIST:
      { term_t tail = PL_copy_term_ref(A1);
	term_t head = PL_new_term_ref();

	if ( len > FAST_ARGV )
	{ if ( (argv = malloc(len*sizeof(intptr_t))) == NULL )
	    return PL_no_memory();
	}
        for(argc=0; PL_get_list(tail, head, tail); argc++)
	{ if ( get_key_arg_ex(head, &argv[argc], FALSE PASS_LD) != TRUE )
	  { rc = FALSE;
	    goto out;
	  }
	}
	assert(PL_get_nil(tail));
	break;
      }
      default:
	return PL_type_error("sort_key", A1);
    }
  }

  if ( !(rc=PL_get_atom_ex(A2, &order_name)) )
    goto out;

  for(od=order_defs; od->name; od++)
  { if ( od->name == order_name )
      break;
  }
  if ( !od->name )
  { rc = PL_domain_error("order", A2);
    goto out;
  }

  rc = pl_nat_sort(A3, A4,
		   od->remove_dups, od->order,
		   argc, argv, FALSE PASS_LD);

out:
  if ( argv && argv != tmp )
    free(argv);

  return rc;
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(list)
  PRED_DEF("is_list", 1, is_list, 0)
  PRED_DEF("$length", 2, dlength, 0)
  PRED_DEF("memberchk", 2, memberchk, 0)
  PRED_DEF("sort", 2, sort, PL_FA_ISO)
  PRED_DEF("msort", 2, msort, 0)
  PRED_DEF("keysort", 2, keysort, PL_FA_ISO)
  PRED_DEF("sort", 4, sort, 0)
EndPredDefs
