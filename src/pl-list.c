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

#include "pl-incl.h"

#undef LD
#define LD LOCAL_LD

static
PRED_IMPL("is_list", 1, is_list, 0)
{ if ( lengthList(A1, FALSE) >= 0 )
    succeed;

  fail;
}


word
pl_length(term_t list, term_t l)
{ GET_LD
  int n;

  if ( PL_get_integer(l, &n) )
  { if ( n >= 0 )
    { term_t h = PL_new_term_ref();
      term_t l = PL_copy_term_ref(list);

      while( n-- > 0 )
      { TRY(PL_unify_list(l, h, l));
      }

      return PL_unify_nil(l);
    }
    fail;
  }

  if ( PL_is_variable(l) )
  { long n;
  
    if ( (n=lengthList(list, FALSE)) >= 0 )
      return PL_unify_integer(l, n);

    fail;			/* both variables: generate in Prolog */
  }
  
  return PL_error("length", 2, NULL, ERR_TYPE, ATOM_integer, l);
}  


word
pl_memberchk(term_t e, term_t list)
{ GET_LD
  term_t h = PL_new_term_ref();
  term_t l = PL_copy_term_ref(list);

  for(;;)
  { TRY(PL_unify_list(l, h, l));
      
    if ( PL_unify(e, h) )
      succeed;
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

#ifndef COMPARE
#define COMPARE(x,y) compareStandard((x)->term, (y)->term, FALSE PASS_LD)
#endif
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
    int c = compare_keys ? COMPARE_KEY(&(x)->item, &(y)->item) \
                         : COMPARE(    &(x)->item, &(y)->item)

static list
nat_sort(list data, int remove_dups, int compare_keys)
{ GET_LD
  list stack[64];			/* enough for biggest machine */
  list *sp = stack;
  int runs = 0;				/* total number of runs processed */
  register list p, q, r, s;
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a list on the global stack, just   at  the place the final result
will be.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
prolog_list_to_sort_list(term_t t, int key, list *lp, Word *end)
{ GET_LD
  int n = lengthList(t, TRUE);
  Word l;
  list p;
  long minfree;

  if ( n < 0 )
    fail;				/* not a proper list */
  minfree = sizeof(word)*n*3;

#ifdef O_SHIFT_STACKS
  if ( roomStack(global) < minfree )
    growStacks(NULL, NULL, NULL, FALSE, minfree, FALSE);
#else
  requireStack(global, minfree);
#endif

  p = (list)gTop;
  *lp = p;
  l = valTermRef(t);
  deRef(l);

  for(;;)
  { p->item.term = HeadList(l);
    deRef(p->item.term);
    if ( key )
    { word w = *p->item.term;

      if ( hasFunctor(w, FUNCTOR_minus2) )
      { p->item.key = argTermP(w, 0);
	deRef(p->item.key);
      } else
      { term_t t = wordToTermRef(p->item.term);

	return PL_error("keysort", 2, NULL,
			ERR_TYPE, ATOM_key_value_pair, t);
      }
    }
    l = TailList(l);
    deRef(l);
    if ( isList(*l) )
    { p->next = p+1;
      p++;
    } else
    { p->next = NULL;
      *end = (Word)(p+1);
      succeed;
    }
  }
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
pl_nat_sort(term_t in, term_t out, int remove_dups, int compare_keys ARG_LD)
{ if ( PL_get_nil(in) )
    return PL_unify_atom(out, ATOM_nil);
  else
  { list l;
    term_t tmp = PL_new_term_ref();
    Word top;

    if ( prolog_list_to_sort_list(in, compare_keys, &l, &top) )
    { l = nat_sort(l, remove_dups, compare_keys);
      put_sort_list(tmp, l);
      gTop = top;

      return PL_unify(out, tmp);
    }

    fail;
  }
}


static
PRED_IMPL("ok_sort", 2, sort, 0)
{ PRED_LD

  return pl_nat_sort(A1, A2, TRUE, FALSE PASS_LD);
}


static
PRED_IMPL("ok_msort", 2, msort, 0)
{ PRED_LD

  return pl_nat_sort(A1, A2, FALSE, FALSE PASS_LD);
}


static
PRED_IMPL("ok_keysort", 2, keysort, 0)
{ PRED_LD

  return pl_nat_sort(A1, A2, FALSE, TRUE PASS_LD);
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(list)
  PRED_DEF("is_list", 1, is_list, 0)
  PRED_DEF("sort", 2, sort, 0)
  PRED_DEF("msort", 2, msort, 0)
  PRED_DEF("keysort", 2, keysort, 0)
EndPredDefs
