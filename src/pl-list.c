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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
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
  term_t h = PL_new_term_ref();
  term_t l = PL_copy_term_ref(A2);
  fid_t fid;

  if ( !(fid=PL_open_foreign_frame()) )
    return FALSE;

  for(;;)
  { if ( !PL_unify_list(l, h, l) )
    { PL_close_foreign_frame(fid);
      PL_unify_nil_ex(l);
      return FALSE;
    }

    if ( PL_unify(A1, h) )
    { term_t ex = 0;

      if ( foreignWakeup(&ex PASS_LD) )
      { PL_close_foreign_frame(fid);
	succeed;
      } else
      { if ( ex )
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a list on the global stack, just   at  the place the final result
will be.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
prolog_list_to_sort_list(term_t t, int remove_dups, int key,
			 list *lp, Word *end)
{ GET_LD
  Word l, tail;
  list p;
  intptr_t len;
  int rc;

  l = valTermRef(t);
  len = skip_list(l, &tail PASS_LD);
  if ( !(isNil(*tail) ||			/* proper list */
	 (isList(*tail) && remove_dups)) )	/* sort/2 on cyclic list */
  {
    if ( isVar(*tail) )
      return PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
    else
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, t);
  }

  if ( !hasGlobalSpace(len*3) )
  { if ( (rc=ensureGlobalSpace(len*3, ALLOW_GC)) != TRUE )
      return raiseStackOverflow(rc);
    l = valTermRef(t);			/* may be shifted */
    deRef(l);
  }

  p = (list)gTop;
  *lp = p;

  while(len-- > 0)
  { p->item.term = HeadList(l);
    deRef(p->item.term);
    if ( key )
    { word w = *p->item.term;

      if ( hasFunctor(w, FUNCTOR_minus2) )
      { p->item.key = argTermP(w, 0);
	deRef(p->item.key);
      } else
      { PL_error("keysort", 2, NULL, ERR_TYPE,
		 ATOM_pair, pushWordAsTermRef(p->item.term));
	popTermRef();
	return FALSE;
      }
    }
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

  succeed;
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
  { list l = 0;
    term_t tmp = PL_new_term_ref();
    Word top = NULL;

    if ( prolog_list_to_sort_list(in, remove_dups, compare_keys, &l, &top) )
    { l = nat_sort(l, remove_dups, compare_keys);
      put_sort_list(tmp, l);
      gTop = top;

      return PL_unify(out, tmp);
    }

    fail;
  }
}


static
PRED_IMPL("sort", 2, sort, PL_FA_ISO)
{ PRED_LD

  return pl_nat_sort(A1, A2, TRUE, FALSE PASS_LD);
}


static
PRED_IMPL("msort", 2, msort, 0)
{ PRED_LD

  return pl_nat_sort(A1, A2, FALSE, FALSE PASS_LD);
}


static
PRED_IMPL("keysort", 2, keysort, PL_FA_ISO)
{ PRED_LD

  return pl_nat_sort(A1, A2, FALSE, TRUE PASS_LD);
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
EndPredDefs
