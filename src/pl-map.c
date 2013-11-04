/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

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
#include "pl-map.h"

int PL_get_map_ex(term_t data, term_t class, term_t map);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Maps are associative arrays,  where  keys   are  either  atoms  or small
integers. Maps should be considered  an   abstract  data  type. They are
currently represented as compound terms   using the functor `map`/Arity.
The term has the following layout on the global stack:

  -----------
  | `map`/A |
  -----------
  | class   |
  -----------
  | key1    |
  -----------
  | value1  |
  -----------
  | key2    |
  -----------
  | value2  |
      ...

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define CACHED_MAP_FUNCTORS 128

static functor_t map_functors[CACHED_MAP_FUNCTORS] = {0};

functor_t
map_functor(int pairs)
{ if ( pairs < CACHED_MAP_FUNCTORS )
  { if ( map_functors[pairs] )
      return map_functors[pairs];

    map_functors[pairs] = lookupFunctorDef(ATOM_map, pairs*2+1);
    return map_functors[pairs];
  } else
  { return lookupFunctorDef(ATOM_map, pairs*2+1);
  }
}


		 /*******************************
		 *	      SORTING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Copied from https://github.com/noporpoise/sort_r
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if defined(__MINGW32__) || defined(__OpenBSD__) || defined(AMIGA) || \
defined(__gnu_hurd__) || (__GLIBC__ == 2 && __GLIBC_MINOR__ < 8)
  #define QSORT_WITH_NESTED_FUNCTIONS 1
#endif

#ifdef QSORT_WITH_NESTED_FUNCTIONS

void sort_r(void *base, size_t nel, size_t width,
            int (*compar)(const void *a1, const void *a2, void *aarg), void *arg)
{
  int nested_cmp(const void *a, const void *b)
  {
    return compar(a, b, arg);
  }

  qsort(base, nel, width, nested_cmp);
}

#else

struct sort_r_data
{
  void *arg;
  int (*compar)(const void *a1, const void *a2, void *aarg);
};

int sort_r_arg_swap(void *s, const void *aa, const void *bb)
{
  struct sort_r_data *ss = (struct sort_r_data*)s;
  return (ss->compar)(aa, bb, ss->arg);
}

#if (defined _GNU_SOURCE || defined __GNU__ || defined __linux__)

typedef int(* __compar_d_fn_t)(const void *, const void *, void *);
extern void qsort_r (void *__base, size_t __nmemb, size_t __size,
                     __compar_d_fn_t __compar, void *__arg)
  __nonnull ((1, 4));

#endif

void sort_r(void *base, size_t nel, size_t width,
            int (*compar)(const void *a1, const void *a2, void *aarg), void *arg)
{
  #if (defined _GNU_SOURCE || defined __GNU__ || defined __linux__)

    qsort_r(base, nel, width, compar, arg);

  #elif (defined __APPLE__ || defined __MACH__ || defined __DARWIN__ || \
         defined __FREEBSD__ || defined __BSD__ || \
         defined OpenBSD3_1 || defined OpenBSD3_9)

    struct sort_r_data tmp;
    tmp.arg = arg;
    tmp.compar = compar;
    qsort_r(base, nel, width, &tmp, &sort_r_arg_swap);

  #elif (defined _WIN32 || defined _WIN64 || defined __WINDOWS__)

    struct sort_r_data tmp = {arg, compar};
    qsort_s(*base, nel, width, &sort_r_arg_swap, &tmp);

  #else
    #error Cannot detect operating system
  #endif
}

#endif /* !QSORT_WITH_NESTED_FUNCTIONS */



		 /*******************************
		 *      LOW-LEVEL FUNCTIONS	*
		 *******************************/

static int
get_map_ex(term_t t, Word mp, int create ARG_LD)
{ Word p = valTermRef(t);

  deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);
    FunctorDef fd = valueFunctor(f->definition);

    if ( fd->name == ATOM_map &&
	 fd->arity%2 == 1 )		/* does *not* validate ordering */
    { *mp = *p;
      return TRUE;
    }
  }

  if ( create )
  { term_t new;

    if ( (new = PL_new_term_ref()) &&
	  PL_get_map_ex(t, 0, new) )
    { p = valTermRef(new);
      deRef(p);
      *mp = *p;
      return TRUE;
    }

    return FALSE;
  }

  return PL_type_error("map", t);
}


static inline int
is_key(word w)
{ return isAtom(w) || isTaggedInt(w);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
map_lookup_ptr() returns a pointer to the value for a given key
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Word
map_lookup_ptr(word map, word name ARG_LD)
{ Functor data = valueTerm(map);
  int arity = arityFunctor(data->definition);
  int l = 1, h = arity-2;		/* odd numbers are the keys */

  assert(arity%2 == 1);

  for(;;)
  { int m = ((l+h)/2)|0x1;
    Word p;

    deRef2(&data->arguments[m], p);
    if ( *p == name )
      return p+1;

    if ( l == h )
      return NULL;

    if ( *p < name )
      l=m;
    else if ( h > m )
      h=m;
    else
      h=m-2;
  }
}


/* True if the keys are proper keys and ordered.
*/

static int
map_ordered(Word data, int count, int ex ARG_LD)
{ Word n1, n2;

  deRef2(data, n1);
  if ( !is_key(*n1) )
    return (count == 0);		/* ordered if empty map */
  for(; count > 1; count--, data += 2, n1=n2)
  { deRef2(data+2, n2);
    if ( !is_key(*n2) )
      return FALSE;
    if ( *n1 < *n2 )
      continue;
    if ( *n1 > *n2 )
      return FALSE;
    if ( *n1 == *n2 )
    { if ( ex )
      { term_t t = PL_new_term_ref();
	*valTermRef(t) = linkVal(n1);
	return PL_error(NULL, 0, NULL, ERR_DUPLICATE_KEY, t);
      }
      return FALSE;
    }
  }

  return TRUE;
}


static int
compare_map_entry(const void *a, const void *b, void *arg)
{ PL_local_data_t *__PL_ld = arg;
  Word p = (Word)a;
  Word q = (Word)b;

  deRef(p);
  deRef(q);
  return (*p<*q ? -1 : *p>*q ? 1 : 0);
}


static int
map_order(Word map, int ex ARG_LD)
{ Functor data = (Functor)map;
  int arity = arityFunctor(data->definition);

  assert(arity%2 == 1);

  sort_r(data->arguments+1, arity/2, sizeof(word)*2,
	 compare_map_entry, LD);

  return map_ordered(data->arguments+1, arity/2, ex PASS_LD);
}


/* map_order_term_refs() orders an array of indexes into a key/value array
   of term references. Returns 0 if there are no duplicates and else the
   index of the first duplicate.
*/

typedef struct order_term_refs
{ PL_local_data_t *ld;
  term_t *av;
} order_term_refs;


static int
compare_term_refs(const void *a, const void *b, void *arg)
{ const int *ip1 = a;
  const int *ip2 = b;
  order_term_refs *ctx = arg;
  PL_local_data_t *__PL_ld = ctx->ld;
  Word p = valTermRef(ctx->av[*ip1*2]);
  Word q = valTermRef(ctx->av[*ip2*2]);

  assert(!isRef(*p));
  assert(!isRef(*q));

  return (*p<*q ? -1 : *p>*q ? 1 : 0);
}


int
map_order_term_refs(term_t *av, int *indexes, int count ARG_LD)
{ order_term_refs ctx;

  ctx.ld = LD;
  ctx.av = av;

  sort_r(indexes, count, sizeof(int), compare_term_refs, &ctx);
  if ( count > 1 )
  { word k = *valTermRef(av[indexes[0]*2]);
    int i;

    for(i=1; i<count; i++)
    { word k2 = *valTermRef(av[indexes[i]*2]);

      if ( k == k2 )
	return i;
      k = k2;
    }
  }

  return 0;
}


int
put_map(word map, int size, Word nv, word *new_map ARG_LD)
{ Functor data = valueTerm(map);
  int arity = arityFunctor(data->definition);
  Word new, out, in, in_end, nv_end;
  int modified = FALSE;

  assert(arity%2 == 1);

  if ( size == 0 )
  { *new_map = map;
    return TRUE;
  }

  if ( gTop+1+arity+2*size > gMax )
    return GLOBAL_OVERFLOW;

  new    = gTop;
  out    = new+2;			/* functor, class */
  in     = data->arguments+1;
  in_end = in+arity-1;
  nv_end = nv+size*2;

  while(in < in_end && nv < nv_end)
  { Word i_name, n_name;

    deRef2(in, i_name);
    deRef2(nv, n_name);
    if ( *i_name == *n_name )
    { *out++ = *i_name;
      *out++ = linkVal(nv+1);
      in += 2;
      nv += 2;
      if ( !modified && compareStandard(nv+1, in+1, TRUE PASS_LD) )
	modified = TRUE;
    } else if ( *i_name < *n_name )
    { *out++ = *i_name;
      *out++ = linkVal(in+1);
      in += 2;
    } else
    { *out++ = *n_name;
      *out++ = linkVal(nv+1);
      nv += 2;
      modified = TRUE;
    }
  }

  if ( nv == nv_end )
  { if ( !modified )
    { *new_map = map;
      return TRUE;
    }
    while(in < in_end)
    { Word i_name;

      deRef2(in, i_name);
      *out++ = *i_name;
      *out++ = linkVal(in+1);
      in += 2;
    }
  } else
  { while(nv < nv_end)
    { Word n_name;

      deRef2(nv, n_name);
      *out++ = *n_name;
      *out++ = linkVal(nv+1);
      nv += 2;
    }
  }

  gTop = out;
  new[1] = linkVal(&data->arguments[0]);
  new[0] = map_functor((out-(new+1))/2);

  *new_map = consPtr(new, TAG_COMPOUND|STG_GLOBAL);

  return TRUE;
}


static int
del_map(word map, word key, word *new_map ARG_LD)
{ Functor data = valueTerm(map);
  int arity = arityFunctor(data->definition);
  Word new, out, in, in_end;

  assert(arity%2 == 1);

  if ( gTop+1+arity-2 > gMax )
    return GLOBAL_OVERFLOW;

  new    = gTop;
  out    = new+2;			/* functor, class */
  in     = data->arguments+1;
  in_end = in+arity-1;

  while(in < in_end)
  { Word i_name;

    deRef2(in, i_name);
    if ( *i_name != key )
    { *out++ = *i_name;
      *out++ = linkVal(in+1);
    }
    in += 2;
  }

  gTop = out;
  new[1] = linkVal(&data->arguments[0]); /* class */
  new[0] = map_functor((out-(new+1))/2); /* arity */

  *new_map = consPtr(new, TAG_COMPOUND|STG_GLOBAL);

  return TRUE;
}




static int
get_name_value(Word p, Word name, Word value ARG_LD)
{ deRef(p);

  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);

    if ( f->definition == FUNCTOR_minus2 ||  /* Name-Value */
	 f->definition == FUNCTOR_equals2 || /* Name=Value */
	 f->definition == FUNCTOR_colon2 )   /* Name:Value */
    { Word np, vp;

      deRef2(&f->arguments[0], np);
      if ( is_key(*np) )
      { *name = *np;
	deRef2(&f->arguments[1], vp);
	*value = linkVal(vp);

	return TRUE;
      }
    } else if ( arityFunctor(f->definition) == 1 ) /* Name(Value) */
    { Word vp;

      *name = nameFunctor(f->definition);
      deRef2(&f->arguments[0], vp);
      *value = linkVal(vp);
      return TRUE;
    }
  }

  return FALSE;				/* type error */
}



		 /*******************************
		 *	 FOREIGN SUPPORT	*
		 *******************************/

int
PL_is_map(term_t t)
{ GET_LD
  Word p = valTermRef(t);

  deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);
    FunctorDef fd = valueFunctor(f->definition);

    if ( fd->name == ATOM_map &&
	 fd->arity%2 == 1 &&
	 map_ordered(f->arguments+1, fd->arity/2, FALSE PASS_LD) )
      return TRUE;
  }

  return FALSE;
}


int
PL_get_map_ex(term_t data, term_t class, term_t map)
{ GET_LD

  if ( PL_is_map(data) )
  { PL_put_term(map, data);
    return TRUE;
  }

  if ( PL_is_list(data) )
  { intptr_t len = lengthList(data, TRUE);
    Word m, ap, tail;

    if ( len < 0 )
      return FALSE;			/* not a proper list */
  retry:
    if ( !(m = allocGlobal(len*2+2)) )
      return FALSE;			/* global overflow */
    ap = m;
    *ap++ = map_functor(len);
    if ( class )
    { Word cp = valTermRef(class);

      *ap = linkVal(cp);		/* TBD: maybe move to another function */
      if ( tagex(*ap) == (TAG_REFERENCE|STG_LOCAL) )
      { if ( unlikely(tTop+1 >= tMax) )
	{ int rc;

	  if ( (rc=ensureTrailSpace(1) != TRUE) )
	    return raiseStackOverflow(rc);
	  gTop = m;
	  goto retry;
	}
	deRef(cp)
	setVar(*ap);
	Trail(cp, makeRef(ap));
      }
    } else
    { setVar(*ap);
    }
    ap++;

    tail = valTermRef(data);
    deRef(tail);
    while( isList(*tail) )
    { Word head = HeadList(tail);

      if ( !get_name_value(head, ap, ap+1 PASS_LD) )
      { gTop = m;
	PL_type_error("name-value", pushWordAsTermRef(head));
	popTermRef();
	return FALSE;
      }
      ap += 2;
      tail = TailList(tail);
      deRef(tail);
    }

    if ( map_order(m, TRUE PASS_LD) )
    { gTop = ap;
      *valTermRef(map) = consPtr(m, TAG_COMPOUND|STG_GLOBAL);
      DEBUG(CHK_SECURE, checkStacks(NULL));
      return TRUE;
    } else
    { return FALSE;
    }
  }					/* TBD: {name:value, ...} */

  return PL_type_error("map-data", data);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_for_map()  runs  func  on  each  key-value    pair  in  map.  Returns
immediately with the return value of func   if func returns non-zero. If
the flag MAP_SORTED is given, the  key-value   pairs  are  called in the
standard order of terms.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct cmp_map_index_data
{ Word  data;
  int  *indexes;
  PL_local_data_t *ld;
} cmp_map_index_data;

static int
cmp_map_index(const void *a1, const void *a2, void *arg)
{ int *ip1 = (int*)a1;
  int *ip2 = (int*)a2;
  cmp_map_index_data *ctx = arg;
  PL_local_data_t *__PL_ld = ctx->ld;
  Word p = &ctx->data[*ip1*2];
  Word q = &ctx->data[*ip2*2];
  int rc;

  deRef(p);
  deRef(q);

  if ( *p == *q )
  { rc = CMP_EQUAL;
  } else
  { if ( isAtom(*p) )
    { if ( isAtom(*q) )
	rc = compareAtoms(*p, *q);
      else
	rc = CMP_GREATER;
    } else
    { if ( isTaggedInt(*p) )
	rc = valInt(*p) > valInt(*q) ? CMP_GREATER : CMP_LESS;
      else
	rc = CMP_LESS;
    }
  }

  return rc;
}


int
PL_for_map(term_t map,
	   int (*func)(term_t key, term_t value, int last, void *closure),
	   void *closure,
	   int flags)
{ GET_LD
  term_t av = PL_new_term_refs(2);
  int i, arity, pairs;
  Word p = valTermRef(map);
  int index_buf[256];
  int *indexes = NULL;
  int rc = 0;

  deRef(p);
  arity = arityTerm(*p);
  pairs = arity/2;

  if ( (flags&MAP_SORTED) )
  { cmp_map_index_data ctx;

    if ( pairs < 256 )
      indexes = index_buf;
    else if ( !(indexes = malloc(pairs*sizeof(int))) )
      return PL_no_memory();

    for(i=0; i<pairs; i++)
      indexes[i] = i;

    ctx.ld = LD;
    ctx.data = argTermP(*p,1);
    ctx.indexes = indexes;

    sort_r(indexes, pairs, sizeof(int), cmp_map_index, &ctx);
  }

  for(i=0; i < pairs; )
  { Word p = valTermRef(map);
    int in;

    if ( indexes )
    { in = indexes[i]*2+1;
    } else
    { in = i*2+1;
    }

    deRef(p);
    Functor f = valueTerm(*p);
    *valTermRef(av+0) = linkVal(&f->arguments[in]);
    *valTermRef(av+1) = linkVal(&f->arguments[in+1]);

    if ( (rc=(*func)(av+0, av+1, ++i == pairs, closure)) != 0 )
      break;
  }

  if ( indexes && indexes != index_buf )
    free(indexes);

  return rc;
}


		 /*******************************
		 *       PROLOG PREDICATES	*
		 *******************************/

/** is_map(@Term, ?Class)

True if Term is a map that belongs to Class.

@tbd What if Term has a variable class?
*/

static
PRED_IMPL("is_map", 2, is_map, 0)
{ PRED_LD
  Word p = valTermRef(A1);

  deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);
    FunctorDef fd = valueFunctor(f->definition);

    if ( fd->name == ATOM_map &&
	 fd->arity%2 == 1 &&
	 map_ordered(f->arguments+1, fd->arity/2, FALSE PASS_LD) )
      return unify_ptrs(&f->arguments[0], valTermRef(A2),
			ALLOW_GC|ALLOW_SHIFT PASS_LD);
  }

  return FALSE;
}


/** get_map(?Key, +Map, ?Value)

True when Key is associated with Value in Map. If Name is unbound, this
predicate is true for all Name/Value  pairs   in  the  map. The order in
which these pairs are enumerated is _undefined_.
*/

static
PRED_IMPL("get_map", 3, get_map, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  int i;
  word map;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { Word np = valTermRef(A1);

      if ( !get_map_ex(A2, &map, FALSE PASS_LD) )
	return FALSE;

      deRef(np);
      if ( is_key(*np) )
      { Word vp;

	if ( (vp=map_lookup_ptr(map, *np PASS_LD)) )
	  return unify_ptrs(vp, valTermRef(A3), ALLOW_GC|ALLOW_SHIFT PASS_LD);

	return FALSE;
      }
      if ( canBind(*np) )
      { i = 1;
	goto search;
      }
      return PL_type_error("key", A2);
    }
    case FRG_REDO:
    { Functor f;
      int arity;
      fid_t fid;
      Word p;

      i = (int)CTX_INT + 2;
      p = valTermRef(A2);
      deRef(p);
      map = *p;

    search:
      f = valueTerm(map);
      arity = arityFunctor(f->definition);

      if ( (fid=PL_open_foreign_frame()) )
      { for( ; i < arity; i += 2 )
	{ Word np;

	  deRef2(&f->arguments[i], np);	/* TBD: check type */
	  if ( unify_ptrs(&f->arguments[i+1], valTermRef(A3),
			  ALLOW_GC|ALLOW_SHIFT PASS_LD) &&
	       _PL_unify_atomic(A1, *np) )
	  { PL_close_foreign_frame(fid);

	    if ( i+2 < arity )
	      ForeignRedoInt(i);
	    else
	      return TRUE;
	  } else if ( exception_term )
	  { PL_close_foreign_frame(fid);
	    return FALSE;
	  }
	  PL_rewind_foreign_frame(fid);
	}
	PL_close_foreign_frame(fid);
      }
      return FALSE;
    }
    default:
      return TRUE;
  }
}


/** map_create(-Map, ?Class, +Data) is det.

Map represents the name-value pairs  in  Data.   If  Data  is a map, Map
unified  with  Data.  Otherwise,  a  new    Map   is  created.  Suitable
representations for Data are:

  - {Name:Value, ...}
  - [Name=Value, ...]
  - [Name-Value, ...]
  - [Name(Value), ...]
*/


static
PRED_IMPL("map_create", 3, map_create, 0)
{ PRED_LD
  term_t m = PL_new_term_ref();

  if ( PL_get_map_ex(A3, A2, m) )
    return PL_unify(A1, m);

  return FALSE;
}


/** put_map(+New, +MapIn, -MapOut)

True when Map is a copy of Map0 where values from Map1 replace or extend
the value set of Map0.
*/

static
PRED_IMPL("put_map", 3, put_map, 0)
{ PRED_LD
  word m1, m2;
  fid_t fid = PL_open_foreign_frame();

retry:

  if ( get_map_ex(A2, &m1, TRUE PASS_LD) &&
       get_map_ex(A1, &m2, TRUE PASS_LD) )
  { Functor f2 = valueTerm(m2);
    int arity = arityFunctor(f2->definition);
    word new;
    int rc;

    if ( (rc = put_map(m1, arity/2, &f2->arguments[1], &new PASS_LD)) == TRUE )
    { term_t t = PL_new_term_ref();

      *valTermRef(t) = new;
      return PL_unify(A3, t);
    } else
    { assert(rc == GLOBAL_OVERFLOW);
      if ( ensureGlobalSpace(0, ALLOW_GC) == TRUE )
      { PL_rewind_foreign_frame(fid);
	goto retry;
      }
    }
  }

  return FALSE;
}

/** put_map(+Key, +Map0, +Value, -Map)

True when Map is a copy of Map0 with Name Value added or replaced.
*/

static int
get_name_ex(term_t t, Word np ARG_LD)
{ Word p = valTermRef(t);

  deRef(p);
  if ( is_key(*p) )
  { *np = *p;
    return TRUE;
  }

  return PL_type_error("map-key", t);
}


static
PRED_IMPL("put_map", 4, put_map, 0)
{ PRED_LD
  word m1;
  term_t av = PL_new_term_refs(2);
  fid_t fid = PL_open_foreign_frame();

retry:
  if ( get_map_ex(A2, &m1, TRUE PASS_LD) &&
       get_name_ex(A1, valTermRef(av) PASS_LD) &&
       PL_put_term(av+1, A3) )
  { word new;
    int rc;

    if ( (rc = put_map(m1, 1, valTermRef(av), &new PASS_LD)) == TRUE )
    { term_t t = PL_new_term_ref();

      *valTermRef(t) = new;
      return PL_unify(A4, t);
    } else
    { assert(rc == GLOBAL_OVERFLOW);
      if ( ensureGlobalSpace(0, ALLOW_GC) == TRUE )
      { PL_rewind_foreign_frame(fid);
	goto retry;
      }
    }
  }

  return FALSE;
}


/** del_map(+Key, +MapIn, ?Value, -MapOut)

True when Key-Value is in MapIn and   MapOut  contains all keys of MapIn
except for Key.
*/

static
PRED_IMPL("del_map", 4, del_map, 0)
{ PRED_LD
  word key;
  term_t mt = PL_new_term_ref();
  fid_t fid = PL_open_foreign_frame();

retry:
  if ( get_map_ex(A2, valTermRef(mt), TRUE PASS_LD) &&
       get_name_ex(A1, &key PASS_LD) )
  { Word vp;

    if ( (vp=map_lookup_ptr(*valTermRef(mt), key PASS_LD)) &&
	 unify_ptrs(vp, valTermRef(A3), ALLOW_GC|ALLOW_SHIFT PASS_LD) )
    { int rc;
      word new;

      if ( (rc=del_map(*valTermRef(mt), key, &new PASS_LD)) == TRUE )
      { term_t t = PL_new_term_ref();

	*valTermRef(t) = new;
	return PL_unify(A4, t);
      } else
      { assert(rc == GLOBAL_OVERFLOW);
	if ( ensureGlobalSpace(0, ALLOW_GC) == TRUE )
	{ PL_rewind_foreign_frame(fid);
	  goto retry;
	}
      }
    }
  }

  return FALSE;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(map)
  PRED_DEF("is_map",     2, is_map,     0)
  PRED_DEF("map_create", 3, map_create, 0)
  PRED_DEF("put_map",    3, put_map,    0)
  PRED_DEF("put_map",    4, put_map,    0)
  PRED_DEF("get_map",    3, get_map,    PL_FA_NONDETERMINISTIC)
  PRED_DEF("del_map",    4, del_map,    0)
EndPredDefs
