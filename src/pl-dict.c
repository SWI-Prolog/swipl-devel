/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2017, VU University Amsterdam
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
#include "pl-rsort.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Dicts are associative arrays,  where  keys   are  either  atoms  or small
integers. Dicts should be considered  an   abstract  data  type. They are
currently represented as compound terms   using the functor `dict`/Arity.
The term has the following layout on the global stack:

  ------------
  | `dict`/A |
  ------------
  | tag      |
  ------------
  | value1   |
  ------------
  | key1     |
  ------------
  | value2   |
  ------------
  | key2     |
      ...

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int PL_get_dict_ex(term_t data, term_t tag, term_t dict, int flags);
#define DICT_GET_ALL	0xff
#define DICT_GET_PAIRS	0x01
#define DICT_GET_EQUALS	0x02
#define DICT_GET_COLON	0x04
#define DICT_GET_TERM	0x08

#define CACHED_DICT_FUNCTORS 128

static functor_t dict_functors[CACHED_DICT_FUNCTORS] = {0};

functor_t
dict_functor(int pairs)
{ if ( pairs < CACHED_DICT_FUNCTORS )
  { if ( dict_functors[pairs] )
      return dict_functors[pairs];

    dict_functors[pairs] = lookupFunctorDef(ATOM_dict, pairs*2+1);
    return dict_functors[pairs];
  } else
  { return lookupFunctorDef(ATOM_dict, pairs*2+1);
  }
}

		 /*******************************
		 *      LOW-LEVEL FUNCTIONS	*
		 *******************************/

static int
get_dict_ex(term_t t, Word dp, int ex ARG_LD)
{ Word p = valTermRef(t);

  deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);
    FunctorDef fd = valueFunctor(f->definition);

    if ( fd->name == ATOM_dict &&
	 fd->arity%2 == 1 )		/* does *not* validate ordering */
    { *dp = *p;
      return TRUE;
    }
  }

  if ( !ex )
    return FALSE;

  PL_type_error("dict", t);
  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_create_dict_ex(+t, -dict ARG_LD) extracts a dict  from t or raises a
type error. The term reference dict contains   a  plain dict term handle
and is never a reference.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
get_create_dict_ex(term_t t, term_t dt ARG_LD)
{ Word p = valTermRef(t);

  deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);
    FunctorDef fd = valueFunctor(f->definition);

    if ( fd->name == ATOM_dict &&
	 fd->arity%2 == 1 )		/* does *not* validate ordering */
    { *valTermRef(dt) = *p;
      return TRUE;
    }
  }

  if ( PL_get_dict_ex(t, 0, dt, DICT_GET_ALL) )
  { assert(isTerm(*valTermRef(dt)));
    return TRUE;
  }

  return PL_type_error("dict", t);
}


static inline int
is_key(word w)
{ return isAtom(w) || isTaggedInt(w);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dict_lookup_ptr() returns a pointer to the value for a given key
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Word
dict_lookup_ptr(word dict, word name ARG_LD)
{ Functor data = valueTerm(dict);
  int arity = arityFunctor(data->definition);
  int l = 1, h = arity/2;

  if ( arity == 1 )
    return NULL;			/* empty */
  assert(arity%2 == 1);

  for(;;)
  { int m = (l+h)/2;
    Word p;

    deRef2(&data->arguments[m*2], p);

    if ( *p == name )
      return p-1;

    if ( l == h )
      return NULL;

    if ( *p < name )
      l=m+1;
    else if ( m == l )
      h=m;
    else
      h=m-1;
  }
}


/* True if the keys are proper keys and ordered.  Return values:

   TRUE:  correctly ordered dict
   FALSE: not ordered
   -1:    not a key
   -2:    duplicate key
*/

static int
dict_ordered(Word data, int count, int ex ARG_LD)
{ int ordered = TRUE;
  Word n1, n2;

  if ( count > 0 )
  { data++;			/* skip to key */
    deRef2(data, n1);
    if ( !is_key(*n1) )
      return -1;
  }

  for(; count > 1; count--, data += 2, n1=n2)
  { deRef2(data+2, n2);
    if ( !is_key(*n2) )
      return -1;
    if ( *n1 < *n2 )
      continue;
    if ( *n1 > *n2 )
      ordered = FALSE;
    if ( *n1 == *n2 )
    { if ( ex )
      { term_t t = PL_new_term_ref();
	*valTermRef(t) = linkVal(n1);
	PL_error(NULL, 0, NULL, ERR_DUPLICATE_KEY, t);
      }
      return -2;
    }
  }

  return ordered;
}


#if defined(O_PLMT) || defined(O_MULTIPLE_ENGINES)
#define GET_LDARG(x) PL_local_data_t *__PL_ld = (x)
#else
#define GET_LDARG(x)
#endif

static int
compare_dict_entry(const void *a, const void *b, void *arg)
{ GET_LDARG(arg);
  Word p = (Word)a+1;
  Word q = (Word)b+1;

  deRef(p);
  deRef(q);
  return (*p<*q ? -1 : *p>*q ? 1 : 0);
}


static int
dict_order(Word dict, int ex ARG_LD)
{ Functor data = (Functor)dict;
  int arity = arityFunctor(data->definition);

  assert(arity%2 == 1);

  sort_r(data->arguments+1, arity/2, sizeof(word)*2,
	 compare_dict_entry, LD);

  return dict_ordered(data->arguments+1, arity/2, ex PASS_LD) == TRUE;
}


/* dict_order_term_refs() orders an array of indexes into a key/value array
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
  GET_LDARG(ctx->ld);
  Word p = valTermRef(ctx->av[*ip1*2]);
  Word q = valTermRef(ctx->av[*ip2*2]);

  assert(!isRef(*p));
  assert(!isRef(*q));

  return (*p<*q ? -1 : *p>*q ? 1 : 0);
}


int
dict_order_term_refs(term_t *av, int *indexes, int count ARG_LD)
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


static int
assign_in_dict(Word dp, Word val ARG_LD)
{ deRef(val);

  if ( !canBind(*val) )
  { *dp = *val;
  } else if ( isAttVar(*val) )
  { *dp = makeRef(val);
  } else
  { if ( dp < val )
    { if ( unlikely(tTop+1 >= tMax) )
	return TRAIL_OVERFLOW;
      setVar(*dp);
      Trail(val, makeRef(dp));
    } else
    { *dp = makeRef(val);
    }
  }

  return TRUE;
}


int
put_dict(word dict, int size, Word nv, word *new_dict ARG_LD)
{ Functor data = valueTerm(dict);
  int arity = arityFunctor(data->definition);
  Word new, out, in, in_end, nv_end;
  int modified = FALSE;

  assert(arity%2 == 1);

  if ( size == 0 )
  { *new_dict = dict;
    return TRUE;
  }

  if ( gTop+1+arity+2*size > gMax )
    return GLOBAL_OVERFLOW;

  new    = gTop;
  out    = new+2;			/* functor, tag */
  in     = data->arguments+1;
  in_end = in+arity-1;
  nv_end = nv+size*2;

  while(in < in_end && nv < nv_end)
  { Word i_name, n_name;
    int rc;

    deRef2(in+1, i_name);
    deRef2(nv+1, n_name);

    if ( *i_name == *n_name )
    { if ( (rc=assign_in_dict(out++, nv PASS_LD)) != TRUE )
	return rc;
      *out++ = *i_name;
      if ( !modified && compareStandard(nv, in, TRUE PASS_LD) )
	modified = TRUE;
      in += 2;
      nv += 2;
    } else if ( *i_name < *n_name )
    { *out++ = linkVal(in);
      *out++ = *i_name;
      in += 2;
    } else
    { if ( (rc=assign_in_dict(out++, nv PASS_LD)) != TRUE )
	return rc;
      *out++ = *n_name;
      nv += 2;
      modified = TRUE;
    }
  }

  if ( nv == nv_end )
  { if ( !modified )
    { *new_dict = dict;
      return TRUE;
    }
    while(in < in_end)
    { Word i_name;
      deRef2(in+1, i_name);
      *out++ = linkVal(in);
      *out++ = *i_name;
      in += 2;
    }
  } else
  { while(nv < nv_end)
    { Word n_name;
      int rc;

      deRef2(nv+1, n_name);
      if ( (rc=assign_in_dict(out++, nv PASS_LD)) != TRUE )
	return rc;
      *out++ = *n_name;
      nv += 2;
    }
  }

  gTop = out;
  new[1] = linkVal(&data->arguments[0]);
  new[0] = dict_functor((out-(new+1))/2);

  *new_dict = consPtr(new, TAG_COMPOUND|STG_GLOBAL);

  return TRUE;
}


static int
del_dict(word dict, word key, word *new_dict ARG_LD)
{ Functor data = valueTerm(dict);
  int arity = arityFunctor(data->definition);
  Word new, out, in, in_end;

  assert(arity%2 == 1);

  if ( gTop+1+arity-2 > gMax )
    return GLOBAL_OVERFLOW;

  new    = gTop;
  out    = new+2;			/* functor, tag */
  in     = data->arguments+1;
  in_end = in+arity-1;

  while(in < in_end)
  { Word i_name;

    deRef2(in+1, i_name);
    if ( *i_name != key )
    { *out++ = linkVal(in);
      *out++ = *i_name;
    }
    in += 2;
  }

  gTop = out;
  new[1] = linkVal(&data->arguments[0]); /* tag */
  new[0] = dict_functor((out-(new+1))/2); /* arity */

  *new_dict = consPtr(new, TAG_COMPOUND|STG_GLOBAL);

  return TRUE;
}


/* partial_unify_dict(dict1, dict2) unifies all common elements of two
   dicts.  It returns TRUE on success, FALSE on a failed unification
   and *_OVERFLOW on some memory overflow
*/

static int
partial_unify_dict(word dict1, word dict2 ARG_LD)
{ Functor d1 = valueTerm(dict1);
  Functor d2 = valueTerm(dict2);
  Word in1  = d1->arguments;
  Word in2  = d2->arguments;
  Word end1 = in1+arityFunctor(d1->definition);
  Word end2 = in2+arityFunctor(d2->definition);
  int rc;

  /* unify the tages */
  if ( (rc=unify_ptrs(in1, in2, ALLOW_RETCODE PASS_LD)) != TRUE )
    return rc;

  /* advance to first v+k entry */
  in1++;
  in2++;

  while(in1 < end1 && in2 < end2)
  { Word n1, n2;

    deRef2(in1+1, n1);
    deRef2(in2+1, n2);
    if ( *n1 == *n2 )
    { if ( (rc = unify_ptrs(in1, in2, ALLOW_RETCODE PASS_LD)) != TRUE )
	return rc;
      in1 += 2;
      in2 += 2;
    } else if ( *n1 < *n2 )
    { in1 += 2;
    } else
    { in2 += 2;
    }
  }

  return TRUE;
}


/* select_dict() demands del to be a sub-dict of from and assigns
   all remaining values in new.

   Note that unify_ptrs() can push data onto the global stack in
   case it encounters attributed variables.  Therefore we need a
   two pass process.
*/

static int
select_dict(word del, word from, word *new_dict ARG_LD)
{ Functor dd = valueTerm(del);
  Functor fd = valueTerm(from);
  Word din  = dd->arguments;
  Word fin  = fd->arguments;
  Word dend = din+arityFunctor(dd->definition);
  Word fend = fin+arityFunctor(fd->definition);
  size_t left = 0;
  int rc;

  /* unify the tags */
  if ( (rc=unify_ptrs(din, fin, ALLOW_RETCODE PASS_LD)) != TRUE )
    return rc;

  /* advance to first v+k entry */
  din++;
  fin++;

  while(din < dend && fin < fend)
  { Word d, f;

    deRef2(din+1, d);
    deRef2(fin+1, f);

    if ( *d == *f )
    { if ( (rc = unify_ptrs(din, fin, ALLOW_RETCODE PASS_LD)) != TRUE )
	return rc;
      din += 2;
      fin += 2;
    } else if ( *d < *f )
    { return FALSE;
    } else
    { fin += 2;
      left++;
    }
  }
  if ( din < dend )
    return FALSE;
  left += (fend-fin)/2;

  if ( !new_dict )
    return TRUE;

  if ( gTop+2+2*left <= gMax )
  { Word out = gTop;

    *new_dict = consPtr(out, TAG_COMPOUND|STG_GLOBAL);

    *out++ = dict_functor(left);
    setVar(*out++);			/* tag for new dict */

    din = dd->arguments+1;
    fin = fd->arguments+1;

    while(left > 0)
    { Word d, f;

      deRef2(din+1, d);
      deRef2(fin+1, f);
      if ( *d == *f )
      { din += 2;
	fin += 2;
      } else
      { *out++ = linkVal(fin);
	*out++ = *f;
	fin += 2;
	left--;
      }
    }
    gTop = out;

    return TRUE;
  }

  return GLOBAL_OVERFLOW;
}


static int
get_name_ex(term_t t, Word np ARG_LD)
{ Word p = valTermRef(t);

  deRef(p);
  if ( is_key(*p) )
  { *np = *p;
    return TRUE;
  }

  PL_type_error("dict-key", t);
  return FALSE;
}


static int
get_name_value(Word p, Word name, Word value, Word mark, int flags ARG_LD)
{ const char *type;

  deRef(p);

  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);

    if ( (f->definition == FUNCTOR_minus2  && (flags&DICT_GET_PAIRS)) ||
	 (f->definition == FUNCTOR_equals2 && (flags&DICT_GET_EQUALS)) ||
	 (f->definition == FUNCTOR_colon2  && (flags&DICT_GET_COLON)))
    { Word np, vp;

      deRef2(&f->arguments[0], np);
      if ( is_key(*np) )
      { *name = *np;
	deRef2(&f->arguments[1], vp);
	*value = linkVal(vp);

	return TRUE;
      } else
      { gTop = mark;
	PL_type_error("dict-key", pushWordAsTermRef(np));
	popTermRef();

	return FALSE;
      }
    } else if ( arityFunctor(f->definition) == 1 &&
		(flags&DICT_GET_TERM) ) /* Name(Value) */
    { Word vp;

      *name = nameFunctor(f->definition);
      deRef2(&f->arguments[0], vp);
      *value = linkVal(vp);
      return TRUE;
    }
  }

  if ( flags == DICT_GET_PAIRS )
    type = "pair";
  else
    type = "key-value";

  gTop = mark;
  PL_type_error(type, pushWordAsTermRef(p));
  popTermRef();

  return FALSE;				/* type error */
}



		 /*******************************
		 *	 FOREIGN SUPPORT	*
		 *******************************/

int
PL_is_dict(term_t t)
{ GET_LD
  Word p = valTermRef(t);

  deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);
    FunctorDef fd = valueFunctor(f->definition);

    if ( fd->name == ATOM_dict &&
	 fd->arity%2 == 1 &&
	 dict_ordered(f->arguments+1, fd->arity/2, FALSE PASS_LD) == TRUE )
      return TRUE;
  }

  return FALSE;
}


static int
PL_get_dict_ex(term_t data, term_t tag, term_t dict, int flags)
{ GET_LD

  if ( PL_is_dict(data) )
  { PL_put_term(dict, data);
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
    *ap++ = dict_functor(len);
    if ( tag )
    { Word cp = valTermRef(tag);

      *ap = linkVal(cp);		/* TBD: maybe move to another function */
      if ( tagex(*ap) == (TAG_REFERENCE|STG_LOCAL) )
      { if ( unlikely(tTop+1 >= tMax) )
	{ if ( !makeMoreStackSpace(TRAIL_OVERFLOW, ALLOW_GC|ALLOW_SHIFT) )
	    return FALSE;
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

      if ( !get_name_value(head, ap+1, ap, m, flags PASS_LD) )
      {
	return FALSE;
      }
      ap += 2;
      tail = TailList(tail);
      deRef(tail);
    }

    if ( dict_order(m, TRUE PASS_LD) )
    { gTop = ap;
      *valTermRef(dict) = consPtr(m, TAG_COMPOUND|STG_GLOBAL);
      DEBUG(CHK_SECURE, checkStacks(NULL));
      return TRUE;
    } else
    { return FALSE;
    }
  }					/* TBD: {name:value, ...} */

  return PL_type_error("dict-data", data);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_for_dict()  runs  func  on  each  key-value    pair  in  dict.  Returns
immediately with the return value of func   if func returns non-zero. If
the flag DICT_SORTED is given, the  key-value   pairs  are  called in the
standard order of terms.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct cmp_dict_index_data
{ Word  data;
  int  *indexes;
  PL_local_data_t *ld;
} cmp_dict_index_data;

static int
cmp_dict_index(const void *a1, const void *a2, void *arg)
{ int *ip1 = (int*)a1;
  int *ip2 = (int*)a2;
  cmp_dict_index_data *ctx = arg;
  GET_LDARG(ctx->ld);
  Word p = &ctx->data[*ip1*2+1];
  Word q = &ctx->data[*ip2*2+1];
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
PL_for_dict(term_t dict,
	   int (*func)(term_t key, term_t value, int last, void *closure),
	   void *closure,
	   int flags)
{ GET_LD
  term_t av = PL_new_term_refs(2);
  int i, arity, pairs;
  Word p = valTermRef(dict);
  int index_buf[256];
  int *indexes = NULL;
  int rc = 0;

  deRef(p);
  arity = arityTerm(*p);
  pairs = arity/2;

  if ( (flags&DICT_SORTED) )
  { cmp_dict_index_data ctx;

    if ( pairs < 256 )
      indexes = index_buf;
    else if ( !(indexes = malloc(pairs*sizeof(int))) )
      return PL_no_memory();

    for(i=0; i<pairs; i++)
      indexes[i] = i;

    ctx.ld = LD;
    ctx.data = argTermP(*p,1);
    ctx.indexes = indexes;

    sort_r(indexes, pairs, sizeof(int), cmp_dict_index, &ctx);
  }

  for(i=0; i < pairs; )
  { Word p = valTermRef(dict);
    int in;

    if ( indexes )
    { in = indexes[i]*2+1;
    } else
    { in = i*2+1;
    }

    deRef(p);
    Functor f = valueTerm(*p);
    *valTermRef(av+0) = linkVal(&f->arguments[in+1]);
    *valTermRef(av+1) = linkVal(&f->arguments[in]);

    if ( (rc=(*func)(av+0, av+1, ++i == pairs, closure)) != 0 )
      break;
  }

  if ( indexes && indexes != index_buf )
    free(indexes);

  return rc;
}


		 /*******************************
		 *	  RELOAD SUPPORT	*
		 *******************************/

/* resortDictsInClause() resorts the contents of dicts in a clause

This predicate is called from pl-wic.c after   reloading a clause from a
.qlf file or state if pl-wic.c  detected   a  dict  inside the clause. It
identifies the code ranges that constitude the  k-v pairs in the dict and
re-orders them according to the new atom-handle ordering.

There    is    a    complicating     factor    with    B_FIRSTVAR/B_VAR,
B_ARGFIRSTVAR/B_ARGVAR and H_FIRSTVAR/H_VAR, that may  get swapped after
reordering.  This  is  corrected   by    fix_firstvars().   The  current
implementation is quadratic in the number of variables in the dict.
*/

typedef struct kv_code
{ word key;
  size_t start;
  size_t len;
} kv_code;

#define KV_PREALOCATED 32
#define C_PREALLOCATED 256

static int
cmp_kv_pos(const void *p1, const void *p2)
{ const kv_code *k1 = p1;
  const kv_code *k2 = p2;

  return k1->key < k2->key ? -1 : k1->key == k2->key ? 0 : 1;
}

static void
fix_firstvars(Code start, Code end)
{ Code PC;

  for( PC=start; PC < end; PC = stepPC(PC) )
  { code op = fetchop(PC);
    code first;
    code var;

    switch(op)
    { case B_VAR0:
      case B_VAR1:
      case B_VAR2:
	var = (code)VAROFFSET(op-B_VAR0);
	first = B_FIRSTVAR;
        goto find_first;
      case B_VAR:
	var = PC[1];
	first = B_FIRSTVAR;
        goto find_first;
      case B_ARGVAR:
	var = PC[1];
	first = B_ARGFIRSTVAR;
        goto find_first;
      case H_VAR:
	var = PC[1];
	first = H_FIRSTVAR;
      find_first:
      { Code pc;

	for(pc=PC; pc < end; pc = stepPC(pc))
	{ code op2 = fetchop(pc);

	  if ( op2 == first && pc[1] == var )
	  { DEBUG(MSG_DICT, Sdprintf("Swapping first vars\n"));
	    *PC = *pc;
	    *pc = encode(op);
	  }
	}
      }
    }
  }
}

int
resortDictsInCodes(Code PC, Code end)
{
  for( ; PC < end; PC = stepPC(PC) )
  { code op = fetchop(PC);

    switch(op)
    { case H_RFUNCTOR:
      case H_FUNCTOR:
      case B_RFUNCTOR:
      case B_FUNCTOR:
      { word w = (word)PC[1];
	FunctorDef fd = valueFunctor(w);

	if ( fd->name == ATOM_dict &&
	     fd->arity > 1 &&
	     fd->arity%2 == 1 )
	{ int f, fields = fd->arity/2;
	  kv_code kv_buf[KV_PREALOCATED];
	  code c_buf[C_PREALLOCATED];
	  kv_code *kv_pos;
	  Code c_tmp;
	  Code fields_start, fs;

	  if ( fields <= KV_PREALOCATED )
	    kv_pos = kv_buf;
	  else if ( !(kv_pos = malloc(sizeof(kv_code)*fields)) )
	    return PL_no_memory();

	  PC = stepPC(PC);		/* skip *_FUNCTOR */
	  PC = skipArgs(PC, 1);		/* skip the type */
	  fields_start = PC;

	  for(f = 0; f < fields; f++)
	  { Code PCv = PC;

	    kv_pos[f].start = PC-fields_start;
	    PC = skipArgs(PC, 1);	/* skip value */

	    code op = fetchop(PC);
	    switch(op)
	    { case H_ATOM:
	      case B_ATOM:
	      case H_SMALLINT:
	      case B_SMALLINT:
	      { kv_pos[f].key = (word)PC[1];
		break;
	      }
	      default:
	      { if ( kv_pos != kv_buf )
		  free(kv_pos);
		return TRUE;		/* not a dict */
	      }
	    }

	    if ( !resortDictsInCodes(PCv, PC) )
	    { if ( kv_pos != kv_buf )
		free(kv_pos);
	      return FALSE;
	    }

	    PC = stepPC(PC);		/* skip key */
	    kv_pos[f].len = PC-fields_start-kv_pos[f].start;

	    DEBUG(MSG_DICT,
		  { if ( isAtom(kv_pos[f].key) )
		      Sdprintf("Got %s from %p..%p\n",
			       stringAtom(kv_pos[f].key), kv_pos[f].start, PC);
		    else
		      Sdprintf("Got %ld from %p..%p\n",
			       (long)valInt(kv_pos[f].key), kv_pos[f].start, PC);
		  });
	  }

	  qsort(kv_pos, fields, sizeof(*kv_pos), cmp_kv_pos);
	  if ( PC-fields_start <= C_PREALLOCATED )
	    c_tmp = c_buf;
	  else if ( !(c_tmp = malloc((PC-fields_start)*sizeof(code))) )
	  { if ( kv_pos != kv_buf )
	      free(kv_pos);
	    return PL_no_memory();
	  }

	  memcpy(c_tmp, fields_start, (PC-fields_start)*sizeof(code));
	  for(fs=fields_start, f = 0; f < fields; f++)
	  { size_t len = kv_pos[f].len*sizeof(code);

	    memcpy(fs, c_tmp+kv_pos[f].start, len);
	    fs += kv_pos[f].len;
	  }

	  if ( kv_pos != kv_buf )
	    free(kv_pos);
	  if ( c_tmp != c_buf )
	    free(c_tmp);

	  fix_firstvars(fields_start, PC);
	}
      }
    }
  }

  return TRUE;
}

int
resortDictsInClause(Clause clause)
{ Code PC, end;

  PC  = clause->codes;
  end = &PC[clause->code_size];

  return resortDictsInCodes(PC, end);
}


/* resortDictsInTerm() re-sorts dicts inside term.
   Used by loadQlfTerm().  Term may not be cyclic.
*/

static void
resort_dicts_in_term(Word p ARG_LD)
{
right_arg:
  deRef(p);

  if ( isTerm(*p) )
  { Functor t = valueTerm(*p);
    FunctorDef fd = valueFunctor(t->definition);
    Word ea;

    if ( fd->name == ATOM_dict && fd->arity%2 == 1 &&
	 dict_ordered(&t->arguments[1], fd->arity/2, FALSE PASS_LD) == FALSE )
    { DEBUG(MSG_DICT, Sdprintf("Re-ordering dict\n"));
      dict_order((Word)t, FALSE PASS_LD);
    }

    ea = &t->arguments[fd->arity-1];
    for(p=t->arguments; p<ea; p++)
      resort_dicts_in_term(p PASS_LD);

    goto right_arg;
  }
}


void
resortDictsInTerm(term_t t)
{ GET_LD
  Word p = valTermRef(t);

  resort_dicts_in_term(p PASS_LD);
}



		 /*******************************
		 *       PROLOG PREDICATES	*
		 *******************************/

/** is_dict(@Term)
    is_dict(@Term, ?Tag)

True if Term is a dict that belongs to Tag.

@tbd What if Term has a variable tag?
*/

static
PRED_IMPL("is_dict", 1, is_dict, 0)
{ PRED_LD
  Word p = valTermRef(A1);

  deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);
    FunctorDef fd = valueFunctor(f->definition);

    if ( fd->name == ATOM_dict &&
	 fd->arity%2 == 1 /*&&
	 dict_ordered(f->arguments+1, fd->arity/2, FALSE PASS_LD) == TRUE*/ )
      return TRUE;
  }

  return FALSE;
}


static
PRED_IMPL("is_dict", 2, is_dict, 0)
{ PRED_LD
  Word p = valTermRef(A1);

  deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);
    FunctorDef fd = valueFunctor(f->definition);

    if ( fd->name == ATOM_dict &&
	 fd->arity%2 == 1 /*&&
	 dict_ordered(f->arguments+1, fd->arity/2, FALSE PASS_LD) == TRUE*/ )
      return unify_ptrs(&f->arguments[0], valTermRef(A2),
			ALLOW_GC|ALLOW_SHIFT PASS_LD);
  }

  return FALSE;
}


/** get_dict(?Key, +Dict, ?Value)

True when Key is associated with Value in Dict. If Name is unbound, this
predicate is true for all Name/Value  pairs   in  the  dict. The order in
which these pairs are enumerated is _undefined_.
*/

static foreign_t
pl_get_dict(term_t PL__t0, int PL__ac, int ex, control_t PL__ctx)
{ PRED_LD
  int i;
  word dict;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { Word np = valTermRef(A1);

      if ( !get_dict_ex(A2, &dict, !ex PASS_LD) )
	return FALSE;

      deRef(np);
      if ( is_key(*np) )
      { Word vp;

	if ( (vp=dict_lookup_ptr(dict, *np PASS_LD)) )
	  return unify_ptrs(vp, valTermRef(A3), ALLOW_GC|ALLOW_SHIFT PASS_LD);

	if ( ex )
	  return PL_error(NULL, 0, NULL, ERR_EXISTENCE3,
			  ATOM_key, A1, A2);
	return FALSE;
      }
      if ( canBind(*np) )
      { i = 1;
	goto search;
      }
      if ( !ex )
	return PL_type_error("dict-key", A1);
      return FALSE;
    }
    case FRG_REDO:
    { Functor f;
      int arity;
      fid_t fid;
      Word p;

      i = (int)CTX_INT + 2;
      p = valTermRef(A2);
      deRef(p);
      dict = *p;

    search:
      f = valueTerm(dict);
      arity = arityFunctor(f->definition);

      if ( (fid=PL_open_foreign_frame()) )
      { for( ; i < arity; i += 2 )
	{ Word np;

	  deRef2(&f->arguments[i+1], np);	/* TBD: check type */
	  if ( unify_ptrs(&f->arguments[i], valTermRef(A3),
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


static
PRED_IMPL("get_dict", 3, get_dict, PL_FA_NONDETERMINISTIC)
{ return pl_get_dict(PL__t0, PL__ac, FALSE, PL__ctx);
}


static
PRED_IMPL("get_dict_ex", 3, get_dict_ex, PL_FA_NONDETERMINISTIC)
{ return pl_get_dict(PL__t0, PL__ac, TRUE, PL__ctx);
}


/** get_dict(+Key, +Dict, -Value, -NewDict, -NewValue) is semidet.
*/

static
PRED_IMPL("get_dict", 5, get_dict, 0)
{ PRED_LD
  term_t dt = PL_new_term_refs(4);
  term_t av = dt+1;
  word key;
  Word vp;

  if ( !get_name_ex(A1, &key PASS_LD) ||
       !(*valTermRef(av+1) = key) ||
       !get_create_dict_ex(A2, dt PASS_LD) ||
       !(vp=dict_lookup_ptr(*valTermRef(dt), key PASS_LD)) ||
       !unify_ptrs(vp, valTermRef(A3), ALLOW_GC|ALLOW_SHIFT PASS_LD) ||
       !PL_put_term(av+0, A5) )
    return FALSE;

  for(;;)
  { word new;
    int rc;

    if ( (rc = put_dict(*valTermRef(dt),
			1, valTermRef(av), &new PASS_LD)) == TRUE )
    { term_t t = dt+3;

      *valTermRef(t) = new;
      return PL_unify(A4, t);
    } else
    { if ( !makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
	return FALSE;
    }
  }
}


/** dict_create(-Dict, ?Tag, +Data) is det.

Dict represents the name-value pairs  in  Data.   If  Data  is a dict, Dict
unified  with  Data.  Otherwise,  a  new    Dict   is  created.  Suitable
representations for Data are:

  - {Name:Value, ...}
  - [Name=Value, ...]
  - [Name-Value, ...]
  - [Name(Value), ...]
*/


static
PRED_IMPL("dict_create", 3, dict_create, 0)
{ PRED_LD
  term_t m = PL_new_term_ref();

  if ( PL_get_dict_ex(A3, A2, m, DICT_GET_ALL) )
    return PL_unify(A1, m);

  return FALSE;
}


/** dict_pairs(+Dict, ?Tag, -Pairs)
    dict_pairs(-Dict, ?Tag, +Pairs)
*/

typedef struct dict_pairs_ctx
{ PL_local_data_t *ld;
  term_t head;
  term_t tail;
  term_t tmp;
} dict_pairs_ctx;


static int
put_pair(term_t key, term_t value, int last, void *closure)
{ dict_pairs_ctx *ctx = closure;
  GET_LDARG(ctx->ld);

  if ( PL_cons_functor(ctx->tmp, FUNCTOR_minus2, key, value) &&
       PL_unify_list_ex(ctx->tail, ctx->head, ctx->tail) &&
       PL_unify(ctx->head, ctx->tmp) )
    return 0;

  return -1;
}

#undef GET_LDARG


static
PRED_IMPL("dict_pairs", 3, dict_pairs, 0)
{ PRED_LD

  if ( !PL_is_variable(A1) )
  { term_t dict = PL_new_term_ref();

    if ( get_create_dict_ex(A1, dict PASS_LD) )
    { dict_pairs_ctx ctx;

      ctx.ld = LD;
      ctx.tail = PL_copy_term_ref(A3);
      ctx.head = PL_new_term_refs(2);
      ctx.tmp  = ctx.head+1;

      if ( PL_get_arg(1, dict, ctx.tmp) &&
	   PL_unify(ctx.tmp, A2) &&
	   PL_for_dict(dict, put_pair, &ctx, DICT_SORTED) == 0 )
	return PL_unify_nil_ex(ctx.tail);

      return FALSE;
    }
  } else
  { term_t m = PL_new_term_ref();

    if ( PL_get_dict_ex(A3, A2, m, DICT_GET_PAIRS) )
      return PL_unify(A1, m);
  }

  return FALSE;
}


/** put_dict(+New, +DictIn, -DictOut)

True when Dict is a copy of Dict0 where values from Dict1 replace or extend
the value set of Dict0.
*/

static
PRED_IMPL("put_dict", 3, put_dict, 0)
{ PRED_LD
  term_t dt;
  fid_t fid = PL_open_foreign_frame();

retry:
  if ( (dt = PL_new_term_refs(2)) &&
       get_create_dict_ex(A2, dt+0 PASS_LD) &&
       get_create_dict_ex(A1, dt+1 PASS_LD) )
  { Functor f2 = valueTerm(*valTermRef(dt+1));
    int arity = arityFunctor(f2->definition);
    word new;
    int rc;

    if ( (rc = put_dict(*valTermRef(dt+0),
			arity/2, &f2->arguments[1],
			&new PASS_LD)) == TRUE )
    { term_t t = PL_new_term_ref();

      *valTermRef(t) = new;
      return PL_unify(A3, t);
    } else
    { assert(rc == GLOBAL_OVERFLOW);
      PL_rewind_foreign_frame(fid);
      if ( makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
	goto retry;
    }
  }

  return FALSE;
}

/** put_dict(+Key, +Dict0, +Value, -Dict)

True when Dict is a copy of Dict0 with Name Value added or replaced.
*/

static foreign_t
put_dict4(term_t key, term_t dict, term_t value, term_t newdict ARG_LD)
{ term_t dt = PL_new_term_refs(3);
  term_t av = dt+1;
  fid_t fid = PL_open_foreign_frame();

retry:
  if ( get_create_dict_ex(dict, dt PASS_LD) &&
       get_name_ex(key, valTermRef(av+1) PASS_LD) &&
       PL_put_term(av, value) )
  { word new;
    int rc;

    if ( (rc = put_dict(*valTermRef(dt),
			1, valTermRef(av), &new PASS_LD)) == TRUE )
    { term_t t = PL_new_term_ref();

      *valTermRef(t) = new;
      return PL_unify(newdict, t);
    } else
    { if ( makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
      { PL_rewind_foreign_frame(fid);
	goto retry;
      }
    }
  }

  return FALSE;
}



static
PRED_IMPL("put_dict", 4, put_dict, 0)
{ PRED_LD

  return put_dict4(A1, A2, A3, A4 PASS_LD);
}


/** b_set_dict(+Key, !Dict, +Value)

Backtrackable destructive assignment, similar to setarg/3.
*/

#define SETDICT_BACKTRACKABLE    0x1
#define SETDICT_LINK		0x2

static int
setdict(term_t key, term_t dict, term_t value, int flags ARG_LD)
{ word k, m;
  Word val;

retry:
  val = valTermRef(value);
  deRef(val);

  if ( (flags&SETDICT_BACKTRACKABLE) )
  { if ( !hasGlobalSpace(0) )
    { int rc;

      if ( (rc=ensureGlobalSpace(0, ALLOW_GC)) != TRUE )
	return raiseStackOverflow(rc);
      goto retry;
    }
  } else
  { if ( storage(*val) == STG_GLOBAL )
    { if ( !(flags & SETDICT_LINK) )
      { term_t copy = PL_new_term_ref();

	if ( !duplicate_term(value, copy PASS_LD) )
	  return FALSE;
	value = copy;
	val = valTermRef(value);
	deRef(val);
      }
      freezeGlobal(PASS_LD1);
    }
  }

  if ( get_dict_ex(dict, &m, TRUE PASS_LD) &&
       get_name_ex(key, &k PASS_LD) )
  { Word vp;

    if ( (vp=dict_lookup_ptr(m, k PASS_LD)) )
    { if ( (flags&SETDICT_BACKTRACKABLE) )
	TrailAssignment(vp);
      unify_vp(vp, val PASS_LD);
      return TRUE;
    }

    return PL_error(NULL, 0, NULL, ERR_EXISTENCE3,
		    ATOM_key, key, dict);
  }

  return FALSE;
}


static
PRED_IMPL("b_set_dict", 3, b_set_dict, 0)
{ PRED_LD

  return setdict(A1, A2, A3, SETDICT_BACKTRACKABLE PASS_LD);
}

static
PRED_IMPL("nb_set_dict", 3, nb_set_dict, 0)
{ PRED_LD

  return setdict(A1, A2, A3, 0 PASS_LD);
}

static
PRED_IMPL("nb_link_dict", 3, nb_link_dict, 0)
{ PRED_LD

  return setdict(A1, A2, A3, SETDICT_LINK PASS_LD);
}


/** del_dict(+Key, +DictIn, ?Value, -DictOut)

True when Key-Value is in DictIn and   DictOut  contains all keys of DictIn
except for Key.
*/

static
PRED_IMPL("del_dict", 4, del_dict, 0)
{ PRED_LD
  word key;
  term_t mt = PL_new_term_ref();
  fid_t fid = PL_open_foreign_frame();

retry:
  if ( get_create_dict_ex(A2, mt PASS_LD) &&
       get_name_ex(A1, &key PASS_LD) )
  { Word vp;

    if ( (vp=dict_lookup_ptr(*valTermRef(mt), key PASS_LD)) &&
	 unify_ptrs(vp, valTermRef(A3), ALLOW_GC|ALLOW_SHIFT PASS_LD) )
    { int rc;
      word new;

      if ( (rc=del_dict(*valTermRef(mt), key, &new PASS_LD)) == TRUE )
      { term_t t = PL_new_term_ref();

	*valTermRef(t) = new;
	return PL_unify(A4, t);
      } else
      { assert(rc == GLOBAL_OVERFLOW);
	if ( makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
	{ PL_rewind_foreign_frame(fid);
	  goto retry;
	}
      }
    }
  }

  return FALSE;
}


/** select_dict(+Select, +From) is semidet.
    select_dict(+Select, +From, -Rest) is semidet.
*/

static
PRED_IMPL("select_dict", 3, select_dict, 0)
{ PRED_LD
  term_t dt = PL_new_term_refs(2);
  word r;

retry:
  if ( get_create_dict_ex(A1, dt+0 PASS_LD) &&
       get_create_dict_ex(A2, dt+1 PASS_LD) )
  { int rc = select_dict(*valTermRef(dt+0), *valTermRef(dt+1), &r PASS_LD);

    switch(rc)
    { case TRUE:
      { term_t t = PL_new_term_ref();

	*valTermRef(t) = r;
	return PL_unify(A3, t);
      }
      case FALSE:
	return rc;
      case MEMORY_OVERFLOW:
	return PL_no_memory();
      default:
	if ( !makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
	  return FALSE;
        goto retry;
    }
  }

  return FALSE;
}


static
PRED_IMPL(":<", 2, select_dict, 0)
{ PRED_LD
  term_t dt = PL_new_term_refs(2);

retry:
  if ( get_create_dict_ex(A1, dt+0 PASS_LD) &&
       get_create_dict_ex(A2, dt+1 PASS_LD) )
  { int rc = select_dict(*valTermRef(dt+0), *valTermRef(dt+1), NULL PASS_LD);

    switch(rc)
    { case TRUE:
      case FALSE:
	return rc;
      case MEMORY_OVERFLOW:
	return PL_no_memory();
      default:
	if ( !makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
	  return FALSE;
        goto retry;
    }
  }

  return FALSE;
}


static
PRED_IMPL(">:<", 2, punify_dict, 0)
{ PRED_LD
  term_t dt = PL_new_term_refs(2);

retry:
  if ( get_create_dict_ex(A1, dt+0 PASS_LD) &&
       get_create_dict_ex(A2, dt+1 PASS_LD) )
  { int rc = partial_unify_dict(*valTermRef(dt+0), *valTermRef(dt+1) PASS_LD);

    switch(rc)
    { case TRUE:
      case FALSE:
	return rc;
      case MEMORY_OVERFLOW:
	return PL_no_memory();
      default:
	if ( !makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
	  return FALSE;
        goto retry;
    }
  }

  return FALSE;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(dict)
  PRED_DEF("is_dict",	   1, is_dict,	    0)
  PRED_DEF("is_dict",	   2, is_dict,	    0)
  PRED_DEF("dict_create",  3, dict_create,  0)
  PRED_DEF("dict_pairs",   3, dict_pairs,   0)
  PRED_DEF("put_dict",	   3, put_dict,	    0)
  PRED_DEF("put_dict",	   4, put_dict,	    0)
  PRED_DEF("b_set_dict",   3, b_set_dict,   0)
  PRED_DEF("nb_set_dict",  3, nb_set_dict,  0)
  PRED_DEF("nb_link_dict", 3, nb_link_dict, 0)
  PRED_DEF("get_dict",	   3, get_dict,	    PL_FA_NONDETERMINISTIC)
  PRED_DEF("$get_dict_ex", 3, get_dict_ex,  PL_FA_NONDETERMINISTIC)
  PRED_DEF("del_dict",	   4, del_dict,	    0)
  PRED_DEF("get_dict",     5, get_dict,     0)
  PRED_DEF("select_dict",  3, select_dict,  0)
  PRED_DEF(":<",	   2, select_dict,  0)
  PRED_DEF(">:<",	   2, punify_dict,  0)
EndPredDefs
