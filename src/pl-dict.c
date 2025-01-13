/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2024, VU University Amsterdam
			      SWI-Prolog Solutions b.v.
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
#include "pl-dict.h"
#include "pl-rsort.h"
#include "pl-funct.h"
#include "pl-prims.h"
#include "pl-wam.h"
#include "pl-fli.h"
#include "pl-gc.h"
#include "pl-gvar.h"
#include "pl-copyterm.h"

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

functor_t
dict_functor(int pairs)
{ if ( pairs < CACHED_DICT_FUNCTORS )
  { if ( GD->dict.dict_functors[pairs] )
      return GD->dict.dict_functors[pairs];

    GD->dict.dict_functors[pairs] = lookupFunctorDef(ATOM_dict, pairs*2+1);
    return GD->dict.dict_functors[pairs];
  } else
  { return lookupFunctorDef(ATOM_dict, pairs*2+1);
  }
}

static bool
is_dict_functor(functor_t f)
{ FunctorDef fd = valueFunctor(f);

  return ( fd->name == ATOM_dict &&
	   fd->arity%2 == 1 );
}

#define is_dict_term(w) \
	LDFUNC(is_dict_term, w)

static bool
is_dict_term(DECL_LD word w)
{ if ( isTerm(w) )
  { Functor f = valueTerm(w);

    return is_dict_functor(f->definition);
  }

  return false;
}

		 /*******************************
		 *      LOW-LEVEL FUNCTIONS	*
		 *******************************/

#define get_dict_ex(t, dp, ex) LDFUNC(get_dict_ex, t, dp, ex)
static bool
get_dict_ex(DECL_LD term_t t, Word dp, bool ex)
{ Word p = valTermRef(t);

  deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);

    if ( is_dict_functor(f->definition) )
    { *dp = *p;
      return true;
    }
  }

  if ( !ex )
    return false;

  return PL_type_error("dict", t),false;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_create_dict_ex(DECL_LD +t, -dict) extracts a dict  from t or raises a
type error. The term reference dict contains   a  plain dict term handle
and is never a reference.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define get_create_dict_ex(t, dt) LDFUNC(get_create_dict_ex, t, dt)
static int
get_create_dict_ex(DECL_LD term_t t, term_t dt)
{ Word p = valTermRef(t);

  deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);

    if ( is_dict_functor(f->definition) )
    { *valTermRef(dt) = *p;
      return true;
    }
  }

  if ( PL_get_dict_ex(t, 0, dt, DICT_GET_ALL) )
  { assert(isTerm(*valTermRef(dt)));
    return true;
  }

  return PL_type_error("dict", t);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dict_lookup_ptr() returns a pointer to the value for a given key
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Word
dict_lookup_ptr(DECL_LD word dict, word name, size_t *arg)
{ Functor data = valueTerm(dict);
  size_t arity = arityFunctor(data->definition);
  size_t l = 1, h = arity/2;

  if ( arity == 1 )
    return NULL;			/* empty */
  assert(arity%2 == 1);

  for(;;)
  { size_t m = (l+h)/2;
    Word p;

    deRef2(&data->arguments[m*2], p);

    if ( *p == name )
    { if ( arg )
	*arg = m;
      return p-1;
    }

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

   true:  correctly ordered dict
   false: not ordered
   -1:    not a key
   -2:    duplicate key
*/

#define dict_ordered(data, count, dupl) LDFUNC(dict_ordered, data, count, dupl)
static int
dict_ordered(DECL_LD Word data, int count, Word dupl)
{ int ordered = true;
  Word n1, n2;

  if ( count > 0 )
  { data++;			/* skip to key */
    deRef2(data, n1);
    if ( !is_dict_key(*n1) )
      return -1;

    for(; count > 1; count--, data += 2, n1=n2)
    { deRef2(data+2, n2);
      if ( !is_dict_key(*n2) )
	return -1;
      if ( *n1 < *n2 )
	continue;
      if ( *n1 > *n2 )
	ordered = false;
      if ( *n1 == *n2 )
      { if ( dupl )
	{ *dupl = *n1;
	} else
	{ term_t t = PL_new_term_ref();
	  *valTermRef(t) = linkValI(n1);
	  PL_error(NULL, 0, NULL, ERR_DUPLICATE_KEY, t);
	}
	return -2;
      }
    }
  }

  return ordered;
}


static int
compare_dict_entry(const void *a, const void *b, void *arg)
{ Word p = (Word)a+1;
  Word q = (Word)b+1;

  deRef(p);
  deRef(q);

  return SCALAR_TO_CMP(*p, *q);
}


int
dict_order(DECL_LD Word dict, Word dupl)
{ Functor data = (Functor)dict;
  int arity = arityFunctor(data->definition);

  assert(arity%2 == 1);

  sort_r(data->arguments+1, arity/2, sizeof(word)*2,
	 compare_dict_entry, LD);

  return dict_ordered(data->arguments+1, arity/2, dupl);
}


/* dict_order_term_refs() orders an array of indexes into a key/value array
   of term references. Returns 0 if there are no duplicates and else the
   index of the first duplicate.
*/

typedef struct order_term_refs
{ PL_local_data_t *ld;
  term_t *av;
} order_term_refs;


#define compare_term_refs(ip1, ip2, ctx) LDFUNC(compare_term_refs, ip1, ip2, ctx)
static inline int compare_term_refs(DECL_LD const int *ip1, const int *ip2, order_term_refs *ctx);

static int
(compare_term_refs)(const void *a, const void *b, void *arg)
{ return compare_term_refs(PASS_AS_LD(((order_term_refs*)arg)->ld) a, b, arg);
}

static inline int
compare_term_refs(DECL_LD const int *ip1, const int *ip2, order_term_refs *ctx)
{ Word p = valTermRef(ctx->av[*ip1*2]);
  Word q = valTermRef(ctx->av[*ip2*2]);

  assert(!isRef(*p));
  assert(!isRef(*q));

  return (*p<*q ? -1 : *p>*q ? 1 : 0);
}


int
dict_order_term_refs(DECL_LD term_t *av, int *indexes, int count)
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


#define assign_in_dict(dp, val) LDFUNC(assign_in_dict, dp, val)
static int
assign_in_dict(DECL_LD Word dp, Word val)
{ deRef(val);

  if ( !canBind(*val) )
  { *dp = *val;
  } else if ( isAttVar(*val) )
  { *dp = makeRefG(val);
  } else
  { if ( dp < val )
    { if ( unlikely(tTop+1 >= tMax) )
	return TRAIL_OVERFLOW;
      setVar(*dp);
      Trail(val, makeRefG(dp));
    } else
    { *dp = makeRefG(val);
    }
  }

  return true;
}


#define put_dict(dict, size, nv, new_dict) \
	LDFUNC(put_dict, dict, size, nv, new_dict)

static int
put_dict(DECL_LD word dict, int size, Word nv, word *new_dict)
{ Functor data = valueTerm(dict);
  int arity = arityFunctor(data->definition);
  Word new, out, in, in_end, nv_end;
  int modified = false;

  assert(arity%2 == 1);

  if ( size == 0 )
  { *new_dict = dict;
    return true;
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
    { if ( (rc=assign_in_dict(out++, nv)) != true )
	return rc;
      *out++ = *i_name;
      if ( !modified && compareStandard(nv, in, true) )
	modified = true;
      in += 2;
      nv += 2;
    } else if ( *i_name < *n_name )
    { *out++ = linkValI(in);
      *out++ = *i_name;
      in += 2;
    } else
    { if ( (rc=assign_in_dict(out++, nv)) != true )
	return rc;
      *out++ = *n_name;
      nv += 2;
      modified = true;
    }
  }

  if ( nv == nv_end )
  { if ( !modified )
    { *new_dict = dict;
      return true;
    }
    while(in < in_end)
    { Word i_name;
      deRef2(in+1, i_name);
      *out++ = linkValI(in);
      *out++ = *i_name;
      in += 2;
    }
  } else
  { while(nv < nv_end)
    { Word n_name;
      int rc;

      deRef2(nv+1, n_name);
      if ( (rc=assign_in_dict(out++, nv)) != true )
	return rc;
      *out++ = *n_name;
      nv += 2;
    }
  }

  gTop = out;
  new[1] = linkValI(&data->arguments[0]);
  new[0] = dict_functor((out-(new+1))/2);

  *new_dict = consPtr(new, TAG_COMPOUND|STG_GLOBAL);

  return true;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Push a new dict to the global stack   that  has the same keys as `dict`,
but whose tag and values are all set to variables.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define copy_keys_dict(dict, new_dict) \
	LDFUNC(copy_keys_dict, dict, new_dict)

static int
copy_keys_dict(DECL_LD word dict, word *new_dict)
{ Functor data = valueTerm(dict);
  size_t arity = arityFunctor(data->definition);
  Word new, out, in, in_end;

  if ( gTop+1+arity > gMax )
    return GLOBAL_OVERFLOW;

  new    = gTop;
  out    = new;
  *out++ = data->definition;		/* copy C'dict functor */
  setVar(*out++);			/* set tag to var */
  in     = data->arguments+1;
  in_end = in+arity-1;

  while(in < in_end)
  { Word i_name;

    deRef2(in+1, i_name);
    setVar(*out++);
    *out++ = *i_name;
    in += 2;
  }

  gTop = out;
  *new_dict = consPtr(new, TAG_COMPOUND|STG_GLOBAL);

  return true;
}

#define same_keys_dict(dict1, dict2) \
	LDFUNC(same_keys_dict, dict1, dict2)

static bool
same_keys_dict(DECL_LD word dict1, word dict2)
{ Functor data1 = valueTerm(dict1);
  Functor data2 = valueTerm(dict2);
  size_t arity = arityFunctor(data1->definition);

  if ( data1->definition != data2->definition )
    return false;

  Word d1 = data1->arguments+1;
  Word d2 = data2->arguments+1;
  Word dend = d1+arity-1;

  for(; d1 < dend; d1+=2, d2+=2)
  { Word k1, k2;

    deRef2(d1+1, k1);
    deRef2(d2+1, k2);
    if ( *k1 != *k2 )
      return false;
  }

  return true;
}


#define del_dict(dict, key, new_dict) LDFUNC(del_dict, dict, key, new_dict)
static int
del_dict(DECL_LD word dict, word key, word *new_dict)
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
    { *out++ = linkValI(in);
      *out++ = *i_name;
    }
    in += 2;
  }

  gTop = out;
  new[1] = linkValI(&data->arguments[0]); /* tag */
  new[0] = dict_functor((out-(new+1))/2); /* arity */

  *new_dict = consPtr(new, TAG_COMPOUND|STG_GLOBAL);

  return true;
}


/* partial_unify_dict(dict1, dict2) unifies all common elements of two
   dicts.  It returns true on success, false on a failed unification
   and *_OVERFLOW on some memory overflow
*/

#define partial_unify_dict(dict1, dict2) LDFUNC(partial_unify_dict, dict1, dict2)
static int
partial_unify_dict(DECL_LD word dict1, word dict2)
{ Functor d1 = valueTerm(dict1);
  Functor d2 = valueTerm(dict2);
  Word in1  = d1->arguments;
  Word in2  = d2->arguments;
  Word end1 = in1+arityFunctor(d1->definition);
  Word end2 = in2+arityFunctor(d2->definition);
  int rc;

  /* unify the tages */
  if ( (rc=unify_ptrs(in1, in2, ALLOW_RETCODE)) != true )
    return rc;

  /* advance to first v+k entry */
  in1++;
  in2++;

  while(in1 < end1 && in2 < end2)
  { Word n1, n2;

    deRef2(in1+1, n1);
    deRef2(in2+1, n2);
    if ( *n1 == *n2 )
    { if ( (rc = unify_ptrs(in1, in2, ALLOW_RETCODE)) != true )
	return rc;
      in1 += 2;
      in2 += 2;
    } else if ( *n1 < *n2 )
    { in1 += 2;
    } else
    { in2 += 2;
    }
  }

  return true;
}


/* select_dict() demands del to be a sub-dict of from and assigns
   all remaining values in new.

   Note that unify_ptrs() can push data onto the global stack in
   case it encounters attributed variables.  Therefore we need a
   two pass process.
*/

#define unify_left_dict(del, from) \
	LDFUNC(unify_left_dict, del, from)

static int
unify_left_dict(DECL_LD word del, word from)
{ Functor dd = valueTerm(del);
  Functor fd = valueTerm(from);
  Word din  = dd->arguments;
  Word fin  = fd->arguments;
  Word dend = din+arityFunctor(dd->definition);
  Word fend = fin+arityFunctor(fd->definition);
  int rc;

  /* unify the tags */
  if ( (rc=unify_ptrs(din, fin, ALLOW_RETCODE)) != true )
    return rc;

  /* advance to first v+k entry */
  din++;
  fin++;

  while(din < dend && fin < fend)
  { Word d, f;

    deRef2(din+1, d);
    deRef2(fin+1, f);

    if ( *d == *f )		/* same keys */
    { if ( (rc = unify_ptrs(din, fin, ALLOW_RETCODE)) != true )
	return rc;
      din += 2;
      fin += 2;
    } else if ( *d < *f )
    { return false;
    } else
    { fin += 2;
    }
  }

  return din == dend;
}


#define select_dict(del, from, new_dict) \
	LDFUNC(select_dict, del, from, new_dict)

static int
select_dict(DECL_LD word del, word from, word *new_dict)
{ Functor dd = valueTerm(del);
  Functor fd = valueTerm(from);
  Word din  = dd->arguments;
  Word fin  = fd->arguments;
  Word dend = din+arityFunctor(dd->definition);
  Word fend = fin+arityFunctor(fd->definition);
  int buf[256];
  bit_vector *keep = NULL;
  int rc;

  /* unify the tags */
  if ( (rc=unify_ptrs(din, fin, ALLOW_RETCODE)) != true )
    return rc;

  /* advance to first v+k entry */
  din++;
  fin++;

  for(size_t i=0; din < dend && fin < fend; i++, fin += 2)
  { Word d, f;

    deRef2(din+1, d);
    deRef2(fin+1, f);

    if ( *d == *f )		/* same keys */
    { if ( (rc = unify_ptrs(din, fin, ALLOW_RETCODE)) != true )
	return rc;
      din += 2;
    } else if ( *d < *f )
    { return false;
    } else
    { if ( !keep )
      { size_t entries = arityFunctor(fd->definition)/2;
	size_t sz = sizeof_bitvector(entries);

	if ( sz <= sizeof(buf) )
	  keep = (bit_vector*)buf;
	else if ( !(keep = malloc(sz)) )
	  return MEMORY_OVERFLOW;
	init_bitvector(keep, entries);
      }
      set_bit(keep, i);
    }
  }

  if ( din == dend )		/* unification succeeded */
  { size_t nsize = (keep ? popcount_bitvector(keep) : 0) + (fend-fin)/2;

    if ( gTop+nsize*2+2 <= gMax )
    { Word out = gTop;

      *new_dict = consPtr(out, TAG_COMPOUND|STG_GLOBAL);
      *out++ = dict_functor(nsize);
      setVar(*out++);

      Word fe = fin;
      fin  = fd->arguments+1;
      for(size_t i=0; fin < fend; i++, fin += 2)
      { if ( fin >= fe || (keep && true_bit(keep, i)) )
	{ Word f;

	  deRef2(fin+1, f);
	  *out++ = linkValI(fin);
	  *out++ = *f;
	}
      }
      gTop = out;
      rc = true;
    } else
    { rc = GLOBAL_OVERFLOW;
    }
  } else
  { rc = false;
  }

  if ( keep && keep != (bit_vector*)buf )
    free(keep);

  return rc;
}


#define get_name_ex(t, np) LDFUNC(get_name_ex, t, np)
static int
get_name_ex(DECL_LD term_t t, Word np)
{ Word p = valTermRef(t);

  deRef(p);
  if ( is_dict_key(*p) )
  { *np = *p;
    return true;
  }

  PL_type_error("dict-key", t);
  return false;
}


#define get_name_value(p, name, value, m, flags) LDFUNC(get_name_value, p, name, value, m, flags)
static int
get_name_value(DECL_LD Word p, Word name, Word value, mark *m, int flags)
{ const char *type;

  deRef(p);

  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);

    if ( (f->definition == FUNCTOR_minus2  && (flags&DICT_GET_PAIRS)) ||
	 (f->definition == FUNCTOR_equals2 && (flags&DICT_GET_EQUALS)) ||
	 (f->definition == FUNCTOR_colon2  && (flags&DICT_GET_COLON)))
    { Word np, vp;

      deRef2(&f->arguments[0], np);
      if ( is_dict_key(*np) )
      { *name = *np;
	deRef2(&f->arguments[1], vp);
	*value = linkValI(vp);

	return true;
      } else
      { Undo(*m);
	PL_type_error("dict-key", pushWordAsTermRef(np));
	popTermRef();

	return false;
      }
    } else if ( arityFunctor(f->definition) == 1 &&
		(flags&DICT_GET_TERM) ) /* Name(Value) */
    { Word vp;

      *name = nameFunctor(f->definition);
      deRef2(&f->arguments[0], vp);
      *value = linkValI(vp);
      return true;
    }
  }

  if ( flags == DICT_GET_PAIRS )
    type = "pair";
  else
    type = "key-value";

  Undo(*m);
  PL_type_error(type, pushWordAsTermRef(p));
  popTermRef();

  return false;				/* type error */
}



		 /*******************************
		 *	 FOREIGN SUPPORT	*
		 *******************************/

int
PL_is_dict(DECL_LD term_t t)
{ Word p = valTermRef(t);

  deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);
    FunctorDef fd = valueFunctor(f->definition);
    word dupl;

    if ( fd->name == ATOM_dict &&
	 fd->arity%2 == 1 &&
	 dict_ordered(f->arguments+1, fd->arity/2, &dupl) == true )
      return true;
  }

  return false;
}

API_STUB(bool)
(PL_is_dict)(term_t t)
( valid_term_t(t);
  return PL_is_dict(t);
)


/* Turn data into a dict if it is not already a dict.
 */

static int
PL_get_dict_ex(term_t data, term_t tag, term_t dict, int flags)
{ GET_LD
  word dupl;

  if ( PL_is_dict(data) )
  { PL_put_term(dict, data);
    return true;
  }

  if ( PL_is_list(data) )
  { intptr_t len = lengthList(data, true);
    Word ap, dp, tail;
    mark m;
    int rc;

    if ( len < 0 )
      return false;			/* not a proper list */

    if ( unlikely(tTop+1 >= tMax) )
    { if ( !makeMoreStackSpace(TRAIL_OVERFLOW, ALLOW_GC|ALLOW_SHIFT) )
	return false;
    }
    if ( (rc=ensureGlobalSpace(len*2+2, ALLOW_GC)) != true )
      return raiseStackOverflow(rc);
    ap = gTop;
    Mark(m);
    dp = ap;
    *ap++ = dict_functor(len);
    if ( tag )
    { Word cp = valTermRef(tag);

      deRef(cp);
      if ( needsRef(*cp) )
      { if ( isVar(*cp) )
	{ setVar(*ap);
	  Trail(cp, makeRefG(ap));
	} else
	{ assert(onStack(global, cp));
	  *ap = makeRefG(cp);
	}
      } else
      { *ap = *cp;
      }
    } else
    { setVar(*ap);
    }
    ap++;

    tail = valTermRef(data);
    deRef(tail);
    while( isList(*tail) )
    { Word head = HeadList(tail);

      if ( !get_name_value(head, ap+1, ap, &m, flags) )
	return false;
      ap += 2;
      tail = TailList(tail);
      deRef(tail);
    }

    if ( (rc=dict_order(dp, &dupl)) == true )
    { gTop = ap;
      *valTermRef(dict) = consPtr(dp, TAG_COMPOUND|STG_GLOBAL);
      DEBUG(CHK_SECURE, checkStacks(NULL));
      return true;
    } else
    { term_t ex;

      assert(rc == -2);
      Undo(m);
      return ( (ex = PL_new_term_ref()) &&
	       PL_unify_atomic(ex, dupl) &&
	       PL_error(NULL, 0, NULL, ERR_DUPLICATE_KEY, ex) );
    }
  }					/* TBD: {name:value, ...} */

  return PL_type_error("dict-data", data);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_for_dict()  runs  func  on  each  key-value  pair  in  dict.  Returns
immediately with the return value of func   if func returns non-zero. If
the flag PL_FOR_DICT_SORTED is given, the  key-value pairs are called in
the standard order of terms.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct cmp_dict_index_data
{ Word  data;
  int  *indexes;
  PL_local_data_t *ld;
} cmp_dict_index_data;

#define cmp_dict_index(ip1, ip2, ctx) LDFUNC(cmp_dict_index, ip1, ip2, ctx)
static inline int cmp_dict_index(DECL_LD const int *ip1, const int *ip2, cmp_dict_index_data *ctx);

static int
(cmp_dict_index)(const void *a1, const void *a2, void *arg)
{ return cmp_dict_index(PASS_AS_LD(((cmp_dict_index_data*)arg)->ld) a1, a2, arg);
}

static inline int
cmp_dict_index(DECL_LD const int *ip1, const int *ip2, cmp_dict_index_data *ctx)
{ Word p = &ctx->data[*ip1*2+1];
  Word q = &ctx->data[*ip2*2+1];
  int rc;

  deRef(p);
  deRef(q);

  if ( *p == *q )
  { rc = CMP_EQUAL;
  } else
  { if ( isAtom(*p) )
    { if ( isAtom(*q) )
	rc = compareAtoms(word2atom(*p), word2atom(*q));
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

typedef struct dict_helper
{ int (*function)(term_t key, term_t value, void *closure);
  void *closure;
} dict_helper;

#define public_for_dict_helper(k,v,l,c) \
	LDFUNC(public_for_dict_helper, k,v,l,c)

static int
public_for_dict_helper(DECL_LD term_t key, term_t value, int last, void *closure)
{ struct dict_helper *h = closure;

  return (*h->function)(key, value, h->closure);
}

int
PL_for_dict(term_t dict,
	    int (*func)(term_t key, term_t value, void *closure),
	    void *closure,
	    int flags)
{ GET_LD
  struct dict_helper h = {.function = func, .closure = closure};

  return _PL_for_dict(dict, public_for_dict_helper, &h, flags);
}


int
pl_for_dict(DECL_LD term_t dict,
	   int LDFUNCP (*func)(DECL_LD term_t key, term_t value, int last, void *closure),
	   void *closure,
	   int flags)
{ term_t av = PL_new_term_refs(2);
  int i, arity, pairs;
  Word p = valTermRef(dict);
  int index_buf[256];
  int *indexes = NULL;
  int rc = 0;

  deRef(p);
  arity = arityTerm(*p);
  pairs = arity/2;

  if ( (flags&PL_FOR_DICT_SORTED) )
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
    *valTermRef(av+0) = linkValI(&f->arguments[in+1]);
    *valTermRef(av+1) = linkValI(&f->arguments[in]);

    if ( (rc=LDFUNCP(*func)(av+0, av+1, ++i == pairs, closure)) != 0 )
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

This predicate is called from pl-qlf.c after   reloading a clause from a
.qlf file or state if pl-qlf.c  detected   a  dict inside the clause. It
identifies the code ranges that constitute the k-v pairs in the dict and
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

static int
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
	  int h_void = 0;

	  if ( fields <= KV_PREALOCATED )
	    kv_pos = kv_buf;
	  else if ( !(kv_pos = malloc(sizeof(kv_code)*fields)) )
	    return PL_no_memory();

	  PC = stepPC(PC);		/* skip *_FUNCTOR */
	  if ( fetchop(PC) == H_VOID_N ) /* deal with _{key:_, ...} */
	  { assert(PC[1] == (code)2);
	    PC[0] = encode(H_VOID);
	    PC[1] = encode(H_VOID);
	  }
	  PC = skipArgs(PC, 1, &h_void); /* skip the type */
	  fields_start = PC;

	  for(f = 0; f < fields; f++)
	  { Code PCv = PC;

	    kv_pos[f].start = PC-fields_start;
	    PC = skipArgs(PC, 1, &h_void); /* skip value */

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
		return true;		/* not a dict */
	      }
	    }

	    if ( !resortDictsInCodes(PCv, PC) )
	    { if ( kv_pos != kv_buf )
		free(kv_pos);
	      return false;
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

  return true;
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

#define resort_dicts_in_term(p) LDFUNC(resort_dicts_in_term, p)
static void
resort_dicts_in_term(DECL_LD Word p)
{
right_arg:
  deRef(p);

  if ( isTerm(*p) )
  { Functor t = valueTerm(*p);
    FunctorDef fd = valueFunctor(t->definition);
    Word ea;
    word dupl;

    if ( fd->name == ATOM_dict && fd->arity > 1 && fd->arity%2 == 1 &&
	 dict_ordered(&t->arguments[1], fd->arity/2, &dupl) == false )
    { DEBUG(MSG_DICT, Sdprintf("Re-ordering dict\n"));
      dict_order((Word)t, &dupl);
    }

    ea = &t->arguments[fd->arity-1];
    for(p=t->arguments; p<ea; p++)
      resort_dicts_in_term(p);

    goto right_arg;
  }
}


void
resortDictsInTerm(term_t t)
{ GET_LD
  Word p = valTermRef(t);

  resort_dicts_in_term(p);
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
  return is_dict_term(*p);
}


static
PRED_IMPL("is_dict", 2, is_dict, 0)
{ PRED_LD
  Word p = valTermRef(A1);

  deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);

    if ( is_dict_functor(f->definition) )
      return unify_ptrs(&f->arguments[0], valTermRef(A2),
			ALLOW_GC|ALLOW_SHIFT);
  }

  return false;
}


/** get_dict(?Key, +Dict, ?Value)

True when Key is associated with Value in Dict. If Name is unbound, this
predicate is true for all Name/Value  pairs   in  the  dict. The order in
which these pairs are enumerated is _undefined_.
*/

static foreign_t
pl_get_dict(term_t PL__t0, size_t PL__ac, int ex, control_t PL__ctx)
{ PRED_LD
  int i;
  word dict;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { Word np = valTermRef(A1);

      if ( !get_dict_ex(A2, &dict, !ex) )
	return false;

      deRef(np);
      if ( is_dict_key(*np) )
      { Word vp;

	if ( (vp=dict_lookup_ptr(dict, *np, NULL)) )
	  return unify_ptrs(vp, valTermRef(A3), ALLOW_GC|ALLOW_SHIFT);

	if ( ex )
	  return PL_error(NULL, 0, NULL, ERR_EXISTENCE3,
			  ATOM_key, A1, A2);
	return false;
      }
      if ( canBind(*np) )
      { i = 1;
	goto search;
      }
      if ( !ex )
	return PL_type_error("dict-key", A1);
      return false;
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
			  ALLOW_GC|ALLOW_SHIFT) &&
	       PL_unify_atomic(A1, *np) )
	  { PL_close_foreign_frame(fid);

	    if ( i+2 < arity )
	      ForeignRedoInt(i);
	    else
	      return true;
	  } else if ( exception_term )
	  { PL_close_foreign_frame(fid);
	    return false;
	  }
	  PL_rewind_foreign_frame(fid);
	}
	PL_close_foreign_frame(fid);
      }
      return false;
    }
    default:
      return true;
  }
}


static
PRED_IMPL("get_dict", 3, get_dict, PL_FA_NONDETERMINISTIC)
{ return pl_get_dict(PL__t0, PL__ac, false, PL__ctx);
}


static
PRED_IMPL("get_dict_ex", 3, get_dict_ex, PL_FA_NONDETERMINISTIC)
{ return pl_get_dict(PL__t0, PL__ac, true, PL__ctx);
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

  if ( !get_name_ex(A1, &key) ||
       !(*valTermRef(av+1) = key) ||
       !get_create_dict_ex(A2, dt) ||
       !(vp=dict_lookup_ptr(*valTermRef(dt), key, NULL)) ||
       !unify_ptrs(vp, valTermRef(A3), ALLOW_GC|ALLOW_SHIFT) ||
       !PL_put_term(av+0, A5) )
    return false;

  for(;;)
  { word new;
    int rc;

    if ( (rc = put_dict(*valTermRef(dt),
			1, valTermRef(av), &new)) == true )
    { term_t t = dt+3;

      *valTermRef(t) = new;
      return PL_unify(A4, t);
    } else
    { if ( !makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
	return false;
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

  return false;
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

#define put_pair(key, value, last, closure) LDFUNC(put_pair, key, value, last, closure)

static int
put_pair(DECL_LD term_t key, term_t value, int last, void *closure)
{ dict_pairs_ctx *ctx = closure;

  if ( PL_cons_functor(ctx->tmp, FUNCTOR_minus2, key, value) &&
       PL_unify_list_ex(ctx->tail, ctx->head, ctx->tail) &&
       PL_unify(ctx->head, ctx->tmp) )
    return 0;

  return -1;
}


static
PRED_IMPL("dict_pairs", 3, dict_pairs, 0)
{ PRED_LD

  if ( !PL_is_variable(A1) )
  { term_t dict = PL_new_term_ref();

    if ( get_create_dict_ex(A1, dict) )
    { dict_pairs_ctx ctx;

      ctx.ld = LD;
      ctx.tail = PL_copy_term_ref(A3);
      ctx.head = PL_new_term_refs(2);
      ctx.tmp  = ctx.head+1;

      if ( _PL_get_arg(1, dict, ctx.tmp) &&
	   PL_unify(ctx.tmp, A2) &&
	   _PL_for_dict(dict, put_pair, &ctx, PL_FOR_DICT_SORTED) == 0 )
	return PL_unify_nil_ex(ctx.tail);

      return false;
    }
  } else
  { term_t m = PL_new_term_ref();

    if ( PL_get_dict_ex(A3, A2, m, DICT_GET_PAIRS) )
      return PL_unify(A1, m);
  }

  return false;
}

/** dict_same_keys(?Dict1, ?Dict2) is semidet.
*/

#define unify_dict_copy(t, dt, dict)		\
	LDFUNC(unify_dict_copy, t, dt, dict)

static bool
unify_dict_copy(DECL_LD term_t t, term_t dt, word dict)
{ term_t tmp = PL_new_term_ref(); /* safe, we can allocate 10 */
  word copy;
  int rc;

  for(;;)
  { if ( (rc=copy_keys_dict(dict, &copy)) == true )
    { *valTermRef(tmp) = copy;
      return PL_unify(tmp, t);
    }

    if ( !makeMoreStackSpace(rc, ALLOW_SHIFT|ALLOW_GC) )
      return false;

    Word p = valTermRef(dt);
    deRef(p);
    dict = *p;
  }
}

static
PRED_IMPL("dict_same_keys", 2, dict_same_keys, 0)
{ PRED_LD
  Word d1 = valTermRef(A1);
  Word d2 = valTermRef(A2);

  deRef(d1);
  deRef(d2);

  if ( is_dict_term(*d1) )
  { if ( is_dict_term(*d2) )
      return same_keys_dict(*d1, *d2);
    else if ( canBind(*d2) )
      return unify_dict_copy(A2, A1, *d1);
    else
      return PL_type_error("dict", A2);
  } else if ( is_dict_term(*d2) )
  { if ( canBind(*d1) )
      return unify_dict_copy(A1, A2, *d2);
    else
      return PL_type_error("dict", A1);
  }

  return PL_type_error("dict", A1);
}



/** put_dict(+New, +DictIn, -DictOut)

True when Dict is a copy of Dict0 where values from Dict1 replace or extend
the value set of Dict0.
*/

static
PRED_IMPL("put_dict", 3, put_dict, 0)
{ PRED_LD
  term_t dt;

  if ( (dt = PL_new_term_refs(2)) &&
       get_create_dict_ex(A2, dt+0) &&
       get_create_dict_ex(A1, dt+1) )
  { retry:
    Mark(fli_context->mark);
    Functor f2 = valueTerm(*valTermRef(dt+1));
    int arity = arityFunctor(f2->definition);
    word new;
    int rc;

    if ( (rc = put_dict(*valTermRef(dt+0),
			arity/2, &f2->arguments[1],
			&new)) == true )
    { term_t t = PL_new_term_ref();

      *valTermRef(t) = new;
      return PL_unify(A3, t);
    } else
    { assert(rc == GLOBAL_OVERFLOW);
      Undo(fli_context->mark);
      if ( makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
	goto retry;
    }
  }

  return false;
}

/** put_dict(+Key, +Dict0, +Value, -Dict)

True when Dict is a copy of Dict0 with Name Value added or replaced.
*/

#define put_dict4(key, dict, value, newdict) \
	LDFUNC(put_dict4, key, dict, value, newdict)

static foreign_t
put_dict4(DECL_LD term_t key, term_t dict, term_t value, term_t newdict)
{ term_t dt = PL_new_term_refs(3);
  term_t av = dt+1;

  if ( get_create_dict_ex(dict, dt) &&
       get_name_ex(key, valTermRef(av+1)) &&
       PL_put_term(av, value) )
  { retry:
    Mark(fli_context->mark);
    word new;
    int rc;

    if ( (rc = put_dict(*valTermRef(dt),
			1, valTermRef(av), &new)) == true )
    { term_t t = PL_new_term_ref();

      *valTermRef(t) = new;
      return PL_unify(newdict, t);
    } else
    { if ( makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
      { Undo(fli_context->mark);
	goto retry;
      }
    }
  }

  return false;
}



static
PRED_IMPL("put_dict", 4, put_dict, 0)
{ PRED_LD

  return put_dict4(A1, A2, A3, A4);
}


/** b_set_dict(+Key, !Dict, +Value)

Backtrackable destructive assignment, similar to setarg/3.
*/

#define SETDICT_BACKTRACKABLE    0x1
#define SETDICT_LINK		0x2

#define setdict(key, dict, value, flags) \
	LDFUNC(setdict, key, dict, value, flags)

static bool
setdict(DECL_LD term_t key, term_t dict, term_t value, unsigned int flags)
{ word k, m;

  if ( get_dict_ex(dict, &m, true) &&
       get_name_ex(key, &k) )
  { Word vp;
    size_t arg;

    if ( (vp=dict_lookup_ptr(m, k, &arg)) )
      return setarg(arg*2, dict, value, flags);

    return PL_error(NULL, 0, NULL, ERR_EXISTENCE3,
		    ATOM_key, key, dict);
  }

  return false;
}


static
PRED_IMPL("b_set_dict", 3, b_set_dict, 0)
{ PRED_LD

  return setdict(A1, A2, A3, SETARG_BACKTRACKABLE);
}

static
PRED_IMPL("nb_set_dict", 3, nb_set_dict, 0)
{ PRED_LD

  return setdict(A1, A2, A3, 0);
}

static
PRED_IMPL("nb_link_dict", 3, nb_link_dict, 0)
{ PRED_LD

  return setdict(A1, A2, A3, SETARG_LINK);
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
  if ( get_create_dict_ex(A2, mt) &&
       get_name_ex(A1, &key) )
  { Word vp;

    if ( (vp=dict_lookup_ptr(*valTermRef(mt), key, NULL)) &&
	 unify_ptrs(vp, valTermRef(A3), ALLOW_GC|ALLOW_SHIFT) )
    { int rc;
      word new;

      if ( (rc=del_dict(*valTermRef(mt), key, &new)) == true )
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

  return false;
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
  if ( get_create_dict_ex(A1, dt+0) &&
       get_create_dict_ex(A2, dt+1) )
  { Mark(fli_context->mark);
    int rc = select_dict(*valTermRef(dt+0), *valTermRef(dt+1), &r);

    switch(rc)
    { case true:
      { term_t t = PL_new_term_ref();

	*valTermRef(t) = r;
	return PL_unify(A3, t);
      }
      case false:
	return rc;
      case MEMORY_OVERFLOW:
	return PL_no_memory();
      default:
	Undo(fli_context->mark);
	if ( !makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
	  return false;
        goto retry;
    }
  }

  return false;
}


static
PRED_IMPL(":<", 2, unify_left_dict, 0)
{ PRED_LD
  term_t dt = PL_new_term_refs(2);

retry:
  if ( get_create_dict_ex(A1, dt+0) &&
       get_create_dict_ex(A2, dt+1) )
  { Mark(fli_context->mark);
    int rc = unify_left_dict(*valTermRef(dt+0), *valTermRef(dt+1));

    switch(rc)
    { case true:
      case false:
	return rc;
      case MEMORY_OVERFLOW:
	return PL_no_memory();
      default:
	Undo(fli_context->mark);
	if ( !makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
	  return false;
        goto retry;
    }
  }

  return false;
}


static
PRED_IMPL(">:<", 2, punify_dict, 0)
{ PRED_LD
  term_t dt = PL_new_term_refs(2);

retry:
  if ( get_create_dict_ex(A1, dt+0) &&
       get_create_dict_ex(A2, dt+1) )
  { int rc = partial_unify_dict(*valTermRef(dt+0), *valTermRef(dt+1));

    switch(rc)
    { case true:
      case false:
	return rc;
      case MEMORY_OVERFLOW:
	return PL_no_memory();
      default:
	if ( !makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
	  return false;
        goto retry;
    }
  }

  return false;
}

/** '$get_dict_kv'(+Index, +Dict, -K, -V)
    '$get_dict_kv'(+Index, +Dict1, +Dict2, -K, -V1, -V2)
*/

#define get_dict_kv(t0, arity)			\
	LDFUNC(get_dict_kv, t0, arity)

static bool
get_dict_kv(DECL_LD term_t t0, int arity)
{ size_t i;

  if ( !PL_get_size_ex(t0, &i) || i == 0 )
    return false;

  Word p = valTermRef(t0+1);
  deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);
    size_t darity = arityFunctor(f->definition);

    if ( !is_dict_functor(f->definition) )
      return PL_type_error("dict", t0+1);

    if ( i > darity/2 )
      return false;
    i--;

    size_t dict_count = (arity-2)/2;
    size_t key_arg = t0+1+dict_count;
    term_t value = PL_new_term_ref();
    term_t key = PL_new_term_ref();

    for(size_t di=0; di < dict_count; di++)
    { term_t dict = t0+1+di;

      if ( di > 0 && !PL_is_functor(dict, f->definition) )
      { if ( !PL_is_dict(dict) )
	  return PL_type_error("dict", dict);
	else
	  return PL_domain_error("compatible_dict", dict);
      }
      _PL_get_arg(2+i*2, dict, value);
      _PL_get_arg(3+i*2, dict, key);

      if ( !PL_unify(key_arg, key) )
      { if ( di == 0 )
	  return false;
	return PL_domain_error("compatible_dict", dict);
      }

      if ( !PL_unify(key_arg+1+di, value) )
	return false;
    }

    return true;
  }

  return PL_type_error("dict", t0+1);
}

static
PRED_IMPL("$get_dict_kv", 4, get_dict_kv, 0)
{ PRED_LD

  return get_dict_kv(A1, 4);
}

static
PRED_IMPL("$get_dict_kv", 6, get_dict_kv, 0)
{ PRED_LD

  return get_dict_kv(A1, 6);
}

static
PRED_IMPL("$get_dict_kv", 8, get_dict_kv, 0)
{ PRED_LD

  return get_dict_kv(A1, 8);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Part of FLI
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
PL_get_dict_key(atom_t key, term_t dict, term_t value)
{ GET_LD
  word d;
  Word vp;

  valid_dict_key(key);
  valid_term_t(dict);
  valid_term_t(value);

  if ( !get_dict_ex(dict, &d, false) )
    return false;
  if ( (vp=dict_lookup_ptr(d, key, NULL)) )
  { *valTermRef(value) = linkValI(vp);
    return true;
  }

  return false;
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(dict)
  PRED_DEF("is_dict",	     1, is_dict,	0)
  PRED_DEF("is_dict",	     2, is_dict,	0)
  PRED_DEF("dict_create",    3, dict_create,    0)
  PRED_DEF("dict_pairs",     3, dict_pairs,     0)
  PRED_DEF("dict_same_keys", 2, dict_same_keys, 0)
  PRED_DEF("put_dict",	     3, put_dict,       0)
  PRED_DEF("put_dict",	     4, put_dict,       0)
  PRED_DEF("b_set_dict",     3, b_set_dict,     0)
  PRED_DEF("nb_set_dict",    3, nb_set_dict,    0)
  PRED_DEF("nb_link_dict",   3, nb_link_dict,   0)
  PRED_DEF("get_dict",	     3, get_dict,	PL_FA_NONDETERMINISTIC)
  PRED_DEF("$get_dict_ex",   3, get_dict_ex,    PL_FA_NONDETERMINISTIC)
  PRED_DEF("del_dict",	     4, del_dict,	0)
  PRED_DEF("get_dict",       5, get_dict,       0)
  PRED_DEF("select_dict",    3, select_dict,    0)
  PRED_DEF(":<",	     2, unify_left_dict,0)
  PRED_DEF(">:<",	     2, punify_dict,    0)
  PRED_DEF("$get_dict_kv",   4, get_dict_kv,    0)
  PRED_DEF("$get_dict_kv",   6, get_dict_kv,    0)
  PRED_DEF("$get_dict_kv",   8, get_dict_kv,    0)
EndPredDefs
