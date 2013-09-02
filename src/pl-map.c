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

static functor_t
map_functor(int arity)
{ if ( map_functors[arity] )
    return map_functors[arity];

  map_functors[arity] = lookupFunctorDef(ATOM_map, arity);
  return map_functors[arity];
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
map_lookup_ptr() returns a pointer to the value for a given key
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Word
map_lookup_ptr(word map, word name)
{ Functor data = valueTerm(map);
  int arity = arityFunctor(data->definition);
  int l = 1, h = (arity-1)/2-1;

  assert(arity%2 == 1);

  while(l<h)
  { int m = ((l+h)/2)&~0x1;
    Word p;

    deRef2(&data->arguments[m], p);
    if ( *p == name )
      return p+1;
    if ( *p < name )
      l=m;
    else
      h=m;
  }

  return NULL;
}


static int
compare_map_entry(const void *a, const void *b)
{ Word p = a;
  Word q = b;

  deRef(p);
  deRef(q);
  return (*p<*q ? -1 : *p>*q ? 1 : 0);
}


static int
map_ordered(Word *data, int count)
{  Word n1, n2;

  deRef(data, n1);
  for(; count > 1; count--, data += 2, n1=n2)
  { deRef(data+2, n2);
    if ( *n1 >= *n2 )
      return FALSE;
  }

  return TRUE;
}


static int
map_order(word map)
{ Functor data = valueTerm(map);
  int arity = arityFunctor(data->definition);

  assert(arity%2 == 0);

  qsort(data->arguments, arity/2, sizeof(word)*2,
	compare_map_entry);

  return map_ordered(data->arguments, arity/2);
}


int
map_into(word map, int size, Word nv, word *new_map)
{ Functor data = valueTerm(map);
  int arity = arityFunctor(data->definition);
  Word new, out, in, in_end;
  int modified = FALSE;

  assert(arity%2 == 1);

  if ( size == 0 )
    return map;

  if ( gTop+1+arity+2*size > gMax )
    return GLOBAL_OVERFLOW;

  new    = gTop
  out    = new+1;
  in     = data->arguments+1;
  in_end = in+arity;
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
      return map;
    while(in < in_end)
    { *out++ = *i_name;
      *out++ = linkVal(in+1);
      in += 2;
    }
  } else
  { while(nv < nv_end)
    { *out++ = *n_name;
      *out++ = linkVal(nv+1);
      nv += 2;
    }
  }

  gTop = out;
  new->arguments[0] = linkVal(data->arguments[0]);
  new->definition = map_functor(out-(new+1));

  *new_map = consPtr(new, TAG_COMPOUND|STG_GLOBAL);

  return TRUE;
}


static int
get_name_value_ex(term_t t, word *name, Word *value)
{ Word p = valTermRef(t);

  deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);

    if ( f->definition == FUNCTOR_minus2 ||  /* Name-Value */
	 f->definition == FUNCTOR_equals2 || /* Name=Value */
	 f->definition == FUNCTOR_colon2 )   /* Name:Value */
    { Word np, vp;

      deRef2(&f->arguments[0], np);
      if ( isAtom(*np) && isTaggedInt(*np) )
      { *name = *np;
	deRef2(&f->arguments[1], vp);
	*value = vp;

	return TRUE;
      }
    } else if ( arityFunctor(f->definition) == 1 ) /* Name(Value) */
    { Word vp;

      *name = nameFunctor(f->definition);
      deRef2(&f->arguments[0], vp);
      *value = vp;
      return TRUE;
    }
  }

  return PL_type_error("name-value", t);
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
{ Word p = valTermRef(A1);
  Functor f;

  deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);
    FunctorDef fd = valueFunctor(f);

    if ( fd->name == ATOM_map &&
	 fd->arity%2 == 1 &&
	 map_ordered(f->arguments+1, (fd->arity-1)/2) )
      unify_ptrs(&fd->arguments[0], valTermRef(A2), ALLOW_GC|ALLOW_SHIFT PASS_LD);
  }

  return FALSE;
}


/** map_lookup(+Map, ?Name, ?Value)

True when Name is associated with Value in Map. If Name is unbound, this
predicate is true for all Name/Value  pairs   in  the  map. The order in
which these pairs are enumerated is _undefined_.
*/

static
PRED_IMPL("map_lookup", 3, map_lookup, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  int i;
  word map;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { Word np = valTermRef(A2);

      if ( !get_map_ex(A1, &map PASS_LD) )
	return FALSE;

      deRef(np);
      if ( isAtom(*np) )
      { Word vp;

	if ( (vp=map_lookup_ptr(map, *np)) )
	  return unify_ptrs(vp, valTermRef(A3), ALLOW_GC|ALLOW_SHIFT PASS_LD);

	return FALSE;
      }
      if ( canBind(*np) )
	i = 1;
      goto search;
    }
    case FRG_REDO:
    { Functor f;
      int arity;

      i = (int)CTX_INT + 2;
      if ( !get_map_ex(A1, &map PASS_LD) )
	return FALSE;
      f = valueTerm(map);
      arity = arityFunctor(f->definition);

    search:
      if ( (fid=PL_open_foreign_frame()) )
      { for( ; i < arity; i += 2 )
	{ Word np;

	  deRef2(&f->arguments[i], np);	/* TBD: check type */
	  if ( unify_ptrs(&f->arguments[i+1], valTermRef(A3), ALLOW_GC|ALLOW_SHIFT PASS_LD) &&
	       _PL_unify_atomic(A2, *np) )
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


/** map_create(-Map, +Data) is det.

Map represents the name-value pairs  in  Data.   If  Data  is a map, Map
unified  with  Data.  Otherwise,  a  new    Map   is  created.  Suitable
representations for Data are:

  - Class{Name:Value, ...}
  - {Name:Value, ...}
  - [Name:Value, ...]
  - [Name=Value, ...]
  - [Name-Value, ...]
  - [Name(Value), ...]
*/


static
PRED_IMPL("map_create", 2, map_create, 0)
{ PRED_LD
  term_t m = PL_new_term_ref();

  if ( PL_get_map_ex(A2, m) )
    return PL_unify(A1, m);

  return FALSE;
}


/** map_into(+Map0, +Map1, -Map)

True when Map is a copy of Map0 where values from Map1 replace or extend
the value set of Map0.
*/

static
PRED_IMPL("map_into", 3, map_into, 0)
{ PRED_LD
}
