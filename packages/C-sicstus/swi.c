/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

#define _PL_INLINE
#include "SWI-Stream.h"
#include "swi.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

		 /*******************************
		 *	       ATOMS		*
		 *******************************/
static atom_t
_SP_atom_nil()
{ static atom_t nil = 0;

  if ( !nil )
  { nil = SP_atom_from_string("[]");
    SP_register_atom(nil);
  }

  return nil;
}

atom_t
_SP_atom_user()
{ static atom_t user = 0;

  if ( !user )
  { user = SP_atom_from_string("user");
    SP_register_atom(user);
  }

  return user;
}

static atom_t
_SP_atom_module()
{ static atom_t module = 0;

  if ( !module )
  { module = SP_atom_from_string(":");
    SP_register_atom(module);
  }

  return module;
}

atom_t
_SP_atom_record()
{ static atom_t record = 0;

  if ( !record )
  { record = SP_atom_from_string("PL_record");
    SP_register_atom(record);
  }

  return record;
}

#define ATOM_nil    _SP_atom_nil()
#define ATOM_module _SP_atom_module()
#define ATOM_record _SP_atom_record()
#define ATOM_user   _SP_atom_user()

		 /*******************************
		 *	      FUNCTORS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Functors are unique in SWI-Prolog and don't exist in SICStus. This layer
guarantees full emulation, but at a certain  price. Probably not too bad
if you are concerned with calling the best functions.

For example, one should use PL_is_functor() rather then PL_get_fucntor()
and compare the result.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define FUNCTOR_HASHSIZE 512

static functor_t functor_table[FUNCTOR_HASHSIZE];

static int
functor_hash_key(atom_t name, int arity)
{ return (name + arity) % (FUNCTOR_HASHSIZE-1);
}


functor_t
PL_new_functor(atom_t name, int arity)
{ int k = functor_hash_key(name, arity);
  functor_t l, n;

  for( l = functor_table[k]; l; l = l->next )
  { if ( l->name == name && l->arity == arity )
      return l;
  }

  n = SP_malloc(sizeof(*n));
  n->name = name;
  n->arity = arity;
  n->next = functor_table[k];
  functor_table[k] = n;
  
  return n;
}

		 /*******************************
		 *	      BUFFERS		*
		 *******************************/

#define BUFFER_RING_SIZE 	4	/* foreign buffer ring (pl-fli.c) */

typedef struct
{ char *	base;			/* allocated base */
  char *	top;			/* pointer to top */
  char *	max;			/* current location */
  char		static_buffer[sizeof(char *)];
} buffer, *Buffer;

#define initBuffer(b)            ((b)->base = (b)->top = (b)->static_buffer, \
				  (b)->max = (b)->base + \
				  sizeof((b)->static_buffer))
#define emptyBuffer(b)           ((b)->top  = (b)->base)
#define baseBuffer(b, type)	 ((type *) (b)->base)
#define addMultipleBuffer(b, ptr, times, type) \
	do \
	{ int len = sizeof(type) * (times); \
	  if ( (b)->top + len > (b)->max ) \
	    growBuffer((Buffer)b, len); \
	  memcpy((b)->top, ptr, len); \
          (b)->top += len; \
	} while(0)

static void
outOfCore()
{ SP_fprintf(SP_stderr, "FATAL ERROR: Out of memory\n");
  exit(1);
}


void
growBuffer(Buffer b, long int minfree)
{ long osz = b->max - b->base, sz = osz;
  long top = b->top - b->base;

  while( top + minfree > sz )
    sz *= 2;

  if ( b->base != b->static_buffer )
  { b->base = SP_realloc(b->base, sz);
    if ( !b->base )
      outOfCore();
  } else
  { char *old = b->base;
    b->base = SP_malloc(sz);
    if ( !b->base )
      outOfCore();
    memcpy(b->base, old, osz);
  }

  b->top = b->base + top;
  b->max = b->base + sz;
}


static buffer	discardable_buffer;	/* PL_*() character buffers */
static buffer	buffer_ring[BUFFER_RING_SIZE];
static int	current_buffer_id;


static Buffer
findBuffer(int flags)
{ Buffer b;

  if ( flags & BUF_RING )
  { if ( ++current_buffer_id == BUFFER_RING_SIZE )
      current_buffer_id = 0;
    b = &buffer_ring[current_buffer_id];
  } else
    b = &discardable_buffer;

  if ( !b->base )
    initBuffer(b);

  emptyBuffer(b);
  return b;
}


		 /*******************************
		 *	       ANALYSIS		*
		 *******************************/

#define	DOBUF_SOMETIMES	0
#define	DOBUF_NEVER	1
#define	DOBUF_ALWAYS	2

int
PL_get_chars(term_t t, char **s, unsigned flags)
{ char tmp[100];
  char *r;
  int type;
  int dobuffer = DOBUF_SOMETIMES;

  if ( (flags & CVT_ATOM) && SP_get_string(t, &r) )
  { type = DOBUF_NEVER;
  } else if ( (flags & CVT_INTEGER) && SP_is_integer(t) )
  { long l;

    SP_get_integer(t, &l);
    sprintf(tmp, "%ld", l);
    r = tmp;
  } else if ( (flags & CVT_FLOAT) && SP_is_float(t) )
  { double d;
    
    SP_get_float(t, &d);
    sprintf(tmp, "%g", d);
    r = tmp;
  } else if ( (flags & CVT_LIST) && SP_get_list_chars(t, &r) )
  { type = DOBUF_ALWAYS;
  } else if ( (flags & CVT_VARIABLE) )
  { strcpy(tmp, "_");			/* TBD: identify them? */
    r = tmp;
  } else
    return FALSE;
    
  if ( flags & BUF_MALLOC )
  { *s = SP_malloc(strlen(r)+1);

    strcpy(*s, r);
  } else if ( ((flags & BUF_RING) && dobuffer != DOBUF_NEVER) ||
	      (dobuffer == DOBUF_ALWAYS) ||	/* always buffer strings */
	      r == tmp )			/* always buffer tmp */
  { Buffer b = findBuffer(flags);
    int l = strlen(r) + 1;

    addMultipleBuffer(b, r, l, char);
    *s = baseBuffer(b, char);
  } else
    *s = r;

  return TRUE;
}


int
PL_get_list_chars(term_t t, char **s, unsigned flags)
{ return PL_get_chars(t, s, CVT_LIST|flags);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SWI-Prolog does not silently truncate floats to longs as SICStus does.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_get_long(term_t t, long *v)
{ double f;

  if ( SP_is_integer(t) )
    return SP_get_integer(t, v);

  if ( SP_get_float(t, &f) )
  { long l;

#ifdef DOUBLE_TO_LONG_CAST_RAISES_SIGFPE
    if ( !((f >= LONG_MIN) && (f <= LONG_MIN)) )
      fail;
#endif

    l = (long)f;
    if ( (double)l == f )
    { *v = l;
      return TRUE;
    }
  }

  return FALSE;
}


#ifndef INT_MIN
#define INT_MIN (int)(1<<(sizeof(int)*8-1))
#define INT_MAX (int)~(1<<(sizeof(int)*8-1))
#endif

int
PL_get_integer(term_t t, int *v)
{ long l;

  if ( PL_get_long(t, &l) &&
       l >= INT_MIN && l <= INT_MAX )
  { *v = (int)l;
    return TRUE;
  }

  return FALSE;
}


int
PL_get_functor(term_t t, functor_t *f)
{ atom_t name;
  int arity;

  if ( SP_get_functor(t, &name, &arity) )
  { *f = PL_new_functor(name, arity);
  
    return TRUE;
  }

  return FALSE;
}


int
PL_get_head(term_t l, term_t h)
{ int rval;

  term_t t = SP_new_term_ref();
  rval = SP_get_list(l, h, t);
  SP_reset_term_refs(t);
  
  return rval;
}


int
PL_get_tail(term_t l, term_t t)
{ int rval;

  term_t h = SP_new_term_ref();
  rval = SP_get_list(l, h, t);
  SP_reset_term_refs(h);
  
  return rval;
}


int
PL_get_nil(term_t l)
{ atom_t a;

  if ( SP_get_atom(l, &a) && a == ATOM_nil )
    return TRUE;

  return FALSE;
}


int
PL_get_term_value(term_t t, term_value_t *val)
{ int rval = PL_term_type(t);

  switch(rval)
  { case PL_VARIABLE:
      break;
    case PL_INTEGER:
      SP_get_integer(t, &val->i);
      break;
    case PL_FLOAT:
      SP_get_float(t, &val->f);
      break;
    case PL_ATOM:
      SP_get_atom(t, &val->a);
      break;
    case PL_TERM:
    { SP_get_functor(t, &val->t.name, &val->t.arity);
      break;
    }
    default:
      assert(0);
  }

  return rval;
}


		 /*******************************
		 *	   CONSTRUCTING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SICStus says: top 4-bit should share with malloc(), bottom 2-bit should be
zero.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
PL_put_pointer(term_t t, void *ptr)
{ static unsigned long maddr = 0L;
  static unsigned long mask;

  if ( !maddr )
  { mask = (0xf << (sizeof(void *)*8-4)) | 0x3;
    maddr = (unsigned long)SP_malloc(sizeof(int)) & mask;
  }

  assert(((unsigned long)ptr & mask) == maddr);

  SP_put_address(t, ptr);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If it doesn't prove  necessary,  we   don't  want  to add initialisation
functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
PL_put_nil(term_t in)
{ SP_put_atom(in, ATOM_nil);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note: these functions should assing to the result *after* reading the
term-arguments to allow for:

	PL_cons_functor(t, PL_new_functor(PL_new_atom("@"), 1), t)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
PL_cons_functor(term_t t, functor_t f, ...)
{ if ( f->arity == 0 )
    SP_put_atom(t, f->name);
  else
  { va_list args;
    int i;
    term_t a = SP_new_term_ref();
    term_t r = SP_new_term_ref();

    va_start(args, f);
    SP_put_functor(r, f->name, f->arity);
    for(i=0; i<f->arity; i++)
    { term_t next = va_arg(args, term_t);

      SP_get_arg(i+1, r, a);
      SP_unify(a, next);
    }
    SP_put_term(t, r);
    SP_reset_term_refs(a);
    va_end(args);
  }
}


void
PL_cons_functor_v(term_t t, functor_t f, term_t a0)
{ if ( f->arity == 0 )
    SP_put_atom(t, f->name);
  else
  { int i;
    term_t a = SP_new_term_ref();
    term_t r = SP_new_term_ref();

    SP_put_functor(r, f->name, f->arity);
    for(i=0; i<f->arity; i++)
    { SP_get_arg(i+1, r, a);
      SP_unify(a, a0+i);
    }
    SP_put_term(t, r);
    SP_reset_term_refs(a);
    va_end(args);
  }
}


		 /*******************************
		 *	      UNIFYING		*
		 *******************************/

int
PL_unify_atom(term_t t, atom_t a)
{ term_t tmp = SP_new_term_ref();
  int rval;

  SP_put_atom(tmp, a);
  rval = SP_unify(t, tmp);
  SP_reset_term_refs(tmp);

  return rval;
}


int
PL_unify_atom_chars(term_t t, const char *chars)
{ term_t tmp = SP_new_term_ref();
  int rval;

  SP_put_string(tmp, (char *)chars);
  rval = SP_unify(t, tmp);
  SP_reset_term_refs(tmp);

  return rval;
}


int
PL_unify_list_chars(term_t t, const char *chars)
{ term_t tmp = SP_new_term_ref();
  term_t tail = SP_new_term_ref();
  int rval;

  SP_put_atom(tail, ATOM_nil);
  SP_put_list_chars(tmp, tail, (char *)chars);
  rval = SP_unify(t, tmp);
  SP_reset_term_refs(tmp);

  return rval;
}


int
PL_unify_integer(term_t t, long i)
{ term_t tmp = SP_new_term_ref();
  int rval;

  SP_put_integer(tmp, i);
  rval = SP_unify(t, tmp);
  SP_reset_term_refs(tmp);

  return rval;
}


int
PL_unify_float(term_t t, double i)
{ term_t tmp = SP_new_term_ref();
  int rval;

  SP_put_float(tmp, i);
  rval = SP_unify(t, tmp);
  SP_reset_term_refs(tmp);

  return rval;
}


int
PL_unify_functor(term_t t, functor_t f)
{ term_t tmp = SP_new_term_ref();
  int rval;

  SP_put_functor(tmp, f->name, f->arity);
  rval = SP_unify(t, tmp);
  SP_reset_term_refs(tmp);

  return rval;
}


int
PL_unify_pointer(term_t t, void *ptr)
{ term_t tmp = SP_new_term_ref();
  int rval;

  PL_put_pointer(tmp, ptr);
  rval = SP_unify(t, tmp);
  SP_reset_term_refs(tmp);

  return rval;
}

/* PL_unify_list(+l, -h, -t) */

int
PL_unify_list(term_t l, term_t h, term_t t)
{ if ( SP_is_variable(l) )
  { term_t tmp = SP_new_term_ref();
  
    SP_put_list(tmp);
    SP_unify(l, tmp);

    SP_reset_term_refs(tmp);
  }

  return SP_get_list(l, h, t) ? TRUE : FALSE;
}


int
PL_unify_nil(term_t t)
{ term_t tmp = SP_new_term_ref();
  int rval;

  SP_put_atom(tmp, ATOM_nil);
  rval = SP_unify(t, tmp);
  SP_reset_term_refs(tmp);

  return rval;
}


int
PL_unify_arg(int index, term_t t, term_t a)
{ term_t tmp = SP_new_term_ref();
  int rval;

  if ( SP_get_arg(index, t, tmp) && SP_unify(tmp, a) )
    rval = TRUE;
  else
    rval = FALSE;

  SP_reset_term_refs(tmp);

  return rval;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_unify_term(term_t t, ...)
	Unify a term with a term-description.  Thi is taken directly from
	the SWI-Prolog native implementation.  This function is there to
	allow for short and readable source-code, not for speed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct va_list_rec {
  va_list v;
} va_list_rec;

#define args argsRec.v

static int
unify_termVP(term_t t, va_list_rec *argsRecP)
{ va_list_rec argsRec = *argsRecP;
  int rval;

  switch(va_arg(args, int))
  { case PL_VARIABLE:
      rval = TRUE;
      break;
    case PL_ATOM:
      rval = PL_unify_atom(t, va_arg(args, atom_t));
      break;
    case PL_INTEGER:
      rval = PL_unify_integer(t, va_arg(args, long));
      break;
    case PL_POINTER:
      rval = PL_unify_pointer(t, va_arg(args, void *));
      break;
    case PL_FLOAT:
      rval = PL_unify_float(t, va_arg(args, double));
      break;
    case PL_TERM:
      rval = PL_unify(t, va_arg(args, term_t));
      break;
    case PL_CHARS:
      rval = PL_unify_atom_chars(t, va_arg(args, const char *));
      break;
    case PL_FUNCTOR:
    { functor_t ft = va_arg(args, functor_t);
      int arity = ft->arity;
      term_t tmp = PL_new_term_ref();
      int n;

      if ( !PL_unify_functor(t, ft) )
	goto failout;

      for(n=1; n<=arity; n++)
      {	_PL_get_arg(n, t, tmp);
	
	rval = unify_termVP(tmp, &argsRec);
	if ( !rval )
	  goto failout;
      }

      rval = TRUE;
      PL_reset_term_refs(tmp);
      break;
    failout:
      rval = FALSE;
      PL_reset_term_refs(tmp);
      break;
    }
    case PL_LIST:
    { int length = va_arg(args, int);
      term_t tmp = PL_copy_term_ref(t);
      term_t h   = PL_new_term_ref();

      for( ; length-- > 0; )
      { PL_unify_list(tmp, h, tmp);
	rval = unify_termVP(h, &argsRec);
	if ( !rval )
	  goto listfailout;
      }

      rval = PL_unify_nil(tmp);
      PL_reset_term_refs(tmp);
      break;
    listfailout:
      PL_reset_term_refs(tmp);
      break;
    }
    default:
      PL_warning("Format error in PL_unify_term()");
      rval = FALSE;
  }

  *argsRecP = argsRec;
  return rval;
}

int
PL_unify_term(term_t t, ...)
{
  va_list_rec argsRec;
  int rval;

  va_start(args, t);
  rval = unify_termVP(t, &argsRec);
  va_end(args);

  return rval;
}

#undef args


		 /*******************************
		 *	       MODULES		*
		 *******************************/

int
PL_strip_module(term_t raw, module_t *m, term_t plain)
{ term_t a = SP_new_term_ref();
  atom_t name;
  int arity;

  SP_put_term(plain, raw);
  while( SP_get_functor(plain, &name, &arity) &&
	 name == ATOM_module && arity == 2 )
  { SP_get_arg(1, plain, a);

    if ( SP_get_atom(a, &name) )
    { *m = name;
      SP_get_arg(2, plain, plain);
    } else
      break;
  }

  return TRUE;
}


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

#define PREDICATE_HASHSIZE 512

static predicate_t predicate_table[PREDICATE_HASHSIZE];

static int
predicate_hash_key(atom_t name, int arity, module_t module)
{ unsigned int k = arity;

  k ^= name;
  k ^= module;

  return k % (FUNCTOR_HASHSIZE-1);
}


static predicate_t
PL_pred3(atom_t name, int arity, module_t module)
{ int k = predicate_hash_key(name, arity, module);
  predicate_t l, n;

  for( l = predicate_table[k]; l; l = l->next )
  { if ( l->name == name && l->arity == arity && l->module == module )
      return l;
  }

  n = SP_malloc(sizeof(*n));
  n->name = name;
  n->arity = arity;
  n->module = module;
  n->predicate = SP_pred(name, arity, module);
  n->next = predicate_table[k];
  predicate_table[k] = n;
  
  return n;
}


predicate_t
PL_pred(functor_t f, module_t m)
{ return PL_pred3(f->name, f->arity, m);
}


predicate_t
PL_predicate(const char *name, int arity, const char *module)
{ return PL_pred3(SP_atom_from_string((char *)name), arity,
		  PL_new_module(SP_atom_from_string((char *)module)));
}


void
PL_predicate_info(predicate_t p, atom_t *n, int *a, module_t *m)
{ *n = p->name;
  *a = p->arity;
  *m = p->module;
}


		 /*******************************
		 *	     CALLING		*
		 *******************************/

qid_t
PL_open_query(module_t ctx, int flags, predicate_t p, term_t t0)
{ qid_t qid = SP_malloc(sizeof(*qid));
  qid->flags = flags;

  if ( !p->predicate )
  { if ( !(p->predicate = SP_pred(p->name, p->arity, p->module)) )
    { PL_warning("Undefined predicate: %s:%s/%d",
		 PL_atom_chars(p->module),
		 PL_atom_chars(p->name),
		 p->arity);
      qid->flags |= _PL_Q_UNDEFINED_PRED;
					/* TBD: raise exception */
      return qid;
    }
  }

#if 0
  switch(p->arity)
  { case 0:
      qid->query = SP_open_query(p->predicate);
      return qid;
    case 1:
      qid->query = SP_open_query(p->predicate, t0);
      return qid;
  }
#endif

  { SP_term_ref *args = alloca(p->arity * sizeof(SP_term_ref));
    int i;

    for(i=0; i<p->arity; i++)
      args[i] = t0+i;

    qid->query = SP_open_query_array(p->predicate, args);

    return qid;
  }
}


void
PL_cut_query(qid_t qid)
{ SP_cut_query(qid->query);

  if ( qid->flags & _PL_Q_PENDING_EXCEPTION &&
       qid->flags & PL_Q_PASS_EXCEPTION )
  { term_t t = PL_new_term_ref();
    SP_exception_term(t);

    SP_free(qid);
    SP_raise_exception(t);
  } else
    SP_free(qid);
}


void
PL_close_query(qid_t qid)
{ SP_close_query(qid->query);

  if ( qid->flags & _PL_Q_PENDING_EXCEPTION &&
       qid->flags & PL_Q_PASS_EXCEPTION )
  { term_t t = PL_new_term_ref();
    SP_exception_term(t);

    SP_free(qid);
    SP_raise_exception(t);
  } else
    SP_free(qid);
}


int
PL_next_solution(qid_t qid)
{ if ( qid->flags & _PL_Q_UNDEFINED_PRED )
    return FALSE;

  switch(SP_next_solution(qid->query))
  { case SP_SUCCESS:
      return TRUE;
    case SP_FAILURE:
      return FALSE;
    case SP_ERROR:
      qid->flags |= _PL_Q_PENDING_EXCEPTION;
      return FALSE;
    default:
      assert(0);
  }
}


term_t
PL_exception(qid_t qid)
{ if ( qid->flags & _PL_Q_PENDING_EXCEPTION )
  { term_t t = PL_new_term_ref();
    SP_exception_term(t);
    return t;
  }

  return 0;
}


int
PL_call(term_t goal, module_t context)
{ SP_pred_ref call = 0;

  if ( !call )
    call = SP_predicate("call", 1, "user");

  switch(SP_query(call, goal))
  { case SP_SUCCESS:
      return TRUE;
    case SP_FAILURE:
    case SP_ERROR:
      return FALSE;
    default:
      assert(0);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This could be optimised a bit to avoid the malloc/free.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_call_predicate(module_t ctx, int flags, predicate_t pred, term_t h0)
{ int rval;

  qid_t qid = PL_open_query(ctx, flags, pred, h0);
  rval = PL_next_solution(qid);
  PL_cut_query(qid);

  return rval;
}


		 /*******************************
		 *	      RECORDS  		*
		 *******************************/

static long current_key = 0;

record_t
PL_record(term_t t)
{ static SP_pred_ref bbput = 0;
  SP_term_ref k = SP_new_term_ref();
  SP_term_ref m = SP_new_term_ref();

  if ( !bbput )
    bbput = SP_predicate("bb_put", 2, "user");

  current_key += 2;			/* XPCE assumes low-order bit 0 */
  SP_put_integer(k, current_key);
  SP_put_atom(m, ATOM_record);
  SP_cons_functor(k, ATOM_module, 2, m, k);
  SP_query(bbput, k, t);

  return (record_t)current_key;
}


void
PL_recorded(record_t record, term_t t)
{ static SP_pred_ref bbget = 0;
  SP_term_ref k = SP_new_term_ref();
  SP_term_ref m = SP_new_term_ref();

  if ( !bbget )
    bbget = SP_predicate("bb_get", 2, "user");
  
  SP_put_integer(k, (long)record);
  SP_put_atom(m, ATOM_record);
  SP_cons_functor(k, ATOM_module, 2, m, k);
  SP_query(bbget, k, t);
}


void
PL_erase(record_t record)
{ static SP_pred_ref bbdel = 0;
  SP_term_ref k = SP_new_term_ref();
  SP_term_ref t = SP_new_term_ref();
  SP_term_ref m = SP_new_term_ref();

  if ( !bbdel )
    bbdel = SP_predicate("bb_delete", 2, "user");
  
  SP_put_variable(t);
  SP_put_integer(k, (long)record);
  SP_put_atom(m, ATOM_record);
  SP_cons_functor(k, ATOM_module, 2, m, k);

  SP_query(bbdel, k, t);
}

		 /*******************************
		 *	     STREAMS		*
		 *******************************/

static int
dummy_close(void *handle)
{ return 0;
}


static int
open_stream(term_t t, IOSTREAM *s, SP_stream **spstream)
{ SP_term_ref scode = SP_new_term_ref();
  static SP_pred_ref stream_code;

  if ( s->flags & SIO_INPUT )
  { SP_make_stream(s,
		   Sfgetc,
		   NULL,
		   NULL,
		   Sfeof,
		   Sclearerr,
		   Sclose,
		   spstream);
  } else
  { SP_make_stream(s,
		   NULL,
		   Sputc,
		   Sflush,
		   Sfeof,
		   Sclearerr,
		   Sclose,
		   spstream);
  }

  SP_put_address(scode, *spstream);
  if ( !stream_code )
    stream_code = SP_predicate("stream_code", 2, "user");
  
  switch(SP_query(stream_code, t, scode))
  { case SP_SUCCESS:
      return TRUE;
    default:
      assert(0);
      return FALSE;
  }
}


int
PL_open_stream(term_t t, IOSTREAM *s)
{ SP_stream *spstream;

  return open_stream(t, s, &spstream);
}


int
PL_get_stream_handle(term_t t, IOSTREAM **s)
{ PL_warning("PL_get_stream_handle(): Not yet implemented");

  return FALSE;
}


		 /*******************************
		 *	       WRITE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Complicated, but complete except for the   context  precedence, which is
ignored. 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define FUNCT(n, a) PL_new_functor(PL_new_atom(n), a)

int
PL_write_term(IOSTREAM *s,
	      term_t term,
	      int precedence,
	      int flags)
{ term_t stream = PL_new_term_ref();
  term_t options = PL_new_term_ref();
  term_t optlist = PL_copy_term_ref(options);
  term_t opt     = PL_new_term_ref();
  static SP_pred_ref write_term, close;
  SP_stream *spstream;

  if ( flags & PL_WRT_QUOTED )
  { PL_unify_term(opt,
		  PL_FUNCTOR, FUNCT("quoted", 1),
		    PL_CHARS, "true");
    PL_unify_list(optlist, opt, optlist);
    PL_put_variable(opt);
  }
  if ( flags & PL_WRT_IGNOREOPS )
  { PL_unify_term(opt,
		  PL_FUNCTOR, FUNCT("ignore_ops", 1),
		    PL_CHARS, "true");
    PL_unify_list(optlist, opt, optlist);
    PL_put_variable(opt);
  }
  if ( flags & PL_WRT_NUMBERVARS )
  { PL_unify_term(opt,
		  PL_FUNCTOR, FUNCT("numbervars", 1),
		    PL_CHARS, "true");
    PL_unify_list(optlist, opt, optlist);
    PL_put_variable(opt);
  }
  if ( flags & PL_WRT_PORTRAY )
  { PL_unify_term(opt,
		  PL_FUNCTOR, FUNCT("portrayed", 1),
		    PL_CHARS, "true");
    PL_unify_list(optlist, opt, optlist);
  }
  PL_unify_nil(optlist);

  if ( !write_term )
    write_term = SP_predicate("write_term", 3, "user");
  if ( !close )
    close = SP_predicate("close", 1, "user");

  open_stream(stream, s, &spstream);
  spstream->sclose = dummy_close;
  SP_query(write_term, stream, term, options);
  SP_query(close, stream);
  SP_reset_term_refs(stream);

  return TRUE;
}


		 /*******************************
		 *	    RESOURCES		*
		 *******************************/

IOSTREAM *
PL_open_resource(module_t m,
		 const char *name,
		 const char *rc_class,
		 const char *mode)
{ static SP_pred_ref locate = NULL;
  term_t a1 = SP_new_term_ref();
  term_t a2 = SP_new_term_ref();
  term_t a3 = SP_new_term_ref();
  term_t a4 = SP_new_term_ref();
  term_t a5 = SP_new_term_ref();

  if ( !locate )
  { if ( !(locate = SP_predicate("locate_resource", 5, "swi_")) )
    { PL_warning("Cannot find pce_host:locate_resource/4");
      return NULL;
    }
  }

  if ( m )
    SP_put_atom(a1, m);
  else
    SP_put_atom(a1, ATOM_user);
  SP_put_string(a2, (char *)name);
  if ( rc_class )
    SP_put_string(a3, (char *)rc_class);
  else
    SP_put_variable(a3);
  SP_put_string(a4, (char *)mode);
  SP_put_variable(a5);

  if ( SP_query(locate, a1, a2, a3, a4, a5) )
  { char *fname;

    if ( SP_get_string(a5, &fname) )
      return Sopen_file(fname, mode);
  }

  errno = ENOENT;

  return NULL;
}

		 /*******************************
		 *	TOPLEVEL CONTROL	*
		 *******************************/

typedef void (*halt_function)(int, void *);
typedef struct on_halt *OnHalt;

struct on_halt
{ halt_function	function;
  void *	argument;
  OnHalt	next;
};

static int halting = FALSE;
static OnHalt on_halt_list = NULL;

void
PL_on_halt(halt_function f, void *arg)
{ if ( !halting )
  { OnHalt h = SP_malloc(sizeof(struct on_halt));

    h->function = f;
    h->argument = arg;
    h->next = on_halt_list;
    on_halt_list = h;
  }
}


void
PL_deinit(int exitcode)
{ if ( !halting )
  { OnHalt h;

    halting = TRUE;

    for(h = on_halt_list; h; h = h->next)
      (*h->function)(exitcode, h->argument);
  }
}


int
PL_action(int action, ...)
{ int rval = TRUE;
  va_list args;

  va_start(args, action);

  switch(action)
  { case PL_ACTION_TRACE:
      SP_action(SP_ACTION_TRACE, 0);
      break;
    case PL_ACTION_DEBUG:
      SP_action(SP_ACTION_DEBUG, 0);
      break;
    case PL_ACTION_HALT:
    { int a = va_arg(args, int);

      PL_deinit(a);
      SP_action(SP_ACTION_HALT, 0);
      rval = FALSE;			/* appearently, we failed */
      break;
    }
    case PL_ACTION_ABORT:
      SP_action(SP_ACTION_ABORT, 0);
      break;
    case PL_ACTION_BREAK:
    { SP_pred_ref brk = SP_predicate("break", 0, "user");

      SP_query(brk);
      break;
    }
    case PL_ACTION_WRITE:
    { char *s = va_arg(args, char *);

      SP_puts(s);
      break;
    }
    case PL_ACTION_FLUSH:
    { SP_fflush(SP_curout);
      break;
    }
    case PL_ACTION_BACKTRACE:
    case PL_ACTION_SYMBOLFILE:
    case PL_ACTION_GUIAPP:
      PL_warning("PL_action(%d): Unkown action", action);
      rval = FALSE;
  }

  va_end(args);

  return rval;
}


#ifndef LONG_MIN
#define LONG_MIN (int)(1<<(sizeof(long)*8-1))
#define LONG_MAX (int)~(1<<(sizeof(long)*8-1))
#endif


long
PL_query(int query)
{ switch(query)
  { case PL_QUERY_ARGC:
      return 0;
    case PL_QUERY_ARGV:
      return 0;
    case PL_QUERY_SYMBOLFILE:
    case PL_QUERY_ORGSYMBOLFILE:
      return 0L;
    case PL_QUERY_GETC:
      return SP_getc();
    case PL_QUERY_MAX_INTEGER:
    case PL_QUERY_MAX_TAGGED_INT:	/* Is there a difference? */
      return LONG_MAX;
    case PL_QUERY_MIN_INTEGER:
    case PL_QUERY_MIN_TAGGED_INT:
      return LONG_MIN;
    case PL_QUERY_VERSION:
      return 30701;			/* 3.7.1 (how to get it?) */
    default:
      PL_warning("PL_query(%d): Unkown query", query);
  }

  return -1;
}

		 /*******************************
		 *	       HOOKS		*
		 *******************************/

typedef int (*SP_getc_t)(void *handle);

static PL_dispatch_hook_t dispatch_events;
static SP_ReadHookProc *org_read_hook;

int
PL_dispatch(int fd, int wait)
{ int rval;

  if ( wait == PL_DISPATCH_INSTALLED )
    return dispatch_events ? TRUE : FALSE;

  if ( dispatch_events )
  { do
    { rval = (*dispatch_events)(fd);
    } while( wait == PL_DISPATCH_WAIT && rval == PL_DISPATCH_TIMEOUT );
  } else
    rval = PL_DISPATCH_INPUT;

  return rval;
}


static int
SP_dispatch(int fd)
{ if ( PL_dispatch(fd, PL_DISPATCH_WAIT) == PL_DISPATCH_INPUT )
    return 1;

  return 0;
}


PL_dispatch_hook_t
PL_dispatch_hook(PL_dispatch_hook_t hook)
{ PL_dispatch_hook_t old = dispatch_events;

  if ( hook )
  { org_read_hook = SP_set_read_hook(SP_dispatch);
  } else
  { SP_set_read_hook(org_read_hook);
  }

  dispatch_events = hook;
  return old;
}


		 /*******************************
		 *	      ABORTS		*
		 *******************************/

typedef struct abort_handle *AbortHandle;

struct abort_handle
{ AbortHandle	  next;			/* Next handle */
  PL_abort_hook_t function;		/* The handle itself */
};

static AbortHandle abort_head;
static AbortHandle abort_tail;

void
PL_abort_hook(PL_abort_hook_t func)
{ AbortHandle h = (AbortHandle) SP_malloc(sizeof(struct abort_handle));
  h->next = NULL;
  h->function = func;

  if ( abort_head == NULL )
  { abort_head = abort_tail = h;
  } else
  { abort_tail->next = h;
    abort_tail = h;
  }
}


int
PL_abort_unhook(PL_abort_hook_t func)
{ AbortHandle h = abort_head;

  for(; h; h = h->next)
  { if ( h->function == func )
    { h->function = NULL;
      return TRUE;
    }
  }

  return FALSE;
}


void
PL_call_abort_handlers()
{ AbortHandle h = abort_head;

  for(; h; h = h->next)
  { if ( h->function )
    { (*h->function)();
    }
  }
}


		 /*******************************
		 *            MISC		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Should use SP_vfprintf, but this doesn't exists!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_warning(const char *fmt, ...)
{ char msg[2048];
  va_list args;

  va_start(args, fmt);
  vsprintf(msg, fmt, args);
  va_end(args);

  SP_fprintf(SP_stderr, "[WARNING: %s]\n", msg);

  return FALSE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Hack to deal with the stat functions when compiled without -O for gcc and
glibc 2.0.7.  Delete if problems arise!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef __linux__
#include <sys/stat.h>
int stat (__const char *__path, struct stat *__statbuf)
{ return __xstat (_STAT_VER, __path, __statbuf);
}

int fstat (int __fd, struct stat *__statbuf)
{ return __fxstat (_STAT_VER, __fd, __statbuf);
}
#endif
