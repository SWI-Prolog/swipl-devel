/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2023, University of Amsterdam
                              VU University Amsterdam
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

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#include "pl-termhash.h"
#include "pl-fli.h"
#include "os/pl-text.h"
#include "pl-arith.h"
#include "pl-gmp.h"
#include "pl-pro.h"
#include "pl-bisim.h"
#define AC_TERM_WALK 1
#include "pl-termwalk.c"

/* Hashes are the same on all machines.  MurmurHashAligned2() and SHA1
 * read bytes in a fixed order, so the data we hash must be the same:
 * integers are hashed as fixed-width little-endian values (le32(),
 * le64()), sizes as 64-bit integers and wide text as little-endian
 * code points.
 */

static inline uint32_t
le32(uint32_t v)
{
#if WORDS_BIGENDIAN
  return __builtin_bswap32(v);
#else
  return v;
#endif
}

static inline uint64_t
le64(uint64_t v)
{
#if WORDS_BIGENDIAN
  return __builtin_bswap64(v);
#else
  return v;
#endif
}

#undef LD
#define LD LOCAL_LD

#ifdef __WINDOWS__
typedef unsigned int uint32_t;
#endif


		 /*******************************
		 *	    TERM_HASH/2		*
		 *******************************/

#define CYCLE_CONST 123456
#define nodeID(ptr, b) ((ptr)-baseBuffer(b, th_data))
#define nodePTR(id, b) (baseBuffer(b, th_data) + (id))


typedef struct th_data
{ size_t	parent_offset;		/* My parent */
  Functor	term;			/* Term being processed */
  functor_t	functor;		/* Functor of the term */
  unsigned int	hash;			/* Hash collected sofar */
  unsigned	arg : 31;		/* Current argument */
  unsigned	in_cycle : 1;		/* We are part of a cycle */
} th_data;


/* Numbers are hashed by value, so the hash does not depend on how they
 * are stored (cell layout, range of small integers, GMP or LibBF): an
 * integer that fits in 64 bits as int64_t, a larger integer as its sign
 * and its magnitude in big-endian bytes, a rational as its numerator
 * and denominator and a float as its double.
 *
 * number_hash_data() returns true and sets *ip if the number is an
 * integer that fits in 64 bits.  Otherwise it adds the data to hash to
 * `b`.
 */

#ifdef O_BIGNUM
static void
add_mpz_hash_data(mpz_t mpz, Buffer b)
{ size_t bytes = (mpz_sizeinbase(mpz, 2)+7)/8;
  size_t count;
  char *p;

  uint64_t lebytes = le64(bytes);

  addBuffer(b, (char)(mpz_sgn(mpz) < 0 ? '-' : '+'), char);
  addMultipleBuffer(b, (char*)&lebytes, sizeof(lebytes), char);
  p = allocFromBuffer(b, bytes);
  mpz_export(p, &count, 1, 1, 1, 0, mpz);
  DEBUG(0, assert(count == bytes));
}
#endif

#define number_hash_data(w, ip, b) LDFUNC(number_hash_data, w, ip, b)
static bool
number_hash_data(DECL_LD word w, int64_t *ip, Buffer b)
{ number n;
  bool rc = false;

  if ( isTaggedInt(w) )
  { *ip = valInt(w);
    return true;
  }

  get_number(w, &n);
  switch(n.type)
  { case V_INTEGER:
      *ip = n.value.i;
      rc = true;
      break;
#ifdef O_BIGNUM
    case V_MPZ:
      if ( mpz_to_int64(n.value.mpz, ip) )
	rc = true;
      else
	add_mpz_hash_data(n.value.mpz, b);
      break;
    case V_MPQ:
      add_mpz_hash_data(mpq_numref(n.value.mpq), b);
      add_mpz_hash_data(mpq_denref(n.value.mpq), b);
      break;
#endif
    case V_FLOAT:
    { uint64_t bits;

      memcpy(&bits, &n.value.f, sizeof(bits));
      bits = le64(bits);
      addMultipleBuffer(b, (char*)&bits, sizeof(bits), char);
      break;
    }
    default:
      assert(0);
  }
  clearNumber(&n);

  return rc;
}


/* A variable numbered by variant_sha1/2 or variant_hash/2 is hashed by
 * its number, not by the marked cell, which depends on the cell layout.
 */

#define HASH_VAR(p) \
	do { uint64_t vnum = le64(valTagged(*(p))); \
	     HASH("V", 1); \
	     HASH(&vnum, sizeof(vnum)); \
	   } while(0)


#define primitiveHashValue(term, hval) LDFUNC(primitiveHashValue, term, hval)
static int
primitiveHashValue(DECL_LD word term, unsigned int *hval)
{ switch(tag(term))
  { case TAG_VAR:
    case TAG_ATTVAR:
      return false;
    case TAG_ATOM:
    { uint32_t ah = le32(atomValue(term)->hash_value);

      *hval = MurmurHashAligned2(&ah, sizeof(ah), *hval);
      return true;
    }
    case TAG_STRING:
    { size_t len;
      char *s;

      if ( (s = getCharsString(term, &len)) )
      { *hval = MurmurHashAligned2(s, len, *hval);
      } else
      { PL_chars_t text;
	if ( get_string_text(term, &text) &&
	     PL_mb_text(&text, REP_UTF8) )
	{ *hval = MurmurHashAligned2(text.text.t, text.length, *hval);
	  PL_free_text(&text);
	} else
	  return false;
      }

      return true;
    }
    case TAG_INTEGER:
      if ( isTaggedInt(term) )
      { uint64_t v = le64(valInt(term));

	*hval = MurmurHashAligned2(&v, sizeof(v), *hval);

	return true;
      }
    /*FALLTHROUGH*/
    case TAG_FLOAT:
    { int64_t v;
      tmp_buffer b;

      initBuffer(&b);
      if ( number_hash_data(term, &v, (Buffer)&b) )
      { uint64_t lv = le64(v);

	*hval = MurmurHashAligned2(&lv, sizeof(lv), *hval);
      }
      else
	*hval = MurmurHashAligned2(baseBuffer(&b, char),
				   entriesBuffer(&b, char), *hval);
      discardBuffer(&b);

      return true;
    }
    default:
      assert(0);
      return false;
  }
}


#define start_term(work, b, w) LDFUNC(start_term, work, b, w)
static void
start_term(DECL_LD th_data *work, Buffer b, word w)
{ atom_t name;

  work->term     = valueTerm(w);
  work->functor  = word2functor(work->term->definition);
  work->hash     = MURMUR_SEED;
  work->arg      = 0;
  work->in_cycle = 0;

  name = nameFunctor(work->functor);
  uint32_t ah = le32(atomValue(name)->hash_value);
  work->hash = MurmurHashAligned2(&ah, sizeof(ah), work->hash);

  DEBUG(1, Sdprintf("Added node %ld, %s/%d, hash=%d\n",
		    nodeID(work, b),
		    stringAtom(name), arityFunctor(work->functor),
		    work->hash));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) This deals with A = s(A), hash_term(s(s(A)), Hash)
This is not enough.  Consider:

	Term = [X=s(Y),Y=Y], %where
	X = s(Y),
	Y = [X|X].

Making every parent a cycle works, but might loose a bit too much :-(
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define next_arg(workp, b) LDFUNC(next_arg, workp, b)
static Word
next_arg(DECL_LD th_data **workp, Buffer b)
{ th_data *work = *workp;

  for(;;)
  { if ( work->arg < arityFunctor(work->functor) )
    { Word p = &work->term->arguments[work->arg];

      DEBUG(1, Sdprintf("-> arg %d from node %ld\n",
			work->arg, nodeID(work, b)));

      deRef(p);
      return p;
    } else if ( work->parent_offset != (size_t)-1 )
    { th_data *parent = nodePTR(work->parent_offset, b);
      unsigned int myhash;

      myhash = le32(work->in_cycle ? CYCLE_CONST : work->hash);
      parent->hash = MurmurHashAligned2(&myhash, sizeof(myhash),
					parent->hash);
      if ( work->in_cycle /*&& parent->functor == work->functor*/ ) /* (*) */
      { DEBUG(1, Sdprintf("Mark parent %ld as cycle\n",
			  nodeID(parent, b)));
	parent->in_cycle = true;
      }
      DEBUG(1, Sdprintf("Updated hash in node %ld%s to %d\n",
			work->parent_offset,
			work->in_cycle ? " (cycle)" : "",
			parent->hash));

      work = *workp = parent;
      work->arg++;
    } else
    { return NULL;
    }
  }
}


static void
update_cycle(th_data *here, th_data *start, Buffer b)
{ uint32_t myhash = le32(CYCLE_CONST);

  here->hash = MurmurHashAligned2(&myhash, sizeof(myhash),
				  here->hash);
  DEBUG(1, Sdprintf("here = %ld, hash -> %d\n",
		    nodeID(here, b), here->hash));

  for(;;)
  { DEBUG(1, Sdprintf("Marking cycle for node %ld\n",
		      nodeID(here, b)));
    here->in_cycle = true;
    if ( here == start )
      break;
    here = nodePTR(here->parent_offset, b);
  }
}


#define termHashValue(p, hval) LDFUNC(termHashValue, p, hval)

static int  /* bool or -1 for out-of-memory */
termHashValue(DECL_LD Word p, unsigned int *hval)
{ deRef(p);
  if ( !isTerm(*p) )
  { *hval = MURMUR_SEED;
    return primitiveHashValue(*p, hval);
  } else
  { tmp_buffer tmp;
    Buffer b = (Buffer)&tmp;
    th_data *work;
    int rc = true;
    Functor t = valueTerm(*p);

    initBuffer(&tmp);
    work = allocFromBuffer(b, sizeof(*work));	/* cannot fail */
    start_term(work, b, *p);
    work->parent_offset = (size_t)-1;
    t->definition = consInt(0);

    while ( (p = next_arg(&work, b)) )
    { if ( !isTerm(*p) )
      { if ( primitiveHashValue(*p, &work->hash) )
	{ work->arg++;
	} else
	{ rc = false;
	  goto out;
	}
      } else
      { Functor t = valueTerm(*p);

	if ( isInteger(t->definition) )
	{ th_data *seen = nodePTR(valInt(t->definition), b);

	  if ( seen->arg < arityFunctor(seen->functor) )
	  { DEBUG(1, Sdprintf("cycle to %ld\n", valInt(t->definition)));
	    update_cycle(work, seen, b);
	  } else
	  { uint32_t shash = le32(seen->in_cycle ? CYCLE_CONST : seen->hash);

	    work->hash = MurmurHashAligned2(&shash, sizeof(shash), work->hash);
	    DEBUG(1, Sdprintf("shared with %ld, reusing hash %d node %ld -> %d\n",
			      valInt(t->definition), seen->hash,
			      nodeID(work, b), work->hash));
	    if ( seen->in_cycle /*&& seen->functor == work->functor*/ )
	      work->in_cycle = true;
	  }
	  work->arg++;
	} else
	{ size_t parent = nodeID(work, b);

	  if ( !(work = allocFromBuffer(b, sizeof(*work))) )
	  { rc = -1;			/* out of memory */
	    goto out;
	  }
	  start_term(work, b, *p);
	  work->parent_offset = parent;
	  t->definition = consInt(nodeID(work, b));
	}
      }
    }
					/* restore */
  out:;
    { th_data *d   = baseBuffer(b, th_data);
      th_data *end = d + entriesBuffer(b, th_data);

      if ( rc == true )
	*hval = d->hash;

      for(; d<end; d++)
      { d->term->definition = d->functor;
      }
    }

    discardBuffer(b);

    return rc;
  }
}


/* term_hash(+Term, -HashKey) */

static
PRED_IMPL("term_hash", 2, term_hash, 0)
{ PRED_LD
  Word p = valTermRef(A1);
  unsigned int hash;
  int rc = termHashValue(p, &hash);

  if ( rc < 0 )
    return PL_resource_error("memory");
  else if ( rc )
    return PL_unify_int64(A2, (int64_t)hash);

  return true;			/* term is non-ground */
}


		 /*******************************
		 *	    VARIANT SHA1	*
		 *******************************/

/* type to hold the SHA256 context  */

typedef struct
{   uint32_t count[2];
    uint32_t hash[5];
    uint32_t wbuf[16];
} sha1_ctx;

/* Note that these prototypes are the same for both bit and */
/* byte oriented implementations. However the length fields */
/* are in bytes or bits as appropriate for the version used */
/* and bit sequences are input as arrays of bytes in which  */
/* bit sequences run from the most to the least significant */
/* end of each byte                                         */

#define VOID_RETURN static void

VOID_RETURN sha1_compile(sha1_ctx ctx[1]);

VOID_RETURN sha1_begin(sha1_ctx ctx[1]);
VOID_RETURN sha1_hash(const unsigned char data[], unsigned long len, sha1_ctx ctx[1]);
VOID_RETURN sha1_end(unsigned char hval[], sha1_ctx ctx[1]);


		 /*******************************
		 *    INCREMENTAL MURMUR HASH	*
		 *******************************/

#define HASH_BLOCK_SIZE 256

typedef struct hash_state
{ unsigned int	  hash;
  size_t	  len;
  unsigned char	  buf[HASH_BLOCK_SIZE];
} hash_state;

static void
hash_init(hash_state *state)
{ state->len  = 0;
  state->hash = MURMUR_SEED;
}

static void
hash_compile(hash_state *state, const unsigned char *data, size_t len)
{ if ( len+state->len <= HASH_BLOCK_SIZE )
  { memcpy(&state->buf[state->len], data, len);
    state->len += len;
  } else
  { while(len > 0)
    { size_t copy = len;
      if ( len > HASH_BLOCK_SIZE-state->len )
	copy = HASH_BLOCK_SIZE-state->len;
      len -= copy;
      memcpy(&state->buf[state->len], data, copy);
      data += copy;
      state->len += copy;
      if ( state->len == HASH_BLOCK_SIZE )
      { state->hash = MurmurHashAligned2(state->buf, HASH_BLOCK_SIZE,
					 state->hash);
	state->len = 0;
      }
    }
  }
}

static unsigned int
hash_end(hash_state *state)
{ return MurmurHashAligned2(state->buf, state->len, state->hash);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
State for processing the term
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ int		var_count;
  hash_algo	algorithm;
  union
  { sha1_ctx	sha1[1];			/* The SHA1 Context */
    hash_state	murmur[1];
  } ctx;
  segstack	vars;
  Word		vars_first_chunk[64];
} sha1_state;

typedef enum
{ E_OK,
  E_ATTVAR,
  E_RESOURCE,
  E_CYCLE
} status;


static int
push_var(Word p, sha1_state *state)
{ return pushSegStack(&state->vars, p, Word);
}

static int
push_attvar(Word p, sha1_state *state)
{ Word w = word2ptr(Word, *p);
  return ( pushSegStack(&state->vars, p, Word) &&
	   pushSegStack(&state->vars, w, Word) );
}

#define HASH(p,l) \
	do \
	{ if (state->algorithm == HASH_SHA1) \
	    sha1_hash((const unsigned char*)(p), (l), state->ctx.sha1); \
	  else \
	    hash_compile(state->ctx.murmur, (const unsigned char*)(p), (l)); \
	} while(0)

/* Hash text as its length (in characters, as a 64-bit little-endian
 * integer) followed by the characters: bytes or, for wide text,
 * little-endian 32-bit code points (see MurmurHashWide()).
 */

static void
hash_text(sha1_state *state, const char *s, size_t len)
{ uint64_t l = le64(len);

  HASH(&l, sizeof(l));
  HASH(s, (unsigned long)len);
}

static void
hash_wide_text(sha1_state *state, const pl_wchar_t *s, size_t len)
{
#if !WORDS_BIGENDIAN && SIZEOF_WCHAR_T == 4
  uint64_t l = le64(len);

  HASH(&l, sizeof(l));
  HASH(s, (unsigned long)(len*sizeof(*s)));
#else
  const pl_wchar_t *e = s+len;
  uint64_t n = 0;

  for(const pl_wchar_t *q=s; q<e; q++)	/* count code points */
  {
#if SIZEOF_WCHAR_T == 2
    if ( *q >= 0xd800 && *q < 0xdc00 && q+1 < e &&
	 q[1] >= 0xdc00 && q[1] < 0xe000 )
      q++;
#endif
    n++;
  }
  n = le64(n);
  HASH(&n, sizeof(n));
  while(s < e)
  { uint32_t c = (uint32_t)*s++;
#if SIZEOF_WCHAR_T == 2
    if ( c >= 0xd800 && c < 0xdc00 && s < e &&
	 (uint32_t)*s >= 0xdc00 && (uint32_t)*s < 0xe000 )
      c = 0x10000 + ((c-0xd800)<<10) + ((uint32_t)*s++ - 0xdc00);
#endif
    c = le32(c);
    HASH(&c, sizeof(c));
  }
#endif
}

static void
hash_atom_text(sha1_state *state, Atom a)
{ if ( ison(a->type, PL_BLOB_WCHAR) )
    hash_wide_text(state, (const pl_wchar_t*)a->name,
		   a->length/sizeof(pl_wchar_t));
  else
    hash_text(state, a->name, a->length);
}

#define variant_sha1(agenda, state) LDFUNC(variant_sha1, agenda, state)
static status
variant_sha1(DECL_LD ac_term_agenda *agenda, sha1_state *state)
{ Word p;

  while( (p=ac_nextTermAgenda(agenda)) )
  { word w = *p;

    switch(tag(w))
    { case TAG_ATTVAR:
      { if ( state->algorithm == HASH_SHA1 )
	{ return E_ATTVAR;
	} else
	{ if ( isGlobalPtr(w) )
	  { word i = state->var_count++;

	    if ( !push_attvar(p, state) )
	      return E_RESOURCE;
	    *p = consTagged(i, TAGEX_VAR)|MARK_MASK;
	  }
	  HASH_VAR(p);
	  continue;
	}
      }
      case TAG_VAR:
      { if ( isVar(w) )
	{ word i = state->var_count++;

	  if ( !push_var(p, state) )
	    return E_RESOURCE;
	  *p = consTagged(i, TAGEX_VAR)|MARK_MASK;
	}
	HASH_VAR(p);
	continue;
      }
      case TAG_ATOM:
      { Atom av = atomValue(w);

	HASH("A", 1);
	hash_atom_text(state, av);
	HASH(av->type->name, (unsigned long)strlen(av->type->name));

					/* TBD: Include type */
	continue;
      }
      case TAG_INTEGER:
      case TAG_FLOAT:
      { int64_t val;
	tmp_buffer b;

	initBuffer(&b);
	if ( number_hash_data(w, &val, (Buffer)&b) )
	{ uint64_t lv = le64(val);

	  HASH("i", 1);
	  HASH(&lv, sizeof(lv));
	} else
	{ HASH(tag(w) == TAG_FLOAT ? "F" : "I", 1);
	  HASH(baseBuffer(&b, char), (unsigned long)entriesBuffer(&b, char));
	}
	discardBuffer(&b);
	continue;
      }
      case TAG_STRING:
      { size_t len;
	char *s;
	pl_wchar_t *ws;

	HASH("S", 1);
	if ( (s=getCharsString(w, &len)) )
	  hash_text(state, s, len);
	else if ( (ws=getCharsWString(w, &len)) )
	  hash_wide_text(state, ws, len);
	continue;
      }
      case TAG_COMPOUND:
      { functor_t f;

	switch(ac_pushTermAgenda(agenda, w, &f))
	{ case -1:			/* Resource error */
	    return E_RESOURCE;
	  case false:			/* Cycle */
	    return E_CYCLE;
	  default:
	  { FunctorDef fd = valueFunctor(f);
	    size_t arity = arityFunctor(f); /* was int, may affect code below */

	    Atom fn = atomValue(fd->name);

	    uint64_t ar  = le64(arity);

	    HASH("T", 1);
	    hash_atom_text(state, fn);
	    HASH(&ar, sizeof(ar));
	  }
	}
	continue;
      }
    }
  }

  return E_OK;
}


/* hash_prefix() mixes a tag into the digest before the term.  It exists so
   that the digest of a cyclic term, which is taken over its canonical form,
   cannot come out equal to the digest of an acyclic term that happens to
   look like that form.  Nothing is mixed in on the acyclic path, so those
   digests are unchanged; boot/init.pl names cache files with them.
*/

static void
hash_prefix(sha1_state *state, const char *prefix)
{ HASH(prefix, strlen(prefix));
}


#define variant_hash_walk(term, hash, algorithm, prefix) \
	LDFUNC(variant_hash_walk, term, hash, algorithm, prefix)

static status
variant_hash_walk(DECL_LD term_t term, termhash_t *hash, hash_algo algorithm,
		  const char *prefix)
{ status rc;
  ac_term_agenda agenda;
  sha1_state state;
  Word p;

  state.var_count = 0;
  state.algorithm = algorithm;
  if ( algorithm == HASH_SHA1 )
    sha1_begin(state.ctx.sha1);
  else
    hash_init(state.ctx.murmur);
  if ( prefix )
    hash_prefix(&state, prefix);
  ac_initTermAgenda(&agenda, valTermRef(term));
  initSegStack(&state.vars, sizeof(Word),
	       sizeof(state.vars_first_chunk), state.vars_first_chunk);
  rc = variant_sha1(&agenda, &state);
  ac_clearTermAgenda(&agenda);
  while(popSegStack(&state.vars, &p, Word))
  { word w = ptr2word(p);
    if ( unlikely(isAttVar(w)) )
    { popSegStack(&state.vars, &p, Word);
      *p = w;
    } else
    { setVar(*p);
    }
  }

  DEBUG(CHK_SECURE, checkData(valTermRef(term)));

  if ( rc == E_OK )
  { if ( state.algorithm == HASH_SHA1 )
      sha1_end(hash->sha1, state.ctx.sha1);
    else
      hash->murmur = hash_end(state.ctx.murmur);
  }

  return rc;
}


int
variant_hash(DECL_LD term_t term, termhash_t *hash, hash_algo algorithm)
{ status rc = variant_hash_walk(term, hash, algorithm, NULL);

  if ( rc == E_CYCLE )
  { fid_t fid;
    term_t form;
    bool attvars = false;
    bool ok;

    /* A cyclic term cannot be walked to the end, so hash its canonical form
       instead: a ground acyclic term that is == for two terms exactly when
       they are =@=.  That is the "canonical cycle" this predicate has always
       said it would need; given A = [a|A] and B = [a,a|B], both reach the
       same form and so the same digest.
    */
    if ( !(fid=PL_open_foreign_frame()) )
      return false;
    ok = ( (form=PL_new_term_ref()) &&
	   term_canonical_form(term, form, &attvars) );
    /* The form holds an attributed variable as a plain one, which is what
       variant_hash/2 wants and what variant_sha1/2 refuses.
    */
    attvars = attvars && algorithm == HASH_SHA1;
    if ( ok && !attvars )
      rc = variant_hash_walk(form, hash, algorithm, "C");
    PL_discard_foreign_frame(fid);	/* the form was only needed here */
    if ( !ok )
      return false;			/* exception already raised */
    if ( attvars )
      rc = E_ATTVAR;
  }

  switch( rc )
  { case E_ATTVAR:
      return PL_error(NULL, 0, NULL,
		      ERR_TYPE, ATOM_free_of_attvar, term);
    case E_CYCLE:			/* a canonical form is acyclic */
      return PL_error(NULL, 0, NULL,
		      ERR_TYPE, ATOM_acyclic_term, term);
    case E_RESOURCE:
      return PL_error(NULL, 0, NULL,
		      ERR_RESOURCE, ATOM_memory);
    case E_OK:
      break;
  }

  return true;
}

/** variant_sha1(@Term, -SHA1:string) is det.

Compute an SHA1 hash for Term. The hash  is designed such that two terms
have the same hash iff variant(T1,T2) is true. This implies that we must
basically execute numbervars.
*/

static
PRED_IMPL("variant_sha1", 2, variant_sha1, 0)
{ PRED_LD
  termhash_t hash;

  if ( variant_hash(A1, &hash, HASH_SHA1) )
  { char hex[SHA1_DIGEST_SIZE*2];
    const char hexd[] = "0123456789abcdef";
    char *o;
    const unsigned char *i;
    int n;

    o = hex;
    i = hash.sha1;
    for(n=0; n<SHA1_DIGEST_SIZE; n++,i++)
    { *o++ = hexd[*i >> 4];
      *o++ = hexd[*i&0x0f];
    }

    return PL_unify_chars(A2, PL_ATOM|REP_ISO_LATIN_1, sizeof(hex), hex);
  } else
  { return false;
  }
}

static
PRED_IMPL("variant_hash", 2, variant_hash, 0)
{ PRED_LD
  termhash_t hash;

  if ( variant_hash(A1, &hash, HASH_MURMUR) )
  { return PL_unify_integer(A2, hash.murmur&PLMAXTAGGEDINT32);
  } else
  { return false;
  }
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(termhash)
  PRED_DEF("variant_sha1", 2, variant_sha1, 0)
  PRED_DEF("variant_hash", 2, variant_hash, 0)
  PRED_DEF("term_hash",    2, term_hash,    0)
EndPredDefs

		 /*******************************
		 *	  INCLUDED STUFF	*
		 *******************************/

/*
 ---------------------------------------------------------------------------
 Copyright (c) 2002, Dr Brian Gladman, Worcester, UK.   All rights reserved.

 LICENSE TERMS

 The free distribution and use of this software in both source and binary
 form is allowed (with or without changes) provided that:

   1. distributions of this source code include the above copyright
      notice, this list of conditions and the following disclaimer;

   2. distributions in binary form include the above copyright
      notice, this list of conditions and the following disclaimer
      in the documentation and/or other associated materials;

   3. the copyright holder's name is not used to endorse products
      built using this software without specific written permission.

 ALTERNATIVELY, provided that this notice is retained in full, this product
 may be distributed under the terms of the GNU General Public License (GPL),
 in which case the provisions of the GPL apply INSTEAD OF those given above.

 DISCLAIMER

 This software is provided 'as is' with no explicit or implied warranties
 in respect of its properties, including, but not limited to, correctness
 and/or fitness for purpose.
 ---------------------------------------------------------------------------
 Issue Date: 01/08/2005

 This is a byte oriented version of SHA1 that operates on arrays of bytes
 stored in memory.
*/

#include <string.h>     /* for memcpy() etc.        */

/*
 ---------------------------------------------------------------------------
 Copyright (c) 2002, Dr Brian Gladman, Worcester, UK.   All rights reserved.

 LICENSE TERMS

 The free distribution and use of this software in both source and binary
 form is allowed (with or without changes) provided that:

   1. distributions of this source code include the above copyright
      notice, this list of conditions and the following disclaimer;

   2. distributions in binary form include the above copyright
      notice, this list of conditions and the following disclaimer
      in the documentation and/or other associated materials;

   3. the copyright holder's name is not used to endorse products
      built using this software without specific written permission.

 ALTERNATIVELY, provided that this notice is retained in full, this product
 may be distributed under the terms of the GNU General Public License (GPL),
 in which case the provisions of the GPL apply INSTEAD OF those given above.

 DISCLAIMER

 This software is provided 'as is' with no explicit or implied warranties
 in respect of its properties, including, but not limited to, correctness
 and/or fitness for purpose.
 ---------------------------------------------------------------------------
 Issue Date: 01/08/2005
*/

#define SHA1_BLOCK_SIZE  64

#define IS_BIG_ENDIAN      4321 /* byte 0 is most significant (mc68k) */
#define IS_LITTLE_ENDIAN   1234 /* byte 0 is least significant (i386) */

#if WORDS_BIGENDIAN
#define PLATFORM_BYTE_ORDER IS_BIG_ENDIAN
#else
#define PLATFORM_BYTE_ORDER IS_LITTLE_ENDIAN
#endif

#define rotl32(x,n)   (((x) << n) | ((x) >> (32 - n)))
#define rotr32(x,n)   (((x) >> n) | ((x) << (32 - n)))

#if !defined(bswap_32)
#define bswap_32(x) ((rotr32((x), 24) & 0x00ff00ff) | (rotr32((x), 8) & 0xff00ff00))
#endif

#if (PLATFORM_BYTE_ORDER == IS_LITTLE_ENDIAN)
#define SWAP_BYTES
#else
#undef  SWAP_BYTES
#endif

#if defined(SWAP_BYTES)
#define bsw_32(p,n) \
    { int _i = (n); while(_i--) ((uint32_t*)p)[_i] = bswap_32(((uint32_t*)p)[_i]); }
#else
#define bsw_32(p,n)
#endif

#define SHA1_MASK   (SHA1_BLOCK_SIZE - 1)

#define ch(x,y,z)       ((z) ^ ((x) & ((y) ^ (z))))
#define parity(x,y,z)   ((x) ^ (y) ^ (z))
#define maj(x,y,z)      (((x) & (y)) | ((z) & ((x) ^ (y))))

/* Compile 64 bytes of hash data into SHA1 context. Note    */
/* that this routine assumes that the byte order in the     */
/* ctx->wbuf[] at this point is in such an order that low   */
/* address bytes in the ORIGINAL byte stream will go in     */
/* this buffer to the high end of 32-bit words on BOTH big  */
/* and little endian systems                                */

#ifdef ARRAY
#define q(v,n)  v[n]
#else
#define q(v,n)  v##n
#endif

#define one_cycle(v,a,b,c,d,e,f,k,h)            \
    q(v,e) += rotr32(q(v,a),27) +               \
              f(q(v,b),q(v,c),q(v,d)) + k + h;  \
    q(v,b)  = rotr32(q(v,b), 2)

#define five_cycle(v,f,k,i)                 \
    one_cycle(v, 0,1,2,3,4, f,k,hf(i  ));   \
    one_cycle(v, 4,0,1,2,3, f,k,hf(i+1));   \
    one_cycle(v, 3,4,0,1,2, f,k,hf(i+2));   \
    one_cycle(v, 2,3,4,0,1, f,k,hf(i+3));   \
    one_cycle(v, 1,2,3,4,0, f,k,hf(i+4))

VOID_RETURN sha1_compile(sha1_ctx ctx[1])
{   uint32_t    *w = ctx->wbuf;

#ifdef ARRAY
    uint32_t    v[5];
    memcpy(v, ctx->hash, 5 * sizeof(uint32_t));
#else
    uint32_t    v0, v1, v2, v3, v4;
    v0 = ctx->hash[0]; v1 = ctx->hash[1];
    v2 = ctx->hash[2]; v3 = ctx->hash[3];
    v4 = ctx->hash[4];
#endif

#define hf(i)   w[i]

    five_cycle(v, ch, 0x5a827999,  0);
    five_cycle(v, ch, 0x5a827999,  5);
    five_cycle(v, ch, 0x5a827999, 10);
    one_cycle(v,0,1,2,3,4, ch, 0x5a827999, hf(15)); \

#undef  hf
#define hf(i) (w[(i) & 15] = rotl32(                    \
                 w[((i) + 13) & 15] ^ w[((i) + 8) & 15] \
               ^ w[((i) +  2) & 15] ^ w[(i) & 15], 1))

    one_cycle(v,4,0,1,2,3, ch, 0x5a827999, hf(16));
    one_cycle(v,3,4,0,1,2, ch, 0x5a827999, hf(17));
    one_cycle(v,2,3,4,0,1, ch, 0x5a827999, hf(18));
    one_cycle(v,1,2,3,4,0, ch, 0x5a827999, hf(19));

    five_cycle(v, parity, 0x6ed9eba1,  20);
    five_cycle(v, parity, 0x6ed9eba1,  25);
    five_cycle(v, parity, 0x6ed9eba1,  30);
    five_cycle(v, parity, 0x6ed9eba1,  35);

    five_cycle(v, maj, 0x8f1bbcdc,  40);
    five_cycle(v, maj, 0x8f1bbcdc,  45);
    five_cycle(v, maj, 0x8f1bbcdc,  50);
    five_cycle(v, maj, 0x8f1bbcdc,  55);

    five_cycle(v, parity, 0xca62c1d6,  60);
    five_cycle(v, parity, 0xca62c1d6,  65);
    five_cycle(v, parity, 0xca62c1d6,  70);
    five_cycle(v, parity, 0xca62c1d6,  75);

#ifdef ARRAY
    ctx->hash[0] += v[0]; ctx->hash[1] += v[1];
    ctx->hash[2] += v[2]; ctx->hash[3] += v[3];
    ctx->hash[4] += v[4];
#else
    ctx->hash[0] += v0; ctx->hash[1] += v1;
    ctx->hash[2] += v2; ctx->hash[3] += v3;
    ctx->hash[4] += v4;
#endif
}

VOID_RETURN sha1_begin(sha1_ctx ctx[1])
{
    ctx->count[0] = ctx->count[1] = 0;
    ctx->hash[0] = 0x67452301;
    ctx->hash[1] = 0xefcdab89;
    ctx->hash[2] = 0x98badcfe;
    ctx->hash[3] = 0x10325476;
    ctx->hash[4] = 0xc3d2e1f0;
}

/* SHA1 hash data in an array of bytes into hash buffer and */
/* call the hash_compile function as required.              */

VOID_RETURN sha1_hash(const unsigned char data[], long unsigned int len, sha1_ctx ctx[1])
{   uint32_t pos = (uint32_t)(ctx->count[0] & SHA1_MASK),
            space = SHA1_BLOCK_SIZE - pos;
    const unsigned char *sp = data;

    ctx->count[0] = (uint32_t) (ctx->count[0] + len);
    if(ctx->count[0] < len)
        ++(ctx->count[1]);

    while(len >= space)     /* tranfer whole blocks if possible  */
    {
        memcpy(((unsigned char*)ctx->wbuf) + pos, sp, space);
        sp += space; len -= space; space = SHA1_BLOCK_SIZE; pos = 0;
        bsw_32(ctx->wbuf, SHA1_BLOCK_SIZE >> 2);
        sha1_compile(ctx);
    }

    memcpy(((unsigned char*)ctx->wbuf) + pos, sp, len);
}

/* SHA1 final padding and digest calculation  */

VOID_RETURN sha1_end(unsigned char hval[], sha1_ctx ctx[1])
{   uint32_t    i = (uint32_t)(ctx->count[0] & SHA1_MASK);

    /* put bytes in the buffer in an order in which references to   */
    /* 32-bit words will put bytes with lower addresses into the    */
    /* top of 32 bit words on BOTH big and little endian machines   */
    bsw_32(ctx->wbuf, (i + 3) >> 2);

    /* we now need to mask valid bytes and add the padding which is */
    /* a single 1 bit and as many zero bits as necessary. Note that */
    /* we can always add the first padding byte here because the    */
    /* buffer always has at least one empty slot                    */
    ctx->wbuf[i >> 2] &= 0xffffff80U << 8 * (~i & 3);
    ctx->wbuf[i >> 2] |= 0x00000080U << 8 * (~i & 3);

    /* we need 9 or more empty positions, one for the padding byte  */
    /* (above) and eight for the length count. If there is not      */
    /* enough space, pad and empty the buffer                       */
    if(i > SHA1_BLOCK_SIZE - 9)
    {
        if(i < 60) ctx->wbuf[15] = 0;
        sha1_compile(ctx);
        i = 0;
    }
    else    /* compute a word index for the empty buffer positions  */
        i = (i >> 2) + 1;

    while(i < 14) /* and zero pad all but last two positions        */
        ctx->wbuf[i++] = 0;

    /* the following 32-bit length fields are assembled in the      */
    /* wrong byte order on little endian machines but this is       */
    /* corrected later since they are only ever used as 32-bit      */
    /* word values.                                                 */
    ctx->wbuf[14] = (ctx->count[1] << 3) | (ctx->count[0] >> 29);
    ctx->wbuf[15] = ctx->count[0] << 3;
    sha1_compile(ctx);

    /* extract the hash value as bytes in case the hash buffer is   */
    /* misaligned for 32-bit words                                  */
    for(i = 0; i < SHA1_DIGEST_SIZE; ++i)
        hval[i] = (unsigned char)(ctx->hash[i >> 2] >> (8 * (~i & 3)));
}
