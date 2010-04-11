/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2010, University of Amsterdam

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
#define AC_TERM_WALK 1
#include "pl-termwalk.c"

/* type to hold the SHA256 context  */

#define SHA1_DIGEST_SIZE 20

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

typedef struct
{ int		var_count;
  sha1_ctx	ctx[1];			/* The SHA1 Context */
  segstack	vars;
  char		vars_first_chunk[256];
} sha1_state;

typedef enum
{ E_OK,
  E_ATTVAR,
  E_RESOURCE,
  E_CYCLE
} status;


static int
push_var(Word p, sha1_state *state)
{ return pushSegStack(&state->vars, &p);
}


#define HASH(p,l) sha1_hash((const unsigned char*)(p), (l), state->ctx)

static status
variant_sha1(ac_term_agenda *agenda, sha1_state *state ARG_LD)
{ Word p;

  while( (p=ac_nextTermAgenda(agenda)) )
  { word w = *p;

    switch(tag(w))
    { case TAG_VAR:
      { if ( isVar(w) )
	{ word i = state->var_count++;

	  if ( !push_var(p, state) )
	    return E_RESOURCE;
	  *p = (i<<LMASK_BITS)|MARK_MASK;
	}
        HASH("V", 1);
	HASH(p, sizeof(word));
	continue;
      }
      case TAG_ATTVAR:
      { return E_ATTVAR;
      }
      case TAG_ATOM:
      { Atom av = atomValue(w);
	HASH("A", 1);
	HASH(&av->length, sizeof(av->length));
	HASH(av->name, av->length);
	HASH(av->type->name,
	     strlen(av->type->name));

					/* TBD: Include type */
	continue;
      }
      case TAG_INTEGER:
      { if ( !isIndirect(w) )
	{ int64_t val = valInteger(w);

	  HASH("I", 1);
	  HASH(&val, sizeof(val));
	  continue;
	}
      }
      /*FALLTHROUGH*/
      case TAG_STRING:
      case TAG_FLOAT:
      { Word d = addressIndirect(w);
	size_t n = wsizeofInd(*d);

	HASH("X", 1);
	HASH(d, n*sizeof(word));
	continue;
      }
      case TAG_COMPOUND:
      { functor_t f;

	switch(ac_pushTermAgenda(agenda, w, &f))
	{ case -1:			/* Resource error */
	    return E_RESOURCE;
	  case FALSE:			/* Cycle */
	    return E_CYCLE;
	  default:
	  { FunctorDef fd = valueFunctor(f);
	    int arity = arityFunctor(f);

	    Atom fn = atomValue(fd->name);

	    HASH("T", 1);
	    HASH(&fn->length, sizeof(fn->length));
	    HASH(fn->name, fn->length);
	    HASH(&arity, sizeof(arity));
	  }
	}
	continue;
      }
    }
  }

  return E_OK;
}


/** variant_sha1(@Term, -SHA1:string) is det.

Compute an SHA1 hash for Term. The hash  is designed such that two terms
have the same hash iff variant(T1,T2) is true. This implies that we must
basically execute numbervars.
*/

static
PRED_IMPL("variant_sha1", 2, variant_sha1, 0)
{ PRED_LD
  int rc;
  ac_term_agenda agenda;
  sha1_state state;
  unsigned char sha1[SHA1_DIGEST_SIZE];
  char hex[SHA1_DIGEST_SIZE*2];
  const char hexd[] = "0123456789abcdef";
  char *o;
  const unsigned char *i;
  Word p;
  int n;

  state.var_count = 0;
  sha1_begin(state.ctx);
  ac_initTermAgenda(&agenda, valTermRef(A1));
  initSegStack(&state.vars, sizeof(Word),
	       sizeof(state.vars_first_chunk), state.vars_first_chunk);
  rc = variant_sha1(&agenda, &state PASS_LD);
  ac_clearTermAgenda(&agenda);
  while(popSegStack(&state.vars, &p))
    setVar(*p);

  SECURE(checkData(valTermRef(A1)));

  switch( rc )
  { case E_ATTVAR:
      return PL_error(NULL, 0, NULL,
		      ERR_TYPE, ATOM_free_of_attvar, A1);
    case E_CYCLE:
      return PL_error(NULL, 0, NULL,
		      ERR_TYPE, ATOM_acyclic_term, A1);
    case E_RESOURCE:
      return PL_error(NULL, 0, NULL,
		      ERR_RESOURCE, ATOM_memory);
  }

  sha1_end(sha1, state.ctx);

  o = hex;
  i = sha1;
  for(n=0; n<SHA1_DIGEST_SIZE; n++,i++)
  { *o++ = hexd[*i >> 4];
    *o++ = hexd[*i&0x0f];
  }

  return PL_unify_chars(A2, PL_ATOM|REP_ISO_LATIN_1, sizeof(hex), hex);
}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(termhash)
  PRED_DEF("variant_sha1", 2, variant_sha1, 0)
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

VOID_RETURN sha1_hash(const unsigned char data[], unsigned long len, sha1_ctx ctx[1])
{   uint32_t pos = (uint32_t)(ctx->count[0] & SHA1_MASK),
            space = SHA1_BLOCK_SIZE - pos;
    const unsigned char *sp = data;

    if((ctx->count[0] += len) < len)
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
    ctx->wbuf[i >> 2] &= 0xffffff80 << 8 * (~i & 3);
    ctx->wbuf[i >> 2] |= 0x00000080 << 8 * (~i & 3);

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
