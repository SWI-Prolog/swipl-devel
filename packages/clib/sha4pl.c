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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define _ISOC99_SOURCE
#define USE_SHA256 1

#include <SWI-Prolog.h>
#include "error.h"
#include "sha1/sha1.h"
#include "sha1/sha2.h"
#include "sha1/hmac.h"
#include <assert.h>

static atom_t ATOM_sha1;
static atom_t ATOM_sha224;
static atom_t ATOM_sha256;
static atom_t ATOM_sha384;
static atom_t ATOM_sha512;
static atom_t ATOM_algorithm;

typedef enum
{ ALGORITHM_SHA1,
  ALGORITHM_SHA224,
  ALGORITHM_SHA256,
  ALGORITHM_SHA384,
  ALGORITHM_SHA512
} sha_algorithm;


typedef struct
{ sha_algorithm algorithm;
  size_t	digest_size;
  term_t	algorithm_term;
} optval;

static int
sha_options(term_t options, optval *result)
{ term_t opts = PL_copy_term_ref(options);
  term_t opt = PL_new_term_ref();

					/* defaults */
  memset(result, 0, sizeof(*result));
  result->algorithm   = ALGORITHM_SHA1;
  result->digest_size = SHA1_DIGEST_SIZE;

  while(PL_get_list(opts, opt, opts))
  { atom_t aname;
    int arity;

    if ( PL_get_name_arity(opt, &aname, &arity) && arity == 1 )
    { term_t a = PL_new_term_ref();

      PL_get_arg(1, opt, a);

      if ( aname == ATOM_algorithm )
      { atom_t a_algorithm;
	
	result->algorithm_term = a;
	if ( !PL_get_atom(a, &a_algorithm) )
	  return pl_error(NULL, 0, NULL, ERR_TYPE, a, "algorithm");
	if ( a_algorithm == ATOM_sha1 )
	{ result->algorithm   = ALGORITHM_SHA1;
	  result->digest_size = SHA1_DIGEST_SIZE;
	} else if ( a_algorithm == ATOM_sha224 )
	{ result->algorithm = ALGORITHM_SHA224;
	  result->digest_size = SHA224_DIGEST_SIZE;
	} else if ( a_algorithm == ATOM_sha256 )
	{ result->algorithm = ALGORITHM_SHA256;
	  result->digest_size = SHA256_DIGEST_SIZE;
	} else if ( a_algorithm == ATOM_sha384 )
	{ result->algorithm = ALGORITHM_SHA384;
	  result->digest_size = SHA384_DIGEST_SIZE;
	} else if ( a_algorithm == ATOM_sha512 )
	{ result->algorithm = ALGORITHM_SHA512;
	  result->digest_size = SHA512_DIGEST_SIZE;
	} else
	  return pl_error(NULL, 0, NULL, ERR_DOMAIN, a, "algorithm");
      }
    } else
    { return pl_error(NULL, 0, NULL, ERR_TYPE, opt, "option");
    }
  }

  if ( !PL_get_nil(opts) )
    return pl_error("sha_hash", 1, NULL, ERR_TYPE, opts, "list");

  return TRUE;
}




static foreign_t
pl_sha_hash(term_t from, term_t hash, term_t options)
{ char *data;
  size_t datalen;
  optval opts;
  unsigned char hval[SHA2_MAX_DIGEST_SIZE];

  if ( !sha_options(options, &opts) )
    return FALSE;

  if ( !PL_get_nchars(from, &datalen, &data, 
		      CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
    return FALSE;

  if ( opts.algorithm == ALGORITHM_SHA1 )
  { sha1((unsigned char*)hval,
	 (unsigned char*)data, (unsigned long)datalen);
  } else
  { sha2((unsigned char*)hval, (unsigned long) opts.digest_size,
	 (unsigned char*)data, (unsigned long)datalen);
  }

  return PL_unify_list_ncodes(hash, opts.digest_size, (char*)hval);
}


static foreign_t
pl_hmac_sha(term_t key, term_t data, term_t mac, term_t options)
{ char *sdata, *skey;
  size_t datalen, keylen;
  optval opts;
  unsigned char digest[SHA2_MAX_DIGEST_SIZE];

  if ( !PL_get_nchars(key, &keylen, &skey, 
		      CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
    return FALSE;
  if ( !PL_get_nchars(data, &datalen, &sdata, 
		      CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
    return FALSE;

  if ( !sha_options(options, &opts) )
    return FALSE;

  switch(opts.algorithm)
  { case ALGORITHM_SHA1:
      hmac_sha1((unsigned char*)skey, (unsigned long)keylen,
		(unsigned char*)sdata, (unsigned long)datalen,
		digest, (unsigned long)opts.digest_size);
      break;
    case ALGORITHM_SHA256:
      hmac_sha256((unsigned char*)skey, (unsigned long)keylen,
		  (unsigned char*)sdata, (unsigned long)datalen,
		  digest, (unsigned long)opts.digest_size);
      break;
    default:
      return pl_error(NULL, 0, "HMAC-SHA only for SHA-1 and SHA-256",
		      ERR_DOMAIN, opts.algorithm_term, "algorithm");
  }

  return PL_unify_list_ncodes(mac, opts.digest_size, (char*)digest);
}


#define MKATOM(n) ATOM_ ## n = PL_new_atom(#n);

install_t
install_sha4pl()
{ MKATOM(sha1);				/* =160 */
  MKATOM(sha224);
  MKATOM(sha256);
  MKATOM(sha384);
  MKATOM(sha512);
  MKATOM(algorithm);

  PL_register_foreign("sha_hash", 3, pl_sha_hash, 0);
  PL_register_foreign("hmac_sha", 4, pl_hmac_sha, 0);
}
