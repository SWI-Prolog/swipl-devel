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

#include <SWI-Prolog.h>
#include "error.h"
#include "sha1/sha1.h"
#include "sha1/sha2.h"
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


static foreign_t
pl_sha_hash(term_t from, term_t hash, term_t options)
{ term_t opts = PL_copy_term_ref(options);
  term_t opt = PL_new_term_ref();
  sha_algorithm algorithm = ALGORITHM_SHA1;
  char *data;
  size_t datalen;

  while(PL_get_list(opts, opt, opts))
  { atom_t aname;
    int arity;

    if ( PL_get_name_arity(opt, &aname, &arity) && arity == 1 )
    { term_t a = PL_new_term_ref();

      PL_get_arg(1, opt, a);

      if ( aname == ATOM_algorithm )
      { atom_t a_algorithm;

	if ( !PL_get_atom(a, &a_algorithm) )
	  return pl_error("sha_hash", 1, NULL, ERR_TYPE, a, "algorithm");
	if ( a_algorithm == ATOM_sha1 )
	  algorithm = ALGORITHM_SHA1;
	else if ( a_algorithm == ATOM_sha224 )
	  algorithm = ALGORITHM_SHA224;
	else if ( a_algorithm == ATOM_sha256 )
	  algorithm = ALGORITHM_SHA256;
	else if ( a_algorithm == ATOM_sha384 )
	  algorithm = ALGORITHM_SHA384;
	else if ( a_algorithm == ATOM_sha512 )
	  algorithm = ALGORITHM_SHA512;
	else
	  return pl_error("sha_hash", 1, NULL, ERR_DOMAIN, a, "algorithm");
      }
    } else
    { return pl_error("sha_hash", 1, NULL, ERR_TYPE, opt, "option");
    }
  }

  if ( !PL_get_nil(opts) )
    return pl_error("sha_hash", 1, NULL, ERR_TYPE, opts, "list");

  if ( !PL_get_nchars(from, &datalen, &data, CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
    return FALSE;

  switch(algorithm)
  { case ALGORITHM_SHA1:
    { unsigned char hval[SHA1_DIGEST_SIZE];

      sha1(hval, (unsigned char*)data, (unsigned long)datalen);

      return PL_unify_list_ncodes(hash, SHA1_DIGEST_SIZE, (char*)hval);
    }
    case ALGORITHM_SHA224:
    { unsigned char hval[SHA224_DIGEST_SIZE];

      sha224(hval, (unsigned char*)data, (unsigned long)datalen);

      return PL_unify_list_ncodes(hash, SHA224_DIGEST_SIZE, (char*)hval);
    }
    case ALGORITHM_SHA256:
    { unsigned char hval[SHA256_DIGEST_SIZE];

      sha256(hval, (unsigned char*)data, (unsigned long)datalen);

      return PL_unify_list_ncodes(hash, SHA256_DIGEST_SIZE, (char*)hval);
    }
    case ALGORITHM_SHA384:
    { unsigned char hval[SHA384_DIGEST_SIZE];

      sha384(hval, (unsigned char*)data, (unsigned long)datalen);

      return PL_unify_list_ncodes(hash, SHA384_DIGEST_SIZE, (char*)hval);
    }
    case ALGORITHM_SHA512:
    { unsigned char hval[SHA512_DIGEST_SIZE];

      sha512(hval, (unsigned char*)data, (unsigned long)datalen);

      return PL_unify_list_ncodes(hash, SHA512_DIGEST_SIZE, (char*)hval);
    }
    default:
      assert(0);
      return FALSE;
  }
}


#define MKATOM(n) ATOM_ ## n = PL_new_atom(#n);

install_t
install_sha4pl()
{ MKATOM(sha1);
  MKATOM(sha224);
  MKATOM(sha256);
  MKATOM(sha384);
  MKATOM(sha512);
  MKATOM(algorithm);

  PL_register_foreign("sha_hash", 3, pl_sha_hash, 0);
}
