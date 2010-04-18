/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, VU University, Amsterdam

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

#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include "libstemmer_c/include/libstemmer.h"
#include <pthread.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

#define CACHE_SIZE (20)			/* cache CACHE_SIZE languages */

static functor_t FUNCTOR_error2;	/* error(Formal, Context) */
static functor_t FUNCTOR_type_error2;	/* type_error(Term, Expected) */
static functor_t FUNCTOR_domain_error2;	/* domain_error(Term, Expected) */

static int
type_error(const char *expected, term_t actual)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_type_error2,
		         PL_CHARS, expected,
		         PL_TERM, actual,
		       PL_VARIABLE) )
    return PL_raise_exception(ex);

  return FALSE;
}


static int
domain_error(const char *domain, term_t actual)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_FUNCTOR, FUNCTOR_domain_error2,
		         PL_CHARS, domain,
		         PL_TERM, actual,
		       PL_VARIABLE) )
  return PL_raise_exception(ex);

  return FALSE;
}


static int
resource_error(const char *res)
{ term_t ex;

  if ( (ex = PL_new_term_ref()) &&
       PL_unify_term(ex,
		     PL_FUNCTOR, FUNCTOR_error2,
		       PL_CHARS, "resource_error",
		       PL_CHARS, res) )
    return PL_raise_exception(ex);

  return FALSE;
}



typedef struct
{ atom_t		language;
  struct sb_stemmer    *stemmer;
} stemmer;

typedef struct
{ stemmer	stemmers[CACHE_SIZE];
} stem_cache;

static pthread_key_t stem_key;
#ifndef __WINDOWS__
static pthread_once_t stem_key_once = PTHREAD_ONCE_INIT;
#endif

static void
stem_destroy_cache(void *buf)
{ stem_cache *cache = buf;
  int i;

  for(i=0; i<CACHE_SIZE; i++)
  { if ( cache->stemmers[i].stemmer )
    { PL_unregister_atom(cache->stemmers[i].language);
      sb_stemmer_delete(cache->stemmers[i].stemmer);
    }
  }

  PL_free(cache);
}

static void
stem_key_alloc(void)
{ pthread_key_create(&stem_key, stem_destroy_cache);
}

static stem_cache *
get_cache(void)
{ stem_cache *cache;

#ifndef __WINDOWS__
  pthread_once(&stem_key_once, stem_key_alloc);
#endif

  if ( (cache=(stem_cache*)pthread_getspecific(stem_key)) )
    return cache;
  if ( (cache = PL_malloc(sizeof(stem_cache))) )
    memset(cache, 0, sizeof(*cache));

  pthread_setspecific(stem_key, cache);

  return cache;
}


static int
get_lang_stemmer(term_t t, struct sb_stemmer **stemmer)
{ stem_cache *cache = get_cache();
  atom_t lang;
  int i;

  if ( !PL_get_atom(t, &lang) )
    return type_error("atom", t);

  for(i=0; i<CACHE_SIZE; i++)
  { if ( cache->stemmers[i].language == lang )
    { *stemmer = cache->stemmers[i].stemmer;
      return TRUE;
    }
  }
  for(i=0; i<CACHE_SIZE; i++)
  { if ( !cache->stemmers[i].stemmer )
    { struct sb_stemmer *st;

      if ( !(st= sb_stemmer_new(PL_atom_chars(lang), NULL)) )
      { if ( errno == ENOMEM )
	  return resource_error("memory");
	else
	  return domain_error("snowball_algorithm", t);
      }

      cache->stemmers[i].language = lang;
      cache->stemmers[i].stemmer  = st;
      PL_register_atom(cache->stemmers[i].language);

      *stemmer = cache->stemmers[i].stemmer;
      return TRUE;
    }
  }

  assert(0);				/* TBD: clean cache */
  return FALSE;
}


static foreign_t
snowball(term_t lang, term_t in, term_t out)
{ struct sb_stemmer *stemmer = NULL;
  char *s;
  size_t len, olen;
  const sb_symbol *stemmed;

  if ( !get_lang_stemmer(lang, &stemmer) )
    return FALSE;
  if ( !PL_get_nchars(in, &len, &s,
		      CVT_ATOM|CVT_STRING|CVT_LIST|REP_UTF8|CVT_EXCEPTION) )
    return FALSE;

  if ( !(stemmed = sb_stemmer_stem(stemmer, (const sb_symbol*)s, (int)len)) )
    return resource_error("memory");
  olen = sb_stemmer_length(stemmer);

  return PL_unify_chars(out, PL_ATOM|REP_UTF8, olen, (const char*)stemmed);
}


static foreign_t
snowball_algorithms(term_t list)
{ term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();
  const char **algos = sb_stemmer_list();
  int i;

  for(i=0; algos[i]; i++)
  { if ( !PL_unify_list(tail, head, tail) ||
	 !PL_unify_atom_chars(head, algos[i]) )
      return FALSE;
  }

  return PL_unify_nil(tail);
}

#define MKFUNCTOR(name, arity) PL_new_functor(PL_new_atom(name), arity)

install_t
install_snowball()
{ assert(sizeof(sb_symbol) == sizeof(char));

  FUNCTOR_error2        = MKFUNCTOR("error", 2);
  FUNCTOR_type_error2   = MKFUNCTOR("type_error", 2);
  FUNCTOR_domain_error2 = MKFUNCTOR("domain_error", 2);

  PL_register_foreign("snowball", 3, snowball, 0);
  PL_register_foreign("snowball_algorithms", 1, snowball_algorithms, 0);

#ifdef __WINDOWS__
  stem_key_alloc();
#endif
}
