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

#ifdef DEFINE_XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_CRYPT_H
#include <crypt.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include "clib.h"

/* md5passwd.c */
extern char *md5_crypt(const char *pw, const char *salt);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Simple interface to the Unix   password  encryption routine. Implemented
for providing authorization in  the   multi-threaded  Prolog  based HTTP
deamon and therefore providing a thread-safe interface.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef _REENTRANT
#include <pthread.h>

static pthread_mutex_t crypt_mutex = PTHREAD_MUTEX_INITIALIZER;
#define LOCK() pthread_mutex_lock(&crypt_mutex)
#define UNLOCK() pthread_mutex_unlock(&crypt_mutex)
#else
#define LOCK()
#define UNLOCK()
#endif


static foreign_t
pl_crypt(term_t passwd, term_t encrypted)
{ char *pw, *e;
  char salt[20];

  if ( !PL_get_chars(passwd, &pw, CVT_ATOM|CVT_STRING|CVT_LIST|BUF_RING) )
    return pl_error("crypt", 2, NULL, ERR_ARGTYPE,
		    1, passwd, "text");

  if ( PL_get_chars(encrypted, &e, CVT_ATOM|CVT_STRING|CVT_LIST|BUF_RING) )
  { int rval;
    char *s2;
    
    if ( strncmp(e, "$1$", 3) == 0 )	/* MD5 Hash */
    { char *p = strchr(e+3, '$');
      size_t slen;

      if ( p && (slen=(size_t)(p-e-3)) < sizeof(salt) )
      { strncpy(salt, e+3, slen);
	salt[slen] = 0;
	s2 = md5_crypt(pw, salt);
	return (strcmp(s2, e) == 0) ? TRUE : FALSE;
      } else
      { Sdprintf("No salt???\n");
	return FALSE;
      }
    } else
    {
#ifdef HAVE_CRYPT
      salt[0] = e[0];
      salt[1] = e[1];
      salt[2] = '\0';

      LOCK();
      s2 = crypt(pw, salt);
      rval = (strcmp(s2, e) == 0 ? TRUE : FALSE);
      UNLOCK();

      return rval;
#else
      return PL_warning("Password encryption not supported");
#endif
    }
  } else
  { term_t tail = PL_copy_term_ref(encrypted);
    term_t head = PL_new_term_ref();
    int slen = 2;
    int n;
    int (*unify)(term_t t, const char *s) = PL_unify_list_codes;
    char *s2;
    int rval;

    for(n=0; n<slen; n++)
    { if ( PL_get_list(tail, head, tail) )
      { int i;
	char *t;

	if ( PL_get_integer(head, &i) && i>=0 && i<=255 )
	{ salt[n] = i;
	} else if ( PL_get_atom_chars(head, &t) && t[1] == '\0' )
	{ salt[n] = t[0];
	  unify = PL_unify_list_chars;
	} else
	{ return pl_error("crypt", 2, NULL, ERR_ARGTYPE,
			  2, head, "character");
	}

	if ( n == 1 && salt[0] == '$' && salt[1] == '1' )
	  slen = 3;
	else if ( n == 2 && salt[2] == '$' )
	  slen = 8+3;
      } else
	break;
    }

#ifndef HAVE_CRYPT
    slen = 8;
#endif

    for( ; n < slen; n++ )
    { int c = 'a'+(int)(26.0*rand()/(RAND_MAX+1.0));

      if ( rand() & 0x1 )
	c += 'A' - 'a';

      salt[n] = c;
    }
    salt[n] = 0;
    LOCK();
    if ( slen > 2 )
    { s2 = md5_crypt(pw, salt);
    } else
    {
#ifdef HAVE_CRYPT
      s2 = crypt(pw, salt);
#endif
    }
    rval = (*unify)(encrypted, s2);
    UNLOCK();
    
    return rval;
  }
}


install_t
install_crypt()
{ PL_register_foreign("crypt", 2, pl_crypt, 0);
}
