/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h> 
#endif
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <SWI-Prolog.h>
#include "clib.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Provide	an interface to the Unix system resources (getrlimit()/setrlimit()).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if !defined(RLIMIT_NOFILE) && defined(RLIMIT_OFILE)
#define RLIMIT_NOFILE RLIMIT_OFILE
#endif

foreign_t
pl_rlimit(term_t what, term_t old, term_t new)
{ char *s;
  int resource;
  struct rlimit rlim;

  if ( PL_get_atom_chars(what, &s) )
  { if      ( strcmp(s, "cpu") == 0 )
      resource = RLIMIT_CPU;
    else if ( strcmp(s, "fsize") == 0 )
      resource = RLIMIT_FSIZE;
    else if ( strcmp(s, "data") == 0 )
      resource = RLIMIT_DATA;
    else if ( strcmp(s, "stack") == 0 )
      resource = RLIMIT_STACK;
    else if ( strcmp(s, "core") == 0 )
      resource = RLIMIT_CORE;
#ifdef RLIMIT_RSS
    else if ( strcmp(s, "rss") == 0 )
      resource = RLIMIT_RSS;
#endif
#ifdef RLIMIT_MEMLOCK
    else if ( strcmp(s, "memlock") == 0 )
      resource = RLIMIT_MEMLOCK;
#endif
#ifdef RLIMIT_NPROC
    else if ( strcmp(s, "nproc") == 0 )
      resource = RLIMIT_NPROC;
#endif
#ifdef RLIMIT_NOFILE
    else if ( strcmp(s, "nofile") == 0 )
      resource = RLIMIT_NOFILE;
#endif
    else
      return pl_error("rlimit", 3, NULL, ERR_DOMAIN,
		      what, "resource");
  } else
    return pl_error("rlimit", 3, NULL, ERR_TYPE,
		    what, "atom");

  if ( getrlimit(resource, &rlim) == 0 )
  { int rval;

    if ( rlim.rlim_cur == RLIM_INFINITY )
      rval = PL_unify_atom_chars(old, "unlimited");
    else
      rval = PL_unify_integer(old, rlim.rlim_cur);

    if ( rval )
    { long n;

      if ( PL_get_long(new, &n) )
      { 
      set:
	if ( rlim.rlim_cur != n )
	{ rlim.rlim_cur = n;
	  if ( !setrlimit(resource, &rlim) == 0 )
	    return pl_error("rlimit", 3, NULL, ERR_ERRNO, errno);
	}
	return TRUE;
      } else if ( PL_get_atom_chars(new, &s) && strcmp(s, "unlimited") == 0 )
      { n = RLIM_INFINITY;
	goto set;
      } else
	return pl_error("rlimit", 3, NULL, ERR_TYPE,
			new, "integer_or_unlimited");
    } else
      return FALSE;
  } else
    return pl_error("rlimit", 3, NULL, ERR_ERRNO, errno);
}


install_t
install_rlimit()
{ PL_register_foreign("rlimit", 3, pl_rlimit, 0);
}
