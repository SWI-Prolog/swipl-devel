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
#include "clib.h"
#include <stdlib.h>
#include <time.h>
#include <sys/types.h>
#ifdef HAVE_UTIME_H
#include <utime.h>
#endif
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

static functor_t FUNCTOR_access1;
static functor_t FUNCTOR_modified1;
static functor_t FUNCTOR_changed1;
static atom_t    ATOM_now;

static int
add_time_option(term_t list, functor_t f, time_t time)
{ term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();

  while(PL_get_list(tail, head, tail))
  { if ( PL_unify_functor(head, f) )
    { term_t a = PL_new_term_ref();
      
      PL_get_arg(1, head, a);
      return PL_unify_float(a, (double)time);
    }
  }

  if ( PL_unify_list(tail, head, tail) )
    return PL_unify_term(head, PL_FUNCTOR, f, PL_FLOAT, (double)time);

  return FALSE;
}


static int
close_list(term_t list)
{ term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();

  while(PL_get_list(tail, head, tail))
    ;

  return PL_unify_nil(tail);
}


static int
get_time_option(term_t list, functor_t f, time_t def, time_t *tme)
{ term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();

  while(PL_get_list(tail, head, tail))
  { if ( PL_is_functor(head, f) )
    { term_t a = PL_new_term_ref();
      double f;
      
      PL_get_arg(1, head, a);
      if ( !PL_get_float(a, &f) )
      { atom_t now;

	if ( PL_get_atom(a, &now) && now == ATOM_now )
	{ time(tme);
	  return TRUE;
	} else
	  return pl_error(NULL, 0, NULL, ERR_TYPE, a, "time");
      }
      *tme = (long)f;
      return TRUE;
    }
  }

  *tme = def;
  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
set_file_time(+Spec, -Old, +New)
    Query/set file-times.  

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
pl_set_time_file(term_t spec, term_t old, term_t new)
{ char *name;
  struct stat sbuf;

  if ( !PL_get_file_name(spec, &name, 0) )
    return FALSE;

  if ( stat(name, &sbuf) )
    return pl_error(NULL, 0, NULL, ERR_ERRNO, name, "stat");

  add_time_option(old, FUNCTOR_access1,   sbuf.st_atime);
  add_time_option(old, FUNCTOR_modified1, sbuf.st_mtime);
  add_time_option(old, FUNCTOR_changed1,  sbuf.st_ctime);
  close_list(old);

  if ( !PL_get_nil(new) )
#ifdef HAVE_UTIME
  { struct utimbuf tbuf;

    if ( !get_time_option(new, FUNCTOR_access1,
			  sbuf.st_atime, &tbuf.actime) )
      return FALSE;
    if ( !get_time_option(new, FUNCTOR_modified1,
			  sbuf.st_mtime, &tbuf.modtime) )
      return FALSE;

    if ( utime(name, &tbuf) != 0 )
      return pl_error(NULL, 0, NULL, ERR_ERRNO, name, "set_time");
  }
#else
    return pl_error(NULL, 0, NULL, ERR_NOTIMPLEMENTED, "set_time", name);
#endif

  return TRUE;
}


install_t
install_files()
{ FUNCTOR_access1   = PL_new_functor(PL_new_atom("access"), 1);
  FUNCTOR_modified1 = PL_new_functor(PL_new_atom("modified"), 1);
  FUNCTOR_changed1  = PL_new_functor(PL_new_atom("changed"), 1);
  ATOM_now          = PL_new_atom("now");

  PL_register_foreign("set_time_file", 3, pl_set_time_file, 0);
}
