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

#include "pl-incl.h"
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SWI-Prolog interface for runtime loading of foreign code (plugins).

There  are  two  interfaces  defined  for    this  that  are  united  by
library(shlib). The one defined here is for Unix `shared objects', while
the implementation for Windows is in pl-nt.c (loading DLL files).

It might be a  good  idea  to   integrate  these  two  interfaces at the
C-level, making runtime checks in library(shlib) unnecessary.

Currently, this interface is implemented only  for ELF systems (based on
dlopen()) and HPUX (based on slh_load()).   Despite, this covers a large
number of modern Unix platforms. To name a few: Solaris, Linux, freeBSD,
IRIX, HPUX.

Basically, 3 operations are required:

	open_shared_object(+File, [+Options], -Handle)
	    Load a shared object into the current image.

	call_shared_object_function(+Handle, +FunctionName)
	    Call a named function without arguments.  Return value
	    is ignored too.

	close_shared_object(+Handle)
	    Unload a shared object.

Feel free to add this functionality for your favorite OS and mail me the
contributions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *     DLOPEN() AND FRIENDS	*
		 *******************************/

#ifdef HAVE_DLOPEN			/* sysvr4, elf binaries */

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#endif /*HAVE_DLOPEN*/

#ifdef HAVE_SHL_LOAD			/* HPUX */

#include <dl.h>
#define dlopen(path, flags) shl_load((path), (flags), 0L)
#define dlclose(handle)	    shl_unload((handle))
#define dlerror()	    OsError()

void *
dlsym(shl_t handle, const char *name)
{ void *value;

  if ( shl_findsym(&handle, name, TYPE_PROCEDURE, &value) < 0 )
    return NULL;

  return value;
}

#define RTLD_LAZY	BIND_DEFERRED
#ifdef BIND_IMMEDIATE
#define RTLD_NOW	BIND_IMMEDIATE
#endif

#endif

#if defined(HAVE_DLOPEN) || defined(HAVE_SHL_LOAD) || defined(EMULATE_DLOPEN)

#ifndef RTLD_GLOBAL			/* solaris defines this */
#define RTLD_GLOBAL 0
#endif
#ifndef RTLD_NOW			/* implicit on some versions */
#define RTLD_NOW 0
#endif
#ifndef RTLD_LAZY			/* freeBSD doesn't have this? */
#define RTLD_LAZY 0
#endif

typedef int (*dl_funcptr)();

typedef struct dl_entry *DlEntry;
struct dl_entry
{ int	  id;				/* Prolog's identifier */
  void   *dlhandle;			/* DL libraries identifier */
  atom_t  file;				/* Loaded filed */
  DlEntry next;				/* Next in table */
};

int	dl_plid;			/* next id to give */
DlEntry dl_head;			/* loaded DL's */
DlEntry dl_tail;			/* end of this chain */

#define DL_NOW	  0x1
#define DL_GLOBAL 0x2

word
pl_open_shared_object(term_t file, term_t plhandle,
		      term_t flags)
{ void *dlhandle;
  atom_t afile;
  DlEntry e;
  int dlflags;
  int n;

  if ( PL_get_integer(flags, &n) )
  { dlflags = (n & DL_NOW) ? RTLD_NOW : RTLD_LAZY;
    if ( n & DL_GLOBAL )
      dlflags |= RTLD_GLOBAL;
  } else
    dlflags = RTLD_LAZY;

  if ( !PL_get_atom_ex(file, &afile) )
    fail;
  if ( !(dlhandle = dlopen(stringAtom(afile), dlflags)) )
    return PL_error(NULL, 0, NULL, ERR_SHARED_OBJECT_OP,
		    ATOM_open, dlerror());
  e = allocHeap(sizeof(struct dl_entry));
  e->id       = ++dl_plid;
  e->dlhandle = dlhandle;
  e->file     = afile;
  e->next     = NULL;
  if ( !dl_tail )
    dl_head = dl_tail = e;
  else
    dl_tail->next = e;

  return PL_unify_integer(plhandle, e->id);
}


static DlEntry
find_dl_entry(term_t h)
{ DlEntry e;
  int id;

  if ( PL_get_integer(h, &id) )
  { for(e = dl_head; e; e = e->next)
    { if ( e->id == id )
	return e;
    }
    PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_shared_object_handle, h);
    return NULL;
  }
  
  PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_shared_object_handle, h);

  return NULL;
}


word
pl_close_shared_object(term_t plhandle)
{ DlEntry e = find_dl_entry(plhandle);

  if ( e && e->dlhandle) 
  { dlclose(e->dlhandle);
    e->dlhandle = NULL;

    succeed;
  }

  fail;
}


word
pl_call_shared_object_function(term_t plhandle, term_t name)
{ DlEntry e = find_dl_entry(plhandle);
  char *fname;
  dl_funcptr ef;

  if ( !e || !e->dlhandle ||
       !PL_get_chars_ex(name, &fname, CVT_ALL) )
    fail;
  
#ifdef LD_SYMBOL_PREFIX
  { char symname[MAXSYMBOLLEN+1];
    
    if ( strlen(fname)+strlen(LD_SYMBOL_PREFIX) > MAXSYMBOLLEN )
      return PL_error(NULL, 0,
		      "Symbol too long",
		      ERR_REPRESENTATION,
		      PL_new_atom("symbol"));

    strcpy(symname, LD_SYMBOL_PREFIX);
    strcat(symname, fname);
    ef = (dl_funcptr) dlsym(e->dlhandle, symname);
  }
#else
  ef = (dl_funcptr) dlsym(e->dlhandle, fname);
#endif
  if ( ef )
  { (*ef)();
    succeed;
  } else
    fail;
}

#else /*HAVE_DLOPEN*/

word
pl_open_shared_object(term_t file, term_t plhandle, term_t flags)
{ return warning("open_shared_object/3: not ported to this machine");
}

#endif /*HAVE_DLOPEN*/
