/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2013, University of Amsterdam
                              VU University Amsterdam
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

#include "pl-incl.h"
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

#define LOCK()   PL_LOCK(L_FOREIGN)
#define UNLOCK() PL_UNLOCK(L_FOREIGN)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SWI-Prolog interface for runtime loading of foreign code (plugins).

Currently, this interface is implemented only  for ELF systems (based on
dlopen()) and HPUX (based on slh_load()).   Despite, this covers a large
number of modern Unix platforms. To name a few: Solaris, Linux, freeBSD,
IRIX, HPUX, MacOS X.

For some platforms we emulate the ELF   interface and set the cpp symbol
EMULATE_DLOPEN. You find examples in pl-nt.c   (for Win32) and pl-beos.c
(for BeOS).

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

#else /*HAVE_DLOPEN*/

#ifdef HAVE_SHL_LOAD			/* HPUX */

#include <dl.h>
#define dlopen(path, flags) shl_load((path), (flags), 0L)
#define dlclose(handle)	    shl_unload((handle))
#define dlerror()	    OsError()

void *
dlsym(void *handle, const char *name)
{ void *value;
  shl_t h = handle;

  if ( shl_findsym(&h, name, TYPE_PROCEDURE, &value) < 0 )
    return NULL;

  return value;
}

#define RTLD_LAZY	BIND_DEFERRED
#ifdef BIND_IMMEDIATE
#define RTLD_NOW	BIND_IMMEDIATE
#endif

#endif /*HAVE_SHL_LOAD*/
#endif /*HAVE_DLOPEN*/

#if defined(HAVE_DLOPEN) || defined(HAVE_SHL_LOAD) || defined(EMULATE_DLOPEN)
#define HAVE_SHARED_OBJECTS

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
under_valgrind()

True if we are running under valgrind.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef HAVE_VALGRIND_VALGRIND_H
#include <valgrind/valgrind.h>
#else
#define RUNNING_ON_VALGRIND (getenv("VALGRIND_OPTS") != NULL)
#endif

static int
under_valgrind(void)
{ static int vg = -1;

  if ( vg == -1 )
  {
#ifdef RUNNING_ON_VALGRIND
    if ( RUNNING_ON_VALGRIND )
      vg = TRUE;
    else
#endif
      vg = FALSE;
  }

  return vg;
}


static
PRED_IMPL("$open_shared_object", 3, open_shared_object, 0)
{ PRED_LD
  void *dlhandle;
  char *fn;
  atom_t afile;
  DlEntry e;
  int dlflags;
  int n;

  term_t file     = A1;
  term_t plhandle = A2;
  term_t flags    = A3;


  if ( PL_get_integer(flags, &n) )
  { dlflags = (n & DL_NOW) ? RTLD_NOW : RTLD_LAZY;
    if ( n & DL_GLOBAL )
      dlflags |= RTLD_GLOBAL;
  } else
    dlflags = RTLD_LAZY;

  if ( !PL_get_atom_ex(file, &afile) ||
       !PL_get_file_name(file, &fn, 0) )
    fail;
  if ( !(dlhandle = dlopen(fn, dlflags)) )
    return PL_error(NULL, 0, NULL, ERR_SHARED_OBJECT_OP,
		    ATOM_open, dlerror());

  e = allocHeapOrHalt(sizeof(struct dl_entry));

  LOCK();
  e->id       = ++dl_plid;
  e->dlhandle = dlhandle;
  e->file     = afile;
  e->next     = NULL;

  if ( !dl_tail )
  { dl_tail = e;
    dl_head = e;
  } else
  { dl_tail->next = e;
    dl_tail = e;
  }
  UNLOCK();

  return PL_unify_integer(plhandle, e->id);
}


static DlEntry
find_dl_entry(term_t h)
{ GET_LD
  DlEntry e;
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


static
PRED_IMPL("close_shared_object", 1, close_shared_object, 0)
{ DlEntry e = find_dl_entry(A1);

  if ( e && e->dlhandle)
  { if ( !under_valgrind() )
      dlclose(e->dlhandle);
    e->dlhandle = NULL;

    succeed;
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Some systems (notably MacOS X) prefixes symbols with _. In some version
of this OS, dlsym() adds an _, in others not.  We'll try to work around
this junk with a runtime test ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("call_shared_object_function", 2, call_shared_object_function,
	  PL_FA_TRANSPARENT)
{ DlEntry e = find_dl_entry(A1);
  char *fname;
  dl_funcptr ef;

  if ( !e || !e->dlhandle ||
       !PL_get_chars(A2, &fname, CVT_ALL|CVT_EXCEPTION) )
    fail;

#ifdef LD_SYMBOL_PREFIX			/* first try plain anyway */
  if ( !(ef = (dl_funcptr) dlsym(e->dlhandle, fname)) )
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unload all foreign libraries.  As we are doing this at the very end of
the cleanup, it should be safe now.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
cleanupForeign(void)
{ DlEntry e, next;

  for(e = dl_head; e; e = next)
  { next = e->next;

    if ( e->dlhandle )
    { if ( !under_valgrind() )
	dlclose(e->dlhandle);
    }

    freeHeap(e, sizeof(*e));
  }

  dl_plid = 0;
  dl_head = dl_tail = NULL;
}

#else /*HAVE_DLOPEN*/

static
PRED_IMPL("$open_shared_object", 3, open_shared_object, 0)
{ return notImplemented("open_shared_object", 3);
}

#endif /*HAVE_DLOPEN*/

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(dlopen)
  PRED_DEF("$open_shared_object", 3, open_shared_object, 0)
#ifdef HAVE_SHARED_OBJECTS
  PRED_DEF("close_shared_object", 1, close_shared_object, 0)
  PRED_DEF("call_shared_object_function", 2, call_shared_object_function,
	   PL_FA_TRANSPARENT)
#endif
EndPredDefs
