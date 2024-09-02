/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2022, University of Amsterdam
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

#define _GNU_SOURCE			/* get dladdr() */
#include "pl-load.h"
#include "pl-fli.h"

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

#ifndef O_STATIC_EXTENSIONS

		 /*******************************
		 *     DLOPEN() AND FRIENDS	*
		 *******************************/

#ifndef EMULATE_DLOPEN
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
#endif /*EMULATE_DLOPEN*/

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

#ifndef EMULATE_DLOPEN
void *
PL_dlopen(const char *file, int flags)
{ return dlopen(file, flags);
}

const char *
PL_dlerror(void)
{ return dlerror();
}

void *
PL_dlsym(void *handle, char *symbol)
{
#ifdef RTLD_DEFAULT
  if ( !handle )
    handle = RTLD_DEFAULT;
#endif
  return dlsym(handle, symbol);
}

int
PL_dlclose(void *handle)
{ return dlclose(handle);
}

#endif /*EMULATE_DLOPEN*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
under_valgrind()

True if we are running under valgrind.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef HAVE_VALGRIND_VALGRIND_H
#include <valgrind/valgrind.h>
#endif
#ifndef RUNNING_ON_VALGRIND
#define RUNNING_ON_VALGRIND (getenv("VALGRIND_OPTS") != NULL)
#endif

static int
under_valgrind(void)
{ static int vg = -1;

  if ( vg == -1 )
  {
#ifdef __SANITIZE_ADDRESS__
    char *s;

    if ( (s=getenv("ASAN_OPTIONS")) && strstr(s,"detect_leaks=1") )
    { vg = true;
      return vg;
    }
#endif

    if ( RUNNING_ON_VALGRIND )
    { vg = true;
      return vg;
    }

    vg = false;
  }

  return vg;
}

static const PL_option_t open_shared_object_options[] =
{ { ATOM_now,		    OPT_BOOL },
  { ATOM_global,            OPT_BOOL },
  { ATOM_delete,	    OPT_BOOL },
  { ATOM_load,              OPT_BOOL },
  { ATOM_deepbind,          OPT_BOOL },
  { ATOM_resolve,	    OPT_ATOM },
  { ATOM_visibility,        OPT_ATOM },
  { NULL_ATOM,		    0 }
};

#ifndef RTLD_NODELETE
#define RTLD_NODELETE 0
#endif
#ifndef RTLD_NOLOAD
#define RTLD_NOLOAD 0
#endif
#ifndef RTLD_DEEPBIND
#define RTLD_DEEPBIND 0
#endif

static int
atom_domain_error(const char *domain, atom_t found)
{ term_t t;

  return ( (t=PL_new_term_ref()) &&
	   PL_put_atom(t, found) &&
	   PL_domain_error(domain, found) );
}

#define open_shared_object(file, plhandle, options) \
	LDFUNC(open_shared_object, file, plhandle, options)

static foreign_t
open_shared_object(DECL_LD term_t file, term_t plhandle, term_t options)
{ void *dlhandle;
  char *fn;
  atom_t afile;
  DlEntry e;
  int dlflags;
  int now = -1;
  int global = -1;
  int delete = true;
  int load = true;
  int deepbind = false;
  atom_t resolve = 0;
  atom_t visibility = 0;

  if ( options &&
       !PL_scan_options(options, 0, "open_shared_object_options",
			open_shared_object_options,
			&now, &global, &delete, &load, &deepbind,
			&resolve, &visibility) )
    return false;

  if ( resolve == ATOM_now )
    now = true;
  else if ( resolve == ATOM_lazy )
    now = false;
  else if ( resolve )
    return atom_domain_error("resolve", resolve);
  else if ( now == -1 )
    now = false;

  if ( visibility == ATOM_global )
    global = true;
  else if ( visibility == ATOM_local )
    global = false;
  else if ( visibility )
    return atom_domain_error("visibility", visibility);
  else if ( global == -1 )
    global = false;

  dlflags = now ? RTLD_NOW : RTLD_LAZY;
  if ( global   ) dlflags |= RTLD_GLOBAL;
  if ( !delete  ) dlflags |= RTLD_NODELETE;
  if ( !load    ) dlflags |= RTLD_NOLOAD;
  if ( deepbind ) dlflags |= RTLD_DEEPBIND;

  if ( !PL_get_atom_ex(file, &afile) ||
       !PL_get_file_name(file, &fn, 0) )
    fail;
  if ( !(dlhandle = PL_dlopen(fn, dlflags)) )
  { if ( !load )
      return false;
    return PL_error(NULL, 0, NULL, ERR_SHARED_OBJECT_OP,
		    ATOM_open, PL_dlerror());
  }

  e = allocHeapOrHalt(sizeof(struct dl_entry));

  PL_LOCK(L_FOREIGN);
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
  PL_UNLOCK(L_FOREIGN);

  return PL_unify_integer(plhandle, e->id);
}

static
PRED_IMPL("open_shared_object", 3, open_shared_object, 0)
{ PRED_LD
  return open_shared_object(A1, A2, A3);
}

static
PRED_IMPL("open_shared_object", 2, open_shared_object, 0)
{ PRED_LD
  return open_shared_object(A1, A2, 0);
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
      PL_dlclose(e->dlhandle);
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
  if ( !(ef = (dl_funcptr) PL_dlsym(e->dlhandle, fname)) )
  { char symname[MAXSYMBOLLEN+1];

    if ( strlen(fname)+strlen(LD_SYMBOL_PREFIX) > MAXSYMBOLLEN )
      return PL_error(NULL, 0,
		      "Symbol too long",
		      ERR_REPRESENTATION,
		      PL_new_atom("symbol"));

    strcpy(symname, LD_SYMBOL_PREFIX);
    strcat(symname, fname);
    ef = (dl_funcptr)(intptr_t)dlsym(e->dlhandle, symname);
  }
#else
  ef = (dl_funcptr)(intptr_t)PL_dlsym(e->dlhandle, fname);
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

#define CLEANUP_FOREIGN_DONE
void
cleanupForeign(void)
{ DlEntry e, next;

  for(e = dl_head; e; e = next)
  { next = e->next;

    if ( e->dlhandle )
    { if ( !under_valgrind() )
	PL_dlclose(e->dlhandle);
    }

    freeHeap(e, sizeof(*e));
  }

  dl_plid = 0;
  dl_head = dl_tail = NULL;
}

#endif /*O_STATIC_EXTENSIONS*/
#endif /*HAVE_DLOPEN*/

#ifdef O_STATIC_EXTENSIONS
#if USE_DLOPEN_SELF
#include <dlfcn.h>
#include <errno.h>
#endif

static int
activate_static_extension(const char *ename)
{
#if USE_DLOPEN_SELF
  static void *handle = NULL;
  char fname[256];

  if ( !handle )
  { handle = dlopen(NULL, 0);
    if ( !handle )
      Sdprintf("Could not open dlopen() executable: %s\n",
	       strerror(errno));
  }

  strcpy(fname, "install_");
  strcat(fname, ename);

  if ( handle )
  { void *sym = dlsym(handle, fname);

    if ( sym )
    { typedef void (*install_function)(void);
      install_function f = sym;
      (*f)();
      return true;
    }

    return false;
  }
#else
  typedef void (*install_func)(void);
  typedef struct static_extension
  { const char *name;
    const install_func func;
  } static_extension;

#include <static_packages.h>

  for(const static_extension *ext = static_extensions;
      ext->name;
      ext++)
  { if ( strcmp(ename, ext->name) == 0 )
    { (*ext->func)();
      return true;
    }
  }

  return false;
#endif
}

static
PRED_IMPL("$activate_static_extension", 1, activate_static_extension,
	  PL_FA_TRANSPARENT)
{ char *ename;

  if ( PL_get_chars(A1, &ename, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) )
  { if ( activate_static_extension(ename) )
      return true;
    return PL_existence_error("foreign_extension", A1);
  } else
    return false;
}

#endif /*O_STATIC_EXTENSIONS*/

#ifndef CLEANUP_FOREIGN_DONE
/* No-op stub for pl-init.c to call. */
void
cleanupForeign(void)
{}
#endif


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(dlopen)
#if defined(HAVE_SHARED_OBJECTS) && !defined(O_STATIC_EXTENSIONS)
  PRED_DEF("open_shared_object",  2, open_shared_object, 0)
  PRED_DEF("open_shared_object",  3, open_shared_object, 0)
  PRED_DEF("close_shared_object", 1, close_shared_object, 0)
  PRED_DEF("call_shared_object_function", 2, call_shared_object_function,
	   PL_FA_TRANSPARENT)
#endif
#ifdef O_STATIC_EXTENSIONS
  PRED_DEF("$activate_static_extension", 1, activate_static_extension,
	   PL_FA_TRANSPARENT)
#endif
EndPredDefs
