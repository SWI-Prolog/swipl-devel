/*  $Id$

    Part of SWI-Prolog.

    Purpose: Windows DDL interface
*/

#if defined(__WINDOWS__) || defined(__WIN32__)

#include "windows.h"
#include "pl-incl.h"
#include <stdio.h>

#ifdef O_DLL

extern char *WinError(void);

#define MAX_DLL_INSTANCES	32	/* handle allocation */

static HINSTANCE dll[MAX_DLL_INSTANCES];

static int
allocDllHandle(HINSTANCE handle)
{ int i;

  for(i=0; i<MAX_DLL_INSTANCES; i++)
  { if ( !dll[i] )
    { dll[i] = handle;
      return i;
    }
  }

  warning("DLL manager: out of handles");
  return -1;
}


static int
get_dll_handle(term_t handle, int *hdl)
{ int i;

  if ( PL_get_integer(handle, &i) &&
       i >= 0 && i < MAX_DLL_INSTANCES && dll[i] )
  { *hdl = i;

    succeed;
  }

  fail;
}


static word
dll_warning(char *fmt)
{ return warning("%s failed: %s", fmt, WinError());
}



word
pl_open_dll(term_t name, term_t handle)
{ HINSTANCE h;
  char *s;

  if ( !PL_get_chars(name, &s, CVT_ALL) )
    return warning("open_dll/2: illegal name");

  if ( (h = LoadLibrary(s)) )
  { int plhandle = allocDllHandle(h);

    return PL_unify_integer(handle, plhandle);
  }

  return dll_warning("open_dll/2");
}


word
pl_close_dll(term_t handle)
{ int i;

  if ( !get_dll_handle(handle, &i) )
    return warning("close_dll/1: illegal handle");
  
  FreeLibrary(dll[i]);
  dll[i] = NULL;

  succeed;
}


word
pl_call_dll_function(term_t handle, term_t funcname)
{ int i;
  FARPROC proc;
  char *fname;

  if ( !get_dll_handle(handle, &i) )
    return warning("call_dll_function/2: illegal handle");
  if ( !PL_get_chars(funcname, &fname, CVT_ALL) )
    return warning("call_dll_function/2: illegal function name");
  
  if ( !(proc = GetProcAddress(dll[i], fname)) )
    fail;

  (*proc)();

  succeed;
}

#endif /*O_DLL*/
#endif /*defined(__WINDOWS__) || defined(__WIN32__)*/
