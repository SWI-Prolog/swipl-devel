/*  $Id$

    Part of SWI-Prolog.

    Purpose: Windows DDL interface
*/

#if defined(__WINDOWS__) || defined(__WIN32__)

#include "windows.h"
#undef TRANSPARENT

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
findDllHandle(word handle)
{ if ( isInteger(handle) )
  { int i = valNum(handle);

    if ( i >= 0 && i < MAX_DLL_INSTANCES && dll[i] )
      return i;
  }

  return -1;
}


static word
dll_warning(char *fmt)
{ return warning("%s failed: %s", fmt, WinError());
}



word
pl_open_dll(Word name, Word handle)
{ HINSTANCE h;

  if ( !isAtom(*name) )
    return warning("open_dll/2: illegal name");

  if ( (h = LoadLibrary(stringAtom((Atom)*name))) )
  { int plhandle = allocDllHandle(h);

    return unifyAtomic(handle, consNum(plhandle));
  }

  return dll_warning("open_dll/2");
}


word
pl_close_dll(Word handle)
{ int i;

  if ( (i = findDllHandle(*handle))  < 0 )
    return warning("close_dll/1: illegal handle");
  
  FreeLibrary(dll[i]);
  dll[i] = NULL;

  succeed;
}


word
pl_call_dll_function(Word handle, Word funcname)
{ int i;
  FARPROC proc;

  if ( (i = findDllHandle(*handle)) < 0 )
    return warning("call_dll_function/2: illegal handle");
  if ( !isAtom(*funcname) )
    return warning("call_dll_function/2: illegal function name");
  
  if ( !(proc = GetProcAddress(dll[i], stringAtom((Atom)*funcname))) )
/*    return dll_warning("call_dll_function/2"); */
    fail;

  (*proc)();

  succeed;
}

#endif /*O_DLL*/
#endif /*defined(__WINDOWS__) || defined(__WIN32__)*/
