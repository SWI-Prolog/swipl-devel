/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifdef _WIN64
#define _WIN32_WINNT 0x0501            /* get RtlCaptureContext() */
#endif

#include "pl-incl.h"
#include "os/pl-cstack.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The task of the library is to save   the  <N> most recent C stack traces
for later retrieval. I.e., although this library   can  be used to print
the stack in case of a crash, it is   intended  to _save_ the stack on a
critical event such as GC and retrieve it  later if it turns out that an
error occurs.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define SAVE_TRACES 10

		 /*******************************
		 *	      LIBUNWIND		*
		 *******************************/

#if !defined(BTRACE_DONE) && defined(HAVE_LIBUNWIND)
#define BTRACE_DONE 1
#define UNW_LOCAL_ONLY
#include <libunwind.h>

#define MAX_DEPTH 10

typedef struct
{ char name[32];				/* function called */
  unw_word_t offset;				/* offset in function */
} frame_info;

typedef struct
{ const char *name;				/* label of the backtrace */
  int depth;					/* # frames collectec */
  frame_info frame[MAX_DEPTH];			/* per-frame info */
} btrace_stack;

typedef struct btrace
{ btrace_stack dumps[SAVE_TRACES];		/* ring of buffers */
  int current;					/* next to fill */
} btrace;


void
btrace_destroy(struct btrace *bt)
{ free(bt);
}


static btrace *
get_trace_store(void)
{ GET_LD

  if ( !LD->btrace_store )
  { btrace *s = malloc(sizeof(*s));
    if ( s )
    { memset(s, 0, sizeof(*s));
      LD->btrace_store = s;
    }
  }

  return LD->btrace_store;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
next_btrace_id() produces the  id  for  the   next  backtrace  and  sets
bt->current to the subsequent id. Although bt is thread-local, it may be
called from a signal  handler  or   (Windows)  exception.  We cannot use
locking because the mutex functions are not   async  signal safe. So, we
use atomic instructions if possible. Otherwise, we ensure consistency of
the datastructures, but we may overwrite an older stack trace.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
next_btrace_id(btrace *bt)
{ int current;
#ifdef COMPARE_AND_SWAP
  int next;

  do
  { current = bt->current;
    next = current+1;
    if ( next == SAVE_TRACES )
      next = 0;
  } while ( !COMPARE_AND_SWAP(&bt->current, current, next) );
#else
  current = bt->current++ % SAVE_TRACES;

  if ( bt->current >= SAVE_TRACES )
    bt->current %= SAVE_TRACES;
#endif

  return current;
}


void
save_backtrace(const char *why)
{ btrace *bt = get_trace_store();

  if ( bt )
  { btrace_stack *s;
    unw_cursor_t cursor; unw_context_t uc;
    int depth;
    int current = next_btrace_id(bt);

    s = &bt->dumps[current];
    unw_getcontext(&uc);
    unw_init_local(&cursor, &uc);
    for(depth=0; unw_step(&cursor) > 0 && depth < MAX_DEPTH; depth++)
    { unw_get_proc_name(&cursor,
			s->frame[depth].name, sizeof(s->frame[depth].name),
			&s->frame[depth].offset);
    }
    s->name = why;
    s->depth = depth;
  }

}


static void
print_trace(btrace *bt, int me)
{ btrace_stack *s = &bt->dumps[me];

  if ( s && s->name )
  { int depth;

    Sdprintf("Stack trace labeled \"%s\":\n", s->name);
    for(depth=0; depth<s->depth; depth++)
    { Sdprintf("  [%d] %s+%p\n", depth,
	       s->frame[depth].name,
	       (void*)s->frame[depth].offset);
    }
  } else
  { Sdprintf("No stack trace\n");
  }
}


void
print_backtrace(int last)		/* 1..SAVE_TRACES */
{ btrace *bt = get_trace_store();

  if ( bt )
  { int me = bt->current-last;
    if ( me < 0 )
      me += SAVE_TRACES;

    print_trace(bt, me);
  } else
  { Sdprintf("No backtrace store?\n");
  }
}


void
print_backtrace_named(const char *why)
{ btrace *bt = get_trace_store();

  if ( bt )
  { int me = bt->current-1;

    for(;;)
    { if ( --me < 0 )
	me += SAVE_TRACES;
      if ( bt->dumps[me].name && strcmp(bt->dumps[me].name, why) == 0 )
      { print_trace(bt, me);
	return;
      }
      if ( me == bt->current-1 )
	break;
    }
  }

  Sdprintf("No backtrace named %s\n", why);
}

#endif /*HAVE_LIBUNWIND*/


		 /*******************************
		 *	       GLIBC		*
		 *******************************/

#if !defined(BTRACE_DONE) && defined(HAVE_EXECINFO_H) && !defined(DMALLOC)
#define BTRACE_DONE 1
#include <execinfo.h>
#include <string.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This implementation uses the libgcc unwinding capabilities.

Disabled of dmalloc is used because the  free of the memory allocated by
backtrace_symbols() is considered an error by dmalloc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct btrace
{ char	      **symbols[SAVE_TRACES];
  const char   *why[SAVE_TRACES];
  size_t	sizes[SAVE_TRACES];
  int		current;
} btrace;


void
btrace_destroy(struct btrace *bt)
{ int i;

  for(i=0; i<SAVE_TRACES; i++)
  { if ( bt->symbols[i] )
      free(bt->symbols[i]);
  }

  free(bt);
}


static btrace *
get_trace_store(void)
{ GET_LD

  if ( !LD->btrace_store )
  { btrace *s = malloc(sizeof(*s));
    if ( s )
    { memset(s, 0, sizeof(*s));
      LD->btrace_store = s;
    }
  }

  return LD->btrace_store;
}


/* Copy of same function above.  Relies on a different btrace structure.
   Ideally, this should be shared :-(
*/

static int
next_btrace_id(btrace *bt)
{ int current;
#ifdef COMPARE_AND_SWAP
  int next;

  do
  { current = bt->current;
    next = current+1;
    if ( next == SAVE_TRACES )
      next = 0;
  } while ( !COMPARE_AND_SWAP(&bt->current, current, next) );
#else
  current = bt->current++ % SAVE_TRACES;

  if ( bt->current >= SAVE_TRACES )
    bt->current %= SAVE_TRACES;
#endif

  return current;
}


void
save_backtrace(const char *why)
{ btrace *bt = get_trace_store();

  if ( bt )
  { void *array[100];
    size_t frames;
    int current = next_btrace_id(bt);

    frames = backtrace(array, sizeof(array)/sizeof(void *));
    bt->sizes[current] = frames;
    if ( bt->symbols[current] )
      free(bt->symbols[current]);
    bt->symbols[current] = backtrace_symbols(array, frames);
    bt->why[current] = why;
  }
}


static void
print_trace(btrace *bt, int me)
{ size_t i;

  if ( bt->why[me] )
  { Sdprintf("Stack trace labeled \"%s\":\n", bt->why[me]);

    for(i=0; i<bt->sizes[me]; i++)
      Sdprintf("  [%d] %s\n", i, bt->symbols[me][i]);
  } else
  { Sdprintf("No stack trace\n");
  }
}

void
print_backtrace(int last)		/* 1..SAVE_TRACES */
{ btrace *bt = get_trace_store();

  if ( bt )
  { int me = bt->current-last;
    if ( me < 0 )
      me += SAVE_TRACES;

    print_trace(bt, me);
  } else
  { Sdprintf("No backtrace store?\n");
  }
}


void
print_backtrace_named(const char *why)
{ btrace *bt = get_trace_store();

  if ( bt )
  { int me = bt->current-1;

    for(;;)
    { if ( --me < 0 )
	me += SAVE_TRACES;
      if ( bt->why[me] && strcmp(bt->why[me], why) == 0 )
      { print_trace(bt, me);
	return;
      }
      if ( me == bt->current-1 )
	break;
    }
  }

  Sdprintf("No backtrace named %s\n", why);
}


#endif /*HAVE_EXECINFO_H*/


		 /*******************************
		 *	  ADD AS HANDLER	*
		 *******************************/

#ifdef BTRACE_DONE

static void
crashHandler(int sig)
{ Sdprintf("\nSWI-Prolog [thread %d]: received fatal signal %d (%s)\n",
	   PL_thread_self(), sig, signal_name(sig));
  save_backtrace("crash");
  print_backtrace_named("crash");
  abort();
}

void
initBackTrace(void)
{
#ifdef SIGSEGV
  PL_signal(SIGSEGV, crashHandler);
#endif
#ifdef SIGILL
  PL_signal(SIGILL, crashHandler);
#endif
#ifdef SIGBUS
  PL_signal(SIGBUS, crashHandler);
#endif
#ifdef SIGFPE
  PL_signal(SIGFPE, crashHandler);
#endif
}

#endif


		 /*******************************
		 *   WINDOWS IMPLEMENTATION	    *
		 *******************************/


#if !defined(BTRACE_DONE) && defined(__WINDOWS__) && defined(HAVE_DBGHELP_H)
#include <windows.h>
#include <dbghelp.h>
#define MAX_SYMBOL_LEN 1024
#define MAX_DEPTH 10
#define BTRACE_DONE 1

#define MAX_FUNCTION_NAME_LENGTH 32
/* Note that the module name may include the full path in some versions
   of dbghelp. For me, 32 was not enough to see the module name in some
   cases.
*/
#define MAX_MODULE_NAME_LENGTH 64

#define LOCK()   PL_LOCK(L_CSTACK)
#define UNLOCK() PL_UNLOCK(L_CSTACK)

typedef struct
{ char name[MAX_FUNCTION_NAME_LENGTH];	/* function called */
  DWORD64 offset;			/* offset in function */
  char module[MAX_MODULE_NAME_LENGTH];	/* module of function */
  DWORD module_reason;                  /* reason for module being absent */
} frame_info;

typedef struct
{ const char *name;			/* label of the backtrace */
  int depth;				/* # frames collectec */
  frame_info frame[MAX_DEPTH];		/* per-frame info */
} btrace_stack;

typedef struct btrace
{ btrace_stack dumps[SAVE_TRACES];	/* ring of buffers */
  int current;				/* next to fill */
} btrace;

void
btrace_destroy(struct btrace *bt)
{ free(bt);
}


static btrace *
get_trace_store(void)
{ GET_LD

  if ( !LD->btrace_store )
  { btrace *s = malloc(sizeof(*s));
    if ( s )
    { memset(s, 0, sizeof(*s));
      LD->btrace_store = s;
    }
  }

  return LD->btrace_store;
}

/* Copy of same function above.  Relies on a different btrace structure.
   Ideally, this should be shared :-(
*/

static int
next_btrace_id(btrace *bt)
{ int current;
#ifdef COMPARE_AND_SWAP
  int next;

  do
  { current = bt->current;
    next = current+1;
    if ( next == SAVE_TRACES )
      next = 0;
  } while ( !COMPARE_AND_SWAP(&bt->current, current, next) );
#else
  current = bt->current++ % SAVE_TRACES;

  if ( bt->current >= SAVE_TRACES )
    bt->current %= SAVE_TRACES;
#endif

  return current;
}

int backtrace(btrace_stack* trace, PEXCEPTION_POINTERS pExceptionInfo)
{ STACKFRAME64 frame;
  CONTEXT context;
  int rc = 0;
  HANDLE hThread = GetCurrentThread();
  HANDLE hProcess = GetCurrentProcess();
  char symbolScratch[sizeof(SYMBOL_INFO) + MAX_SYMBOL_LEN];
  SYMBOL_INFO* symbol = (SYMBOL_INFO*)&symbolScratch;
  IMAGEHLP_MODULE64 moduleInfo;
  DWORD64 offset;
  DWORD imageType;
  int skip = 0;
  int depth = 0;

  if (pExceptionInfo == NULL)
  { memset(&context, 0, sizeof(CONTEXT));
    // If we dont have the context, then we can get the current one from the CPU
    // However, we should skip the first N frames, since these relate to the
    // exception handler itself
    // Obviously N is a magic number - it might differ if this code is modified!
#if _WIN32_WINNT > 0x0500
    // Good, just use RtlCaptureContext
    skip = 2;
    RtlCaptureContext(&context);
#else
    // For earlier than WinXPsp1 we have to do some weird stuff
    // For win32, we can use inline assembly to get eip, esp and ebp but
    // the MSVC2005 compiler refuses to emit inline assembly for AMD64
    // Luckily, the oldest AMD64 build of Windows is XP, so we should be able to
    // use RtlCaptureContext!
#ifdef WIN64
#error You appear to have a 64 bit build of a pre-XP version of Windows?!
#else
    skip = 2;
    __asm
    { call steal_eip
      steal_eip:
      pop eax
      mov context.Eip, eax
      mov eax, ebp
      mov context.Ebp, eax
      mov eax, esp
      mov context.Esp, eax
    }
#endif

#endif
  } else
  { context = *(pExceptionInfo->ContextRecord);
  }

  ZeroMemory(&frame, sizeof( STACKFRAME64));
  memset(&moduleInfo,0,sizeof(IMAGEHLP_MODULE64));
  moduleInfo.SizeOfStruct = sizeof(IMAGEHLP_MODULE64);
  rc = SymInitialize(hProcess, NULL, TRUE);
  if (rc == 0)
    return 0;

#ifdef _WIN64
   imageType = IMAGE_FILE_MACHINE_AMD64;
   frame.AddrPC.Offset = context.Rip;
   frame.AddrFrame.Offset = context.Rsp;
   frame.AddrStack.Offset = context.Rsp;
#else
   imageType = IMAGE_FILE_MACHINE_I386;
   frame.AddrPC.Offset = context.Eip;
   frame.AddrFrame.Offset = context.Ebp;
   frame.AddrStack.Offset = context.Esp;
#endif
   frame.AddrPC.Mode = AddrModeFlat;
   frame.AddrFrame.Mode = AddrModeFlat;
   frame.AddrStack.Mode = AddrModeFlat;

   while(depth < MAX_DEPTH &&
	 (rc =  StackWalk64(imageType,
			    hProcess,
			    hThread,
			    &frame,
			    &context,
			    NULL,
			    SymFunctionTableAccess64,
			    SymGetModuleBase64,
			    NULL)) != 0)
   { int hasModule = 0;
     BOOL hasSymbol = FALSE;

     if (skip > 0)
     { skip--;
       continue;
     }

     memset(symbol, 0, sizeof(SYMBOL_INFO) + MAX_SYMBOL_LEN);
     symbol->SizeOfStruct = sizeof(SYMBOL_INFO);
     symbol->MaxNameLen = MAX_SYMBOL_LEN;
     trace->frame[depth].offset = frame.AddrPC.Offset;
     hasModule = SymGetModuleInfo64(hProcess, frame.AddrPC.Offset, &moduleInfo);

     if (hasModule == 0)
     {
       // Note that this CAN be caused by a very out of date dbghelp.dll,
       // like the one that ships with Windows XP
       // Dropping version 6.x into the bin directory can magically
       // make this work. At least we will have the offset
       trace->frame[depth].name[0] = '\0';
       trace->frame[depth].module[0] = '\0';
       trace->frame[depth].module_reason = GetLastError();
     } else
     { hasSymbol = SymFromAddr(hProcess, frame.AddrPC.Offset, &offset, symbol);
       strncpy(trace->frame[depth].module,
	       moduleInfo.ImageName,
	       MAX_MODULE_NAME_LENGTH);
       trace->frame[depth].module[MAX_MODULE_NAME_LENGTH-1] = '\0';
       trace->frame[depth].module_reason = 0;
       if (hasSymbol)
       { strncpy(trace->frame[depth].name,
		 symbol->Name,
		 MAX_FUNCTION_NAME_LENGTH);
         trace->frame[depth].name[MAX_FUNCTION_NAME_LENGTH-1] = '\0';
       } else
       { trace->frame[depth].name[0] = '\0';
       }
     }
     depth++;
   }
   return depth;
}

void
win_save_backtrace(const char *why, PEXCEPTION_POINTERS pExceptionInfo)
{ btrace *bt = get_trace_store();
  if ( bt )
  { int current = next_btrace_id(bt);
    btrace_stack *s = &bt->dumps[current];
    LOCK();
    s->depth = backtrace(s, pExceptionInfo);
    UNLOCK();
    s->name = why;
  }
}


void save_backtrace(const char *why)
{ win_save_backtrace(why, NULL);
}


static void
print_trace(btrace *bt, int me)
{ btrace_stack *s = &bt->dumps[me];
  if ( s->name )
  { int depth;

    Sdprintf("Stack trace labeled \"%s\":\n", s->name);
    for(depth=0; depth<s->depth; depth++)
    { Sdprintf("  [%d] <%s>:%s+%p\n", depth,
               (s->frame[depth].module[0] == 0) ? "unknown module"
						: s->frame[depth].module,
	       s->frame[depth].name,
	       (void*)s->frame[depth].offset);
    }
  } else
  { Sdprintf("No stack trace\n");
  }
}



void
print_backtrace(int last)		/* 1..SAVE_TRACES */
{ btrace *bt = get_trace_store();

  if ( bt )
  { int me = bt->current-last;
    if ( me < 0 )
      me += SAVE_TRACES;

    print_trace(bt, me);
  } else
  { Sdprintf("No backtrace store?\n");
  }
}


void
print_backtrace_named(const char *why)
{ btrace *bt = get_trace_store();

  if ( bt )
  { int me = bt->current-1;

    for(;;)
    { if ( --me < 0 )
	me += SAVE_TRACES;
      if ( bt->dumps[me].name && strcmp(bt->dumps[me].name, why) == 0 )
      { print_trace(bt, me);
	return;
      }
      if ( me == bt->current-1 )
	break;
    }
  }

  Sdprintf("No backtrace named %s\n", why);
}

static LONG WINAPI crashHandler(PEXCEPTION_POINTERS pExceptionInfo)
{ win_save_backtrace("crash", pExceptionInfo);
  print_backtrace_named("crash");
  abort();

  return EXCEPTION_CONTINUE_SEARCH; /* ? */
}

void
initBackTrace(void)
{ SetUnhandledExceptionFilter(crashHandler);
}

#endif /*__WINDOWS__*/


	         /*******************************
		 *   FALLBACK IMPLEMENTATION	*
		 *******************************/


#ifndef BTRACE_DONE

void
save_backtrace(const char *why)
{
}

void
btrace_destroy(struct btrace *bt)
{
}

void
print_backtrace(int last)
{ Sdprintf("%s:%d C-stack dumps are not supported on this platform\n",
	   __FILE__, __LINE__);
}

void
print_backtrace_named(const char *why)
{ Sdprintf("%s:%d C-stack dumps are not supported on this platform\n",
	   __FILE__, __LINE__);
}

void
initBackTrace(void)
{
}

#endif /*BTRACE_DONE*/
