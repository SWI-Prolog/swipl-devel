/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2011-2025, University of Amsterdam
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

#define _GNU_SOURCE
#include "../pl-incl.h"
#include "pl-cstack.h"
#include "../pl-setup.h"
#include <time.h>
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

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

#if !defined(BTRACE_DONE) && defined(HAVE_LIBUNWIND) && !defined(HAVE_DLADDR)
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
  int shared;					/* shared on LD */
} btrace;


void
btrace_destroy(struct btrace *bt)
{ free(bt);
}


static btrace *
get_trace_store(int create)
{ GET_LD

  if ( LD )
  { if ( !LD->btrace_store )
    { btrace *s = malloc(sizeof(*s));
      if ( s )
      { memset(s, 0, sizeof(*s));
	s->shared = true;
	LD->btrace_store = s;
      }
    }

    return LD->btrace_store;
  } else if ( create )
  { btrace *s = malloc(sizeof(*s));

    if ( s )
      memset(s, 0, sizeof(*s));

    return s;
  } else
    return NULL;
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


btrace *
save_backtrace(const char *why)
{ btrace *bt = get_trace_store(true);

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

  return bt;
}


static void
print_trace(btrace *bt, int me)
{ btrace_stack *s = &bt->dumps[me];

  if ( s && s->name )
  { int depth;

    Sdprintf("C-stack trace labeled \"%s\":\n", s->name);
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
{ btrace *bt = get_trace_store(false);

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
bstore_print_backtrace_named(btrace *bt, const char *why)
{ if ( bt )
  { int me = bt->current-1;

    for(;;)
    { if ( me < 0 )
	me += SAVE_TRACES;
      if ( bt->dumps[me].name && strcmp(bt->dumps[me].name, why) == 0 )
      { print_trace(bt, me);
	return;
      }
      if ( --me == bt->current-1 )
	break;
    }
  }
}

#endif /*HAVE_LIBUNWIND*/


		 /*******************************
		 *	       GLIBC		*
		 *******************************/

#if !defined(BTRACE_DONE) && defined(HAVE_EXECINFO_H) && defined(HAVE_BACKTRACE)
#define BTRACE_DONE 1
#include <execinfo.h>
#include <string.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This implementation uses the libgcc unwinding capabilities. If possible,
addr2line(1) is used to obtain information at the line level.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct btrace
{ char	      **retaddr[SAVE_TRACES];
  const char   *why[SAVE_TRACES];
  size_t	sizes[SAVE_TRACES];
  int		current;
  int		shared;
} btrace;


void
btrace_destroy(struct btrace *bt)
{ int i;

  for(i=0; i<SAVE_TRACES; i++)
  { if ( bt->retaddr[i] )
      free(bt->retaddr[i]);
  }

  free(bt);
}


static btrace *
get_trace_store(int create)
{ GET_LD

  if ( HAS_LD )
  { if ( !LD->btrace_store )
    { btrace *s = malloc(sizeof(*s));
      if ( s )
      { memset(s, 0, sizeof(*s));
	s->shared = true;
	LD->btrace_store = s;
      }
    }

    return LD->btrace_store;
  } else if ( create )
  { btrace *s = malloc(sizeof(*s));

    if ( s )
      memset(s, 0, sizeof(*s));

    return s;
  }

  return NULL;
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


btrace *
save_backtrace(const char *why)
{ btrace *bt = get_trace_store(true);

  if ( bt )
  { void *array[100];
    size_t frames;
    int current = next_btrace_id(bt);

    frames = backtrace(array, sizeof(array)/sizeof(void *));
    bt->sizes[current] = frames;
    if ( bt->retaddr[current] )
      free(bt->retaddr[current]);
    if ( (bt->retaddr[current] = malloc(sizeof(void*)*frames)) )
      memcpy(bt->retaddr[current], array, sizeof(void*)*frames);
    bt->why[current] = why;
  }

  return bt;
}

static void
print_trace(btrace *bt, int me)
{ size_t i;

  if ( bt->why[me] )
  { Sdprintf("C-stack trace labeled \"%s\":\n", bt->why[me]);

    for(i=0; i<bt->sizes[me]; i++)
    { char buf[512];
      void *addr = bt->retaddr[me][i];

      if ( addr2line(addr, buf, sizeof(buf)) )
	Sdprintf("  [%zd] %s [%p]\n", i, buf, addr);
      else
	Sdprintf("  [%zd] ??? [%p]\n", i, addr);
    }
  } else
  { Sdprintf("No stack trace\n");
  }
}

void
print_backtrace(int last)		/* 1..SAVE_TRACES */
{ btrace *bt = get_trace_store(false);

  if ( bt )
  { int me = bt->current-last;
    if ( me < 0 )
      me += SAVE_TRACES;

    print_trace(bt, me);
  } else
  { Sdprintf("No backtrace store?\n");
  }
}


static void
bstore_print_backtrace_named(btrace *bt, const char *why)
{ if ( bt )
  { int me = bt->current-1;

    for(;;)
    { if ( me < 0 )
	me += SAVE_TRACES;
      if ( bt->why[me] && strcmp(bt->why[me], why) == 0 )
      { print_trace(bt, me);
	return;
      }
      if ( --me == bt->current-1 )
      { Sdprintf("No backtrace named %s\n", why);
	break;
      }
    }
  }
}


#endif /*HAVE_EXECINFO_H*/


		 /*******************************
		 *	  ADD AS HANDLER	*
		 *******************************/

#ifdef BTRACE_DONE

void
initBackTrace(void)
{
#if defined(SIGSEGV) && !defined(O_ALTSIGSTACK)
  PL_signal(SIGSEGV, sigCrashHandler);
#endif
#ifdef SIGILL
  PL_signal(SIGILL, sigCrashHandler);
#endif
#if defined(SIGBUS) && SIGBUS != SIGSEGV
  PL_signal(SIGBUS, sigCrashHandler);
#endif
#ifdef SIGFPE
  PL_signal(SIGFPE, sigCrashHandler);
#endif
#ifdef SIGSYS
  PL_signal(SIGSYS, sigCrashHandler);
#endif
}

#endif


		 /*******************************
		 *   WINDOWS IMPLEMENTATION	    *
		 *******************************/


#if !defined(BTRACE_DONE) && defined(__WINDOWS__) && defined(HAVE_DBGHELP_H)
#include <windows.h>
#include <dbghelp.h>
#include <libgen.h>
#define MAX_SYMBOL_LEN 1024
#define MAX_DEPTH 10
#define BTRACE_DONE 1

#define MAX_FUNCTION_NAME_LENGTH 32
/* Note that the module name may include the full path in some versions
   of dbghelp. For me, 32 was not enough to see the module name in some
   cases.
*/
#define MAX_MODULE_NAME_LENGTH 64

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
  int shared;
} btrace;

void
btrace_destroy(struct btrace *bt)
{ free(bt);
}


static btrace *
get_trace_store(int create)
{ GET_LD

  if ( !LD->btrace_store )
  { btrace *s = malloc(sizeof(*s));
    if ( s )
    { memset(s, 0, sizeof(*s));
      s->shared = true;
      LD->btrace_store = s;
    }
  } else if ( create )
  { btrace *s = malloc(sizeof(*s));

    if ( s )
      memset(s, 0, sizeof(*s));

    return s;
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

#ifdef HAVE_LIBDWARF
#include "windows/dwarf-debug.c"
#endif

int backtrace(btrace_stack* trace, PEXCEPTION_POINTERS pExceptionInfo)
{ STACKFRAME64 frame;
  CONTEXT context;
  int rc = 0;
  HANDLE hThread = GetCurrentThread();
  HANDLE hProcess = GetCurrentProcess();
  char symbolScratch[sizeof(SYMBOL_INFO) + MAX_SYMBOL_LEN];
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
  rc = SymInitialize(hProcess, NULL, true);
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
     BOOL hasSymbol = false;

     if (skip > 0)
     { skip--;
       continue;
     }

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
     { strncpy(trace->frame[depth].module,
	       basename(moduleInfo.ImageName),
	       MAX_MODULE_NAME_LENGTH);
       trace->frame[depth].module[MAX_MODULE_NAME_LENGTH-1] = '\0';
       trace->frame[depth].module_reason = 0;
#ifdef HAVE_LIBDWARF
       char *dwarf_symbol = symbolScratch;
       memset(dwarf_symbol, 0, MAX_SYMBOL_LEN);
       hasSymbol = dwarf_sym_from_addr(&moduleInfo, frame.AddrPC.Offset, &dwarf_symbol);
       if (hasSymbol)
       { strncpy(trace->frame[depth].name,
		 dwarf_symbol,
		 MAX_FUNCTION_NAME_LENGTH);
	 trace->frame[depth].name[MAX_FUNCTION_NAME_LENGTH-1] = '\0';
       } else
#endif
       { SYMBOL_INFO* symbol = (SYMBOL_INFO*)&symbolScratch;

	 memset(symbol, 0, sizeof(SYMBOL_INFO) + MAX_SYMBOL_LEN);
	 symbol->SizeOfStruct = sizeof(SYMBOL_INFO);
	 symbol->MaxNameLen = MAX_SYMBOL_LEN;
	 hasSymbol = SymFromAddr(hProcess, frame.AddrPC.Offset, &offset, symbol);
	 if (hasSymbol)
	 { strncpy(trace->frame[depth].name,
		   symbol->Name,
		   MAX_FUNCTION_NAME_LENGTH);
	   trace->frame[depth].name[MAX_FUNCTION_NAME_LENGTH-1] = '\0';
	 } else
	 { trace->frame[depth].name[0] = '\0';
	 }
       }
     }
     depth++;
   }
   SymCleanup(hProcess);
   return depth;
}

btrace *
win_save_backtrace(const char *why, PEXCEPTION_POINTERS pExceptionInfo)
{ btrace *bt = get_trace_store(true);
  if ( bt )
  { int current = next_btrace_id(bt);
    btrace_stack *s = &bt->dumps[current];
    PL_LOCK(L_CSTACK);
    s->depth = backtrace(s, pExceptionInfo);
    PL_UNLOCK(L_CSTACK);
    s->name = why;
  }

  return bt;
}


btrace *
save_backtrace(const char *why)
{ return win_save_backtrace(why, NULL);
}


static void
print_trace(btrace *bt, int me)
{ btrace_stack *s = &bt->dumps[me];

  if ( s->name )
  { int depth;
    HANDLE hProcess = GetCurrentProcess();

    SymInitialize(hProcess, NULL, true);

    Sdprintf("Stack trace labeled \"%s\":\n", s->name);
    for(depth=0; depth<s->depth; depth++)
    { if (s->frame[depth].module[0])
      {
#ifdef HAVE_LIBDWARF
	IMAGEHLP_MODULE64 moduleInfo;
	char dwarf_srclinebuf[PATH_MAX];
	char *dwarf_srcline = dwarf_srclinebuf;

	memset(&moduleInfo,0,sizeof(IMAGEHLP_MODULE64));
	memset(dwarf_srcline, 0, PATH_MAX);
	moduleInfo.SizeOfStruct = sizeof(IMAGEHLP_MODULE64);
	if ( SymGetModuleInfo64(hProcess, s->frame[depth].offset, &moduleInfo) &&
	     dwarf_addr2line(&moduleInfo, s->frame[depth].offset, &dwarf_srcline) )
	{ Sdprintf("  [%d] <%s>:%s() at %s [%p]\n", depth,
		   s->frame[depth].module,
		   s->frame[depth].name,
		   dwarf_srcline,
		   (void*)s->frame[depth].offset);
	} else
#endif
	{ Sdprintf("  [%d] <%s>:%s() [%p]\n", depth,
		   s->frame[depth].module,
		   s->frame[depth].name,
		   (void*)s->frame[depth].offset);
	}
      } else
      { Sdprintf("  [%d] <unknown module>:%s [%p]\n", depth,
		 s->frame[depth].name,
		 (void*)s->frame[depth].offset);
      }
    }
    SymCleanup(hProcess);
  } else
  { Sdprintf("No stack trace\n");
  }
}



void
print_backtrace(int last)		/* 1..SAVE_TRACES */
{ btrace *bt = get_trace_store(false);

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
bstore_print_backtrace_named(btrace *bt, const char *why)
{ if ( bt )
  { int me = bt->current-1;

    for(;;)
    { if ( me < 0 )
	me += SAVE_TRACES;
      if ( bt->dumps[me].name && strcmp(bt->dumps[me].name, why) == 0 )
      { print_trace(bt, me);
	return;
      }
      if ( --me == bt->current-1 )
	break;
    }
  }
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
		 *	     SHARED		*
		 *******************************/

#ifndef HAVE_CTIME_R
#define ctime_r(timep, buf) strcpy(buf, ctime(timep))
#endif

#ifdef BTRACE_DONE

void
print_backtrace_named(const char *why)
{ bstore_print_backtrace_named(get_trace_store(false), why);
}


void
print_c_backtrace(const char *why)
{ btrace *bt = save_backtrace(why);

  bstore_print_backtrace_named(bt, why);
  if ( bt && !bt->shared )
    btrace_destroy(bt);
}

void
sigCrashHandler(int sig)
{
#ifdef O_PLMT
  GET_LD
#endif

  signal(sig,     SIG_DFL);
#ifdef SIGALRM
  signal(SIGALRM, SIG_DFL);
#endif
#ifdef SIGABRT
  signal(SIGABRT, SIG_DFL);
#endif
#ifdef SIGSEGV
  signal(SIGSEGV, SIG_DFL);
#endif
#ifdef HAVE_ALARM
  alarm(10);				/* try to avoid deadlocks */
#endif

  Sdprintf("\nERROR: Received fatal signal %d (%s)\n",
	   sig, signal_name(sig));
  save_backtrace("crash");
#ifdef O_PLMT
  LD->thread.info->c_stack_low = true; /* Actually, we are on an alt-stack */
#endif
  printCrashContext("crash");

  Sdprintf("Running on_halt hooks with status %d\n", 128+sig);
  run_on_halt(&GD->os.exit_hooks, 128+sig);

#ifdef HAVE_KILL
{ int pid;
# ifdef O_PLMT
#  ifdef HAVE_GETTID_SYSCALL
#   ifdef HAVE_SYS_SYSCALL_H
#    include <sys/syscall.h>
#   endif
  pid = syscall(__NR_gettid);
#  elif defined(HAVE_GETTID_MACRO)
  pid = gettid();
#  else
  pid = getpid();
#  endif
# else /*O_PLMT*/
  pid = getpid();
# endif
  Sdprintf("Killing %d with default signal handlers\n", pid);
  kill(pid, sig);
}
#else /*HAVE_KILL*/
  Sdprintf("Aborting\n");
  abort();
#endif
}

#endif /*BTRACE_DONE*/

		 /*******************************
		 *	PROLOG CONNECTION	*
		 *******************************/

#if defined(O_DEBUG) && defined(BTRACE_DONE)
#define BTRACE_PREDS 1

static
PRED_IMPL("c_backtrace_clear", 0, c_backtrace_clear, 0)
{ PRED_LD

  if ( LD->btrace_store )
  { btrace_destroy(LD->btrace_store);
    LD->btrace_store = NULL;
  }

  return true;
}

static
PRED_IMPL("c_backtrace_print", 1, c_backtrace_print, 0)
{ char *s;

  if ( PL_get_chars(A1, &s, CVT_ATOM|CVT_STRING|CVT_EXCEPTION) )
  { print_backtrace_named(s);
    return true;
  }

  return false;
}

#endif

BeginPredDefs(cbtrace)
#ifdef BTRACE_PREDS
  PRED_DEF("c_backtrace_clear", 0, c_backtrace_clear, 0)
  PRED_DEF("c_backtrace_print", 1, c_backtrace_print, 0)
#endif
EndPredDefs

		 /*******************************
		 *   FALLBACK IMPLEMENTATION	*
		 *******************************/


#ifndef BTRACE_DONE

struct btrace *
save_backtrace(const char *why)
{ return NULL;
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
print_c_backtrace(const char *why)
{
}

void
initBackTrace(void)
{
}

#if O_SIGNALS && defined(HAVE_SIGNAL)
void
sigCrashHandler(int sig)
{ int tid;
  atom_t alias;
  const pl_wchar_t *name = L"";
  time_t now = time(NULL);
  char tbuf[48];

  signal(sig, SIG_DFL);
  tid = PL_thread_self();
  ctime_r(&now, tbuf);
  tbuf[24] = '\0';

  if ( PL_get_thread_alias(tid, &alias) )
    name = PL_atom_wchars(alias, NULL);

  Sdprintf("\nSWI-Prolog [thread %d (%Ws) at %s]: "
	   "received fatal signal %d (%s)\n",
	   PL_thread_self(), name, tbuf, sig, signal_name(sig));
  run_on_halt(&GD->os.exit_hooks, 4);

#if defined(HAVE_KILL) && defined(HAVE_GETPID)
  kill(getpid(), sig);
#else
  abort();
#endif
}

#else

void
sigCrashHandler(int sig)
{ fatalError("Something went wrong");
}

#endif /*O_SIGNALS && HAVE_SIGNAL*/

#endif /*BTRACE_DONE*/


		 /*******************************
		 *   STACK LOCATION AND SIZE	*
		 *******************************/

#ifdef O_PLMT
static pthread_once_t c_stack_key_created = PTHREAD_ONCE_INIT;
static pthread_key_t c_stack_key = 0;

static void
c_stack_create_key(void)
{ pthread_key_create(&c_stack_key, free);
}

static c_stack_info *
thread_c_stack_info(c_stack_info *cinfo)
{ pthread_once(&c_stack_key_created, c_stack_create_key);

  if ( cinfo )
  { pthread_setspecific(c_stack_key, cinfo);
  } else if ( (cinfo=pthread_getspecific(c_stack_key)) )
  { return cinfo;
  } else
  { if ( !(cinfo = malloc(sizeof(*cinfo))) )
      outOfCore();
    memset(cinfo, 0, sizeof(*cinfo));
    pthread_setspecific(c_stack_key, cinfo);
  }

  return cinfo;
}

static size_t
round_pages(size_t n)
{ size_t psize;

#if defined(HAVE_SYSCONF) && defined(_SC_PAGESIZE)
  if ( (psize = sysconf(_SC_PAGESIZE)) == (size_t)-1 )
    psize = 8192;
#else
  psize = 8192;
#endif

  return ROUND(n, psize);
}

static void
c_stack_base(c_stack_info *cinfo)
{ if ( !cinfo->base && cinfo->size )
  { size_t top = round_pages((size_t)&cinfo);
    cinfo->base = (void*)(top - cinfo->size);
  }
}

#endif /*O_PLMT*/

c_stack_info *
CStackSize(DECL_LD)
{
#ifdef O_PLMT
  PL_thread_info_t *info = LD->thread.info;
  c_stack_info *cinfo;

  if ( !(cinfo=info->c_stack) )
    cinfo = info->c_stack = thread_c_stack_info(NULL);

  if ( !cinfo->initialised )
  { info->c_stack = thread_c_stack_info(info->c_stack);

    if ( cinfo->size && !cinfo->base )
    { c_stack_base(cinfo);
    } else
    { if ( info->pl_tid == 1 )
      {
#ifdef HAVE_GETRLIMIT
	struct rlimit rlim;

	if ( getrlimit(RLIMIT_STACK, &rlim) == 0 &&
	     rlim.rlim_cur != RLIM_INFINITY && rlim.rlim_cur )
	{ cinfo->size = (size_t)rlim.rlim_cur;
	  c_stack_base(cinfo);
	}
#endif
      } else
      {
#ifdef HAVE_PTHREAD_GETATTR_NP
	pthread_attr_t attr;

	if ( pthread_getattr_np(info->tid, &attr) == 0 )
	{ pthread_attr_getstack(&attr, &cinfo->base, &cinfo->size);
	  pthread_attr_destroy(&attr);
	}
#endif
      }
    }
    cinfo->initialised = true;
    DEBUG(MSG_CSTACK, Sdprintf("[%d]: C-stack %p[%zd]\n",
			       info->pl_tid, cinfo->base, cinfo->size));

  }

  return cinfo;
#else /*O_PLMT*/
  return NULL;
#endif
}

		/*******************************
		*          ADDR2LINE           *
		*******************************/

#if !defined(__WINDOWS__) && !defined(__EMSCRIPTEN__)
#include <stdio.h>
#include <dlfcn.h>
#define HAVE_ADDR2LINE2 1
#define MAXCMD 1024

/* ADDR2LINE_CMD is used by addr2line_popen() and looks up a single
 * address.  ADDR2LINE_EXEC() runs the same program for the server (see
 * below), which reads addresses from stdin.  Keep the two in sync.
 */

#ifdef __APPLE__
/* Emits e.g. "prologToplevel (in libswipl.8.5.20.dylib) (pl-pro.c:560)" */
#define ADDR2LINE_CMD "atos -o \"%s\" --fullPath %p"
#define ADDR2LINE_EXEC(fname) \
	execlp("atos", "atos", "-o", fname, "--fullPath", (char*)NULL)
#else
/* Emits two lines: "function\nfile:line"  */
#define ADDR2LINE_CMD "addr2line -fe \"%s\" %p"
#define ADDR2LINE_EXEC(fname) \
	execlp("addr2line", "addr2line", "-fe", fname, (char*)NULL)
#endif

/* Append s to the description under construction.  Returns false if it
 * does not fit, which ends the scan and leaves what we have so far.
 */

static bool
add_str(char **op, const char *ebuf, const char *s)
{ size_t len = strlen(s);
  char *o = *op;

  if ( o+len >= ebuf )
    return false;

  memcpy(o, s, len);
  *op = o+len;

  return true;
}

/* The description is "func() at File:Line".  The "()" is added as soon as
 * the function name ends, but the " at " only once we know a location
 * follows: without source line information atos(1) emits
 * "func (in lib.dylib) + 0" and addr2line(1) emits "??:0", and a dangling
 * "func() at " is worse than a plain "func()".
 */

/* Read one line into buf, discarding what does not fit.  Always
 * consumes the whole line such that a persistent addr2line or atos
 * process stays in sync.
 */

static bool
read_line(FILE *fd, char *buf, size_t size)
{ size_t len = 0;
  int c;

  while( (c=fgetc(fd)) != EOF && c != '\n' )
  { if ( len+1 < size )
      buf[len++] = (char)c;
  }
  buf[len] = '\0';

  return c == '\n' || len > 0;
}

#ifdef __APPLE__

/* Copy from s to buf until end or one of stop, returning the first
 * character not copied.
 */

static const char *
copy_field(const char *s, const char *stop, char **op, const char *ebuf)
{ char *o = *op;

  for(; *s && !strchr(stop, *s); s++)
  { if ( o >= ebuf )
      break;
    *o++ = *s;
  }
  *op = o;

  return s;
}

/* atos(1) emits one line per address, e.g. "func (in lib.dylib)
 * (/path/file.c:42)".  The source location, if any, follows the second
 * '('.  Without it we get "func (in lib.dylib) + 32" and if even the
 * function is unknown just the address.
 */

static bool
read_addr2line(FILE *fd, char *buf, size_t size)
{ char line[1024];
  char *ebuf = &buf[size-1];
  char *o = buf;
  const char *s;

  if ( !read_line(fd, line, sizeof(line)) )
    return false;

  s = copy_field(line, " ", &o, ebuf);	/* copy the function */
  if ( *s == ' ' && add_str(&o, ebuf, "()") )
  { const char *loc;

    if ( (loc=strchr(s, '(')) && (loc=strchr(loc+1, '(')) &&
	 add_str(&o, ebuf, " at ") )
      copy_field(loc+1, ")", &o, ebuf);
  }

  *o = '\0';
  return o > buf;
}

#else /*__APPLE__*/

static bool
read_addr2line(FILE *fd, char *buf, size_t size)
{ char func[256];
  char loc[1024];
  char *ebuf = &buf[size-1];
  char *o = buf;

  if ( !read_line(fd, func, sizeof(func)) ||
       !read_line(fd, loc, sizeof(loc)) )
    return false;

  if ( add_str(&o, ebuf, func) &&
       add_str(&o, ebuf, "()") &&
       loc[0] != '?' &&		/* "??:0": no source location */
       add_str(&o, ebuf, " at ") )
    add_str(&o, ebuf, loc);

  *o = '\0';
  return o > buf;
}

#endif /*__APPLE__*/

static bool
addr2line_popen(const char *fname, uintptr_t offset, char *buf, size_t size)
{ char cmd[MAXCMD];
  int len = snprintf(cmd, sizeof(cmd), ADDR2LINE_CMD, fname, (void*)offset);

  if ( len > 0 && (size_t)len < sizeof(cmd) )
  { FILE *fd;

    if ( (fd=popen(cmd, "r")) )
    { bool rc = read_addr2line(fd, buf, size);

      pclose(fd);
      return rc;
    }
  }

  return false;
}

/* Starting addr2line(1) (atos(1) on MacOS) for each frame is slow.  If
 * possible, we keep such a process per object file and feed it addresses
 * through stdin.  Communication uses a socketpair() such that we can
 * avoid SIGPIPE if the process died using either send(MSG_NOSIGNAL) or
 * the SO_NOSIGPIPE socket option (BSD/MacOS).  Processes inherited
 * through fork() are not ours and are discarded.
 */

#ifdef HAVE_FORK
#include <sys/socket.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>
#if defined(MSG_NOSIGNAL) || defined(SO_NOSIGPIPE)
#define HAVE_ADDR2LINE_SERVER 1
#define A2L_SERVERS 4
#ifdef MSG_NOSIGNAL
#define A2L_SEND_FLAGS MSG_NOSIGNAL
#else
#define A2L_SEND_FLAGS 0
#endif

typedef struct a2l_server
{ char   *fname;			/* Object file we serve */
  pid_t	  pid;				/* Process id of addr2line */
  pid_t	  owner;			/* Process that started it */
  int	  fd;				/* Our end of the socketpair */
  FILE   *in;				/* Read end of fd */
} a2l_server;

static a2l_server a2l_servers[A2L_SERVERS];
static int	  a2l_next;		/* Next to replace */
#ifdef O_PLMT
static pthread_mutex_t a2l_mutex = PTHREAD_MUTEX_INITIALIZER;
#endif

static void
a2l_stop(a2l_server *s)
{ if ( s->fname )
  { free(s->fname);
    s->fname = NULL;
    fclose(s->in);			/* also closes s->fd */
    s->in = NULL;
    if ( s->owner == getpid() )		/* after fork() it is not our child */
      waitpid(s->pid, NULL, 0);		/* the server exits on EOF */
  }
}

/* Create a socketpair that does not leak into processes we start and
 * that does not raise SIGPIPE if the peer died.  dup2() in the child
 * clears FD_CLOEXEC, so the server does get its end.
 */

static bool
a2l_socketpair(int sv[2])
{ int type = SOCK_STREAM;

#ifdef SOCK_CLOEXEC
  type |= SOCK_CLOEXEC;
#endif

  if ( socketpair(AF_UNIX, type, 0, sv) != 0 )
    return false;

#ifndef SOCK_CLOEXEC
  for(int i=0; i<2; i++)
    (void)fcntl(sv[i], F_SETFD, FD_CLOEXEC);
#endif
#ifdef SO_NOSIGPIPE			/* MSG_NOSIGNAL may not be honoured */
  { int on = 1;
    (void)setsockopt(sv[0], SOL_SOCKET, SO_NOSIGPIPE, &on, sizeof(on));
  }
#endif

  return true;
}

static a2l_server *
a2l_start(a2l_server *s, const char *fname)
{ int sv[2];
  pid_t pid;

  if ( !a2l_socketpair(sv) )
    return NULL;

  if ( (pid=fork()) == 0 )
  { if ( dup2(sv[1], 0) < 0 || dup2(sv[1], 1) < 0 )
      _exit(1);
    ADDR2LINE_EXEC(fname);
    _exit(1);
  }
  close(sv[1]);
  if ( pid < 0 )
  { close(sv[0]);
    return NULL;
  }

  if ( !(s->in = fdopen(sv[0], "r")) ||
       !(s->fname = strdup(fname)) )
  { if ( s->in )
      fclose(s->in);
    else
      close(sv[0]);
    s->in = NULL;
    waitpid(pid, NULL, 0);
    return NULL;
  }
  s->fd	   = sv[0];
  s->pid   = pid;
  s->owner = getpid();

  return s;
}

static a2l_server *
a2l_find(const char *fname)
{ pid_t me = getpid();
  a2l_server *s;

  for(int i=0; i<A2L_SERVERS; i++)
  { s = &a2l_servers[i];
    if ( s->fname )
    { if ( s->owner != me )
	a2l_stop(s);
      else if ( strcmp(s->fname, fname) == 0 )
	return s;
    }
  }

  s = &a2l_servers[a2l_next];
  a2l_next = (a2l_next+1)%A2L_SERVERS;
  a2l_stop(s);

  return a2l_start(s, fname);
}

/* Returns 1 on success, 0 if there is no information and -1 if the
 * connection is broken.
 */

static int
a2l_query(a2l_server *s, uintptr_t offset, char *buf, size_t size)
{ char req[64];
  int len = snprintf(req, sizeof(req), "%p\n", (void*)offset);
  const char *r = req;

  while ( len > 0 )
  { ssize_t n = send(s->fd, r, len, A2L_SEND_FLAGS);

    if ( n < 0 )
    { if ( errno == EINTR )
	continue;
      return -1;
    }
    r += n;
    len -= (int)n;
  }

  if ( read_addr2line(s->in, buf, size) )
    return 1;

  return ferror(s->in) || feof(s->in) ? -1 : 0;
}

/* Returns 1 on success, 0 on failure and -1 if the server is not
 * available, in which case we should fall back to popen().
 */

static int
addr2line_server(const char *fname, uintptr_t offset, char *buf, size_t size)
{ int rc = -1;
  a2l_server *s;

#ifdef O_PLMT
  if ( pthread_mutex_trylock(&a2l_mutex) != 0 )
    return -1;			/* busy or called recursively from a crash */
#endif

  if ( (s=a2l_find(fname)) )
  { if ( (rc=a2l_query(s, offset, buf, size)) < 0 )
      a2l_stop(s);		/* process died; restart next time */
  }

#ifdef O_PLMT
  pthread_mutex_unlock(&a2l_mutex);
#endif

  return rc;
}

#endif /*MSG_NOSIGNAL || SO_NOSIGPIPE*/
#endif /*HAVE_FORK*/

static bool
addr2line2(const char *fname, uintptr_t offset, char *buf, size_t size)
{
#ifdef HAVE_ADDR2LINE_SERVER
  int rc = addr2line_server(fname, offset, buf, size);

  if ( rc >= 0 )
    return rc;
#endif

  return addr2line_popen(fname, offset, buf, size);
}
#endif/*!__WINDOWS__ && !__EMSCRIPTEN__ */

bool
addr2line(void *addr, char *buf, size_t size)
{
#if HAVE_DLADDR && HAVE_ADDR2LINE2
  Dl_info info;

  if ( dladdr(addr, &info) )
  { uintptr_t offset = (uintptr_t)addr - (uintptr_t)info.dli_fbase;

    if ( info.dli_fname )
    { if ( ( strstr(info.dli_fname, ".so")
#if __APPLE__
	     || strstr(info.dli_fname, ".dylib")
#endif
	   ) )
      { return addr2line2(info.dli_fname, offset, buf, size);
      } else if ( info.dli_sname )
      { snprintf(buf, size, "%s(%s+0x%tx)",
		 info.dli_fname, info.dli_sname,
		 (char*)addr-(char*)info.dli_saddr);
	return true;
      }
    }
  }
#endif/*HAVE_DLADDR*/

  return false;
}
