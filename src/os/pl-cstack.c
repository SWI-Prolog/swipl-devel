/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2017, University of Amsterdam
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

#ifdef _WIN64
#define _WIN32_WINNT 0x0501            /* get RtlCaptureContext() */
#endif

#define _GNU_SOURCE
#include "pl-incl.h"
#include "os/pl-cstack.h"
#include <time.h>

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
	s->shared = TRUE;
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
{ btrace *bt = get_trace_store(TRUE);

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
{ btrace *bt = get_trace_store(FALSE);

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

  Sdprintf("No backtrace named %s\n", why);
}

#endif /*HAVE_LIBUNWIND*/


		 /*******************************
		 *	       GLIBC		*
		 *******************************/

#if !defined(BTRACE_DONE) && defined(HAVE_EXECINFO_H) && defined(HAVE_BACKTRACE)
#define BTRACE_DONE 1
#include <execinfo.h>
#include <string.h>
#include <dlfcn.h>

#define MAXCMD 1024

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
	s->shared = TRUE;
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
{ btrace *bt = get_trace_store(TRUE);

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


static int
addr2line(const char *fname, uintptr_t offset, char *buf, size_t size)
{ char cmd[MAXCMD];

  if ( snprintf(cmd, size, "addr2line -fe \"%s\" %p",
		fname, (void*)offset) < size )
  { FILE *fd;

    if ( (fd=popen(cmd, "r")) )
    { int c;
      char *ebuf = &buf[size-1];
      char *o = buf;
      int nl = 0;

      while((c=fgetc(fd)) != EOF && o<ebuf)
      { if ( c == '\n' )
	{ const char *sep = "() at ";
	  nl++;

	  if ( nl == 1 && o+strlen(sep) < ebuf)
	  { strcpy(o, sep);
	    o += strlen(sep);
	  }
	} else
	{ *o++ = c;
	}
      }

      *o = '\0';

      fclose(fd);
      return o > buf;
    }
  }

  return FALSE;
}


static void
print_trace(btrace *bt, int me)
{ size_t i;

  if ( bt->why[me] )
  { Sdprintf("C-stack trace labeled \"%s\":\n", bt->why[me]);

    for(i=0; i<bt->sizes[me]; i++)
    { Dl_info info;
      void *addr = bt->retaddr[me][i];

      if ( dladdr(addr, &info) )
      { uintptr_t offset = (uintptr_t)addr - (uintptr_t)info.dli_fbase;

	if ( info.dli_fname )
	{ char buf[512];

	  if ( strstr(info.dli_fname, ".so") &&
	       addr2line(info.dli_fname, offset, buf, sizeof(buf)) )
	    Sdprintf("  [%d] %s [%p]\n", i, buf, addr);
	  else if ( info.dli_sname )
	    Sdprintf("  [%d] %s(%s+%p) [%p]\n",
		     i, info.dli_fname, info.dli_sname, addr-info.dli_saddr,
		     addr);
	  else
	    Sdprintf("  [%d] %s(+%p) [%p]\n",
		     i, info.dli_fname, (void*)offset, addr);
	} else
	{ Sdprintf("  [%d] ??? [%p]\n", i, addr);
	}
      }
    }
  } else
  { Sdprintf("No stack trace\n");
  }
}

void
print_backtrace(int last)		/* 1..SAVE_TRACES */
{ btrace *bt = get_trace_store(FALSE);

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
      if ( bt->why[me] && strcmp(bt->why[me], why) == 0 )
      { print_trace(bt, me);
	return;
      }
      if ( --me == bt->current-1 )
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

#ifndef HAVE_CTIME_R
#define ctime_r(timep, buf) strcpy(buf, ctime(timep))
#endif

static void
crashHandler(int sig)
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
  print_c_backtrace("crash");
  run_on_halt(&GD->os.exit_hooks, 4);

#if defined(HAVE_KILL) && defined(HAVE_GETPID)
  kill(getpid(), sig);
#else
  abort();
#endif
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
#if defined(SIGBUS) && SIGBUS != SIGSEGV
  PL_signal(SIGBUS, crashHandler);
#endif
#ifdef SIGFPE
  PL_signal(SIGFPE, crashHandler);
#endif
#ifdef SIGSYS
  PL_signal(SIGSYS, crashHandler);
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
      s->shared = TRUE;
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
{ btrace *bt = get_trace_store(TRUE);
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

    SymInitialize(hProcess, NULL, TRUE);

    Sdprintf("Stack trace labeled \"%s\":\n", s->name);
    for(depth=0; depth<s->depth; depth++)
    { if (s->frame[depth].module[0])
      {
#ifdef HAVE_LIBDWARF
        IMAGEHLP_MODULE64 moduleInfo;
        char dwarf_srclinebuf[MAX_PATH];
        char *dwarf_srcline = dwarf_srclinebuf;

        memset(&moduleInfo,0,sizeof(IMAGEHLP_MODULE64));
        memset(dwarf_srcline, 0, MAX_PATH);
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
{ btrace *bt = get_trace_store(FALSE);

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
		 *	     SHARED		*
		 *******************************/

#ifdef BTRACE_DONE

void
print_backtrace_named(const char *why)
{ bstore_print_backtrace_named(get_trace_store(FALSE), why);
}


void
print_c_backtrace(const char *why)
{ btrace *bt = save_backtrace(why);

  bstore_print_backtrace_named(bt, why);
  if ( bt && !bt->shared )
    btrace_destroy(bt);
}

#endif /*BTRACE_DONE*/


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

#endif /*BTRACE_DONE*/
