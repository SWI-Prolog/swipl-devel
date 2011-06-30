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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

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


void
save_backtrace(const char *why)
{ btrace *bt = get_trace_store();

  if ( bt )
  { btrace_stack *s = &bt->dumps[bt->current];
    unw_cursor_t cursor; unw_context_t uc;
    int depth;

    unw_getcontext(&uc);
    unw_init_local(&cursor, &uc);
    for(depth=0; unw_step(&cursor) > 0 && depth < MAX_DEPTH; depth++)
    { unw_get_proc_name(&cursor,
			s->frame[depth].name, sizeof(s->frame[depth].name),
			&s->frame[depth].offset);
    }
    s->name = why;
    s->depth = depth;

    if ( ++bt->current == SAVE_TRACES )
      bt->current = 0;
  }

}


static void
print_trace(btrace *bt, int me)
{ btrace_stack *s = &bt->dumps[me];

  if ( s->name )
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
    { if ( bt->dumps[me].name && strcmp(bt->dumps[me].name, why) == 0 )
      { print_trace(bt, me);
	return;
      }
      if ( --me < 0 )
	me += SAVE_TRACES;
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


void
save_backtrace(const char *why)
{ btrace *bt = get_trace_store();

  if ( bt )
  { void *array[100];
    size_t frames;

    frames = backtrace(array, sizeof(array)/sizeof(void *));
    bt->sizes[bt->current] = frames;
    if ( bt->symbols[bt->current] )
      free(bt->symbols[bt->current]);
    bt->symbols[bt->current] = backtrace_symbols(array, frames);
    bt->why[bt->current] = why;
    if ( ++bt->current == SAVE_TRACES )
      bt->current = 0;
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
    { if ( bt->why[me] && strcmp(bt->why[me], why) == 0 )
      { print_trace(bt, me);
	return;
      }
      if ( --me < 0 )
	me += SAVE_TRACES;
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
