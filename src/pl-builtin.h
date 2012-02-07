/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2008, University of Amsterdam

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

#ifndef PL_BUILTIN_H_INCLUDED
#define PL_BUILTIN_H_INCLUDED

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This header provides definition that allow us compiling the C files that
define built-in predicates, disclosing as little   as  possible from the
SWI-Prolog kernel. The aim of this seperate   module is to allow reusing
these files in other Prolog systems with  minimal effort by providing an
implementation of this macro layer  that   satisfies  the  target Prolog
system.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	   COMMON GROUND	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
First, include config.h or, if MD is  specified, this file.  This allows
for -DMD="config/win64.h"
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if defined(O_PLMT) && !defined(_REENTRANT)
#define _REENTRANT 1
#endif

#if HAVE_XOS_H
#include <xos.h>			/* Windows POSIX enhancements */
#endif
#ifdef HAVE_UXNT_H
#include "os/windows/uxnt.h"		/* More Windows POSIX enhancements */
#endif

#include "pl-mutex.h"
#include "os/SWI-Stream.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Symbols are local to shared objects  by   default  in  COFF based binary
formats, and public in ELF based formats.   In some ELF based systems it
is possible to make them local   anyway. This enhances encapsulation and
avoids an indirection for calling these   functions.  Functions that are
supposed to be local to the SWI-Prolog kernel are declared using

    COMMON(<type) <function>(<args>);
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef HAVE_VISIBILITY_ATTRIBUTE
#define SO_LOCAL __attribute__((visibility("hidden")))
#else
#define SO_LOCAL
#endif
#define COMMON(type) SO_LOCAL type


		 /*******************************
		 *	    ENGINE ACCESS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The  engine  is  a  structure  that  is    represented  by  LD.  On  the
single-engine version this is simply the  address of a global structure.
On the multi-threaded version we must use some thread-specific mechanism
to find the engine structure  associated   with  the current thread. The
user of this module must provide two   definitions:

	* struct PL_local_data
	Structure definition for the engine
	* LD_GLOBAL
	Is a macro that expands to a pointer to the current engine.  The
	implementation varies.  Without engines it is just an alias for
	the address of the global engine structure.  With engines, it
	can use pthread_getspecific() (POSIX), TlsGetValue (Windows) or
	some compiler provided extension for thread-local variables.

LD always points to the  engine,  but   retrieving  it  everywhere  in a
function can be costly. GET_LD and  PRED_LD create an automatic variable
__PL_ld pointing to the engine.  LOCAL_LD   points  there. Time critical
modules generally start with:

#undef LD
#define LD LOCAL_LD

Now, functions that access LD must be written as:

func()
{ GET_LD

  ...
}

And predicates as

static
PRED_IMPL("foo", 2, foo, 0)
{ PRED_LD

   ...
}

In addition, LD can be passed between functions.  This is written as

func1(int arg ARG_LD)
{ ...
}

  func1(42 PASS_LD)

Note there is *NO* "," before ARG_LD   and PASS_LD, because these macros
expand to nothing if there is just  one engine. The versions ARG1_LD and
PASS_LD1 are used if there is no other argument.

On some frequently used functions,   writing PASS_LD everywhere clutters
the code too much. In this case  we   use  the schema below. Many of the
time critical functions from the public   foreign  interface are handled
this way for the internal builtins.

func__LD(int arg ARG_LD)
{ ...
}

#define func(arg) func__LD(arg PASS_LD)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct PL_local_data  PL_local_data_t;
typedef struct PL_global_data PL_global_data_t;

#define LD	  GLOBAL_LD

#if defined(O_PLMT) || defined(O_MULTIPLE_ENGINES)

#define GET_LD	  PL_local_data_t *__PL_ld = GLOBAL_LD;
#define PRED_LD   PL_local_data_t *__PL_ld = PL__ctx->engine;

#define ARG1_LD   PL_local_data_t *__PL_ld
#define ARG_LD    , ARG1_LD
#define PASS_LD1  LD
#define PASS_LD   , LD
#define LOCAL_LD  __PL_ld
#define IGNORE_LD (void)__PL_ld;

#else

#define GET_LD
#define ARG_LD
#define ARG1_LD void
#define PASS_LD
#define PASS_LD1
#define LOCAL_LD  (&PL_local_data)
#define GLOBAL_LD (&PL_local_data)
#define LD	  GLOBAL_LD
#define IGNORE_LD

#endif


		 /*******************************
		 *	 PRINT DEBUG INFO	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Internal debugging  and extra security checks.   To  enable them, compile
with -DO_DEBUG. Information should be printed using Sdprintf, which takes
the same arguments as printf(). Using Sdprintf() ensures that information
is also printed if stdio is not available.

    DEBUG(1, Sdprintf("Running with pid=%d\n", getpid()));
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include "pl-debug.h"

#if O_DEBUG
#define DEBUG(n, g) do { if ((n <= DBG_LEVEL9 && GD->debug_level >= (n)) || \
                             (n > DBG_LEVEL9 && GD->debug_topics && \
                              true_bit(GD->debug_topics, n))) \
                         { g; } } while(0)
#define DEBUGGING(n) (GD->debug_topics && true_bit(GD->debug_topics, n))
#else
#define DEBUG(a, b) ((void)0)
#endif

#if O_SECURE
#define SECURE(g) do { g; } while(0)
#else
#define SECURE(g) ((void)0)
#endif


		 /*******************************
		 *   NON-DET PREDICATE CONTEXT	*
		 *******************************/

typedef enum
{ FRG_FIRST_CALL = 0,		/* Initial call */
  FRG_CUTTED     = 1,		/* Context was cutted */
  FRG_REDO	 = 2		/* Normal redo */
} frg_code;

typedef struct foreign_context
{ uintptr_t		context;	/* context value */
  frg_code		control;	/* FRG_* action */
  struct PL_local_data *engine;		/* invoking engine */
} *control_t;

#define FRG_REDO_MASK	0x03
#define FRG_REDO_BITS	2
#define REDO_INT	0x02		/* Returned an integer */
#define REDO_PTR	0x03		/* returned a pointer */

#define ForeignRedoIntVal(v)	(((uintptr_t)(v)<<FRG_REDO_BITS)|REDO_INT)
#define ForeignRedoPtrVal(v)	(((uintptr_t)(v))|REDO_PTR)

#define ForeignRedoInt(v)	return ForeignRedoIntVal(v)
#define ForeignRedoPtr(v)	return ForeignRedoPtrVal(v)

#define ForeignControl(h)	((h)->control)
#define ForeignContextInt(h)	((intptr_t)(h)->context)
#define ForeignContextPtr(h)	((void *)(h)->context)
#define ForeignEngine(h)	((h)->engine)


		 /*******************************
		 *	   BIND PREDICATES	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Built-in predicates are defined as

PRED_IMPL("name", arity, c_symbol, flags)
{ <implementation body>
}

The macros A1..A10 provide access to the arguments (of type term_t). The
CTX_*  macros  provide  the  actual  arity  and  context  arguments  for
non-deterministic predicates. The implementation returns   using  one of
these  constructs.  The  `Redo'   variations    are   only   allowed  if
PL_FA_NONDETERMINISTIC is present in `flags'.

    * return FALSE
    * return TRUE
    * ForeignRedoInt(intptr_t val)
    * ForeignRedoPtr(void *ptr)

At the end of the file there is   a  section that looks like this, where
each PRED_DEF line  is  a  simple   direct  copy  of  the  corresponding
PRED_IMPL declaration:

BeginPredDefs(module)
  PRED_DEF("name", arity, c_symbol, flags)
  ...
EndPredDefs
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define PRED_IMPL(name, arity, fname, flags) \
        foreign_t \
        pl_ ## fname ## arity ## _va(term_t PL__t0, int PL__ac, \
				     control_t PL__ctx)

#define Arg(N)  (PL__t0+((n)-1))
#define A1      (PL__t0)
#define A2      (PL__t0+1)
#define A3      (PL__t0+2)
#define A3      (PL__t0+2)
#define A4      (PL__t0+3)
#define A5      (PL__t0+4)
#define A6      (PL__t0+5)
#define A7      (PL__t0+6)
#define A8      (PL__t0+7)
#define A9      (PL__t0+8)
#define A10     (PL__t0+9)

#define CTX_CNTRL ForeignControl(PL__ctx)
#define CTX_PTR   ForeignContextPtr(PL__ctx)
#define CTX_INT   ForeignContextInt(PL__ctx)
#define CTX_ARITY PL__ac

#define BeginPredDefs(id) \
        const PL_extension PL_predicates_from_ ## id[] = {
#define PRED_DEF(name, arity, fname, flags) \
        { name, arity, pl_ ## fname ## arity ## _va, (flags)|PL_FA_VARARGS },
#define PRED_SHARE(name, arity, fname, flags) \
        { name, arity, pl_ ## fname ## va_va, (flags)|PL_FA_VARARGS },
#define EndPredDefs \
        { NULL, 0, NULL, 0 } \
        };

		 /*******************************
		 *	       TIME		*
		 *******************************/

#define PL_unify_time(t, s) PL_unify_float(t, (double)(s))


		 /*******************************
		 *	       MISC		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
blockGC() and unblockGC() control  garbage   collection  in the provided
engine. The calls can be  nested,  but   the  program  must  ensure each
blockGC() is matched by an unblockGC().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

COMMON(void)	blockGC(int flags ARG_LD);	/* disallow garbage collect */
COMMON(void)	unblockGC(int flags ARG_LD);	/* re-allow garbage collect */
COMMON(void)	suspendTrace(int suspend);	/* suspend/resume tracing */

#endif /*PL_BUILTIN_H_INCLUDED*/
