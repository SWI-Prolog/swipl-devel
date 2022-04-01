/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2015, University of Amsterdam
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

#ifndef PL_BUILTIN_H_INCLUDED
#define PL_BUILTIN_H_INCLUDED

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This header provides definition that allow us compiling the C files that
define built-in predicates, disclosing as little   as  possible from the
SWI-Prolog kernel. The aim of this separate   module is to allow reusing
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

#include "os/SWI-Stream.h"

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
	* GLOBAL_LD
	Is a macro that expands to a pointer to the current engine.  The
	implementation varies.  Without engines it is just an alias for
	the address of the global engine structure.  With engines, it
	can use pthread_getspecific() (POSIX), TlsGetValue (Windows) or
	some compiler provided extension for thread-local variables.

However,  thread-local  variable retrieval  can be costly when overused,
and moreover this assumes that only one engine will ever get accessed in
a given thread.  To improve performance and allow for accessing multiple
engines in a single thread  (without having to resort to  altering which
structure is pointed to  by GLOBAL_LD on every context switch,  which is
even more costly),  the  LD pointer  gets passed between  functions that
need  to reference it.  In order not to pollute  the codebase  with lots
of references  to LD arguments,  this  context-passing functionality  is
encapsulated in and implemented by the macro LDFUNC.

Ideally, all  library-internal functions  save those that neither access
the LD structure  nor call functions that do  will be declared using the
LDFUNC syntax. However, to support cases where the context object cannot
for whatever reason  be passed in  as a function argument,  it is  still
possible  to use  the macros  GET_LD or PRED_LD  at  the beginning  of a
function to retrieve the (thread-local) default LD object via GLOBAL_LD.
Note that,  regardless of whether a function  is declared as LDFUNC,  or
whether it declares GET_LD,  the LD macro  (when defined as ANY_LD,  the
default) will always provide a pointer to whatever is currently the best
way to access the structure using a compile-time switch. The logic is:

 - If there is a variable/parameter called __PL_ld in scope (as declared
   by GET_LD or LDFUNC, below), use it.
 - If there is a variable/parameter called PL__ctx in scope (as with
   some API functions, or in a PRED_IMPL() definition), use
   PL__ctx->engine.
 - Otherwise, fall back to using GLOBAL_LD.

The LOCAL_LD logic is similar,  but it does not  fall back to GLOBAL_LD.
Redefining LD as LOCAL_LD  provides a compile-time  (GCC, Clang, etc) or
link-time (non-GCC) check to ensure no functions are inadvertently using
the slow-access GLOBAL_LD.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			LDFUNC DECLARATION
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

To declare a function as an LD-using function, it needs to have its name
defined as a preprocessor macro  prior to its declaration,  in whichever
of the following three forms is appropriate:

    #define func_two_args(arg1, arg2) LDFUNC(func_two_args, arg1, arg2)
    #define func_va_args(arg, ...) LDFUNC(func_va_args, arg, __VA_ARGS__)
    #define func_no_args(_) LDFUNC(func_no_args, _)

Note that  the zero-argument function  is declared as  a single-argument
macro,  but C99 macro syntax allows a single-argument macro to be called
with  empty parentheses.  It's good practice  to place these definitions
near their prototypes and/or in a header file,  and to guard them behind
a directive of [[#if USE_LD_MACROS]] to assist static code analysis, but
the only strong requirement  is that it occur before  the first usage of
a function name.

A function's declaration/prototype  also needs to be annotated to accept
the LD argument, which can be done in one of two ways.  The first, which
is suitable for  header files  or anywhere multiple  function prototypes
are declared together, is to have the macro LDFUNC_DECLARATIONS defined,
and then declare the function with the usual syntax:

    #define LDFUNC_DECLARATIONS
    int func_two_args(int arg1, int arg2);
    int func_va_args(int arg, ...);
    int func_no_args(void);
    #undef LDFUNC_DECLARATIONS

The alternative, which should be used for function definition prototypes
but can also  be used for  declaration prototypes,  is to put  a DECL_LD
annotation  immediately after the open parenthesis of the argument list,
with no following comma. For zero-argument functions, it can replace the
void keyword:

    int func_two_args(DECL_LD int arg1, int arg2);
    int func_va_args(DECL_LD int arg, ...);
    int func_no_args(DECL_LD);

    int
    func_two_args(DECL_LD int arg1, int arg2)
    { return arg1 + arg2;
    }

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			LDFUNC NAME MANGLING
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

The LDFUNC macro  mangles the  function's name,  even in a build without
threading or  multiple engine  support,  so that  an internal LD-passing
variant  of an API function  can and should  share the same  programmer-
visible name. To declare a non-mangled function with the same name as an
LDFUNC, it is sufficient to put the function name in parentheses,  which
is transparent to C but suppresses macro expansion:

    int
    (PL_api_func)(int arg)
    { GET_LD
      return PL_api_func(arg);  <-- calls the LDFUNC variant!
    }

Note that  the GET_LD  isn't strictly necessary here  unless LD has been
explicitly declared to LOCAL_LD,  because of  the automatic LD selection
described above. For tiny API functions like this, however, the API_STUB
macro (which does include GET_LD) can be used:

    API_STUB(int)
    (PL_api_func)(int arg)
    ( return PL_api_func(arg); )

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			POINTERS TO LDFUNCS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Because the  name mangling only occurs  in a function call  or prototype
context,  where an opening parenthesis  immediately follows the function
name,  special care  needs to be  taken  when declaring,  assigning,  or
calling pointers  to LDFUNC functions.  Declaring a pointer to an LDFUNC
function uses the LDFUNCP macro, with the same DECL_LD rules as defining
an LDFUNC itself:

    int LDFUNCP (*p_ldfunc)(DECL_LD int arg1, int arg2) = NULL;

    #define LDFUNC_DECLARATIONS
    int call_ld_func(int LDFUNCP (*ldfunc_arg)(int, int), int arg);
    #undef LDFUNC_DECLARATIONS

Dereferencing and calling a pointer also uses the LDFUNCP macro:

    return LDFUNCP(*p_ldfunc)(1, 2);

(Note that the difference in spacing is just convention; for declaration
it looks like a syntax keyword, for dereferencing it looks like a
function call.)

To assign a specific function's address to such a pointer, however,  you
need the  LDFUNC_REF macro,  which  performs  the same  name mangling as
LDFUNC, but without any of the argument processing:

    p_ldfunc = LDFUNC_REF(func_two_args);
    return LDFUNCP(*p_ldfunc)(1, 2);

If a function  takes an LDFUNCP  as an argument  and expects callers  to
specify a literal function by name,  it may be helpful to define a macro
which inserts LDFUNC_REF. See pl-trie.h for an example.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			EXPLICIT LD SELECTION
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Two other  macros  need mentioning,  for cases where  the code  needs to
specify  exactly which  LD structure  is passed  to an  LDFUNC function,
rather than its own: WITH_LD and PASS_AS_LD.

WITH_LD is  a macro  that introduces a  statement-scope redefinition  of
the LD pointer.  This applies not just to calls to LDFUNC functions, but
also to  direct references  to the quasi-variable LD.  The WITH_LD macro
is defined as a for() loop that iterates exactly one time, and so shares
semantics with it;  this means that  break statements  will break out of
the WITH_LD itself.  If you need to break/continue  an enclosing for  or
while loop, either use a goto or use the PASS_AS_LD macro instead.

    #define get_current_ld(_) LDFUNC(get_current_ld,  _)
    PL_local_data_t *my_ld = LD;
    PL_local_data_t *other_ld = get_other_ld();
    WITH_LD(other_ld)
    { assert(LD == other_ld);
      assert(get_current_ld() == other_ld);
      break;
      assert(FALSE);
    }
    assert(LD == my_ld);

The PASS_AS_LD macro only works for LDFUNC function calls, but it can be
used in an expression context, and it does not override the current LD:

    #define get_current_ld(_) LDFUNC(get_current_ld,  _)
    PL_local_data_t *my_ld = LD;
    PL_local_data_t *other_ld = get_other_ld();

    assert(LD == my_ld);
    assert(get_current_ld(PASS_AS_LD(other_ld)) == other_ld);
    return func_two_args(PASS_AS_LD(other_ld) 1, 2);

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct PL_local_data  PL_local_data_t;
typedef struct PL_global_data PL_global_data_t;

#define LD	  ANY_LD

/* API_STUB(rtype)(fname)(adecls...)(stub body) */
#define API_STUB(rtype)		rtype _API_STUB_1
#define _API_STUB_1(fname)	fname _API_STUB_2
#define _API_STUB_2(...)	(__VA_ARGS__) _API_STUB_3
#define _API_STUB_3(...)	{GET_LD __VA_ARGS__;}

#if (defined(O_PLMT) || defined(O_MULTIPLE_ENGINES)) && USE_LD_MACROS

#ifdef __GNUC__
/* Instructing GCC to treat this as a system header greatly simplifies
 * diagnostic output when, for example, getting a no_local_ld error. */
#pragma GCC system_header
#endif

/* These are defined in pl-setup.c, but they'll never actually get used.
 * They're just here for scope-detection. */
const extern intptr_t __PL_ld;
const extern intptr_t PL__ctx;

/* __FIND_LD uses the C11 _Generic operator to resolve to the first
 * expression iff it is recognized as a PL_local_data_t* in the current
 * context, otherwise the engine of the second iff it is a control_t,
 * otherwise the third. Since the above const declarations are in global
 * scope and have type intptr_t, they will fail this test safely (i.e.
 * without causing an undefined reference or syntax error) if no shadowing
 * declaration has been made at block scope. And, since they are concrete
 * variables set to a constant -1, a debugger can use the actual __FIND_LD
 * function defined in pl-setup.c to resolve LD or LD-based expressions at
 * debug-time, even if (like GDB) it doesn't recognize the _Generic operator.
 */
PL_local_data_t* __FIND_LD(PL_local_data_t *pl_ld, control_t pl_ctx, PL_local_data_t *fallback);
#define __FIND_LD(pl_ld, pl_ctx, fallback) \
	_Generic \
	( (pl_ld), \
	  PL_local_data_t*: (PL_local_data_t*)(pl_ld), \
	  default: _Generic \
	  ( (pl_ctx), \
	    control_t: ((control_t)(pl_ctx))->engine, \
	    default: (fallback) \
	  ) \
	)

/* Abusing a GCC idiosyncracy here (don't call it a bug, they might fix it)
 * because I haven't found ANY documented way to exercise control over which
 * macros are recorded in the object file at -g3. In GCC, when a macro def
 * is pushed and then popped, it gets removed from the list of macros
 * presented to the debugger. The debugger will then fall back to the real
 * definition of __FIND_LD present in pl-setup.c rather than stumbling on
 * the above.
 */
#ifdef __GNUC__
# pragma push_macro("__FIND_LD")
# pragma pop_macro("__FIND_LD")
#endif

#if defined(__GNUC__) && __has_attribute(error)
/* If code references LOCAL_LD (either explicitly or by redefining LD as its
 * alias) but the current function is neither an LDFUNC nor provides a GET_LD
 * or similar, that's a programming error we want to report. With GCC, we can
 * use the error attribute to force a failure at compile-time... */
PL_local_data_t* __attribute__((error("LOCAL_LD referenced without a local LD declaration"))) no_local_ld(void);
#else
/* ...otherwise, we reference this nonexistent but descriptively-named
 * function to cause the failure at link-time. */
#define no_local_ld error_LOCAL_LD_referenced_without_a_local_LD_declaration
extern PL_local_data_t* no_local_ld(void);
#endif

#define _LD_WITH_FALLBACK(f) __FIND_LD(__PL_ld, PL__ctx, f)
#define LOCAL_LD  _LD_WITH_FALLBACK(no_local_ld())
#define ANY_LD    _LD_WITH_FALLBACK(GLOBAL_LD)

#define GET_LD	  PL_local_data_t *__PL_ld = GLOBAL_LD;
#define PRED_LD   PL_local_data_t *__PL_ld = PL__ctx->engine;
/* The comma in the DECL_LD definition is the important part, the tilde
 * is just to force a parse error if it ends up in the output.
 * M_DEFER1 prevents this from being expanded before we can wrap it
 * (and its following argument) in parentheses, below. */
#define DECL_LD			M_DEFER1(~DECL_LD,)

#define LDFUNC_NAME(func)	func ## ___LD

/* Using triple-underscore here because the calling convention has
 * changed; LD now goes in first position, rather than last.
 * Passing a0 to USE_LDFUNC in parentheses relaxes some of the
 * timing constraints imposed by the M_DEFER1 above, since we don't
 * need the expansion to occur at an exact moment in the processing. */
#define LDFUNC(func, a0, ...)	LDFUNC_NAME(func) \
				  M_IFEMPTY(LDFUNC_DECLARATIONS) \
				  ( DECL_LDFUNC(a0, ## __VA_ARGS__) ) \
				  ( USE_LDFUNC((a0), ## __VA_ARGS__) )
/* «int LDFUNCP (*funcptr)(DECL_LD)» declares a variable or argument named
 * "funcptr" which is a pointer to an LD-using function that accepts
 * no arguments (other than a possible LD-passing arg) and returns int.
 * You can call this function pointer with:
 * int x = LDFUNCP(funcptr)();
 */
#define LDFUNCP(pdecl)		(pdecl) _LDFUNCP_ARGS
/* Important that this mirrors the definition of LDFUNC() very
 * closely, because of the timing constraints of the M_DEFER1
 * in DECL_LD above. */
#define _LDFUNCP_ARGS(a0, ...)	M_IFEMPTY(LDFUNC_DECLARATIONS) \
				( DECL_LDFUNC(a0, ## __VA_ARGS__) ) \
				( USE_LDFUNC((a0), ## __VA_ARGS__) )

/* Gets a pointer to a function declared as an LDFUNC */
#define LDFUNC_REF(func)	(&LDFUNC_NAME(func))

#define _ARG1_LD PL_local_data_t *__PL_ld
#define IGNORE_LD (void)__PL_ld;
#define HAS_LD (LD != 0)

#define __VOID_EMPTY_
#define __VOID_EMPTY_void
#define DECL_LDFUNC(a0, ...) \
( A_UNWRAP	/* Remove parentheses from results of A_ARGN, below.	*/ \
  ( A_ARGN	/* Expand to the third argument (index 2) after the arg	*/ \
    ( 2,	/* expansion  pass.  If  a0 is  empty or  «void»,  then	*/ \
      A_TRAILING_COMMA(__VOID_EMPTY_ ## a0) /* ← this will be empty, so	*/ \
      (__VOID_EMPTY_ ## a0),	/* ← this is arg 0 (rather than arg 1).	*/ \
      (_ARG1_LD, a0),		/* ← This is arg 2 when a0 is nonempty;	*/ \
      (_ARG1_LD)		/* ← this is arg 2 when empty/void.	*/ \
	/* (The argument after the A_TRAILING_COMMA is only used in the	*/ \
	/* PASS_AS_LD macro; see its definition below for more info.)	*/ \
    )			/* A_UNWRAP then  removes the parentheses  from	*/ \
  ), ## __VA_ARGS__	/* that, and we append any other args declared.	*/ \
)

#define USE_LDFUNC(a0, ...) \
( A_UNWRAP	/* As above,  but the extra comma in a0  comes from the	*/ \
  ( A_ARGN	/* DECL_LD definition above.  The M_DEFER2 ensures that	*/ \
    ( 2,	/* a0  isn't expanded  into its comma form  until right	*/ \
      A_UNWRAP(a0),	/* ← this moment, so it sneaks in as "one" arg.	*/ \
      A_CALL(DECL_LDFUNC, A_SHIFT1 a0),		/* ←  This then  is the	*/ \
	/* expansion when DECL_LD is used in a prototype, and it passes	*/ \
	/* the expansion  to DECL_LDFUNC() above.  The  A_CALL()  macro	*/ \
        /* provides a level of indirection,  so that A_SHIFT1 (used for	*/ \
        /* discarding DECL_LD's initial «~,»)  has settled before being	*/ \
        /* passed to DECL_LDFUNC(), thus we get the right token in a0.	*/ \
      (LD A_LEADING_COMMA(A_ECHO a0) )	/* ← This is the expansion when	*/ \
        /* this is  a callsite.  LD is inserted  as the first argument,	*/ \
        /* and  A_LEADING_COMMA  puts  a comma before a0  unless  a0 is	*/ \
	/* empty (i.e. if this function's only argument is LD).		*/ \
    ) \
  ), ## __VA_ARGS__ \
)

/* Block-scope redefinition of LD in a function */
#define WITH_LD(ld)	for (PL_local_data_t *__PL_ld = (ld), *__loopctr = NULL; !__loopctr; __loopctr++)
/* Passing an alternate LD to a called function. This uses the same mechanism
 * as DECL_LD, but takes it one step further. _VE_PASSLD below is a macro that
 * does not exist, but when DECL_LDFUNC() pastes __VOID_EMPTY_ to the beginning
 * you get a macro that includes an *additional* comma, pushing the "arg 0" that
 * doesn't normally get used into the proper position. */
#define PASS_AS_LD(ld)	M_DEFER1(~PASS_AS_LD, _VE_PASSLD(ld))
#define __VOID_EMPTY__VE_PASSLD(ld)	(ld),

#else

#define GET_LD
#define PRED_LD
#define DECL_LD
#define LDFUNC_NAME(func)	func ## ___NOLD
#define LDFUNC(func, ...) LDFUNC_NAME(func)(__VA_ARGS__)
#define LDFUNCP(pdecl)		(pdecl)
#define LDFUNC_REF(func)	(&LDFUNC_NAME(func))
#define LOCAL_LD GLOBAL_LD
#define ANY_LD GLOBAL_LD
#define IGNORE_LD
#define HAS_LD (1)

#if USE_LD_MACROS
/* There are reasons why one might use WITH_LD or PASS_AS_LD in code written for single- or multi-engine */
# define WITH_LD(_) for (int i=0; i < 1; i++)
# define PASS_AS_LD(_)
#else
/* For type-checking in static analysis */
static inline int pass_as_ld_helper(PL_local_data_t *ld) {return 0;}
# define WITH_LD(ld) for(PL_local_data_t *__ignore_ld=(ld), *__loopctr = NULL; !__loopctr; __loopctr++)
# define PASS_AS_LD(ld) pass_as_ld_helper(ld) +
#endif

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
#define DEBUG(n, g)	do \
			{ if (DEBUGGING(n)) \
			  { ENTER_DEBUG(n); g; EXIT_DEBUG(n); } \
			} while(0)
#define ENTER_DEBUG(n)	pl_internaldebugstatus_t \
			  __orig_ld_debug = GLOBAL_LD->internal_debug, \
			  __new_ld_debug = GLOBAL_LD->internal_debug = \
			  (pl_internaldebugstatus_t) \
			  { .channel = DEBUGGING(n) ? prolog_debug_topic_name(n) : NULL, \
			    .depth = __orig_ld_debug.depth + 1, \
			  }
#define EXIT_DEBUG(n)	GLOBAL_LD->internal_debug = \
			( GLOBAL_LD->internal_debug.depth != __new_ld_debug.depth \
			? Sdprintf("DEBUG stack depth mismatch! %d != %d\n", GLOBAL_LD->internal_debug.depth, __new_ld_debug.depth) \
			: 1 \
			) ? __orig_ld_debug : __orig_ld_debug
#define DEBUGGING(n)	(((n) <= DBG_LEVEL9 && GD->debug_level >= (n)) || \
			 ((n) > DBG_LEVEL9 && GD->debug_topics && true_bit(GD->debug_topics, n)))
#define WITH_DEBUG_FOR(n) for \
			( ENTER_DEBUG(n); \
			  __orig_ld_debug.depth >= 0; \
			  EXIT_DEBUG(n), __orig_ld_debug.depth = -1 )
#define IF_DEBUGGING(n)	if (DEBUGGING(n)) WITH_DEBUG_FOR(n)

/* We want to use the version of Sdprintf with the debug channel, if possible */
#undef Sdprintf
#define Sdprintf(fmt...) Sdprintf_ex(GLOBAL_LD->internal_debug.channel, __FILE__, __LINE__, fmt)
int Sdprintf_ex(const char *channel, const char *file, int line, const char *fm, ...);

#else
#define DEBUG(a, b) ((void)0)
#define ENTER_DEBUG(n) ;
#define EXIT_DEBUG(n) ;
#define DEBUGGING(n) FALSE
#define WITH_DEBUG_FOR(n) /* empty */
#define IF_DEBUGGING(n) if (0)
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
  FRG_REDO	 = 2,		/* Normal redo */
  FRG_RESUME	 = 3		/* Resume from yield */
} frg_code;

struct foreign_context
{ uintptr_t		context;	/* context value */
  frg_code		control;	/* FRG_* action */
  struct PL_local_data *engine;		/* invoking engine */
  struct definition    *predicate;	/* called Prolog predicate */
};

#define FRG_REDO_MASK	0x03
#define FRG_REDO_BITS	2
#define REDO_PTR	0x00		/* Returned a pointer */
#define YIELD_PTR	0x01		/* Returned a pointer */
#define REDO_INT	0x02		/* Returned an integer */

#define ForeignRedoIntVal(v)	(((uintptr_t)((v)<<FRG_REDO_BITS))|REDO_INT)
#define ForeignRedoPtrVal(v)	(((uintptr_t)(v))|REDO_PTR)
#define ForeignYieldPtrVal(v)	(((uintptr_t)(v))|YIELD_PTR)

#define ForeignRedoInt(v)	return ForeignRedoIntVal(v)
#define ForeignRedoPtr(v)	return ForeignRedoPtrVal(v)
#define ForeignYieldPtr(v)	return ForeignYieldPtrVal(v)

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

/* Moved to pl-gc.c:
 * blockGC(int flags);
 * unblockGC(int flags);
 *
 * Moved to pl-trace.c:
 * suspendTrace(int suspend);
 */
#endif /*PL_BUILTIN_H_INCLUDED*/
