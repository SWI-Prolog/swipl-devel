/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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

#ifndef _PL_INCLUDE_H
#define _PL_INCLUDE_H

#if defined(WIN32) && !defined(__WIN32__)
#define __WIN32__ 1
#endif

#ifdef __WIN32__
#define MD	     "config/win32.h"
#define PLHOME       "c:/Program Files/pl"
#define DEFSTARTUP   "pl.ini"
#define ARCH	     "i386-win32"
#define C_LIBS	     ""
#define C_STATICLIBS ""
#define C_CC	     "cl"
#define C_LDFLAGS    ""
#else
#include <parms.h>			/* pick from the working dir */
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
First, include config.h or, if MD is  specified, this file.  This allows
for -DMD="md-mswin.h"
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef CONFTEST
#ifdef MD
#include MD
#else
#include <config.h>
#endif
#endif

#if defined(O_PLMT) && !defined(_REENTRANT)
#define _REENTRANT 1
#endif

#ifdef HAVE_DMALLOC_H
#include <dmalloc.h>			/* Use www.dmalloc.com debugger */
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		      PROLOG SYSTEM OPTIONS

These are not really options normally.  They are there because I use  to
add  new  features  conditional  using  #if ... #endif.  In many cases I
leave them in for ducumentation purposes.   Notably  O_STRING  might  be
handy for it someone wants to add a data type to the system.

  O_STRING
      Include data type string.  This  feature  does  not  rely  on  any
      system  feature.   It  hardly has any consequences for the system.
      Because of its experimental nature it is optional.  The definition
      of the predicates operating on strings might change.
      (NOTE: Currently some of the boot files rely on strings. It is NOT
      suggested to leave them out).
  O_COMPILE_OR
      Compile ->/2, ;/2 and |/2 into WAM.  This  no  longer  is  a  real
      option.   the mechanism to handle cuts without compiling ;/2, etc.
      has been taken out.
  O_COMPILE_ARITH
      Include arithmetic compiler (compiles is/2, >/2, etc. into WAM).
  O_PROLOG_FUNCTIONS
      Include evaluable Prolog functions into the arithmetic module.
  O_BLOCK
      Include support for block/3, !/1, fail/1 and exit/2 in the VM.
  O_LABEL_ADDRESSES
      Means we can pick up the address of a label in  a function using
      the var  = `&&label' construct  and jump to  it using goto *var;
      This construct is known by the GNU-C compiler gcc version 2.  It
      is buggy in gcc-2.0, but seems to works properly in gcc-2.1.
  VMCODE_IS_ADDRESS
      Can only  be set when  O_LABEL_ADDRESSES is  set.  It causes the
      prolog  compiler  to put the  code  (=  label-) addresses in the
      compiled Prolog  code  rather than the  virtual-machine numbers.
      This speeds-up  the vm  instruction dispatching in  interpret().
      See also pl-comp.c
  O_LOGICAL_UPDATE
      Use `logical' update-view for dynamic predicates rather then the
      `immediate' update-view of older Prolog systems.
  O_PLMT
      Include support for multi-threaded Prolog application.  Currently
      very incomplete and only for the POSIX thread library.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define PL_KERNEL		1
#define O_COMPILE_OR		1
#define O_SOFTCUT		1
#define O_COMPILE_ARITH		1
#define O_STRING		1
#define O_PROLOG_FUNCTIONS	1
#define O_BLOCK			1
#define O_CATCHTHROW		1
#define O_INLINE_FOREIGNS	1
#define O_DEBUGGER		1
#define O_INTERRUPT		1
#define O_DESTRUCTIVE_ASSIGNMENT 1
#define O_HASHTERM		1
#define O_LIMIT_DEPTH		1
#define O_SAFE_SIGNALS		1
#define O_LOGICAL_UPDATE	1
#define O_ATOMGC		1
#define O_CLAUSEGC		1

#ifndef DOUBLE_TO_LONG_CAST_RAISES_SIGFPE
#ifdef __i386__
#define DOUBLE_TO_LONG_CAST_RAISES_SIGFPE 1
#endif
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
On the MIPS, whether or not alignment can   be used appears to depend on
the compilation flags. We play safe for   the  case the user changes the
flags after running configure.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if !defined(DOUBLE_ALIGNMENT) && defined(__mips__)
#define DOUBLE_ALIGNMENT sizeof(double)
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The ia64 says setjmp()/longjmp() buffer must be aligned at 128 bits
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef JMPBUF_ALIGNMENT
#ifdef __ia64__
#define JMPBUF_ALIGNMENT 128
#else
#ifdef DOUBLE_ALIGNMENT
#define JMPBUF_ALIGNMENT DOUBLE_ALIGNMENT
#endif
#endif
#endif

#if MMAP_STACK || HAVE_VIRTUALALLOC
#define O_DYNAMIC_STACKS 1		/* sparse memory management */
#else
#ifndef O_SHIFT_STACKS
#define O_SHIFT_STACKS 1		/* use stack-shifter */
#endif
#endif

#ifndef O_LABEL_ADDRESSES
#if __GNUC__ == 2
#define O_LABEL_ADDRESSES	1
#endif
#endif

#if O_LABEL_ADDRESSES && !defined(VMCODE_IS_ADDRESS)
#define VMCODE_IS_ADDRESS	1
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Runtime version.  Uses somewhat less memory and has no tracer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_RUNTIME
#undef O_PROFILE			/* no profiling */
#undef O_DEBUGGER			/* no debugging */
#undef O_READLINE			/* no readline too */
#undef O_INTERRUPT			/* no interrupts too */
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The macros below try to establish a common basis for various  compilers,
so  we  can  write  most  of the real code without having to worry about
compiler limits and differences.

The current version has prototypes  defined   for  all functions. If you
have a very old compiler, try  the   unprotoize  program that comes with
gcc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef __unix__
#if defined(_AIX) || defined(__APPLE__) || defined(__unix) || defined(__BEOS__)
#define __unix__ 1
#endif
#endif

/* AIX requires this to be the first thing in the file.  */
#ifdef __GNUC__
# define alloca __builtin_alloca
#else
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
void *alloca ();
#   endif
#  endif
# endif
#endif

#if HAVE_XOS_H
#include <xos.h>
#endif
#ifdef HAVE_UXNT_H
#include <uxnt.h>
#endif

#include "pl-mutex.h"
#include "pl-stream.h"

#include <sys/types.h>
#include <setjmp.h>
#ifdef ASSERT_H_REQUIRES_STDIO_H
#include <stdio.h>
#endif /*ASSERT_H_REQUIRES_STDIO_H*/
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <stdarg.h>

#ifdef HAVE_SIGNAL
#include <signal.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#else
#ifdef HAVE_SYS_MALLOC_H
#include <sys/malloc.h>
#endif
#endif

#if defined(STDC_HEADERS) || defined(HAVE_STRING_H)
#include <string.h>
/* An ANSI string.h and pre-ANSI memory.h might conflict.  */
#if !defined(STDC_HEADERS) && defined(HAVE_MEMORY_H)
#include <memory.h>
#endif /* not STDC_HEADERS and HAVE_MEMORY_H */
#else /* not STDC_HEADERS and not HAVE_STRING_H */
#include <strings.h>
/* memory.h and strings.h conflict on some systems.  */
#endif /* not STDC_HEADERS and not HAVE_STRING_H */

#if OS2 && EMX
#include <process.h>
#include <io.h>
#endif /* OS2 */

/* prepare including BeOS types */
#ifdef __BEOS__
#define bool BOOL

#include <BeBuild.h>
#if (B_BEOS_VERSION <= B_BEOS_VERSION_5)
# include <socket.h>      /* include socket.h to get the fd_set structure */
#else
# include <SupportDefs.h> /* not needed for a BONE-based networking stack */
#endif
#include <OS.h>

#undef true
#undef false
#undef bool
#define EMULATE_DLOPEN 1		/* Emulated dlopen() in pl-beos.c */
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A common basis for C keywords.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if __GNUC__ && !__STRICT_ANSI__
#define HAVE_INLINE 1
#define HAVE_VOLATILE 1
#endif

#if !defined(HAVE_INLINE) && !defined(inline)
#define inline
#endif

#if defined(__GNUC__) && !defined(register)
#define register
#endif

#if defined(__GNUC__) && !defined(__OPTIMIZE__)
#define _DEBUG 1
#endif

#ifndef HAVE_VOLATILE
#define volatile
#endif


#if __STRICT_ANSI__
#undef TAGGED_LVALUE
#endif

#if defined(__STRICT_ANSI__) || defined(NO_ASM_NOP)
#define ASM_NOP { static int nop; nop++; }
#endif

#define forwards static		/* forwards function declarations */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Booleans,  addresses,  strings  and other   goodies.   Note that  ANSI
compilers have  `Void'.   This should  be  made part  of  the  general
platform as well.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef int			bool;
typedef double			real;
typedef void *			Void;

#if __GNUC__ && !__STRICT_ANSI__
#define LocalArray(t, n, s)	t n[s]
#else
#define LocalArray(t, n, s)	t *n = (t *) alloca((s)*sizeof(t))
#endif

#define TermVector(name, s)	LocalArray(Word, name, s)

#ifndef TRUE
#define TRUE			1
#define FALSE			0
#endif
#define succeed			return TRUE
#define fail			return FALSE
#define TRY(goal)		if ((goal) == FALSE) fail

#define CL_START		0	/* asserta */
#define CL_END			1	/* assertz */

typedef void *			caddress;

#define EOS			('\0')
#define ESC			((char) 27)
#define streq(s, q)		((strcmp((s), (q)) == 0))

#define CHAR_MODE 0		/* See PL_unify_char() */
#define CODE_MODE 1
#define BYTE_MODE 2

#ifndef abs
#define abs(x)			((x) < 0 ? -(x) : (x))
#endif
				/* n is 2^m !!! */
#define ROUND(p, n)		((((p) + (n) - 1) & ~((n) - 1)))
#define addPointer(p, n)	((void *) ((char *)(p) + (long)(n)))
#define diffPointers(p1, p2)	((char *)(p1) - (char *)(p2))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			     LIMITS

Below are some arbitrary limits on object sizes.  Feel free  to  enlarge
them.  Descriptions:

	* LINESIZ
	Buffer used to store textual info.  It is not concerned with
	critical things, just things like building an error message,
	reading a command for the tracer, etc.

	* MAXARITY
	Maximum arity of a predicate.  May be enarged further, but
	wastes stack (4 bytes for each argument) on machines that
	use malloc() for allocating the stack as the local and global
	stack need to be apart by this amount.  Also, an interrupt
	skips this amount of stack.
	
	* MAXVARIABLES
	Maximum number of variables in a clause.  May be increased
	further, but the global local stacks need to be separated
	by this amount for the segmentation-fault based stack expansion
	version (just costs virtual memory).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define BUFFER_RING_SIZE 	16	/* foreign buffer ring (pl-fli.c) */
#define LINESIZ			1024	/* size of a data line */
#define MAXARITY		1024	/* arity of predicate */
#define MAXSYMBOLLEN		256	/* max size of foreign symbols */
#define MAXVARIABLES		65536	/* number of variables/clause */
#define OP_MAXPRIORITY		1200	/* maximum operator priority */
#define SMALLSTACK		200 * 1024 /* GC policy */

#define WORDBITSIZE		(8 * sizeof(word))
#define LONGBITSIZE		(8 * sizeof(long))
#define INTBITSIZE		(8 * sizeof(int))
#define WORDS_PER_DOUBLE        ((sizeof(double)+sizeof(word)-1)/sizeof(word))

				/* Prolog's integer range */
#define PLMINTAGGEDINT		(-(long)(1L<<(WORDBITSIZE - LMASK_BITS - 1)))
#define PLMAXTAGGEDINT		(-PLMINTAGGEDINT - 1)
#define inTaggedNumRange(n)	(((n)&~PLMAXTAGGEDINT) == 0 || \
				 ((n)&~PLMAXTAGGEDINT) == ~PLMAXTAGGEDINT)
#define PLMININT		((long)(1L<<(WORDBITSIZE-1)))
#define PLMAXINT		(-(PLMININT+1))

#if vax
#define MAXREAL			(1.701411834604692293e+38)
#else					/* IEEE double */
#define MAXREAL			(1.79769313486231470e+308)
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Allocation Parameters
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define ALLOCSIZE	(1<<15)	/* size of allocation chunks (64K) */
#define ALLOCFAST	512	/* big enough for all structures */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Macros to handle hash tables.  See pl-table.c for  details.   First  the
sizes  of  the  hash  tables are defined.  Note that these should all be
2^N.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define ATOMHASHSIZE		1024	/* global atom table */
#define FUNCTORHASHSIZE		512	/* global functor table */
#define PROCEDUREHASHSIZE	256	/* predicates in module user */
#define MODULEPROCEDUREHASHSIZE 16	/* predicates in other modules */
#define MODULEHASHSIZE		16	/* global module table */
#define PUBLICHASHSIZE		8	/* Module export table */
#define FLAGHASHSIZE		16	/* global flag/3 table */
#define ARITHHASHSIZE		32	/* arithmetic function table */

#define pointerHashValue(p, size) ((((long)(p) >> LMASK_BITS) ^ \
				    ((long)(p) >> (LMASK_BITS+5)) ^ \
				    ((long)(p))) & \
				   ((size)-1))

#define TABLE_REF_MASK		0x1L
#define isTableRef(p)		((unsigned long)(p) & TABLE_REF_MASK)
#define makeTableRef(p)		((void*)((unsigned long)(p) | TABLE_REF_MASK))
#define unTableRef(s, p)	(*((s*)((unsigned long)(p) & ~TABLE_REF_MASK)))

#define return_next_table(t, v, clean) \
	{ for((v) = (v)->next; isTableRef(v) && (v); (v) = unTableRef(t, v)) \
	  if ( (v) == (t)NULL ) \
	  { clean; \
	    succeed; \
	  } \
	  ForeignRedoPtr(v); \
	}

#define for_table(ht, s, code) \
	{ int _k; \
	  PL_LOCK(L_TABLE); \
	  for(_k = 0; _k < (ht)->buckets; _k++) \
	  { Symbol _n, s; \
	    for(s=(ht)->entries[_k]; s; s = _n) \
	    { _n = s->next; \
	      code; \
	    } \
	  } \
          PL_UNLOCK(L_TABLE); \
	}
#define for_unlocked_table(ht, s, code) \
	{ int _k; \
	  for(_k = 0; _k < (ht)->buckets; _k++) \
	  { Symbol _n, s; \
	    for(s=(ht)->entries[_k]; s; s = _n) \
	    { _n = s->next; \
	      code; \
	    } \
	  } \
	}

/* Definition->indexPattern is set to NEED_REINDEX if the definition's index
   pattern needs to be recomputed */
#define NEED_REINDEX ((unsigned long)1L << (LONGBITSIZE-1))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Foreign language interface definitions.  Note that these macros MUST  be
consistent  with  the  definitions  in  pl-itf.h, which is included with
users foreign language code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define FRG_CONTROL_MASK	0x00000003L
#define FRG_CONTROL_BITS	2

typedef enum
{ FRG_FIRST_CALL = 0,		/* Initial call */
  FRG_CUTTED     = 1,		/* Context was cutted */
  FRG_REDO	 = 2		/* Normal redo */
} frg_code;

#define FIRST_CALL	(0L)

#define ForeignRedoIntVal(v)	(((unsigned long)(v)<<FRG_CONTROL_BITS)|FRG_REDO)
#define ForeignRedoPtrVal(v)	(((unsigned long)(v))|FRG_REDO)

#define ForeignRedoInt(v)	return ForeignRedoIntVal(v)
#define ForeignRedoPtr(v)	return ForeignRedoPtrVal(v)

#define ForeignControl(h)	((frg_code)((h) & FRG_CONTROL_MASK))
#define ForeignContextInt(h)	((long)(h)>>FRG_CONTROL_BITS)
#define ForeignContextPtr(h)	((void*)((unsigned long)(h)&~FRG_CONTROL_MASK))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Virtual machine instruction declarations.  Prefixes:

  I_	General instructions
  B_	Body specific version
  H_	Head specific versin
  A_	Arithmetic compilation specific
  C_	Control (compilation of ;/2, etc.)

Numbering these things is arbitrary,  but  for  fast  operation  of  the
switch  in  pl-wam.c,  numbering  should start at 0 and be without gaps.
I_HIGHEST must be made equal to the highest  value  of  the  instruction
codes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define I_NOP		((code) 0)		/* nop */
#define I_ENTER		((code) 1)		/* enter body */
#define I_CALL		((code) 2)		/* call procedure */
#define I_DEPART	((code) 3)		/* last call of procedure */
#define I_EXIT		((code) 4)		/* exit procedure */
#define B_FUNCTOR	((code) 5)		/* start functor */
#define B_RFUNCTOR	((code) 6)		/* start functor */
#define H_FUNCTOR	((code) 7)
#define H_RFUNCTOR	((code) 8)
#define I_POPF		((code) 9)		/* end functor */
#define B_VAR		((code)10)		/* variable */
#define H_VAR		((code)11)
#define B_CONST		((code)12)		/* constant (atomic) */
#define H_CONST		((code)13)
#define H_INDIRECT	((code)14)		/* indirect in the head */
#define B_INTEGER	((code)15)		/* bignum in the head */
#define H_INTEGER	((code)16)		/* bignum in the body */
#define B_FLOAT		((code)17)		/* double in the head */
#define H_FLOAT		((code)18)		/* double in the body */

#define B_FIRSTVAR	((code)19)		/* first occurrence of var */
#define H_FIRSTVAR	((code)20)
#define B_VOID		((code)21)		/* anonimous variables */
#define H_VOID		((code)22)
#define B_ARGFIRSTVAR	((code)23)		/* body vars nested in funct */
#define B_ARGVAR	((code)24)

#define H_NIL		((code)25)		/* [] in the head */
#define B_NIL		((code)26)		/* [] in the body */
#define H_LIST		((code)27)		/* ./2 in the head */
#define H_RLIST		((code)28)		/* right-recursive list */
#define B_LIST		((code)29)		/* ./2 in the body */
#define B_RLIST		((code)30)		/* right-recursive list */

#define B_VAR0		((code)31)		/* B_VAR 0 */
#define B_VAR1		((code)32)		/* B_VAR 1 */
#define B_VAR2		((code)33)		/* B_VAR 2 */

#define I_USERCALL0	((code)34)		/* variable in body (call/1) */
#define I_USERCALLN	((code)35)		/* call/[2...] */
#define I_CUT		((code)36)		/* ! */
#define I_APPLY		((code)37)		/* apply/2 */

#if O_COMPILE_ARITH
#define A_ENTER		((code)38)		/* start arithmetic sequence */
#define A_INTEGER	((code)39)		/* 32-bit signed int */
#define A_DOUBLE	((code)40)		/* 64-bit double */
#define A_VAR0		((code)41)		/* variable-0 */
#define A_VAR1		((code)42)		/* variable-1 */
#define A_VAR2		((code)43)		/* variable-2 */
#define A_VAR		((code)44)		/* variable-n */
#define A_FUNC0		((code)45)		/* nullary arithmic function */
#define A_FUNC1		((code)46)		/* unary arithmic function */
#define A_FUNC2		((code)47)		/* binary arithmic function */
#define A_FUNC		((code)48)		/* n-ary arithmic function */
#define A_LT		((code)49)		/* < */
#define A_GT		((code)50)		/* > */
#define A_LE		((code)51)		/* =< */
#define A_GE		((code)52)		/* >= */
#define A_EQ		((code)53)		/* =:= */
#define A_NE		((code)54)		/* =\= */
#define A_IS		((code)55)		/* is */
#endif /* O_COMPILE_ARITH */

#if O_COMPILE_OR
#define C_OR		((code)56)		/* In-clause backtract point */
#define C_JMP		((code)57)		/* Jump over code */
#define C_MARK		((code)58)		/* Sub-clause cut mark */
#define C_CUT		((code)59)		/* cut to corresponding mark */
#define C_IFTHENELSE	((code)60)		/* if-then-else start */
#define C_VAR		((code)61)		/* make a variable */
#define C_END		((code)62)		/* dummy to help decompiler */
#define C_NOT		((code)63)		/* same as C_IFTHENELSE */
#define C_FAIL		((code)64)		/* fail */
#endif /* O_COMPILE_OR */

#define B_INDIRECT	((code)65)		/* INDIRECT in body */

#if O_BLOCK
#define I_CUT_BLOCK	((code)66)		/* !(block) */
#define B_EXIT		((code)67)		/* exit(block, rval) */
#endif /*O_BLOCK*/

#if O_INLINE_FOREIGNS
#define I_CALL_FV0	((code)68)		/* call foreign, no args */
#define I_CALL_FV1	((code)69)		/* call foreign, 1 var arg */
#define I_CALL_FV2	((code)70)		/* call foreign, 2 var args */
#endif /*O_INLINE_FOREIGNS*/

#define I_FAIL		((code)71)		/* fail */
#define I_TRUE		((code)72)		/* true */

#ifdef O_SOFTCUT
#define C_SOFTIF	((code)73)		/* Start A *-> B ; C */
#define C_SOFTCUT	((code)74)		/* `Cut' of A *-> B ; C */
#endif /*O_SOFTCUT*/

#define I_EXITFACT	((code)75)		/* exit from a fact */
#define D_BREAK		((code)76)		/* Debugger break-point */

#if O_CATCHTHROW
#define I_CATCH		((code)77)		/* $catch (catch/3) */
#define B_THROW		((code)78)		/* throw(Exception) */
#endif

#define I_CONTEXT	((code)79)		/* Push context module */
#define C_LCUT		((code)80)		/* ! local in \+ and -> */
#define I_CALLCLEANUP	((code)81)		/* $call_cleanup */
#define I_EXITCLEANUP	((code)82)		/* $exit_cleanup */

#define I_HIGHEST	((code)82)		/* largest WAM code !!! */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Arithmetic comparison
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define LT 1
#define GT 2
#define LE 3
#define GE 4
#define NE 5
#define EQ 6

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operator types.  NOTE: if you change OP_*, check operatorTypeToAtom()!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define OP_PREFIX  0
#define OP_INFIX   1
#define OP_POSTFIX 2
#define OP_MASK    0xf

#define	OP_FX	(0x10|OP_PREFIX)
#define OP_FY	(0x20|OP_PREFIX)
#define OP_XF	(0x30|OP_POSTFIX)
#define OP_YF	(0x40|OP_POSTFIX)
#define OP_XFX	(0x50|OP_INFIX)
#define OP_XFY	(0x60|OP_INFIX)
#define OP_YFX	(0x70|OP_INFIX)
#define	OP_YFY	(0x80|OP_INFIX)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Type fields.  These codes are  included  in  a  number  of  the  runtime
structures  at  a  fixed  point, so the runtime environment can tell the
difference.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define HeapMagic(n)	((n) | 0x25678000)
#define PROCEDURE_TYPE	HeapMagic(1)	/* a procedure */
#define RECORD_TYPE	HeapMagic(2)	/* a record list */
#define StackMagic(n)	((n) | 0x98765000)
#define QID_MAGIC	StackMagic(1)	/* Query frame */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			  PROLOG DATA REPRESENTATION

Prolog data objects live on various places:

	- In the variable and argument slots of environment frames.
	- As arguments to complex terms on the global stack.
	- In records (recorda/recorded database) in the heap.
	- In variables in foreign language functions.

All Prolog data is packed into a `word'.  A word is  a  32  bit  entity.
The top 3 bits are used to indicate the type; the bottom 2 bits are used
for  the  garbage  collector.   The  bits  for the garbage collector are
always 0 during normal execution.  This implies we do not have  to  care
about  them  for  pointers  and  as  pointers  always  point  to 4 bytes
entities, the range is not harmed by the garbage collection bits.

The remaining 27 bits can hold a  unique  representation  of  the  value
itself  or  can be a pointer to the global stack where the real value is
stored.  We call the latter type of data `indirect'.

Below is a description of the  representation  used  for  each  type  of
Prolog data:

INTEGER
    Integers are stored in the  27  remaining  bits  of  a  word.   This
    implies they are limited to +- 2^26.
REAL
    For a real, the 27 bits are a pointer to a 8 byte unit on the global
    stack.  For both words of the 8 byte unit, the top 3  and  bottom  2
    bits  are  reserved  for identification and garbage collection.  The
    remaining bits hold the exponent and mantisse.  See pack_real()  and
    unpack_real() in pl-alloc.c for details.
ATOM
    For atoms, the 27 bits represent a pointer  to  an  atom  structure.
    Atom  structures are cells of a hash table.  Equality of the pointer
    implies equality of the atoms and visa versa.  Atom  structures  are
    not  collected by the garbage collector and thus live for the entire
    Prolog session.
STRING
    For a string, the 27 bits are a pointer to the  global  stack.   The
    first  word  of  the  string  again reserves  the top 3 and bottom 2
    bits.  The remaining bits indicate the lenght of the  string.   Next
    follows a 0 terminated character string.  Finally a word exactly the
    same  as the header word, to allow the garbage collector to traverse
    the stack downwards and identify the string.
TERM
    For a compound term, the 27 bits are a pointer to the global  stack.
    the  first  word there is a pointer to a functordef structure, which
    determines the name and arity of the  term.   functordef  structures
    are  cells  of  a hash table like atom structures.  They to live for
    the entire Prolog session.  Next, there are just as  many  words  as
    the  arity  of the term, each word representing a normal Prolog data
    object.
VARIABLES
    An unbound variable is represented by NULL.
REFERENCES
    References are the result of sharing variables.   If  two  variables
    must  share,  the one with the shortest livetime is made a reference
    pointer to the other.  This way a tree of reference pointers can  be
    constructed.   The root of the tree is the variable with the longest
    livetime.  To bind the entire tree of variables this root is  bound.
    The  others remain reference pointers.  This implies that ANY prolog
    data object might be a reference  pointer  to  another  Prolog  data
    object,  holding  the  real  value.  To find the real value, a macro
    called deRef() is available.

    The direction of reference pointers is critical.  It MUST  point  in
    the direction of the longest living variable.  If not, the reference
    pointer  will  point  into  the  dark  if  the other end dies.  This
    implies that if both cells are part of an environment frame, the one
    in the child function (closest to the top of the stack)  must  point
    to  the  one in the parent function.  If one is on the local and one
    on the global stack, the  pointer  must  point  towards  the  global
    stack.   Inside  the global stack it is irrelevant.  If backtracking
    destroys a variable, it also will reset the reference towards it  if
    there is one.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include "pl-data.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Common Prolog objects typedefs.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef unsigned long		term_t;		/* external term-reference */
typedef unsigned long		word;		/* Anonymous 4 byte object */
typedef word *			Word;		/* a pointer to anything */
typedef word			atom_t;		/* encoded atom */
typedef unsigned long		code;		/* bytes codes */
typedef code *			Code;		/* pointer to byte codes */
typedef int			Char;		/* char that can pass EOF */
typedef word			(*Func)();	/* foreign functions */

typedef struct atom *		Atom;		/* atom */
typedef struct functor *	Functor;	/* complex term */
typedef struct functorDef *	FunctorDef;	/* name/arity pair */
typedef struct procedure *	Procedure;	/* predicate */
typedef struct definition *	Definition;	/* predicate definition */
typedef struct definition_chain *DefinitionChain; /* linked list of defs */
typedef struct clause *		Clause;		/* compiled clause */
typedef struct clause_ref *	ClauseRef;      /* reference to a clause */
typedef struct clause_index *	ClauseIndex;    /* Clause indexing table */
typedef struct clause_chain *	ClauseChain;    /* Chain of clauses in table */
typedef struct operator *	Operator;	/* see pl-op.c, pl-read.c */
typedef struct record *		Record;		/* recorda/3, etc. */
typedef struct recordRef *	RecordRef;      /* reference to a record */
typedef struct recordList *	RecordList;	/* list of these */
typedef struct module *		Module;		/* predicate modules */
typedef struct sourceFile *	SourceFile;	/* file adminitration */
typedef struct list_cell *	ListCell;	/* Anonymous list */
typedef struct table *		Table;		/* (numeric) hash table */
typedef struct symbol *		Symbol;		/* symbol of hash table */
typedef struct table_enum *	TableEnum; 	/* Enumerate table entries */
typedef struct localFrame *	LocalFrame;	/* environment frame */
typedef struct choice *		Choice;		/* Choice-point */
typedef struct queryFrame *	QueryFrame;     /* toplevel query frame */
typedef struct fliFrame *	FliFrame; 	/* FLI interface frame */
typedef struct trail_entry *	TrailEntry;	/* Entry of trail stack */
typedef struct gc_trail_entry *	GCTrailEntry;	/* Entry of trail stack (GC) */
typedef struct data_mark	mark;		/* backtrack mark */
typedef struct index *		Index;		/* clause indexing */
typedef struct stack *		Stack;		/* machine stack */
typedef struct arithFunction * 	ArithFunction;  /* arithmetic function */
typedef struct _varDef *	VarDef;		/* pl-comp.c */
typedef struct extension_cell *	ExtensionCell;  /* pl-ext.c */
typedef struct abort_handle *	AbortHandle;	/* PL_abort_hook() */
typedef struct initialise_handle * InitialiseHandle;
typedef struct tempfile *	TempFile; 	/* pl-os.c */
typedef struct canonical_dir *	CanonicalDir;	/* pl-os.c */
typedef struct on_halt *	OnHalt;		/* pl-os.c */
typedef struct find_data_tag *	FindData; 	/* pl-trace.c */
typedef struct feature *	Feature; 	/* pl-prims.c */


		 /*******************************
		 *	    ARITHMETIC		*
		 *******************************/

typedef struct
{ int	type;				/* type of number */
  union { real  f;			/* value as real */
	  long  i;			/* value as integer */
	  word  w[WORDS_PER_DOUBLE];	/* for packing/unpacking the double */
	} value;
} number, *Number;

#define V_INTEGER	0		/* integer (long) value */
#define V_REAL		1		/* just a real */
#define V_EXPLICIT_REAL 3		/* explicely specified real */

#define intNumber(n)	((n)->type == V_INTEGER)
#define floatNumber(n)	((n)->type & V_REAL)

		 /*******************************
		 *	   GET-PROCEDURE	*
		 *******************************/

#define GP_FIND		0		/* find anywhere */
#define GP_FINDHERE	1		/* find in this module */
#define GP_CREATE	2		/* create (in this module) */
#define GP_DEFINE	4		/* define a procedure */
#define GP_RESOLVE	5		/* find defenition */

#define GP_HOW_MASK	0x0ff
#define GP_NAMEARITY	0x100		/* or'ed mask */
#define GP_HIDESYSTEM	0x200		/* hide system module */
#define GP_QUIET	0x400		/* don't throw errors */


		 /*******************************
		 *	     CLEANUP		*
		 *******************************/

typedef enum
{ CLN_NORMAL = 0,			/* Normal mode */
  CLN_ACTIVE,				/* Started cleanup */
  CLN_FOREIGN,				/* Foreign hooks */
  CLN_PROLOG,				/* Prolog hooks */
  CLN_SHARED,				/* Unload shared objects */
  CLN_DATA				/* Remaining data */
} cleanup_status;


		 /*******************************
		 *	      FLAGS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Many of the structures have a large number of booleans  associated  with
them.   Early  versions defined these using `unsigned <name> : 1' in the
structure definition.  When I ported SWI-Prolog to a  machine  that  did
not  understand  this  construct  I  decided  to pack all the flags in a
short.  As this allows us to set, clear and test combinations  of  flags
with one operation, it turns out to be faster as well.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define true(s, a)		((s)->flags & (a))
#define false(s, a)		(!true((s), (a)))
#define set(s, a)		((s)->flags |= (a))
#define clear(s, a)		((s)->flags &= ~(a))
#define clearFlags(s)		((s)->flags = 0)

#define NONDETERMINISTIC	(0x00000001L) /* predicate */
#define DISCONTIGUOUS		(0x00000002L) /* predicate */
#define DYNAMIC			(0x00000004L) /* predicate */
#define FOREIGN			(0x00000008L) /* predicate */
#define HIDE_CHILDS		(0x00000010L) /* predicate */
#define MULTIFILE		(0x00000020L) /* predicate */
#define PROFILE_TICKED		(0x00000040L) /* predicate */
#define SPY_ME			(0x00000080L) /* predicate */
#define SYSTEM			(0x00000100L) /* predicate, module */
#define TRACE_ME		(0x00000200L) /* predicate */
#define METAPRED		(0x00000400L) /* predicate */
#define GC_SAFE			(0x00000800L) /* predicate */
#define TRACE_CALL		(0x00001000L) /* predicate */
#define TRACE_REDO		(0x00002000L) /* predicate */
#define TRACE_EXIT		(0x00004000L) /* predicate */
#define TRACE_FAIL		(0x00008000L) /* predicate */
					/* This may be changed later ... */
#define LOCKED			(SYSTEM)      /* predicate */
#define FILE_ASSIGNED		(0x00010000L) /* predicate */
#define VOLATILE		(0x00020000L) /* predicate */
#define AUTOINDEX		(0x00040000L) /* predicate */
#define NEEDSCLAUSEGC		(0x00080000L) /* predicate */
#define NEEDSREHASH		(0x00100000L) /* predicate */
#define P_VARARG		(0x00200000L) /* predicate */
#define P_SHARED		(0x00400000L) /* predicate */
#define P_REDEFINED		(0x00800000L) /* predicate */
#define PROC_DEFINED		(DYNAMIC|FOREIGN|MULTIFILE|DISCONTIGUOUS)

#define ERASED			(0x0001) /* clause, record */
#define UNIT_CLAUSE		(0x0002) /* clause */
#define HAS_BREAKPOINTS		(0x0004) /* clause */
#define GOAL_CLAUSE		(0x0008) /* clause */

#define CHARESCAPE		(0x0004) /* module */
#define DBLQ_CHARS		(0x0008) /* "ab" --> ['a', 'b'] */
#define DBLQ_ATOM		(0x0010) /* "ab" --> 'ab' */
#define DBLQ_STRING		(0x0020) /* "ab" --> "ab" */
#define DBLQ_MASK 		(DBLQ_CHARS|DBLQ_ATOM|DBLQ_STRING)
#define MODULE_COPY_FLAGS	(DBLQ_MASK|CHARESCAPE)
#define UNKNOWN_ERROR		(0x0040) /* module */
#define UNKNOWN_WARNING		(0x0080) /* module */

#define INLINE_F		(0x0001) /* functor (inline foreign) */
#define CONTROL_F		(0x0002) /* functor (compiled controlstruct) */
#define ARITH_F			(0x0004) /* functor (arithmetic operator) */

#define R_DIRTY			(0x0001) /* recordlist */
#define R_EXTERNAL		(0x0002) /* record: inline atoms */
#define R_DUPLICATE		(0x0004) /* record: include references */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Handling environment (or local stack) frames.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define FR_BITS			5	/* mask-bits */
#define FR_NODEBUG		(0x01L)	/* Invisible frame */
#define FR_SKIPPED		(0x02L)	/* We have skipped on this frame */
#define FR_MARKED		(0x04L)	/* GC */
#define FR_WATCHED		(0x08L)	/* GUI debugger */
#define FR_CATCHED		(0x10L)	/* Frame catched an exception */

#define FR_LEVEL		((1L<<FR_BITS)-1)

#define ARGOFFSET		((int)sizeof(struct localFrame))
#define VAROFFSET(var) 		((var)+(ARGOFFSET/(int)sizeof(word)))

#define setLevelFrame(fr, l)	{ (fr)->flags &= ~FR_LEVEL;   \
				  (fr)->flags |= ((l) << FR_BITS); \
				}
#define levelFrame(fr)		(fr->flags >> FR_BITS)
#define incLevel(fr)		(fr->flags += (1<<FR_BITS))
#define argFrameP(f, n)		((Word)((f)+1) + (n))
#define argFrame(f, n)		(*argFrameP((f), (n)) )
#define varFrameP(f, n)		((Word)(f) + (n))
#define varFrame(f, n)		(*varFrameP((f), (n)) )
#define refFliP(f, n)		((Word)((f)+1) + (n))
#define parentFrame(f)		((f)->parent ? (f)->parent\
					     : (LocalFrame)varFrame((f), -1))
#define slotsFrame(f)		(true((f)->predicate, FOREIGN) ? \
				      (f)->predicate->functor->arity : \
				      (f)->clause->clause->prolog_vars)
#define contextModule(f)	((f)->context)
#ifdef O_LOGICAL_UPDATE
#define generationFrame(f)	((f)->generation)
#else
#define generationFrame(f)	(0)
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Predicate reference counting. The aim  of   this  mechanism  is to avoid
modifying the predicate structure while  it   has  choicepoints  or (MT)
other threads running the predicate. For dynamic  code we allow to clean
the predicate as the reference-count drops to   zero. For static code we
introduce a garbage collector (TBD).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define enterDefinitionNOLOCK(def) \
	if ( true(def, DYNAMIC) ) (def->references++)
#define enterDefinition(def) \
	if ( true(def, DYNAMIC) ) \
	{ PL_LOCK(L_PREDICATE); \
	  def->references++; \
	  PL_UNLOCK(L_PREDICATE); \
	}
#define leaveDefinitionNOLOCK(def) \
	if ( true(def, DYNAMIC) ) \
	{ if ( --def->references == 0 && \
	       true(def, NEEDSCLAUSEGC|NEEDSREHASH) ) \
	    gcClausesDefinition(def); \
	  DEBUG(0, assert(def->references >= 0)); \
	}
#define leaveDefinition(def) \
	if ( true(def, DYNAMIC) ) \
	{ PL_LOCK(L_PREDICATE); \
	  if ( --def->references == 0 && \
	       true(def, NEEDSCLAUSEGC|NEEDSREHASH) ) \
	    gcClausesDefinition(def); \
	  PL_UNLOCK(L_PREDICATE); \
	}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Heuristics functions to determine whether an integer reference passed to
erase and assert/2, clause/3, etc.  really points to a clause or record.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define inCore(a)	((char *)(a) >= hBase && (char *)(a) <= hTop)
#define isProcedure(w)	(((Procedure)(w))->type == PROCEDURE_TYPE)
#define isRecordList(w)	(((RecordList)(w))->type == RECORD_TYPE)
#define isClause(c)	((inCore(c) || onStackArea(local, (c))) && \
			 inCore(((Clause)(c))->procedure) && \
			 isProcedure(((Clause)(c))->procedure))
#define isRecordRef(r)	(inCore(((RecordRef)(r))->list) && \
			 isRecordList(((RecordRef)(r))->list))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
At times an abort is not allowed because the heap  is  inconsistent  the
programmer  should  call  startCritical  to start such a code region and
endCritical to end it.

MT/TBD: how to handle this gracefully in the multi-threading case.  Does
it mean anything?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_PLMT
#define startCritical (void)(GD->critical++)
#define endCritical   (void)(GD->critical--)
#else
#define startCritical (void)(GD->critical++)
#define endCritical   if ( --(GD->critical) == 0 && LD->aborted ) \
			pl_abort(ABORT_NORMAL)
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
LIST processing macros.

    isNil(w)		word is the nil list ([]).
    isList(w)		word is a './2' term.
    HeadList(p)		Pointer to the head of list *p (NOT dereferenced).
    TailList(p)		Pointer to the tail of list *p (NOT dereferenced).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define HeadList(p)	(argTermP(*(p), 0) )
#define TailList(p)	(argTermP(*(p), 1) )

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Doubles. To and from are Word pointers pointing to the data of a double,
but generally not  satisfying  the   double  alignment  requirements. We
assume

  sizeof(*to) == sizeof(*from) &&
  sizeof(*to) * n == sizeof(*double)
  	with n == 1 or n == 2.

We assume the compiler will optimise this properly. 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define cpDoubleData(to, from) \
	{ Word _f = (Word)(from); \
	  switch(WORDS_PER_DOUBLE) \
	  { case 2: \
	      *(to)++ = *_f++; \
	    case 1: \
	      *(to)++ = *_f++; \
	      from = (void *)_f; \
	      break; \
	    default: \
	      assert(0); \
	  } \
	}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Structure declarations that must be shared across multiple files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

struct atom
{ Atom		next;		/* next in chain */
  word		atom;		/* as appearing on the global stack */
#ifdef O_HASHTERM
  int		hash_value;	/* hash-key value */
#endif
#ifdef O_ATOMGC
  unsigned int	references;	/* reference-count */
#endif
  unsigned int  length;		/* length of the atom */
  char *	name;		/* name associated with atom */
};

#ifdef O_ATOMGC
#define ATOM_MARKED_REFERENCE ((unsigned int)1 << (INTBITSIZE-1))
#ifdef O_DEBUG_ATOMGC
#define PL_register_atom(a) \
	_PL_debug_register_atom(a, __FILE__, __LINE__, __PRETTY_FUNCTION__)
#define PL_unregister_atom(a) \
	_PL_debug_unregister_atom(a, __FILE__, __LINE__, __PRETTY_FUNCTION__)
#undef atomValue
#define atomValue(a) _PL_debug_atom_value(a)
extern Atom _PL_debug_atom_value(atom_t a);
#endif
#else
#define PL_register_atom(a)
#define PL_unregister_atom(a)
#endif

struct index
{ unsigned long key;		/* key of index */
  unsigned long varmask;	/* variable field mask */
};

struct functorDef
{ FunctorDef	next;		/* next in chain */
  word		functor;	/* as appearing on the global stack */
  word		name;		/* Name of functor */
  int		arity;		/* arity of functor */
  unsigned      flags;		/* Flag field holding: */
		/* INLINE_F	   Inlined foreign (system) predicate */
};


#ifdef O_LOGICAL_UPDATE
#define visibleClause(cl, gen) \
	((cl)->generation.created <= (gen) && \
	 (cl)->generation.erased   > (gen))
#else
#define visibleClause(cl, gen) false(cl, ERASED)
#endif


#define sizeofClause(n) ((int)&((Clause)NULL)->codes[n])

struct clause
{ Procedure	procedure;		/* procedure we belong to */
  struct index	index;			/* index key of clause */
#ifdef O_LOGICAL_UPDATE
  struct
  { unsigned long created;		/* Generation that created me */
    unsigned long erased;		/* Generation I was erased */
  } generation;
#endif /*O_LOGICAL_UPDATE*/
  unsigned short	code_size;	/* size of ->codes */
  unsigned short	variables;	/* # of variables for frame */
  unsigned short	prolog_vars;	/* # real Prolog variables */
#ifdef O_SHIFT_STACKS
  unsigned short	marks;		/* C_MARK reserved */
#endif
  unsigned short	line_no;	/* Source line-number */
  unsigned short	source_no;	/* Index of source-file */
  unsigned short	flags;		/* Flag field holding: */
		/* ERASED	   Clause is retracted, but referenced */
		/* UNIT_CLAUSE     Clause has no body */
		/* HAS_BREAKPOINTS Break-instructions in the clause */
		/* GOAL_CLAUSE	   Temporary 'islocal' clause (no head) */
  code		codes[1];		/* VM codes of clause */
};

struct clause_ref
{ Clause	clause;
  ClauseRef	next;
};

#define CA1_PROC	1	/* code arg 1 is procedure */
#define CA1_FUNC	2	/* code arg 1 is functor */
#define CA1_DATA	3	/* code arg 2 is prolog data */
#define CA1_INTEGER	4	/* long value */
#define CA1_FLOAT	5	/* next WORDS_PER_DOUBLE are double */
#define CA1_STRING	6	/* inlined string */
#define CA1_MODULE	7	/* a module */
#define CA1_VAR		8	/* a variable(-offset) */

typedef struct
{ char		*name;		/* name of the code */
  code		code;		/* number of the code */
  char		arguments;	/* # arguments code takes */
  char		argtype;	/* # `external' arguments code takes */
} code_info;

struct data_mark
{ TrailEntry	trailtop;	/* top of the trail stack */
  Word		globaltop;	/* top of the global stack */
};

struct functor
{ word		definition;	/* Tagged definition pointer */
  word		arguments[1];	/* arguments vector */
};

struct procedure
{ Definition	definition;	/* definition of procedure */
  int		type;		/* PROCEDURE_TYPE */
};

struct clause_index
{ int		buckets;		/* # entries */
  int		size;			/* # elements (clauses) */
  int		alldirty;		/* all chains need checked */
  ClauseChain 	entries;		/* chains holding the clauses */
};

struct clause_chain
{ ClauseRef	head;
  ClauseRef	tail;
  int		dirty;			/* # of garbage clauses */
};

struct definition
{ FunctorDef	functor;		/* Name/Arity of procedure */
  union
  { ClauseRef	clauses;		/* clause list of procedure */
    Func	function;		/* function pointer of procedure */
  } definition;
  ClauseRef	lastClause;		/* last clause of list */
  Module	module;			/* module of the predicate */
  int		references;		/* reference count */
#ifdef O_PROFILE
  int		profile_ticks;		/* profiler: call times active */
  int		profile_calls;		/* profiler: number of calls */
  int		profile_redos;		/* profiler: number of redos */
  int		profile_fails;		/* profiler: number of fails */
#endif /* O_PROFILE */
  unsigned int  erased_clauses;		/* #erased but not reclaimed clauses */
  ClauseIndex 	hash_info;		/* clause hash-tables */
  unsigned long indexPattern;		/* indexed argument pattern */
  unsigned long	flags;			/* booleans: */
		/*	FOREIGN		   foreign predicate? */
		/*	PROFILE_TICKED	   has been ticked this time? */
		/*	TRACE_ME	   is my call visible? */
		/*	HIDE_CHILDS	   hide childs for the debugger? */
		/*	SPY_ME		   spy point set? */
		/*	DYNAMIC		   dynamic predicate? */
		/*	MULTIFILE	   defined over more files? */
		/*	SYSTEM		   system predicate */
		/*	METAPRED	   procedure transparent to modules */
		/*	DISCONTIGUOUS	   procedure might be discontiguous */
		/*	NONDETERMINISTIC   deterministic foreign (not used) */
		/*	GC_SAFE		   Save to perform GC while active */
		/*	TRACE_CALL	   Trace call-port */
		/*	TRACE_REDO	   Trace redo-port */
		/*	TRACE_EXIT	   Trace exit-port */
		/*	TRACE_FAIL	   Trace fail-port */
		/*	VOLATILE	   Don't save my clauses */
		/*	AUTOINDEX	   Automatically guess index */
		/*	NEEDSCLAUSEGC	   Clauses have been erased */
		/*	NEEDSREHASH	   Hash-table is out-of-date */
		/*	P_VARARG	   Foreign called using t0, ac, ctx */
  		/*	P_SHARED	   Multiple procs are using me */
  unsigned	indexCardinality : 8;	/* cardinality of index pattern */
  unsigned	number_of_clauses : 24;	/* number of associated clauses */
};


struct definition_chain
{ Definition		definition;	/* chain on definition */
  DefinitionChain 	next;		/* next in chain */
};


struct localFrame
{ Code		programPointer;		/* pointer into program */
  LocalFrame	parent;			/* parent local frame */
  ClauseRef	clause;			/* Current clause of frame */
  Definition	predicate;		/* Predicate we are running */
  Module	context;		/* context module of frame */
#ifdef O_LOGICAL_UPDATE
  unsigned long generation;		/* generation of the database */
#endif
  unsigned long flags;			/* packed long holding: */
		/*	LEVEL	   recursion level (28 bits) */
		/*	FR_CUT     has frame been cut ? */
		/*	FR_NODEBUG don't debug this frame ? */
};  


typedef enum
{ CHP_JUMP = 0,				/* A jump due to ; */
  CHP_CLAUSE,				/* Next clause of predicate */
  CHP_FOREIGN,				/* Foreign code choicepoint */
  CHP_TOP,				/* First (toplevel) choice */
  CHP_CATCH,				/* $catch initiated choice */
  CHP_DEBUG,				/* Enable redo */
  CHP_NONE				/* not a choice any-more */
} choice_type;

typedef enum
{ ABORT_NORMAL,				/* normal abort */
  ABORT_FATAL				/* abort on fatal error */
} abort_type;
   
typedef enum
{ DBG_OFF = 0,				/* no debugging */
  DBG_ON,				/* switch on in current environment */
  DBG_ALL				/* switch on globally */
} debug_type;

struct choice
{ choice_type	type;			/* CHP_* */
  Choice	parent;			/* Alternative if I fail */
  mark		mark;			/* data mark for undo */
  LocalFrame 	frame;			/* Frame I am related to */
  union
  { ClauseRef	clause;			/* Next candidate clause */
    Code	PC;			/* Next candidate program counter */
    word        foreign;		/* foreign redo handle */
  } value;
};


#define QF_NODEBUG		0x0001	/* debug-able query */
#define QF_DETERMINISTIC	0x0002	/* deterministic success */
#define	QF_INTERACTIVE		0x0004	/* interactive goal (prolog()) */

struct queryFrame
{ long		magic;			/* Magic code for security */
#if O_SHIFT_STACKS
  struct				/* Interpreter registers */
  { LocalFrame  fr;
    LocalFrame  bfr;
  } registers;
#endif
#ifdef O_LIMIT_DEPTH
  long		saved_depth_limit;	/* saved values of these */
  long		saved_depth_reached;
#endif
#if O_CATCHTHROW
  term_t	exception;		/* Exception term */
  jmp_buf	exception_jmp_env;	/* longjmp() buffer for exception */
#endif
  unsigned long	flags;
  debug_type	debugSave;		/* saved debugstatus.debugging */
  Word	       *aSave;			/* saved argument-stack */
  int		solutions;		/* # of solutions produced */
  Choice	saved_bfr;		/* Saved choice-point */
  struct choice	choice;			/* First (dummy) choice-point */
  LocalFrame	saved_environment;	/* Parent local-frame */
					/* Do not put anything between */
					/* or check parentFrame() */
  struct localFrame frame;		/* The local frame */
};


struct fliFrame
{ int		size;			/* # slots on it */
  FliFrame	parent;			/* parent FLI frame */
  mark		mark;			/* data-stack mark */
};

struct record
{ int		size;			/* # bytes of the record */
  int		nvars;			/* # variables in the term */
  unsigned	gsize : 28;		/* Stack space required (words) */
  unsigned	flags : 4;		/* Flags, holding */
					/* ERASED */
					/* R_EXTERNAL */
					/* R_DUPLICATE */
					/* R_LIST */
  int		references;		/* PL_duplicate_record() support */
  char 		buffer[1];		/* array holding codes */
};

struct recordList
{ int		type;		/* RECORD_TYPE */
  int		references;	/* choicepoints reference count */
  word		key;		/* key of record */
  RecordRef	firstRecord;	/* first record associated with key */
  RecordRef	lastRecord;	/* last record associated with key */
  unsigned int  flags;		/* R_DIRTY */
};

struct recordRef
{ RecordList	list;			/* list I belong to */
  RecordRef	next;			/* next in list */
  Record	record;			/* the record itself */
};

struct sourceFile
{ atom_t	name;		/* name of source file */
  int		count;		/* number of times loaded */
  long		time;		/* load time of file */
  ListCell	procedures;	/* List of associated procedures */
  Procedure	current_procedure;	/* currently loading one */
  int		index;		/* index number (1,2,...) */
  bool		system;		/* system sourcefile: do not reload */
};


struct list_cell
{ void *	value;		/* object in the cell */
  ListCell	next;		/* next in chain */
};


struct module
{ word		name;		/* name of module */
  SourceFile	file;		/* file from which module is loaded */
  Table		procedures;	/* predicates associated with module */
  Table		public;		/* public predicates associated */
  Table		operators;	/* local operator declarations */
  Module	super;		/* Import predicates from here */
#ifdef O_PROLOG_HOOK
  Procedure	hook;		/* Hooked module */
#endif
  unsigned int  flags;		/* booleans: */
		/*	SYSTEM	   system module */
  		/*	DBLQ_INHERIT inherit from default module */
		/*	DBLQ_CHARS "ab" --> ['a', 'b'] */
		/*	DBLQ_ATOM  "ab" --> 'ab' */
		/*	UNKNOWN_WARNING Warn on unknown pred */
		/*	UNKNOWN_ERROR Error on unknown pred */
};

struct trail_entry
{ Word		address;	/* address of the variable */
};

struct gc_trail_entry
{ word		address;	/* address of the variable */
};

struct table
{ int		buckets;	/* size of hash table */
  int		size;		/* # symbols in the table */
  TableEnum	enumerators;	/* Handles for enumeration */
  void 		(*copy_symbol)(Symbol s);
  void 		(*free_symbol)(Symbol s);
  Symbol	*entries;	/* array of hash symbols */
};

struct symbol
{ Symbol	next;		/* next in chain */
  void *	name;		/* name entry of symbol */
  void *	value;		/* associated value with name */
};

struct table_enum
{ Table		table;		/* Table we are working on */
  int		key;		/* Index of current symbol-chain */
  Symbol	current;	/* The current symbol */
  TableEnum	next;		/* More choice points */
};



		 /*******************************
		 *	     MARK/UNDO		*
		 *******************************/

#define setVar(w)		((w) = (word) 0)

#ifdef O_DESTRUCTIVE_ASSIGNMENT

#define Undo(b)			do_undo(&b)

#else /*O_DESTRUCTIVE_ASSIGNMENT*/

#define Undo(b)		{ TrailEntry tt = tTop; \
			  TrailEntry mt = (b).trailtop; \
			  while(tt > mt) \
			  { tt--; \
			    setVar(*tt->address); \
			  } \
			  tTop = tt; \
			  gTop = (b).globaltop; \
			}
#endif /*O_DESTRUCTIVE_ASSIGNMENT*/

#define Mark(b)		{ (b).trailtop  = tTop; \
			  LD->mark_bar = (b).globaltop = gTop; \
			}


		 /*******************************
		 *	   FLI INTERNALS	*
		 *******************************/

typedef unsigned long qid_t;		/* external query-id */
typedef unsigned long PL_fid_t;		/* external foreign context-id */

#define fid_t PL_fid_t			/* avoid AIX name-clash */

#define consTermRef(p)	 ((Word)(p) - (Word)(lBase))
#define valTermRef(r)	 (&((Word)(lBase))[r])
#define resetTermRefs(r) (lTop = (LocalFrame) valTermRef(r))

#if O_SHIFT_STACKS
#define SaveLocalPtr(s, ptr)	term_t s = consTermRef(ptr)
#define RestoreLocalPtr(s, ptr) (ptr) = (void *) valTermRef(s)
#else
#define SaveLocalPtr(s, ptr)	
#define RestoreLocalPtr(s, ptr) 
#endif

#define QueryFromQid(qid)	((QueryFrame) valTermRef(qid))
#define QidFromQuery(f)		(consTermRef(f))
#define QID_EXPORT_WAM_TABLE	(qid_t)(-1)

#include "pl-itf.h"

		 /*******************************
		 *	       SIGNALS		*
		 *******************************/

#if HAVE_SIGNAL
#define MAXSIGNAL		32	/* highest system signal number */

typedef RETSIGTYPE (*handler_t)(int);
typedef void *SignalContext;		/* struct sigcontext on sun */

typedef struct
{ handler_t   saved_handler;		/* Original handler */
  handler_t   handler;			/* User signal handler */
  predicate_t predicate;		/* Prolog handler */
  int	      flags;			/* PLSIG_*, defined in pl-setup.c */
} sig_handler, *SigHandler;
#endif /* HAVE_SIGNAL */

#define SIG_EXCEPTION	   9		/* cannot be catched anyway */
#define SIG_ATOM_GC	  30		/* `safe' and reserved signals */
#define SIG_THREAD_SIGNAL 31

		 /*******************************
		 *	   OPTION LISTS		*
		 *******************************/

#define OPT_BOOL	(0)		/* types */
#define OPT_INT		(1)
#define OPT_STRING	(2)
#define OPT_ATOM	(3)
#define OPT_TERM	(4)		/* arbitrary term */
#define OPT_LONG	(5)

#define OPT_ALL		0x1		/* flags */

typedef struct
{ word		name;			/* Name of option */
  int		type;			/* Type of option */
} opt_spec, *OptSpec;

		 /*******************************
		 *	      EVENTS		*
		 *******************************/

#define PLEV_ERASED	   0 		/* clause or record was erased */
#define PLEV_DEBUGGING	   1		/* changed debugging mode */
#define PLEV_TRACING	   2		/* changed tracing mode */
#define PLEV_SPY	   3		/* changed spypoint */
#define PLEV_BREAK	   4		/* a break-point was set */
#define PLEV_NOBREAK	   5		/* a break-point was cleared */
#define PLEV_FRAMEFINISHED 6		/* A watched frame was discarded */


		/********************************
		*             STACKS            *
		*********************************/

#ifdef small				/* defined by MSVC++ 2.0 windows.h */
#undef small
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If we have access to the virtual   memory management of the machine, use
this to enlarge the runtime stacks.  Otherwise use the stack-shifter.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define GC_FAST_POLICY 0x1		/* not really used yet */

#define STACK(type) \
	{ type		base;		/* base address of the stack */     \
	  type		top;		/* current top of the stack */      \
	  type		min;		/* donot shrink below this value */ \
	  type		max;		/* allocated maximum */		    \
	  type		limit;		/* top the the range (base+limit) */\
	  long		minfree;	/* minimum amount of free space */  \
	  bool		gc;		/* Can be GC'ed? */		    \
	  long		gced_size;	/* size after last GC */	    \
	  long		small;		/* Donot GC below this size */	    \
	  int		factor;		/* How eager we are */		    \
	  int		policy;		/* Time, memory optimization */	    \
	  char		*name;		/* Symbolic name of the stack */    \
	}

struct stack STACK(caddress);		/* Anonymous stack */

#define N_STACKS (4)

typedef struct
{ struct STACK(LocalFrame) local;	/* local (environment) stack */
  struct STACK(Word)	   global;	/* local (environment) stack */
  struct STACK(TrailEntry) trail;	/* trail stack */
  struct STACK(Word *)	   argument;	/* argument stack */
} pl_stacks_t;

#define tBase	(LD->stacks.trail.base)
#define tTop	(LD->stacks.trail.top)
#define tMax	(LD->stacks.trail.max)

#define lBase	(LD->stacks.local.base)
#define lTop	(LD->stacks.local.top)
#define lMax	(LD->stacks.local.max)
#define lLimit	(LD->stacks.local.limit)

#define gBase	(LD->stacks.global.base)
#define gTop	(LD->stacks.global.top)
#define gMax	(LD->stacks.global.max)
#define gLimit	(LD->stacks.global.limit)

#define aBase	(LD->stacks.argument.base)
#define aTop	(LD->stacks.argument.top)
#define aMax	(LD->stacks.argument.max)
#define aLimit	(LD->stacks.argument.limit)

#define SetHTop(val)	{ if ( (char *)(val) > hTop  ) hTop  = (char *)(val); }
#define SetHBase(val)	{ if ( (char *)(val) < hBase ) hBase = (char *)(val); }

#define onStack(name, addr) \
	((char *)(addr) >= (char *)LD->stacks.name.base && \
	 (char *)(addr) <  (char *)LD->stacks.name.top)
#ifdef O_SHIFT_STACKS
#define onStackArea(name, addr) \
	((char *)(addr) >= (char *)LD->stacks.name.base && \
	 (char *)(addr) <  (char *)LD->stacks.name.max)
#else
#define onStackArea(name, addr) \
	((char *)(addr) >= (char *)LD->stacks.name.base && \
	 (char *)(addr) <  (char *)LD->stacks.name.limit)
#endif
#define usedStack(name) ((char *)LD->stacks.name.top - \
			 (char *)LD->stacks.name.base)
#define sizeStack(name) ((char *)LD->stacks.name.max - \
			 (char *)LD->stacks.name.base)
#define roomStack(name) ((char *)LD->stacks.name.max - \
			 (char *)LD->stacks.name.top)
#define spaceStack(name) ((char *)LD->stacks.name.limit - \
			 (char *)LD->stacks.name.top)
#define limitStack(name) ((char *)LD->stacks.name.limit - \
			 (char *)LD->stacks.name.base)
#define narrowStack(name) (roomStack(name) < LD->stacks.name.minfree)

typedef enum
{ STACK_OVERFLOW_SIGNAL,
  STACK_OVERFLOW_RAISE,
  STACK_OVERFLOW_THROW,
  STACK_OVERFLOW_FATAL
} stack_overflow_action;

#if O_DYNAMIC_STACKS
#ifdef NO_SEGV_HANDLING
#define requireStack(s, n) \
	{ if ( roomStack(s) < (int)(n) ) \
 	    ensureRoomStack((Stack)&LD->stacks.s, n); \
	}
#else /*NO_SEGV_HANDLING*/
#define requireStack(s, n)
#endif /*NO_SEGV_HANDLING*/
#else
#define requireStack(s, n) \
	{ if ( roomStack(s) < (n) ) \
 	    outOfStack((Stack)&LD->stacks.s, STACK_OVERFLOW_FATAL); \
	}
#endif


		/********************************
		*       READ WARNINGS           *
		*********************************/

#define ReadingSource (source_line_no > 0 && \
		       source_file_name != NULL_ATOM)


		/********************************
		*        FAST DISPATCHING	*
		********************************/

#if VMCODE_IS_ADDRESS
#define encode(wam) (wam_table[wam])		/* WAM --> internal */
						/* internal --> WAM */
#define decode(c)   ((code) (dewam_table[(unsigned long)(c) - \
					 dewam_table_offset]))
#else /* VMCODE_IS_ADDRESS */
#define encode(wam) (wam)
#define decode(wam) (wam)
#endif /* VMCODE_IS_ADDRESS */

		/********************************
		*            STATUS             *
		*********************************/

typedef struct
{ bool		requested;		/* GC is requested by stack expander */
  int		blocked;		/* GC is blocked now */
  bool		active;			/* Currently running? */
  long		collections;		/* # garbage collections */
  long		global_gained;		/* global stack bytes collected */
  long		trail_gained;		/* trail stack bytes collected */
  real		time;			/* time spent in collections */
} pl_gc_status_t;


typedef struct
{ int		blocked;		/* No shifts allowed */
  real		time;			/* time spent in stack shifts */
  int		local_shifts;		/* Shifts of the local stack */
  int		global_shifts;		/* Shifts of the global stack */
  int		trail_shifts;		/* Shifts of the trail stack */
} pl_shift_status_t;


#define NO_PROFILING		0
#define CUMULATIVE_PROFILING	1
#define PLAIN_PROFILING		2


		/********************************
		*            MODULES            *
		*********************************/

#define MODULE_user	(GD->modules.user)
#define MODULE_system	(GD->modules.system)
#define MODULE_parse	(ReadingSource ? LD->modules.source \
				       : MODULE_user)
#define MODULE_context	(environment_frame ? contextModule(environment_frame) \
					   : MODULE_user)

		/********************************
		*         PREDICATES            *
		*********************************/

#define PROCEDURE_garbage_collect0	(GD->procedures.garbage_collect0)
#define PROCEDURE_block3		(GD->procedures.block3)
#define PROCEDURE_catch3		(GD->procedures.catch3)
#define PROCEDURE_true0			(GD->procedures.true0)
#define PROCEDURE_fail0			(GD->procedures.fail0)
#define PROCEDURE_event_hook1		(GD->procedures.event_hook1)
#define PROCEDURE_print_message2	(GD->procedures.print_message2)
#define PROCEDURE_dcall1		(GD->procedures.dcall1)
#define PROCEDURE_call_cleanup3		(GD->procedures.call_cleanup3)

extern const code_info codeTable[]; /* Instruction info (read-only) */

		/********************************
		*            DEBUGGER           *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Tracer communication declarations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define ACTION_CONTINUE	0
#define ACTION_RETRY	1
#define ACTION_FAIL	2
#define ACTION_IGNORE	3
#define ACTION_AGAIN	4

#define CALL_PORT	0x001		/* port masks */
#define EXIT_PORT	0x002
#define FAIL_PORT	0x004
#define REDO_PORT	0x008
#define UNIFY_PORT	0x010
#define BREAK_PORT	0x020
#define CUT_CALL_PORT   0x040
#define CUT_EXIT_PORT   0x080
#define EXCEPTION_PORT	0x100
#define CUT_PORT	(CUT_CALL_PORT|CUT_EXIT_PORT)
#define PORT_MASK	0x1ff
#define VERY_DEEP	1000000000L	/* deep skiplevel */

#define LONGATOM_CHECK	    0x01	/* read/1: error on long atoms */
#define SINGLETON_CHECK	    0x02	/* read/1: check singleton vars */
#define DOLLAR_STYLE	    0x04	/* dollar is lower case */
#define DISCONTIGUOUS_STYLE 0x08	/* warn on discontiguous predicates */
#define DYNAMIC_STYLE	    0x10	/* warn on assert/retract active */
#define MAXNEWLINES	    5		/* maximum # of newlines in atom */
#define SYSTEM_MODE	    (debugstatus.styleCheck & DOLLAR_STYLE)

typedef struct debuginfo
{ unsigned long	skiplevel;		/* current skip level */
  bool		tracing;		/* are we tracing? */
  debug_type	debugging;		/* are we debugging? */
  int		leashing;		/* ports we are leashing */
  int	        visible;		/* ports that are visible */
  bool		showContext;		/* tracer shows context module */
  int		styleCheck;		/* source style checking */
  int		suspendTrace;		/* tracing is suspended now */
  LocalFrame	retryFrame;		/* Frame to retry */
} pl_debugstatus_t;

#define FT_ATOM		0		/* atom feature */
#define FT_BOOL		1		/* boolean feature (true, false) */
#define FT_INTEGER	2		/* integer feature */
#define FT_TERM		3		/* term feature */
#define FT_MASK		0x0f		/* mask to get type */

#define FF_READONLY	0x10		/* feature is read-only */


#define CHARESCAPE_FEATURE	  0x00001 /* handle \ in atoms */
#define GC_FEATURE		  0x00002 /* do GC */
#define TRACE_GC_FEATURE	  0x00004 /* verbose gc */
#define TTY_CONTROL_FEATURE	  0x00008 /* allow for tty control */
#define READLINE_FEATURE	  0x00010 /* readline is loaded */
#define DEBUG_ON_ERROR_FEATURE	  0x00020 /* start tracer on error */
#define REPORT_ERROR_FEATURE	  0x00040 /* print error message */
#define FILE_CASE_FEATURE	  0x00080 /* file names are case sensitive */
#define FILE_CASE_PRESERVING_FEATURE 0x0100 /* case preserving file names */
#define DOS_FILE_NAMES_FEATURE    0x00200 /* dos (8+3) file names */
#define ALLOW_VARNAME_FUNCTOR	  0x00400 /* Read Foo(x) as 'Foo'(x) */
#define ISO_FEATURE		  0x00800 /* Strict ISO compliance */
#define OPTIMISE_FEATURE	  0x01000 /* -O: optimised compilation */
#define FILEVARS_FEATURE	  0x02000 /* Expand $var and ~ in filename */
#define AUTOLOAD_FEATURE	  0x04000 /* do autoloading */
#define CHARCONVERSION_FEATURE	  0x08000 /* do character-conversion */
#define TAILRECURSION_FEATURE	  0x10000 /* Tail recursion enabled? */
#define EX_ABORT_FEATURE	  0x20000 /* abort with exception */
#define BACKQUOTED_STRING_FEATURE 0x40000 /* `a string` */
#define SIGNALS_FEATURE		  0x80000 /* Handle signals */

typedef struct
{ unsigned long flags;			/* the feature flags */
} pl_features_t;

#define trueFeature(mask)	true(&features, mask)
#define setFeatureMask(mask)	set(&features, mask)
#define clearFeatureMask(mask)	clear(&features, mask)

#ifdef O_LIMIT_DEPTH
#define DEPTH_NO_LIMIT	(~0x0L)		/* Highest value */
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Administration of loaded intermediate code files  (see  pl-wic.c).  Used
with the -c option to include all these if necessary.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct state * State;

struct state
{ char *	name;			/* name of state */
  State		next;			/* next state loaded */
};

#define QLF_TOPLEVEL 0x1		/* toplevel wic file */
#define QLF_OPTIONS  0x2		/* only load options */
#define QLF_EXESTATE 0x4		/* probe qlf exe state */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Sourcelocation information (should be used at more places).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ atom_t	file;			/* name of the file */
  int		line;			/* line number */
} sourceloc, *SourceLoc;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Include debugging info to make it (very) verbose.  SECURE adds  code  to
check  consistency mainly in the WAM interpreter.  Prolog gets VERY slow
if SECURE is  used.   DEBUG  is  not  too  bad  (about  20%  performance
decrease).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define REL(a)		((Word)(a) - (Word)(lBase))

#if O_DEBUG
#define DEBUG(n, g) { if (GD->debug_level >= (n)) { g; } }
#else
#define DEBUG(a, b) 
#endif

#if O_SECURE
#define SECURE(g) {g;}
#else
#define SECURE(g)
#endif

#include "pl-os.h"			/* OS dependencies */

#ifdef SYSLIB_H
#include SYSLIB_H
#endif

#include "pl-main.h"			/* Declarations needed by pl-main.c */
#include "pl-error.h"			/* Exception generation */
#include "pl-global.h"			/* global data */
#include "pl-thread.h"			/* thread manipulation */
#include "pl-funcs.h"			/* global functions */

#ifdef __DECC				/* Dec C-compiler: avoid conflicts */
#undef leave
#undef except
#undef try
#endif

#define NULL_ATOM ((atom_t)0)
#define MK_ATOM(n)    		((n)<<7|TAG_ATOM|STG_STATIC)
#include "pl-atom.ih"
#include "pl-funct.ih"

#endif /*_PL_INCLUDE_H*/
