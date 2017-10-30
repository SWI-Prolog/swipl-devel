/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2017, University of Amsterdam,
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

#ifndef _PL_INCLUDE_H
#define _PL_INCLUDE_H

#define PLNAME "swi"

#ifdef __WINDOWS__
#ifdef WIN64
#include "config/win64.h"
#define PLHOME       "c:/Program Files/swipl"
#else
#include "config/win32.h"
#define PLHOME       "c:/Program Files (x86)/swipl"
#endif
#define DEFSTARTUP   "swipl.ini"
#else
#include <config.h>
#endif

#ifdef _MSC_VER
#define C_LIBS	     ""
#define C_STATICLIBS ""
#define C_CC	     "cl"
#if (_MSC_VER < 1400)
#define C_CFLAGS     "/MD /GX"
#else
#define C_CFLAGS     "/MD /EHsc"
#endif
#define C_LDFLAGS    ""
#if defined(_DEBUG)
#define C_PLLIB	    "swiplD.lib"
#else
#define C_PLLIB	    "swipl.lib"
#endif
#else					/* !_MSC_VER  */
#ifdef __WINDOWS__			/* I.e., MinGW */
#define C_LIBS	     ""
#define C_STATICLIBS ""
#define C_CC	     "gcc"
#define C_CFLAGS     ""
#define C_PLLIB	     "-lswipl"
#define C_LDFLAGS    ""
#endif
#include <parms.h>			/* pick from the working dir */
#endif

#define PL_KERNEL		1
#include <inttypes.h>
#include "pl-builtin.h"

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
  O_QUASIQUOTATIONS
      Support quasi quoted content in read_term/3 and friends.
  O_COMPILE_OR
      Compile ->/2, ;/2 and |/2 into WAM.  This  no  longer  is  a  real
      option.   the mechanism to handle cuts without compiling ;/2, etc.
      has been taken out.
  O_COMPILE_ARITH
      Include arithmetic compiler (compiles is/2, >/2, etc. into WAM).
  O_COMPILE_IS
      Compile Var = Value in the body.
  O_CALL_AT_MODULE
      Support the Goal@Module control-structure
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
      Include support for multi-threading. Too much of the system relies
      on this now, so it cannot be disabled without significant work.
  O_LARGEFILES
      Supports files >2GB on 32-bit systems (if the OS provides it).
  O_ATTVAR
      Include support for attributes variables.
      This option requires O_DESTRUCTIVE_ASSIGNMENT.
  O_GVAR
      Include support for backtrackable global variables.  This option
      requires O_DESTRUCTIVE_ASSIGNMENT.
  O_CYCLIC
      Provide support for cyclic terms.
  O_LOCALE
      Provide locale support on streams.
  O_GMP
      Use GNU gmp library for infinite precision arthmetic
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define O_COMPILE_OR		1
#define O_SOFTCUT		1
#define O_COMPILE_ARITH		1
#define O_COMPILE_IS		1
#define O_CALL_AT_MODULE	1
#define O_STRING		1
#define O_RESERVED_SYMBOLS	1
#define O_QUASIQUOTATIONS	1
#define O_CATCHTHROW		1
#define O_DEBUGGER		1
#define O_INTERRUPT		1
#define O_DESTRUCTIVE_ASSIGNMENT 1
#define O_TERMHASH		1
#define O_LIMIT_DEPTH		1
#define O_INFERENCE_LIMIT	1
#define O_SAFE_SIGNALS		1
#define O_LOGICAL_UPDATE	1
#define O_LOCALE		1
#define O_ATOMGC		1
#define O_CLAUSEGC		1
#define O_ATTVAR		1
#define O_CALL_RESIDUE		1
#define O_GVAR			1
#define O_CYCLIC		1

#if defined(O_PLMT)
#if defined(O_SIGPROF_PROFILE) || defined(__WINDOWS__)
#define O_PROFILE		1
#endif
#endif

#ifdef HAVE_GMP_H
#define O_GMP			1
#endif
#ifdef __WINDOWS__
#define NOTTYCONTROL           TRUE
#define O_DDE 1
#define O_DLL 1
#define O_HASDRIVES 1
#define O_HASSHARES 1
#define O_XOS 1
#define O_RLC 1
#endif

#ifndef DOUBLE_TO_LONG_CAST_RAISES_SIGFPE
#ifdef __i386__
#define DOUBLE_TO_LONG_CAST_RAISES_SIGFPE 1
#endif
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The ia64 says setjmp()/longjmp() buffer must be aligned at 128 bits
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef JMPBUF_ALIGNMENT
#ifdef __ia64__
#define JMPBUF_ALIGNMENT 128
#else
#if ALIGNOF_DOUBLE != ALIGNOF_VOIDP
#define JMPBUF_ALIGNMENT ALIGNOF_DOUBLE
#endif
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
#if defined(_AIX) || defined(__APPLE__) || defined(__unix) || defined(__BEOS__) || defined(__NetBSD__)
#define __unix__ 1
#endif
#endif

/* AIX requires this to be the first thing in the file.  */
#ifdef __GNUC__
# ifndef alloca
#  define alloca __builtin_alloca
# endif
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

#if _FILE_OFFSET_BITS == 64 || defined(_LARGE_FILES)
#define O_LARGEFILES 1		/* use for conditional code in Prolog */
#else
#undef O_LARGEFILES
#endif

#include <sys/types.h>
#if __MINGW32__
typedef _sigset_t sigset_t;
#endif
#include <setjmp.h>
#ifdef ASSERT_H_REQUIRES_STDIO_H
#include <stdio.h>
#endif /*ASSERT_H_REQUIRES_STDIO_H*/
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <stdarg.h>
#include <limits.h>

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

#ifdef O_GMP
#ifdef _MSC_VER			/* ignore warning in gmp 5.0.2 header */
#pragma warning( disable : 4146 )
#endif
#include <gmp.h>
#ifdef _MSC_VER
#pragma warning( default : 4146 )
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

/* MAXPATHLEN is an optional POSIX feature (Bug#63).  As SWI-Prolog has
   no length limits on text except for representing paths, we should
   rewrite all file handling code to avoid MAXPATHLEN.  For now we just
   define it.
*/

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A common basis for C keywords.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if __GNUC__ && !__STRICT_ANSI__
#define HAVE_INLINE 1
#define HAVE_VOLATILE 1
#define HAVE___BUILTIN_EXPECT 1
#endif

#if !defined(HAVE_INLINE) && !defined(inline)
#define inline
#endif

#if defined(__GNUC__) && !defined(__OPTIMIZE__)
#define _DEBUG 1
#endif

#ifndef HAVE_VOLATILE
#define volatile
#endif

#if defined(__GNUC__) && !defined(NORETURN)
#define NORETURN __attribute__ ((noreturn))
#else
#define NORETURN
#endif

#if defined(__GNUC__) && !defined(MAY_ALIAS)
#define MAY_ALIAS __attribute__ ((__may_alias__))
#else
#define MAY_ALIAS
#endif

#ifdef HAVE___BUILTIN_EXPECT
#define likely(x)       __builtin_expect((x), 1)
#define unlikely(x)     __builtin_expect((x), 0)
#else
#define likely(x)	(x)
#define unlikely(x)	(x)
#endif

#if defined(__STRICT_ANSI__) || defined(NO_ASM_NOP)
#define ASM_NOP { static int nop; nop++; }
#endif

#ifdef DMALLOC
#include <dmalloc.h>			/* Use www.dmalloc.com debugger */

#define PL_ALLOC_DONE 1
#define DMALLOC_FUNC_CHECK 1
#define allocHeap(n)		malloc(n)
#define allocHeapOrHalt(n)	xmalloc(n)
#define freeHeap(ptr, n)	do { (void)(n); xfree(ptr); } while(0)
#endif /*DMALLOC*/

#define forwards static		/* forwards function declarations */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Booleans,  addresses,  strings  and other   goodies.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef int			bool;

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
#define TRY(goal)		do { if (!(goal)) return FALSE; } while(0)

#define CL_START		((ClauseRef)1)	/* asserta */
#define CL_END			((ClauseRef)2)	/* assertz */

typedef void *			caddress;

#define EOS			('\0')
#define ESC			((char) 27)
#define streq(s, q)		((strcmp((s), (q)) == 0))

				/* n is 2^m !!! */
#define ROUND(p, n)		((((p) + (n) - 1) & ~((n) - 1)))
#define addPointer(p, n)	((void *) ((intptr_t)(p) + (intptr_t)(n)))
#define diffPointers(p1, p2)	((intptr_t)(p1) - (intptr_t)(p2))

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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define BUFFER_RING_SIZE	16	/* foreign buffer ring (pl-fli.c) */
#define LINESIZ			1024	/* size of a data line */
#define MAXARITY		1024	/* arity of predicate */
#define MINFOREIGNSIZE		32	/* Minimum term_t in foreign frame */
#define MAXSYMBOLLEN		256	/* max size of foreign symbols */
#define OP_MAXPRIORITY		1200	/* maximum operator priority */
#define SMALLSTACK		32 * 1024 /* GC policy */
#define MAX_PORTRAY_NESTING	100	/* Max recursion in portray */

#define LOCAL_MARGIN ((size_t)argFrameP((LocalFrame)NULL, MAXARITY) + \
		      sizeof(struct choice))

#define WORDBITSIZE		(8 * sizeof(word))
#define LONGBITSIZE		(8 * sizeof(long))
#define INTBITSIZE		(8 * sizeof(int))
#define INT64BITSIZE		(8 * sizeof(int64_t))
#define WORDS_PER_DOUBLE        ((sizeof(double)+sizeof(word)-1)/sizeof(word))
#define WORDS_PER_INT64		(sizeof(int64_t)/sizeof(word))

				/* Prolog's integer range */
#define PLMINTAGGEDINT		(-(intptr_t)((word)1<<(WORDBITSIZE-LMASK_BITS-1)))
#define PLMAXTAGGEDINT		(-PLMINTAGGEDINT - 1)
#define PLMINTAGGEDINT32	(-(intptr_t)((word)1<<(32-LMASK_BITS-1)))
#define PLMAXTAGGEDINT32	(-PLMINTAGGEDINT32 - 1)
#define inTaggedNumRange(n)	(valInt(consInt(n)) == (n))
#define PLMININT		(-PLMAXINT - 1)
#define PLMAXINT		((int64_t)(((uint64_t)1<<(INT64BITSIZE-1)) - 1))
#if SIZEOF_WCHAR_T == 2
#define PLMAXWCHAR		(0xffff)
#else
#define PLMAXWCHAR		(0x10ffff)
#endif

#if vax
#define MAXREAL			(1.701411834604692293e+38)
#else					/* IEEE double */
#define MAXREAL			(1.79769313486231470e+308)
#endif

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

#include "os/pl-table.h"
#include "pl-vmi.h"

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Magic for assertions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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

***TBD*** This is totally out of date.  The datatypes are accessed using
macros defined in pl-data.h.

INTEGER
    Integers are stored in the  27  remaining  bits  of  a  word.   This
    implies they are limited to +- 2^26.
FLOAT
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Common Prolog objects typedefs. Note that   code is word-aligned for two
reasons. First of all, we want to get   the maximum speed and second, we
must ensure that sizeof(struct clause) is  a multiple of sizeof(word) to
place them on the stack (see I_USERCALL).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef __GNUC__
#define WORD_ALIGNED __attribute__ ((aligned (sizeof(word))))
#else
#define WORD_ALIGNED
#endif

#ifndef PL_HAVE_TERM_T
#define PL_HAVE_TERM_T
typedef uintptr_t		term_t;		/* external term-reference */
#endif

typedef uintptr_t		word;		/* Anonymous 4 byte object */
typedef word *			Word;		/* a pointer to anything */
typedef word			atom_t;		/* encoded atom */
typedef word			functor_t;	/* encoded functor */
typedef uintptr_t		code WORD_ALIGNED; /* bytes codes */
typedef code *			Code;		/* pointer to byte codes */
typedef int			Char;		/* char that can pass EOF */
typedef word			(*Func)();	/* foreign functions */
typedef int			(*ArithF)();	/* arithmetic function */

typedef struct atom *		Atom;		/* atom */
typedef struct functor *	Functor;	/* complex term */
typedef struct functorDef *	FunctorDef;	/* name/arity pair */
typedef struct procedure *	Procedure;	/* predicate */
typedef struct definition *	Definition;	/* predicate definition */
typedef struct definition_chain *DefinitionChain; /* linked list of defs */
typedef struct clause *		Clause;		/* compiled clause */
typedef struct clause_ref *	ClauseRef;      /* reference to a clause */
typedef struct clause_index *	ClauseIndex;    /* Clause indexing table */
typedef struct clause_bucket *	ClauseBucket;   /* Bucked in clause-index table */
typedef struct operator *	Operator;	/* see pl-op.c, pl-read.c */
typedef struct record *		Record;		/* recorda/3, etc. */
typedef struct recordRef *	RecordRef;      /* reference to a record */
typedef struct recordList *	RecordList;	/* list of these */
typedef struct module *		Module;		/* predicate modules */
typedef struct sourceFile *	SourceFile;	/* file adminitration */
typedef struct list_cell *	ListCell;	/* Anonymous list */
typedef struct localFrame *	LocalFrame;	/* environment frame */
typedef struct local_definitions *LocalDefinitions; /* thread-local preds */
typedef struct choice *		Choice;		/* Choice-point */
typedef struct clause_choice *  ClauseChoice;   /* firstClause()/nextClause() */
typedef struct queryFrame *	QueryFrame;     /* toplevel query frame */
typedef struct fliFrame *	FliFrame;	/* FLI interface frame */
typedef struct trail_entry *	TrailEntry;	/* Entry of trail stack */
typedef struct gc_trail_entry *	GCTrailEntry;	/* Entry of trail stack (GC) */
typedef struct mark		mark;		/* backtrack mark */
typedef struct stack *		Stack;		/* machine stack */
typedef struct _varDef *	VarDef;		/* pl-comp.c */
typedef struct extension_cell *	ExtensionCell;  /* pl-ext.c */
typedef struct abort_handle *	AbortHandle;	/* PL_abort_hook() */
typedef struct initialise_handle * InitialiseHandle;
typedef struct canonical_dir *	CanonicalDir;	/* pl-os.c */
typedef struct on_halt *	OnHalt;		/* pl-os.c */
typedef struct find_data_tag *	FindData;	/* pl-trace.c */
typedef struct feature *	Feature;	/* pl-prims.c */
typedef struct dirty_def_info * DirtyDefInfo;

typedef uintptr_t qid_t;		/* external query-id */
typedef uintptr_t PL_fid_t;		/* external foreign context-id */

#define fid_t PL_fid_t			/* avoid AIX name-clash */

		 /*******************************
		 *	    ARITHMETIC		*
		 *******************************/

/* the numtype enum requires total ordering.
*/

typedef enum
{ V_INTEGER,				/* integer (64-bit) value */
#ifdef O_GMP
  V_MPZ,				/* mpz_t */
  V_MPQ,				/* mpq_t */
#endif
  V_FLOAT				/* Floating point number (double) */
} numtype;

typedef struct
{ numtype type;				/* type of number */
  union { double f;			/* value as a floating point number */
	  int64_t i;			/* value as integer */
	  word  w[WORDS_PER_DOUBLE];	/* for packing/unpacking the double */
#ifdef O_GMP
	  mpz_t mpz;			/* GMP integer */
	  mpq_t mpq;			/* GMP rational */
#endif
	} value;
} number, *Number;

#define TOINT_CONVERT_FLOAT	0x1	/* toIntegerNumber() */
#define TOINT_TRUNCATE		0x2

#ifdef O_GMP
#define intNumber(n)	((n)->type <=  V_MPZ)
#else
#define intNumber(n)	((n)->type <  V_FLOAT)
#endif
#define floatNumber(n)	((n)->type >= V_FLOAT)

typedef enum
{ NUM_ERROR = FALSE,			/* Syntax error */
  NUM_OK    = TRUE,			/* Ok */
  NUM_FUNDERFLOW = -1,			/* Float underflow */
  NUM_FOVERFLOW = -2,			/* Float overflow */
  NUM_IOVERFLOW = -3,			/* Integer overflow */
  NUM_CONSTRANGE = -4			/* numeric constant out of range */
} strnumstat;



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
#define GP_TYPE_QUIET	0x400		/* don't throw errors on wrong types */
#define GP_EXISTENCE_ERROR 0x800	/* throw error if proc is not found */
#define GP_QUALIFY	0x1000		/* Always module-qualify */
#define GP_NOT_QUALIFIED 0x2000		/* Demand unqualified name/arity */

					/* get_functor() */
#define GF_EXISTING	0x1
#define GF_PROCEDURE	0x2		/* check for max arity */
#define GF_NAMEARITY	0x4		/* only accept name/arity */

#define SM_NOCREATE	0x1		/* stripModule(): do not create modules */

		 /*******************************
		 *	       ALERT		*
		 *******************************/

/* See updateAlerted()
*/

#define	ALERT_SIGNAL	     0x01
#define	ALERT_GCREQ	     0x02
#define	ALERT_PROFILE	     0x04
#define	ALERT_EXITREQ	     0x08
#define	ALERT_DEPTHLIMIT     0x10
#define	ALERT_INFERENCELIMIT 0x20
#define	ALERT_WAKEUP	     0x40
#define	ALERT_DEBUG	     0x80


		 /*******************************
		 *	     CLEANUP		*
		 *******************************/

typedef enum
{ CLN_NORMAL = 0,			/* Normal mode */
  CLN_PROLOG,				/* Prolog hooks */
  CLN_FOREIGN,				/* Foreign hooks */
  CLN_IO,				/* Cleaning I/O */
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
#define set(s, a)		ATOMIC_OR(&(s)->flags, (a))
#define clear(s, a)		ATOMIC_AND(&(s)->flags, ~(a))
#define clearFlags(s)		((s)->flags = 0)

/* Flags on predicates (packed in unsigned int */

#define P_CLAUSABLE		(0x00000002) /* Clause/2 always works */
#define P_QUASI_QUOTATION_SYNTAX (0x00000004) /* {|Type||Quasi Quote|} */
#define P_NON_TERMINAL		(0x00000008) /* Grammar rule (Name//Arity) */
#define P_SHRUNKPOW2		(0x00000010) /* See reconsider_index() */
#define P_FOREIGN		(0x00000020) /* Implemented in C */
#define P_NONDET		(0x00000040) /* Foreign: nondet */
#define P_VARARG		(0x00000080) /* Foreign: use alt calling API */
#define P_FOREIGN_CREF		(0x00000100) /* Foreign: ndet ctx is clause */
#define P_DYNAMIC		(0x00000200) /* Dynamic predicate */
#define P_THREAD_LOCAL		(0x00000400) /* Thread local dynamic predicate */
#define P_VOLATILE		(0x00000800) /* Clauses are not saved */
#define P_DISCONTIGUOUS		(0x00001000) /* Clauses are not together */
#define P_MULTIFILE		(0x00002000) /* Clauses are in multiple files */
#define P_PUBLIC		(0x00004000) /* Called from somewhere */
#define P_ISO			(0x00008000) /* Part of the ISO standard */
#define P_LOCKED		(0x00010000) /* Locked as system predicate */
#define P_NOPROFILE		(0x00020000) /* Profile children, not me */
#define P_TRANSPARENT		(0x00040000) /* Inherit calling module */
#define P_META			(0x00080000) /* Has meta_predicate declaration */
#define P_MFCONTEXT		(0x00100000) /* Used for Goal@Module */
#define P_DIRTYREG		(0x00200000) /* Part of GD->procedures.dirty */
#define P_ERASED		(0x00400000) /* Predicate has been destroyed */
#define HIDE_CHILDS		(0x00800000) /* Hide children from tracer */
#define SPY_ME			(0x01000000) /* Spy point placed */
#define TRACE_ME		(0x02000000) /* Can be debugged */
#define TRACE_CALL		(0x04000000) /* Trace calls */
#define TRACE_REDO		(0x08000000) /* Trace redo */
#define TRACE_EXIT		(0x10000000) /* Trace edit */
#define TRACE_FAIL		(0x20000000) /* Trace fail */
#define FILE_ASSIGNED		(0x40000000) /* Is assigned to a file */
#define P_REDEFINED		(0x80000000) /* Overrules a definition */
#define PROC_DEFINED		(P_DYNAMIC|P_FOREIGN|P_MULTIFILE|P_DISCONTIGUOUS)
/* flags for p_reload data (reconsult) */
#define P_MODIFIED		P_DIRTYREG
#define P_NEW			SPY_ME
#define P_NO_CLAUSES		TRACE_ME

/* Flags on clauses (packed in unsigned flags : 8) */

#define CL_ERASED		(0x0001) /* clause was erased */
#define UNIT_CLAUSE		(0x0002) /* Clause has no body */
#define HAS_BREAKPOINTS		(0x0004) /* Clause has breakpoints */
#define GOAL_CLAUSE		(0x0008) /* Dummy for meta-calling */
#define COMMIT_CLAUSE		(0x0010) /* This clause will commit */
#define DBREF_CLAUSE		(0x0020) /* Clause has db-reference */
#define DBREF_ERASED_CLAUSE	(0x0040) /* Deleted while referenced */
#define CL_BODY_CONTEXT		(0x0080) /* Module context of body is different */
					 /* from predicate */

/* Flags on module.  Most of these flags are copied to the read context
   in pl-read.c.
*/

#define M_SYSTEM		(0x0001) /* system module */
#define M_CHARESCAPE		(0x0002) /* module */
#define DBLQ_CHARS		(0x0004) /* "ab" --> ['a', 'b'] */
#define DBLQ_ATOM		(0x0008) /* "ab" --> 'ab' */
#define DBLQ_STRING		(0x0010) /* "ab" --> "ab" */
#define DBLQ_MASK		(DBLQ_CHARS|DBLQ_ATOM|DBLQ_STRING)
#define BQ_STRING		(0x0020) /* `ab` --> "ab" */
#define BQ_CODES		(0x0040) /* `ab` --> [97,98] */
#define BQ_CHARS		(0x0080) /* `ab` --> [a,b] */
#define BQ_MASK			(BQ_STRING|BQ_CODES|BQ_CHARS)
#define UNKNOWN_FAIL		(0x0100) /* module */
#define UNKNOWN_WARNING		(0x0200) /* module */
#define UNKNOWN_ERROR		(0x0400) /* module */
#define UNKNOWN_MASK		(UNKNOWN_ERROR|UNKNOWN_WARNING|UNKNOWN_FAIL)
#define M_VARPREFIX		(0x0800)

/* Flags on functors */

#define CONTROL_F		(0x0002) /* functor (compiled controlstruct) */
#define ARITH_F			(0x0004) /* functor (arithmetic operator) */
#define VALID_F			(0x0008) /* functor (fully defined) */

/* Flags on record lists (recorded database keys) */

#define RL_DIRTY		(0x0001) /* recordlist */

/* Flags on recorded database records (also PL_record()) */

#define R_ERASED		(0x0001) /* record: record is erased */
#define R_EXTERNAL		(0x0002) /* record: inline atoms */
#define R_DUPLICATE		(0x0004) /* record: include references */
#define R_NOLOCK		(0x0008) /* record: do not lock atoms */
#define R_DBREF			(0x0010) /* record: has DB-reference */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Macros for environment frames (local stack frames)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define FR_HIDE_CHILDS		(0x0001) /* flag of pred after I_DEPART */
#define FR_SKIPPED		(0x0002) /* We have skipped on this frame */
#define FR_MARKED		(0x0004) /* GC */
#define FR_MARKED_PRED		(0x0008) /* GC predicates/clauses */
#define FR_WATCHED		(0x0010) /* GUI debugger */
#define FR_CATCHED		(0x0020) /* Frame caught an exception */
#define FR_INBOX		(0x0040) /* Inside box (for REDO in built-in) */
#define FR_CONTEXT		(0x0080) /* fr->context is set */
#define FR_CLEANUP		(0x0100) /* setup_call_cleanup/4: marked for cleanup */
#define FR_INRESET		(0x0200) /* Continuations: inside reset/3 */
#define FR_MAGIC_MASK		(0xfffff000)
#define FR_MAGIC_MASK2		(0xffff0000)
#define FR_MAGIC		(0x549d5000)

#define isFrame(fr)		(((fr)->flags&FR_MAGIC_MASK) == FR_MAGIC)
#define wasFrame(fr)		(((fr)->flags&FR_MAGIC_MASK2) == \
				 (FR_MAGIC&FR_MAGIC_MASK2))
#define killFrame(fr)		clear(fr, (FR_MAGIC_MASK&~FR_MAGIC_MASK2))

#define ARGOFFSET		((int)sizeof(struct localFrame))
#define VAROFFSET(var)		((var)+(ARGOFFSET/(int)sizeof(word)))

#define setLevelFrame(fr, l)	do { (fr)->level = (l); } while(0)
#define levelFrame(fr)		((fr)->level)
#define argFrameP(f, n)		((Word)((f)+1) + (n))
#define argFrame(f, n)		(*argFrameP((f), (n)) )
#define varFrameP(f, n)		((Word)(f) + (n))
#define varFrame(f, n)		(*varFrameP((f), (n)) )
#define refFliP(f, n)		((Word)((f)+1) + (n))
#define parentFrame(f)		((f)->parent ? (f)->parent\
					     : (LocalFrame)varFrame((f), -1))
#define slotsFrame(f)		(true((f)->predicate, P_FOREIGN) ? \
				      (f)->predicate->functor->arity : \
				      (f)->clause->clause->prolog_vars)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Generations must be 64-bit to  avoid   overflow  in realistic scenarios.
This makes them the only 64-bit value in struct localFrame. Stack frames
mix with variables on the stacks and  are thus word-aligned. We have two
options here. One is to represent a  generation as a struct (used below)
or we must align frame at 8-byte  boundaries. The latter is probably the
best solution, but merely aligning lTop in   I_ENTER  doesn't seem to be
doing the trick: it causes failure of the  test suite for which I failed
to find the reason. Enabling the structure   on x86 causes a slowdown of
about 5%. I'd assume the difference is smaller on real 32-bit hardware.

We enable this  if the alignment  of an int64_t type  is not the same as
the alignment of pointers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_LOGICAL_UPDATE
typedef uint64_t gen_t;

#define GEN_MAX (~(gen_t)0)
#define GEN_NEW_DIRTY (gen_t)0

#if ALIGNOF_INT64_T != ALIGNOF_VOIDP
typedef struct lgen_t
{ uint32_t	gen_l;
  uint32_t	gen_u;
} lgen_t;

#define generationFrame(f) \
	((gen_t)(f)->generation.gen_u<<32 | (gen_t)(f)->generation.gen_l)
#define setGenerationFrame(f, gen) \
	do { (f)->generation.gen_u = (uint32_t)(gen>>32); \
	     (f)->generation.gen_l = (uint32_t)(gen); \
	   } while(0)
#else
typedef uint64_t lgen_t;
#define generationFrame(f)	((f)->generation)
#define setGenerationFrame(f, gen) \
	do { (f)->generation = (gen); } while(0)
#endif
#ifdef HAVE___SYNC_ADD_AND_FETCH_8
#define global_generation() (GD->_generation)
#define next_global_generation() ATOMIC_INC(&GD->_generation)
typedef uint64_t ggen_t;
#else
#define ATOMIC_GENERATION_HACK 1
typedef struct ggen_t
{ uint32_t	gen_l;
  uint32_t	gen_u;
} ggen_t;
#endif /*HAVE___SYNC_ADD_AND_FETCH_8*/
#else /*O_LOGICAL_UPDATE*/
#define global_generation()	(0)
#define next_global_generation() (0)
#define generationFrame(f)	(0)
#define setGenerationFrame(f)	(void)0
#endif /*O_LOGICAL_UPDATE*/

#define FR_CLEAR_NEXT	FR_SKIPPED|FR_WATCHED|FR_CATCHED|FR_HIDE_CHILDS|FR_CLEANUP
#define FR_CLEAR_FLAGS	(FR_CLEAR_NEXT|FR_CONTEXT)

#define setNextFrameFlags(next, fr) \
	do \
	{ (next)->level = (fr)->level+1; \
	  (next)->flags = ((fr)->flags) & ~FR_CLEAR_FLAGS; \
	} while(0)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Predicate reference counting. The aim  of   this  mechanism  is to avoid
modifying the predicate structure while  it   has  choicepoints  or (MT)
other threads running the predicate. For dynamic  code we allow to clean
the predicate as the reference-count drops to   zero. For static code we
introduce a garbage collector (TBD).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define enterDefinition(def) (void)0
#define leaveDefinition(def) (void)0


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
At times an abort is not allowed because the heap  is  inconsistent  the
programmer  should  call  startCritical  to start such a code region and
endCritical to end it.

MT/TBD: how to handle this gracefully in the multi-threading case.  Does
it mean anything?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define startCritical (void)(LD->critical++)
#define endCritical   ((--(LD->critical) == 0 && LD->alerted) \
				? endCritical__LD(PASS_LD1) : TRUE)

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

#define cpInt64Data(to, from) \
	{ Word _f = (Word)(from); \
	  switch(WORDS_PER_INT64) \
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
#ifdef O_TERMHASH
  unsigned int  hash_value;	/* hash-key value */
#endif
#ifdef O_ATOMGC
  unsigned int	references;	/* reference-count */
#endif
  union
  { struct PL_blob_t * type;	/* blob-extension */
    uintptr_t	next_invalid;	/* next invalidated atom */
  };
  size_t	length;		/* length of the atom */
  char *	name;		/* name associated with atom */
};


typedef struct atom_array
{ Atom blocks[8*sizeof(void*)];
} atom_array;

typedef struct atom_table * AtomTable;

typedef struct atom_table
{ AtomTable	prev;
  int		buckets;
  Atom *	table;
} atom_table;


#ifdef O_ATOMGC

#define ATOM_STATE_MASK		((unsigned int)0xF << (INTBITSIZE-4))
#define ATOM_RESERVED_REFERENCE	((unsigned int)0x1 << (INTBITSIZE-1))
#define ATOM_VALID_REFERENCE	((unsigned int)0x1 << (INTBITSIZE-2))
#define ATOM_MARKED_REFERENCE	((unsigned int)0x1 << (INTBITSIZE-3))
#define ATOM_DESTROY_REFERENCE	((unsigned int)0x1 << (INTBITSIZE-4))

#define ATOM_IS_FREE(ref)	(((ref) & ATOM_STATE_MASK) == 0)
#define ATOM_IS_RESERVED(ref)	((ref) & ATOM_RESERVED_REFERENCE)
#define ATOM_IS_VALID(ref)	((ref) & ATOM_VALID_REFERENCE)
#define ATOM_IS_MARKED(ref)	((ref) & ATOM_MARKED_REFERENCE)
#define ATOM_IS_DESTROYED(ref)	((ref) & ATOM_DESTROY_REFERENCE)

#define ATOM_REF_COUNT_MASK	(~ATOM_STATE_MASK)
#define ATOM_REF_COUNT(ref)	((ref) & ATOM_REF_COUNT_MASK)

#define ATOM_TYPE_INVALID	((PL_blob_t*)0x007)

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

struct functorDef
{ FunctorDef	next;		/* next in chain */
  word		functor;	/* as appearing on the global stack */
  word		name;		/* Name of functor */
  size_t	arity;		/* arity of functor */
  unsigned      flags;		/* Flag field holding: */
		  /* CONTROL_F	   Compiled control-structure */
		  /* ARITH_F	   Arithmetic function */
		  /* VALID_F	   Fully defined functor */
};


typedef struct functor_array
{ FunctorDef *blocks[8*sizeof(void*)];
} functor_array;

typedef struct functor_table * FunctorTable;

typedef struct functor_table
{ FunctorTable	prev;
  int		buckets;
  FunctorDef *	table;
} functor_table;

#define FUNCTOR_IS_VALID(flags)		((flags) & VALID_F)


#ifdef O_LOGICAL_UPDATE
#define VISIBLE_CLAUSE(cl, gen) \
	( ( (cl)->generation.created <= (gen) && \
	    (cl)->generation.erased   > (gen) && \
	    (cl)->generation.erased  != LD->gen_reload \
	  ) || \
	  ( (cl)->generation.created == LD->gen_reload \
	  ) \
	)
#define GLOBALLY_VISIBLE_CLAUSE(cl, gen) \
	( (cl)->generation.created <= (gen) && \
	  (cl)->generation.erased   > (gen) \
	)
#else
#define VISIBLE_CLAUSE(cl, gen) false(cl, CL_ERASED)
#define GLOBALLY_VISIBLE_CLAUSE(cl, gen) false(cl, CL_ERASED)
#endif

#define visibleClause(cl, gen) visibleClause__LD(cl, gen PASS_LD)
#define visibleClauseCNT(cl, gen) visibleClauseCNT__LD(cl, gen PASS_LD)

#define GEN_INVALID 0

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Struct clause must be a  multiple   of  sizeof(word)  for compilation on
behalf  of  I_USERCALL.  This   is   verified    in   an   assertion  in
checkCodeTable().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define sizeofClause(n) ((char *)&((Clause)NULL)->codes[n] - (char *)NULL)

struct clause
{ Definition		predicate;	/* Predicate I belong to */
#ifdef O_LOGICAL_UPDATE
  struct
  { gen_t created;			/* Generation that created me */
    gen_t erased;			/* Generation I was erased */
  } generation;
#endif /*O_LOGICAL_UPDATE*/
  unsigned int		variables;	/* # of variables for frame */
  unsigned int		prolog_vars;	/* # real Prolog variables */
  unsigned int		flags;		/* Flag field holding: */
  unsigned int		line_no;	/* Source line-number */
  unsigned int		source_no;	/* Index of source-file */
  unsigned int		owner_no;	/* Index of owning source-file */
  unsigned int		references;	/* # ClauseRef pointing at me */
  code			code_size;	/* size of ->codes */
  code			codes[1];	/* VM codes of clause */
};

typedef struct clause_list
{ ClauseRef	first_clause;		/* clause list of procedure */
  ClauseRef	last_clause;		/* last clause of list */
  ClauseIndex  *clause_indexes;		/* Hash index(es) */
  unsigned int	number_of_clauses;	/* number of associated clauses */
  unsigned int	erased_clauses;		/* number of erased clauses in set */
  unsigned int	number_of_rules;	/* number of real rules */
  unsigned int	jiti_tried;		/* number of times we tried to find */
} clause_list, *ClauseList;

typedef struct clause_ref
{ ClauseRef	next;			/* Next in list */
  union
  { word	key;			/* Index key */
    ClauseRef	gnext;			/* Next garbage clause reference */
  } d;
  union
  { Clause	clause;			/* Single clause value */
    clause_list	clauses;		/* Clause list (in hash-tables) */
  } value;
} clause_ref;

#define SIZEOF_CREF_CLAUSE	(offsetof(clause_ref, value.clause) + \
				 sizeof(Clause))
#define SIZEOF_CREF_LIST	sizeof(clause_ref)

typedef struct cgc_stats
{ int		threads;		/* # threads to scan */
  size_t	local_size;		/* Summed size of local stacks */
  size_t	dirty_pred_clauses;	/* # clauses in dirty predicates */
  int64_t	erased_skipped;		/* # skipped clauses that are erased  */
} cgc_stats;


#define VM_DYNARGC    255	/* compute argcount dynamically */

#define CA1_PROC	1	/* code arg 1 is procedure */
#define CA1_FUNC	2	/* code arg 1 is functor */
#define CA1_DATA	3	/* code arg 2 is prolog data (H_ATOM, H_SMALLINT) */
#define CA1_INTEGER	4	/* intptr_t value */
#define CA1_INT64	5	/* int64 value */
#define CA1_FLOAT	6	/* next WORDS_PER_DOUBLE are double */
#define CA1_STRING	7	/* inlined string */
#define CA1_MODULE	8	/* a module */
#define CA1_VAR		9	/* a variable(-offset) */
#define CA1_FVAR       10	/* a variable(-offset), used as `firstvar' */
#define CA1_CHP	       11	/* ChoicePoint (also variable(-offset)) */
#define CA1_MPZ	       12	/* GNU mpz number */
#define CA1_FOREIGN    13	/* Foreign function pointer */
#define CA1_CLAUSEREF  14	/* Clause reference */
#define CA1_JUMP       15	/* Instructions to skip */
#define CA1_AFUNC      16	/* Number of arithmetic function */

#define VIF_BREAK      0x01	/* Can be a breakpoint */

typedef enum
{ VMI_REPLACE,
  VMI_STEP_ARGUMENT
} vmi_merge_type;

typedef struct
{ vmi		code;		/* Code to merge with */
  vmi_merge_type how;		/* How to merge? */
  vmi		merge_op;	/* Opcode of merge */
  int		merge_ac;	/* #arguments of merged code */
  code		merge_av[1];	/* Argument vector */
} vmi_merge;

typedef struct
{ char	       *name;		/* name of the code */
  vmi		code;		/* number of the code */
  unsigned char flags;		/* Addional flags (VIF_*) */
  unsigned char	arguments;	/* #args code takes (or VM_DYNARGC) */
  char		argtype[4];	/* Argument type(s) code takes */
} code_info;

struct mark
{ TrailEntry	trailtop;	/* top of the trail stack */
  Word		globaltop;	/* top of the global stack */
  Word		saved_bar;	/* saved LD->mark_bar */
};

struct functor
{ word		definition;	/* Tagged definition pointer */
  word		arguments[1];	/* arguments vector */
};

struct clause_bucket
{ ClauseRef	head;
  ClauseRef	tail;
  unsigned int	dirty;			/* # of garbage clauses */
};

#define MAX_MULTI_INDEX 4

struct clause_index
{ unsigned int	 buckets;		/* # entries */
  unsigned int	 size;			/* # clauses */
  unsigned int	 resize_above;		/* consider resize > #clauses */
  unsigned int	 resize_below;		/* consider resize < #clauses */
  unsigned int	 dirty;			/* # chains that are dirty */
  unsigned short args[MAX_MULTI_INDEX];	/* Indexed arguments */
  unsigned	 is_list : 1;		/* Index with lists */
  float		 speedup;		/* Estimated speedup */
  ClauseBucket	 entries;		/* chains holding the clauses */
};

typedef struct arg_info
{ float		speedup;		/* Computed speedup */
  unsigned	ln_buckets : 5;		/* lg2(bucket count) */
  unsigned	assessed   : 1;		/* Value was assessed */
  unsigned	meta	   : 4;		/* Meta-argument info */
} arg_info;

#define MAX_BLOCKS 20			/* allows for 2M threads */

typedef struct local_definitions
{ Definition *blocks[MAX_BLOCKS];
  Definition preallocated[7];
} local_definitions;

struct definition
{ FunctorDef	functor;		/* Name/Arity of procedure */
  Module	module;			/* module of the predicate */
  Code		codes;			/* Executable code */
  union
  { void *	any;			/* has some value */
    clause_list	clauses;		/* (Indexed) list of clauses */
    Func	function;		/* function pointer of procedure */
    LocalDefinitions local;		/* P_THREAD_LOCAL predicates */
  } impl;
  arg_info     *args;			/* Meta and indexing info */
  unsigned int  flags;			/* booleans (P_*) */
  unsigned int  shared;			/* #procedures sharing this def */
  struct linger_list  *lingering;	/* Assocated lingering objects */
  gen_t		last_modified;		/* Generation I was last modified */
#ifdef O_PROF_PENTIUM
  int		prof_index;		/* index in profiling */
  char	       *prof_name;		/* name in profiling */
#endif
};

struct definition_chain
{ Definition		definition;	/* chain on definition */
  DefinitionChain	next;		/* next in chain */
};

struct dirty_def_info
{ gen_t		oldest_generation;	/* Oldest generation seen */
};

typedef struct definition_ref
{ Definition predicate;			/* Referenced definition */
  gen_t	     generation;		/* at generation */
} definition_ref;

typedef struct definition_refs
{ definition_ref *blocks[MAX_BLOCKS];
  definition_ref preallocated[7];
  size_t     top;
} definition_refs;

#define	PROC_WEAK	 (0x0001)	/* implicit import */
#define	PROC_MULTISOURCE (0x0002)	/* Assigned to multiple sources */

struct procedure
{ Definition	 definition;		/* definition of procedure */
  unsigned short flags;			/* PROC_WEAK */
  unsigned short source_no;		/* Source I'm assigned to */
};

struct localFrame
{ Code		programPointer;		/* pointer into program */
  LocalFrame	parent;			/* parent local frame */
  ClauseRef	clause;			/* Current clause of frame */
  Definition	predicate;		/* Predicate we are running */
  Module	context;		/* context module of frame */
#ifdef O_PROFILE
  struct call_node *prof_node;		/* Profiling node */
#endif
#ifdef O_LOGICAL_UPDATE
  lgen_t	generation;		/* generation of the database */
#endif
  unsigned int	level;			/* recursion level */
  unsigned int	flags;			/* packed long holding: */
};


typedef enum
{ CHP_JUMP = 0,				/* A jump due to ; */
  CHP_CLAUSE,				/* Next clause of predicate */
  CHP_TOP,				/* First (toplevel) choice */
  CHP_CATCH,				/* $catch initiated choice */
  CHP_DEBUG				/* Enable redo */
} choice_type;

typedef enum
{ DBG_OFF = 0,				/* no debugging */
  DBG_ON,				/* switch on in current environment */
  DBG_ALL				/* switch on globally */
} debug_type;

#define SKIP_VERY_DEEP	  ((size_t)-1)	/* deep skiplevel */
#define SKIP_REDO_IN_SKIP (SKIP_VERY_DEEP-1)

struct clause_choice
{ ClauseRef	cref;			/* Next clause reference */
  word		key;			/* Search key */
};

#ifdef O_PLMT
#define acquire_def(def) do { LD->thread.info->access.predicate = def; } while(0)
#define release_def(def) do { LD->thread.info->access.predicate = NULL; } while(0)
#else
#define acquire_def(def) (void)0
#define release_def(def) (void)0
#endif

struct choice
{ choice_type	type;			/* CHP_* */
  Choice	parent;			/* Alternative if I fail */
  mark		mark;			/* data mark for undo */
  LocalFrame	frame;			/* Frame I am related to */
#ifdef O_PROFILE
  struct call_node *prof_node;		/* Profiling node */
#endif
  union
  { struct clause_choice clause;	/* Next candidate clause */
    Code	PC;			/* Next candidate program counter */
    word        foreign;		/* foreign redo handle */
  } value;
};


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
EXCEPTION_GUARDED(code, cleanup) must be used  in environments that need
cleanup  should  a  PL_throw()  happen.  The   most  commpn  reason  for
PL_throw() instead of the nicely   synchronous PL_raise_exception() is a
stack overflow.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define EXCEPTION_GUARDED(code, cleanup) \
	{ exception_frame __throw_env; \
	  __throw_env.parent = LD->exception.throw_environment; \
	  if ( setjmp(__throw_env.exception_jmp_env) != 0 ) \
	  { LD->exception.throw_environment = __throw_env.parent; \
	    cleanup; \
	  } else \
	  { __throw_env.magic = THROW_MAGIC; \
	    LD->exception.throw_environment = &__throw_env; \
	    code; \
	    assert(LD->exception.throw_environment == &__throw_env); \
	    __throw_env.magic = 41414141; \
	    LD->exception.throw_environment = __throw_env.parent; \
	  } \
	}

#define THROW_MAGIC 42424242

typedef struct exception_frame		/* PL_throw exception environments */
{ struct exception_frame *parent;	/* parent frame */
  int		magic;			/* THROW_MAGIC */
  jmp_buf	exception_jmp_env;	/* longjmp environment */
} exception_frame;


#define QF_NODEBUG		0x0001	/* debug-able query */
#define QF_DETERMINISTIC	0x0002	/* deterministic success */
#define	QF_INTERACTIVE		0x0004	/* interactive goal (prolog()) */

struct queryFrame
{ uintptr_t magic;			/* Magic code for security */
  struct				/* Interpreter registers */
  { LocalFrame  fr;
    Word	argp;
    Code	pc;
  } registers;
  LocalFrame	next_environment;	/* See D_BREAK and get_vmi_state() */
#ifdef O_LIMIT_DEPTH
  uintptr_t saved_depth_limit;		/* saved values of these */
  uintptr_t saved_depth_reached;
#endif
#if O_CATCHTHROW
  term_t	exception;		/* Exception term */
#endif
  struct
  { term_t	term;			/* Handle to exchange data */
  } yield;
  fid_t		foreign_frame;		/* Frame after PL_next_solution() */
  unsigned int	flags;
  debug_type	debugSave;		/* saved debugstatus.debugging */
  unsigned int	flags_saved;		/* Saved boolean Prolog flags */
  int		solutions;		/* # of solutions produced */
  Word	       *aSave;			/* saved argument-stack */
  Choice	saved_bfr;		/* Saved choice-point */
  LocalFrame	saved_ltop;		/* Saved lTop */
  QueryFrame	parent;			/* Parent queryFrame */
  struct choice	choice;			/* First (dummy) choice-point */
  LocalFrame	saved_environment;	/* Parent local-frame */
					/* Do not put anything between */
					/* or check parentFrame() */
  struct localFrame top_frame;		/* The (dummy) top local frame */
  struct localFrame frame;		/* The initial frame */
};


#define FLI_MAGIC		82649821
#define FLI_MAGIC_CLOSED	42424242

struct fliFrame
{ int		magic;			/* Magic code */
  int		size;			/* # slots on it */
  FliFrame	parent;			/* parent FLI frame */
  mark		mark;			/* data-stack mark */
};

#ifdef O_MAINTENANCE
#define REC_MAGIC 27473244
#endif

struct record
{ int		size;			/* # bytes of the record */
  unsigned      gsize;			/* Size on global stack */
  unsigned	nvars;			/* # variables in the term */
  unsigned	flags;			/* Flags, holding */
					/* R_ERASED */
					/* R_EXTERNAL */
					/* R_DUPLICATE */
					/* R_NOLOCK */
					/* R_DBREF */
#ifdef REC_MAGIC
  int		magic;			/* REC_MAGIC */
#endif
  int		references;		/* PL_duplicate_record() support */
  char		buffer[1];		/* array holding codes */
};

struct recordList
{ RecordRef	firstRecord;		/* first record associated with key */
  RecordRef	lastRecord;		/* last record associated with key */
  struct recordList *next;		/* Next recordList */
  word		key;			/* key of record */
  unsigned int	flags;			/* RL_DIRTY */
  int		references;		/* choicepoints reference count */
};

struct recordRef
{ RecordList	list;			/* list I belong to */
  RecordRef	next;			/* next in list */
  RecordRef	prev;			/* previous in list */
  Record	record;			/* the record itself */
};


		 /*******************************
		 *	EXCEPTION CLASSES	*
		 *******************************/

typedef enum except_class
{ EXCEPT_NONE = 0,			/* no exception */
  EXCEPT_OTHER,				/* any other exception */
  EXCEPT_ERROR,				/* ISO error(Formal,Context) */
  EXCEPT_RESOURCE,			/* ISO error(resource_error(_), _) */
  EXCEPT_TIMEOUT,			/* time_limit_exceeded */
  EXCEPT_ABORT				/* '$aborted' */
} except_class;


		 /*******************************
		 *	SOURCE FILE ADMIN	*
		 *******************************/

#define SF_MAGIC 0x14a3c90f
#define SF_MAGIC_DESTROYING 0x14a3c910

typedef struct p_reload
{ Definition	predicate;		/* definition we are working on */
  gen_t		generation;		/* generation we update */
  ClauseRef	current_clause;		/* currently reloading clause */
  arg_info     *args;			/* Meta info on arguments */
  unsigned	flags;			/* new flags (P_DYNAMIC, etc.) */
  unsigned	number_of_clauses;	/* Number of clauses we've seen */
} p_reload;

typedef struct m_reload
{ Module	module;
  Table		public;			/* new export list */
} m_reload;

typedef struct sf_reload
{ Table		procedures;		/* Procedures being reloaded */
  gen_t		reload_gen;		/* Magic gen for reloading */
  size_t	pred_access_count;	/* Top of predicate access stack */
  Table		modules;		/* Modules seen during reload */
  unsigned	number_of_clauses;	/* reload clause count */
} sf_reload;


struct sourceFile
{ atom_t	name;			/* name of source file */
  double	mtime;			/* modification time when loaded */
  ListCell	procedures;		/* List of associated procedures */
  Procedure	current_procedure;	/* currently loading one */
  ListCell	modules;		/* Modules associated to this file */
  sf_reload     *reload;		/* Reloading context */
#ifdef O_PLMT
  counting_mutex *mutex;		/* Mutex to guard procedures */
#endif
  int		magic;			/* Magic number */
  int		count;			/* number of times loaded */
  unsigned	number_of_clauses;	/* number of clauses */
  unsigned	index : 24;		/* index number (1,2,...) */
  unsigned	system : 1;		/* system sourcefile: do not reload */
};

typedef struct srcfile_array
{ SourceFile *blocks[8*sizeof(void*)];
} srcfile_array;

struct list_cell
{ void *	value;		/* object in the cell */
  ListCell	next;		/* next in chain */
};


		 /*******************************
		 *	      MODULES		*
		 *******************************/

struct module
{ atom_t	name;		/* name of module */
  atom_t	class;		/* class of the module */
  SourceFile	file;		/* file from which module is loaded */
  Table		procedures;	/* predicates associated with module */
  Table		public;		/* public predicates associated */
  Table		operators;	/* local operator declarations */
  ListCell	supers;		/* Import predicates from here */
  ListCell	lingering;	/* Lingering definitions */
  size_t	code_size;	/* #Bytes used for its procedures */
  size_t	code_limit;	/* Limit for code_size */
#ifdef O_PLMT
  counting_mutex *mutex;	/* Mutex to guard procedures */
#endif
#ifdef O_PROLOG_HOOK
  Procedure	hook;		/* Hooked module */
#endif
  int		level;		/* Distance to root (root=0) */
  unsigned int	line_no;	/* Source line-number */
  unsigned int  flags;		/* booleans: */
  gen_t		last_modified;	/* Generation I was last modified */
};

struct trail_entry
{ Word		address;	/* address of the variable */
};

struct gc_trail_entry
{ word		address;	/* address of the variable */
};

		 /*******************************
		 *	   META PREDICATE	*
		 *******************************/

/*0..9*/				/* 0..9: `Extra meta arguments' */
#define MA_META		10		/* : */
#define MA_VAR		11		/* - */
#define MA_ANY		12		/* ? */
#define MA_NONVAR	13		/* + */
#define MA_HAT		14		/* ^ */
#define MA_DCG		15		/* // */

#define MA_NEEDS_TRANSPARENT(m) \
	((m) < 10 || (m) == MA_META || (m) == MA_HAT || (m) == MA_DCG)

		 /*******************************
		 *	     MARK/UNDO		*
		 *******************************/

#define setVar(w)	((w) = (word) 0)

#ifdef O_DESTRUCTIVE_ASSIGNMENT

#define Undo(b)		do_undo(&b)

#else /*O_DESTRUCTIVE_ASSIGNMENT*/

#define Undo(b)		do { TrailEntry tt = tTop; \
			     TrailEntry mt = (b).trailtop; \
			     while(tt > mt) \
			     { tt--; \
			       setVar(*tt->address); \
			     } \
			     tTop = tt; \
			     gTop = (LD->frozen_bar > (b).globaltop ? \
			             LD->frozen_bar : (b).globaltop); \
			    } while(0)
#endif /*O_DESTRUCTIVE_ASSIGNMENT*/

#define NO_MARK_BAR	(Word)(~(uintptr_t)0)

#define Mark(b)		do { (b).trailtop  = tTop; \
			     (b).saved_bar = LD->mark_bar; \
			     DEBUG(CHK_SECURE, \
				   assert((b).saved_bar == NO_MARK_BAR || \
					  ((b).saved_bar >= gBase && \
					   (b).saved_bar <= gTop))); \
			     (b).globaltop = gTop; \
			     if ( LD->mark_bar != NO_MARK_BAR ) \
			       LD->mark_bar = (b).globaltop; \
			   } while(0)
#define DiscardMark(b)	do { LD->mark_bar = (LD->frozen_bar > (b).saved_bar ? \
					     LD->frozen_bar : (b).saved_bar); \
			     DEBUG(CHK_SECURE, \
				   assert(LD->mark_bar == NO_MARK_BAR || \
					  (LD->mark_bar >= gBase && \
					   LD->mark_bar <= gTop))); \
			   } while(0)
#define NOT_A_MARK	(TrailEntry)(~(word)0)
#define NoMark(b)	do { (b).trailtop = NOT_A_MARK; \
			   } while(0)
#define isRealMark(b)	((b).trailtop != NOT_A_MARK)


		 /*******************************
		 *	     TRAILING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that all trail operations demand that   the caller ensures there is
at least one free cell on the trail-stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define Trail(p, w) Trail__LD(p, w PASS_LD)
					/* trail local stack pointer */
#define LTrail(p) \
  (void)((tTop++)->address = p)
					/* trail global stack pointer */
#define GTrail(p) \
  do { if ( p < LD->mark_bar ) \
         (tTop++)->address = p; \
     } while(0)


		 /*******************************
		 *	    SUPERVISORS		*
		 *******************************/

#define SUPERVISOR(name)	(&PL_code_data.supervisors.name[1])



		 /*******************************
		 *	   FLI INTERNALS	*
		 *******************************/

#define consTermRef(p)	 ((Word)(p) - (Word)(lBase))
#define valTermRef(r)	 (&((Word)(lBase))[r])

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Temporary store/restore pointers to make them safe over GC/shift
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define TMP_PTR_SIZE	(4)
#define PushPtr(p)	do { int i = LD->tmp.top++; \
			     assert(i<TMP_PTR_SIZE); \
			     *valTermRef(LD->tmp.h[i]) = makeRef(p); \
			   } while(0)
#define PopPtr(p)	do { int i = --LD->tmp.top; \
			     p = unRef(*valTermRef(LD->tmp.h[i])); \
			     setVar(*valTermRef(LD->tmp.h[i])); \
			   } while(0)
#define PushVal(w)	do { int i = LD->tmp.top++; \
			     assert(i<TMP_PTR_SIZE); \
			     *valTermRef(LD->tmp.h[i]) = w; \
			   } while(0)
#define PopVal(w)	do { int i = --LD->tmp.top; \
			     w = *valTermRef(LD->tmp.h[i]); \
			     setVar(*valTermRef(LD->tmp.h[i])); \
			   } while(0)


#define QueryFromQid(qid)	((QueryFrame) valTermRef(qid))
#define QidFromQuery(f)		(consTermRef(f))
#define QID_EXPORT_WAM_TABLE	(qid_t)(-1)

#define PL_ARITY_AS_SIZE
#include "SWI-Prolog.h"


		 /*******************************
		 *	       SIGNALS		*
		 *******************************/

#if HAVE_SIGNAL
#define MAXSIGNAL		64	/* highest system signal number */
#define SIG_PROLOG_OFFSET	32	/* Start of Prolog signals */

typedef RETSIGTYPE (*handler_t)(int);

typedef struct
{ handler_t   saved_handler;		/* Original handler */
  handler_t   handler;			/* User signal handler */
  predicate_t predicate;		/* Prolog handler */
  int	      flags;			/* PLSIG_*, defined in pl-setup.c */
} sig_handler, *SigHandler;
#endif /* HAVE_SIGNAL */

#define SIG_EXCEPTION	  (SIG_PROLOG_OFFSET+0)
#ifdef O_ATOMGC
#define SIG_ATOM_GC	  (SIG_PROLOG_OFFSET+1)
#endif
#define SIG_GC		  (SIG_PROLOG_OFFSET+2)
#ifdef O_PLMT
#define SIG_THREAD_SIGNAL (SIG_PROLOG_OFFSET+3)
#endif
#define SIG_CLAUSE_GC	  (SIG_PROLOG_OFFSET+4)
#define SIG_PLABORT	  (SIG_PROLOG_OFFSET+5)


		 /*******************************
		 *	      EVENTS		*
		 *******************************/

typedef enum pl_event_type
{ PLEV_ABORT,				/* Execution aborted */
  PLEV_ERASED_CLAUSE,			/* clause was erased */
  PLEV_ERASED_RECORD,			/* record was erased */
  PLEV_DEBUGGING,			/* changed debugging mode */
  PLEV_TRACING,				/* changed tracing mode */
  PLEV_SPY,				/* changed spypoint */
  PLEV_BREAK,				/* a break-point was set */
  PLEV_NOBREAK,				/* a break-point was cleared */
  PLEV_FRAMEFINISHED,			/* A watched frame was discarded */
  PL_EV_THREADFINISHED			/* A thread has finished */
} pl_event_type;


		 /*******************************
		 *	       COMPARE		*
		 *******************************/

/* Results from comparison operations.  Mostly used by compareStandard() */

#define CMP_COMPOUND -3			/* compare_primitive */
#define CMP_ERROR    -2			/* Error (out of memory) */
#define CMP_LESS     -1			/* < */
#define CMP_EQUAL     0			/* == */
#define CMP_GREATER   1			/* > */
#define CMP_NOTEQ     2			/* \== */

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
	  type		max;		/* allocated maximum */		    \
	  size_t	size_limit;	/* Max size the stack can grow to */\
	  size_t	gced_size;	/* size after last GC */	    \
	  size_t	small;		/* Do not GC below this size */	    \
	  size_t	spare;		/* Current reserved area */	    \
	  size_t	def_spare;	/* Desired reserved area */	    \
	  size_t	min_free;	/* Free left when trimming */	    \
	  bool		gc;		/* Can be GC'ed? */		    \
	  int		factor;		/* How eager we are */		    \
	  int		policy;		/* Time, memory optimization */	    \
	  int	        overflow_id;	/* OVERFLOW_* */		    \
	  const char   *name;		/* Symbolic name of the stack */    \
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

#define gBase	(LD->stacks.global.base)
#define gTop	(LD->stacks.global.top)
#define gMax	(LD->stacks.global.max)

#define aBase	(LD->stacks.argument.base)
#define aTop	(LD->stacks.argument.top)
#define aMax	(LD->stacks.argument.max)

#define tSpare	(LD->stacks.trail.spare)

#define onStack(name, addr) \
	((char *)(addr) >= (char *)LD->stacks.name.base && \
	 (char *)(addr) <  (char *)LD->stacks.name.top)
#define onStackArea(name, addr) \
	((char *)(addr) >= (char *)LD->stacks.name.base && \
	 (char *)(addr) <  (char *)LD->stacks.name.max)
#define onTrailArea(addr) \
	((char *)(addr) >= (char *)tBase && \
	 (char *)(addr) <  (char *)tMax + tSpare)
#define onGlobalArea(addr) \
	((char *)(addr) >= (char *)gBase && \
	 (char *)(addr) <  (char *)lBase)
#define usedStackP(s) ((intptr_t)((char *)(s)->top - (char *)(s)->base))
#define sizeStackP(s) ((intptr_t)((char *)(s)->max - (char *)(s)->base))
#define roomStackP(s) ((intptr_t)((char *)(s)->max - (char *)(s)->top))
#define spaceStackP(s) (limitStackP(s)-usedStackP(s))
#define limitStackP(s) ((intptr_t)((s)->size_limit))
#define narrowStackP(s) (roomStackP(s) < (intptr_t)(s)->minfree)

#define usedStack(name) usedStackP(&LD->stacks.name)
#define sizeStack(name) sizeStackP(&LD->stacks.name)
#define roomStack(name) roomStackP(&LD->stacks.name)
#define spaceStack(name) spaceStackP(&LD->stacks.name)
#define limitStack(name) limitStackP(&LD->stacks.name)
#define narrowStack(name) narrowStackP(&LD->stacks.name)

#define GROW_TRIM ((size_t)-1)

#define	LOCAL_OVERFLOW	  (-1)
#define	GLOBAL_OVERFLOW	  (-2)
#define	TRAIL_OVERFLOW	  (-3)
#define	ARGUMENT_OVERFLOW (-4)
#define	MEMORY_OVERFLOW   (-5)		/* out of malloc()-heap */

#define ALLOW_NOTHING	0x0
#define ALLOW_GC	0x1		/* allow GC on stack overflow */
#define ALLOW_SHIFT	0x2		/* allow shift on stack overflow */
#define ALLOW_CHECKED	0x4		/* we already verified space */
#define ALLOW_RETCODE	0x8		/* do not allow anything; return status */

typedef enum
{ STACK_OVERFLOW_RAISE,
  STACK_OVERFLOW_THROW
} stack_overflow_action;

#define pushArgumentStack(p) \
	do { if ( likely(aTop+1 < aMax) ) \
	       *aTop++ = (p); \
	     else \
	       pushArgumentStack__LD((p) PASS_LD); \
	   } while(0)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
hasGlobalSpace(n) is true if we have enough space to create an object of
size N on the global stack AND  can   use  bindConst()  to bind it to an
(attributed) variable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define BIND_GLOBAL_SPACE (7)
#define BIND_TRAIL_SPACE (6)
#define hasGlobalSpace(n) \
	(likely(gTop+(n)+BIND_GLOBAL_SPACE <= gMax) && \
	 likely(tTop+BIND_TRAIL_SPACE <= tMax))
#define overflowCode(n) \
	( (gTop+(n)+BIND_GLOBAL_SPACE > gMax) ? GLOBAL_OVERFLOW \
					      : TRAIL_OVERFLOW )


		 /*******************************
		 *	     NUMBERVARS		*
		 *******************************/

typedef enum
{ AV_BIND,
  AV_SKIP,
  AV_ERROR
} av_action;

#define NV_ERROR (PLMINTAGGEDINT-1)

typedef struct
{ functor_t functor;			/* Functor to use ($VAR/1) */
  intptr_t  offset;			/* offset */
  av_action on_attvar;			/* How to handle attvars */
  int	    singletons;			/* Write singletons as $VAR('_') */
  int	    numbered_check;		/* Check for already numbered */
} nv_options;

#define BEGIN_NUMBERVARS(save) \
	{ fid_t _savedf; \
	  if ( save ) \
	  { _savedf = LD->var_names.numbervars_frame; \
	    LD->var_names.numbervars_frame = PL_open_foreign_frame(); \
	  }
#define END_NUMBERVARS(save) \
          if ( save ) \
	  { PL_discard_foreign_frame(LD->var_names.numbervars_frame); \
	    LD->var_names.numbervars_frame = _savedf; \
	  } \
	}


		 /*******************************
		 *	      WAKEUP		*
		 *******************************/

#define WAKEUP_STATE_WAKEUP          0x1 /* State contains a wakeup */
#define WAKEUP_STATE_EXCEPTION	     0x2 /* State contains an exception */
#define WAKEUP_STATE_SKIP_EXCEPTION  0x4 /* Do not restore exception from state */
#define WAKEUP_KEEP_URGENT_EXCEPTION 0x8 /* Keep the most urgent exception */

typedef struct wakeup_state
{ fid_t		fid;			/* foreign frame reference */
  Stack		outofstack;		/* Stack we are out of */
  int		flags;			/* WAKEUP_STATE_* */
} wakeup_state;




		 /*******************************
		 *	    STREAM I/O		*
		 *******************************/

#define REDIR_MAGIC 0x23a9bef3

typedef struct redir_context
{ int		magic;			/* REDIR_MAGIC */
  IOSTREAM     *stream;			/* temporary output */
  int		is_stream;		/* redirect to stream */
  int		redirected;		/* output is redirected */
  term_t	term;			/* redirect target */
  int		out_format;		/* output type */
  int		out_arity;		/* 2 for difference-list versions */
  size_t	size;			/* size of I/O buffer */
  char	       *data;			/* data written */
  char		buffer[1024];		/* fast temporary buffer */
} redir_context;


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
#define decode(c)   ((code) (dewam_table[(uintptr_t)(c) - \
					 dewam_table_offset]))
#else /* VMCODE_IS_ADDRESS */
#define encode(wam) (wam)
#define decode(wam) (wam)
#endif /* VMCODE_IS_ADDRESS */

		/********************************
		*            STATUS             *
		*********************************/

typedef struct
{ int		blocked;		/* GC is blocked now */
  bool		active;			/* Currently running? */
  long		collections;		/* # garbage collections */
  int64_t	global_gained;		/* global stack bytes collected */
  int64_t	trail_gained;		/* trail stack bytes collected */
  int64_t	global_left;		/* global stack bytes left after GC */
  int64_t	trail_left;		/* trail stack bytes left after GC */
  double	time;			/* time spent in collections */
} pl_gc_status_t;


typedef struct
{ int		blocked;		/* No shifts allowed */
  double	time;			/* time spent in stack shifts */
  int		local_shifts;		/* Shifts of the local stack */
  int		global_shifts;		/* Shifts of the global stack */
  int		trail_shifts;		/* Shifts of the trail stack */
} pl_shift_status_t;


		/********************************
		*            MODULES            *
		*********************************/

#define MODULE_user	(GD->modules.user)
#define MODULE_system	(GD->modules.system)
#define MODULE_parse	(ReadingSource ? LD->modules.source \
				       : MODULE_user)


		/********************************
		*         PREDICATES            *
		*********************************/

#define PROCEDURE_catch3		(GD->procedures.catch3)
#define PROCEDURE_reset3		(GD->procedures.reset3)
#define PROCEDURE_true0			(GD->procedures.true0)
#define PROCEDURE_fail0			(GD->procedures.fail0)
#define PROCEDURE_event_hook1		(GD->procedures.event_hook1)
#define PROCEDURE_print_message2	(GD->procedures.print_message2)
#define PROCEDURE_dcall1		(GD->procedures.dcall1)
#define PROCEDURE_setup_call_catcher_cleanup4 \
				(GD->procedures.setup_call_catcher_cleanup4)
#define PROCEDURE_dwakeup1		(GD->procedures.dwakeup1)
#define PROCEDURE_dthread_init0		(GD->procedures.dthread_init0)
#define PROCEDURE_exception_hook4	(GD->procedures.exception_hook4)
#define PROCEDURE_dc_call_prolog	(GD->procedures.dc_call_prolog0)
#define PROCEDURE_dinit_goal		(GD->procedures.dinit_goal3)

extern const code_info codeTable[]; /* Instruction info (read-only) */

		 /*******************************
		 *	  TEXT PROCESSING	*
		 *******************************/

typedef enum
{ CVT_ok = 0,				/* Conversion ok */
  CVT_wide,				/* Conversion needs wide characters */
  CVT_partial,				/* Input list is partial */
  CVT_nolist,				/* Input list is not a list */
  CVT_nocode,				/* List contains a non-code */
  CVT_nochar,				/* List contains a non-char */
  CVT_representation			/* List contains non-reprentable code */
} CVT_status;

typedef struct
{ CVT_status status;
  word culprit;				/* for CVT_nocode/CVT_nochar */
} CVT_result;


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
#define ACTION_ABORT	5		/* only for Prolog interception */

#define CALL_PORT	0x001		/* port masks */
#define EXIT_PORT	0x002
#define FAIL_PORT	0x004
#define REDO_PORT	0x008
#define UNIFY_PORT	0x010
#define CUT_CALL_PORT   0x040
#define CUT_EXIT_PORT   0x080
#define EXCEPTION_PORT	0x100
#define CUT_PORT	(CUT_CALL_PORT|CUT_EXIT_PORT)
#define PORT_MASK	0x1ff

/* keep in sync with style_name/1 in boot/prims.pl */

#define SINGLETON_CHECK	    0x0002	/* read/1: check singleton vars */
#define MULTITON_CHECK	    0x0004	/* read/1: check multiton vars */
#define DISCONTIGUOUS_STYLE 0x0008	/* warn on discontiguous predicates */
/* reserved		    0x0010 */
#define CHARSET_CHECK	    0x0020	/* warn on unquoted characters */
#define SEMSINGLETON_CHECK  0x0040	/* Semantic singleton checking */
#define NOEFFECT_CHECK	    0x0080	/* Check for meaningless statements */
#define VARBRANCH_CHECK	    0x0100	/* warn on unbalanced variables */

/* checkDataEx() flags */

#define CHK_DATA_NOATTVAR_CHAIN 0x001	/* attvars might not be on chain */

typedef struct debuginfo
{ size_t	skiplevel;		/* current skip level */
  bool		tracing;		/* are we tracing? */
  debug_type	debugging;		/* are we debugging? */
  int		leashing;		/* ports we are leashing */
  int	        visible;		/* ports that are visible */
  bool		showContext;		/* tracer shows context module */
  int		styleCheck;		/* source style checking */
  int		suspendTrace;		/* tracing is suspended now */
  intptr_t	retryFrame;		/* Frame to retry (local stack offset) */
} pl_debugstatus_t;

#define FT_ATOM		0		/* atom feature */
#define FT_BOOL		1		/* boolean feature (true, false) */
#define FT_INTEGER	2		/* integer feature */
#define FT_FLOAT	3		/* float feature */
#define FT_TERM		4		/* term feature */
#define FT_INT64	5		/* passed as int64_t */
#define FT_FROM_VALUE	0x0f		/* Determine type from value */
#define FT_MASK		0x0f		/* mask to get type */

#define PLFLAG_CHARESCAPE	    0x00000001 /* handle \ in atoms */
#define PLFLAG_GC		    0x00000002 /* do GC */
#define PLFLAG_TRACE_GC		    0x00000004 /* verbose gc */
#define PLFLAG_TTY_CONTROL	    0x00000008 /* allow for tty control */
//				    0x00000010 /* not used */
#define PLFLAG_DEBUG_ON_ERROR	    0x00000020 /* start tracer on error */
#define PLFLAG_REPORT_ERROR	    0x00000040 /* print error message */
#define PLFLAG_FILE_CASE	    0x00000080 /* file names are case sensitive */
#define PLFLAG_FILE_CASE_PRESERVING 0x00000100 /* case preserving file names */
#define PLFLAG_DOS_FILE_NAMES       0x00000200 /* dos (8+3) file names */
#define ALLOW_VARNAME_FUNCTOR	    0x00000400 /* Read Foo(x) as 'Foo'(x) */
#define PLFLAG_ISO		    0x00000800 /* Strict ISO compliance */
#define PLFLAG_OPTIMISE		    0x00001000 /* -O: optimised compilation */
#define PLFLAG_FILEVARS		    0x00002000 /* Expand $var and ~ in filename */
#define PLFLAG_AUTOLOAD		    0x00004000 /* do autoloading */
#define PLFLAG_CHARCONVERSION	    0x00008000 /* do character-conversion */
#define PLFLAG_LASTCALL		    0x00010000 /* Last call optimization enabled? */
//				    0x00020000 /* not used */
#define PLFLAG_SIGNALS		    0x00040000 /* Handle signals */
#define PLFLAG_DEBUGINFO	    0x00080000 /* generate debug info */
#define PLFLAG_FILEERRORS	    0x00100000 /* Edinburgh file errors */
#define PLFLAG_WARN_OVERRIDE_IMPLICIT_IMPORT 0x00200000 /* Warn overriding weak symbols */
#define PLFLAG_QUASI_QUOTES	    0x00400000 /* Support quasi quotes */
#define PLFLAG_DOT_IN_ATOM	    0x00800000 /* Allow atoms a.b.c */
#define PLFLAG_VARPREFIX	    0x01000000 /* Variable must start with _ */
#define PLFLAG_PROTECT_STATIC_CODE  0x02000000 /* Deny clause/2 on static code */
#define PLFLAG_ERROR_AMBIGUOUS_STREAM_PAIR 0x04000000
#define PLFLAG_GCTHREAD		    0x08000000 /* Do atom/clause GC in a thread */

typedef struct
{ unsigned int flags;		/* Fast access to some boolean Prolog flags */
} pl_features_t;

#define truePrologFlag(flag)	  true(&LD->prolog_flag.mask, flag)
#define setPrologFlagMask(flag)	  set(&LD->prolog_flag.mask, flag)
#define clearPrologFlagMask(flag) clear(&LD->prolog_flag.mask, flag)

typedef enum
{ OCCURS_CHECK_FALSE = 0,	/* allow rational trees */
  OCCURS_CHECK_TRUE,		/* fail if rational tree would result */
  OCCURS_CHECK_ERROR		/* exception if rational tree would result */
} occurs_check_t;

typedef enum
{ ACCESS_LEVEL_USER = 0,	/* Default user view */
  ACCESS_LEVEL_SYSTEM		/* Allow low-level access */
} access_level_t;

#define SYSTEM_MODE	    (LD->prolog_flag.access_level == ACCESS_LEVEL_SYSTEM)

#ifdef O_LIMIT_DEPTH
#define DEPTH_NO_LIMIT	(~(uintptr_t)0x0) /* Highest value */
#endif

#ifdef O_INFERENCE_LIMIT
#define INFERENCE_NO_LIMIT (~((int64_t)1<<63)) /* Highest value */
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

#if defined(_DEBUG) && !defined(O_MAINTENANCE)
#define O_MAINTENANCE
#endif

#include "os/pl-os.h"			/* OS dependencies */

#ifdef SYSLIB_H
#include SYSLIB_H
#endif

#define NULL_ATOM ((atom_t)0)
#define MK_ATOM(n)		((atom_t)((n)<<7|TAG_ATOM|STG_STATIC))
#include "pl-atom.ih"
#include "pl-funct.ih"

#include "pl-alloc.h"			/* Allocation primitives */
#include "pl-init.h"			/* Declarations needed by pl-init.c */
#include "pl-error.h"			/* Exception generation */
#include "pl-thread.h"			/* thread manipulation */
#include "pl-data.h"			/* Access Prolog data */
#include "pl-segstack.h"		/* Segmented stacks */
#include "pl-gmp.h"			/* GNU-GMP support */
#include "os/pl-locale.h"		/* Locale objects */
#include "os/pl-file.h"			/* Stream management */
#include "pl-global.h"			/* global data */
#include "pl-funcs.h"			/* global functions */
#include "pl-ldpass.h"			/* Wrap __LD functions */
#include "pl-inline.h"			/* Inline facilities */
#include "pl-privitf.h"			/* private foreign interface */
#include "os/pl-text.h"			/* text manipulation */
#include "pl-hash.h"			/* Murmurhash function */
#include "os/pl-option.h"		/* Option processing */
#include "os/pl-files.h"		/* File management */
#include "os/pl-string.h"		/* Basic string functions */
#include "pl-ressymbol.h"		/* Meta atom handling */

#ifdef __DECC				/* Dec C-compiler: avoid conflicts */
#undef leave
#undef except
#undef try
#endif

#endif /*_PL_INCLUDE_H*/
