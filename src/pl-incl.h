/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: SWI-Prolog general include file
*/

#ifndef _PL_INCLUDE_H
#define _PL_INCLUDE_H

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
First, include config.h or, if MD is  specified, this file.  This allows
for -DMD="md-mswin.h"
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef CONFTEST
#ifdef MD
#include MD
#else
#include "config.h"
#endif
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
      Include evaluatable Prolog functions into the arithmetic module.
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define PL_KERNEL		1
#define O_COMPILE_OR		1
#define O_COMPILE_ARITH		1
#define O_STRING		1
#define O_PROLOG_FUNCTIONS	1
#define O_BLOCK			1
#define O_PCE			1	/* doesn't depend any longer */

#ifndef O_LABEL_ADDRESSES
#if __GNUC__ == 2
#define O_LABEL_ADDRESSES	1
#ifndef VMCODE_IS_ADDRESS
#define VMCODE_IS_ADDRESS	1
#endif
#endif
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The macros below try to establish a common basis for various  compilers,
so  we  can  write  most  of the real code without having to worry about
compiler limits and differences.

The  current  version  has  prototypes  defined   for   all   functions.
Unfortunately  there  are  still a lot of old compilers around and it is
hard to write and maintain code that runs on both old and new compilers.
This has worked on TURBO_C not very long ago.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if defined(__unix__) && !defined(unix)
#define unix 1
#endif

#include <stdio.h>
#if O_XOS
#include <xos/xos.h>
#endif

#include <sys/types.h>
#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <stdarg.h>

#ifdef HAVE_SIGNAL
#include <signal.h>
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A common basis for C keywords.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if !__GNUC__ || __STRICT_ANSI__
#define volatile		/* volatile functions do not return */
#define inline			/* inline functions are integrated in */
				/* their caller */
#define signed			/* some compilers don't have this. */
#endif

#if __STRICT_ANSI__
#undef TAGGED_LVALUE
#define ASM_NOP { static int nop; nop++; }
#endif

#define forwards static		/* forwards function declarations */

#ifndef GLOBAL			/* global variables */
#define GLOBAL extern
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Booleans,  addresses,  strings  and other   goodies.   Note that  ANSI
compilers have  `Void'.   This should  be  made part  of  the  general
platform as well.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef int			bool;
typedef double			real;
typedef void *			Void;

#if HAVE_SIGNAL
#define MAXSIGNAL		32	/* highest system signal number */

typedef RETSIGTYPE (*handler_t)(int);
typedef void *SignalContext;		/* struct sigcontext on sun */

GLOBAL struct
{ handler_t os;				/* Os signal handler */
  handler_t user;			/* User signal handler */
  bool catched;				/* Prolog catches this one */
} signalHandlers[MAXSIGNAL];
#endif /* HAVE_SIGNAL */

#ifndef TRUE
#define TRUE			1
#define FALSE			0
#endif
#define succeed			return TRUE
#define fail			return FALSE
#define TRY(goal)		{ if ((goal) == FALSE) fail; }

typedef char *			caddress;

#define EOS			('\0')
#define ESC			((char) 27)
#define streq(s, q)		((strcmp((s), (q)) == 0))

#ifndef abs
#define abs(x)			((x) < 0 ? -(x) : (x))
#endif
				/* n is 2^m !!! */
#define ROUND(p, n)		((((p) + (n) - 1) & ~((n) - 1)))
#define addPointer(p, n)	((Void) ((char *)(p) + (long)(n)))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			     LIMITS

Below are some arbitrary limits on object sizes.  Feel free  to  enlarge
them,  but  be aware of the fact that this increases memory requirements
and  slows  down  for  some  of  these  options.    Also,   MAXARITY   <
MAXVARIABLES, MAXVARIABLES and MAXEXTERNALS must be lower that 32 K. One
day,  I  should  try  to  get  rid  of these limits.  This requires some
redesign of parts of the compiler.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define LINESIZ			1024	/* size of a data line */
#define MAXARITY		128	/* arity of predicate */
#define MAXVARIABLES		256	/* number of variables/clause */
#define MAXEXTERNALS	    ((1<<15)-1)	/* external references of a clause */

				/* Prolog's largest int */
#define PLMAXINT		((1L<<(32 - MASK_BITS - LMASK_BITS - 1)) - 1)
				/* Prolog's smallest int */
#define PLMININT		(-(1L<<(32 - MASK_BITS - LMASK_BITS - 1)))
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
#define PROCEDUREHASHSIZE	512	/* predicates in module user */
#define MODULEPROCEDUREHASHSIZE 128	/* predicates in other modules */
#define RECORDHASHSIZE		512	/* global recorda/recordz table */
#define MODULEHASHSIZE		64	/* global module table */
#define PUBLICHASHSIZE		32	/* Module export table */
#define OPERATORHASHSIZE	256	/* global operator table */
#define FLAGHASHSIZE		256	/* global flag/3 table */
#define ARITHHASHSIZE		64	/* arithmetic function table */

#define pointerHashValue(p, size) ((int)(((long)(p)>>2) & ((size)-1)))

#define for_table(s, t) for(s = firstHTable(t); s; s = nextHTable(t, s))
#define return_next_table(t, v) \
	{ for((v) = (v)->next; isRef((word)(v)) && (v); (v) = *((t *)unRef(v))) \
	  if ( (v) == (t)NULL ) \
	    succeed; \
	  ForeignRedo(v); \
	}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Foreign language interface definitions.  Note that these macros MUST  be
consistent  with  the  definitions  in  pl-itf.h, which is included with
users foreign language code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef DATA_AT_0X4
#define FRG_MASK	(0x20000000L)		/* Mask to indicate redo */
#define FRG_BITS	3
#else
#define FRG_MASK	(0x40000000L)		/* Mask to indicate redo */
#define FRG_BITS	2
#endif
#define FRG_CUT 	(0x80000000L)		/* highest bit */
#define FRG_MASK_MASK	(FRG_CUT|FRG_MASK)

#define FRG_FIRST_CALL	(0)
#define FRG_CUTTED	(1)
#define FRG_REDO	(2)

#define FIRST_CALL	(0L)

#define ForeignRedo(v)		return (word) (((long)(v) & ~FRG_MASK_MASK) \
					      | FRG_MASK)
#define ForeignControl(h)	((h) == FIRST_CALL ? FRG_FIRST_CALL : \
				 (h) & FRG_CUT	   ? FRG_CUTTED : \
						     FRG_REDO)
#define ForeignContext(h)	(((long)(h) << FRG_BITS) >> FRG_BITS)
#define ForeignContextAddress(h) ((Void)((long)(h) & ~FRG_MASK_MASK))

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
#define H_FUNCTOR	((code) 6)
#define I_POP		((code) 7)		/* end functor */
#define I_POPN		((code) 8)		/* end functor */
#define B_VAR		((code) 9)		/* variable */
#define H_VAR		((code)10)
#define B_CONST		((code)11)		/* constant (atomic) */
#define H_CONST		((code)12)
#define H_REAL		((code)13)		/* real in the head */
#if O_STRING
#define H_STRING	((code)14)		/* string in the head */
#endif /* O_STRING */

#define B_FIRSTVAR	((code)15)		/* first occurrence of var */
#define H_FIRSTVAR	((code)16)
#define B_VOID		((code)17)		/* anonimous variables */
#define H_VOID		((code)18)
#define B_ARGFIRSTVAR	((code)19)		/* body vars nested in functor */
#define B_ARGVAR	((code)20)

#define H_NIL		((code)21)		/* [] in the head */
#define H_CONST0	((code)22)		/* H_CONST 0, etc. */
#define H_CONST1	((code)23)
#define H_CONST2	((code)24)

#define H_LIST		((code)25)		/* ./2 in the head */
#define H_FUNCTOR0	((code)26)		/* H_FUNCTOR 0, etc. */
#define H_FUNCTOR1	((code)27)
#define H_FUNCTOR2	((code)28)

#define B_VAR0		((code)29)		/* B_VAR 0 */
#define B_VAR1		((code)30)		/* B_VAR 1 */
#define B_VAR2		((code)31)		/* B_VAR 2 */

#define H_SINT		((code)32)		/* Small integer in the Head */
#define B_SINT		((code)33)		/* Small integer in the Body */

#define I_USERCALL	((code)34)		/* variable in body (call/1) */
#define I_CUT		((code)35)		/* ! */
#define I_APPLY		((code)36)		/* apply/2 */

#if O_COMPILE_ARITH
#define A_FUNC0		((code)37)		/* nullary arithmic function */
#define A_FUNC1		((code)38)		/* unary arithmic function */
#define A_FUNC2		((code)39)		/* binary arithmic function */
#define A_FUNC		((code)40)		/* n-ary arithmic function */
#define A_LT		((code)41)		/* < */
#define A_GT		((code)42)		/* > */
#define A_LE		((code)43)		/* =< */
#define A_GE		((code)44)		/* >= */
#define A_EQ		((code)45)		/* =:= */
#define A_NE		((code)46)		/* =\= */
#define A_IS		((code)47)		/* is */
#endif /* O_COMPILE_ARITH */

#if O_COMPILE_OR
#define C_OR		((code)48)		/* In-clause backtract point */
#define C_JMP		((code)49)		/* Jump over code */
#define C_MARK		((code)50)		/* Sub-clause cut mark */
#define C_CUT		((code)51)		/* cut to corresponding mark */
#define C_IFTHENELSE	((code)52)		/* if-then-else start */
#define C_VAR		((code)53)		/* make a variable */
#define C_END		((code)54)		/* dummy to help decompiler */
#define C_NOT		((code)55)		/* same as C_IFTHENELSE */
#define C_FAIL		((code)56)		/* fail */
#endif /* O_COMPILE_OR */

#define B_REAL		((code)57)		/* REAL in body */
#define B_STRING	((code)58)		/* STRING in body */

#if O_BLOCK
#define I_CUT_BLOCK	((code)59)		/* !(block) */
#define B_EXIT		((code)60)		/* exit(block, rval) */
#endif /*O_BLOCK*/

#define I_HIGHEST	((code)60)		/* largest WAM code !!! */

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
Operator types
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define	OP_FX	0
#define OP_FY	1
#define OP_XF	2
#define OP_YF	3
#define OP_XFX	4
#define OP_XFY	5
#define OP_YFX	6
#define	OP_YFY	7

#define OP_PREFIX  1
#define OP_INFIX   2
#define OP_POSTFIX 3

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Files and streams
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define F_CLOSED	0		/* closed entry */
#define F_READ		1		/* open for reading */
#define F_WRITE		2		/* open for writing */
#define F_APPEND	6		/* open for append writing */

#define streamOutput(stream, goal) \
	{ int SOn = streamNo(stream, F_WRITE); \
	  int SOout = Output; \
	  word SOrval; \
	  if ( SOn < 0 ) fail; \
	  Output = SOn; \
	  SOrval = goal; \
	  Output = SOout; \
	  return SOrval; \
	}

#define streamInput(stream, goal) \
	{ int SOn = streamNo(stream, F_READ); \
	  int SOin = Input; \
	  word SOrval; \
	  if ( SOn < 0 ) fail; \
	  Input = SOn; \
	  SOrval = goal; \
	  Input = SOin; \
	  return SOrval; \
	}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Type fields.  These codes are  included  in  a  number  of  the  runtime
structures  at  a  fixed  point, so the runtime environment can tell the
difference.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define HeapMagic(n)	((n) | 0x25678000)
#define ATOM_TYPE	HeapMagic(1)	/* an atom */
#define FUNCTOR_TYPE	HeapMagic(2)	/* a Functor */
#define PROCEDURE_TYPE	HeapMagic(3)	/* a procedure */
#define RECORD_TYPE	HeapMagic(4)	/* a record list */

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Masks.  Currently the top 3 bits of a word are used as  mask.   The  top
bit  is  reserved  for  references,  which  are  represented as negative
numbers.  At least the 68020 is faster in checking for negative  numbers
and  turning  negative  numbers into positive ones.  This trick gives an
overall performance increase of about 5%. The other two  bits  are  used
for  integers, reals and strings.  Both reals and strings are `indirect'
data types (tagged pointers to the real value).  This  has  consequences
in  unification an similar functions.  Therefore a macro `isIndirect(w)'
has been introduced.  If you decide to change things here make sure this
macro  operates  oppropriately  and  is  FAST  (its  used  in   critical
unification code).

(The RT under AIX uses a somewhat irregular memory model:

0x10000000 text... 0x20000000 ...data...bss...malloc... ...stack 03fffffff

This conflicts with the data representation used for SUN.  So we put the
tag bits on bits 32, 31 and 29 instead of 32, 31 and 30.   This  reduces
the range of integers to +- 2^25.  (Macros have to be rewritten))
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define REF_MASK	0x80000000L	/* Reference (= negative) */
#define MARK_MASK	0x00000001L	/* GC marking bit */

#if O_16_BITS
#define INDIRECT_MASK	0x40000000L	/* Indirect constant */
#define INT_MASK	0x20000000L	/* Integer constant */
#define MASK_BITS	4		/* high order mask bits */
#define LMASK_BITS	1		/* low order mask bits */
#define DMASK_BITS	5		/* DATA_TAG_MASK bits */
#define FIRST_MASK	0x10000000L	/* first member of relocation chain */
#define STRING_MASK	0x60000000L	/* Header mask on global stack */
#define REAL_MASK	0x68000000L	/* Header mask on global stack */
#define MASK_MASK	(INT_MASK|REF_MASK|INDIRECT_MASK|FIRST_MASK)
#define DATA_TAG_MASK	0xf8000000L	/* Indirect data type mask */

#else /* !O_16_BITS */

#define REAL_MASK	0x70000000L	/* Header mask on global stack */

#ifdef DATA_AT_0X4
#define INDIRECT_MASK	0x20000000L	/* Indirect constant */
#define INT_MASK	0x10000000L	/* Integer constant */
#define SIGN_MASK	0x40000000L     /* Sign of an integer */
#define MASK_BITS	3		/* high order mask bits */
#define SIGN_OFFSET     1               /* Offset of sign bit from M.S. bit */
#define STRING_MASK	0x30000000L	/* Header mask on global stack */
#else
#ifdef DATA_AT_0X2
#define INDIRECT_MASK	0x40000000L	/* Indirect constant */
#define INT_MASK	0x10000000L	/* Integer constant */
#define MASK_BITS	4		/* high order mask bits */
#define STRING_MASK	0x50000000L	/* Header mask on global stack */
#else /* 0X1 or lower */
#define INDIRECT_MASK	0x40000000L	/* Indirect constant */
#define INT_MASK	0x20000000L	/* Integer constant */
#define MASK_BITS	3		/* high order mask bits */
#define STRING_MASK	0x60000000L	/* Header mask on global stack */
#endif /* DATA_AT_0X2 */
#endif /* DATA_AT_0X4 */

#define makeRef(p)	((word)(-(long)(p)))
#define unRef(w)	((Word)(-(long)(w)))
#define isRef(w)	((long)(w) < 0)

#ifdef DATA_AT_0X8

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Masking schema for system which have their malloc()-area in the negative
addresses (>= 0x80000000L).  We basically use  the same masking strategy
as for low addresses (< 0x20000000L), but  we place the reference tag on
0x10000000L.  NOT TESTED!!!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#undef REF_MASK				/* redefine! */
#undef MASK_BITS
#undef REAL_MASK
#undef makeRef
#undef unRef
#undef isRef

#define REF_MASK	0x10000000L	/* put here */
#define MASK_BITS	4
#define REAL_MASK	0xe0000000L	/* real on indirect value */

#define makeRef(p)	((word)(p) | REF_MASK)
#define unRef(w)	((Word)((word)(w) & ~REF_MASK))
#define isRef(w)	((word)(w) & REF_MASK)
#endif /* DATA_AT_0X8 */

#define LMASK_BITS	2		/* low order mask bits */
#define DMASK_BITS	4		/* DATA_TAG_MASK bits */
#define FIRST_MASK	0x00000002L	/* first member of relocation chain */
#define MASK_MASK	(INT_MASK|REF_MASK|INDIRECT_MASK)
#define DATA_TAG_MASK	0xf0000000L	/* Indirect data type mask */

#endif /* O_16_BITS */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Common Prolog objects typedefs.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef unsigned long		word;		/* Anonimous 4 byte object */
typedef word *			Word;		/* a pointer to anything */
typedef unsigned short		code;		/* bytes codes */
typedef code *			Code;		/* pointer to byte codes */
typedef int			Char;		/* char that can pass EOF */
typedef word			(*Func)();	/* foreign functions */
typedef unsigned long		lock;		/* foreign lock */
typedef lock *			Lock;		/* pointer to a lock */

typedef struct atom *		Atom;		/* atom */
typedef struct functor *	Functor;	/* complex term */
typedef struct functorDef *	FunctorDef;	/* name/arity pair */
typedef struct procedure *	Procedure;	/* predicate */
typedef struct definition *	Definition;	/* predicate definition */
typedef struct clause *		Clause;		/* compiled clause */
typedef struct code_info *	CodeInfo;	/* WAM op-code info */
typedef struct operator *	Operator;	/* see pl-op.c, pl-read.c */
typedef struct record *		Record;		/* recorda/3, etc. */
typedef struct recordList *	RecordList;	/* list of these */
typedef struct module *		Module;		/* predicate modules */
typedef struct sourceFile *	SourceFile;	/* file adminitration */
typedef struct table *		Table;		/* (numeric) hash table */
typedef struct symbol *		Symbol;		/* symbol of hash table */
typedef struct localFrame *	LocalFrame;	/* environment frame */
typedef struct trail_entry *	TrailEntry;	/* Entry of train stack */
typedef struct data_mark	mark;		/* backtrack mark */
typedef struct index *		Index;		/* clause indexing */
typedef struct stack *		Stack;		/* machine stack */

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

#define NONDETERMINISTIC	(0x0001) /* predicate */
#define DISCONTIGUOUS		(0x0002) /* predicate */
#define DYNAMIC			(0x0004) /* predicate */
#define FOREIGN			(0x0008) /* predicate */
#define HIDE_CHILDS		(0x0010) /* predicate */
#define MULTIFILE		(0x0020) /* predicate */
#define PROFILE_TICKED		(0x0040) /* predicate */
#define SPY_ME			(0x0080) /* predicate */
#define SYSTEM			(0x0100) /* predicate */
#define TRACE_ME		(0x0200) /* predicate */
#define TRANSPARENT		(0x0400) /* predicate */
#define GC_SAVE			(0x0800) /* predicate */
#define TRACE_CALL		(0x1000) /* predicate */
#define TRACE_REDO		(0x2000) /* predicate */
#define TRACE_EXIT		(0x4000) /* predicate */
#define TRACE_FAIL		(0x8000) /* predicate */

#define ERASED			(0x0001) /* clause */
#define UNKNOWN			(0x0002) /* module */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Handling environment (or local stack) frames.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define FR_LEVEL		(0xFFFFFFF0L)
#define FR_CUT			(0x00000001L)
#define FR_NODEBUG		(0x00000002L)
#define FR_SKIPPED		(0x00000004L)
#define FR_MARKED		(0x00000008L)

#define ARGOFFSET		((int) sizeof(struct localFrame))

#define setLevelFrame(fr, l)	{ (fr)->flags &= ~FR_LEVEL;   \
				  (fr)->flags |= ((l) << 4); \
				}
#define levelFrame(fr)		(fr->flags >> 4)
#define incLevel(fr)		(fr->flags += 0x10)
#define argFrameP(f, n)		((Word)((f)+1) + (n))
#define argFrame(f, n)		(*argFrameP((f), (n)) )
#define varFrameP(f, n)		((Word)(f) + (n))
#define varFrame(f, n)		(*varFrameP((f), (n)) )
#define parentFrame(f)		((f)->parent ? (f)->parent\
					     : (LocalFrame)varFrame((f), -1))
#define slotsFrame(f)		(true((f)->procedure->definition, FOREIGN) ? \
				      (f)->procedure->functor->arity : \
				      (f)->clause->slots)
#define contextModule(f)	((f)->context)

#define leaveClause(clause) { if ( --clause->references == 0 && \
				   true(clause, ERASED) ) \
				unallocClause(clause); \
			    }

#define leaveFrame(fr) { if ( true(fr->procedure->definition, FOREIGN) ) \
			   leaveForeignFrame(fr); \
			 else \
			 { if ( fr->clause ) \
			     leaveClause(fr->clause); \
			 } \
		       }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Macros to turn pointers into Prolog integers and  vice-versa.   Used  to
pass  references  for  recorda,  erase, clause/3, etc.  As AIX addresses
range from 0x2000000, which is above the maximum integer value  for  the
AIX  version  we  substract  this value and add it again when converting
integers to pointers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef DATA_AT_0X8
#define PTR_TO_NUM_OFFSET	  0x80000000L
#else
#  ifdef DATA_AT_0X4
#    define PTR_TO_NUM_OFFSET	  0x40000000L
#  else
#    ifdef DATA_AT_0X2
#      define PTR_TO_NUM_OFFSET	  0x20000000L
#    else
#      ifdef DATA_AT_OX1
#        define PTR_TO_NUM_OFFSET 0x10000000L
#      else
#        define PTR_TO_NUM_OFFSET 0x0L
#      endif
#    endif
#  endif
#endif

#define pointerToNum(p)	   consNum(((long)(p)-PTR_TO_NUM_OFFSET)/sizeof(int))
#define numToPointer(n)	   ((Word)(valNum(n)*sizeof(int)+PTR_TO_NUM_OFFSET))


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Macros to handle the anonimous types.  'w' implies we expect a word, 'p'
for a pointer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define unMask(w)		((w) & ~MASK_MASK)
#define mask(w)			(w & MASK_MASK)
#define consNum(n)		((word) (unMask((n)<<LMASK_BITS) | INT_MASK))
#ifdef DATA_AT_0X4
#define valNum(w)               ((long) ((((w) & SIGN_MASK) << SIGN_OFFSET) | \
				 (unMask(w) << MASK_BITS)) >> \
				 (MASK_BITS+LMASK_BITS))
#else
#define valNum(w)		((long)((w)<<MASK_BITS)>>(MASK_BITS+LMASK_BITS))
#endif

#define consNumFromCode(c)	consNum((signed short)(c))
#define valReal(w)		unpack_real((Word)unMask(w))
#if O_STRING
#define allocSizeString(l)	(ROUND(l+1, sizeof(word)) + 2 * sizeof(word))
#define valString(w)		((char *)((Word)unMask(w)+1))
#define sizeString(w)		(((long)(*(Word)unMask(w))<<DMASK_BITS)>> \
						(DMASK_BITS+LMASK_BITS))
#define equalString(w1,w2)	(sizeString(w1) == sizeString(w2) && \
				 streq(valString(w1), valString(w2)))
#endif /* O_STRING */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Handling references.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define deRef(p)	{ while(isRef(*(p))) (p) = unRef(*(p)); }
#define deRef2(p, d)	{ (d) = (p); deRef((d)); }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Handling dereferenced arbitrary Prolog runtime objects.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define isMasked(w)	(mask(w) != (word) NULL)
#define isIndirect(w)	((w) & INDIRECT_MASK)
#define isInteger(w)	((w) & INT_MASK)
#define isReal(w)	(isIndirect(w) && \
			 (*((Word)unMask(w)) & DATA_TAG_MASK) == REAL_MASK)
#if O_STRING
#define isString(w)	(isIndirect(w) && \
			 (*((Word)unMask(w)) & DATA_TAG_MASK) == STRING_MASK)
#endif /* O_STRING */
#define isNumber(w)	(isInteger(w) || isReal(w))
#define isVar(w)	((w) == (word) NULL)
#define nonVar(w)	((w) != (word) NULL)
#define isPointer(w)	(nonVar(w) && !isMasked(w))
#define pointerIsAtom(w) (((Atom)(w))->type == ATOM_TYPE)
#define pointerIsFunctor(w) (((Functor)(w))->type == FUNCTOR_TYPE)
#define isAtom(w)	(isPointer(w) && pointerIsAtom(w))
#define isFunctor(w)	(isPointer(w) && pointerIsFunctor(w))
#define stringAtom(w)	(((Atom)(w))->name)
#define isPrimitive(w)  (isVar(w) || isAtomic(w))
#define isAtomic(w)	(nonVar(w) && (isMasked(w) || pointerIsAtom(w)))
#define isTerm(w)	(isPointer(w) && !pointerIsAtom(w))
#define isList(w)	(isPointer(w) && functorTerm(w) == FUNCTOR_dot2)
#define functorTerm(w)	(((Functor)(w))->definition)
#define argTerm(w, n)	(*argTermP((w), (n)))
#define argTermP(w, n)	(((Word)(w)+1+(n)))
#define isProcedure(w)	(((Procedure)(w))->type == PROCEDURE_TYPE)
#define isRecordList(w)	(((RecordList)(w))->type == RECORD_TYPE)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Heuristics functions to determine whether an integer reference passed to
erase and assert/2, clause/3, etc.  really points to a clause or record.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define inCore(a)	((char *)(a) >= hBase && (char *)(a) <= hTop)
#define isClause(c)	(inCore(((Clause)(c))->procedure) && \
			  isProcedure(((Clause)(c))->procedure))
#define isRecord(r)	(inCore(((Record)(r))->list) && \
			  isRecordList(((Record)(r))->list))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
At times an abort is not allowed because the heap  is  inconsistent  the
programmer  should  call  startCritical  to start such a code region and
endCritical to end it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define startCritical { critical++; }
#define endCritical   { if (--critical == 0 && aborted == TRUE) pl_abort(); }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
LIST processing macros.

    isNil(w)		word is the nil list ([]).
    isList(w)		word is a './2' term.
    HeadList(p)		Pointer to the head of list *p (NOT dereferenced).
    TailList(p)		Pointer to the tail of list *p (NOT dereferenced).
    APPENDLIST(l, p)	Append *p to list *l. l points to the tail afterwards.
    CLOSELIST(l)	Unify the tail of the list with [].
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define isNil(w)	((w) == (word) ATOM_nil)
#define HeadList(p)	(argTermP(*(p), 0) )
#define TailList(p)	(argTermP(*(p), 1) )

#define APPENDLIST(l, p) { TRY(unifyFunctor(l, FUNCTOR_dot2) ); \
			   TRY(pl_unify(HeadList(l), p) ); \
			   l = TailList(l); deRef(l); \
			 }
#define CLOSELIST(l)     { TRY(unifyAtomic(l, ATOM_nil)); }


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Structure declarations that must be shared across multiple files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

struct atom
{ Atom		next;		/* next in chain */
  int		type;		/* ATOM_TYPE */
  char *	name;		/* name associated with atom */
};

struct index
{ unsigned long key;		/* key of index */
  unsigned long varmask;	/* variable field mask */
};

struct functorDef
{ FunctorDef	next;		/* next in chain */
  int		type;		/* FUNCTOR_TYPE */
  Atom		name;		/* Name of functor */
  int		arity;		/* arity of functor */
};

struct clause
{ Procedure	procedure;	/* procedure we belong to */
  Clause	next;		/* next clause of procedure */
  Word		externals;	/* External references */
  Code		codes;		/* byte codes of clause */
  struct index	index;		/* index key of clause */
  unsigned int	references;	/* no of. references from interpreter */
  short		code_size;	/* size of byte code array */
  short		subclauses;	/* number of subclauses in body (decompiler) */
  code		XR_size;	/* size of external reference table */
  code		variables;	/* number of variables */
  code		slots;		/* # variables holding Prolog data */
  unsigned short	flags;		/* Flag field holding: */
		/* ERASED	   Clause is retracted, but referenced */
};

struct code_info
{ char		*name;		/* name of the code */
  char		arguments;	/* # arguments code takes */
  code		code;		/* number of the code */
};

struct data_mark
{ TrailEntry	trailtop;	/* top of the trail stack */
  Word		globaltop;	/* top of the global stack */
};

struct functor
{ FunctorDef	definition;	/* Name/Arity */
  word		arguments[1];	/* arguments vector */
};

struct operator
{ Operator	next;		/* next of chain */
  Atom		name;		/* name of operator */
  short		type;		/* OP_FX, ... */
  short		priority;	/* priority of operator */
};

struct procedure
{ FunctorDef	functor;	/* Name/Arity of procedure */
  int		type;		/* PROCEDURE_TYPE */
  Definition	definition;	/* definition of procedure */
};

struct definition
{ union
  { Clause	clauses;		/* clause list of procedure */
    Func	function;		/* function pointer of procedure */
  } definition;
  Clause	lastClause;		/* last clause of list */
  Module	module;			/* module of the predicate */
  SourceFile	source;			/* source file of predicate */
#ifdef O_PROFILE
  int		profile_ticks;		/* profiler: call times active */
  int		profile_calls;		/* profiler: number of calls */
  int		profile_redos;		/* profiler: number of redos */
  int		profile_fails;		/* profiler: number of fails */
#endif /* O_PROFILE */
  unsigned long indexPattern;		/* indexed argument pattern */
  char		indexCardinality;	/* cardinality of index pattern */
  short		line_no;		/* Line number for the predicate */
  short		source_count;		/* times (re)consulted */
  unsigned short	flags;			/* booleans: */
		/*	FOREIGN		   foreign predicate? */
		/*	PROFILE_TICKED	   has been ticked this time? */
		/*	TRACE_ME	   is my call visible? */
		/*	HIDE_CHILDS	   hide childs for the debugger? */
		/*	SPY_ME		   spy point set? */
		/*	DYNAMIC		   dynamic predicate? */
		/*	MULTIFILE	   defined over more files? */
		/*	SYSTEM		   system predicate */
		/*	TRANSPARENT	   procedure transparent to modules */
		/*	DISCONTIGUOUS	   procedure might be discontiguous */
		/*	NONDETERMINISTIC   deterministic foreign (not used) */
		/*	GC_SAVE		   Save to perform GC while active */
		/*	TRACE_CALL	   Trace call-port */
		/*	TRACE_REDO	   Trace redo-port */
		/*	TRACE_EXIT	   Trace exit-port */
		/*	TRACE_FAIL	   Trace fail-port */
};

struct localFrame
{ Code		programPointer;	/* pointer into program */
  LocalFrame	parent;		/* parent local frame */
  Clause	clause;		/* Current clause of frame */
  LocalFrame	backtrackFrame;	/* Frame for backtracking */
  Procedure	procedure;	/* Procedure we are running */
  mark		mark;		/* data backtrack mark */
  Module	context;	/* context module of frame */
  unsigned long flags;		/* packet long holding: */
		/*	LEVEL	   recursion level (28 bits) */
		/*	FR_CUT     has frame been cut ? */
		/*	FR_NODEBUG don't debug this frame ? */
};  

struct record
{ RecordList	list;		/* list I belong to */
  Record	next;		/* next of chain */
  word		term;		/* term associated */
  int		n_vars;		/* number of variables */
  Word		variables;	/* array of variables */
};

struct recordList
{ RecordList	next;		/* next record chain with same key */
  int		type;		/* RECORD_TYPE */
  word		key;		/* key of record */
  Record	firstRecord;	/* first record associated with key */
  Record	lastRecord;	/* last record associated with key */
};

struct sourceFile
{ Atom		name;		/* name of source file */
  SourceFile	next;		/* next of chain */
  int		count;		/* number of times loaded */
  long		time;		/* load time of file */
  bool		system;		/* system sourcefile: do not reload */
};

struct module
{ Atom		name;		/* name of module */
  SourceFile	file;		/* file from which module is loaded */
  Table		procedures;	/* predicates associated with module */
  Table		public;		/* public predicates associated */
  Module	super;		/* Import predicates from here */
  unsigned short flags;		/* booleans: */
		/*	SYSTEM	   system module */
		/*	UNKNOWN	   trap unknown predicates */
};

struct trail_entry
{ Word		address;	/* address of the variable */
};

struct table
{ int		size;		/* size of hash table */
  Symbol	entries[1];	/* array of hash symbols */
};

struct symbol
{ Symbol	next;		/* next in chain */
  word		name;		/* name entry of symbol */
  word		value;		/* associated value with name */
};

		 /*******************************
		 *	     MARK/UNDO		*
		 *******************************/

#define setVar(w)	((w) = (word) NULL)

#define DoUndo(b)	{ register TrailEntry tt = tTop; \
			  while(tt > (b).trailtop) \
			    setVar(*(--tt)->address); \
			  tTop = tt; \
			  gTop = (b).globaltop; \
			}
#define DoMark(b)	{ (b).trailtop = tTop; \
			  (b).globaltop = gTop; \
			}

#define Mark(b)		{ DoMark(b); \
			  lockMark(&b); \
			}
#define Undo(b)		{ DoUndo(b); \
			  unlockMark(&b); \
			}


		/********************************
		*             STACKS            *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If we have access to the virtual   memory management of the machine, use
this to enlarge the runtime stacks.  Otherwise use the stack-shifter.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if MMAP_STACK || O_SHARED_MEMORY
#define O_DYNAMIC_STACKS 1
#else
#ifndef O_SHIFT_STACKS
#define O_SHIFT_STACKS 1
#endif
#endif


#if O_SHARED_MEMORY
#define O_SHM_FREE_IMMEDIATE 1
#define MAX_STACK_SEGMENTS  20
#define STACK(type) \
	{ type		base;		/* base address of the stack */     \
	  type		top;		/* current top of the stack */      \
	  type		max;		/* allocated maximum */		    \
	  long		limit;		/* how big it is allowed to grow */ \
	  long		maxlimit;	/* maximum limit */                 \
	  char		*name;		/* Symbolic name of the stack */    \
	  long		segment_initial;/* initial size */		    \
	  int		segment_double;	/* times to double */		    \
	  int		segment_top;	/* Next segment to be allocated */  \
	  struct							    \
	  { caddress	base;		/* Base of this segment */	    \
	    long	size;		/* Size of this segment */	    \
	  } segments[MAX_STACK_SEGMENTS];				    \
	}
#else /* !O_SHARED_MEMORY */
#define STACK(type) \
	{ type		base;		/* base address of the stack */     \
	  type		top;		/* current top of the stack */      \
	  type		min;		/* donot shrink below this value */ \
	  type		max;		/* allocated maximum */		    \
	  long		limit;		/* how big it is allowed to grow */ \
	  long		maxlimit;	/* maximum limit */                 \
	  long		minfree;	/* minimum amount of free space */  \
	  bool		gc;		/* Can be GC'ed? */		    \
	  long		gced_size;	/* size after last GC */	    \
	  long		small;		/* Donot GC below this size */	    \
	  char		*name;		/* Symbolic name of the stack */    \
	}
#endif

struct stack STACK(caddress);		/* Anonymous stack */

#define N_STACKS (5)

GLOBAL struct
{ struct STACK(LocalFrame) local;	/* local (environment) stack */
  struct STACK(Word)	   global;	/* local (environment) stack */
  struct STACK(TrailEntry) trail;	/* trail stack */
  struct STACK(Word *)	   argument;	/* argument stack */
  struct STACK(Lock)	   lock;	/* Foreign code locks */
} stacks;

#define tBase	(stacks.trail.base)
#define tTop	(stacks.trail.top)
#define tMax	(stacks.trail.max)

#define lBase	(stacks.local.base)
#define lTop	(stacks.local.top)
#define lMax	(stacks.local.max)

#define gBase	(stacks.global.base)
#define gTop	(stacks.global.top)
#define gMax	(stacks.global.max)

#define aBase	(stacks.argument.base)
#define aTop	(stacks.argument.top)
#define aMax	(stacks.argument.max)

#define pBase	(stacks.lock.base)
#define pTop	(stacks.lock.top)
#define pMax	(stacks.lock.max)

GLOBAL char *	hTop;			/* highest allocated heap address */
GLOBAL char *	hBase;			/* lowest allocated heap address */

#define SetHTop(val)	{ if ( (char *)(val) > hTop  ) hTop  = (char *)(val); }
#define SetHBase(val)	{ if ( (char *)(val) < hBase ) hBase = (char *)(val); }

#define onStack(name, addr) \
	((char *)(addr) >= (char *)stacks.name.base && \
	 (char *)(addr) <  (char *)stacks.name.top)
#define onStackArea(name, addr) \
	((char *)(addr) >= (char *)stacks.name.base && \
	 (char *)(addr) <  (char *)stacks.name.max)
#define usedStack(name) ((char *)stacks.name.top - (char *)stacks.name.base)
#define sizeStack(name) ((char *)stacks.name.max - (char *)stacks.name.base)
#define roomStack(name) ((char *)stacks.name.max - (char *)stacks.name.top)
#define narrowStack(name) (roomStack(name) < stacks.name.minfree)

#if O_DYNAMIC_STACKS
#define STACKVERIFY(g)			/* hardware stack verify */
#define verifyStack(s)
#else
#define STACKVERIFY(g)	{ g; }
#define verifyStack(s) \
	{ if ( roomStack(s) < 100 ) \
 	    outOf((Stack)&stacks.s); \
	}
#endif


		/********************************
		*       GLOBAL VARIABLES        *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
General global variables  to  indicate  status  or  communicate  between
modules.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

GLOBAL int 	  critical;		/* in critical code for abort? */
GLOBAL bool	  aborted;		/* have we been aborted */
GLOBAL char 	 *cannot_save_program;	/* Program cannot be saved */
GLOBAL LocalFrame environment_frame;	/* current context frame */
GLOBAL bool	  novice;		/* novice user */
GLOBAL Atom	  source_file_name;	/* Current source file_name */
GLOBAL int	  source_line_no;	/* Current source line_no */
GLOBAL bool	  fileerrors;		/* Report file errors? */

#define ReadingSource (source_line_no > 0 && source_file_name != NULL)

		/********************************
		*        FAST DISPATCHING	*
		********************************/

#if VMCODE_IS_ADDRESS
GLOBAL char  *dewam_table;			/* decoding table */
GLOBAL code  wam_table[I_HIGHEST+1];		/* code --> address */
GLOBAL void **interpreter_jmp_table;		/* interpreters table */

#define encode(wam) (wam_table[wam])		/* WAM --> internal */
#define decode(c)   ((code) dewam_table[c])	/* internal --> WAM */
#else /* VMCODE_IS_ADDRESS */
#define encode(wam) (wam)
#define decode(wam) (wam)
#endif /* VMCODE_IS_ADDRESS */

		/********************************
		*            STATUS             *
		*********************************/

GLOBAL struct
{ bool		requested;		/* GC is requested by stack expander */
  int		blocked;		/* GC is blocked now */
  bool		active;			/* Currently running? */
  long		collections;		/* # garbage collections */
  long		global_gained;		/* global stack bytes collected */
  long		trail_gained;		/* trail stack bytes collected */
  real		time;			/* time spent in collections */
} gc_status;

#ifdef O_SHIFT_STACKS
GLOBAL struct
{ int		blocked;		/* No shifts allowed */
  real		time;			/* time spent in stack shifts */
  int		local_shifts;		/* Shifts of the local stack */
  int		global_shifts;		/* Shifts of the global stack */
  int		trail_shifts;		/* Shifts of the trail stack */
} shift_status;
#endif

GLOBAL struct
{ Atom		symbolfile;		/* current symbol file */
  Atom		orgsymbolfile;		/* symbol file we started with */
  Atom		restored_state;		/* -r/restore state restored */
} loaderstatus;

#define NO_PROFILING		0
#define CUMULATIVE_PROFILING	1
#define PLAIN_PROFILING		2

/*  statistics information */

GLOBAL struct
{ long		inferences;		/* logical inferences made */
  long		heap;			/* heap in use */
  int		atoms;			/* No. of atoms defined */
  int		functors;		/* No. of functors defined */
  int		predicates;		/* No. of predicates defined */
  int		modules;		/* No. of modules in the system */
  long		externals;		/* No. of clause external references */
  long		codes;			/* No. of byte codes generated */
  long		collections;		/* No. of garbage collections */
  long		global_gained;		/* No. of cells global stack gained */
  long		trail_gained;		/* No. of cells trail stack gained */
#ifdef O_PROFILE
  int		profiling;		/* profiler is on? */
  long		profile_ticks;		/* profile ticks total */
#endif /* O_PROFILE */
} statistics;

		/********************************
		*            MODULES            *
		*********************************/

#define MODULE_user	(modules.user)
#define MODULE_system	(modules.system)

GLOBAL struct
{ Module	typein;			/* module for type in goals */
  Module	source;			/* module we are reading clauses in */
  Module	user;			/* user module */
  Module	system;			/* system predicate module */
} modules;

GLOBAL Table	moduleTable;		/* hash table of available modules */

		/********************************
		*         PREDICATES            *
		*********************************/

GLOBAL Procedure	PROCEDURE_alt1;	/* $alt/1, see C_OR */
GLOBAL Procedure	PROCEDURE_garbage_collect0;
GLOBAL Procedure	PROCEDURE_block3;

extern struct code_info	codeTable[];

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

#define CALL_PORT	0x1		/* port masks */
#define EXIT_PORT	0x2
#define FAIL_PORT	0x4
#define REDO_PORT	0x8
#define UNIFY_PORT	0x10
#define VERY_DEEP	10000000L	/* deep skiplevel */

#define LONGATOM_CHECK	    0x1		/* read/1: error on long atoms */
#define SINGLETON_CHECK	    0x2		/* read/1: check singleton vars */
#define DOLLAR_STYLE	    0x4		/* dollar is lower case */
#define DISCONTIGUOUS_STYLE 0x8		/* warn on discontiguous predicates */
#if O_STRING
#define O_STRING_STYLE	    0x10	/* read ".." as string instead of list */
#endif /* O_STRING */
#define MAXNEWLINES	    5		/* maximum number of newlines in atom */
#define SYSTEM_MODE	    (debugstatus.styleCheck & DOLLAR_STYLE)

GLOBAL struct debuginfo
{ long		skiplevel;		/* current skip level */
  bool		tracing;		/* are we tracing? */
  bool		debugging;		/* are we debugging? */
  unsigned long leashing;		/* ports we are leashing */
  unsigned long visible;		/* ports that are visible */
  int		style;			/* print style of tracer */
  bool		showContext;		/* tracer shows context module */
  int		styleCheck;		/* source style checking */
  int		suspendTrace;		/* tracing is suspended now */
} debugstatus;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Administration of loaded intermediate code files  (see  pl-wic.c).  Used
with the -c option to include all these if necessary.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct state * State;

struct state
{ char *	name;			/* name of state */
  State		next;			/* next state loaded */
};

GLOBAL State stateList;			/* list of loaded states */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Include debugging info to make it (very) verbose.  SECURE adds  code  to
check  consistency mainly in the WAM interpreter.  Prolog gets VERY slow
if SECURE is  used.   DEBUG  is  not  too  bad  (about  20%  performance
decrease).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define REL(a)		((Word)(a) - (Word)(lBase))

#if O_DEBUG
#define DEBUG(n, g) { if (status.debugLevel >= n) { g; fflush(stdout); } }
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

#include "pl-funcs.h"			/* global functions */
#include "pl-main.h"			/* Declarations needed by pl-main.c */

extern struct atom atoms[];
extern struct functorDef functors[];

#include "pl-atom.ih"
#include "pl-funct.ih"

#endif /*_PL_INCLUDE_H*/
