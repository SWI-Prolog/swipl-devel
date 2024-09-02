/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1996-2024, University of Amsterdam
			      CWI, Amsterdam
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file provides the access macros and inline functions for all Prolog
data. As of version 9.3.4, all Prolog data   is of type `word`, which is
64 bits, also on 32-bit hardware. The  current data representation is an
_intermediate_ between the 32/64 bit  model   of  previous  versions and
taking full advantage of the 64 bit space. Notably, where older versions
used _relative_ addresses with a _storage_   tag  that tells relative to
which memory area, the current version uses _absolute_ addresses.

Tags
====

Tags are stored in the low bits.  This allows extracting the value using
a single shift and creating a tagged version as a shift+bitwise-or.

We need type and GC bits. GC requires   two bits: the `MARK` bit is used
to mark that data  is  accessible  and   the  `FIRST`  plays  two roles:
indicate the _head of a relocation chain_ and   the end of a range while
traversing the graph of linked Prolog data. These two bits are zero when
not in GC and are used by  many   algorithms  on  terms to avoid cycles,
avoid processing terms twice, etc.

The type currently consists of a combination of type and _storage_ mask.
The latter was used to indicate the   memory  area of the (old) relative
pointer, but still keeps its role  as   type  indication. For example, a
string is represented as `TAG_STRING|STG_GLOBAL`   as Prolog data, while
the string itself is stored on the  global stack guarded by an _indirect
guard_ tagged as `TAG_STRING|STG_LOCAL`. Future   versions  will combine
these and reorganise the tag space.

Types:
======

Sorted to standard order of terms:

Storage places:

	S (00)	Static/inline/trail
	L (10)	Local stack
	G (01)	Global stack
	T (00)	Trail stack
	- (00)	Inline

	      INDEX  STORAGE
----------------------------
Var		0      -
Integer		1      G-
Float		2      G
Atom		3      S
String		4      G
List		5      G
Term		6      G
Reference	7      LG
-----------------------------

Bit layout
==========

 - Value are the top-bits, so extracting the value is just a
   shift.

 - GC masks follow, so, as they are normally both 0, shifting
   suffices for this too.

 - Type is the low 3-bits, so a simple mask yields the type.

 - Storage in bits 4 and 5

Variables
=========

SWI-Prolog uses the `word` 0 for a  Prolog variable. Unlike most systems
that create a variable as a  _self   reference_.  The  advantage of self
references is that you can always pass Prolog data as a `word` (i.e., by
value). In SWI-Prolog we typically pass `Word`,   a  pointer to a `word`
or, when robustness against GC/SHIFT is  required, as `term_t`, which is
an offset to the local stack where we can find the `word`.

The advantage of this approach is  that  we   can  use  the _value_ of a
variable for making annotations about  it   in  many  algorithms such as
copy_term/2, variant/2, etc.

Global stack data
=================

 - Terms (compound) uses <functor_t, word*>, where functor_t
   uses `TAG_ATOM|STG_GLOBAL`.

Indirect data
-------------

Data that is too big to fit  in   a  64 bit `word` is called _indirect_.
This is stored on the global stack  as   a  binary  blob, guarded by two
equal quard words. The guards indicate the  type and length of the blob.
The guards are required for GC to be   able  to enumerate the objects on
the stack both upward and downward.

 - Using normal tag, but the storage-specifier is STG_LOCAL.  Tag
   is only INTEGER, STRING or FLOAT

 - Using value: size in words of the object * 8

 - String uses the low-order 3 bits for specifying the amount of
   padding bytes (0-7, 0 means 8).  Note that we cannot store the
   size in bytes instead as this would get a rounded down word-size
   of the blob while the size needs to be rounded up.

NOTE: the tag-numbers are  mapped  to   public  constants  (PL_*) in the
type_map array in pl-fli.c.  Make  sure   this  is  consistent  with the
definitions below. Also the tagtypeex[] array defined in pl-setup.c must
be kept consistent.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include "os/pl-buffer.h"

#define LMASK_BITS	7		/* total # mask bits */

#define TAG_MASK	0x00000007L	/* mask for tag */
#define TAG_VAR		0x00000000L	/* tag for variable (= 0L) */
#define TAG_ATTVAR	0x00000001L	/* tag for attributed variable */
#define TAG_FLOAT	0x00000002L	/* Floating point number */
#define TAG_INTEGER	0x00000003L	/* Tagged small int, 64-bit indirect, */
					/* GMP mpz or mpq (rational) */
#define TAG_STRING	0x00000004L	/* String */
#define TAG_ATOM	0x00000005L	/* an atom */
#define TAG_COMPOUND	0x00000006L	/* Compound term */
#define TAG_REFERENCE	0x00000007L	/* Reference pointer */

					/* Trail tag-bits */
#define TAG_TRAILMASK	0x00000001L	/* mask for tag */
#define TAG_TRAILADDR	0x00000000L	/* Trail-only: address */
#define TAG_TRAILVAL	0x00000001L	/* Trail-only: value */
#define tagTrailPtr(p)	((Word)((uintptr_t)(p)|TAG_TRAILVAL))
#define isTrailVal(p)	((uintptr_t)(p)&TAG_TRAILVAL)
#define trailValP(p)	((Word)((uintptr_t)(p)&~TAG_TRAILMASK))
#define trailVal(p)	(*trailValP(p))

#define STG_MASK	(0x3<<3)
#define STG_STATIC	(0x0<<3)	/* storage masks */
#define STG_GLOBAL	(0x1<<3)	/* global stack */
#define STG_LOCAL	(0x2<<3)	/* local stack */
#define STG_RESERVED	(0x3<<3)

#define STG_INLINE	STG_STATIC
#define STG_TRAIL	STG_STATIC

#define MARK_MASK	(0x1<<5)	/* GC mark */
#define FIRST_MASK	(0x2<<5)	/* GC first mark */

#define set_marked(p)	do { *(p) |= MARK_MASK; } while(0)
#define set_first(p)	do { *(p) |= FIRST_MASK; } while(0)
#define clear_marked(p)	do { *(p) &= ~MARK_MASK; } while(0)
#define clear_first(p)	do { *(p) &= ~FIRST_MASK; } while(0)
#define clear_both(p)	do { *(p) &= ~(FIRST_MASK|MARK_MASK); } while(0)
#define is_marked(p)	(*(p) & MARK_MASK)
#define is_first(p)	(*(p) & FIRST_MASK)
#define is_marked_or_first(p) (*(p) & (MARK_MASK|FIRST_MASK))

#define tag(w)		((w) & TAG_MASK)
#define storage(w)	((w) & STG_MASK)
#define valInt(w)	((sword)(w) >> LMASK_BITS)
#define valUInt(w)	((word)(w) >> LMASK_BITS)

		 /*******************************
		 *	  EXTENDED TAG		*
		 *******************************/

#define EXBIT(w)	(1<<(w))
#define INDIRECT_BM	( EXBIT(STG_GLOBAL|TAG_INTEGER) | \
			  EXBIT(STG_LOCAL|TAG_INTEGER) | \
			  EXBIT(STG_GLOBAL|TAG_FLOAT) | \
			  EXBIT(STG_LOCAL|TAG_FLOAT) | \
			  EXBIT(STG_GLOBAL|TAG_STRING) | \
			  EXBIT(STG_LOCAL|TAG_STRING) \
			)

#define tagex(w)	((w) & (TAG_MASK|STG_MASK))
#define isIndirect(w)	(EXBIT(tagex(w)) & INDIRECT_BM)


		 /*******************************
		 *	 BASIC TYPE TESTS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
For atom, we use tagex() to avoid detecting functor_t on stacks. This is
only important for the  atom-garbage  collector   that  must  make  this
distinction while scanning the global stack   as well as for record-keys
and while loading .wic files.  It comes at no price.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*#define isVar(w)	(tag(w)   == TAG_VAR)*/
#define isVar(w)	((w)      == (word)0)
#define isAtom(w)	(tagex(w) == (TAG_ATOM|STG_STATIC))
#define isFunctor(w)	(tagex(w) == (TAG_ATOM|STG_GLOBAL))
#define isTextAtom(w)	(isAtom(w) && ison(atomValue(w)->type, PL_BLOB_TEXT))
#define isCallableAtom(w) (isTextAtom(w) || (w == ATOM_nil))
#define isRational(w)	(tag(w)   == TAG_INTEGER)
#define isFloat(w)	(tag(w)   == TAG_FLOAT)
#define isString(w)	(tag(w)   == TAG_STRING)
#define isTerm(w)	(tag(w)   == TAG_COMPOUND)
#define isConst(w)	(isAtom(w) || isTaggedInt(w)) /* H_ATOM, B_ATOM, H_SMALLINT, B_SMALLINT */

#if O_BIGNUM
#define isInteger(w)	( isTaggedInt(w) || \
			  ((tag(w) == TAG_INTEGER) && !isMPQNum(w)) )
#else
#define isInteger(w)	isRational(w)
#endif

		 /*******************************
		 *	    REFERENCES		*
		 *******************************/

#define isRef(w)	(tag(w) == TAG_REFERENCE)
#define unRef(w)	valPtr(w)
#define deRef(p)	{ while(isRef(*(p))) (p) = unRef(*(p)); }
#define deRef2(p, d)	{ (d) = (p); deRef(d); }
#define makeRefG(p)	consPtr(p, TAG_REFERENCE|STG_GLOBAL)
#ifdef O_ATTVAR
#define needsRef(w)	(tag(w) <= TAG_ATTVAR)
#else
#define needsRef(w)	isVar(w)
#endif

/* We use local references during GC and shift */
#define makeRefLok(p)	consPtr(p, TAG_REFERENCE|STG_LOCAL)
#define makeRefLG(p)	((void*)(p) >= (void*)lBase ? makeRefLok(p) : makeRefG(p))


		 /*******************************
		 *	COMPOUNDS AND LISTS	*
		 *******************************/

#define functorTerm(w)	word2functor(valueTerm(w)->definition)
#define arityTerm(w)	arityFunctor(valueTerm(w)->definition)
#define valueTerm(w)	((Functor)valPtr(w))
#define hasFunctor(w,f) (isTerm(w) && valueTerm(w)->definition == (f))
#define argTerm(w, n)	(valueTerm(w)->arguments[n])
#define argTermP(w, n)	(&argTerm(w, n))

#define isList(w)	hasFunctor(w, FUNCTOR_dot2)
#define isNil(w)	((w) == ATOM_nil)

		 /*******************************
		 *     ATTRIBUTED VARIABLES	*
		 *******************************/

#define isAttVar(w)	(tag(w) == TAG_ATTVAR)
#define valPAttVar(w)	valPtr(w)

#define canBind(w)	needsRef(w)


		 /*******************************
		 *	      INDIRECTS		*
		 *******************************/

#if SIZEOF_WORD == 8
#define PADBITS 3
#define PADMASK (sizeof(word)-1)
#else
#error "Word must be 8 bytes"
#endif

static inline word
mkIndHdr(size_t size, int tag)
{ word w = size;
  return (w<<(LMASK_BITS+PADBITS))|tag|STG_LOCAL;
}

static inline size_t
wsizeofInd(word w)
{ return (size_t)(w>>(LMASK_BITS+PADBITS));
}

#define addressIndirect(w)	valPtr(w)		/* Pointer to start guard */
#define valIndirectP(w)		(((Word)valPtr(w))+1)	/* Pointer to the blob data */

#define padHdr(iw)		((size_t)(((iw)>>LMASK_BITS & PADMASK) ?	\
					  ((iw)>>LMASK_BITS & PADMASK) : sizeof(word)))
#define mkPadHdr(n)		(((n)&PADMASK) << LMASK_BITS)
#define mkBlobHdr(n,pad,tag)	(mkIndHdr(n, tag)|mkPadHdr(pad))
#define wsizeofIndirect(w)	(wsizeofInd(*addressIndirect(w)))

#define isTaggedInt(w)	(tagex(w) == (TAG_INTEGER|STG_INLINE))

#define MP_RAT_MASK	(0x1)

/* valFloat(w) moved to pl-inline.h */
#define isBString(w)	(isString(w) && ((char *)valIndirectP(w))[0] == 'B')
#define isWString(w)	(isString(w) && ((char *)valIndirectP(w))[0] == 'W')

		 /*******************************
		 *	       VALUES		*
		 *******************************/

/* TODO: putting a prototype here to satisfy the compiler, but fetchAtomArray()
 * may want to be moved somewhere else.  */
static inline Atom	fetchAtomArray(size_t index);
#define indexAtom(w)	((size_t)((w)>>LMASK_BITS))
#define atomValue(w)	fetchAtomArray(indexAtom(w))
#define stringAtom(w)	(atomValue(w)->name)

		 /*******************************
		 *	      FUNCTORS		*
		 *******************************/

#define F_ARITY_BITS	5		/* upto 32 inlined arity */
#define F_ARITY_MASK	((1<<F_ARITY_BITS)-1)
#define MK_FUNCTOR(n, a) (functor_t)(((((n)<<F_ARITY_BITS)|(a))<<LMASK_BITS) | \
			  TAG_ATOM|STG_GLOBAL)
#define functorHashValue(f, n)	((f)>>(LMASK_BITS) & ((n)-1))
#define indexFunctor(w)	((size_t)((w)>>(LMASK_BITS+F_ARITY_BITS)))
#define valueFunctor(w) fetchFunctorArray(indexFunctor(w))
#define _arityFunc_(w)	((size_t)(((w) >> LMASK_BITS) & F_ARITY_MASK))
#define arityFunctor(w) (unlikely(_arityFunc_(w) == F_ARITY_MASK) \
				? valueFunctor(w)->arity \
				: _arityFunc_(w) )
#define isAtomFunctor(w) (arityFunctor(w) == 0)
#define nameFunctor(w)	(valueFunctor(w)->name)

		 /*******************************
		 *	  DERIVED TESTS		*
		 *******************************/

#define nonvar(w)	(!isVar(w))
#define isNumber(w)	(isRational(w) || isFloat(w))
#define isAtomic(w)	(!canBind(w) && !isTerm(w))


		 /*******************************
		 *	   CREATING WORDS	*
		 *******************************/

#define MAXTAGGEDPTR	(((word)1<<((8*sizeof(word))-(LMASK_BITS-2))) - 1)

#define consInt(n)	(((word)(n)<<LMASK_BITS) | TAG_INTEGER)
#define consUInt(n)	(((word)(n)<<LMASK_BITS) | TAG_INTEGER)
