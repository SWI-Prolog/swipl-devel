/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Proposal for new Prolog data-structures.

Aim
===

Flexibel adaption to different memory model.   Possible  to make `clean'
programs, i.e. programs that donot make assumptions on the memory model.
The latter appears necessary on some systems to put Prolog into a DLL.

Fast comparison and checking. The hope  is   that  the  result will have
comparable or better speed.

Approach
========

	* No direct pointers in Prolog machine words anymore

	* Tags in the low bits to exploit SPARC and possible other
	  machines fixed-width instruction, so masks can be loaded
	  in one instead of two instructions.

	* Explicit encoding of the `user' data-types in the word,
	  so PL_term_type() can be much faster.

	* Explicit encoding of the storage regime used, so more code
	  can be generic.

Types:
======

Sorted to standard order of terms:

Storage places:

	S	Static (global variable)
	H	Heap
	L	Local
	G	Global
	T	Trail
	-	`In the variable'

	      INDEX  STORAGE  L  G  H  T  S  -  I
----------------------------------------------------------------
Var		0      -                    00
Integer		1      HG-      10 01       00
Float		2      HG       10 01
Atom		3      HS          01    00
String		4      HG       10 01
List		5      HG       10 01
Term		6      HG       10 01
Reference	7      HLG   11 10 01
----------------------------------------------------------------

Adding 2 bits for the garbage collector, this adds up to 7-bits tag info,
leaving us with 32-7 is 25 bits data, or:

	* Tagged ints from -16M to +16M
	* 128 MB per memory area, assuming all data is 4-byte aligned.

Giving this, stacks can be freely shifted!

Bit layout
==========

	* Value are the top-bits, so extracting the value is just a
	  shift.

	* GC masks follow, so, as they are normally both 0, shifting
	  suffices for this too.

	* Type is the low 3-bits, so a simple mask yields the type.

	* Storage in bits 4 and 5

Indirect data
=============

	* Using normal tag, but the storage-specifier is 0x3 (11).  Tag
	  is only INTEGER, STRING or FLOAT

	* Using value: size in words of the object * 4
	
	* String uses the low-order 2 bits for specifying the amount of
	  padding bytes (0-3, 0 means 4).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include "pl-buffer.h"

#define LMASK_BITS	7		/* total # mask bits */

#define TAG_MASK	0x00000007L	/* mask for tag */
#define TAG_VAR		0x00000000L	/* tag for variable (= 0L) */
#define TAG_FLOAT	0x00000001L	/* Floating point number */
#define TAG_INTEGER	0x00000002L	/* Tagged or indirect integer */
#define TAG_ATOM	0x00000003L	/* an atom */
#define TAG_STRING	0x00000004L	/* String */
#define TAG_LIST	0x00000005L	/* List */
#define TAG_COMPOUND	0x00000006L	/* Compound term */
#define TAG_REFERENCE	0x00000007L	/* Reference pointer */

					/* Trail tag-bits */
#define TAG_TRAILMASK	0x00000001L	/* mask for tag */
#define TAG_TRAILADDR	0x00000000L	/* Trail-only: address */
#define TAG_TRAILVAL	0x00000001L	/* Trail-only: value */
#define tagTrailPtr(p)	((Word)((ulong)(p)|TAG_TRAILVAL))
#define isTrailVal(p)	((ulong)(p)&TAG_TRAILVAL)
#define trailValP(p)	((Word)((ulong)(p)&~TAG_TRAILMASK))
#define trailVal(p)	(*trailValP(p))

#define STG_MASK	(0x3<<3)
#define STG_STATIC	(0x0<<3)	/* storage masks */
#define STG_GLOBAL	(0x1<<3)
#define STG_LOCAL	(0x2<<3)
#define STG_RESERVED	(0x3<<3)

#define STG_INLINE	STG_STATIC
#define STG_TRAIL	STG_STATIC

#define MARK_MASK	(0x1<<5)	/* GC mark */
#define FIRST_MASK	(0x2<<5)	/* GC first mark */

#define tag(w)		((w) & TAG_MASK)
#define storage(w)	((w) & STG_MASK)
#define valPtr2(w, s)	((Word)(((w) >> 5) + base_addresses[s]))
#define valPtr(w)	valPtr2(w, storage(w))
#define valInt(w)	((long)(w) >> 7)

		 /*******************************
		 *	  EXTENDED TAG		*
		 *******************************/

extern const unsigned int tagtypeex[];

#define tagex(w)	((w) & (TAG_MASK|STG_MASK))

#define TAGEX_INDIRECT	0x1
#define isIndirect(w)	(tagtypeex[tagex(w)] & TAGEX_INDIRECT)

		 /*******************************
		 *	 BASIC TYPE TESTS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
For atom, we use tagex() to avoid detecting functor_t on stacks. This is
only important for the  atom-garbage  collector   that  must  make  this
distinction while scanning the global stack   as well as for record-keys
and while loading .wic files.  It comes at no price.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define isVar(w)	((w) == 0L)	/* variable */
#define isAtom(w)	(tagex(w) == TAG_ATOM)
#define isInteger(w)	(tag(w) == TAG_INTEGER)
#define isReal(w)	(tag(w) == TAG_FLOAT)
#define isString(w)	(tag(w) == TAG_STRING)
#define isTerm(w)	(tag(w) == TAG_COMPOUND)


		 /*******************************
		 *	    REFERENCES		*
		 *******************************/

#define isRef(w)	(tag(w) == TAG_REFERENCE)
#define isRefL(w)	(tagex(w) == (TAG_REFERENCE|STG_LOCAL))
#define unRef(w)	((Word)valPtr(w))
#define unRefL(w)	((Word)valPtr2(w, STG_LOCAL))
#define deRef(p)	{ while(isRef(*(p))) (p) = unRef(*(p)); }
#define deRef2(p, d)	{ (d) = (p); deRef(d); }


		 /*******************************
		 *	COMPOUNDS AND LISTS	*
		 *******************************/

#define functorTerm(w)	valueTerm(w)->definition
#define arityTerm(w)	arityFunctor(valueTerm(w)->definition)
#define valueTerm(w)	((Functor)valPtr2(w, STG_GLOBAL))
#define hasFunctor(w,f) (isTerm(w) && valueTerm(w)->definition == (f))
#define argTerm(w, n)	(valueTerm(w)->arguments[n])
#define argTermP(w, n)	(&argTerm(w, n))

#define isList(w)	hasFunctor(w, FUNCTOR_dot2)
#define isNil(w)	((w) == ATOM_nil)


		 /*******************************
		 *	      INDIRECTS		*
		 *******************************/

#define mkIndHdr(n, t)	(((n)<<9) | (t) | STG_LOCAL)
#define wsizeofInd(iw)	((iw)>>9)
#define addressIndirect(w) valPtr(w)
#define valIndirectP(w)	(((Word)valPtr(w))+1)

#define padHdr(iw)	(((iw)>>7 & 0x3) ? ((iw)>>7 & 0x3) : 4)
#define mkPadHdr(n)	(((n)&(sizeof(word)-1)) << 7)
#define mkStrHdr(n,p)	(mkIndHdr(n, TAG_STRING)|mkPadHdr(pad))

#define valString(w)	((char *)valIndirectP(w))
#define valBignum(w)	(*(long *)valIndirectP(w))
#define isBignum(w)	(isInteger(w) && storage(w) != STG_INLINE)
#define isTaggedInt(w)	(tagex(w) == (TAG_INTEGER|STG_INLINE))
			/* == (isInteger(w) && storage(w) == STG_INLINE) */

		 /*******************************
		 *	       VALUES		*
		 *******************************/

#define indexAtom(w)	((w)>>LMASK_BITS)
#define atomValue(w)	fetchBuffer(&atom_array, indexAtom(w), Atom)
#define stringAtom(w)	(atomValue(w)->name)
#define valInteger(w)	(storage(w) == STG_INLINE ? valInt(w) : valBignum(w))

		 /*******************************
		 *	      FUNCTORS		*
		 *******************************/

#define F_ARITY_BITS	5		/* upto 32 inlined arity */
#define F_ARITY_MASK	((1<<F_ARITY_BITS)-1)
#define MK_FUNCTOR(n, a) (((((n)<<F_ARITY_BITS)|(a))<<LMASK_BITS) | \
			  TAG_ATOM|STG_GLOBAL)
#define functorHashValue(f, n)	((f)>>(LMASK_BITS) & ((n)-1))
#define indexFunctor(w)	((w)>>(LMASK_BITS+F_ARITY_BITS))
#define valueFunctor(w) fetchBuffer(&functor_array,indexFunctor(w),FunctorDef)
#define _arityFunc_(w)	(((w) >> LMASK_BITS) & F_ARITY_MASK)
#define arityFunctor(w) (_arityFunc_(w)!=F_ARITY_MASK ? _arityFunc_(w) \
						      : valueFunctor(w)->arity)
#define isAtomFunctor(w) (arityFunctor(w) == 0)
#define nameFunctor(w)	(valueFunctor(w)->name)

		 /*******************************
		 *	  DERIVED TESTS		*
		 *******************************/

#define nonvar(w)	(!isVar(w))
#define isNumber(w)	(isInteger(w) || isReal(w))
#define isAtomic(w)	(!isVar(w) && !isTerm(w))


		 /*******************************
		 *	   CREATING WORDS	*
		 *******************************/

#define MAXTAGGEDPTR	((1L<<((8*sizeof(word))-5)) - 1)

#define consInt(n)	(((word)(n)<<7) | TAG_INTEGER)
#if !O_DEBUG
#define consPtr(p,ts)	((word)((((word)(p)-base_addresses[(ts)&STG_MASK])<<5)|(ts)))
#endif

