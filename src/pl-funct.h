/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Keri Harris
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2023, University of Amsterdam
                              VU University Amsterdam
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

#include "pl-incl.h"

#ifndef _PL_FUNCT_H
#define _PL_FUNCT_H

#if USE_LD_MACROS
#define lookupFunctorDef(atom, arity) LDFUNC(lookupFunctorDef, atom, arity)
#define lookupFunctorRef(atom, arity) LDFUNC(lookupFunctorRef, atom, arity)
#define lookupVolatileFunctor(atom, arity) \
	LDFUNC(lookupVolatileFunctor, atom, arity)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS
functor_t	lookupFunctorDef(atom_t atom, size_t arity);
functor_t	lookupVolatileFunctor(atom_t atom, size_t arity);
functor_t	lookupFunctorRef(atom_t atom, size_t arity);
functor_t	isCurrentFunctor(atom_t atom, size_t arity);
void		permanentFunctor(functor_t f);
void		acquireFunctor(functor_t f);
void		releaseFunctor(functor_t f);
void		markFunctor(functor_t w);
size_t		collectFunctors(void);
int64_t		collectedFunctors(void);
void		initFunctors(void);
void		cleanupFunctors(void);
int		checkFunctors(void);
foreign_t	pl_current_functor(term_t name, term_t arity,
				   control_t h);
size_t		functor_space(void);

#undef LDFUNC_DECLARATIONS

/* Code that temporarily replaces the functor of compounds on the global
   stack, e.g., to detect cycles, must call startHideFunctors() before
   and endHideFunctors() after, and restore the functors using
   restoreHiddenFunctor().  So, AGC does not collect a functor it could
   not see.  See collectFunctors().
*/

#if USE_LD_MACROS
#define startHideFunctors(_) LDFUNC(startHideFunctors, _)
#define endHideFunctors(_)   LDFUNC(endHideFunctors, _)
#endif

static inline void
startHideFunctors(DECL_LD)
{ LD->atoms.functor_hide++;		/* odd: hiding */
  MEMORY_BARRIER();
}

static inline void
endHideFunctors(DECL_LD)
{ MEMORY_RELEASE();
  LD->atoms.functor_hide++;
}

/* A functor is copied to a new cell on the global stack, which AGC may
   not scan.  Compare pushVolatileAtom().
*/

static inline void
pushVolatileFunctor(word f)
{ if ( unlikely(LOAD_SEQ_CST(GD->atoms.gc_active)) )
    markFunctor(f);
}

static inline void
restoreHiddenFunctor(Word p, word f)
{ *p = f;
  if ( unlikely(LOAD_SEQ_CST(GD->atoms.gc_active)) )
    markFunctor(f);
}


#endif /*_PL_FUNCT_H*/
