/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Support for virtual machine
*/

#include "pl-incl.h"

		/********************************
		*    CALLING THE INTERPRETER    *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Starts a new Prolog toplevel.  Resets I/O to point to the user and stops
the debugger.  Restores I/O and debugger on exit.  The Prolog  predicate
`$break' is called to actually built the break environment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_break()
{ word goal = (word) ATOM_break;

  return pl_break1(&goal);
}

word
pl_break1(goal)
Word goal;
{ extern int Input, Output;
  bool rval;

  int	     inSave    = Input;
  int	     outSave   = Output;
  long	     skipSave  = debugstatus.skiplevel;
  bool	     traceSave = debugstatus.tracing;
  bool	     debugSave = debugstatus.debugging;
  int	     suspSave  = debugstatus.suspendTrace;

  Input = 0;
  Output = 1;

  debugstatus.tracing = FALSE;
  debugstatus.debugging = FALSE;
  debugstatus.skiplevel = 0;
  debugstatus.suspendTrace = 0;

  rval = callGoal(MODULE_user, *goal, FALSE);

  debugstatus.suspendTrace = suspSave;
  debugstatus.skiplevel    = skipSave;
  debugstatus.debugging    = debugSave;
  debugstatus.tracing      = traceSave;

  Output = outSave;
  Input = inSave;

  return rval;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Call a prolog goal from C. The argument must  be  an  instantiated  term
like for the Prolog predicate call/1.  The goal is executed in a kind of
break environment and thus bindings which result of the call are lost.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
callGoal(module, goal, debug)
Module module;
word goal;
bool debug;
{ LocalFrame lSave   = lTop;
  LocalFrame envSave = environment_frame;
  mark       m;
  Word *     aSave = aTop;
  bool	     rval;

  lTop = (LocalFrame)addPointer(lTop, sizeof(struct localFrame) +
				      MAXARITY * sizeof(word));
  lTop = (LocalFrame) addPointer(lTop, sizeof(LocalFrame));
  verifyStack(local);
  varFrame(lTop, -1) = (word) environment_frame;

  Mark(m);
/*  lockMark(&m); */
  gc_status.blocked++;
  rval = interpret(module, goal, debug);
  gc_status.blocked--;
  Undo(m);
/*  unlockMark(&m); */
  lTop = lSave;
  aTop = aSave;
  environment_frame = envSave;

  return rval;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Bring the Prolog system itself to life.  Prolog  saves  the  C-stack  to
enable  aborts.   pl_abort()  will  close  open  files, reset all clause
references to `0' and finally long_jumps back to prolog().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static jmp_buf abort_context;		/* jmp buffer for abort() */

word
pl_abort()
{ if (critical > 0)			/* abort in critical region: delay */
  { aborted = TRUE;
    succeed;
  }
  PopTty(&ttytab);
  resetRead();
  closeFiles();
  resetReferences();
  resetForeign();
#if O_PROFILE
  pl_reset_profiler();
#endif

  longjmp(abort_context, 1);
  /*NOTREACHED*/
  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initial entry point from C to start  the  Prolog  engine.   Saves  abort
context,  clears  the  stack  and  finally  starts  the  virtual machine
interpreter with the toplevel goal.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
prolog(goal)
volatile word goal;
{ if (setjmp(abort_context) != 0)
  { goal = (word) ATOM_abort;
  } else
  { debugstatus.debugging = FALSE;
  }

  lTop = (LocalFrame) addPointer(lBase, sizeof(LocalFrame));
  varFrame(lTop, -1) = (word) NULL;
  tTop = tBase;
  gTop = gBase;
  aTop = aBase;
  pTop = pBase;
  gc_status.blocked   = 0;
  gc_status.requested = FALSE;
  status.arithmetic   = 0;

  debugstatus.tracing = FALSE;
  debugstatus.suspendTrace = 0;

  return interpret(MODULE_system, goal, FALSE);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Cut (!) as called via the  meta-call  mechanism has no effect.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_metacut()
{ succeed;
}


		/********************************
		*          UNIFICATION          *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unify is the general unification procedure.   This  raw  routine  should
only be called by interpret as it does not undo bindings made during the
unification  in  case  the  unification fails.  pl_unify() (implementing
=/2) does undo bindings and should be used by foreign predicates.

Unification depends on the datatypes available in the system and will in
general need updating if new types are added.  It should be  noted  that
unify()  is  not  the only place were unification happens.  Other points
are:
  - various of the virtual machine instructions
  - various macros, for example APPENDLIST and CLOSELIST
  - unifyAtomic(), unifyFunctor(): unification of atomic data.
  - various builtin predicates. They should be flagged some way.

The Gould does not accept the construct (word)t1 = *t1.  This implies we
have to define extra variables, slowing down execution a bit (on the SUN
this trick saves about 10% on this function).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if !O_NO_LEFT_CAST
#define w1 ((word)t1)
#define w2 ((word)t2)
#endif

bool
unify(t1, t2)
register Word t1, t2;
{
#if O_NO_LEFT_CAST
  register word w1, w2;
#endif

  deRef(t1);  
  deRef(t2);

  if (isVar(*t1) )
  { if (isVar(*t2) )
    { if (t1 < t2)		/* always point downwards */
      { Trail(t2);
        *t2 = makeRef(t1);
	succeed;
      }
      if (t1 == t2)
	succeed;
      Trail(t1);
      *t1 = makeRef(t2);
      succeed;
    }
    Trail(t1);
    *t1 = *t2;
    succeed;
  }
  if (isVar(*t2) )
  { Trail(t2);
    *t2 = *t1;
    succeed;
  }

  if ( (w1 = *t1) == (w2 = *t2) )
    succeed;
  if ( mask(w1) != mask(w2) )
    fail;

  if ( mask(w1) != 0 )
  { if ( !isIndirect(w1) )
      fail;
#if O_STRING
    if ( isString(w1) && isString(w2) && equalString(w1, w2) )
      succeed;
#endif /* O_STRING */
    if ( isReal(w1) && isReal(w2) && valReal(w1) == valReal(w2) )
      succeed;
    fail;
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Now both w1 and w2 can still represent a term or an atom.  If  both  are
atoms  they are not the same atom.  We can do a quick and dirty test for
atom as it is not a variable, nor a masked type.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  { register int arity;
    register FunctorDef fd;

    if ( pointerIsAtom(w1) || 
	 pointerIsAtom(w2) ||
	 (fd = functorTerm(w1)) != functorTerm(w2) )
      fail;

    arity = fd->arity;
    t1 = argTermP(w1, 0);
    t2 = argTermP(w2, 0);
    for(; arity > 0; arity--, t1++, t2++)
      if (unify(t1, t2) == FALSE)
	fail;
  }

  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unify_atomic(p, a) is normally called through unifyAtomic(). It  unifies
a  term,  represented  by  a pointer to it, with an atomic value.  It is
intended for foreign language functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
unify_atomic(p, a)
register Word p;
word a;
{ deRef(p);

  if (*p == a)
    succeed;

  if (isVar(*p) )
  { Trail(p);
    *p = a;
    succeed;
  }

  if (isIndirect(a) && isIndirect(*p) )
  { if (isReal(a) && isReal(*p) && valReal(a) == valReal(*p))
      succeed;
#if O_STRING
    if (isString(a) && isString(*p) && equalString(a, *p))
      succeed;
#endif /* O_STRING */
  }

  fail;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unify a (pointer to a) term with a functor (is name/arity pair).  If the
term is instantiated to a term of the name and arity  indicated  by  the
functor  this  call just succeeds.  If the term is a free variable it is
bound to a term whose arguments are all variables.  Otherwise this  call
fails.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
unifyFunctor(term, functor)
register Word term;
register FunctorDef functor;
{ if (functor->arity == 0)
    return unifyAtomic(term, functor->name);

  deRef(term);

  if (isVar(*term) )
  { Trail(term);
    *term = globalFunctor(functor);
    succeed;
  }
  if (isTerm(*term) && functorTerm(*term) == functor)
    succeed;

  fail;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
checkData(p) verifies p points to valid  Prolog  data  and  generates  a
system  error  otherwise.  The checks performed are much more rigid than
those during normal execution.  Arity of terms is limited to  100  as  a
kind of heuristic.

Note that we expect terms on the global stack.   This  is  true  in  the
interpreter,  but  not everywere in the system (records use terms on the
heap).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define onGlobal(p) ((char *)p >= (char *)gBase && (char *)p <= (char *)gTop)
#define onLocal(p) ((char *)p >= (char *)lBase && (char *)p <= (char *)lTop)
#define onHeap(  p) ((char *)p >= (char *)hBase && (char *)p <= (char *)hTop)

#if TEST
void
checkData(p)
register Word p;
{ int arity; int n;
  register Word p2;

  if (isVar(*p))
    return;
  while(isRef(*p))
  { p2 = unRef(*p);
    if (p2 > p)
      sysError("Reference to higher address");
    if (!onLocal(p2) && !onGlobal(p2) && !onHeap(p2))
      sysError("Illegal reference pointer: 0x%x", *p);
    return checkData(p2);
  }
  if ((*p & MASK_MASK) == INT_MASK)
    return;
  if ((*p & MASK_MASK) == REAL_MASK)
  { p2 = (Word)unMask(*p);
    if (!onGlobal(p2) && !onHeap(p2))
      sysError("Illegal real: 0x%x", *p);
    return;
  }
#if O_STRING
  if ((*p & MASK_MASK) == STRING_MASK)
  { p2 = (Word)unMask(*p);
    if (!onGlobal(p2) && !onHeap(p2))
      sysError("Illegal string: 0x%x", *p);
    if ( sizeString(*p) != strlen(valString(*p)) )
      sysError("String has inconsistent length: 0x%x", *p);
    return;
  }
#endif /* O_STRING */
  if (onHeap(*p) && !onGlobal(*p))
  { if (((Atom)(*p))->type != ATOM_TYPE)
      sysError("Illegal atom: 0x%x", *p);
    succeed;
  }
  if (!onGlobal(*p))
    warning("Term not on global stack: 0x%x", *p);
  if (functorTerm(*p)->type != FUNCTOR_TYPE)
    sysError("Illegal term: 0x%x", *p);
  arity = functorTerm(*p)->arity;
  if (arity <= 0 || arity > 100)
    sysError("Illegal arity");
  for(n=0; n<arity; n++)
    checkData(argTermP(*p, n));

  return;
}
#endif /* TEST */
