/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Support for virtual machine
*/

/*#define O_SECURE 1*/
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
  Lock	     pSave   = pTop;		/* TMP */
  LocalFrame envSave = environment_frame;
  mark       m;
  Word *     aSave = aTop;
  bool	     rval;

  lockp(&lSave);
  lockp(&envSave);
  lTop = (LocalFrame)addPointer(lTop, sizeof(struct localFrame) +
				      MAXARITY * sizeof(word));
  lTop = (LocalFrame) addPointer(lTop, sizeof(LocalFrame));
  verifyStack(local);
  varFrame(lTop, -1) = (word) environment_frame;

  Mark(m);
  gc_status.blocked++;
  rval = interpret(module, goal, debug);
  gc_status.blocked--;
  Undo(m);
  lTop = lSave;
  aTop = aSave;
  environment_frame = envSave;
  unlockp(&envSave);
  unlockp(&lSave);
  assert(pSave == pTop);

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

  environment_frame = NULL;
  lTop = (LocalFrame) addPointer(lBase, sizeof(LocalFrame));
  varFrame(lTop, -1) = (word) NULL;
  tTop = tBase;
  gTop = gBase;
  aTop = aBase;
  pTop = pBase;
  gc_status.blocked   = 0;
  gc_status.requested = FALSE;
  status.arithmetic   = 0;

  lockp(&environment_frame);

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


#if O_SECURE

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
checkData(p) verifies p points to valid  Prolog  data  and  generates  a
system  error  otherwise.  The checks performed are much more rigid than
those during normal execution.  Arity of terms is limited to  100  as  a
kind of heuristic.

Note that we expect terms on the global stack.   This  is  true  in  the
interpreter,  but  not everywere in the system (records use terms on the
heap).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define onGlobal(p) onStack(global, p)
#define onLocal(p) onStack(local, p)
#define onHeap(p) ((char *)p >= (char *)hBase && (char *)p <= (char *)hTop)

trap_gdb()
{
}

static void
printk(char *fm, ...)
{ va_list args;

  va_start(args, fm);
  fprintf(stderr, "[DATA INCONSISTENCY: ");
  vfprintf(stderr, fm, args);
  fprintf(stderr, "]\n");
  va_end(args);

  trap_gdb();
}


void
checkData(p, on_heap)
register Word p;
int on_heap;
{ int arity; int n;
  register Word p2;

  while(isRef(*p))
  { p2 = unRef(*p);
    if ( !on_heap )
    { if (p2 > p)
	printk("Reference to higher address");
      if ( !onLocal(p2) && !onGlobal(p2) )
	printk("Illegal reference pointer at 0x%x --> 0x%x", p, p2);
    } else if ( !onHeap(p2) )
      printk("Illegal reference pointer at 0x%x --> 0x%x", p, p2);

    return checkData(p2, on_heap);
  }

  if ( isVar(*p) )
    return;

  if ( isInteger(*p) )
    return;

  if ( isIndirect(*p) )
  { if ( !on_heap && !onGlobal(unMask(*p)) )
      printk("Indirect data not on global");
    if ( isReal(*p) )
      return;
    if ( isString(*p) )
    { if ( sizeString(*p) != strlen(valString(*p)) )
	printk("String has inconsistent length: 0x%x", *p);
      return;
    }
    printk("Illegal indirect datatype");
  }

  if ( !on_heap )
  { if ( !onGlobal(*p) )
    { if (((Atom)(*p))->type != ATOM_TYPE)
	printk("Illegal atom: 0x%x", *p);
      return;
    }
  } else
  { if ( onStackArea(global, *p) ||
	 onStackArea(local, *p) ||
	 onStackArea(trail, *p) )
      printk("Heap term from 0x%x points to stack at 0x%x", p, *p);

    if ( isAtom(*p) )
      return;
  }
					/* now it should be a term */
  if (functorTerm(*p)->type != FUNCTOR_TYPE)
    printk("Illegal term: 0x%x", *p);
  arity = functorTerm(*p)->arity;
  if (arity <= 0 || arity > 100)
    printk("Illegal arity");
  for(n=0; n<arity; n++)
    checkData(argTermP(*p, n), on_heap);

  return;
}
#endif /* TEST */
