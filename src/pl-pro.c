/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Support for virtual machine
*/

#define O_SECURE 1			/* include checkData() */
#include "pl-incl.h"
#include "pl-itf.h"
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

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
pl_break1(Word goal)
{ extern int Input, Output;
  bool rval;
  mark m;

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

  Mark(m);
  rval = callGoal(MODULE_user, *goal, FALSE);
  Undo(m);

  debugstatus.suspendTrace = suspSave;
  debugstatus.skiplevel    = skipSave;
  debugstatus.debugging    = debugSave;
  debugstatus.tracing      = traceSave;

  Output = outSave;
  Input = inSave;

  return rval;
}


word
pl_notrace1(Word goal)
{ bool rval;

  long	     skipSave  = debugstatus.skiplevel;
  bool	     traceSave = debugstatus.tracing;
  bool	     debugSave = debugstatus.debugging;
  int	     suspSave  = debugstatus.suspendTrace;

  debugstatus.tracing = FALSE;
  debugstatus.debugging = FALSE;
  debugstatus.skiplevel = 0;
  debugstatus.suspendTrace = 1;

  rval = callGoal(NULL, *goal, FALSE);

  debugstatus.suspendTrace = suspSave;
  debugstatus.skiplevel    = skipSave;
  debugstatus.debugging    = debugSave;
  debugstatus.tracing      = traceSave;

  return rval;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Call a prolog goal from C. The argument must  be  an  instantiated  term
like for the Prolog predicate call/1.  The goal is executed in a kind of
break environment and thus bindings which result of the call are lost.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
callGoal(Module module, word goal, bool debug)
{ FunctorDef fd;
  Module m;
  Procedure proc;
  Word g, ap;
  
  if ( (m=module) == NULL )
  { if ( environment_frame )
      m = contextModule(environment_frame);
    else
      m = MODULE_user;
  }

  TRY(g = stripModule(&goal, &m));
  goal = *g;

  if ( isAtom(goal) )
  { fd = lookupFunctorDef((Atom)goal, 0);
    ap = NULL;
  } else if ( isTerm(goal) )
  { fd = functorTerm(goal);
    ap = argTermP(goal, 0);
  } else
    return warning("Illegal goal whiled called from C");

  proc = lookupProcedure(fd, m);

  if ( ap )
  { TermVector(argv, fd->arity);
    Word *av = argv;
    int n;

    for( n=fd->arity; n-- > 0; )
      *av++ = ap++;

    return PL_call_predicate(m, debug, proc, argv);
  } else
    return PL_call_predicate(m, debug, proc, NULL);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Bring the Prolog system itself to life.  Prolog  saves  the  C-stack  to
enable  aborts.   pl_abort()  will  close  open  files, reset all clause
references to `0' and finally long_jumps back to prolog().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static jmp_buf abort_context;		/* jmp buffer for abort() */
static int can_abort;			/* embeded code can't abort */

word
pl_abort()
{ if ( !can_abort )
  { warning("Embedded system, cannot abort");
    Halt(1);
  }

  if (critical > 0)			/* abort in critical region: delay */
  { aborted = TRUE;
    succeed;
  }
#if !O_READLINE
  PopTty(&ttytab);
#endif
  resetRead();
  closeFiles();
  resetReferences();
  resetForeign();
#ifdef O_PROFILE
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
prolog(volatile word goal)
{ bool rval;

  if ( setjmp(abort_context) != 0 )
  { goal = (word) ATOM_abort;
  } else
  { debugstatus.debugging = FALSE;
  }

  environment_frame = NULL;
  lTop = lBase;
  tTop = tBase;
  gTop = gBase;
  aTop = aBase;
  pTop = pBase;

  gc_status.blocked    = -1;
  gc_status.requested  = FALSE;
#if O_SHIFT_STACKS
  shift_status.blocked = 0;
#endif
  status.arithmetic    = 0;

  lockp(&environment_frame);

  debugstatus.tracing      = FALSE;
  debugstatus.suspendTrace = 0;

  can_abort = TRUE;
  rval = callGoal(MODULE_system, goal, FALSE);
  can_abort = FALSE;

  return rval;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Cut (!) as called via the  meta-call  mechanism has no effect.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_metacut(void)
{ succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Just for debugging now and then.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
trap_gdb()
{ return 0;
}

#if O_SECURE || O_DEBUG

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

static void
printk(char *fm, ...)
{ va_list args;

  va_start(args, fm);
  Sfprintf(Serror, "[DATA INCONSISTENCY: ");
  Svfprintf(Serror, fm, args);
  Sfprintf(Serror, "]\n");
  va_end(args);

  trap_gdb();
}


word
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
    return 0x737473;			/* just a random number */

  if ( isInteger(*p) )
    return *p;

  if ( isIndirect(*p) )
  { if ( !on_heap && !onGlobal(unMask(*p)) )
      printk("Indirect data not on global");
    if ( isReal(*p) )
      return (word) valReal(*p);
    if ( isString(*p) )
    { if ( sizeString(*p) != strlen(valString(*p)) )
	printk("String has inconsistent length: 0x%x", *p);
      return *(Word)unMask(*p);
    }
    printk("Illegal indirect datatype");
  }

  if ( !on_heap )
  { if ( !onGlobal(*p) )
    { if (((Atom)(*p))->type != ATOM_TYPE)
	printk("Illegal atom: 0x%x", *p);
      return *p;
    }
  } else
  { if ( onStackArea(global, *p) ||
	 onStackArea(local, *p) ||
	 onStackArea(trail, *p) )
      printk("Heap term from 0x%x points to stack at 0x%x", p, *p);

    if ( isAtom(*p) )
      return *p;
  }
					/* now it should be a term */
  { word key = 0L;

    if (functorTerm(*p)->type != FUNCTOR_TYPE)
      printk("Illegal term: 0x%x", *p);
    arity = functorTerm(*p)->arity;
    if (arity <= 0 || arity > 100)
      printk("Illegal arity");
    for(n=0; n<arity; n++)
      key += checkData(argTermP(*p, n), on_heap);

    return key;
  }
}
#endif /* TEST */
