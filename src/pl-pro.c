/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Support for virtual machine
*/

#ifdef SECURE_GC
#define O_SECURE 1			/* include checkData() */
#endif
#include "pl-incl.h"
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
{ fid_t cid = PL_open_foreign_frame();
  term_t goal = PL_new_term_ref();
  word rval;

  PL_put_atom_chars(goal, "$break");
  rval = pl_break1(goal);
  PL_discard_foreign_frame(cid);

  return rval;
}


word
pl_break1(term_t goal)
{ extern int Input, Output;
  bool rval;

  int  inSave    = Input;
  int  outSave   = Output;
  long skipSave  = debugstatus.skiplevel;
  int  suspSave  = debugstatus.suspendTrace;
  int  traceSave, debugSave;

  tracemode(FALSE, &traceSave);
  debugmode(FALSE, &debugSave);

  Input = 0;
  Output = 1;

  debugstatus.skiplevel = 0;
  debugstatus.suspendTrace = 0;

  { fid_t cid = PL_open_foreign_frame();

    rval = callProlog(MODULE_user, goal, FALSE);

    PL_discard_foreign_frame(cid);
  }

  debugstatus.suspendTrace = suspSave;
  debugstatus.skiplevel    = skipSave;
  tracemode(traceSave, NULL);
  debugmode(debugSave, NULL);

  Output = outSave;
  Input = inSave;

  return rval;
}


word
pl_notrace1(term_t goal)
{ bool rval;

  long	     skipSave  = debugstatus.skiplevel;
  bool	     traceSave = debugstatus.tracing;

  rval = callProlog(NULL, goal, FALSE);

  debugstatus.skiplevel    = skipSave;
  debugstatus.tracing      = traceSave;

  return rval;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Call a prolog goal from C. The argument must  be  an  instantiated  term
like for the Prolog predicate call/1.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
callProlog(Module module, term_t goal, int debug)
{ term_t g = PL_new_term_ref();
  FunctorDef fd;
  Procedure proc;

  PL_strip_module(goal, &module, g);
  if ( !PL_get_functor(g, &fd) )
    return warning("callProlog(): Illegal goal");
  
  proc = lookupProcedure(fd, module);
  
  { int arity = fd->arity;
    term_t args = PL_new_term_refs(arity);
    qid_t qid;
    int n, rval;

    for(n=0; n<arity; n++)
      PL_get_arg(n+1, g, args+n);

    qid  = PL_open_query(module, debug, proc, args);
    rval = PL_next_solution(qid);
    PL_cut_query(qid);

    return rval;
  }
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

  if ( !trueFeature(READLINE_FEATURE) )
    PopTty(&ttytab);
  status.outofstack = FALSE;
  resetRead();
  closeFiles();
  resetReferences();
  resetForeign();
#ifdef O_PROFILE
  pl_reset_profiler();
#endif
  resetStacks();
  resetTracer();
  resetSignals();

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
prolog(volatile atom_t goal)
{ bool rval;

  if ( setjmp(abort_context) != 0 )
  { goal = ATOM_abort;
  } else
  { debugstatus.debugging = FALSE;
  }

  environment_frame = NULL;
  fli_context       = NULL;
  lTop = lBase;
  tTop = tBase;
  gTop = gBase;
  aTop = aBase;

#ifdef O_LIMIT_DEPTH
  depth_limit   = (unsigned long)DEPTH_NO_LIMIT;
#endif

  PL_open_foreign_frame();

  gc_status.blocked    = 0;
  gc_status.requested  = FALSE;
#if O_SHIFT_STACKS
  shift_status.blocked = 0;
#endif
  status.arithmetic    = 0;

  tracemode(FALSE, NULL);
  debugmode(FALSE, NULL);
  debugstatus.suspendTrace = 0;

  can_abort = TRUE;
  { fid_t cid = PL_open_foreign_frame();
    Procedure p = lookupProcedure(lookupFunctorDef(goal, 0), MODULE_system);
    qid_t qid  = PL_open_query(MODULE_system, TRUE, p, 0);
    rval = PL_next_solution(qid);
    PL_close_query(qid);
    PL_discard_foreign_frame(cid);
  }
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

#if O_SECURE || O_DEBUG || defined(O_MAINTENANCE)

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
    { if ( p2 > p )
	printk("Reference to higher address");
      if ( !onLocal(p2) && !onGlobal(p2) )
	printk("Illegal reference pointer at 0x%x --> 0x%x", p, p2);
    } else if ( !onHeap(p2) )
      printk("Illegal reference pointer at 0x%x --> 0x%x", p, p2);

    return checkData(p2, on_heap);
  }

  if ( isVar(*p) )
    return 0x737473;			/* just a random number */

  if ( isTaggedInt(*p) )
    return *p;

  if ( isIndirect(*p) )
  { if ( storage(*p) != STG_GLOBAL )
      printk("Indirect data not on global");
    if ( isBignum(*p) )
      return (word) valBignum(*p);
    if ( isReal(*p) )
      return (word) valReal(*p);
    if ( isString(*p) )
    { if ( sizeString(*p) != strlen(valString(*p)) )
	printk("String has inconsistent length: 0x%x", *p);
      return *addressIndirect(*p);
    }
    printk("Illegal indirect datatype");
  }

  if ( isAtom(*p) )
    return *p;
					/* now it should be a term */
  { word key = 0L;
    Functor f = valueTerm(*p);

    if ( tag(f->definition) != TAG_ATOM ||
         storage(f->definition != STG_GLOBAL) )
      printk("Illegal term: 0x%x", *p);
    arity = arityFunctor(f->definition);
    if (arity <= 0 || arity > 100)
      printk("Illegal arity");
    for(n=0; n<arity; n++)
      key += checkData(&f->arguments[n], on_heap);

    return key;
  }
}
#endif /* TEST */
