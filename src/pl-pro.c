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

#ifdef SECURE_GC
#define O_SECURE 1			/* include checkData() */
#endif
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
{ bool rval;

  IOSTREAM *inSave  = Scurin;
  IOSTREAM *outSave = Scurout;
  long skipSave     = debugstatus.skiplevel;
  int  suspSave     = debugstatus.suspendTrace;
  int  traceSave;
  debug_type debugSave;

  tracemode(FALSE, &traceSave);
  debugmode(DBG_OFF, &debugSave);

  Scurin  = Sinput;
  Scurout = Soutput;

  resetTracer();

  { fid_t cid = PL_open_foreign_frame();

    rval = callProlog(MODULE_user, goal, PL_Q_NORMAL, NULL);

    PL_discard_foreign_frame(cid);
  }

  debugstatus.suspendTrace = suspSave;
  debugstatus.skiplevel    = skipSave;
  tracemode(traceSave, NULL);
  debugmode(debugSave, NULL);

  Scurout = outSave;
  Scurin  = inSave;

  return rval;
}


word
pl_notrace1(term_t goal)
{ bool rval;

  long	     skipSave  = debugstatus.skiplevel;
  bool	     traceSave = debugstatus.tracing;

  rval = callProlog(NULL, goal, PL_Q_NODEBUG, NULL);

  debugstatus.skiplevel    = skipSave;
  debugstatus.tracing      = traceSave;

  return rval;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Call a prolog goal from C. The argument must  be  an  instantiated  term
like for the Prolog predicate call/1.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
callProlog(Module module, term_t goal, int flags, term_t *ex)
{ term_t g = PL_new_term_ref();
  functor_t fd;
  Procedure proc;

  if ( ex )
    *ex = 0;

  PL_strip_module(goal, &module, g);
  if ( !PL_get_functor(g, &fd) )
  { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, goal);
    if ( ex )
      *ex = exception_term;

    fail;
  }
  
  proc = lookupProcedure(fd, module);
  
  { int arity = arityFunctor(fd);
    term_t args = PL_new_term_refs(arity);
    qid_t qid;
    int n, rval;

    for(n=0; n<arity; n++)
      _PL_get_arg(n+1, g, args+n);

    qid  = PL_open_query(module, flags, proc, args);
    rval = PL_next_solution(qid);
    if ( !rval && ex )
      *ex = PL_exception(qid);
    PL_cut_query(qid);

    return rval;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Abort and toplevel. At the  moment,   prologToplevel()  sets a longjmp()
context and pl_abort() jumps to this   context and resets the SWI-Prolog
engine. 

Using the multi-threaded version, this is   not  acceptable. Each thread
needs such a context, but worse  is   that  we cannot properly reset the
reference count and ensure locks are all in a sane state. 

A cleaner solution is to  map  an   abort  onto  a Prolog exception. The
exception-handling   code   should    ensure     proper    handling   of
reference-counts and locks anyhow. Small disadvantage   is  that the old
abort()   mechanism   was   capable   of     recovering   from   serious
data-inconsistencies, while the throw-based requires   the Prolog engine
to be in a sane state.  Anyhow,   in  the multi-threaded version we have
little choice.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_PLMT
#define O_ABORT_WITH_THROW 1
#endif

static word
pl_throw_abort()
{ pl_notrace();

  if ( LD->critical > 0 )		/* abort in critical region: delay */
  { LD->aborted = TRUE;
    succeed;
  } else
  { fid_t fid = PL_open_foreign_frame();
    term_t ex = PL_new_term_ref();

    PL_put_atom(ex, ATOM_aborted);
    PL_throw(ex);			/* use longjmp() to ensure */

    PL_close_foreign_frame(fid);	/* should not be reached */
    fail;				
  }
}


#ifdef O_ABORT_WITH_THROW

word
pl_abort(abort_type type)
{ return pl_throw_abort();
}

#else /*O_ABORT_WITH_THROW*/

static jmp_buf abort_context;		/* jmp buffer for abort() */
static int can_abort;			/* embeded code can't abort */

word
pl_abort(abort_type type)
{ if ( !can_abort ||
       (trueFeature(EX_ABORT_FEATURE) && type == ABORT_NORMAL) )
    return pl_throw_abort();

  if ( LD->critical > 0 )		/* abort in critical region: delay */
  { pl_notrace();
    LD->aborted = TRUE;
    succeed;
  }

  if ( !trueFeature(READLINE_FEATURE) )
    PopTty(Sinput, &ttytab);
  LD->outofstack = NULL;
  closeFiles(FALSE);
  resetReferences();
#ifdef O_PROFILE
  pl_reset_profiler();
#endif
  resetStacks();
  resetTracer();
  resetSignals();
  resetForeign();
  resetAtoms();
  resetBags();

  longjmp(abort_context, 1);
  /*NOTREACHED*/
  fail;
}

#endif /*O_ABORT_WITH_THROW*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initial entry point from C to start  the  Prolog  engine.   Saves  abort
context,  clears  the  stack  and  finally  starts  the  virtual machine
interpreter with the toplevel goal.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
prologToplevel(volatile atom_t goal)
{ bool rval;
  volatile int aborted = FALSE;

#ifndef O_ABORT_WITH_THROW
  if ( setjmp(abort_context) != 0 )
  { if ( LD->current_signal )
      unblockSignal(LD->current_signal);
    
    aborted = TRUE;
  } else
#endif
  { debugstatus.debugging = DBG_OFF;
  }

  emptyStacks();

#ifdef O_LIMIT_DEPTH
  depth_limit   = (unsigned long)DEPTH_NO_LIMIT;
#endif

  gc_status.blocked    = 0;
  gc_status.requested  = FALSE;
#if O_SHIFT_STACKS
  shift_status.blocked = 0;
#endif
  LD->in_arithmetic    = 0;

  tracemode(FALSE, NULL);
  debugmode(DBG_OFF, NULL);
  debugstatus.suspendTrace = 0;

#ifndef O_ABORT_WITH_THROW
  can_abort = TRUE;
#endif
  for(;;)
  { fid_t fid = PL_open_foreign_frame();
    qid_t qid;
    term_t except = 0;
    Procedure p;
    word gn;

    if ( aborted )
    { aborted = FALSE;
      gn = PL_new_atom("$abort");
    } else
      gn = goal;

    p = lookupProcedure(lookupFunctorDef(gn, 0), MODULE_system);

    qid = PL_open_query(MODULE_system, PL_Q_NORMAL, p, 0);
    rval = PL_next_solution(qid);
    if ( !rval && (except = PL_exception(qid)) )
    { atom_t a;
      
      tracemode(FALSE, NULL);
      debugmode(DBG_OFF, NULL);
      if ( PL_get_atom(except, &a) && a == ATOM_aborted )
      { printMessage(ATOM_informational, PL_ATOM, ATOM_aborted);
      } else if ( !PL_is_functor(except, FUNCTOR_error2) )
      { printMessage(ATOM_error,
		     PL_FUNCTOR_CHARS, "unhandled_exception", 1,
		       PL_TERM, except);
      }
    }
    PL_close_query(qid);
    PL_discard_foreign_frame(fid);
    if ( !except )
      break;
  }
#ifndef O_ABORT_WITH_THROW
  can_abort = FALSE;
#endif

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
checkData(Word p)
{ int arity; int n;
  Word p2;

  while(isRef(*p))
  { p2 = unRef(*p);
    if ( p2 > p )
      printk("Reference to higher address");
    if ( !onLocal(p2) && !onGlobal(p2) )
      printk("Illegal reference pointer at 0x%x --> 0x%x", p, p2);

    return checkData(p2);
  }

  if ( isVar(*p) )
    return 0x737473;			/* just a random number */

  if ( isTaggedInt(*p) )
    return *p;

  if ( isIndirect(*p) )
  { Word a = addressIndirect(*p);

    if ( !onGlobal(a) )
      printk("Indirect at %p not on global stack", a);
    if ( storage(*p) != STG_GLOBAL )
      printk("Indirect data not on global");
    if ( isBignum(*p) )
      return (word) valBignum(*p);
    if ( isReal(*p) )
      return (word) valReal(*p);
    if ( isString(*p) )
    { long sz, len;

      if ( (sz=sizeString(*p)) != (len=strlen(valString(*p))) )
      { if ( sz < len )
	  printk("String has inconsistent length: 0x%x", *p);
	else if ( valString(*p)[sz] )
	  printk("String not not followed by NUL-char: 0x%x", *p);
/*	else
	  printf("String contains NUL-chars: 0x%x", *p);
*/
      }
      return *addressIndirect(*p);
    }
    printk("Illegal indirect datatype");
  }

  if ( isAtom(*p) )
  { if ( storage(*p) != STG_STATIC )
      printk("Atom doesn't have STG_STATIC");
    return *p;
  }
					/* now it should be a term */
  if ( tag(*p) != TAG_COMPOUND ||
       storage(*p) != STG_GLOBAL )
    printk("Illegal term at: %p: 0x%x", p, *p);

  { word key = 0L;
    Functor f = valueTerm(*p);

    if ( !onGlobal(f) )
      printk("Term at %p not on global stack", f);
      
    if ( tag(f->definition) != TAG_ATOM ||
         storage(f->definition) != STG_GLOBAL )
      printk("Illegal term: 0x%x", *p);
    arity = arityFunctor(f->definition);
    if (arity <= 0 || arity > 256)
      printk("Dubious arity (%d)", arity);
    for(n=0; n<arity; n++)
      key += checkData(&f->arguments[n]);

    return key;
  }
}
#endif /* TEST */
