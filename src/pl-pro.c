/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2017, University of Amsterdam
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

#ifdef SECURE_GC
#define O_DEBUG 1			/* include checkData() */
#endif
#include "pl-incl.h"
#include "os/pl-cstack.h"


		/********************************
		*    CALLING THE INTERPRETER    *
		*********************************/

static int
resetProlog(int clear_stacks)
{ GET_LD
  IOSTREAM *in = Suser_input;

  if ( Sferror(in) )
  { Sclearerr(in);
    LD->prompt.next = TRUE;
  }

  Scurin  = in;
  Scurout = Suser_output;

  PL_clear_exception();
  resetTracer();

  if ( clear_stacks )
  { if ( !LD->gvar.nb_vars )		/* we would loose nb_setval/2 vars */
      emptyStacks();

    gc_status.blocked        = 0;
    LD->shift_status.blocked = 0;
    LD->in_arithmetic        = 0;
    LD->in_print_message     = 0;
  }

#ifdef O_LIMIT_DEPTH
  depth_limit   = (uintptr_t)DEPTH_NO_LIMIT;
#endif

  updateAlerted(LD);

  return TRUE;
}


static int
restore_after_exception(term_t except)
{ GET_LD
  atom_t a;
  int rc = TRUE;

  tracemode(FALSE, NULL);
  debugmode(DBG_OFF, NULL);
  setPrologFlagMask(PLFLAG_LASTCALL);
  if ( PL_get_atom(except, &a) && a == ATOM_aborted )
  { rc = ( callEventHook(PLEV_ABORT) &&
	   printMessage(ATOM_informational, PL_ATOM, ATOM_aborted) );
  }

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
query_loop() runs a zero-argument goal on   behalf  of the toplevel. The
reason for this to be in C is to   be able to handle exceptions that are
considered unhandled and thus  can  trap   the  debugger.  I.e., if goal
terminates due to an exception, the exception   is  reported and goal is
restarted. Before the restart, the system is   restored to a sane state.
This notably affects I/O  (reset  current  I/O   to  user  I/O)  and the
debugger.  Return: FALSE: failed, TRUE: success, -1: exception.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
query_loop(atom_t goal, int loop)
{ GET_LD
  int rc;
  int clear_stacks = (LD->query == NULL);

  do
  { fid_t fid;
    qid_t qid = 0;
    term_t except = 0;
    predicate_t p;

    if ( !resetProlog(clear_stacks) )
      goto error;
    if ( !(fid = PL_open_foreign_frame()) )
      goto error;

    p = PL_pred(PL_new_functor(goal, 0), MODULE_system);

    if ( (qid = PL_open_query(MODULE_system, PL_Q_NORMAL, p, 0)) )
    { rc = PL_next_solution(qid);
    } else
    { error:
      except = exception_term;
      rc = -1;				/* Won't get any better */
      break;
    }

    if ( !rc && (except = PL_exception(qid)) )
    { restore_after_exception(except);
      rc = -1;
    }

    if ( qid ) PL_close_query(qid);
    if ( fid ) PL_discard_foreign_frame(fid);
    if ( !except )
      break;
  } while(loop);

  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Starts a new Prolog toplevel.  Resets I/O to point to the user and stops
the debugger.  Restores I/O and debugger on exit.  The Prolog  predicate
`$break' is called to actually built the break environment.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
pl_break1(atom_t goal)
{ GET_LD
  int rc = TRUE;
  int old_level = LD->break_level;

  IOSTREAM *inSave  = Scurin;
  IOSTREAM *outSave = Scurout;
  intptr_t skipSave = debugstatus.skiplevel;
  int  suspSave     = debugstatus.suspendTrace;
  int  traceSave;
  debug_type debugSave;

  tracemode(FALSE, &traceSave);
  debugmode(DBG_OFF, &debugSave);

  Scurin  = Sinput;
  Scurout = Soutput;

  LD->break_level++;
  if ( LD->break_level > 0 )
  { rc = printMessage(ATOM_informational,
		      PL_FUNCTOR, FUNCTOR_break2,
		        PL_ATOM, ATOM_begin,
		        PL_INT,  LD->break_level);
  }

  rc = rc && (query_loop(goal, TRUE) == TRUE);

  if ( LD->break_level > 0 )
  { rc = rc && printMessage(ATOM_informational,
			    PL_FUNCTOR, FUNCTOR_break2,
			      PL_ATOM, ATOM_end,
			      PL_INT,  LD->break_level);
  }
  LD->break_level = old_level;

  debugstatus.suspendTrace = suspSave;
  debugstatus.skiplevel    = skipSave;
  tracemode(traceSave, NULL);
  debugmode(debugSave, NULL);

  Scurout = outSave;
  Scurin  = inSave;

  return rc;
}


/** break

Run a nested toplevel. Do not  use   PRED_IMPL()  for this because it is
very handy to use from e.g., the gdb debugger.
*/

word
pl_break(void)
{ GET_LD
  wakeup_state wstate;

  if ( saveWakeup(&wstate, TRUE PASS_LD) )
  { word rc;

    rc = pl_break1(ATOM_dquery_loop);
    restoreWakeup(&wstate PASS_LD);

    return rc;
  }

  return FALSE;
}


int
currentBreakLevel(void)
{ GET_LD

  return LD->break_level;
}


static
PRED_IMPL("notrace", 1, notrace, PL_FA_TRANSPARENT|PL_FA_NOTRACE)
{ PRED_LD
  int rval;
  term_t ex;

  uintptr_t  skipSave  = debugstatus.skiplevel;
  bool	     traceSave = debugstatus.tracing;

  rval = callProlog(NULL, A1, PL_Q_CATCH_EXCEPTION|PL_Q_NODEBUG, &ex);

  debugstatus.skiplevel    = skipSave;
  debugstatus.tracing      = traceSave;

  if ( !rval && ex )
    return PL_raise_exception(ex);

  return rval;
}

#undef LD
#define LD LOCAL_LD

/** '$sig_atomic'(:Goal) is semidet.

Execute Goal as once/1 while blocking signals.

@see setup_call_catcher_cleanup/4 in boot/init.pl
@see callCleanupHandler() uses the same mechanism to protect the cleanup
*/

static
PRED_IMPL("$sig_atomic", 1, sig_atomic, PL_FA_TRANSPARENT)
{ PRED_LD
  int rval;

  startCritical;
  rval = callProlog(NULL, A1, PL_Q_PASS_EXCEPTION, NULL);
  if ( !endCritical )
    fail;				/* aborted */

  return rval;
}


/** '$call_no_catch'(:Goal)
 *
 * Runs a goal for the toplevel.  This notably means that exceptions
 * are considered _uncaught_, are printed and ignored.  Also the
 * truth value is ignored.
 */

static
PRED_IMPL("$call_no_catch", 1, call_no_catch, PL_FA_TRANSPARENT)
{ int rc;
  term_t ex;

  rc = callProlog(NULL, A1, PL_Q_NORMAL, &ex);
  if ( !rc && ex )
  { restore_after_exception(ex);
    PL_clear_exception();
  }

  return TRUE;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Call a prolog goal from C. The argument must  be  an  instantiated  term
like for the Prolog predicate call/1.

Note that the caller must provide a   foreign context. We cannot do that
here because closing will loose the exception.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
callProlog(Module module, term_t goal, int flags, term_t *ex)
{ GET_LD
  term_t reset=0, g, ex2 = 0;
  functor_t fd;
  Procedure proc;

  assert((Word)lTop == refFliP(fli_context, fli_context->size));

  if ( ex )
  { if ( !(ex2=PL_new_term_ref()) )
      goto error;
    *ex = 0;
  }

  if ( !(g=PL_new_term_ref()) )
  { error:
    if ( ex )
      *ex = exception_term;
    return FALSE;
  }

  if ( !PL_strip_module(goal, &module, g) )
    return FALSE;
  if ( !PL_get_functor(g, &fd) )
  { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, goal);
    if ( ex )
      *ex = exception_term;

    PL_reset_term_refs(g);
    fail;
  }

  proc = resolveProcedure(fd, module);

  { int arity = arityFunctor(fd);
    term_t args;
    qid_t qid = 0;
    int n, rval;

    if ( (args = PL_new_term_refs(arity)) )
    { for(n=0; n<arity; n++)
	_PL_get_arg(n+1, g, args+n);

      if ( (qid = PL_open_query(module, flags, proc, args)) )
      { rval = PL_next_solution(qid);
      } else
	goto error;
    } else
      goto error;

    if ( !rval && ex )
    { term_t qex = PL_exception(qid);

      if ( qex )
      { PL_put_term(ex2, qex);
	*ex = ex2;
	reset = g;
      } else
      { *ex = 0;
      }
    }

    PL_cut_query(qid);
    if ( !reset )
      reset = (ex2 ? ex2 : g);
    PL_reset_term_refs(reset);

    return rval;
  }
}


int
abortProlog(void)
{ GET_LD
  fid_t fid;
  term_t ex;
  int rc = FALSE;

  pl_notrace();
  Sreset();				/* Discard pending IO */

  LD->exception.processing = TRUE;	/* allow using spare stack */

  if ( (fid = PL_open_foreign_frame()) &&
       (ex = PL_new_term_ref()) )
  { clearSegStack(&LD->cycle.lstack);	/* can do no harm */
    clearSegStack(&LD->cycle.vstack);

    PL_put_atom(ex, ATOM_aborted);
    rc = PL_raise_exception(ex);
    PL_close_foreign_frame(fid);
  }

#ifdef O_MAINTENANCE
  save_backtrace("abort");
#endif

  return rc;
}


static
PRED_IMPL("abort", 0, abort, 0)
{ return abortProlog();
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
prologToplevel is called with various goals

  - Using '$initialise to run the initialization
  - Using '$compile'   if Prolog is called with -c ...
  - Using '$toplevel'  from PL_toplevel() to run the interactive toplevel

It can only be ran when there is  no actively running Prolog goal (i.e.,
it is not reentrant).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bool
prologToplevel(atom_t goal)
{ GET_LD
  int rc;
  int old_level = LD->break_level;
  int loop;

  if ( goal == ATOM_dquery_loop ||
       goal == ATOM_dtoplevel )
  { LD->break_level++;
    loop = TRUE;
  } else
    loop = FALSE;
  rc = query_loop(goal, loop);
  LD->break_level = old_level;

  return rc == TRUE;
}


access_level_t
setAccessLevel(access_level_t accept)
{ GET_LD
  bool old;

  old = LD->prolog_flag.access_level;
  LD->prolog_flag.access_level = accept;
  return old;
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

static
PRED_IMPL("$trap_gdb", 0, trap_gdb, 0)
{ trap_gdb();
  return TRUE;
}

#if O_DEBUG || defined(O_MAINTENANCE)
#define HAVE_CHECK_DATA 1

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
checkData(p) verifies p points to valid  Prolog  data  and  generates  a
system  error  otherwise.  The checks performed are much more rigid than
those during normal execution.  Arity of terms is limited to  100  as  a
kind of heuristic.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct
{ int recursive;
  int errors;
  int flags;
} chk_data;

#define onGlobal(p) onStack(global, p)
#define onLocal(p) onStack(local, p)

static void
printk(chk_data *context, char *fm, ...)
{ va_list args;

  va_start(args, fm);
  Sfprintf(Serror, "[DATA INCONSISTENCY: ");
  Svfprintf(Serror, fm, args);
  Sfprintf(Serror, "]\n");
  va_end(args);

  context->errors++;
  trap_gdb();
}

static intptr_t check_marked;

#define mark(p)		(*(p) |= MARK_MASK, check_marked++)
#define unmark(p)	(*(p) &= ~MARK_MASK, check_marked--)

static void
unmark_data(Word p ARG_LD)
{
last_arg:
  deRef(p);

  if ( isAttVar(*p) && is_marked(p) )
  { unmark(p);
    p = valPAttVar(*p);
    goto last_arg;
  } else if ( isTerm(*p) && is_marked(p) )
  { Functor f;
    int n, arity;

    unmark(p);
    f = valueTerm(*p);
    arity = arityFunctor(f->definition);
    for(n=0; n<arity-1; n++)
      unmark_data(&f->arguments[n] PASS_LD);
    p = &f->arguments[n];
    goto last_arg;
  }
}


#ifdef O_DEBUG
static int				/* avoid false alarm in CHR */
is_ht_capacity(int arity)
{ int cap = 89;				/* chr_hashtable_store.pl */

  while(cap < arity)
    cap = cap*2+1;

  return cap == arity;
}
#endif

static word
check_data(Word p, chk_data *context ARG_LD)
{ int arity; int n;
  Word p2;
  word key = 0L;

last_arg:

  while(isRef(*p))
  { assert(!is_marked(p));
    p2 = unRef(*p);
    DEBUG(CHK_HIGHER_ADDRESS,
          { if ( p2 > p )
            {
#ifdef O_ATTVAR
              if ( !isAttVar(*p2) )
#endif
                if ( !gc_status.blocked )
                  printk(context, "Reference to higher address");
            }
          });
    if ( p2 == p )
      printk(context, "Reference to same address");
    if ( !onLocal(p2) && !onGlobal(p2) )
      printk(context, "Illegal reference pointer at %p --> %p", p, p2);

    p = p2;
  }

  if ( isVar(*p) )
    return key+0x737473;		/* just a random number */

#ifdef O_ATTVAR
  if ( isAttVar(*p) )
  { if ( is_marked(p) )			/* loop */
    { context->recursive++;
      return key;
    }

    key += 0x427e8ac;			/* another random number */
    p2 = valPAttVar(*p);
    mark(p);

					/* See argument_stack_to_term_refs() */
    if ( !onGlobal(p) && (!gc_status.active || p < (Word)environment_frame) )
      printk(context, "attvar: not on global stack: %p", p);
    if ( !onGlobal(p2) )
      printk(context, "attvar: attribute not on global stack: %p --> %p", p, p2);
    if ( p == p2 )
      printk(context, "attvar: self-reference: %p", p);
    if ( !(context->flags&CHK_DATA_NOATTVAR_CHAIN) && !on_attvar_chain(p) )
      printk(context, "attvar: not on attvar chain: %p", p);

    p = p2;
    goto last_arg;
  }
#endif

  if ( isTaggedInt(*p) )
  { assert(!is_marked(p));
    return key + *p;
  }

  if ( isIndirect(*p) )
  { Word a = addressIndirect(*p);

    assert(!is_marked(p));

    if ( !onGlobal(a) )
      printk(context, "Indirect at %p not on global stack", a);
    if ( storage(*p) != STG_GLOBAL )
      printk(context, "Indirect data not on global");
    if ( isBignum(*p) )
      return key+(word) valBignum(*p);
    if ( isFloat(*p) )
      return key+(word) valFloat(*p);
    if ( isString(*p) )
    { if ( isBString(*p) )
      { size_t sz, len;
	char *s;

	s = getCharsString(*p, &sz);

	if ( sz != (len=strlen(s)) )
	{ if ( sz < len )
	    printk(context, "String has inconsistent length: %p", *p);
	  else if ( s[sz] )
	    printk(context, "String not followed by NUL-char: %p", *p);
/*	else
	    printf("String contains NUL-chars: %p", *p);
*/
	}
      } else
      { size_t sz, len;
	pl_wchar_t *s;

	s = getCharsWString(*p, &sz);

	if ( sz != (len=wcslen(s)) )
	{ if ( sz < len )
	    printk(context, "String has inconsistent length: %p", *p);
	  else if ( s[sz] )
	    printk(context, "String not followed by NUL-char: %p", *p);
	}
      }
      return key + *addressIndirect(*p);
    }
#ifdef O_GMP
    if ( isMPZNum(*p) )
      return 0x62f8da3c;		/* TBD: make key from MPZ */
#endif
    printk(context, "Illegal indirect datatype");
    return key;
  }

  if ( isAtom(*p) )
  { size_t idx;
    size_t mx = GD->atoms.highest;

    assert(!is_marked(p));
    if ( storage(*p) != STG_STATIC )
      printk(context, "Atom doesn't have STG_STATIC");

    idx = indexAtom(*p);
    if ( idx >= mx )
      printk(context, "Atom index out of range (%ld > %ld)", idx, mx);
    return key + *p;
  }
  if ( tagex(*p) == (TAG_VAR|STG_RESERVED) )
    return key + *p;			/* Used by read_term/2,3 and compiler */

					/* now it should be a term */
  if ( tag(*p) != TAG_COMPOUND ||
       storage(*p) != STG_GLOBAL )
    printk(context, "Illegal term at: %p: %p", p, *p);

  if ( is_marked(p) )
  { context->recursive++;
    return key;				/* recursive */
  }

  { Functor f = valueTerm(*p);

    if ( !onGlobal(f) )
      printk(context, "Term at %p not on global stack", f);

    if ( tag(f->definition) != TAG_ATOM ||
         storage(f->definition) != STG_GLOBAL )
      printk(context, "Illegal functor: %p", *p);
    if ( f->definition & MARK_MASK )
      printk(context, "functor with mark: %p", *p);
    if ( f->definition & FIRST_MASK )
      printk(context, "functor with first: %p", *p);
    arity = arityFunctor(f->definition);
    if ( arity < 0 )
      printk(context, "Illegal arity (%d)", arity);
    else if ( arity == 0 )
      return key;
    else
      DEBUG(CHK_HIGH_ARITY,
            { if ( arity > 256 && !is_ht_capacity(arity) )
                printk(context, "Dubious arity (%d)", arity);
            });

    mark(p);
    for(n=0; n<arity-1; n++)
      key += check_data(&f->arguments[n], context PASS_LD);

    p = &f->arguments[n];
    goto last_arg;
  }
}


word
checkDataEx(Word p, int flags)
{ GET_LD
  chk_data context = {0};
  word key;

  context.flags = flags;
  key = check_data(p, &context PASS_LD);
  unmark_data(p PASS_LD);

  return key;
}

word
checkData(Word p)
{ return checkDataEx(p, 0);
}

#endif /* TEST */

int
PL_check_data(term_t data)
{
#ifdef HAVE_CHECK_DATA
  GET_LD

  (void)checkData(valTermRef(data));
  return TRUE;
#else
  return FALSE;
#endif
}


		 /*******************************
		 *         LLVM-GCC HACK	*
		 *******************************/

/* This avoids an optimizer bug in llvm-gcc-4.2 as distributed with
   MacOS Lion.  Called from pl-vmi.c in I_EXITCLEANUP.
*/

#ifdef __llvm__
int
llvm_dummy(void)
{ return 0;
}
#endif

		 /*******************************
		 *	      MISC		*
		 *******************************/

int
getAccessLevelMask(atom_t a, access_level_t *val)
{ if ( a == ATOM_user )
    *val = ACCESS_LEVEL_USER;
  else if ( a == ATOM_system )
    *val = ACCESS_LEVEL_SYSTEM;
  else
    return FALSE;

  return TRUE;
}


atom_t
accessLevel(void)
{ GET_LD

  switch(LD->prolog_flag.access_level)
  { case ACCESS_LEVEL_USER:	return ATOM_user;
    case ACCESS_LEVEL_SYSTEM:	return ATOM_system;
  }

  return NULL_ATOM;
}





		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(pro)
  PRED_DEF("abort", 0, abort, 0)
  PRED_DEF("notrace", 1, notrace, PL_FA_TRANSPARENT|PL_FA_NOTRACE)
  PRED_DEF("$sig_atomic", 1, sig_atomic, PL_FA_TRANSPARENT)
  PRED_DEF("$trap_gdb", 0, trap_gdb, 0)
  PRED_DEF("$call_no_catch", 1, call_no_catch, PL_FA_TRANSPARENT)
EndPredDefs
