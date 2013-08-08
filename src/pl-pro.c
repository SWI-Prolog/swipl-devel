/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2009, University of Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifdef SECURE_GC
#define O_DEBUG 1			/* include checkData() */
#endif
#include "pl-incl.h"


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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
query_loop() runs a zero-argument goal on   behalf  of the toplevel. The
reason for this to be in C is to   be able to handle exceptions that are
considered unhandled and thus  can  trap   the  debugger.  I.e., if goal
terminates due to an exception, the exception   is  reported and goal is
restarted. Before the restart, the system is   restored to a sane state.
This notably affects I/O  (reset  current  I/O   to  user  I/O)  and the
debugger.
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
      rc = FALSE;			/* Won't get any better */
      break;
    }

    if ( !rc && (except = PL_exception(qid)) )
    { atom_t a;

      tracemode(FALSE, NULL);
      debugmode(DBG_OFF, NULL);
      setPrologFlagMask(PLFLAG_LASTCALL);
      if ( PL_get_atom(except, &a) && a == ATOM_aborted )
      {
#ifdef O_DEBUGGER
        callEventHook(PLEV_ABORT);
#endif
        printMessage(ATOM_informational, PL_ATOM, ATOM_aborted);
      }
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
  int rc;
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
  { printMessage(ATOM_informational,
		 PL_FUNCTOR, FUNCTOR_break2,
	           PL_ATOM, ATOM_begin,
		   PL_INT,  LD->break_level);
  }

  rc = query_loop(goal, TRUE);

  if ( LD->break_level > 0 )
  { printMessage(ATOM_informational,
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


word
pl_notrace1(term_t goal)
{ GET_LD
  bool rval;

  uintptr_t  skipSave  = debugstatus.skiplevel;
  bool	     traceSave = debugstatus.tracing;

  rval = callProlog(NULL, goal, PL_Q_NODEBUG, NULL);

  debugstatus.skiplevel    = skipSave;
  debugstatus.tracing      = traceSave;

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
  term_t ex;
  int rval;

  startCritical;
  rval = callProlog(NULL, A1, PL_Q_CATCH_EXCEPTION, &ex);
  if ( !endCritical )
    fail;				/* aborted */

  if ( !rval && ex )
    return PL_raise_exception(ex);

  return rval;
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
  term_t reset, g, ex2;
  functor_t fd;
  Procedure proc;

  assert((Word)lTop == refFliP(fli_context, fli_context->size));

  if ( ex )
  { if ( !(ex2=PL_new_term_ref()) )
      goto error;
    reset = ex2;
    *ex = 0;
  } else
  { reset = 0;
    ex2 = 0;				/* keep compiler happy */
  }

  if ( !(g=PL_new_term_ref()) )
  { error:
    if ( ex )
      *ex = exception_term;
    return FALSE;
  }
  if ( !reset )
    reset = g;

  PL_strip_module(goal, &module, g);
  if ( !PL_get_functor(g, &fd) )
  { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, goal);
    if ( ex )
      *ex = exception_term;

    PL_reset_term_refs(g);
    fail;
  }

  proc = lookupProcedure(fd, module);

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

  return rc;
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

#define onGlobal(p) onStack(global, p)
#define onLocal(p) onStack(local, p)

static void
printk(char *fm, ...)
{ va_list args;

  va_start(args, fm);
  Sfprintf(Serror, "[DATA INCONSISTENCY: ");
  Svfprintf(Serror, fm, args);
  Sfprintf(Serror, "]\n");
  va_end(args);

  assert(0);
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
check_data(Word p, int *recursive ARG_LD)
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
                  printk("Reference to higher address");
            }
          });
    if ( p2 == p )
      printk("Reference to same address");
    if ( !onLocal(p2) && !onGlobal(p2) )
      printk("Illegal reference pointer at 0x%x --> 0x%x", p, p2);

    p = p2;
  }

  if ( isVar(*p) )
    return key+0x737473;		/* just a random number */

#ifdef O_ATTVAR
  if ( isAttVar(*p) )
  { if ( is_marked(p) )			/* loop */
    { (*recursive)++;
      return key;
    }

    key += 0x427e8ac;			/* another random number */
    p2 = valPAttVar(*p);
    mark(p);

					/* See argument_stack_to_term_refs() */
    if ( !onGlobal(p) && (!gc_status.active || p < (Word)environment_frame) )
      printk("attvar: not on global stack: 0x%x", p);
    if ( !onGlobal(p2) )
      printk("attvar: attribute not on global stack: 0x%x --> 0x%x", p, p2);
    if ( p == p2 )
      printk("attvar: self-reference: 0x%x", p);

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
      printk("Indirect at %p not on global stack", a);
    if ( storage(*p) != STG_GLOBAL )
      printk("Indirect data not on global");
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
	    printk("String has inconsistent length: 0x%x", *p);
	  else if ( s[sz] )
	    printk("String not followed by NUL-char: 0x%x", *p);
/*	else
	    printf("String contains NUL-chars: 0x%x", *p);
*/
	}
      } else
      { size_t sz, len;
	pl_wchar_t *s;

	s = getCharsWString(*p, &sz);

	if ( sz != (len=wcslen(s)) )
	{ if ( sz < len )
	    printk("String has inconsistent length: 0x%x", *p);
	  else if ( s[sz] )
	    printk("String not followed by NUL-char: 0x%x", *p);
	}
      }
      return key + *addressIndirect(*p);
    }
#ifdef O_GMP
    if ( isMPZNum(*p) )
      return 0x62f8da3c;		/* TBD: make key from MPZ */
#endif
    printk("Illegal indirect datatype");
    return key;
  }

  if ( isAtom(*p) )
  { size_t idx;
    size_t mx = GD->atoms.highest;

    assert(!is_marked(p));
    if ( storage(*p) != STG_STATIC )
      printk("Atom doesn't have STG_STATIC");

    idx = indexAtom(*p);
    if ( idx >= mx )
      printk("Atom index out of range (%ld > %ld)", idx, mx);
    return key + *p;
  }
  if ( tagex(*p) == (TAG_VAR|STG_RESERVED) )
    return key + *p;			/* Used by read_term/2,3 and compiler */

					/* now it should be a term */
  if ( tag(*p) != TAG_COMPOUND ||
       storage(*p) != STG_GLOBAL )
    printk("Illegal term at: %p: 0x%x", p, *p);

  if ( is_marked(p) )
  { (*recursive)++;
    return key;				/* recursive */
  }

  { Functor f = valueTerm(*p);

    if ( !onGlobal(f) )
      printk("Term at %p not on global stack", f);

    if ( tag(f->definition) != TAG_ATOM ||
         storage(f->definition) != STG_GLOBAL )
      printk("Illegal functor: 0x%x", *p);
    if ( f->definition & MARK_MASK )
      printk("functor with mark: 0x%x", *p);
    if ( f->definition & FIRST_MASK )
      printk("functor with first: 0x%x", *p);
    arity = arityFunctor(f->definition);
    if ( arity < 0 )
      printk("Illegal arity (%d)", arity);
    else
      DEBUG(CHK_HIGH_ARITY,
            { if ( arity > 256 && !is_ht_capacity(arity) )
                printk("Dubious arity (%d)", arity);
            });

    mark(p);
    for(n=0; n<arity-1; n++)
      key += check_data(&f->arguments[n], recursive PASS_LD);

    p = &f->arguments[n];
    goto last_arg;
  }
}


word
checkData(Word p)
{ GET_LD
  int recursive = 0;
  word key;

  key = check_data(p, &recursive PASS_LD);
  unmark_data(p PASS_LD);

  return key;
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
  PRED_DEF("$sig_atomic", 1, sig_atomic, PL_FA_TRANSPARENT)
  PRED_DEF("$trap_gdb", 0, trap_gdb, 0)
EndPredDefs
