/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2021, University of Amsterdam
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

#include "pl-trace.h"
#include "pl-comp.h"
#include "os/pl-ctype.h"
#include "os/pl-cstack.h"
#include "pl-inline.h"
#include "pl-dbref.h"
#include "pl-fli.h"
#include "pl-gc.h"
#include "pl-rec.h"
#include "pl-prims.h"
#include "pl-attvar.h"
#include "pl-pro.h"
#include "pl-util.h"
#include "pl-wam.h"
#include "pl-write.h"
#include "pl-setup.h"
#include "pl-proc.h"
#include "os/pl-prologflag.h"
#include <stdio.h>

#define WFG_TRACING	0x02000
#define WFG_BACKTRACE	0x04000
#define WFG_CHOICE	0x08000

#define TRACE_FIND_NONE	0
#define TRACE_FIND_ANY	1
#define TRACE_FIND_NAME	2
#define TRACE_FIND_TERM	3

typedef struct find_data_tag
{ int	 port;				/* Port to find */
  bool	 searching;			/* Currently searching? */
  int	 type;				/* TRACE_FIND_* */
  union
  { atom_t	name;			/* Name of goal to find */
    struct
    { functor_t	functor;		/* functor of the goal */
      Record	term;			/* Goal to find */
    } term;
  } goal;
} find_data;


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Convert between integer frame reference and LocalFrame pointer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
PL_unify_frame(term_t t, LocalFrame fr)
{ GET_LD

  if ( fr )
  { assert(fr >= lBase && fr < lTop);

    return PL_unify_integer(t, (Word)fr - (Word)lBase);
  } else
    return PL_unify_atom(t, ATOM_none);
}


int
PL_put_frame(term_t t, LocalFrame fr)
{ GET_LD

  if ( fr )
  { assert(fr >= lBase && fr < lTop);

    return PL_put_intptr(t, (Word)fr - (Word)lBase);
  } else
    return PL_put_atom(t, ATOM_none);
}


static int
PL_get_frame(term_t r, LocalFrame *fr)
{ GET_LD
  intptr_t i;
  atom_t a;

  if ( PL_get_intptr(r, &i) )
  { LocalFrame f = ((LocalFrame)((Word)lBase + i));

    if ( !(f >= lBase && f < lTop) )
      fail;
    *fr = f;

    succeed;
  } else if ( PL_get_atom(r, &a) && a == ATOM_none )
  { *fr = NULL;

    succeed;
  }

  fail;
}


void
PL_put_choice(term_t t, Choice ch)
{ GET_LD

  if ( ch )
  { assert(ch >= (Choice)lBase && ch < (Choice)lTop);

    PL_put_intptr(t, (Word)ch - (Word)lBase);
  } else
    PL_put_atom(t, ATOM_none);
}


static int
PL_unify_choice(term_t t, Choice ch)
{ GET_LD

  if ( ch )
  { assert(ch >= (Choice)lBase && ch < (Choice)lTop);

    return PL_unify_integer(t, (Word)ch - (Word)lBase);
  } else
    return PL_unify_atom(t, ATOM_none);
}


#define valid_choice(ch) LDFUNC(valid_choice, ch)
static inline int
valid_choice(DECL_LD Choice ch)
{ if ( (int)ch->type >= 0 && (int)ch->type <= CHP_DEBUG &&
       onStack(local, ch->frame) )
    return TRUE;

  return FALSE;
}


static int
PL_get_choice(term_t r, Choice *chp)
{ GET_LD
  long i;

  if ( PL_get_long(r, &i) )
  { Choice ch = ((Choice)((Word)lBase + i));

    if ( !(ch >= (Choice)lBase && ch < (Choice)lTop) ||
	 !valid_choice(ch) )
      return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_choice, r);
    *chp = ch;

    succeed;
  } else
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_choice, r);
}


#ifdef O_DEBUGGER

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
isDebugFrame(LocalFrame FR) is true if this call  must be visible in the
tracer. `No-debug' code has HIDE_CHILDS. Calls to  it must be visible if
the parent is a debug frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
isDebugFrame(LocalFrame FR)
{ if ( false(FR->predicate, TRACE_ME) )
    return FALSE;			/* hidden predicate */

  if ( false(FR->predicate, HIDE_CHILDS) )
    return TRUE;			/* user pred */

  if ( FR->parent )
  { LocalFrame parent = FR->parent;

    if ( levelFrame(FR) == levelFrame(parent)+1 )
    {					/* not last-call optimized */
      if ( false(parent->predicate, HIDE_CHILDS) )
	return TRUE;			/* user calls system */
      return FALSE;			/* system calls system */
    } else
    { if ( false(parent, FR_HIDE_CHILDS) )
	return TRUE;
      return FALSE;
    }
  } else
  { QueryFrame qf = queryOfFrame(FR);

    return (qf->flags & PL_Q_NODEBUG) ? FALSE : TRUE;
  }
}


static int
exitFromDebugger(const char *msg, int status)
{ GET_LD

#ifdef O_PLMT
  if ( PL_thread_self() > 1 && !LD->exit_requested )
  { Sfprintf(Sdout, "%sexit session\n", msg);
    LD->exit_requested = EXIT_REQ_THREAD;
    return ACTION_ABORT;
  }
#endif
  Sfprintf(Sdout, "%sexit (status 4)\n", msg);
  PL_halt(status);
  return -1;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines the tracer and interrupt  handler  that  allows  the
user  to break the normal Prolog execution.  The tracer is written in C,
but before taking action it calls Prolog.   This  mechanism  allows  the
user to intercept and redefine the tracer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

					/* Frame <-> Prolog integer */
static void		helpTrace(void);
#ifdef O_INTERRUPT
static void		helpInterrupt(void);
#endif
static bool		hasAlternativesFrame(LocalFrame);
static void		alternatives(Choice);
static int		exceptionDetails(void);
static int		listGoal(LocalFrame frame);
static LocalFrame	frameAtLevel(LocalFrame frame, int at_depth,
				     bool interactive);
static int		saveGoal(LocalFrame frame, int at_depth,
				 bool interactive);
static int		traceInterception(LocalFrame, Choice, int, Code);
static int		traceAction(char *cmd,
				    int port,
				    LocalFrame frame,
				    Choice bfr,
				    bool interactive);
static void		interruptHandler(int sig);
static int		writeFrameGoal(IOSTREAM *out, LocalFrame frame, Code PC,
				       unsigned int flags);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
redoFrame() returns the latest skipped frame or NULL if  no  such  frame
exists.   This  is used to give the redo port of the goal skipped rather
than the redo port of some subgoal of this port.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static LocalFrame
redoFrame(LocalFrame fr, Code *PC)
{ while( fr && false(fr, FR_SKIPPED))
  { *PC = fr->programPointer;
    fr = parentFrame(fr);
  }

  return fr;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
canUnifyTermWithGoal() is used to check whether the given frame satisfies
the /search specification.  This function cannot use the `neat' interface
as the record is not in the proper format.

This function fails if its execution would require a stack-shift of GC!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
canUnifyTermWithGoal(LocalFrame fr)
{ GET_LD
  find_data *find = LD->trace.find;

  switch(find->type)
  { case TRACE_FIND_ANY:
      succeed;
    case TRACE_FIND_NAME:
      return find->goal.name == fr->predicate->functor->name;
    case TRACE_FIND_TERM:
    { if ( find->goal.term.functor == fr->predicate->functor->functor )
      { fid_t cid;

	if ( (cid=PL_open_foreign_frame()) )
	{ term_t t = PL_new_term_ref();
	  term_t frref = consTermRef(fr);
	  int i, arity = fr->predicate->functor->arity;
	  int rval = TRUE;

	  if ( copyRecordToGlobal(t, find->goal.term.term,
				  ALLOW_GC|ALLOW_SHIFT) < 0 )
	    fail;
	  for(i=0; i<arity; i++)
	  { Word a, b;

	    a = valTermRef(t);
	    deRef(a);
	    a = argFrameP(*a, i);
	    fr = (LocalFrame)valTermRef(frref);
	    b = argFrameP(fr, i);

	    if ( !can_unify(a++, b++, 0) )
	    { rval = FALSE;
	      break;
	    }
	  }

	  PL_discard_foreign_frame(cid);
	  return rval;
	}
      }

      fail;
    }
    default:
      assert(0);
      fail;
  }
}


static const char *
portPrompt(int port)
{ switch(port)
  { case CALL_PORT:	 return " Call:  ";
    case REDO_PORT:	 return " Redo:  ";
    case FAIL_PORT:	 return " Fail:  ";
    case EXIT_PORT:	 return " Exit:  ";
    case UNIFY_PORT:	 return " Unify: ";
    case EXCEPTION_PORT: return " Exception: ";
    case CUT_CALL_PORT:	 return " Cut call: ";
    case CUT_EXIT_PORT:	 return " Cut exit: ";
    default:		 return "";
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Toplevel  of  the  tracer.   This  function  is  called  from  the   WAM
interpreter.   It  can  take  care of most of the tracer actions itself,
except if the execution path is to  be  changed.   For  this  reason  it
returns to the WAM interpreter how to continue the execution:

    ACTION_CONTINUE:	Continue normal
    ACTION_FAIL:	Go to the fail port of this goal
    ACTION_RETRY:	Redo the current goal
    ACTION_IGNORE:	Go to the exit port of this goal
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define SAVE_PTRS() \
	frameref = consTermRef(frame); \
	chref    = consTermRef(bfr); \
	frref    = (fr ? consTermRef(fr) : 0); \
	pcref    = (onStack(local, PC) ? consTermRef(PC) : 0);
#define RESTORE_PTRS() \
	frame = (LocalFrame)valTermRef(frameref); \
	bfr   = (Choice)valTermRef(chref); \
	fr    = (frref ? (LocalFrame)valTermRef(frref) : NULL); \
	PC    = (pcref ? (Code)valTermRef(pcref) : PC);

int
tracePort(DECL_LD LocalFrame frame, Choice bfr, int port, Code PC)
{ int action = ACTION_CONTINUE;
  wakeup_state wstate;
  term_t frameref, chref, frref, pcref;
  Definition def = frame->predicate;
  LocalFrame fr = NULL;

  if ( (!isDebugFrame(frame) && !SYSTEM_MODE) || /* hidden */
       debugstatus.suspendTrace )	        /* called back */
    return ACTION_CONTINUE;

  if ( port == EXCEPTION_PORT )		/* do not trace abort */
  { Word p = valTermRef(LD->exception.pending);

    deRef(p);
    if ( *p == ATOM_aborted )
      return ACTION_CONTINUE;
  }

  if ( !debugstatus.tracing &&
       (false(def, SPY_ME) || (port & (CUT_PORT|REDO_PORT))) )
    return ACTION_CONTINUE;		/* not tracing and no spy-point */
  if ( debugstatus.skiplevel < levelFrame(frame) )
    return ACTION_CONTINUE;		/* skipped */
  if ( debugstatus.skiplevel == levelFrame(frame) &&
       (port & (REDO_PORT|CUT_PORT|UNIFY_PORT)) )
    return ACTION_CONTINUE;		/* redo, unify or ! in skipped pred */
  if ( false(def, TRACE_ME) )
    return ACTION_CONTINUE;		/* non-traced predicate */
  if ( (!(debugstatus.visible & port)) )
    return ACTION_CONTINUE;		/* wrong port */
  if ( (true(def, HIDE_CHILDS) && !SYSTEM_MODE) &&
       (port & CUT_PORT) )
    return ACTION_CONTINUE;		/* redo or ! in system predicates */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Give a trace on the skipped goal for a redo.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  { Code pc2 = NULL;

    if ( port == REDO_PORT && debugstatus.skiplevel == SKIP_VERY_DEEP &&
	 (fr = redoFrame(frame, &pc2)) != NULL )
    { int rc;

      debugstatus.skiplevel = SKIP_REDO_IN_SKIP;
      SAVE_PTRS();
      rc = tracePort(fr, bfr, REDO_PORT, pc2);
      RESTORE_PTRS();
      debugstatus.skiplevel = levelFrame(fr);
      set(fr, FR_SKIPPED);		/* cleared by "creep" */

      return rc;
    }
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We are in searching mode; should we actually give this port?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( LD->trace.find &&  LD->trace.find->searching )
  { DEBUG(2, Sdprintf("Searching\n"));

    if ( (port & LD->trace.find->port) )
    { int rc;

      SAVE_PTRS();
      rc = canUnifyTermWithGoal(frame);
      RESTORE_PTRS()
      if ( rc )
	LD->trace.find->searching = FALSE; /* Got you */
      return ACTION_CONTINUE;		/* Continue the search */
    } else
    { return ACTION_CONTINUE;		/* Continue the search */
    }
  }

  if ( !saveWakeup(&wstate, FALSE) )
    return action;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Do the Prolog trace interception.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  SAVE_PTRS();
  action = traceInterception(frame, bfr, port, PC);
  RESTORE_PTRS();
  if ( action >= 0 )
    goto out;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
All failed.  Things now are upto the normal Prolog tracer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  action = ACTION_CONTINUE;

again:
  SAVE_PTRS();
  writeFrameGoal(Suser_error, frame, PC, port|WFG_TRACING);
  RESTORE_PTRS();

  if (debugstatus.leashing & port)
  { char buf[LINESIZ];

    debugstatus.skiplevel = SKIP_VERY_DEEP;
    debugstatus.tracing   = TRUE;

    Sfprintf(Sdout, " ? ");
    Sflush(Sdout);
    if ( !truePrologFlag(PLFLAG_TTY_CONTROL) )
    { buf[0] = EOS;
      if ( !readLine(Sdin, Sdout, buf) )
      { action = exitFromDebugger("EOF: ", 4);
	goto out;
      }
    } else
    { int c = getSingleChar(Sdin, FALSE);

      if ( c == EOF )
      { action = exitFromDebugger("EOF: ", 4);
	goto out;
      }
      buf[0] = c;
      buf[1] = EOS;
      if ( isDigit(buf[0]) || buf[0] == '/' || buf[0] == '-' )
      { Sfprintf(Sdout, buf);
	readLine(Sdin, Sdout, buf);
      }
    }
    SAVE_PTRS();
    action = traceAction(buf, port, frame, bfr,
			 truePrologFlag(PLFLAG_TTY_CONTROL));
    RESTORE_PTRS();
    if ( action == ACTION_AGAIN )
      goto again;
  } else
    Sfprintf(Sdout, "\n");

out:
  restoreWakeup(&wstate);
  if ( action == ACTION_ABORT )
    abortProlog();

  return action;
}


static int
setupFind(char *buf)
{ GET_LD
  char *s;
  int port = 0;

  for(s = buf; *s && isBlank(*s); s++)	/* Skip blanks */
    ;
  if ( *s == EOS )			/* No specification: repeat */
  { if ( !LD->trace.find || !LD->trace.find->port )
    { Sfprintf(Sdout, "[No previous search]\n");
      fail;
    }
    LD->trace.find->searching = TRUE;
    succeed;
  }
  for( ; *s && !isBlank(*s); s++ )	/* Parse the port specification */
  { switch( *s )
    { case 'c':	port |= CALL_PORT;  continue;
      case 'e':	port |= EXIT_PORT;  continue;
      case 'r':	port |= REDO_PORT;  continue;
      case 'f':	port |= FAIL_PORT;  continue;
      case 'u':	port |= UNIFY_PORT; continue;
      case 'a':	port |= CALL_PORT|REDO_PORT|FAIL_PORT|EXIT_PORT|UNIFY_PORT;
				    continue;
      default:  Sfprintf(Sdout, "[Illegal port specification]\n");
		fail;
    }
  }
  for( ; *s && isBlank(*s); s++)	/* Skip blanks */
    ;

  if ( *s == EOS )			/* Nothing is a variable */
  { s = buf;
    buf[0] = '_',
    buf[1] = EOS;
  }

  { fid_t cid = PL_open_foreign_frame();
    term_t t = PL_new_term_ref();
    FindData find;

    if ( !(find = LD->trace.find) )
      find = LD->trace.find = allocHeapOrHalt(sizeof(find_data));

    if ( !PL_chars_to_term(s, t) )
    { PL_discard_foreign_frame(cid);
      fail;
    }

    if ( find->type == TRACE_FIND_TERM && find->goal.term.term )
      freeRecord(find->goal.term.term);

    if ( PL_is_variable(t) )
    { find->type = TRACE_FIND_ANY;
    } else if ( PL_get_atom(t, &find->goal.name) )
    { find->type = TRACE_FIND_NAME;
    } else if ( PL_get_functor(t, &find->goal.term.functor) )
    { if ( (find->goal.term.term = compileTermToHeap(t, 0)) )
      { find->type = TRACE_FIND_TERM;
      } else
      { Sfprintf(Sdout, "ERROR: no memory to safe find target\n");
	fail;
      }
    } else
    { Sfprintf(Sdout, "[Illegal goal specification]\n");
      fail;
    }

    find->port      = port;
    find->searching = TRUE;

    DEBUG(2,
	  Sdprintf("setup ok, port = 0x%x, goal = ", port);
	  PL_write_term(Serror, t, 1200, 0);
	  Sdprintf("\n") );

    PL_discard_foreign_frame(cid);
  }

  succeed;
}


static void
setPrintOptions(word t)
{ GET_LD
  fid_t fid;

  if ( (fid=PL_open_foreign_frame()) )
  { term_t av = PL_new_term_ref();
    predicate_t pred = PL_predicate("$set_debugger_write_options", 1,
				    "system");

    _PL_put_atomic(av, t);
    PL_call_predicate(NULL, PL_Q_NODEBUG, pred, av);

    PL_discard_foreign_frame(fid);
  }
}


static int
traceAction(char *cmd, int port, LocalFrame frame, Choice bfr,
	    bool interactive)
{ GET_LD
  int num_arg;				/* numeric argument */
  int def_arg = TRUE;			/* arg is default */
  char *s;

#define FeedBack(msg)	{ if (interactive) { if (cmd[1] != EOS) \
					       Sfprintf(Sdout, "\n"); \
					     else \
					       Sfprintf(Sdout, "%s", msg); } }
#define Warn(msg)	{ if (interactive) \
			    Sfprintf(Sdout, "%s", msg); \
			  else \
			    warning(msg); \
			}

  for(s=cmd; *s && isBlank(*s); s++)
    ;
  if ( isDigit(*s) || (*s == '-' && s[1] && isDigit(s[1])) )
  { num_arg = strtol(s, &s, 10);

    while(isBlank(*s))
      s++;
    def_arg = FALSE;
  } else
    num_arg = 0;

  switch( *s )
  { case 'a':	FeedBack("abort\n");
		return ACTION_ABORT;
    case 'b':	FeedBack("break\n");
		pl_break();
		return ACTION_AGAIN;
    case '/':	FeedBack("/");
		Sflush(Sdout);
		if ( setupFind(&s[1]) )
		{ clear(frame, FR_SKIPPED);
		  return ACTION_CONTINUE;
		}
		return ACTION_AGAIN;
    case '.':   if ( LD->trace.find &&
		     LD->trace.find->type != TRACE_FIND_NONE )
	        { FeedBack("repeat search\n");
		  LD->trace.find->searching = TRUE;
		  clear(frame, FR_SKIPPED);
		  return ACTION_CONTINUE;
		} else
		{ Warn("No previous search\n");
		}
		return ACTION_AGAIN;
    case EOS:
    case ' ':
    case '\n':
    case '\r':
    case 'c':	FeedBack("creep\n");
		if ( !(port & EXIT_PORT) )
		  clear(frame, FR_SKIPPED);
		return ACTION_CONTINUE;
    case '\04': FeedBack("EOF: ");
    case 'e':	return exitFromDebugger("", 4);
    case 'f':	FeedBack("fail\n");
		return ACTION_FAIL;
    case 'i':	if (port & (CALL_PORT|REDO_PORT|FAIL_PORT))
		{ FeedBack("ignore\n");
		  return ACTION_IGNORE;
		} else
		  Warn("Can't ignore goal at this port\n");
		return ACTION_CONTINUE;
    case 'r':	if ( !def_arg ||
		     (port & (REDO_PORT|FAIL_PORT|EXIT_PORT|EXCEPTION_PORT)) )
		{ LocalFrame fr;

		  if ( (fr = frameAtLevel(frame, def_arg ? 0 : num_arg,
					  interactive)) )
		  { if ( interactive )
		      Sfprintf(Sdout, "retry\nretry %s at level %d\n",
			       predicateName(fr->predicate), levelFrame(fr));
		    debugstatus.retryFrame = consTermRef(fr);
		    return ACTION_RETRY;
		  } else
		  { return ACTION_CONTINUE;
		  }
		} else
		  Warn("Can't retry at this port\n");
		return ACTION_CONTINUE;
    case 's':	if (port & (CALL_PORT|REDO_PORT))
		{ FeedBack("skip\n");
		  set(frame, FR_SKIPPED);
		  debugstatus.skiplevel = levelFrame(frame);
		} else
		{ FeedBack("creep\n");
		}
		return ACTION_CONTINUE;
    case 'u':	FeedBack("up\n");
		debugstatus.skiplevel = levelFrame(frame) - 1;
		return ACTION_CONTINUE;
    case 'd':   FeedBack("depth\n");
                setPrintOptions(def_arg ? 10 : consInt(num_arg));
		return ACTION_AGAIN;
    case 'w':   FeedBack("write\n");
                setPrintOptions(ATOM_write);
		return ACTION_AGAIN;
    case 'p':   FeedBack("print\n");
		setPrintOptions(ATOM_print);
		return ACTION_AGAIN;
    case 'l':	FeedBack("leap\n");
		tracemode(FALSE, NULL);
		return ACTION_CONTINUE;
    case 'n':	FeedBack("no debug\n");
		tracemode(FALSE, NULL);
		debugmode(DBG_OFF, NULL);
		return ACTION_CONTINUE;
    case 'g':	FeedBack("goals\n");
		PL_backtrace(def_arg ? 5 : num_arg, PL_BT_USER);
		return ACTION_AGAIN;
    case 'A':	FeedBack("alternatives\n");
		alternatives(bfr);
		return ACTION_AGAIN;
    case 'C':	debugstatus.showContext = 1 - debugstatus.showContext;
		if ( debugstatus.showContext == TRUE )
		{ FeedBack("Show context\n");
		} else
		{ FeedBack("No show context\n");
		}
		return ACTION_AGAIN;
    case 'm':	FeedBack("Exception details\n");
	        if ( port & EXCEPTION_PORT )
		{ exceptionDetails();
		} else
		   Warn("No exception\n");
		return ACTION_AGAIN;
    case 'L':	FeedBack("Listing\n");
		listGoal(frame);
		return ACTION_AGAIN;
    case 'S':	FeedBack("Save goal");
		saveGoal(frame, def_arg ? 0 : num_arg, interactive);
		return ACTION_AGAIN;
    case '+':	FeedBack("spy\n");
		set(frame->predicate, SPY_ME);
		return ACTION_AGAIN;
    case '-':	FeedBack("no spy\n");
		clear(frame->predicate, SPY_ME);
		return ACTION_AGAIN;
    case '?':
    case 'h':	helpTrace();
		return ACTION_AGAIN;
#ifdef O_DEBUG
    case 'D':   GD->debug_level = def_arg ? 0 : num_arg;
		FeedBack("Debug level\n");
		return ACTION_AGAIN;
#endif
    default:	Warn("Unknown option (h for help)\n");
		return ACTION_AGAIN;
  }
}

typedef struct trace_command
{ const char *keys;
  const char *comment;
} trace_command;

static void
set_max_len(int *i, const char *s)
{ size_t len = strlen(s);
  if ( len > (int)*i )
    *i = (int)len;
}

static void
helpTrace(void)
{ GET_LD
  const trace_command commands[] =
  {
    { "+",		   "set spy point" },
    { "-",		   "clear spy point" },
    { "/c|e|r|f|u|a goal", "find goal at port" },
    { ".",		   "repeat find" },
    { "a",		   "abort to toplevel" },
    { "A",		   "alternatives" },
    { "b",		   "break (new toplevel)" },
    { "c (ret, space)",	   "creep to next port" },
    { "[depth] d",	   "depth for printing" },
    { "e",		   "exit Prolog" },
    { "f",		   "make goal fail" },
    { "[depth] g",	   "backtrace (-N from top)" },
    { "h (?)",		   "help" },
    { "i",		   "ignore current goal" },
    { "l",		   "leap to spy point" },
    { "L",		   "list current goal" },
    { "[level] r",	   "retry goal [at level]" },
    { "s",		   "skip over" },
    { "[level] S",	   "save goal [at level]" },
    { "u",		   "up (complete goal)" },
    { "p",		   "print goals" },
    { "w",		   "(quoted) write goals" },
    { "m",		   "exception details" },
    { "C",		   "toggle show context" },
  #if O_DEBUG
    { "[level] D",         "system debug level" },
  #endif
    { NULL,		   NULL }
  };
  const trace_command *cmd;
  int lkw = 0, lcw = 0, rkw = 0, rcw = 0;

  for(cmd=commands; cmd->keys; cmd+=2)
  { set_max_len(&lkw, cmd[0].keys);
    set_max_len(&lcw, cmd[0].comment);

    if ( cmd[1].keys )
    { set_max_len(&rkw, cmd[1].keys);
      set_max_len(&rcw, cmd[1].comment);
    } else
      break;
  }

  Sfprintf(Sdout, "Trace commands:\n");
  for(cmd=commands; cmd->keys; cmd+=2)
  { if ( cmd[1].keys )
    { Sfprintf(Sdout, "%-*s %-*s | %-*s %s\n",
	       lkw, cmd[0].keys, lcw, cmd[0].comment,
	       rkw, cmd[1].keys,      cmd[1].comment);
    } else
    { Sfprintf(Sdout, "%-*s %s\n",
	       lkw, cmd[0].keys, cmd[0].comment);
      break;
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Write goal of stack frame. First  a   term  representing the goal of the
frame is constructed. Note that the  new   goal  term is the most recent
term on the global stack and shared  variables always live on the global
stack, so their identity is unaffected.   Variables  pushed by B_VOID do
change identity as they become a reference pointer into the goal term.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
put_frame_goal(term_t goal, LocalFrame frame)
{ GET_LD
  Definition def = frame->predicate;
  int argc = def->functor->arity;
  term_t fref = consTermRef((Word)frame);

  if ( !PL_unify_functor(goal, def->functor->functor) )
    return FALSE;
  if ( tTop+argc > tMax )
  { int rc;

    if ( (rc=ensureTrailSpace(argc)) != TRUE )
      return raiseStackOverflow(rc);
  }

  frame = (LocalFrame)valTermRef(fref);
  if ( argc > 0 )
  { Word argv = argFrameP(frame, 0);
    Word argp = valTermRef(goal);
    int i;

    deRef(argp);
    argp = argTermP(*argp, 0);

    for(i=0; i<argc; i++)
    { Word a;

      deRef2(argv+i, a);
      if ( needsRef(*a) )
      { if ( a > argp )
	{ setVar(argp[i]);
	  Trail(a, makeRefG(&argp[i]));
	} else
	{ Trail(&argp[i], makeRefG(a));
	}
      } else
      { argp[i] = *a;
      }
    }
  }

  if ( def->module != MODULE_user &&
       (false(def->module, M_SYSTEM) || SYSTEM_MODE))
  { term_t a;

    if ( !(a=PL_new_term_ref()) )
      return FALSE;

    PL_put_atom(a, def->module->name);
    return PL_cons_functor(goal, FUNCTOR_colon2, a, goal);
  }

  return TRUE;
}


typedef struct
{ unsigned int flags;			/* flag mask */
  atom_t name;				/* name */
} portname;

static const portname portnames[] =
{ { WFG_BACKTRACE,  ATOM_backtrace },
  { WFG_CHOICE,     ATOM_choice },
  { CALL_PORT,	    ATOM_call },
  { EXIT_PORT,	    ATOM_exit },
  { FAIL_PORT,	    ATOM_fail },
  { REDO_PORT,	    ATOM_redo },
  { UNIFY_PORT,	    ATOM_unify },
  { CUT_CALL_PORT,  ATOM_cut_call },
  { CUT_EXIT_PORT,  ATOM_cut_exit },
  { EXCEPTION_PORT, ATOM_exception },
  { 0,		    NULL_ATOM }
};


static int
writeFrameGoal(IOSTREAM *out, LocalFrame frame, Code PC, unsigned int flags)
{ GET_LD
  wakeup_state wstate;
  Definition def = frame->predicate;
  int rc = TRUE;

  if ( !saveWakeup(&wstate, TRUE) )
  { rc = FALSE;
    goto out;
  }

  if ( gc_status.active )
  { Sfprintf(out, " (%d): %s\n",
	     levelFrame(frame), predicateName(frame->predicate));
  } else if ( !GD->bootsession && GD->initialised && GD->debug_level == 0 )
  { term_t fr   = PL_new_term_ref();
    term_t port = PL_new_term_ref();
    term_t pc   = PL_new_term_ref();
    const portname *pn = portnames;

    if ( true(def, P_FOREIGN) )
      PL_put_atom(pc, ATOM_foreign);
    else if ( PC && frame->clause )
      rc = PL_put_intptr(pc, PC-frame->clause->value.clause->codes);
    else
      PL_put_nil(pc);

    if ( rc )
      PL_put_frame(fr, frame);

    if ( rc )
    { for(; pn->flags; pn++)
      { if ( flags & pn->flags )
	{ PL_put_atom(port, pn->name);
	  break;
	}
      }
    }

    if ( rc )
    { IOSTREAM *old = Suser_error;
      Suser_error = out;
      rc = printMessage(ATOM_debug,
			PL_FUNCTOR, FUNCTOR_frame3,
			  PL_TERM, fr,
			  PL_TERM, port,
			  PL_TERM, pc);
      Suser_error = old;
    }
  } else
  { debug_type debugSave = debugstatus.debugging;
    term_t fref    = consTermRef((Word)frame);
    term_t goal    = PL_new_term_ref();
    term_t options = PL_new_term_ref();
    term_t tmp     = PL_new_term_ref();
    char msg[3];
    const char *pp = portPrompt(flags&PORT_MASK);
    struct foreign_context ctx;

    frame = (LocalFrame)valTermRef(fref);
    put_frame_goal(goal, frame);
    debugstatus.debugging = DBG_OFF;
    PL_put_atom(tmp, ATOM_debugger_write_options);
    ctx.context = 0;
    ctx.control = FRG_FIRST_CALL;
    ctx.engine  = LD;
    if ( !pl_prolog_flag(tmp, options, &ctx) )
      PL_put_nil(options);
    PL_unify_stream_or_alias(tmp, out);

    msg[0] = true(def, P_TRANSPARENT) ? '^' : ' ';
    msg[1] = true(def, SPY_ME)	      ? '*' : ' ';
    msg[2] = EOS;

    frame = (LocalFrame)valTermRef(fref);
    Sfprintf(out, "%s%s(%d) ", msg, pp, levelFrame(frame));
    if ( debugstatus.showContext )
      Sfprintf(out, "[%s] ", stringAtom(contextModule(frame)->name));
#ifdef O_LIMIT_DEPTH
    if ( levelFrame(frame) > LD->depth_info.limit )
      Sfprintf(out, "[depth-limit exceeded] ");
#endif

    pl_write_term3(tmp, goal, options);
    if ( flags & (WFG_BACKTRACE|WFG_CHOICE) )
      Sfprintf(out, "\n");

    debugstatus.debugging = debugSave;
  }

out:
  restoreWakeup(&wstate);
  return rc;
}

/*  Write those frames on the stack that have alternatives left.

 ** Tue May 10 23:23:11 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static void
alternatives(Choice ch)
{ GET_LD

  for(; ch; ch = ch->parent)
  { if ( ch->type == CHP_DEBUG )
      continue;
    if ( (isDebugFrame(ch->frame) || SYSTEM_MODE) )
      writeFrameGoal(Suser_error, ch->frame, NULL, WFG_CHOICE);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
messageToString() is a  wrapper   around  $messages:message_to_string/2,
translating a message-term as used for exceptions into a C-string.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *
messageToString(term_t msg)
{ GET_LD
  fid_t fid;

  if ( (fid=PL_open_foreign_frame()) )
  { term_t av = PL_new_term_refs(2);
    predicate_t pred = PL_predicate("message_to_string", 2, "$messages");
    int rc;
    char *s;

    PL_put_term(av+0, msg);
    rc = (PL_call_predicate(MODULE_system, PL_Q_NODEBUG, pred, av) &&
	  PL_get_chars(av+1, &s, CVT_ALL|BUF_STACK));
    PL_discard_foreign_frame(fid);

    return rc ? s : (char*)NULL;
  }

  return NULL;
}


static int
exceptionDetails()
{ GET_LD
  term_t except = LD->exception.pending;
  fid_t cid;

  if ( (cid = PL_open_foreign_frame()) )
  { int rc;

    Sflush(Suser_output);		/* make sure to stay in sync */
    Sfprintf(Sdout, "\tException term: ");
    rc = PL_write_term(Sdout, except, 1200, PL_WRT_QUOTED);
    Sfprintf(Sdout, "\n\t       Message: %s\n", messageToString(except));

    PL_discard_foreign_frame(cid);
    return rc;
  }

  return FALSE;
}


static int
listGoal(LocalFrame frame)
{ GET_LD
  term_t fref = consTermRef((Word)frame);
  fid_t cid;

  if ( (cid=PL_open_foreign_frame()) )
  { term_t goal = PL_new_term_ref();
    predicate_t pred = PL_predicate("$prolog_list_goal", 1, "system");
    IOSTREAM *old = Scurout;
    int rc;

    Scurout = Sdout;
    frame = (LocalFrame)valTermRef(fref);
    rc = ( put_frame_goal(goal, frame) &&
	   PL_call_predicate(MODULE_system, PL_Q_NODEBUG, pred, goal)
	 );
    Scurout = old;

    PL_discard_foreign_frame(cid);
    return rc;
  }

  return FALSE;
}


static LocalFrame
frameAtLevel(LocalFrame frame, int at_depth, bool interactive)
{ GET_LD

  if ( at_depth )
  { pl_context_t ctx;

    if ( PL_get_context(&ctx, 0) )
    { do
      { if ( levelFrame(ctx.fr) == at_depth )
	  return ctx.fr;
      } while(PL_step_context(&ctx));

      if ( interactive )
	Sfprintf(Sdout, "No frame at level %d", at_depth);

      return NULL;
    }

    return NULL;
  }

  return frame;
}


static int
saveGoal(LocalFrame frame, int at_depth, bool interactive)
{ GET_LD
  term_t goal;

  if ( !(frame = frameAtLevel(frame, at_depth, interactive)) )
    return FALSE;

  if ( (goal = PL_new_term_ref()) &&
       put_frame_goal(goal, frame) &&
       PL_record_az(ATOM_saved_goals, goal, 0, RECORDA) )
  { if ( interactive )
      Sfprintf(Sdout, "\nRecorded goal to key `saved_goals`\n");
    return TRUE;
  }

  Sfprintf(Sdout, "\nFailed to save goal\n");
  return FALSE;
}



static void
writeContextFrame(IOSTREAM *out, pl_context_t *ctx, int flags)
{ if ( (flags&PL_BT_SAFE) )
  { char buf[256];

    PL_describe_context(ctx, buf, sizeof(buf));
    Sfprintf(out, "  %s\n", buf);
  } else
  { writeFrameGoal(out, ctx->fr, ctx->pc, WFG_BACKTRACE);
  }
}


#define SHOW_FRAME(fr) ( isDebugFrame(fr) || !(flags&PL_BT_USER) )

static void
_PL_backtrace(IOSTREAM *out, int depth, int flags)
{ pl_context_t ctx;

  if ( PL_get_context(&ctx, 0) )
  { GET_LD
    Definition def = NULL;
    int same_proc = 0;
    pl_context_t rctx;			/* recursive context */

    if ( gc_status.active )
    { flags |= PL_BT_SAFE;
      flags &= ~PL_BT_USER;
    }
    if ( SYSTEM_MODE )
      flags &= ~PL_BT_USER;

    if ( depth < 0 )			/* deph < 0: top depth frames */
    { pl_context_t from = ctx;
      int skip;

      skip = depth = -depth;
      while( PL_step_context(&ctx) )
      { if ( SHOW_FRAME(ctx.fr) && --skip <= 0 )
	  break;
      }
      while( PL_step_context(&ctx) )
      { if ( SHOW_FRAME(ctx.fr) )
	{ do
	  { PL_step_context(&from);
	  } while( !SHOW_FRAME(from.fr) );
	}
      }

      ctx = from;
    }

    startCritical();
    for(; depth > 0 && ctx.fr; PL_step_context(&ctx))
    { LocalFrame frame = ctx.fr;

      if ( frame->predicate == def )
      { if ( ++same_proc >= 10 )
	{ if ( same_proc == 10 )
	    Sfprintf(out, "    ...\n    ...\n", Sdout);
	  rctx = ctx;
	  continue;
	}
      } else
      { if ( same_proc >= 10 )
	{ if ( isDebugFrame(rctx.fr) || !(flags&PL_BT_USER) )
	  { writeContextFrame(out, &rctx, flags);
	    depth--;
	  }
	  same_proc = 0;
	}
	def = frame->predicate;
      }

      if ( SHOW_FRAME(frame) )
      { writeContextFrame(out, &ctx, flags);
	depth--;
      }
    }
    endCritical();
  } else
  { Sfprintf(out, "No stack??\n");
  }
}


void
PL_backtrace(int depth, int flags)
{ GET_LD

  _PL_backtrace(Suser_error, depth, flags);
}


char *
PL_backtrace_string(int depth, int flags)
{ char *data = NULL;
  size_t len = 0;
  IOSTREAM *out;

  if ( (out=Sopenmem(&data, &len, "w")) )
  { out->encoding = ENC_UTF8;
    out->newline  = SIO_NL_POSIX;

    _PL_backtrace(out, depth, flags);
    Sclose(out);

    return data;
  }

  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Trace interception mechanism.  Whenever the tracer wants to perform some
action   it   will   first   call   the    users'    Prolog    predicate
prolog_trace_interception/4, allowing the user to define his/her action.
If  this procedure succeeds the tracer assumes the trace action has been
done and returns, otherwise the  default  C-defined  trace  actions  are
performed.

This predicate is supposed to return one of the following atoms:

	continue			simply continue (creep)
	fail				fail this goal
	retry				retry this goal
	ignore				pretend this call succeeded
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
traceInterception(LocalFrame frame, Choice bfr, int port, Code PC)
{ GET_LD
  int rval = -1;			/* Default C-action */
  predicate_t proc;
  term_t ex;

  proc = _PL_predicate("prolog_trace_interception", 4, "user",
		       &GD->procedures.prolog_trace_interception4);
  if ( !getProcDefinition(proc)->impl.any.defined )
    return rval;

  if ( !GD->bootsession && GD->debug_level == 0 )
  { fid_t cid=0;
    qid_t qid=0;
    LocalFrame fr = NULL;
    term_t frameref, chref, frref, pcref;
    term_t argv, rarg;
    atom_t portname = NULL_ATOM;
    functor_t portfunc = 0;
    int nodebug = FALSE;

    SAVE_PTRS();
    if ( !(cid=PL_open_foreign_frame()) )
      goto out;
    argv = PL_new_term_refs(4);
    rarg = argv+3;

    switch(port)
    { case CALL_PORT:	   portname = ATOM_call;         break;
      case REDO_PORT:	   portfunc = FUNCTOR_redo1;     break;
      case EXIT_PORT:	   portname = ATOM_exit;         break;
      case FAIL_PORT:	   portname = ATOM_fail;         break;
      case UNIFY_PORT:	   portname = ATOM_unify;	 break;
      case EXCEPTION_PORT:
	if ( !PL_unify_term(argv,
			    PL_FUNCTOR, FUNCTOR_exception1,
			      PL_TERM, LD->exception.pending) )
	  goto out;
	break;
      case CUT_CALL_PORT:  portfunc = FUNCTOR_cut_call1; break;
      case CUT_EXIT_PORT:  portfunc = FUNCTOR_cut_exit1; break;
      default:
	assert(0);
        goto out;
    }
    RESTORE_PTRS();

    if ( portname )
    { PL_put_atom(argv, portname);
    } else if ( portfunc )
    { int pcn;

      if ( PC && false(frame->predicate, P_FOREIGN) && frame->clause )
	pcn = (int)(PC - frame->clause->value.clause->codes);
      else
	pcn = 0;

      if ( !PL_unify_term(argv,
			  PL_FUNCTOR, portfunc,
			    PL_INT, pcn) )
	goto out;
    }

    RESTORE_PTRS();
    PL_put_frame(argv+1, frame);
    PL_put_choice(argv+2, bfr);
    if ( !(qid = PL_open_query(MODULE_user, PL_Q_NODEBUG|PL_Q_CATCH_EXCEPTION, proc, argv)) )
      goto out;
    if ( PL_next_solution(qid) )
    { atom_t a;

      RESTORE_PTRS();

      if ( PL_get_atom(rarg, &a) )
      { if ( a == ATOM_continue )
	{ if ( !(port & EXIT_PORT) )
	    clear(frame, FR_SKIPPED);
	  rval = ACTION_CONTINUE;
	} else if ( a == ATOM_nodebug )
	{ rval = ACTION_CONTINUE;
	  nodebug = TRUE;
	} else if ( a == ATOM_fail )
	{ rval = ACTION_FAIL;
	} else if ( a == ATOM_skip )
	{ if ( (port & (CALL_PORT|REDO_PORT)) )
	  { debugstatus.skiplevel = levelFrame(frame);
	    set(frame, FR_SKIPPED);
	  }
	  rval = ACTION_CONTINUE;
	} else if ( a == ATOM_up )
	{ debugstatus.skiplevel = levelFrame(frame) - 1;
	  rval = ACTION_CONTINUE;
	} else if ( a == ATOM_retry )
	{ debugstatus.retryFrame = consTermRef(frame);
	  rval = ACTION_RETRY;
	} else if ( a == ATOM_ignore )
	{ rval = ACTION_IGNORE;
	} else if ( a == ATOM_abort )
	{ rval = ACTION_ABORT;
	} else
	  PL_warning("Unknown trace action: %s", stringAtom(a));
      } else if ( PL_is_functor(rarg, FUNCTOR_retry1) )
      { LocalFrame fr;
	term_t arg = PL_new_term_ref();

	if ( PL_get_arg(1, rarg, arg) && PL_get_frame(arg, &fr) )
	{ debugstatus.retryFrame = consTermRef(fr);
	  rval = ACTION_RETRY;
	} else
	  PL_warning("prolog_trace_interception/4: bad argument to retry/1");
      }
    } else if ( (ex=PL_exception(qid)) )
    { if ( classify_exception(ex) == EXCEPT_ABORT )
      { rval = ACTION_ABORT;
      } else
      { if ( printMessage(ATOM_error, PL_TERM, ex) )
	{ nodebug = TRUE;
	  rval = ACTION_CONTINUE;
	} else if ( classify_exception(exception_term) >= EXCEPT_TIMEOUT )
	{ PL_clear_exception();
	  rval = ACTION_ABORT;
	} else
	{ PL_clear_exception();
	  nodebug = TRUE;
	  rval = ACTION_CONTINUE;
	}
      }
    }

  out:
    if ( qid ) PL_close_query(qid);
    if ( cid ) PL_discard_foreign_frame(cid);

    if ( nodebug )
    { tracemode(FALSE, NULL);
      debugmode(DBG_OFF, NULL);
    }
  }

  return rval;
}


		 /*******************************
		 *	 SAFE STACK TRACE	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PL_get_context(pl_context_t *ctx, int tid)
PL_step_context(pl_context_t *ctx)
PL_describe_context(pl_context_t *ctx, char *buf, size_t len)

These functions provide a public API  to   obtain  a trace of the Prolog
stack in a fairly safe manner.

    static void
    dump_stack(void)
    { pl_context_t ctx;

      if ( PL_get_context(&ctx, 0) )
      { int max = 5;

	Sdprintf("Prolog stack:\n");

	do
	{ char buf[256];

	  PL_describe_context(&ctx, buf, sizeof(buf));
	  Sdprintf("  %s\n", buf);
	} while ( max-- > 0 && PL_step_context(&ctx) );
      } else
	Sdprintf("No stack??\n");
    }

The second argument of PL_get_context() is a Prolog thread-id. Passing 0
gets the context of the calling   thread. The current implementation can
only deal with extracting the stack for  the calling thread, but the API
is prepared to generalise this.

See also PL_backtrace() and os/pl-cstack.c.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_get_context(pl_context_t *c, int thread_id)
{ GET_LD
  (void)thread_id;

  if ( !HAS_LD )
    return FALSE;

  c->ld = LD;
  c->qf = LD->query;
  if ( c->qf && c->qf->registers.fr )
    c->fr = c->qf->registers.fr;
  else
    c->fr = environment_frame;
  if ( c->qf && c->qf->registers.pc )
    c->pc = c->qf->registers.pc;
  else
    c->pc = NULL;

  return TRUE;
}


int
PL_step_context(pl_context_t *c)
{ if ( c->fr )
  { GET_LD

    if ( !onStack(local, c->fr) )
      return FALSE;

    if ( c->fr->parent )
    { c->pc = c->fr->programPointer;
      c->fr = c->fr->parent;
    } else
    { c->pc = NULL;
      c->qf = queryOfFrame(c->fr);
      c->fr = parentFrame(c->fr);
    }
  }

  return c->fr ? TRUE : FALSE;
}


int
PL_describe_context(pl_context_t *c, char *buf, size_t len)
{ LocalFrame fr;

  buf[0] = 0;

  if ( (fr=c->fr) )
  { GET_LD
    long level;
    int printed;

    if ( !onStack(local, fr) )
      return snprintf(buf, len, "<invalid frame reference %p>", fr);

    level = levelFrame(fr);
    if ( !fr->predicate )
      return snprintf(buf, len, "[%ld] <no predicate>", level);

    printed = snprintf(buf, len, "[%ld] %s ", level, predicateName(fr->predicate));
    len -= printed;
    buf += printed;

    if ( c->pc >= fr->predicate->codes &&
	 c->pc < &fr->predicate->codes[fr->predicate->codes[-1]] )
    { return printed+snprintf(buf, len, "[PC=%ld in supervisor]",
			      (long)(c->pc - fr->predicate->codes));
    }

    if ( false(fr->predicate, P_FOREIGN) )
    { int clause_no = 0;
      intptr_t pc = -1;

      if ( fr->clause )
      { Clause cl = fr->clause->value.clause;

	if ( c->pc >= cl->codes && c->pc < &cl->codes[cl->code_size] )
	  pc = c->pc - cl->codes;

	if ( fr->predicate == PROCEDURE_dc_call_prolog->definition )
	  return printed+snprintf(buf, len, "[PC=%ld in top query clause]",
				  (long)pc);

	clause_no = clauseNo(cl, 0);
	return printed+snprintf(buf, len, "[PC=%ld in clause %d]",
				(long)pc,
				clause_no);
      }
      return printed+snprintf(buf, len, "<no clause>");
    } else
    { return printed+snprintf(buf, len, "<foreign>");
    }
  }

  return 0;
}


#endif /*O_DEBUGGER*/

#ifndef offset
#define offset(s, f) ((size_t)(&((struct s *)NULL)->f))
#endif

static QueryFrame
findQuery(LocalFrame fr)
{ while(fr && fr->parent)
    fr = fr->parent;

  if ( fr )
    return queryOfFrame(fr);
  return NULL;
}


static bool
hasAlternativesFrame(LocalFrame frame)
{ GET_LD
  QueryFrame qf;
  LocalFrame fr = environment_frame;
  Choice ch = LD->choicepoints;

  for(;;)
  { for( ; ch; ch = ch->parent )
    { if ( (void *)ch < (void *)frame )
	return FALSE;

      if ( ch->frame == frame )
      { switch( ch->type )
	{ case CHP_CLAUSE:
	  case CHP_JUMP:
	    return TRUE;
	  case CHP_TOP:			/* no default to get warning */
	  case CHP_CATCH:
	  case CHP_DEBUG:
	    continue;
	}
      }
    }
    if ( (qf = findQuery(fr)) )
    { fr = qf->saved_environment;
      ch = qf->saved_bfr;
    } else
      return FALSE;
  }
}


#ifdef O_DEBUG
static intptr_t
loffset(void *p)
{ GET_LD
  if ( p == NULL )
    return 0;

  assert((intptr_t)p % sizeof(word) == 0);
  return (Word)p-(Word)lBase;
}

#endif

static LocalFrame
alternativeFrame(LocalFrame frame)
{ GET_LD
  QueryFrame qf;
  LocalFrame fr = environment_frame;
  Choice ch = LD->choicepoints;

  DEBUG(3, Sdprintf("Looking for choice of #%d\n", loffset(frame)));

  for(;;)
  { for( ; ch; ch = ch->parent )
    { if ( (void *)ch < (void *)frame )
	return NULL;

      if ( ch->frame == frame )
      { DEBUG(3, Sdprintf("First: %s\n", chp_chars(ch)));

	for(ch = ch->parent; ch; ch = ch->parent )
	{ if ( ch->frame == frame )
	  { DEBUG(3, Sdprintf("\tSkipped: %s\n", chp_chars(ch)));
	    continue;
	  }

	  switch( ch->type )
	  { case CHP_CLAUSE:
	    case CHP_JUMP:
	      DEBUG(3, Sdprintf("\tReturning: %s\n", chp_chars(ch)));
	      return ch->frame;
	    default:
	      break;
	  }
	}

        return NULL;
      }
    }

    if ( (qf = findQuery(fr)) )
    { fr = qf->saved_environment;
      ch = qf->saved_bfr;
    } else
      return NULL;
  }
}


void
resetTracer(void)
{ GET_LD

  debugstatus.tracing      = FALSE;
  debugstatus.debugging    = DBG_OFF;
  debugstatus.suspendTrace = 0;
  debugstatus.skiplevel    = 0;
  debugstatus.retryFrame   = 0;

  setPrologFlagMask(PLFLAG_LASTCALL);
}


#ifdef O_INTERRUPT

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Handling  interrupts.   We  know  we  are  not  in  critical  code  (see
startCritical()  and endCritical(), so the heap is consistent.  The only
problem can be that we are currently writing the arguments of  the  next
goal  above  the  local  stack  top  pointer.  To avoid problems we just
increment the top pointer to point above the furthest argument.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
helpInterrupt(void)
{ GET_LD

  Sfprintf(Sdout,
	   "Options:\n"
	   "a:           abort         b:           break\n"
	   "c:           continue      e:           exit\n"
	   "g:           goals         s:           C-backtrace\n"
	   "t:           trace         p:		  Show PID\n"
	   "h (?):       help\n");
}

static void
interruptHandler(int sig)
{ GET_LD
  int c;
  int safe;
  int first = TRUE;

  if ( !GD->initialised )
  { Sfprintf(Serror, "Interrupt during startup. Cannot continue\n");
    PL_halt(1);
  }

#ifdef O_PLMT
  if ( !LD )				/* we can't handle this; main thread */
  { PL_thread_raise(1, sig);		/* should try to do this */
    return;
  }

  if ( LD->exit_requested )
  { term_t rval = PL_new_term_ref();
    PL_put_atom(rval, ATOM_true);
    pl_thread_exit(rval);
    assert(0);				/* should not return */
  }
#endif

#if __unix__				/* actually, asynchronous signal handling */
  if ( !LD->signal.is_sync )
  { if ( PL_pending(sig) )
    { PL_clearsig(sig);
      safe = FALSE;
    } else
    { DEBUG(1, Sdprintf("Reposting as synchronous\n"));
      PL_raise(sig);
      return;
    }
  } else
  { safe = TRUE;
  }
#else
  safe = !LD->critical;
#endif					/* no async signals; always safe */

  Sreset();
again:
  if ( safe )
  { if ( !printMessage(ATOM_debug, PL_FUNCTOR, FUNCTOR_interrupt1,
		                     PL_ATOM, ATOM_begin) )
      PL_clear_exception();
  } else
  { if ( first )
    { first = FALSE;
      Sfprintf(Sdout,
	       "\n"
	       "WARNING: By typing Control-C twice, you have forced an asynchronous\n"
	       "WARNING: interrupt.  Your only SAFE operations are: c(ontinue), p(id),\n"
	       "WARNING: s(stack) and e(xit).  Notably a(abort) often works, but\n"
	       "WARNING: leaves the system in an UNSTABLE state\n\n");
    }
    Sfprintf(Sdout, "Action (h for help) ? ");
  }
  ResetTty();                           /* clear pending input -- atoenne -- */
  c = getSingleChar(Sdin, FALSE);

  switch(c)
  { case 'a':	Sfprintf(Sdout, "abort\n");
    action_a:
		unblockSignal(sig);
		abortProlog();
		if ( !safe )
		  PL_rethrow();
		break;
    case 'b':	Sfprintf(Sdout, "break\n");
		if ( safe )
		{ unblockSignal(sig);	/* into pl_break() itself */
		  pl_break();
		} else
		{ Sfprintf(Sdout, "Cannot break from forced interrupt\n");
		}
		goto again;
    case 'c':	if ( safe )
		{ if ( !printMessage(ATOM_debug,
				     PL_FUNCTOR, FUNCTOR_interrupt1,
				       PL_ATOM, ATOM_end) )
		    PL_clear_exception();
		} else
		{ Sfprintf(Sdout, "continue\n");
		}
		break;
    case 04:
    case EOF:	Sfprintf(Sdout, "EOF: ");
    case 'e':	if ( exitFromDebugger("", 4) == ACTION_ABORT )
		  goto action_a;
		break;
#ifdef O_DEBUGGER
    case 'g':	Sfprintf(Sdout, "goals\n");
		PL_backtrace(5, PL_BT_USER);
		goto again;
#endif /*O_DEBUGGER*/
    case 's':	save_backtrace("INT");
		print_backtrace_named("INT");
		goto again;
    case 'p':	Sfprintf(Sdout, "PID: %d\n", getpid());
                goto again;
    case 'h':
    case '?':	helpInterrupt();
		goto again;
#ifdef O_DEBUGGER
    case 't':	if ( safe )
		{ Sfprintf(Sdout, "trace\n");
		  if ( !printMessage(ATOM_debug,
				     PL_FUNCTOR, FUNCTOR_interrupt1,
				       PL_ATOM, ATOM_trace) )
		    PL_clear_exception();
		  pl_trace();
		  break;
		} else
		{ Sfprintf(Sdout, "Cannot start tracer from forced interrupt\n");
		  goto again;
		}
#endif /*O_DEBUGGER*/
    default:	Sfprintf(Sdout, "Unknown option (h for help)\n");
		goto again;
  }
}

#endif /*O_INTERRUPT*/


void
PL_interrupt(int sig)
{
#ifdef O_INTERRUPT
   interruptHandler(sig);
#endif
}


void
initTracer(DECL_LD)
{ debugstatus.visible      =
  debugstatus.leashing     = CALL_PORT|FAIL_PORT|REDO_PORT|EXIT_PORT|
			     EXCEPTION_PORT;
  debugstatus.showContext  = FALSE;
  resetTracer();

  if ( truePrologFlag(PLFLAG_DEBUG_ON_INTERRUPT) )
    enable_debug_on_interrupt(TRUE);
}

int
enable_debug_on_interrupt(DECL_LD int enable)
{
#if O_SIGNALS && defined(SIGINT)
  if ( enable )
  { if ( truePrologFlag(PLFLAG_SIGNALS) )
    { PL_signal(SIGINT, PL_interrupt);
      setPrologFlagMask(PLFLAG_DEBUG_ON_INTERRUPT);
    } else
    { return FALSE;
    }
  } else
  { terminate_on_signal(SIGINT);
    clearPrologFlagMask(PLFLAG_DEBUG_ON_INTERRUPT);
  }
  return TRUE;
#else
  return FALSE;
#endif
}





		/********************************
		*       PROLOG PREDICATES       *
		*********************************/

#if O_DEBUGGER

void
suspendTrace(int suspend)
{ GET_LD

  if ( suspend )
    debugstatus.suspendTrace++;
  else
    debugstatus.suspendTrace--;
}


int
tracemode(int doit, int *old)
{ GET_LD

  if ( doit )
  { debugmode(DBG_ON, NULL);
    doit = TRUE;
  }

  if ( old )
    *old = debugstatus.tracing;

  if ( debugstatus.tracing != doit )
  { debugstatus.tracing = doit;
    return printMessage(ATOM_silent,
			PL_FUNCTOR_CHARS, "trace_mode", 1,
			  PL_ATOM, doit ? ATOM_on : ATOM_off);
  }
  if ( doit )				/* make sure trace works inside skip */
  { debugstatus.skiplevel = SKIP_VERY_DEEP;
    if ( LD->trace.find )
      LD->trace.find->searching = FALSE;
  }

  succeed;
}


static int
have_space_for_debugging(void)
{ GET_LD

  return ( usedStack(local) +
	   usedStack(global) +
	   usedStack(trail) +
	   100000*sizeof(void*) < LD->stacks.limit );
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Enable the tracer if we have a safe amount of available space. This is
used to start tracing uncaught overflow exceptions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
trace_if_space(void)
{ int trace;

  if ( have_space_for_debugging() )
  { trace = TRUE;
    tracemode(trace, NULL);

  } else
    trace = FALSE;

  return trace;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
enlargeMinFreeStacks() sets the minimum free space   of all stacks a bit
higher to accomodate debugging. This causes less  GC calls and thus less
cases where debugging is harmed due to <garbage_collected> atoms.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define enlargeMinFreeStacks(l, g, t) LDFUNC(enlargeMinFreeStacks, l, g, t)
static int
enlargeMinFreeStacks(DECL_LD size_t l, size_t g, size_t t)
{ if ( LD->stacks.local.min_free < l )
    LD->stacks.local.min_free = l;
  if ( LD->stacks.global.min_free < g )
    LD->stacks.global.min_free = g;
  if ( LD->stacks.trail.min_free < l )
    LD->stacks.trail.min_free = t;

  return shiftTightStacks();		/* no GC: we want to keep variables! */
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
debugmode(debug_type new, debug_type *old)

Set the current debug mode. If DBG_ALL,  debugging in switched on in all
queries. This behaviour is intended to allow   using  spy and debug from
PceEmacs that runs its Prolog work in non-debug mode.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
debugmode(debug_type doit, debug_type *old)
{ GET_LD

  if ( old )
    *old = debugstatus.debugging;

  if ( debugstatus.debugging != doit )
  { if ( doit )
    { if ( have_space_for_debugging() &&
	   !enlargeMinFreeStacks(8*1024*SIZEOF_VOIDP,
				 8*1024*SIZEOF_VOIDP,
				 8*1024*SIZEOF_VOIDP) )
	return FALSE;

      debugstatus.skiplevel = SKIP_VERY_DEEP;
      clearPrologFlagMask(PLFLAG_LASTCALL);
      if ( doit == DBG_ALL )
      { QueryFrame qf;

	for(qf = LD->query; qf; qf = qf->parent)
	  qf->debugSave = DBG_ON;

	doit = DBG_ON;
      }
    } else
    { setPrologFlagMask(PLFLAG_LASTCALL);
    }
    debugstatus.debugging = doit;
    updateAlerted(LD);
    return printMessage(ATOM_silent,
			PL_FUNCTOR_CHARS, "debug_mode", 1,
			  PL_ATOM, doit ? ATOM_on : ATOM_off);
  }

  return TRUE;
}

#else /*O_DEBUGGER*/

int
tracemode(int doit, int *old)
{ succeed;
}

int
debugmode(debug_type doit, debug_type *old)
{ succeed;
}

#endif

word
pl_trace()
{ return tracemode(TRUE, NULL);
}

word
pl_notrace()
{ return tracemode(FALSE, NULL);
}

word
pl_tracing()
{ GET_LD

  return debugstatus.tracing;
}

static
PRED_IMPL("prolog_skip_level", 2, prolog_skip_level, PL_FA_NOTRACE)
{ GET_LD
  term_t old = A1;
  term_t new = A2;
  atom_t a;
  size_t sl;

  if ( debugstatus.skiplevel == SKIP_VERY_DEEP )
  { TRY(PL_unify_atom(old, ATOM_very_deep));
  } else if ( debugstatus.skiplevel == SKIP_REDO_IN_SKIP )
  { TRY(PL_unify_atom(old, ATOM_redo_in_skip));
  } else
  { TRY(PL_unify_integer(old, debugstatus.skiplevel));
  }

  if ( PL_compare(A1, A2) == 0 )
    return TRUE;

  if ( PL_get_atom(new, &a) )
  { if ( a == ATOM_very_deep )
    { debugstatus.skiplevel = SKIP_VERY_DEEP;
      succeed;
    } else if ( a == ATOM_redo_in_skip )
    { debugstatus.skiplevel = SKIP_REDO_IN_SKIP;
      succeed;
    }
  }

  if ( PL_get_size_ex(new, &sl) )
  { debugstatus.skiplevel = sl;
    succeed;
  }

  fail;
}


static
PRED_IMPL("prolog_skip_frame", 1, prolog_skip_frame, PL_FA_NOTRACE)
{ PRED_LD
  LocalFrame fr;

  if ( !PL_get_frame(A1, &fr) || !fr )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_frame_reference, A1);

  debugstatus.skiplevel = levelFrame(fr);
  set(fr, FR_SKIPPED);

  return TRUE;
}


word
pl_spy(term_t p)
{ GET_LD
  Procedure proc;

  if ( get_procedure(p, &proc, 0, GP_FIND) )
  { Definition def = getProcDefinition(proc);

    if ( false(def, SPY_ME) )
    { LOCKDEF(def);
      set(def, SPY_ME);
      UNLOCKDEF(def);
      if ( !printMessage(ATOM_informational,
			 PL_FUNCTOR_CHARS, "spy", 1,
			   PL_TERM, p) )
	return FALSE;
    }
    return debugmode(DBG_ALL, NULL);
  }

  return FALSE;
}

word
pl_nospy(term_t p)
{ GET_LD
  Procedure proc;

  if ( get_procedure(p, &proc, 0, GP_FIND|GP_EXISTENCE_ERROR) )
  { Definition def = getProcDefinition(proc);

    if ( true(def, SPY_ME) )
    { LOCKDEF(def);
      clear(def, SPY_ME);
      UNLOCKDEF(def);
      return printMessage(ATOM_informational,
			  PL_FUNCTOR_CHARS, "nospy", 1,
			    PL_TERM, p);
    }
    return TRUE;
  }

  return FALSE;
}

word
pl_leash(term_t old, term_t new)
{ GET_LD
  return setInteger(&debugstatus.leashing, old, new);
}

word
pl_visible(term_t old, term_t new)
{ GET_LD
  return setInteger(&debugstatus.visible, old, new);
}


word
pl_debuglevel(term_t old, term_t new)
{ return setInteger(&GD->debug_level, old, new);
}


word
pl_prolog_current_frame(term_t frame)
{ GET_LD
  LocalFrame fr = environment_frame;

  if ( fr->predicate->impl.foreign.function == pl_prolog_current_frame )
    fr = parentFrame(fr);		/* thats me! */

  return PL_unify_frame(frame, fr);
}


/** prolog_current_choice(-Choice) is semidet.

True when Choice refers to the most recent choice-point.
*/

static
PRED_IMPL("prolog_current_choice", 1, prolog_current_choice, 0)
{ PRED_LD
  Choice ch = LD->choicepoints;

  while(ch && ch->type == CHP_DEBUG)
    ch = ch->parent;
  if ( ch )
    return PL_unify_choice(A1, ch);

  return FALSE;
}


static QueryFrame
clear_frame_vars(LocalFrame target)
{ GET_LD
  LocalFrame fr = environment_frame;
  Choice ch = LD->choicepoints;
  Code PC = NULL;

  for(;;)
  { while(fr && fr >= target)
    { if ( fr == target )
      { DEBUG(2, Sdprintf("Cleaned frame for %s from PC=%zd\n",
			  predicateName(fr->predicate),
			  fr->clause && PC ? PC-fr->clause->value.clause->codes
					   : -1));
	clearUninitialisedVarsFrame(fr, PC);
	return NULL;
      }

      PC = fr->programPointer;
      if ( fr->parent )
      { fr = fr->parent;
      } else
      { QueryFrame qf =  queryOfFrame(fr);

	if ( qf->parent )
	{ fr = qf->saved_environment;
	  PC = NULL;
	  ch = qf->saved_bfr;
	}
      }
    }

    if ( ch )
    { ch = ch->parent;
      if ( ch )
      { fr = ch->frame;
	if ( ch->type == CHP_JUMP )
	{ PC = ch->value.pc;
	  DEBUG(0,
		{ Code codes = fr->clause->value.clause->codes;
		  assert(PC < &codes[codes[-1]]);
		});
	} else
	  PC = NULL;

	continue;
      }
    }
    DEBUG(0, Sdprintf("FAILED to find frame (%p for %s) to clear!?\n",
		      target, predicateName(target->predicate)));
  }
}


static int
prolog_frame_attribute(term_t frame, term_t what, term_t value)
{ GET_LD
  LocalFrame fr;
  atom_t key;
  size_t arity;
  term_t result = PL_new_term_ref();
  Module m = NULL;

  if ( !PL_get_frame(frame, &fr) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_frame_reference, frame);
  if ( !fr )
    return FALSE;				/* frame == 'none' */
  if ( !PL_get_name_arity(what, &key, &arity) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, what);
  if ( !PL_strip_module(value, &m, value) )
    return FALSE;

  set(fr, FR_DEBUG);			/* explicit call to do this? */

  if ( key == ATOM_argument && arity == 1 )
  { term_t arg = PL_new_term_ref();
    size_t argn;

    if ( !PL_get_arg_ex(1, what, arg) || !PL_get_size_ex(arg, &argn) )
      fail;
    if ( argn < 1 )
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_natural, arg);

    if ( true(fr->predicate, P_FOREIGN) || !fr->clause )
    { if ( argn > fr->predicate->functor->arity )
	fail;
    } else
    { if ( argn > fr->clause->value.clause->prolog_vars )
	fail;
    }

#ifdef O_DEBUGLOCAL			/* see pl-wam.c */
    assert( *argFrameP(fr, argn-1) != (word)(((char*)ATOM_nil) + 1) );
    checkData(argFrameP(fr, argn-1));
#endif

   if ( !hasGlobalSpace(0) )
   { int rc;

     if ( (rc=ensureGlobalSpace(0, ALLOW_GC)) != TRUE )
       return raiseStackOverflow(rc);
     PL_get_frame(frame, &fr);
   }

   return PL_unify(value, consTermRef(argFrameP(fr, argn-1)));
  }

  if ( !(arity == 0 || (arity == 1 && key == ATOM_parent_goal)) )
  { unknown_key:
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_frame_attribute, what);
  }

  if (        key == ATOM_level)
  { PL_put_integer(result, levelFrame(fr));
  } else if (key == ATOM_has_alternatives)
  { PL_put_atom(result, hasAlternativesFrame(fr) ? ATOM_true : ATOM_false);
  } else if (key == ATOM_skipped)
  { PL_put_atom(result, true(fr, FR_SKIPPED) ? ATOM_true : ATOM_false);
  } else if (key == ATOM_alternative)
  { LocalFrame alt;

    if ( (alt = alternativeFrame(fr)) )
      PL_put_frame(result, alt);
    else
      fail;
  } else if (key == ATOM_parent)
  { LocalFrame parent;

    if ( (parent = parentFrame(fr)) )
    { clear_frame_vars(parent);
      PL_put_frame(result, parent);
    } else
      return FALSE;
  } else if (key == ATOM_top)
  { PL_put_atom(result, fr->parent ? ATOM_false : ATOM_true);
  } else if (key == ATOM_context_module)
  { PL_put_atom(result, contextModule(fr)->name);
  } else if (key == ATOM_clause)
  { if ( false(fr->predicate, P_FOREIGN) &&
	 fr->clause && fr->clause->value.clause &&
	 fr->predicate != PROCEDURE_dc_call_prolog->definition &&
	 fr->predicate != PROCEDURE_dcall1->definition )
    { if ( !PL_unify_clref(result, fr->clause->value.clause) )
	return FALSE;
    } else
    { return FALSE;
    }
  } else if (key == ATOM_goal)
  { if ( !put_frame_goal(result, fr) )
      return FALSE;
  } else if ( key == ATOM_predicate_indicator )
  { if ( !unify_definition(m, result, fr->predicate, 0, GP_NAMEARITY) )
      return FALSE;
  } else if ( key == ATOM_parent_goal )
  { Procedure proc;
    term_t head = PL_new_term_ref();
    term_t a = PL_new_term_ref();
    fid_t fid;

    if ( !get_procedure(value, &proc, head, GP_FIND) )
      fail;

    if ( (fid = PL_open_foreign_frame()) )
    { while( fr )
      { while(fr && fr->predicate != proc->definition)
	  fr = parentFrame(fr);

	if ( fr )
	{ int i, garity = fr->predicate->functor->arity;
	  term_t fref = consTermRef(fr);

	  for(i=0; i<garity; i++)
	  { term_t fa;

	    fa = consTermRef(argFrameP(fr, i));

	    _PL_get_arg(i+1, head, a);
	    if ( !PL_unify(a, fa) )
	      break;
	    fr = (LocalFrame)valTermRef(fref);	/* deal with possible shift */
	  }
	  if ( i == garity )
	  { if ( arity == 1 )
	    { LocalFrame parent;
	      term_t arg = PL_new_term_ref();

	      _PL_get_arg(1, what, arg);
	      if ( (parent = parentFrame(fr)) )
	      { if ( PL_unify_frame(arg, parent) )
		  return TRUE;
	      } else
	      { if ( PL_unify_atom(arg, ATOM_none) )
		  return TRUE;
	      }
	    } else
	    { return TRUE;
	    }
	  }

	  if ( PL_exception(0) )
	  { return FALSE;
	  } else
	  { PL_rewind_foreign_frame(fid);

	    fr = (LocalFrame)valTermRef(fref);	/* deal with possible shift */
	    fr = parentFrame(fr);
	  }
	} else
	{ PL_close_foreign_frame(fid);
	  return FALSE;
	}
      }
    }
  } else if ( key == ATOM_pc )
  { if ( fr->programPointer &&
	 fr->parent &&
	 false(fr->parent->predicate, P_FOREIGN) &&
	 fr->parent->clause &&
	 fr->parent->predicate != PROCEDURE_dcall1->definition )
    { intptr_t pc = fr->programPointer - fr->parent->clause->value.clause->codes;

      PL_put_intptr(result, pc);
    } else
    { fail;
    }
  } else if ( key == ATOM_hidden )
  { atom_t a;

    if ( SYSTEM_MODE )
    { a = ATOM_true;
    } else
    { if ( isDebugFrame(fr) )
	a = ATOM_false;
      else
	a = ATOM_true;
    }

    PL_put_atom(result, a);
  } else if ( key == ATOM_depth_limit_exceeded )
  { atom_t a;				/* get limit from saved query */

#ifdef O_LIMIT_DEPTH
    QueryFrame qf = findQuery(environment_frame);

    if ( qf && (uintptr_t)levelFrame(fr) > qf->saved_depth_limit )
      a = ATOM_true;
    else
#endif
      a = ATOM_false;

    PL_put_atom(result, a);
  } else
    goto unknown_key;

  return PL_unify(value, result);
}


/** prolog_frame_attribute(+Frame, +Key, -Value) is semidet.

*/

static
PRED_IMPL("prolog_frame_attribute", 3, prolog_frame_attribute, PL_FA_TRANSPARENT)
{ int rc = prolog_frame_attribute(A1, A2, A3);

  DEBUG(CHK_SECURE, scan_global(0));

  return rc;
}

		 /*******************************
		 *	 CHOICEPOINT STACK	*
		 *******************************/

/** prolog_choice_attribute(+Choice, +Key, -Value) is semidet.

*/

static size_t
in_clause_jump(Choice ch)
{ Clause cl;

  if ( ch->type == CHP_JUMP &&
       false(ch->frame->predicate, P_FOREIGN) &&
       ch->frame->clause &&
       (cl=ch->frame->clause->value.clause) &&
       ch->value.pc >= cl->codes &&
       ch->value.pc < &cl->codes[cl->code_size] )
    return ch->value.pc - cl->codes;

  return (size_t)-1;
}


static atom_t
choice_type_atom(Choice ch)
{ static const atom_t types[] =
  { ATOM_jump,
    ATOM_clause,
    ATOM_top,
    ATOM_catch,
    ATOM_debug
  };

  if ( ch->type == CHP_JUMP &&
       in_clause_jump(ch) == (size_t)-1 )
  { if ( ch->value.pc == SUPERVISOR(next_clause) )
      return ATOM_clause;
    if ( decode(ch->value.pc[0]) == I_FREDO )
      return ATOM_foreign;
    assert(0);
    return FALSE;
  } else
    return types[ch->type];
}


static
PRED_IMPL("prolog_choice_attribute", 3, prolog_choice_attribute, 0)
{ PRED_LD
  Choice ch = NULL;
  atom_t key;

  if ( !PL_get_choice(A1, &ch) ||
       !PL_get_atom_ex(A2, &key) )
    fail;

  if ( key == ATOM_parent )
  { do
    { ch = ch->parent;
    } while(ch && ch->type == CHP_DEBUG);

    if ( ch )
      return PL_unify_choice(A3, ch);
    fail;
  } else if ( key == ATOM_frame )
  { return PL_unify_frame(A3, ch->frame);
  } else if ( key == ATOM_type )
  { return PL_unify_atom(A3, choice_type_atom(ch));
  } else if ( key == ATOM_pc )
  { size_t offset = in_clause_jump(ch);

    if ( offset != (size_t)-1 )
      return PL_unify_int64(A3, offset);
    return FALSE;
  } else if ( key == ATOM_clause )
  { if ( ch->type == CHP_CLAUSE )
    { return PL_unify_clref(A3, ch->value.clause.cref->value.clause);
    } else if ( choice_type_atom(ch) == ATOM_clause )
    { Sdprintf("S_ALLCLAUSES not yet supported\n");
    }
    return FALSE;
  } else
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_key, A2);

}


		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(trace)
  PRED_DEF("prolog_current_choice", 1, prolog_current_choice, 0)
  PRED_DEF("prolog_frame_attribute", 3, prolog_frame_attribute, PL_FA_TRANSPARENT)
  PRED_DEF("prolog_choice_attribute", 3, prolog_choice_attribute, 0)
  PRED_DEF("prolog_skip_frame", 1, prolog_skip_frame, PL_FA_NOTRACE)
  PRED_DEF("prolog_skip_level", 2, prolog_skip_level, PL_FA_NOTRACE)
EndPredDefs
