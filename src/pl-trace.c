/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: tracer
*/

#include "pl-incl.h"
#include "pl-ctype.h"

int trace_continuation;			/* how to continue? */

#define W_PRINT		1		/* print/1 for displaying goal */
#define W_WRITE		2		/* write/1 */
#define W_WRITEQ	3		/* writeq/1 */
#define W_DISPLAY	4		/* display/1 */

static struct
{ int	 port;				/* Port to find */
  bool	 searching;			/* Currently searching? */
  Record goal;				/* Goal to find */
} find;

#define PrologRef(fr)	 ((Word)fr - (Word)lBase)
#define FrameRef(w)	 ((LocalFrame)((Word)lBase + w))

#ifdef O_DEBUGGER

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines the tracer and interrupt  handler  that  allows  the
user  to break the normal Prolog execution.  The tracer is written in C,
but before taking action it calls Prolog.   This  mechanism  allows  the
user to intercept and redefine the tracer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

					/* Frame <-> Prolog integer */
forwards LocalFrame	redoFrame(LocalFrame, Code *PC);
forwards int		traceAction(char *, int, LocalFrame, bool);
forwards void		helpTrace(void);
#ifdef O_INTERRUPT
forwards void		helpInterrupt(void);
#endif
forwards bool		hasAlternativesFrame(LocalFrame);
forwards void		alternatives(LocalFrame);
forwards void		listProcedure(Definition);
forwards int		traceInterception(LocalFrame, LocalFrame, int, Code);
forwards bool		canUnifyTermWithGoal(Word, LocalFrame);
forwards void		writeFrameGoal(LocalFrame frame, int how);

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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
canUnifyTermWithGoal(Word t, LocalFrame fr)
{ deRef(t);
  if ( isVar(*t) )
    succeed;
  if ( isAtom(*t) && fr->predicate->functor->name == *t )
    succeed;
  if ( hasFunctor(*t, fr->predicate->functor) )
  { Word a, b;
    int arity;

    a = argTermP(*t, 0);
    b = argFrameP(fr, 0);
    arity = functorTerm(*t)->arity;
    while( arity > 0 )
    { if ( !can_unify(a, b) )
        fail;
    }
    succeed;
  }
  
  fail;
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

int
tracePort(LocalFrame frame, LocalFrame bfr, int port, Code PC)
{ int OldOut;
  extern int Output;
  int action = ACTION_CONTINUE;
  Definition def = frame->predicate;
  LocalFrame fr;

  if ( (true(frame, FR_NODEBUG) && !(SYSTEM_MODE))	|| /* hidden */
       debugstatus.suspendTrace )			   /* called back */
    return ACTION_CONTINUE;

  if ( port == FAIL_PORT )
    Undo(frame->mark);

					/* trace/[1,2] */
  if ( true(def, TRACE_CALL|TRACE_REDO|TRACE_EXIT|TRACE_FAIL) &&
       !(port & (BREAK_PORT|CUT_PORT)) )
  { char *fmt = NULL;

    switch(port)
    { case CALL_PORT:
	if ( true(def, TRACE_CALL) )
	  fmt = "T Call:  (%3ld) ";
        break;
      case REDO_PORT:
	if ( true(def, TRACE_REDO) )
	  fmt = "T Redo:  (%3ld) ";
        break;
      case EXIT_PORT:
	if ( true(def, TRACE_EXIT) )
	  fmt = "T Exit:  (%3ld) ";
        break;
      case FAIL_PORT:
	if ( true(def, TRACE_FAIL) )
	  fmt = "T Fail:  (%3ld) ";
        break;
    }
    if ( fmt )
    { Putf(fmt, levelFrame(frame));
      writeFrameGoal(frame, debugstatus.style);
      Put('\n');
    }
  }

  if ( port != BREAK_PORT &&
       ((!debugstatus.tracing && false(def, SPY_ME))	|| /* non-tracing */
	debugstatus.skiplevel < levelFrame(frame)	|| /* skipped */
	((port & (BREAK_PORT|CUT_PORT)) &&
	 ((debugstatus.skiplevel == levelFrame(frame)) ||
	  true(def, HIDE_CHILDS)))			|| /* also skipped */
	false(def, TRACE_ME)				|| /* non-tracing */
	(!(debugstatus.visible & port))			|| /* wrong port */
	(port == REDO_PORT && (debugstatus.skiplevel == levelFrame(frame) ||
			       (true(def, SYSTEM) && !SYSTEM_MODE)
			      ))) )				   /* redos */
    return ACTION_CONTINUE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Give a trace on the skipped goal for a redo.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  { Code pc2;

    if ( port == REDO_PORT && debugstatus.skiplevel == VERY_DEEP &&
	 (fr = redoFrame(frame, &pc2)) != NULL )
    { debugstatus.skiplevel--;				   /* avoid a loop */
      switch( tracePort(fr, bfr, REDO_PORT, pc2) )
      { case ACTION_CONTINUE:
	  if ( debugstatus.skiplevel < levelFrame(frame) )
	    return ACTION_CONTINUE;
	  break;
	case ACTION_RETRY:
	case ACTION_IGNORE:
	case ACTION_FAIL:
	  Putf("Action not yet implemented here\n");
	  break;
      }
    }
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We are in searching mode; should we actually give this port?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( find.searching )
  { DEBUG(2, Sdprintf("Searching\n"));

    if ( (port & find.port) && canUnifyTermWithGoal(&find.goal->term, frame) )
    { find.searching = FALSE;		/* Got you */
    } else
    { return ACTION_CONTINUE;		/* Continue the search */
    }
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Do the Prolog trace interception.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  action = traceInterception(frame, bfr, port, PC);
  if ( action >= 0 )
    return action;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
All failed.  Things now are upto the normal Prolog tracer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  action = ACTION_CONTINUE;
  OldOut = Output;
  Output = 1;

again:
  Put( true(def, SPY_ME) ? '*' : ' ' );
  Put( true(def, TRANSPARENT) ? '^' : ' ');

  switch(port)
  { case CALL_PORT:	Putf(" Call:  ");	break;
    case REDO_PORT:	Putf(" Redo:  ");	break;
    case FAIL_PORT:	Putf(" Fail:  ");	break;
    case EXIT_PORT:	Putf(" Exit:  ");	break;
    case UNIFY_PORT:	Putf(" Unify: ");	break;
    case BREAK_PORT:	Putf(" Break: ");	break;
    case CUT_CALL_PORT:	Putf(" Cut call: ");	break;
    case CUT_EXIT_PORT:	Putf(" Cut exit: ");	break;
  }
  Putf("(%3ld) ", levelFrame(frame));
  writeFrameGoal(frame, debugstatus.style);

  debugstatus.skiplevel = VERY_DEEP;
  debugstatus.tracing = TRUE;

  if (debugstatus.leashing & port)
  { char buf[LINESIZ];

    Putf(" ? ");
    pl_flush();
    if ( status.notty )
    { buf[0] = EOS;
      readLine(buf);
    } else
    { buf[0] = getSingleChar();
      buf[1] = EOS;
      if ( isDigit(buf[0]) || buf[0] == '/' )
      { Putf(buf);
	readLine(buf);
      }
    }
    if ((action = traceAction(buf, port, frame, status.notty ? FALSE : TRUE))
							== ACTION_AGAIN)
      goto again;
  } else
    Put('\n');
  Output = OldOut;

  return action;
}


static int
setupFind(char *buf)
{ long rval;
  char *s;
  int port = 0;

  for(s = buf; *s && isBlank(*s); s++)	/* Skip blanks */
    ;
  if ( *s == EOS )			/* No specification: repeat */
  { if ( find.port == 0 )
    { Putf("[No previous search]\n");
      fail;
    }
    find.searching = TRUE;
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
      default:  Putf("[Illegal port specification]\n");
		fail;
    }
  }
  for( ; *s && isBlank(*s); s++)	/* Skip blanks */
    ;

  if ( *s == EOS )			/* Nothing is a variable */
  { s = buf;
    strcpy(buf, "_");
  }

  { fid_t cid = PL_open_foreign_frame();
    term_t t = PL_new_term_ref();

    seeString(s);
    rval = pl_read(t);
    seenString();

    if ( rval == FALSE )
    { PL_discard_foreign_frame(cid);
      fail;
    }

    if ( find.goal )
      freeRecord(find.goal);
    find.port      = port;
    find.goal      = copyTermToHeap(t);
    find.searching = TRUE;

    DEBUG(2, Sdprintf("setup ok, port = 0x%x, goal = ", port);
	  pl_write(t);
	  Sdprintf("\n") );

    PL_discard_foreign_frame(cid);
  }

  succeed;
}


static int
traceAction(char *cmd, int port, LocalFrame frame, bool interactive)
{ int num_arg;				/* numeric argument */
  char *s;

#define FeedBack(msg)	{ if (interactive) { if (cmd[1] != EOS) \
					       Putf("\n"); \
					     else \
					       Putf(msg); } }
#define Warn(msg)	{ if (interactive) Putf(msg); else warning(msg); }
#define Default		(-1)

  for(s=cmd; *s && isBlank(*s); s++)
    ;
  if ( isDigit(*s) )
  { num_arg = strtol(s, &s, 10);

    while(isBlank(*s))
      s++;
  } else
    num_arg = Default;

  switch( *s )
  { case 'a':	FeedBack("abort\n");
		pl_abort();
    case 'b':	FeedBack("break\n");
		pl_break();
		return ACTION_AGAIN;
    case '/': 	FeedBack("/");
    		pl_flush();
    		if ( setupFind(&s[1]) )
		{ clear(frame, FR_SKIPPED);
		  return ACTION_CONTINUE;
		}
		return ACTION_AGAIN;    		
    case '.':   if ( find.goal != NULL )
      	        { FeedBack("repeat search\n");
		  find.searching = TRUE;
		  clear(frame, FR_SKIPPED);
		  return ACTION_CONTINUE;
		} else
		{ Warn("No previous search\n");
		}
		return ACTION_AGAIN;    		
    case EOS:
    case ' ':
    case '\n':
    case 'c':	FeedBack("creep\n");
		clear(frame, FR_SKIPPED);
		return ACTION_CONTINUE;
    case '\04':
    case EOF:	FeedBack("EOF: ");
    case 'e':	FeedBack("exit\n");
		Halt(0);
    case 'f':	FeedBack("fail\n");
		return ACTION_FAIL;
    case 'i':	if (port & (CALL_PORT|REDO_PORT|FAIL_PORT))
		{ FeedBack("ignore\n");
		  return ACTION_IGNORE;
		} else
		  Warn("Can't ignore goal at this port\n");
		return ACTION_CONTINUE;
    case 'r':	if (port & (REDO_PORT|FAIL_PORT|EXIT_PORT))
		{ FeedBack("retry\n[retry]\n");
		  return ACTION_RETRY;
		} else
		  Warn("Can't retry at this port\n");
		return ACTION_CONTINUE;
    case 's':	FeedBack("skip\n");
		set(frame, FR_SKIPPED);
		debugstatus.skiplevel = levelFrame(frame);
		return ACTION_CONTINUE;
    case 'u':	FeedBack("up\n");
		debugstatus.skiplevel = levelFrame(frame) - 1;
		return ACTION_CONTINUE;
    case 'w':	FeedBack("write\n");
		debugstatus.style = W_WRITEQ;
		return ACTION_AGAIN;
    case 'p':	FeedBack("print\n");
		debugstatus.style = W_PRINT;
		return ACTION_AGAIN;
    case 'd':	FeedBack("display\n");
		debugstatus.style = W_DISPLAY;
		return ACTION_AGAIN;
    case 'l':	FeedBack("leap\n");
		debugstatus.tracing = FALSE;
		return ACTION_CONTINUE;
    case 'n':	FeedBack("no debug\n");
		debugstatus.debugging = FALSE;
		debugstatus.tracing = FALSE;
		return ACTION_CONTINUE;
    case 'g':	FeedBack("goals\n");
		backTrace(frame, num_arg == Default ? 5 : num_arg);
		return ACTION_AGAIN;
    case 'A':	FeedBack("alternatives\n");
		alternatives(frame);
		return ACTION_AGAIN;
    case 'C':	debugstatus.showContext = 1 - debugstatus.showContext;
		if ( debugstatus.showContext == TRUE )
		{ FeedBack("Show context\n");
		} else
		{ FeedBack("No show context\n");
		}
		return ACTION_AGAIN;
    case 'L':	FeedBack("Listing");
		listProcedure(frame->predicate);
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
    case 'D':   status.debugLevel = num_arg;
		FeedBack("Debug level\n");
		return ACTION_AGAIN;
    default:	Warn("Unknown option (h for help)\n");
		return ACTION_AGAIN;
  }
}

static void
helpTrace(void)
{ Putf("Options:\n");
  Putf("+:                  spy        -:                 no spy\n");
  Putf("/c|e|r|f|u|a} goal: find       .:                 repeat find\n");
  Putf("a:                  abort      A:                 alternatives\n");
  Putf("b:                  break      c (return, space): creep\n");
  Putf("d:                  display    e:                 exit\n");
  Putf("f:                  fail       [depth] g:         goals\n");
  Putf("h (?):              help       i:                 ignore\n");
  Putf("l:                  leap       L:                 listing\n");
  Putf("n:                  no debug   p:                 print\n");
  Putf("r:                  retry      s:                 skip\n");
  Putf("u:                  up         w:                 write\n");
  Putf("C:                  toggle show context\n");
#if O_DEBUG
  Putf("[level] D:	    set system debug level\n");
#endif
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Write goal of stack frame.  First a term representing the  goal  of  the
frame  is  constructed.  Trail and global stack are marked and undone to
avoid garbage on the global stack.

Trick, trick, O big trick ... In order to print the  goal  we  create  a
term  for  it  (otherwise  we  would  have to write a special version of
write/1, etc.  for stack frames).  A small problem arises: if the  frame
holds a variable we will make a reference to the new term, thus printing
the wrong variable: variables sharing in a clause does not seem to share
any  longer  in  the  tracer  (Anjo  Anjewierden discovered this ackward
feature of the tracer).  The solution is simple: we make  the  reference
pointer  the other way around.  Normally references should never go from
the global to the local stack as the local stack frame  might  cease  to
exists  before  the  global frame.  In this case this does not matter as
the local stack frame definitely survives the tracer (measuring does not
always mean influencing in computer science).

For the above reason, the code  below uses low-level manipulation rather
than normal unification, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
writeFrameGoal(LocalFrame frame, int how)
{ Definition def = frame->predicate;
  Word argv = argFrameP(frame, 0);
  int argc = def->functor->arity;
  int debugSave = debugstatus.debugging;
  fid_t cid = PL_open_foreign_frame();
  term_t goal = PL_new_term_ref();

  if ( debugstatus.showContext )
    Putf("[%s] ", stringAtom(contextModule(frame)->name));
  if ( def->module != MODULE_user &&
       (false(def->module, SYSTEM) || SYSTEM_MODE))
    Putf("%s:", stringAtom(def->module->name));

  PL_unify_functor(goal, def->functor);
  if ( argc > 0 )
  { Word argp = valTermRef(goal);
    int i;

    deRef(argp);
    argp = argTermP(*argp, 0);

    for(i=0; i<argc; i++)
    { Word a;

      deRef2(argv+i, a);
      *argp++ = (isVar(*a) ? makeRef(a) : *a);
    }
  }
  
  switch(how)
  { case W_PRINT:
	debugstatus.debugging = FALSE;
	if ( status.boot )
	  pl_write(goal);
	else
	  pl_print(goal);
	debugstatus.debugging = debugSave;
	break;
    case W_WRITE:
	pl_write(goal);
	break;
    case W_WRITEQ:
	pl_writeq(goal);
	break;
    case W_DISPLAY:
	pl_display(goal);
	break;
  }

  PL_discard_foreign_frame(cid);
}

/*  Write those frames on the stack that have alternatives left.

 ** Tue May 10 23:23:11 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static void
alternatives(LocalFrame frame)
{ for(; frame; frame = frame->backtrackFrame)
  { if (hasAlternativesFrame(frame) &&
	 (false(frame, FR_NODEBUG) || SYSTEM_MODE) )
    { Putf("    [%3ld] ", levelFrame(frame));
      writeFrameGoal(frame, debugstatus.style);
      Put('\n');
    }
  }
}    


static void
listProcedure(Definition def)
{ fid_t cid = PL_open_foreign_frame();
  qid_t qid;
  extern int Output;
  int OldOut = Output;
  int debugSave = debugstatus.debugging;
  term_t argv = PL_new_term_refs(1);
  Procedure proc = lookupProcedure(FUNCTOR_listing1, MODULE_system);

  unify_definition(argv, def, 0);	/* module:name(args) */
  Output = 1;
  debugstatus.debugging = FALSE;
  qid = PL_open_query(MODULE_user, FALSE, proc, argv);
  PL_next_solution(qid);
  PL_close_query(qid);
  debugstatus.debugging = debugSave;
  Output = OldOut;
  PL_discard_foreign_frame(cid);
}


void
backTrace(LocalFrame frame, int depth)
{ extern int Output;
  int OldOut = Output;
  LocalFrame same_proc_frame = NULL;
  Definition def = NULL;
  int same_proc = 0;
  int alien = FALSE;

  if ( frame == NULL )
     frame = environment_frame;

  Output = 1;
  for(; depth > 0 && frame;
        alien = (frame->parent == NULL), frame = parentFrame(frame))
  { if ( alien )
      Putf("    <Alien goal>\n");

    if ( frame->predicate == def )
    { if ( ++same_proc >= 10 )
      { if ( same_proc == 10 )
	  Putf("    ...\n    ...\n");
	same_proc_frame = frame;  
	continue;
      }
    } else
    { if ( same_proc_frame != NULL )
      { if ( false(same_proc_frame, FR_NODEBUG) || SYSTEM_MODE )
        { Putf("    [%3ld] ", levelFrame(same_proc_frame));
	  writeFrameGoal(same_proc_frame, debugstatus.style);
	  depth--;
	  Put('\n');
	}
	same_proc_frame = NULL;
	same_proc = 0;
      }
      def = frame->predicate;
    }

    if (false(frame, FR_NODEBUG) || SYSTEM_MODE)
    { Putf("    [%3ld] ", levelFrame(frame));
      writeFrameGoal(frame, debugstatus.style);
      depth--;
      Put('\n');
    }
  }
  Output = OldOut;
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
traceInterception(LocalFrame frame, LocalFrame bfr, int port, Code PC)
{ int rval = -1;			/* Default C-action */
  static predicate_t proc;

  if ( !proc )
    proc = PL_predicate("prolog_trace_interception", 4, "user");
  if ( !proc->definition->definition.clauses )
    return rval;

  if ( !status.boot && status.debugLevel == 0 )
  { fid_t cid = PL_open_foreign_frame();
    qid_t qid;
    term_t argv = PL_new_term_refs(4);
    term_t rarg = argv+3;
    atom_t portname = NULL_ATOM;
    functor_t portfunc = NULL;

    switch(port)
    { case CALL_PORT:	  portname = ATOM_call;		break;
      case REDO_PORT:	  portname = ATOM_redo;		break;
      case EXIT_PORT:	  portname = ATOM_exit;		break;
      case FAIL_PORT:	  portname = ATOM_fail;		break;
      case UNIFY_PORT:	  portname = ATOM_unify;	break;
      case BREAK_PORT:    portfunc = FUNCTOR_break1;	break;
      case CUT_CALL_PORT: portfunc = FUNCTOR_cut_call1; break;
      case CUT_EXIT_PORT: portfunc = FUNCTOR_cut_exit1; break;
      default:
	assert(0);
        return rval;
    }

    if ( portname )
      PL_put_atom(argv, portname);
    else
    { int pcn;

      if ( PC && false(frame->predicate, FOREIGN) && frame->clause )
	pcn = PC - frame->clause->clause->codes;
      else
	pcn = 0;

      PL_unify_term(argv,
		    PL_FUNCTOR, portfunc,
		    PL_INTEGER, pcn);
    }

    PL_put_integer(argv+1, PrologRef(frame));
    PL_put_integer(argv+2, PrologRef(bfr));
    PL_put_variable(rarg);

    debugstatus.suspendTrace++;
    qid = PL_open_query(MODULE_user, FALSE, proc, argv);
    if ( PL_next_solution(qid) )
    { atom_t a;

      if ( PL_get_atom(rarg, &a) )
      { if ( a == ATOM_continue )
	  rval = ACTION_CONTINUE;
	else if ( a == ATOM_fail )
	  rval = ACTION_FAIL;
	else if ( a == ATOM_retry )
	  rval = ACTION_RETRY;
	else if ( a == ATOM_ignore )
	  rval = ACTION_IGNORE;
      } else if ( PL_is_functor(rarg, FUNCTOR_retry1) )
      { long w;
	term_t arg = PL_new_term_ref();

	if ( PL_get_arg(1, rarg, arg) && PL_get_long(arg, &w) )
	{ debugstatus.retryFrame = FrameRef(w);
	  rval = ACTION_RETRY;
	} else
	  warning("prolog_trace_interception/3: bad argument to retry/1");
      }
    }
    PL_close_query(qid);
    PL_discard_foreign_frame(cid);
    debugstatus.suspendTrace--;
  }

  return rval;
}

#endif /*O_DEBUGGER*/

static bool
hasAlternativesFrame(register LocalFrame frame)
{ ClauseRef cref;

  if ( true(frame, FR_CUT) )
    fail;
  if (true(frame->predicate, FOREIGN))
    succeed;
  for(cref = frame->clause; cref; cref = cref->next)
    if ( false(cref->clause, ERASED) )
      succeed;
  fail;
}

word
pl_trace_continuation(term_t what)
{ return PL_get_integer(what, &trace_continuation);
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
{ Putf("Options:\n");
  Putf("a:                 abort      b:                 break\n");
  Putf("c:                 continue   e:                 exit\n");
#ifdef O_DEBUGGER
  Putf("g:                 goals      t:                 trace\n");
#endif
  Putf("h (?):             help\n");

}

static void
interruptHandler(int sig)
{ extern int Output;
  int OldOut = Output;
  LocalFrame oldltop = lTop;
  Char c; 

  if ( status.initialised == FALSE )
  { Sfprintf(Serror, "Interrupt during startup. Cannot continue\n");
    Halt(1);
  }  

  Output = 1;
  gc_status.blocked++;
#if O_SHIFT_STACKS
  shift_status.blocked++;
#endif
  lTop = (LocalFrame)addPointer(lTop, sizeof(struct localFrame) +
				      MAXARITY * sizeof(word));

again:
  Putf("\nAction (h for help) ? ");
  pl_flush();
  ResetTty();                           /* clear pending input -- atoenne -- */
  c = getSingleChar();

#ifndef BSD_SIGNALS
#ifdef SIG_ACK
  signal(SIGINT, SIG_ACK);
#else
  signal(SIGINT, interruptHandler);	/* reinsert handler */
#endif
#endif

  switch(c)
  { case 'a':	Putf("abort\n");
		pl_abort();
		break;
    case 'b':	Putf("break\n");
		pl_break();
		goto again;		
    case 'c':	Putf("continue\n");
		break;
    case 04:
    case EOF:	Putf("EOF: ");
    case 'e':	Putf("exit\n");
		Halt(0);
		break;
#ifdef O_DEBUGGER
    case 'g':	Putf("goals\n");
		backTrace(environment_frame, 5);
		goto again;
#endif /*O_DEBUGGER*/
    case 'h':
    case '?':	helpInterrupt();
		goto again;
#ifdef O_DEBUGGER
    case 't':	Putf("trace\n");
		pl_trace();
		break;
#endif /*O_DEBUGGER*/
    default:	Putf("Unknown option (h for help)\n");
		goto again;
  }
#if O_SHIFT_STACKS
  shift_status.blocked--;
#endif
  gc_status.blocked--;
  Output = OldOut;
  lTop = oldltop;
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
resetTracer(void)
{
#ifdef O_INTERRUPT
  signal(SIGINT, interruptHandler);
#endif
}


void
initTracer(void)
{ 
#if defined(SIGINT) && defined(O_INTERRUPT)
  pl_signal(SIGINT, interruptHandler);
#endif

  debugstatus.visible      = 
  debugstatus.leashing     = CALL_PORT|FAIL_PORT|REDO_PORT|EXIT_PORT|
			     BREAK_PORT;
  debugstatus.tracing      =
  debugstatus.debugging    = FALSE;
  debugstatus.suspendTrace = FALSE;
  debugstatus.skiplevel    = 0;
  debugstatus.style        = status.boot ? W_WRITE : W_PRINT; 
  debugstatus.showContext  = FALSE;
  debugstatus.retryFrame   = NULL;
}

		/********************************
		*       PROLOG PREDICATES       *
		*********************************/

int
tracemode(int doit, int *old)
{ if ( doit )
    doit = TRUE;

  if ( old )
    *old = debugstatus.tracing;

  if ( debugstatus.tracing != doit )
  { if ( doit )
    { debugstatus.debugging = TRUE;
      debugstatus.skiplevel = VERY_DEEP;
      find.searching = FALSE;
    }
    debugstatus.tracing = doit;
    callEventHook(PLEV_TRACING, doit);
  }

  succeed;
}


int
debugmode(int doit, int *old)
{ if ( doit )
    doit = TRUE;

  if ( old )
    *old = debugstatus.debugging;

  if ( debugstatus.debugging != doit )
  { if ( doit )
      debugstatus.skiplevel = VERY_DEEP;
    debugstatus.debugging = doit;
    callEventHook(PLEV_DEBUGGING, doit);
  }

  succeed;
}


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
{ return debugstatus.tracing;
}

word
pl_debug()
{ return debugmode(TRUE, NULL);
}

word
pl_nodebug()
{ return debugmode(FALSE, NULL);
}

word
pl_debugging()
{ return debugstatus.debugging;
}

word
pl_skip_level(term_t old, term_t new)
{ atom_t a;

  if ( debugstatus.skiplevel == VERY_DEEP )
  { TRY(PL_unify_atom(old, ATOM_very_deep));
  } else
  { TRY(PL_unify_integer(old, debugstatus.skiplevel));
  }
      
  if ( PL_get_long(new, &debugstatus.skiplevel) )
    succeed;
  if ( PL_get_atom(new, &a) && a == ATOM_very_deep)
  { debugstatus.skiplevel = VERY_DEEP;
    succeed;
  }

  fail;
}

word
pl_spy(term_t p)
{ Procedure proc;

  if ( get_procedure(p, &proc, 0, GP_FIND) )
  { set(proc->definition, SPY_ME);
    return pl_debug();
  }

  fail;
}

word
pl_nospy(term_t p)
{ Procedure proc;

  if ( get_procedure(p, &proc, 0, GP_FIND) )
  { clear(proc->definition, SPY_ME);
    succeed;
  }

  fail;
}

word
pl_leash(term_t old, term_t new)
{ return setInteger(&debugstatus.leashing, "$leash", old, new);
}

word
pl_visible(term_t old, term_t new)
{ return setInteger(&debugstatus.visible, "$visible", old, new);
}


word
pl_debuglevel(term_t old, term_t new)
{ return setInteger(&status.debugLevel, "$visible", old, new);
}


word
pl_unknown(term_t old, term_t new)
{ Module m = contextModule(environment_frame);
  atom_t a = (true(m, UNKNOWN) ? ATOM_trace : ATOM_fail);

  if ( !PL_unify_atom(old, a) )
    fail;
  if ( PL_get_atom(new, &a) )
  { if ( a == ATOM_fail ) 
    { clear(m, UNKNOWN);
      succeed;
    } else if ( a == ATOM_trace )
    { set(m, UNKNOWN);
      succeed;
    }
  }
  
  return warning("unknown/2: instantiation fault");
}


word
pl_prolog_current_frame(term_t frame)
{ LocalFrame fr = environment_frame;

  if ( fr->predicate->definition.function == pl_prolog_current_frame )
    fr = parentFrame(fr);		/* thats me! */

  return PL_unify_integer(frame, PrologRef(fr));
}


word
pl_prolog_frame_attribute(term_t frame, term_t what,
			  term_t value)
{ LocalFrame fr;
  atom_t key;
  int arity;
  term_t result = PL_new_term_ref();
  long fri;

  if ( !PL_get_long(frame, &fri) ||
       !PL_get_name_arity(what, &key, &arity) )
  { ierr:
    return warning("prolog_frame_attribute/3: instantiation fault");
  }

  if ((fr = FrameRef(fri)) < lBase || fr > lTop)
    return warning("prolog_frame_attribute/3: illegal frame reference");

  if ( key == ATOM_argument && arity == 1 )
  { term_t arg = PL_new_term_ref();
    int argn;
    Word p = valTermRef(value);

    if ( !PL_get_arg(1, what, arg) || !PL_get_integer(arg, &argn) || argn < 1 )
      goto ierr;

    if ( true(fr->predicate, FOREIGN) || !fr->clause )
    { if ( argn > fr->predicate->functor->arity )
	fail;
    } else
    { if ( argn > fr->clause->clause->prolog_vars )
	fail;
    }

#ifdef O_DEBUGLOCAL			/* see pl-wam.c */
    assert( *argFrameP(fr, argn-1) != (word)(((char*)ATOM_nil) + 1) );
    checkData(argFrameP(fr, argn-1), FALSE);
#endif

   deRef(p);
   if ( isVar(*p) )
   { *p = makeRef(argFrameP(fr, argn-1));
     DoTrail(p);
     succeed;
   }

   fail;
  }
  if ( arity != 0 )
    goto ierr;

  if (        key == ATOM_level)
  { PL_put_integer(result, levelFrame(fr));
  } else if (key == ATOM_has_alternatives)
  { PL_put_atom(result, hasAlternativesFrame(fr) ? ATOM_true : ATOM_false);
  } else if (key == ATOM_alternative)
  { if (fr->backtrackFrame == (LocalFrame) NULL)
      fail;
    PL_put_integer(result, PrologRef(fr->backtrackFrame));
  } else if (key == ATOM_parent)
  { LocalFrame parent;

    if ( fr->parent )
      clearUninitialisedVarsFrame(fr->parent, fr->programPointer);

    if ( (parent = parentFrame(fr)) )
      PL_put_integer(result, PrologRef(parent));
    else
      fail;
  } else if (key == ATOM_top)
  { PL_put_atom(result, fr->parent ? ATOM_false : ATOM_true);
  } else if (key == ATOM_context_module)
  { PL_put_atom(result, contextModule(fr)->name);
  } else if (key == ATOM_clause)
  { if ( false(fr->predicate, FOREIGN) && fr->clause )
      PL_put_pointer(result, fr->clause->clause);
    else
      fail;
  } else if (key == ATOM_goal)
  { int arity, n;
    term_t arg = PL_new_term_ref();
    
    if (fr->predicate->module != MODULE_user)
    { PL_put_functor(result, FUNCTOR_module2);
      PL_get_arg(1, result, arg);
      PL_unify_atom(arg, fr->predicate->module->name);
      PL_get_arg(2, result, arg);
    } else
      PL_put_term(arg, result);

    if ((arity = fr->predicate->functor->arity) == 0)
    { PL_unify_atom(arg, fr->predicate->functor->name);
    } else
    { term_t a = PL_new_term_ref();

      PL_unify_functor(arg, fr->predicate->functor);
      for(n=0; n < arity; n++)
      { PL_get_arg(n+1, arg, a);
	unify_ptrs(valTermRef(a), argFrameP(fr, n));
      }
    }
  } else if ( key == ATOM_pc )
  { if ( fr->programPointer &&
	 fr->parent &&
	 false(fr->parent->predicate, FOREIGN) )
      PL_put_integer(result,
		     fr->programPointer - fr->parent->clause->clause->codes);
    else
      fail;
  } else if ( key == ATOM_hidden )
  { atom_t a;

    if ( SYSTEM_MODE )
    { a = ATOM_true;
    } else
    { if ( true(fr, FR_NODEBUG) || false(fr->predicate, TRACE_ME) )
	a = ATOM_true;
      else
	a = ATOM_false;
    }

    PL_put_atom(result, a);
  } else
    return warning("prolog_frame_attribute/3: unknown key");

  return PL_unify(value, result);
}


		 /*******************************
		 *	  PROLOG EVENET HOOK	*
		 *******************************/

void
callEventHook(int ev, ...)
{ if ( !PROCEDURE_event_hook1 )
    PROCEDURE_event_hook1 = PL_predicate("prolog_event_hook", 1, "user");
  
  if ( PROCEDURE_event_hook1->definition->definition.clauses )
  { va_list args;
    fid_t fid = PL_open_foreign_frame();
    term_t arg = PL_new_term_ref();

    va_start(args, ev);
    switch(ev)
    { case PLEV_ERASED:
      {	void *ptr = va_arg(args, void *); 	/* object erased */

	PL_unify_term(arg, PL_FUNCTOR, FUNCTOR_erased1,
		           PL_POINTER, ptr);
	break;
      }
      case PLEV_DEBUGGING:
      { int dbg = va_arg(args, int);
	
	PL_unify_term(arg, PL_FUNCTOR, FUNCTOR_debugging1,
			   PL_ATOM, dbg ? ATOM_true : ATOM_false);
	break;
      }
      case PLEV_TRACING:
      { int trc = va_arg(args, int);
	
	PL_unify_term(arg, PL_FUNCTOR, FUNCTOR_tracing1,
			   PL_ATOM, trc ? ATOM_true : ATOM_false);
	break;
      }
      case PLEV_BREAK:
      case PLEV_NOBREAK:
      { Clause clause = va_arg(args, Clause);
	int offset = va_arg(args, int);

	PL_unify_term(arg, PL_FUNCTOR, FUNCTOR_break3,
		           PL_POINTER, clause,
		           PL_INTEGER, offset,
			   PL_ATOM, ev == PLEV_BREAK ? ATOM_true
					             : ATOM_false);
	break;
      }
      default:
	warning("callEventHook(): unknown event: %d", ev);
        goto out;
    }
    
    PL_call_predicate(MODULE_user, FALSE, PROCEDURE_event_hook1, arg);
  out:
    PL_discard_foreign_frame(fid);
    va_end(args);
  }
}
