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

#define PrologRef(fr)	 consNum((Word)fr - (Word)lBase)
#define FrameRef(w)	 ((LocalFrame)((Word)lBase + valNum(w)))

#ifdef O_DEBUGGER

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines the tracer and interrupt  handler  that  allows  the
user  to break the normal Prolog execution.  The tracer is written in C,
but before taking action it calls Prolog.   This  mechanism  allows  the
user to intercept and redefine the tracer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

					/* Frame <-> Prolog integer */
forwards LocalFrame	redoFrame(LocalFrame);
forwards int		traceAction(char *, int, LocalFrame, bool);
forwards void		helpTrace(void);
#ifdef O_INTERRUPT
forwards void		helpInterrupt(void);
#endif
forwards bool		hasAlternativesFrame(LocalFrame);
forwards void		alternatives(LocalFrame);
forwards void		listProcedure(Definition);
forwards int		traceInterception(LocalFrame, int);
forwards bool		canUnifyTermWithGoal(Word, LocalFrame);
forwards int		setupFind(char *);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
redoFrame() returns the latest skipped frame or NULL if  no  such  frame
exists.   This  is used to give the redo port of the goal skipped rather
than the redo port of some subgoal of this port.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static LocalFrame
redoFrame(register LocalFrame fr)
{ for( ; fr && false(fr, FR_SKIPPED); fr = parentFrame(fr) )
    ;

  return fr;
}

static bool
canUnifyTermWithGoal(Word t, LocalFrame fr)
{ deRef(t);
  if ( isVar(*t) )
    succeed;
  if ( isAtom(*t) && fr->predicate->functor->name == (Atom)*t )
    succeed;
  if ( isTerm(*t) && functorTerm(*t) == fr->predicate->functor )
  { mark m;
    Word a, b;
    int arity;

    Mark(m);
    a = argTermP(*t, 0);
    b = argFrameP(fr, 0);
    arity = functorTerm(*t)->arity;
    while( arity > 0 )
    { if ( !unify(a, b, environment_frame) )
      { Undo(m);
        fail;
      }
    }
    Undo(m);
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
tracePort(LocalFrame frame, int port)
{ int OldOut;
  extern int Output;
  int action = ACTION_CONTINUE;
  Definition def = frame->predicate;
  LocalFrame fr;

  if ( (true(frame, FR_NODEBUG) && !(SYSTEM_MODE))	|| /* hidden */
       debugstatus.suspendTrace )			   /* called back */
    return ACTION_CONTINUE;

  if ( true(def, TRACE_CALL|TRACE_REDO|TRACE_EXIT|TRACE_FAIL) )
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

  if ( (!debugstatus.tracing && false(def, SPY_ME))	|| /* non-tracing */
       debugstatus.skiplevel < levelFrame(frame)	|| /* skipped */
       false(def, TRACE_ME)				|| /* non-tracing */
       (!(debugstatus.visible & port))			|| /* wrong port */
       (port == REDO_PORT && (debugstatus.skiplevel == levelFrame(frame) ||
			      (true(def, SYSTEM) && !SYSTEM_MODE)
			     )) )				   /* redos */
    return ACTION_CONTINUE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Give a trace on the skipped goal for a redo.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( port == REDO_PORT && debugstatus.skiplevel == VERY_DEEP &&
       (fr = redoFrame(frame)) != NULL )
  { debugstatus.skiplevel--;				   /* avoid a loop */
    switch( tracePort(fr, REDO_PORT) )
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

  if ((action = traceInterception(frame, port)) >= 0)
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
    case FAIL_PORT:	Putf(" Fail:  ");
			DoUndo(frame->mark);	break;
    case EXIT_PORT:	Putf(" Exit:  ");	break;
    case UNIFY_PORT:	Putf(" Unify: ");	break;
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
{ Word w;
  mark m;
  long rval;
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

  Mark(m);
  seeString(s);
  w = newTerm();
  rval = pl_read(w);
  seenString();

  if ( rval == FALSE )
  { Undo(m);
    fail;
  }

  if ( find.goal != NULL )
    freeRecord(find.goal);
  find.port      = port;
  find.goal      = copyTermToHeap(w);
  find.searching = TRUE;
  Undo(m);

  DEBUG(2, Sdprintf("setup ok, port = 0x%x, goal = ", port);
	   pl_write(&find.goal->term);
	   Sdprintf("\n") );

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
    		if ( setupFind(&s[1]) == TRUE )
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
writeFrameGoal(LocalFrame frame, int how)
{ Definition def = frame->predicate;
  Word argv = argFrameP(frame, 0);
  Word argp;
  int argc = def->functor->arity;
  int n;
  word goal;
  mark m;
  int debugSave = debugstatus.debugging;

  if ( debugstatus.showContext )
    Putf("[%s] ", stringAtom(contextModule(frame)->name));
  if ( def->module != MODULE_user &&
       (false(def->module, SYSTEM) || SYSTEM_MODE))
    Putf("%s:", stringAtom(def->module->name));

  Mark(m);
  if (argc == 0)
    goal = (word) def->functor->name;
  else
  { goal = globalFunctor(def->functor);
    argp = argTermP(goal, 0);
    for(n=0; n<argc; n++, argp++, argv++)
    { register Word a;
  
      deRef2(argv, a);
      *argp = (isVar(*a) ? makeRef(a) : *a);
    }
  }
  
  switch(how)
  { case W_PRINT:
	debugstatus.debugging = FALSE;
	if ( status.boot )
	  pl_write(&goal);
	else
	  pl_print(&goal);
	debugstatus.debugging = debugSave;
	break;
    case W_WRITE:
	pl_write(&goal);
	break;
    case W_WRITEQ:
	pl_writeq(&goal);
	break;
    case W_DISPLAY:
	pl_display(&goal);
	break;
  }
  Undo(m);
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
{ extern int Output;
  int OldOut = Output;
  word goal, mod, spec;
  int debugSave = debugstatus.debugging;
  mark m;

  Mark(m);
  goal = globalFunctor(FUNCTOR_listing1);
  mod  = globalFunctor(FUNCTOR_module2);
  spec = globalFunctor(FUNCTOR_divide2);
  argTerm(goal, 0) = mod;
  argTerm(mod, 0)  = (word) def->module->name;
  argTerm(mod, 1)  = spec;
  argTerm(spec, 0) = (word) def->functor->name;
  argTerm(spec, 1) = consNum(def->functor->arity);

  Output = 1;
  debugstatus.debugging = FALSE;
  callGoal(MODULE_system, goal, FALSE);		/* listing(mod:name/arity) */
  debugstatus.debugging = debugSave;
  Output = OldOut;
  Undo(m);
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
prolog_trace_interception/3, allowing the user to define his/her action.
If  this procedure succeeds the tracer assumes the trace action has been
done and returns, otherwise the  default  C-defined  trace  actions  are
performed.

The functions traceInterception() and pl_prolog_trace_continuation() are
the entry points from the C-defined tracer. $prolog_trace_interception/2
is responsible for the communication with the users' predicate.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
traceInterception(LocalFrame frame, int port)
{ word goal;
  Word arg;
  mark m;
  bool rval;

  if (status.boot == TRUE || status.debugLevel > 0)
    return -1;

  Mark(m);
  goal = globalFunctor(FUNCTOR_traceinterc2);
  arg = argTermP(goal, 0);
  switch(port)
  { case CALL_PORT:	*arg = (word) ATOM_call;	break;
    case REDO_PORT:	*arg = (word) ATOM_redo;	break;
    case EXIT_PORT:	*arg = (word) ATOM_exit;	break;
    case FAIL_PORT:	*arg = (word) ATOM_fail;	break;
    case UNIFY_PORT:	*arg = (word) ATOM_unify;	break;
  }
  *++arg = PrologRef(frame);

  debugstatus.suspendTrace++;
  rval = callGoal(MODULE_system, goal, FALSE);
  debugstatus.suspendTrace--;

  Undo(m);

  if (rval == TRUE)
    return trace_continuation;
  else
    return -1;
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
pl_trace_continuation(Word what)
{ if (isInteger(*what) )
  { trace_continuation = (int)valNum(*what);
    succeed;
  }

  fail;
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

void
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
  Output = OldOut;
  lTop = oldltop;
}

#endif /*O_INTERRUPT*/

void
initTracer(void)
{ 
#if defined(SIGINT) && defined(O_INTERRUPT)
  pl_signal(SIGINT, interruptHandler);
#endif

  debugstatus.visible  = CALL_PORT|FAIL_PORT|REDO_PORT|EXIT_PORT;
  debugstatus.leashing = CALL_PORT|FAIL_PORT|REDO_PORT|EXIT_PORT;
  debugstatus.tracing = debugstatus.debugging = FALSE;
  debugstatus.suspendTrace = FALSE;
  debugstatus.skiplevel = 0;
  debugstatus.style = status.boot ? W_WRITE : W_PRINT; 
  debugstatus.showContext = FALSE;
}

		/********************************
		*       PROLOG PREDICATES       *
		*********************************/

word
pl_trace(void)
{ debugstatus.debugging = debugstatus.tracing = TRUE;
  debugstatus.skiplevel = VERY_DEEP;
  find.searching = FALSE;

  succeed;
}

word
pl_notrace(void)
{ debugstatus.tracing = FALSE;

  succeed;
}

word
pl_tracing(void)
{ return debugstatus.tracing;
}

word
pl_debug(void)
{ debugstatus.debugging = TRUE;
  debugstatus.skiplevel = VERY_DEEP;

  succeed;
}

word
pl_nodebug(void)
{ debugstatus.debugging = FALSE;

  succeed;
}

word
pl_debugging()
{ if ( debugstatus.debugging )
    succeed;

  fail;
}

word
pl_skip_level(Word old, Word new)
{ TRY(unifyAtomic(old, debugstatus.skiplevel == VERY_DEEP ?
			(word) ATOM_very_deep :
			consNum(debugstatus.skiplevel)) );

  if (isInteger(*new) )
  { debugstatus.skiplevel = valNum(*new);
    succeed;
  }
  if (isAtom(*new) && *new == (word) ATOM_very_deep)
  { debugstatus.skiplevel = VERY_DEEP;
    succeed;
  }
  fail;
}

word
pl_spy(Word p)
{ Procedure proc;

  if ((proc = findProcedure(p)) == (Procedure) NULL)
    fail;
  set(proc->definition, SPY_ME);

  return pl_debug();
}

word
pl_nospy(Word p)
{ Procedure proc;

  if ((proc = findProcedure(p)) == (Procedure) NULL)
    fail;
  clear(proc->definition, SPY_ME);

  succeed;
}

word
pl_leash(Word old, Word new)
{ TRY(unifyAtomic(old, consNum(debugstatus.leashing) ));

  if (!isInteger(*new) )
    fail;
  debugstatus.leashing = valNum(*new) & 0x1f;

  succeed;
}

word
pl_visible(Word old, Word new)
{ TRY(unifyAtomic(old, consNum(debugstatus.visible) ));

  if (!isInteger(*new) )
    fail;
  debugstatus.visible = valNum(*new) & 0x1f;

  succeed;
}


word
pl_debuglevel(Word old, Word new)
{ TRY(unifyAtomic(old, consNum(status.debugLevel)));
  if ( isInteger(*new) )
    status.debugLevel = valNum(*new);

  succeed;
}


word
pl_unknown(Word old, Word new)
{ Module m = contextModule(environment_frame);

  TRY(unifyAtomic(old, true(m, UNKNOWN) ? ATOM_trace : ATOM_fail) );
  if (*new == (word) ATOM_trace)
    set(m, UNKNOWN);
  else if (*new == (word) ATOM_fail)
    clear(m, UNKNOWN);
  else
    return warning("unknown/2: argument should be 'fail' or 'trace'");

  succeed;
}

word
pl_prolog_current_frame(Word fr)
{ return unifyAtomic(fr, PrologRef(parentFrame(environment_frame)));
}

word
pl_prolog_frame_attribute(Word frame, Word what, Word value)
{ LocalFrame fr;
  Atom key;
  word result;

  if (!isInteger(*frame) || !isAtom(*what) || !isVar(*value))
    return warning("prolog_frame_attribute/3: instantiation fault");

  if ((fr = FrameRef(*frame)) < lBase || fr > lTop)
    return warning("prolog_frame_attribute/3: illegal frame reference");
  key = (Atom) *what;
  
  if (        key == ATOM_level)
  { result = consNum(levelFrame(fr));
  } else if (key == ATOM_has_alternatives)
  { result = consNum(hasAlternativesFrame(fr) );
  } else if (key == ATOM_alternative)
  { if (fr->backtrackFrame == (LocalFrame) NULL)
      fail;
    result = PrologRef(fr->backtrackFrame);
  } else if (key == ATOM_parent)
  { LocalFrame parent;

    if ((parent = parentFrame(fr)) != (LocalFrame) NULL)
      result = PrologRef(parent);
    else
      fail;
  } else if (key == ATOM_top)
  { result = consNum(fr->parent == (LocalFrame) NULL ? 1 : 0);
  } else if (key == ATOM_context_module)
  { result = (word) contextModule(fr)->name;
  } else if (key == ATOM_clause)
  { if ( false(fr->predicate, FOREIGN) && fr->clause )
      result = pointerToNum(fr->clause);
    else
      fail;
  } else if (key == ATOM_goal)
  { int arity, n;
    Word arg;
    
    if (fr->predicate->module != MODULE_user)
    { result = globalFunctor(FUNCTOR_module2);
      argTerm(result, 0) = (word) fr->predicate->module->name;
      arg = argTermP(result, 1);
    } else
      arg = &result;

    if ((arity = fr->predicate->functor->arity) == 0)
    { *arg = (word) fr->predicate->functor->name;
    } else
    { *arg = globalFunctor(fr->predicate->functor);
      for(arg=argTermP(*arg, 0), n=0; n < arity; arg++, n++)
	pl_unify(arg, argFrameP(fr, n) );
    }
  } else
    return warning("prolog_frame_attribute/3: unknown key");

  return pl_unify(value, &result);
}
