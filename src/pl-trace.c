/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: tracer
*/

#include "pl-incl.h"
#include "pl-ctype.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines the tracer and interrupt  handler  that  allows  the
user  to break the normal Prolog execution.  The tracer is written in C,
but before taking action it calls Prolog.   This  mechanism  allows  the
user to intercept and redefine the tracer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define W_PRINT		1		/* print/1 for displaying goal */
#define W_WRITE		2		/* write/1 */
#define W_WRITEQ	3		/* writeq/1 */
#define W_DISPLAY	4		/* display/1 */

					/* Frame <-> Prolog integer */
#define PrologRef(fr)	 consNum((Word)fr - (Word)lBase)
#define FrameRef(w)	 ((LocalFrame)((Word)lBase + valNum(w)))

forwards LocalFrame	redoFrame P((LocalFrame));
forwards int		traceAction P((char, int, LocalFrame, bool));
forwards void		helpTrace P((void));
forwards void		helpInterrupt P((void));
forwards bool		hasAlternativesFrame P((LocalFrame));
forwards void		alternatives P((LocalFrame));
forwards void		listProcedure P((Procedure));
forwards int		traceInterception P((LocalFrame, int));
forwards bool		canUnifyTermWithGoal P((Word, LocalFrame));
forwards int		setupFind P((void));

static struct
{ int	 port;				/* Port to find */
  bool	 searching;			/* Currently searching? */
  Record goal;				/* Goal to find */
} find;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
redoFrame() returns the latest skipped frame or NULL if  no  such  frame
exists.   This  is used to give the redo port of the goal skipped rather
than the redo port of some subgoal of this port.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static LocalFrame
redoFrame(fr)
register LocalFrame fr;
{ for( ; fr && false(fr, FR_SKIPPED); fr = parentFrame(fr) )
    ;

  return fr;
}

static bool
canUnifyTermWithGoal(t, fr)
Word t;
LocalFrame fr;
{ deRef(t);
  if ( isVar(*t) )
    succeed;
  if ( isAtom(*t) && fr->procedure->functor->name == (Atom)*t )
    succeed;
  if ( isTerm(*t) && functorTerm(*t) == fr->procedure->functor )
  { mark m;
    Word a, b;
    int arity;

    Mark(m);
    a = argTermP(*t, 0);
    b = argFrameP(fr, 0);
    arity = functorTerm(*t)->arity;
    while( arity > 0 )
    { if ( unify(a, b) == FALSE )
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
tracePort(frame, port)
LocalFrame frame;
int port;
{ int OldOut;
  extern int Output;
  int action = ACTION_CONTINUE;
  Procedure proc = frame->procedure;
  Definition def = proc->definition;
  LocalFrame fr;

  if ( (true(frame, FR_NODEBUG) && !(SYSTEM_MODE))	|| /* hidden */
       debugstatus.suspendTrace				|| /* called back */
       (!debugstatus.tracing && false(def, SPY_ME))	|| /* non-tracing */
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
  { DEBUG(2, printf("Searching\n"));

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
			Undo(frame->mark);	break;
    case EXIT_PORT:	Putf(" Exit:  ");	break;
    case UNIFY_PORT:	Putf(" Unify: ");	break;
  }
  Putf("(%3ld) ", levelFrame(frame));
  writeFrameGoal(frame, debugstatus.style);

  debugstatus.skiplevel = VERY_DEEP;
  debugstatus.tracing = TRUE;

  if (debugstatus.leashing & port)
  { char c;

    Putf(" ? ");
    pl_flush();
    c = getSingleChar();
    if ((action = traceAction(c, port, frame, status.notty ? FALSE : TRUE))
							== ACTION_AGAIN)
      goto again;
  } else
    Put('\n');
  Output = OldOut;

  return action;
}

static int
setupFind()
{ static word w;
  mark m;
  long rval;
  char buf[LINESIZ];
  char *s;
  int port = 0;

  readLine(buf, 0);  

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
  setVar(w);
  rval = pl_read(&w);
  seenString();

  if ( rval == FALSE )
  { Undo(m);
    fail;
  }

  if ( find.goal != NULL )
    freeRecord(find.goal);
  find.port      = port;
  find.goal      = copyTermToHeap(&w);
  find.searching = TRUE;
  Undo(m);

  DEBUG(2, printf("setup ok, port = 0x%x, goal = ", port);
	   pl_write(&find.goal->term);
	   printf("\n") );

  succeed;
}

#if PROTO
static int
traceAction(char c, int port, LocalFrame frame, bool interactive)
#else
static int
traceAction(c, port, frame, interactive)
char c;
int port;
LocalFrame frame;
bool interactive;
#endif
{
#define FeedBack(msg)	{ if (interactive) Putf(msg); }
#define Warn(msg)	{ if (interactive) Putf(msg); else warning(msg); }

  switch(c)
  { case 'a':	FeedBack("abort\n");
		pl_abort();
    case 'b':	FeedBack("break\n");
		pl_break();
		return ACTION_AGAIN;
    case '/': 	FeedBack("/");
    		pl_flush();
    		if ( setupFind() == TRUE )
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
    case ' ':
    case '\n':
    case 'c':	FeedBack("creep\n");
		clear(frame, FR_SKIPPED);
		return ACTION_CONTINUE;
    case '\04':
    case EOF:	FeedBack("EOF: ");
    case 'e':	FeedBack("exit\n");
		pl_halt();
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
		backTrace(frame);
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
		listProcedure(frame->procedure);
		return ACTION_AGAIN;
    case '+':	FeedBack("spy\n");
		set(frame->procedure->definition, SPY_ME);
		return ACTION_AGAIN;
    case '-':	FeedBack("no spy\n");
		clear(frame->procedure->definition, SPY_ME);
		return ACTION_AGAIN;
    case '?': 
    case 'h':	helpTrace();
		return ACTION_AGAIN;
    default:	if (isDigit(c))
		{ status.debugLevel = (int)c - '0';
		  FeedBack("Debug level\n");
		  return ACTION_AGAIN;
		}
		Warn("Unknown option (h for help)\n");
		return ACTION_AGAIN;
  }
}

static void
helpTrace()
{ Putf("Options:\n");
  Putf("+:                 spy        -:                 no spy\n");
  Putf("/ports goal:       find       .:                 repeat find\n");
  Putf("a:                 abort      A:                 alternatives\n");
  Putf("b:                 break      c (return, space): creep\n");
  Putf("d:                 display    e:                 exit\n");
  Putf("f:                 fail       g:                 goals\n");
  Putf("h (?):             help       i:                 ignore\n");
  Putf("l:                 leap       L:                 listing\n");
  Putf("n:                 no debug   p:                 print\n");
  Putf("r:                 retry      s:                 skip\n");
  Putf("u:                 up         w:                 write\n");
  Putf("C:                 toggle show context\n");
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
writeFrameGoal(frame, how)
LocalFrame frame;
int how;
{ Procedure proc = frame->procedure;
  Definition def = proc->definition;
  Word argv = argFrameP(frame, 0);
  Word argp;
  int argc = proc->functor->arity;
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
    goal = (word) proc->functor->name;
  else
  { goal = globalFunctor(proc->functor);
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

static bool
hasAlternativesFrame(frame)
register LocalFrame frame;
{ register Clause clause;

  if ( true(frame, FR_CUT) )
    fail;
  if (true(frame->procedure->definition, FOREIGN))
    succeed;
  for(clause = frame->clause; clause; clause = clause->next)
    if ( false(clause, ERASED) )
      succeed;
  fail;
}

static void
alternatives(frame)
LocalFrame frame;
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
listProcedure(proc)
Procedure proc;
{ extern int Output;
  int OldOut = Output;
  Word gSave = gTop;
  word goal = globalFunctor(FUNCTOR_listing1);
  word mod  = globalFunctor(FUNCTOR_module2);
  word spec = globalFunctor(FUNCTOR_divide2);
  int debugSave = debugstatus.debugging;

  argTerm(goal, 0) = mod;
  argTerm(mod, 0)  = (word) proc->definition->module->name;
  argTerm(mod, 1)  = spec;
  argTerm(spec, 0) = (word) proc->functor->name;
  argTerm(spec, 1) = consNum(proc->functor->arity);

  Output = 1;
  debugstatus.debugging = FALSE;
  callGoal(MODULE_system, goal, FALSE);		/* listing(mod:name/arity) */
  debugstatus.debugging = debugSave;
  Output = OldOut;
  gTop = gSave;
}

void
backTrace(frame)
LocalFrame frame;
{ extern int Output;
  int OldOut = Output;
  LocalFrame same_proc_frame = NULL;
  Procedure proc = NULL;
  int same_proc = 0;

  if ( frame == NULL )
     frame = environment_frame;

  Output = 1;
  for( ; frame; frame = frame->parent)
  { if ( frame->procedure == proc )
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
	  Put('\n');
	}
	same_proc_frame = NULL;
	same_proc = 0;
      }
      proc = frame->procedure;
    }

    if (false(frame, FR_NODEBUG) || SYSTEM_MODE)
    { Putf("    [%3ld] ", levelFrame(frame));
      writeFrameGoal(frame, debugstatus.style);
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

int trace_continuation;			/* how to continue? */

word
pl_trace_continuation(what)
Word what;
{ if (isInteger(*what) )
  { trace_continuation = (int)valNum(*what);
    succeed;
  }

  fail;
}

static int
traceInterception(frame, port)
LocalFrame frame;
int port;
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Handling  interrupts.   We  know  we  are  not  in  critical  code  (see
startCritical()  and endCritical(), so the heap is consistent.  The only
problem can be that we are currently writing the arguments of  the  next
goal  above  the  local  stack  top  pointer.  To avoid problems we just
increment the top pointer to point above the furthest argument.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
helpInterrupt()
{ Putf("Options:\n");
  Putf("a:                 abort      b:                 break\n");
  Putf("c:                 continue   e:                 exit\n");
  Putf("g:                 goals      h (?):             help\n");
  Putf("t:                 trace\n");
}

void
interruptHandler()
{ extern int Output;
  int OldOut = Output;
  LocalFrame oldltop = lTop;
  Char c; 

  if ( status.initialised == FALSE )
  { fprintf(stderr, "Interrupt during startup. Cannot continue\n");
    Halt(1);
  }  

  Output = 1;
  lTop = (LocalFrame)addPointer(lTop, sizeof(struct localFrame) +
				      MAXARITY * sizeof(word));
again:
  Putf("\nAction (h for help) ? ");
  pl_flush();
  c = getSingleChar();

#if unix && O_SIG_AUTO_RESET
  signal(SIGINT, interruptHandler);	/* reinsert handler */
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
		pl_halt();
		break;
    case 'g':	Putf("goals\n");
		backTrace(environment_frame);
		goto again;
    case 'h':
    case '?':	helpInterrupt();
		goto again;
    case 't':	Putf("trace\n");
		pl_trace();
		break;
    default:	Putf("Unknown option (h for help)\n");
		goto again;
  }
  Output = OldOut;
  lTop = oldltop;
}

void
initTracer()
{ 
#if unix
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
pl_trace()
{ debugstatus.debugging = debugstatus.tracing = TRUE;
  debugstatus.skiplevel = VERY_DEEP;
  find.searching = FALSE;

  succeed;
}

word
pl_notrace()
{ debugstatus.tracing = FALSE;

  succeed;
}

word
pl_tracing()
{ return debugstatus.tracing;
}

word
pl_debug()
{ debugstatus.debugging = TRUE;
  debugstatus.skiplevel = VERY_DEEP;

  succeed;
}

word
pl_nodebug()
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
pl_skip_level(old, new)
Word old, new;
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
pl_spy(p)
Word p;
{ Procedure proc;

  if ((proc = findProcedure(p)) == (Procedure) NULL)
    fail;
  set(proc->definition, SPY_ME);

  return pl_debug();
}

word
pl_nospy(p)
Word p;
{ Procedure proc;

  if ((proc = findProcedure(p)) == (Procedure) NULL)
    fail;
  clear(proc->definition, SPY_ME);

  succeed;
}

word
pl_leash(old, new)
Word old, new;
{ TRY(unifyAtomic(old, consNum(debugstatus.leashing) ));

  if (!isInteger(*new) )
    fail;
  debugstatus.leashing = valNum(*new) & 0x1f;

  succeed;
}

word
pl_visible(old, new)
Word old, new;
{ TRY(unifyAtomic(old, consNum(debugstatus.visible) ));

  if (!isInteger(*new) )
    fail;
  debugstatus.visible = valNum(*new) & 0x1f;

  succeed;
}

word
pl_unknown(old, new)
Word old, new;
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
pl_prolog_current_frame(fr)
Word fr;
{ return unifyAtomic(fr, PrologRef(parentFrame(environment_frame)));
}

word
pl_prolog_frame_attribute(frame, what, value)
Word frame, what, value;
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
    fail;
  } else if (key == ATOM_top)
  { result = consNum(fr->parent == (LocalFrame) NULL ? 1 : 0);
  } else if (key == ATOM_context_module)
  { result = (word) contextModule(fr)->name;
  } else if (key == ATOM_goal)
  { int arity, n;
    Word arg;
    
    if (fr->procedure->definition->module != MODULE_user)
    { result = globalFunctor(FUNCTOR_module2);
      argTerm(result, 0) = (word) fr->procedure->definition->module->name;
      arg = argTermP(result, 1);
    } else
      arg = &result;

    if ((arity = fr->procedure->functor->arity) == 0)
    { *arg = (word) fr->procedure->functor->name;
    } else
    { *arg = globalFunctor(fr->procedure->functor);
      for(arg=argTermP(*arg, 0), n=0; n < arity; arg++, n++)
	pl_unify(arg, argFrameP(fr, n) );
    }
  } else
    return warning("prolog_frame_attribute/3: unknown key");

  return pl_unify(value, &result);
}
