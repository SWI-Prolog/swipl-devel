/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include "pl-incl.h"
#include "pl-ctype.h"

#define WFG_TRACE	0x01000
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
{ if ( fr )
  { assert(fr >= lBase && fr < lTop);

    return PL_unify_integer(t, (Word)fr - (Word)lBase);
  } else
    return PL_unify_atom(t, ATOM_none);
}


static void
PL_put_frame(term_t t, LocalFrame fr)
{ if ( fr )
  { assert(fr >= lBase && fr < lTop);

    PL_put_integer(t, (Word)fr - (Word)lBase);
  } else
    PL_put_atom(t, ATOM_none);
}


#if 0
static void
PL_put_choice(term_t t, Choice ch)
{ if ( ch )
  { assert(ch >= (Choice)lBase && ch < (Choice)lTop);

    PL_put_integer(t, (Word)ch - (Word)lBase);
  } else
    PL_put_atom(t, ATOM_none);
}
#endif


static int
PL_get_frame(term_t r, LocalFrame *fr)
{ long i;
  atom_t a;

  if ( PL_get_long(r, &i) )
  { LocalFrame f = ((LocalFrame)((Word)lBase + i));

    assert(f >= lBase && f < lTop);
    *fr = f;

    succeed;
  } else if ( PL_get_atom(r, &a) && a == ATOM_none )
  { *fr = NULL;

    succeed;
  }

  fail;
}


#ifdef O_DEBUGGER

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines the tracer and interrupt  handler  that  allows  the
user  to break the normal Prolog execution.  The tracer is written in C,
but before taking action it calls Prolog.   This  mechanism  allows  the
user to intercept and redefine the tracer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

					/* Frame <-> Prolog integer */
forwards LocalFrame	redoFrame(LocalFrame, Code *PC);
forwards void		helpTrace(void);
#ifdef O_INTERRUPT
forwards void		helpInterrupt(void);
#endif
forwards bool		hasAlternativesFrame(LocalFrame);
static void		alternatives(Choice);
static void		exceptionDetails(void);
forwards void		listGoal(LocalFrame frame);
forwards int		traceInterception(LocalFrame, Choice, int, Code);
static int		traceAction(char *cmd,
				    int port,
				    LocalFrame frame,
				    Choice bfr,
				    bool interactive);
forwards void		interruptHandler(int sig);
static void		writeFrameGoal(LocalFrame frame, Code PC,
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
canUnifyTermWithGoal(LocalFrame fr)
{ find_data *find = LD->trace.find;

  switch(find->type)
  { case TRACE_FIND_ANY:
      succeed;
    case TRACE_FIND_NAME:
      return find->goal.name == fr->predicate->functor->name;
    case TRACE_FIND_TERM:
    { if ( find->goal.term.functor == fr->predicate->functor->functor )
      { fid_t cid = PL_open_foreign_frame();
	term_t t = PL_new_term_ref();
	Word a, b;
	int arity = fr->predicate->functor->arity;
	int rval = TRUE;

	copyRecordToGlobal(t, find->goal.term.term PASS_LD);
	a = valTermRef(t);
	deRef(a);
	a = argTermP(*a, 0);
	b = argFrameP(fr, 0);
	while( arity-- > 0 )
	{ if ( !can_unify(a++, b++) )
	  { rval = FALSE;
	    break;
	  }
	}

	PL_discard_foreign_frame(cid);
	return rval;
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
    case BREAK_PORT:	 return " Break: ";
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

int
tracePort(LocalFrame frame, Choice bfr, int port, Code PC)
{ int action = ACTION_CONTINUE;
  Definition def = frame->predicate;
  LocalFrame fr;

  if ( !bfr )
    bfr = LD->choicepoints;

  if ( (true(frame, FR_NODEBUG) && !(SYSTEM_MODE)) || /* hidden */
       debugstatus.suspendTrace )		      /* called back */
    return ACTION_CONTINUE;

					/* trace/[1,2] */
  if ( true(def, TRACE_CALL|TRACE_REDO|TRACE_EXIT|TRACE_FAIL) )
  { int doit = FALSE;

    switch(port)
    { case CALL_PORT: doit = true(def, TRACE_CALL); break;
      case EXIT_PORT: doit = true(def, TRACE_EXIT); break;
      case FAIL_PORT: doit = true(def, TRACE_FAIL); break;
      case REDO_PORT: doit = true(def, TRACE_REDO); break;
    }

    if ( doit )
      writeFrameGoal(frame, PC, port|WFG_TRACE);
  }

  if ( port & BREAK_PORT )
    goto ok;				/* always do break-points */

  if ( !debugstatus.tracing &&
       (false(def, SPY_ME) || (port & CUT_PORT)) )
    return ACTION_CONTINUE;		/* not tracing and no spy-point */
  if ( debugstatus.skiplevel < levelFrame(frame) )
    return ACTION_CONTINUE;		/* skipped */
  if ( debugstatus.skiplevel == levelFrame(frame) &&
       (port == REDO_PORT || port == CUT_CALL_PORT || port == CUT_EXIT_PORT) )
    return ACTION_CONTINUE;		/* skipped */
  if ( false(def, TRACE_ME) )
    return ACTION_CONTINUE;		/* non-traced predicate */
  if ( (!(debugstatus.visible & port)) )
    return ACTION_CONTINUE;		/* wrong port */
  if ( port == REDO_PORT &&
       (true(def, HIDE_CHILDS) && !SYSTEM_MODE) )
    return ACTION_CONTINUE;		/* redo's in system predicates */
ok:

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
	  Sfputs("Action not yet implemented here\n", Sdout);
	  break;
      }
    }
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We are in searching mode; should we actually give this port?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( LD->trace.find &&  LD->trace.find->searching )
  { DEBUG(2, Sdprintf("Searching\n"));

    if ( (port & LD->trace.find->port) && canUnifyTermWithGoal(frame) )
    { LD->trace.find->searching = FALSE; /* Got you */
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

again:
  writeFrameGoal(frame, PC, port|WFG_TRACING);

  if (debugstatus.leashing & port)
  { char buf[LINESIZ];

    debugstatus.skiplevel = VERY_DEEP;
    debugstatus.tracing   = TRUE;

    Sfputs(" ? ", Sdout);
    Sflush(Sdout);
    if ( !trueFeature(TTY_CONTROL_FEATURE) )
    { buf[0] = EOS;
      if ( !readLine(Sdin, Sdout, buf) )
      { Sfputs("EOF: exit\n", Sdout);
	PL_halt(0);
      }
    } else
    { int c = getSingleChar(Sdin);

      if ( c == EOF )
      { Sfputs("EOF: exit\n", Sdout);
	PL_halt(0);
      }
      buf[0] = c;
      buf[1] = EOS;
      if ( isDigit(buf[0]) || buf[0] == '/' )
      { Sfputs(buf, Sdout);
	readLine(Sdin, Sdout, buf);
      }
    }
    action = traceAction(buf, port, frame, bfr, trueFeature(TTY_CONTROL_FEATURE));
    if ( action == ACTION_AGAIN )
      goto again;
  } else
    Sputc('\n', Sdout);

  return action;
}


static int
setupFind(char *buf)
{ char *s;
  int port = 0;

  for(s = buf; *s && isBlank(*s); s++)	/* Skip blanks */
    ;
  if ( *s == EOS )			/* No specification: repeat */
  { if ( !LD->trace.find || !LD->trace.find->port )
    { Sfputs("[No previous search]\n", Sdout);
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
      default:  Sfputs("[Illegal port specification]\n", Sdout);
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
      find = LD->trace.find = allocHeap(sizeof(find_data));

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
    { find->type = TRACE_FIND_TERM;
      find->goal.term.term = compileTermToHeap(t, 0);
    } else
    { Sfputs("[Illegal goal specification]\n", Sdout);
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
{ fid_t fid        = PL_open_foreign_frame();
  term_t av        = PL_new_term_ref();
  predicate_t pred = PL_predicate("$set_debugger_print_options", 1, "system");

  _PL_put_atomic(av, t);
  PL_call_predicate(NULL, PL_Q_NODEBUG, pred, av);

  PL_discard_foreign_frame(fid);
}


static int
traceAction(char *cmd, int port, LocalFrame frame, Choice bfr, bool interactive)
{ int num_arg;				/* numeric argument */
  char *s;

#define FeedBack(msg)	{ if (interactive) { if (cmd[1] != EOS) \
					       Sputc('\n', Sdout); \
					     else \
					       Sfputs(msg, Sdout); } }
#define Warn(msg)	{ if (interactive) \
			    Sfputs(msg, Sdout); \
			  else \
			    warning(msg); \
			}
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
		pl_abort(ABORT_NORMAL);
    case 'b':	FeedBack("break\n");
		pl_break();
		return ACTION_AGAIN;
    case '/': 	FeedBack("/");
    		Sflush(Suser_output);
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
    case 'c':	FeedBack("creep\n");
		clear(frame, FR_SKIPPED);
		return ACTION_CONTINUE;
    case '\04':
    case EOF:	FeedBack("EOF: ");
    case 'e':	FeedBack("exit\n");
		PL_halt(0);
    case 'f':	FeedBack("fail\n");
		return ACTION_FAIL;
    case 'i':	if (port & (CALL_PORT|REDO_PORT|FAIL_PORT))
		{ FeedBack("ignore\n");
		  return ACTION_IGNORE;
		} else
		  Warn("Can't ignore goal at this port\n");
		return ACTION_CONTINUE;
    case 'r':	if (port & (REDO_PORT|FAIL_PORT|EXIT_PORT|EXCEPTION_PORT))
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
    case 'd':   FeedBack("depth\n");
                setPrintOptions(consInt(num_arg));
		return ACTION_AGAIN;
    case 'w':   FeedBack("write\n");
                setPrintOptions(ATOM_write);
		return ACTION_AGAIN;
    case 'p':   FeedBack("print\n");
		setPrintOptions(ATOM_print);
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
		alternatives(bfr);
		return ACTION_AGAIN;
    case 'C':	debugstatus.showContext = 1 - debugstatus.showContext;
		if ( debugstatus.showContext == TRUE )
		{ FeedBack("Show context\n");
		} else
		{ FeedBack("No show context\n");
		}
		return ACTION_AGAIN;
    case 'm':	FeedBack("Exception details");
    	        if ( port & EXCEPTION_PORT )
		{ exceptionDetails();
		} else
		   Warn("No exception\n");
		return ACTION_AGAIN;
    case 'L':	FeedBack("Listing");
		listGoal(frame);
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
    case 'D':   GD->debug_level = num_arg;
		FeedBack("Debug level\n");
		return ACTION_AGAIN;
    default:	Warn("Unknown option (h for help)\n");
		return ACTION_AGAIN;
  }
}

static void
helpTrace(void)
{ Sfputs("Options:\n"
	 "+:                  spy        -:              no spy\n"
	 "/c|e|r|f|u|a goal:  find       .:              repeat find\n"
	 "a:                  abort      A:              alternatives\n"
	 "b:                  break      c (ret, space): creep\n"
	 "[depth] d:          depth      e:              exit\n"
	 "f:                  fail       [ndepth] g:     goals (backtrace)\n"
	 "h (?):              help       i:              ignore\n"
	 "l:                  leap       L:              listing\n"
	 "n:                  no debug   p:              print\n"
	 "r:                  retry      s:              skip\n"
	 "u:                  up         w:              write\n"
	 "m:		      exception details\n"
	 "C:                  toggle show context\n"
#if O_DEBUG
	 "[level] D:	      set system debug level\n"
#endif
	 "", Sdout);
}


static void
put_frame_goal(term_t goal, LocalFrame frame)
{ Definition def = frame->predicate;
  int argc = def->functor->arity;
  Word argv = argFrameP(frame, 0);

  PL_unify_functor(goal, def->functor->functor);
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
  if ( def->module != MODULE_user &&
       (false(def->module, SYSTEM) || SYSTEM_MODE))
  { term_t a = PL_new_term_ref();

    PL_put_atom(a, def->module->name);
    PL_cons_functor(goal, FUNCTOR_module2, a, goal);
  }
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
always mean influencing in computer science :-).

For the above reason, the code  below uses low-level manipulation rather
than normal unification, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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
  { BREAK_PORT,	    ATOM_break },
  { CUT_CALL_PORT,  ATOM_cut_call },
  { CUT_EXIT_PORT,  ATOM_cut_exit },
  { EXCEPTION_PORT, ATOM_exception },
  { 0,		    NULL_ATOM }
};


static void
writeFrameGoal(LocalFrame frame, Code PC, unsigned int flags)
{ fid_t cid = PL_open_foreign_frame();
  Definition def = frame->predicate;

  if ( !GD->bootsession && GD->initialised )
  { term_t fr   = PL_new_term_ref();
    term_t port = PL_new_term_ref();
    term_t pc   = PL_new_term_ref();
    const portname *pn = portnames;

    if ( true(def, FOREIGN) )
      PL_put_atom(pc, ATOM_foreign);
    else if ( PC && frame->clause )
      PL_put_integer(pc, PC-frame->clause->clause->codes);
    else
      PL_put_nil(pc);
    
    PL_put_frame(fr, frame);

    for(; pn->flags; pn++)
    { if ( flags & pn->flags )
      { PL_put_atom(port, pn->name);
	break;
      }
    }
    if ( flags & WFG_TRACE )
      PL_cons_functor(port, FUNCTOR_trace1, port);

    printMessage(ATOM_debug,
		 PL_FUNCTOR, FUNCTOR_frame3,
		   PL_TERM, fr,
		   PL_TERM, port,
		   PL_TERM, pc);
  } else
  { int debugSave = debugstatus.debugging;
    term_t goal    = PL_new_term_ref();
    term_t options = PL_new_term_ref();
    term_t tmp     = PL_new_term_ref();
    char msg[3];
    const char *pp = portPrompt(flags&PORT_MASK);

    put_frame_goal(goal, frame);
    debugstatus.debugging = FALSE;
    PL_put_atom(tmp, ATOM_debugger_print_options);
    if ( !pl_feature(tmp, options, 0) )
      PL_put_nil(options);
    PL_put_atom(tmp, ATOM_user_output);

    msg[0] = true(def, METAPRED) ? '^' : ' ';
    msg[1] = (flags&WFG_TRACE) ? 'T' : true(def, SPY_ME) ? '*' : ' ';
    msg[2] = EOS;

    Sfprintf(Sdout, "%s%s(%d) ", msg, pp, levelFrame(frame));
    if ( debugstatus.showContext )
      Sfprintf(Sdout, "[%s] ", stringAtom(contextModule(frame)->name));

    pl_write_term3(tmp, goal, options);
    if ( flags & (WFG_BACKTRACE|WFG_CHOICE) )
      Sfprintf(Sdout, "\n");

    debugstatus.debugging = debugSave;
  }

  PL_discard_foreign_frame(cid);
}

/*  Write those frames on the stack that have alternatives left.

 ** Tue May 10 23:23:11 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static void
alternatives(Choice ch)
{ for(; ch; ch = ch->parent)
  { if ( (false(ch->frame, FR_NODEBUG) || SYSTEM_MODE) )
      writeFrameGoal(ch->frame, NULL, WFG_CHOICE);
  }
}    


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
messageToString() is a  wrapper   around  $messages:message_to_string/2,
translating a message-term as used for exceptions into a C-string.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *
messageToString(term_t msg)
{ fid_t fid = PL_open_foreign_frame();
  term_t av = PL_new_term_refs(2);
  predicate_t pred = PL_predicate("message_to_string", 2, "$messages");
  char *s;

  PL_put_term(av+0, msg);
  PL_call_predicate(MODULE_system, PL_Q_NODEBUG, pred, av);
  PL_get_chars(av+1, &s, CVT_ALL|BUF_RING);
  PL_discard_foreign_frame(fid);

  return s;
}


static void
exceptionDetails()
{ term_t except = LD->exception.pending;
  fid_t cid = PL_open_foreign_frame();

  Sflush(Suser_output);			/* make sure to stay `in sync' */
  Sfputs("\n\n\tException term: ", Sdout);
  PL_write_term(Sdout, except, 1200, PL_WRT_QUOTED);
  Sfprintf(Sdout, "\n\t       Message: %s\n", messageToString(except));

  PL_discard_foreign_frame(cid);
}


static void
listGoal(LocalFrame frame)
{ fid_t cid = PL_open_foreign_frame();
  term_t goal = PL_new_term_ref();
  predicate_t pred = PL_predicate("$prolog_list_goal", 1, "system");
  IOSTREAM *old = Scurout;

  Scurout = Sdout;
  put_frame_goal(goal, frame);
  PL_call_predicate(MODULE_system, PL_Q_NODEBUG, pred, goal);
  Scurout = old;

  PL_discard_foreign_frame(cid);
}


void
backTrace(LocalFrame frame, int depth)
{ LocalFrame same_proc_frame = NULL;
  Definition def = NULL;
  int same_proc = 0;
  int alien = FALSE;
  Code PC = NULL;

  if ( frame == NULL )
     frame = environment_frame;

  for(; depth > 0 && frame;
        alien = (frame->parent == NULL),
        PC = frame->programPointer,
        frame = parentFrame(frame))
  { if ( alien )
      Sfputs("    <Alien goal>\n", Sdout);

    if ( frame->predicate == def )
    { if ( ++same_proc >= 10 )
      { if ( same_proc == 10 )
	  Sfputs("    ...\n    ...\n", Sdout);
	same_proc_frame = frame;  
	continue;
      }
    } else
    { if ( same_proc_frame != NULL )
      { if ( false(same_proc_frame, FR_NODEBUG) || SYSTEM_MODE )
        { writeFrameGoal(same_proc_frame, PC, WFG_BACKTRACE);
	  depth--;
	}
	same_proc_frame = NULL;
	same_proc = 0;
      }
      def = frame->predicate;
    }

    if (false(frame, FR_NODEBUG) || SYSTEM_MODE)
    { writeFrameGoal(frame, PC, WFG_BACKTRACE);
      depth--;
    }
  }
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
{ int rval = -1;			/* Default C-action */
  predicate_t proc;

  proc = _PL_predicate("prolog_trace_interception", 4, "user",
		       &GD->procedures.prolog_trace_interception4);
  if ( !proc->definition->definition.clauses )
    return rval;

  if ( !GD->bootsession && GD->debug_level == 0 )
  { fid_t cid = PL_open_foreign_frame();
    qid_t qid;
    term_t argv = PL_new_term_refs(4);
    term_t rarg = argv+3;
    atom_t portname = NULL_ATOM;
    functor_t portfunc = 0;
    int nodebug = FALSE;

    switch(port)
    { case CALL_PORT:	   portname = ATOM_call;         break;
      case REDO_PORT:	   portname = ATOM_redo;         break;
      case EXIT_PORT:	   portname = ATOM_exit;         break;
      case FAIL_PORT:	   portname = ATOM_fail;         break;
      case UNIFY_PORT:	   portname = ATOM_unify;	 break;
      case EXCEPTION_PORT: portname = ATOM_exception; 	 break;
      case BREAK_PORT:     portfunc = FUNCTOR_break1;	 break;
      case CUT_CALL_PORT:  portfunc = FUNCTOR_cut_call1; break;
      case CUT_EXIT_PORT:  portfunc = FUNCTOR_cut_exit1; break;
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

    PL_put_frame(argv+1, frame);
    PL_put_frame(argv+2, bfr->frame);	/* PL_put_choice() */
    PL_put_variable(rarg);

    qid = PL_open_query(MODULE_user, PL_Q_NODEBUG, proc, argv);
    if ( PL_next_solution(qid) )
    { atom_t a;

      if ( PL_get_atom(rarg, &a) )
      { if ( a == ATOM_continue )
	  rval = ACTION_CONTINUE;
	else if ( a == ATOM_nodebug )
	{ rval = ACTION_CONTINUE;
	  nodebug = TRUE;
	} else if ( a == ATOM_fail )
	  rval = ACTION_FAIL;
	else if ( a == ATOM_retry )
	  rval = ACTION_RETRY;
	else if ( a == ATOM_ignore )
	  rval = ACTION_IGNORE;
      } else if ( PL_is_functor(rarg, FUNCTOR_retry1) )
      { LocalFrame fr;
	term_t arg = PL_new_term_ref();

	if ( PL_get_arg(1, rarg, arg) && PL_get_frame(arg, &fr) )
	{ debugstatus.retryFrame = fr;
	  rval = ACTION_RETRY;
	} else
	  warning("prolog_trace_interception/3: bad argument to retry/1");
      }
    }
    PL_close_query(qid);
    PL_discard_foreign_frame(cid);

    if ( nodebug )
    { tracemode(FALSE, NULL);
      debugmode(FALSE, NULL);
    }
  }

  return rval;
}

#endif /*O_DEBUGGER*/

#ifndef offset
#define offset(s, f) ((int)(&((struct s *)NULL)->f))
#endif

static QueryFrame
findQuery(LocalFrame fr)
{ while(fr->parent)
    fr = fr->parent;

  return (QueryFrame)addPointer(fr, -offset(queryFrame, frame));
}


static bool
hasAlternativesFrame(LocalFrame frame)
{ QueryFrame qf;
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
	  case CHP_FOREIGN:
	    return TRUE;
	  case CHP_TOP:			/* no default to get warning */
	  case CHP_CATCH:
	  case CHP_NONE:
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


static LocalFrame
alternativeFrame(LocalFrame frame)
{ QueryFrame qf;
  LocalFrame fr = environment_frame;
  Choice ch = LD->choicepoints;

  for(;;)
  { for( ; ch; ch = ch->parent )
    { if ( (void *)ch < (void *)frame )
	return NULL;

      if ( ch->frame == frame )
      { for(ch = ch->parent; ch; ch = ch->parent )
	{ if ( ch->frame == frame )
	    continue;

	  switch( ch->type )
	  { case CHP_CLAUSE:
	    case CHP_FOREIGN:
	    case CHP_JUMP:
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
{
#ifdef O_INTERRUPT
  PL_signal(SIGINT, interruptHandler);
#endif

  debugstatus.tracing      =
  debugstatus.debugging    = FALSE;
  debugstatus.suspendTrace = FALSE;
  debugstatus.skiplevel    = 0;
  debugstatus.retryFrame   = NULL;

  setFeatureMask(TAILRECURSION_FEATURE);
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
{ Sfputs("Options:\n"
        "a:                 abort      b:                 break\n"
        "c:                 continue   e:                 exit\n"
#ifdef O_DEBUGGER
        "g:                 goals      t:                 trace\n"
#endif
        "h (?):             help\n", Sdout);
}

static void
interruptHandler(int sig)
{ int c; 

  if ( !GD->initialised )
  { Sfprintf(Serror, "Interrupt during startup. Cannot continue\n");
    PL_halt(1);
  }  

again:
  Sfputs("\nAction (h for help) ? ", Sdout);
  Sflush(Sdout);
  ResetTty();                           /* clear pending input -- atoenne -- */
  c = getSingleChar(Sdin);

  switch(c)
  { case 'a':	Sfputs("abort\n", Sdout);
		unblockSignal(sig);
    		pl_abort(ABORT_NORMAL);
		break;
    case 'b':	Sfputs("break\n", Sdout);
		unblockSignal(sig);	/* into pl_break() itself */
		pl_break();
		goto again;		
    case 'c':	Sfputs("continue\n", Sdout);
		break;
    case 04:
    case EOF:	Sfputs("EOF: ", Sdout);
    case 'e':	Sfputs("exit\n", Sdout);
		PL_halt(0);
		break;
#ifdef O_DEBUGGER
    case 'g':	Sfputs("goals\n", Sdout);
		backTrace(environment_frame, 5);
		goto again;
#endif /*O_DEBUGGER*/
    case 'h':
    case '?':	helpInterrupt();
		goto again;
#ifdef O_DEBUGGER
    case 't':	Sfputs("trace\n", Sdout);
		pl_trace();
		break;
#endif /*O_DEBUGGER*/
    default:	Sfputs("Unknown option (h for help)\n", Sdout);
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
initTracer(void)
{ debugstatus.visible      = 
  debugstatus.leashing     = CALL_PORT|FAIL_PORT|REDO_PORT|EXIT_PORT|
			     BREAK_PORT|EXCEPTION_PORT;
  debugstatus.showContext  = FALSE;

  resetTracer();
}

		/********************************
		*       PROLOG PREDICATES       *
		*********************************/

#if O_DEBUGGER

int
tracemode(int doit, int *old)
{ if ( doit )
  { debugmode(TRUE, NULL);
    doit = TRUE;
  }

  if ( old )
    *old = debugstatus.tracing;

  if ( debugstatus.tracing != doit )
  { if ( doit )
    { debugstatus.skiplevel = VERY_DEEP;
      if ( LD->trace.find )
	LD->trace.find->searching = FALSE;
    }
    debugstatus.tracing = doit;
    printMessage(ATOM_silent,
		 PL_FUNCTOR_CHARS, "trace_mode", 1,
		   PL_ATOM, doit ? ATOM_on : ATOM_off);
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
    { debugstatus.skiplevel = VERY_DEEP;
      clearFeatureMask(TAILRECURSION_FEATURE);
    } else
    { setFeatureMask(TAILRECURSION_FEATURE);
    }
    debugstatus.debugging = doit;
    printMessage(ATOM_silent,
		 PL_FUNCTOR_CHARS, "debug_mode", 1,
		   PL_ATOM, doit ? ATOM_on : ATOM_off);
  }

  succeed;
}

#else /*O_DEBUGGER*/

int
tracemode(int doit, int *old)
{ succeed;
}

int
debugmode(int doit, int *old)
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
{ return debugstatus.tracing;
}

word
pl_skip_level(term_t old, term_t new)
{ atom_t a;
  long sl;

  if ( debugstatus.skiplevel == VERY_DEEP )
  { TRY(PL_unify_atom(old, ATOM_very_deep));
  } else
  { TRY(PL_unify_integer(old, debugstatus.skiplevel));
  }
      
  if ( PL_get_long(new, &sl) )
  { debugstatus.skiplevel = (unsigned long) sl;
    succeed;
  }
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
    debugmode(TRUE, NULL);
    succeed;
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
{ return setInteger(&GD->debug_level, "$debuglevel", old, new);
}


word
pl_prolog_current_frame(term_t frame)
{ LocalFrame fr = environment_frame;

  if ( fr->predicate->definition.function == pl_prolog_current_frame )
    fr = parentFrame(fr);		/* thats me! */

  return PL_unify_frame(frame, fr);
}


word
pl_prolog_frame_attribute(term_t frame, term_t what,
			  term_t value)
{ LocalFrame fr;
  atom_t key;
  int arity;
  term_t result = PL_new_term_ref();

  if ( !PL_get_frame(frame, &fr) ||
       !PL_get_name_arity(what, &key, &arity) )
  { ierr:
    return warning("prolog_frame_attribute/3: instantiation fault");
  }

  set(fr, FR_WATCHED);			/* explicit call to do this? */

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
    checkData(argFrameP(fr, argn-1));
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
  { LocalFrame alt;

    if ( (alt = alternativeFrame(fr)) )
      PL_put_frame(result, alt);
    else
      fail;
  } else if (key == ATOM_parent)
  { LocalFrame parent;

    if ( fr->parent )
      clearUninitialisedVarsFrame(fr->parent, fr->programPointer);

    if ( (parent = parentFrame(fr)) )
      PL_put_frame(result, parent);
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

      PL_unify_functor(arg, fr->predicate->functor->functor);
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


#if O_DEBUGGER

		 /*******************************
		 *	  PROLOG EVENT HOOK	*
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
      case PLEV_FRAMEFINISHED:
      { LocalFrame fr = va_arg(args, LocalFrame);
	term_t ref = PL_new_term_ref();

	PL_put_frame(ref, fr);
	PL_unify_term(arg, PL_FUNCTOR, FUNCTOR_frame_finished1,
		           PL_TERM, ref);
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

#endif /*O_DEBUGGER*/
