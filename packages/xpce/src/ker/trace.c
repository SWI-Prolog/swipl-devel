/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/trace.h>
#include <h/interface.h>
#include <h/graphics.h>

void
resetDebugger(void)
{ CurrentGoal = NULL;
  ExecuteMode = MODE_SYSTEM;
  SkipMode    = FALSE;
  GoalDepth   = 0;
}


void
initDebugger(void)
{ resetDebugger();

  VmiSend = globalObject(NAME_vmiSend, ClassVmi, NAME_send, 0);
  VmiGet  = globalObject(NAME_vmiGet,  ClassVmi, NAME_get,  0);
  VmiNew  = globalObject(NAME_vmiNew,  ClassVmi, NAME_new,  0);
  VmiFree = globalObject(NAME_vmiFree, ClassVmi, NAME_free, 0);

  systemProgramObject(VmiSend, OFF);
  systemProgramObject(VmiGet,  OFF);
  systemProgramObject(VmiNew,  OFF);
  systemProgramObject(VmiFree, OFF);
}


status
parentGoal(ProgramObject obj, Any rec, Name sel) /* goal is on stack */
{ Goal g;

  for(g = CurrentGoal; g; g = g->parent)
    if ( g->object == obj &&
	 g->receiver == rec &&
	 g->selector == sel )
      succeed;

  fail;
}


#ifndef O_RUNTIME

static int
tracingGoal(Goal g, Name port)
{ ProgramObject obj = g->object;
  ulong flag = nameToTraceFlag(port);

  if ( onDFlag(obj, D_TRACE_INHERIT) )
  { Class class = classOfObject(obj);

    if ( isName(g->selector) &&
	 strName(g->selector)[0] == syntax.word_separator &&
	 TraceMode != TRACE_ALWAYS )
      fail;

    while(notNil(class) && onDFlag(class, D_TRACE_INHERIT))
      class = class->super_class;
    
    return notNil(class) &&
           onDFlag(class, flag) &&
	   evalTraceConditionProgramObject((ProgramObject) class, g);
  }
  
  return onDFlag(obj, flag) &&
         evalTraceConditionProgramObject(obj, g);
}


static int
breakingGoal(Goal g, Name port)
{ ProgramObject obj = g->object;
  ulong flag = nameToBreakFlag(port);

  if ( onDFlag(obj, D_BREAK_INHERIT) )
  { Class class = classOfObject(obj);

    if ( isName(g->selector) &&
	 strName(g->selector)[0] == syntax.word_separator &&
	 TraceMode != TRACE_ALWAYS )
      fail;

    while(notNil(class) && onDFlag(class, D_BREAK_INHERIT))
      class = class->super_class;
    
    return notNil(class) &&
           onDFlag(class, flag) &&
	   evalBreakConditionProgramObject((ProgramObject) class, g);
  }
  
  return onDFlag(obj, flag) &&
         evalBreakConditionProgramObject(obj, g);
}


static int
levelGoal(Goal g)
{ int i;
  
  for(i=0; g; g=g->parent)
    i++;

  return i;
}


static void
actionHelp(void)
{ writef("\nXPCE Tracer options:\n");
  writef(" a\t\tabort\t\tAbort to host-language toplevel\n");
  writef(" b\t\tbreak\t\tStart interactive toplevel\n");
  writef(" c[nua]\t\tcontinue\tContinue to next break-point in trace-mode\n");
  writef("\t\t\t\t[never/user/always]\n");
  writef(" e[iwef] [id]\terror kind\tSet kind to [ignored/warning/error/fatal]\n");
  writef(" g[ah] [depth]\tgoals\t\tPrint stack [always/host]\n");
  writef(" q\t\tquit\t\tQuit XPCE\n");
  writef(" s [level]\tskip\t\tSkip to the exit/fail of goal at [level]\n");
  writef(" t\t\ttrace\t\tSwitch to host language tracer\n");
  writef(" n\t\tnever trace\tDisable tracer\n");
  writef(" ? (h)\t\thelp\t\tPrint this text\n\n");
}


static void
actionGoal(Goal g, Name port, Any rval)
{ int do_break, do_trace;

  if ( SkipMode )
  { if ( offGFlag(g, G_SKIP) )
      return;
    else
      SkipMode = FALSE;
  }

again:
  Trace(TRACE_NEVER,
	do_break = (breakingGoal(g, port) || onGFlag(g, G_EXCEPTION));
	do_trace = do_break || tracingGoal(g, port));

  if ( do_trace )
  { Trace(TRACE_NEVER,
	  writef("PCE: %2d ", toInt(levelGoal(g)));
    
	  writeGoal(g, port);
	  if ( rval )
	    writef(" --> %O", rval));
     

    if ( do_break )
    { char buf[LINESIZE];
      char *s;

      Trace(TRACE_NEVER,
	    writef(" ? ");
	    s = Cgetline(buf, sizeof(buf)));

      if ( s )
      { int argc = 0;
	char *argv[100];
	char *q;
	Int numarg = DEFAULT;

	for(q = s; *q; )
	{ while(*q && islayout(*q))
	    q++;
	  if ( *q == EOS )
	    break;
	  argv[argc++] = q;
	  while(*q && !islayout(*q))
	    q++;
	  if ( *q != EOS )
	    *q++ = EOS;
	}
	
	if ( argc >= 2 && isdigit(argv[1][0]) )
	  numarg = toInt(atoi(argv[1]));

	if ( argc == 0 )
	  return;

	switch(argv[0][0])
	{ case 'g':
	    Trace(TRACE_NEVER,
		  if ( argv[0][1] == 'h' )
		    hostAction(HOST_BACKTRACE,
			       isDefault(numarg) ? 5 : valInt(numarg));
		  else
		    traceBackPce(numarg, argv[0][1] == 'a' ? NAME_always
				 		           : NAME_user));
	    goto again;
	  case 'b':
	    Trace(TRACE_NEVER,
		  if ( !hostAction(HOST_BREAK) )
		    send(HostObject(), NAME_break, 0));
	    goto again;
	  case 'a':
	    Trace(TRACE_NEVER,
		  if ( !hostAction(HOST_ABORT) )
		    send(HostObject(), NAME_abort, 0));
	    goto again;			/* should not happen */
	  case 'q':
	    Trace(TRACE_NEVER, hostAction(HOST_HALT));
	    exit(1);
	  case 'n':
	    tracePce(PCE, NAME_never);
	    return;
	  case 'e':
	    { Error e;

	      if ( argc == 2 )
	      { if ( !(e = getConvertError(ClassError, CtoName(argv[1]))) )
		{ writef("No such error: %s\n", CtoName(argv[1]));
		  goto again;
		}
	      } else
	      { if ( !(e = getConvertError(ClassError, PCE->last_error)) )
		{ writef("No current error\n");
		  goto again;
		}
	      }

	      if ( !e )
	      { writef("No current error\n");
		goto again;
	      }

	      switch(argv[0][1])
	      { case 'i':
		  assign(e, kind, NAME_ignored);
		  break;
		case 'e':
		  assign(e, kind, NAME_error);
		  break;
		case 'f':
		  assign(e, kind, NAME_fatal);
		  break;
		default:
		case 'w':
		  assign(e, kind, NAME_warning);
		  break;
	      }

	      writef("Switched error \"%s\" to ->kind \"%s\"\n",
		     e->id, e->kind);

	      goto again;
	    }
	  case 's':
	    if ( port == NAME_enter )
	    { int level = levelGoal(g);
	      int skiplevel = isDefault(numarg) ? level : valInt(numarg);

	      if ( skiplevel < 0 )	/* negative: relative to top */
		skiplevel += level;
	      if ( skiplevel > level )	/* boundaries */
		skiplevel = level;
	      else if ( skiplevel < 0 )
		skiplevel = 0;

	      while(level > skiplevel && g)
	      { g = g->parent;
		level--;
	      }

	      setGFlag(g, G_SKIP);
	      SkipMode = TRUE;
	      return;
	    }
	    return;
	  case 't':
	    if ( hostAction(HOST_TRACE) )
	    { tracePce(PCE, NAME_never);
	      return;
	    } else
	    { writef("Trace not supported by host-language\n");
	      goto again;
	    }
	  case 'c':
	    switch(argv[0][1])
	    { case 'n':
		tracePce(PCE, NAME_never);
		break;
	      case 'u':
		tracePce(PCE, NAME_user);
		break;
	      case 'a':
		tracePce(PCE, NAME_always);
		break;
	    }
	    return;
	  case EOS:
	    return;
	  case '?':
	  case 'h':
	    actionHelp();
	    goto again;
	  default:
	    writef("Unknown option. (? for help)\n");
	    goto again;
	}
      } else
      { Trace(TRACE_NEVER, hostAction(HOST_HALT));
	exit(1);
      }
    } else
    { writef("\n");
    }
  }
}


void
doTraceEnter(Goal g)
{ if ( offDFlag(g->object, D_TRACE_ENTER|D_BREAK_ENTER) &&
       offGFlag(g, G_EXCEPTION) &&
       TraceMode == TRACE_USER &&
       ExecuteMode == MODE_SYSTEM )
    return;

  actionGoal(g, NAME_enter, NULL);
}


void
doTraceReturn(Goal g, status rval)
{ if ( offDFlag(g->object, D_TRACE_EXIT|D_BREAK_EXIT) &&
       offGFlag(g, G_EXCEPTION) &&
       TraceMode == TRACE_USER &&
       ExecuteMode == MODE_SYSTEM )
    return;

  actionGoal(g, rval ? NAME_exit : NAME_fail, NULL);
}


void
doTraceAnswer(Goal g, Any rval)
{ if ( offDFlag(g->object, D_TRACE_EXIT|D_BREAK_EXIT) &&
       offGFlag(g, G_EXCEPTION) &&
       TraceMode == TRACE_USER &&
       ExecuteMode == MODE_SYSTEM )
    return;

  actionGoal(g, rval ? NAME_exit : NAME_fail, rval);
}


static void
writeVmGoal(Goal g, Name vm)
{ int i;

  writef("%s(%O, %O", vm, g->receiver, g->selector);
  for(i=0; i<g->argc; i++)
    writef(", %O", g->argv[i]);
  writef(")");
}


static void
writeNewGoal(Goal g)
{ int i;
  Class class = g->receiver;

  writef("new(%s", instanceOfObject(class, ClassClass) ? class->name
						       : (Name) class);
  if ( g->argc > 0 )
  { writef("(");
    for(i=0; i<g->argc; i++)
      writef(i == 0 ? "%O" : ", %O", g->argv[i]);
    writef(")");
  }

  writef(")");
}


static void
writeFreeGoal(Goal g)
{ writef("free(%O)", g->receiver);
}


void
writeGoal(Goal g, Name port)
{ Class class = classOfObject(g->object);

  if ( notNil(port) )
    writef("%s: ", port);

  if ( class->trace_function )
    (*class->trace_function)(g->object, g, port);
  else if ( g->object == VmiSend )
    writeVmGoal(g, NAME_send);
  else if ( g->object == VmiGet )
    writeVmGoal(g, NAME_get);
  else if ( g->object == VmiNew )
    writeNewGoal(g);
  else if ( g->object == VmiFree )
    writeFreeGoal(g);
}


void
traceBackPce(Int depth, Name mode)
{ int n = isDefault(depth) ? 5 : valInt(depth);
  Goal g = CurrentGoal;
  int level = levelGoal(g);

  if ( !g )
    writef("\t<No goal>\n");

  for(; n > 0 && g; g = g->parent, level--)
  { if ( onGFlag(g, G_SYSTEM) && mode == NAME_user )
      continue;

    writef("\t[%2d] ", toInt(level));
    writeGoal(g, NIL);
    writef("\n");
    n--;
  }
}

#endif /*O_RUNTIME*/

int
getModeGoal(Any obj)
{ Goal g = CurrentGoal;

  if ( obj )
  { for(; g && g->object != obj; g = g->parent)
      ;
  }

  if ( g && offGFlag(g, G_SYSTEM) )
    return MODE_USER;

  return MODE_SYSTEM;
}
