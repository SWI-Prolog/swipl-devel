/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/trace.h>

static status	breakConditionProgramObject P((ProgramObject, Code));
static status	traceConditionProgramObject(ProgramObject obj, Code c);

static HashTable TraceConditionTable;
static HashTable BreakConditionTable;

status
initialiseProgramObject(Any obj)
{ ((ProgramObject)obj)->dflags = (ulong) ZERO;
  setDFlag(obj, D_TRACE_INHERIT|D_BREAK_INHERIT);

  if ( getModeGoal(VmiNew) == MODE_SYSTEM )
    setDFlag(obj, D_SYSTEM);

  succeed;
}


status
initialiseNewSlotProgramObject(ProgramObject obj, Variable var)
{ if ( var->name == NAME_dflags )
  { obj->dflags = (ulong) ZERO;
    setDFlag(obj, D_TRACE_INHERIT|D_BREAK_INHERIT);
  }

  succeed;
}


ulong
nameToTraceFlag(Name name)
{ if ( name == NAME_enter )
    return D_TRACE_ENTER;
  if ( name == NAME_exit )
    return D_TRACE_EXIT;
  if ( name == NAME_fail )
    return D_TRACE_FAIL;
/*if ( name == NAME_full || isDefault(what) )*/
    return D_TRACE;
}


ulong
nameToBreakFlag(Name name)
{ if ( name == NAME_enter )
    return D_BREAK_ENTER;
  if ( name == NAME_exit )
    return D_BREAK_EXIT;
  if ( name == NAME_fail )
    return D_BREAK_FAIL;
/*if ( name == NAME_full || isDefault(what) )*/
    return D_BREAK;
}


static status
traceProgramObject(ProgramObject obj, Bool val, Name what, Code cond)
{ if ( isDefault(val) )
  { setDFlag(obj, D_TRACE_INHERIT);
    clearDFlag(obj, D_TRACE);
  } else
  { ulong flag = nameToTraceFlag(what);

    clearDFlag(obj, D_TRACE_INHERIT);
    if ( val == ON )
    { setDFlag(obj, flag);
      if ( PCE->trace == NAME_never )
	tracePce(PCE, NAME_user);
    } else
      clearDFlag(obj, flag);

    if ( notDefault(cond) )
      traceConditionProgramObject(obj, cond);
  }

  succeed;
}


static Bool
getTraceProgramObject(ProgramObject obj, Name what)
{ ulong flag = nameToTraceFlag(what);

  answer(onDFlag(obj, flag) ? ON : OFF);
}


static status
breakProgramObject(ProgramObject obj, Bool val, Name what, Code cond)
{ if ( isDefault(val) )
  { setDFlag(obj, D_BREAK_INHERIT);
    clearDFlag(obj, D_BREAK);
  } else
  { ulong flag = nameToBreakFlag(what);

    clearDFlag(obj, D_BREAK_INHERIT);
    if ( val == ON )
    { setDFlag(obj, flag);
      if ( PCE->trace == NAME_never )
	tracePce(PCE, NAME_user);
    } else
      clearDFlag(obj, flag);

    if ( notDefault(cond) )
      breakConditionProgramObject(obj, cond);
  }

  succeed;
}


static Bool
getBreakProgramObject(ProgramObject obj, Name what)
{ ulong flag = nameToBreakFlag(what);

  answer(onDFlag(obj, flag) ? ON : OFF);
}


status
systemProgramObject(ProgramObject obj, Bool val)
{ if ( val == ON )
    setDFlag(obj, D_SYSTEM);
  else
    clearDFlag(obj, D_SYSTEM);

  succeed;
}


static Bool
getSystemProgramObject(ProgramObject obj)
{ answer(onDFlag(obj, D_SYSTEM) ? ON : OFF);
}


		/********************************
		*          CONDITIONS		*
		********************************/

static status
traceConditionProgramObject(ProgramObject obj, Code c)
{ if ( isNil(c) )
  { clearDFlag(obj, D_TRACE_CONDITION);
    deleteHashTable(TraceConditionTable, obj);
  } else
  { setDFlag(obj, D_TRACE_CONDITION);
    appendHashTable(TraceConditionTable, obj, c);
  }
  
  succeed;
}


static status
breakConditionProgramObject(ProgramObject obj, Code c)
{ if ( isNil(c) )
  { clearDFlag(obj, D_BREAK_CONDITION);
    deleteHashTable(BreakConditionTable, obj);
  } else
  { setDFlag(obj, D_BREAK_CONDITION);
    appendHashTable(BreakConditionTable, obj, c);
  }
  
  succeed;
}


static Code
getTraceConditionProgramObject(ProgramObject obj)
{ if ( onDFlag(obj, D_TRACE_CONDITION) )
    answer(getMemberHashTable(TraceConditionTable, obj));

  fail;
}


static Code
getBreakConditionProgramObject(ProgramObject obj)
{ if ( onDFlag(obj, D_BREAK_CONDITION) )
    answer(getMemberHashTable(BreakConditionTable, obj));

  fail;
}


status
evalTraceConditionProgramObject(ProgramObject obj, Goal g)
{ Code c;

  if ( (c = getTraceConditionProgramObject(obj)) )
  { status rval;

    withLocalVars({ assignVar(SELECTOR, g->selector, NAME_local);
		    rval = forwardReceiverCodev(c, g->receiver,
						g->argc, g->argv);
		  });

    return rval;
  }

  succeed;
}


status
evalBreakConditionProgramObject(ProgramObject obj, Goal g)
{ Code c;

  if ( (c = getBreakConditionProgramObject(obj)) )
  { status rval;

    withLocalVars({ assignVar(SELECTOR, g->selector, NAME_local);
		    rval = forwardReceiverCodev(c, g->receiver,
						g->argc, g->argv);
		  });

    return rval;
  }

  succeed;
}


#if O_NO_TAGGED_LVALUE
void
setDFlagProgramObject(obj, mask)
Any obj;
ulong mask;
{ ProgramObject po = obj;

  po->dflags |= mask;
}


void
clearDFlagProgramObject(obj, mask)
Any obj;
ulong mask;
{ ProgramObject po = obj;

  po->dflags &= ~mask;
}
#endif


status
makeClassProgramObject(Class class)
{ localClass(class, NAME_dflags, NAME_debugging, "int", NAME_both,
	     "Debugging-flags of the program_object");

  sourceClass(class, makeClassProgramObject, __FILE__, "$Revision$");
  termClass(class, "program_object", 0);

  sendMethod(class, NAME_initialise, DEFAULT, 0,
	     "Create program_object",
	     initialiseProgramObject);
  sendMethod(class, NAME_trace, NAME_debugging, 3,
	     "value=[bool]", "ports=[{full,enter,exit,fail}]",
	     "condition=[code]",
	     "set/clear trace-point on object",
	     traceProgramObject);
  sendMethod(class, NAME_break, NAME_debugging, 3,
	     "value=[bool]", "ports=[{full,enter,exit,fail}]",
	     "condition=[code]",
	     "set/clear break-point on object",
	     breakProgramObject);
  sendMethod(class, NAME_traceCondition, NAME_debugging, 1, "code*",
	     "Condition associated with this trace-point",
	     traceConditionProgramObject);
  sendMethod(class, NAME_breakCondition, NAME_debugging, 1, "code*",
	     "Condition associated with this break-point",
	     breakConditionProgramObject);
  sendMethod(class, NAME_system, NAME_meta, 1, "bool",
	     "System defined object?",
	     systemProgramObject);
  sendMethod(class, NAME_initialiseNewSlot, NAME_compatibility, 1, "variable",
	     "Initialise <-dflags",
	     initialiseNewSlotProgramObject);

  getMethod(class, NAME_trace, NAME_debugging, "bool", 1,
	    "port=[{enter,exit,fail}]",
	    "Current setting of trace-point",
	    getTraceProgramObject);
  getMethod(class, NAME_break, NAME_debugging, "bool", 1,
	    "port=[{enter,exit,fail}]",
	    "Current setting of break-point",
	    getBreakProgramObject);
  getMethod(class, NAME_system, NAME_meta, "bool", 0,
	    "System defined object?",
	    getSystemProgramObject);
  getMethod(class, NAME_traceCondition, NAME_debugging, "code", 0,
	    "Associated trace-condition",
	    getTraceConditionProgramObject);
  getMethod(class, NAME_breakCondition, NAME_debugging, "code", 0,
	    "Associated break-condition",
	    getBreakConditionProgramObject);

  TraceConditionTable = globalObject(NAME_traceConditions, ClassHashTable, 0);
  BreakConditionTable = globalObject(NAME_breakConditions, ClassHashTable, 0);

  succeed;
}

