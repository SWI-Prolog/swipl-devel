/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/trace.h>

#ifndef O_RUNTIME
static status	breakConditionProgramObject(ProgramObject, Code);
static status	traceConditionProgramObject(ProgramObject obj, Code c);

static HashTable TraceConditionTable;
static HashTable BreakConditionTable;
#endif /*O_RUNTIME*/

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


#ifndef O_RUNTIME
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
      if ( PCE->trace == NAME_never || PCE->trace == NAME_error )
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
      if ( PCE->trace == NAME_never || PCE->trace == NAME_error )
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

#endif /*O_RUNTIME*/

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

#ifndef O_RUNTIME

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

#endif /*O_RUNTIME*/

#ifndef TAGGED_LVALUE
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

#endif /*TAGGED_LVALUE*/

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

#ifndef O_RUNTIME
static char *T_debug[] =
        { "value=[bool]", "ports=[{full,enter,exit,fail}]", "condition=[code]" };
#endif

/* Instance Variables */

static vardecl var_programObject[] =
{ IV(NAME_dflags, "int", IV_BOTH,
     NAME_debugging, "Debugging-flags of the program_object")
};

/* Send Methods */

static senddecl send_programObject[] =
{ SM(NAME_initialise, 0, NULL, initialiseProgramObject,
     DEFAULT, "Create program_object"),
  SM(NAME_initialiseNewSlot, 1, "variable", initialiseNewSlotProgramObject,
     NAME_compatibility, "Initialise <-dflags"),
#ifndef O_RUNTIME
  SM(NAME_break, 3, T_debug, breakProgramObject,
     NAME_debugging, "set/clear break-point on object"),
  SM(NAME_breakCondition, 1, "code*", breakConditionProgramObject,
     NAME_debugging, "Condition associated with this break-point"),
  SM(NAME_trace, 3, T_debug, traceProgramObject,
     NAME_debugging, "set/clear trace-point on object"),
  SM(NAME_traceCondition, 1, "code*", traceConditionProgramObject,
     NAME_debugging, "Condition associated with this trace-point"),
#endif /*O_RUNTIME*/
  SM(NAME_system, 1, "bool", systemProgramObject,
     NAME_meta, "System defined object?")
};

/* Get Methods */

static getdecl get_programObject[] =
{
#ifndef O_RUNTIME
  GM(NAME_break, 1, "bool", "port=[{enter,exit,fail}]", getBreakProgramObject,
     NAME_debugging, "Current setting of break-point"),
  GM(NAME_breakCondition, 0, "code", NULL, getBreakConditionProgramObject,
     NAME_debugging, "Associated break-condition"),
  GM(NAME_trace, 1, "bool", "port=[{enter,exit,fail}]", getTraceProgramObject,
     NAME_debugging, "Current setting of trace-point"),
  GM(NAME_traceCondition, 0, "code", NULL, getTraceConditionProgramObject,
     NAME_debugging, "Associated trace-condition"),
#endif /*O_RUNTIME*/
  GM(NAME_system, 0, "bool", NULL, getSystemProgramObject,
     NAME_meta, "System defined object?")
};

/* Resources */

#define rc_programObject NULL
/*
static resourcedecl rc_programObject[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(programObject_decls,
          var_programObject, send_programObject,
	  get_programObject, rc_programObject,
          0, NULL,
          "$Rev$");


status
makeClassProgramObject(Class class)
{ declareClass(class, &programObject_decls);

#ifndef O_RUNTIME
  TraceConditionTable = globalObject(NAME_traceConditions, ClassHashTable, 0);
  BreakConditionTable = globalObject(NAME_breakConditions, ClassHashTable, 0);
#endif

  succeed;
}

