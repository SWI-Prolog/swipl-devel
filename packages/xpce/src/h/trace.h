/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#ifndef PCE_TRACE_H
#define PCE_TRACE_H

#define makeGFlag(n)		(1L << ((n) - 1))
#define setGFlag(g, mask)	(((Goal)(g))->gflags |= (mask))
#define clearGFlag(g, mask)	(((Goal)(g))->gflags &= ~(mask))
#define onGFlag(g, mask)	(((Goal)(g))->gflags & (mask))
#define offGFlag(g, mask)	(!onGFlag(g, mask))

#define G_SYSTEM	makeGFlag(1)	/* System/User initiated goal */
#define G_EXCEPTION	makeGFlag(2)	/* Exception during execution */
#define G_SKIP		makeGFlag(3)	/* Skipping this goal */

#define TRACE_NEVER  0
#define TRACE_ERROR  0			/* same! */
#define TRACE_USER   1
#define TRACE_ALWAYS 2

#define MODE_USER	0
#define MODE_SYSTEM	G_SYSTEM	/* So we can use it as initial */

#define NoTraceMode (PCE->trap_errors == ON ? NAME_error : NAME_never)

GLOBAL Goal	CurrentGoal;		/* current active goal */
GLOBAL int	TraceMode;		/* Current trace mode */
GLOBAL int	ExecuteMode;		/* MODE_USER/MODE_SYSTEM */
GLOBAL int	ServiceMode;		/* Running a service  call-back */
GLOBAL int	SkipMode;		/* We're skipping */
GLOBAL Any	VmiSend;		/* VMI representing object */
GLOBAL Any	VmiGet;			/* VMI representing object */
GLOBAL Any	VmiNew;			/* VMI representing object */
GLOBAL Any	VmiFree;		/* VMI representing object */
GLOBAL int	GoalDepth;		/* recursion level */
GLOBAL int	MaxGoalDepth;		/* maximum recursion level */

#define Trace(how, code) \
  { int _tms = TraceMode; \
    TraceMode = how; \
    code; \
    TraceMode = _tms; \
  }


#define Mode(mode, code) \
  { int _xmode = ExecuteMode; \
    ExecuteMode = mode; \
    code; \
    ExecuteMode = _xmode; \
  }


#define ServiceMode(mode, code) \
  { int _smode = ServiceMode; \
    ServiceMode = mode; \
    code; \
    ServiceMode = _smode; \
  }


typedef struct _goal
{ Goal		parent;			/* Parent goal */
  Any		object;			/* Principal object of the goal */
  ulong		gflags;			/* Goal flags */
  Any		receiver;		/* Related receiving object */
  Name		selector;		/* Related name */
  int		argc;			/* Argument-count */
  const Any* 	argv;			/* Argument-vector */
} goal;


#define pushGoal(g, obj, rec, sel, ac, av) \
  { g->object   = obj; \
    g->parent	= CurrentGoal; \
    g->gflags   = ExecuteMode; \
    g->receiver = rec; \
    g->selector = sel; \
    g->argc     = ac; \
    g->argv     = av; \
    CurrentGoal = g; \
    if ( GoalDepth++ > MaxGoalDepth ) \
      errorPce(obj, NAME_stackOverflow, toInt(MaxGoalDepth)); \
  }


#define popGoal() \
  { if ( CurrentGoal ) \
    { CurrentGoal = CurrentGoal->parent; \
      GoalDepth--; \
    } \
  }

#define outGoal(val) { rval = val; goto out; }
#define failGoal     outGoal(FAIL)


#ifndef O_RUNTIME

#define traceEnter(g) \
  { if ( TraceMode ) \
      doTraceEnter(g); \
  }

#define traceReturn(g, rval) \
  { if ( TraceMode ) \
      doTraceReturn(g, rval); \
  }

#define traceAnswer(g, rval) \
  { if ( TraceMode ) \
      doTraceAnswer(g, rval); \
  }

#else /*O_RUNTIME*/

#define traceEnter(g)
#define traceReturn(g, rval)
#define traceAnswer(g, rval)
#define setTraceFunctionClass(class, f)

#endif /*O_RUNTIME*/

#endif /*PCE_TRACE_H*/
