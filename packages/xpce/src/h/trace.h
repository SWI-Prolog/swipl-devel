/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#ifndef PCE_TRACE_H
#define PCE_TRACE_H

GLOBAL PceGoal	CurrentGoal;		/* current active goal */
GLOBAL int	ServiceMode;		/* Running a service  call-back */
GLOBAL int	GoalDepth;		/* recursion level */
GLOBAL int	MaxGoalDepth;		/* maximum recursion level */

#define DebuggingProgramObject(o, flags) \
	(PCEdebugging && (ServiceMode == PCE_EXEC_USER) && onDFlag((o), (flags)))

#define ServiceMode(mode, code) \
  { int _smode = ServiceMode; \
    ServiceMode = mode; \
    code; \
    ServiceMode = _smode; \
  }

#define pushGoal(g) { (g)->parent   = CurrentGoal; \
		      CurrentGoal = g; \
		    }
#define popGoal(g)  { CurrentGoal = (g)->parent; \
		    }


extern void	writeGoal(PceGoal g);
extern int	isProperGoal(PceGoal g);
extern void	pceBackTrace(PceGoal g, int depth);

#endif /*PCE_TRACE_H*/
