/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#ifndef PCE_TRACE_H
#define PCE_TRACE_H

GLOBAL PceGoal	CurrentGoal;		/* current active goal */
GLOBAL int	ServiceMode;		/* Running a service  call-back */
GLOBAL int	GoalDepth;		/* recursion level */
GLOBAL int	MaxGoalDepth;		/* maximum recursion level */

#define NO_MAX_GOAL_DEPTH INT_MAX 	/* Setting for MaxGoalDepth if unlimited */

#define DebuggingProgramObject(o, flags) \
	(PCEdebugging && (ServiceMode == PCE_EXEC_USER) && onDFlag((o), (flags)))

#define ServiceMode(mode, code) \
  { int _smode = ServiceMode; \
    ServiceMode = mode; \
    { code; } \
    ServiceMode = _smode; \
  }

extern void	writeGoal(PceGoal g);
extern int	isProperGoal(PceGoal g);
extern void	pceBackTrace(PceGoal g, int depth);

#endif /*PCE_TRACE_H*/
