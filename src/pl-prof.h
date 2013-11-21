/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef PL_PROF_H_INCLUDED
#define PL_PROF_H_INCLUDED

typedef enum
{ PROF_INACTIVE = 0,		/* Profiler is inactive */
  PROF_CPU,			/* Profile CPU time */
  PROF_WALL			/* Profile wall time */
} prof_status;

COMMON(void)		stopItimer(void);
COMMON(int)		activateProfiler(prof_status status ARG_LD);
COMMON(bool)		resetProfiler(void);
COMMON(struct call_node*) profCall(Definition def ARG_LD);
COMMON(void)		profResumeParent(struct call_node *node ARG_LD);
COMMON(void)		profExit(struct call_node *node ARG_LD);
COMMON(void)		profRedo(struct call_node *node ARG_LD);
COMMON(void)		profSetHandle(struct call_node *node, void *handle);

#endif /*PL_PROF_H_INCLUDED*/
