/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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


#ifndef PROF_H_INCLUDED
#define PROF_H_INCLUDED
#ifdef O_PROF_PENTIUM

typedef int64_t ticks;
typedef struct
{ ticks ticks;				/* time spent */
  ticks fastest;			/* fastest call */
  int	calls;				/* # calls */
  char *name;				/* id name */
} prof_record;

extern ticks		pentium_clock(void);
extern void		prof_report(void);
extern void		reset_profile();
extern prof_record	prof_data[];
extern prof_record     *prof_current;
extern ticks		prof_ticks;

#define MAXPROF I_HIGHEST+2000		/* see createForeignSupervisor() */
#define CPU_SPEED 200.0

#define PROF(Id, code) \
	{ prof_record *pr = &prof_data[Id]; \
	  ticks t, old = pentium_clock(); \
	  code; \
	  t = pentium_clock() - old; \
	  pr->calls++; \
	  pr->name = #code; \
	  if ( !pr->fastest || pr->fastest > t ) pr->fastest = t; \
	  pr->ticks += t; \
	}

#define START_PROF(Id, Name) \
	prof_current = &prof_data[Id]; \
	prof_current->name = Name; \
	prof_ticks = pentium_clock();
#define END_PROF() \
	if ( prof_current ) \
	{ ticks t = pentium_clock() - prof_ticks; \
	  if ( t >= 0 ) \
	  { prof_record *pr = prof_current; \
	    pr->calls++; \
	    if ( !pr->fastest || pr->fastest > t ) pr->fastest = t; \
	    pr->ticks += t; \
	    prof_current = NULL; \
	  } else \
	  { Sdprintf("%s: T=%lld (t0=%lld)\n", prof_current->name, prof_ticks); \
	  } \
	}

/* non-VMI profile identifiers */

#define DEPART_CONTINUE 	(I_HIGHEST+1)
#define P_GC 			(I_HIGHEST+2)
+#define P_SHALLOW_BACKTRACK    (I_HIGHEST+3)
+#define P_DEEP_BACKTRACK       (I_HIGHEST+4)

#else /*O_PROF_PENTIUM*/

#define START_PROF(id, name)
#define END_PROF()

#endif /*O_PROF_PENTIUM*/

#endif /*PROF_H_INCLUDED*/
