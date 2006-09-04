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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/


#ifndef PROF_H_INCLUDED
#define PROF_H_INCLUDED

typedef long long ticks;
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

#define MAXPROF 100
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
	{ prof_record *pr = prof_current; \
	  ticks t = pentium_clock() - prof_ticks; \
	  pr->calls++; \
	  if ( !pr->fastest || pr->fastest > t ) pr->fastest = t; \
	  pr->ticks += t; \
	  prof_current = NULL; \
	}
	
#endif /*PROF_H_INCLUDED*/
