/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2003-2011, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
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
