/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@uva.nl
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

#include <pentium.h>
#include <stdio.h>

prof_record prof_data[MAXPROF];
prof_record *prof_current;
ticks prof_ticks;

static ticks overhead;

/* See http://www.technovelty.org/code/c/reading-rdtsc.html */

ticks
pentium_clock()
{ unsigned int iax;
  unsigned int idx;

  __asm__ __volatile__ ("rdtsc"
			: "=a" (iax), "=d" (idx)
		       );

  return (ticks)iax | ((ticks)idx<<32);
}


static double
CPU_MHz()
{ static double mhz = 1400.00;
  static int done = 0;

  if ( !done )
  { FILE *fp = fopen("/proc/cpuinfo", "r");
    char buf[256];

    if ( fp )
    { while(fgets(buf, sizeof(buf), fp))
      { if ( sscanf(buf, "cpu MHz : %lf", &mhz) == 1 )
	{ /*printf("%f MHz CPU\n", mhz);*/
	  break;
	}
      }

      fclose(fp);
    }

    done++;
  }

  return mhz;
}


static int
prof_compare(const void *p1, const void *p2)
{ const prof_record *pr1 = p1;
  const prof_record *pr2 = p2;
  ticks t1 = pr1->ticks - pr1->calls*overhead;
  ticks t2 = pr2->ticks - pr2->calls*overhead;

  return t1  > t2 ? -1 :
         t1 == t2 ?  0 :
		     1;
}


static void
prof_report_field(prof_record *pr)
{ if ( pr->name && pr->calls > 0 )
  { double f   = (double)(pr->fastest-overhead)/CPU_MHz();
    double av  = (double)(pr->ticks/pr->calls-overhead)/CPU_MHz();
    double tot = av*(double)pr->calls;

    printf("%9d %10.3f %10.3f %20.3f %s\n",
	   pr->calls, f, av, tot, pr->name);
  }
  memset(pr, 0, sizeof(*pr));
}

#define PROF_NOOP

void
prof_report()
{ int i;
  ticks totalticks = 0, totalfast = 0, totalav = 0;
  long long totalcalls = 0;
  prof_record *pr = &prof_data[0];

  for(i=0; i<100; i++)
    PROF(0, PROF_NOOP);

  printf("------------------------------------------------------\n");
  printf("Pentium rdtsc timing in micro-seconds (%.0f MHz CPU)\n", CPU_MHz());
  printf("------------------------------------------------------\n");
  printf("%9s %10s %10s %20s %s\n",
	 "calls", "fastest", "av", "total", "statement");
  printf("------------------------------------------------------\n");
  overhead = 0;
  prof_report_field(pr);
  overhead = pr->fastest;
  pr++;
  printf("------------------------------------------------------\n");

  qsort(pr, MAXPROF-1, sizeof(*pr), prof_compare);
  for(i=1; i<MAXPROF; i++, pr++)
  { if ( pr->name && pr->calls > 0 )
    { totalfast  += pr->fastest - overhead;
      totalav    += pr->ticks/pr->calls - overhead;
      totalticks += pr->ticks - pr->calls*overhead;
      totalcalls += pr->calls;
    }

    prof_report_field(pr);
  }

  printf("------------------------------------------------------\n");

  { double tot = (double)totalticks/CPU_MHz();

    printf("%9lld %10s %10s %20.3f %s\n",
	   totalcalls, "", "", tot, "Totals");
  }
}


void
prof_reset()
{ prof_record *pr = &prof_data[0];
  int i;

  END_PROF();

  for(i=0; i<MAXPROF; i++, pr++)
    memset(pr, 0, sizeof(*pr));
}
