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


#include <pentium.h>
#include <stdio.h>

prof_record prof_data[MAXPROF];
prof_record *prof_current;
ticks prof_ticks;

static ticks overhead;

ticks
pentium_clock()
{ register unsigned long iax;
  unsigned long idx;

  __asm__ __volatile__ ("rdtsc"
			: "=a" (iax), "=d" (idx)
		       );

  return (ticks)iax + ((ticks)idx<<32);
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

    printf("%7d %10.3f %10.3f %20.3f %s\n",
	   pr->calls, f, av, tot, pr->name);
  }
  memset(pr, 0, sizeof(*pr));
}

#define PROF_NOOP

void
prof_report()
{ int i;
  ticks totalfast = 0, totalav = 0;
  prof_record *pr = &prof_data[0];

  for(i=0; i<100; i++)
    PROF(0, PROF_NOOP);

  printf("------------------------------------------------------\n");
  printf("Pentium rdtsc timing in micro-seconds (%.0f MHz CPU)\n", CPU_MHz());
  printf("------------------------------------------------------\n");
  printf("%7s %10s %10s %20s %s\n",
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
    { totalfast += pr->fastest - overhead;
      totalav   += pr->ticks/pr->calls - overhead;
    }

    prof_report_field(pr);
  }

  printf("------------------------------------------------------\n");

  { double f  = (double)totalfast/CPU_MHz();
    double av = (double)totalav/CPU_MHz();

    printf("%-7s %10.3f %10.3f Totals\n", "", f, av);
  }
}


void
prof_reset()
{ prof_record *pr = &prof_data[0];
  int i;

  for(i=0; i<MAXPROF; i++, pr++)
    memset(pr, 0, sizeof(*pr));
}
