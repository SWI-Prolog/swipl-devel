/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This program   was written  to  determine the  memory  model   of your
machine.  Normally this will be called by configure.

Compile this file using:

	% cc -o m-model m-model.c
	% ./m-model
	Memory layout:

		Text at 0x2290
		Global variable at 0x40d0
		Local variable at 0xeffff938
		malloc() at 0x61a0
		C-Stack grows Downward

	No special declarations needed in "md.h"

	%
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <stdio.h>

#define K * 1024
#define MAX_DECL 100

int	global_var;

main(argc, argv)
int argc;
char *argv[];
{ char buf[10];
  unsigned long gva = (unsigned long) &global_var;
  unsigned long mad = (unsigned long) malloc(2000);
  char *decl[MAX_DECL];
  int ndecl = 0;
  

#ifdef VERBOSE
  printf("Memory layout:\n\n");
  printf("\tGlobal variable at 0x%x\n", gva);
  printf("\tLocal variable at 0x%x\n", buf);
  printf("\tmalloc() at 0x%x\n", mad);
#endif
	 
  if ( (gva & 0xfC000000L) )
  { if ( (gva & 0xfC000000L) == (mad & 0xfC000000L) )
    { static char msg[100];

      sprintf(msg, "POINTER_OFFSET=0x%08xL", gva & 0xfC000000L );
      decl[ndecl++] = msg;
    } else
    { fprintf(stderr, "Static and malloced data far apart\n");
      exit(1);
    }
  }
  
  printf("mmodel=ok\n");	/* Some sh don't like eval '' */
  if ( ndecl > 0 )
  { int n;

    for(n=0; n<ndecl; n++)
      printf("%s\n", decl[n]);
  }

  exit(0);
}
