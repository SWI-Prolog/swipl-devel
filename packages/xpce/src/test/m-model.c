/*  $Id$

    Copyright (c) 1991 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Determine machines memory-model
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

char *
sub()
{ char buf[10];

  return buf;
}

main(argc, argv)
int argc;
char *argv[];
{ char buf[10];
  unsigned long gva = (unsigned long) &global_var;
  unsigned long gta = (unsigned long) sub;
  unsigned long mad = (unsigned long) malloc(2000);
  int stack_up = (sub() > buf);
  char *decl[MAX_DECL];
  int ndecl = 0;
  

#ifdef VERBOSE
  printf("Memory layout:\n\n");
  printf("\tText at 0x%x\n", sub);
  printf("\tGlobal variable at 0x%x\n", gva);
  printf("\tLocal variable at 0x%x\n", buf);
  printf("\tmalloc() at 0x%x\n", mad);
  printf("\tC-Stack grows %sward\n", stack_up ? "Up" : "Down");
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
  
  if ( stack_up )
    decl[ndecl++] = "STACK_DIRECTION=1";
  else
    decl[ndecl++] = "STACK_DIRECTION=-1";

  if ( ndecl > 0 )
  { int n;

    for(n=0; n<ndecl; n++)
      printf("%s\n", decl[n]);
  }

  exit(0);
}
