/*  $Id$

    Copyright (c) 1991 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Determine machines memory-model
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This program   was written  to  determine the  memory  model   of your
machine.  It may be used when writing a new md-machine.h file.

Compile this file using:

	% cc -o m-model m-model.c
	% ./m-model
	Memory layout:

		Global variable at 0x41f0
		Local variable at 0xeffff94c
		malloc() at 0x6298
		C-Stack grows Downward

	Required declarations in "md.h":

	%
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <stdio.h>

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
  int stack_up = (sub() > buf);

  printf("Memory layout:\n\n");
  printf("\tGlobal variable at 0x%x\n", gva);
  printf("\tLocal variable at 0x%x\n", buf);
  printf("\tmalloc() at 0x%x\n", malloc(10));
  printf("\tC-Stack grows %sward\n", stack_up ? "Up" : "Down");
	 
  printf("\nRequired declarations in \"md.h\":\n\n");

  if      ( (gva & 0xf0000000L) == 0x40000000L )
    printf("#define O_DATA_AT_0X4	1\n");
  else if ( (gva & 0xf0000000L) == 0x20000000L )
    printf("#define O_DATA_AT_0X2	1\n");
  else if ( (gva & 0xf0000000L) == 0x10000000L )
    printf("#define O_DATA_AT_0X1	1\n");
  else if ( (gva & 0xf0000000L) )
    printf("PROBLEM: Memory model not supported; see \"pl-incl.h\"\n");

  if ( stack_up )
    printf("#define O_C_STACK_GROWS_UP	1\n");

  exit(0);
}
