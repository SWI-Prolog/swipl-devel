/*  $Id$

    Copyright (c) 1991 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Skeleton for extensions
*/

#include <stdio.h>
#include "pl-itf.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C-extension   can  either  be   loaded  through the   foreign language
interface  implemented   by   load_foreign/[1,2,5]  or  through static
linking.  The latter  mechanism  is  to be  used if  the first is  not
ported to your machine/operating system.  Static linking is also to be
preferred  for large   applications   as  it  puts  the   text  in the
text-section of the (unix) process rather  than te  data section.  The
text-section  of  a  process  is  normally write-protected  (providing
better protection) and shared between multiple copies of the program.

To create a statically linked executable, perform the following steps:

  1) Make a copy of this file.  In this copy:
  2) Put the right #include directives
  3) Fill the table below.  
  4) Link pl.o with this file and the .o files defining your application.

If there are prolog parts involved:

  5) Start the image; load the prolog and create a state using
     save_program/2.

     OR

     if save_program/2 is not supported, create a new boot file using

     pl -o <my_state> -b boot/init.pl -c <your-pl-files>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

PL_extension PL_extensions [] =
{
/*{ "name",	arity,  function,	PL_FA_<flags> },*/

  { NULL,	0, 	NULL,		0 }	/* terminating line */
};


int
main(int argc, char **argv, char **env)
{ if ( !PL_initialise(argc, argv, env) )
    PL_halt(1);

  PL_halt(PL_toplevel() ? 0 : 1);

  return 0;
}


