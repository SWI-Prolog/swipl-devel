/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <stdio.h>
#include <SWI-Prolog.h>

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

  1) Make the file `pl.o' containing all of SWI-Prolog using
     `make pl.o' in the machine-directory and install it.
  2) Make a copy of this file.  In this copy:
  3) Put the right #include directives
  4) Fill the table below.  
  5) Link pl.o with this file and the .o files defining your application.

If there are prolog parts involved:

  6) Start the image; load the prolog and create a state using
     save_program/2.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

extern foreign_t prolog_pce_init();

static foreign_t
pl_pce_init()
{ return prolog_pce_init();
}


PL_extension PL_extensions [] =
{
/*{ "name",	arity,  function,	PL_FA_<flags> },*/
  { "$pce_init", 0,	pl_pce_init,	PL_FA_TRANSPARENT },

  { NULL,	0, 	NULL,		0 }	/* terminating line */
};
