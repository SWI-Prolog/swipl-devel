/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <pce/Pce.h>
#include <pce/String.h>
#include <stdio.h>

PceStatus
pceInitApplication(int argc, char *argv[])
{ printf("%s\n", PceObject("string", "Edit person").pp());


  return SUCCEED;
}
