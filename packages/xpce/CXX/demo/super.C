/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <pce/Pce.h>
#include <pce/Class.h>

PceVariable *z_point_z;

static PceStatus
initialiseZPoint(PceReceiver p, PceArg x, PceArg y, PceArg z)
{ p.sendSuper("initialise", x, y);
  if ( z != TheDefault )
    p.store(z_point_z, z);

  return SUCCEED;
}


PceStatus
makeClassZPoint(PceClass cl)
{ z_point_z =
    cl.defvar("z", "coordinate", "Z-coordinate of 3-D point",
	      "int", "both", 0);
  
  cl.defsendmethod("initialise", "oms", "Create from X, Y, and Z",
		   initialiseZPoint, "[int]", "[int]", "[int]");

  return SUCCEED;
}

PceClass ClassZPoint("z_point", "point", "Point in 3-D space",
		     makeClassZPoint);
  
PceStatus
pceInitApplication(int argc, char *argv[])
{ PceObject z("z_point", 3, 4, 5);

  ThePce.send("write_ln", z.get("x"), z.get("y"), z.get("z"));

  return SUCCEED;
}
