/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <pce/Pce.h>
#include <pce/Box.h>
#include <pce/Point.h>

PceStatus
drawBoxes(PceArg dev, PceArg x, PceArg y, PceArg w, PceArg h, PceArg n)
{ while(n-- > 0)
  { dev.send("display", PceBox(w, h), PcePoint(x, y));
    x += w;
  }
  
  return SUCCEED;
}

PceStatus
pceInitApplication(int argc, char *argv[])
{ PceObject p("picture");
  
  drawBoxes(p, 10, 10, 20, 50, 20);
  p.send("open");

  return SUCCEED;
}
