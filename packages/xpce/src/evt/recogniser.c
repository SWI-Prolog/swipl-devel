/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>


status
initialiseRecogniser(Recogniser r)
{ assign(r, active, ON);

  succeed;
}


static status
eventRecogniser(Recogniser r, EventObj ev)
{ fail;
}


status
makeClassRecogniser(Class class)
{ sourceClass(class, makeClassRecogniser, __FILE__, "$Revision$");

  localClass(class, NAME_active, NAME_status, "bool", NAME_both,
	     "Ignore events when @off");

  sendMethod(class, NAME_initialise, DEFAULT, 0,
	     "Create new recogniser",
	     initialiseRecogniser);
  sendMethod(class, NAME_event, NAME_event, 1, "event",
	     "Process an event (fails)",
	     eventRecogniser);

  succeed;
}

