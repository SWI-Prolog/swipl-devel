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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static const vardecl var_recogniser[] =
{ IV(NAME_active, "bool", IV_BOTH,
     NAME_status, "Ignore events when @off")
};

/* Send Methods */

static const senddecl send_recogniser[] =
{ SM(NAME_initialise, 0, NULL, initialiseRecogniser,
     DEFAULT, "Create new recogniser"),
  SM(NAME_event, 1, "event", eventRecogniser,
     NAME_event, "Process an event (fails)")
};

/* Get Methods */

static const getdecl get_recogniser[] =
{ 
};

/* Resources */

static const resourcedecl rc_recogniser[] =
{ 
};

/* Class Declaration */

ClassDecl(recogniser_decls,
          var_recogniser, send_recogniser, get_recogniser, rc_recogniser,
          ARGC_INHERIT, NULL,
          "$Rev$");


status
makeClassRecogniser(Class class)
{ return declareClass(class, &recogniser_decls);
}

