/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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

static vardecl var_recogniser[] =
{ IV(NAME_active, "bool", IV_BOTH,
     NAME_status, "Ignore events when @off")
};

/* Send Methods */

static senddecl send_recogniser[] =
{ SM(NAME_initialise, 0, NULL, initialiseRecogniser,
     DEFAULT, "Create new recogniser"),
  SM(NAME_event, 1, "event", eventRecogniser,
     NAME_event, "Process an event (fails)")
};

/* Get Methods */

#define get_recogniser NULL
/*
static getdecl get_recogniser[] =
{ 
};
*/

/* Resources */

#define rc_recogniser NULL
/*
static classvardecl rc_recogniser[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(recogniser_decls,
          var_recogniser, send_recogniser, get_recogniser, rc_recogniser,
          ARGC_INHERIT, NULL,
          "$Rev$");


status
makeClassRecogniser(Class class)
{ return declareClass(class, &recogniser_decls);
}

