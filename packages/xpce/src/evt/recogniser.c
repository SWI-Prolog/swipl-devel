/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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

