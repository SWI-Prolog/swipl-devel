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


static status
initialiseBool(Bool b)
{ return errorPce(classOfObject(b), NAME_cannotCreateInstances);
}


static status
unlinkBool(Bool b)
{ fail;					/* should not happen! */
}


static Bool
getConvertBool(Class class, Any obj)
{ answer(toBool(obj));
}


static Bool
getNegateBool(Bool b)
{ answer(b == ON ? OFF : ON);
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */


/* Instance Variables */

#define var_bool NULL
/*
static vardecl var_bool[] =
{ 
};
*/

/* Send Methods */

static senddecl send_bool[] =
{ SM(NAME_initialise, 0, NULL, initialiseBool,
     DEFAULT, "Create bool (cannot be created)"),
  SM(NAME_unlink, 0, NULL, unlinkBool,
     DEFAULT, "Destroy boolean (cannot be done)")
};

/* Get Methods */

static getdecl get_bool[] =
{ GM(NAME_convert, 1, "bool", "any", getConvertBool,
     DEFAULT, "Converts true, false and integer"),
  GM(NAME_negate, 0, "bool", NULL, getNegateBool,
     NAME_calculate, "Maps @on <-> @off")
};

/* Resources */

#define rc_bool NULL
/*
static classvardecl rc_bool[] =
{ 
};
*/

/* Class Declaration */

static Name bool_termnames[] = { NAME_self };

ClassDecl(bool_decls,
          var_bool, send_bool, get_bool, rc_bool,
          1, bool_termnames,
          "$Rev$");


status
makeClassBool(Class class)
{ declareClass(class, &bool_decls);

  saveStyleClass(class, NAME_external);
  cloneStyleClass(class, NAME_none);

  ON->class = OFF->class = class;
  newAssoc(NAME_on,  ON);
  newAssoc(NAME_off, OFF);

  ON->name = OFF->name = NIL;
  ON->summary = OFF->summary = NIL;
  assign(ON, name, NAME_on);
  assign(OFF, name, NAME_off);
  assign(ON, summary, staticCtoString("Boolean true"));
  assign(OFF, summary, staticCtoString("Boolean false"));

  succeed;
}

