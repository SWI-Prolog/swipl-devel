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

typedef struct assoc *Assoc;

NewClass(assoc)
  ABSTRACT_CODE
  Name	reference;			/* reference of the object */
  Any	object;				/* Object to reference */
End;


static status
initialiseAssoc(Assoc a, Name name, Any object)
{ initialiseCode((Code) a);

  assign(a, reference, name);
  assign(a, object,    object);

  succeed;
}


static status
ExecuteAssoc(Assoc a)
{ return nameReferenceObject(a->object, a->reference);
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "reference=name", "object=object|function" };

/* Instance Variables */

static vardecl var_assoc[] =
{ IV(NAME_reference, "name", IV_BOTH,
     NAME_reference, "Reference to give to the object"),
  IV(NAME_object, "object|function", IV_BOTH,
     NAME_reference, "Object to assign reference")
};

/* Send Methods */

static senddecl send_assoc[] =
{ SM(NAME_Execute, 0, NULL, ExecuteAssoc,
     DEFAULT, "Assign the reference"),
  SM(NAME_initialise, 2, T_initialise, initialiseAssoc,
     DEFAULT, "Create from reference and object")
};

/* Get Methods */

#define get_assoc NULL
/*
static getdecl get_assoc[] =
{ 
};
*/

/* Resources */

#define rc_assoc NULL
/*
static classvardecl rc_assoc[] =
{ 
};
*/

/* Class Declaration */

static Name assoc_termnames[] = { NAME_reference, NAME_object };

ClassDecl(assoc_decls,
          var_assoc, send_assoc, get_assoc, rc_assoc,
          2, assoc_termnames,
          "$Rev$");


status
makeClassAssoc(Class class)
{ return declareClass(class, &assoc_decls);
}

