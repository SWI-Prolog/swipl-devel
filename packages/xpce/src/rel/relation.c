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
createRelation(Relation r, Any from, Any to)
{ return send(r, NAME_forwards, from, to, EAV);
}

static status
ignoreReleation(Relation r, Any from, Any to)
{ succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_fromAobject_toAobject[] =
        { "from=object", "to=object" };

/* Instance Variables */

#define var_relation NULL
/*
vardecl var_relation[] =
{ 
};
*/

/* Send Methods */

static senddecl send_relation[] =
{ SM(NAME_backwards, 2, T_fromAobject_toAobject, ignoreReleation,
     NAME_constraint, "Called to update after a change of `to'"),
  SM(NAME_create, 2, T_fromAobject_toAobject, createRelation,
     NAME_constraint, "Called to initiate the relation"),
  SM(NAME_forwards, 2, T_fromAobject_toAobject, ignoreReleation,
     NAME_constraint, "Called to update after a change of `from'")
};

/* Get Methods */

#define get_relation NULL
/*
static getdecl get_relation[] =
{ 
};
*/

/* Resources */

#define rc_relation NULL
/*
static classvardecl rc_relation[] =
{ 
};
*/

/* Class Declaration */

ClassDecl(relation_decls,
          var_relation, send_relation, get_relation, rc_relation,
          0, NULL,
          "$Rev$");

status
makeClassRelation(Class class)
{ return declareClass(class, &relation_decls);
}
