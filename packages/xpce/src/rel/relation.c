/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

static status
createRelation(Relation r, Any from, Any to)
{ return send(r, NAME_forwards, from, to, 0);
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
