/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

static status	forwardsIdentity(Identity id, Any from, Any to);

static status
initialiseIdentity(Identity id, Name from, Name to)
{ assign(id, from, from);
  assign(id, to, isDefault(to) ? from : to);

  succeed;
}


static status
createIdentity(Identity id, Any from, Any to)
{ if (isNil(from) || isNil(to))
    succeed;
  return forwardsIdentity(id, from, to);
}


static status
forwardsIdentity(Identity id, Any from, Any to)
{ Any value;
  status rval;

  TRY(value = get(from, id->from, 0));
  rval = send(to, id->to, value, 0);
  if ( isObject(value) )
    doneObject(value);
  
  return rval;
}


static status
backwardsIdentity(Identity id, Any from, Any to)
{ Any value;
  status rval;

  TRY(value = get(to, id->to, 0));
  rval = send(from, id->from, value, 0);
  if ( isObject(value) )
    doneObject(value);
  
  return rval;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_create[] =
        { "from=object*", "to=object*" };
static char *T_fromAobject_toAobject[] =
        { "from=object", "to=object" };
static char *T_initialise[] =
        { "selector1=name", "selector2=[name]" };

/* Instance Variables */

static vardecl var_identity[] =
{ IV(NAME_from, "selector1=name", IV_BOTH,
     NAME_selector, "Attribute at the `From' side"),
  IV(NAME_to, "selector2=name", IV_BOTH,
     NAME_selector, "Attribute at the `To' side")
};

/* Send Methods */

static senddecl send_identity[] =
{ SM(NAME_backwards, 2, T_fromAobject_toAobject, backwardsIdentity,
     DEFAULT, "Update after `from' object has changed"),
  SM(NAME_create, 2, T_create, createIdentity,
     DEFAULT, "Update after instantiation"),
  SM(NAME_forwards, 2, T_fromAobject_toAobject, forwardsIdentity,
     DEFAULT, "Update after `to' object has changed"),
  SM(NAME_initialise, 2, T_initialise, initialiseIdentity,
     DEFAULT, "Create from attribute names")
};

/* Get Methods */

#define get_identity NULL
/*
static getdecl get_identity[] =
{ 
};
*/

/* Resources */

#define rc_identity NULL
/*
static classvardecl rc_identity[] =
{ 
};
*/

/* Class Declaration */

static Name identity_termnames[] = { NAME_from, NAME_to };

ClassDecl(identity_decls,
          var_identity, send_identity, get_identity, rc_identity,
          2, identity_termnames,
          "$Rev$");

status
makeClassIdentity(Class class)
{ return declareClass(class, &identity_decls);
}

