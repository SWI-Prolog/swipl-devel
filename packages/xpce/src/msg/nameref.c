/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
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

