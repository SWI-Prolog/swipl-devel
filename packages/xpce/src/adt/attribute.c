/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/trace.h>

static status
initialiseAttribute(Attribute att, Any name, Any value)
{ initialiseProgramObject(att);

  assign(att, name, name);
  assign(att, value, value);

  succeed;
}


static Attribute
getConvertAttribute(Class class, Any name)
{ answer(answerObject(ClassAttribute, name, NIL, EAV));
}


static status
sendAttribute(Attribute att, Any rec, Any value)
{ assign(att, value, value);
  
  succeed;
}


static Any
getAttribute(Attribute att, Any rec)
{ return att->value;
}


		/********************************
		*            TRACING		*
		********************************/

static Type
getArgumentTypeAttribute(Attribute att, Int n)
{ if ( isDefault(n) || n == ONE )
    answer(TypeAny);

  fail;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_send[] =
        { "context=object", "value=any" };
static char *T_initialise[] =
        { "name=any", "value=any" };

/* Instance Variables */

static vardecl var_attribute[] =
{ IV(NAME_name, "any", IV_BOTH,
     NAME_storage, "Name of the attribute"),
  IV(NAME_value, "any", IV_BOTH,
     NAME_storage, "Value of the attribute")
};

/* Send Methods */

static senddecl send_attribute[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseAttribute,
     DEFAULT, "Create attribute from name and value"),
  SM(NAME_send, 2, T_send, sendAttribute,
     NAME_execute, "Invoke (write) object-attribute")
};

/* Get Methods */

static getdecl get_attribute[] =
{ GM(NAME_convert, 1, "attribute", "any", getConvertAttribute,
     DEFAULT, "Converts name to attribute(name, @nil)"),
  GM(NAME_get, 1, "any", "object", getAttribute,
     NAME_execute, "Invoke (read) object-attribute"),
  GM(NAME_argumentType, 1, "type", "index=[int]", getArgumentTypeAttribute,
     NAME_meta, "Type of n-th1 argument")
};

/* Resources */

#define rc_attribute NULL
/*
static classvardecl rc_attribute[] =
{ 
};
*/

/* Class Declaration */

static Name attribute_termnames[] = { NAME_name, NAME_value };

ClassDecl(attribute_decls,
          var_attribute, send_attribute, get_attribute, rc_attribute,
          2, attribute_termnames,
          "$Rev$");


status
makeClassAttribute(Class class)
{ declareClass(class, &attribute_decls);

  succeed;
}

