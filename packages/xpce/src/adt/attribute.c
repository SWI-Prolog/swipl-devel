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

