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
#include <h/unix.h>

static status
initialiseRC(RC rc, Name name, Class rc_class)
{ assign(rc, name,     name);
  assign(rc, rc_class, rc_class);

  if ( TheCallbackFunctions.getHostContext )
  { Any context = (*TheCallbackFunctions.getHostContext)(HOST);

    assign(rc, context, context);
  }

  succeed;
}


static RC
getConvertRC(Class class, Name name)
{ answer(answerObject(ClassRC, name, EAV));
}


		 /*******************************
		 *	     PROPERTIES		*
		 *******************************/

static status
existsRC(RC rc)
{ IOSTREAM *s;

  catchErrorPce(PCE, NAME_openFile);
  s = Sopen_object(rc, "rbr");
  catchPopPce(PCE);

  if ( s )
  { Sclose(s);
    succeed;
  }

  fail;
}


static status
accessRC(RC rc, Name mode)
{ if ( mode == NAME_read )
    return existsRC(rc);

  fail;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "name=name", "class=[name]" };

/* Instance Variables */

static vardecl var_rc[] =
{ IV(NAME_name, "name", IV_BOTH,
     NAME_name, "Name of the resource"),
  IV(NAME_class, "[name]", IV_BOTH,
     NAME_name, "Class of the resource (@default: don't care)"),
  IV(NAME_context, "any*", IV_GET,
     NAME_storage, "Host context information")
};

/* Send Methods */

static senddecl send_rc[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseRC,
     DEFAULT, "Create from name and class"),
  SM(NAME_access, 1, "mode={read,write,append,execute}", accessRC,
     NAME_test, "Test if resource has access"),
  SM(NAME_exists, 0, NULL, existsRC,
     NAME_test, "Test if resource exists")
};

/* Get Methods */

static getdecl get_rc[] =
{ GM(NAME_convert, 1, "resource", "name=name", getConvertRC,
     DEFAULT, "Convert name to resource")
};

/* Resources */

#define rc_rc NULL
/*
static classvardecl rc_rc[] =
{ 
};
*/

/* Class Declaration */

static Name rc_termnames[] = { NAME_name, NAME_class };

ClassDecl(rc_decls,
          var_rc, send_rc, get_rc, rc_rc,
          2, rc_termnames,
          "$Rev$");


status
makeClassRC(Class class)
{ return declareClass(class, &rc_decls);
}

