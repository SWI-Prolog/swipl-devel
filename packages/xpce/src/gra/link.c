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
#include <h/graphics.h>

static status
initialiseLink(Link link, Name from, Name to, Line line, Class cl)
{ if ( isDefault(from) )
    from = NAME_link;

  assign(link, from, from);
  assign(link, to, isDefault(to) ? from : to);
  assign(link, line, isDefault(line) ? (Line) newObject(ClassLine, EAV) :	line);
  assign(link, connection_class, cl);

  succeed;
}


		 /*******************************
		 *	     CONNECTION		*
		 *******************************/

					/* lazy binding: fixes save/load! */

static Connection
getConnectionLink(Link link, Graphical gr, Graphical gr2, Name from, Name to)
{ if ( !instanceOfObject(link->connection_class, ClassClass) )
    assign(link, connection_class, ClassConnection);

  return newObject(link->connection_class, gr, gr2, link, from, to, EAV);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_connection[] =
        { "from=graphical", "to=graphical", "from_handle=[name]", "to_handle=[name]" };
static char *T_initialise[] =
        { "handle_kind1=[name]", "handle_kind2=[name]", "line=[line]", "connection_class=[class]" };

/* Instance Variables */

static vardecl var_link[] =
{ IV(NAME_line, "line", IV_GET,
     NAME_appearance, "Line (with pen, arrows, etc.)"),
  IV(NAME_from, "name", IV_BOTH,
     NAME_relation, "Name of valid handles at `from' side"),
  IV(NAME_to, "name", IV_BOTH,
     NAME_relation, "Name of valid handles at `to' side"),
  IV(NAME_connectionClass, "[class]", IV_BOTH,
     NAME_relation, "Class used by <-connection")
};

/* Send Methods */

static senddecl send_link[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseLink,
     DEFAULT, "Create from handle names, line")
};

/* Get Methods */

static getdecl get_link[] =
{ GM(NAME_connection, 4, "connection", T_connection, getConnectionLink,
     NAME_relation, "Instantiate the link by creating a connection")
};

/* Resources */

#define rc_link NULL
/*
static classvardecl rc_link[] =
{ 
};
*/

/* Class Declaration */

static Name link_termnames[] = { NAME_from, NAME_to, NAME_line };

ClassDecl(link_decls,
          var_link, send_link, get_link, rc_link,
          3, link_termnames,
          "$Rev$");


status
makeClassLink(Class class)
{ declareClass(class, &link_decls);
  delegateClass(class, NAME_line);

  succeed;
}

