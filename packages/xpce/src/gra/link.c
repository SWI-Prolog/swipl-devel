/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialiseLink(Link link, Name from, Name to, Line line, Class cl)
{ if ( isDefault(from) )
    from = NAME_link;

  assign(link, from, from);
  assign(link, to, isDefault(to) ? from : to);
  assign(link, line, isDefault(line) ? (Line) newObject(ClassLine, 0) :	line);
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

  return newObject(link->connection_class, gr, gr2, link, from, to, 0);
}


status
makeClassLink(Class class)
{ sourceClass(class, makeClassLink, __FILE__, "$Revision$");

  localClass(class, NAME_line, NAME_appearance, "line", NAME_get,
	     "Line (with pen, arrows, etc.)");
  localClass(class, NAME_from, NAME_relation, "name", NAME_both,
	     "Name of valid handles at `from' side");
  localClass(class, NAME_to, NAME_relation, "name", NAME_both,
	     "Name of valid handles at `to' side");
  localClass(class, NAME_connectionClass, NAME_relation, "[class]", NAME_both,
	     "Class used by <-connection");

  delegateClass(class, NAME_line);
  termClass(class, "link", 3, NAME_from, NAME_to, NAME_line);

  sendMethod(class, NAME_initialise, DEFAULT, 4,
	     "handle_kind1=[name]", "handle_kind2=[name]",
	     "line=[line]", "connection_class=[class]",
	     "Create from handle names, line",
	     initialiseLink);

  getMethod(class, NAME_connection, NAME_relation, "connection", 4,
	    "from=graphical", "to=graphical",
	    "from_handle=[name]", "to_handle=[name]",
	    "Instantiate the link by creating a connection",
	    getConnectionLink);

  succeed;
}

