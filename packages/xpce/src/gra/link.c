/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialiseLink(Link link, Name from, Name to, Line line)
{ if ( isDefault(from) )
    from = NAME_link;
  assign(link, from, from);
  assign(link, to, isDefault(to) ? from : to);
  assign(link, line, isDefault(line) ? (Line) newObject(ClassLine, 0) :	line);

  succeed;
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

  delegateClass(class, NAME_line);
  termClass(class, "link", 3, NAME_from, NAME_to, NAME_line);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "handle_kind1=[name]", "handle_kind2=[name]", "line=[line]",
	     "Create from handle names, line",
	     initialiseLink);

  succeed;
}

