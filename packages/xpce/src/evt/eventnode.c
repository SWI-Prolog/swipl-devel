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

static EventTreeObj	getTreeEventNode(EventNodeObj n);
static status		sonEventNode(EventNodeObj n, EventNodeObj son);

static status
initialiseEventNode(EventNodeObj n, Any value, EventNodeObj parent)
{ if ( isDefault(parent) )
    parent = NIL;

  assign(n, value,  value);

  if ( isName(parent) )
  { EventNodeObj p;

    if ( !EventTree )
      realiseClass(ClassEvent);

    if ( !(p=getNodeEventTree(EventTree, parent)) )
      return errorPce(EventTree, NAME_noEvent, parent);
    parent = p;
  }

  if ( notNil(parent) )
    sonEventNode(parent, n);

  succeed;
}


static status
sonEventNode(EventNodeObj n, EventNodeObj son)
{ if ( notNil(son->parent) )
    return errorPce(son, NAME_alreadyHasParent);

  if ( isNil(n->sons) )
    assign(n, sons, newObject(ClassChain, EAV));

  appendChain(n->sons, son);
  son->parent = n;
  addNodeEventTree(getTreeEventNode(n), son);
  
  succeed;
}


status
isAEventNode(EventNodeObj sb, EventNodeObj super)
{ do
  { if ( sb == super )
      succeed;
    sb = sb->parent;
  } while( isObject(sb) );

  fail;
}


static EventTreeObj
getTreeEventNode(EventNodeObj n)
{ while( instanceOfObject(n->parent, ClassEventNode) )
    n = n->parent;

  if ( instanceOfObject(n->parent, ClassEventTree) )
    answer((EventTreeObj) n->parent);

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "value=name", "parent=[name|event_node]*" };

/* Instance Variables */

static vardecl var_eventNode[] =
{ IV(NAME_value, "event_id", IV_GET,
     NAME_value, "Value of the node"),
  IV(NAME_parent, "event_node|event_tree", IV_GET,
     NAME_hierarchy, "Direct parent of the node"),
  IV(NAME_sons, "chain*", IV_GET,
     NAME_hierarchy, "Chain of direct sons")
};

/* Send Methods */

static senddecl send_eventNode[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseEventNode,
     DEFAULT, "Create from value and parent"),
  SM(NAME_son, 1, "event_node", sonEventNode,
     NAME_edit, "Add a son to the node"),
  SM(NAME_isA, 1, "event_node", isAEventNode,
     NAME_test, "Test if node is a subnode of argument")
};

/* Get Methods */

static getdecl get_eventNode[] =
{ GM(NAME_tree, 0, "event_tree", NULL, getTreeEventNode,
     NAME_organisation, "The tree holding this event_node")
};

/* Resources */

#define rc_eventNode NULL
/*
static classvardecl rc_eventNode[] =
{ 
};
*/

/* Class Declaration */

static Name eventNode_termnames[] = { NAME_value, NAME_parent };

ClassDecl(eventNode_decls,
          var_eventNode, send_eventNode, get_eventNode, rc_eventNode,
          2, eventNode_termnames,
          "$Rev$");

status
makeClassEventNode(Class class)
{ return declareClass(class, &eventNode_decls);
}

