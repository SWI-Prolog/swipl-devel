/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static EventTreeObj	getTreeEventNode(EventNodeObj n);

static status
initialiseEventNode(EventNodeObj n, Any value, EventNodeObj parent)
{ if ( isDefault(parent) )
    parent = NIL;

  assign(n, value,  value);
  assign(n, parent, parent);
  assign(n, sons,   NIL);

  succeed;
}


static status
sonEventNode(EventNodeObj n, EventNodeObj son)
{ if ( notNil(son->parent) )
    return errorPce(son, NAME_alreadyHasParent);

  if ( isNil(n->sons) )
    assign(n, sons, newObject(ClassChain, 0));

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


status
makeClassEventNode(Class class)
{ sourceClass(class, makeClassEventNode, __FILE__, "$Revision$");

  localClass(class, NAME_value, NAME_value,
	     "event_id", NAME_get,
	     "Value of the node");
  localClass(class, NAME_parent, NAME_hierarchy,
	     "event_node|event_tree", NAME_get,
	     "Direct parent of the node");
  localClass(class, NAME_sons, NAME_hierarchy,
	     "chain*", NAME_get,
	     "Chain of direct sons");

  termClass(class, "event_node", 2, NAME_value, NAME_parent);

  sendMethod(class, NAME_initialise, DEFAULT, 2,
	     "value=name", "parent=[event_node]*",
	     "Create from value and parent",
	     initialiseEventNode);
  sendMethod(class, NAME_son, NAME_edit, 1, "event_node",
	     "Add a son to the node",
	     sonEventNode);
  sendMethod(class, NAME_isA, NAME_test, 1, "event_node",
	     "Test if node is a subnode of argument",
	     isAEventNode);
  
  getMethod(class, NAME_tree, NAME_organisation, "event_tree", 0,
	    "The tree holding this event_node",
	    getTreeEventNode);

  succeed;
}

