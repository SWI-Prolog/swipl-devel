/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include <pce/Pce.h>
#include <pce/Class.h>

// References to XPCE instance variables of the class.

static PceVariable *bnode_left;
static PceVariable *bnode_right;
static PceVariable *bnode_key;
static PceVariable *bnode_value;

// Method to initialise the instance (like a C++ constructor)

static PceStatus
initialiseBNode(PceReceiver n, PceArg key, PceArg value)
{ n.store(bnode_key, key);
  n.store(bnode_value, value);

  return SUCCEED;
}


// Send method to insert a new key/value pair into the tree

PceStatus
insertBNode(PceReceiver n, PceArg key, PceArg value)
{ int c = strcmp(n.fetch(bnode_key), key);

  if ( c == 0 )				/* replace value */
    return n.store(bnode_value, value);

  PceArg n2 = n.fetch(c > 0 ? bnode_right : bnode_left);
  if ( n2 != TheNil )
    return insertBNode(n2, key, value);

  return n.store(c > 0 ?  bnode_right : bnode_left,
		 PceObject("b_node", key, value));
}


// Method to find a node from a key.

PceArg
getNodeBNode(PceReceiver n, PceArg key)
{ int c = strcmp(n.fetch(bnode_key), key);

  if ( c == 0 )
    return n;

  PceArg n2 = n.fetch(c > 0 ? bnode_right : bnode_left);

  if ( n2 != TheNil )
    return getNodeBNode(n2, key);

  return FAIL;
}


// c++ function to build the XPCE class definition

PceStatus
makeClassBNode(PceClass cl)
{ bnode_left =
    cl.defvar("left", "tree", "Node to my left",
	      "b_node*", "get", TheNil);
  bnode_right =
    cl.defvar("right", "tree", "Node to my right",
	      "b_node*", "get", TheNil);
  bnode_key =
    cl.defvar("key", "table", "Key-name of the node",
	      "name", "get", TheNil);
  bnode_value =
    cl.defvar("value", "table", "Value of the node",
	      "any", "get", TheNil);

  cl.defsendmethod("initialise", "oms", "Create from key and value",
		   initialiseBNode, "key=name", "value=any");
  cl.defsendmethod("insert", "edit", "Add entry to the table",
		   insertBNode, "key=name", "value=any");
  
  cl.defgetmethod("node", "lookup", "Lookup node from key",
		   "b_node", getNodeBNode, "key=name");

  return SUCCEED;
}
  
	    
// let a C++ global constructor declare the class

PceClass ClassBNode("b_node", "object", "Node of a binary tree",
		    makeClassBNode);


PceStatus
pceInitApplication(int argc, char *argv[])
{ return SUCCEED;
}
