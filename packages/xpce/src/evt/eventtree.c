/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status	rootEventTree(EventTreeObj t, EventNodeObj n);

static status
initialiseEventTree(EventTreeObj t, EventNodeObj n)
{ assign(t, root,  NIL);
  assign(t, table, newObject(ClassHashTable, toInt(101), 0));

  if ( notDefault(n) )
    rootEventTree(t, n);

  succeed;
}


static status
rootEventTree(EventTreeObj t, EventNodeObj n)
{ if ( notNil(t->root) )
    return errorPce(t, NAME_alreadyHasRoot);

  assign(t, root,   n);
  assign(n, parent, t);
  
  addNodeEventTree(t, n);

  succeed;
}


status
addNodeEventTree(EventTreeObj t, EventNodeObj n)
{ return appendHashTable(t->table, n->value, n);
}


EventNodeObj
getNodeEventTree(EventTreeObj t, Any value)
{ return getMemberHashTable(t->table, value);
}


status
makeClassEventTree(Class class)
{ sourceClass(class, makeClassEventTree, __FILE__, "$Revision$");

  localClass(class, NAME_root, NAME_hierarchy, "event_node", NAME_get,
	     "Root node of the hierarchy");
  localClass(class, NAME_table, NAME_hashing, "hash_table", NAME_none,
	     "Hashtable to find nodes by value");

  termClass(class, "event_tree", 1, NAME_root);

  storeMethod(class, NAME_root, rootEventTree);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "root=[event_node]",
	     "Create from root node",
	     initialiseEventTree);
	
  getMethod(class, NAME_node, NAME_lookup, "event_node", 1, "event_id",
	    "Find a node from it's associated value",
	    getNodeEventTree);

  succeed;
}

