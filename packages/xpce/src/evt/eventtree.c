/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status	rootEventTree(EventTreeObj t, EventNodeObj n);

static status
initialiseEventTree(EventTreeObj t, EventNodeObj n)
{ assign(t, root,  NIL);
  assign(t, table, newObject(ClassHashTable, toInt(101), EAV));

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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_eventTree[] =
{ SV(NAME_root, "event_node", IV_GET|IV_STORE, rootEventTree,
     NAME_hierarchy, "Root node of the hierarchy"),
  IV(NAME_table, "hash_table", IV_NONE,
     NAME_hashing, "Hashtable to find nodes by value")
};

/* Send Methods */

static senddecl send_eventTree[] =
{ SM(NAME_initialise, 1, "root=[event_node]", initialiseEventTree,
     DEFAULT, "Create from root node")
};

/* Get Methods */

static getdecl get_eventTree[] =
{ GM(NAME_node, 1, "event_node", "event_id", getNodeEventTree,
     NAME_lookup, "Find a node from it's associated value")
};

/* Resources */

#define rc_eventTree NULL
/*
static classvardecl rc_eventTree[] =
{ 
};
*/

/* Class Declaration */

static Name eventTree_termnames[] = { NAME_root };

ClassDecl(eventTree_decls,
          var_eventTree, send_eventTree, get_eventTree, rc_eventTree,
          1, eventTree_termnames,
          "$Rev$");


status
makeClassEventTree(Class class)
{ return declareClass(class, &eventTree_decls);
}

