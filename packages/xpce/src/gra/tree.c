/*  $Id$ $

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status	updateHandlesTree P((Tree));
static status	rootTree(Tree t, Node root);


static Any div_h_2;			/* h/2 */
static Any div_w_2;			/* h/2 */


static status
initialiseTree(Tree t, Node node)
{ if ( isDefault(node) )
    node = NIL;

  initialiseDevice((Device) t);
  assign(t, auto_layout, 	ON);
  assign(t, direction,		DEFAULT);
  assign(t, levelGap,		DEFAULT);
  assign(t, neighbourGap,	DEFAULT);
  assign(t, linkGap,		DEFAULT);
  assign(t, link, 		newObject(ClassLink,
					  NAME_parent, NAME_son, 0));
  assign(t, rootHandlers,	newObject(ClassChain, 0));
  assign(t, leafHandlers,	newObject(ClassChain, 0));
  assign(t, nodeHandlers,	newObject(ClassChain, 0));
  assign(t, collapsedHandlers,	newObject(ClassChain, 0));

  obtainResourcesObject(t);
    
  if ( !div_h_2 )
  { div_h_2 = newObject(ClassDivide, NAME_h, TWO, 0);
    protectObject(div_h_2);
  }

  assign(t, sonHandle,
	 newObject(ClassHandle,
		   minInt(t->linkGap),
		   div_h_2,
		   NAME_son, 0));
  assign(t, parentHandle,
	 newObject(ClassHandle,
		   newObject(ClassPlus, NAME_w, t->linkGap, 0),
		   div_h_2,
		   NAME_parent, 0));

  assign(t, root, NIL);
  assign(t, displayRoot, NIL);

  if ( notNil(node) )
    rootTree(t, node);

  requestComputeTree(t);

  succeed;
}


static status
rootTree(Tree t, Node root)
{ if ( notNil(t->root) )
    return errorPce(t, NAME_alreadyHasRoot);

  displayTree(t, root);
  assign(t, root, root);
  assign(t, displayRoot, root);
  requestComputeTree(t);

  succeed;
}


static status
unlinkTree(Tree t)
{ if ( notNil(t->root) )
    freeObject(t->root);

  return unlinkDevice((Device) t);
}


status
requestComputeTree(Tree t)
{ return requestComputeGraphical(t, DEFAULT);
}


		/********************************
		*             EVENTS		*
		********************************/

static status
eventTree(Tree t, EventObj ev)
{ Cell cell;

  if ( eventDevice(t, ev) )
    succeed;

  for_cell(cell, t->pointed)
  { Node n;
    Cell cell2;

    if ( (n = getFindNodeNode(t->displayRoot, cell->value)) == FAIL )
      continue;

    if ( n->collapsed == ON )
    { for_cell(cell2, t->collapsedHandlers)
      { if ( postEvent(ev, n->image, cell2->value) == SUCCEED )
	  succeed;
      }
    }
    if ( emptyChain(n->sons) == SUCCEED )
    { for_cell(cell2, t->leafHandlers)
	if ( postEvent(ev, n->image, cell2->value) == SUCCEED )
	  succeed;
    }
    if ( n->tree->displayRoot == n )
    { for_cell(cell2, t->rootHandlers)
	if ( postEvent(ev, n->image, cell2->value) == SUCCEED )
	  succeed;
    }
    for_cell(cell2, t->nodeHandlers)
      if ( postEvent(ev, n->image, cell2->value) == SUCCEED )
	succeed;
  }

  fail;
}


static status
levelGapTree(Tree t, Int i)
{ if (i == t->levelGap)
    succeed;

  assign(t, levelGap, i);
  requestComputeTree(t);

  succeed;
}


status
displayTree(Tree t, Node n)
{ if ( notNil(n->tree) )
    return errorPce(t, NAME_alreadyShown, n, n->tree);

  send(n->image, NAME_handle,     t->sonHandle, 0);
  send(n->image, NAME_handle,     t->parentHandle, 0);

  assign(n, tree, t);
  
  succeed;
}


static status
neighbourGapTree(Tree t, Int i)
{ if (i == t->neighbourGap)
    succeed;

  assign(t, neighbourGap, i);
  requestComputeTree(t);

  succeed;
}


static status
linkGapTree(Tree t, Int i)
{ if ( i == t->linkGap)
    succeed;

  assign(t, linkGap, i);
  updateHandlesTree(t);
  requestComputeTree(t);

  succeed;
}
  

static status
computeTree(Tree t)
{ if ( notNil(t->request_compute) )
  { if ( t->auto_layout == ON )
    { Any old = t->request_compute;

      assign(t, request_compute, NIL);	/* hack ... */
      send(t, NAME_layout, 0);
      assign(t, request_compute, old);
    }

    computeDevice(t);
  }

  succeed;
}


static status
layoutTree(Tree t)
{ if ( isNil(t->displayRoot) )
    succeed;

  if ( send(t->displayRoot, NAME_computeLevel, ZERO, 0) &&
       get(t->displayRoot, NAME_computeSize, ZERO, 0) &&
       send(t->displayRoot, NAME_computeLayout, ZERO, ZERO, ZERO, 0) )
    succeed;

  fail;
}


static status
autoLayoutTree(Tree t, Bool val)
{ if ( t->auto_layout != val )
  { assign(t, auto_layout, val);
    if ( val == ON )
      send(t, NAME_layout, 0);
  }

  succeed;
}


static status
directionTree(Tree t, Name dir)
{ if ( t->direction == dir )
    succeed;

  assign(t, direction, dir);
  updateHandlesTree(t);
  requestComputeTree(t);

  succeed;
}


static int
updateHandlesTree(Tree t)
{ if ( equalName(t->direction, NAME_horizontal) )
  { send(t->parentHandle, NAME_xPosition,
	 newObject(ClassPlus, NAME_w, t->linkGap, 0), 0);
    send(t->parentHandle, NAME_yPosition, div_h_2, 0);
    send(t->sonHandle,    NAME_xPosition, minInt(t->linkGap), 0);
    send(t->sonHandle,    NAME_yPosition, div_h_2, 0);
    send(t->parentHandle, NAME_kind,      NAME_parent, 0);
    send(t->sonHandle,    NAME_kind,      NAME_son, 0);
  } else if ( equalName(t->direction, NAME_vertical) )
  { if ( !div_w_2)
    { div_w_2 = newObject(ClassDivide, NAME_w, TWO, 0);
      protectObject(div_w_2);
    }

    send(t->parentHandle, NAME_xPosition, div_w_2, 0);
    send(t->parentHandle, NAME_yPosition,
	 newObject(ClassPlus, NAME_h, t->linkGap, 0), 0);
    send(t->sonHandle,    NAME_xPosition, div_w_2, 0);
    send(t->sonHandle,    NAME_yPosition, minInt(t->linkGap), 0);
    send(t->parentHandle, NAME_kind,      NAME_parent, 0);
    send(t->sonHandle,    NAME_kind,      NAME_son, 0);
  } else
  { send(t->parentHandle, NAME_kind,      NAME_none, 0);
    send(t->sonHandle,    NAME_kind,      NAME_none, 0);
  }

  succeed;
}


status
unzoomTree(Tree t)
{ return zoomTree(t, t->root);
}


status
zoomTree(Tree t, Node n)
{ if ( n->tree != t )
    fail;

  if ( t->displayRoot == n )
    succeed;

  assign(t, displayRoot, n);
  updateDisplayedTree(t);
  requestComputeTree(t);

  succeed;
}


static status
rootHandlerTree(Tree t, Handler h)
{ return prependChain(t->rootHandlers, h);
}


static status
leafHandlerTree(Tree t, Handler h)
{ return prependChain(t->leafHandlers, h);
}


static status
nodeHandlerTree(Tree t, Handler h)
{ return prependChain(t->nodeHandlers, h);
}


static status
collapsedHandlerTree(Tree t, Handler h)
{ return prependChain(t->collapsedHandlers, h);
}


static status
forAllTree(Tree t, Code msg)
{ if ( notNil(t->root) )
    return forAllNode(t->root, msg);
  
  succeed;
}


static status
forSomeTree(Tree t, Code msg)
{ if ( notNil(t->root) )
    return forSomeNode(t->root, msg);
  
  succeed;
}


		/********************************
		*             VISUAL		*
		********************************/

static void
add_nodes_tree(Node n, Chain ch)
{  if ( notNil(n) )
   { Cell cell;

     appendChain(ch, n);

     for_cell(cell, n->sons)
       add_nodes_tree(cell->value, ch);
   }
}


static Chain
getContainsTree(Tree t)
{ Chain ch = answerObject(ClassChain, 0);

  add_nodes_tree(t->root, ch);
  answer(ch);
}


status
makeClassTree(Class class)
{ sourceClass(class, makeClassTree, __FILE__, "$Revision$");

  localClass(class, NAME_root, NAME_nodes, "node*", NAME_get,
	     "Root-node of the tree");
  localClass(class, NAME_displayRoot, NAME_scroll, "node*", NAME_get,
	     "Root of visible subtree");
  localClass(class, NAME_autoLayout, NAME_layout, "bool", NAME_get,
	     "Enforce automatic layout?");
  localClass(class, NAME_levelGap, NAME_layout, "int", NAME_get,
	     "Distance between levels");
  localClass(class, NAME_neighbourGap, NAME_layout, "int", NAME_get,
	     "Distance between neighbours");
  localClass(class, NAME_linkGap, NAME_layout, "int", NAME_get,
	     "Distance between line and image");
  localClass(class, NAME_direction, NAME_layout,
	     "{horizontal,vertical,list}", NAME_get,
	     "Layout of the tree");
  localClass(class, NAME_link, NAME_layout, "link", NAME_get,
	     "Generic connection between graphicals");
  localClass(class, NAME_parentHandle, NAME_relation, "handle", NAME_get,
	     "Connection point to parent");
  localClass(class, NAME_sonHandle, NAME_relation, "handle", NAME_get,
	     "Connection point to sons");
  localClass(class, NAME_rootHandlers, NAME_event, "chain", NAME_get,
	     "Event handlers for root");
  localClass(class, NAME_leafHandlers, NAME_event, "chain", NAME_get,
	     "Event handlers for leafs");
  localClass(class, NAME_nodeHandlers, NAME_event, "chain", NAME_get,
	     "Event handlers for any type of node");
  localClass(class, NAME_collapsedHandlers, NAME_event, "chain", NAME_get,
	     "Event handlers for collapsed nodes");

  termClass(class, "tree", 1, NAME_root);
  delegateClass(class, NAME_root);

  storeMethod(class, NAME_levelGap,     levelGapTree);
  storeMethod(class, NAME_neighbourGap, neighbourGapTree);
  storeMethod(class, NAME_direction,    directionTree);
  storeMethod(class, NAME_linkGap,      linkGapTree);
  storeMethod(class, NAME_autoLayout,	autoLayoutTree);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "root=[node]*",
	     "Create a tree from a root node",
	     initialiseTree);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Removes all the nodes",
	     unlinkTree);
  sendMethod(class, NAME_compute, DEFAULT, 0,
	     "Recompute the tree layout",
	     computeTree);
  sendMethod(class, NAME_unzoom, NAME_scroll, 0,
	     "Make the visible root the real root",
	     unzoomTree);
  sendMethod(class, NAME_zoom, NAME_scroll, 1, "node",
	     "Zoom to a particular node",
	     zoomTree);
  sendMethod(class, NAME_root, NAME_nodes, 1, "node",
	     "Set root node",
	     rootTree);
  sendMethod(class, NAME_event, DEFAULT, 1, "event",
	     "Process an event",
	     eventTree);
  sendMethod(class, NAME_rootHandler, NAME_event, 1, "recogniser",
	     "Add recogniser for root node",
	     rootHandlerTree);
  sendMethod(class, NAME_leafHandler, NAME_event, 1, "recogniser",
	     "Add recogniser for leaf node",
	     leafHandlerTree);
  sendMethod(class, NAME_nodeHandler, NAME_event, 1, "recogniser",
	     "Add recogniser for any node node",
	     nodeHandlerTree);
  sendMethod(class, NAME_collapsedHandler, NAME_event, 1, "recogniser",
	     "Add recogniser for collapsed node",
	     collapsedHandlerTree);
  sendMethod(class, NAME_forAll, NAME_iterate, 1, "code",
	     "Run code on all nodes (demand acceptance)",
	     forAllTree);
  sendMethod(class, NAME_forSome, NAME_iterate, 1, "code",
	     "Run code on all nodes",
	     forSomeTree);
  sendMethod(class, NAME_layout, NAME_update, 0,
	     "Recompute layout",
	     layoutTree);

  getMethod(class, NAME_contains, DEFAULT, "chain", 0,
	    "New chain with nodes I manage",
	    getContainsTree);

  attach_resource(class, "direction",     "name",    "horizontal",
		  "Default style {horizontal,vertical,list}");
  attach_resource(class, "link_gap",      "int", "2",
		  "Gap between link-line and image");
  attach_resource(class, "level_gap",     "int", "50",
		  "Gap between levels");
  attach_resource(class, "neighbour_gap", "int", "0",
		  "Gap between neighbours");

  succeed;
}
