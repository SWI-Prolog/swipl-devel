/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static void	swap_parents(Node, Node, Chain);
static int	isSonNode2(Node, Node);
static int	isParentNode2(Node, Node);
static status	unrelate_node(Node, Node);
static status	unlinkParentsNode(Node n);
static status	unlinkSonsNode(Node n);
static status	relateNode(Node n, Node n2);
static status	relateImageNode(Node n, Node n2);
static status	unrelateImageNode(Node n, Node n2);
static status	unrelateImagesNode(Node n);
static status	relateImagesNode(Node n);
static status	isSonNode(Node n, Node n2);
static status	isParentNode(Node n, Node n2);

static status
initialiseNode(Node n, Graphical gr)
{ assign(n, tree,      NIL);
  assign(n, image,     gr);
  assign(n, parents,   newObject(ClassChain,0));
  assign(n, sons,      newObject(ClassChain,0));
  assign(n, sons_size, ZERO);
  assign(n, my_size,   ZERO);
  assign(n, level,     ZERO);
  assign(n, computed,  NIL);
  assign(n, collapsed, OFF);
  assign(n, displayed, ON);
  
  succeed;
}


static status
unlinkNode(Node n)
{ Tree tree = n->tree;
  Cell cell1, cell2;

  if ( notNil(tree) )
  { if ( onFlag(tree, F_FREED|F_FREEING) ) /* destroying the tree */
    { Node son;

      for_chain(n->sons, son, freeObject(son));
      assign(n, image, NIL);

      succeed;
    } else				/* just remove this node */
    { for_cell(cell1, n->parents)
	for_cell(cell2, n->sons)
	  relateNode(cell1->value, cell2->value);

      if ( n == tree->root )
      { if ( emptyChain(n->sons) )
	  assign(tree, root, NIL);
        else
	  assign(tree, root, n->sons->head->value);

	if ( tree->displayRoot == n )
	  assign(tree, displayRoot, tree->root);
      } else if ( n == tree->displayRoot )
	tree->displayRoot = n->parents->head->value;

      unlinkParentsNode(n);
      unlinkSonsNode(n);
    }

    if ( notNil(n->image) && !isFreedObj(n->image) )
      send(n->image, NAME_device, NIL, 0);

    assign(n, tree, NIL);
    assign(n, image, NIL);
  }

  succeed;
}


static status
unlinkParentsNode(Node n)
{ Node parent;

  for_chain(n->parents, parent, unrelate_node(parent, n));
  succeed;
}


static status
unlinkSonsNode(Node n)
{ Node son;

  for_chain(n->sons, son, unrelate_node(n, son));
  succeed;
}


static Node
getConvertNode(Class class, Graphical gr)
{ if ( instanceOfObject(gr->device, ClassTree) )
    answer(getNodeGraphical(gr));

  answer(newObject(ClassNode, gr, 0));
}


static status
computeLevelNode(Node n, Int l, Bool force)
{ Cell cell;
  Int nextLevel;			/* level + 1 */

  if ( n->computed != NAME_level || force == ON )
  { assign(n, computed, NAME_level);
    assign(n, level, l);

    if ( n->collapsed == ON )
      succeed;

    nextLevel = inc(l);
    for_cell(cell, n->sons)
      computeLevelNode(cell->value, nextLevel, force);
  } else
  { if ( valInt(l) > valInt(n->level) )
      computeLevelNode(n, l, ON);
  }

  succeed;
} 


static Int
getComputeSizeNode(Node n, Int l)
{ Cell cell;
  int hor = n->tree->direction != NAME_vertical;
  Int nextLevel;	/* level + 1 */
  int sons_size;	/* total size of sons */

  if ( n->computed == NAME_size ||	/* already done */
       n->level != l )			/* to be done in another brance */
    answer(ZERO);

  assign(n, computed, NAME_size);
  assign(n, my_size, getv(n->image, hor ? NAME_height : NAME_width, 0, NULL));

  if ( n->collapsed == ON )
  { assign(n, sons_size, ZERO);
    answer(n->my_size);
  }

  nextLevel = inc(l);

  sons_size = 0;
  for_cell(cell, n->sons)
  { if ( sons_size > 0 )				/* not first */
      sons_size += valInt(n->tree->neighbourGap);
    sons_size += valInt(getComputeSizeNode(cell->value, nextLevel));
  }
  
  assign(n, sons_size, toInt(sons_size));
  
  if ( n->tree->direction == NAME_list )
    answer(add(add(n->sons_size, n->my_size), n->tree->neighbourGap));
  else
    answer(maxInt(n->sons_size, n->my_size));
}


static status
computeLayoutNode(Node n, Int l, Int x, Int y)
{ Int y2, x2;
  int hor = n->tree->direction != NAME_vertical;
  int list = n->tree->direction == NAME_list;
  Int size = maxInt(n->my_size,n->sons_size);
  Int nextLevel;
  Cell cell;
  Node son;

  if ( n->displayed == OFF )
    succeed;

  if ( n->computed == NAME_layout ||
       n->level != l )
    succeed;
  assign(n, computed, NAME_layout);

  x2 = ( hor || list ? x : add(x, div(sub(size,n->my_size), TWO)));
  y2 = (!hor || list ? y : add(y, div(sub(size,n->my_size), TWO)));

  if ( (Tree) n->image->device != n->tree ||
       n->image->displayed == OFF )
    send(n->tree, NAME_display, n->image, 0);
  if ( n->image->area->x != x2 || n->image->area->y != y2 )
    send(n->image, NAME_set, x2, y2, 0);

  if ( n->collapsed == ON )
    succeed;

  if ( list )
  { x2 = add(x2, n->tree->levelGap);
    y2 = add(y2, add(get(n->image, NAME_height, 0), n->tree->neighbourGap));
  } else if ( hor )
  { x2 = add(x, add(get(n->image, NAME_width, 0), n->tree->levelGap));
    if ( valInt(n->sons_size) > valInt(size) )
      y2 = y;
    else
      y2 = add(y, div(sub(size, n->sons_size), TWO));
  } else
  { y2 = add(y, add(get(n->image, NAME_height, 0), n->tree->levelGap));
    if ( valInt(n->sons_size) > valInt(size) )
      x2 = x;
    else
      x2 = add(x, div(sub(size, n->sons_size), TWO));
  }

  nextLevel = inc(l);

  for_cell(cell, n->sons)
  { son = cell->value;

    if ( son->level == nextLevel && son->computed != NAME_layout )
    { computeLayoutNode(son, nextLevel, x2, y2);
      if ( list )
      { y2 = toInt(valInt(y2) +
		   valInt(son->sons_size) + valInt(son->my_size) +
		   valInt(n->tree->neighbourGap));
      } else
      { size = maxInt(son->my_size, son->sons_size);
	if ( hor )
	  y2 = add(y2, add(size, n->tree->neighbourGap));
	else
	  x2 = add(x2, add(size, n->tree->neighbourGap));
      }
    }
  }

  succeed;
}


static void
initUpdateDisplayedNode(Node n)
{ Cell cell;

  assign(n, displayed, DEFAULT);
  for_cell(cell, n->sons)
    initUpdateDisplayedNode(cell->value);
}


static void
markDisplayedNode(Node n)
{ assign(n, displayed, ON);
  if ( n->collapsed != ON )
  { Cell cell;

    for_cell(cell, n->sons)
      markDisplayedNode(cell->value);
  }
}


static status
updateDisplayedNode(Node n)
{ Cell cell;

  if ( isDefault(n->displayed) )
    assign(n, displayed, OFF);

  if ( notNil(n->image) && n->image->displayed != n->displayed )
    DisplayedGraphical(n->image, n->displayed);
  
  for_cell(cell, n->sons)
    updateDisplayedNode(cell->value);

  succeed;
}


status
updateDisplayedTree(Tree t)
{ if ( notNil(t->root) )
  { initUpdateDisplayedNode(t->root);
    if ( notNil(t->displayRoot) )
      markDisplayedNode(t->displayRoot);
    updateDisplayedNode(t->root);
  }

  succeed;
}


static status
moveAfterNode(Node n, Node n2)
{ Cell cell;

  if ( isDefault(n2) || isNil(n2) )
  { Node parent = getHeadChain(n->parents);
    
    if ( isObject(parent) )
    { status rval;

      rval = moveAfterChain(parent->sons, n, DEFAULT);
      requestComputeTree(n->tree);

      return rval;
    }

    fail;
  }


  for_cell(cell, n->parents)
  { Node parent = cell->value;

    if ( memberChain(n2->parents, parent) == SUCCEED )
    { status rval;

      rval = moveAfterChain(parent->sons, n, n2);
      requestComputeTree(n->tree);

      return rval;
    }
  }

  fail;
}


static status
moveNode(Node n, Node n2)		/* n2 becomes a son of n */
{ if ( n->tree != n2->tree ||
       isNil(n->tree) ||
       n == n2 ||
       isSonNode(n2, n) == SUCCEED )
    fail;

  if ( memberChain(n->sons, n2 ) == SUCCEED)
    succeed;

  unlinkParentsNode(n2);
  relateNode(n, n2);
  requestComputeTree(n->tree);

  succeed;
}


static status
sonNode(Node n, Node n2)		/* make n2 a son of n */
{ if ( notNil(n2->tree) && n2->tree != n->tree )
    return errorPce(n, NAME_alreadyShown, n2, n2->tree);
  if ( isNil(n->tree) )
    return errorPce(n, NAME_nodeNotInTree);

  if ( memberChain(n->sons, n2) )
    succeed;

  if ( isParentNode(n, n2) || n2 == n )
    return errorPce(n, NAME_wouldBeCyclic);

  if ( isNil(n2->tree) )
    displayTree(n->tree, n2);

  relateNode(n, n2);
  requestComputeTree(n->tree);

  succeed;
}


static status
imageNode(Node n, Graphical gr)		/* change image of node */
{ if ( isNil(n->tree) )
  { assign(n, image, gr);
    succeed;
  }

  unrelateImagesNode( n );
  send(n->image, NAME_device, NIL, 0);

  send(gr, NAME_handle,     n->tree->sonHandle, 0);
  send(gr, NAME_handle,     n->tree->parentHandle, 0);

  assign(n, image, gr);
  relateImagesNode( n );

  requestComputeTree(n->tree);
  
  succeed;
}


/*  Delete an entire subtree, but leave all nodes that still have a
    link to the root node of the tree in the tree. Used in
    unrelateNode().

 ** Thu May 18 15:43:00 1989  jan@swivax.UUCP (Jan Wielemaker)  */

static status
delete_tree_node(Node n)
{ Cell cell, c;
  Tree tree = n->tree;

  if ( isParentNode(n, tree->root) == SUCCEED )
    succeed;				/* has some other path */
  
  for_cell_save(cell, c, n->sons)
  { Node son = cell->value;

    unrelate_node(n, son);
    delete_tree_node(son);
  }
  freeObject(n);

  succeed;
}


static status
deleteTreeNode(Node n)
{ Cell cell, c2;

  for_cell_save(cell, c2, n->parents)
    unrelate_node(cell->value, n);

  return delete_tree_node(n);
}


/*  Delete a node from a tree.  All sons of this node  are  connected to
    all  parents of this node.  If the node is the root node of the tree
    it can only be deleted if it has exactly ONE son, which then becomes
    the parent.

 ** Thu Oct 20 22:24:57 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static status
deleteNode(Node n)
{ return freeObject(n);
}


static status
swapTreeNode(Node n, Node n2)		/* swap the positions of two entire subtrees */
           
{ Cell cell;
  Chain intersection, tmp;
  Node parent;

  if ( n->tree != n2->tree ||
       isNil(n->tree) ||
       isSonNode(n, n2) == SUCCEED ||
       isSonNode(n2, n) == SUCCEED )
    fail;

  TRY( intersection = getIntersectionChain(n->parents, n2->parents) );

  for_cell(cell, intersection)
  { parent = cell->value;
    swapChain(parent->sons, n, n2);
  }
  
  swap_parents(n, n2, intersection);
  swap_parents(n2, n, intersection);

  tmp = n2->parents;			/* swap the parent chains. Do not */
  n2->parents = n->parents;		/* use assign as `tmp' will drop out */
  n->parents = tmp;			/* ref. counts is not affected */

  freeObject(intersection);
  requestComputeTree(n->tree);

  succeed;
}

/*  swap the son links of the parents of n to point to n2. Parents in
    `intersection are already swapped.

 ** Fri Oct 21 10:14:51 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static void
swap_parents(Node n, Node n2, Chain intersection)
{ Node parent;
  Cell cell, cell2;

  for_cell(cell, n->parents)
  { parent = cell->value;
    if ( memberChain(intersection, parent) == SUCCEED )
      continue;
    for_cell(cell2, parent->sons)
    { if ( (Node) cell2->value == n )
      { unrelateImageNode(parent, n);
	relateImageNode(parent, n2);
	cell2->value = n2;		/* do not use assign here as the son */
	break;				/* might drop. Will be corrected */
      }					/* in second pass anyway */
    }
  }
}


static status
swapNode(Node n, Node n2)			/* swap images of two nodes */
           
{ Graphical gr;

  if ( n->tree != n2->tree || isNil(n->tree) )
    fail;

  unrelateImagesNode(n);
  unrelateImagesNode(n2);

  gr = n->image;
  assign(n, image, n2->image);
  assign(n2, image, gr);

  relateImagesNode(n);
  relateImagesNode(n2);
  requestComputeTree(n->tree);

  succeed;
}


static status
relateNode(Node n, Node n2)
{ appendChain(n->sons, n2);
  appendChain(n2->parents, n);
  relateImageNode(n, n2);

  succeed;
}


static status
relateImageNode(Node n, Node n2)
{ connectGraphical(n->image, n2->image, n->tree->link, DEFAULT, DEFAULT);

  succeed;
}


static status
unrelate_node(Node n, Node n2)
{ status rval;

  addCodeReference(n);
  addCodeReference(n2);

  if ( deleteChain(n->sons, n2) && deleteChain(n2->parents, n) )
  { unrelateImageNode(n, n2);
    rval = SUCCEED;
  } else
    rval = FAIL;

  delCodeReference(n);
  delCodeReference(n2);

  return rval;
}
  

static status
unrelateNode(Node n, Node n2)
{ status rval = SUCCEED;

  if ( isNil(n->tree) )
    succeed;			/* cannot be related yet */

  if ( memberChain(n->sons, n2) == SUCCEED )
  { unrelate_node(n, n2);
    delete_tree_node(n2);	/* trow away unconnected subtree */
  } else if ( memberChain(n2->sons, n) == SUCCEED )
  { unrelate_node(n2, n);
    delete_tree_node(n);
  } else
    rval = FAIL;

  requestComputeTree(n->tree);

  return rval;
}


static status
unrelateImageNode(Node n, Node n2)
{ disconnectGraphical(n->image, n2->image, n->tree->link, DEFAULT, DEFAULT);

  succeed;
}


static status
unrelateImagesNode(Node n)
{ Cell cell;

  for_cell(cell, n->sons)
    unrelateImageNode(n, cell->value);
  for_cell(cell, n->parents)
    unrelateImageNode(cell->value, n);

  succeed;
}


static status
isSonNode(Node n, Node n2)
{ Cell cell;

  for_cell(cell, n->sons)
    DONE(isSonNode2(cell->value, n2));

  fail;
}


static int
isSonNode2(Node n, Node n2)
{ Cell cell;

  if ( n == n2 )
    succeed;
  for_cell(cell, n->sons)
    DONE(isSonNode2(cell->value, n2));

  fail;
}


static status
isParentNode(Node n, Node n2)		/* is n2 a parent of n? */
{ Cell cell;

  for_cell(cell, n->parents)
    DONE(isParentNode2(cell->value, n2));

  fail;
}

static status
isParentNode2(Node n, Node n2)
{ Cell cell;

  if ( n == n2 )
    succeed;
  for_cell(cell, n->parents)
    DONE(isParentNode2(cell->value, n2));

  fail;
}

static status
zoomNode(Node n)
{ return zoomTree(n->tree, n);
}

static status
unzoomNode(Node n)
{ return unzoomTree(n->tree);
}

static status
collapsedNode(Node n, Bool val)
{ if ( n->collapsed == val )
    succeed;

  if ( notNil(n->tree) )
  { assign(n, collapsed, val);
    updateDisplayedTree(n->tree);
    requestComputeTree(n->tree);
  } else
    assign(n, collapsed, val);  
  
  succeed;
}


static status
relateImagesNode(Node n)
{ Cell cell;

  for_cell(cell, n->parents)
    relateImageNode(cell->value, n);
  for_cell(cell, n->sons)
    relateImageNode(n, cell->value);

  succeed;
}


status
forAllNode(Node n, Code msg)
{ Cell cell, c2;

  TRY( forwardCode(msg, n, 0) );
  for_cell_save(cell, c2, n->sons)
    TRY( forAllNode(cell->value, msg) );

  succeed;
}


status
forSomeNode(Node n, Code msg)
{ Cell cell, c2;

  forwardCode(msg, n, 0);
  for_cell_save(cell, c2, n->sons)
    forSomeNode(cell->value, msg);

  succeed;
}


Node
getFindNodeNode(Node n, Graphical gr)
{ Cell cell;
  Node n2;

  if ( n->image == gr )
    answer( n );

  for_cell(cell, n->sons)
    if ( (n2 = getFindNodeNode(cell->value, gr)) )
      answer(n2);

  fail;
}


static Node
getFindNode(Node n, Code msg)
{ Cell cell;
  Node n2;

  if ( forwardCode(msg, n, 0) != FAIL )
    answer(n);

  for_cell(cell, n->sons)
    if ( (n2 = getFindNode(cell->value, msg)) )
      answer(n2);

  fail;
}


		/********************************
		*             VISUAL		*
		********************************/

static Chain
getContainsNode(Node n)
{ answer(answerObject(ClassChain, n->image, 0));
}


static Any
getContainedInNode(Node n)
{ answer(n->tree);
}


status
makeClassNode(Class class)
{ sourceClass(class, makeClassNode, __FILE__, "$Revision$");

  localClass(class, NAME_image, NAME_appearance, "graphical", NAME_get,
	     "Graphical image of the node");
  localClass(class, NAME_tree, NAME_organisation, "tree", NAME_get,
	     "Tree the node is in");
  localClass(class, NAME_level, NAME_hierarchy, "int", NAME_get,
	     "Distance from the root");
  localClass(class, NAME_sons, NAME_hierarchy, "chain", NAME_get,
	     "Sub nodes of this node");
  localClass(class, NAME_parents, NAME_hierarchy, "chain", NAME_get,
	     "Parent nodes of this node");
  localClass(class, NAME_collapsed, NAME_scroll, "bool", NAME_get,
	     "Sub nodes are invisible");
  localClass(class, NAME_displayed, NAME_scroll, "[bool]", NAME_get,
	     "Node is visible (@default used by update)");
  localClass(class, NAME_sonsSize, NAME_update, "int", NAME_none,
	     "Height of the combined sons");
  localClass(class, NAME_mySize, NAME_update, "int", NAME_none,
	     "Height of myself");
  localClass(class, NAME_computed, NAME_update, "{level,size,layout}*",
	     NAME_none,
	     "Stage of the layout process");

  termClass(class, "node", 1, NAME_image);
  delegateClass(class, NAME_image);

  storeMethod(class, NAME_collapsed, collapsedNode);
  storeMethod(class, NAME_image, imageNode);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "image=graphical",
	     "Create from graphical",
	     initialiseNode);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Unlink from tree",
	     unlinkNode);
  sendMethod(class, NAME_delete, NAME_edit, 0,
	     "Delete, reconnect sons to parents",
	     deleteNode);
  sendMethod(class, NAME_deleteTree, NAME_edit, 0,
	     "Delete node and all subnodes",
	     deleteTreeNode);
  sendMethod(class, NAME_isParent, NAME_hierarchy, 1, "node",
	     "Test if node is a parent (recursively)",
	     isParentNode);
  sendMethod(class, NAME_isSon, NAME_hierarchy, 1, "node",
	     "Test if node is a son (recursively)",
	     isSonNode);
  sendMethod(class, NAME_move, NAME_edit, 1, "node",
	     "Move argument to become a son of me",
	     moveNode);
  sendMethod(class, NAME_son, NAME_edit, 1, "node",
	     "Add argument to my sons",
	     sonNode);
  sendMethod(class, NAME_swap, NAME_edit, 1, "node",
	     "Swap images of two nodes",
	     swapNode);
  sendMethod(class, NAME_swapTree, NAME_edit, 1, "node",
	     "Swap positions of two entire trees",
	     swapTreeNode);
  sendMethod(class, NAME_zoom, NAME_scroll, 0,
	     "Zoom the tree to this node",
	     zoomNode);
  sendMethod(class, NAME_unzoom, NAME_scroll, 0,
	     "Unzoom tree back to its root",
	     unzoomNode);
  sendMethod(class, NAME_forAll, NAME_iterate, 1, "code",
	     "Run code on all sons; demand acceptance",
	     forAllNode);
  sendMethod(class, NAME_forSome, NAME_iterate, 1, "code",
	     "Run code on all sons",
	     forSomeNode);
  sendMethod(class, NAME_unrelate, NAME_edit, 1, "node",
	     "Delete (direct) relation to argument",
	     unrelateNode);
  sendMethod(class, NAME_moveAfter, NAME_edit, 1, "[node]*",
	     "Move node to be just after (below) arg",
	     moveAfterNode);
  sendMethod(class, NAME_computeLevel, NAME_internal, 2, "int", "[bool]",
	     "Recursively assign each node a <-level",
	     computeLevelNode);
  sendMethod(class, NAME_computeLayout, NAME_update, 3, "int", "int", "int",
	     "Recursively place each node (level, x, y)",
	     computeLayoutNode);


  getMethod(class, NAME_findNode, NAME_iterate, "node", 1, "graphical",
	    "Find node that has ->image graphical",
	    getFindNodeNode);
  getMethod(class, NAME_find, NAME_iterate, "node", 1, "code",
	    "Find node that accepts code",
	    getFindNode);
  getMethod(class, NAME_containedIn, DEFAULT, "tree", 0,
	    "Visual I'm contained in (tree)",
	    getContainedInNode);
  getMethod(class, NAME_contains, DEFAULT, "chain", 0,
	    "New chain with visuals I manage (holding <-image)",
	    getContainsNode);
  getMethod(class, NAME_convert, DEFAULT, "node", 1, "graphical",
	    "Convert graphical object",
	    getConvertNode);
  getMethod(class, NAME_computeSize, NAME_update, "int", 1, "int",
	    "Recursively compute the sub-tree-size (level)",
	    getComputeSizeNode);

  succeed;
}

