/*  $Id$ $

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status	updateHandlesTree(Tree);
static status	rootTree(Tree t, Node root);


static Any div_h_2;			/* h/2 */
static Any div_w_2;			/* h/2 */


static status
initialiseTree(Tree t, Node node)
{ if ( isDefault(node) )
    node = NIL;

  initialiseFigure((Figure) t);
  assign(t, auto_layout, 	ON);
  assign(t, link, 		newObject(ClassLink,
					  NAME_parent, NAME_son, 0));
  assign(t, rootHandlers,	newObject(ClassChain, 0));
  assign(t, leafHandlers,	newObject(ClassChain, 0));
  assign(t, nodeHandlers,	newObject(ClassChain, 0));
  assign(t, collapsedHandlers,	newObject(ClassChain, 0));

  obtainClassVariablesObject(t);
    
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
{ if ( isNil(root) )
  { if ( notNil(t->root) )
    { setFlag(t, F_FREEING);		/* HACK! */
      freeObject(t->root);
      clearFlag(t, F_FREEING);
      assign(t, root, NIL);
      assign(t, displayRoot, NIL);
      clearDevice((Device)t);
    }
  } else
  { if ( notNil(t->root) )
      rootTree(t, NIL);

    displayTree(t, root);
    assign(t, root, root);
    assign(t, displayRoot, root);
  }

  return requestComputeTree(t);
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

		 /*******************************
		 *	      REPAINT		*
		 *******************************/

static int
leading_x_tree(Tree t)
{ Node n;

  if ( notNil((n=t->displayRoot)) && t->direction == NAME_list )
  { Image img = NULL;

    if ( n->collapsed == ON )
      img = getClassVariableValueObject(t, NAME_collapsedImage);
    else if ( n->collapsed == OFF )
      img = getClassVariableValueObject(t, NAME_expandedImage);

    if ( img && notNil(img) )
    { int lg = valInt(t->levelGap)/2;
      int iw2 = (valInt(img->size->w)+1)/2;

      return lg+iw2;
    }
  }

  return 0;
}


static void
RedrawAreaNode(Node node, Image cimg, Image eimg)
{ Graphical img = node->image;
  Tree t = node->tree;
  int lg = valInt(t->levelGap)/2;
  Node lastnode;
  int ly = valInt(img->area->y) + valInt(img->area->h)/2;
  int lx = valInt(img->area->x);
  Image i = NULL;

  if ( node->collapsed == OFF && eimg )
    i = eimg;
  else if ( node->collapsed == ON && cimg )
    i = cimg;

  if ( i || node != t->displayRoot )
    r_line(lx-lg, ly, lx, ly);	/* line to parent */

  if ( i )
  { int iw = valInt(i->size->w);
    int ih = valInt(i->size->h);

    r_image(i, 0, 0, lx-lg-(iw+1)/2, ly-(ih+1)/2, iw, ih, OFF);
  }

  if ( notNil(node->sons) && node->collapsed != ON &&
       (lastnode = getTailChain(node->sons)) )	/* I have sons */
  { int fy	   = valInt(getBottomSideGraphical(img));
    Graphical last = lastnode->image;
    int	ty	   = valInt(last->area->y) + valInt(last->area->h)/2;
    int lx	   = valInt(img->area->x) + lg;
    Cell cell;

    r_line(lx, fy, lx, ty);
    
    for_cell(cell, node->sons)
      RedrawAreaNode(cell->value, cimg, eimg);
  }
}


static status
RedrawAreaTree(Tree t, Area area)
{ device_draw_context ctx;
  Any bg, obg;

  if ( notNil(bg = RedrawBoxFigure((Figure)t, area)) )
    obg = r_background(bg);
  else
    obg = NULL;

  if ( EnterRedrawAreaDevice((Device)t, area, &ctx) )
  { Cell cell;

					/* list-style connection lines */
    if ( t->direction == NAME_list && notNil(t->displayRoot) )
    { Line proto = t->link->line;

      if ( proto->pen != ZERO )
      { Colour old = NULL;
	Image cimg = getClassVariableValueObject(t, NAME_collapsedImage);
	Image eimg = getClassVariableValueObject(t, NAME_expandedImage);

	r_thickness(valInt(proto->pen));
	r_dash(proto->texture);
	if ( notDefault(proto->colour) )
	  old = r_colour(proto->colour);
    
	RedrawAreaNode(t->displayRoot, cimg, eimg);
	if ( old )
	  r_colour(old);
      }
    }

					/* from class device (the nodes) */
    for_cell(cell, t->graphicals)
    { Graphical gr = cell->value;

      if ( gr->displayed == ON && overlapArea(area, gr->area) )
	RedrawArea(gr, area);
    }

    ExitRedrawAreaDevice((Device)t, area, &ctx);
  }

  RedrawAreaGraphical(t, area);	/* selection and orther generic stuff*/
  
  if ( obg )
    r_background(obg);

  succeed;
}



		/********************************
		*             EVENTS		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
When displayed in `list' mode, return the node with a [+] or [-] sign to
indicate a expand/collapse request.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Node
getNodeToCollapseOrExpand(Node node, int x, int y, Image cimg, Image eimg)
{ Graphical img = node->image;
  Tree t = node->tree;
  int lg = valInt(t->levelGap)/2;
  Node lastnode;
  Image i = NULL;

  if ( node->collapsed == OFF && eimg )
    i = eimg;
  else if ( node->collapsed == ON && cimg )
    i = cimg;

  if ( i )
  { int ly = valInt(img->area->y) + valInt(img->area->h)/2;
    int lx = valInt(img->area->x);
    int iw = valInt(i->size->w);
    int ih = valInt(i->size->h);
    int iw2 = (ih+1)/2;
    int ih2 = (iw+1)/2;
    int x0 = lx-lg-iw2;
    int y0 = ly-ih2;

    if ( x >= x0 && x <= x0+iw && y >= y0 && y <= y0+ih )
      return node;
  }

  if ( notNil(node->sons) && node->collapsed != ON &&
       (lastnode = getTailChain(node->sons)) )	/* I have sons */
  { Cell cell;

    for_cell(cell, node->sons)
    { Node n2;

      if ( (n2=getNodeToCollapseOrExpand(cell->value, x, y, cimg, eimg)) )
	return n2;
    }
  }

  fail;
}


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

  if ( t->direction == NAME_list &&
       notNil(t->displayRoot) &&
       isAEvent(ev, NAME_msLeftDown) )
  { Image cimg = getClassVariableValueObject(t, NAME_collapsedImage);
    Image eimg = getClassVariableValueObject(t, NAME_expandedImage);
    Int x, y;
    Node n;

    get_xy_event(ev, t, OFF, &x, &y);

    if ( (n=getNodeToCollapseOrExpand(t->displayRoot,
				      valInt(x), valInt(y),
				      cimg, eimg)) )
    { send(n, NAME_collapsed, n->collapsed == ON ? OFF : ON, 0);
      succeed;
    }
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
{ if ( n->tree != t )
  { Cell cell;

    if ( notNil(n->tree) )
      return errorPce(t, NAME_alreadyShown, n, n->tree);

    send(n->image, NAME_handle,     t->sonHandle, 0);
    send(n->image, NAME_handle,     t->parentHandle, 0);

    assign(n, tree, t);
    for_cell(cell, n->parents)
      relateImageNode(cell->value, n);
    for_cell(cell, n->sons)
      displayTree(t, cell->value);
  }
  
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
computeBoundingBoxFigureTree(Tree t)
{ if ( t->badBoundingBox == ON )
  { Area a = t->area;
    Int ox = a->x, oy = a->y, ow = a->w, oh = a->h;
    int ex = leading_x_tree(t);

    computeBoundingBoxDevice((Device) t);

    if ( t->border != ZERO )
      increaseArea(t->area, t->border);
    if ( ex )
    { assign(a, x, toInt(valInt(a->x)-ex));
      assign(a, w, toInt(valInt(a->w)+2*ex));
    }

    if ( ox != a->x || oy != a->y || ow != a->w || oh != a->h )
      changedAreaGraphical((Graphical)t, ox, oy, ow, oh);
  }

  succeed;
}


static status
computeFigureTree(Tree t)
{ if ( notNil(t->request_compute) )
  { if ( t->pen != ZERO || notNil(t->background) )
    { CHANGING_GRAPHICAL(t, { computeGraphicalsDevice((Device) t);
			      computeBoundingBoxFigureTree(t);
			    });
    } else
    { computeGraphicalsDevice((Device) t);
      computeBoundingBoxFigureTree(t);
    }

    assign(t, request_compute, NIL);
  }

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

    computeFigureTree(t);
  }

  succeed;
}


static status
layoutTree(Tree t)
{ int ex;
  
  if ( isNil(t->displayRoot) )
    succeed;

  ex = leading_x_tree(t);

  if ( send(t->displayRoot, NAME_computeLevel, ZERO, 0) &&
       get(t->displayRoot, NAME_computeSize, ZERO, 0) &&
       send(t->displayRoot, NAME_computeLayout, ZERO, toInt(ex), ZERO, 0) )
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

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_tree[] =
{ IV(NAME_root, "node*", IV_GET,
     NAME_nodes, "Root-node of the tree"),
  IV(NAME_displayRoot, "node*", IV_GET,
     NAME_scroll, "Root of visible subtree"),
  SV(NAME_autoLayout, "bool", IV_GET|IV_STORE, autoLayoutTree,
     NAME_layout, "Enforce automatic layout?"),
  SV(NAME_levelGap, "int", IV_GET|IV_STORE, levelGapTree,
     NAME_layout, "Distance between levels"),
  SV(NAME_neighbourGap, "int", IV_GET|IV_STORE, neighbourGapTree,
     NAME_layout, "Distance between neighbours"),
  SV(NAME_linkGap, "int", IV_GET|IV_STORE, linkGapTree,
     NAME_layout, "Distance between line and image"),
  SV(NAME_direction, "{horizontal,vertical,list}", IV_GET|IV_STORE,
     directionTree,
     NAME_layout, "Layout of the tree"),
  IV(NAME_link, "link", IV_GET,
     NAME_layout, "Generic connection between graphicals"),
  IV(NAME_parentHandle, "handle", IV_GET,
     NAME_relation, "Connection point to parent"),
  IV(NAME_sonHandle, "handle", IV_GET,
     NAME_relation, "Connection point to sons"),
  IV(NAME_rootHandlers, "chain", IV_GET,
     NAME_event, "Event handlers for root"),
  IV(NAME_leafHandlers, "chain", IV_GET,
     NAME_event, "Event handlers for leafs"),
  IV(NAME_nodeHandlers, "chain", IV_GET,
     NAME_event, "Event handlers for any type of node"),
  IV(NAME_collapsedHandlers, "chain", IV_GET,
     NAME_event, "Event handlers for collapsed nodes")
};

/* Send Methods */

static senddecl send_tree[] =
{ SM(NAME_compute, 0, NULL, computeTree,
     DEFAULT, "Recompute the tree layout"),
  SM(NAME_event, 1, "event", eventTree,
     DEFAULT, "Process an event"),
  SM(NAME_initialise, 1, "root=[node]*", initialiseTree,
     DEFAULT, "Create a tree from a root node"),
  SM(NAME_unlink, 0, NULL, unlinkTree,
     DEFAULT, "Removes all the nodes"),
  SM(NAME_collapsedHandler, 1, "recogniser", collapsedHandlerTree,
     NAME_event, "Add recogniser for collapsed node"),
  SM(NAME_leafHandler, 1, "recogniser", leafHandlerTree,
     NAME_event, "Add recogniser for leaf node"),
  SM(NAME_nodeHandler, 1, "recogniser", nodeHandlerTree,
     NAME_event, "Add recogniser for any node node"),
  SM(NAME_rootHandler, 1, "recogniser", rootHandlerTree,
     NAME_event, "Add recogniser for root node"),
  SM(NAME_forAll, 1, "code", forAllTree,
     NAME_iterate, "Run code on all nodes (demand acceptance)"),
  SM(NAME_forSome, 1, "code", forSomeTree,
     NAME_iterate, "Run code on all nodes"),
  SM(NAME_root, 1, "node*", rootTree,
     NAME_nodes, "Set root node"),
  SM(NAME_unzoom, 0, NULL, unzoomTree,
     NAME_scroll, "Make the visible root the real root"),
  SM(NAME_zoom, 1, "node", zoomTree,
     NAME_scroll, "Zoom to a particular node"),
  SM(NAME_layout, 0, NULL, layoutTree,
     NAME_update, "Recompute layout"),
  SM(NAME_DrawPostScript, 0, NULL, drawPostScriptTree,
     NAME_postscript, "Create PostScript")
};

/* Get Methods */

static getdecl get_tree[] =
{ GM(NAME_contains, 0, "chain", NULL, getContainsTree,
     DEFAULT, "New chain with nodes I manage")
};

/* Resources */

static classvardecl rc_tree[] =
{ RC(NAME_direction, "name", "horizontal",
     "Default style {horizontal,vertical,list}"),
  RC(NAME_levelGap, "int", "50",
     "Gap between levels"),
  RC(NAME_linkGap, "int", "2",
     "Gap between link-line and image"),
  RC(NAME_neighbourGap, "int", "0",
     "Gap between neighbours"),
  RC(NAME_expandedImage, "image*", "@tree_expanded_image",
     "Image left of node if it is expanded <-direction: list"),
  RC(NAME_collapsedImage, "image*", "@tree_collapsed_image",
     "Image left of node if it is collapsed <-direction: list")
};

/* Class Declaration */

static Name tree_termnames[] = { NAME_root };

ClassDecl(tree_decls,
          var_tree, send_tree, get_tree, rc_tree,
          1, tree_termnames,
          "$Rev$");

status
makeClassTree(Class class)
{ declareClass(class, &tree_decls);
  delegateClass(class, NAME_root);
  setRedrawFunctionClass(class, RedrawAreaTree);

  succeed;
}
