/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include "boxes.h"

static status computeLBox(LBox lb);

static status
initialiseLBox(LBox lb, Int w)
{ if ( isDefault(w) )
    w = getClassVariableValueObject(lb, NAME_width);

  obtainClassVariablesObject(lb);
  initialiseDevice((Device)lb);

  assign(lb->area, w, w);

  succeed;
}

		 /*******************************
		 *	      COMPUTE		*
		 *******************************/

static status
PlaceLBox(LBox lp, Graphical gr, Int x, Int y, Int w)
{ DEBUG(NAME_lbox,
	Cprintf("Placing %s on %s at %d,%d (width = %s)\n",
		pp(gr), pp(lp), valInt(x), valInt(y), pp(w)));

  ComputeGraphical(gr);
  if ( gr->area->x != x || gr->area->y != y ||
       (notDefault(w) && gr->area->w != w) )
    setGraphical(gr, x, y, w, DEFAULT);

  succeed;
}


static Graphical
getLabelItem(Graphical item)
{ Chain ch;

  if ( (ch = getAllHypersObject(item, OFF)) )
  { Cell cell;

    for_cell(cell, ch)
    { Hyper h = cell->value;

      if ( h->to == item && h->backward_name == NAME_label &&
	   instanceOfObject(h->from, ClassGraphical) )
	answer(h->from);		/* I'm an item */
      if ( h->from == item && h->forward_name == NAME_item )
	fail;				/* I'm a label */
    }
  }

  return NIL;				/* I have no label */
}


static status
computeLBox(LBox lb)
{ if ( notNil(lb->request_compute) )
  { Cell cell;
    int lm   = valInt(lb->left_margin);
    int rm   = valInt(lb->right_margin);
    int isep = valInt(lb->item_sep);
    int lsep = valInt(lb->label_sep);
    int cy   = valInt(lb->top_sep);
    int iw   = valInt(lb->area->w) - lm - rm;

    if ( iw < 0 )
      goto out;

    for_cell(cell, lb->graphicals)
    { Graphical label;
      Graphical item = cell->value;

      if ( (label = getLabelItem(item)) ) /* i.e. I am an item */
      { int lh;

	if ( notNil(label) )
	{ ComputeGraphical(label);

	  PlaceLBox(lb, label,
		    toInt(lm - lsep - valInt(label->area->w)),
		    toInt(cy),
		    lb->label_width);
	  lh = valInt(label->area->h);
	} else
	  lh = 0;

	PlaceLBox(lb, cell->value, toInt(lm), toInt(cy), toInt(iw));

	cy += max(lh, valInt(item->area->h));
	cy += isep;
      }
    }

    cy -= isep;				/* correct for last */
    cy += valInt(lb->top_sep);
    
    if ( valInt(lb->area->h) != cy )
    { CHANGING_GRAPHICAL(lb,
      { assign(lb->area, h, toInt(cy));
      });
    }

  out:
    assign(lb, request_compute, NIL);
  }

  succeed;
}


static Int
getItemWidthLBox(LBox lb)
{ int iw = valInt(lb->area->w) -
           valInt(lb->left_margin) -
	   valInt(lb->right_margin);

  if ( iw < 0 )
    iw = 0;
  
  answer(toInt(iw));
}

		 /*******************************
		 *	      GEOMETRY		*
		 *******************************/

static status
geometryLBox(LBox lb, Int x, Int y, Int w, Int h)
{ if ( notDefault(w) && w != lb->area->w )
  { CHANGING_GRAPHICAL(lb,
		       assign(lb->area, w, w);
		       assign(lb, request_compute, DEFAULT);
		       computeLBox(lb));
  }

  return geometryDevice((Device)lb, x, y, DEFAULT, DEFAULT);
}

		 /*******************************
		 *	       ITEM		*
		 *******************************/

static status
appendLBox(LBox lb, Graphical label, Graphical item)
{ if ( isDefault(label) )
    label = get(lb, NAME_newLabel, 0);
  if ( isDefault(item) )
    item  = get(lb, NAME_newItem, 0);

  if ( item && (item = checkType(item, TypeGraphical, lb)) )
  { if ( label && (label = checkType(label, TypeGraphical, lb)) )
    { send(lb, NAME_display, label, 0);
      send(lb, NAME_display, item,  0);
      newObject(ClassChainHyper, label, item, NAME_item, NAME_label, 0);
    } else
      send(lb, NAME_display, item,  0);

    succeed;
  }

  fail;					/* error? */
}


static Graphical
getNewLabelLBox(LBox lb)
{ fail;
}


static Graphical
getNewItemLBox(LBox lb)
{ answer(answerObject(ClassParBox, getItemWidthLBox(lb), 0));
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_append[] = 
	{ "label=[graphical]", "item=[graphical]" };
static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };

/* Instance Variables */

static vardecl var_lbox[] =
{ IV(NAME_leftMargin, "0..", IV_GET,
     NAME_layout, "Distance left of paragraphs"),
  IV(NAME_rightMargin, "0..", IV_GET,
     NAME_layout, "Distance right of paragraphs"),
  IV(NAME_topSep, "0..", IV_GET,
     NAME_layout, "Vertical skip around environment"),
  IV(NAME_itemSep, "0..", IV_GET,
     NAME_layout, "Vertical skip between items"),
  IV(NAME_labelSep, "0..", IV_GET,
     NAME_layout, "Horizontal skip from label to paragraph"),
  IV(NAME_labelWidth, "[0..]", IV_GET,
     NAME_layout, "Width of label-box")
};

/* Send Methods */

static senddecl send_lbox[] =
{ SM(NAME_initialise, 1, "width=[0..]", initialiseLBox,
     DEFAULT, "Create lbox of specified width"),
  SM(NAME_compute, 0, NULL, computeLBox,
     DEFAULT, "Recompute layout"),
  SM(NAME_geometry, 4, T_geometry, geometryLBox,
     DEFAULT, "Change lbox width"),
  SM(NAME_append,  2, T_append, appendLBox,
     DEFAULT, "Append label and item")
};

/* Get Methods */

static getdecl get_lbox[] =
{ GM(NAME_newLabel, 0, "graphical*", NULL, getNewLabelLBox,
     NAME_organisation, "Create label for new item"),
  GM(NAME_newItem,  0, "graphical*", NULL, getNewItemLBox,
     NAME_organisation, "Create new item body"),
  GM(NAME_itemWidth,  0, "int", NULL, getItemWidthLBox,
     NAME_layout, "Width of an item")
};

/* Resources */

static classvardecl rc_lbox[] =
{
  RC(NAME_leftMargin,	NULL,  "40",	   NULL),
  RC(NAME_rightMargin,	NULL,  "20",	   NULL),
  RC(NAME_topSep,	NULL,  "5",	   NULL),
  RC(NAME_itemSep,	NULL,  "10",	   NULL),
  RC(NAME_labelSep,	NULL,  "5",	   NULL),
  RC(NAME_labelWidth,	NULL,  "@default", NULL),
  RC(NAME_width,	"0..", "0",	   "Default initial width")
};

/* Class Declaration */

ClassDecl(lbox_decls,
          var_lbox, send_lbox, get_lbox, rc_lbox,
          0, NULL,
          "$Rev$");


status
makeClassLBox(Class class)
{ return declareClass(class, &lbox_decls);
}
