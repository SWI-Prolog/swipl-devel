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

  assign(lb->area, w, width);

  succeed;
}

		 /*******************************
		 *	      COMPUTE		*
		 *******************************/

static status
computeLBox(LBox lb)
{ if ( notNil(lb->request_compute) )
  { 


    assign(lb, request_compute, NIL);
  }

  succeed;
}


static Int
getItemWidthLBox(LBox lb)
{ int iw = valInt(lb->area->width) -
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
{
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

/* Instance Variables */

static vardecl var_lbox[] =
{ IV(NAME_left_margin, "0..", IV_GET,
     NAME_layout, "Distance left of paragraphs"),
  IV(NAME_right_margin, "0..", IV_GET,
     NAME_layout, "Distance right of paragraphs"),
  IV(NAME_top_sep, "0..", IV_GET,
     NAME_layout, "Vertical skip around environment"),
  IV(NAME_item_sep, "0..", IV_GET,
     NAME_layout, "Vertical skip between items"),
  IV(NAME_label_sep, "0..", IV_GET,
     NAME_layout, "Horizontal skip from label to paragraph"),
  IV(NAME_label_width, "0..", IV_GET,
     NAME_layout, "Width of label-box")
};

/* Send Methods */

static senddecl send_lbox[] =
{ SM(NAME_initialise, 0, NULL, initialiseLBox,
     DEFAULT, "Create lbox"),
  SM(NAME_compute, 0, NULL, computeLBox,
     DEFAULT, "Recompute layout")
};

/* Get Methods */

#define get_lbox NULL
/*
static getdecl get_lbox[] =
{ 
};
*/

/* Resources */

static classvardecl rc_lbox[] =
{
  RC(NAME_left_margin,	NULL,  "20",  NULL),
  RC(NAME_right_margin,	NULL,  "20",  NULL),
  RC(NAME_top_sep,	NULL,  "10",  NULL),
  RC(NAME_item_sep,	NULL,  "10",  NULL),
  RC(NAME_label_sep,	NULL,  "5",   NULL),
  RC(NAME_label_width,	NULL,  "15",  NULL),
  RC(NAME_width,	"0..", "500", "Default initial width")
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

