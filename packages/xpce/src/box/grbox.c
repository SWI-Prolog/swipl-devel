/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include "boxes.h"

static status computeGrBox(GrBox grb);

static status
initialiseGrBox(GrBox grb, Graphical gr, Any baseline, Rubber rubber)
{ if ( isDefault(baseline) )
    baseline = NAME_bottom;
  if ( isDefault(rubber) )
    rubber = NIL;

  assign(grb, graphical, gr);
  assign(grb, baseline,  baseline);
  assign(grb, rubber,    rubber);

  return computeGrBox(grb);
}


		 /*******************************
		 *	      COMPUTE		*
		 *******************************/

static status
computeGrBox(GrBox grb)
{ Graphical gr = grb->graphical;

  ComputeGraphical(gr);
  assign(grb, width, gr->area->w);
  computeAscentDescentGrBox(grb);

  succeed;
}


status
computeAscentDescentGrBox(GrBox grb)
{ Graphical gr = grb->graphical;
  int h, ascent, descent;

  ComputeGraphical(gr);
  h = valInt(gr->area->h);

  if ( grb->baseline == NAME_top )
    ascent = 0;
  else if ( grb->baseline == NAME_bottom )
    ascent = h;
  else if ( grb->baseline == NAME_center )
    ascent = h/2;
  else
    ascent = valInt(grb->baseline);

  descent = h-ascent;
  if ( grb->ascent  != toInt(ascent) ||
       grb->descent != toInt(descent) )
  { assign(grb, ascent,  toInt(ascent));
    assign(grb, descent, toInt(descent));

    succeed;				/* changed */
  } else
    fail;				/* no change */
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "graphical=graphical",
	  "baseline=[{top,center,bottom}|int]",
	  "rubber=[rubber]*"
	};

/* Instance Variables */

static vardecl var_grbox[] =
{ IV(NAME_graphical, "graphical", IV_GET,
     NAME_content, "Represented graphical object"),
  IV(NAME_baseline, "{top,center,bottom}|int", IV_GET,
     NAME_layout, "Location of the baseline"),
  IV(NAME_alignment, "{left,right}*", IV_GET,
     NAME_layout, "Alignment in paragraph")
};

/* Send Methods */

static senddecl send_grbox[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseGrBox,
     DEFAULT, "Create grbox from graphical and baseline")
};

/* Get Methods */

#define get_grbox NULL
/*
static getdecl get_grbox[] =
{ 
};
*/

/* Resources */

#define rc_grbox NULL
/*
static classvardecl rc_grbox[] =
{ 
};
*/

/* Class Declaration */

static Name grbox_termnames[] = { NAME_garphical };

ClassDecl(grbox_decls,
          var_grbox, send_grbox, get_grbox, rc_grbox,
          1, grbox_termnames,
          "$Rev$");


status
makeClassGrBox(Class class)
{ declareClass(class, &grbox_decls);
  delegateClass(class, NAME_graphical);

  succeed;
}

