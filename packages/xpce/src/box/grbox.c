/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include "boxes.h"

static status
initialiseGrBox(GrBox grb, Graphical gr,
		Any align,		/* left, right or @nil */
		Rubber rubber)
{ if ( isDefault(align) )
    align = NAME_center;
  if ( isDefault(rubber) )
    rubber = NIL;

  assign(grb, graphical, gr);
  assign(grb, alignment, align);
  assign(grb, rubber,    rubber);

  ComputeGraphical(gr);
  assign(grb, width, gr->area->w);
  computeAscentDescentGrBox(grb);

  succeed;
}


		 /*******************************
		 *	      COMPUTE		*
		 *******************************/

status
computeGrBox(GrBox grb)
{ Graphical gr = grb->graphical;

  ComputeGraphical(gr);
  if ( isNil(grb->rubber) ||
       ( grb->rubber->stretch == ZERO &&
	 grb->rubber->shrink  == ZERO 
       ) )
  { DEBUG(NAME_grbox,
	  Cprintf("%s width %d --> %d\n",
		  pp(grb), valInt(grb->width), valInt(gr->area->w)));
    assign(grb, width, gr->area->w);	/* TBD */
  } else
  { DEBUG(NAME_grbox,
	  Cprintf("%s IGNORING width %d --> %d\n",
		  pp(grb), valInt(grb->width), valInt(gr->area->w)));
  }
  computeAscentDescentGrBox(grb);

  succeed;
}


status
computeAscentDescentGrBox(GrBox grb)
{ Graphical gr = grb->graphical;
  int h, ascent, descent;

  ComputeGraphical(gr);
  h = valInt(gr->area->h);

  if ( grb->alignment == NAME_top )
    ascent = 0;
  else if ( grb->alignment == NAME_bottom )
    ascent = h;
  else
    ascent = h/2;

  descent = h-ascent;
  if ( grb->ascent  != toInt(ascent) ||
       grb->descent != toInt(descent) )
  { assign(grb, ascent,  toInt(ascent));
    assign(grb, descent, toInt(descent));

    succeed;				/* changed */
  } else
    fail;				/* no change */
}


static status
alignmentGrBox(GrBox grb, Any alignment)
{ if ( grb->alignment != alignment )
  { assign(grb, alignment, alignment);
    computeAscentDescentGrBox(grb);
  }

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "graphical=graphical",
	  "alignment=[{top,center,bottom,left,right}]",
	  "rubber=[rubber]*"
	};

/* Instance Variables */

static vardecl var_grbox[] =
{ IV(NAME_graphical, "graphical", IV_GET,
     NAME_content, "Represented graphical object"),
  SV(NAME_alignment, "{top,center,bottom,left,right}", IV_GET|IV_STORE,
     alignmentGrBox,
     NAME_layout, "Alignment in paragraph")
};

/* Send Methods */

static senddecl send_grbox[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseGrBox,
     DEFAULT, "Create grbox from graphical, alignment and rubber"),
  SM(NAME_compute, 0, NULL, computeGrBox,
     NAME_update, "Compute <-graphical and update dimensions")
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

static Name grbox_termnames[] = { NAME_graphical };

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

