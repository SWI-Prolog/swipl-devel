/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include "boxes.h"

status
initialiseHBox(HBox hb, Int width, Int ascent, Int descent, Rubber rubber)
{ if ( isDefault(rubber) )		/* resource? */
    rubber = NIL;
  if ( isDefault(width) )
    width = ZERO;
  if ( isDefault(ascent) )
    ascent = ZERO;
  if ( isDefault(descent) )
    descent = ZERO;

  assign(hb, width,   width);
  assign(hb, ascent,  ascent);
  assign(hb, descent, descent);
  assign(hb, rubber, rubber);

  succeed;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "width=[int]",
	  "ascent=[int]", "descent=[int]",
	  "rubber=[rubber]*"
	};

/* Instance Variables */

static vardecl var_hbox[] =
{ IV(NAME_width,  "int", IV_GET,
     NAME_dimension, "Natural width of content"),
  IV(NAME_ascent, "0..", IV_GET,
     NAME_dimension, "Heigth above baseline"),
  IV(NAME_descent, "0..", IV_GET,
     NAME_dimension, "Depth below baseline"),
  IV(NAME_rubber, "rubber*", IV_GET,
     NAME_layout, "Stretch/shrinkability")
};

/* Send Methods */

static senddecl send_hbox[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseHBox,
     DEFAULT, "Create hbox from dimensions")
};

/* Get Methods */

#define get_hbox NULL
/*
static getdecl get_hbox[] =
{ 
};
*/

/* Resources */

#define rc_hbox NULL
/*
static classvardecl rc_hbox[] =
{ 
};
*/

/* Class Declaration */

static Name hbox_termnames[] = \
	{ NAME_width, NAME_ascent, NAME_descent, NAME_rubber };

ClassDecl(hbox_decls,
          var_hbox, send_hbox, get_hbox, rc_hbox,
          4, hbox_termnames,
          "$Rev$");


status
makeClassHBox(Class class)
{ return declareClass(class, &hbox_decls);
}

