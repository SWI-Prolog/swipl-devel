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

