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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Rubber levels:

	0	not used (fixed?)
	1	Spacing
	2	hfil (left/center/right alignment)
	3	hfill
	4	hfilll
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
initialiseRubber(Rubber r, Int level, Int stretch, Int shrink, Name linebreak)
{ if ( isDefault(level) )
    level = ONE;
  if ( isDefault(stretch) )		/* resource? */
    stretch = ZERO;
  if ( isDefault(shrink) )
    shrink = ZERO;
  if ( isDefault(linebreak) )
    linebreak = NIL;

  assign(r, stretch,   stretch);
  assign(r, shrink,    shrink);
  assign(r, linebreak, linebreak);
  assign(r, level,     level);
  assign(r, natural,   DEFAULT);

  succeed;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "level=[1..]",
	  "stretch=[0..]",
	  "shrink=[0..]",
	  "linebreak=[{allow,force}]*"
	};

/* Instance Variables */

static vardecl var_rubber[] =
{ IV(NAME_stretch,  "0..", IV_GET,
     NAME_rubber, "Ease to get bigger"),
  IV(NAME_shrink, "0..", IV_GET,
     NAME_rubber, "Ease to get smaller"),
  IV(NAME_level, "1..", IV_GET,
     NAME_rubber, "Level of the rubber (TeX hfil/hfill/hfilll)"),
  IV(NAME_natural, "[int]", IV_BOTH,
     NAME_dimension, "Natrual size"),
  IV(NAME_minimum, "int*", IV_BOTH,
     NAME_dimension, "Mimimum size"),
  IV(NAME_maximum, "int*", IV_BOTH,
     NAME_dimension, "Maximum size"),
  IV(NAME_linebreak, "{allow,force}*", IV_GET,
     NAME_layout, "Can be use this box as a linebreak")
};

/* Send Methods */

static senddecl send_rubber[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseRubber,
     DEFAULT, "Create rubber from stretch, shrink and linebreak")
};

/* Get Methods */

#define get_rubber NULL
/*
static getdecl get_rubber[] =
{ 
};
*/

/* Resources */

#define rc_rubber NULL
/*
static classvardecl rc_rubber[] =
{ 
};
*/

/* Class Declaration */

static Name rubber_termnames[] = { NAME_stretch, NAME_shrink };

ClassDecl(rubber_decls,
          var_rubber, send_rubber, get_rubber, rc_rubber,
          2, rubber_termnames,
          "$Rev$");


status
makeClassRubber(Class class)
{ declareClass(class, &rubber_decls);

  globalObject(NAME_spaceRubber,
	       ClassRubber,
	       toInt(1),
	       toInt(100),
	       toInt(1),
	       NAME_allow,
	       EAV);

  succeed;
}

