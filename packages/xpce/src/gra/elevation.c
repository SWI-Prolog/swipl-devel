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

#include <h/kernel.h>
#include <h/graphics.h>

static HashTable ElevationTable;	/* @elevations */

static status
initialiseElevation(Elevation e, Any name,
		    Int height, Any colour, Any relief, Any shadow,
		    Name kind, Any bg)
{ if ( isDefault(name) )
    name = NIL;
  
  assign(e, name,       name);
  assign(e, background, bg);

  if ( isDefault(height) && isInteger(name) )
    height = name;

  if ( notDefault(colour) ) assign(e, colour,     colour);
  if ( notDefault(relief) ) assign(e, relief,     relief);
  if ( notDefault(shadow) ) assign(e, shadow,     shadow);
  if ( notDefault(kind) )   assign(e, kind,       kind);
  if ( notDefault(height) ) assign(e, height,     height);

  obtainClassVariablesObject(e);

  if ( notNil(name) )
    appendHashTable(ElevationTable, name, e);

  succeed;
}


static Elevation
getLookupElevation(Any receiver, Any name,
		   Int height, Any colour, Any relief, Any shadow,
		   Name kind, Any bg)
{ Elevation e = getMemberHashTable(ElevationTable, name);

  if ( e &&
       isName(name) &&
       (isDefault(height) || height == e->height) &&
       (isDefault(colour) || colour == e->colour) &&
       (isDefault(bg)     || bg     == e->background) &&
       (isDefault(relief) || relief == e->relief) &&
       (isDefault(shadow) || shadow == e->shadow) &&
       (isDefault(kind)   || kind   == e->kind) )
    answer(e);
  if ( e &&
       isInteger(name) &&
       isDefault(height) &&
       isDefault(colour) &&
       isDefault(bg) &&
       isDefault(relief) &&
       isDefault(shadow) &&
       isDefault(kind) )
    answer(e);

  fail;
}


static Elevation
getConvertElevation(Any receiver, Any val)
{ Int i;
  Elevation e;

  if ( (e= getLookupElevation(receiver, val,
			      DEFAULT, DEFAULT, DEFAULT,
			      DEFAULT, DEFAULT, DEFAULT)) )
    return e;

  if ( (i = toInteger(val)) )
    answer(answerObject(ClassElevation, i, i, EAV));

  fail;
}


static status
unlinkElevation(Elevation e)
{ if ( notNil(e->name) )
    deleteHashTable(ElevationTable, e->name);

  succeed;
}


		 /*******************************
		 *	    ATTRIBUTES		*
		 *******************************/

static status
attributeElevation(Elevation e, Name att, Any val)
{ if ( notNil(e->name) )
    return errorPce(e, NAME_readOnly);

  return slotObject(e, att, val);
}


static status
heightElevation(Elevation e, Int height)
{ return attributeElevation(e, NAME_height, height);
}


static status
colourElevation(Elevation e, Any colour)
{ return attributeElevation(e, NAME_colour, colour);
}


static status
backgroundElevation(Elevation e, Any colour)
{ return attributeElevation(e, NAME_background, colour);
}


static status
reliefElevation(Elevation e, Any colour)
{ return attributeElevation(e, NAME_relief, colour);
}


static status
shadowElevation(Elevation e, Any colour)
{ return attributeElevation(e, NAME_shadow, colour);
}


static status
kindElevation(Elevation e, Name kind)
{ return attributeElevation(e, NAME_kind, kind);
}


Elevation
getModifyElevation(Elevation e, Name att, Any val)
{ if ( notNil(e->name) )
  { Int height = e->height;
    Any colour = e->colour;
    Any relief = e->relief;
    Any shadow = e->shadow;
    Any bg     = e->background;
    Name kind  = e->kind;

    if      ( att == NAME_height ) height = val;
    else if ( att == NAME_colour ) colour = val;
    else if ( att == NAME_relief ) relief = val;
    else if ( att == NAME_shadow ) shadow = val;
    else if ( att == NAME_kind   ) kind   = val;
    else if ( att == NAME_background ) bg = val;
    
    answer(answerObject(ClassElevation, NIL, height, colour,
			relief, shadow, kind, bg, EAV));
  } else
  { attributeElevation(e, att, val);
    answer(e);
  }
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_modify[] =
        { "attribute={height,colour,relief,shadow,kind,background}",
	  "value=any" };
static char *T_initialise[] =
{ "name=[name|int]*", "height=[int]",
  "colour=[{hilited,reduced}|colour|pixmap]",
  "relief=[colour|pixmap]", "shadow=[colour|pixmap]",
  "kind=[{3d,shadow}]", "background=[{reduced}|colour|pixmap]" };
static char T_cbg[] = "[{hilited,reduced}|colour|pixmap]";

/* Instance Variables */

static vardecl var_elevation[] =
{ IV(NAME_name, "name|int*", IV_GET,
     NAME_name, "Name for reuse"),
  SV(NAME_height, "int", IV_GET|IV_STORE, heightElevation,
     NAME_appearance, "Height above the surface"),
  SV(NAME_colour, T_cbg, IV_GET|IV_STORE, colourElevation,
     NAME_appearance, "Colour/pixmap to paint the `top'"),
  SV(NAME_background, T_cbg, IV_GET|IV_STORE, backgroundElevation,
     NAME_appearance, "Colour/pixmap when area is lowered"),
  SV(NAME_relief, "[colour|pixmap]", IV_GET|IV_STORE, reliefElevation,
     NAME_appearance, "Colour/pixmap used at `light' side"),
  SV(NAME_shadow, "[colour|pixmap]", IV_GET|IV_STORE, shadowElevation,
     NAME_appearance, "Colour/pixmap used at `dark' side"),
  SV(NAME_kind, "{3d,shadow}", IV_GET|IV_STORE, kindElevation,
     NAME_appearance, "How the elevation is realised")
};

/* Send Methods */

static senddecl send_elevation[] =
{ SM(NAME_initialise, 7, T_initialise, initialiseElevation,
     DEFAULT, "Create elevation from name, height and colours"),
  SM(NAME_unlink, 0, NULL, unlinkElevation,
     DEFAULT, "Delete from @elevations")
};

/* Get Methods */

static getdecl get_elevation[] =
{ GM(NAME_convert, 1, "elevation", "name|int", getConvertElevation,
     DEFAULT, "Convert name to object (reuse) or int to height"),
  GM(NAME_lookup, 7, "elevation", T_initialise, getLookupElevation,
     DEFAULT, "Lookup from @elevations"),
  GM(NAME_modify, 2, "elevation", T_modify, getModifyElevation,
     NAME_appearance, "Return modified (new) elevation object")
};

/* Resources */

static classvardecl rc_elevation[] =
{ RC(NAME_colour, T_cbg, "@default",
     "Colour of the top"),
  RC(NAME_height, "int", "2",
     "Default height of the evaluation"),
  RC(NAME_kind, "{3d,shadow}", "3d",
     "How the elevation is realised"),
  RC(NAME_relief, "[colour|pixmap]", "@default",
     "Colour of lighted sides"),
  RC(NAME_shadow, "[colour|pixmap]", "@default",
     "Colour of dark sides")
};

/* Class Declaration */

static Name elevation_termnames[] =
	{ NAME_name, NAME_height, NAME_colour, NAME_relief, NAME_shadow };

ClassDecl(elevation_decls,
          var_elevation, send_elevation, get_elevation, rc_elevation,
          5, elevation_termnames,
          "$Rev$");


status
makeClassElevation(Class class)
{ declareClass(class, &elevation_decls);

  cloneStyleClass(class, NAME_none);

  ElevationTable = globalObject(NAME_elevations, ClassHashTable, EAV);

  succeed;
}
