/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static HashTable ColourMaps;		/* name --> maps */

static status
initialiseColourMap(ColourMap cm, Name name, Vector colours)
{ if ( isDefault(name) )
    name = NAME_system;
  if ( isDefault(colours) )
    colours = NIL;

  assign(cm, name,    name);
  assign(cm, colours, colours);

  succeed;
}


static ColourMap
getConvertColourMap(Class class, Name name)
{ ColourMap cm;
  int size;

  if ( ColourMaps && (cm = getMemberHashTable(ColourMaps, name)) )
    answer(cm);

  if ( isstr8(&name->data) &&
       sscanf(strName(name), "colour_cube_%d", &size) == 1 )
  { cm = newObject(ClassColourMap, name, NIL, 0);
    lockObject(cm, ON);

    ws_colour_cube(cm, size);
    answer(cm);
  }

  fail;
}


static ColourMap
getLookupColourMap(Class class, Name name)
{ return getConvertColourMap(class, name);
}


static Vector
getColoursColourMap(ColourMap cm)
{ if ( isNil(cm->colours) )
    ws_colour_map_colours(cm);

  if ( notNil(cm->colours) )
    answer(cm->colours);

  fail;
}


/* Type declaractions */

static char *T_initialise[] = { "name=[name]*", "colours=[vector]*" };

/* Instance Variables */

static vardecl var_colour_map[] =
{ IV(NAME_name, "name*", IV_GET,
     NAME_name, "Name (for lookup) of the colourmap"),
  IV(NAME_colours, "vector*", IV_NONE,
     NAME_storage, "Vector of colours defining the map"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_storage, "Window system handle")
};

/* Send Methods */

static senddecl send_colour_map[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseColourMap,
     DEFAULT, "Create from name and colours")
};

/* Get Methods */

static getdecl get_colour_map[] =
{ GM(NAME_lookup, 1, "colour_map", "name", getLookupColourMap,
     NAME_oms, "Reuse existing named colour_map"),
  GM(NAME_convert, 1, "colour_map", "name", getConvertColourMap,
     DEFAULT, "Allow using name to specify a map"),
  GM(NAME_colours, 0, "vector*", NULL, getColoursColourMap,
     DEFAULT, "Get the colours of the map")
};

/* Resources */

#define rc_colour_map NULL
/*
static classvardecl rc_colour_map[] =
{ 
};
*/

/* Class Declaration */

static Name colour_map_termnames[] = { NAME_name, NAME_colours };

ClassDecl(colour_map_decls,
          var_colour_map, send_colour_map, get_colour_map, rc_colour_map,
          2, colour_map_termnames,
          "$Rev$");

status
makeClassColourMap(Class class)
{ declareClass(class, &colour_map_decls);

  ColourMaps = globalObject(NAME_colourMaps, ClassHashTable, toInt(8), 0);

  succeed;
}
