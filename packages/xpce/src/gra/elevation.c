/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static HashTable ElevationTable;	/* @elevations */

static status
initialiseElevation(Elevation e, Any name,
		    Int height, Any colour, Any relief, Any shadow,
		    Name kind)
{ if ( isDefault(name) )
    name = NIL;
  
  if ( isDefault(height) )
  { if ( isInteger(name) )
      height = name;
    else
      height = getResourceValueObject(e, NAME_height);
  }
  if ( isDefault(colour) ) colour = getResourceValueObject(e, NAME_colour);
  if ( isDefault(relief) ) relief = getResourceValueObject(e, NAME_relief);
  if ( isDefault(shadow) ) shadow = getResourceValueObject(e, NAME_shadow);
  if ( isDefault(kind) )   kind   = getResourceValueObject(e, NAME_kind);

  assign(e, name,   name);
  assign(e, height, height);
  assign(e, colour, colour);
  assign(e, relief, relief);
  assign(e, shadow, shadow);
  assign(e, kind,   kind);

  if ( notNil(name) )
    appendHashTable(ElevationTable, name, e);

  succeed;
}


static Elevation
getLookupElevation(Any receiver, Any name,
		   Int height, Any colour, Any relief, Any shadow,
		   Name kind)
{ Elevation e = getMemberHashTable(ElevationTable, name);

  if ( e &&
       (isDefault(height) || height == e->height) &&
       (isDefault(colour) || colour == e->colour) &&
       (isDefault(relief) || relief == e->relief) &&
       (isDefault(shadow) || shadow == e->shadow) &&
       (isDefault(kind)   || kind   == e->kind) )
    answer(e);

  fail;
}


static Elevation
getConvertElevation(Any receiver, Any val)
{ Int i;
  Elevation e;

  if ( (e= getLookupElevation(receiver, val,
			      DEFAULT, DEFAULT, DEFAULT, DEFAULT, DEFAULT)) )
    return e;

  if ( (i = toInteger(val)) )
    answer(answerObject(ClassElevation, i, i, 0));

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
reliefElevation(Elevation e, Any colour)
{ return attributeElevation(e, NAME_relief, colour);
}


static status
shadowElevation(Elevation e, Any colour)
{ return attributeElevation(e, NAME_shadow, colour);
}


static status
kindElevation(Elevation e, Name kind)
{ return attributeElevation(e, NAME_shadow, kind);
}


status
makeClassElevation(Class class)
{ sourceClass(class, makeClassElevation, __FILE__, "$Revision$");

  localClass(class, NAME_name, NAME_name, "name|int*", NAME_get,
	     "Name for reuse");
  localClass(class, NAME_height, NAME_appearance, "int", NAME_get,
	     "Height above the surface");
  localClass(class, NAME_colour, NAME_appearance, "[colour|pixmap]", NAME_get,
	     "Colour/pixmap used at `light' side");
  localClass(class, NAME_relief, NAME_appearance, "[colour|pixmap]", NAME_get,
	     "Colour/pixmap used at `light' side");
  localClass(class, NAME_shadow, NAME_appearance, "[colour|pixmap]", NAME_get,
	     "Colour/pixmap used at `dark' side");
  localClass(class, NAME_kind, NAME_appearance, "{3d,shadow}", NAME_get,
	     "How the elevation is realised");

  termClass(class, "elevation", 5,
	    NAME_name, NAME_height, NAME_colour, NAME_relief, NAME_shadow);
  cloneStyleClass(class, NAME_none);

  storeMethod(class, NAME_height, heightElevation);
  storeMethod(class, NAME_colour, colourElevation);
  storeMethod(class, NAME_relief, reliefElevation);
  storeMethod(class, NAME_shadow, shadowElevation);
  storeMethod(class, NAME_kind,   kindElevation);

  sendMethod(class, NAME_initialise, DEFAULT, 6,
	     "name=[name|int]*", "height=[int]",
	     "colour=[colour|pixmap]",
	     "relief=[colour|pixmap]", "shadow=[colour|pixmap]",
	     "kind=[{3d,shadow}]",
	     "Create elevation from name, heigth and colours",
	     initialiseElevation);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Delete from @elevations",
	     unlinkElevation);
  
  getMethod(class, NAME_convert, DEFAULT, "elevation", 1, "name|int",
	    "Convert name to object (reuse) or int to height",
	    getConvertElevation);
  getMethod(class, NAME_lookup, DEFAULT, "elevation", 6,
	    "name=[name|int]*", "height=[int]",
	    "colour=[colour|pixmap]",
	    "relief=[colour|pixmap]", "shadow=[colour|pixmap]",
	    "kind=[{3d,shadow}]",
	    "Lookup from @elevations",
	    getLookupElevation);

  attach_resource(class, "height", "int", "2",
		  "Default height of the evaluation");
  attach_resource(class, "colour", "[colour|pixmap]", "@default",
		  "Colour of the top");
  attach_resource(class, "relief", "[colour|pixmap]", "@default",
		  "Colour of lighted sides");
  attach_resource(class, "shadow", "[colour|pixmap]", "@default",
		  "Colour of dark sides");
  attach_resource(class, "kind", "{3d,shadow}", "3d",
		  "How the elevation is realised");

  ElevationTable = globalObject(NAME_elevations, ClassHashTable, 0);

  succeed;
}
