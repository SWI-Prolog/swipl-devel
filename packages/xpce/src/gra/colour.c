/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialiseColour(Colour c, Name name, Int r, Int g, Int b)
{ char tmp[256];

  assign(c, name,    name);

  if ( isDefault(r) && isDefault(g) && isDefault(b) )
  { DisplayObj d;

    if ( (d = CurrentDisplay(NIL)) && !ws_colour_name(d, name) )
      return errorPce(c, NAME_noNamedColour, name);

    assign(c, kind, NAME_named);
  } else
    assign(c, kind, NAME_rgb);

  assign(c, red,   r);
  assign(c, green, g);
  assign(c, blue,  b);

  protectObject(c);
  sprintf(tmp, "%s_colour", strName(name));
  newAssoc(CtoKeyword(tmp), c);
  appendHashTable(ColourTable, c->name, c);

  succeed;
}


static Colour
getLookupColour(Class class, Name name)
{ answer(getMemberHashTable(ColourTable, (Any) name));
}


static status
storeColour(Colour c, FileObj file)
{ return storeSlotsObject(c, file);
}


static status
loadColour(Colour c, FILE *fd, ClassDef def)
{ TRY( loadSlotsObject(c, fd, def) );

  if ( c->kind == NAME_named )
  { assign(c, red, DEFAULT);
    assign(c, green, DEFAULT);
    assign(c, blue, DEFAULT);
  }

  succeed;
}


static Colour
getConvertColour(Class class, Name name)
{ Colour c;

  if ( (c = getMemberHashTable(ColourTable, name)) )
    answer(c);

  answer(answerObject(ClassColour, name, 0));
}  


static status
XopenColour(Colour c, DisplayObj d)
{ return ws_create_colour(c, d);
}


Colour
getHiliteColour(Colour c)
{ Colour c2;
  Int r, g, b;
  Name n2;

  if ( (c2 = getAttributeObject(c, NAME_hilite)) )
    answer(c2);
  if ( isDefault(c->green) )
    getXrefObject(c2, CurrentDisplay(NIL));
  
  r = toInt(min(65535, (valInt(c->red)   * 3)/2)); /* resource? */
  g = toInt(min(65535, (valInt(c->green) * 3)/2));
  b = toInt(min(65535, (valInt(c->blue)  * 3)/2));

  n2 = getAppendName(CtoName("hilited_"), c->name);
  c2 = newObject(ClassColour, n2, r, g, b, 0);
  attributeObject(c, newObject(ClassAttribute, NAME_hilite, c2, 0));

  answer(c2);
}


Colour
getReduceColour(Colour c)
{ Colour c2;
  Int r, g, b;
  Name n2;

  if ( (c2 = getAttributeObject(c, NAME_reduce)) )
    answer(c2);
  if ( isDefault(c->green) )
    getXrefObject(c2, CurrentDisplay(NIL));
  
  r = toInt(max(0, (valInt(c->red)   * 2)/3)); /* resource? */
  g = toInt(max(0, (valInt(c->green) * 2)/3));
  b = toInt(max(0, (valInt(c->blue)  * 2)/3));

  n2 = getAppendName(CtoName("reduced_"), c->name);
  c2 = newObject(ClassColour, n2, r, g, b, 0);
  attributeObject(c, newObject(ClassAttribute, NAME_reduce, c2, 0));

  answer(c2);
}


status
makeClassColour(Class class)
{ sourceClass(class, makeClassColour, __FILE__, "$Revision$");

  localClass(class, NAME_name, NAME_name, "name", NAME_get,
	     "Name of the colour");
  localClass(class, NAME_kind, NAME_kind, "{named,rgb}", NAME_none,
	     "From X-colour database or user-defined");
  localClass(class, NAME_red, NAME_colour, "[0..65535]", NAME_get,
	     "Red value");
  localClass(class, NAME_green, NAME_colour, "[0..65535]", NAME_get,
	     "Green value");
  localClass(class, NAME_blue, NAME_colour, "[0..65535]", NAME_get,
	     "Blue value");

  termClass(class, "colour", 4, NAME_name, NAME_red, NAME_green, NAME_blue);
  setLoadStoreFunctionClass(class, loadColour, storeColour);

  sendMethod(class, NAME_initialise, DEFAULT, 4,
	     "name=name",
	     "red=[0..65535]", "green=[0..65535]", "blue=[0..65535]",
	     "Create from name and optional rgb",
	     initialiseColour);
  sendMethod(class, NAME_Xopen, NAME_x, 1, "display",
	     "Relate X-colour",
	     XopenColour);

  getMethod(class, NAME_convert, NAME_conversion, "colour", 1, "name",
	    "Convert X-colour name",
	    getConvertColour);
  getMethod(class, NAME_lookup, NAME_oms, "colour", 1, "name",
	    "Lookup in @colours table",
	    getLookupColour);
  getMethod(class, NAME_hilite, NAME_3d, "colour", 0,
	    "Hilited version of the colour",
	    getHiliteColour);
  getMethod(class, NAME_reduce, NAME_3d, "colour", 0,
	    "Reduced version of the colour",
	    getReduceColour);

  ColourTable = globalObject(NAME_colours, ClassHashTable, toInt(32), 0);

  succeed;
}

