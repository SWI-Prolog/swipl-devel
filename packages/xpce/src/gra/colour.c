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


status
equalColour(Colour c1, Colour c2)
{ if ( c1 == c2 )
    succeed;
  if ( instanceOfObject(c1, ClassColour) &&
       instanceOfObject(c2, ClassColour) )
  { if ( c1->name == c2->name ||
	 (c1->red   == c2->red &&
	  c1->green == c2->green &&
	  c1->blue  == c2->blue) )
      succeed;
  }

  fail;
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


static int
take_hex(char *s, int digits)
{ unsigned int v = 0;

  for(; digits-- > 0; s++)
  { if ( *s >= '0' && *s <= '9' )
      v = v * 16 + *s - '0';
    else if ( *s >= 'a' && *s <= 'f' )
      v = v * 16 + *s - 'a' + 10;
    else if ( *s >= 'A' && *s <= 'F' )
      v = v * 16 + *s - 'A' + 10;
    else
      return -1;			/* error */
  }

  return v;
}


static Colour
getConvertColour(Class class, Name name)
{ Colour c;
  char *s;

  if ( (c = getMemberHashTable(ColourTable, name)) )
    answer(c);

  if ( (s=strName(name))[0] == '#' )
  { int r, g, b;
    int dgs = 0;
    int l = strlen(s);

    if ( l == 7 )
      dgs = 2;
    else if ( l == 13 )
      dgs = 4;
    
    if ( dgs )
    { s++;				/* skip # */
      r = take_hex(s, dgs); s+= dgs;
      g = take_hex(s, dgs); s+= dgs;
      b = take_hex(s, dgs);

      if ( r >= 0 && g >= 0 && b >= 0 )
      { if ( dgs == 2 )
	{ r = r*256 + r;
	  g = g*256 + g;
	  b = b*256 + b;
	} 

	answer(answerObject(ClassColour, name,
			    toInt(r), toInt(g), toInt(b), 0));
      }
    }

    fail;
  }

  answer(answerObject(ClassColour, name, 0));
}  


static status
XopenColour(Colour c, DisplayObj d)
{ return ws_create_colour(c, d);
}


Colour
getHiliteColour(Colour c)
{ Colour c2;
  int r, g, b;
  Name n2;
  Real h = getResourceValueObject(c, NAME_hiliteFactor);
  float hf = h ? h->value : 0.5;

  if ( (c2 = getAttributeObject(c, NAME_hilite)) )
    answer(c2);
  if ( isDefault(c->green) )
    getXrefObject(c2, CurrentDisplay(NIL));
  
  r = valInt(c->red);
  g = valInt(c->green);
  b = valInt(c->blue);

  r = r + (int)((float)(65535 - r) * hf);
  g = g + (int)((float)(65535 - g) * hf);
  b = b + (int)((float)(65535 - b) * hf);
  
  n2 = getAppendName(CtoName("hilited_"), c->name);
  c2 = newObject(ClassColour, n2, toInt(r), toInt(g), toInt(b), 0);
  attributeObject(c, NAME_hilite, c2);

  answer(c2);
}


Colour
getReduceColour(Colour c)
{ Colour c2;
  int r, g, b;
  Name n2;
  Real rfactor = getResourceValueObject(c, NAME_reduceFactor);
  float rf = rfactor ? rfactor->value : 0.5;

  if ( (c2 = getAttributeObject(c, NAME_reduce)) )
    answer(c2);
  if ( isDefault(c->green) )
    getXrefObject(c2, CurrentDisplay(NIL));
  
  r = valInt(c->red);
  g = valInt(c->green);
  b = valInt(c->blue);

  r = (int)((float)r * rf);
  g = (int)((float)g * rf);
  b = (int)((float)b * rf);

  n2 = getAppendName(CtoName("reduced_"), c->name);
  c2 = newObject(ClassColour, n2, toInt(r), toInt(g), toInt(b), 0);
  attributeObject(c, NAME_reduce, c2);

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

  attach_resource(class, "reduce_factor", "real", "0.5",
		  "Factor for <-reduce'd colour");
  attach_resource(class, "hilite_factor", "real", "0.5",
		  "Factor for <-hilite'd colour");

  succeed;
}

