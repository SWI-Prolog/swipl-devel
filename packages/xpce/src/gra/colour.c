/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static XCloseColour(Colour c, DisplayObj d);

static Int
defcolourname(Int r, Int g, Int b)
{ return toInt(((valInt(r)/256) << 16) +
	       ((valInt(g)/256) << 8) +
	       (valInt(b)/256));
}


static status
initialiseColour(Colour c, Name name, Int r, Int g, Int b)
{ assign(c, name, name);

  if ( isDefault(r) && isDefault(g) && isDefault(b) )
  { DisplayObj d;

    if ( (d = CurrentDisplay(NIL)) && !ws_colour_name(d, name) )
      return errorPce(c, NAME_noNamedColour, name);

    assign(c, kind, NAME_named);
  } else if ( notDefault(r) && notDefault(g) && notDefault(b) )
  { assign(c, kind, NAME_rgb);
    if ( isDefault(name) )
    { name = defcolourname(r, g, b);
      assign(c, name, name);
    }
  } else
    return errorPce(c, NAME_instantiationFault,
		    getMethodFromFunction(initialiseColour));

  assign(c, red,   r);
  assign(c, green, g);
  assign(c, blue,  b);

  appendHashTable(ColourTable, c->name, c);

  succeed;
}


static status
unlinkColour(Colour c)
{ deleteHashTable(ColourTable, c->name);
  XCloseColour(c, DEFAULT);

  succeed;
}


static Colour
getLookupColour(Class class, Name name, Int r, Int g, Int b)
{ if ( isDefault(name) )
    name = defcolourname(r, g, b);

  answer(getMemberHashTable(ColourTable, (Any) name));
}


static Name
getStorageReferenceColour(Colour c)
{ if ( c->kind == NAME_named )
    answer(c->name);
  else
  { char tmp[256];

    sprintf(tmp, "#%02x%02x%02x",
	    (unsigned int) valInt(c->red)/256,
	    (unsigned int) valInt(c->green)/256,
	    (unsigned int) valInt(c->blue)/256);

    answer(CtoName(tmp));
  }
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


static status
XCloseColour(Colour c, DisplayObj d)
{ ws_uncreate_colour(c, d);

  succeed;
}


Int
getRedColour(Colour c)
{ if ( isDefault(c->red) )
    getXrefObject(c, CurrentDisplay(NIL));

  return c->red;
}


Int
getGreenColour(Colour c)
{ if ( isDefault(c->green) )
    getXrefObject(c, CurrentDisplay(NIL));

  return c->green;
}


Int
getBlueColour(Colour c)
{ if ( isDefault(c->blue) )
    getXrefObject(c, CurrentDisplay(NIL));

  return c->blue;
}


Colour
getHiliteColour(Colour c)
{ Colour c2;
  int r, g, b;
  Name n2;
  Real h;
  float hf;

  if ( (c2 = getAttributeObject(c, NAME_hilite)) )
    answer(c2);
  h  = getResourceValueObject(c, NAME_hiliteFactor);
  hf = h ? h->value : 0.5;
  if ( isDefault(c->green) )
    getXrefObject(c, CurrentDisplay(NIL));
  
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
  Real rfactor;
  float rf;

  if ( (c2 = getAttributeObject(c, NAME_reduce)) )
    answer(c2);
  rfactor = getResourceValueObject(c, NAME_reduceFactor);
  rf = rfactor ? rfactor->value : 0.5;
  if ( isDefault(c->green) )
    getXrefObject(c, CurrentDisplay(NIL));
  
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


static Int
getIntensityColour(Colour c)
{ int r, g, b;

  if ( isDefault(c->green) )
    getXrefObject(c, CurrentDisplay(NIL));
  
  r = valInt(c->red);
  g = valInt(c->green);
  b = valInt(c->blue);
  
  answer(toInt((r*20 + g*32 + b*18)/(20+32+18)));
}


status
makeClassColour(Class class)
{ sourceClass(class, makeClassColour, __FILE__, "$Revision$");

  localClass(class, NAME_name, NAME_name, "name|int", NAME_get,
	     "Name of the colour");
  localClass(class, NAME_kind, NAME_kind, "{named,rgb}", NAME_none,
	     "From X-colour database or user-defined");
  localClass(class, NAME_red, NAME_colour, "[0..65535]", NAME_none,
	     "Red value");
  localClass(class, NAME_green, NAME_colour, "[0..65535]", NAME_none,
	     "Green value");
  localClass(class, NAME_blue, NAME_colour, "[0..65535]", NAME_none,
	     "Blue value");

  termClass(class, "colour", 1, NAME_name);
  setLoadStoreFunctionClass(class, loadColour, storeColour);
  cloneStyleClass(class, NAME_none);

  fetchMethod(class, NAME_red,   getRedColour);
  fetchMethod(class, NAME_green, getGreenColour);
  fetchMethod(class, NAME_blue,  getBlueColour);

  sendMethod(class, NAME_initialise, DEFAULT, 4,
	     "name=[name]",
	     "red=[0..65535]", "green=[0..65535]", "blue=[0..65535]",
	     "Create from name and optional rgb",
	     initialiseColour);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Deallocate the colour object",
	     unlinkColour);
  sendMethod(class, NAME_Xopen, NAME_x, 1, "display",
	     "Create window-system counterpart",
	     XopenColour);
  sendMethod(class, NAME_Xclose, NAME_x, 1, "display",
	     "Destroy window-system counterpart",
	     XCloseColour);

  getMethod(class, NAME_convert, NAME_conversion, "colour", 1, "name",
	    "Convert X-colour name",
	    getConvertColour);
  getMethod(class, NAME_lookup, NAME_oms, "colour", 4,
	    "[name|int]",
	    "red=[0..65535]", "green=[0..65535]", "blue=[0..65535]",
	    "Lookup in @colours table",
	    getLookupColour);
  getMethod(class, NAME_hilite, NAME_3d, "colour", 0,
	    "Hilited version of the colour",
	    getHiliteColour);
  getMethod(class, NAME_reduce, NAME_3d, "colour", 0,
	    "Reduced version of the colour",
	    getReduceColour);
  getMethod(class, NAME_intensity, NAME_grey, "0..65535", 0,
	    "Total light intensity of the colour",
	    getIntensityColour);
  getMethod(class, NAME_storageReference, NAME_file, "name", 0,
	    "Description name for ->save_in_file",
	    getStorageReferenceColour);

  ColourTable = globalObject(NAME_colours, ClassHashTable, toInt(32), 0);
  assign(ColourTable, refer, OFF);

  attach_resource(class, "reduce_factor", "real", "0.5",
		  "Factor for <-reduce'd colour");
  attach_resource(class, "hilite_factor", "real", "0.9",
		  "Factor for <-hilite'd colour");

  succeed;
}

