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
{ char buf[50];

  sprintf(buf, "#%02x%02x%02x",
	  (unsigned int)valInt(r)>>8,
	  (unsigned int)valInt(g)>>8,
	  (unsigned int)valInt(b)>>8);

  return CtoName(buf);
}


static status
initialiseColour(Colour c, Name name, Int r, Int g, Int b)
{ if ( notDefault(name) )
    assign(c, name, name);

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
  { if ( c1->name == c2->name )
      succeed;

    if ( isDefault(c1->red) )		/* `open' both colours */
      getXrefObject(c1, CurrentDisplay(NIL));
    if ( isDefault(c2->red) )
      getXrefObject(c2, CurrentDisplay(NIL));

    if ( c1->red   == c2->red &&	/* tolerance? */
	 c1->green == c2->green &&
	 c1->blue  == c2->blue )
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
  hf = h ? valReal(h) : 0.5;
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
  rf = rfactor ? valReal(rfactor) : 0.5;
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

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_lookup[] =
        { "[name|int]", "red=[0..65535]", "green=[0..65535]",
	  "blue=[0..65535]" };
static char *T_initialise[] =
        { "name=[name]", "red=[0..65535]", "green=[0..65535]",
	  "blue=[0..65535]" };

/* Instance Variables */

static vardecl var_colour[] =
{ IV(NAME_name, "name|int", IV_GET,
     NAME_name, "Name of the colour"),
  IV(NAME_kind, "{named,rgb}", IV_GET,
     NAME_kind, "From colour-name database or user-defined"),
  SV(NAME_red, "[0..65535]", IV_NONE|IV_FETCH, getRedColour,
     NAME_colour, "Red value"),
  SV(NAME_green, "[0..65535]", IV_NONE|IV_FETCH, getGreenColour,
     NAME_colour, "Green value"),
  SV(NAME_blue, "[0..65535]", IV_NONE|IV_FETCH, getBlueColour,
     NAME_colour, "Blue value")
};

/* Send Methods */

static senddecl send_colour[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseColour,
     DEFAULT, "Create from name and optional rgb"),
  SM(NAME_unlink, 0, NULL, unlinkColour,
     DEFAULT, "Deallocate the colour object"),
  SM(NAME_Xclose, 1, "display", XCloseColour,
     NAME_x, "Destroy window-system counterpart"),
  SM(NAME_Xopen, 1, "display", XopenColour,
     NAME_x, "Create window-system counterpart"),
  SM(NAME_equal, 1, "colour", equalColour,
     DEFAULT, "Test if colours have equal RGB")
};

/* Get Methods */

static getdecl get_colour[] =
{ GM(NAME_hilite, 0, "colour", NULL, getHiliteColour,
     NAME_3d, "Hilited version of the colour"),
  GM(NAME_reduce, 0, "colour", NULL, getReduceColour,
     NAME_3d, "Reduced version of the colour"),
  GM(NAME_convert, 1, "colour", "name", getConvertColour,
     NAME_conversion, "Convert X-colour name"),
  GM(NAME_storageReference, 0, "name", NULL, getStorageReferenceColour,
     NAME_file, "Description name for ->save_in_file"),
  GM(NAME_intensity, 0, "0..65535", NULL, getIntensityColour,
     NAME_grey, "Total light intensity of the colour"),
  GM(NAME_lookup, 4, "colour", T_lookup, getLookupColour,
     NAME_oms, "Lookup in @colours table")
};

/* Resources */

static resourcedecl rc_colour[] =
{ RC(NAME_hiliteFactor, "real", "0.9",
     "Factor for <-hilite'd colour"),
  RC(NAME_reduceFactor, "real", "0.5",
     "Factor for <-reduce'd colour")
};

/* Class Declaration */

static Name colour_termnames[] = { NAME_name };

ClassDecl(colour_decls,
          var_colour, send_colour, get_colour, rc_colour,
          1, colour_termnames,
          "$Rev$");


status
makeClassColour(Class class)
{ declareClass(class, &colour_decls);

  setLoadStoreFunctionClass(class, loadColour, storeColour);
  cloneStyleClass(class, NAME_none);

  ColourTable = globalObject(NAME_colours, ClassHashTable, toInt(32), 0);
  assign(ColourTable, refer, NAME_none);

  succeed;
}

