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

static status XCloseColour(Colour c, DisplayObj d);

static status
toRBG(Int *r, Int *g, Int *b, Name model)
{ if ( isDefault(*r) || isDefault(*g) || isDefault(*b) )
    fail;

  if ( model == NAME_hsv )
  { int	ih = valInt(*r) % 360;
    int is = valInt(*g);
    int iv = valInt(*b);
    float R,G,B;

    if ( is > 100 )
      return errorPce(*g, NAME_unexpectedType, CtoType("0..100"));
    if ( iv > 100 )
      return errorPce(*g, NAME_unexpectedType, CtoType("0..100"));

    if ( ih < 0 )
      ih += 360;

    HSVToRGB((float)ih/360.0, (float)is/100.0, (float)iv/100.0,
	     &R, &G, &B);
    *r = toInt((int)(R*65535));
    *g = toInt((int)(G*65535));
    *b = toInt((int)(B*65535));
  }

  succeed;
}


static Name
defcolourname(Int r, Int g, Int b)
{ if ( notDefault(r) && notDefault(g) && notDefault(b) )
  { char buf[50];

    sprintf(buf, "#%02x%02x%02x",
	    (unsigned int)valInt(r)>>8,
	    (unsigned int)valInt(g)>>8,
	    (unsigned int)valInt(b)>>8);

    return CtoName(buf);
  }

  fail;
}


static status
initialiseColour(Colour c, Name name, Int r, Int g, Int b, Name model)
{ if ( notDefault(name) )
    assign(c, name, name);

  if ( isDefault(r) && isDefault(g) && isDefault(b) )
  { assign(c, kind, NAME_named);
  } else if ( notDefault(r) && notDefault(g) && notDefault(b) )
  { assign(c, kind, NAME_rgb);

    if ( !toRBG(&r, &g, &b, model) )
      fail;

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
getLookupColour(Class class, Name name, Int r, Int g, Int b, Name model)
{ if ( isDefault(name) && notDefault(r) && notDefault(g) && notDefault(b) )
  { if ( !toRBG(&r, &g, &b, model) )
      fail;

    name = defcolourname(r, g, b);
  }

  if ( name )
    answer(getMemberHashTable(ColourTable, name));

  fail;
}


static Name
getStorageReferenceColour(Colour c)
{ if ( c->kind == NAME_named )
    answer(c->name);
  else
    answer(defcolourname(c->red, c->green, c->blue));
}


static status
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
loadColour(Colour c, IOSTREAM *fd, ClassDef def)
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
			    toInt(r), toInt(g), toInt(b), EAV));
      }
    }

    fail;
  }

  answer(answerObject(ClassColour, name, EAV));
}  


static status
XopenColour(Colour c, DisplayObj d)
{ if ( c->kind == NAME_named )
  { DisplayObj d;

    if ( (d = CurrentDisplay(NIL)) && !ws_colour_name(d, c->name) )
    { errorPce(c, NAME_noNamedColour, c->name);
      assign(c, name, NAME_black);
    }
  }

  return ws_create_colour(c, d);
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


static status
get_hsv_colour(Colour c, float *h, float *s, float *v)
{ if ( isDefault(c->red) )
  { TRY(getXrefObject(c, CurrentDisplay(NIL)));
  }
  
  RGBToHSV((float)valInt(c->red)/65535.0,
	   (float)valInt(c->green)/65535.0,
	   (float)valInt(c->blue)/65535.0,
	   h, s, v);

  succeed;
}


static Int
getHueColour(Colour c)
{ float h, s, v;

  TRY(get_hsv_colour(c, &h, &s, &v));

  return toInt((int)(h*360.0));
}


static Int
getSaturationColour(Colour c)
{ float h, s, v;

  TRY(get_hsv_colour(c, &h, &s, &v));

  return toInt((int)(s*100.0));
}


static Int
getValueColour(Colour c)
{ float h, s, v;

  TRY(get_hsv_colour(c, &h, &s, &v));

  return toInt((int)(v*100.0));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We store derived colours in a chain  associated with the main colour, so
they remain in existence as long as the main colour.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Colour
associateColour(Colour c, Int r, Int g, Int b)
{ Name name;
  Colour c2;
  Chain ch;

  name = defcolourname(r, g, b);
  if ( !(c2=getMemberHashTable(ColourTable, name)) )
    c2 = newObject(ClassColour, name, r, g, b, EAV);

  if ( !(ch=getAttributeObject(c, NAME_associates)) )
    attributeObject(c, NAME_associates, newObject(ClassChain, c2, EAV));
  else
    addChain(ch, c2);

  answer(c2);
}



Colour
getHiliteColour(Colour c, Real h)
{ int r, g, b;
  float hf;

  if ( isDefault(h) )
    h = getClassVariableValueObject(c, NAME_hiliteFactor);
  hf = h ? valReal(h) : 0.9;

  if ( isDefault(c->green) )		/* realise the colour */
    getXrefObject(c, CurrentDisplay(NIL));
  
  r = valInt(c->red);
  g = valInt(c->green);
  b = valInt(c->blue);

  r = r + (int)((float)(65535 - r) * hf);
  g = g + (int)((float)(65535 - g) * hf);
  b = b + (int)((float)(65535 - b) * hf);
  
  return associateColour(c, toInt(r), toInt(g), toInt(b));
}


Colour
getReduceColour(Colour c, Real re)
{ int r, g, b;
  float rf;

  if ( isDefault(re) )
    re = getClassVariableValueObject(c, NAME_reduceFactor);
  rf = re ? valReal(re) : 0.6;

  if ( isDefault(c->green) )		/* realise the colour */
    getXrefObject(c, CurrentDisplay(NIL));
  
  r = valInt(c->red);
  g = valInt(c->green);
  b = valInt(c->blue);

  r = (int)((float)r * rf);
  g = (int)((float)g * rf);
  b = (int)((float)b * rf);

  return associateColour(c, toInt(r), toInt(g), toInt(b));
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
        { "[name|int]",
	  "red=[0..65535]", "green=[0..65535]", "blue=[0..65535]",
	  "model=[{rgb,hsv}]" };
static char *T_initialise[] =
        { "name=[name]",
	  "red=[0..65535]", "green=[0..65535]", "blue=[0..65535]",
	  "model=[{rgb,hsv}]" };

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
{ SM(NAME_initialise, 5, T_initialise, initialiseColour,
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
{ GM(NAME_hilite, 1, "colour", "factor=[0.0..1.0]", getHiliteColour,
     NAME_3d, "Hilited version of the colour"),
  GM(NAME_reduce, 1, "colour", "factor=[0.0..1.0]", getReduceColour,
     NAME_3d, "Reduced version of the colour"),
  GM(NAME_convert, 1, "colour", "name", getConvertColour,
     NAME_conversion, "Convert X-colour name"),
  GM(NAME_storageReference, 0, "name", NULL, getStorageReferenceColour,
     NAME_file, "Description name for ->save_in_file"),
  GM(NAME_intensity, 0, "0..65535", NULL, getIntensityColour,
     NAME_grey, "Total light intensity of the colour"),
  GM(NAME_lookup, 5, "colour", T_lookup, getLookupColour,
     NAME_oms, "Lookup in @colours table"),
  GM(NAME_hue, 0, "0..360", NULL, getHueColour,
     NAME_colour, "Hue from the HSV-model"),
  GM(NAME_saturnation, 0, "0..100", NULL, getSaturationColour,
     NAME_colour, "Saturnation from the HSV-model"),
  GM(NAME_value, 0, "0..100", NULL, getValueColour,
     NAME_colour, "Value from the HSV-model")
};

/* Resources */

static classvardecl rc_colour[] =
{ RC(NAME_hiliteFactor, "real", "0.9",
     "Default factor for <-hilite'd colour"),
  RC(NAME_reduceFactor, "real", "0.6",
     "Default factor for <-reduce'd colour")
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

  ColourTable = globalObject(NAME_colours, ClassHashTable, toInt(32), EAV);
  assign(ColourTable, refer, NAME_none);

/* Don't know why this is done, it cannot be here as it is the reason why
   the X11 display is opened during XPCE's initialisation.  Possibly related
   to the variable BLACK_COLOUR, set when opening the display

   Well, it has to initialise the emulated colour resources on Windows.
   There is no harm in this, so we'll just call it then.  Maybe some day
   we should make this more lazy.
*/

#ifdef WIN32_GRAPHICS
  ws_colour_name(CurrentDisplay(NIL), NAME_black);
#endif

  succeed;
}

