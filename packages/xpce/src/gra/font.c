/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status	defaultPostScriptFont(FontObj f);
static Int	getPointsFont(FontObj f);

static Name
fontName(Name family, Name style, Int points)
{ char buf[100];
  char *s;

  s = strcpyskip(buf, strName(family));
  *s++ = '_';
  s = strcpyskip(s, strName(style));
  if ( notDefault(points) )
    sprintf(s, "_%ld", valInt(points));

  return CtoKeyword(buf);
}


static status
initialiseFont(FontObj f, Name family, Name style, Int points, Name xname)
{ Name name = fontName(family, style, points);

  assign(f, family,      family);
  assign(f, style,       style);
  assign(f, points,      points);
  assign(f, fixed_width, DEFAULT);
  assign(f, b16,	 DEFAULT);
  assign(f, x_name,      xname);

  defaultPostScriptFont(f);

  protectObject(f);
  newAssoc(name, f);

  return appendHashTable(FontTable, name, f);
}


static FontObj
getLookupFont(Class class, Name family, Name style, Int points)
{ Name name = fontName(family, style, points);
  FontObj f2;

  makeBuiltinFonts();
  if ( (f2 = getMemberHashTable(FontTable, name)) )
    answer(f2);

  fail;
}


static FontObj
getConvertFont(Class class, Name name)
{ char *s = strName(name);

  makeBuiltinFonts();

  if ( s[0] == '@' )
  { Name ref_name;

    for(s++; *s == ' ' || *s == '\t'; s++)
      ;
    ref_name = CtoKeyword(s);

    answer(getMemberHashTable(FontTable, ref_name));
  } else
  { DisplayObj d = CurrentDisplay(NIL);
    FontObj f;
    Name fn = (syntax.uppercase ? CtoKeyword(s) : name);

    if ( d && (f = getMemberHashTable(d->font_table, fn)) )
    { answer(f);
    } else
    { for_hash_table(FontTable, sy,
		     { FontObj f = sy->value;
		       if ( f->x_name == fn ) /* case? */
			 answer(f);
		     })
    }
  }

  fail;
}


status
replaceFont(FontObj f, DisplayObj d)
{ FontObj nofont;
  void *wsref;

  if ( !(nofont = getResourceValueObject(d, NAME_noFont)) )
    errorPce(f, NAME_noDefaultFont);
  
  if ( !(wsref = getXrefObject(nofont, d)) )
    fail;

  errorPce(f, NAME_replacedFont, nofont);
  registerXrefObject(f, d, wsref);

  assign(f, fixed_width, nofont->fixed_width);

  succeed;
}


static status
XopenFont(FontObj f, DisplayObj d)
{ if ( isDefault(d) )
    d = CurrentDisplay(f);

  makeBuiltinFonts();

  if ( !ws_create_font(f, d) )
  { errorPce(f, NAME_noRelatedXFont);
    return replaceFont(f, d);
  }      

  succeed;
}


static status
XcloseFont(FontObj f, DisplayObj d)
{ ws_destroy_font(f, d);

  succeed;
}


status
makeBuiltinFonts(void)
{ DisplayObj d;
  static int done = FALSE;

  if ( done )
    succeed;
  done = TRUE;

  if ( (d = CurrentDisplay(NIL)) &&
       send(d, NAME_loadFonts, 0) &&	/* XPCE predefined fonts */
       ws_system_fonts(d) &&		/* Window-system fonts */
       send(d, NAME_loadFontAliases, NAME_systemFonts, 0) )
  { send(d, NAME_loadFontAliases, NAME_userFonts, 0);
    succeed;
  }

  fail;
}

		/********************************
		*          POSTSCRIPT		*
		********************************/

static status
defaultPostScriptFont(FontObj f)
{ char buf[LINESIZE];

  if ( f->family == NAME_helvetica )
  { strcpy(buf, "Helvetica");

    if ( f->style == NAME_bold )
      strcat(buf, "-Bold");
    else if ( f->style == NAME_oblique )
      strcat(buf, "-Oblique");
  } else if ( f->family == NAME_times )
  { strcpy(buf, "Times");

    if ( f->style == NAME_bold )
      strcat(buf, "-Bold");
    else if ( f->style == NAME_italic )
      strcat(buf, "-Italic");
    else /*if ( f->style == NAME_roman )*/
      strcat(buf, "-Roman");
  } else /*if ( f->family == NAME_screen ||
	        f->family == NAME_courier )*/
  { strcpy(buf, "Courier");

    if ( f->style == NAME_bold )
      strcat(buf, "-Bold");
    else if ( f->style == NAME_oblique )
      strcat(buf, "-Oblique");
  }

  assign(f, postscript_size, getPointsFont(f));
  assign(f, postscript_font, CtoName(buf));

  succeed;
}


		/********************************
		*           GET INFO		*
		********************************/


static Int
getWidthFont(FontObj f, CharArray txt)
{ if ( isDefault(txt) )
  { if ( getB16Font(f) == ON )
      txt = (CharArray) CtoName("xx");
    else
      txt = (CharArray) CtoName("x");
  }

  d_ensure_display();			/* TBD */

  answer(toInt(str_width(&txt->data, 0, txt->data.size, f)));
}


Int
getExFont(FontObj f)
{ if ( isNil(f->ex) )
    assign(f, ex, getWidthFont(f, DEFAULT));

  answer(f->ex);
}


Int
getHeightFont(FontObj f)
{ d_ensure_display();

  answer(toInt(s_height(f)));
}


Int
getAscentFont(FontObj f)
{ d_ensure_display();

  answer(toInt(s_ascent(f)));
}


Int
getDescentFont(FontObj f)
{ d_ensure_display();

  answer(toInt(s_descent(f)));
}


static Size
getSizeFont(FontObj f)
{ answer(answerObject(ClassSize, getExFont(f), getHeightFont(f), 0));
}


Bool
getFixedWidthFont(FontObj f)
{ if ( isDefault(f->fixed_width) )
    XopenFont(f, CurrentDisplay(NIL));

  answer(f->fixed_width);
}


Bool
getB16Font(FontObj f)
{ if ( isDefault(f->b16) )
    XopenFont(f, CurrentDisplay(NIL));

  answer(f->b16);
}


static status
memberFont(FontObj f, Int chr)
{ d_ensure_display();

  if ( s_has_char(f, valInt(chr)) )
    succeed;

  fail;
}


static Int
getDefaultCharacterFont(FontObj f)
{ d_ensure_display();

  answer(toInt(s_default_char(f)));
}


static Tuple
getDomainFont(FontObj f, Name which)
{ int a, z;

  if ( isDefault(which) )
    which = NAME_x;

  f_domain(f, which, &a, &z);
  return answerObject(ClassTuple, toInt(a), toInt(z), 0);
}


static Int
getPointsFont(FontObj f)
{ if ( notDefault(f->points) )
    answer(f->points);

  answer(getHeightFont(f));
}



status
makeClassFont(Class class)
{ sourceClass(class, makeClassFont, __FILE__, "$Revision$");

  localClass(class, NAME_family, NAME_name, "name", NAME_get,
	     "Family the font belongs to (times, etc.)");
  localClass(class, NAME_style, NAME_name, "name", NAME_get,
	     "Style of the font (bold, italic, etc.)");
  localClass(class, NAME_points, NAME_name, "[int]", NAME_none,
	     "Point-size of the font");
  localClass(class, NAME_ex, NAME_dimension, "int*", NAME_none,
	     "Width of the letter `x' in this font");
  localClass(class, NAME_xName, NAME_x, "[name]", NAME_get,
	     "Window-system name for the font");
  localClass(class, NAME_fixedWidth, NAME_property, "[bool]", NAME_none,
	     "If @off, font is proportional");
  localClass(class, NAME_b16, NAME_property, "[bool]", NAME_none,
	     "If @on, font is a 16-bit font");
  localClass(class, NAME_postscriptFont, NAME_postscript, "name", NAME_both,
	     "PostScript-name of the font");
  localClass(class, NAME_postscriptSize, NAME_postscript, "int", NAME_both,
	     "PostScript point-size of the font");

  termClass(class, "font", 3, NAME_family, NAME_style, NAME_points);
  saveStyleClass(class, NAME_external);
  cloneStyleClass(class, NAME_none);

  sendMethod(class, NAME_initialise, DEFAULT, 4,
	     "family=name", "style=name", "points=[int]", "x_name=[name]",
	     "Create from fam, style, points, name",
	     initialiseFont);
  sendMethod(class, NAME_Xopen, NAME_x, 1, "display",
	     "Open the associated X-resources",
	     XopenFont);
  sendMethod(class, NAME_Xclose, NAME_x, 1, "display",
	     "Destroy associated X-resources",
	     XcloseFont);
  sendMethod(class, NAME_member, NAME_set, 1, "char",
	     "Test if font defines character",
	     memberFont);

  getMethod(class, NAME_width, NAME_dimension, "int", 1, "[char_array]",
	    "Width of string (default \"x\")",
	    getWidthFont);
  getMethod(class, NAME_ex, NAME_dimension, "int", 0,
	    "Width of the letter `x'",
	    getExFont);
  getMethod(class, NAME_height, NAME_dimension, "int", 0,
	    "Height of highest character in font",
	    getHeightFont);
  getMethod(class, NAME_size, NAME_dimension, "size", 0,
	    "New size from <-width and <-height",
	    getSizeFont);
  getMethod(class, NAME_ascent, NAME_dimension, "int", 0,
	    "Highest point above baseline",
	    getAscentFont);
  getMethod(class, NAME_descent, NAME_dimension, "int", 0,
	    "Lowest point below baseline",
	    getDescentFont);
  getMethod(class, NAME_fixedWidth, NAME_property, "bool", 0,
	    "Boolean to indicate font is fixed-width",
	    getFixedWidthFont);
  getMethod(class, NAME_b16, NAME_encoding, "bool", 0,
	    "Boolean to indicate font is 16-bits",
	    getB16Font);
  getMethod(class, NAME_convert, NAME_conversion, "font", 1, "name",
	    "Convert logical font-name and @family_style_points",
	    getConvertFont);
  getMethod(class, NAME_lookup, NAME_oms, "font", 3, "name", "name", "[int]",
	    "Lookup in @fonts table",
	    getLookupFont);
  getMethod(class, NAME_defaultCharacter, NAME_property, "char", 0,
	    "Character painted for non-existing entries",
	    getDefaultCharacterFont);
  getMethod(class, NAME_domain, NAME_property, "tuple", 1, "[{x,y}]",
	    "Range of valid characters",
	    getDomainFont);
  getMethod(class, NAME_points, DEFAULT, "int", 0,
	    "Specified point-size or <-height",
	    getPointsFont);

  FontTable = globalObject(NAME_fonts, ClassHashTable, toInt(101), 0);

#ifdef __WINDOWS__
  attach_resource(class, "scale", "real", "1.4",
                "Multiplication factor for all fonts");
#endif

  succeed;
}
