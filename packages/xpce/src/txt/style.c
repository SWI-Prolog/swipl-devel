/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/unix.h>
#include <h/text.h>

static status	highlightStyle(Style s, Bool on);
static status	underlineStyle(Style s, Bool on);
static status	greyStyle(Style s, Bool on);

static status
initialiseStyle(Style s, Image icon, FontObj font, Colour colour,
		Bool highlight, Bool underline, Bool bold, Bool grey)
{ if ( isDefault(icon) )
    icon = NIL;

  assign(s, font,   font);
  assign(s, icon,   icon);
  assign(s, colour, colour);
  s->attributes = 0;

  if ( notDefault(highlight) ) highlightStyle(s, highlight);
  if ( notDefault(underline) ) underlineStyle(s, underline);
  if ( notDefault(bold) )      boldStyle(s, bold);
  if ( notDefault(grey) )      greyStyle(s, grey);

  succeed;
}


static status
storeStyle(Style s, FileObj file)
{ storeSlotsObject(s, file);
  storeIntFile(file, toInt(s->attributes));

  succeed;
}


static status
loadStyle(Style s, FILE *fd, ClassDef def)
{ loadSlotsObject(s, fd, def);
  s->attributes = loadWord(fd);
  if ( s->font == NIL )			/* prior version 10 */
    assign(s, font, DEFAULT);
  if ( s->colour == NIL )
    assign(s, colour, DEFAULT);

  succeed;
}


static status
attribute_style(Style s, unsigned char att, Bool on)
{ if ( on == ON )
  { if ( s->attributes && att )
      succeed;
    s->attributes |= att;
  } else
  { if ( !(s->attributes && att) )
      succeed;
    s->attributes &= ~att;
  }

  succeed;
}


static status
highlightStyle(Style s, Bool on)
{ return attribute_style(s, TXT_HIGHLIGHTED, on);
}


static status
underlineStyle(Style s, Bool on)
{ return attribute_style(s, TXT_UNDERLINED, on);
}


static status
greyStyle(Style s, Bool on)
{ return attribute_style(s, TXT_GREYED, on);
}


status
boldStyle(Style s, Bool on)
{ return attribute_style(s, TXT_BOLDEN, on);
}


static status
closedStyle(Style s, Bool on)
{ return attribute_style(s, TXT_CLOSED, on);
}


static Bool
get_attribute_style(Style s, unsigned char att)
{ return (s->attributes && att) ? ON : OFF;
}


static Bool
getHighlightStyle(Style s)
{ return get_attribute_style(s, TXT_HIGHLIGHTED);
}


static Bool
getUnderlineStyle(Style s)
{ return get_attribute_style(s, TXT_UNDERLINED);
}


static Bool
getGreyStyle(Style s)
{ return get_attribute_style(s, TXT_GREYED);
}


static Bool
getBoldStyle(Style s)
{ return get_attribute_style(s, TXT_BOLDEN);
}


static Bool
getClosedStyle(Style s)
{ return get_attribute_style(s, TXT_CLOSED);
}


status
makeClassStyle(Class class)
{ sourceClass(class, makeClassStyle, __FILE__, "$Revision$");

  localClass(class, NAME_font, NAME_appearance, "[font]", NAME_both,
	     "Font of characters");
  localClass(class, NAME_colour, NAME_appearance, "[colour]", NAME_both,
	     "Colour of the characters");
  localClass(class, NAME_icon, NAME_appearance, "image*", NAME_both,
	     "Image for annotation margin");
  localClass(class, NAME_attributes, NAME_appearance, "alien:long", NAME_none,
	     "Character attributes");

  termClass(class, "style", 2, NAME_icon, NAME_font);
  setLoadStoreFunctionClass(class, loadStyle, storeStyle);

  sendMethod(class, NAME_initialise, DEFAULT, 7,
	     "icon=[image]*", "font=[font]", "colour=[colour]",
	     "highlight=[bool]", "underline=[bool]",
	     "bold=[bool]", "grey=[bool]",
	     "Create from icon, font, colour and attributes",
	     initialiseStyle);
  sendMethod(class, NAME_highlight, NAME_appearance, 1, "bool",
	     "Inverse video",
	     highlightStyle);
  sendMethod(class, NAME_underline, NAME_appearance, 1, "bool",
	     "Underlined text",
	     underlineStyle);
  sendMethod(class, NAME_grey, NAME_appearance, 1, "bool",
	     "Greyed-out text",
	     greyStyle);
  sendMethod(class, NAME_bold, NAME_appearance, 1, "bool",
	     "Bold text",
	     boldStyle);
  sendMethod(class, NAME_closed, NAME_appearance, 1, "bool",
	     "Make text invisible",
	     closedStyle);

  getMethod(class, NAME_highlight, NAME_appearance, "bool", 0,
	    "Boolean to indicate inverse video",
	    getHighlightStyle);
  getMethod(class, NAME_underline, NAME_appearance, "bool", 0,
	    "Boolean to indicate underline",
	    getUnderlineStyle);
  getMethod(class, NAME_grey, NAME_appearance, "bool", 0,
	    "Boolean to indicate grey",
	    getGreyStyle);
  getMethod(class, NAME_bold, NAME_appearance, "bool", 0,
	    "Boolean to indicate bold",
	    getBoldStyle);
  getMethod(class, NAME_closed, NAME_appearance, "bool", 0,
	    "Boolean to indicate closed",
	    getClosedStyle);

  succeed;
}

