/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include "boxes.h"

static status computeLBox(LBox lb);

status
initialiseLBox(LBox lb, CharArray text, Style style)
{ if ( isDefault(style) )
    style = getClassVariableValueObject(lb, NAME_style);

  assign(lb, text,  text);
  assign(lb, style, style);

  return computeLBox(lb);
}

		 /*******************************
		 *	      COMPUTE		*
		 *******************************/

static FontObj
getFontLBox(LBox lb)
{ if ( notDefault(lb->style->font) )
    answer(lb->style->font);
  
  answer(getClassVariableValueObject(lb, NAME_font));
}


static status
computeLBox(LBox lb)
{ FontObj f = getFontLBox(lb);

  assign(lb, width,   getWidthFont(f, lb->text));
  assign(lb, ascent,  getAscentFont(f));
  assign(lb, descent, getDescentFont(f));

  succeed;
}

		 /*******************************
		 *	      REDRAW		*
		 *******************************/

void
drawLBox(LBox lb, int x, int y, int w)
{ FontObj f = getFontLBox(lb);
  Style s = lb->style;
  Colour old_colour = NULL;

  if ( notDefault(s->colour) )
    old_colour = r_colour(s->colour);

  s_print(&lb->text->data, x, y, f);

  if ( old_colour )
    r_colour(old_colour);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "text=char_array", "style=[style]" };

/* Instance Variables */

static vardecl var_tbox[] =
{ IV(NAME_text, "char_array", IV_GET,
     NAME_content, "Represented text"),
  IV(NAME_style, "style", IV_GET,
     NAME_appearance, "Appearence of the text")
};

/* Send Methods */

static senddecl send_tbox[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseLBox,
     DEFAULT, "Create tbox from text and style")
};

/* Get Methods */

#define get_tbox NULL
/*
static getdecl get_tbox[] =
{ 
};
*/

/* Resources */

static classvardecl rc_tbox[] =
{ RC(NAME_style, "style", "style()", "Appearance of text"),
  RC(NAME_font,  "font",  "normal",  "Font if <-style has no font")
};

/* Class Declaration */

static Name tbox_termnames[] = { NAME_text, NAME_style };

ClassDecl(tbox_decls,
          var_tbox, send_tbox, get_tbox, rc_tbox,
          2, tbox_termnames,
          "$Rev$");


status
makeClassLBox(Class class)
{ return declareClass(class, &tbox_decls);
}

