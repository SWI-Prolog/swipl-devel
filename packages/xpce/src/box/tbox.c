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

#include "boxes.h"

static status computeTBox(TBox tb);

status
initialiseTBox(TBox tb, CharArray text, Style style)
{ obtainClassVariablesObject(tb);

  assign(tb, text,  text);
  if ( notDefault(style) )
    assign(tb, style, style);

  return computeTBox(tb);
}

		 /*******************************
		 *	      COMPUTE		*
		 *******************************/

static FontObj
getFontTBox(TBox tb)
{ if ( notDefault(tb->style->font) )
    answer(tb->style->font);
  
  answer(getClassVariableValueObject(tb, NAME_font));
}


static status
computeTBox(TBox tb)
{ FontObj f = getFontTBox(tb);

  assign(tb, width,   getWidthFont(f, tb->text));
  assign(tb, ascent,  getAscentFont(f));
  assign(tb, descent, getDescentFont(f));

  succeed;
}


HBox
getSpaceHBoxFont(FontObj f)
{ return answerObject(ClassHBox,
		      getAdvanceFont(f, (CharArray)name_space),
		      getAscentFont(f),
		      getDescentFont(f),
		      findGlobal(NAME_spaceRubber),
		      EAV);
}


static HBox
getSpaceTBox(TBox tb)
{ FontObj f = getFontTBox(tb);

  return getSpaceHBoxFont(f);
}


		 /*******************************
		 *	      REDRAW		*
		 *******************************/

void
drawTBox(TBox tb, int x, int y, int w)
{ FontObj f = getFontTBox(tb);
  Style s = tb->style;
  Colour old_colour = NULL;

  if ( notDefault(s->colour) )
    old_colour = r_colour(s->colour);

  s_print_aligned(&tb->text->data, x, y, f);
  if ( s->attributes & TXT_UNDERLINED )
  { r_thickness(1);
    r_dash(NAME_none);
    r_line(x, y+1, x+w, y+1);
  }

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
{ SM(NAME_initialise, 2, T_initialise, initialiseTBox,
     DEFAULT, "Create tbox from text and style")
};

/* Get Methods */

static getdecl get_tbox[] =
{ GM(NAME_space, 0, "hbox", NULL, getSpaceTBox,
     NAME_layout, "Yield hbox for a space compatible with tbox")
};

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
makeClassTBox(Class class)
{ return declareClass(class, &tbox_decls);
}

