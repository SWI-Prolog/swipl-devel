/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/text.h>

static status	fontTextCursor(TextCursor c, FontObj font);
static status	styleTextCursor(TextCursor c, Name style);

static status
initialiseTextCursor(TextCursor c, FontObj font)
{ initialiseGraphical(c, ZERO, ZERO, ZERO, ZERO);

  if ( notDefault(font) )
    return fontTextCursor(c, font);
  else
    return styleTextCursor(c, getClassVariableValueObject(c, NAME_style));
}


static status
RedrawAreaTextCursor(TextCursor c, Area a)
{ int x, y, w, h;

  initialiseDeviceGraphical(c, &x, &y, &w, &h);

  if ( c->style == NAME_arrow )
  { int cx = x+w/2;			/* TBD: consider r_caret()! */
    ipoint pts[3];
    
    r_thickness(1);
    r_dash(NAME_none);
    r_line(cx, y, cx, y+h-1);
    
    pts[0].x = x;
    pts[0].y = y+h;
    pts[1].x = x + w;
    pts[1].y = y+h;
    pts[2].x = cx;
    pts[2].y = y + h - (h+2)/3;
    
    r_fillpattern(c->active == ON ? BLACK_IMAGE : GREY50_IMAGE,
		  NAME_foreground);
    r_fill_polygon(pts, 3);
  } else if ( c->style == NAME_image )
  { r_image(c->image, 0, 0, x, y, w, h, ON);
  } else if ( c->style == NAME_openLook )
  { if ( c->active == ON )
    { int cx = x + w/2;
      Any colour = getDisplayColourGraphical((Graphical)c);

      r_fillpattern(colour ? colour : (Any) BLACK_IMAGE, NAME_foreground);
      r_fill_triangle(cx, y, x, y+h, x+w, y+h);
    } else
    { ipoint pts[4];
      int cx = x + w/2;
      int cy = y + h/2;
      int i = 0;

      pts[i].x = cx;  pts[i].y = y;   i++;
      pts[i].x = x;   pts[i].y = cy;  i++;
      pts[i].x = cx;  pts[i].y = y+h; i++;
      pts[i].x = x+w; pts[i].y = cy;  i++;
      
      r_fillpattern(GREY50_IMAGE, NAME_foreground);
      r_fill_polygon(pts, i);
    }
  } else /*if ( c->style == NAME_block )*/
  { if ( c->active == ON )
      r_complement(x, y, w, h);
    else
      r_box(x, y, w, h, 0, NIL);
  }

  succeed;
}


static status
fontTextCursor(TextCursor c, FontObj font)
{ Int h = getHeightFont(font);
  Int w = getExFont(font);
  Name style = getClassVariableValueObject(c, getFixedWidthFont(font) == ON ?
				      		NAME_fixedFontStyle :
						NAME_proportionalFontStyle);

  geometryGraphical(c, DEFAULT, DEFAULT, w, h);

  if ( style )
    return styleTextCursor(c, style);

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set the  text_cursor; (x,y) is  the top-right corner  of the character
after which the insertion point is.  h is the height  of the line, y+b
is  the  baseline  of the line.   w is the width   of the font  of the
character.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define OL_CURSOR_SIZE	9

status
setTextCursor(TextCursor c, Int x, Int y, Int w, Int h, Int b)
{ if ( c->style == NAME_arrow )
    return geometryGraphical(c, dif(x, w), y, w, h);
  if ( c->style == NAME_image )
    return geometryGraphical(c,
			     sub(x, c->hot_spot->x),
			     sub(add(y, b), c->hot_spot->y),
			     c->image->size->w, c->image->size->h);
  if ( c->style == NAME_openLook )
    return geometryGraphical(c,
			     sub(x, toInt(OL_CURSOR_SIZE/2)),
			     sub(add(y, b), ONE),
			     toInt(OL_CURSOR_SIZE),
			     toInt(OL_CURSOR_SIZE));
  return geometryGraphical(c, x, y, w, h);
}

		/********************************
		*           ATTRIBUTES		*
		********************************/


static status
styleTextCursor(TextCursor c, Name style)
{ Int w = DEFAULT;
  Int h = DEFAULT;

  if ( style == NAME_image &&
       (isNil(c->image) || isNil(c->hot_spot)) )
    return errorPce(c, NAME_needImageAndHotSpot);

  if ( style == NAME_openLook )
    w = h = toInt(OL_CURSOR_SIZE);

  CHANGING_GRAPHICAL(c,
		     geometryGraphical(c, DEFAULT, DEFAULT, w, h);
		     assign(c, style, style);
		     changedEntireImageGraphical(c));

  succeed;
}


static status
imageTextCursor(TextCursor c, Image image, Point hot)
{ CHANGING_GRAPHICAL(c,
	assign(c, image,    image);
	assign(c, hot_spot, hot);
	assign(c, style,    NAME_image);
	changedEntireImageGraphical(c));
  
  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_set[] =
        { "x=int", "y=int", "width=int", "height=int", "baseline=int" };

/* Instance Variables */

static vardecl var_textCursor[] =
{ SV(NAME_style, "{arrow,image,block,open_look}", IV_GET|IV_STORE,
     styleTextCursor,
     NAME_appearance, "How the text_cursor object is visualised"),
  SV(NAME_image, "image*", IV_GET|IV_STORE, imageTextCursor,
     NAME_appearance, "Image when <->style is image"),
  IV(NAME_hotSpot, "point*", IV_GET,
     NAME_appearance, "The `hot-spot' of the image")
};

/* Send Methods */

static senddecl send_textCursor[] =
{ SM(NAME_initialise, 1, "for=[font]", initialiseTextCursor,
     DEFAULT, "Create for specified font"),
  SM(NAME_font, 1, "font", fontTextCursor,
     NAME_appearance, "Set ->style according to font"),
  SM(NAME_set, 5, T_set, setTextCursor,
     NAME_area, "Set x, y, w, h and baseline")
};

/* Get Methods */

#define get_textCursor NULL
/*
static getdecl get_textCursor[] =
{ 
};
*/

/* Resources */

static classvardecl rc_textCursor[] =
{ RC(NAME_fixedFontStyle, "name", "open_look",
     "->style for fixed fonts"),
  RC(NAME_proportionalFontStyle, "name", "open_look",
     "->style for proportional fonts"),
  RC(NAME_style, NULL, "open_look", NULL),
  RC(NAME_colour, RC_REFINE, "when(@colour_display, red, black)", NULL),
  RC(NAME_inactiveColour, RC_REFINE, "black", NULL)
};

/* Class Declaration */

static Name textCursor_termnames[] = { NAME_style };

ClassDecl(textCursor_decls,
          var_textCursor, send_textCursor, get_textCursor, rc_textCursor,
          1, textCursor_termnames,
          "$Rev$");

status
makeClassTextCursor(Class class)
{ declareClass(class, &textCursor_decls);
  setRedrawFunctionClass(class, RedrawAreaTextCursor);

  succeed;
}

