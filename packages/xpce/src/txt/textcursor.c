/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/text.h>

static status
initialiseTextCursor(TextCursor c, FontObj font)
{ initialiseGraphical(c, ZERO, ZERO, ZERO, ZERO);

  if ( notDefault(font) )
    return fontTextCursor(c, font);
  else
    return styleTextCursor(c, getResourceValueObject(c, NAME_style));
}


static status
RedrawAreaTextCursor(TextCursor c, Area a)
{ int x, y, w, h;

  initialiseDeviceGraphical(c, &x, &y, &w, &h);

  if ( c->style == NAME_arrow )
  { int cx = x+w/2;			/* TBD: consider r_caret()! */
    struct ipoint pts[3];
    
    r_thickness(1);
    r_dash(NAME_none);
    r_line(cx, y, cx, y+h-1);
    
    pts[0].x = x;
    pts[0].y = y+h;
    pts[1].x = x + w;
    pts[1].y = y+h;
    pts[2].x = cx;
    pts[2].y = y + h - (h+2)/3;
    
    r_fillpattern(c->active == ON ? BLACK_IMAGE : GREY50_IMAGE);
    r_fill_polygon(pts, 3);
  } else if ( c->style == NAME_image )
  { r_image(c->image, 0, 0, x, y, w, h, ON);
  } else if ( c->style == NAME_openLook )
  { if ( c->active == ON )
    { int cx = x + w/2;
      Any colour = getDisplayColourGraphical((Graphical)c);

      r_fillpattern(colour ? colour : (Any) BLACK_IMAGE);
      r_fill_triangle(cx, y, x, y+h, x+w, y+h);
    } else
    { struct ipoint pts[4];
      int cx = x + w/2;
      int cy = y + h/2;
      int i = 0;

      pts[i].x = cx;  pts[i].y = y;   i++;
      pts[i].x = x;   pts[i].y = cy;  i++;
      pts[i].x = cx;  pts[i].y = y+h; i++;
      pts[i].x = x+w; pts[i].y = cy;  i++;
      
      r_fillpattern(GREY50_IMAGE);
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


status
fontTextCursor(TextCursor c, FontObj font)
{ Int h = getHeightFont(font);
  Int w = getExFont(font);
  Name style = getResourceValueObject(c, getFixedWidthFont(font) == ON ?
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


status
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


status
makeClassTextCursor(Class class)
{ sourceClass(class, makeClassTextCursor, __FILE__, "$Revision$");

  localClass(class, NAME_style, NAME_appearance,
	     "{arrow,image,block,open_look}", NAME_get,
	     "How the text_cursor object is visualised");
  localClass(class, NAME_image, NAME_appearance, "image*", NAME_get,
	     "Image when <->style is image");
  localClass(class, NAME_hotSpot, NAME_appearance, "point*", NAME_get,
	     "The `hot-spot' of the image");

  termClass(class, "text_cursor", 1, NAME_style);
  setRedrawFunctionClass(class, RedrawAreaTextCursor);

  storeMethod(class, NAME_image,  imageTextCursor);
  storeMethod(class, NAME_style,  styleTextCursor);

  sendMethod(class, NAME_initialise, DEFAULT, 1, "for=[font]",
	     "Create for specified font",
	     initialiseTextCursor);
  sendMethod(class, NAME_set, NAME_area, 5,
	     "x=int", "y=int", "width=int", "height=int", "baselin=int",
	     "Set x, y, w, h and baseline",
	     setTextCursor);
  sendMethod(class, NAME_font, NAME_appearance, 1, "font",
	     "Set ->style according to font",
	     fontTextCursor);

  variable_resource(class, NAME_style, "open_look");
  attach_resource(class, "fixed_font_style", "name", "block",
		  "->style for fixed fonts");
  attach_resource(class, "proportional_font_style", "name", "open_look",
		  "->style for proportional fonts");

  succeed;
}

