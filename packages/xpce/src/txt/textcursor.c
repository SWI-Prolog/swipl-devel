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
initialiseTextCursor(TextCursor c, Int w, Int h)
{ initialiseGraphical(c, ZERO, ZERO, w, h);
  assign(c, style, NAME_block);

  succeed;
}


static status
RedrawAreaTextCursor(TextCursor c, Area a)
{ int x, y, w, h;

  initialiseDeviceGraphical(c, &x, &y, &w, &h);

  if ( equalName(c->style, NAME_arrow) )
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
  } else if ( equalName(c->style, NAME_image) )
  { r_image(c->image, 0, 0, x, y, w, h);
  } else /*if ( equalName(c->style, NAME_block) )*/
  { if ( c->active == ON )
      r_complement(x, y, w, h);
    else
      r_box(x, y, w, h, 0, NIL);
  }

  succeed;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set the  text_cursor; (x,y) is  the top-right corner  of the character
after which the insertion point is.  h is the height  of the line, y+b
is  the  baseline  of the line.   w is the width   of the font  of the
character.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
setTextCursor(TextCursor c, Int x, Int y, Int w, Int h, Int b)
{ if ( equalName(c->style, NAME_arrow) )
    return geometryGraphical(c, dif(x, w), y, w, h);
  if ( equalName(c->style, NAME_image) )
    return geometryGraphical(c,
			     sub(x, c->hot_spot->x),
			     sub(add(y, b), c->hot_spot->y),
			     c->image->size->w, c->image->size->h);
  return geometryGraphical(c, x, y, w, h);
}

		/********************************
		*           ATTRIBUTES		*
		********************************/


status
styleTextCursor(TextCursor c, Name style)
{ if ( equalName(style, NAME_image) &&
       (isNil(c->image) || isNil(c->hot_spot)) )
    return errorPce(c, NAME_needImageAndHotSpot);

  CHANGING_GRAPHICAL(c,
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
	     "{arrow,image,block}", NAME_get,
	     "One of {arrow,image,block}");
  localClass(class, NAME_image, NAME_appearance, "image*", NAME_get,
	     "Image when <->style is image");
  localClass(class, NAME_hotSpot, NAME_appearance, "point*", NAME_get,
	     "The `hot-spot' of the image");

  termClass(class, "text_cursor", 2, NAME_width, NAME_height);
  setRedrawFunctionClass(class, RedrawAreaTextCursor);

  storeMethod(class, NAME_image,  imageTextCursor);
  storeMethod(class, NAME_style,  styleTextCursor);

  sendMethod(class, NAME_initialise, DEFAULT, 2, "width=int", "heigth=int",
	     "Create from width and hight",
	     initialiseTextCursor);
  sendMethod(class, NAME_set, NAME_area, 5,
	     "x=int", "y=int", "width=int", "height=int", "baselin=int",
	     "Set x, y, w, h and baseline",
	     setTextCursor);

  succeed;
}

