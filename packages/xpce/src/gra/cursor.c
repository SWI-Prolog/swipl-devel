/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

forwards status XcloseCursor P((CursorObj, DisplayObj));

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Creating cursors.

In X, cursors can be created two ways: from the cursor  font  and from
two pixmaps.  PCE  supports both ways  to create a   cursor.  For this
reason various instantiation patterns for cursors exist.

?- new(C, cursor(Name))
	Create cursor from cursor font
?- new(C, cursor(Name, Source, [Mask], [X, Y]
	Create cursor from an image.  If Mask is not supplied it defaults
	to Source.  If X and Y are not supplied they default to (0,0) This
	function is in the first place meant to maintain compatibility with
	the SunView version of cursors.

Cursors from now on are shared objects like fonts.  That  is, a second
`new' to a  cursor of the  same name returns  the same  cursor object.
This because they are limited resources on the X-server.
-  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
initialiseCursor(CursorObj c, Name name,
		 Image image, Image mask,
		 Point hot,
		 Colour foreground, Colour background)
{ char tmp[100];

  assign(c, name, name);

  if ( isDefault(image) )
  { if ( ws_cursor_font_index(name) == FAIL )
      return errorPce(NAME_noNamedCursor, name);

    assign(c, font_id, DEFAULT);
  } else
  { if ( isDefault(mask) ) mask = image;
    if ( isDefault(hot) )  hot  = newObject(ClassPoint, 0);

    assign(c, image,      image);
    assign(c, mask,       mask);
    assign(c, hot_spot,   hot);
    assign(c, foreground, foreground);
    assign(c, background, background);
  }

  if ( notNil(name) )
  { protectObject(c);
    sprintf(tmp, "%s_cursor", strName(c->name));
    newAssoc(CtoKeyword(tmp), c);
    appendHashTable(CursorTable, c->name, c);
  }

  succeed;
}


static status
unlinkCursor(CursorObj c)
{ XcloseCursor(c, DEFAULT);

  succeed;
}


static CursorObj
getLookupCursor(Class class, Name name)
{ answer(getMemberHashTable(CursorTable, name));
}


static status
XopenCursor(CursorObj c, DisplayObj d)
{ return ws_create_cursor(c, d);
}


static status
XcloseCursor(CursorObj c, DisplayObj d)
{ ws_destroy_cursor(c, d);

  succeed;
}


static CursorObj
getConvertCursor(Class class, Name name)
{ CursorObj c;

  if ( (c = getMemberHashTable(CursorTable, name)) )
    answer(c);
  if ( syntax.uppercase &&
       (c = getMemberHashTable(CursorTable, CtoKeyword(strName(name)))) )
    answer(c);
    
  return answerObject(ClassCursor, name, 0);
}


status
makeClassCursor(Class class)
{ sourceClass(class, makeClassCursor, __FILE__, "$Revision$");

  localClass(class, NAME_name, NAME_name, "name*", NAME_get,
	     "Name of the cursor");
  localClass(class, NAME_fontId, NAME_appearance, "[int]*", NAME_get,
	     "Id in X-cursor font");
  localClass(class, NAME_image, NAME_appearance, "image*", NAME_get,
	     "User-defined image");
  localClass(class, NAME_mask, NAME_appearance, "image*", NAME_get,
	     "User-defined mask");
  localClass(class, NAME_hotSpot, NAME_appearance, "point*", NAME_get,
	     "User-defined hot spot");
  localClass(class, NAME_foreground, NAME_appearance, "[colour]*", NAME_get,
	     "Foreground colour of the cursor");
  localClass(class, NAME_background, NAME_appearance, "[colour]*", NAME_get,
	     "Background colour of the cursor");

  termClass(class, "cursor", 1, NAME_name);
  cloneStyleClass(class, NAME_none);

  sendMethod(class, NAME_initialise, DEFAULT, 6,
	     "name=name*", "image=[image]", "mask=[image]", "hot_spot=[point]",
	     "foreground=[colour]", "background=[colour]",
	     "Create from name or name, image, mask, hot_spot",
	     initialiseCursor);
  sendMethod(class, NAME_unlink, DEFAULT, 0,
	     "Destroy the cursor",
	     unlinkCursor);
  sendMethod(class, NAME_Xopen, NAME_x, 1, "display",
	     "Create X-cursor on display",
	     XopenCursor);
  sendMethod(class, NAME_Xclose, NAME_x, 1, "display",
	     "Destroy X-cursor on display",
	     XcloseCursor);

  getMethod(class, NAME_convert, NAME_conversion, "cursor", 1, "name",
	    "Convert cursor-name to cursor",
	    getConvertCursor);
  getMethod(class, NAME_lookup, NAME_oms, "cursor", 1, "name",
	    "Lookup from @cursors table",
	    getLookupCursor);

  CursorTable = globalObject(NAME_cursors, ClassHashTable, toInt(32), 0);
  ws_init_cursor_font();

  succeed;
}


