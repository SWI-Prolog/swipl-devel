/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

static status
initialiseSize(Size s, Int w, Int h)
{ if ( isDefault(w) ) w = ZERO;
  if ( isDefault(h) ) h = ZERO;
  assign(s, w, w);
  assign(s, h, h);

  succeed;
}


static Size
getConvertSize(Class class, Name name)
{ int w, h;

  if ( isstr8(&name->data) &&
       (sscanf(name->data.s_text8, "%dx%d", &w, &h) == 2 ||
       (syntax.uppercase && sscanf(name->data.s_text8, "%dX%d", &w, &h) == 2)))
    answer(newObject(ClassSize, toInt(w), toInt(h), 0));

  fail;
}


static StringObj
getPrintNameSize(Size s)
{ char buf[200];

  sprintf(buf, "%ldx%ld", valInt(s->w), valInt(s->h));
  answer(CtoString(buf));
}


status
equalSize(Size s, Size s2)
{ if (s->w == s2->w && s->h == s2->h)
    succeed;
  fail;
}


static status
unionSize(Size s, Size s2)
{ if (valInt(s->w) < valInt(s2->w))
    assign(s, w, s2->w);
  if (valInt(s->h) < valInt(s2->h))
    assign(s, h, s2->h);
  succeed;
}


status
copySize(Size s, Size s2)
{ assign(s, w, s2->w);
  assign(s, h, s2->h);

  succeed;
}


status
setSize(Size s, Int w, Int h)
{ if ( notDefault(w) ) assign(s, w, w);
  if ( notDefault(w) ) assign(s, h, h);

  succeed;
}


static status
offsetSize(Size s, Int w, Int h)
{ assign(s, w, add(w, s->w));
  assign(s, h, add(h, s->h));

  succeed;
}


status
makeClassSize(Class class)
{ sourceClass(class, makeClassSize, __FILE__, "$Revision$");

  localClass(class, NAME_width, NAME_dimension, "int", NAME_both,
	     "Width (W) of the size");
  localClass(class, NAME_height, NAME_dimension, "int", NAME_both,
	     "Height (H) of the size");

  termClass(class, "size", 2, NAME_width, NAME_height);

  sendMethod(class, NAME_initialise, DEFAULT, 2, "width=[int]", "height=[int]",
	     "Create size from width and height",
	     initialiseSize);
  sendMethod(class, NAME_copy, NAME_copy, 1, "size",
	     "Copy W and H from argument",
	     copySize);
  sendMethod(class, NAME_equal, NAME_compare, 1, "size",
	     "Test if equal to argument",
	     equalSize);
  sendMethod(class, NAME_offset, NAME_calculate, 2, "width=int", "height=int",
	     "Add 1st argument to W, 2nd to H",
	     offsetSize);
  sendMethod(class, NAME_set, NAME_calculate, 2, "width=[int]", "height=[int]",
	     "Set W and H from arguments",
	     setSize);
  sendMethod(class, NAME_union, NAME_calculate, 1, "size",
	     "set W and H to maximum of the two",
	     unionSize);

  getMethod(class, NAME_printName, NAME_textual, "string", 0,
	    "Printed representation as %dx%d",
	    getPrintNameSize);
  getMethod(class, NAME_convert, NAME_textual, "size", 1, "name",
	    "Convert text `WxH'",
	    getConvertSize);

  succeed;
}
