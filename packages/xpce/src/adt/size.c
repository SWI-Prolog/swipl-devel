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

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static const char *T_widthADintD_heightADintD[] =
        { "width=[int]", "height=[int]" };
static const char *T_offset[] =
        { "width=int", "height=int" };

/* Instance Variables */

static const vardecl var_size[] =
{ IV(NAME_width, "int", IV_BOTH,
     NAME_dimension, "Width (W) of the size"),
  IV(NAME_height, "int", IV_BOTH,
     NAME_dimension, "Height (H) of the size")
};

/* Send Methods */

static const senddecl send_size[] =
{ SM(NAME_initialise, 2, T_widthADintD_heightADintD, initialiseSize,
     DEFAULT, "Create size from width and height"),
  SM(NAME_offset, 2, T_offset, offsetSize,
     NAME_calculate, "Add 1st argument to W, 2nd to H"),
  SM(NAME_set, 2, T_widthADintD_heightADintD, setSize,
     NAME_calculate, "Set W and H from arguments"),
  SM(NAME_union, 1, "size", unionSize,
     NAME_calculate, "set W and H to maximum of the two"),
  SM(NAME_equal, 1, "size", equalSize,
     NAME_compare, "Test if equal to argument"),
  SM(NAME_copy, 1, "size", copySize,
     NAME_copy, "Copy W and H from argument")
};

/* Get Methods */

static const getdecl get_size[] =
{ GM(NAME_convert, 1, "size", "name", getConvertSize,
     NAME_textual, "Convert text `WxH'"),
  GM(NAME_printName, 0, "string", NULL, getPrintNameSize,
     NAME_textual, "Printed representation as %dx%d")
};

/* Resources */

static const resourcedecl rc_size[] =
{ 
};

/* Class Declaration */

static Name size_termnames[] = { NAME_width, NAME_height };

ClassDecl(size_decls,
          var_size, send_size, get_size, rc_size,
          2, size_termnames,
          "$Rev$");


status
makeClassSize(Class class)
{ return declareClass(class, &size_decls);
}
