/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialiseBox(Box b, Int w, Int h)
{ initialiseGraphical(b, ZERO, ZERO, w, h);
  assign(b, radius,	  ZERO);
  assign(b, shadow,	  ZERO);
  assign(b, fill_pattern, NIL);

  succeed;
}


static status
RedrawAreaBox(Box b, Area a)
{ int x, y, w, h;

  initialiseDeviceGraphical(b, &x, &y, &w, &h);

  r_thickness(valInt(b->pen));
  r_dash(b->texture);
  r_shadow_box(x, y, w, h,
	       valInt(b->radius), valInt(b->shadow), b->fill_pattern);

  return RedrawAreaGraphical(b, a);
}


static status
radiusBox(Box b, Int r)
{ if (r != b->radius)
  { CHANGING_GRAPHICAL(b, assign(b, radius, r);
		          changedEntireImageGraphical(b));
  }

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "width=[int]", "height=[int]" };

/* Instance Variables */

static vardecl var_box[] =
{ SV(NAME_radius, "int", IV_GET|IV_STORE, radiusBox,
     NAME_appearance, "Rounding radius for corners"),
  SV(NAME_shadow, "int", IV_GET|IV_STORE, shadowGraphical,
     NAME_appearance, "Shadow at bottom-right of box"),
  SV(NAME_fillPattern, "image|colour*", IV_GET|IV_STORE, fillPatternGraphical,
     NAME_appearance, "Fill pattern for internals")
};

/* Send Methods */

static senddecl send_box[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseBox,
     DEFAULT, "Create box from width and height"),
  SM(NAME_DrawPostScript, 0, NULL, drawPostScriptBox,
     NAME_postscript, "Create PostScript")
};

/* Get Methods */

#define get_box NULL
/*
static getdecl get_box[] =
{ 
};
*/

/* Resources */

#define rc_box NULL
/*
static resourcedecl rc_box[] =
{ 
};
*/

/* Class Declaration */

static Name box_termnames[] = { NAME_width, NAME_height };

ClassDecl(box_decls,
          var_box, send_box, get_box, rc_box,
          2, box_termnames,
          "$Rev$");


status
makeClassBox(Class class)
{ declareClass(class, &box_decls);

  cloneStyleVariableClass(class, NAME_fillPattern, NAME_reference);
  setRedrawFunctionClass(class, RedrawAreaBox);

  succeed;
}
