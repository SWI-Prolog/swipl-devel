/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>


static status
initialisePicture(Picture p, Name name, Size size, DisplayObj display)
{ initialiseWindow((PceWindow) p, name, size, display);

  return send(p, NAME_scrollbars, NAME_both, 0);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "label=[name]", "size=[size]", "display=[display]" };

/* Instance Variables */

#define var_picture NULL
/*
vardecl var_picture[] =
{ 
};
*/

/* Send Methods */

static senddecl send_picture[] =
{ SM(NAME_initialise, 3, T_initialise, initialisePicture,
     DEFAULT, "Create from label, size and display")
};

/* Get Methods */

#define get_picture NULL
/*
static getdecl get_picture[] =
{ 
};
*/

/* Resources */

static resourcedecl rc_picture[] =
{ RC(NAME_size, "size", "size(400,200)",
     "Default size in pixels")
};

/* Class Declaration */

static Name picture_termnames[] = { NAME_label, NAME_displaySize, NAME_display };

ClassDecl(picture_decls,
          var_picture, send_picture, get_picture, rc_picture,
          1, picture_termnames,
          "$Rev$");


status
makeClassPicture(Class class)
{ return declareClass(class, &picture_decls);
}

