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


status
makeClassPicture(Class class)
{ sourceClass(class, makeClassPicture, __FILE__, "$Revision$");

  termClass(class, "picture", 1, NAME_label, NAME_displaySize, NAME_display);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "label=[name]", "size=[size]", "display=[display]",
	     "Create from label, size and display",
	     initialisePicture);

  attach_resource(class, "size", "size", "size(400,200)",
		  "Default size in pixels");

  succeed;
}

