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

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialisePen(Pen p, Int thickness, Name texture, Any colour)
{ if ( isDefault(thickness) )
    thickness = ONE;
  if ( isDefault(texture) )
    texture = NAME_none;

  assign(p, thickness, thickness);
  assign(p, texture,   texture);
  assign(p, colour,    colour);

  succeed;
}


static Pen
getConvertPen(Class class, Int thickness)
{ answer(newObject(ClassPen, thickness, EAV));
}


status
makeClassPen(Class class)
{ sourceClass(class, makeClassPen, __FILE__, "$Revision$");

  localClass(class, NAME_thickness, NAME_dimension, "0..", NAME_both,
	     "Thickness of the line (pixels)");
  localClass(class, NAME_texture, NAME_appearance, "texture_name", NAME_both,
	     "Dash pattern");
  localClass(class, NAME_colour, NAME_appearance, "[colour]", NAME_both,
	     "Colour of the line");

  termClass(class, "pen", 3, NAME_thickness, NAME_texture, NAME_colour);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "thickness=[0..]",
	     "[texture=texture_name]",
	     "colour=[colour|pixmap]",
	     "Create pen from thickness, texture and colour",
	     initialisePen);

  getMethod(class, NAME_convert, DEFAULT, "pen", 1, "0..",
	    "Convert to pen of indicated thickness",
	    getConvertPen);

  succeed;
}
