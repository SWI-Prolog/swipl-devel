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
initialiseFormat(Format l, Name direction, Int width, Bool columns)
{ assign(l, direction, isDefault(direction) ? NAME_horizontal : direction);
  assign(l, width, isDefault(width) ? ONE : width);
  assign(l, columns, isDefault(columns) ? ON : columns);
  assign(l, column_sep, toInt(10));
  assign(l, row_sep, toInt(10));
  assign(l, adjustment, NIL);

  succeed;
}


status
makeClassFormat(Class class)
{ sourceClass(class, makeClassFormat, __FILE__, "$Revision$");

  localClass(class, NAME_direction, NAME_orientation,
	     "{horizontal,vertical}", NAME_both,
	     "horizontal (rows) or vertical (columns)");
  localClass(class, NAME_width, NAME_dimension, "int", NAME_both,
	     "Width in columns/rows or pixels");
  localClass(class, NAME_columns, NAME_table, "bool", NAME_both,
	     "Use columns/rows");
  localClass(class, NAME_columnSep, NAME_table, "int", NAME_both,
	     "Distance between columns/rows");
  localClass(class, NAME_rowSep, NAME_table, "int", NAME_both,
	     "Distance between rows/columns");
  localClass(class, NAME_adjustment, NAME_table, "vector*", NAME_both,
	     "left,center,right alignment");

  termClass(class, "format", 3, NAME_direction, NAME_width, NAME_columns);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "orientation=[{horizontal,vertical}]",
	     "width=[1..]", "columns=[bool]",
	     "Create from direction, width and columns",
	     initialiseFormat);

  succeed;
}

