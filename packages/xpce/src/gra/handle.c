/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/arith.h>

static status
initialiseHandle(Handle h, Expression x, Expression y, Name kind, Name name)
{ if ( isDefault(kind) )
    kind = NAME_link;
  if ( isDefault(name) )
    name = kind;

  assign(h, xPosition, x);
  assign(h, yPosition, y);
  assign(h, kind, kind);
  assign(h, name, name);

  succeed;
}


status
getXYHandle(Handle h, Graphical gr, Device dev, Int *X, Int *Y)
{ Int x, y, gx, gy;

  if ( isDefault(dev) )
    dev = gr->device;

  TRY( get_absolute_xy_graphical(gr, &dev, &gx, &gy) );

  if ( X )
  { TRY(x = getValueExpression(h->xPosition, VarW, gr->area->w, 0));
    *X = add(x, gx);
  }
  if ( Y )
  { TRY(y = getValueExpression(h->yPosition, VarH, gr->area->h, 0));
    *Y = add(y, gy);
  }
  DEBUG(NAME_handle,
	Cprintf("handle %s on gr=%s,dev=%s at x=%s,y=%s\n",
		pp(h->name), pp(gr), pp(dev), X?"":pp(*X), Y?"":pp(*Y)));

  succeed;
}


static Point
getPositionHandle(Handle h, Graphical gr, Device dev)
{ Int x, y;

  TRY(getXYHandle(h, gr, dev, &x, &y));

  answer(answerObject(ClassPoint, x, y, 0));
}


Int
getXHandle(Handle h, Graphical gr, Device dev)
{ Int x;

  TRY(getXYHandle(h, gr, dev, &x, NULL));

  answer(x);
}


Int
getYHandle(Handle h, Graphical gr, Device dev)
{ Int y;

  TRY(getXYHandle(h, gr, dev, NULL, &y));

  answer(y);
}


status
makeClassHandle(Class class)
{ sourceClass(class, makeClassHandle, __FILE__, "$Revision$");

  localClass(class, NAME_xPosition, NAME_location, "expression", NAME_both,
	     "Expression for X in variable `w'");
  localClass(class, NAME_yPosition, NAME_location, "expression", NAME_both,
	     "Expression for Y in variable `h'");
  localClass(class, NAME_kind, NAME_relation, "name", NAME_both,
	     "Kind of valid connection end-point");
  localClass(class, NAME_name, NAME_name, "name", NAME_both,
	     "Logical name of handle");

  termClass(class, "handle",
	    4, NAME_xPosition, NAME_yPosition, NAME_kind, NAME_name);

  sendMethod(class, NAME_initialise, DEFAULT,
	     4, "x=expression", "y=expression", "kind=[name]", "name=[name]",
	     "Create from X-, Y expression, kind and name",
	     initialiseHandle);

  getMethod(class, NAME_x, NAME_location, "int", 2,
	    "relative_to=graphical", "coordinate_system_of=[device]",
	    "X-position on graphical relative to device",
	    getXHandle);
  getMethod(class, NAME_y, NAME_location, "int", 2,
	    "relative_to=graphical", "coordinate_system_of=[device]",
	    "Y-position on graphical relative to device",
	    getYHandle);
  getMethod(class, NAME_position, NAME_location, "point", 2,
	    "relative_to=graphical", "coordinate_system_of=[device]",
	    "New point with position on graphical relative to device",
	    getPositionHandle);

  succeed;
}

