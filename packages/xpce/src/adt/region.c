/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/arith.h>

#define InitAreaA	int ax = valInt(a->x), ay = valInt(a->y), \
			    aw = valInt(a->w), ah = valInt(a->h)
#define InitAreaB	int bx = valInt(b->x), by = valInt(b->y), \
			    bw = valInt(b->w), bh = valInt(b->h)


static status
initialiseRegion(RegionObj r, Equation x, Equation y, Equation w, Equation h)
{ assign(r, x, x);
  assign(r, y, y);
  assign(r, w, w);
  assign(r, h, h);

  succeed;
}


#define XYWH	VarX, a->x, VarY, a->y, VarW, a->w, VarH, a->h

static Int
getAreaXRegion(RegionObj r, Area a)
{ answer(getValueExpression(r->x, XYWH, EAV));
}


static Int
getAreaYRegion(RegionObj r, Area a)
{ answer(getValueExpression(r->y, XYWH, EAV));
}

static Int
getAreaWRegion(RegionObj r, Area a)
{ answer(getValueExpression(r->w, XYWH, EAV));
}

static Int
getAreaHRegion(RegionObj r, Area a)
{ answer(getValueExpression(r->h, XYWH, EAV));
}


status
insideRegion(RegionObj r, Area a, Point p)
{ int x, y, w, h;
  int px = valInt(p->x);
  int py = valInt(p->y);

  x = valInt(getAreaXRegion(r, a));
  w = valInt(getAreaWRegion(r, a));

  if ((w >= 0 && (px < x || px > x+w)) || (w < 0 && (px < x+w || px > x)))
    fail;

  y = valInt(getAreaYRegion(r, a));
  h = valInt(getAreaHRegion(r, a));

  if ((h >= 0 && (py < y || py > y+h)) || (h < 0 && (py < y+h || py > y)))
    fail;
  
  succeed;
}


static Area
getAreaRegion(RegionObj r, Area a)
{ Int x=a->x, y=a->y, w=a->w, h=a->h;

  answer(answerObject(ClassArea, 
    getValueExpression(r->x, VarX, x, VarW, w, VarY, y, VarH, h, EAV), 
    getValueExpression(r->y, VarX, x, VarW, w, VarY, y, VarH, h, EAV), 
    getValueExpression(r->w, VarX, x, VarW, w, VarY, y, VarH, h, EAV), 
    getValueExpression(r->h, VarX, x, VarW, w, VarY, y, VarH, h, EAV), 
    EAV));
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_inside[] =
        { "area", "point" };
static char *T_initialise[] =
        { "x=expression", "y=expression", "width=expression", "height=expression" };

/* Instance Variables */

static vardecl var_region[] =
{ IV(NAME_x, "expression", IV_BOTH,
     NAME_dimension, "X of area expressed in XYWH of area"),
  IV(NAME_y, "expression", IV_BOTH,
     NAME_dimension, "Y of area expressed in XYWH of area"),
  IV(NAME_width, "expression", IV_BOTH,
     NAME_dimension, "W of area expressed in XYWH of area"),
  IV(NAME_height, "expression", IV_BOTH,
     NAME_dimension, "H of area expressed in XYWH of area")
};

/* Send Methods */

static senddecl send_region[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseRegion,
     DEFAULT, "Create region from XYWH-expressions"),
  SM(NAME_inside, 2, T_inside, insideRegion,
     NAME_compare, "Test if point is inside region of area")
};

/* Get Methods */

static getdecl get_region[] =
{ GM(NAME_area, 1, "area", "area", getAreaRegion,
     NAME_calculate, "New area describing region of argument"),
  GM(NAME_areaHeight, 1, "int", "area", getAreaHRegion,
     NAME_calculate, "H of region of argument"),
  GM(NAME_areaWidth, 1, "int", "area", getAreaWRegion,
     NAME_calculate, "W of region of argument"),
  GM(NAME_areaX, 1, "int", "area", getAreaXRegion,
     NAME_calculate, "X of region of argument"),
  GM(NAME_areaY, 1, "int", "area", getAreaYRegion,
     NAME_calculate, "Y of region of argument")
};

/* Resources */

#define rc_region NULL
/*
static classvardecl rc_region[] =
{ 
};
*/

/* Class Declaration */

static Name region_termnames[] = { NAME_x, NAME_y, NAME_width, NAME_height };

ClassDecl(region_decls,
          var_region, send_region, get_region, rc_region,
          4, region_termnames,
          "$Rev$");


status
makeClassRegion(Class class)
{ return declareClass(class, &region_decls);
}

