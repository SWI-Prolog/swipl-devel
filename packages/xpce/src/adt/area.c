/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

/* FN will include the following names from this comment.  Donot remove
   it!

NAME_northWest 
NAME_southWest
NAME_northEast
NAME_southEast
*/

#define InitAreaA	int ax = valInt(a->x), ay = valInt(a->y), 	\
			    aw = valInt(a->w), ah = valInt(a->h)

#define InitAreaB	int bx = valInt(b->x), by = valInt(b->y), 	\
			    bw = valInt(b->w), bh = valInt(b->h)

#define ExitAreaA	assign(a, x, toInt(ax));			\
		        assign(a, y, toInt(ay));			\
		        assign(a, w, toInt(aw));			\
		        assign(a, h, toInt(ah));

static status normaliseArea(Area a);

static status
initialiseArea(Area a, Int x, Int y, Int w, Int h)
{ assign(a, x, isDefault(x) ? ZERO : x);
  assign(a, y, isDefault(y) ? ZERO : y);
  assign(a, w, isDefault(w) ? ZERO : w);
  assign(a, h, isDefault(h) ? ZERO : h);

  succeed;
}


static status
equalArea(Area a, Area b)
{ InitAreaA;
  InitAreaB;

  NormaliseArea(ax, ay, aw, ah);
  NormaliseArea(bx, by, bw, bh);

  if (ax == bx && ay == by && aw == bw && ah == bh)
    succeed;
  fail;
}


status
intersectionArea(Area a, Area b)
{ int x, y, w, h;
  Name orientation;

  InitAreaA;	
  InitAreaB;

  orientation = OrientationArea(aw, ah);

  NormaliseArea(ax, ay, aw, ah);
  NormaliseArea(bx, by, bw, bh);

  x = (ax > bx ? ax : bx);
  y = (ay > by ? ay : by);
  w = (ax + aw < bx + bw ? ax + aw : bx + bw) - x;
  h = (ay + ah < by + bh ? ay + ah : by + bh) - y;

  if (w < 0 || h < 0)
    fail;

  OrientateArea(x, y, w, h, orientation);

  assign(a, x, toInt(x));
  assign(a, y, toInt(y));
  assign(a, w, toInt(w));
  assign(a, h, toInt(h));

  succeed;
}


static Area
getIntersectionArea(Area a, Area b)
{ Area c;

  c = answerObject(ClassArea, a->x, a->y, a->w, a->h, 0);
  if ( intersectionArea(c, b) )
    answer(c);

  freeObject(c);
  fail;
}


static status
unionArea(Area a, Area b)
{ int x, y, w, h;
  Name orientation;

  if ( b->w == ZERO && b->h == ZERO )
    succeed;
  if ( a->w == ZERO && a->h == ZERO )
  { copyArea(a, b);
    return normaliseArea(a);
  }

  { InitAreaA;
    InitAreaB;

    orientation = OrientationArea(aw, ah);

    NormaliseArea(ax, ay, aw, ah);
    NormaliseArea(bx, by, bw, bh);

    x = (ax < bx ? ax : bx);
    y = (ay < by ? ay : by);
    w = (ax + aw > bx + bw ? ax + aw : bx + bw) - x;
    h = (ay + ah > by + bh ? ay + ah : by + bh) - y;

    OrientateArea(x, y, w, h, orientation);

    assign(a, x, toInt(x));
    assign(a, y, toInt(y));
    assign(a, w, toInt(w));
    assign(a, h, toInt(h));
  }

  succeed;
}


status
unionNormalisedArea(Area a, Area b)
{ int x, y, w, h;

  if ( b->w == ZERO && b->h == ZERO )
    succeed;
  if ( a->w == ZERO && a->h == ZERO )
  { copyArea(a, b);
    return normaliseArea(a);
  }

  { InitAreaA;
    InitAreaB;

    NormaliseArea(bx, by, bw, bh);

    x = (ax < bx ? ax : bx);
    y = (ay < by ? ay : by);
    w = (ax + aw > bx + bw ? ax + aw : bx + bw) - x;
    h = (ay + ah > by + bh ? ay + ah : by + bh) - y;

    assign(a, x, toInt(x));
    assign(a, y, toInt(y));
    assign(a, w, toInt(w));
    assign(a, h, toInt(h));
  }

  succeed;
}


static Area
getUnionArea(Area a, Area b)
{ Area c;

  c = answerObject(ClassArea, a->x, a->y, a->w, a->h, 0);
  unionArea(c, b);
  answer(c);
}


status
sizeArea(Area a, Size s)
{ assign(a, w, s->w);
  assign(a, h, s->h);

  succeed;
}


Size
getSizeArea(Area a)
{ answer(answerObject(ClassSize, a->w, a->h, 0));
}


static Int
getMeasureArea(Area a)
{ int w = valInt(a->w);
  int h = valInt(a->h);

  answer(toInt(abs(w) * abs(h)));
}


static status
pointInArea(Area a, Point p)
{ return inArea(a, p->x, p->y);
}


status
overlapArea(Area a, Area b)
{ InitAreaA;
  InitAreaB;

  NormaliseArea(ax, ay, aw, ah);
  NormaliseArea(bx, by, bw, bh);

  if (by > ay+ah || by+bh < ay || bx > ax+aw || bx+bw < ax)
    fail;
  succeed;
}


status
relativeMoveArea(Area a, Point p)
{ assign(a,x, add(a->x, p->x));
  assign(a,y, add(a->y, p->y));

  succeed;
}


status
relativeMoveBackArea(Area a, Point p)
{ assign(a,x, sub(a->x, p->x));
  assign(a,y, sub(a->y, p->y));

  succeed;
}


static status
positionArea(Area a, Point p)
{ assign(a, x, p->x);
  assign(a, y, p->y);

  succeed;
}


Point
getPositionArea(Area a)
{ answer(answerObject(ClassPoint, a->x, a->y, 0));
}


status
inArea(Area a, Int x, Int y)
{ InitAreaA;

  NormaliseArea(ax, ay, aw, ah);

  if (valInt(x) >= ax && valInt(x) <= ax+aw
   && valInt(y) >= ay && valInt(y) <= ay+ah)
    succeed;
  fail;
}


status
copyArea(Area a, Area b)
{ assign(a, x, b->x);
  assign(a, y, b->y);
  assign(a, w, b->w);
  assign(a, h, b->h);

  succeed;
}


status
insideArea(Area a, Area b)
{ InitAreaA;
  InitAreaB;

  NormaliseArea(ax, ay, aw, ah);
  NormaliseArea(bx, by, bw, bh);

  if (bx < ax)		fail;
  if (bx+bw > ax+aw-1)	fail;
  if (by < ay)		fail;
  if (by+bh > ay+ah-1)	fail;

  succeed;
}


Int
getDistanceXArea(Area a, Area b)
{ int ax = valInt(a->x), aw = valInt(a->w);
  int bx = valInt(b->x), bw = valInt(b->w);

  if ( aw < 0 ) ax += aw, aw = -aw;
  if ( bw < 0 ) bx += bw, bw = -bw;

  if ( ax + aw < bx ) answer(toInt(bx-(ax+aw)));
  if ( bx + bw < ax ) answer(toInt(ax-(bx+bw)));

  answer(ZERO);
}


Int
getDistanceYArea(Area a, Area b)
{ int ay = valInt(a->y), ah = valInt(a->h);
  int by = valInt(b->y), bh = valInt(b->h);

  if ( ah < 0 ) ay += ah, ah = -ah;
  if ( bh < 0 ) by += bh, bh = -bh;

  if ( ay + ah < by ) answer(toInt(by-(ay+ah)));
  if ( by + bh < ay ) answer(toInt(ay-(by+bh)));

  answer(ZERO);
}


Int
getDistanceArea(Area a, Area b)
{ InitAreaA;
  InitAreaB;

  NormaliseArea(ax, ay, aw, ah);
  NormaliseArea(bx, by, bw, bh);

  if (overlapArea(a, b) == SUCCEED)
    answer(ZERO);

  if (ay+ah < by)
  { if (bx+bw < ax)
      answer(toInt(distance(bx+bw, by, ax, ay+ah)));
    if (bx > ax+aw)
      answer(toInt(distance(ax+aw, ay+ah, bx, by)));
    answer(toInt(by-(ay+ah)));
  }

  if (by+bh < ay)
  { if (ax+aw < bx)
      answer(toInt(distance(ax+aw, ay, bx, by+bh)));
    if (bx+bw < ax)
      answer(toInt(distance(bx+bw, by+bh, ax, ay)));
    answer(toInt(ay-(by+bh)));
  }

  if (ax+aw < bx)
    answer(toInt(bx-(ax+aw)));

  answer(toInt(ax-(bx+bw)));
}


status
clearArea(Area a)
{ assign(a, x, ZERO);
  assign(a, y, ZERO);
  assign(a, w, ZERO);
  assign(a, h, ZERO);

  succeed;
}


static status
centerArea(Area a, Point pos)
{ assign(a, x, dif(pos->x,a->w));
  assign(a, y, dif(pos->y,a->h));

  succeed;
}


static Int
getLeftSideArea(Area a)
{ if ( valInt(a->w) >= 0 )
    answer(a->x);
  else
    answer(add(a->x, a->w));
}


static Int
getRightSideArea(Area a)
{ if ( valInt(a->w) >= 0 )
    answer(add(a->x, a->w));
  else
    answer(a->x);
}


static Int
getTopSideArea(Area a)
{ if ( valInt(a->h) >= 0 )
    answer(a->y);
  else
    answer(add(a->y, a->h));
}


static Int
getBottomSideArea(Area a)
{ if ( valInt(a->h) >= 0 )
    answer(add(a->y, a->h));
  else
    answer(a->y);
}


static Point
getCenterArea(Area a)
{ answer(answerObject(ClassPoint, mid(a->x,a->w), mid(a->y,a->h), 0));
}


static status
cornerArea(Area a, Point pos)
{ int w, h;

  w = valInt(pos->x) - valInt(a->x);
  w += (w>=0 ? 1 : -1);
  h = valInt(pos->y) - valInt(a->y);
  h += (h>=0 ? 1 : -1);

  assign(a, w, toInt(w));
  assign(a, h, toInt(h));

  succeed;
}


static Point
getCornerArea(Area a)
{ answer(answerObject(ClassPoint, add(a->x,a->w), add(a->y,a->h), 0));
}


status
setArea(Area a, Int x, Int y, Int w, Int h)
{ if (notDefault(x)) assign(a, x, x);
  if (notDefault(y)) assign(a, y, y);
  if (notDefault(w)) assign(a, w, w);
  if (notDefault(h)) assign(a, h, h);

  succeed;
}


status
increaseArea(Area a, Int i)
{ int d = valInt(i);
  InitAreaA;

  if (aw >= 0)
    aw += 2*d, ax -= d;
  else
    aw -= 2*d, ax += d;

  if (ah >= 0)
    ah += 2*d, ay -= d;
  else
    ah -= 2*d, ay += d;

  ExitAreaA;

  succeed;
}


static status
decreaseArea(Area a, Int i)
{ return increaseArea(a, toInt(-valInt(i)));
}


static status
normaliseArea(Area a)
{ InitAreaA;
  NormaliseArea(ax, ay, aw, ah);
  ExitAreaA;

  succeed;
}


static Area
getNormalisedArea(Area a)
{ InitAreaA;
  NormaliseArea(ax, ay, aw, ah);

  answer(answerObject(ClassArea,
		      toInt(ax), toInt(ay), toInt(aw), toInt(ah), 0));
}


status
orientationArea(Area a, Name orientation)
{ InitAreaA;
  OrientateArea(ax, ay, aw, ah, orientation);
  ExitAreaA;
	  
  succeed;
}


Name
getOrientationArea(Area a)
{ Name orientation;
  int aw = valInt(a->w);
  int ah = valInt(a->h);

  orientation = OrientationArea(aw, ah);

  answer(orientation);
}


#define NEAR(a, b, d, m, p)	{ if (abs(a-b) <= d) m |= p; }
#define SAME(a, b, m, p)	{ if (a == b) m |= p; }
#define LESS(a, b, m, p)	{ if (a < b) m |= p; }

static Int
getNearSidesArea(Area a, Area b, Int distance)
{ int d=valInt(distance);
  int a_top, a_center, a_bottom, a_left, a_middle, a_right;
  int b_top, b_center, b_bottom, b_left, b_middle, b_right;
  register unsigned long mask;

  InitAreaA;
  InitAreaB;

  NormaliseArea(ax, ay, aw, ah);
  NormaliseArea(bx, by, bw, bh);

  a_top = ay;
  a_bottom = ay+ah-1;
  a_center = (a_top+a_bottom+1)/2;

  a_left = ax;
  a_right = ax+aw-1;
  a_middle = (a_left+a_right+1)/2;

  b_top = by;
  b_bottom = by+bh-1;
  b_center = (b_top+b_bottom+1)/2;

  b_left = bx;
  b_right = bx+bw-1;
  b_middle = (b_left+b_right+1)/2;

  mask = 0;

  NEAR(a_top,    b_top,    d, mask, 01);
  NEAR(a_top,    b_center, d, mask, 02);
  NEAR(a_top,    b_bottom, d, mask, 04);
  NEAR(a_center, b_top,    d, mask, 010);
  NEAR(a_center, b_center, d, mask, 020);
  NEAR(a_center, b_bottom, d, mask, 040);
  NEAR(a_bottom, b_top,    d, mask, 0100);
  NEAR(a_bottom, b_center, d, mask, 0200);
  NEAR(a_bottom, b_bottom, d, mask, 0400);

  NEAR(a_left,   b_left,   d, mask, 01000);
  NEAR(a_left,   b_middle, d, mask, 02000);
  NEAR(a_left,   b_right,  d, mask, 04000);
  NEAR(a_middle, b_left,   d, mask, 010000);
  NEAR(a_middle, b_middle, d, mask, 020000);
  NEAR(a_middle, b_right,  d, mask, 040000);
  NEAR(a_right,  b_left,   d, mask, 0100000);
  NEAR(a_right,  b_middle, d, mask, 0200000);
  NEAR(a_right,  b_right,  d, mask, 0400000);

  answer(toInt(mask));
}


static Int
sameSidesArea(Area a, Area b)
{ int a_top, a_center, a_bottom, a_left, a_middle, a_right;
  int b_top, b_center, b_bottom, b_left, b_middle, b_right;
  register unsigned long mask;

  InitAreaA;
  InitAreaB;

  NormaliseArea(ax, ay, aw, ah);
  NormaliseArea(bx, by, bw, bh);

  a_top = ay;
  a_bottom = ay+ah-1;
  a_center = (a_top+a_bottom+1)/2;

  a_left = ax;
  a_right = ax+aw-1;
  a_middle = (a_left+a_right+1)/2;

  b_top = by;
  b_bottom = by+bh-1;
  b_center = (b_top+b_bottom+1)/2;

  b_left = bx;
  b_right = bx+bw-1;
  b_middle = (b_left+b_right+1)/2;

  mask = 0;

  SAME(a_top,    b_top,    mask, 01);
  SAME(a_top,    b_center, mask, 02);
  SAME(a_top,    b_bottom, mask, 04);
  SAME(a_center, b_top,    mask, 010);
  SAME(a_center, b_center, mask, 020);
  SAME(a_center, b_bottom, mask, 040);
  SAME(a_bottom, b_top,    mask, 0100);
  SAME(a_bottom, b_center, mask, 0200);
  SAME(a_bottom, b_bottom, mask, 0400);

  SAME(a_left,   b_left,   mask, 01000);
  SAME(a_left,   b_middle, mask, 02000);
  SAME(a_left,   b_right,  mask, 04000);
  SAME(a_middle, b_left,   mask, 010000);
  SAME(a_middle, b_middle, mask, 020000);
  SAME(a_middle, b_right,  mask, 040000);
  SAME(a_right,  b_left,   mask, 0100000);
  SAME(a_right,  b_middle, mask, 0200000);
  SAME(a_right,  b_right,  mask, 0400000);

  answer(toInt(mask));
}


static Int
getLessSidesArea(Area a, Area b)
{ int a_top, a_center, a_bottom, a_left, a_middle, a_right;
  int b_top, b_center, b_bottom, b_left, b_middle, b_right;
  register unsigned long mask;

  InitAreaA;
  InitAreaB;

  NormaliseArea(ax, ay, aw, ah);
  NormaliseArea(bx, by, bw, bh);

  a_top = ay;
  a_bottom = ay+ah-1;
  a_center = (a_top+a_bottom+1)/2;

  a_left = ax;
  a_right = ax+aw-1;
  a_middle = (a_left+a_right+1)/2;

  b_top = by;
  b_bottom = by+bh-1;
  b_center = (b_top+b_bottom+1)/2;

  b_left = bx;
  b_right = bx+bw-1;
  b_middle = (b_left+b_right+1)/2;

  mask = 0;

  LESS(a_top,    b_top,    mask, 01);
  LESS(a_top,    b_center, mask, 02);
  LESS(a_top,    b_bottom, mask, 04);
  LESS(a_center, b_top,    mask, 010);
  LESS(a_center, b_center, mask, 020);
  LESS(a_center, b_bottom, mask, 040);
  LESS(a_bottom, b_top,    mask, 0100);
  LESS(a_bottom, b_center, mask, 0200);
  LESS(a_bottom, b_bottom, mask, 0400);

  LESS(a_left,   b_left,   mask, 01000);
  LESS(a_left,   b_middle, mask, 02000);
  LESS(a_left,   b_right,  mask, 04000);
  LESS(a_middle, b_left,   mask, 010000);
  LESS(a_middle, b_middle, mask, 020000);
  LESS(a_middle, b_right,  mask, 040000);
  LESS(a_right,  b_left,   mask, 0100000);
  LESS(a_right,  b_middle, mask, 0200000);
  LESS(a_right,  b_right,  mask, 0400000);

  answer(toInt(mask));
}


status
makeClassArea(Class class)
{ sourceClass(class, makeClassArea, __FILE__, "$Revision$");

  localClass(class, NAME_x, NAME_position, "int", NAME_both,
	     "Origin's X-value");
  localClass(class, NAME_y, NAME_position, "int", NAME_both,
	     "Origin's Y-value");
  localClass(class, NAME_width, NAME_dimension, "int", NAME_both,
	     "Width in pixels (may be negative)");
  localClass(class, NAME_height, NAME_dimension, "int", NAME_both,
	     "Height in pixels (may be negative)");

  termClass(class, "area", 4, NAME_x, NAME_y, NAME_width, NAME_height);

  sendMethod(class, NAME_initialise, DEFAULT,
	     4, "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Create area from X, Y, W and H",
	     initialiseArea);
  sendMethod(class, NAME_center, NAME_position, 1, "point",
	     "Move to make point the center",
	     centerArea);
  sendMethod(class, NAME_clear, NAME_resize, 0,
	     "Set X, Y, W and H to 0",
	     clearArea);
  sendMethod(class, NAME_copy, NAME_copy, 1, "area",
	     "Copy X, Y, W and H from argument area",
	     copyArea);
  sendMethod(class, NAME_corner, NAME_resize, 1, "point",
	     "Resize to make opposite of origin point",
	     cornerArea);
  sendMethod(class, NAME_decrease, NAME_resize, 1, "int",
	     "Move all sides inwards",
	     decreaseArea);
  sendMethod(class, NAME_equal, NAME_equality, 1, "area",
	     "Test if area is equal to the argument",
	     equalArea);
  sendMethod(class, NAME_in, NAME_relation, 1, "point",
	     "Test if point is in area",
	     pointInArea);
  sendMethod(class, NAME_increase, NAME_resize, 1, "int",
	     "Move all sides outwards",
	     increaseArea);
  sendMethod(class, NAME_inside, NAME_relation, 1, "area",
	     "Test if argument is entirely in area",
	     insideArea);
  sendMethod(class, NAME_intersection, NAME_relation, 1, "area",
	     "Make area the intersection with argument",
	     intersectionArea);
  sendMethod(class, NAME_normalise, NAME_orientation, 0,
	     "Make top-left corner the origin",
	     normaliseArea);
  sendMethod(class, NAME_orientation, NAME_orientation,
	     1, "{north_west,south_east,north_east,south_east}",
	     "Put origin at indicated corner",
	     orientationArea);
  sendMethod(class, NAME_overlap, NAME_relation, 1, "area",
	     "Test whether area overlaps argument",
	     overlapArea);
  sendMethod(class, NAME_position, NAME_position, 1, "point",
	     "Move origin to point",
	     positionArea);
  sendMethod(class, NAME_relativeMove, NAME_position, 1, "point",
	     "Move origin relative by point",
	     relativeMoveArea);
  sendMethod(class, NAME_set, NAME_resize, 4,
	     "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	     "Set X, Y, W and H values",
	     setArea);
  sendMethod(class, NAME_size, NAME_resize, 1, "size",
	     "Resize area to size",
	     sizeArea);
  sendMethod(class, NAME_union, NAME_resize, 1, "area",
	     "Enlarge area to entail argument",
	     unionArea);

  getMethod(class, NAME_leftSide, NAME_side, "int", 0,
	    "Left-side of area",
	    getLeftSideArea);
  getMethod(class, NAME_rightSide, NAME_side, "int", 0,
	    "Right-side of area",
	    getRightSideArea);
  getMethod(class, NAME_topSide, NAME_side, "int", 0,
	    "Top-side of area",
	    getTopSideArea);
  getMethod(class, NAME_bottomSide, NAME_side, "int", 0,
	    "Bottom-side of area",
	    getBottomSideArea);
  getMethod(class, NAME_center, NAME_position, "point", 0,
	    "New point from center position",
	    getCenterArea);
  getMethod(class, NAME_corner, NAME_position, "point", 0,
	    "New point from point opposite origin",
	    getCornerArea);
  getMethod(class, NAME_distance, NAME_relation, "int", 1, "area",
	    "Closest distance between areas",
	    getDistanceArea);
  getMethod(class, NAME_distanceX, NAME_relation, "int", 1, "area",
	    "Distance between area's in X-direction",
	    getDistanceXArea);
  getMethod(class, NAME_distanceY, NAME_relation, "int", 1, "area",
	    "Distance between area's in Y-direction",
	    getDistanceYArea);
  getMethod(class, NAME_intersection, NAME_relation, "area", 1, "area",
	    "New area from intersection",
	    getIntersectionArea);
  getMethod(class, NAME_lessSides, NAME_relation, "int", 1, "area",
	    "Bitmask (int) of sides closer to origin",
	    getLessSidesArea);
  getMethod(class, NAME_nearSides, NAME_relation, "int", 2, "area", "int",
	    "Bitmask (int) of almost equal sides",
	    getNearSidesArea);
  getMethod(class, NAME_normalised, NAME_copy, "area", 0,
	    "New area with origin at top-left",
	    getNormalisedArea);
  getMethod(class, NAME_orientation, NAME_orientation,
	    "{north_west,south_east,north_east,south_east}",
	    0,
	    "Current orientation",
	    getOrientationArea);
  getMethod(class, NAME_position, NAME_position, "point", 0,
	    "New point from origin",
	    getPositionArea);
  getMethod(class, NAME_sameSides, NAME_relation, "int", 1, "area",
	    "Bitmask (int) of equal sides",
	    sameSidesArea);
  getMethod(class, NAME_size, NAME_dimension, "size", 0,
	    "New size from size of area",
	    getSizeArea);
  getMethod(class, NAME_union, NAME_resize, "area", 1, "area",
	    "New area from union",
	    getUnionArea);
  getMethod(class, NAME_measure, NAME_dimension, "int", 0,
	    "`Area' of the area",
	    getMeasureArea);
  
  succeed;
}
