/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_AREA_H
#define _PCE_AREA_H

extern Any ClassArea;
class PceArea :public PceObject
{
public:
  PceArea() :
    PceObject(ClassArea)
  {
  }
  PceArea(PceArg x) :
    PceObject(ClassArea, x)
  {
  }
  PceArea(PceArg x, PceArg y) :
    PceObject(ClassArea, x, y)
  {
  }
  PceArea(PceArg x, PceArg y, PceArg width) :
    PceObject(ClassArea, x, y, width)
  {
  }
  PceArea(PceArg x, PceArg y, PceArg width, PceArg height) :
    PceObject(ClassArea, x, y, width, height)
  {
  }
};

inline PceArea
AsArea(PceArg a)
{ return *((PceArea*) &a);
}

#endif /*!_PCE_AREA_H*/
