/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_POINT_H
#define _PCE_POINT_H

extern Any ClassPoint;
class PcePoint :public PceObject
{
public:
  PcePoint() :
    PceObject(ClassPoint)
  {
  }
  PcePoint(PceArg x) :
    PceObject(ClassPoint, x)
  {
  }
  PcePoint(PceArg x, PceArg y) :
    PceObject(ClassPoint, x, y)
  {
  }
};

inline PcePoint
AsPoint(PceArg a)
{ return *((PcePoint*) &a);
}

#endif /*!_PCE_POINT_H*/
