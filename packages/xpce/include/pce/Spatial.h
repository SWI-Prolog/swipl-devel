/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_SPATIAL_H
#define _PCE_SPATIAL_H

extern Any ClassSpatial;
class PceSpatial :public PceObject
{
public:
  PceSpatial() :
    PceObject(ClassSpatial)
  {
  }
  PceSpatial(PceArg x1) :
    PceObject(ClassSpatial, x1)
  {
  }
  PceSpatial(PceArg x1, PceArg y1) :
    PceObject(ClassSpatial, x1, y1)
  {
  }
  PceSpatial(PceArg x1, PceArg y1, PceArg x2) :
    PceObject(ClassSpatial, x1, y1, x2)
  {
  }
  PceSpatial(PceArg x1, PceArg y1, PceArg x2, PceArg y2) :
    PceObject(ClassSpatial, x1, y1, x2, y2)
  {
  }
  PceSpatial(PceArg x1, PceArg y1, PceArg x2, PceArg y2, PceArg width) :
    PceObject(ClassSpatial, x1, y1, x2, y2, width)
  {
  }
  PceSpatial(PceArg x1, PceArg y1, PceArg x2, PceArg y2, PceArg width, PceArg height) :
    PceObject(ClassSpatial, x1, y1, x2, y2, width, height)
  {
  }
};

inline PceSpatial
AsSpatial(PceArg a)
{ return *((PceSpatial*) &a);
}

#endif /*!_PCE_SPATIAL_H*/
