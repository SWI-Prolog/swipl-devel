/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_REGION_H
#define _PCE_REGION_H

extern Any ClassRegion;
class PceRegion :public PceObject
{
public:
  PceRegion(PceArg x, PceArg y, PceArg width, PceArg height) :
    PceObject(ClassRegion, x, y, width, height)
  {
  }
};

inline PceRegion
AsRegion(PceArg a)
{ return *((PceRegion*) &a);
}

#endif /*!_PCE_REGION_H*/
