/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_GRAPHICAL_H
#define _PCE_GRAPHICAL_H

extern Any ClassGraphical;
class PceGraphical :public PceObject
{
public:
  PceGraphical() :
    PceObject(ClassGraphical)
  {
  }
  PceGraphical(PceArg x) :
    PceObject(ClassGraphical, x)
  {
  }
  PceGraphical(PceArg x, PceArg y) :
    PceObject(ClassGraphical, x, y)
  {
  }
  PceGraphical(PceArg x, PceArg y, PceArg width) :
    PceObject(ClassGraphical, x, y, width)
  {
  }
  PceGraphical(PceArg x, PceArg y, PceArg width, PceArg height) :
    PceObject(ClassGraphical, x, y, width, height)
  {
  }
};

inline PceGraphical
AsGraphical(PceArg a)
{ return *((PceGraphical*) &a);
}

#endif /*!_PCE_GRAPHICAL_H*/
