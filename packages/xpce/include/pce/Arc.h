/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_ARC_H
#define _PCE_ARC_H

extern Any ClassArc;
class PceArc :public PceObject
{
public:
  PceArc() :
    PceObject(ClassArc)
  {
  }
  PceArc(PceArg radius) :
    PceObject(ClassArc, radius)
  {
  }
  PceArc(PceArg radius, PceArg start) :
    PceObject(ClassArc, radius, start)
  {
  }
  PceArc(PceArg radius, PceArg start, PceArg size) :
    PceObject(ClassArc, radius, start, size)
  {
  }
};

inline PceArc
AsArc(PceArg a)
{ return *((PceArc*) &a);
}

#endif /*!_PCE_ARC_H*/
