/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_PEN_H
#define _PCE_PEN_H

PceExternalClass(ClassPen);
class PcePen :public PceObject
{
public:
  PcePen() :
    PceObject(ClassPen)
  {
  }
  PcePen(PceArg thickness) :
    PceObject(ClassPen, thickness)
  {
  }
  PcePen(PceArg thickness, PceArg a) :
    PceObject(ClassPen, thickness, a)
  {
  }
  PcePen(PceArg thickness, PceArg a, PceArg colour) :
    PceObject(ClassPen, thickness, a, colour)
  {
  }
};

inline PcePen
AsPen(PceArg a)
{ return *((PcePen*) &a);
}

#endif /*!_PCE_PEN_H*/
