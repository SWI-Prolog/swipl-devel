/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_ARROW_H
#define _PCE_ARROW_H

PceExternalClass(ClassArrow);
class PceArrow :public PceObject
{
public:
  PceArrow() :
    PceObject(ClassArrow)
  {
  }
  PceArrow(PceArg length) :
    PceObject(ClassArrow, length)
  {
  }
  PceArrow(PceArg length, PceArg wing) :
    PceObject(ClassArrow, length, wing)
  {
  }
};

inline PceArrow
AsArrow(PceArg a)
{ return *((PceArrow*) &a);
}

#endif /*!_PCE_ARROW_H*/
