/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_CIRCLE_H
#define _PCE_CIRCLE_H

extern Any ClassCircle;
class PceCircle :public PceObject
{
public:
  PceCircle() :
    PceObject(ClassCircle)
  {
  }
  PceCircle(PceArg diameter) :
    PceObject(ClassCircle, diameter)
  {
  }
};

inline PceCircle
AsCircle(PceArg a)
{ return *((PceCircle*) &a);
}

#endif /*!_PCE_CIRCLE_H*/
