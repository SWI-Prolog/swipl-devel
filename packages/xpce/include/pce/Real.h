/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_REAL_H
#define _PCE_REAL_H

PceExternalClass(ClassReal);
class PceReal :public PceObject
{
public:
  PceReal() :
    PceObject(ClassReal)
  {
  }
  PceReal(PceArg value) :
    PceObject(ClassReal, value)
  {
  }
};

inline PceReal
AsReal(PceArg a)
{ return *((PceReal*) &a);
}

#endif /*!_PCE_REAL_H*/
