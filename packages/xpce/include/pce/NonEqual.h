/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_NONEQUAL_H
#define _PCE_NONEQUAL_H

extern Any ClassNonEqual;
class PceNonEqual :public PceObject
{
public:
  PceNonEqual() :
    PceObject(ClassNonEqual)
  {
  }
  PceNonEqual(PceArg left) :
    PceObject(ClassNonEqual, left)
  {
  }
  PceNonEqual(PceArg left, PceArg right) :
    PceObject(ClassNonEqual, left, right)
  {
  }
};

inline PceNonEqual
AsNonEqual(PceArg a)
{ return *((PceNonEqual*) &a);
}

#endif /*!_PCE_NONEQUAL_H*/
