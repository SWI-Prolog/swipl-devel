/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_WHEN_H
#define _PCE_WHEN_H

PceExternalClass(ClassWhen);
class PceWhen :public PceObject
{
public:
  PceWhen(PceArg condition) :
    PceObject(ClassWhen, condition)
  {
  }
  PceWhen(PceArg condition, PceArg thn) :
    PceObject(ClassWhen, condition, thn)
  {
  }
  PceWhen(PceArg condition, PceArg thn, PceArg els) :
    PceObject(ClassWhen, condition, thn, els)
  {
  }
};

inline PceWhen
AsWhen(PceArg a)
{ return *((PceWhen*) &a);
}

#endif /*!_PCE_WHEN_H*/
