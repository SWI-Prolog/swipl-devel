/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_IF_H
#define _PCE_IF_H

PceExternalClass(ClassIf);
class PceIf :public PceObject
{
public:
  PceIf(PceArg condition) :
    PceObject(ClassIf, condition)
  {
  }
  PceIf(PceArg condition, PceArg thn) :
    PceObject(ClassIf, condition, thn)
  {
  }
  PceIf(PceArg condition, PceArg thn, PceArg els) :
    PceObject(ClassIf, condition, thn, els)
  {
  }
};

inline PceIf
AsIf(PceArg a)
{ return *((PceIf*) &a);
}

#endif /*!_PCE_IF_H*/
