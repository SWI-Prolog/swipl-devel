/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_HANDLE_H
#define _PCE_HANDLE_H

PceExternalClass(ClassHandle);
class PceHandle :public PceObject
{
public:
  PceHandle(PceArg x, PceArg y) :
    PceObject(ClassHandle, x, y)
  {
  }
  PceHandle(PceArg x, PceArg y, PceArg kind) :
    PceObject(ClassHandle, x, y, kind)
  {
  }
  PceHandle(PceArg x, PceArg y, PceArg kind, PceArg name) :
    PceObject(ClassHandle, x, y, kind, name)
  {
  }
};

inline PceHandle
AsHandle(PceArg a)
{ return *((PceHandle*) &a);
}

#endif /*!_PCE_HANDLE_H*/
