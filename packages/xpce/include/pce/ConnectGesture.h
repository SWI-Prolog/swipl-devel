/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_CONNECTGESTURE_H
#define _PCE_CONNECTGESTURE_H

PceExternalClass(ClassConnectGesture);
class PceConnectGesture :public PceObject
{
public:
  PceConnectGesture() :
    PceObject(ClassConnectGesture)
  {
  }
  PceConnectGesture(PceArg button) :
    PceObject(ClassConnectGesture, button)
  {
  }
  PceConnectGesture(PceArg button, PceArg modifier) :
    PceObject(ClassConnectGesture, button, modifier)
  {
  }
  PceConnectGesture(PceArg button, PceArg modifier, PceArg link) :
    PceObject(ClassConnectGesture, button, modifier, link)
  {
  }
};

inline PceConnectGesture
AsConnectGesture(PceArg a)
{ return *((PceConnectGesture*) &a);
}

#endif /*!_PCE_CONNECTGESTURE_H*/
