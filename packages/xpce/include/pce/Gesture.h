/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_GESTURE_H
#define _PCE_GESTURE_H

extern Any ClassGesture;
class PceGesture :public PceObject
{
public:
  PceGesture() :
    PceObject(ClassGesture)
  {
  }
  PceGesture(PceArg button) :
    PceObject(ClassGesture, button)
  {
  }
  PceGesture(PceArg button, PceArg modifier) :
    PceObject(ClassGesture, button, modifier)
  {
  }
};

inline PceGesture
AsGesture(PceArg a)
{ return *((PceGesture*) &a);
}

#endif /*!_PCE_GESTURE_H*/
