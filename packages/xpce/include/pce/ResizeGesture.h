/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_RESIZEGESTURE_H
#define _PCE_RESIZEGESTURE_H

extern Any ClassResizeGesture;
class PceResizeGesture :public PceObject
{
public:
  PceResizeGesture() :
    PceObject(ClassResizeGesture)
  {
  }
  PceResizeGesture(PceArg button) :
    PceObject(ClassResizeGesture, button)
  {
  }
  PceResizeGesture(PceArg button, PceArg modifier) :
    PceObject(ClassResizeGesture, button, modifier)
  {
  }
};

inline PceResizeGesture
AsResizeGesture(PceArg a)
{ return *((PceResizeGesture*) &a);
}

#endif /*!_PCE_RESIZEGESTURE_H*/
