/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_RESIZEOUTLINEGESTURE_H
#define _PCE_RESIZEOUTLINEGESTURE_H

extern Any ClassResizeOutlineGesture;
class PceResizeOutlineGesture :public PceObject
{
public:
  PceResizeOutlineGesture() :
    PceObject(ClassResizeOutlineGesture)
  {
  }
  PceResizeOutlineGesture(PceArg button) :
    PceObject(ClassResizeOutlineGesture, button)
  {
  }
  PceResizeOutlineGesture(PceArg button, PceArg modifier) :
    PceObject(ClassResizeOutlineGesture, button, modifier)
  {
  }
};

inline PceResizeOutlineGesture
AsResizeOutlineGesture(PceArg a)
{ return *((PceResizeOutlineGesture*) &a);
}

#endif /*!_PCE_RESIZEOUTLINEGESTURE_H*/
