/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_POPUPGESTURE_H
#define _PCE_POPUPGESTURE_H

PceExternalClass(ClassPopupGesture);
class PcePopupGesture :public PceObject
{
public:
  PcePopupGesture() :
    PceObject(ClassPopupGesture)
  {
  }
  PcePopupGesture(PceArg popup) :
    PceObject(ClassPopupGesture, popup)
  {
  }
  PcePopupGesture(PceArg popup, PceArg button) :
    PceObject(ClassPopupGesture, popup, button)
  {
  }
  PcePopupGesture(PceArg popup, PceArg button, PceArg modifier) :
    PceObject(ClassPopupGesture, popup, button, modifier)
  {
  }
};

inline PcePopupGesture
AsPopupGesture(PceArg a)
{ return *((PcePopupGesture*) &a);
}

#endif /*!_PCE_POPUPGESTURE_H*/
