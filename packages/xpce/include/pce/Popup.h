/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_POPUP_H
#define _PCE_POPUP_H

extern Any ClassPopup;
class PcePopup :public PceObject
{
public:
  PcePopup() :
    PceObject(ClassPopup)
  {
  }
  PcePopup(PceArg name) :
    PceObject(ClassPopup, name)
  {
  }
  PcePopup(PceArg name, PceArg message) :
    PceObject(ClassPopup, name, message)
  {
  }
};

inline PcePopup
AsPopup(PceArg a)
{ return *((PcePopup*) &a);
}

#endif /*!_PCE_POPUP_H*/
