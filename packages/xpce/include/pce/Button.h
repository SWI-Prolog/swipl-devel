/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_BUTTON_H
#define _PCE_BUTTON_H

PceExternalClass(ClassButton);
class PceButton :public PceObject
{
public:
  PceButton(PceArg name) :
    PceObject(ClassButton, name)
  {
  }
  PceButton(PceArg name, PceArg message) :
    PceObject(ClassButton, name, message)
  {
  }
  PceButton(PceArg name, PceArg message, PceArg label) :
    PceObject(ClassButton, name, message, label)
  {
  }
};

inline PceButton
AsButton(PceArg a)
{ return *((PceButton*) &a);
}

#endif /*!_PCE_BUTTON_H*/
