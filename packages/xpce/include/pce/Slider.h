/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_SLIDER_H
#define _PCE_SLIDER_H

PceExternalClass(ClassSlider);
class PceSlider :public PceObject
{
public:
  PceSlider(PceArg name, PceArg low, PceArg high, PceArg value) :
    PceObject(ClassSlider, name, low, high, value)
  {
  }
  PceSlider(PceArg name, PceArg low, PceArg high, PceArg value, PceArg message) :
    PceObject(ClassSlider, name, low, high, value, message)
  {
  }
};

inline PceSlider
AsSlider(PceArg a)
{ return *((PceSlider*) &a);
}

#endif /*!_PCE_SLIDER_H*/
