/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_DISPLAY_H
#define _PCE_DISPLAY_H

extern Any ClassDisplay;
class PceDisplay :public PceObject
{
public:
  PceDisplay() :
    PceObject(ClassDisplay)
  {
  }
  PceDisplay(PceArg address) :
    PceObject(ClassDisplay, address)
  {
  }
  PceDisplay(PceArg address, PceArg resource_class) :
    PceObject(ClassDisplay, address, resource_class)
  {
  }
};

inline PceDisplay
AsDisplay(PceArg a)
{ return *((PceDisplay*) &a);
}

#endif /*!_PCE_DISPLAY_H*/
