/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_WINDOWDECORATOR_H
#define _PCE_WINDOWDECORATOR_H

PceExternalClass(ClassWindowDecorator);
class PceWindowDecorator :public PceObject
{
public:
  PceWindowDecorator(PceArg window) :
    PceObject(ClassWindowDecorator, window)
  {
  }
  PceWindowDecorator(PceArg window, PceArg scrollbars) :
    PceObject(ClassWindowDecorator, window, scrollbars)
  {
  }
  PceWindowDecorator(PceArg window, PceArg scrollbars, PceArg label) :
    PceObject(ClassWindowDecorator, window, scrollbars, label)
  {
  }
};

inline PceWindowDecorator
AsWindowDecorator(PceArg a)
{ return *((PceWindowDecorator*) &a);
}

#endif /*!_PCE_WINDOWDECORATOR_H*/
