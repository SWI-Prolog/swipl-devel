/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_SCROLLBAR_H
#define _PCE_SCROLLBAR_H

PceExternalClass(ClassScrollBar);
class PceScrollBar :public PceObject
{
public:
  PceScrollBar() :
    PceObject(ClassScrollBar)
  {
  }
  PceScrollBar(PceArg object) :
    PceObject(ClassScrollBar, object)
  {
  }
  PceScrollBar(PceArg object, PceArg orientation) :
    PceObject(ClassScrollBar, object, orientation)
  {
  }
  PceScrollBar(PceArg object, PceArg orientation, PceArg message) :
    PceObject(ClassScrollBar, object, orientation, message)
  {
  }
};

inline PceScrollBar
AsScrollBar(PceArg a)
{ return *((PceScrollBar*) &a);
}

#endif /*!_PCE_SCROLLBAR_H*/
