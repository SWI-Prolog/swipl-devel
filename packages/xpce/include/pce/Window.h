/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_WINDOW_H
#define _PCE_WINDOW_H

extern Any ClassWindow;
class PceWindow :public PceObject
{
public:
  PceWindow() :
    PceObject(ClassWindow)
  {
  }
  PceWindow(PceArg label) :
    PceObject(ClassWindow, label)
  {
  }
  PceWindow(PceArg label, PceArg size) :
    PceObject(ClassWindow, label, size)
  {
  }
  PceWindow(PceArg label, PceArg size, PceArg display) :
    PceObject(ClassWindow, label, size, display)
  {
  }
};

inline PceWindow
AsWindow(PceArg a)
{ return *((PceWindow*) &a);
}

#endif /*!_PCE_WINDOW_H*/
