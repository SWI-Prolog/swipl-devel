/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_VIEW_H
#define _PCE_VIEW_H

PceExternalClass(ClassView);
class PceView :public PceObject
{
public:
  PceView() :
    PceObject(ClassView)
  {
  }
  PceView(PceArg label) :
    PceObject(ClassView, label)
  {
  }
  PceView(PceArg label, PceArg size) :
    PceObject(ClassView, label, size)
  {
  }
  PceView(PceArg label, PceArg size, PceArg display) :
    PceObject(ClassView, label, size, display)
  {
  }
  PceView(PceArg label, PceArg size, PceArg display, PceArg editor) :
    PceObject(ClassView, label, size, display, editor)
  {
  }
};

inline PceView
AsView(PceArg a)
{ return *((PceView*) &a);
}

#endif /*!_PCE_VIEW_H*/
