/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_MENU_H
#define _PCE_MENU_H

PceExternalClass(ClassMenu);
class PceMenu :public PceObject
{
public:
  PceMenu() :
    PceObject(ClassMenu)
  {
  }
  PceMenu(PceArg name) :
    PceObject(ClassMenu, name)
  {
  }
  PceMenu(PceArg name, PceArg kind) :
    PceObject(ClassMenu, name, kind)
  {
  }
  PceMenu(PceArg name, PceArg kind, PceArg message) :
    PceObject(ClassMenu, name, kind, message)
  {
  }
};

inline PceMenu
AsMenu(PceArg a)
{ return *((PceMenu*) &a);
}

#endif /*!_PCE_MENU_H*/
