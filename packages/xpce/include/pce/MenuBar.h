/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_MENUBAR_H
#define _PCE_MENUBAR_H

extern Any ClassMenuBar;
class PceMenuBar :public PceObject
{
public:
  PceMenuBar() :
    PceObject(ClassMenuBar)
  {
  }
  PceMenuBar(PceArg name) :
    PceObject(ClassMenuBar, name)
  {
  }
};

inline PceMenuBar
AsMenuBar(PceArg a)
{ return *((PceMenuBar*) &a);
}

#endif /*!_PCE_MENUBAR_H*/
