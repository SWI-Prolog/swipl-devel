/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_TAB_H
#define _PCE_TAB_H

PceExternalClass(ClassTab);
class PceTab :public PceObject
{
public:
  PceTab() :
    PceObject(ClassTab)
  {
  }
  PceTab(PceArg name) :
    PceObject(ClassTab, name)
  {
  }
};

inline PceTab
AsTab(PceArg a)
{ return *((PceTab*) &a);
}

#endif /*!_PCE_TAB_H*/
