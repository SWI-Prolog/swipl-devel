/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_MENUITEM_H
#define _PCE_MENUITEM_H

extern Any ClassMenuItem;
class PceMenuItem :public PceObject
{
public:
  PceMenuItem() :
    PceObject(ClassMenuItem)
  {
  }
  PceMenuItem(PceArg value) :
    PceObject(ClassMenuItem, value)
  {
  }
  PceMenuItem(PceArg value, PceArg message) :
    PceObject(ClassMenuItem, value, message)
  {
  }
  PceMenuItem(PceArg value, PceArg message, PceArg label) :
    PceObject(ClassMenuItem, value, message, label)
  {
  }
  PceMenuItem(PceArg value, PceArg message, PceArg label, PceArg end_group) :
    PceObject(ClassMenuItem, value, message, label, end_group)
  {
  }
  PceMenuItem(PceArg value, PceArg message, PceArg label, PceArg end_group, PceArg condition) :
    PceObject(ClassMenuItem, value, message, label, end_group, condition)
  {
  }
  PceMenuItem(PceArg value, PceArg message, PceArg label, PceArg end_group, PceArg condition, PceArg accelerator) :
    PceObject(ClassMenuItem, value, message, label, end_group, condition, accelerator)
  {
  }
};

inline PceMenuItem
AsMenuItem(PceArg a)
{ return *((PceMenuItem*) &a);
}

#endif /*!_PCE_MENUITEM_H*/
