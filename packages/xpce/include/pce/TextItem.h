/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_TEXTITEM_H
#define _PCE_TEXTITEM_H

extern Any ClassTextItem;
class PceTextItem :public PceObject
{
public:
  PceTextItem() :
    PceObject(ClassTextItem)
  {
  }
  PceTextItem(PceArg name) :
    PceObject(ClassTextItem, name)
  {
  }
  PceTextItem(PceArg name, PceArg def) :
    PceObject(ClassTextItem, name, def)
  {
  }
  PceTextItem(PceArg name, PceArg def, PceArg message) :
    PceObject(ClassTextItem, name, def, message)
  {
  }
};

inline PceTextItem
AsTextItem(PceArg a)
{ return *((PceTextItem*) &a);
}

#endif /*!_PCE_TEXTITEM_H*/
