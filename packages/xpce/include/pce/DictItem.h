/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_DICTITEM_H
#define _PCE_DICTITEM_H

PceExternalClass(ClassDictItem);
class PceDictItem :public PceObject
{
public:
  PceDictItem() :
    PceObject(ClassDictItem)
  {
  }
  PceDictItem(PceArg key) :
    PceObject(ClassDictItem, key)
  {
  }
  PceDictItem(PceArg key, PceArg label) :
    PceObject(ClassDictItem, key, label)
  {
  }
  PceDictItem(PceArg key, PceArg label, PceArg object) :
    PceObject(ClassDictItem, key, label, object)
  {
  }
  PceDictItem(PceArg key, PceArg label, PceArg object, PceArg style) :
    PceObject(ClassDictItem, key, label, object, style)
  {
  }
};

inline PceDictItem
AsDictItem(PceArg a)
{ return *((PceDictItem*) &a);
}

#endif /*!_PCE_DICTITEM_H*/
