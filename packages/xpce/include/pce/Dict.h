/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_DICT_H
#define _PCE_DICT_H

extern Any ClassDict;
class PceDict :public PceObject
{
public:
  PceDict() :
    PceObject(ClassDict)
  {
  }
  PceDict(PceArg member) :
    PceObject(ClassDict, member)
  {
  }
  PceDict(PceArg member, PceArg member2) :
    PceObject(ClassDict, member, member2)
  {
  }
  PceDict(PceArg member, PceArg member2, PceArg member3) :
    PceObject(ClassDict, member, member2, member3)
  {
  }
  PceDict(PceArg member, PceArg member2, PceArg member3, PceArg member4) :
    PceObject(ClassDict, member, member2, member3, member4)
  {
  }
  PceDict(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5) :
    PceObject(ClassDict, member, member2, member3, member4, member5)
  {
  }
  PceDict(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5, PceArg member6) :
    PceObject(ClassDict, member, member2, member3, member4, member5, member6)
  {
  }
  PceDict(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5, PceArg member6, PceArg member7) :
    PceObject(ClassDict, member, member2, member3, member4, member5, member6, member7)
  {
  }
  PceDict(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5, PceArg member6, PceArg member7, PceArg member8) :
    PceObject(ClassDict, member, member2, member3, member4, member5, member6, member7, member8)
  {
  }
  PceDict(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5, PceArg member6, PceArg member7, PceArg member8, PceArg member9) :
    PceObject(ClassDict, member, member2, member3, member4, member5, member6, member7, member8, member9)
  {
  }
};

inline PceDict
AsDict(PceArg a)
{ return *((PceDict*) &a);
}

#endif /*!_PCE_DICT_H*/
