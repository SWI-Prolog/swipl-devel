/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_HANDLERGROUP_H
#define _PCE_HANDLERGROUP_H

extern Any ClassHandlerGroup;
class PceHandlerGroup :public PceObject
{
public:
  PceHandlerGroup() :
    PceObject(ClassHandlerGroup)
  {
  }
  PceHandlerGroup(PceArg member) :
    PceObject(ClassHandlerGroup, member)
  {
  }
  PceHandlerGroup(PceArg member, PceArg member2) :
    PceObject(ClassHandlerGroup, member, member2)
  {
  }
  PceHandlerGroup(PceArg member, PceArg member2, PceArg member3) :
    PceObject(ClassHandlerGroup, member, member2, member3)
  {
  }
  PceHandlerGroup(PceArg member, PceArg member2, PceArg member3, PceArg member4) :
    PceObject(ClassHandlerGroup, member, member2, member3, member4)
  {
  }
  PceHandlerGroup(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5) :
    PceObject(ClassHandlerGroup, member, member2, member3, member4, member5)
  {
  }
  PceHandlerGroup(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5, PceArg member6) :
    PceObject(ClassHandlerGroup, member, member2, member3, member4, member5, member6)
  {
  }
  PceHandlerGroup(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5, PceArg member6, PceArg member7) :
    PceObject(ClassHandlerGroup, member, member2, member3, member4, member5, member6, member7)
  {
  }
  PceHandlerGroup(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5, PceArg member6, PceArg member7, PceArg member8) :
    PceObject(ClassHandlerGroup, member, member2, member3, member4, member5, member6, member7, member8)
  {
  }
  PceHandlerGroup(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5, PceArg member6, PceArg member7, PceArg member8, PceArg member9) :
    PceObject(ClassHandlerGroup, member, member2, member3, member4, member5, member6, member7, member8, member9)
  {
  }
};

inline PceHandlerGroup
AsHandlerGroup(PceArg a)
{ return *((PceHandlerGroup*) &a);
}

#endif /*!_PCE_HANDLERGROUP_H*/
