/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_TABSTACK_H
#define _PCE_TABSTACK_H

PceExternalClass(ClassTabStack);
class PceTabStack :public PceObject
{
public:
  PceTabStack() :
    PceObject(ClassTabStack)
  {
  }
  PceTabStack(PceArg member) :
    PceObject(ClassTabStack, member)
  {
  }
  PceTabStack(PceArg member, PceArg member2) :
    PceObject(ClassTabStack, member, member2)
  {
  }
  PceTabStack(PceArg member, PceArg member2, PceArg member3) :
    PceObject(ClassTabStack, member, member2, member3)
  {
  }
  PceTabStack(PceArg member, PceArg member2, PceArg member3, PceArg member4) :
    PceObject(ClassTabStack, member, member2, member3, member4)
  {
  }
  PceTabStack(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5) :
    PceObject(ClassTabStack, member, member2, member3, member4, member5)
  {
  }
  PceTabStack(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5, PceArg member6) :
    PceObject(ClassTabStack, member, member2, member3, member4, member5, member6)
  {
  }
  PceTabStack(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5, PceArg member6, PceArg member7) :
    PceObject(ClassTabStack, member, member2, member3, member4, member5, member6, member7)
  {
  }
  PceTabStack(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5, PceArg member6, PceArg member7, PceArg member8) :
    PceObject(ClassTabStack, member, member2, member3, member4, member5, member6, member7, member8)
  {
  }
  PceTabStack(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5, PceArg member6, PceArg member7, PceArg member8, PceArg member9) :
    PceObject(ClassTabStack, member, member2, member3, member4, member5, member6, member7, member8, member9)
  {
  }
};

inline PceTabStack
AsTabStack(PceArg a)
{ return *((PceTabStack*) &a);
}

#endif /*!_PCE_TABSTACK_H*/
