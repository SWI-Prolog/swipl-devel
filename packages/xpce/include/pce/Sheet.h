/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_SHEET_H
#define _PCE_SHEET_H

PceExternalClass(ClassSheet);
class PceSheet :public PceObject
{
public:
  PceSheet() :
    PceObject(ClassSheet)
  {
  }
  PceSheet(PceArg member) :
    PceObject(ClassSheet, member)
  {
  }
  PceSheet(PceArg member, PceArg member2) :
    PceObject(ClassSheet, member, member2)
  {
  }
  PceSheet(PceArg member, PceArg member2, PceArg member3) :
    PceObject(ClassSheet, member, member2, member3)
  {
  }
  PceSheet(PceArg member, PceArg member2, PceArg member3, PceArg member4) :
    PceObject(ClassSheet, member, member2, member3, member4)
  {
  }
  PceSheet(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5) :
    PceObject(ClassSheet, member, member2, member3, member4, member5)
  {
  }
  PceSheet(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5, PceArg member6) :
    PceObject(ClassSheet, member, member2, member3, member4, member5, member6)
  {
  }
  PceSheet(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5, PceArg member6, PceArg member7) :
    PceObject(ClassSheet, member, member2, member3, member4, member5, member6, member7)
  {
  }
  PceSheet(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5, PceArg member6, PceArg member7, PceArg member8) :
    PceObject(ClassSheet, member, member2, member3, member4, member5, member6, member7, member8)
  {
  }
  PceSheet(PceArg member, PceArg member2, PceArg member3, PceArg member4, PceArg member5, PceArg member6, PceArg member7, PceArg member8, PceArg member9) :
    PceObject(ClassSheet, member, member2, member3, member4, member5, member6, member7, member8, member9)
  {
  }
};

inline PceSheet
AsSheet(PceArg a)
{ return *((PceSheet*) &a);
}

#endif /*!_PCE_SHEET_H*/
