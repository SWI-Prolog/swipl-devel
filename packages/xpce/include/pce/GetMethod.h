/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_GETMETHOD_H
#define _PCE_GETMETHOD_H

PceExternalClass(ClassGetMethod);
class PceGetMethod :public PceObject
{
public:
  PceGetMethod(PceArg name) :
    PceObject(ClassGetMethod, name)
  {
  }
  PceGetMethod(PceArg name, PceArg ret) :
    PceObject(ClassGetMethod, name, ret)
  {
  }
  PceGetMethod(PceArg name, PceArg ret, PceArg types) :
    PceObject(ClassGetMethod, name, ret, types)
  {
  }
  PceGetMethod(PceArg name, PceArg ret, PceArg types, PceArg implementation) :
    PceObject(ClassGetMethod, name, ret, types, implementation)
  {
  }
  PceGetMethod(PceArg name, PceArg ret, PceArg types, PceArg implementation, PceArg summary) :
    PceObject(ClassGetMethod, name, ret, types, implementation, summary)
  {
  }
  PceGetMethod(PceArg name, PceArg ret, PceArg types, PceArg implementation, PceArg summary, PceArg source) :
    PceObject(ClassGetMethod, name, ret, types, implementation, summary, source)
  {
  }
  PceGetMethod(PceArg name, PceArg ret, PceArg types, PceArg implementation, PceArg summary, PceArg source, PceArg group) :
    PceObject(ClassGetMethod, name, ret, types, implementation, summary, source, group)
  {
  }
};

inline PceGetMethod
AsGetMethod(PceArg a)
{ return *((PceGetMethod*) &a);
}

#endif /*!_PCE_GETMETHOD_H*/
