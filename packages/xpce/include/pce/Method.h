/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_METHOD_H
#define _PCE_METHOD_H

PceExternalClass(ClassMethod);
class PceMethod :public PceObject
{
public:
  PceMethod(PceArg name) :
    PceObject(ClassMethod, name)
  {
  }
  PceMethod(PceArg name, PceArg types) :
    PceObject(ClassMethod, name, types)
  {
  }
  PceMethod(PceArg name, PceArg types, PceArg implementation) :
    PceObject(ClassMethod, name, types, implementation)
  {
  }
  PceMethod(PceArg name, PceArg types, PceArg implementation, PceArg summary) :
    PceObject(ClassMethod, name, types, implementation, summary)
  {
  }
  PceMethod(PceArg name, PceArg types, PceArg implementation, PceArg summary, PceArg source) :
    PceObject(ClassMethod, name, types, implementation, summary, source)
  {
  }
  PceMethod(PceArg name, PceArg types, PceArg implementation, PceArg summary, PceArg source, PceArg group) :
    PceObject(ClassMethod, name, types, implementation, summary, source, group)
  {
  }
};

inline PceMethod
AsMethod(PceArg a)
{ return *((PceMethod*) &a);
}

#endif /*!_PCE_METHOD_H*/
