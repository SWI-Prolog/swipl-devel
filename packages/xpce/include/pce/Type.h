/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_TYPE_H
#define _PCE_TYPE_H

PceExternalClass(ClassType);
class PceType :public PceObject
{
public:
  PceType(PceArg name) :
    PceObject(ClassType, name)
  {
  }
  PceType(PceArg name, PceArg kind) :
    PceObject(ClassType, name, kind)
  {
  }
  PceType(PceArg name, PceArg kind, PceArg context) :
    PceObject(ClassType, name, kind, context)
  {
  }
  PceType(PceArg name, PceArg kind, PceArg context, PceArg supers) :
    PceObject(ClassType, name, kind, context, supers)
  {
  }
};

inline PceType
AsType(PceArg a)
{ return *((PceType*) &a);
}

#endif /*!_PCE_TYPE_H*/
