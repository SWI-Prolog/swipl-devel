/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_CLASSSTUB_H
#define _PCE_CLASSSTUB_H

PceExternalClass(ClassClassStub);
class PceClassStub :public PceObject
{
public:
  PceClassStub(PceArg name) :
    PceObject(ClassClassStub, name)
  {
  }
  PceClassStub(PceArg name, PceArg super) :
    PceObject(ClassClassStub, name, super)
  {
  }
  PceClassStub(PceArg name, PceArg super, PceArg summary) :
    PceObject(ClassClassStub, name, super, summary)
  {
  }
};

inline PceClassStub
AsClassStub(PceArg a)
{ return *((PceClassStub*) &a);
}

#endif /*!_PCE_CLASSSTUB_H*/
