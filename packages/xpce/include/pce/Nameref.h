/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_NAMEREF_H
#define _PCE_NAMEREF_H

PceExternalClass(ClassNameref);
class PceNameref :public PceObject
{
public:
  PceNameref(PceArg reference) :
    PceObject(ClassNameref, reference)
  {
  }
  PceNameref(PceArg reference, PceArg object) :
    PceObject(ClassNameref, reference, object)
  {
  }
};

inline PceNameref
AsNameref(PceArg a)
{ return *((PceNameref*) &a);
}

#endif /*!_PCE_NAMEREF_H*/
