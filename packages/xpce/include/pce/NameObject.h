/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_NAMEOBJECT_H
#define _PCE_NAMEOBJECT_H

PceExternalClass(ClassNameObject);
class PceNameObject :public PceObject
{
public:
  PceNameObject(PceArg reference) :
    PceObject(ClassNameObject, reference)
  {
  }
  PceNameObject(PceArg reference, PceArg object) :
    PceObject(ClassNameObject, reference, object)
  {
  }
};

inline PceNameObject
AsNameObject(PceArg a)
{ return *((PceNameObject*) &a);
}

#endif /*!_PCE_NAMEOBJECT_H*/
