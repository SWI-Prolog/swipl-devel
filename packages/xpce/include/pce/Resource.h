/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_RESOURCE_H
#define _PCE_RESOURCE_H

PceExternalClass(ClassResource);
class PceResource :public PceObject
{
public:
  PceResource(PceArg name) :
    PceObject(ClassResource, name)
  {
  }
  PceResource(PceArg name, PceArg cl) :
    PceObject(ClassResource, name, cl)
  {
  }
  PceResource(PceArg name, PceArg cl, PceArg type) :
    PceObject(ClassResource, name, cl, type)
  {
  }
  PceResource(PceArg name, PceArg cl, PceArg type, PceArg def) :
    PceObject(ClassResource, name, cl, type, def)
  {
  }
  PceResource(PceArg name, PceArg cl, PceArg type, PceArg def, PceArg context) :
    PceObject(ClassResource, name, cl, type, def, context)
  {
  }
  PceResource(PceArg name, PceArg cl, PceArg type, PceArg def, PceArg context, PceArg summary) :
    PceObject(ClassResource, name, cl, type, def, context, summary)
  {
  }
};

inline PceResource
AsResource(PceArg a)
{ return *((PceResource*) &a);
}

#endif /*!_PCE_RESOURCE_H*/
