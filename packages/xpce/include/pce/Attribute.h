/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_ATTRIBUTE_H
#define _PCE_ATTRIBUTE_H

extern Any ClassAttribute;
class PceAttribute :public PceObject
{
public:
  PceAttribute() :
    PceObject(ClassAttribute)
  {
  }
  PceAttribute(PceArg name) :
    PceObject(ClassAttribute, name)
  {
  }
  PceAttribute(PceArg name, PceArg value) :
    PceObject(ClassAttribute, name, value)
  {
  }
};

inline PceAttribute
AsAttribute(PceArg a)
{ return *((PceAttribute*) &a);
}

#endif /*!_PCE_ATTRIBUTE_H*/
