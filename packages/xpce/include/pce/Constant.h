/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_CONSTANT_H
#define _PCE_CONSTANT_H

PceExternalClass(ClassConstant);
class PceConstant :public PceObject
{
public:
  PceConstant(PceArg name, PceArg summary) :
    PceObject(ClassConstant, name, summary)
  {
  }
};

inline PceConstant
AsConstant(PceArg a)
{ return *((PceConstant*) &a);
}

#endif /*!_PCE_CONSTANT_H*/
