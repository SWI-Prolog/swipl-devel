/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_EQUAL_H
#define _PCE_EQUAL_H

PceExternalClass(ClassEqual);
class PceEqual :public PceObject
{
public:
  PceEqual() :
    PceObject(ClassEqual)
  {
  }
  PceEqual(PceArg left) :
    PceObject(ClassEqual, left)
  {
  }
  PceEqual(PceArg left, PceArg right) :
    PceObject(ClassEqual, left, right)
  {
  }
};

inline PceEqual
AsEqual(PceArg a)
{ return *((PceEqual*) &a);
}

#endif /*!_PCE_EQUAL_H*/
