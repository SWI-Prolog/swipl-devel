/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_GREATEREQUAL_H
#define _PCE_GREATEREQUAL_H

extern Any ClassGreaterEqual;
class PceGreaterEqual :public PceObject
{
public:
  PceGreaterEqual(PceArg left, PceArg right) :
    PceObject(ClassGreaterEqual, left, right)
  {
  }
};

inline PceGreaterEqual
AsGreaterEqual(PceArg a)
{ return *((PceGreaterEqual*) &a);
}

#endif /*!_PCE_GREATEREQUAL_H*/
