/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_MINUS_H
#define _PCE_MINUS_H

PceExternalClass(ClassMinus);
class PceMinus :public PceObject
{
public:
  PceMinus(PceArg left) :
    PceObject(ClassMinus, left)
  {
  }
  PceMinus(PceArg left, PceArg right) :
    PceObject(ClassMinus, left, right)
  {
  }
};

inline PceMinus
AsMinus(PceArg a)
{ return *((PceMinus*) &a);
}

#endif /*!_PCE_MINUS_H*/
