/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_DIVIDE_H
#define _PCE_DIVIDE_H

PceExternalClass(ClassDivide);
class PceDivide :public PceObject
{
public:
  PceDivide(PceArg left, PceArg right) :
    PceObject(ClassDivide, left, right)
  {
  }
};

inline PceDivide
AsDivide(PceArg a)
{ return *((PceDivide*) &a);
}

#endif /*!_PCE_DIVIDE_H*/
