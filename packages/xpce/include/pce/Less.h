/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_LESS_H
#define _PCE_LESS_H

extern Any ClassLess;
class PceLess :public PceObject
{
public:
  PceLess(PceArg left, PceArg right) :
    PceObject(ClassLess, left, right)
  {
  }
};

inline PceLess
AsLess(PceArg a)
{ return *((PceLess*) &a);
}

#endif /*!_PCE_LESS_H*/
