/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_NOT_H
#define _PCE_NOT_H

extern Any ClassNot;
class PceNot :public PceObject
{
public:
  PceNot(PceArg test) :
    PceObject(ClassNot, test)
  {
  }
};

inline PceNot
AsNot(PceArg a)
{ return *((PceNot*) &a);
}

#endif /*!_PCE_NOT_H*/
