/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_C_H
#define _PCE_C_H

extern Any ClassC;
class PceC :public PceObject
{
public:
  PceC() :
    PceObject(ClassC)
  {
  }
};

inline PceC
AsC(PceArg a)
{ return *((PceC*) &a);
}

#endif /*!_PCE_C_H*/
