/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_GREATER_H
#define _PCE_GREATER_H

extern Any ClassGreater;
class PceGreater :public PceObject
{
public:
  PceGreater(PceArg left, PceArg right) :
    PceObject(ClassGreater, left, right)
  {
  }
};

inline PceGreater
AsGreater(PceArg a)
{ return *((PceGreater*) &a);
}

#endif /*!_PCE_GREATER_H*/
