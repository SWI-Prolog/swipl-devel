/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_EQUATION_H
#define _PCE_EQUATION_H

extern Any ClassEquation;
class PceEquation :public PceObject
{
public:
  PceEquation(PceArg left, PceArg right) :
    PceObject(ClassEquation, left, right)
  {
  }
};

inline PceEquation
AsEquation(PceArg a)
{ return *((PceEquation*) &a);
}

#endif /*!_PCE_EQUATION_H*/
