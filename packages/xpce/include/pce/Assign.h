/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_ASSIGN_H
#define _PCE_ASSIGN_H

extern Any ClassAssign;
class PceAssign :public PceObject
{
public:
  PceAssign(PceArg variable) :
    PceObject(ClassAssign, variable)
  {
  }
  PceAssign(PceArg variable, PceArg value) :
    PceObject(ClassAssign, variable, value)
  {
  }
  PceAssign(PceArg variable, PceArg value, PceArg scope) :
    PceObject(ClassAssign, variable, value, scope)
  {
  }
};

inline PceAssign
AsAssign(PceArg a)
{ return *((PceAssign*) &a);
}

#endif /*!_PCE_ASSIGN_H*/
