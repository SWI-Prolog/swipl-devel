/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_IDENTITY_H
#define _PCE_IDENTITY_H

extern Any ClassIdentity;
class PceIdentity :public PceObject
{
public:
  PceIdentity(PceArg selector1) :
    PceObject(ClassIdentity, selector1)
  {
  }
  PceIdentity(PceArg selector1, PceArg selector2) :
    PceObject(ClassIdentity, selector1, selector2)
  {
  }
};

inline PceIdentity
AsIdentity(PceArg a)
{ return *((PceIdentity*) &a);
}

#endif /*!_PCE_IDENTITY_H*/
