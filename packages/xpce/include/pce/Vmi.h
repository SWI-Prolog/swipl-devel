/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_VMI_H
#define _PCE_VMI_H

extern Any ClassVmi;
class PceVmi :public PceObject
{
public:
  PceVmi(PceArg name) :
    PceObject(ClassVmi, name)
  {
  }
};

inline PceVmi
AsVmi(PceArg a)
{ return *((PceVmi*) &a);
}

#endif /*!_PCE_VMI_H*/
