/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_NUMBER_H
#define _PCE_NUMBER_H

extern Any ClassNumber;
class PceNumber :public PceObject
{
public:
  PceNumber(PceArg value) :
    PceObject(ClassNumber, value)
  {
  }
};

inline PceNumber
AsNumber(PceArg a)
{ return *((PceNumber*) &a);
}

#endif /*!_PCE_NUMBER_H*/
