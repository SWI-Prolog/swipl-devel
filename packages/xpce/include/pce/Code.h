/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_CODE_H
#define _PCE_CODE_H

PceExternalClass(ClassCode);
class PceCode :public PceObject
{
public:
  PceCode() :
    PceObject(ClassCode)
  {
  }
};

inline PceCode
AsCode(PceArg a)
{ return *((PceCode*) &a);
}

#endif /*!_PCE_CODE_H*/
