/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_PROGN_H
#define _PCE_PROGN_H

PceExternalClass(ClassProgn);
class PceProgn :public PceObject
{
public:
  PceProgn() :
    PceObject(ClassProgn)
  {
  }
  PceProgn(PceArg statement) :
    PceObject(ClassProgn, statement)
  {
  }
  PceProgn(PceArg statement, PceArg statement2) :
    PceObject(ClassProgn, statement, statement2)
  {
  }
  PceProgn(PceArg statement, PceArg statement2, PceArg statement3) :
    PceObject(ClassProgn, statement, statement2, statement3)
  {
  }
  PceProgn(PceArg statement, PceArg statement2, PceArg statement3, PceArg statement4) :
    PceObject(ClassProgn, statement, statement2, statement3, statement4)
  {
  }
  PceProgn(PceArg statement, PceArg statement2, PceArg statement3, PceArg statement4, PceArg statement5) :
    PceObject(ClassProgn, statement, statement2, statement3, statement4, statement5)
  {
  }
  PceProgn(PceArg statement, PceArg statement2, PceArg statement3, PceArg statement4, PceArg statement5, PceArg statement6) :
    PceObject(ClassProgn, statement, statement2, statement3, statement4, statement5, statement6)
  {
  }
  PceProgn(PceArg statement, PceArg statement2, PceArg statement3, PceArg statement4, PceArg statement5, PceArg statement6, PceArg statement7) :
    PceObject(ClassProgn, statement, statement2, statement3, statement4, statement5, statement6, statement7)
  {
  }
  PceProgn(PceArg statement, PceArg statement2, PceArg statement3, PceArg statement4, PceArg statement5, PceArg statement6, PceArg statement7, PceArg statement8) :
    PceObject(ClassProgn, statement, statement2, statement3, statement4, statement5, statement6, statement7, statement8)
  {
  }
  PceProgn(PceArg statement, PceArg statement2, PceArg statement3, PceArg statement4, PceArg statement5, PceArg statement6, PceArg statement7, PceArg statement8, PceArg statement9) :
    PceObject(ClassProgn, statement, statement2, statement3, statement4, statement5, statement6, statement7, statement8, statement9)
  {
  }
};

inline PceProgn
AsProgn(PceArg a)
{ return *((PceProgn*) &a);
}

#endif /*!_PCE_PROGN_H*/
