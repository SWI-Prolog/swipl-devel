/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_OR_H
#define _PCE_OR_H

PceExternalClass(ClassOr);
class PceOr :public PceObject
{
public:
  PceOr() :
    PceObject(ClassOr)
  {
  }
  PceOr(PceArg test) :
    PceObject(ClassOr, test)
  {
  }
  PceOr(PceArg test, PceArg test2) :
    PceObject(ClassOr, test, test2)
  {
  }
  PceOr(PceArg test, PceArg test2, PceArg test3) :
    PceObject(ClassOr, test, test2, test3)
  {
  }
  PceOr(PceArg test, PceArg test2, PceArg test3, PceArg test4) :
    PceObject(ClassOr, test, test2, test3, test4)
  {
  }
  PceOr(PceArg test, PceArg test2, PceArg test3, PceArg test4, PceArg test5) :
    PceObject(ClassOr, test, test2, test3, test4, test5)
  {
  }
  PceOr(PceArg test, PceArg test2, PceArg test3, PceArg test4, PceArg test5, PceArg test6) :
    PceObject(ClassOr, test, test2, test3, test4, test5, test6)
  {
  }
  PceOr(PceArg test, PceArg test2, PceArg test3, PceArg test4, PceArg test5, PceArg test6, PceArg test7) :
    PceObject(ClassOr, test, test2, test3, test4, test5, test6, test7)
  {
  }
  PceOr(PceArg test, PceArg test2, PceArg test3, PceArg test4, PceArg test5, PceArg test6, PceArg test7, PceArg test8) :
    PceObject(ClassOr, test, test2, test3, test4, test5, test6, test7, test8)
  {
  }
  PceOr(PceArg test, PceArg test2, PceArg test3, PceArg test4, PceArg test5, PceArg test6, PceArg test7, PceArg test8, PceArg test9) :
    PceObject(ClassOr, test, test2, test3, test4, test5, test6, test7, test8, test9)
  {
  }
};

inline PceOr
AsOr(PceArg a)
{ return *((PceOr*) &a);
}

#endif /*!_PCE_OR_H*/
