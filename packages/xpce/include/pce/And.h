/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_AND_H
#define _PCE_AND_H

PceExternalClass(ClassAnd);
class PceAnd :public PceObject
{
public:
  PceAnd() :
    PceObject(ClassAnd)
  {
  }
  PceAnd(PceArg test) :
    PceObject(ClassAnd, test)
  {
  }
  PceAnd(PceArg test, PceArg test2) :
    PceObject(ClassAnd, test, test2)
  {
  }
  PceAnd(PceArg test, PceArg test2, PceArg test3) :
    PceObject(ClassAnd, test, test2, test3)
  {
  }
  PceAnd(PceArg test, PceArg test2, PceArg test3, PceArg test4) :
    PceObject(ClassAnd, test, test2, test3, test4)
  {
  }
  PceAnd(PceArg test, PceArg test2, PceArg test3, PceArg test4, PceArg test5) :
    PceObject(ClassAnd, test, test2, test3, test4, test5)
  {
  }
  PceAnd(PceArg test, PceArg test2, PceArg test3, PceArg test4, PceArg test5, PceArg test6) :
    PceObject(ClassAnd, test, test2, test3, test4, test5, test6)
  {
  }
  PceAnd(PceArg test, PceArg test2, PceArg test3, PceArg test4, PceArg test5, PceArg test6, PceArg test7) :
    PceObject(ClassAnd, test, test2, test3, test4, test5, test6, test7)
  {
  }
  PceAnd(PceArg test, PceArg test2, PceArg test3, PceArg test4, PceArg test5, PceArg test6, PceArg test7, PceArg test8) :
    PceObject(ClassAnd, test, test2, test3, test4, test5, test6, test7, test8)
  {
  }
  PceAnd(PceArg test, PceArg test2, PceArg test3, PceArg test4, PceArg test5, PceArg test6, PceArg test7, PceArg test8, PceArg test9) :
    PceObject(ClassAnd, test, test2, test3, test4, test5, test6, test7, test8, test9)
  {
  }
};

inline PceAnd
AsAnd(PceArg a)
{ return *((PceAnd*) &a);
}

#endif /*!_PCE_AND_H*/
