/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_OBTAIN_H
#define _PCE_OBTAIN_H

extern Any ClassObtain;
class PceObtain :public PceObject
{
public:
  PceObtain() :
    PceObject(ClassObtain)
  {
  }
  PceObtain(PceArg receiver) :
    PceObject(ClassObtain, receiver)
  {
  }
  PceObtain(PceArg receiver, PceArg selector) :
    PceObject(ClassObtain, receiver, selector)
  {
  }
  PceObtain(PceArg receiver, PceArg selector, PceArg argument) :
    PceObject(ClassObtain, receiver, selector, argument)
  {
  }
  PceObtain(PceArg receiver, PceArg selector, PceArg argument, PceArg argument2) :
    PceObject(ClassObtain, receiver, selector, argument, argument2)
  {
  }
  PceObtain(PceArg receiver, PceArg selector, PceArg argument, PceArg argument2, PceArg argument3) :
    PceObject(ClassObtain, receiver, selector, argument, argument2, argument3)
  {
  }
  PceObtain(PceArg receiver, PceArg selector, PceArg argument, PceArg argument2, PceArg argument3, PceArg argument4) :
    PceObject(ClassObtain, receiver, selector, argument, argument2, argument3, argument4)
  {
  }
  PceObtain(PceArg receiver, PceArg selector, PceArg argument, PceArg argument2, PceArg argument3, PceArg argument4, PceArg argument5) :
    PceObject(ClassObtain, receiver, selector, argument, argument2, argument3, argument4, argument5)
  {
  }
  PceObtain(PceArg receiver, PceArg selector, PceArg argument, PceArg argument2, PceArg argument3, PceArg argument4, PceArg argument5, PceArg argument6) :
    PceObject(ClassObtain, receiver, selector, argument, argument2, argument3, argument4, argument5, argument6)
  {
  }
  PceObtain(PceArg receiver, PceArg selector, PceArg argument, PceArg argument2, PceArg argument3, PceArg argument4, PceArg argument5, PceArg argument6, PceArg argument7) :
    PceObject(ClassObtain, receiver, selector, argument, argument2, argument3, argument4, argument5, argument6, argument7)
  {
  }
};

inline PceObtain
AsObtain(PceArg a)
{ return *((PceObtain*) &a);
}

#endif /*!_PCE_OBTAIN_H*/
