/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_HYPER_H
#define _PCE_HYPER_H

PceExternalClass(ClassHyper);
class PceHyper :public PceObject
{
public:
  PceHyper() :
    PceObject(ClassHyper)
  {
  }
  PceHyper(PceArg from) :
    PceObject(ClassHyper, from)
  {
  }
  PceHyper(PceArg from, PceArg to) :
    PceObject(ClassHyper, from, to)
  {
  }
  PceHyper(PceArg from, PceArg to, PceArg forward) :
    PceObject(ClassHyper, from, to, forward)
  {
  }
  PceHyper(PceArg from, PceArg to, PceArg forward, PceArg backward) :
    PceObject(ClassHyper, from, to, forward, backward)
  {
  }
};

inline PceHyper
AsHyper(PceArg a)
{ return *((PceHyper*) &a);
}

#endif /*!_PCE_HYPER_H*/
