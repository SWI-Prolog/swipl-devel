/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_TIMES_H
#define _PCE_TIMES_H

PceExternalClass(ClassTimes);
class PceTimes :public PceObject
{
public:
  PceTimes(PceArg left, PceArg right) :
    PceObject(ClassTimes, left, right)
  {
  }
};

inline PceTimes
AsTimes(PceArg a)
{ return *((PceTimes*) &a);
}

#endif /*!_PCE_TIMES_H*/
