/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_TIMER_H
#define _PCE_TIMER_H

extern Any ClassTimer;
class PceTimer :public PceObject
{
public:
  PceTimer(PceArg interval) :
    PceObject(ClassTimer, interval)
  {
  }
  PceTimer(PceArg interval, PceArg message) :
    PceObject(ClassTimer, interval, message)
  {
  }
};

inline PceTimer
AsTimer(PceArg a)
{ return *((PceTimer*) &a);
}

#endif /*!_PCE_TIMER_H*/
