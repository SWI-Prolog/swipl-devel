/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_EVENT_H
#define _PCE_EVENT_H

extern Any ClassEvent;
class PceEvent :public PceObject
{
public:
  PceEvent(PceArg id) :
    PceObject(ClassEvent, id)
  {
  }
  PceEvent(PceArg id, PceArg origin) :
    PceObject(ClassEvent, id, origin)
  {
  }
  PceEvent(PceArg id, PceArg origin, PceArg x) :
    PceObject(ClassEvent, id, origin, x)
  {
  }
  PceEvent(PceArg id, PceArg origin, PceArg x, PceArg y) :
    PceObject(ClassEvent, id, origin, x, y)
  {
  }
  PceEvent(PceArg id, PceArg origin, PceArg x, PceArg y, PceArg button_mask) :
    PceObject(ClassEvent, id, origin, x, y, button_mask)
  {
  }
  PceEvent(PceArg id, PceArg origin, PceArg x, PceArg y, PceArg button_mask, PceArg time) :
    PceObject(ClassEvent, id, origin, x, y, button_mask, time)
  {
  }
};

inline PceEvent
AsEvent(PceArg a)
{ return *((PceEvent*) &a);
}

#endif /*!_PCE_EVENT_H*/
