/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_HANDLER_H
#define _PCE_HANDLER_H

PceExternalClass(ClassHandler);
class PceHandler :public PceObject
{
public:
  PceHandler(PceArg event, PceArg message) :
    PceObject(ClassHandler, event, message)
  {
  }
  PceHandler(PceArg event, PceArg message, PceArg restrict_to) :
    PceObject(ClassHandler, event, message, restrict_to)
  {
  }
};

inline PceHandler
AsHandler(PceArg a)
{ return *((PceHandler*) &a);
}

#endif /*!_PCE_HANDLER_H*/
