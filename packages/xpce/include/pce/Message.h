/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_MESSAGE_H
#define _PCE_MESSAGE_H

PceExternalClass(ClassMessage);
class PceMessage :public PceObject
{
public:
  PceMessage() :
    PceObject(ClassMessage)
  {
  }
  PceMessage(PceArg receiver) :
    PceObject(ClassMessage, receiver)
  {
  }
  PceMessage(PceArg receiver, PceArg selector) :
    PceObject(ClassMessage, receiver, selector)
  {
  }
  PceMessage(PceArg receiver, PceArg selector, PceArg argument) :
    PceObject(ClassMessage, receiver, selector, argument)
  {
  }
  PceMessage(PceArg receiver, PceArg selector, PceArg argument, PceArg argument2) :
    PceObject(ClassMessage, receiver, selector, argument, argument2)
  {
  }
  PceMessage(PceArg receiver, PceArg selector, PceArg argument, PceArg argument2, PceArg argument3) :
    PceObject(ClassMessage, receiver, selector, argument, argument2, argument3)
  {
  }
  PceMessage(PceArg receiver, PceArg selector, PceArg argument, PceArg argument2, PceArg argument3, PceArg argument4) :
    PceObject(ClassMessage, receiver, selector, argument, argument2, argument3, argument4)
  {
  }
  PceMessage(PceArg receiver, PceArg selector, PceArg argument, PceArg argument2, PceArg argument3, PceArg argument4, PceArg argument5) :
    PceObject(ClassMessage, receiver, selector, argument, argument2, argument3, argument4, argument5)
  {
  }
  PceMessage(PceArg receiver, PceArg selector, PceArg argument, PceArg argument2, PceArg argument3, PceArg argument4, PceArg argument5, PceArg argument6) :
    PceObject(ClassMessage, receiver, selector, argument, argument2, argument3, argument4, argument5, argument6)
  {
  }
  PceMessage(PceArg receiver, PceArg selector, PceArg argument, PceArg argument2, PceArg argument3, PceArg argument4, PceArg argument5, PceArg argument6, PceArg argument7) :
    PceObject(ClassMessage, receiver, selector, argument, argument2, argument3, argument4, argument5, argument6, argument7)
  {
  }
};

inline PceMessage
AsMessage(PceArg a)
{ return *((PceMessage*) &a);
}

#endif /*!_PCE_MESSAGE_H*/
