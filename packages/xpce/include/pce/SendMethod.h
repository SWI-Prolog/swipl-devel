/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_SENDMETHOD_H
#define _PCE_SENDMETHOD_H

PceExternalClass(ClassSendMethod);
class PceSendMethod :public PceObject
{
public:
  PceSendMethod(PceArg name) :
    PceObject(ClassSendMethod, name)
  {
  }
  PceSendMethod(PceArg name, PceArg types) :
    PceObject(ClassSendMethod, name, types)
  {
  }
  PceSendMethod(PceArg name, PceArg types, PceArg implementation) :
    PceObject(ClassSendMethod, name, types, implementation)
  {
  }
  PceSendMethod(PceArg name, PceArg types, PceArg implementation, PceArg summary) :
    PceObject(ClassSendMethod, name, types, implementation, summary)
  {
  }
  PceSendMethod(PceArg name, PceArg types, PceArg implementation, PceArg summary, PceArg source) :
    PceObject(ClassSendMethod, name, types, implementation, summary, source)
  {
  }
  PceSendMethod(PceArg name, PceArg types, PceArg implementation, PceArg summary, PceArg source, PceArg group) :
    PceObject(ClassSendMethod, name, types, implementation, summary, source, group)
  {
  }
};

inline PceSendMethod
AsSendMethod(PceArg a)
{ return *((PceSendMethod*) &a);
}

#endif /*!_PCE_SENDMETHOD_H*/
