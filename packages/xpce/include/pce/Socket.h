/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_SOCKET_H
#define _PCE_SOCKET_H

extern Any ClassSocket;
class PceSocket :public PceObject
{
public:
  PceSocket(PceArg address) :
    PceObject(ClassSocket, address)
  {
  }
  PceSocket(PceArg address, PceArg domain) :
    PceObject(ClassSocket, address, domain)
  {
  }
};

inline PceSocket
AsSocket(PceArg a)
{ return *((PceSocket*) &a);
}

#endif /*!_PCE_SOCKET_H*/
