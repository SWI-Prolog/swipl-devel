/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_CONNECTION_H
#define _PCE_CONNECTION_H

PceExternalClass(ClassConnection);
class PceConnection :public PceObject
{
public:
  PceConnection(PceArg from, PceArg to) :
    PceObject(ClassConnection, from, to)
  {
  }
  PceConnection(PceArg from, PceArg to, PceArg link) :
    PceObject(ClassConnection, from, to, link)
  {
  }
  PceConnection(PceArg from, PceArg to, PceArg link, PceArg handle_from) :
    PceObject(ClassConnection, from, to, link, handle_from)
  {
  }
  PceConnection(PceArg from, PceArg to, PceArg link, PceArg handle_from, PceArg handle_to) :
    PceObject(ClassConnection, from, to, link, handle_from, handle_to)
  {
  }
};

inline PceConnection
AsConnection(PceArg a)
{ return *((PceConnection*) &a);
}

#endif /*!_PCE_CONNECTION_H*/
