/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_HOST_H
#define _PCE_HOST_H

extern Any ClassHost;
class PceHost :public PceObject
{
public:
  PceHost(PceArg name) :
    PceObject(ClassHost, name)
  {
  }
};

inline PceHost
AsHost(PceArg a)
{ return *((PceHost*) &a);
}

#endif /*!_PCE_HOST_H*/
