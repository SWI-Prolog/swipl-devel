/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_APPLICATION_H
#define _PCE_APPLICATION_H

PceExternalClass(ClassApplication);
class PceApplication :public PceObject
{
public:
  PceApplication(PceArg name) :
    PceObject(ClassApplication, name)
  {
  }
};

inline PceApplication
AsApplication(PceArg a)
{ return *((PceApplication*) &a);
}

#endif /*!_PCE_APPLICATION_H*/
