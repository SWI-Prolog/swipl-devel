/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_DEVICE_H
#define _PCE_DEVICE_H

PceExternalClass(ClassDevice);
class PceDevice :public PceObject
{
public:
  PceDevice() :
    PceObject(ClassDevice)
  {
  }
};

inline PceDevice
AsDevice(PceArg a)
{ return *((PceDevice*) &a);
}

#endif /*!_PCE_DEVICE_H*/
