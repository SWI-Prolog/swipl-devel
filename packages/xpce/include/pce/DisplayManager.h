/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_DISPLAYMANAGER_H
#define _PCE_DISPLAYMANAGER_H

extern Any ClassDisplayManager;
class PceDisplayManager :public PceObject
{
public:
  PceDisplayManager() :
    PceObject(ClassDisplayManager)
  {
  }
};

inline PceDisplayManager
AsDisplayManager(PceArg a)
{ return *((PceDisplayManager*) &a);
}

#endif /*!_PCE_DISPLAYMANAGER_H*/
