/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_PLUS_H
#define _PCE_PLUS_H

PceExternalClass(ClassPlus);
class PcePlus :public PceObject
{
public:
  PcePlus(PceArg left, PceArg right) :
    PceObject(ClassPlus, left, right)
  {
  }
};

inline PcePlus
AsPlus(PceArg a)
{ return *((PcePlus*) &a);
}

#endif /*!_PCE_PLUS_H*/
