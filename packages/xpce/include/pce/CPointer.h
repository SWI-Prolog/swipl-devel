/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_CPOINTER_H
#define _PCE_CPOINTER_H

PceExternalClass(ClassCPointer);
class PceCPointer :public PceObject
{
public:
  PceCPointer() :
    PceObject(ClassCPointer)
  {
  }
  PceCPointer(PceArg a) :
    PceObject(ClassCPointer, a)
  {
  }
};

inline PceCPointer
AsCPointer(PceArg a)
{ return *((PceCPointer*) &a);
}

#endif /*!_PCE_CPOINTER_H*/
