/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_VECTOR_H
#define _PCE_VECTOR_H

PceExternalClass(ClassVector);
class PceVector :public PceObject
{
public:
  PceVector() :
    PceObject(ClassVector)
  {
  }
  PceVector(PceArg element) :
    PceObject(ClassVector, element)
  {
  }
  PceVector(PceArg element, PceArg element2) :
    PceObject(ClassVector, element, element2)
  {
  }
  PceVector(PceArg element, PceArg element2, PceArg element3) :
    PceObject(ClassVector, element, element2, element3)
  {
  }
  PceVector(PceArg element, PceArg element2, PceArg element3, PceArg element4) :
    PceObject(ClassVector, element, element2, element3, element4)
  {
  }
  PceVector(PceArg element, PceArg element2, PceArg element3, PceArg element4, PceArg element5) :
    PceObject(ClassVector, element, element2, element3, element4, element5)
  {
  }
  PceVector(PceArg element, PceArg element2, PceArg element3, PceArg element4, PceArg element5, PceArg element6) :
    PceObject(ClassVector, element, element2, element3, element4, element5, element6)
  {
  }
  PceVector(PceArg element, PceArg element2, PceArg element3, PceArg element4, PceArg element5, PceArg element6, PceArg element7) :
    PceObject(ClassVector, element, element2, element3, element4, element5, element6, element7)
  {
  }
  PceVector(PceArg element, PceArg element2, PceArg element3, PceArg element4, PceArg element5, PceArg element6, PceArg element7, PceArg element8) :
    PceObject(ClassVector, element, element2, element3, element4, element5, element6, element7, element8)
  {
  }
  PceVector(PceArg element, PceArg element2, PceArg element3, PceArg element4, PceArg element5, PceArg element6, PceArg element7, PceArg element8, PceArg element9) :
    PceObject(ClassVector, element, element2, element3, element4, element5, element6, element7, element8, element9)
  {
  }
};

inline PceVector
AsVector(PceArg a)
{ return *((PceVector*) &a);
}

#endif /*!_PCE_VECTOR_H*/
