/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_CODEVECTOR_H
#define _PCE_CODEVECTOR_H

PceExternalClass(ClassCodeVector);
class PceCodeVector :public PceObject
{
public:
  PceCodeVector() :
    PceObject(ClassCodeVector)
  {
  }
  PceCodeVector(PceArg element) :
    PceObject(ClassCodeVector, element)
  {
  }
  PceCodeVector(PceArg element, PceArg element2) :
    PceObject(ClassCodeVector, element, element2)
  {
  }
  PceCodeVector(PceArg element, PceArg element2, PceArg element3) :
    PceObject(ClassCodeVector, element, element2, element3)
  {
  }
  PceCodeVector(PceArg element, PceArg element2, PceArg element3, PceArg element4) :
    PceObject(ClassCodeVector, element, element2, element3, element4)
  {
  }
  PceCodeVector(PceArg element, PceArg element2, PceArg element3, PceArg element4, PceArg element5) :
    PceObject(ClassCodeVector, element, element2, element3, element4, element5)
  {
  }
  PceCodeVector(PceArg element, PceArg element2, PceArg element3, PceArg element4, PceArg element5, PceArg element6) :
    PceObject(ClassCodeVector, element, element2, element3, element4, element5, element6)
  {
  }
  PceCodeVector(PceArg element, PceArg element2, PceArg element3, PceArg element4, PceArg element5, PceArg element6, PceArg element7) :
    PceObject(ClassCodeVector, element, element2, element3, element4, element5, element6, element7)
  {
  }
  PceCodeVector(PceArg element, PceArg element2, PceArg element3, PceArg element4, PceArg element5, PceArg element6, PceArg element7, PceArg element8) :
    PceObject(ClassCodeVector, element, element2, element3, element4, element5, element6, element7, element8)
  {
  }
  PceCodeVector(PceArg element, PceArg element2, PceArg element3, PceArg element4, PceArg element5, PceArg element6, PceArg element7, PceArg element8, PceArg element9) :
    PceObject(ClassCodeVector, element, element2, element3, element4, element5, element6, element7, element8, element9)
  {
  }
};

inline PceCodeVector
AsCodeVector(PceArg a)
{ return *((PceCodeVector*) &a);
}

#endif /*!_PCE_CODEVECTOR_H*/
