/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_VISUAL_H
#define _PCE_VISUAL_H

extern Any ClassVisual;
class PceVisual :public PceObject
{
public:
  PceVisual() :
    PceObject(ClassVisual)
  {
  }
  PceVisual(PceArg a) :
    PceObject(ClassVisual, a)
  {
  }
  PceVisual(PceArg a, PceArg a2) :
    PceObject(ClassVisual, a, a2)
  {
  }
  PceVisual(PceArg a, PceArg a2, PceArg a3) :
    PceObject(ClassVisual, a, a2, a3)
  {
  }
  PceVisual(PceArg a, PceArg a2, PceArg a3, PceArg a4) :
    PceObject(ClassVisual, a, a2, a3, a4)
  {
  }
  PceVisual(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5) :
    PceObject(ClassVisual, a, a2, a3, a4, a5)
  {
  }
  PceVisual(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6) :
    PceObject(ClassVisual, a, a2, a3, a4, a5, a6)
  {
  }
  PceVisual(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6, PceArg a7) :
    PceObject(ClassVisual, a, a2, a3, a4, a5, a6, a7)
  {
  }
  PceVisual(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6, PceArg a7, PceArg a8) :
    PceObject(ClassVisual, a, a2, a3, a4, a5, a6, a7, a8)
  {
  }
  PceVisual(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6, PceArg a7, PceArg a8, PceArg a9) :
    PceObject(ClassVisual, a, a2, a3, a4, a5, a6, a7, a8, a9)
  {
  }
};

inline PceVisual
AsVisual(PceArg a)
{ return *((PceVisual*) &a);
}

#endif /*!_PCE_VISUAL_H*/
