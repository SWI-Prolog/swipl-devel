/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_RELATION_H
#define _PCE_RELATION_H

extern Any ClassRelation;
class PceRelation :public PceObject
{
public:
  PceRelation() :
    PceObject(ClassRelation)
  {
  }
  PceRelation(PceArg a) :
    PceObject(ClassRelation, a)
  {
  }
  PceRelation(PceArg a, PceArg a2) :
    PceObject(ClassRelation, a, a2)
  {
  }
  PceRelation(PceArg a, PceArg a2, PceArg a3) :
    PceObject(ClassRelation, a, a2, a3)
  {
  }
  PceRelation(PceArg a, PceArg a2, PceArg a3, PceArg a4) :
    PceObject(ClassRelation, a, a2, a3, a4)
  {
  }
  PceRelation(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5) :
    PceObject(ClassRelation, a, a2, a3, a4, a5)
  {
  }
  PceRelation(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6) :
    PceObject(ClassRelation, a, a2, a3, a4, a5, a6)
  {
  }
  PceRelation(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6, PceArg a7) :
    PceObject(ClassRelation, a, a2, a3, a4, a5, a6, a7)
  {
  }
  PceRelation(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6, PceArg a7, PceArg a8) :
    PceObject(ClassRelation, a, a2, a3, a4, a5, a6, a7, a8)
  {
  }
  PceRelation(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6, PceArg a7, PceArg a8, PceArg a9) :
    PceObject(ClassRelation, a, a2, a3, a4, a5, a6, a7, a8, a9)
  {
  }
};

inline PceRelation
AsRelation(PceArg a)
{ return *((PceRelation*) &a);
}

#endif /*!_PCE_RELATION_H*/
