/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_VAR_H
#define _PCE_VAR_H

extern Any ClassVar;
class PceVar :public PceObject
{
public:
  PceVar() :
    PceObject(ClassVar)
  {
  }
  PceVar(PceArg type) :
    PceObject(ClassVar, type)
  {
  }
  PceVar(PceArg type, PceArg name) :
    PceObject(ClassVar, type, name)
  {
  }
  PceVar(PceArg type, PceArg name, PceArg value) :
    PceObject(ClassVar, type, name, value)
  {
  }
};

inline PceVar
AsVar(PceArg a)
{ return *((PceVar*) &a);
}

#endif /*!_PCE_VAR_H*/
