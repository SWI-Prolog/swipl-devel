/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_ELEVATION_H
#define _PCE_ELEVATION_H

PceExternalClass(ClassElevation);
class PceElevation :public PceObject
{
public:
  PceElevation() :
    PceObject(ClassElevation)
  {
  }
  PceElevation(PceArg name) :
    PceObject(ClassElevation, name)
  {
  }
  PceElevation(PceArg name, PceArg height) :
    PceObject(ClassElevation, name, height)
  {
  }
  PceElevation(PceArg name, PceArg height, PceArg colour) :
    PceObject(ClassElevation, name, height, colour)
  {
  }
  PceElevation(PceArg name, PceArg height, PceArg colour, PceArg relief) :
    PceObject(ClassElevation, name, height, colour, relief)
  {
  }
  PceElevation(PceArg name, PceArg height, PceArg colour, PceArg relief, PceArg shadow) :
    PceObject(ClassElevation, name, height, colour, relief, shadow)
  {
  }
  PceElevation(PceArg name, PceArg height, PceArg colour, PceArg relief, PceArg shadow, PceArg kind) :
    PceObject(ClassElevation, name, height, colour, relief, shadow, kind)
  {
  }
  PceElevation(PceArg name, PceArg height, PceArg colour, PceArg relief, PceArg shadow, PceArg kind, PceArg background) :
    PceObject(ClassElevation, name, height, colour, relief, shadow, kind, background)
  {
  }
};

inline PceElevation
AsElevation(PceArg a)
{ return *((PceElevation*) &a);
}

#endif /*!_PCE_ELEVATION_H*/
