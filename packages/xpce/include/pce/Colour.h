/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_COLOUR_H
#define _PCE_COLOUR_H

extern Any ClassColour;
class PceColour :public PceObject
{
public:
  PceColour(PceArg name) :
    PceObject(ClassColour, name)
  {
  }
  PceColour(PceArg name, PceArg red) :
    PceObject(ClassColour, name, red)
  {
  }
  PceColour(PceArg name, PceArg red, PceArg green) :
    PceObject(ClassColour, name, red, green)
  {
  }
  PceColour(PceArg name, PceArg red, PceArg green, PceArg blue) :
    PceObject(ClassColour, name, red, green, blue)
  {
  }
};

inline PceColour
AsColour(PceArg a)
{ return *((PceColour*) &a);
}

#endif /*!_PCE_COLOUR_H*/
