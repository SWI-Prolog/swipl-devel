/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_COLOURMAP_H
#define _PCE_COLOURMAP_H

PceExternalClass(ClassColourMap);
class PceColourMap :public PceObject
{
public:
  PceColourMap() :
    PceObject(ClassColourMap)
  {
  }
  PceColourMap(PceArg name) :
    PceObject(ClassColourMap, name)
  {
  }
  PceColourMap(PceArg name, PceArg colours) :
    PceObject(ClassColourMap, name, colours)
  {
  }
};

inline PceColourMap
AsColourMap(PceArg a)
{ return *((PceColourMap*) &a);
}

#endif /*!_PCE_COLOURMAP_H*/
