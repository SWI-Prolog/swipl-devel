/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_TILE_H
#define _PCE_TILE_H

extern Any ClassTile;
class PceTile :public PceObject
{
public:
  PceTile() :
    PceObject(ClassTile)
  {
  }
  PceTile(PceArg object) :
    PceObject(ClassTile, object)
  {
  }
  PceTile(PceArg object, PceArg width) :
    PceObject(ClassTile, object, width)
  {
  }
  PceTile(PceArg object, PceArg width, PceArg height) :
    PceObject(ClassTile, object, width, height)
  {
  }
};

inline PceTile
AsTile(PceArg a)
{ return *((PceTile*) &a);
}

#endif /*!_PCE_TILE_H*/
