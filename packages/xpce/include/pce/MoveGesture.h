/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_MOVEGESTURE_H
#define _PCE_MOVEGESTURE_H

extern Any ClassMoveGesture;
class PceMoveGesture :public PceObject
{
public:
  PceMoveGesture() :
    PceObject(ClassMoveGesture)
  {
  }
  PceMoveGesture(PceArg button) :
    PceObject(ClassMoveGesture, button)
  {
  }
  PceMoveGesture(PceArg button, PceArg modifier) :
    PceObject(ClassMoveGesture, button, modifier)
  {
  }
};

inline PceMoveGesture
AsMoveGesture(PceArg a)
{ return *((PceMoveGesture*) &a);
}

#endif /*!_PCE_MOVEGESTURE_H*/
