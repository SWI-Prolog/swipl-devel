/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_MOVEOUTLINEGESTURE_H
#define _PCE_MOVEOUTLINEGESTURE_H

extern Any ClassMoveOutlineGesture;
class PceMoveOutlineGesture :public PceObject
{
public:
  PceMoveOutlineGesture() :
    PceObject(ClassMoveOutlineGesture)
  {
  }
  PceMoveOutlineGesture(PceArg button) :
    PceObject(ClassMoveOutlineGesture, button)
  {
  }
  PceMoveOutlineGesture(PceArg button, PceArg modifier) :
    PceObject(ClassMoveOutlineGesture, button, modifier)
  {
  }
};

inline PceMoveOutlineGesture
AsMoveOutlineGesture(PceArg a)
{ return *((PceMoveOutlineGesture*) &a);
}

#endif /*!_PCE_MOVEOUTLINEGESTURE_H*/
