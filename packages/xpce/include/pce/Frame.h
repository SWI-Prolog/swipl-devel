/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_FRAME_H
#define _PCE_FRAME_H

extern Any ClassFrame;
class PceFrame :public PceObject
{
public:
  PceFrame() :
    PceObject(ClassFrame)
  {
  }
  PceFrame(PceArg label) :
    PceObject(ClassFrame, label)
  {
  }
  PceFrame(PceArg label, PceArg kind) :
    PceObject(ClassFrame, label, kind)
  {
  }
  PceFrame(PceArg label, PceArg kind, PceArg display) :
    PceObject(ClassFrame, label, kind, display)
  {
  }
};

inline PceFrame
AsFrame(PceArg a)
{ return *((PceFrame*) &a);
}

#endif /*!_PCE_FRAME_H*/
