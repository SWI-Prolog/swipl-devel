/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_CLICKGESTURE_H
#define _PCE_CLICKGESTURE_H

extern Any ClassClickGesture;
class PceClickGesture :public PceObject
{
public:
  PceClickGesture() :
    PceObject(ClassClickGesture)
  {
  }
  PceClickGesture(PceArg button) :
    PceObject(ClassClickGesture, button)
  {
  }
  PceClickGesture(PceArg button, PceArg modifier) :
    PceObject(ClassClickGesture, button, modifier)
  {
  }
  PceClickGesture(PceArg button, PceArg modifier, PceArg multiple) :
    PceObject(ClassClickGesture, button, modifier, multiple)
  {
  }
  PceClickGesture(PceArg button, PceArg modifier, PceArg multiple, PceArg message) :
    PceObject(ClassClickGesture, button, modifier, multiple, message)
  {
  }
  PceClickGesture(PceArg button, PceArg modifier, PceArg multiple, PceArg message, PceArg preview) :
    PceObject(ClassClickGesture, button, modifier, multiple, message, preview)
  {
  }
  PceClickGesture(PceArg button, PceArg modifier, PceArg multiple, PceArg message, PceArg preview, PceArg cancel) :
    PceObject(ClassClickGesture, button, modifier, multiple, message, preview, cancel)
  {
  }
};

inline PceClickGesture
AsClickGesture(PceArg a)
{ return *((PceClickGesture*) &a);
}

#endif /*!_PCE_CLICKGESTURE_H*/
