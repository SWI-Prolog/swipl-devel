/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_MODIFIER_H
#define _PCE_MODIFIER_H

PceExternalClass(ClassModifier);
class PceModifier :public PceObject
{
public:
  PceModifier() :
    PceObject(ClassModifier)
  {
  }
  PceModifier(PceArg shift) :
    PceObject(ClassModifier, shift)
  {
  }
  PceModifier(PceArg shift, PceArg control) :
    PceObject(ClassModifier, shift, control)
  {
  }
  PceModifier(PceArg shift, PceArg control, PceArg meta) :
    PceObject(ClassModifier, shift, control, meta)
  {
  }
};

inline PceModifier
AsModifier(PceArg a)
{ return *((PceModifier*) &a);
}

#endif /*!_PCE_MODIFIER_H*/
