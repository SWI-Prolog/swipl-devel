/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_KEYBINDING_H
#define _PCE_KEYBINDING_H

PceExternalClass(ClassKeyBinding);
class PceKeyBinding :public PceObject
{
public:
  PceKeyBinding() :
    PceObject(ClassKeyBinding)
  {
  }
  PceKeyBinding(PceArg name) :
    PceObject(ClassKeyBinding, name)
  {
  }
  PceKeyBinding(PceArg name, PceArg super) :
    PceObject(ClassKeyBinding, name, super)
  {
  }
  PceKeyBinding(PceArg name, PceArg super, PceArg super2) :
    PceObject(ClassKeyBinding, name, super, super2)
  {
  }
  PceKeyBinding(PceArg name, PceArg super, PceArg super2, PceArg super3) :
    PceObject(ClassKeyBinding, name, super, super2, super3)
  {
  }
  PceKeyBinding(PceArg name, PceArg super, PceArg super2, PceArg super3, PceArg super4) :
    PceObject(ClassKeyBinding, name, super, super2, super3, super4)
  {
  }
  PceKeyBinding(PceArg name, PceArg super, PceArg super2, PceArg super3, PceArg super4, PceArg super5) :
    PceObject(ClassKeyBinding, name, super, super2, super3, super4, super5)
  {
  }
  PceKeyBinding(PceArg name, PceArg super, PceArg super2, PceArg super3, PceArg super4, PceArg super5, PceArg super6) :
    PceObject(ClassKeyBinding, name, super, super2, super3, super4, super5, super6)
  {
  }
  PceKeyBinding(PceArg name, PceArg super, PceArg super2, PceArg super3, PceArg super4, PceArg super5, PceArg super6, PceArg super7) :
    PceObject(ClassKeyBinding, name, super, super2, super3, super4, super5, super6, super7)
  {
  }
  PceKeyBinding(PceArg name, PceArg super, PceArg super2, PceArg super3, PceArg super4, PceArg super5, PceArg super6, PceArg super7, PceArg super8) :
    PceObject(ClassKeyBinding, name, super, super2, super3, super4, super5, super6, super7, super8)
  {
  }
};

inline PceKeyBinding
AsKeyBinding(PceArg a)
{ return *((PceKeyBinding*) &a);
}

#endif /*!_PCE_KEYBINDING_H*/
