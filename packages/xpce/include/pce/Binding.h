/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_BINDING_H
#define _PCE_BINDING_H

extern Any ClassBinding;
class PceBinding :public PceObject
{
public:
  PceBinding(PceArg name) :
    PceObject(ClassBinding, name)
  {
  }
  PceBinding(PceArg name, PceArg value) :
    PceObject(ClassBinding, name, value)
  {
  }
};

inline PceBinding
AsBinding(PceArg a)
{ return *((PceBinding*) &a);
}

#endif /*!_PCE_BINDING_H*/
