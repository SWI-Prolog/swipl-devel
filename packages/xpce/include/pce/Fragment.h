/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_FRAGMENT_H
#define _PCE_FRAGMENT_H

extern Any ClassFragment;
class PceFragment :public PceObject
{
public:
  PceFragment(PceArg text, PceArg start, PceArg length) :
    PceObject(ClassFragment, text, start, length)
  {
  }
  PceFragment(PceArg text, PceArg start, PceArg length, PceArg style) :
    PceObject(ClassFragment, text, start, length, style)
  {
  }
};

inline PceFragment
AsFragment(PceArg a)
{ return *((PceFragment*) &a);
}

#endif /*!_PCE_FRAGMENT_H*/
