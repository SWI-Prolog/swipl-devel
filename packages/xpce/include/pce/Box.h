/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_BOX_H
#define _PCE_BOX_H

extern Any ClassBox;
class PceBox :public PceObject
{
public:
  PceBox() :
    PceObject(ClassBox)
  {
  }
  PceBox(PceArg width) :
    PceObject(ClassBox, width)
  {
  }
  PceBox(PceArg width, PceArg height) :
    PceObject(ClassBox, width, height)
  {
  }
};

inline PceBox
AsBox(PceArg a)
{ return *((PceBox*) &a);
}

#endif /*!_PCE_BOX_H*/
