/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_SIZE_H
#define _PCE_SIZE_H

PceExternalClass(ClassSize);
class PceSize :public PceObject
{
public:
  PceSize() :
    PceObject(ClassSize)
  {
  }
  PceSize(PceArg width) :
    PceObject(ClassSize, width)
  {
  }
  PceSize(PceArg width, PceArg height) :
    PceObject(ClassSize, width, height)
  {
  }
};

inline PceSize
AsSize(PceArg a)
{ return *((PceSize*) &a);
}

#endif /*!_PCE_SIZE_H*/
