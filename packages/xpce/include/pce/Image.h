/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_IMAGE_H
#define _PCE_IMAGE_H

extern Any ClassImage;
class PceImage :public PceObject
{
public:
  PceImage() :
    PceObject(ClassImage)
  {
  }
  PceImage(PceArg name) :
    PceObject(ClassImage, name)
  {
  }
  PceImage(PceArg name, PceArg width) :
    PceObject(ClassImage, name, width)
  {
  }
  PceImage(PceArg name, PceArg width, PceArg height) :
    PceObject(ClassImage, name, width, height)
  {
  }
  PceImage(PceArg name, PceArg width, PceArg height, PceArg kind) :
    PceObject(ClassImage, name, width, height, kind)
  {
  }
};

inline PceImage
AsImage(PceArg a)
{ return *((PceImage*) &a);
}

#endif /*!_PCE_IMAGE_H*/
