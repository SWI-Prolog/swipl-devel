/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_TEXTIMAGE_H
#define _PCE_TEXTIMAGE_H

PceExternalClass(ClassTextImage);
class PceTextImage :public PceObject
{
public:
  PceTextImage() :
    PceObject(ClassTextImage)
  {
  }
  PceTextImage(PceArg text) :
    PceObject(ClassTextImage, text)
  {
  }
  PceTextImage(PceArg text, PceArg width) :
    PceObject(ClassTextImage, text, width)
  {
  }
  PceTextImage(PceArg text, PceArg width, PceArg height) :
    PceObject(ClassTextImage, text, width, height)
  {
  }
};

inline PceTextImage
AsTextImage(PceArg a)
{ return *((PceTextImage*) &a);
}

#endif /*!_PCE_TEXTIMAGE_H*/
