/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_PICTURE_H
#define _PCE_PICTURE_H

extern Any ClassPicture;
class PcePicture :public PceObject
{
public:
  PcePicture() :
    PceObject(ClassPicture)
  {
  }
  PcePicture(PceArg label) :
    PceObject(ClassPicture, label)
  {
  }
  PcePicture(PceArg label, PceArg size) :
    PceObject(ClassPicture, label, size)
  {
  }
  PcePicture(PceArg label, PceArg size, PceArg display) :
    PceObject(ClassPicture, label, size, display)
  {
  }
};

inline PcePicture
AsPicture(PceArg a)
{ return *((PcePicture*) &a);
}

#endif /*!_PCE_PICTURE_H*/
