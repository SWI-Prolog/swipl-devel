/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_PIXMAP_H
#define _PCE_PIXMAP_H

PceExternalClass(ClassPixmap);
class PcePixmap :public PceObject
{
public:
  PcePixmap() :
    PceObject(ClassPixmap)
  {
  }
  PcePixmap(PceArg source) :
    PceObject(ClassPixmap, source)
  {
  }
  PcePixmap(PceArg source, PceArg foreground) :
    PceObject(ClassPixmap, source, foreground)
  {
  }
  PcePixmap(PceArg source, PceArg foreground, PceArg background) :
    PceObject(ClassPixmap, source, foreground, background)
  {
  }
  PcePixmap(PceArg source, PceArg foreground, PceArg background, PceArg width) :
    PceObject(ClassPixmap, source, foreground, background, width)
  {
  }
  PcePixmap(PceArg source, PceArg foreground, PceArg background, PceArg width, PceArg height) :
    PceObject(ClassPixmap, source, foreground, background, width, height)
  {
  }
};

inline PcePixmap
AsPixmap(PceArg a)
{ return *((PcePixmap*) &a);
}

#endif /*!_PCE_PIXMAP_H*/
