/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_CURSOR_H
#define _PCE_CURSOR_H

extern Any ClassCursor;
class PceCursor :public PceObject
{
public:
  PceCursor(PceArg name) :
    PceObject(ClassCursor, name)
  {
  }
  PceCursor(PceArg name, PceArg image) :
    PceObject(ClassCursor, name, image)
  {
  }
  PceCursor(PceArg name, PceArg image, PceArg mask) :
    PceObject(ClassCursor, name, image, mask)
  {
  }
  PceCursor(PceArg name, PceArg image, PceArg mask, PceArg hot_spot) :
    PceObject(ClassCursor, name, image, mask, hot_spot)
  {
  }
  PceCursor(PceArg name, PceArg image, PceArg mask, PceArg hot_spot, PceArg foreground) :
    PceObject(ClassCursor, name, image, mask, hot_spot, foreground)
  {
  }
  PceCursor(PceArg name, PceArg image, PceArg mask, PceArg hot_spot, PceArg foreground, PceArg background) :
    PceObject(ClassCursor, name, image, mask, hot_spot, foreground, background)
  {
  }
};

inline PceCursor
AsCursor(PceArg a)
{ return *((PceCursor*) &a);
}

#endif /*!_PCE_CURSOR_H*/
