/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_FONT_H
#define _PCE_FONT_H

extern Any ClassFont;
class PceFont :public PceObject
{
public:
  PceFont(PceArg family, PceArg style, PceArg points) :
    PceObject(ClassFont, family, style, points)
  {
  }
  PceFont(PceArg family, PceArg style, PceArg points, PceArg x_name) :
    PceObject(ClassFont, family, style, points, x_name)
  {
  }
};

inline PceFont
AsFont(PceArg a)
{ return *((PceFont*) &a);
}

#endif /*!_PCE_FONT_H*/
