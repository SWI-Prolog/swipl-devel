/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_STYLE_H
#define _PCE_STYLE_H

extern Any ClassStyle;
class PceStyle :public PceObject
{
public:
  PceStyle() :
    PceObject(ClassStyle)
  {
  }
  PceStyle(PceArg icon) :
    PceObject(ClassStyle, icon)
  {
  }
  PceStyle(PceArg icon, PceArg font) :
    PceObject(ClassStyle, icon, font)
  {
  }
  PceStyle(PceArg icon, PceArg font, PceArg colour) :
    PceObject(ClassStyle, icon, font, colour)
  {
  }
  PceStyle(PceArg icon, PceArg font, PceArg colour, PceArg highlight) :
    PceObject(ClassStyle, icon, font, colour, highlight)
  {
  }
  PceStyle(PceArg icon, PceArg font, PceArg colour, PceArg highlight, PceArg underline) :
    PceObject(ClassStyle, icon, font, colour, highlight, underline)
  {
  }
  PceStyle(PceArg icon, PceArg font, PceArg colour, PceArg highlight, PceArg underline, PceArg bold) :
    PceObject(ClassStyle, icon, font, colour, highlight, underline, bold)
  {
  }
  PceStyle(PceArg icon, PceArg font, PceArg colour, PceArg highlight, PceArg underline, PceArg bold, PceArg grey) :
    PceObject(ClassStyle, icon, font, colour, highlight, underline, bold, grey)
  {
  }
  PceStyle(PceArg icon, PceArg font, PceArg colour, PceArg highlight, PceArg underline, PceArg bold, PceArg grey, PceArg background) :
    PceObject(ClassStyle, icon, font, colour, highlight, underline, bold, grey, background)
  {
  }
  PceStyle(PceArg icon, PceArg font, PceArg colour, PceArg highlight, PceArg underline, PceArg bold, PceArg grey, PceArg background, PceArg hidden) :
    PceObject(ClassStyle, icon, font, colour, highlight, underline, bold, grey, background, hidden)
  {
  }
  PceStyle(PceArg icon, PceArg font, PceArg colour, PceArg highlight, PceArg underline, PceArg bold, PceArg grey, PceArg background, PceArg hidden, PceArg left_margin) :
    PceObject(ClassStyle, icon, font, colour, highlight, underline, bold, grey, background, hidden, left_margin)
  {
  }
  PceStyle(PceArg icon, PceArg font, PceArg colour, PceArg highlight, PceArg underline, PceArg bold, PceArg grey, PceArg background, PceArg hidden, PceArg left_margin, PceArg right_margin) :
    PceObject(ClassStyle, icon, font, colour, highlight, underline, bold, grey, background, hidden, left_margin, right_margin)
  {
  }
};

inline PceStyle
AsStyle(PceArg a)
{ return *((PceStyle*) &a);
}

#endif /*!_PCE_STYLE_H*/
