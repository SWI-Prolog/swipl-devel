/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_TEXT_H
#define _PCE_TEXT_H

extern Any ClassText;
class PceText :public PceObject
{
public:
  PceText() :
    PceObject(ClassText)
  {
  }
  PceText(PceArg string) :
    PceObject(ClassText, string)
  {
  }
  PceText(PceArg string, PceArg format) :
    PceObject(ClassText, string, format)
  {
  }
  PceText(PceArg string, PceArg format, PceArg font) :
    PceObject(ClassText, string, format, font)
  {
  }
};

inline PceText
AsText(PceArg a)
{ return *((PceText*) &a);
}

#endif /*!_PCE_TEXT_H*/
