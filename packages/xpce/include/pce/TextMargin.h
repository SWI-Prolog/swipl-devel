/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_TEXTMARGIN_H
#define _PCE_TEXTMARGIN_H

extern Any ClassTextMargin;
class PceTextMargin :public PceObject
{
public:
  PceTextMargin(PceArg editor, PceArg width, PceArg height) :
    PceObject(ClassTextMargin, editor, width, height)
  {
  }
};

inline PceTextMargin
AsTextMargin(PceArg a)
{ return *((PceTextMargin*) &a);
}

#endif /*!_PCE_TEXTMARGIN_H*/
