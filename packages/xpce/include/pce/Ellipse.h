/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_ELLIPSE_H
#define _PCE_ELLIPSE_H

PceExternalClass(ClassEllipse);
class PceEllipse :public PceObject
{
public:
  PceEllipse() :
    PceObject(ClassEllipse)
  {
  }
  PceEllipse(PceArg width) :
    PceObject(ClassEllipse, width)
  {
  }
  PceEllipse(PceArg width, PceArg height) :
    PceObject(ClassEllipse, width, height)
  {
  }
};

inline PceEllipse
AsEllipse(PceArg a)
{ return *((PceEllipse*) &a);
}

#endif /*!_PCE_ELLIPSE_H*/
