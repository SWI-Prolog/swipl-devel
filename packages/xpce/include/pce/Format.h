/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_FORMAT_H
#define _PCE_FORMAT_H

PceExternalClass(ClassFormat);
class PceFormat :public PceObject
{
public:
  PceFormat() :
    PceObject(ClassFormat)
  {
  }
  PceFormat(PceArg orientation) :
    PceObject(ClassFormat, orientation)
  {
  }
  PceFormat(PceArg orientation, PceArg width) :
    PceObject(ClassFormat, orientation, width)
  {
  }
  PceFormat(PceArg orientation, PceArg width, PceArg columns) :
    PceObject(ClassFormat, orientation, width, columns)
  {
  }
};

inline PceFormat
AsFormat(PceArg a)
{ return *((PceFormat*) &a);
}

#endif /*!_PCE_FORMAT_H*/
