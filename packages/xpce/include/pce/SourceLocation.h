/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_SOURCELOCATION_H
#define _PCE_SOURCELOCATION_H

extern Any ClassSourceLocation;
class PceSourceLocation :public PceObject
{
public:
  PceSourceLocation(PceArg file) :
    PceObject(ClassSourceLocation, file)
  {
  }
  PceSourceLocation(PceArg file, PceArg line) :
    PceObject(ClassSourceLocation, file, line)
  {
  }
};

inline PceSourceLocation
AsSourceLocation(PceArg a)
{ return *((PceSourceLocation*) &a);
}

#endif /*!_PCE_SOURCELOCATION_H*/
