/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_REGEX_H
#define _PCE_REGEX_H

PceExternalClass(ClassRegex);
class PceRegex :public PceObject
{
public:
  PceRegex(PceArg pattern) :
    PceObject(ClassRegex, pattern)
  {
  }
};

inline PceRegex
AsRegex(PceArg a)
{ return *((PceRegex*) &a);
}

#endif /*!_PCE_REGEX_H*/
