/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_ERROR_H
#define _PCE_ERROR_H

PceExternalClass(ClassError);
class PceError :public PceObject
{
public:
  PceError(PceArg name, PceArg format) :
    PceObject(ClassError, name, format)
  {
  }
  PceError(PceArg name, PceArg format, PceArg kind) :
    PceObject(ClassError, name, format, kind)
  {
  }
  PceError(PceArg name, PceArg format, PceArg kind, PceArg feedback) :
    PceObject(ClassError, name, format, kind, feedback)
  {
  }
};

inline PceError
AsError(PceArg a)
{ return *((PceError*) &a);
}

#endif /*!_PCE_ERROR_H*/
