/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_FUNCTION_H
#define _PCE_FUNCTION_H

extern Any ClassFunction;
class PceFunction :public PceObject
{
public:
  PceFunction() :
    PceObject(ClassFunction)
  {
  }
};

inline PceFunction
AsFunction(PceArg a)
{ return *((PceFunction*) &a);
}

#endif /*!_PCE_FUNCTION_H*/
