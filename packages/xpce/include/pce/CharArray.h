/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_CHARARRAY_H
#define _PCE_CHARARRAY_H

extern Any ClassCharArray;
class PceCharArray :public PceObject
{
public:
  PceCharArray(PceArg text) :
    PceObject(ClassCharArray, text)
  {
  }
};

inline PceCharArray
AsCharArray(PceArg a)
{ return *((PceCharArray*) &a);
}

#endif /*!_PCE_CHARARRAY_H*/
