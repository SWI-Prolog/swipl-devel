/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_LESSEQUAL_H
#define _PCE_LESSEQUAL_H

extern Any ClassLessEqual;
class PceLessEqual :public PceObject
{
public:
  PceLessEqual(PceArg left, PceArg right) :
    PceObject(ClassLessEqual, left, right)
  {
  }
};

inline PceLessEqual
AsLessEqual(PceArg a)
{ return *((PceLessEqual*) &a);
}

#endif /*!_PCE_LESSEQUAL_H*/
