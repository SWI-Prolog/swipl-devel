/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_BOOL_H
#define _PCE_BOOL_H

PceExternalClass(ClassBool);
class PceBool :public PceObject
{
public:
  PceBool() :
    PceObject(ClassBool)
  {
  }
};

inline PceBool
AsBool(PceArg a)
{ return *((PceBool*) &a);
}

#endif /*!_PCE_BOOL_H*/
