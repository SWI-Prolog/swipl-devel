/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_TUPLE_H
#define _PCE_TUPLE_H

extern Any ClassTuple;
class PceTuple :public PceObject
{
public:
  PceTuple() :
    PceObject(ClassTuple)
  {
  }
  PceTuple(PceArg first) :
    PceObject(ClassTuple, first)
  {
  }
  PceTuple(PceArg first, PceArg second) :
    PceObject(ClassTuple, first, second)
  {
  }
};

inline PceTuple
AsTuple(PceArg a)
{ return *((PceTuple*) &a);
}

#endif /*!_PCE_TUPLE_H*/
