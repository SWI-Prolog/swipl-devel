/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_OPERATOR_H
#define _PCE_OPERATOR_H

PceExternalClass(ClassOperator);
class PceOperator :public PceObject
{
public:
  PceOperator(PceArg name, PceArg priority, PceArg kind) :
    PceObject(ClassOperator, name, priority, kind)
  {
  }
};

inline PceOperator
AsOperator(PceArg a)
{ return *((PceOperator*) &a);
}

#endif /*!_PCE_OPERATOR_H*/
