/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_BINARYCONDITION_H
#define _PCE_BINARYCONDITION_H

extern Any ClassBinaryCondition;
class PceBinaryCondition :public PceObject
{
public:
  PceBinaryCondition(PceArg left, PceArg right) :
    PceObject(ClassBinaryCondition, left, right)
  {
  }
};

inline PceBinaryCondition
AsBinaryCondition(PceArg a)
{ return *((PceBinaryCondition*) &a);
}

#endif /*!_PCE_BINARYCONDITION_H*/
