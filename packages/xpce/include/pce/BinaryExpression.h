/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_BINARYEXPRESSION_H
#define _PCE_BINARYEXPRESSION_H

extern Any ClassBinaryExpression;
class PceBinaryExpression :public PceObject
{
public:
  PceBinaryExpression(PceArg left, PceArg right) :
    PceObject(ClassBinaryExpression, left, right)
  {
  }
};

inline PceBinaryExpression
AsBinaryExpression(PceArg a)
{ return *((PceBinaryExpression*) &a);
}

#endif /*!_PCE_BINARYEXPRESSION_H*/
