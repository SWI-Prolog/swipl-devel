/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_WHILE_H
#define _PCE_WHILE_H

extern Any ClassWhile;
class PceWhile :public PceObject
{
public:
  PceWhile(PceArg condition, PceArg statement) :
    PceObject(ClassWhile, condition, statement)
  {
  }
};

inline PceWhile
AsWhile(PceArg a)
{ return *((PceWhile*) &a);
}

#endif /*!_PCE_WHILE_H*/
