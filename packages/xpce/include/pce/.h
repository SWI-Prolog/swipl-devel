/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_/_H
#define _PCE_/_H

extern Any ClassDivide;
class PceDivide :public PceObject
{
public:
  PceDivide(PceArg left, PceArg right) :
    PceObject(ClassDivide, left, right)
  {
  }
};

#endif /*!_PCE_/_H*/
