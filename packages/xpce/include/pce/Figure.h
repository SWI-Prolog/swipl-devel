/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_FIGURE_H
#define _PCE_FIGURE_H

PceExternalClass(ClassFigure);
class PceFigure :public PceObject
{
public:
  PceFigure() :
    PceObject(ClassFigure)
  {
  }
};

inline PceFigure
AsFigure(PceArg a)
{ return *((PceFigure*) &a);
}

#endif /*!_PCE_FIGURE_H*/
