/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_LINE_H
#define _PCE_LINE_H

PceExternalClass(ClassLine);
class PceLine :public PceObject
{
public:
  PceLine() :
    PceObject(ClassLine)
  {
  }
  PceLine(PceArg start_x) :
    PceObject(ClassLine, start_x)
  {
  }
  PceLine(PceArg start_x, PceArg start_y) :
    PceObject(ClassLine, start_x, start_y)
  {
  }
  PceLine(PceArg start_x, PceArg start_y, PceArg end_x) :
    PceObject(ClassLine, start_x, start_y, end_x)
  {
  }
  PceLine(PceArg start_x, PceArg start_y, PceArg end_x, PceArg end_y) :
    PceObject(ClassLine, start_x, start_y, end_x, end_y)
  {
  }
  PceLine(PceArg start_x, PceArg start_y, PceArg end_x, PceArg end_y, PceArg arrows) :
    PceObject(ClassLine, start_x, start_y, end_x, end_y, arrows)
  {
  }
};

inline PceLine
AsLine(PceArg a)
{ return *((PceLine*) &a);
}

#endif /*!_PCE_LINE_H*/
