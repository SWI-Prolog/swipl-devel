/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_PATH_H
#define _PCE_PATH_H

PceExternalClass(ClassPath);
class PcePath :public PceObject
{
public:
  PcePath() :
    PceObject(ClassPath)
  {
  }
  PcePath(PceArg kind) :
    PceObject(ClassPath, kind)
  {
  }
  PcePath(PceArg kind, PceArg intervals) :
    PceObject(ClassPath, kind, intervals)
  {
  }
};

inline PcePath
AsPath(PceArg a)
{ return *((PcePath*) &a);
}

#endif /*!_PCE_PATH_H*/
