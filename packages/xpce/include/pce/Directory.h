/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_DIRECTORY_H
#define _PCE_DIRECTORY_H

PceExternalClass(ClassDirectory);
class PceDirectory :public PceObject
{
public:
  PceDirectory(PceArg path) :
    PceObject(ClassDirectory, path)
  {
  }
};

inline PceDirectory
AsDirectory(PceArg a)
{ return *((PceDirectory*) &a);
}

#endif /*!_PCE_DIRECTORY_H*/
