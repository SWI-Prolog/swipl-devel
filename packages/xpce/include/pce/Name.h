/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_NAME_H
#define _PCE_NAME_H

extern Any ClassName;
class PceName :public PceObject
{
public:
  PceName(PceArg text) :
    PceObject(ClassName, text)
  {
  }
};

inline PceName
AsName(PceArg a)
{ return *((PceName*) &a);
}

#endif /*!_PCE_NAME_H*/
