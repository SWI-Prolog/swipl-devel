/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_PROGRAMOBJECT_H
#define _PCE_PROGRAMOBJECT_H

extern Any ClassProgramObject;
class PceProgramObject :public PceObject
{
public:
  PceProgramObject() :
    PceObject(ClassProgramObject)
  {
  }
};

inline PceProgramObject
AsProgramObject(PceArg a)
{ return *((PceProgramObject*) &a);
}

#endif /*!_PCE_PROGRAMOBJECT_H*/
