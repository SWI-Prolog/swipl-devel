/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_LABEL_H
#define _PCE_LABEL_H

extern Any ClassLabel;
class PceLabel :public PceObject
{
public:
  PceLabel() :
    PceObject(ClassLabel)
  {
  }
  PceLabel(PceArg name) :
    PceObject(ClassLabel, name)
  {
  }
  PceLabel(PceArg name, PceArg selection) :
    PceObject(ClassLabel, name, selection)
  {
  }
  PceLabel(PceArg name, PceArg selection, PceArg font) :
    PceObject(ClassLabel, name, selection, font)
  {
  }
};

inline PceLabel
AsLabel(PceArg a)
{ return *((PceLabel*) &a);
}

#endif /*!_PCE_LABEL_H*/
