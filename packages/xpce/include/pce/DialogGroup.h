/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_DIALOGGROUP_H
#define _PCE_DIALOGGROUP_H

PceExternalClass(ClassDialogGroup);
class PceDialogGroup :public PceObject
{
public:
  PceDialogGroup() :
    PceObject(ClassDialogGroup)
  {
  }
  PceDialogGroup(PceArg name) :
    PceObject(ClassDialogGroup, name)
  {
  }
  PceDialogGroup(PceArg name, PceArg kind) :
    PceObject(ClassDialogGroup, name, kind)
  {
  }
};

inline PceDialogGroup
AsDialogGroup(PceArg a)
{ return *((PceDialogGroup*) &a);
}

#endif /*!_PCE_DIALOGGROUP_H*/
