/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_DIALOG_H
#define _PCE_DIALOG_H

extern Any ClassDialog;
class PceDialog :public PceObject
{
public:
  PceDialog() :
    PceObject(ClassDialog)
  {
  }
  PceDialog(PceArg label) :
    PceObject(ClassDialog, label)
  {
  }
  PceDialog(PceArg label, PceArg size) :
    PceObject(ClassDialog, label, size)
  {
  }
  PceDialog(PceArg label, PceArg size, PceArg display) :
    PceObject(ClassDialog, label, size, display)
  {
  }
};

inline PceDialog
AsDialog(PceArg a)
{ return *((PceDialog*) &a);
}

#endif /*!_PCE_DIALOG_H*/
