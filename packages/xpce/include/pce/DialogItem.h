/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_DIALOGITEM_H
#define _PCE_DIALOGITEM_H

extern Any ClassDialogItem;
class PceDialogItem :public PceObject
{
public:
  PceDialogItem(PceArg name) :
    PceObject(ClassDialogItem, name)
  {
  }
};

inline PceDialogItem
AsDialogItem(PceArg a)
{ return *((PceDialogItem*) &a);
}

#endif /*!_PCE_DIALOGITEM_H*/
