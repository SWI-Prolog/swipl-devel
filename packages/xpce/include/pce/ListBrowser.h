/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_LISTBROWSER_H
#define _PCE_LISTBROWSER_H

PceExternalClass(ClassListBrowser);
class PceListBrowser :public PceObject
{
public:
  PceListBrowser() :
    PceObject(ClassListBrowser)
  {
  }
  PceListBrowser(PceArg dict) :
    PceObject(ClassListBrowser, dict)
  {
  }
  PceListBrowser(PceArg dict, PceArg width) :
    PceObject(ClassListBrowser, dict, width)
  {
  }
  PceListBrowser(PceArg dict, PceArg width, PceArg height) :
    PceObject(ClassListBrowser, dict, width, height)
  {
  }
};

inline PceListBrowser
AsListBrowser(PceArg a)
{ return *((PceListBrowser*) &a);
}

#endif /*!_PCE_LISTBROWSER_H*/
