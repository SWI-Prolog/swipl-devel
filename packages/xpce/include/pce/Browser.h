/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_BROWSER_H
#define _PCE_BROWSER_H

extern Any ClassBrowser;
class PceBrowser :public PceObject
{
public:
  PceBrowser() :
    PceObject(ClassBrowser)
  {
  }
  PceBrowser(PceArg label) :
    PceObject(ClassBrowser, label)
  {
  }
  PceBrowser(PceArg label, PceArg size) :
    PceObject(ClassBrowser, label, size)
  {
  }
  PceBrowser(PceArg label, PceArg size, PceArg display) :
    PceObject(ClassBrowser, label, size, display)
  {
  }
};

inline PceBrowser
AsBrowser(PceArg a)
{ return *((PceBrowser*) &a);
}

#endif /*!_PCE_BROWSER_H*/
