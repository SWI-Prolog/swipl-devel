/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_LINK_H
#define _PCE_LINK_H

extern Any ClassLink;
class PceLink :public PceObject
{
public:
  PceLink() :
    PceObject(ClassLink)
  {
  }
  PceLink(PceArg handle_kind1) :
    PceObject(ClassLink, handle_kind1)
  {
  }
  PceLink(PceArg handle_kind1, PceArg handle_kind2) :
    PceObject(ClassLink, handle_kind1, handle_kind2)
  {
  }
  PceLink(PceArg handle_kind1, PceArg handle_kind2, PceArg line) :
    PceObject(ClassLink, handle_kind1, handle_kind2, line)
  {
  }
};

inline PceLink
AsLink(PceArg a)
{ return *((PceLink*) &a);
}

#endif /*!_PCE_LINK_H*/
