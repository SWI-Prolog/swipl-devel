/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_TEXTCURSOR_H
#define _PCE_TEXTCURSOR_H

extern Any ClassTextCursor;
class PceTextCursor :public PceObject
{
public:
  PceTextCursor() :
    PceObject(ClassTextCursor)
  {
  }
  PceTextCursor(PceArg for) :
    PceObject(ClassTextCursor, for)
  {
  }
};

inline PceTextCursor
AsTextCursor(PceArg a)
{ return *((PceTextCursor*) &a);
}

#endif /*!_PCE_TEXTCURSOR_H*/
