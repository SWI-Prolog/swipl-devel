/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_TEXTBUFFER_H
#define _PCE_TEXTBUFFER_H

extern Any ClassTextBuffer;
class PceTextBuffer :public PceObject
{
public:
  PceTextBuffer() :
    PceObject(ClassTextBuffer)
  {
  }
  PceTextBuffer(PceArg contents) :
    PceObject(ClassTextBuffer, contents)
  {
  }
};

inline PceTextBuffer
AsTextBuffer(PceArg a)
{ return *((PceTextBuffer*) &a);
}

#endif /*!_PCE_TEXTBUFFER_H*/
