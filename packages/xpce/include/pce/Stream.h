/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_STREAM_H
#define _PCE_STREAM_H

PceExternalClass(ClassStream);
class PceStream :public PceObject
{
public:
  PceStream() :
    PceObject(ClassStream)
  {
  }
  PceStream(PceArg rfd) :
    PceObject(ClassStream, rfd)
  {
  }
  PceStream(PceArg rfd, PceArg wfd) :
    PceObject(ClassStream, rfd, wfd)
  {
  }
  PceStream(PceArg rfd, PceArg wfd, PceArg input_message) :
    PceObject(ClassStream, rfd, wfd, input_message)
  {
  }
  PceStream(PceArg rfd, PceArg wfd, PceArg input_message, PceArg record_separator) :
    PceObject(ClassStream, rfd, wfd, input_message, record_separator)
  {
  }
};

inline PceStream
AsStream(PceArg a)
{ return *((PceStream*) &a);
}

#endif /*!_PCE_STREAM_H*/
