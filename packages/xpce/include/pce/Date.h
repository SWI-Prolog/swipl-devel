/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_DATE_H
#define _PCE_DATE_H

extern Any ClassDate;
class PceDate :public PceObject
{
public:
  PceDate() :
    PceObject(ClassDate)
  {
  }
  PceDate(PceArg seconds) :
    PceObject(ClassDate, seconds)
  {
  }
  PceDate(PceArg seconds, PceArg minutes) :
    PceObject(ClassDate, seconds, minutes)
  {
  }
  PceDate(PceArg seconds, PceArg minutes, PceArg hours) :
    PceObject(ClassDate, seconds, minutes, hours)
  {
  }
  PceDate(PceArg seconds, PceArg minutes, PceArg hours, PceArg day) :
    PceObject(ClassDate, seconds, minutes, hours, day)
  {
  }
  PceDate(PceArg seconds, PceArg minutes, PceArg hours, PceArg day, PceArg month) :
    PceObject(ClassDate, seconds, minutes, hours, day, month)
  {
  }
  PceDate(PceArg seconds, PceArg minutes, PceArg hours, PceArg day, PceArg month, PceArg year) :
    PceObject(ClassDate, seconds, minutes, hours, day, month, year)
  {
  }
};

inline PceDate
AsDate(PceArg a)
{ return *((PceDate*) &a);
}

#endif /*!_PCE_DATE_H*/
