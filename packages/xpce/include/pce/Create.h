/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_CREATE_H
#define _PCE_CREATE_H

extern Any ClassCreate;
class PceCreate :public PceObject
{
public:
  PceCreate(PceArg cl) :
    PceObject(ClassCreate, cl)
  {
  }
  PceCreate(PceArg cl, PceArg argument) :
    PceObject(ClassCreate, cl, argument)
  {
  }
  PceCreate(PceArg cl, PceArg argument, PceArg argument2) :
    PceObject(ClassCreate, cl, argument, argument2)
  {
  }
  PceCreate(PceArg cl, PceArg argument, PceArg argument2, PceArg argument3) :
    PceObject(ClassCreate, cl, argument, argument2, argument3)
  {
  }
  PceCreate(PceArg cl, PceArg argument, PceArg argument2, PceArg argument3, PceArg argument4) :
    PceObject(ClassCreate, cl, argument, argument2, argument3, argument4)
  {
  }
  PceCreate(PceArg cl, PceArg argument, PceArg argument2, PceArg argument3, PceArg argument4, PceArg argument5) :
    PceObject(ClassCreate, cl, argument, argument2, argument3, argument4, argument5)
  {
  }
  PceCreate(PceArg cl, PceArg argument, PceArg argument2, PceArg argument3, PceArg argument4, PceArg argument5, PceArg argument6) :
    PceObject(ClassCreate, cl, argument, argument2, argument3, argument4, argument5, argument6)
  {
  }
  PceCreate(PceArg cl, PceArg argument, PceArg argument2, PceArg argument3, PceArg argument4, PceArg argument5, PceArg argument6, PceArg argument7) :
    PceObject(ClassCreate, cl, argument, argument2, argument3, argument4, argument5, argument6, argument7)
  {
  }
  PceCreate(PceArg cl, PceArg argument, PceArg argument2, PceArg argument3, PceArg argument4, PceArg argument5, PceArg argument6, PceArg argument7, PceArg argument8) :
    PceObject(ClassCreate, cl, argument, argument2, argument3, argument4, argument5, argument6, argument7, argument8)
  {
  }
};

inline PceCreate
AsCreate(PceArg a)
{ return *((PceCreate*) &a);
}

#endif /*!_PCE_CREATE_H*/
