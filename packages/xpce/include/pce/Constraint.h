/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_CONSTRAINT_H
#define _PCE_CONSTRAINT_H

extern Any ClassConstraint;
class PceConstraint :public PceObject
{
public:
  PceConstraint() :
    PceObject(ClassConstraint)
  {
  }
  PceConstraint(PceArg from) :
    PceObject(ClassConstraint, from)
  {
  }
  PceConstraint(PceArg from, PceArg to) :
    PceObject(ClassConstraint, from, to)
  {
  }
  PceConstraint(PceArg from, PceArg to, PceArg relation) :
    PceObject(ClassConstraint, from, to, relation)
  {
  }
  PceConstraint(PceArg from, PceArg to, PceArg relation, PceArg propagate) :
    PceObject(ClassConstraint, from, to, relation, propagate)
  {
  }
};

inline PceConstraint
AsConstraint(PceArg a)
{ return *((PceConstraint*) &a);
}

#endif /*!_PCE_CONSTRAINT_H*/
