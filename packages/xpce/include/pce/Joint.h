/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_JOINT_H
#define _PCE_JOINT_H

extern Any ClassJoint;
class PceJoint :public PceObject
{
public:
  PceJoint() :
    PceObject(ClassJoint)
  {
  }
  PceJoint(PceArg x) :
    PceObject(ClassJoint, x)
  {
  }
  PceJoint(PceArg x, PceArg y) :
    PceObject(ClassJoint, x, y)
  {
  }
  PceJoint(PceArg x, PceArg y, PceArg width) :
    PceObject(ClassJoint, x, y, width)
  {
  }
  PceJoint(PceArg x, PceArg y, PceArg width, PceArg height) :
    PceObject(ClassJoint, x, y, width, height)
  {
  }
  PceJoint(PceArg x, PceArg y, PceArg width, PceArg height, PceArg arrows) :
    PceObject(ClassJoint, x, y, width, height, arrows)
  {
  }
};

inline PceJoint
AsJoint(PceArg a)
{ return *((PceJoint*) &a);
}

#endif /*!_PCE_JOINT_H*/
