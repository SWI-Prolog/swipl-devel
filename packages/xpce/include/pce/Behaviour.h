/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_BEHAVIOUR_H
#define _PCE_BEHAVIOUR_H

PceExternalClass(ClassBehaviour);
class PceBehaviour :public PceObject
{
public:
  PceBehaviour(PceArg name) :
    PceObject(ClassBehaviour, name)
  {
  }
  PceBehaviour(PceArg name, PceArg context) :
    PceObject(ClassBehaviour, name, context)
  {
  }
};

inline PceBehaviour
AsBehaviour(PceArg a)
{ return *((PceBehaviour*) &a);
}

#endif /*!_PCE_BEHAVIOUR_H*/
