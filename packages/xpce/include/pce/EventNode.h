/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_EVENTNODE_H
#define _PCE_EVENTNODE_H

extern Any ClassEventNode;
class PceEventNode :public PceObject
{
public:
  PceEventNode(PceArg value) :
    PceObject(ClassEventNode, value)
  {
  }
  PceEventNode(PceArg value, PceArg parent) :
    PceObject(ClassEventNode, value, parent)
  {
  }
};

inline PceEventNode
AsEventNode(PceArg a)
{ return *((PceEventNode*) &a);
}

#endif /*!_PCE_EVENTNODE_H*/
