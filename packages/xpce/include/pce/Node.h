/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_NODE_H
#define _PCE_NODE_H

extern Any ClassNode;
class PceNode :public PceObject
{
public:
  PceNode(PceArg image) :
    PceObject(ClassNode, image)
  {
  }
};

inline PceNode
AsNode(PceArg a)
{ return *((PceNode*) &a);
}

#endif /*!_PCE_NODE_H*/
