/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_TREE_H
#define _PCE_TREE_H

extern Any ClassTree;
class PceTree :public PceObject
{
public:
  PceTree() :
    PceObject(ClassTree)
  {
  }
  PceTree(PceArg root) :
    PceObject(ClassTree, root)
  {
  }
};

inline PceTree
AsTree(PceArg a)
{ return *((PceTree*) &a);
}

#endif /*!_PCE_TREE_H*/
