/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_EVENTTREE_H
#define _PCE_EVENTTREE_H

extern Any ClassEventTree;
class PceEventTree :public PceObject
{
public:
  PceEventTree() :
    PceObject(ClassEventTree)
  {
  }
  PceEventTree(PceArg root) :
    PceObject(ClassEventTree, root)
  {
  }
};

inline PceEventTree
AsEventTree(PceArg a)
{ return *((PceEventTree*) &a);
}

#endif /*!_PCE_EVENTTREE_H*/
