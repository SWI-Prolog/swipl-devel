/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_CHAINTABLE_H
#define _PCE_CHAINTABLE_H

PceExternalClass(ClassChainTable);
class PceChainTable :public PceObject
{
public:
  PceChainTable() :
    PceObject(ClassChainTable)
  {
  }
  PceChainTable(PceArg buckets) :
    PceObject(ClassChainTable, buckets)
  {
  }
};

inline PceChainTable
AsChainTable(PceArg a)
{ return *((PceChainTable*) &a);
}

#endif /*!_PCE_CHAINTABLE_H*/
