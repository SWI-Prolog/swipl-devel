/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_HASHTABLE_H
#define _PCE_HASHTABLE_H

extern Any ClassHashTable;
class PceHashTable :public PceObject
{
public:
  PceHashTable() :
    PceObject(ClassHashTable)
  {
  }
  PceHashTable(PceArg buckets) :
    PceObject(ClassHashTable, buckets)
  {
  }
};

inline PceHashTable
AsHashTable(PceArg a)
{ return *((PceHashTable*) &a);
}

#endif /*!_PCE_HASHTABLE_H*/
