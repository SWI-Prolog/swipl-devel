/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_TABLE_H
#define _PCE_TABLE_H

extern Any ClassTable;
class PceTable :public PceObject
{
public:
  PceTable(PceArg names, PceArg keys) :
    PceObject(ClassTable, names, keys)
  {
  }
};

inline PceTable
AsTable(PceArg a)
{ return *((PceTable*) &a);
}

#endif /*!_PCE_TABLE_H*/
