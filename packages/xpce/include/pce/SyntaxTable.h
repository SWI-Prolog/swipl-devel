/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_SYNTAXTABLE_H
#define _PCE_SYNTAXTABLE_H

PceExternalClass(ClassSyntaxTable);
class PceSyntaxTable :public PceObject
{
public:
  PceSyntaxTable() :
    PceObject(ClassSyntaxTable)
  {
  }
  PceSyntaxTable(PceArg name) :
    PceObject(ClassSyntaxTable, name)
  {
  }
  PceSyntaxTable(PceArg name, PceArg prototype) :
    PceObject(ClassSyntaxTable, name, prototype)
  {
  }
};

inline PceSyntaxTable
AsSyntaxTable(PceArg a)
{ return *((PceSyntaxTable*) &a);
}

#endif /*!_PCE_SYNTAXTABLE_H*/
