/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_QUOTEFUNCTION_H
#define _PCE_QUOTEFUNCTION_H

PceExternalClass(ClassQuoteFunction);
class PceQuoteFunction :public PceObject
{
public:
  PceQuoteFunction(PceArg function) :
    PceObject(ClassQuoteFunction, function)
  {
  }
};

inline PceQuoteFunction
AsQuoteFunction(PceArg a)
{ return *((PceQuoteFunction*) &a);
}

#endif /*!_PCE_QUOTEFUNCTION_H*/
