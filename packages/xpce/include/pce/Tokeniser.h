/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_TOKENISER_H
#define _PCE_TOKENISER_H

extern Any ClassTokeniser;
class PceTokeniser :public PceObject
{
public:
  PceTokeniser() :
    PceObject(ClassTokeniser)
  {
  }
  PceTokeniser(PceArg syntax) :
    PceObject(ClassTokeniser, syntax)
  {
  }
};

inline PceTokeniser
AsTokeniser(PceArg a)
{ return *((PceTokeniser*) &a);
}

#endif /*!_PCE_TOKENISER_H*/
