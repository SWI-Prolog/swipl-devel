/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_PARSER_H
#define _PCE_PARSER_H

extern Any ClassParser;
class PceParser :public PceObject
{
public:
  PceParser(PceArg tokeniser) :
    PceObject(ClassParser, tokeniser)
  {
  }
};

inline PceParser
AsParser(PceArg a)
{ return *((PceParser*) &a);
}

#endif /*!_PCE_PARSER_H*/
