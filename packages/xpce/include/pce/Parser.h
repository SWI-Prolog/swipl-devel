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
  PceParser(PceArg tokeniser, PceArg operators) :
    PceObject(ClassParser, tokeniser, operators)
  {
  }
  PceParser(PceArg tokeniser, PceArg operators, PceArg operators2) :
    PceObject(ClassParser, tokeniser, operators, operators2)
  {
  }
  PceParser(PceArg tokeniser, PceArg operators, PceArg operators2, PceArg operators3) :
    PceObject(ClassParser, tokeniser, operators, operators2, operators3)
  {
  }
  PceParser(PceArg tokeniser, PceArg operators, PceArg operators2, PceArg operators3, PceArg operators4) :
    PceObject(ClassParser, tokeniser, operators, operators2, operators3, operators4)
  {
  }
  PceParser(PceArg tokeniser, PceArg operators, PceArg operators2, PceArg operators3, PceArg operators4, PceArg operators5) :
    PceObject(ClassParser, tokeniser, operators, operators2, operators3, operators4, operators5)
  {
  }
  PceParser(PceArg tokeniser, PceArg operators, PceArg operators2, PceArg operators3, PceArg operators4, PceArg operators5, PceArg operators6) :
    PceObject(ClassParser, tokeniser, operators, operators2, operators3, operators4, operators5, operators6)
  {
  }
  PceParser(PceArg tokeniser, PceArg operators, PceArg operators2, PceArg operators3, PceArg operators4, PceArg operators5, PceArg operators6, PceArg operators7) :
    PceObject(ClassParser, tokeniser, operators, operators2, operators3, operators4, operators5, operators6, operators7)
  {
  }
  PceParser(PceArg tokeniser, PceArg operators, PceArg operators2, PceArg operators3, PceArg operators4, PceArg operators5, PceArg operators6, PceArg operators7, PceArg operators8) :
    PceObject(ClassParser, tokeniser, operators, operators2, operators3, operators4, operators5, operators6, operators7, operators8)
  {
  }
};

inline PceParser
AsParser(PceArg a)
{ return *((PceParser*) &a);
}

#endif /*!_PCE_PARSER_H*/
