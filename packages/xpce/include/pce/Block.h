/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_BLOCK_H
#define _PCE_BLOCK_H

extern Any ClassBlock;
class PceBlock :public PceObject
{
public:
  PceBlock() :
    PceObject(ClassBlock)
  {
  }
  PceBlock(PceArg test) :
    PceObject(ClassBlock, test)
  {
  }
  PceBlock(PceArg test, PceArg test2) :
    PceObject(ClassBlock, test, test2)
  {
  }
  PceBlock(PceArg test, PceArg test2, PceArg test3) :
    PceObject(ClassBlock, test, test2, test3)
  {
  }
  PceBlock(PceArg test, PceArg test2, PceArg test3, PceArg test4) :
    PceObject(ClassBlock, test, test2, test3, test4)
  {
  }
  PceBlock(PceArg test, PceArg test2, PceArg test3, PceArg test4, PceArg test5) :
    PceObject(ClassBlock, test, test2, test3, test4, test5)
  {
  }
  PceBlock(PceArg test, PceArg test2, PceArg test3, PceArg test4, PceArg test5, PceArg test6) :
    PceObject(ClassBlock, test, test2, test3, test4, test5, test6)
  {
  }
  PceBlock(PceArg test, PceArg test2, PceArg test3, PceArg test4, PceArg test5, PceArg test6, PceArg test7) :
    PceObject(ClassBlock, test, test2, test3, test4, test5, test6, test7)
  {
  }
  PceBlock(PceArg test, PceArg test2, PceArg test3, PceArg test4, PceArg test5, PceArg test6, PceArg test7, PceArg test8) :
    PceObject(ClassBlock, test, test2, test3, test4, test5, test6, test7, test8)
  {
  }
  PceBlock(PceArg test, PceArg test2, PceArg test3, PceArg test4, PceArg test5, PceArg test6, PceArg test7, PceArg test8, PceArg test9) :
    PceObject(ClassBlock, test, test2, test3, test4, test5, test6, test7, test8, test9)
  {
  }
};

inline PceBlock
AsBlock(PceArg a)
{ return *((PceBlock*) &a);
}

#endif /*!_PCE_BLOCK_H*/
