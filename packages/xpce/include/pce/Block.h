/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_BLOCK_H
#define _PCE_BLOCK_H

PceExternalClass(ClassBlock);
class PceBlock :public PceObject
{
public:
  PceBlock() :
    PceObject(ClassBlock)
  {
  }
  PceBlock(PceArg a) :
    PceObject(ClassBlock, a)
  {
  }
  PceBlock(PceArg a, PceArg a2) :
    PceObject(ClassBlock, a, a2)
  {
  }
  PceBlock(PceArg a, PceArg a2, PceArg a3) :
    PceObject(ClassBlock, a, a2, a3)
  {
  }
  PceBlock(PceArg a, PceArg a2, PceArg a3, PceArg a4) :
    PceObject(ClassBlock, a, a2, a3, a4)
  {
  }
  PceBlock(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5) :
    PceObject(ClassBlock, a, a2, a3, a4, a5)
  {
  }
  PceBlock(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6) :
    PceObject(ClassBlock, a, a2, a3, a4, a5, a6)
  {
  }
  PceBlock(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6, PceArg a7) :
    PceObject(ClassBlock, a, a2, a3, a4, a5, a6, a7)
  {
  }
  PceBlock(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6, PceArg a7, PceArg a8) :
    PceObject(ClassBlock, a, a2, a3, a4, a5, a6, a7, a8)
  {
  }
  PceBlock(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6, PceArg a7, PceArg a8, PceArg a9) :
    PceObject(ClassBlock, a, a2, a3, a4, a5, a6, a7, a8, a9)
  {
  }
};

inline PceBlock
AsBlock(PceArg a)
{ return *((PceBlock*) &a);
}

#endif /*!_PCE_BLOCK_H*/
