/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_CHAIN_H
#define _PCE_CHAIN_H

extern "C" {
Any	XPCE_chain_head(Any);
Any	XPCE_next_cell(Any);
Any	XPCE_cell_value(Any);
}

PceExternalClass(ClassChain);

class PceCell
{ Any self;

public:
  PceCell()
  {
  }

  PceCell(Any cell)
  { self = cell;
  }

  PceArg value()
  { return PceArg(XPCE_cell_value(self));
  }

  operator void *()
  { return self;			/* for if ( cell ) */
  }

  PceCell& operator=(const PceCell&q)	/* x = y */
  { self = q.self;
    return *this;
  }
  PceCell operator ++()			/* ++cell */
  { Any old = self;
    self = XPCE_next_cell(self);
    return PceCell(old);
  }
  PceCell& operator ++(int)		/* cell++ */
  { self = XPCE_next_cell(self);
    return *this;
  }
};


class PceChain :public PceObject
{
public:
  PceChain() :
    PceObject(ClassChain)
  {
  }
  PceChain(PceArg a) :
    PceObject(ClassChain, a)
  {
  }
  PceChain(PceArg a, PceArg a2) :
    PceObject(ClassChain, a, a2)
  {
  }
  PceChain(PceArg a, PceArg a2, PceArg a3) :
    PceObject(ClassChain, a, a2, a3)
  {
  }
  PceChain(PceArg a, PceArg a2, PceArg a3, PceArg a4) :
    PceObject(ClassChain, a, a2, a3, a4)
  {
  }
  PceChain(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5) :
    PceObject(ClassChain, a, a2, a3, a4, a5)
  {
  }
  PceChain(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6) :
    PceObject(ClassChain, a, a2, a3, a4, a5, a6)
  {
  }
  PceChain(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6,
	   PceArg a7) :
    PceObject(ClassChain, a, a2, a3, a4, a5, a6, a7)
  {
  }
  PceChain(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6,
	   PceArg a7, PceArg a8) :
    PceObject(ClassChain, a, a2, a3, a4, a5, a6, a7, a8)
  {
  }
  PceChain(PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6,
	   PceArg a7, PceArg a8, PceArg a9) :
    PceObject(ClassChain, a, a2, a3, a4, a5, a6, a7, a8, a9)
  {
  }

  PceCell head()
  { return PceCell(XPCE_chain_head(self));
  }
};


inline PceChain
AsChain(PceArg a)
{ return *((PceChain*) &a);
}

#endif /*!_PCE_CHAIN_H*/
