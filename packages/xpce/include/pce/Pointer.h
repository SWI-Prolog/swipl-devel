/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_POINTER_H
#define _PCE_POINTER_H

extern "C" {
void *XPCE_to_pointer(void *);
void *XPCE_pointer_of(void *);
}

class PcePointer :public PceArg
{
public:
  PcePointer(void *p)
  { self = XPCE_to_pointer(p);
  }

  operator void *(void)
  { return XPCE_pointer_of(self);
  }
};

#endif /*_PCE_POINTER_H*/
