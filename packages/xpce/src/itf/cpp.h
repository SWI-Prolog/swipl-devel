/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

extern "C" {
					/* C ---> XPCE */

void *	XPCE_to_string(char *text);
void *	XPCE_to_name(char *text);
void *	XPCE_to_integer(long value);
void *	XPCE_to_real(float value);
void *	XPCE_to_object(void *name);
void *	XPCE_to_class(void *name);

					/* XPCE ---> C */

char *	XPCE_charp_of(void *string);
long	XPCE_int_of(void *integer);
float	XPCE_float_of(void *real);

typedef int		XPCE_status;

					/* VMI */
int	XPCE_sendv(void * receiver, void * selector,
		   int argc, const void * argv[]);
void *	XPCE_getv(void * receiver, void * selector,
		  int argc, const void * argv[]);
void *	XPCE_newv(void * cl, const char *name,
		  int argc, const void * argv[]);

int	XPCE_free(void *);
}

class PceBase;

class PceBase
{
protected:
  void*		self;
};

class P :public PceBase;
class XPCE_Object :public PceBase;

#ifndef NULL				/* TBD */
#define NULL ((void *)0)
#endif

class P :public PceBase
{ 
public:
  
  P(char *text)
  { self = XPCE_to_name(text);
  }
  P(int i)
  { self =  XPCE_to_integer((long) i);
  }
  P(short i)
  { self = XPCE_to_integer((long) i);
  }
  P(long i)
  { self = XPCE_to_integer((long) i);
  }
  P(float f)
  { self = XPCE_to_real(f);
  }
  P(double f)
  { self = XPCE_to_real((float) f);
  }
  P(const PceBase &obj)
  { self = obj.self;
  }
  P(P &p)
  { self = p.self;
  }
  P& operator=(const P&q)
  { self = q.self;
    return *this;
  }
};


class XPCE_Object :public PceBase
{
private:
  XPCE_Object(void *p)
  { self = p;
  }

public:
					/* NEW */
  XPCE_Object(P cl)
    { self = XPCE_newv(cl.self, NULL, 0, NULL);
    }
  XPCE_Object(P cl, P a1)
    { self = XPCE_newv(cl.self, NULL, 1, &a1.self);
    }
  XPCE_Object(P cl, P a1, P a2)
    { void *av[2] = { a1.self, a2.self };
      self = XPCE_newv(cl.self, NULL, 2, av);
    }

  XPCE_status send(P sel)
    { return XPCE_sendv(self, sel.self, 0, NULL);
    }
  XPCE_status send(P sel, P a1)
    { return XPCE_sendv(self, sel.self, 1, &a1.self); 
    }
  XPCE_status send(P sel, P a1, P a2)
    { void * av[2] = { a1.self, a2.self };
      return XPCE_sendv(self, sel.self, 2, av);
    }
  XPCE_status send(P sel, P a1, P a2, P a3)
    { void * av[3] = { a1.self, a2.self, a3.self };
      return XPCE_sendv(self, sel.self, 3, av);
    }
					/* GET */
  XPCE_Object get(P sel)
    { return XPCE_Object(XPCE_getv(self, sel.self, 0, NULL));
    }
  XPCE_Object get(P sel, P a1)
    { return XPCE_getv(self, sel.self, 1, &a1.self);
    }
  XPCE_Object get(P sel, P a1, P a2)
    { void * av[2] = { a1.self, a2.self };
      return XPCE_getv(self, sel.self, 2, av);
    }
  XPCE_Object get(P sel, P a1, P a2, P a3)
    { void * av[3] = { a1.self, a2.self, a3.self };
      return XPCE_getv(self, sel.self, 3, av);
    }
};
