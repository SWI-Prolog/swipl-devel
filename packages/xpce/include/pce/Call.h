/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_CALL_H
#define _PCE_CALL_H

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PceFunc(f) is used to pass arguments to PceMethodCall().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define PceFunc(f) ((Any)(f))

extern "C" {
Any  		XPCE_callCPlusPlusv(Any f, int argc, const Any argv[]);
Any  		XPCE_funcallCPlusPlusv(Any f, int argc, const Any argv[]);
Any 		XPCE_callCPlusPlusMethodv(Any function, Any obj,
					  int argc, const Any argv[]);
Any 		XPCE_funcallCPlusPlusMethodv(Any function, Any obj,
					     int argc, const Any argv[]);
}

typedef PceStatus (*PceProc0)();
typedef PceStatus (*PceProc1)(PceArg);
typedef PceStatus (*PceProc2)(PceArg, PceArg);
typedef PceStatus (*PceProc3)(PceArg, PceArg, PceArg);
typedef PceStatus (*PceProc4)(PceArg, PceArg, PceArg, PceArg);
typedef PceStatus (*PceProc5)(PceArg, PceArg, PceArg, PceArg, PceArg);
typedef PceStatus (*PceProc6)(PceArg, PceArg, PceArg, PceArg, PceArg, PceArg);
typedef PceStatus (*PceProc7)(PceArg, PceArg, PceArg, PceArg, PceArg, PceArg,
			      PceArg);
typedef PceStatus (*PceProc8)(PceArg, PceArg, PceArg, PceArg, PceArg, PceArg,
			      PceArg, PceArg);
typedef PceStatus (*PceProc9)(PceArg, PceArg, PceArg, PceArg, PceArg, PceArg,
			      PceArg, PceArg, PceArg);

typedef PceArg (*PceFunc0)();
typedef PceArg (*PceFunc1)(PceArg);
typedef PceArg (*PceFunc2)(PceArg, PceArg);
typedef PceArg (*PceFunc3)(PceArg, PceArg, PceArg);
typedef PceArg (*PceFunc4)(PceArg, PceArg, PceArg, PceArg);
typedef PceArg (*PceFunc5)(PceArg, PceArg, PceArg, PceArg, PceArg);
typedef PceArg (*PceFunc6)(PceArg, PceArg, PceArg, PceArg, PceArg, PceArg);
typedef PceArg (*PceFunc7)(PceArg, PceArg, PceArg, PceArg, PceArg, PceArg,
			   PceArg);
typedef PceArg (*PceFunc8)(PceArg, PceArg, PceArg, PceArg, PceArg, PceArg,
			   PceArg, PceArg);
typedef PceArg (*PceFunc9)(PceArg, PceArg, PceArg, PceArg, PceArg, PceArg,
			   PceArg, PceArg, PceArg);

typedef PceStatus (*CppMethodProc0)(Any);
typedef PceStatus (*CppMethodProc1)(Any, PceArg);
typedef PceStatus (*CppMethodProc2)(Any, PceArg, PceArg);
typedef PceStatus (*CppMethodProc3)(Any, PceArg, PceArg, PceArg);
typedef PceStatus (*CppMethodProc4)(Any, PceArg, PceArg, PceArg, PceArg);
typedef PceStatus (*CppMethodProc5)(Any, PceArg, PceArg, PceArg, PceArg,
				    PceArg);
typedef PceStatus (*CppMethodProc6)(Any, PceArg, PceArg, PceArg, PceArg,
				    PceArg, PceArg);
typedef PceStatus (*CppMethodProc7)(Any, PceArg, PceArg, PceArg, PceArg,
				    PceArg, PceArg, PceArg);
typedef PceStatus (*CppMethodProc8)(Any, PceArg, PceArg, PceArg, PceArg,
				    PceArg, PceArg, PceArg, PceArg);
typedef PceStatus (*CppMethodProc9)(Any, PceArg, PceArg, PceArg,
				    PceArg, PceArg, PceArg, PceArg, PceArg,
				    PceArg);

typedef PceArg (*CppMethodFunc0)(Any);
typedef PceArg (*CppMethodFunc1)(Any, PceArg);
typedef PceArg (*CppMethodFunc2)(Any, PceArg, PceArg);
typedef PceArg (*CppMethodFunc3)(Any, PceArg, PceArg, PceArg);
typedef PceArg (*CppMethodFunc4)(Any, PceArg, PceArg, PceArg, PceArg);
typedef PceArg (*CppMethodFunc5)(Any, PceArg, PceArg, PceArg,
				 PceArg, PceArg);
typedef PceArg (*CppMethodFunc6)(Any, PceArg, PceArg, PceArg,
				 PceArg, PceArg, PceArg);
typedef PceArg (*CppMethodFunc7)(Any, PceArg, PceArg, PceArg,
				 PceArg, PceArg, PceArg, PceArg);
typedef PceArg (*CppMethodFunc8)(Any, PceArg, PceArg, PceArg,
				 PceArg, PceArg, PceArg, PceArg, PceArg);
typedef PceArg (*CppMethodFunc9)(Any, PceArg, PceArg, PceArg,
				 PceArg, PceArg, PceArg, PceArg, PceArg,
				 PceArg);


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PceCall(f, a1, a2, ...) <==> message(@prolog, f, a1, a2, ...)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

class PceCall :public PceArg
{
public:
  PceCall(PceProc0 f)
  { self = XPCE_callCPlusPlusv((Any)f, 0, NULL);
  }
  PceCall(PceProc1 f, PceArg a1)
  { self = XPCE_callCPlusPlusv((Any)f, 1, &a1.self);
  }
  PceCall(PceProc2 f, PceArg a1, PceArg a2)
  { Any av[2];
    av[0] = a1.self;
    av[1] = a2.self;
    self = XPCE_callCPlusPlusv((Any)f, 2, av);
  }
  PceCall(PceProc3 f, PceArg a1, PceArg a2, PceArg a3)
  { Any av[3];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    self = XPCE_callCPlusPlusv((Any)f, 3, av);
  }
  PceCall(PceProc4 f, PceArg a1, PceArg a2, PceArg a3, PceArg a4)
  { Any av[4];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    self = XPCE_callCPlusPlusv((Any)f, 4, av);
  }
  PceCall(PceProc5 f, PceArg a1, PceArg a2, PceArg a3, PceArg a4, PceArg a5)
  { Any av[5];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    av[4] = a5.self;
    self = XPCE_callCPlusPlusv((Any)f, 5, av);
  }
};


class PceFuncall :public PceArg
{
public:
  PceFuncall(PceFunc0 f)
  { self = XPCE_funcallCPlusPlusv((Any)f, 0, NULL);
  }
  PceFuncall(PceFunc1 f, PceArg a1)
  { self = XPCE_funcallCPlusPlusv((Any)f, 1, &a1.self);
  }
  PceFuncall(PceFunc2 f, PceArg a1, PceArg a2)
  { Any av[2];
    av[0] = a1.self;
    av[1] = a2.self;
    self = XPCE_funcallCPlusPlusv((Any)f, 2, av);
  }
  PceFuncall(PceFunc3 f, PceArg a1, PceArg a2, PceArg a3)
  { Any av[3];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    self = XPCE_funcallCPlusPlusv((Any)f, 3, av);
  }
  PceFuncall(PceFunc4 f, PceArg a1, PceArg a2, PceArg a3, PceArg a4)
  { Any av[4];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    self = XPCE_funcallCPlusPlusv((Any)f, 4, av);
  }
  PceFuncall(PceFunc5 f, PceArg a1, PceArg a2, PceArg a3, PceArg a4, PceArg a5)
  { Any av[5];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    av[4] = a5.self;
    self = XPCE_funcallCPlusPlusv((Any)f, 5, av);
  }
};

		 /*******************************
		 *	 CALLING METHODS	*
		 *******************************/

class PceMethodCall :public PceArg
{
public:
  PceMethodCall(Any obj, Any f)
  { self = XPCE_callCPlusPlusMethodv(f, obj, 0, NULL);
  }
  PceMethodCall(Any obj, Any f, PceArg a1)
  { self = XPCE_callCPlusPlusMethodv(f, obj, 1, &a1.self);
  }
  PceMethodCall(Any obj, Any f, PceArg a1, PceArg a2)
  { Any av[2];
    av[0] = a1.self;
    av[1] = a2.self;
    self = XPCE_callCPlusPlusMethodv(f, obj, 2, av);
  }
  PceMethodCall(Any obj, Any f, PceArg a1, PceArg a2, PceArg a3)
  { Any av[3];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    self = XPCE_callCPlusPlusMethodv(f, obj, 3, av);
  }
  PceMethodCall(Any obj, Any f, PceArg a1, PceArg a2, PceArg a3, PceArg a4)
  { Any av[4];
    av[0] = a1.self;
    av[1] = a2.self;
    av[2] = a3.self;
    av[3] = a4.self;
    self = XPCE_callCPlusPlusMethodv(f, obj, 4, av);
  }
};

#endif /*!_PCE_CALL_H*/
