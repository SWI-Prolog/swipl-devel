/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#include "md.h"
#if O_CPLUSPLUS
#define __GLOBAL
#include <pce/Pce.h>
#include <pce/Call.h>
#include <pce/Class.h>
#include <stdio.h>

extern "C" {
PceStatus callCPlusPlusProc(Any f, int ac, const Any av[]);
Any	  callCPlusPlusFunc(Any f, int ac, const Any av[]);
PceStatus callCPlusPlusPceMethodProc(Any o, Any f, int ac, const Any av[]);
Any 	  callCPlusPlusPceMethodFunc(Any o, Any f, int ac, const Any av[]);
PceStatus callCPlusPlusMethodProc(Any o, Any f, int ac, const Any av[]);
Any 	  callCPlusPlusMethodFunc(Any o, Any f, int ac, const Any av[]);
}


#define A(n) PceArg(av[n])		/* PceArg argument */
#define R(o) PceReceiver(o)		/* PceReceiver argument */
#define C(p) (p)			/* C++ object pointer */

PceStatus
callCPlusPlusProc(Any f, int ac, const Any av[])
{ switch(ac)
  { case 0: return (*(PceProc0)f)();
    case 1: return (*(PceProc1)f)(A(0));
    case 2: return (*(PceProc2)f)(A(0), A(1));
    case 3: return (*(PceProc3)f)(A(0), A(1), A(2));
    case 4: return (*(PceProc4)f)(A(0), A(1), A(2), A(3));
    case 5: return (*(PceProc5)f)(A(0), A(1), A(2), A(3), A(4));
    case 6: return (*(PceProc6)f)(A(0), A(1), A(2), A(3), A(4), A(5));
    case 7: return (*(PceProc7)f)(A(0), A(1), A(2), A(3), A(4), A(5), A(6));
    case 8: return (*(PceProc8)f)(A(0), A(1), A(2), A(3), A(4), A(5), A(6),
				  A(7));
    case 9: return (*(PceProc9)f)(A(0), A(1), A(2), A(3), A(4), A(6), A(7),
				  A(7), A(8));
    default:
      fprintf(stderr, "[PCE: Too many C++ arguments]\n");
      return FALSE;
  }
}


Any 
callCPlusPlusFunc(Any f, int ac, const Any av[])
{ switch(ac)
  { case 0: return (*(PceFunc0)f)();
    case 1: return (*(PceFunc1)f)(A(0));
    case 2: return (*(PceFunc2)f)(A(0), A(1));
    case 3: return (*(PceFunc3)f)(A(0), A(1), A(2));
    case 4: return (*(PceFunc4)f)(A(0), A(1), A(2), A(3));
    case 5: return (*(PceFunc5)f)(A(0), A(1), A(2), A(3), A(4));
    case 6: return (*(PceFunc6)f)(A(0), A(1), A(2), A(3), A(4), A(5));
    case 7: return (*(PceFunc7)f)(A(0), A(1), A(2), A(3), A(4), A(5), A(6));
    case 8: return (*(PceFunc8)f)(A(0), A(1), A(2), A(3), A(4), A(5), A(6),
				  A(7));
    case 9: return (*(PceFunc9)f)(A(0), A(1), A(2), A(3), A(4), A(6), A(7),
				  A(7), A(8));
    default:
      fprintf(stderr, "[PCE: Too many C++ arguments]\n");
      return NULL;
  }
}


PceStatus
callCPlusPlusMethodProc(Any obj, Any f, int ac, const Any av[])
{ switch(ac)
  { case 0:
      return (*(CppMethodProc0)f)(C(obj));
    case 1:
      return (*(CppMethodProc1)f)(C(obj), A(0));
    case 2:
      return (*(CppMethodProc2)f)(C(obj), A(0), A(1));
    case 3:
      return (*(CppMethodProc3)f)(C(obj), A(0), A(1), A(2));
    case 4:
      return (*(CppMethodProc4)f)(C(obj), A(0), A(1), A(2), A(3));
    case 5:
      return (*(CppMethodProc5)f)(C(obj), A(0), A(1), A(2), A(3), A(4));
    case 6:
      return (*(CppMethodProc6)f)(C(obj), A(0), A(1), A(2), A(3), A(4), A(5));
    case 7:
      return (*(CppMethodProc7)f)(C(obj), A(0), A(1), A(2), A(3), A(4), A(5),
				  A(6));
    case 8:
      return (*(CppMethodProc8)f)(C(obj), A(0), A(1), A(2), A(3), A(4), A(5),
				  A(6), A(7));
    case 9:
      return (*(CppMethodProc9)f)(C(obj), A(0), A(1), A(2), A(3), A(4), A(5),
				  A(6), A(7), A(8));
    default:
      fprintf(stderr, "[PCE: Too many C++ arguments]\n");
      return FALSE;
  }
}


Any 
callCPlusPlusMethodFunc(Any obj, Any f, int ac, const Any av[])
{ switch(ac)
  { case 0:
      return (*(CppMethodFunc0)f)(C(obj));
    case 1:
      return (*(CppMethodFunc1)f)(C(obj), A(0));
    case 2:
      return (*(CppMethodFunc2)f)(C(obj), A(0), A(1));
    case 3:
      return (*(CppMethodFunc3)f)(C(obj), A(0), A(1), A(2));
    case 4:
      return (*(CppMethodFunc4)f)(C(obj), A(0), A(1), A(2), A(3));
    case 5:
      return (*(CppMethodFunc5)f)(C(obj), A(0), A(1), A(2), A(3), A(4));
    case 6:
      return (*(CppMethodFunc6)f)(C(obj), A(0), A(1), A(2), A(3), A(4), A(5));
    case 7:
      return (*(CppMethodFunc7)f)(C(obj), A(0), A(1), A(2), A(3), A(4), A(5),
				  A(6));
    case 8:
      return (*(CppMethodFunc8)f)(C(obj), A(0), A(1), A(2), A(3), A(4), A(5),
				  A(6), A(7));
    case 9:
      return (*(CppMethodFunc9)f)(C(obj), A(0), A(1), A(2), A(3), A(4), A(5),
				  A(6), A(7), A(8));
    default:
      fprintf(stderr, "[PCE: Too many C++ arguments]\n");
      return NULL;
  }
}


		 /*******************************
		 *   C++ DEFINED PCE CLASSES	*
		 *******************************/

PceStatus
callCPlusPlusPceMethodProc(Any obj, Any f, int ac, const Any av[])
{ switch(ac)
  { case 0:
      return (*(PceMethodProc0)f)(R(obj));
    case 1:
      return (*(PceMethodProc1)f)(R(obj), A(0));
    case 2:
      return (*(PceMethodProc2)f)(R(obj), A(0), A(1));
    case 3:
      return (*(PceMethodProc3)f)(R(obj), A(0), A(1), A(2));
    case 4:
      return (*(PceMethodProc4)f)(R(obj), A(0), A(1), A(2), A(3));
    case 5:
      return (*(PceMethodProc5)f)(R(obj), A(0), A(1), A(2), A(3), A(4));
    case 6:
      return (*(PceMethodProc6)f)(R(obj), A(0), A(1), A(2), A(3), A(4), A(5));
    case 7:
      return (*(PceMethodProc7)f)(R(obj), A(0), A(1), A(2), A(3), A(4), A(5),
				  A(6));
    case 8:
      return (*(PceMethodProc8)f)(R(obj), A(0), A(1), A(2), A(3), A(4), A(5),
				  A(6), A(7));
    case 9:
      return (*(PceMethodProc9)f)(R(obj), A(0), A(1), A(2), A(3), A(4), A(5),
				  A(6), A(7), A(8));
    default:
      fprintf(stderr, "[PCE: Too many C++ arguments]\n");
      return FALSE;
  }
}


Any 
callCPlusPlusPceMethodFunc(Any obj, Any f, int ac, const Any av[])
{ switch(ac)
  { case 0:
      return (*(PceMethodFunc0)f)(R(obj));
    case 1:
      return (*(PceMethodFunc1)f)(R(obj), A(0));
    case 2:
      return (*(PceMethodFunc2)f)(R(obj), A(0), A(1));
    case 3:
      return (*(PceMethodFunc3)f)(R(obj), A(0), A(1), A(2));
    case 4:
      return (*(PceMethodFunc4)f)(R(obj), A(0), A(1), A(2), A(3));
    case 5:
      return (*(PceMethodFunc5)f)(R(obj), A(0), A(1), A(2), A(3), A(4));
    case 6:
      return (*(PceMethodFunc6)f)(R(obj), A(0), A(1), A(2), A(3), A(4), A(5));
    case 7:
      return (*(PceMethodFunc7)f)(R(obj), A(0), A(1), A(2), A(3), A(4), A(5),
				  A(6));
    case 8:
      return (*(PceMethodFunc8)f)(R(obj), A(0), A(1), A(2), A(3), A(4), A(5),
				  A(6), A(7));
    case 9:
      return (*(PceMethodFunc9)f)(R(obj), A(0), A(1), A(2), A(3), A(4), A(5),
				  A(6), A(7), A(8));
    default:
      fprintf(stderr, "[PCE: Too many C++ arguments]\n");
      return NULL;
  }
}


				/* fix g++ bug; remove if g++ is fixed */
#define PceGlobal(arg) PceGlobal(PceArg(arg))

void
initCPlusPlusGlobals(void)
{ TheOn	      = PceGlobal("on");
  TheOff      = PceGlobal("off");
  TheNil      = PceGlobal("nil");
  TheDefault  = PceGlobal("default");
  TheArg1     = PceGlobal("arg1");
  TheArg2     = PceGlobal("arg2");
  TheArg3     = PceGlobal("arg3");
  TheArg4     = PceGlobal("arg4");
  TheArg5     = PceGlobal("arg5");
  TheArg6     = PceGlobal("arg6");
  TheArg7     = PceGlobal("arg7");
  TheArg8     = PceGlobal("arg8");
  TheArg9     = PceGlobal("arg9");
  TheArg10    = PceGlobal("arg10");
  TheEvent    = PceGlobal("event");
  TheReceiver = PceGlobal("receiver");
  ThePce      = PceGlobal("pce");
  TheDisplay  = PceGlobal("display");
}

#endif /*O_CPLUSPLUS*/
