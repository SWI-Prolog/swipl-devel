/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#define INLINE_UTILITIES 1
#include <h/kernel.h>
#define  PCE_INCLUDED
#include <h/trace.h>
#include <h/interface.h>
#include <h/graphics.h>
#include <h/unix.h>
#include "stub.h"

#if !defined(FD_ZERO) && HAVE_SELECT
#include <sys/select.h>
#endif

		/********************************
		*           C --> PCE		*
		********************************/

Any
cToPceInteger(long int i)
{ if (i < PCE_MIN_INT || i > PCE_MAX_INT)
    return PCE_FAIL;

  return toInt(i);
}


Any
cToPceReal(double f)
{ return CtoReal((float) f);
}


Any
cToPceString(char *assoc, char *s)
{ Any str;
  Any c = CtoScratchCharArray(s);
  str = pceNew(assoc, ClassString, 1, &c);
  doneScratchCharArray(c);

  return str;
}


Any
cToPceName(char *s)
{ return (Any) CtoName(s);
}


Any
cToPcePointer(void *ptr)
{ return (Any) answerObjectv(ClassCPointer, 1, &ptr);
}


void *
pcePointerToC(PceObject obj)
{ if ( instanceOfObject(obj, ClassCPointer) )
  { CPointer ptr = (CPointer)obj;

    return ptr->pointer;
  }

  return PCE_NO_POINTER;
}


Any
cToPceAssoc(char *s)
{ return getObjectFromReferencePce(PCE, CtoName(s));
}


Any
cToPceReference(ulong val)
{ return getObjectFromReferencePce(PCE, toInt(val));
}


int
pceExistsReference(long int ref)
{ Any addr = longToPointer(ref);

  if ( !isProperObject(addr) || isFreedObj(addr) )
    return PCE_FAIL;

  return PCE_SUCCEED;
}


int
pceExistsAssoc(char *assoc)
{ Any addr;

  if ( (addr = getObjectAssoc(CtoName(assoc))) == FAIL )
    return PCE_FAIL;
  if ( !isProperObject(addr) || isFreedObj(addr) )
    return PCE_FAIL;

  return PCE_SUCCEED;
}


PceObject
cToPceTmpCharArray(char *s)
{ return CtoScratchCharArray(s);
}

void
donePceTmpCharArray(Any ca)
{ doneScratchCharArray(ca);
}


		/********************************
		*           TYPE TEST		*
		********************************/

status
pceInstanceOf(Any obj, Any classspec)
{ Class class;

  if ( (class = checkType(classspec, TypeClass, NIL)) )
    return instanceOfObject(obj, class);
    
  errorPce(CtoName(pp(classspec)), NAME_unexpectedType, TypeClass);
  fail;
}


		/********************************
		*           PCE --> C		*
		********************************/

int
pceToCReference(Any obj, PceCValue *rval)
{ assert(isObject(obj));

  if ( onFlag(obj, F_ASSOC) )
  { rval->itf_symbol = getMemberHashTable(ObjectToITFTable, obj);
    return PCE_ASSOC;
  } else
  { rval->integer = valInt(PointerToInt(obj));
    return PCE_REFERENCE;
  }
}


int
pceToC(Any obj, PceCValue *rval)
{ if ( isInteger(obj) )
  { rval->integer = valInt((Int) obj);
    return PCE_INTEGER;
  }

  assert(obj);

  if ( onFlag(obj, F_ASSOC|F_ISNAME|F_ISREAL) )
  { if ( onFlag(obj, F_ASSOC) )
    { rval->itf_symbol = getMemberHashTable(ObjectToITFTable, obj);
      return PCE_ASSOC;
    }
    if ( onFlag(obj, F_ISNAME) )
    { rval->itf_symbol = getITFSymbolName(obj);
      return PCE_NAME;
    }
    { rval->real = ((Real)obj)->value;
      return PCE_REAL;
    }    
  } else
  { rval->integer = PointerToCInt(obj);
    return PCE_REFERENCE;
  }
}


char *
pceStringToC(Any val)
{ if ( instanceOfObject(val, ClassString) )
  { StringObj str = val;

    return strName(str);
  }  

  return NULL;
}


char *
pceCharArrayToC(Any val)
{ if ( instanceOfObject(val, ClassCharArray) )
  { CharArray ca = val;

    return strName(ca);
  }

  return NULL;
}


int
pceObject(Any obj)
{ return isObject(obj) ? PCE_SUCCEED : PCE_FAIL;
}

		/********************************
		*          SYMBOL-TABLE		*
		********************************/

PceITFSymbol
getITFSymbolName(Name name)
{ if ( onFlag(name, F_ITFNAME) )	
    return getMemberHashTable(NameToITFTable, name);
  else
  { PceITFSymbol symbol = newSymbol(NULL, name);

    setFlag(name, F_ITFNAME);
    appendHashTable(NameToITFTable, name, symbol);

    return symbol;
  }
}


PceITFSymbol
pceLookupHandle(int n, hostHandle handle)
{ return getMemberHashTable(HandleToITFTables[n], handle);
}


void
pceRegisterName(int n, hostHandle handle, Name name)
{ PceITFSymbol symbol = getITFSymbolName(name);

  symbol->handle[n] = handle;
  appendHashTable(HandleToITFTables[n], handle, symbol);
}


void
pceRegisterAssoc(int n, hostHandle handle, Any obj)
{ if ( (isObject(obj) && onFlag(obj, F_ASSOC)) )
  { PceITFSymbol symbol = getMemberHashTable(ObjectToITFTable, obj);
    symbol->handle[n] = handle;
    appendHashTable(HandleToITFTables[n], handle, symbol);
  } else
  { PceITFSymbol symbol = newSymbol(obj, NULL);
    symbol->handle[n] = handle;

    if ( isObject(obj) )
      setFlag(obj, F_ASSOC);
    appendHashTable(HandleToITFTables[n], handle, symbol);
    appendHashTable(ObjectToITFTable, obj, symbol);
  }
}


		/********************************
		*  VIRTUAL MACHINE INSTRUCTIONS	*
		********************************/

status
pceSend(Any receiver, Name selector, int argc, Any *argv)
{ status rval;

  Mode(MODE_USER,
       rval = vm_send(receiver, selector, NULL, argc, argv));

  return rval;
}


Any
pceGet(Any receiver, Name selector, int argc, Any *argv)
{ Any rval;

  Mode(MODE_USER,
       rval = vm_get(receiver, selector, NULL, argc, argv));

  return rval;
}


Any
pceNew(char *assoc, Any class, int argc, Any *argv)
{ Any rval;

  Mode(MODE_USER,
       if ( (rval = createObjectv(assoc == NULL ? (Name) NIL : CtoName(assoc),
				  class, argc, argv)) )
         pushAnswerObject(rval));

  return rval;
}


		/********************************
		*       EVENT DISPATCHING	*
		********************************/

#ifndef FD_ZERO
#define FD_ZERO(x)	{(x)->fds_bits[0] = 0;}
#define FD_SET(n, x)	{(x)->fds_bits[0] |= 1<<(n); }
#endif

#ifndef HAVE_SELECT
#include <conio.h>
#endif

int
pceDispatch(int fd, int time)
{ if ( DispatchEvents != NULL )
  { int rval;

    rval = (*DispatchEvents)(fd, time);

    return (rval == SUCCEED ? PCE_DISPATCH_INPUT : PCE_DISPATCH_TIMEOUT);
  } else
  {
#ifndef HAVE_SELECT
    while( !kbhit() )
      ;
    return PCE_DISPATCH_INPUT;
#else
    if ( time > 0 )
    { struct timeval timeout;
      fd_set readfds;

      timeout.tv_sec = time / 1000;
      timeout.tv_usec = (time % 1000) * 1000;

      FD_ZERO(&readfds);
      FD_SET(fd, &readfds);
      if ( select(32, &readfds, NULL, NULL, &timeout) > 0 )
	return PCE_DISPATCH_INPUT;
      else
	return PCE_DISPATCH_TIMEOUT;
    } else
    { fd_set readfds;
      FD_ZERO(&readfds);
      FD_SET(fd, &readfds);
      select(32, &readfds, NULL, NULL, NULL);
      return PCE_DISPATCH_INPUT;
    }
#endif /*HAVE_SELECT*/
  }
}


void
pceRedraw(void)
{ RedrawDisplayManager(TheDisplayManager());
}


		/********************************
		*   HOSTACTION()/HOSTQUERY()	*
		********************************/

char *
getHostSymbolTable(void)
{ PceCValue value;

  if ( hostQuery(HOST_SYMBOLFILE, &value) )
    return value.string;

  return NULL;
}


		/********************************
		*           DEBUGGING		*
		********************************/

void
pceReset(void)
{ resetPce(PCE);
}


void
pceTrace(int on)
{
#ifndef O_RUNTIME
  tracePce(PCE, on ? NAME_user : NAME_never);
#endif
}


void
pceTraceBack(int depth)
{
#ifndef O_RUNTIME
  traceBackPce(toInt(depth), NAME_always);
#endif
}


void
pceWriteCurrentGoal(void)
{
#ifndef O_RUNTIME
  writeGoal(CurrentGoal, NIL);
#endif
}


void
pceWriteErrorGoal(void)
{
#ifndef O_RUNTIME
  Goal g = CurrentGoal;

  while(g && offGFlag(g, G_EXCEPTION))
    g = g->parent;

  if ( g )
    writeGoal(g, NIL);
  else
    writef("\t<No exception goal>\n");
#endif
}

		 /*******************************
		 *	    DLL CALLBACK	*
		 *******************************/

pce_callback_functions TheCallbackFunctions =
{ Stub__HostSend,			/* hostSend() */
  Stub__HostGet,			/* hostGet() */
  Stub__HostCallProc,			/* hostCallProc() */
  Stub__HostCallFunc,			/* hostCallFunc() */
  Stub__HostQuery,			/* hostQuery() */
  Stub__HostActionv,			/* hostActionv() */
  Stub__vCprintf,			/* console IO */
  Stub__Cputchar,
  Stub__Cgetline
};


void
pceRegisterCallbacks(pce_callback_functions *fs)
{ TheCallbackFunctions = *fs;		/* structure copy! */
}


int
hostSend(PceObject host, PceName selector, int argc, PceObject argv[])
{ if ( TheCallbackFunctions.hostSend )
    return (*TheCallbackFunctions.hostSend)(host, selector, argc, argv);

  return FAIL;
}


PceObject
hostGet(PceObject host, PceName selector, int argc, PceObject argv[])
{ if ( TheCallbackFunctions.hostGet )
    return (*TheCallbackFunctions.hostGet)(host, selector, argc, argv);

  return FAIL;
}


int
hostQuery(int what, PceCValue *value)
{ if ( TheCallbackFunctions.hostQuery )
    return (*TheCallbackFunctions.hostQuery)(what, value);

  return FAIL;
}


int
hostAction(int what, ...)
{ if ( TheCallbackFunctions.hostActionv )
  { va_list args;
    int rval;

    va_start(args, what);
    rval = (*TheCallbackFunctions.hostActionv)(what, args);
    va_end(args);
    return rval;
  }

  return FAIL;
}


void
Cprintf(const char *fmt, ...)
{ if ( TheCallbackFunctions.vCprintf )
  { va_list args;

    va_start(args, fmt);
    (*TheCallbackFunctions.vCprintf)(fmt, args);
    va_end(args);
  }
}


void
Cvprintf(const char *fmt, va_list args)
{ if ( TheCallbackFunctions.vCprintf )
  { (*TheCallbackFunctions.vCprintf)(fmt, args);
  }
}


int
Cputchar(int chr)
{ if ( TheCallbackFunctions.Cputchar )
    return (*TheCallbackFunctions.Cputchar)(chr);
  else
  { Cprintf("%c", chr);
    return chr;
  }
}


char *
Cgetline(char *line, int size)
{ if ( TheCallbackFunctions.Cgetline )
    return (*TheCallbackFunctions.Cgetline)(line, size);
  else
  { size = 0;				/* signal end-of-file */
    line[0] = '\0';
    return NULL;
  }
}

