/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#define  PCE_INCLUDED
#include <h/trace.h>
#include <h/interface.h>
#include <h/graphics.h>
#include <h/unix.h>

#if !defined(FD_ZERO) && HAVE_SELECT
#include <sys/select.h>
#endif

#ifdef SOME_MISSING_LIB_PROTOTYPES
extern int select (int width,
		   fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
		   struct timeval *timeout);
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

  assert(isObject(obj));

  if ( onFlag(obj, F_ASSOC) )
  { rval->itf_symbol = getMemberHashTable(ObjectToITFTable, obj);

    return PCE_ASSOC;
  } else
  { Class class = classOfObject(obj);

    if ( isAClass(class, ClassName) )
    { rval->itf_symbol = getITFSymbolName(obj);

      return PCE_NAME;
    } else if ( class == ClassReal )
    { rval->real = ((Real)obj)->value;

      return PCE_REAL;
    } else
    { rval->integer = valInt(PointerToInt(obj));
      return PCE_REFERENCE;
    }
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
		*   hostAction()/hostQuesry()	*
		********************************/

char *
getHostSymbolTable(void)
{ PceCValue value;

  if ( hostQuery(HOST_SYMBOLFILE, &value) )
    return value.string;

  return NULL;
}


int
hostGetc(void)
{ PceCValue value;

  if ( hostQuery(HOST_GETC, &value) )
    return value.character;

  return getchar();
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
{ tracePce(PCE, on ? NAME_user : NAME_never);
}


void
pceTraceBack(int depth)
{ traceBackPce(toInt(depth), NAME_always);
}


void
pceWriteCurrentGoal(void)
{ writeGoal(CurrentGoal, NIL);
}


void
pceWriteErrorGoal(void)
{ Goal g = CurrentGoal;

  while(g && offGFlag(g, G_EXCEPTION))
    g = g->parent;

  if ( g )
    writeGoal(g, NIL);
  else
    writef("\t<No exception goal>\n");
}
