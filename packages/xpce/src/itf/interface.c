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
#ifdef HAVE_CONIO_H			/* AIX 4.1 requires this */
#include <conio.h>
#endif
#ifdef HAVE_BSTRING_H
#include <bstring.h>
#endif

		/********************************
		*           C --> PCE		*
		********************************/

Any
cToPceInteger(long int i)
{ Int n = toInt(i);

  if ( valInt(n) != i )
  { errorPce(PCE, NAME_intRange);
    fail;
  }

  return n;
}


Any
cToPceReal(double f)
{ return CtoReal(f);
}


Any
cToPceString(Name assoc, char *s, int translate)
{ Any str;
  Any c = CtoScratchCharArray(s);
  if ( translate )
    str = pceNew(assoc, ClassString, 1, &c);
  else
  { Any av[2];
    
    av[0] = name_procent_s;
    av[1] = c;
    str = pceNew(assoc, ClassString, 2, av);
  }
  doneScratchCharArray(c);

  return str;
}


Any
cToPceName(const char *text)
{ if ( text )
  { string s;

    str_inithdr(&s, ENC_ASCII);
    s.size = strlen(text);
    s.s_text8 = (char8 *)text;

    return StringToName(&s);
  } else
    fail;
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
cToPceAssoc(const char *s)
{ return getObjectFromReferencePce(PCE, CtoName(s));
}


PceObject
pceObjectFromName(PceName name)
{ return findGlobal(name);
}


Any
cToPceReference(ulong val)
{ Instance rval = longToPointer(val);

  if ( rval &&
       validAddress(rval) &&
       (rval->flags & (OBJ_MAGIC_MASK|F_FREED)) == OBJ_MAGIC )
    answer(rval);

  fail;
}


int
pceExistsReference(ulong ref)
{ Any addr = longToPointer(ref);

  if ( !isProperObject(addr) || isFreedObj(addr) )
    return PCE_FAIL;

  return PCE_SUCCEED;
}


char *
pcePPReference(PceObject ref)
{ if ( isInteger(ref) )
  { Any addr = longToPointer(valInt(ref));
    char *rval = pp(addr);

    if ( rval[0] != '@' )
    { char tmp[256];
      sprintf(tmp, "@%ld", valInt(ref));
      return save_string(tmp);
    } else
      return rval;
  } else if ( isName(ref) )
  { Any addr;

    if ( !(addr = getObjectAssoc(ref)) )
    { char tmp[256];

      sprintf(tmp, "@%s", strName(ref));
      return save_string(tmp);
    } else
      return pp(addr);
  } else
    return save_string("invalid reference");
}


int
pceExistsAssoc(PceName assoc)
{ Any addr;

  if ( !(addr = getObjectAssoc(assoc)) )
    return PCE_FAIL;
  if ( !isProperObject(addr) || isFreedObj(addr) )
    return PCE_FAIL;

  return PCE_SUCCEED;
}


PceObject
cToPceTmpCharArray(const char *s)
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
    { rval->real = valReal(obj);
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
pceNew(Name assoc, Any class, int argc, Any *argv)
{ Any rval;

  Mode(MODE_USER,
       if ( (rval = createObjectv(assoc, class, argc, argv)) )
         pushAnswerObject(rval));

  return rval;
}

		 /*******************************
		 *     PROLOG OBJECT SUPPORT	*
		 *******************************/

void *
pceResolveSend(PceObject receiver, PceName selector,
	       int *argc, PceObject **types)
{ SendMethod impl;
  
  if ( (impl = getSendMethodClass(classOfObject(receiver), selector)) &&
       instanceOfObject(impl, ClassSendMethod) &&
       instanceOfObject(impl->message, ClassCPointer) )
  { CPointer ptr = (CPointer)impl->message;
    *types = impl->types->elements;
    *argc  = valInt(impl->types);

    return ptr->pointer;
  }

  return NULL;
}


		/********************************
		*       EVENT DISPATCHING	*
		********************************/

#ifndef FD_ZERO
#define FD_ZERO(x)	{(x)->fds_bits[0] = 0;}
#define FD_SET(n, x)	{(x)->fds_bits[0] |= 1<<(n); }
#endif

#if !defined(HAVE_SELECT) && defined(HAVE_CONIO_H)
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
    ws_dispatch(toInt(fd), toInt(time));
    return PCE_DISPATCH_TIMEOUT;
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
pceRedraw(int sync)
{ if ( sync )
  { static DisplayObj d = NULL;

    if ( !d && !(d = CurrentDisplay(NIL)) )
      return;

    synchroniseDisplay(d);
  } else
  { static DisplayManager dm = NULL;

    if ( !dm && !(dm = getObjectAssoc(NAME_displayManager)) )
      return;

    RedrawDisplayManager(dm);
  }
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pceExecuteMode()
	returns PCE_EXEC_USER is the goal is to be processed in `user'
        space, and PCE_EXEC_SERVICE otherwise.  goals of the latter type
	are not supposed to be visible in the host tracer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
pceExecuteMode(void)
{ return ServiceMode;
}


void
pceReset(void)
{ resetPce(PCE);
}


void
pceTrace(int on)
{
#ifndef O_RUNTIME
 tracePce(PCE, on ? NAME_user : NoTraceMode);
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
  Stub__Cputchar,			/* print single character */
  Stub__Cflush,				/* flush console output */
  Stub__Cgetline,			/* read line from console */
  malloc,				/* malloc */
  realloc,				/* realloc */
  free,					/* free */
  NULL,					/* pad13 */
  NULL,					/* pad14 */
  NULL					/* pad15 */
};


void
pceRegisterCallbacks(pce_callback_functions *fs)
{ void **new = (void **)fs;
  void **old = (void **)&TheCallbackFunctions;
  int i = sizeof(TheCallbackFunctions)/sizeof(void *);

  for( ; i-- > 0; old++, new++)
  { if ( *new )
      *old = *new;
  }
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
    (*TheCallbackFunctions.vCprintf)(fmt, args);
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


void
Cflush()
{ if ( TheCallbackFunctions.Cflush )
    (*TheCallbackFunctions.Cflush)();
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

		 /*******************************
		 *	 MEMORY ALLOCATION	*
		 *******************************/

#undef pceMalloc
#undef pceRealloc
#undef pceFree

void *
pceMalloc(int size)
{ return (*TheCallbackFunctions.malloc)(size);
}


void *
pceRealloc(void *ptr, int size)
{ return (*TheCallbackFunctions.realloc)(ptr, size);
}


void
pceFree(void *ptr)
{ (*TheCallbackFunctions.free)(ptr);
}

		 /*******************************
		 *	 INTERFACE ALLOC	*
		 *******************************/

void *
pceAlloc(int bytes)
{ return alloc(bytes);
}


void
pceUnAlloc(int bytes, void *p)
{ unalloc(bytes, p);
}
