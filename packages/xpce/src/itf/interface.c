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

#define INLINE_UTILITIES 1
#include <h/kernel.h>
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
cToPceString(Name assoc, const char *s, unsigned int len, int translate)
{ Any str;
  string ss;
  Any c;

  str_inithdr(&ss, ENC_ASCII);
  ss.size = len;
  ss.s_text8 = (char8 *)s;
  c = StringToScratchCharArray(&ss);

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
cToPceName_n(const char *text, unsigned int len)
{ if ( text )
  { string s;

    str_inithdr(&s, ENC_ASCII);
    s.size = len;
    s.s_text8 = (char8 *)text;

    return StringToName(&s);
  } else
    fail;
}


Any
cToPcePointer(void *ptr)
{ CPointer p = answerObjectv(ClassCPointer, 0, NULL);
  
  p->pointer = ptr;

  return p;
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
cToPceReference(unsigned long val)
{ Instance rval = longToPointer(val);

  if ( rval &&
       validAddress(rval) &&
       (rval->flags & (OBJ_MAGIC_MASK|F_FREED)) == OBJ_MAGIC )
    answer(rval);

  fail;
}


int
pceExistsReference(unsigned long ref)
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

		 /*******************************
		 *		GC		*
		 *******************************/

export void
_markAnswerStack(AnswerMark *mark)
{ *mark = AnswerStack->index;
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


PceClass
nameToExistingClass(PceName Name)
{ return getMemberHashTable(classTable,	Name);
}


PceClass
pceClassOfObject(PceObject obj)
{ if ( isObject(obj) )
    return classOfObject(obj);

  fail;
}


int
pceReferencesOfObject(PceObject obj)
{ if ( isObject(obj) )
    return refsObject(obj);

  return -1;
}


int
pceFreeObject(PceObject obj)
{ if ( isObject(obj) )
    return freeObject(obj);

  fail;
}


void
pceSendMethod(PceClass class,
	      const char *name,
	      const char *group,
	      int argc,
	      ...)
{ Name n, g;
  va_list args;

  va_start(args, argc);

  n = cToPceName(name);
  g = group ? cToPceName(group) : (Name)DEFAULT;
  sendMethodv(class, n, g, argc, args);
  va_end(args);
}


void
pceGetMethod(PceClass class,
	     const char *name,
	     const char *group,
	     const char *rtype,
	     int argc,
	     ...)
{ Name n, g;
  va_list args;

  va_start(args, argc);

  n = cToPceName(name);
  g = group ? cToPceName(group) : (Name)DEFAULT;
  getMethodv(class, n, g, rtype, argc, args);
  va_end(args);
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

  if ( onFlag(obj, F_ASSOC|F_ISNAME|F_ISREAL|F_ISHOSTDATA) )
  { if ( onFlag(obj, F_ASSOC) )
    { rval->itf_symbol = getMemberHashTable(ObjectToITFTable, obj);
      return PCE_ASSOC;
    }
    if ( onFlag(obj, F_ISNAME) )
    { rval->itf_symbol = getITFSymbolName(obj);
      return PCE_NAME;
    }
    if ( onFlag(obj, F_ISHOSTDATA) )
    { rval->pointer = ((HostData)obj)->handle;
      return PCE_HOSTDATA;
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
pceCharArrayToC(Any val, unsigned int *len)
{ if ( instanceOfObject(val, ClassCharArray) )
  { CharArray ca = val;

    if ( *len )
      *len = (unsigned int)str_datasize(&ca->data);

    return strName(ca);
  }

  return NULL;
}


int
pceObject(Any obj)
{ return isObject(obj) ? PCE_SUCCEED : PCE_FAIL;
}

		 /*******************************
		 *	      METHOD		*
		 *******************************/

static void
convert_trace_flags(PceMethod m, int *flags)
{ static struct dflagmap
  { int internal;
    int external;
  } staticmap[] = 
  { { D_TRACE_ENTER, PCE_METHOD_INFO_TRACE_ENTER },
    { D_TRACE_EXIT,  PCE_METHOD_INFO_TRACE_EXIT },  
    { D_TRACE_FAIL,  PCE_METHOD_INFO_TRACE_FAIL },
    { D_BREAK_ENTER, PCE_METHOD_INFO_BREAK_ENTER },
    { D_BREAK_EXIT,  PCE_METHOD_INFO_BREAK_EXIT },  
    { D_BREAK_FAIL,  PCE_METHOD_INFO_BREAK_FAIL },
    { 0, 0 }
  };
  struct dflagmap *map = staticmap;
  
  for( ; map->internal; map++ )
  { if ( onDFlag(m, map->internal) )
      *flags |= map->external;
  }
}


int
pceGetMethodInfo(PceMethod m, pce_method_info *info)
{ if ( onDFlag(m, D_HOSTMETHOD) )
  { CPointer p = (CPointer)m->message;

    info->handle = p->pointer;
    if ( DebuggingProgramObject(m, D_TRACE|D_BREAK) )
      convert_trace_flags(m, &info->flags);
  
    if ( !(m->flags & PCE_METHOD_INFO_HANDLE_ONLY) )
    { info->name    = m->name;
      info->context = ((Class)m->context)->name;
      info->argc    = valInt(m->types->size);
      info->types   = (PceType*)m->types->elements;
    }
  
    succeed;
  }

  fail;
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

Any
pceNew(Name assoc, Any class, int argc, Any *argv)
{ Any rval;

  if ( (rval = createObjectv(assoc, class, argc, argv)) )
    pushAnswerObject(rval);

  return rval;
}


status
pceSend(Any receiver, Name classname, Name selector, int argc, Any *argv)
{ Class cl;

  if ( classname )
  { if ( !(cl = getMemberHashTable(classTable, classname)) )
      return errorPce(receiver, NAME_noClass, classname);
    if ( !instanceOfObject(receiver, cl) )
      return errorPce(receiver, NAME_noSuperClassOf, classname);
  } else
    cl = NULL;

  return vm_send(receiver, selector, cl, argc, argv);
}


Any
pceGet(Any receiver, Name classname, Name selector, int argc, Any *argv)
{ Class cl;

  if ( classname )
  { if ( !(cl = getMemberHashTable(classTable, classname)) )
    { errorPce(receiver, NAME_noClass, classname);
      fail;
    }
    if ( !instanceOfObject(receiver, cl) )
    { errorPce(receiver, NAME_noSuperClassOf, classname);
      fail;
    }
  } else
    cl = NULL;

  return vm_get(receiver, selector, cl, argc, argv);
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
pceWriteCurrentGoal(void)
{
#ifndef O_RUNTIME
  writeGoal(CurrentGoal);
#endif
}


void
pceWriteErrorGoal(void)
{
#ifndef O_RUNTIME
  writeErrorGoal();
#endif
}

		 /*******************************
		 *	    DLL CALLBACK	*
		 *******************************/

pce_callback_functions TheCallbackFunctions =
{ Stub__HostSend,			/* hostSend() */
  Stub__HostGet,			/* hostGet() */
  Stub__HostCall,			/* hostCallProc() */
  Stub__HostQuery,			/* hostQuery() */
  Stub__HostActionv,			/* hostActionv() */
  Stub__vCprintf,			/* console IO */
  Stub__Cputchar,			/* print single character */
  Stub__Cflush,				/* flush console output */
  Stub__Cgetline,			/* read line from console */
  malloc,				/* malloc */
  realloc,				/* realloc */
  free					/* free */
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


		 /*******************************
		 *	    COLLECTIONS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function to help foreign-code enumerating the elements of XPCE chains
and vectors.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
pceEnumElements(PceObject collection,
		int (*enumfunc)(PceObject, void *),
		void *closure)
{ if ( instanceOfObject(collection, ClassChain) )
  { Chain ch = collection;
    PceObject e;

    for_chain(ch, e,
	      if ( !(*enumfunc)(e, closure) )
	        fail;
	     );
    succeed;
  }

  if ( instanceOfObject(collection, ClassVector) )
  { Vector v = collection;
    PceObject e;

    for_vector(v, e,
	       if ( !(*enumfunc)(e, closure) )
	         fail;
	      );
    succeed;
  }

  assert(0);
  fail;
}
