/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>

status
toString(Any obj, String s)
{ static char tmp[25];
  status rval = FAIL;

  if ( instanceOfObject(obj, ClassCharArray) )
  { CharArray ca = obj;

    str_cphdr(s, &ca->data);
    s->s_text = ca->data.s_text;
    succeed;
  } else if ( isInteger(obj) )
  { sprintf(tmp, "%ld", valInt(obj));
    rval = SUCCEED;
  } else if ( instanceOfObject(obj, ClassReal) )
  { sprintf(tmp, "%g", valReal(obj));
    rval = SUCCEED;
  } else if ( instanceOfObject(obj, ClassNumber) )
  { sprintf(tmp, "%ld", valInt(((Number)obj)->value));
    rval = SUCCEED;
  }

  if ( rval )
  { str_set_ascii(s, tmp);
  }

  return rval;
}


char *
toCharp(Any obj)
{ string s;

  if ( toString(obj, &s) )
    return (char *)s.s_text;

  return NULL;
}


Int
toInteger(Any obj)
{ if ( isInteger(obj) )					/* int */
  { return (Int) obj;
  } else if ( instanceOfObject(obj, ClassNumber) )	/* number */
  { return toInt(((Number)obj)->value);
  } else if ( instanceOfObject(obj, ClassReal) )	/* real */
  { return toInt(rfloat(valReal(obj)));
  } else if ( instanceOfObject(obj, ClassCharArray) )	/* char_array */
  { CharArray ca = obj;
    String s = &ca->data;

    if ( isstr8(s) && s->size > 0 )
    { char *end;
      long i;

      i = strtol((char *)s->s_text8, &end, 10);
      if ( end == (char *)&s->s_text8[s->size] )
	return toInt(i);
    }
  }

  fail;
}


Real
toReal(Any obj)
{ if ( instanceOfObject(obj, ClassReal) )
    return obj;
  
  return getConvertReal(ClassReal, obj);
}


Bool
toBool(Any obj)
{ Int i;
  string s;

  if ( isBoolean(obj) )
    return obj;

  if ( (i = checkType(obj, TypeInt, NIL)) )
  { if ( i == ZERO )
      return OFF;
    else if ( i == ONE )
      return ON;
  }
  
  if ( toString(obj, &s) && isstr8(&s) )
  { if       ( streq_ignore_case((char *)s.s_text8, "@on") ||
	       streq_ignore_case((char *)s.s_text8, "true") ||
	       streq_ignore_case((char *)s.s_text8, "yes") ||
	       str_icase_eq(&s, &ON->name->data) )
      return ON;
    else if ( streq_ignore_case((char *)s.s_text8, "@off") ||
	      streq_ignore_case((char *)s.s_text8, "false") ||
	      streq_ignore_case((char *)s.s_text8, "no") ||
	      str_icase_eq(&s, &OFF->name->data) )
      return OFF;
  }

  fail;
}


Name
toName(Any obj)
{ string s;

  if (isName(obj))
    return obj;
  if ( toString(obj, &s) )
    return StringToName(&s);
  fail;
}


Type
toType(Any obj)
{ Name name;

  if ( instanceOfObject(obj, ClassType) )
    return obj;
  if ( (name = toName(obj)) )
    return nameToType(name);

  fail;
}


		/********************************
		*               PP		*
		********************************/

#define PPRINGSIZE 16

static char *ppring[PPRINGSIZE];
static int   ppindex = 0;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Donot use (un)alloc() to faciliate debugging of the allocation routines.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *
ppsavestring(const char *s)
{ char *q = pceMalloc(strlen(s)+1);

  strcpy(q, s);

  if ( ppring[ppindex] )
    pceFree(ppring[ppindex]);
  ppring[ppindex] = q;

  ppindex = (ppindex+1) % PPRINGSIZE;

  return q;
}


/* (JW)	A special routine to ease debugging.  Type checking is done more
	carefully to avoid core-dumps on any 4 byte value.
 */

static char *
do_pp(Any obj)
{ char tmp[LINESIZE];
  char summary[25];
  char *s;

  if ( !obj )
    return ppsavestring("FAIL");

  if ( isInteger(obj) )
  { sprintf(tmp, "%ld", valInt(obj));
    return ppsavestring(tmp);
  }

  if ( isProperObject(obj) )
  { if ( isName(obj))
      return saveStringName((Name) obj);

    if ( instanceOfObject(obj, ClassCharArray) &&
	 isAddress(((CharArray)obj)->data.s_text) )
    { summary[0] = '"';
      strncpy(summary+1, strName(obj), 23);
      summary[24] = '\0';
      strcat(summary, "\"");
      s = summary;
    } else if ( instanceOfObject(obj, ClassType) &&
		isName(((Type)obj)->fullname) )
    { s = strName(((Type)obj)->fullname);
    } else if ( instanceOfObject(obj, ClassReal) )
    { sprintf(summary, "%g", valReal(obj));
      s = summary;
    } else if ( instanceOfObject(obj, ClassNumber) )
    { sprintf(summary, "%ld", ((Number)obj)->value);
      s = summary;
    } else if ( instanceOfObject(obj, ClassHostData) )
    { Any pn = qadGetv(obj, NAME_printName, 0, NULL);
      char *tmp;

      if ( pn && (tmp = toCharp(pn)) )
	return ppsavestring(tmp);
      else
	s = strName(classOfObject(obj)->name);
    } else
      s = strName(classOfObject(obj)->name);

    { Name name;

      if ( (name = getNameAssoc(obj)) )
        sprintf(tmp, "@%s/%s", strName(name), s);
      else
	sprintf(tmp, "@%ld/%s", valInt(PointerToInt(obj)), s);
    }

    if ( isFreedObj(obj) )
      strcat(tmp, " (freed)");
    else if ( isFreeingObj(obj) )
      strcat(tmp, " (unlinking)");

    return ppsavestring(tmp);
  }

  sprintf(tmp, "0x%lx", (unsigned long) obj);
  return ppsavestring(tmp);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The biggest mess of all.  Basically,  the   above  may  yield error when
passed wrong data. The code below catches   these  errors and prints the
value hexadecimal in case of error.

We have structure exception-handling of   Windows, Unix with traditional
signal(), Unix with sigaction (preserves more  context) and systems with
and without SIGBUS ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef __WIN32__

#include <excpt.h>

char *
pcePP(Any obj)
{ char *s;

#ifndef O_RUNTIME
  int old = PCEdebugging;

  PCEdebugging = FALSE;
#endif

  __try
  { s = do_pp(obj);
  } __except(EXCEPTION_EXECUTE_HANDLER)
  { char tmp[100];
    sprintf(tmp, "0x%lx", (unsigned long)obj);
    s = ppsavestring(tmp);
  }

#ifndef O_RUNTIME
  PCEdebugging = old;
#endif

  return s;
}


#else /*__WIN32__*/

#include <setjmp.h>
#include <signal.h>

jmp_buf pp_env;

static RETSIGTYPE
pp_sig(sig)
int sig;
{ longjmp(pp_env, 1);
}

typedef RETSIGTYPE (*handler_t)();

#ifdef HAVE_SIGACTION

typedef struct sigaction sigsave_t;

static void
set_sighandler(int sig, handler_t func, sigsave_t *old)
{ struct sigaction new;

  memset(&new, 0, sizeof(new));	/* deal with other fields */
  new.sa_handler = func;

  sigaction(sig, &new, (struct sigaction *)old);
/*Cprintf("pcePP: handler = %p, flags = 0x%x\n",
	  old->sa_handler, old->sa_flags);
*/
}

static void
restore_handler(int sig, struct sigaction *old)
{ sigaction(sig, old, NULL);
}

#else /*HAVE_SIGACTION*/

typedef handler_t sigsave_t;

static void
set_sighandler(int sig, handler_t func, handler_t *old)
{ old = signal(sig, new);
}

static void
restore_handler(int sig, handler_t *old)
{ signal(sig, *old);
}

#endif /*HAVE_SIGACTION*/

char *
pcePP(Any obj)
{ char *s;
#ifndef O_RUNTIME
  int old = PCEdebugging;
#endif
  sigsave_t oldsegv;
#ifdef SIGBUS
  sigsave_t oldbus;
#endif

  set_sighandler(SIGSEGV, pp_sig, &oldsegv);
#ifdef SIGBUS
  set_sighandler(SIGBUS, pp_sig, &oldbus);
#endif

#ifndef O_RUNTIME
  PCEdebugging = FALSE;
#endif
  if ( setjmp(pp_env) == 0 )
  { s = do_pp(obj);
  } else
  { char tmp[100];
    sprintf(tmp, "0x%lx", (unsigned long)obj);
    s = ppsavestring(tmp);
  }
#ifndef O_RUNTIME
  PCEdebugging = old;
#endif

  restore_handler(SIGSEGV, &oldsegv);
#ifdef SIGBUS
  restore_handler(SIGBUS, &oldbus);
#endif

  return s;
}

#endif /*__WIN32__*/

		/********************************
		*           FUNCTIONS		*
		********************************/

Any
expandFunction(Any obj)
{ while ( isFunction(obj) )
  { Function f = (Function) obj;
    Any rval = getExecuteFunction(f);
    
    if ( rval == FAIL )
    { DEBUG(NAME_obtain, Cprintf("Function: %s\n", pp(f)));
      fail;
    }

    obj = rval;
  }

  return obj;
}
