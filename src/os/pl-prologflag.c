/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2024, University of Amsterdam
			      VU University Amsterdam
			      CWI, Amsterdam
			      SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/*#define O_DEBUG 1*/
#ifdef __WINDOWS__
#include <winsock2.h>
#include <windows.h>
#include <process.h>			/* getpid() */
#endif
#define _GNU_SOURCE			/* get dladdr() */
#include "pl-prologflag.h"
#include "pl-utf8.h"
#include "pl-ctype.h"
#include "pl-funct.h"
#include "../pl-arith.h"
#include "../pl-tabling.h"
#include "../pl-fli.h"
#include "../pl-write.h"
#include "../pl-pro.h"
#include "../pl-wam.h"
#include "../pl-trace.h"
#include "../pl-setup.h"
#include "../pl-modul.h"
#include "../pl-version.h"
#include <ctype.h>
#include <time.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef __WINDOWS__
#include "../pl-nt.h"
#endif
#ifdef HAVE_DLADDR
#include <dlfcn.h>
#endif

#undef false
#undef true
#undef bool
#define true(s, a)         ((s)->flags & (a))
#define false(s, a)        (!true((s), (a)))


		 /*******************************
		 *	PROLOG FLAG HANDLING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ISO Prolog flags are properties of the   running  Prolog system. Some of
these flags can be set  by  the   user,  such  as whether read/1 honours
character-escapes, whether garbage-collection is enabled,  etc. Some are
global and read-only, such as whether the operating system is unix.

In  the  multi-threading  version,  Prolog  flags  have  to  be  changed
thread-local. Therefore two flag-tables have been  defined: a global one
which is used as long as there is only  one thread, and a local one that
is used to write changes to  after   multiple  threads  exist. On thread
creation this table is copied from  the   parent  and on destruction the
local table is destroyed.  Note  that   the  flag-mask  for  fast access
(truePrologFlag(*PLFLAG_)) is always copied to the local thread-data.

Altogether  this  module  is  a  bit  too  complex,  but  I  see  little
alternative. I considered creating  copy-on-write   hash-tables,  but in
combination to the table-enumator  objects  this   proves  very  hard to
implement safely. Using plain Prolog is not  a good option too: they are
used before we can  use  any  Prolog   at  startup,  predicates  are not
thread-local and some of the prolog flags  require very fast access from
C (the booleans in the mask).

Just using a local table and  copy   it  on  thread-creation would be an
option, but 90% of the prolog flags   are read-only or never changed and
we want to be able to have a lot of flags and don't harm thread_create/3
too much.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void setArgvPrologFlag(const char *flag, int argc, char **argv);
static void setTmpDirPrologFlag(void);
static void setTZPrologFlag(void);
static void setVersionPrologFlag(void);
static void initPrologFlagTable(void);

typedef struct oneof
{ size_t	count;
  int		references;
  atom_t       *values;
} oneof;

typedef struct _prolog_flag
{ unsigned short flags;			/* Type | Flags */
  short		index;			/* index in LD->prolog_flag.mask */
  union
  { atom_t	a;			/* value as atom */
    int64_t	i;			/* value as integer */
    double	f;			/* value as float */
    record_t	t;			/* value as term */
  } value;
  oneof        *oneof;
} prolog_flag;

#define unify_prolog_flag_value(m, key, f, val) \
  LDFUNC(unify_prolog_flag_value, m, key, f, val)

static int unify_prolog_flag_value(DECL_LD Module m, atom_t key, prolog_flag *f, term_t val);
static int unify_prolog_flag_type(prolog_flag *f, term_t type);
static int set_flag_type(prolog_flag *f, int flags);

#define FF_WARN_NOT_ACCESSED 0x0100
#define FF_ACCESSED          0x0200

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C-interface for defining Prolog  flags.  Depending   on  the  type,  the
following arguments are to be provided:

    FT_BOOL	TRUE/FALSE, *PLFLAG_
    FT_INTEGER  intptr_t
    FT_INT64    int64_t
    FT_FLOAT	double
    FT_ATOM	const char *
    FT_TERM	a term
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
free_oneof(oneof *of)
{ for(size_t i=0; i<of->count; i++)
    PL_unregister_atom(of->values[i]);
  freeHeap(of->values, of->count * sizeof(*of->values));
  freeHeap(of, sizeof(of));
}

static oneof *
create_oneof(term_t atoms)
{ term_t tail = PL_copy_term_ref(atoms);
  term_t head = PL_new_term_ref();
  tmp_buffer b;

  initBuffer(&b);
  while(PL_get_list(tail, head, tail))
  { atom_t a;

    if ( PL_get_atom_ex(head, &a) )
    { addBuffer(&b, a, atom_t);
    } else
    { error:
      discardBuffer(&b);
      return NULL;
    }
  }
  if ( !PL_get_nil(tail) )
    goto error;

  oneof *of = allocHeapOrHalt(sizeof(*of));
  of->count = entriesBuffer(&b, atom_t);
  of->references = 1;
  of->values = allocHeapOrHalt(of->count*sizeof(atom_t));
  for(size_t i=0; i<of->count; i++)
  { of->values[i] = fetchBuffer(&b, i, atom_t);
    PL_register_atom(of->values[i]);
  }

  return of;
}


static int
put_oneof(term_t t, const oneof *of)
{ term_t h = PL_new_term_ref();

  PL_put_nil(t);
  for(ssize_t i=(ssize_t)of->count-1; i>=0; i--)
  { if ( !PL_put_atom(h, of->values[i]) ||
	 !PL_cons_list(t, h, t) )
      return FALSE;
  }

  return PL_cons_functor(t, FUNCTOR_oneof1, t);
}



static int
check_oneof_flag(const oneof *of, atom_t a, int error)
{ if ( of )
  { for(size_t i=0; i<of->count; i++)
    { if ( of->values[i] == a )
	return TRUE;
    }

    if ( error )
    { term_t dom, ex;

      return ( (dom=PL_new_term_ref()) &&
	       (ex=PL_new_term_ref()) &&
	       put_oneof(dom, of) &&
	       PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
				   PL_FUNCTOR, FUNCTOR_domain_error2,
				     PL_TERM, dom,
				     PL_ATOM, a,
				   PL_VARIABLE) &&
	       PL_raise_exception(ex) );
    } else
      return FALSE;
  }

  return TRUE;
}

/* `flags` should be `unsigned short`, but may not be subject to
 * argument promotion for use with va_start() according to clang
 */

void
setPrologFlag(const char *name, unsigned int flags, ...)
{ GET_LD
  atom_t an = PL_new_atom(name);
  prolog_flag *f;
  va_list args;
  int type = (flags & FT_MASK);
  int first_def = FALSE;

  initPrologFlagTable();

  if ( type == FT_INT64 )
    flags = (flags & ~FT_MASK)|FT_INTEGER;

  if ( (f = lookupHTableWP(GD->prolog_flag.table, an)) )
  { assert((f->flags & FT_MASK) == (flags & FT_MASK));
    if ( flags & FF_KEEP )
      return;
  } else
  { f = allocHeapOrHalt(sizeof(*f));
    f->index = 0;
    f->flags = (unsigned short)flags;
    f->oneof = NULL;
    addNewHTableWP(GD->prolog_flag.table, an, f);
    first_def = TRUE;
  }

  va_start(args, flags);
  switch(type)
  { case FT_BOOL:
    { int           val = va_arg(args, int);
      unsigned int flag = va_arg(args, unsigned int);

      if ( !first_def && flag && !f->index )	/* type definition */
      { f->index = (short)flag;
	val = (f->value.a == ATOM_true);
      } else if ( first_def )			/* 1st definition */
      { f->index = (short)flag;
	DEBUG(MSG_PROLOG_FLAG,
	      Sdprintf("Prolog flag %s at %d\n", name, flag));
      }

      f->value.a = (val ? ATOM_true : ATOM_false);
      if ( f->index )
      { if ( val )
	  setPrologFlagMask(f->index);
	else
	  clearPrologFlagMask(f->index);
      }
      break;
    }
    case FT_INTEGER:
    { intptr_t val = va_arg(args, intptr_t);
      f->value.i = val;
      break;
    }
    case FT_FLOAT:
    { double val = va_arg(args, double);
      f->value.f = val;
      break;
    }
    case FT_INT64:
    { int64_t val = va_arg(args, int64_t);
      f->value.i = val;
      break;
    }
    case FT_ATOM:
    { PL_chars_t text;

      text.text.t    = va_arg(args, char *);
      text.encoding  = ENC_UTF8;
      text.storage   = PL_CHARS_HEAP;
      text.length    = strlen(text.text.t);
      text.canonical = FALSE;

      f->value.a = textToAtom(&text);	/* registered: ok */
      PL_free_text(&text);

      break;
    }
    case FT_TERM:
    { term_t t = va_arg(args, term_t);

      f->value.t = PL_record(t);
      break;
    }
    default:
      assert(0);
  }
  va_end(args);
}


static void
clean_prolog_flag(prolog_flag *f)
{ int type = (f->flags & FT_MASK);
  oneof *of;

  set_flag_type(f, FT_INTEGER);
  switch(type)
  { case FT_TERM:
      PL_erase(f->value.t);
      break;
    case FT_ATOM:
      PL_unregister_atom(f->value.a);
      break;
    default:
      ;
  }
  memset(&f->value, 0, sizeof(f->value));

  if ( (of=f->oneof) && --of->references == 0 )
  { f->oneof = NULL;
    free_oneof(of);
  }
}

#ifdef O_PLMT
static prolog_flag *
copy_prolog_flag(const prolog_flag *f)
{ prolog_flag *copy = allocHeapOrHalt(sizeof(*copy));

  *copy = *f;
  if ( f->oneof )
    f->oneof->references++;
  switch((f->flags & FT_MASK))
  { case FT_TERM:
      copy->value.t = PL_duplicate_record(f->value.t);
      break;
    case FT_ATOM:
      PL_register_atom(copy->value.a);
      break;
    default:
      ;
  }

  return copy;
}


static void
copySymbolPrologFlagTable(table_key_t name, table_value_t *value)
{ atom_t key = (atom_t)name;
  prolog_flag *f = val2ptr(*value);

  PL_register_atom(key);
  *value = ptr2val(copy_prolog_flag(f));
}


static void
freePrologFlag(prolog_flag *f)
{ clean_prolog_flag(f);

  freeHeap(f, sizeof(*f));
}

static void
freeSymbolPrologFlagTable(table_key_t name, table_value_t value)
{ atom_t key = (atom_t)name;

  PL_unregister_atom(key);
  freePrologFlag(val2ptr(value));
}
#endif


int
setDoubleQuotes(atom_t a, unsigned int *flagp)
{ GET_LD
  unsigned int flags;

  if ( a == ATOM_chars )
    flags = DBLQ_CHARS;
  else if ( a == ATOM_codes )
    flags = 0;
  else if ( a == ATOM_atom )
    flags = DBLQ_ATOM;
  else if ( a == ATOM_string )
    flags = DBLQ_STRING;
  else
  { term_t value = PL_new_term_ref();

    PL_put_atom(value, a);
    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    ATOM_double_quotes, value);
  }

  *flagp &= ~DBLQ_MASK;
  *flagp |= flags;

  succeed;
}


int
setBackQuotes(atom_t a, unsigned int *flagp)
{ GET_LD
  unsigned int flags;

  if ( a == ATOM_string )
    flags = BQ_STRING;
  else if ( a == ATOM_symbol_char )
    flags = 0;
  else if ( a == ATOM_codes )
    flags = BQ_CODES;
  else if ( a == ATOM_chars )
    flags = BQ_CHARS;
  else
  { term_t value = PL_new_term_ref();

    PL_put_atom(value, a);
    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    ATOM_back_quotes, value);
  }

  *flagp &= ~BQ_MASK;
  *flagp |= flags;

  succeed;
}


int
setRationalSyntax(atom_t a, unsigned int *flagp)
{ GET_LD
  unsigned int flags;

  if	  ( a == ATOM_natural )
    flags = RAT_NATURAL;
  else if ( a == ATOM_compatibility )
    flags = RAT_COMPAT;
  else
  { term_t value = PL_new_term_ref();

    PL_put_atom(value, a);
    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    ATOM_rational_syntax, value);
  }

  *flagp &= ~RAT_MASK;
  *flagp |= flags;

  succeed;
}



static int
setUnknown(term_t value, atom_t a, Module m)
{ unsigned int flags = m->flags & ~(UNKNOWN_MASK);

  if ( a == ATOM_error )
    flags |= UNKNOWN_ERROR;
  else if ( a == ATOM_warning )
    flags |= UNKNOWN_WARNING;
  else if ( a == ATOM_fail )
    flags |= UNKNOWN_FAIL;
  else
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_unknown, value);

  if ( !(flags&UNKNOWN_ERROR) && (m == MODULE_user || m == MODULE_system) )
  { GET_LD

    if ( m == MODULE_system && !SYSTEM_MODE )
    { term_t key = PL_new_term_ref();

      PL_put_atom(key, ATOM_unknown);
      return PL_error(NULL, 0, NULL, ERR_PERMISSION,
		      ATOM_modify, ATOM_flag, key);
    }

    if ( !SYSTEM_MODE )
    { if ( !printMessage(ATOM_warning, PL_CHARS, "unknown_in_module_user") )
	return FALSE;
    }
  }

  m->flags = flags;

  return TRUE;
}


static int
checkOnError(term_t value, atom_t a, atom_t key)
{ if ( a == ATOM_print || a == ATOM_halt || a == ATOM_status )
    return TRUE;
  return PL_error(NULL, 0, NULL, ERR_DOMAIN, key, value);
}



static int
setFileNameCaseHandling(atom_t a)
{ GET_LD

  if ( a == ATOM_case_sensitive )
  { setPrologFlagMask(PLFLAG_FILE_CASE);
    setPrologFlagMask(PLFLAG_FILE_CASE_PRESERVING);
  } else if ( a == ATOM_case_preserving )
  { setPrologFlagMask(PLFLAG_FILE_CASE_PRESERVING);
    clearPrologFlagMask(PLFLAG_FILE_CASE);
  } else if ( a == ATOM_case_insensitive )
  { clearPrologFlagMask(PLFLAG_FILE_CASE);
    clearPrologFlagMask(PLFLAG_FILE_CASE_PRESERVING);
  } else
  { term_t value = PL_new_term_ref();

    PL_put_atom(value, a);
    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
		    ATOM_file_name_case_handling, value);
  }

  return TRUE;
}


static atom_t
currentFileNameCaseHandling(void)
{ GET_LD

  if ( truePrologFlag(PLFLAG_FILE_CASE) )
  { if ( truePrologFlag(PLFLAG_FILE_CASE_PRESERVING) )
      return ATOM_case_preserving;
    else
      return ATOM_case_insensitive;
  } else
  { return ATOM_case_sensitive;
  }
}


static int
setWriteAttributes(atom_t a)
{ GET_LD
  int mask = writeAttributeMask(a);

  if ( mask )
  { LD->prolog_flag.write_attributes = mask;
    succeed;
  } else
  { term_t value = PL_new_term_ref();

    PL_put_atom(value, a);
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_write_attributes, value);
  }
}


static int
setAccessLevelFromAtom(atom_t a)
{ GET_LD

  if ( getAccessLevelMask(a, &LD->prolog_flag.access_level) )
  { succeed;
  } else
  { term_t value = PL_new_term_ref();

    PL_put_atom(value, a);
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_access_level, value);
  }
}


static int
getOccursCheckMask(atom_t a, occurs_check_t *val)
{ if ( a == ATOM_false )
  { *val = OCCURS_CHECK_FALSE;
  } else if ( a == ATOM_true )
  { *val = OCCURS_CHECK_TRUE;
  } else if ( a == ATOM_error )
  { *val = OCCURS_CHECK_ERROR;
  } else
    fail;

  succeed;
}


static int
setOccursCheck(atom_t a)
{ GET_LD

  if ( getOccursCheckMask(a, &LD->prolog_flag.occurs_check) )
  { updateAlerted(LD);
    succeed;
  } else
  { term_t value = PL_new_term_ref();

    PL_put_atom(value, a);
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_occurs_check, value);
  }
}


static int
setEncoding(atom_t a)
{ GET_LD
  IOENC enc = PL_atom_to_encoding(a);

  if ( enc == ENC_UNKNOWN )
  { term_t value = PL_new_term_ref();

    PL_put_atom(value, a);
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_encoding, value);
  }

  LD->encoding = enc;

  succeed;
}


static int
setStreamTypeCheck(atom_t a)
{ GET_LD
  st_check check;

  if ( a == ATOM_false )
    check = ST_FALSE;
  else if ( a == ATOM_loose )
    check = ST_LOOSE;
  else if ( a == ATOM_true )
    check = ST_TRUE;
  else
  { term_t value = PL_new_term_ref();

    PL_put_atom(value, a);
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_stream_type_check, value);
  }

  LD->IO.stream_type_check = check;
  return TRUE;
}


static int
setAutoload(atom_t a)
{ GET_LD

  if ( a == ATOM_false )
    clearPrologFlagMask(PLFLAG_AUTOLOAD);
  else if ( a == ATOM_explicit ||
	    a == ATOM_true ||
	    a == ATOM_user ||
	    a == ATOM_user_or_explicit )
    setPrologFlagMask(PLFLAG_AUTOLOAD);
  else
  { term_t value = PL_new_term_ref();

    PL_put_atom(value, a);
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_autoload, value);
  }

  return TRUE;
}


#define propagateAutoload(val) LDFUNC(propagateAutoload, val)
static int
propagateAutoload(DECL_LD term_t val)
{ if ( !GD->bootsession )
  { predicate_t pred;
    term_t av;

    pred = PL_predicate("set_autoload", 1, "$autoload");
    return ( (av=PL_new_term_refs(2)) &&
	     PL_put_term(av+0, val) &&
	     PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, pred, av) );
  } else
  { return TRUE;
  }
}


static void
accessed_prolog_flag(prolog_flag *f, atom_t name, int local)
{ if ( true(f, FF_WARN_NOT_ACCESSED) &&
       false(f, FF_ACCESSED) )
  { set(f, FF_ACCESSED);

    if ( local )
    { prolog_flag *fg;

      if ( (fg = lookupHTableWP(GD->prolog_flag.table, name)) )
	set(fg, FF_ACCESSED);
    }
  }
}


#if O_XOS
typedef struct access_id
{ char *name;
  int   value;
} access_id;

static const access_id access_id_list[] =
{ { "access",          XOS_ACCESS_ACCESS },
  { "getfilesecurity", XOS_ACCESS_GETFILESECURITY },
  { "openclose",       XOS_ACCESS_OPENCLOSE },
  { NULL,              -1 }
};


static int
set_win_file_access_check(term_t a)
{ char *s;
  const access_id *p;

  if ( PL_get_chars(a, &s, CVT_ATOM) )
  { for(p=access_id_list; p->name; p++)
    { if ( strcmp(s, p->name) == 0 )
      { _xos_set_win_file_access_check(p->value);
	return TRUE;
      }
    }
  }

  return PL_domain_error("win_file_access_check", a);
}

static char*
get_win_file_access_check(void)
{ const access_id *p;
  int id = _xos_get_win_file_access_check();

  for(p=access_id_list; p->name; p++)
  { if ( p->value == id )
      return p->name;
  }
  return "unknown";
}
#endif

#define FF_COPY_FLAGS (FF_READONLY|FF_WARN_NOT_ACCESSED)
#define PSEUDO_FLAG ((prolog_flag*)TRUE)

static int
set_flag_type(prolog_flag *f, int flags)
{ f->flags = (f->flags&~FT_MASK) | (flags&FT_MASK);
  return TRUE;
}

static int
set_flag_atom(prolog_flag *f, atom_t a)
{ if ( f->value.a != a )
  { PL_unregister_atom(f->value.a);
    PL_register_atom(a);
    f->value.a = a;
  }

  return TRUE;
}


static int
keep_flag(atom_t k, prolog_flag *f, unsigned short flags, oneof *of, term_t value)
{ if ( (flags&FF_KEEP) )
  { if ( of )
    { if ( check_oneof_flag(of, f->value.a, FALSE) )
	return TRUE;
    } else if ( (flags&FT_MASK) == (f->flags&FT_MASK) )
    { return TRUE;
    } else
    { int ftype = (f->flags&FT_MASK);

      switch(flags&FT_MASK)	/* target type */
      { case FT_FROM_VALUE:
	  return TRUE;
	case FT_ATOM:
	  if ( ftype == FT_BOOL )
	    return set_flag_type(f, flags);
	  break;
	case FT_BOOL:
	  if ( ftype == FT_ATOM )
	  { int bv = atom_to_bool(f->value.a);
	    if ( bv >= 0 )
	    { set_flag_type(f, flags);
	      return set_flag_atom(f, bv ? ATOM_true : ATOM_false);
	    }
	  }
	  break;
	case FT_FLOAT:
	  if ( ftype == FT_INTEGER )
	  { f->value.f = (double)f->value.i;
	    return set_flag_type(f, flags);
	  }
	  break;
	case FT_TERM:
	{ term_t value;

	  if ( (value=PL_new_term_ref()) &&
	       unify_prolog_flag_value(MODULE_user, k, f, value) &&
	       (f->value.t=PL_record(value)) )
	    return set_flag_type(f, flags);
	  return -1;
	}
	default:
	  break;
      }
    }

    prolog_flag f2 = *f;
    f2.oneof = of;
    f2.flags = flags;
    term_t type;
    term_t preset;

    if ( !((type = PL_new_term_ref()) &&
	   (preset = PL_new_term_ref()) &&
	   unify_prolog_flag_type(&f2, type) &&
	   unify_prolog_flag_value(MODULE_user, k, f, preset) &&
	   printMessage(ATOM_warning,
			  PL_FUNCTOR_CHARS, "prolog_flag_invalid_preset", 4,
			    PL_ATOM, k,
			    PL_TERM, preset,
			    PL_TERM, type,
			PL_TERM, value) ) )
      return -1;

    clean_prolog_flag(f);
    set_flag_type(f, flags);
    f->oneof = of;

    return FALSE;
  }

  return FALSE;
}


#define set_prolog_flag_unlocked(m, k, value, flags, of) \
	LDFUNC(set_prolog_flag_unlocked, m, k, value, flags, of)

static prolog_flag *
set_prolog_flag_unlocked(DECL_LD Module m, atom_t k, term_t value, unsigned short flags, oneof *of)
{ prolog_flag *f;
  int rval = TRUE;

					/* set existing Prolog flag */
#ifdef O_PLMT
  if ( LD->prolog_flag.table &&
       (f = lookupHTableWP(LD->prolog_flag.table, k)) )
  { int rc;
    accessed_prolog_flag(f, k, TRUE);
    if ( (rc=keep_flag(k, f, flags, of, value)) == TRUE )
      return f;
    if ( rc == -1 )
      return NULL;
  } else
#endif
    if ( (f = lookupHTableWP(GD->prolog_flag.table, k)) )
  { int rc;
    accessed_prolog_flag(f, k, FALSE);
    if ( (rc=keep_flag(k, f, flags, of, value)) == TRUE )
      return f;
    if ( rc == -1 )
      return NULL;
    if ( (f->flags&FF_READONLY) && !(flags&FF_FORCE) )
    { term_t key;

      if ( (key = PL_new_term_ref()) &&
	   PL_put_atom(key, k) &&
	   PL_error(NULL, 0, NULL, ERR_PERMISSION,
		    ATOM_modify, ATOM_flag, key) )
	return NULL;
      return NULL;
    }

    if ( tbl_is_restraint_flag(k) )
      return tbl_set_restraint_flag(value, k) ? PSEUDO_FLAG : NULL;
    if ( is_arith_flag(k) )
      return set_arith_flag(value, k) ? PSEUDO_FLAG : NULL;

#ifdef O_PLMT
    if ( GD->statistics.threads_created > 1 )
    { f = copy_prolog_flag(f);

      if ( !LD->prolog_flag.table )
      { LD->prolog_flag.table = newHTableWP(4);

	LD->prolog_flag.table->copy_symbol = copySymbolPrologFlagTable;
	LD->prolog_flag.table->free_symbol = freeSymbolPrologFlagTable;
      }

      addNewHTableWP(LD->prolog_flag.table, k, f);
      PL_register_atom(k);
      DEBUG(MSG_PROLOG_FLAG,
	    Sdprintf("Localised Prolog flag %s\n", PL_atom_chars(k)));
    }
#endif
  } else if ( !(flags & FF_NOCREATE) )	/* define new Prolog flag */
  { prolog_flag *f;
    atom_t a;
    int64_t i;
    double d;

  anyway:
    PL_register_atom(k);
    f = allocHeapOrHalt(sizeof(*f));
    f->index = 0;
    f->oneof = NULL;

    switch( (flags & FT_MASK) )
    { case FT_FROM_VALUE:
      { if ( PL_get_atom(value, &a) )
	{ int bv = atom_to_bool(a);

	  if ( bv >= 0 )
	  { f->value.a = bv ? ATOM_true : ATOM_false;
	    f->flags = FT_BOOL;
	  } else
	  { f->value.a = a;
	    f->flags = FT_ATOM;
	  }
	  PL_register_atom(f->value.a);
	} else if ( PL_get_int64(value, &i) )
	{ f->flags = FT_INTEGER;
	  f->value.i = i;
	} else if ( PL_get_float(value, &d) )
	{ f->flags = FT_FLOAT;
	  f->value.f = d;
	} else
	{ f->flags = FT_TERM;
	  if ( !PL_is_ground(value) )
	  { PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
	    goto wrong_type;
	  }
	  if ( !(f->value.t = PL_record(value)) )
	  { freeHeap(f, sizeof(*f));
	    return FALSE;
	  }
	}
	break;
      }
      case FT_ATOM:
	if ( !PL_get_atom_ex(value, &f->value.a) )
	{ wrong_type:
	  freeHeap(f, sizeof(*f));
	  return FALSE;
	}
	f->flags = FT_ATOM;
	PL_register_atom(f->value.a);
	break;
      case FT_BOOL:
      { int b;
	if ( !PL_get_bool_ex(value, &b) )
	  goto wrong_type;
	f->flags = FT_BOOL;
	f->value.a = (b ? ATOM_true : ATOM_false);
	break;
      }
      case FT_INTEGER:
	if ( !PL_get_int64_ex(value, &f->value.i) )
	  goto wrong_type;
	f->flags = FT_INTEGER;
	break;
      case FT_FLOAT:
	if ( !PL_get_float_ex(value, &f->value.f) )
	  goto wrong_type;
	f->flags = FT_FLOAT;
	break;
      case FT_TERM:
	if ( !PL_is_ground(value) )
	{ PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
	  goto wrong_type;
	}
	if ( !(f->value.t = PL_record(value)) )
	  goto wrong_type;
	f->flags = FT_TERM;
	break;
    }

    f->flags |= (flags&FF_COPY_FLAGS);
    addNewHTableWP(GD->prolog_flag.table, k, f);

    return f;
  } else
  { atom_t how;

    if ( PL_current_prolog_flag(ATOM_user_flags, PL_ATOM, &how) )
    { if ( how == ATOM_error )
      { term_t key;

	if ( (key = PL_new_term_ref()) &&
	     PL_put_atom(key, k) &&
	     PL_error(NULL, 0, NULL, ERR_EXISTENCE,
		      ATOM_prolog_flag, key) )
	  return NULL;
	return NULL;
      } else if ( how == ATOM_warning )
	Sdprintf("WARNING: Flag %s: new Prolog flags must be created using "
		 "create_prolog_flag/3\n", stringAtom(k));
    }

    goto anyway;
  }

  switch(f->flags & FT_MASK)
  { case FT_BOOL:
    { int val;

      if ( !PL_get_bool_ex(value, &val) )
	return FALSE;

					/* deal with side-effects */
      if ( k == ATOM_character_escapes )
      { if ( val )
	  set(m, M_CHARESCAPE);
	else
	  clear(m, M_CHARESCAPE);
      } else if ( k == ATOM_var_prefix )
      { if ( val )
	  set(m, M_VARPREFIX);
	else
	  clear(m, M_VARPREFIX);
      } else if ( k == ATOM_debug )
      { if ( val )
	{ rval = debugmode(DBG_ALL, NULL);
	} else
	{ rval = ( tracemode(FALSE, NULL) &&
		   debugmode(DBG_OFF, NULL) );
	}
      } else if ( k == ATOM_debugger_show_context )
      { debugstatus.showContext = val;
#ifdef O_PLMT
      } else if ( k == ATOM_threads )
      { if ( !!val != !!GD->thread.enabled )
	{ rval = enableThreads(val);
	  if ( !rval )
	    break;			/* do not change value */
	}
#endif
      } else if ( k == ATOM_tty_control )
      { if ( val != (f->value.a == ATOM_true) )
	{ if ( !val && ttymodified )
	  { PopTty(Sinput, &ttytab, FALSE);
	  } else if ( val )
	  { setPrologFlagMask(PLFLAG_TTY_CONTROL);
	    PushTty(Sinput, &ttytab, TTY_SAVE);
	  }
	}
      } else if ( k == ATOM_debug_on_interrupt )
      {	rval = enable_debug_on_interrupt(val);
      } else if ( k == ATOM_protect_static_code )
      { if ( val != (f->value.a == ATOM_true) && val == FALSE )
	{ term_t ex;

	  if ( (ex = PL_new_term_ref()) &&
	       PL_put_atom(ex, ATOM_protect_static_code) )
	    return PL_permission_error("set", "prolog_flag", ex),NULL;
	  return FALSE;
	}
      }
					/* set the flag value */
      if ( f->index && rval )
      { if ( val )
	  setPrologFlagMask(f->index);
	else
	  clearPrologFlagMask(f->index);
      }
      f->value.a = (val ? ATOM_true : ATOM_false);

      break;
    }
    case FT_ATOM:
    { atom_t a;

      if ( !PL_get_atom_ex(value, &a) )
	return FALSE;

      if ( !check_oneof_flag(f->oneof, a, TRUE) )
	return FALSE;

      if ( k == ATOM_double_quotes )
      { rval = setDoubleQuotes(a, &m->flags);
      } else if ( k == ATOM_back_quotes )
      { rval = setBackQuotes(a, &m->flags);
      } else if ( k == ATOM_rational_syntax )
      { rval = setRationalSyntax(a, &m->flags);
      } else if ( k == ATOM_unknown )
      { rval = setUnknown(value, a, m);
      } else if ( k == ATOM_on_error || k == ATOM_on_warning )
      { rval = checkOnError(value, a, k);
      } else if ( k == ATOM_write_attributes )
      { rval = setWriteAttributes(a);
      } else if ( k == ATOM_occurs_check )
      { rval = setOccursCheck(a);
      } else if ( k == ATOM_access_level )
      { rval = setAccessLevelFromAtom(a);
      } else if ( k == ATOM_encoding )
      { rval = setEncoding(a);
      } else if ( k == ATOM_stream_type_check )
      { rval = setStreamTypeCheck(a);
      } else if ( k == ATOM_file_name_case_handling )
      { rval = setFileNameCaseHandling(a);
      } else if ( k == ATOM_autoload )
      { rval = setAutoload(a);
      } else if ( k == ATOM_table_monotonic )
      { rval = setMonotonicMode(a);
#if O_XOS
      } else if ( k == ATOM_win_file_access_check )
      { rval = set_win_file_access_check(value);
#endif
      }
      if ( !rval )
	fail;

      if ( f->value.a != a )
      { PL_unregister_atom(f->value.a);
	f->value.a = a;
	PL_register_atom(a);
      }
      break;
    }
    case FT_INTEGER:
    { int64_t i;

      if ( !PL_get_int64_ex(value, &i) )
	return NULL;

#ifdef O_ATOMGC
      if ( k == ATOM_agc_margin )
      { if ( i < 0 || i > SIZE_MAX )
	  return PL_representation_error("size_t"),NULL;
	GD->atoms.margin = (size_t)i;
      } else
#endif
      if ( k == ATOM_table_space )
      { if ( i < 0 || i > SIZE_MAX )
	  return PL_representation_error("size_t"),NULL;
	if ( !LD->tabling.node_pool )
	  LD->tabling.node_pool = new_alloc_pool("private_table_space", (size_t)i);
	else
	  LD->tabling.node_pool->limit = (size_t)i;
      }
#ifdef O_PLMT
      else if ( k == ATOM_shared_table_space )
      { if ( i < 0 || i > SIZE_MAX )
	  return PL_representation_error("size_t"),NULL;
	if ( !GD->tabling.node_pool )
	{ alloc_pool *pool = new_alloc_pool("shared_table_space", (size_t)i);
	  if ( pool && !COMPARE_AND_SWAP_PTR(&GD->tabling.node_pool, NULL, pool) )
	    free_alloc_pool(pool);
	} else
	  GD->tabling.node_pool->limit = (size_t)i;
      }
#endif
      else if ( k == ATOM_stack_limit )
      { if ( i < 0 || i > SIZE_MAX )
	  return PL_representation_error("size_t"),NULL;
	if ( !set_stack_limit((size_t)i) )
	  return FALSE;
      } else if ( k == ATOM_string_stack_tripwire )
      { if ( i < 0 || i > UINT_MAX )
	  return PL_representation_error("uint"),NULL;
	LD->fli.string_buffers.tripwire = (unsigned int)i;
      } else if ( k == ATOM_heartbeat )
      { if ( i < 0 )
	  return PL_error(NULL, 0, NULL, ERR_DOMAIN,
			  ATOM_not_less_than_zero, value),NULL;
	LD->yield.frequency = i/16;
      }

      f->value.i = i;
      break;
    }
    case FT_FLOAT:
    { double d;

      if ( !PL_get_float_ex(value, &d) )
	return FALSE;
      f->value.f = d;
      break;
    }
    case FT_TERM:
    { if ( f->value.t )
	PL_erase(f->value.t);
      f->value.t = PL_record(value);
      break;
    }
    default:
      assert(0);
  }

  return rval ? f : NULL;
}


static prolog_flag *
set_prolog_flag_ptr(term_t key, term_t value, unsigned short flags, oneof *of)
{ GET_LD
  atom_t k;
  Module m = MODULE_parse;
  prolog_flag *f;

  if ( !PL_strip_module(key, &m, key) ||
       !PL_get_atom_ex(key, &k) )
    return FALSE;

  if ( k == ATOM_autoload && !propagateAutoload(value) )
    return FALSE;
  if ( k == ATOM_threads )
    return set_prolog_flag_unlocked(m, k, value, flags, of);

  PL_LOCK(L_PLFLAG);
  f = set_prolog_flag_unlocked(m, k, value, flags, of);
  PL_UNLOCK(L_PLFLAG);

  return f;
}

int
set_prolog_flag(term_t key, term_t value, unsigned short flags)
{ return !!set_prolog_flag_ptr(key, value, flags, NULL);
}

/** set_prolog_flag(+Key, +Value) is det.
*/

static
PRED_IMPL("set_prolog_flag", 2, set_prolog_flag, PL_FA_ISO)
{ return set_prolog_flag(A1, A2, FF_NOCREATE|FT_FROM_VALUE);
}


/** create_prolog_flag(+Key, +Value, +Options) is det.
*/

static const PL_option_t prolog_flag_options[] =
{ { ATOM_type,              OPT_TERM },
  { ATOM_access,            OPT_ATOM },
  { ATOM_keep,              OPT_BOOL },
  { ATOM_warn_not_accessed, OPT_BOOL },
  { NULL_ATOM,              0 }
};

static
PRED_IMPL("create_prolog_flag", 3, create_prolog_flag, PL_FA_ISO)
{ PRED_LD
  unsigned short flags = 0;
  term_t type = 0;
  atom_t access = ATOM_read_write;
  int keep = FALSE;
  int warn_not_accessed = FALSE;
  oneof *of = NULL;
  atom_t a;

  if ( !PL_scan_options(A3, 0, "prolog_flag_option", prolog_flag_options,
			&type, &access, &keep, &warn_not_accessed) )
    return FALSE;

  if ( type == 0 )
  { flags |= FT_FROM_VALUE;
  } else if ( PL_get_atom(type, &a) )
  { if ( a == ATOM_boolean )
      flags |= FT_BOOL;
    else if ( a == ATOM_integer )
      flags |= FT_INTEGER;
    else if ( a == ATOM_float )
      flags |= FT_FLOAT;
    else if ( a == ATOM_atom )
      flags |= FT_ATOM;
    else if ( a == ATOM_term )
      flags |= FT_TERM;
    else
      return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_prolog_flag_type, type);
  } else if ( PL_is_functor(type, FUNCTOR_oneof1) )
  { _PL_get_arg(1, type, type);
    if ( !(of=create_oneof(type)) )
      return FALSE;
    flags |= FT_ATOM;
  } else
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_prolog_flag_type, type);

  if ( access == ATOM_read_only )
    flags |= FF_READONLY;
  else if ( access != ATOM_read_write )
  { term_t a = PL_new_term_ref();
    PL_put_atom(a, access);
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_prolog_flag_access, a);
  }

  if ( keep )
    flags |= FF_KEEP;

  if ( warn_not_accessed )
    flags |= FF_WARN_NOT_ACCESSED;

  prolog_flag *f = set_prolog_flag_ptr(A1, A2, flags, of);

  if ( of )
  { if ( f && f != PSEUDO_FLAG )
      f->oneof = of;
    else
      free_oneof(of);
  }

  return !!f;
}


static prolog_flag *
lookupFlag(atom_t key)
{ GET_LD
#ifdef O_PLMT
  prolog_flag *f = NULL;

  if ( LD->prolog_flag.table &&
       (f = lookupHTableWP(LD->prolog_flag.table, key)) )
  { return f;
  } else
#endif
  { return lookupHTableWP(GD->prolog_flag.table, key);
  }
}


int
PL_current_prolog_flag(atom_t name, int type, void *value)
{ prolog_flag *f;

  if ( (f=lookupFlag(name)) )
  { switch(type)
    { case PL_ATOM:
	if ( (f->flags&FT_MASK) == FT_ATOM )
	{ atom_t *vp = value;
	  *vp = f->value.a;
	  return TRUE;
	}
	return FALSE;
      case PL_INTEGER:
	if ( (f->flags&FT_MASK) == FT_INTEGER )
	{ int64_t *vp = value;
	  *vp = f->value.i;
	  return TRUE;
	}
	return FALSE;
      case PL_FLOAT:
	if ( (f->flags&FT_MASK) == FT_FLOAT )
	{ double *vp = value;
	  *vp = f->value.f;
	  return TRUE;
	}
	return FALSE;
      case PL_TERM:
	if ( (f->flags&FT_MASK) == FT_TERM )
	{ term_t *vp = value;
	  term_t t = *vp;

	  return PL_recorded(f->value.t, t);
	}
	return FALSE;
    }
  }

  return FALSE;
}

#define unify_prolog_flag_value(m, key, f, val) \
  LDFUNC(unify_prolog_flag_value, m, key, f, val)

static int
unify_prolog_flag_value(DECL_LD Module m, atom_t key, prolog_flag *f, term_t val)
{ if ( key == ATOM_character_escapes )
  { return PL_unify_bool(val, true(m, M_CHARESCAPE));
  } else if ( key == ATOM_var_prefix )
  { return PL_unify_bool(val, true(m, M_VARPREFIX));
  } else if ( key == ATOM_double_quotes )
  { atom_t v;

    if ( true(m, DBLQ_CHARS) )
      v = ATOM_chars;
    else if ( true(m, DBLQ_ATOM) )
      v = ATOM_atom;
    else if ( true(m, DBLQ_STRING) )
      v = ATOM_string;
    else
      v = ATOM_codes;

    return PL_unify_atom(val, v);
  } else if ( key == ATOM_back_quotes )
  { atom_t v;

    if ( true(m, BQ_STRING) )
      v = ATOM_string;
    else if ( true(m, BQ_CODES) )
      v = ATOM_codes;
    else if ( true(m, BQ_CHARS) )
      v = ATOM_chars;
    else
      v = ATOM_symbol_char;

    return PL_unify_atom(val, v);
  } else if ( key == ATOM_rational_syntax )
  { atom_t v;

    switch(m->flags&RAT_MASK)
    { case RAT_NATURAL: v = ATOM_natural;       break;
      case RAT_COMPAT:  v = ATOM_compatibility; break;
      default:		v = 0; assert(0);
    }

    return PL_unify_atom(val, v);
  } else if ( key == ATOM_unknown )
  { atom_t v;

    switch ( getUnknownModule(m) )
    { case UNKNOWN_ERROR:
	v = ATOM_error;
	break;
      case UNKNOWN_WARNING:
	v = ATOM_warning;
	break;
      case UNKNOWN_FAIL:
	v = ATOM_fail;
	break;
      default:
	assert(0);
	return FALSE;
    }

    return PL_unify_atom(val, v);
#ifdef O_PLMT
  } else if ( key == ATOM_system_thread_id )
  { return PL_unify_integer(val, system_thread_id(NULL));
#endif
  } else if ( key == ATOM_debug )
  { return PL_unify_bool_ex(val, debugstatus.debugging);
  } else if ( key == ATOM_debugger_show_context )
  { return PL_unify_bool_ex(val, debugstatus.showContext);
  } else if ( key == ATOM_break_level )
  { int bl = currentBreakLevel();

    if ( bl >= 0 )
      return PL_unify_integer(val, bl);
    return FALSE;
  } else if ( key == ATOM_access_level )
  { return PL_unify_atom(val, accessLevel());
  } else if ( key == ATOM_stack_limit )
  { return PL_unify_int64(val, LD->stacks.limit);
  } else if ( tbl_is_restraint_flag(key) )
  { return tbl_get_restraint_flag(val, key) == TRUE;
  } else if ( is_arith_flag(key) )
  { return get_arith_flag(val, key) == TRUE;
  }

  switch(f->flags & FT_MASK)
  { case FT_BOOL:
      if ( f->index )
	return PL_unify_bool_ex(val, truePrologFlag(f->index) != FALSE);
      /*FALLTHROUGH*/
    case FT_ATOM:
      return PL_unify_atom(val, f->value.a);
    case FT_INTEGER:
      return PL_unify_int64(val, f->value.i);
    case FT_FLOAT:
      return PL_unify_float(val, f->value.f);
    case FT_TERM:
    { term_t tmp = PL_new_term_ref();

      if ( PL_recorded(f->value.t, tmp) )
	return PL_unify(val, tmp);
      else
	return raiseStackOverflow(GLOBAL_OVERFLOW);
    }
    default:
      assert(0);
      fail;
  }
}


static int
unify_prolog_flag_access(prolog_flag *f, term_t access)
{ GET_LD

  if ( f->flags & FF_READONLY )
    return PL_unify_atom(access, ATOM_read);
  else
    return PL_unify_atom(access, ATOM_write);
}


static int
unify_prolog_flag_type(prolog_flag *f, term_t type)
{ GET_LD
  atom_t a;

  switch(f->flags & FT_MASK)
  { case FT_BOOL:
      a = ATOM_boolean;
      break;
    case FT_ATOM:
      if ( f->oneof )
      { term_t t;

	return ( (t=PL_new_term_ref()) &&
		 put_oneof(t, f->oneof) &&
		 PL_unify(t, type) );
      }
      a = ATOM_atom;
      break;
    case FT_INTEGER:
      a = ATOM_integer;
      break;
    case FT_FLOAT:
      a = ATOM_float;
      break;
    case FT_TERM:
      a = ATOM_term;
      break;
    default:
      assert(0);
      fail;
  }

  return PL_unify_atom(type, a);
}


typedef struct
{ TableEnum table_enum;
  atom_t scope;
  int explicit_scope;
  Module module;
} prolog_flag_enum;

foreign_t
pl_prolog_flag5(DECL_LD term_t key, term_t value,
		term_t scope, term_t access, term_t type,
		control_t h)
{ prolog_flag_enum *e;
  fid_t fid;
  Module module;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
    { atom_t k;

      module = MODULE_parse;
      if ( !PL_strip_module(key, &module, key) )
	return FALSE;

      if ( PL_get_atom(key, &k) )
      { prolog_flag *f;

#ifdef O_PLMT
	if ( LD->prolog_flag.table &&
	     (f = lookupHTableWP(LD->prolog_flag.table, k)) )
	{ accessed_prolog_flag(f, k, TRUE);
	  return ( unify_prolog_flag_value(module, k, f, value) &&
		   (!access || unify_prolog_flag_access(f, access)) &&
		   (!type   || unify_prolog_flag_type(f, type)) );
	}
#endif
	if ( (f = lookupHTableWP(GD->prolog_flag.table, k)) )
	{ accessed_prolog_flag(f, k, FALSE);
	  return ( unify_prolog_flag_value(module, k, f, value) &&
		   (!access || unify_prolog_flag_access(f, access)) &&
		   (!type   || unify_prolog_flag_type(f, type)) );
	}

	fail;
      } else if ( PL_is_variable(key) )
      { e = allocHeapOrHalt(sizeof(*e));

	e->module = module;

	if ( scope && PL_get_atom(scope, &e->scope) )
	{ e->explicit_scope = TRUE;
	  if ( !(e->scope == ATOM_local || e->scope == ATOM_global) )
	  { freeHeap(e, sizeof(*e));
	    return PL_error(NULL, 0, NULL, ERR_DOMAIN,
			    PL_new_atom("scope"), scope);
	  }
	} else
	{ e->explicit_scope = FALSE;

	  if ( LD->prolog_flag.table )
	    e->scope = ATOM_local;
	  else
	    e->scope = ATOM_global;
	}

	if ( e->scope == ATOM_local )
	  e->table_enum = newTableEnumWP(LD->prolog_flag.table);
	else
	  e->table_enum = newTableEnumWP(GD->prolog_flag.table);

	break;
      } else
	return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, key);
    }
    case FRG_REDO:
      e = ForeignContextPtr(h);
      break;
    case FRG_CUTTED:
      e = ForeignContextPtr(h);
      if ( e )
      { freeTableEnum(e->table_enum);
	freeHeap(e, sizeof(*e));
      }
    default:
      succeed;
  }

  fid = PL_open_foreign_frame();
  PL_LOCK(L_PLFLAG);
  for(;;)
  { table_key_t tk;
    table_value_t tv;

    while( advanceTableEnum(e->table_enum, &tk, &tv) )
    { atom_t fn = (atom_t)tk;
      prolog_flag *f = val2ptr(tv);

      if ( e->explicit_scope == FALSE &&
	   e->scope == ATOM_global &&
	   LD->prolog_flag.table &&
	   lookupHTableWP(LD->prolog_flag.table, fn) )
	continue;

      if ( PL_unify_atom(key, fn) &&
	   unify_prolog_flag_value(e->module, fn, f, value) &&
	   (!scope  || PL_unify_atom(scope, e->scope)) &&
	   (!access || unify_prolog_flag_access(f, access)) &&
	   (!type   || unify_prolog_flag_type(f, type)) )
      { PL_UNLOCK(L_PLFLAG);
	ForeignRedoPtr(e);
      }
      if ( exception_term )
      { exception_term = 0;
	setVar(*valTermRef(exception_bin));
      }
      PL_rewind_foreign_frame(fid);
    }

    if ( e->scope == ATOM_local )
    { e->scope = ATOM_global;
      freeTableEnum(e->table_enum);
      e->table_enum = newTableEnumWP(GD->prolog_flag.table);
    } else
      break;
  }
  PL_UNLOCK(L_PLFLAG);

  freeTableEnum(e->table_enum);
  freeHeap(e, sizeof(*e));

  fail;
}

static
PRED_IMPL("$current_prolog_flag", 5, dcurrent_prolog_flag, PL_FA_NONDETERMINISTIC)
{ PRED_LD

  return pl_prolog_flag5(A1, A2, A3, A4, A5, PL__ctx);
}

static
PRED_IMPL("current_prolog_flag", 2, current_prolog_flag, PL_FA_ISO|PL_FA_NONDETERMINISTIC)
{ PRED_LD

  return pl_prolog_flag5(A1, A2, 0, 0, 0, PL__ctx);
}


static void
set_arch(void)
{
#ifdef __APPLE__
#include <sys/sysctl.h>
  uint32_t cputype;
  size_t size = sizeof(cputype);

  if ( strcmp(PLARCH, "fat-darwin") == 0 )
  { setPrologFlag("apple_universal_binary", FT_BOOL|FF_READONLY, TRUE, 0);

    if ( sysctlbyname("hw.cputype", &cputype, &size, NULL, 0) == 0 )
    { switch(cputype&0xff)
      { case 7:
	  setPrologFlag("arch", FT_ATOM|FF_READONLY, "x86_64-darwin");
	  return;
	case 12:
	  setPrologFlag("arch", FT_ATOM|FF_READONLY, "arm64-darwin");
	  return;
	default:
	  Sdprintf("sysctlbyname() cputype = %d (unknown)\n", (int)cputype);
      }
    }
  }
#endif

  setPrologFlag("arch", FT_ATOM|FF_READONLY, PLARCH);
}

static void
set_libswipl(void)
{
#ifdef __WINDOWS__
  { char buf[PATH_MAX];
    char *s;
    if ( (s=findModulePath("libswipl.dll", buf, sizeof(buf))) )
    { setPrologFlag("libswipl", FT_ATOM|FF_READONLY, s);
      return;
    }
  }
#endif

#if defined(HAVE_DLADDR) && !defined(O_STATIC_EXTENSIONS)
  Dl_info info;

  if ( dladdr((void*)(intptr_t)PL_initialise, &info) && info.dli_fname )
  { setPrologFlag("libswipl", FT_ATOM|FF_READONLY, info.dli_fname);
    return;
  }
#endif

#ifdef LIBPL_PATH
  setPrologFlag("libswipl", FT_ATOM, LIBPL_PATH);
#endif
}

		 /*******************************
		 *	INITIALISE FEATURES	*
		 *******************************/

#ifndef SO_EXT
#define SO_EXT "so"
#endif
#ifndef SO_PATH
#define SO_PATH "LD_LIBRARY_PATH"
#endif
#ifndef C_LIBPLSO
#define C_LIBPLSO ""
#endif

#ifdef __WINDOWS__
#include "../pl-nt.h"
#endif

static void
initPrologFlagTable(void)
{ if ( !GD->prolog_flag.table )
  { initPrologThreads();	/* may be called before PL_initialise() */

    GD->prolog_flag.table = newHTableWP(256);
  }
}

void
initPrologFlags(void)
{ GET_LD

  setPrologFlag("iso",  FT_BOOL, FALSE, PLFLAG_ISO);
  set_arch();
#if __WINDOWS__
  setPrologFlag("windows",	FT_BOOL|FF_READONLY, TRUE, 0);
  const char *wine_version;
  if ( (wine_version=PL_w32_running_under_wine()) )
    setPrologFlag("wine_version", FT_ATOM|FF_READONLY, wine_version, 0);
#endif
#if O_XOS
  setPrologFlag("win_file_access_check", FT_ATOM,
		get_win_file_access_check(), 0);
#endif
  setPrologFlag("file_name_case_handling", FT_ATOM,
		stringAtom(currentFileNameCaseHandling()));
  setPrologFlag("path_max", FT_INTEGER|FF_READONLY, (intptr_t)PATH_MAX);
  setPrologFlag("version", FT_INTEGER|FF_READONLY, (intptr_t)PLVERSION);
  setPrologFlag("dialect", FT_ATOM|FF_READONLY, "swi");
  if ( systemDefaults.home )
    setPrologFlag("home", FT_ATOM|FF_READONLY, systemDefaults.home);
#ifdef PLSHAREDHOME
  setPrologFlag("shared_home", FT_ATOM|FF_READONLY, PLSHAREDHOME);
#endif
  set_libswipl();
#ifdef EXEC_FORMAT
  setPrologFlag("executable_format", FT_ATOM|FF_READONLY, EXEC_FORMAT);
#endif
  if ( GD->paths.executable )
    setPrologFlag("executable", FT_ATOM|FF_READONLY, GD->paths.executable);
#if defined(HAVE_GETPID) || defined(EMULATE_GETPID)
  setPrologFlag("pid", FT_INTEGER|FF_READONLY, (intptr_t)getpid());
#endif
  setPrologFlag("optimise", FT_BOOL, GD->cmdline.optimise, PLFLAG_OPTIMISE);
  setPrologFlag("optimise_unify", FT_BOOL, TRUE, PLFLAG_OPTIMISE_UNIFY);
  setPrologFlag("generate_debug_info", FT_BOOL,
		truePrologFlag(PLFLAG_DEBUGINFO), PLFLAG_DEBUGINFO);
  setPrologFlag("protect_static_code", FT_BOOL, FALSE,
		PLFLAG_PROTECT_STATIC_CODE);
  setPrologFlag("last_call_optimisation", FT_BOOL, TRUE, PLFLAG_LASTCALL);
  setPrologFlag("vmi_builtin", FT_BOOL, TRUE, PLFLAG_VMI_BUILTIN);
  setPrologFlag("warn_override_implicit_import", FT_BOOL, TRUE,
		PLFLAG_WARN_OVERRIDE_IMPLICIT_IMPORT);
  setPrologFlag("tmp_dir", FT_ATOM, SWIPL_TMP_DIR);
#if defined(O_LARGEFILES) || SIZEOF_LONG == 8
  setPrologFlag("large_files", FT_BOOL|FF_READONLY, TRUE, 0);
#endif
  setPrologFlag("unload_foreign_libraries", FT_BOOL, FALSE, 0);
  setPrologFlag("gc",	  FT_BOOL,	       TRUE,  PLFLAG_GC);
  setPrologFlag("trace_gc",  FT_BOOL,	       FALSE, PLFLAG_TRACE_GC);
#ifdef O_ATOMGC
  setPrologFlag("agc_margin", FT_INTEGER, (intptr_t)GD->atoms.margin);
  setPrologFlag("agc_close_streams", FT_BOOL, FALSE, PLFLAG_AGC_CLOSE_STREAMS);
#endif
  setPrologFlag("table_space", FT_INTEGER, (intptr_t)GD->options.tableSpace);
#ifdef O_PLMT
  setPrologFlag("shared_table_space", FT_INTEGER, (intptr_t)GD->options.sharedTableSpace);
#endif
  setPrologFlag("stack_limit", FT_INTEGER, (intptr_t)LD->stacks.limit);
#ifdef O_DYNAMIC_EXTENSIONS
  setPrologFlag("open_shared_object",	     FT_BOOL|FF_READONLY, TRUE, 0);
  setPrologFlag("shared_object_extension",   FT_ATOM|FF_READONLY, SO_EXT);
  setPrologFlag("shared_object_search_path", FT_ATOM|FF_READONLY, SO_PATH);
  setPrologFlag("c_cc",	     FT_ATOM, C_CC);
  setPrologFlag("c_cxx",     FT_ATOM, C_CXX);
  setPrologFlag("c_libs",    FT_ATOM, C_LIBS);
#ifdef C_LIBDIR
  setPrologFlag("c_libdir",  FT_ATOM, C_LIBDIR);
#endif
  setPrologFlag("c_libplso", FT_ATOM, C_LIBPLSO);
  setPrologFlag("c_ldflags", FT_ATOM, C_LDFLAGS);
  setPrologFlag("c_cflags",  FT_ATOM, C_CFLAGS);
#endif
  setPrologFlag("address_bits", FT_INTEGER|FF_READONLY, (intptr_t)sizeof(void*)*8);
#ifdef HAVE_POPEN
  setPrologFlag("pipe", FT_BOOL, TRUE, 0);
#endif
#ifdef O_PLMT
  setPrologFlag("threads",	FT_BOOL, !GD->options.nothreads, 0);
  if ( GD->options.xpce >= 0 )
    setPrologFlag("xpce",	FT_BOOL, GD->options.xpce, 0);
  setPrologFlag("system_thread_id", FT_INTEGER|FF_READONLY, (intptr_t)0);
  setPrologFlag("gc_thread",    FT_BOOL,
		!GD->options.nothreads &&
		truePrologFlag(PLFLAG_GCTHREAD), PLFLAG_GCTHREAD);
#else
  setPrologFlag("threads",	FT_BOOL|FF_READONLY, FALSE, 0);
  setPrologFlag("gc_thread",    FT_BOOL|FF_READONLY, FALSE, PLFLAG_GCTHREAD);
#endif
#ifdef O_DDE
  setPrologFlag("dde", FT_BOOL|FF_READONLY, TRUE, 0);
#endif
#ifdef O_RUNTIME
  setPrologFlag("runtime",	FT_BOOL|FF_READONLY, TRUE, 0);
  setPrologFlag("debug_on_error", FT_BOOL|FF_READONLY, FALSE,
	     PLFLAG_DEBUG_ON_ERROR);
  setPrologFlag("report_error",	FT_BOOL|FF_READONLY, FALSE,
	     PLFLAG_REPORT_ERROR);
#else
  setPrologFlag("debug_on_error",	FT_BOOL, TRUE, PLFLAG_DEBUG_ON_ERROR);
  setPrologFlag("report_error",	FT_BOOL, TRUE, PLFLAG_REPORT_ERROR);
#endif
  setPrologFlag("on_error", FT_ATOM, GD->options.on_error);
  setPrologFlag("on_warning", FT_ATOM, GD->options.on_warning);
  setPrologFlag("break_level", FT_INTEGER|FF_READONLY, (intptr_t)0);
  setPrologFlag("user_flags", FT_ATOM, "silent");
  setPrologFlag("editor", FT_ATOM, "default");
  setPrologFlag("debugger_show_context", FT_BOOL, FALSE, 0);
  setPrologFlag("autoload",  FT_ATOM, "true");
  setPrologFlagMask(PLFLAG_AUTOLOAD);
#ifndef O_BIGNUM
  setPrologFlag("max_integer",	   FT_INT64|FF_READONLY, PLMAXINT);
  setPrologFlag("min_integer",	   FT_INT64|FF_READONLY, PLMININT);
#endif
  setPrologFlag("max_tagged_integer", FT_INT64|FF_READONLY, (int64_t)PLMAXTAGGEDINT);
  setPrologFlag("min_tagged_integer", FT_INT64|FF_READONLY, (int64_t)PLMINTAGGEDINT);
#ifdef O_BIGNUM
  setPrologFlag("bounded",	      FT_BOOL|FF_READONLY,	   FALSE, 0);
  setPrologFlag("rationals",	      FT_BOOL|FF_READONLY,	   TRUE, 0);
  setPrologFlag("prefer_rationals", FT_BOOL, O_PREFER_RATIONALS, PLFLAG_RATIONAL);
  setPrologFlag("rational_syntax",  FT_ATOM,
		O_RATIONAL_SYNTAX == RAT_NATURAL ? "natural" :
						   "compatibility");
#ifdef __GNU_MP__
  setPrologFlag("gmp_version",	   FT_INTEGER|FF_READONLY, (intptr_t)__GNU_MP__);
#endif
#else
  setPrologFlag("bounded",		   FT_BOOL|FF_READONLY,	   TRUE, 0);
#endif
  if ( (-3 / 2) == -2 )
    setPrologFlag("integer_rounding_function", FT_ATOM|FF_READONLY, "down");
  else
    setPrologFlag("integer_rounding_function", FT_ATOM|FF_READONLY, "toward_zero");
  setPrologFlag("max_char_code", FT_INTEGER|FF_READONLY, (intptr_t)UNICODE_MAX);
  setPrologFlag("max_arity", FT_ATOM|FF_READONLY, "unbounded");
  setPrologFlag("max_procedure_arity", FT_INTEGER|FF_READONLY, (intptr_t)MAXARITY);
  setPrologFlag("colon_sets_calling_context", FT_BOOL|FF_READONLY, TRUE, 0);
  setPrologFlag("character_escapes", FT_BOOL, TRUE, PLFLAG_CHARESCAPE);
  setPrologFlag("character_escapes_unicode", FT_BOOL, TRUE,
		PLFLAG_CHARESCAPE_UNICODE);
  setPrologFlag("var_prefix", FT_BOOL, FALSE, PLFLAG_VARPREFIX);
  setPrologFlag("char_conversion", FT_BOOL, FALSE, PLFLAG_CHARCONVERSION);
#ifdef O_QUASIQUOTATIONS
  setPrologFlag("quasi_quotations", FT_BOOL, TRUE, PLFLAG_QUASI_QUOTES);
#endif
  setPrologFlag("write_attributes", FT_ATOM, "ignore");
  setPrologFlag("stream_type_check", FT_ATOM, "loose");
  setPrologFlag("occurs_check", FT_ATOM, "false");
  setPrologFlag("shift_check", FT_BOOL, FALSE,  PLFLAG_SHIFT_CHECK);
  setPrologFlag("access_level", FT_ATOM, "user");
  setPrologFlag("double_quotes", FT_ATOM,
		GD->options.traditional ? "codes" : "string");
  setPrologFlag("back_quotes", FT_ATOM,
		GD->options.traditional ? "symbol_char" : "codes");
  setPrologFlag("portable_vmi", FT_BOOL, TRUE, PLFLAG_PORTABLE_VMI);
  setPrologFlag("traditional", FT_BOOL|FF_READONLY, GD->options.traditional, 0);
  setPrologFlag("unknown", FT_ATOM, "error");
  setPrologFlag("debug", FT_BOOL, FALSE, 0);
  setPrologFlag("debug_on_interrupt", FT_BOOL,
		truePrologFlag(PLFLAG_DEBUG_ON_INTERRUPT),
		PLFLAG_DEBUG_ON_INTERRUPT);
  setPrologFlag("verbose", FT_ATOM|FF_KEEP, GD->options.silent ? "silent" : "normal");
  setPrologFlag("verbose_load", FT_ATOM, "silent");
  setPrologFlag("verbose_autoload", FT_BOOL, FALSE, 0);
  setPrologFlag("verbose_file_search", FT_BOOL, FALSE, 0);
  setPrologFlag("sandboxed_load", FT_BOOL, FALSE, 0);
  setPrologFlag("allow_variable_name_as_functor", FT_BOOL, FALSE,
		ALLOW_VARNAME_FUNCTOR);
  setPrologFlag("allow_dot_in_atom", FT_BOOL, FALSE,
		PLFLAG_DOT_IN_ATOM);
  setPrologFlag("toplevel_var_size", FT_INTEGER, (intptr_t)1000);
  setPrologFlag("toplevel_print_anon", FT_BOOL, FALSE, 0);
  setPrologFlag("toplevel_name_variables", FT_BOOL, TRUE, 0);
  setPrologFlag("toplevel_prompt", FT_ATOM, "~m~d~l~! ?- ");
  setPrologFlag("file_name_variables", FT_BOOL, FALSE, PLFLAG_FILEVARS);
  setPrologFlag("fileerrors", FT_BOOL, TRUE, PLFLAG_FILEERRORS);
  setPrologFlag("determinism_error", FT_ATOM, "error");
#ifdef O_DEBUG
  setPrologFlag("prolog_debug", FT_BOOL|FF_READONLY, TRUE, 0);
#endif
#ifdef __EMSCRIPTEN__
  setPrologFlag("emscripten", FT_BOOL|FF_READONLY, TRUE, 0);
#else
#ifdef __unix__
  setPrologFlag("unix", FT_BOOL|FF_READONLY, TRUE, 0);
#endif
#ifdef __APPLE__
  setPrologFlag("apple", FT_BOOL|FF_READONLY, TRUE, 0);
#endif
#ifdef __ANDROID__
  setPrologFlag("android", FT_BOOL|FF_READONLY, TRUE, 0);
# ifdef __ANDROID_API__
  setPrologFlag("android_api",FT_INTEGER|FF_READONLY, (intptr_t)__ANDROID_API__);
# endif
#endif
#ifdef __CONDA__
  setPrologFlag("conda", FT_BOOL|FF_READONLY, TRUE, 0);
#endif
#endif

#if __WINDOWS__
  #if __CONDA__ || MSYS2
  setPrologFlag("xdg", FT_BOOL, TRUE, 0);
  #endif
#else
  setPrologFlag("xdg", FT_BOOL|FF_READONLY, TRUE, 0);
#endif

  setPrologFlag("encoding", FT_ATOM,
		stringAtom(PL_encoding_to_atom(LD->encoding)));

  setPrologFlag("tty_control", FT_BOOL,
		truePrologFlag(PLFLAG_TTY_CONTROL), PLFLAG_TTY_CONTROL);
  setPrologFlag("signals", FT_BOOL|FF_READONLY,
		truePrologFlag(PLFLAG_SIGNALS), PLFLAG_SIGNALS);
  setPrologFlag("packs", FT_BOOL, GD->cmdline.packs, 0);
  setPrologFlag("heartbeat", FT_INTEGER, (intptr_t)0);

#if defined(__WINDOWS__) && defined(_DEBUG)
  setPrologFlag("kernel_compile_mode", FT_ATOM|FF_READONLY, "debug");
#endif

#if defined(BUILD_TIME) && defined(BUILD_DATE)
  setPrologFlag("compiled_at", FT_ATOM|FF_READONLY, BUILD_DATE ", " BUILD_TIME);
#elif defined(__DATE__) && defined(__TIME__)
  setPrologFlag("compiled_at", FT_ATOM|FF_READONLY, __DATE__ ", " __TIME__);
#endif
  setPrologFlag("error_ambiguous_stream_pair", FT_BOOL, FALSE,
		PLFLAG_ERROR_AMBIGUOUS_STREAM_PAIR);
#ifdef O_MITIGATE_SPECTRE
  setPrologFlag("mitigate_spectre", FT_BOOL, FALSE, PLFLAG_MITIGATE_SPECTRE);
#endif
#ifdef POSIX_SHELL
  setPrologFlag("posix_shell", FT_ATOM, POSIX_SHELL);
#endif
#ifdef __WINDOWS__
  setPrologFlag("path_sep", FT_ATOM, ";");
#else
  setPrologFlag("path_sep", FT_ATOM, ":");
#endif

  setPrologFlag("table_incremental", FT_BOOL, FALSE, PLFLAG_TABLE_INCREMENTAL);
  setPrologFlag("table_subsumptive", FT_BOOL, FALSE, 0);
  setPrologFlag("table_shared",      FT_BOOL, FALSE, PLFLAG_TABLE_SHARED);

  setTmpDirPrologFlag();
  setTZPrologFlag();
  setOSPrologFlags();
  setVersionPrologFlag();
  setArgvPrologFlag("os_argv", GD->cmdline.os_argc,   GD->cmdline.os_argv);
  setArgvPrologFlag("argv",    GD->cmdline.appl_argc, GD->cmdline.appl_argv);
#ifdef __SANITIZE_ADDRESS__
  setPrologFlag("asan", FT_BOOL|FF_READONLY, TRUE, 0);
#endif
}


static void
setTmpDirPrologFlag(void)
 { char envbuf[PATH_MAX];
   char *td = NULL;

#ifdef __unix__
   td=Getenv("TMP", envbuf, sizeof(envbuf));
#elif __WINDOWS__
   td=Getenv("TEMP", envbuf, sizeof(envbuf));
#endif

   if (td == (char *) NULL)
     td = SWIPL_TMP_DIR;

   setPrologFlag("tmp_dir", FT_ATOM, td);
}

static void
setArgvPrologFlag(const char *flag, int argc, char **argv)
{ GET_LD
  fid_t fid = PL_open_foreign_frame();
  term_t e = PL_new_term_ref();
  term_t l = PL_new_term_ref();
  int n;

  PL_put_nil(l);
  for(n=argc-1; n>= 0; n--)
  { PL_put_variable(e);
    if ( !PL_unify_chars(e, PL_ATOM|REP_FN, (size_t)-1, argv[n]) )
      fatalError("Could not set Prolog flag argv:\n\tcurrent locale cannot represent argv[%d] (%s)", n, argv[n]);
    if ( !PL_cons_list(l, e, l) )
      fatalError("Could not set Prolog flag argv: not enough stack");
  }

  setPrologFlag(flag, FT_TERM, l);
  PL_discard_foreign_frame(fid);
}


static void
setTZPrologFlag(void)
{ tzset();

#if defined(__WINDOWS__) || defined(__CYGWIN__)
#define timezone _timezone
#endif

  setPrologFlag("timezone", FT_INTEGER|FF_READONLY, (intptr_t)timezone);
}


static void
setVersionPrologFlag(void)
{ GET_LD
  fid_t fid = PL_open_foreign_frame();
  term_t t = PL_new_term_ref();
  term_t o = PL_new_term_ref();
  int major = PLVERSION/10000;
  int minor = (PLVERSION/100)%100;
  int patch = (PLVERSION%100);

  PL_put_nil(o);
  initFunctors();

#ifdef PLVERSION_TAG
  { const char *tag = PLVERSION_TAG;
    if ( tag && *tag )
    { int rc;
      term_t tt;

      rc = ( (tt=PL_new_term_ref()) &&
	     PL_put_atom_chars(tt, tag) &&
	     PL_cons_functor(tt, FUNCTOR_tag1, tt) &&
	     PL_cons_functor(o,  FUNCTOR_dot2, tt, o) );
      (void)rc;
    }
  }
#endif

  if ( !PL_unify_term(t,
		      PL_FUNCTOR_CHARS, PLNAME, 4,
			PL_INT, major,
			PL_INT, minor,
			PL_INT, patch,
			PL_TERM, o) )
    sysError("Could not set version");

  setPrologFlag("version_data", FF_READONLY|FT_TERM, t);
  PL_discard_foreign_frame(fid);

  setGITVersion();
}

static int
abi_version_dict(term_t dict)
{ GET_LD
  const atom_t keys[] = { ATOM_foreign_interface,
			  ATOM_record,
			  ATOM_qlf,
			  ATOM_qlf_min_load,
			  ATOM_vmi,
			  ATOM_built_in };
  term_t values = PL_new_term_refs(6);

  return ( PL_unify_integer(values+0, PL_version_info(PL_VERSION_FLI)) &&
	   PL_unify_integer(values+1, PL_version_info(PL_VERSION_REC)) &&
	   PL_unify_integer(values+2, PL_version_info(PL_VERSION_QLF)) &&
	   PL_unify_integer(values+3, PL_version_info(PL_VERSION_QLF_LOAD)) &&
	   PL_unify_integer(values+4, PL_version_info(PL_VERSION_VM)) &&
	   PL_unify_integer(values+5, PL_version_info(PL_VERSION_BUILT_IN)) &&

	   PL_put_dict(dict, ATOM_abi, 6, keys, values) );
}


void
setABIVersionPrologFlag(void)
{ GET_LD
  fid_t fid = PL_open_foreign_frame();
  term_t t = PL_new_term_ref();

  if ( abi_version_dict(t) )
    setPrologFlag("abi_version", FF_READONLY|FT_TERM, t);

  PL_discard_foreign_frame(fid);
}


int
checkPrologFlagsAccess(void)
{ int rc = TRUE;

  if ( GD->prolog_flag.table )
  { if ( HAS_LD )
    { GET_LD
      fid_t fid = PL_open_foreign_frame();

      if ( fid )
      { term_t list = PL_new_term_ref();
	term_t tail = PL_copy_term_ref(list);
	term_t head = PL_new_term_ref();
	int found = 0;

	FOR_TABLE(GD->prolog_flag.table, n, v)
	{ atom_t name = (atom_t)n;
	  prolog_flag *f = val2ptr(v);

	  if ( true(f, FF_WARN_NOT_ACCESSED) &&
	       false(f, FF_ACCESSED) )
	  { found++;
	    if ( !PL_unify_list(tail, head, tail) ||
		 !PL_unify_atom(head, name) )
	    { rc = FALSE;
	      break;
	    }
	  }
	}
	if ( found && rc )
	{ rc = ( PL_unify_nil(tail) &&
		 printMessage(ATOM_warning,
			      PL_FUNCTOR_CHARS, "not_accessed_flags", 1,
				PL_TERM, list) );
	}

	PL_discard_foreign_frame(fid);
      }
    }
  }

  return rc;
}


void
cleanupPrologFlags(void)
{ if ( GD->prolog_flag.table )
  { TableWP t = GD->prolog_flag.table;

    GD->prolog_flag.table = NULL;
#ifdef O_PLMT
    t->free_symbol = freeSymbolPrologFlagTable;
#else
    t->free_symbol = NULL;
#endif
    destroyHTableWP(t);
  }
}



		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

#define NDET PL_FA_NONDETERMINISTIC

BeginPredDefs(prologflag)
  PRED_DEF("set_prolog_flag",      2, set_prolog_flag,      PL_FA_ISO)
  PRED_DEF("create_prolog_flag",   3, create_prolog_flag,   0)
  PRED_DEF("current_prolog_flag",  2, current_prolog_flag,  PL_FA_ISO|NDET)
  PRED_DEF("$current_prolog_flag", 5, dcurrent_prolog_flag, NDET)
EndPredDefs
