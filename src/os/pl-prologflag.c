/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2017, University of Amsterdam
                              VU University Amsterdam
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
#include "pl-incl.h"
#include "pl-ctype.h"
#include <ctype.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef __WINDOWS__
#include <process.h>			/* getpid() */
#endif
#include <time.h>


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
static void setTZPrologFlag(void);
static void setVersionPrologFlag(void);
static void initPrologFlagTable(void);


typedef struct _prolog_flag
{ short		flags;			/* Type | Flags */
  short		index;			/* index in PLFLAG_ mask */
  union
  { atom_t	a;			/* value as atom */
    int64_t	i;			/* value as integer */
    double	f;			/* value as float */
    record_t	t;			/* value as term */
  } value;
} prolog_flag;


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

static int
indexOfBoolMask(unsigned int mask)
{ int i=1;

  if ( !mask )
    return -1;

  while(!(mask & 0x1))
  { i++;
    mask >>= 1;
  }
  return i;
}


void
setPrologFlag(const char *name, int flags, ...)
{ GET_LD
  atom_t an = PL_new_atom(name);
  prolog_flag *f;
  va_list args;
  int type = (flags & FT_MASK);
  int first_def = FALSE;

  initPrologFlagTable();

  if ( type == FT_INT64 )
    flags = (flags & ~FT_MASK)|FT_INTEGER;

  if ( (f = lookupHTable(GD->prolog_flag.table, (void *)an)) )
  { assert((f->flags & FT_MASK) == (flags & FT_MASK));
    if ( flags & FF_KEEP )
      return;
  } else
  { f = allocHeapOrHalt(sizeof(*f));
    f->index = -1;
    f->flags = flags;
    addNewHTable(GD->prolog_flag.table, (void *)an, f);
    first_def = TRUE;
  }

  va_start(args, flags);
  switch(type)
  { case FT_BOOL:
    { int           val = va_arg(args, int);
      unsigned int mask = va_arg(args, unsigned int);

      if ( !first_def && mask && f->index < 0 )	/* type definition */
      { f->index = indexOfBoolMask(mask);
	val = (f->value.a == ATOM_true);
      } else if ( first_def )			/* 1st definition */
      { f->index = indexOfBoolMask(mask);
	DEBUG(MSG_PROLOG_FLAG,
	      Sdprintf("Prolog flag %s at 0x%08lx\n", name, mask));
      }

      f->value.a = (val ? ATOM_true : ATOM_false);
      if ( f->index >= 0 )
      { mask = (unsigned int)1 << (f->index-1);

	if ( val )
	  setPrologFlagMask(mask);
	else
	  clearPrologFlagMask(mask);
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
freePrologFlag(prolog_flag *f)
{ switch((f->flags & FT_MASK))
  { case FT_TERM:
      PL_erase(f->value.t);
      break;
    case FT_ATOM:
      PL_unregister_atom(f->value.a);
      break;
    default:
      ;
  }

  freeHeap(f, sizeof(*f));
}


#ifdef O_PLMT
static prolog_flag *
copy_prolog_flag(const prolog_flag *f)
{ prolog_flag *copy = allocHeapOrHalt(sizeof(*copy));

  *copy = *f;
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
copySymbolPrologFlagTable(void *name, void **value)
{ atom_t key = (atom_t)name;
  prolog_flag *f = *value;

  PL_register_atom(key);
  *value = copy_prolog_flag(f);
}


static void
freeSymbolPrologFlagTable(void *name, void *value)
{ atom_t key = (atom_t)name;

  PL_unregister_atom(key);
  freePrologFlag(value);
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
setFileNameCaseHandling(atom_t a)
{ GET_LD

  if ( a == ATOM_case_sensitive )
  { setPrologFlagMask(PLFLAG_FILE_CASE|PLFLAG_FILE_CASE_PRESERVING);
  } else if ( a == ATOM_case_preserving )
  { setPrologFlagMask(PLFLAG_FILE_CASE_PRESERVING);
    clearPrologFlagMask(PLFLAG_FILE_CASE);
  } else if ( a == ATOM_case_insensitive )
  { clearPrologFlagMask(PLFLAG_FILE_CASE|PLFLAG_FILE_CASE_PRESERVING);
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

  switch ( LD->prolog_flag.mask.flags &
	   (PLFLAG_FILE_CASE|PLFLAG_FILE_CASE_PRESERVING) )
  { case 0:
      return ATOM_case_insensitive;
    case PLFLAG_FILE_CASE_PRESERVING:
      return ATOM_case_preserving;
    case PLFLAG_FILE_CASE|PLFLAG_FILE_CASE_PRESERVING:
      return ATOM_case_sensitive;
    default:
      return ATOM_unknown;
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
  IOENC enc = atom_to_encoding(a);

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

static word
set_prolog_flag_unlocked(term_t key, term_t value, int flags)
{ GET_LD
  atom_t k;
  prolog_flag *f;
  Module m = MODULE_parse;
  int rval = TRUE;

  if ( !PL_strip_module(key, &m, key) )
    return FALSE;
  if ( !PL_get_atom(key, &k) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, key);

					/* set existing Prolog flag */
#ifdef O_PLMT
  if ( LD->prolog_flag.table &&
       (f = lookupHTable(LD->prolog_flag.table, (void *)k)) )
  { if ( flags & FF_KEEP )
      return TRUE;
  } else
#endif
  if ( (f = lookupHTable(GD->prolog_flag.table, (void *)k)) )
  { if ( flags & FF_KEEP )
      return TRUE;
    if ( (f->flags&FF_READONLY) && !(flags&FF_FORCE) )
      return PL_error(NULL, 0, NULL, ERR_PERMISSION,
		      ATOM_modify, ATOM_flag, key);

#ifdef O_PLMT
    if ( GD->statistics.threads_created > 1 )
    { f = copy_prolog_flag(f);

      if ( !LD->prolog_flag.table )
      { LD->prolog_flag.table = newHTable(4);

	LD->prolog_flag.table->copy_symbol = copySymbolPrologFlagTable;
	LD->prolog_flag.table->free_symbol = freeSymbolPrologFlagTable;
      }

      addNewHTable(LD->prolog_flag.table, (void *)k, f);
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
    f->index = -1;

    switch( (flags & FT_MASK) )
    { case FT_FROM_VALUE:
      { if ( PL_get_atom(value, &a) )
	{ if ( a == ATOM_true || a == ATOM_false ||
	       a == ATOM_on || a == ATOM_off )
	    f->flags = FT_BOOL;
	  else
	    f->flags = FT_ATOM;
	  f->value.a = a;
	  PL_register_atom(a);
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

    if ( (flags & FF_READONLY) )
      f->flags |= FF_READONLY;

    addNewHTable(GD->prolog_flag.table, (void *)k, f);
    if ( !(lookupHTable(GD->prolog_flag.table, (void *)k) == f) )
    { freePrologFlag(f);
      Sdprintf("OOPS; failed to set Prolog flag!?\n");
    }

    succeed;
  } else
  { atom_t how;

    if ( PL_current_prolog_flag(ATOM_user_flags, PL_ATOM, &how) )
    { if ( how == ATOM_error )
	return PL_error(NULL, 0, NULL, ERR_EXISTENCE,
			ATOM_prolog_flag, key);
      else if ( how == ATOM_warning )
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
	{ debugmode(DBG_ALL, NULL);
	} else
	{ tracemode(FALSE, NULL);
	  debugmode(DBG_OFF, NULL);
	}
      } else if ( k == ATOM_debugger_show_context )
      { debugstatus.showContext = val;
#ifdef O_PLMT
      } else if ( k == ATOM_threads )
      { if ( !(rval = enableThreads(val)) )
	  break;			/* don't change value */
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
      } else if ( k == ATOM_protect_static_code )
      { if ( val != (f->value.a == ATOM_true) && val == FALSE )
	{ term_t ex;

	  if ( (ex = PL_new_term_ref()) &&
	       PL_put_atom(ex, ATOM_protect_static_code) )
	    return PL_permission_error("set", "prolog_flag", ex);
	  return FALSE;
	}
      }
					/* set the flag value */
      if ( f->index > 0 )
      { unsigned int mask = (unsigned int)1 << (f->index-1);

	if ( val )
	  setPrologFlagMask(mask);
	else
	  clearPrologFlagMask(mask);
      }
      f->value.a = (val ? ATOM_true : ATOM_false);

      break;
    }
    case FT_ATOM:
    { atom_t a;

      if ( !PL_get_atom_ex(value, &a) )
	return FALSE;

      if ( k == ATOM_double_quotes )
      { rval = setDoubleQuotes(a, &m->flags);
      } else if ( k == ATOM_back_quotes )
      { rval = setBackQuotes(a, &m->flags);
      } else if ( k == ATOM_unknown )
      { rval = setUnknown(value, a, m);
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
#if O_XOS
      } else if ( k == ATOM_win_file_access_check )
      { rval = set_win_file_access_check(value);
#endif
      }
      if ( !rval )
	fail;

      PL_unregister_atom(f->value.a);
      f->value.a = a;
      PL_register_atom(a);
      break;
    }
    case FT_INTEGER:
    { int64_t i;

      if ( !PL_get_int64_ex(value, &i) )
	return FALSE;
      f->value.i = i;
#ifdef O_ATOMGC
      if ( k == ATOM_agc_margin )
	GD->atoms.margin = (size_t)i;
      else
#endif
      if ( k == ATOM_table_space )
	LD->tabling.node_pool.limit = (size_t)i;
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

  return rval;
}


int
set_prolog_flag(term_t key, term_t value, int flags)
{ int rc;

  PL_LOCK(L_PLFLAG);
  rc = set_prolog_flag_unlocked(key, value, flags);
  PL_UNLOCK(L_PLFLAG);

  return rc;
}

/** set_prolog_flag(+Key, +Value) is det.
*/

static
PRED_IMPL("set_prolog_flag", 2, set_prolog_flag, PL_FA_ISO)
{ return set_prolog_flag(A1, A2, FF_NOCREATE|FT_FROM_VALUE);
}


/** create_prolog_flag(+Key, +Value, +Options) is det.
*/

static const opt_spec prolog_flag_options[] =
{ { ATOM_type,   OPT_ATOM },
  { ATOM_access, OPT_ATOM },
  { ATOM_keep,   OPT_BOOL },
  { NULL_ATOM,   0 }
};

static
PRED_IMPL("create_prolog_flag", 3, create_prolog_flag, PL_FA_ISO)
{ PRED_LD
  int flags = 0;
  atom_t type = 0;
  atom_t access = ATOM_read_write;
  int keep = FALSE;

  if ( !scan_options(A3, 0, ATOM_prolog_flag_option, prolog_flag_options,
		     &type, &access, &keep) )
    return FALSE;

  if ( type == 0 )
    flags |= FT_FROM_VALUE;
  else if ( type == ATOM_boolean )
    flags |= FT_BOOL;
  else if ( type == ATOM_integer )
    flags |= FT_INTEGER;
  else if ( type == ATOM_float )
    flags |= FT_FLOAT;
  else if ( type == ATOM_atom )
    flags |= FT_ATOM;
  else if ( type == ATOM_term )
    flags |= FT_TERM;
  else
  { term_t a = PL_new_term_ref();
    PL_put_atom(a, type);

    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_prolog_flag_type, a);
  }

  if ( access == ATOM_read_only )
    flags |= FF_READONLY;
  else if ( access != ATOM_read_write )
  { term_t a = PL_new_term_ref();
    PL_put_atom(a, access);
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_prolog_flag_access, a);
  }

  if ( keep )
    flags |= FF_KEEP;

  return set_prolog_flag(A1, A2, flags);
}


static prolog_flag *
lookupFlag(atom_t key)
{ GET_LD
#ifdef O_PLMT
  prolog_flag *f = NULL;

  if ( LD->prolog_flag.table &&
       (f = lookupHTable(LD->prolog_flag.table, (void *)key)) )
  { return f;
  } else
#endif
  { return lookupHTable(GD->prolog_flag.table, (void *)key);
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



static int
unify_prolog_flag_value(Module m, atom_t key, prolog_flag *f, term_t val)
{ GET_LD

  if ( key == ATOM_character_escapes )
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
  }

  switch(f->flags & FT_MASK)
  { case FT_BOOL:
      if ( f->index >= 0 )
      { unsigned int mask = (unsigned int)1 << (f->index-1);

	return PL_unify_bool_ex(val, truePrologFlag(mask) != FALSE);
      }
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

word
pl_prolog_flag5(term_t key, term_t value,
	    word scope, word access, word type,
	    control_t h)
{ GET_LD
  prolog_flag_enum *e;
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
	     (f = lookupHTable(LD->prolog_flag.table, (void *)k)) )
	  return unify_prolog_flag_value(module, k, f, value);
#endif
	if ( (f = lookupHTable(GD->prolog_flag.table, (void *)k)) )
	{ if ( unify_prolog_flag_value(module, k, f, value) &&
	       (!access || unify_prolog_flag_access(f, access)) &&
	       (!type   || unify_prolog_flag_type(f, type)) )
	    succeed;
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
	  e->table_enum = newTableEnum(LD->prolog_flag.table);
	else
	  e->table_enum = newTableEnum(GD->prolog_flag.table);

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
  { atom_t fn;
    prolog_flag *f;
    while( advanceTableEnum(e->table_enum, (void**)&fn, (void**)&f) )
    { if ( e->explicit_scope == FALSE &&
	   e->scope == ATOM_global &&
	   LD->prolog_flag.table &&
	   lookupHTable(LD->prolog_flag.table, (void *)fn) )
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
      e->table_enum = newTableEnum(GD->prolog_flag.table);
    } else
      break;
  }
  PL_UNLOCK(L_PLFLAG);

  freeTableEnum(e->table_enum);
  freeHeap(e, sizeof(*e));

  fail;
}


foreign_t
pl_prolog_flag(term_t name, term_t value, control_t h)
{ return pl_prolog_flag5(name, value, 0, 0, 0, h);
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

static void
initPrologFlagTable(void)
{ if ( !GD->prolog_flag.table )
  { initPrologThreads();	/* may be called before PL_initialise() */

    GD->prolog_flag.table = newHTable(64);
  }
}


void
initPrologFlags(void)
{ GET_LD
  setPrologFlag("iso",  FT_BOOL, FALSE, PLFLAG_ISO);
  setPrologFlag("arch", FT_ATOM|FF_READONLY, PLARCH);
#if __WINDOWS__
  setPrologFlag("windows",	FT_BOOL|FF_READONLY, TRUE, 0);
#endif
#if O_XOS
  setPrologFlag("win_file_access_check", FT_ATOM,
		get_win_file_access_check(), 0);
#endif
  setPrologFlag("file_name_case_handling", FT_ATOM,
		stringAtom(currentFileNameCaseHandling()));
  setPrologFlag("version", FT_INTEGER|FF_READONLY, PLVERSION);
  setPrologFlag("dialect", FT_ATOM|FF_READONLY, "swi");
  if ( systemDefaults.home )
    setPrologFlag("home", FT_ATOM|FF_READONLY, systemDefaults.home);
  if ( GD->paths.executable )
    setPrologFlag("executable", FT_ATOM|FF_READONLY, GD->paths.executable);
#if defined(HAVE_GETPID) || defined(EMULATE_GETPID)
  setPrologFlag("pid", FT_INTEGER|FF_READONLY, getpid());
#endif
  setPrologFlag("optimise", FT_BOOL, GD->cmdline.optimise, PLFLAG_OPTIMISE);
  setPrologFlag("optimise_debug", FT_ATOM, "default", 0);
  setPrologFlag("generate_debug_info", FT_BOOL,
		truePrologFlag(PLFLAG_DEBUGINFO), PLFLAG_DEBUGINFO);
  setPrologFlag("protect_static_code", FT_BOOL, FALSE,
		PLFLAG_PROTECT_STATIC_CODE);
  setPrologFlag("last_call_optimisation", FT_BOOL, TRUE, PLFLAG_LASTCALL);
  setPrologFlag("warn_override_implicit_import", FT_BOOL, TRUE,
		PLFLAG_WARN_OVERRIDE_IMPLICIT_IMPORT);
  setPrologFlag("c_cc",	     FT_ATOM, C_CC);
  setPrologFlag("c_libs",    FT_ATOM, C_LIBS);
  setPrologFlag("c_libplso", FT_ATOM, C_LIBPLSO);
  setPrologFlag("c_ldflags", FT_ATOM, C_LDFLAGS);
  setPrologFlag("c_cflags",  FT_ATOM, C_CFLAGS);
#if defined(O_LARGEFILES) || SIZEOF_LONG == 8
  setPrologFlag("large_files", FT_BOOL|FF_READONLY, TRUE, 0);
#endif
  setPrologFlag("unload_foreign_libraries", FT_BOOL, FALSE, 0);
  setPrologFlag("gc",	  FT_BOOL,	       TRUE,  PLFLAG_GC);
  setPrologFlag("trace_gc",  FT_BOOL,	       FALSE, PLFLAG_TRACE_GC);
#ifdef O_ATOMGC
  setPrologFlag("agc_margin",FT_INTEGER,	       GD->atoms.margin);
#endif
  setPrologFlag("table_space", FT_INTEGER, LD->tabling.node_pool.limit);
#if defined(HAVE_DLOPEN) || defined(HAVE_SHL_LOAD) || defined(EMULATE_DLOPEN)
  setPrologFlag("open_shared_object",	  FT_BOOL|FF_READONLY, TRUE, 0);
  setPrologFlag("shared_object_extension",	  FT_ATOM|FF_READONLY, SO_EXT);
  setPrologFlag("shared_object_search_path", FT_ATOM|FF_READONLY, SO_PATH);
#endif
  setPrologFlag("address_bits", FT_INTEGER|FF_READONLY, sizeof(void*)*8);
#ifdef HAVE_POPEN
  setPrologFlag("pipe", FT_BOOL, TRUE, 0);
#endif
#ifdef O_PLMT
  setPrologFlag("threads",	FT_BOOL|FF_READONLY, TRUE, 0);
  setPrologFlag("system_thread_id", FT_INTEGER|FF_READONLY, 0, 0);
  setPrologFlag("gc_thread",    FT_BOOL,
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
  setPrologFlag("break_level", FT_INTEGER|FF_READONLY, 0, 0);
  setPrologFlag("user_flags", FT_ATOM, "silent");
  setPrologFlag("editor", FT_ATOM, "default");
  setPrologFlag("debugger_show_context", FT_BOOL, FALSE, 0);
  setPrologFlag("autoload",  FT_BOOL, TRUE,  PLFLAG_AUTOLOAD);
#ifndef O_GMP
  setPrologFlag("max_integer",	   FT_INT64|FF_READONLY, PLMAXINT);
  setPrologFlag("min_integer",	   FT_INT64|FF_READONLY, PLMININT);
#endif
  setPrologFlag("max_tagged_integer", FT_INTEGER|FF_READONLY, PLMAXTAGGEDINT);
  setPrologFlag("min_tagged_integer", FT_INTEGER|FF_READONLY, PLMINTAGGEDINT);
#ifdef O_GMP
  setPrologFlag("bounded",		   FT_BOOL|FF_READONLY,	   FALSE, 0);
#ifdef __GNU_MP__
  setPrologFlag("gmp_version",	   FT_INTEGER|FF_READONLY, __GNU_MP__);
#endif
#else
  setPrologFlag("bounded",		   FT_BOOL|FF_READONLY,	   TRUE, 0);
#endif
  if ( (-3 / 2) == -2 )
    setPrologFlag("integer_rounding_function", FT_ATOM|FF_READONLY, "down");
  else
    setPrologFlag("integer_rounding_function", FT_ATOM|FF_READONLY, "toward_zero");
  setPrologFlag("max_arity", FT_ATOM|FF_READONLY, "unbounded");
  setPrologFlag("answer_format", FT_ATOM, "~p");
  setPrologFlag("colon_sets_calling_context", FT_BOOL|FF_READONLY, TRUE, 0);
  setPrologFlag("character_escapes", FT_BOOL, TRUE, PLFLAG_CHARESCAPE);
  setPrologFlag("var_prefix", FT_BOOL, FALSE, PLFLAG_VARPREFIX);
  setPrologFlag("char_conversion", FT_BOOL, FALSE, PLFLAG_CHARCONVERSION);
#ifdef O_QUASIQUOTATIONS
  setPrologFlag("quasi_quotations", FT_BOOL, TRUE, PLFLAG_QUASI_QUOTES);
#endif
  setPrologFlag("write_attributes", FT_ATOM, "ignore");
  setPrologFlag("stream_type_check", FT_ATOM, "loose");
  setPrologFlag("occurs_check", FT_ATOM, "false");
  setPrologFlag("access_level", FT_ATOM, "user");
  setPrologFlag("double_quotes", FT_ATOM,
		GD->options.traditional ? "codes" : "string");
  setPrologFlag("back_quotes", FT_ATOM,
		GD->options.traditional ? "symbol_char" : "codes");
  setPrologFlag("traditional", FT_BOOL|FF_READONLY, GD->options.traditional, 0);
  setPrologFlag("unknown", FT_ATOM, "error");
  setPrologFlag("debug", FT_BOOL, FALSE, 0);
  setPrologFlag("verbose", FT_ATOM|FF_KEEP, GD->options.silent ? "silent" : "normal");
  setPrologFlag("verbose_load", FT_ATOM, "silent");
  setPrologFlag("verbose_autoload", FT_BOOL, FALSE, 0);
  setPrologFlag("verbose_file_search", FT_BOOL, FALSE, 0);
  setPrologFlag("sandboxed_load", FT_BOOL, FALSE, 0);
  setPrologFlag("allow_variable_name_as_functor", FT_BOOL, FALSE,
		ALLOW_VARNAME_FUNCTOR);
  setPrologFlag("allow_dot_in_atom", FT_BOOL, FALSE,
		PLFLAG_DOT_IN_ATOM);
  setPrologFlag("toplevel_var_size", FT_INTEGER, 1000);
  setPrologFlag("toplevel_print_anon", FT_BOOL, TRUE, 0);
  setPrologFlag("toplevel_prompt", FT_ATOM, "~m~d~l~! ?- ");
  setPrologFlag("file_name_variables", FT_BOOL, FALSE, PLFLAG_FILEVARS);
  setPrologFlag("fileerrors", FT_BOOL, TRUE, PLFLAG_FILEERRORS);
#ifdef __unix__
  setPrologFlag("unix", FT_BOOL|FF_READONLY, TRUE, 0);
#endif
#ifdef __APPLE__
  setPrologFlag("apple", FT_BOOL|FF_READONLY, TRUE, 0);
#endif

  setPrologFlag("encoding", FT_ATOM, stringAtom(encoding_to_atom(LD->encoding)));

  setPrologFlag("tty_control", FT_BOOL,
		truePrologFlag(PLFLAG_TTY_CONTROL), PLFLAG_TTY_CONTROL);
  setPrologFlag("signals", FT_BOOL|FF_READONLY,
		truePrologFlag(PLFLAG_SIGNALS), PLFLAG_SIGNALS);

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

  setTZPrologFlag();
  setOSPrologFlags();
  setVersionPrologFlag();
  setArgvPrologFlag("os_argv", GD->cmdline.os_argc,   GD->cmdline.os_argv);
  setArgvPrologFlag("argv",    GD->cmdline.appl_argc, GD->cmdline.appl_argv);
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
    if ( !PL_unify_chars(e, PL_ATOM|REP_FN, -1, argv[n]) ||
	 !PL_cons_list(l, e, l) )
      fatalError("Could not set Prolog flag argv: not enough stack");
  }

  setPrologFlag(flag, FT_TERM, l);
  PL_discard_foreign_frame(fid);
}


static void
setTZPrologFlag(void)
{ tzset();

  setPrologFlag("timezone", FT_INTEGER|FF_READONLY, timezone);
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


void
cleanupPrologFlags(void)
{ if ( GD->prolog_flag.table )
  { Table t = GD->prolog_flag.table;

    GD->prolog_flag.table = NULL;
#ifdef O_PLMT
    t->free_symbol = freeSymbolPrologFlagTable;
#else
    t->free_symbol = NULL;
#endif
    destroyHTable(t);
  }
}



		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(prologflag)
  PRED_DEF("set_prolog_flag",    2, set_prolog_flag,    PL_FA_ISO)
  PRED_DEF("create_prolog_flag", 3, create_prolog_flag, 0)
EndPredDefs
