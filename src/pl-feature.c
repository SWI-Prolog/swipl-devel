/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

/*#define O_DEBUG 1*/
#include "pl-incl.h"

#define LOCK()   PL_LOCK(L_FEATURE)
#define UNLOCK() PL_UNLOCK(L_FEATURE)


		 /*******************************
		 *	 FEATURE HANDLING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Features (ISO Prolog prolog_flags) are properties  of the running Prolog
system. Some of these features can be set   by the user, such as whether
read/1 honours character-escapes, whether garbage-collection is enabled,
etc. Some are global and read-only, such as whether the operating system
is unix.

In  the  multi-threading  version,   features    have   to   be  changed
thread-local. Therefore two feature-tables have   been defined: a global
one which is used as long as there is   only one thread, and a local one
that is used to write changes to after multiple threads exist. On thread
creation this table is copied from  the   parent  and on destruction the
local table is destroyed.  Note  that   the  flag-mask  for  fast access
(trueFeature(*_FEATURE)) is always copied to the local thread-data.

Altogether  this  module  is  a  bit  too  complex,  but  I  see  little
alternative. I considered creating  copy-on-write   hash-tables,  but in
combination to the table-enumator  objects  this   proves  very  hard to
implement safely. Using plain Prolog is not  a good option too: they are
used before we can  use  any  Prolog   at  startup,  predicates  are not
thread-local and some of the features require   very  fast access from C
(the booleans in the mask).

Just using a local table and  copy   it  on  thread-creation would be an
option, but 90% of the features are   read-only  or never changed and we
want to be able to have a lot and don't harm thread_create/3 too much.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void	setArgvFeature();

typedef struct _feature
{ short		flags;			/* Type | Flags */
  short		index;			/* index in _FEATURE mask */
  union
  { atom_t	a;			/* value as atom */
    long	i;			/* value as integer */
    record_t	t;			/* value as term */
  } value;
} feature;


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C-interface for defining features.  Depending on the type, the
following arguments are to be provided:

    FT_BOOL	TRUE/FALSE, *_FEATURE
    FT_INTEGER  long
    FT_ATOM	const char *
    FT_TERM	a term
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
indexOfBoolMask(unsigned long mask)
{ int i=1;
  
  if ( !mask )
    return -1;

  while(!(mask & 0x1L))
  { i++;
    mask >>= 1;
  }
  return i;
}


void
defFeature(const char *name, int flags, ...)
{ atom_t an = PL_new_atom(name);
  feature *f;
  Symbol s;
  va_list args;
  int type = (flags & FT_MASK);

  if ( (s = lookupHTable(GD->feature.table, (void *)an)) )
  { f = s->value;
    assert((f->flags & FT_MASK) == (flags & FT_MASK));
  } else
  { f = allocHeap(sizeof(*f));
    f->index = -1;
    f->flags = flags;
    addHTable(GD->feature.table, (void *)an, f);
  }
  
  va_start(args, flags);
  switch(type)
  { case FT_BOOL:
    { int  val           = va_arg(args, int);
      unsigned long mask = va_arg(args, unsigned long);

      if ( !s )
      { f->index = indexOfBoolMask(mask);
	DEBUG(2, Sdprintf("Feature %s at 0x%08lx\n", name, mask));
      }
      f->value.a = (val ? ATOM_true : ATOM_false);
      if ( f->index > 0 )
      { mask = 1L << (f->index-1);

	if ( val )
	  setFeatureMask(mask);
	else
	  clearFeatureMask(mask);
      }
      break;
    }
    case FT_INTEGER:
    { long val = va_arg(args, long);
      f->value.i = val;
      break;
    }
    case FT_ATOM:
    { const char *text = va_arg(args, const char *);

      f->value.a = PL_new_atom(text);	/* registered: ok */
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


#ifdef O_PLMT
static void
copySymbolFeatureTable(Symbol s)
{ feature *f = s->value;
  feature *copy = allocHeap(sizeof(*copy));

  *copy = *f;
  if ( (f->flags & FT_MASK) == FT_TERM )
    copy->value.t = PL_duplicate_record(f->value.t);
  s->value = copy;
}


static void
freeSymbolFeatureTable(Symbol s)
{ feature *f = s->value;

  if ( (f->flags & FT_MASK) == FT_TERM )
    PL_erase(f->value.t);

  freeHeap(f, sizeof(*f));
}
#endif


int
setDoubleQuotes(atom_t a, unsigned int *flagp)
{ unsigned int flags;

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


static int
setUnknown(atom_t a, unsigned int *flagp)
{ unsigned int flags;

  if ( a == ATOM_error )
    flags = UNKNOWN_ERROR;
  else if ( a == ATOM_warning )
    flags = UNKNOWN_WARNING;
  else if ( a == ATOM_fail )
    flags = 0;
  else
  { term_t value = PL_new_term_ref();

    PL_put_atom(value, a);
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_unknown, value);
  }

  *flagp &= ~(UNKNOWN_ERROR|UNKNOWN_WARNING);
  *flagp |= flags;

  succeed;
}


word
pl_set_feature(term_t key, term_t value)
{ atom_t k;
  Symbol s;
  feature *f;
  Module m = MODULE_parse;

  PL_strip_module(key, &m, key);
  if ( !PL_get_atom(key, &k) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, key);

  LOCK();
					/* set existing feature */
#ifdef O_PLMT
  if ( LD->feature.table &&
       (s = lookupHTable(LD->feature.table, (void *)k)) )
  { f = s->value;			/* already local feature */
  } else
#endif
  if ( (s = lookupHTable(GD->feature.table, (void *)k)) )
  { f = s->value;
    if ( f->flags & FF_READONLY )
      return PL_error(NULL, 0, NULL, ERR_PERMISSION,
		      ATOM_modify, ATOM_flag, key);

#ifdef O_PLMT    
    if ( GD->statistics.threads_created > 1 )
    { feature *f2 = allocHeap(sizeof(*f2));

      *f2 = *f;
      if ( (f2->flags & FT_MASK) == FT_TERM )
	f2->value.t = PL_duplicate_record(f2->value.t);

      if ( !LD->feature.table )
      { LD->feature.table = newHTable(4);

	LD->feature.table->copy_symbol = copySymbolFeatureTable;
	LD->feature.table->free_symbol = freeSymbolFeatureTable;
      }

      addHTable(LD->feature.table, (void *)k, f2);
      DEBUG(1, Sdprintf("Localised feature %s\n", PL_atom_chars(k)));
      f = f2;
    }
#endif
  } else				/* define new feature */
  { feature *f = allocHeap(sizeof(*f));
    atom_t a;
    long i;

    f->index = -1;
    if ( PL_get_atom(value, &a) )
    { if ( a == ATOM_true || a == ATOM_false || a == ATOM_on || a == ATOM_off )
	f->flags = FT_BOOL;
      else
	f->flags = FT_ATOM;
      f->value.a = a;
      PL_register_atom(a);
    } else if ( PL_get_long(value, &i) )
    { f->flags = FT_INTEGER;
      f->value.i = i;
    } else
    { f->flags = FT_TERM;
      f->value.t = PL_record(value);
    }

#ifdef O_PLMT
    if ( GD->statistics.threads_created > 1 )
    { if ( !LD->feature.table )
      { LD->feature.table = newHTable(4);

	LD->feature.table->copy_symbol = copySymbolFeatureTable;
	LD->feature.table->free_symbol = freeSymbolFeatureTable;
      }
      addHTable(LD->feature.table, (void *)k, f);
    } else
#endif
      addHTable(GD->feature.table, (void *)k, f);

    UNLOCK();
    succeed;
  }

  switch(f->flags & FT_MASK)
  { case FT_BOOL:
    { int val;

      if ( !PL_get_bool(value, &val) )
	return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_bool, value);
      f->value.a = (val ? ATOM_true : ATOM_false);
      if ( f->index > 0 )
      { unsigned long mask = 1L << (f->index-1);

	if ( val )
	  setFeatureMask(mask);
	else
	  clearFeatureMask(mask);
      }
      if ( k == ATOM_character_escapes )
      { if ( val )
	  set(m, CHARESCAPE);
	else
	  clear(m, CHARESCAPE);
      } else if ( k == ATOM_debug )
      { if ( val )
	{ debugmode(TRUE, NULL);
	} else
	{ tracemode(FALSE, NULL);
	  debugmode(FALSE, NULL);
	}
      } else if ( k == ATOM_debugger_show_context )
      { debugstatus.showContext = val;
      }

      break;
    }
    case FT_ATOM:
    { atom_t a;

      if ( !PL_get_atom(value, &a) )
	return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, value);
      PL_unregister_atom(f->value.a);
      f->value.a = a;
      PL_register_atom(a);
      if ( k == ATOM_float_format )
      { PL_register_atom(a);		/* so it will never be lost! */
	LD->float_format = PL_atom_chars(a);
      } else if ( k == ATOM_double_quotes )
      { if ( !setDoubleQuotes(a, &m->flags) )
	{ UNLOCK();
	  fail;
	}
      } else if ( k == ATOM_unknown )
      { if ( !setUnknown(a, &m->flags) )
	{ UNLOCK();
	  fail;
	}
      }
      break;
    }
    case FT_INTEGER:
    { long i;

      if ( !PL_get_long(value, &i) )
	return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_integer, value);
      f->value.i = i;
#ifdef O_ATOMGC
      if ( k == ATOM_agc_margin )
	GD->atoms.margin = i;
#endif
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
  UNLOCK();

  succeed;
}


static int
unify_feature_value(Module m, atom_t key, feature *f, term_t val)
{ if ( key == ATOM_character_escapes )
  { atom_t v = (true(m, CHARESCAPE) ? ATOM_true : ATOM_false);

    return PL_unify_atom(val, v);
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
  } else if ( key == ATOM_unknown )
  { atom_t v;

    if ( true(m, UNKNOWN_ERROR) )
      v = ATOM_error;
    else if ( true(m, UNKNOWN_WARNING) )
      v = ATOM_warning;
    else
      v = ATOM_fail;

    return PL_unify_atom(val, v);
  } else if ( key == ATOM_debug )
  { return PL_unify_bool_ex(val, debugstatus.debugging);
  } else if ( key == ATOM_debugger_show_context )
  { return PL_unify_bool_ex(val, debugstatus.showContext);
  }

  switch(f->flags & FT_MASK)
  { case FT_BOOL:
    case FT_ATOM:
      return PL_unify_atom(val, f->value.a);
    case FT_INTEGER:
      return PL_unify_integer(val, f->value.i);
    case FT_TERM:
    { term_t tmp = PL_new_term_ref();

      PL_recorded(f->value.t, tmp);
      return PL_unify(val, tmp);
    }
    default:
      assert(0);
      fail;
  }
}


static int
unify_feature_access(feature *f, term_t access)
{ if ( f->flags & FF_READONLY )
    return PL_unify_atom(access, ATOM_read);
  else
    return PL_unify_atom(access, ATOM_write);
}


static int
unify_feature_type(feature *f, term_t type)
{ atom_t a;

  switch(f->flags & FT_MASK)
  { case FT_BOOL:
      a = ATOM_bool;
      break;
    case FT_ATOM:
      a = ATOM_atom;
      break;
    case FT_INTEGER:
      a = ATOM_integer;
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
} feature_enum;

word
pl_feature5(term_t key, term_t value,
	    word scope, word access, word type,
	    word h)
{ feature_enum *e;
  Symbol s;
  mark m;
  Module module;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
    { atom_t k;

      module = MODULE_parse;
      PL_strip_module(key, &module, key);

      if ( PL_get_atom(key, &k) )
      { Symbol s;

#ifdef O_PLMT
	if ( LD->feature.table &&
	     (s = lookupHTable(LD->feature.table, (void *)k)) )
	  return unify_feature_value(module, k, s->value, value);
#endif
	if ( (s = lookupHTable(GD->feature.table, (void *)k)) )
	  return unify_feature_value(module, k, s->value, value);
	else
	  fail;
      } else if ( PL_is_variable(key) )
      { e = allocHeap(sizeof(*e));

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

	  if ( LD->feature.table )
	    e->scope = ATOM_local;
	  else
	    e->scope = ATOM_global;
	}
	  
	if ( e->scope == ATOM_local )
	  e->table_enum = newTableEnum(LD->feature.table);
	else
	  e->table_enum = newTableEnum(GD->feature.table);

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

  Mark(m);
  LOCK();
  for(;;)
  { while( (s=advanceTableEnum(e->table_enum)) )
    { atom_t fn = (atom_t) s->name;

      if ( e->explicit_scope == FALSE &&
	   e->scope == ATOM_global &&
	   LD->feature.table &&
	   lookupHTable(LD->feature.table, (void *)fn) )
	continue;

      if ( PL_unify_atom(key, fn) &&
	   unify_feature_value(e->module, fn, s->value, value) &&
	   (!scope  || PL_unify_atom(scope, e->scope)) &&
	   (!access || unify_feature_access(s->value, access)) &&
	   (!type   || unify_feature_type(s->value, type)) )
      { UNLOCK();
	ForeignRedoPtr(e);
      }
      Undo(m);
    }

    if ( e->scope == ATOM_local )
    { e->scope = ATOM_global;
      freeTableEnum(e->table_enum);
      e->table_enum = newTableEnum(GD->feature.table);
    } else
      break;
  }
  UNLOCK();
  
  freeTableEnum(e->table_enum);
  freeHeap(e, sizeof(*e));

  fail;
}


foreign_t
pl_feature(term_t name, term_t value, word h)
{ return pl_feature5(name, value, 0, 0, 0, h);
}


		 /*******************************
		 *	INITIALISE FEATURES	*
		 *******************************/

#ifndef SO_EXT
#define SO_EXT "so"
#endif

void
initFeatures()
{ GD->feature.table = newHTable(32);

  defFeature("iso",  FT_BOOL, FALSE, ISO_FEATURE);
  defFeature("arch", FT_ATOM|FF_READONLY, ARCH);
#if __WIN32__
  if ( iswin32s() )
    defFeature("win32s", FT_BOOL|FF_READONLY, TRUE, 0);
  defFeature("windows",	FT_BOOL|FF_READONLY, TRUE, 0);
#endif
  defFeature("version",	FT_INTEGER|FF_READONLY, PLVERSION);
  if ( systemDefaults.home )
    defFeature("home", FT_ATOM|FF_READONLY, systemDefaults.home);
  if ( GD->paths.executable )
    defFeature("executable", FT_ATOM|FF_READONLY, GD->paths.executable);

  defFeature("optimise", FT_BOOL, GD->cmdline.optimise, OPTIMISE_FEATURE);
  defFeature("tail_recursion_optimisation", FT_BOOL,
	     TRUE, TAILRECURSION_FEATURE);
  defFeature("abort_with_exception", FT_BOOL,
	     FALSE, EX_ABORT_FEATURE);
  defFeature("c_libs",	  FT_ATOM|FF_READONLY, C_LIBS);
  defFeature("c_cc",	  FT_ATOM|FF_READONLY, C_CC);
  defFeature("c_ldflags", FT_ATOM|FF_READONLY, C_LDFLAGS);
  defFeature("gc",	  FT_BOOL,	       TRUE,  GC_FEATURE);
  defFeature("trace_gc",  FT_BOOL,	       FALSE, TRACE_GC_FEATURE);
#ifdef O_ATOMGC
  defFeature("agc_margin",FT_INTEGER,	       GD->atoms.margin);
#endif
#if defined(HAVE_DLOPEN) || defined(HAVE_SHL_LOAD) || defined(EMULATE_DLOPEN)
  defFeature("open_shared_object",	FT_BOOL|FF_READONLY, TRUE, 0);
  defFeature("shared_object_extension",	FT_ATOM|FF_READONLY, SO_EXT);
#endif
#if O_DYNAMIC_STACKS
  defFeature("dynamic_stacks",	FT_BOOL|FF_READONLY, TRUE, 0);
#endif
#ifdef HAVE_POPEN
  defFeature("pipe", FT_BOOL, TRUE, 0);
#endif
#ifdef O_PLMT
  defFeature("threads",	FT_BOOL, TRUE, 0);	/* FF_READONLY? */
#endif
#ifdef ASSOCIATE_SRC
  defFeature("associate", FT_ATOM, ASSOCIATE_SRC);
#endif
#ifdef O_DDE
  defFeature("dde", FT_BOOL|FF_READONLY, TRUE, 0);
#endif
#ifdef O_RUNTIME
  defFeature("runtime",	FT_BOOL|FF_READONLY, TRUE, 0);
  defFeature("debug_on_error", FT_BOOL|FF_READONLY, FALSE,
	     DEBUG_ON_ERROR_FEATURE);
  defFeature("report_error",	FT_BOOL|FF_READONLY, FALSE,
	     REPORT_ERROR_FEATURE);
#else
  defFeature("debug_on_error",	FT_BOOL, TRUE, DEBUG_ON_ERROR_FEATURE);
  defFeature("report_error",	FT_BOOL, TRUE, REPORT_ERROR_FEATURE);
#endif
  defFeature("debugger_show_context", FT_BOOL, FALSE, 0);
  defFeature("autoload",  FT_BOOL, TRUE,  AUTOLOAD_FEATURE);
  defFeature("max_integer",	   FT_INTEGER|FF_READONLY, PLMAXINT);
  defFeature("min_integer",	   FT_INTEGER|FF_READONLY, PLMININT);
  defFeature("max_tagged_integer", FT_INTEGER|FF_READONLY, PLMAXTAGGEDINT);
  defFeature("min_tagged_integer", FT_INTEGER|FF_READONLY, PLMINTAGGEDINT);
  defFeature("bounded",		   FT_BOOL|FF_READONLY,	   TRUE, 0);
  if ( (-3 / 2) == -2 )
    defFeature("integer_rounding_function", FT_ATOM|FF_READONLY, "down");
  else
    defFeature("integer_rounding_function", FT_ATOM|FF_READONLY, "toward_zero");
  defFeature("max_arity", FT_ATOM|FF_READONLY, "unbounded");
  defFeature("float_format", FT_ATOM, "%g");
  defFeature("answer_format", FT_ATOM, "~p");
  defFeature("character_escapes", FT_BOOL, TRUE, CHARESCAPE_FEATURE);
  defFeature("char_conversion", FT_BOOL, FALSE, CHARCONVERSION_FEATURE);
  defFeature("double_quotes", FT_ATOM, "codes");
  defFeature("unknown", FT_ATOM, "error");
  defFeature("debug", FT_BOOL, FALSE, 0);
  defFeature("allow_variable_name_as_functor", FT_BOOL, FALSE,
	     ALLOW_VARNAME_FUNCTOR);
  defFeature("toplevel_var_size", FT_INTEGER, 1000);
  defFeature("file_name_variables", FT_BOOL, FALSE, FILEVARS_FEATURE);
#ifdef __unix__
  defFeature("unix", FT_BOOL|FF_READONLY, TRUE, 0);
#endif

  defFeature("tty_control", FT_BOOL|FF_READONLY,
	     trueFeature(TTY_CONTROL_FEATURE), TTY_CONTROL_FEATURE);
  defFeature("readline", FT_BOOL/*|FF_READONLY*/, FALSE, 0);

#if defined(__WIN32__) && defined(_DEBUG)
  defFeature("kernel_compile_mode", FT_ATOM|FF_READONLY, "debug");
#endif

#if defined(__DATE__) && defined(__TIME__)
  { char buf[100];

    Ssprintf(buf, "%s, %s", __DATE__, __TIME__);
    defFeature("compiled_at", FT_ATOM|FF_READONLY, buf);
  }
#endif

  setArgvFeature();
}


static void
setArgvFeature()
{ term_t e = PL_new_term_ref();
  term_t l = PL_new_term_ref();
  int argc    = GD->cmdline.argc;
  char **argv = GD->cmdline.argv;
  int n;

  PL_put_nil(l);
  for(n=argc-1; n>= 0; n--)
  { PL_put_atom_chars(e, argv[n]);
    PL_cons_list(l, e, l);
  }

  defFeature("argv", FT_TERM, l);
}
