/*  @(#) pl_dld.c 1.0.0 (UvA SWI) Thu Sep 13 13:56:45 1990

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: linking dld to SWI-Prolog
*/

#include <stdio.h>
#include "../include/SWI-Prolog.h"
#include "../include/dld.h"

static char *
dld_string_error()
{ switch(dld_errno)
  { case DLD_ENOFILE:	  return "cannot open file";
    case DLD_EBADMAGIC:	  return "bad magic number";
    case DLD_EBADHEADER:  return "failure reading header";
    case DLD_ENOTEXT:	  return "premature eof in text section";
    case DLD_ENOSYMBOLS:  return "premature end of file in symbols";
    case DLD_ENOSTRINGS:  return "bad string table";
    case DLD_ENOTXTRELOC: return "premature eof in text relocation";
    case DLD_ENODATA:	  return "premature eof in data section";
    case DLD_ENODATRELOC: return "premature eof in data relocation";
    case DLD_EMULTDEFS:	  return "multiple definitions of symbol";
    case DLD_EBADLIBRARY: return "malformed library archive";
    case DLD_EBADCOMMON:  return "common block not supported";
    case DLD_EBADOBJECT:  return "malformed input file (not rel or archive)";
    case DLD_EBADRELOC:	  return "bad relocation info";
    case DLD_ENOMEMORY:	  return "virtual memory exhausted";
    case DLD_EUNDEFSYM:	  return "undefined symbol";
    default:		  return "unknown dld error";
  }
}


static foreign_t
dld_error(name, arity)
char *name;
int arity;
{ PL_warning("%s/%d: %s", name, arity, dld_string_error());

  PL_fail;
}


static int
dld_initialise(debug)
int debug;
{ static dld_initialised = FALSE;

  if ( dld_initialised == FALSE )
  { if ( dld_init(PL_query(PL_QUERY_SYMBOLFILE), debug) != 0 )
    { dld_error("dld_initialise", 0);
      return FALSE;
    }
    
    dld_initialised = TRUE;
  }

  return TRUE;
}


static char *
get_char_p(name, arity, atom)
char *name;
int arity;
term atom;
{ if ( PL_type(atom) != PL_ATOM )
  { PL_warning("%s/%d: instantiation fault");
    return NULL;
  }
  
  return PL_atom_value(PL_atomic(atom));
}


static foreign_t
pl_dld_link(name)
term name;
{ char *path;

  if ( dld_initialise(FALSE) == FALSE )
    PL_fail;

  if ( (path = get_char_p("dld_link", 1, name)) == NULL )
    PL_fail;

  if ( dld_link(path) != 0 )
    return dld_error("dld_link", 1);

  PL_succeed;
}


static foreign_t
pl_dld_unlink(name)
term name;
{ char *path;

  if ( dld_initialise(FALSE) == FALSE )
    PL_fail;

  if ( (path = get_char_p("dld_unlink", 1, name)) == NULL )
    PL_fail;

  if ( dld_unlink_by_file(path) != 0 )
    return dld_error("dld_unlink", 1);

  PL_succeed;
}


typedef void (*Func)();

static foreign_t
pl_dld_call(name)
term name;
{ char *func_name;
  Func func;

  if ( dld_initialise() == FALSE )
    PL_fail;

  if ( (func_name = get_char_p("dld_call", 1, name)) == NULL )
    PL_fail;

  if ( dld_function_executable_p(func_name) == 0 )
    return PL_warning("dld_call/1: %s is not executable: %s",
		      func_name, dld_string_error());

  if ( (func = (Func) dld_get_func(func_name)) == 0 )
    return dld_error("dld_call", 1);

  (*func)();

  PL_succeed;
}


static foreign_t
pl_dld_list_undefined()
{ if ( dld_initialise(FALSE) == FALSE )
    PL_fail;

  if ( dld_list_undefined() == 0 )
    PL_succeed;

  PL_fail;
}


pl_dld_initialise(debug)
term debug;
{ if ( PL_type(debug) != PL_INTEGER )
    return PL_warning("dld_initialise/2: intantiation fault");
  
  if ( dld_initialise(PL_integer_value(PL_atomic(debug))) == FALSE )
    PL_fail;

  PL_succeed;
}


pl_dld_function(name, address)
term name, address;
{ if ( PL_type(name) == PL_ATOM )
  { char *fn = PL_atom_value(PL_atomic(name));
    long addr;
    
    if ( (addr = dld_get_func(fn)) == FALSE )
      PL_fail;

    return PL_unify_atomic(address, PL_new_integer(addr));
  } else if ( PL_type(address) == PL_INTEGER )
  { long addr = PL_integer_value(PL_atomic(address));
    char *fn;
    int perc;

    fn = dld_find_function(addr, &perc);

    return PL_unify_atomic(name, PL_new_atom(fn));
  } else
    return PL_warning("dld_function/2: intantiation fault");
}


dld_start()
{ PL_register_foreign("dld_initialise",	    1, pl_dld_initialise,	0);
  PL_register_foreign("dld_link",           1, pl_dld_link,   		0);
  PL_register_foreign("dld_unlink",         1, pl_dld_unlink, 		0);
  PL_register_foreign("dld_call",           1, pl_dld_call,   		0);
  PL_register_foreign("dld_list_undefined", 0, pl_dld_list_undefined,   0);
  PL_register_foreign("dld_function",  	    2, pl_dld_function,		0);

  PL_succeed;
}
