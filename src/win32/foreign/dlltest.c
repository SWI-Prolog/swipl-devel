/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../../../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Demonstrate the foreign-language interface.
*/

#include <windows.h>
#include <console.h>
#include <SWI-Prolog.h>
#include <stdio.h>
#include <sys/timeb.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pl_say_hello()  illustrates  a   simple    foreign   language  predicate
implementation  calling  a  Windows  function.     By  convention,  such
functions are called pl_<name_of_predicate>.  Their   type  is foreign_t
and all arguments are of type term_t.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
pl_say_hello(term_t to)
{ char *msg;

  if ( PL_get_atom_chars(to, &msg) )
  { MessageBox(NULL, msg, "DLL test", MB_OK|MB_TASKMODAL);

    PL_succeed;
  }

  PL_fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Interface function to modify the console:
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
pl_rlc_color(term_t which, term_t r, term_t b, term_t g)
{ int w;
  char *s;
  int tr, tb, tg;

  if ( PL_get_atom_chars(which, &s) )
  { if ( strcmp(s, "window") == 0 )
      w = RLC_WINDOW;
    else if ( strcmp(s, "text") == 0 )
      w = RLC_TEXT;
    else if ( strcmp(s, "highlight") == 0 )
      w = RLC_HIGHLIGHT;
    else if ( strcmp(s, "highlighttext") == 0 )
      w = RLC_HIGHLIGHTTEXT;
    else
      goto usage;
  } else
    goto usage;

  if ( PL_get_integer(r, &tr) &&
       PL_get_integer(b, &tb) &&
       PL_get_integer(g, &tg) )
  { if ( tr < 0 || tr > 255 || tb < 0 || tb > 255 | tg < 0 || tg > 255 )
      goto usage;

    rlc_color(w, RGB(tr,tb,tg));
    PL_succeed;
  }

usage:
  PL_warning("rlc_color({window,text,highlight,highlighttext}, R, G, B)");
  PL_fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This function is a handle  called   from  abort/1.   The function should
perform cleanup as Prolog is going to   perform a long_jmp() back to the
toplevel.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
my_abort(void)
{ MessageBox(NULL,
	     "Execution aborted", "Abort handle test",
	     MB_OK|MB_TASKMODAL);
}

  
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Define mclock/1 to query time since Prolog was started in milliseconds. 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static struct _timeb epoch;

void
initMClock()
{ _ftime(&epoch);
} 


unsigned long
mclock()
{ struct _timeb now;

  _ftime(&now);
  return (now.time - epoch.time) * 1000 +
	 (now.millitm - epoch.millitm);
}


foreign_t
pl_mclock(term_t msecs)
{ return PL_unify_integer(msecs, mclock());
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(un)install functions.  Predicates registered with PL_register_foreign()
donot  need  to  be  uninstalled   as    the   Prolog   toplevel  driver
unload_foreign_library/[1,2] will to this automatically for you.

As only hooks need to be uninstalled,  you won't need this function very
often.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static PL_dispatch_hook_t oldhook;

install_t
install()
{ PL_register_foreign("say_hello", 1, pl_say_hello, 0);
  PL_register_foreign("rlc_color", 4, pl_rlc_color, 0);
  PL_register_foreign("mclock",    1, pl_mclock,    0);

  initMClock();
  PL_abort_hook(my_abort);
}


install_t
uninstall()
{ PL_abort_unhook(my_abort);
}
