/* Simple test for a DLL
*/

#include <windows.h>
#include <console.h>
#include "pl-itf.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pl_say_hello()  illustrates  a   simple    foreign   language  predicate
implementation  calling  a  Windows  function.     By  convention,  such
functions are called pl_<name_of_predicate>.  Their   type  is foreign_t
and all arguments are of type term_t.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
pl_say_hello(term_t to)
{ if ( PL_is_atom(to) )
  { char *a = PL_atom_value(PL_atomic(to));

    MessageBox(NULL, a, "DLL test", MB_OK|MB_TASKMODAL);

    PL_succeed;
  }

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
This function illustrates the  event-dispatching   handle  in the Prolog
main loop.  
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
my_dispatch(void)
{ RlcQueue q = rlc_input_queue();

  if ( rlc_is_empty_queue(q) )
  { static char title[256];
    static int titleinited = 0;

    if ( !titleinited )
    { rlc_title(NULL, title, sizeof(title));
      titleinited++;
    } else
      rlc_title(title, NULL, 0);

    while(rlc_is_empty_queue(q))
      rlc_dispatch(q);

    rlc_title("SWI-Prolog: Running", NULL, 0);
  }

  return PL_DISPATCH_INPUT;
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

  PL_abort_hook(my_abort);
  oldhook = PL_dispatch_hook(my_dispatch);
}


install_t
uninstall()
{ PL_abort_unhook(my_abort);
  PL_dispatch_hook(oldhook);
}
