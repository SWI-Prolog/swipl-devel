#include <SWI-Prolog.h>

static atom_t ATOM_three_three_three;


static foreign_t
shlib_fun(term_t t)
{   return PL_unify_atom(t,  ATOM_three_three_three);
}


#define MKATOM(n) ATOM_ ## n = PL_new_atom(#n);

install_t
install(void)
{ MKATOM(three_three_three);

  PL_register_foreign("shlib_fun", 1, shlib_fun, 0);
}
