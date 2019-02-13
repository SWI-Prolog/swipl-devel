#include <SWI-Prolog.h>

const char* shlib_fun_in_dep();

static foreign_t
shlib_fun(term_t t)
{   const char* str = shlib_fun_in_dep();
    return PL_unify_atom_chars(t, str);
}


install_t
install(void)
{ PL_register_foreign("shlib_fun", 1, shlib_fun, 0);
}
