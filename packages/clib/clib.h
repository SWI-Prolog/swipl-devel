/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker

    Copyright (C) 1999 SWI, University of Amseterdam. All rights reserved.
*/

#ifndef CLIB_H_INCLUDED
#define CLIB_H_INCLUDED

#include <SWI-Prolog.h>
#include "error.h"

#define CompoundArg(name, arity) \
	PL_FUNCTOR, PL_new_functor(PL_new_atom(name), (arity))
#define AtomArg(name) \
	PL_CHARS, name
#define IntArg(i) \
	PL_INTEGER, (i)

install_t	install_process(void);
install_t	install_socket(void);

#endif /*CLIB_H_INCLUDED*/
