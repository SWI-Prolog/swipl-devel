/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
