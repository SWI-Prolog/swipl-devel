/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#ifndef H_ERROR_INCLUDED
#define H_ERROR_INCLUDED
#include <stdarg.h>

#define ERR_ERRNO	0
#define ERR_ARGTYPE	1
#define ERR_DOMAIN	2
#define ERR_EXISTENCE	3
#define ERR_PERMISSION	4

int		pl_error(const char *name, int arity,
			 const char *msg, int id, ...);

#endif /*H_ERROR_INCLUDED*/
