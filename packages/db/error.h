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

#ifndef H_ERROR_INCLUDED
#define H_ERROR_INCLUDED
#include <stdarg.h>

typedef enum
{ ERR_ERRNO,				/* , int */
					/* ENOMEM */
					/* EACCES --> file, action */
					/* ENOENT --> file */
  ERR_TYPE,				/* char *expected, term_t actual */
  ERR_DOMAIN,				/* char *expected, term_t actual */
  ERR_EXISTENCE,			/* char *expected, term_t actual */

  ERR_FAIL,				/* term_t goal */

  ERR_LIMIT,				/* char *limit, long max */
  ERR_PACKAGE_INT,			/* char *pkg, int id, char *fmt, ... */
  ERR_PACKAGE_ID			/* char *pkg, char *id, char *fmt, ... */
} plerrorid;

int	pl_error(plerrorid, ...);

#endif /*H_ERROR_INCLUDED*/
