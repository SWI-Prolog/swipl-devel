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

#ifndef CATALOG_H_INCLUDED
#define CATALOG_H_INCLUDED

typedef enum
{ CTL_START,
  CTL_END
} catalog_location;

char   *find_in_catalog(const char *key, const char *name);
int	register_catalog_file(const char *file, catalog_location where);
int	is_absolute_path(const char *name);
char   *localpath(const char *ref, const char *name);

#endif /*CATALOG_H_INCLUDED*/
