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

/*  When we look for a token, we skip layout characters and comments.
    There there is nothing left, we return EOF.
    If we are looking for the beginning of an entry, the possibilities
    are then
*/

#define CAT_OTHER    (0)    /* token + parameter of find... */
#define CAT_SYSTEM   (1)    /* token only */
#define CAT_PUBLIC   (2)    /* token only */
#define CAT_DOCTYPE  (3)    /* token + parameter of find... */
#define CAT_ENTITY   (4)    /* token + parameter of find... */
#define CAT_PENTITY  (5)    /*         parameter of find... only */
#define CAT_OVERRIDE (5)    /* token only */
#define CAT_BASE     (6)    /* token only */
#define OVR_PUBLIC   (CAT_OVERRIDE + CAT_PUBLIC)
#define OVR_DOCTYPE  (CAT_OVERRIDE + CAT_DOCTYPE)
#define OVR_ENTITY   (CAT_OVERRIDE + CAT_ENTITY)


typedef enum
{ CTL_START,
  CTL_END
} catalog_location;

int	register_catalog_file(const char *file, catalog_location where);
int	is_absolute_path(const char *name);
char   *localpath(const char *ref, const char *name);
char const *find_in_catalogue(
    int         kind,
    char const *name,
    char const *pubid,
    char const *sysid,
    int         ci
);

#endif /*CATALOG_H_INCLUDED*/
