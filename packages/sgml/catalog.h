/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
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
