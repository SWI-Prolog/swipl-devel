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

#ifndef DB4PL_H_INCLUDED
#define DB4PL_H_INCLUDED

#include <SWI-Prolog.h>
#include <db.h>

#define DBH_MAGIC 277484232		/* magic for validation */

typedef enum
{ D_TERM,				/* a Prolog term */
  D_ATOM,				/* an atom (length+cahsr) */
  D_CSTRING,				/* a C-string (0-terminated) */
  D_CLONG				/* a C-long */
} dtype;

typedef struct
{ int	magic;				/* DBH_MAGIC */
  DB *db;				/* the database */

  int	duplicates;			/* Duplicates allowed? */
  dtype	key_type;			/* type of the key */
  dtype value_type;			/* type of the data */
} dbh;

typedef unsigned long	atomid_t;	/* internal atom identifier */

#define DB4PL_ATOM_CREATE	0x1	/* Create atoms */

int	db_status(int rval);

int	db_atom_id(dbh *db, atom_t a, atomid_t *id, int flags);
int	pl_atom_from_db(dbh *db, atomid_t id, atom_t *a);

extern  DB_ENV *db4pl_env;		/* Default environment */
#define db_env db4pl_env

#endif /*DB4PL_H_INCLUDED*/
