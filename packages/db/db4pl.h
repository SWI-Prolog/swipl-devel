/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#ifndef DB4PL_H_INCLUDED
#define DB4PL_H_INCLUDED

#include <SWI-Prolog.h>
#include <db.h>

#define DBH_MAGIC 277484232		/* magic for validation */

typedef struct
{ int	magic;				/* DBH_MAGIC */
  DB *db;				/* the database */

  int	duplicates;			/* Duplicates allowed? */
} dbh;

typedef unsigned long	atomid_t;	/* internal atom identifier */

#define DB4PL_ATOM_CREATE	0x1	/* Create atoms */

int	db_status(int rval);

int	db_atom_id(dbh *db, atom_t a, atomid_t *id, int flags);
int	pl_atom_from_db(dbh *db, atomid_t id, atom_t *a);

extern  DB_ENV db4pl_env;		/* Default environment */
#define db_env db4pl_env

#endif /*DB4PL_H_INCLUDED*/
