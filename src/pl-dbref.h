/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2010, University of Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

typedef enum db_ref_type
{ DB_REF_CLAUSE,
  DB_REF_RECORD
} db_ref_type;

COMMON(int)   PL_put_clref(term_t t, Clause clause);
COMMON(int)   PL_unify_clref(term_t t, Clause clause);
COMMON(int)   PL_unify_recref(term_t t, RecordRef rec);
COMMON(void*) PL_get_dbref(term_t t, db_ref_type *type);
COMMON(int)   PL_get_clref(term_t t, Clause *cl);
COMMON(int)   PL_get_recref(term_t t, RecordRef *rec);
COMMON(int)   PL_is_dbref(term_t t);
COMMON(void)  initDBRef(void);
