/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010-2012, VU University Amsterdam

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

#include "pl-incl.h"
#include "pl-dbref.h"

typedef struct clref
{ Clause clause;
} clref;

typedef struct recref
{ RecordRef record;
} recref;


static int
write_clause_ref(IOSTREAM *s, atom_t aref, int flags)
{ clref *ref = PL_blob_data(aref, NULL, NULL);
  (void)flags;

  Sfprintf(s, "<clause>(%p)", ref->clause);
  return TRUE;
}


static void
acquire_clause(atom_t aref)
{ clref *ref = PL_blob_data(aref, NULL, NULL);

  set(ref->clause, DBREF_CLAUSE);
}


static int
release_clause(atom_t aref)
{ clref *ref = PL_blob_data(aref, NULL, NULL);

  clear(ref->clause, DBREF_CLAUSE);
  if ( true(ref->clause, DBREF_ERASED_CLAUSE) )
    unallocClause(ref->clause);

  return TRUE;
}


static int
save_clause_ref(atom_t aref, IOSTREAM *fd)
{ clref *ref = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <clause>(%p)", ref->clause);
}


static atom_t
load_clause_ref(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<saved-clause-ref>");
}


static PL_blob_t clause_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  "clause",
  release_clause,
  NULL,
  write_clause_ref,
  acquire_clause,
  save_clause_ref,
  load_clause_ref
};


static int
write_record_ref(IOSTREAM *s, atom_t aref, int flags)
{ recref *ref = PL_blob_data(aref, NULL, NULL);

  Sfprintf(s, "<record>(%p)", ref->record);
  return TRUE;
}


static void
acquire_record(atom_t aref)
{ recref *ref = PL_blob_data(aref, NULL, NULL);

  set(ref->record->record, R_DBREF);
}


static int
release_record(atom_t aref)
{ recref *ref = PL_blob_data(aref, NULL, NULL);

  if ( ref->record->record )
    clear(ref->record->record, R_DBREF);
  else
    unallocRecordRef(ref->record);

  return TRUE;
}


static PL_blob_t record_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  "record",
  release_record,
  NULL,
  write_record_ref,
  acquire_record
};


int
PL_unify_clref(term_t t, Clause clause)
{ struct clref ref;

  ref.clause = clause;
  return PL_unify_blob(t, &ref, sizeof(ref), &clause_blob);
}


int
PL_put_clref(term_t t, Clause clause)
{ struct clref ref;

  ref.clause = clause;
  PL_put_blob(t, &ref, sizeof(ref), &clause_blob);
  return TRUE;
}


int
PL_unify_recref(term_t t, RecordRef r)
{ struct recref ref;

  ref.record = r;
  return PL_unify_blob(t, &ref, sizeof(ref), &record_blob);
}


int
PL_is_dbref(term_t t)
{ PL_blob_t *type;

  if ( PL_is_blob(t, &type) &&
       ( type == &clause_blob ||
	 type == &record_blob ) )
    return TRUE;

  return FALSE;
}


void *
PL_get_dbref(term_t t, db_ref_type *type_ptr)
{ void *data;
  PL_blob_t *type;

  if ( !PL_get_blob(t, &data, NULL, &type) )
  { error:
    PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_db_reference, t);
    return NULL;
  }

  if ( type == &clause_blob )
  { clref *ref = data;

    if ( false(ref->clause, CL_ERASED) )
    { *type_ptr = DB_REF_CLAUSE;
      return ref->clause;
    }
  } else if ( type == &record_blob )
  { recref *ref = data;

    if ( ref->record->record &&
	 false(ref->record->record, R_ERASED) )
    { *type_ptr = DB_REF_RECORD;
      return ref->record;
    }
  } else
  { goto error;
  }

  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Returns FALSE: error
         TRUE: existing clause
           -1: erased clause
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_get_clref(term_t t, Clause *cl)
{ struct clref *ref;
  PL_blob_t *type;

  if ( !PL_get_blob(t, (void**)&ref, NULL, &type) ||
       type != &clause_blob )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_db_reference, t);

  *cl = ref->clause;

  if ( true(ref->clause, CL_ERASED) )
    return -1;

  return TRUE;
}


int
PL_get_recref(term_t t, RecordRef *rec)
{ struct recref *ref;
  PL_blob_t *type;

  if ( !PL_get_blob(t, (void**)&ref, NULL, &type) ||
       type != &record_blob )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_db_reference, t);

  if ( ref->record->record &&
       false(ref->record->record, R_ERASED) )
  { *rec = ref->record;
    return TRUE;
  }

  return FALSE;
}


void
initDBRef(void)
{ PL_register_blob_type(&record_blob);
  PL_register_blob_type(&clause_blob);
}

