/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2015, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
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


atom_t
lookup_clref(Clause clause)
{ struct clref ref;
  int new;

  DEBUG(0,
	{ GET_LD
	  assert(!onStackArea(local, clause));
	});

  ref.clause = clause;
  return lookupBlob((const char*)&ref, sizeof(ref), &clause_blob, &new);
}


Clause
clause_clref(atom_t aref)
{ PL_blob_t *type;
  clref *ref = PL_blob_data(aref, NULL, &type);
  Clause clause;

  if ( type == &clause_blob )
  { clause = ref->clause;
    if ( false(clause, CL_ERASED) )
      return clause;
  }

  return NULL;
}


int
PL_unify_clref(term_t t, Clause clause)
{ struct clref ref;

  { GET_LD
    assert(!onStackArea(local, clause));
  }

  ref.clause = clause;
  return PL_unify_blob(t, &ref, sizeof(ref), &clause_blob);
}


int
PL_put_clref(term_t t, Clause clause)
{ struct clref ref;

  { GET_LD
    assert(!onStackArea(local, clause));
  }

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

