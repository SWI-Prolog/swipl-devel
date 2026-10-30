/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2019, VU University Amsterdam
			      CWI, Amsterdam
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
#include "pl-proc.h"
#include "pl-rec.h"
#include "pl-fli.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Clause and record references are PL_BLOB_UNIQUE|PL_BLOB_NOCOPY blobs: the
data of the blob is the Clause or RecordRef itself.  NOCOPY blobs hash
on the pointer, so finding the reference of a clause needs no copy of a
key and no allocation.  While a clause has a reference, it has
DBREF_CLAUSE and erasing it is completed by the release hook.  The same
holds for records using R_DBREF.

The references are also PL_BLOB_LOCAL: they start as local atoms of the
thread that creates them and are promoted when they escape or another
thread looks up the same reference (see "Thread-local blobs" in
pl-atom.c).  Use lookup_shared_clref() for references kept in data that
other threads may access.

The VM used the copy of a clause_ref in the old blobs as frame clause.
It now builds a clause_ref on the local stack (see pl-cont.c and
S_TRIE_GEN_COMPILED).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
compare_dbref(atom_t a1, atom_t a2)
{ void *p1 = PL_blob_data(a1, NULL, NULL);
  void *p2 = PL_blob_data(a2, NULL, NULL);

  return p1 < p2 ? -1 : p1 > p2 ? 1 : 0;
}


static int
write_clause_ref(IOSTREAM *s, atom_t aref, int flags)
{ Clause cl = PL_blob_data(aref, NULL, NULL);
  (void)flags;

  Sfprintf(s, "<clause>(%p)", cl);
  return true;
}


static void
acquire_clause_blob(atom_t aref)
{ Clause cl = PL_blob_data(aref, NULL, NULL);

  set(cl, DBREF_CLAUSE);
}


/* Clearing DBREF_CLAUSE and testing DBREF_ERASED_CLAUSE must be one
 * atomic step, as freeClause() tests the first and sets the second.
 */

static int
release_clause_blob(atom_t aref)
{ Clause cl = PL_blob_data(aref, NULL, NULL);
  unsigned int flags;

  do
  { flags = cl->flags;
  } while ( !COMPARE_AND_SWAP_UINT(&cl->flags, flags, flags&~DBREF_CLAUSE) );

  if ( (flags&CL_ERASED) )
    ATOMIC_DEC(&GD->clauses.db_erased_refs);
  if ( (flags&DBREF_ERASED_CLAUSE) )
    unallocClause(cl);

  return true;
}


static int
save_clause_ref(atom_t aref, IOSTREAM *fd)
{ Clause cl = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <clause>(%p)", cl);
}


static atom_t
load_clause_ref(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<saved-clause-ref>");
}


static PL_blob_t clause_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE|PL_BLOB_NOCOPY|PL_BLOB_LOCAL,
  "clause",
  release_clause_blob,
  compare_dbref,
  write_clause_ref,
  acquire_clause_blob,
  save_clause_ref,
  load_clause_ref
};


static int
write_record_ref(IOSTREAM *s, atom_t aref, int flags)
{ RecordRef r = PL_blob_data(aref, NULL, NULL);
  (void)flags;

  Sfprintf(s, "<record>(%p)", r);
  return true;
}


static void
acquire_record(atom_t aref)
{ RecordRef r = PL_blob_data(aref, NULL, NULL);

  set(r->record, R_DBREF);
}


/* freeRecordRef() runs under L_RECORD and tests R_DBREF to decide who
 * frees `r`.  We may run with L_REHASH_ATOMS held, so we must not wait
 * for L_RECORD: if it is busy, we refuse and are called again later.
 */

static int
release_record(atom_t aref)
{ RecordRef r = PL_blob_data(aref, NULL, NULL);

  if ( !PL_TRYLOCK(L_RECORD) )
    return false;
  if ( r->record )
    clear(r->record, R_DBREF);
  else
    unallocRecordRef(r);
  PL_UNLOCK(L_RECORD);

  return true;
}


static PL_blob_t record_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE|PL_BLOB_NOCOPY|PL_BLOB_LOCAL,
  "record",
  release_record,
  compare_dbref,
  write_record_ref,
  acquire_record
};


atom_t
lookup_clref(Clause clause)
{ GET_LD
  int new;

  DEBUG(CHK_SECURE, assert(!onStackArea(local, clause)));

  return lookupBlob((const char*)clause, sizeof(*clause), &clause_blob, &new);
}


/* As lookup_clref(), for a reference kept in data that other threads
 * may access.
 */

atom_t
lookup_shared_clref(Clause clause)
{ atom_t a = lookup_clref(clause);

  promoteAtomIfLocal(a);
  return a;
}


/* The clause of a clause reference or NULL if `aref` is not a clause
 * reference.  Note that the clause may be erased.
 */

Clause
clause_dbref(atom_t aref)
{ PL_blob_t *type;
  Clause cl = PL_blob_data(aref, NULL, &type);

  if ( type == &clause_blob )
    return cl;

  return NULL;
}


bool
PL_unify_clref(term_t t, Clause clause)
{ GET_LD
  atom_t a = lookup_clref(clause);
  bool rc = PL_unify_atomic(t, a);

  PL_unregister_atom(a);

  return rc;
}


int
PL_put_clref(term_t t, Clause clause)
{ atom_t a = lookup_clref(clause);

  _PL_put_atomic(t, a);
  PL_unregister_atom(a);

  return true;
}


int
PL_unify_recref(term_t t, RecordRef r)
{ return PL_unify_blob(t, r, sizeof(*r), &record_blob);
}


int
PL_is_dbref(term_t t)
{ PL_blob_t *type;

  if ( PL_is_blob(t, &type) &&
       ( type == &clause_blob ||
	 type == &record_blob ) )
    return true;

  return false;
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
  { Clause cl = data;

    if ( isoff(cl, CL_ERASED) )
    { *type_ptr = DB_REF_CLAUSE;
      return cl;
    }
  } else if ( type == &record_blob )
  { RecordRef r = data;

    if ( r->record && isoff(r->record, R_ERASED) )
    { *type_ptr = DB_REF_RECORD;
      return r;
    }
  } else
  { goto error;
  }

  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Returns false: error
         true: existing clause
           -1: erased clause
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
PL_get_clref(term_t t, Clause *cl)
{ Clause clause;
  PL_blob_t *type;

  if ( !PL_get_blob(t, (void**)&clause, NULL, &type) ||
       type != &clause_blob )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_db_reference, t);

  *cl = clause;

  if ( ison(clause, CL_ERASED) )
    return -1;

  return true;
}


int
PL_get_recref(term_t t, RecordRef *rec)
{ RecordRef r;
  PL_blob_t *type;

  if ( !PL_get_blob(t, (void**)&r, NULL, &type) ||
       type != &record_blob )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_db_reference, t);

  if ( r->record && isoff(r->record, R_ERASED) )
  { *rec = r;
    return true;
  }

  return false;
}


void
initDBRef(void)
{ PL_register_blob_type(&record_blob);
  PL_register_blob_type(&clause_blob);
}

