/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include <SWI-Stream.h>
#include "db4pl.h"
#include <sys/types.h>
#include <stdlib.h>
#include <assert.h>
#include "error.h"

#define DEBUG(g) (void)0

		 /*******************************
		 *	      TMP HACKS		*
		 *******************************/

extern int unboundStringHashValue(const char *t, unsigned int len);

typedef unsigned long plhash_t;

unsigned long
PL_atom_hash(atom_t a)
{ const char *s;
  unsigned int len;

  s = PL_atom_nchars(a, &len);

  return unboundStringHashValue(s, len);
}


		 /*******************************
		 *	  ATOM MANAGEMENT	*
		 *******************************/

static int maxdupl;

static int
equal_dbt(DBT *a, DBT *b)
{ if ( a->size == b->size )
  { if ( a->data == b->data )
      return TRUE;
    if ( memcmp(a->data, b->data, a->size) == 0 )
      return TRUE;
  }

  return FALSE;
}


int
db_atom_id(dbh *db, atom_t a, atomid_t *id, int flags)
{ unsigned long hash = PL_atom_hash(a) & 0xffffL;
  unsigned long idx = 0;
  int rval;
  DBC *cursor;
  DBT k, v;
  DBT av;

  if ( (rval = db->db->cursor(db->db, NULL, &cursor)) != 0 )
    return db_status(rval);

  memset(&av, 0, sizeof(av));
  memset(&v,  0, sizeof(v));
  memset(&k,  0, sizeof(k));
  k.data = &hash;
  k.size = sizeof(hash);
  av.data = (void *)PL_atom_nchars(a, &av.size);

  if ( (rval=cursor->c_get(cursor, &k, &v, DB_SET)) == 0 )
  { DBT k2;

    if ( equal_dbt(&v, &av) )
    { cursor->c_close(cursor);
      *id = (idx<<16) | hash;
      return TRUE;
    }

    memset(&k2, 0, sizeof(k2));
    for(;;)
    { if ( (rval=cursor->c_get(cursor, &k2, &v, DB_NEXT)) == 0 &&
	   k2.size == sizeof(hash) &&
	   *((plhash_t *)k2.data) == hash )
      { idx++;
	
	if ( equal_dbt(&v, &av) )
	{ cursor->c_close(cursor);
	  *id = (idx<<16) | hash;
	  if ( idx > maxdupl )
	  { Sdprintf("\n%% %s: max-duplicates: %d\n", av.data, idx);
	    maxdupl = idx;
	  }
	  return TRUE;
	}
	continue;
      }
      if ( rval > 0 )			/* some error */
      { cursor->c_close(cursor);
	return db_status(rval);
      }

      cursor->c_close(cursor);
      idx++;
      if ( idx >= 1<<16 )
	return pl_error(ERR_LIMIT, "atoms_per_key", 1<<16);
      DEBUG(Sdprintf("Added '%s' at %ld\n", av.data, (idx<<16)|hash));
      goto add;
    }
  } else if ( rval == DB_NOTFOUND )
  { cursor->c_close(cursor);

    if ( (flags & DB4PL_ATOM_CREATE) )
    {
    add:
      rval = db_status(db->db->put(db->db, NULL, &k, &av, 0));

      if ( rval )
	*id = (idx<<16)|hash;

      return rval;
    } else
      return FALSE;
  } else
  { cursor->c_close(cursor);
    return db_status(rval);
  }
}


int
pl_atom_from_db(dbh *db, atomid_t id, atom_t *a)
{ unsigned long idx = (id>>16) & 0xffff;
  unsigned long kv  = id & 0xffff;
  int rval;
  DBT k, v;
  
  memset(&v, 0, sizeof(v));
  memset(&k, 0, sizeof(k));
  k.size = sizeof(kv);
  k.data = &kv;

  if ( idx == 0 )			/* first, common case */
  { if ( (rval=db->db->get(db->db, NULL, &k, &v, 0)) == 0 )
    { *a = PL_new_atom_nchars(v.size, v.data);
      return TRUE;
    }
  } else
  { DBC *cursor;
    unsigned long i = 0;

    if ( (rval = db->db->cursor(db->db, NULL, &cursor)) != 0 )
      return db_status(rval);

    if ( (rval=cursor->c_get(cursor, &k, &v, DB_SET)) == 0 )
    { DBT k2;

      memset(&k2, 0, sizeof(k2));
      while(i<idx)
      { if ( (rval=cursor->c_get(cursor, &k2, &v, DB_NEXT)) == 0 )
	  i++;
	else
	{ cursor->c_close(cursor);
	  return db_status(rval);
	}
      }
      
      *a = PL_new_atom_nchars(v.size, v.data);
      cursor->c_close(cursor);
      return TRUE;
    }
  }

  assert(0);
  return FALSE;				/* exception */
}
