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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note: this function comes from SWI-Prolog   itself. We could also extend
the SWI-Prolog interface to provide this value. 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
unboundStringHashValue(const char *t, unsigned int len)
{ unsigned int value = 0;
  unsigned int shift = 5;

  while(len-- != 0)
  { unsigned int c = *t++;
    
    c -= 'a';
    value ^= c << (shift & 0xf);
    shift ^= c;
  }

  return value ^ (value >> 16);
}

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

static unsigned long maxdupl;

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

  if ( (rval = db->db->cursor(db->db, NULL, &cursor, 0)) != 0 )
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

    if ( (rval = db->db->cursor(db->db, NULL, &cursor, 0)) != 0 )
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
