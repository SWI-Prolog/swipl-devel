/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: atom management
*/

#define O_DEBUG 1
#include "pl-incl.h"
#include "pl-ctype.h"

static void	rehashAtoms();

static int  atom_buckets = ATOMHASHSIZE;
static int  atom_locked;
static Atom *atomTable;

#define lockAtoms() { atom_locked++; }
#define unlockAtoms() if ( --atom_locked == 0 && \
			   atom_buckets * 2 < statistics.atoms ) \
			rehashAtoms()

#if O_DEBUG
static int	lookups;
static int	cmps;
#endif

Atom
lookupAtom(char *s)
{ int v0 = unboundStringHashValue(s);
  int v = v0 & (atom_buckets-1);
  register Atom a;

  DEBUG(0, lookups++);

  for(a = atomTable[v]; a && !isRef((word)a); a = a->next)
  { DEBUG(0, cmps++);
    if (streq(s, a->name) )
      return a;
  }
  a = (Atom)allocHeap(sizeof(struct atom));
  a->next       = atomTable[v];
  a->type       = ATOM_TYPE;
#ifdef O_HASHTERM
  a->hash_value = v0;
#endif
  a->name       = store_string(s);
  atomTable[v]  = a;
  statistics.atoms++;

  if ( atom_buckets * 2 < statistics.atoms && !atom_locked )
    rehashAtoms();

  return a;
}


static void
makeAtomRefPointers()
{ Atom *a;
  int n;

  for(n=0, a=atomTable; n < (atom_buckets-1); n++, a++)
    *a = (Atom) makeRef(a+1);
  *a = NULL;
}


static void
rehashAtoms()
{ Atom *oldtab   = atomTable;
  int   oldbucks = atom_buckets;
  Atom a, n;

  startCritical;
  atom_buckets *= 2;
  atomTable = allocHeap(atom_buckets * sizeof(Atom));
  makeAtomRefPointers();
  
  DEBUG(0, Sdprintf("rehashing atoms (%d --> %d)\n", oldbucks, atom_buckets));

  for(a=oldtab[0]; a; a = n)
  { int v;

    while(isRef((word)a) )
    { a = *((Atom *)unRef(a));
      if ( a == NULL )
	goto out;
    }
    n = a->next;
    v = a->hash_value & (atom_buckets-1);
    a->next = atomTable[v];
    atomTable[v] = a;
  }

out:
  freeHeap(oldtab, oldbucks * sizeof(Atom));
  endCritical;
}


word
pl_atom_hashstat(Word i, Word n)
{ int m;
  register Atom a;

  if ( !isInteger(*i) || valNum(*i) < 0 || valNum(*i) >= atom_buckets )
    fail;
  for(m = 0, a = atomTable[valNum(*i)]; a && !isRef((word)a); a = a->next)
    m++;

  return unifyAtomic(n, consNum(m));
}

#ifdef O_HASHTERM
#define ATOM(s) { (Atom)NULL, ATOM_TYPE, 0L, s }
#else
#define ATOM(s) { (Atom)NULL, ATOM_TYPE, s }
#endif

struct atom atoms[] = {
#include "pl-atom.ic"
  ATOM((char *)NULL)
};
#undef ATOM

/* Note that the char * of the atoms is copied to the data segment.  This
   is done because some functions temporary change the char string associated
   with an atom (pl_concat_atom()) and GCC Sputs char constants in the text
   segment.
*/

#if O_DEBUG
exitAtoms(void *arg)
{ Sdprintf("hashstat: %d lookupAtom() calls used %d strcmp() calls\n",
	   lookups, cmps);
}
#endif

void
initAtoms(void)
{ register int n;

  atomTable = allocHeap(atom_buckets * sizeof(Atom));
  makeAtomRefPointers();

  { Atom a;

    for( a = &atoms[0]; a->name; a++)
    { int v0 = unboundStringHashValue(a->name);
      int v = v0 & (atom_buckets-1);

      a->name = store_string(a->name);
      a->next       = atomTable[v];
#ifdef O_HASHTERM
      a->hash_value = v0;
#endif
      atomTable[v]  = a;
      statistics.atoms++;
    }
  }    

  DEBUG(0, PL_on_halt(exitAtoms, NULL));
}


word
pl_current_atom(Word a, word h)
{ Atom atom;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      if ( isAtom(*a) ) succeed;
      if ( !isVar(*a) ) fail;

      atom = atomTable[0];
      lockAtoms();
      break;
    case FRG_REDO:
      atom = (Atom) ForeignContextAddress(h);
      break;
    case FRG_CUTTED:
    default:
      unlockAtoms();
      succeed;
  }

  for(; atom; atom = atom->next)
  { while(isRef((word)atom) )
    { atom = *((Atom *)unRef(atom));
      if (atom == (Atom) NULL)
	goto out;
    }
    if (unifyAtomic(a, atom) == FALSE)
      continue;

    return_next_table(Atom, atom);
  }

out:
  unlockAtoms();
}

		 /*******************************
		 *	 ATOM COMPLETION	*
		 *******************************/

#define ALT_SIZ 80		/* maximum length of one alternative */
#define ALT_MAX 256		/* maximum number of alternatives */
#define stringMatch(m)	(stringAtom((m)->name))

forwards bool 	allAlpha(char *);

typedef struct match
{ Atom	name;
  int	length;
} *Match;


static bool
allAlpha(register char *s)
{ for( ; *s; s++)
   if ( !isAlpha(*s) )
     fail;

  succeed;
}


static char *
extendAtom(char *prefix, bool *unique)
{ Atom a = atomTable[0];
  bool first = TRUE;
  static char common[LINESIZ];
  int lp = (int) strlen(prefix);

  *unique = TRUE;

  for(; a; a = a->next)
  { while( isRef((word)a) )
    { a = *((Atom *)unRef(a));
      if ( a == (Atom)NULL)
	goto out;
    }
    if ( strprefix(stringAtom(a), prefix) )
    { if ( strlen(stringAtom(a)) >= LINESIZ )
	continue;
      if ( first == TRUE )
      { strcpy(common, stringAtom(a)+lp);
	first = FALSE;
      } else
      { char *s = common;
	char *q = stringAtom(a)+lp;
	while( *s && *s == *q )
	  s++, q++;
	*s = EOS;
	*unique = FALSE;
      }
    }
  }

out:
  return first == TRUE ? (char *)NULL : common;
}


word
pl_complete_atom(Word prefix, Word common, Word unique)
{ char *p, *s;
  bool u;
  char buf[LINESIZ];
    
  if ( (p = toString(*prefix)) == NULL )
    return warning("$complete_atom/3: instanstiation fault");
  strcpy(buf, p);
  if ( (s = extendAtom(p, &u)) != NULL )
  { strcat(buf, s);
    TRY(unifyStringWithList(buf, common));
    return unifyAtomic(unique, u ? ATOM_unique : ATOM_not_unique);
  }

  fail;
}


static int
compareMatch(const void *m1, const void *m2)
{ return strcmp(stringMatch((Match)m1), stringMatch((Match)m2));
}


static bool
extend_alternatives(char *prefix, struct match *altv, int *altn)
{ Atom a = atomTable[0];
  char *as;
  int l;

  *altn = 0;
  for(; a; a=a->next)
  { while( a && isRef((word)a) )
      a = *((Atom *)unRef(a));
    if ( a == (Atom) NULL )
      break;
    
    as = stringAtom(a);
    if ( strprefix(as, prefix) &&
	 allAlpha(as) &&
	 (l = (int)strlen(as)) < ALT_SIZ )
    { Match m = &altv[(*altn)++];
      m->name = a;
      m->length = l;
      if ( *altn > ALT_MAX )
	break;
    }
  }
  
  qsort(altv, *altn, sizeof(struct match), compareMatch);

  succeed;
}


word
pl_atom_completions(Word prefix, Word alts)
{ char *p;
  char buf[LINESIZ];
  struct match altv[ALT_MAX];
  int altn;
  int i;

  if ( (p = toString(*prefix)) == NULL )
    return warning("$atom_completions/2: instanstiation fault");
  strcpy(buf, p);

  extend_alternatives(buf, altv, &altn);
  
  for(i=0; i<altn; i++)
  { TRY(unifyFunctor(alts, FUNCTOR_dot2));
    TRY(unifyAtomic(argTermP(*alts, 0), altv[i].name));
    alts = argTermP(*alts, 1);
    deRef(alts);
  }
  return unifyAtomic(alts, ATOM_nil);
} 


#ifdef HAVE_LIBREADLINE

#define savestring(x) strcpy(xmalloc(1 + strlen(x)), (x))

char *
atom_generator(char *prefix, int state)
{ static Atom a;

  if ( !state )
  { assert(!a);
    a = atomTable[0];
  } 

  for(; a; a=a->next)
  { char *as;
    int l;

    while( isRef((word)a) )
    { a = *((Atom *)unRef(a));
      if ( !a )
	return NULL;
    }
    
    assert(a->type == ATOM_TYPE);
    as = stringAtom(a);
    if ( strprefix(as, prefix) &&
	 allAlpha(as) &&
	 (l = strlen(as)) < ALT_SIZ )
    { a = a->next;
      return savestring(as);
    }
  }

  return NULL;
}

#endif /*HAVE_LIBREADLINE*/
