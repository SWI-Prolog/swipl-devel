/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: atom management
*/

#include "pl-incl.h"
#include "pl-ctype.h"

static Atom atomTable[ATOMHASHSIZE];

Atom
lookupAtom(char *s)
{ int v = stringHashValue(s, ATOMHASHSIZE);
  register Atom a;

  for(a = atomTable[v]; a && !isRef((word)a); a = a->next)
  { if (streq(s, a->name) )
      return a;
  }
  a = (Atom)allocHeap(sizeof(struct atom));
  a->next = atomTable[v];
  a->type = ATOM_TYPE;
  a->name = store_string(s);
  atomTable[v] = a;
  statistics.atoms++;

  return a;
}


word
pl_atom_hashstat(Word i, Word n)
{ int m;
  register Atom a;

  if ( !isInteger(*i) || valNum(*i) < 0 || valNum(*i) >= ATOMHASHSIZE )
    fail;
  for(m = 0, a = atomTable[valNum(*i)]; a && !isRef((word)a); a = a->next)
    m++;

  return unifyAtomic(n, consNum(m));
}


struct atom atoms[] = {
#include "pl-atom.ic"
{ (Atom)NULL,	ATOM_TYPE,	(char *)NULL }
};

/* Note that the char * of the atoms is copied to the data segment.  This
   is done because some functions temporary change the char string associated
   with an atom (pl_concat_atom()) and GCC puts char constants in the text
   segment.
*/

void
initAtoms(void)
{ register int n;

  { register Atom *a;
    for(n=0, a=atomTable; n < (ATOMHASHSIZE-1); n++, a++)
      *a = (Atom) makeRef(a+1);
  }

  { register Atom a;
    register int v;

    for( a = &atoms[0]; a->name; a++)
    { a->name = store_string(a->name);
      v = stringHashValue(a->name, ATOMHASHSIZE);
      a->next = atomTable[v];
      atomTable[v] = a;
      statistics.atoms++;
    }
  }    
}


word
pl_current_atom(Word a, word h)
{ Atom atom;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      if ( isAtom(*a) ) succeed;
      if ( !isVar(*a) ) fail;

      atom = atomTable[0];
      break;
    case FRG_REDO:
      atom = (Atom) ForeignContextAddress(h);
      break;
    case FRG_CUTTED:
    default:
      succeed;
  }

  for(; atom; atom = atom->next)
  { while(isRef((word)atom) )
    { atom = *((Atom *)unRef(atom));
      if (atom == (Atom) NULL)
	fail;
    }
    if (unifyAtomic(a, atom) == FALSE)
      continue;

    return_next_table(Atom, atom);
  }

  fail;
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


char *
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


bool
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

static char *
xmalloc(int size)
{ char *result = malloc(size);

  if ( !result )
    fatalError("Not enough core");

  return result;
}

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
