/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Do What I Mean support functions
*/

#include "pl-incl.h"
#include "pl-ctype.h"

forwards Atom	dwimMatch P((char *, char *));
forwards bool	oneTypo P((char *, char *));
forwards bool	twoTransposed P((char *, char *));
forwards bool	oneInserted P((char *, char *));
forwards bool	differentSeparated P((char *, char *));
forwards char *	subWord P((char *, char *));
forwards bool	subwordsTransposed P((char *, char *));

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Strings are supposed to be meant identical iff one of the  following  is
the case:

  - They ARE identical
  - One character is different			(spy == spu)
  - One character is inserted/deleted/added	(debug == deug)
  - Two adjecent characters are transposed	(trace == tarce)
  - `Sub-words' have been separated wrong	(aB == a_b == ab)
  - Two `Sub-words' have been transposed	(exists_file == file_exists)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Atom
dwimMatch(str1, str2)
char *str1, *str2;
{ int l1, l2;
  register char *s1 = str1;
  register char *s2 = str2;

  while(*s1 && *s1 == *s2)			/* delete common part */
    s1++, s2++;
  l2 = (int) strlen(s2);
  l1 = (int) strlen(s1);

  if (abs(l1-l2) > 5)				/* speed up a bit */
    fail;
  
  if ( l1 == 0 && l2 == 0 )			return ATOM_equal;
  if ( (s1[0] == EOS || s1[1] == EOS || s1[2] == EOS) ||
       (s2[0] == EOS || s2[1] == EOS || s2[2] == EOS))
    fail;
  if ( l1 == l2 && oneTypo(s1, s2) )		return ATOM_mismatched_char;
  if ( l1 == l2 && twoTransposed(s1, s2) )	return ATOM_transposed_char;
  if ( (l2 == l1 + 1 && oneInserted(s1, s2)) ||
       (l1 == l2 + 1 && oneInserted(s2, s1)) )	return ATOM_inserted_char;
  if ( differentSeparated(str1, str2) )		return ATOM_separated;
  if ( subwordsTransposed(str1, str2) )		return ATOM_transposed_word;

  fail;
}

static bool
oneTypo(s1, s2)
char *s1, *s2;
{ if (s1[1] == EOS || streq(&s1[1], &s2[1]) )
    succeed;
  fail;
}

static
bool
twoTransposed(s1, s2)
register char *s1, *s2;
{ if (s1[1] != EOS && s1[0] == s2[1] && s1[1] == s2[0] &&
       (s1[2] == EOS || streq(&s1[2], &s2[2])))
    succeed;
  fail;
}

static bool
oneInserted(s1, s2)
register char *s1, *s2;
{ if (streq(s1, &s2[1]) )
    succeed;
  fail;
}

static bool
differentSeparated(s1, s2)
register char *s1, *s2;
{ register char c1, c2;

  if ( *s1 != *s2 || *s1 == EOS )
    fail;

  c1 = *++s1, c2 = *++s2;
  while(c1 && c1 == c2)
  { if ((c1 = *++s1) == '_')
    { c1 = *++s1;
    } else
    { if (isLower(s1[-1]) && isUpper(c1))
        c1 = toLower(c1);
    }
    if ((c2 = *++s2) == '_')
    { c2 = *++s2;
    } else
    { if (isLower(s2[-1]) && isUpper(c2))
	c2 = toLower(c2);
    }
  }
  if (c1 == EOS && c2 == EOS)
    succeed;
  fail;
}

static char *
subWord(s, store)
register char *s, *store;
{ *store++ = (isUpper(*s) ? toLower(*s) : *s);
  s++;

  for(;;)
  { if (*s == EOS)
    { *store = EOS;
      return s;
    }
    if (*s == '_')
    { *store = EOS;
      return ++s;
    }
    if (isLower(s[-1]) && isUpper(s[0]) )
    { *store = EOS;
      return s;
    }
    *store++ = *s++;
  }
}    

static bool
subwordsTransposed(s1, s2)
char *s1, *s2;
{ char sw1a[1024], sw1b[1024];
  char sw2a[1024], sw2b[1024];

  while(*s1 && *s2)
  { s1 = subWord(s1, sw1a);
    s2 = subWord(s2, sw2a);
    if (!streq(sw1a, sw2a) )
    { if (*s1 == EOS || *s2 == EOS)
	fail;
      s1 = subWord(s1, sw1b);
      s2 = subWord(s2, sw2b);
      if (!streq(sw1a, sw2b) || !streq(sw1b, sw2a) )
	fail;
    }
  }
  if (*s1 == EOS && *s2 == EOS)
    succeed;
  fail;
}

		/********************************
		*       PROLOG CONNECTION       *
		*********************************/

word
pl_dwim_match(a1, a2, mm)
Word a1, a2, mm;
{ char *s1, *s2 = NULL;		/* initialise to make gcc happy */
  bool rval;
  Atom type;

  initAllocLocal();
  rval = ((s1 = primitiveToString(*a1, TRUE)) != (char *)NULL &&
	  (s2 = primitiveToString(*a2, TRUE)) != (char *)NULL);
  stopAllocLocal();
  if ( !rval )
    fail;

  if ( (type = dwimMatch(s1, s2)) == (Atom) NULL )
    fail;

  return unifyAtomic(mm, type);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
$dwim_predicate(+Term, -Dwim) successively returns all predicates of the
specified module or context module  that  match  in  a  DWIM  sence  the
predicate head.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_dwim_predicate(term, dwim, h)
Word term, dwim;
word h;
{ FunctorDef fdef;
  Module module = (Module) NULL;
  Procedure proc;
  Symbol symb;

  if ( ForeignControl(h) == FRG_CUTTED )
    succeed;

  if ((term = stripModule(term, &module)) == (Word) NULL)
    fail;

  if (isAtom(*term) )
    fdef = lookupFunctorDef((Atom)*term, 0);
  else if (isTerm(*term) )
    fdef = functorTerm(*term);
  else
    return warning("dwim_predicate/2: illegal term specification");
  	
  if ( ForeignControl(h) == FRG_FIRST_CALL )
    symb = firstHTable(module->procedures);
  else
    symb = (Symbol) ForeignContextAddress(h);

  for(; symb; symb = nextHTable(module->procedures, symb))
  { proc = (Procedure) symb->value;
    if ( dwimMatch(stringAtom(fdef->name), stringAtom(proc->functor->name)) &&
         isDefinedProcedure(proc) &&
         (stringAtom(proc->functor->name)[0] != '$' ||
	   SYSTEM_MODE) )
    { if (unifyFunctor(dwim, proc->functor) == FALSE)
	continue;
      if ((symb = nextHTable(module->procedures, symb)) != (Symbol) NULL)
	ForeignRedo(symb);

      succeed;
    }
  }

  fail;
}
