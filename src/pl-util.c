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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "pl-incl.h"
#include "os/pl-ctype.h"
#include "os/pl-utf8.h"

static bool	isUserSystemPredicate(Definition def);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These  functions  return  a  user-printable  name   of  a  predicate  as
name/arity or module:name/arity. The result  is   stored  in the foreign
buffer ring, so we are thread-safe, but   the  result needs to be copied
before the ring is exhausted. See buffer_string() for details.  For wide
character versions, we use UTF-8 encoding.  This isn't very elegant, but
these functions are for debugging only.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
procedureName(Procedure proc)
{ return predicateName(proc->definition);
}


#define fetch_text(s, i) \
	((s)->encoding == ENC_ISO_LATIN_1 ? (s)->text.t[i]&0xff \
					  : (s)->text.w[i])

const char *
atom_summary(atom_t name, unsigned int maxlen)
{ PL_chars_t txt;
  Buffer b;
  size_t i;

  if ( !get_atom_text(name, &txt) )
    return NULL;

  if ( txt.encoding == ENC_ISO_LATIN_1 && txt.length < maxlen )
  { const unsigned char *s = (const unsigned char*) txt.text.t;
    const unsigned char *e = &s[txt.length];

    for( ; s<e; s++ )
    { if ( *s >= 0x80 )
	break;
    }
    if ( s == e )
      return txt.text.t;
  }

  b = findBuffer(BUF_RING);
  for(i=0; i<txt.length; i++)
  { char buf[6];
    char *e;

    e = utf8_put_char(buf, fetch_text(&txt, i));
    addMultipleBuffer(b, buf, e-buf, char);
    if ( i == maxlen - 6 )
    { addMultipleBuffer(b, "...", 3, char);
      i = txt.length - 4;
      maxlen = 0;			/* make sure not to trap again */
    }
  }
  addBuffer(b, 0, char);

  return baseBuffer(b, char);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
predicateName() returns an UTF-8  representation  of   the  name  of the
predicate. Note that we need for the buffer 6*max summary length,
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
predicateName(Definition def)
{ char tmp[650];
  char *e = tmp;

  if ( !def )
    return "(nil)";

  if ( def->module != MODULE_user && !isUserSystemPredicate(def) )
  { strcpy(e, atom_summary(def->module->name, 50));
    e += strlen(e);
    *e++ = ':';
  }
  strcpy(e, atom_summary(def->functor->name, 50));
  e += strlen(e);
  *e++ = '/';
  Ssprintf(e, "%d", def->functor->arity);

  return buffer_string(tmp, BUF_RING);
}


char *
functorName(functor_t f)
{ char tmp[650];
  char *e = tmp;
  FunctorDef fd;

  if ( tagex(f) != (TAG_ATOM|STG_GLOBAL) )
    return "<not-a-functor>";

  fd = valueFunctor(f);
  strcpy(e, atom_summary(fd->name, 50));
  e += strlen(e);
  *e++ = '/';
  Ssprintf(e, "%d", fd->arity);

  return buffer_string(tmp, BUF_RING);
}


char *
keyName(word key)
{ if ( tagex(key) == (TAG_ATOM|STG_GLOBAL) )
  { return functorName(key);
  } else
  { char tmp[650];

    if ( !key )
    { strcpy(tmp, "<nil>");
    } else
    { switch(tag(key))
      { case PL_INTEGER:
	case PL_FLOAT:
	{ GET_LD
	  number n;

	  get_number(key, &n PASS_LD);
	  switch(n.type)
	  { case V_INTEGER:
	      Ssprintf(tmp, "%lld", n.value.i);
	      break;
	    case V_FLOAT:
	      Ssprintf(tmp, "%f", n.value.f);
	      break;
	    default:
	      strcpy(tmp, "<number>");
	  }
	  break;
	}
      case PL_ATOM:
	strcpy(tmp, atom_summary(key, 30));
	break;
      }
    }

    return buffer_string(tmp, BUF_RING);
  }
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
clauseNo() returns the clause index of the given clause
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
clauseNo(Definition def, Clause cl)
{ int i;
  ClauseRef cref;

  for(i=1, cref=def->impl.clauses.first_clause; cref; cref=cref->next, i++)
  { if ( cref->value.clause == cl )
      return i;
  }

  return -1;
}



/*  succeeds if proc is a system predicate exported to the public module.

 ** Fri Sep  2 17:03:43 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static bool
isUserSystemPredicate(Definition def)
{ if ( true(def, P_LOCKED) &&
       isCurrentProcedure(def->functor->functor, MODULE_user) )
    succeed;

  fail;
}


int
notImplemented(char *name, int arity)
{ return PL_error(NULL, 0, NULL, ERR_NOT_IMPLEMENTED_PROC, name, arity);
}


word
setBoolean(int *flag, term_t old, term_t new)
{ if ( !PL_unify_bool_ex(old, *flag) ||
       !PL_get_bool_ex(new, flag) )
    fail;

  succeed;
}


word
setInteger(int *flag, term_t old, term_t new)
{ GET_LD

  if ( !PL_unify_integer(old, *flag) ||
       !PL_get_integer_ex(new, flag) )
    fail;

  succeed;
}

