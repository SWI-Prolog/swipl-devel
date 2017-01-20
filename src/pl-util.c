/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2015, University of Amsterdam
                              VU University Amsterdam
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
  { if ( def->module )
      strcpy(e, atom_summary(def->module->name, 50));
    else
      strcpy(e, "(nil)");
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


char *
sourceFileName(SourceFile sf)
{ char tmp[650];

  strcpy(tmp, atom_summary(sf->name, 50));
  return buffer_string(tmp, BUF_RING);
}


char *
generationName(gen_t gen)
{ char tmp[256];

  if ( gen == GEN_MAX )
    return "GEN_MAX";
  Ssprintf(tmp, "%lld", (int64_t)gen);
  return buffer_string(tmp, BUF_RING);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
clauseNo() returns the clause index of the given clause at the given
generation.  Use the current generation if gen is 0;
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
clauseNo(Definition def, Clause cl, gen_t gen)
{ GET_LD
  int i;
  ClauseRef cref;

  if ( !gen )
    gen = global_generation();

  acquire_def(def);
  for(i=1, cref=def->impl.clauses.first_clause; cref; cref=cref->next)
  { Clause c = cref->value.clause;

    if ( visibleClause(c, gen) )
    { if ( c == cl )
      { release_def(def);
	return i;
      }
    }
    i++;
  }
  release_def(def);

  return -1;
}



/*  succeeds if proc is a system predicate exported to the public module.

 ** Fri Sep  2 17:03:43 1988  jan@swivax.UUCP (Jan Wielemaker)  */

static bool
isUserSystemPredicate(Definition def)
{ GET_LD

  if ( true(def, P_LOCKED) &&
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

