/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2022, University of Amsterdam
                              VU University Amsterdam
			      SWI-Prolog Solutions b.v.
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

/*#define O_DEBUG 1*/
#include "pl-supervisor.h"
#include "pl-comp.h"
#include "pl-inline.h"
#include "pl-wrap.h"
#include "pl-tabling.h"
#include "pl-util.h"

#define MAX_FLI_ARGS 10			/* extend switches on change */

Code
allocCodes(size_t n)
{ Code codes = allocHeapOrHalt(sizeof(code)*(n+1));

  *codes++ = (code)n;

  return codes;
}


static void
freeCodes(Code codes)
{ size_t size = (size_t)codes[-1];

  unregisterWrappedSupervisor(codes); /* holds atom_t references */

  if ( size > 0 )		/* 0: built-in, see initSupervisors() */
    freeHeap(&codes[-1], (size+1)*sizeof(code));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
freeCodesDefinition() destroys the supervisor of  a predicate, replacing
it  by  the  statically  allocated  S_VIRGIN  supervisor.  Note  that  a
predicate *always* has non-NULL def->codes.

If linger == FALSE, we  are  absolutely   sure  that  it  is harmless to
deallocate the old supervisor. If TRUE,   there may be references. I.e.,
other threads may have started executing this predicate.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
free_codes_ptr(void *ptr)
{ freeCodes(ptr);
}


void
freeSupervisor(Definition def, Code codes, int do_linger)
{ size_t size = (size_t)codes[-1];

  if ( size > 0 )		/* 0: built-in, see initSupervisors() */
  { if ( do_linger )
      linger(&def->lingering, free_codes_ptr, codes);
    else
      freeCodes(codes);
  }
}


void
freeCodesDefinition(Definition def, int do_linger)
{ Code codes;

  if ( (codes=def->codes) != SUPERVISOR(virgin) )
  { if ( (codes = def->codes) )
    { if ( unlikely(codes[0] == encode(S_CALLWRAPPER)) )
      { resetWrappedSupervisor(def, do_linger);
	if ( !do_linger )
	{ codes = def->codes;
	  def->codes = SUPERVISOR(virgin);
	  freeSupervisor(def, codes, do_linger);
	}
      } else
      { def->codes = SUPERVISOR(virgin);
	freeSupervisor(def, codes, do_linger);
      }
    } else
      def->codes = SUPERVISOR(virgin);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Foreign supervisors.  Creates one of:

DET code:  I_FOPEN,     I_FCALLDETVA|I_FCALLDET<N>,   I_FEXITDET
NDET code: I_FOPENNDET, I_FCALLNDETVA|I_FCALLNDET<N>, I_FEXITNDET, I_FREDO
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_PROF_PENTIUM
#include "pentium.h"
static int prof_foreign_index = (I_HIGHEST+20);
#endif

int
createForeignSupervisor(Definition def, Func f)
{ assert(true(def, P_FOREIGN));

  if ( false(def, P_VARARG) )
  { if ( def->functor->arity > MAX_FLI_ARGS )
      sysError("Too many arguments to foreign function %s (>%d)", \
	       predicateName(def), MAX_FLI_ARGS); \
  }

  if ( false(def, P_NONDET) )
  { Code codes = allocCodes(4);

    codes[0] = encode(I_FOPEN);
    if ( true(def, P_VARARG) )
      codes[1] = encode(I_FCALLDETVA);
    else
      codes[1] = encode(I_FCALLDET0+def->functor->arity);
    codes[2] = (code)f;
    codes[3] = encode(I_FEXITDET);

    def->codes = codes;
  } else
  { Code codes = allocCodes(5);

    codes[0] = encode(I_FOPENNDET);
    if ( true(def, P_VARARG) )
      codes[1] = encode(I_FCALLNDETVA);
    else
      codes[1] = encode(I_FCALLNDET0+def->functor->arity);
    codes[2] = (code)f;
    codes[3] = encode(I_FEXITNDET);
    codes[4] = encode(I_FREDO);

    def->codes = codes;
  }

#ifdef O_PROF_PENTIUM
  assert(prof_foreign_index < MAXPROF);
  def->prof_index = prof_foreign_index++;
  def->prof_name  = strdup(predicateName(def));
#endif

  succeed;
}


static int
equalSupervisors(const Code s1, const Code s2)
{ if ( s1 != s2 )
  { size_t sz1 = (size_t)s1[-1];
    size_t sz2 = (size_t)s2[-1];

    if ( sz1 && sz1 == sz2 &&
	 memcmp(s1, s2, sz1*sizeof(*s1)) == 0 )
      return TRUE;

    return FALSE;
  }

  return TRUE;
}


		 /*******************************
		 *	   PROLOG CASES		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
getClauses() finds alive clauses and stores them into the array refp. It
stores at most `max' clauses and returns   the total number of candidate
clauses. This code is only executed on   static code and in theory there
should be no reason to validate the  counts, but reconsulting files must
make us careful.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
getClauses(Definition def, ClauseRef *refp, int max)
{ GET_LD
  ClauseRef cref;
  int found = 0;

  acquire_def(def);
  for(cref = def->impl.clauses.first_clause; cref; cref = cref->next)
  { if ( visibleClause(cref->value.clause, global_generation()) )
    { if ( found < max )
	refp[found] = cref;
      found++;
    }
  }
  release_def(def);

  return found;
}


static Code
undefSupervisor(Definition def)
{ if ( def->impl.clauses.number_of_clauses == 0 && false(def, PROC_DEFINED) )
    return SUPERVISOR(undef);

  return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
createSingleClauseSupervisor() creates a supervisor to call the one and
only clause of the predicate.  Creates

	S_TRUSTME <ClauseRef>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Code
singleClauseSupervisor(Definition def)
{ if ( def->impl.clauses.number_of_clauses == 1 )
  { ClauseRef cref;
    Code codes = allocCodes(2);
    int found = getClauses(def, &cref, 1);

    if ( found == 1 )
    { DEBUG(1, Sdprintf("Single clause supervisor for %s\n",
			predicateName(def)));

      codes[0] = encode(S_TRUSTME);
      codes[1] = (code)cref;

      return codes;
    }
    freeCodes(codes);
  }

  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
createListSuperVisor() creates a supervisor for predicates that have two
clauses (possibly swapped):

	pred([], ....)
	pred([H|T], ...)

The code is

	S_LIST <nilclause> <listclause>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Code
listSupervisor(Definition def)
{ if ( def->impl.clauses.number_of_clauses == 2 )
  { ClauseRef cref[2];
    word c[2];
    int found = getClauses(def, cref, 2);

    if ( found == 2 &&
	 arg1Key(cref[0]->value.clause->codes, &c[0]) &&
	 arg1Key(cref[1]->value.clause->codes, &c[1]) &&
	 ( (c[0] == ATOM_nil && c[1] == FUNCTOR_dot2) ||
	   (c[1] == ATOM_nil && c[0] == FUNCTOR_dot2) ) )
    { Code codes = allocCodes(3);

      DEBUG(1, Sdprintf("List supervisor for %s\n", predicateName(def)));

      codes[0] = encode(S_LIST);
      if ( c[0] == ATOM_nil )
      { codes[1] = (code)cref[0];
	codes[2] = (code)cref[1];
      } else
      { codes[1] = (code)cref[1];
	codes[2] = (code)cref[0];
      }

      return codes;
    }
  }

  return NULL;
}


static Code
dynamicSupervisor(Definition def)
{ if ( true(def, P_DYNAMIC) )
  { if ( def->tabling && true(def->tabling, TP_INCREMENTAL) )
      return SUPERVISOR(incr_dynamic);
    else
      return SUPERVISOR(dynamic);
  }

  return NULL;
}


static Code
multifileSupervisor(Definition def)
{ if ( true(def, P_MULTIFILE) )
    return SUPERVISOR(multifile);

  return NULL;
}


static Code
staticSupervisor(Definition def)
{ (void)def;

  return SUPERVISOR(staticp);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Prepend the already provided  supervisor   with  code  for meta-argument
module qualifications.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
copySuperVisorCode(Buffer buf, Code add)
{ size_t len = supervisorLength(add);

  addMultipleBuffer(buf, add, len, code);
}


static void
copyCodes(Code dest, Code src, size_t count)
{ memcpy(dest, src, count*sizeof(code));
}


static Code
chainPredicateSupervisor(Definition def, Code post)
{ if ( (true(def, P_META) && true(def, P_TRANSPARENT)) ||
       true(def, P_SSU_DET|P_DET) )
  { tmp_buffer buf;
    Code codes;

    initBuffer(&buf);

    if ( true(def, P_SSU_DET) )
      addBuffer(&buf, encode(S_SSU_DET), code);
    if ( true(def, P_DET) )
      addBuffer(&buf, encode(S_DET), code);

    if ( true(def, P_META) && true(def, P_TRANSPARENT) )
    { unsigned int i;
      int loffset = -1;

      for(i=0; i < def->functor->arity; i++)
      { int ma = def->impl.any.args[i].meta;

	if ( MA_NEEDS_TRANSPARENT(ma) )
	{ loffset = entriesBuffer(&buf, code);
	  addBuffer(&buf, encode(S_MQUAL), code);
	  addBuffer(&buf, VAROFFSET(i), code);
	}
      }

      if ( loffset >= 0 )
	baseBuffer(&buf, code)[loffset] = encode(S_LMQUAL);
    }

    if ( !isEmptyBuffer(&buf) )
    { copySuperVisorCode((Buffer)&buf, post);
      freeCodes(post);
      codes = allocCodes(entriesBuffer(&buf, code));
      copyCodes(codes, baseBuffer(&buf, code), entriesBuffer(&buf, code));

      return codes;
    } else
    { discardBuffer(&buf);
    }
  }

  return post;
}


		 /*******************************
		 *	      ENTRIES		*
		 *******************************/

int
createUndefSupervisor(Definition def)
{ Code codes;

  if ( (codes = undefSupervisor(def)) )
  { def->codes = codes;

    return TRUE;
  }

  return FALSE;
}


Code
createSupervisor(Definition def)
{ Code codes;
  int has_codes;

  has_codes = ((codes = undefSupervisor(def)) ||
	       (codes = dynamicSupervisor(def)) ||
	       (codes = multifileSupervisor(def)) ||
	       (codes = singleClauseSupervisor(def)) ||
	       (codes = listSupervisor(def)) ||
	       (codes = staticSupervisor(def)));
  assert(has_codes);
  (void)has_codes;
  codes = chainPredicateSupervisor(def, codes);

  return codes;
}


void
setSupervisor(Definition def, Code codes)
{ PL_LOCK(L_PREDICATE);
  Code old = def->codes;

  if ( equalSupervisors(old, codes) )
  { freeSupervisor(def, codes, FALSE);
  } else
  { MEMORY_BARRIER();
    def->codes = codes;
    freeSupervisor(def, old, TRUE);
  }
  PL_UNLOCK(L_PREDICATE);
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setDefaultSupervisor() is synchronised with  unloadFile() (reconsult/1).
Seems this is not yet enough to   stop all racer conditions between this
code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
setDefaultSupervisor(Definition def)
{ if ( false(def, P_LOCKED_SUPERVISOR) )
  { Code codes, old;

    PL_LOCK(L_PREDICATE);
    old = def->codes;
    codes = createSupervisor(def);
    if ( equalSupervisors(old, codes) )
    { freeSupervisor(def, codes, FALSE);
    } else
    { MEMORY_BARRIER();
      def->codes = codes;
      freeSupervisor(def, old, TRUE);
    }
    PL_UNLOCK(L_PREDICATE);
  }

  return TRUE;
}


		 /*******************************
		 *	      INFO		*
		 *******************************/

size_t
supervisorLength(Code base)
{ Code PC = base;
  size_t len = (size_t)base[-1];

  if ( len != 0 )
  { return len;
  } else
  { for(; decode(*PC) != I_EXIT; PC = stepPC(PC))
      ;
    PC++;					/* include I_EXIT */
    return PC-base;
  }
}

/* returns 0 for shared static supervisors
 */

size_t
sizeof_supervisor(Code base)
{ size_t size = (size_t)base[-1];

  return size*sizeof(code);
}


		 /*******************************
		 *	       INIT		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Generic and reusable code-sequences. The  entry-point of these sequences
must be accessed as:

	SUPERVISOR(name)

The code sequence starts with 0 to   avoid freeing using freeCodes(). It
ends in I_EXIT, such that generic code  walkers will always find the end
of the sequence.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAKE_SV1(name, i) { PL_code_data.supervisors.name[0] = (code)0; \
			    PL_code_data.supervisors.name[1] = encode(i); \
			    PL_code_data.supervisors.name[2] = encode(I_EXIT); \
			  }

void
initSupervisors(void)
{ MAKE_SV1(exit,	 I_EXIT);
  MAKE_SV1(next_clause,	 S_NEXTCLAUSE);
  MAKE_SV1(virgin,	 S_VIRGIN);
  MAKE_SV1(undef,	 S_UNDEF);
  MAKE_SV1(dynamic,      S_DYNAMIC);
  MAKE_SV1(incr_dynamic, S_INCR_DYNAMIC);
  MAKE_SV1(thread_local, S_THREAD_LOCAL);
  MAKE_SV1(multifile,    S_MULTIFILE);
  MAKE_SV1(staticp,      S_STATIC);
  MAKE_SV1(wrapper,      S_WRAP);
  MAKE_SV1(trie_gen,     S_TRIE_GEN);
}
