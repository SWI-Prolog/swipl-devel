/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
			      VU University Amsterdam

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

/*#define O_DEBUG 1*/
#define _GNU_SOURCE			/* get dladdr() */
#include "pl-incl.h"
#include "pl-dbref.h"
#include "pl-inline.h"
#include <limits.h>
#ifdef HAVE_DLADDR
#include <dlfcn.h>
#endif

/* MacOS (re-)defines bool, true and false.  This is a bit unelegant,
   but it works
*/
#undef false
#undef true
#undef bool
#define true(s, a)		((s)->flags & (a))
#define false(s, a)		(!true((s), (a)))

#undef LD			/* Get at most once per function */
#define LD LOCAL_LD

#define setHandle(h, w)		(*valTermRef(h) = (w))
#define valHandleP(h)		valTermRef(h)

static void	initVMIMerge(void);

static void
checkCodeTable(void)
{ const code_info *ci;
  int n;

  for(ci = codeTable, n = 0; ci->name != NULL; ci++, n++ )
  { if ( (int)ci->code != n )
      sysError("Wrong entry in codeTable: %d", n);
  }

  if ( n != I_HIGHEST )
    sysError("Mismatch in checkCodeTable()");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			MAPPING VIRTUAL INSTRUCTIONS

The virtual machine interpreter can be optimised considerably by storing
the code addressen with the clauses  rather  than  the  virtual  machine
codes.  Normally the switch in translated (in pseudo assembler) to:

next_instruction:
	r1 = *PC;
	PC += sizeof(code);
	if ( r1 >= I_HIGHEST ) goto default;
	r1 = jmp_table[r1 * 4];
	goto r1;

This is rather silly.  Suppose  we  store  the  addresses  of  the  code
segments  with  the  clauses  rather than the codes themselves, than the
loop overhead can be reduced to:

next_instruction:
	r1 = *PC;
	PC += sizeof(code);
	goto r1;

With gcc-2.1 or later, we can get this result without using assembler.
All this required where a few patches in interpret(), the compiler and
the wic (intermediate code)  generation  code.  The initialisation  is
very critical:

The function PL_next_solution() (the VM   interpreter) declares a static
array holding the  label  addresses  of   the  various  virtual  machine
instructions. When it is called, it will store the address of this table
in   the   global   variable     interpreter_jmp_table.   the   function
initWamTable() than makes the two   translation  tables wam_table[] (wam
code --> label address and dewam_table[]   (label address --> wam code).

NOTE:	If the assert() fails, look at pl-wam.c: VMI(C_NOT, ... for
	more information.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if VMCODE_IS_ADDRESS
void
initWamTable(void)
{ unsigned int n;
  code maxcoded, mincoded;

  assert(I_HIGHEST < 255);	/* need short for dewam_table on overflow */

  if ( interpreter_jmp_table == NULL )
  { if ( !PL_next_solution(QID_EXPORT_WAM_TABLE) )
      sysError("Could not initialise VM jump table");
  }

  wam_table[0] = (code) (interpreter_jmp_table[0]);
  maxcoded = mincoded = wam_table[0];

  for(n = 1; n < I_HIGHEST; n++)
  { wam_table[n] = (code) (interpreter_jmp_table[n]);
    if ( wam_table[n] > maxcoded )
      maxcoded = wam_table[n];
    if ( wam_table[n] < mincoded )
      mincoded = wam_table[n];
  }
  dewam_table_offset = mincoded;

  assert(wam_table[C_NOT] != wam_table[C_IFTHENELSE]);
  dewam_table = (unsigned char *)PL_malloc_atomic(((maxcoded-dewam_table_offset) + 1) *
						  sizeof(char));

  for(n = 0; n < I_HIGHEST; n++)
    dewam_table[wam_table[n]-dewam_table_offset] = (unsigned char) n;

  checkCodeTable();
  initSupervisors();
  initVMIMerge();
}

#else /* VMCODE_IS_ADDRESS */

void
initWamTable()
{ checkCodeTable();
  initSupervisors();
  initVMIMerge();
}

#endif /* VMCODE_IS_ADDRESS */


		 /*******************************
		 *	     COMPILER		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module forms together  with  the  module  'pl-wam.c'  the  complete
kernel  of  SWI-Prolog.   It  contains  the  compiler, the predicates to
interface the compiler to Prolog and the  decompiler.   SWI-Prolog  does
not  offer  a  Prolog  interpreter,  which  implies that common database
predicates such as assert/1 and retract/1 have to do  compilation  resp.
decompilation between the term representation used on the runtime stacks
and the compiled representation used in the heap.

Compiling a clause takes three different stages.  First the variables of
the clause are analysed.   This  phases  determines  `void'  (singleton)
variables  and assigns offsets in the environment frame to each variable
occurring in the clause that is not  singleton.   Variables  serving  on
their  own as an argument in the head are allocated in the corresponding
argument entry of the environment frame.  The others are allocated above
the arguments in the environment frame.   Singleton  variables  are  not
allocated at all.

Second  unification  code  for  the  head  is  produced.   Finally   the
subclauses  are  translated.   Most  vital  from  the  point  of view of
performance is to distinguis between the first time an  entry  from  the
variable  array  is addressed and the following times: the first time we
KNOW the field should be a variable and copying the value  or  making  a
reference  is  the  appropriate action.  This both saves us the variable
test and the need to turn the variable array of  the  environment  frame
really into an array of variables.

			ANALYSING VARIABLES

First of all the clause is scanned and all  variables  are  instantiated
with  a  structure  that  mimics  a term, but isn't one.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* We use STG_RESERVED here to make sure that the value is not 0.
   This value is also used by read_term/2,3 and friends.
*/

#define isVarInfo(w)	(tagex(w) == (TAG_VAR|STG_RESERVED) && (w) != 0)
#define setVarInfo(w,i)	(w = (((word)(i))<<LMASK_BITS)|TAG_VAR|STG_RESERVED)
#define varInfo(w)	(LD->comp.vardefs[(w)>>LMASK_BITS])


typedef struct _varDef
{ word		functor;		/* mimic a functor (FUNCTOR_dvard1) */
  word		saved;			/* saved value */
  Word		address;		/* address of the variable */
  int		times;			/* occurences */
  int		offset;			/* offset in environment frame */
} vardef;

typedef struct
{ int	isize;
  int	entry[1];
} var_table, *VarTable;

#undef struct_offsetp
#define struct_offsetp(t, f) ((size_t)((t*)0)->f)
#define sizeofVarTable(isize) (struct_offsetp(var_table, entry) + sizeof(int)*(isize))

#define mkCopiedVarTable(o) copyVarTable(alloca(sizeofVarTable(o->isize)), o)
#define BITSPERINT (sizeof(int)*8)

typedef struct
{ int		var;			/* Variable for local cuts */
  int		nextvar;
  code		instruction;		/* Instruction to use: C_CUT/C_LCUT */
} cutInfo;

typedef struct merge_state
{ const vmi_merge *candidates;		/* Merge candidates */
  size_t	merge_pos;		/* The merge candidate location */
} merge_state;

typedef enum target_module_type
{ TM_NONE = 0,				/* No explicit target */
  TM_MODULE,				/* Explicit module target */
  TM_VAR				/* Explicit variable target */
} target_module_type;

typedef struct target_module
{ target_module_type type;		/* Type of target module */
  int		     var_index;		/* Index of var if TM_VAR */
  Module	     module;		/* Module = TM_MODULE */
} target_module;

typedef struct
{ Module	module;			/* module to compile into */
  Clause	clause;			/* clause we are constructing */
  int		arity;			/* arity of top-goal */
  int		vartablesize;		/* size of the vartable */
  int		islocal;		/* Temporary local clause */
  int		subclausearg;		/* processing subclausearg */
  int		argvars;		/* islocal argument pseudo vars */
  int		argvar;			/* islocal current pseudo var */
  cutInfo	cut;			/* how to compile ! */
  merge_state	mstate;			/* Instruction merging state */
  VarTable	used_var;		/* boolean array of used variables */
  target_module colon_context;		/* Context:Goal */
#ifdef O_CALL_AT_MODULE
  target_module	at_context;		/* Call@Context */
#endif
  tmp_buffer	codes;			/* scratch code table */
} compileInfo, *CompileInfo;


static void	resetVars(ARG1_LD);

static VarDef
getVarDef(int i ARG_LD)
{ VarDef vd;
  VarDef *vardefs = LD->comp.vardefs;
  int nvd = LD->comp.nvardefs;

  if ( i >= nvd )
  { VarDef *vdp;
    int onvd = nvd, n;

    while ( i >= nvd )
      nvd = nvd > 0 ? (nvd*2) : 32;

    if ( onvd > 0 )
      vardefs = GC_REALLOC(vardefs, sizeof(VarDef) * nvd);
    else
      vardefs = GC_MALLOC(sizeof(VarDef) * nvd);

    if ( !vardefs )
      outOfCore();

    for(vdp = &vardefs[onvd], n=onvd; n++ < nvd; )
      *vdp++ = NULL;

    LD->comp.nvardefs = nvd;
    LD->comp.vardefs = vardefs;
  }

  if ( !(vd = vardefs[i]) )
  { vd = vardefs[i] = PL_malloc_atomic(sizeof(vardef));
    memset(vd, 0, sizeof(*vd));
    vd->functor = FUNCTOR_dvard1;
  }

  return vd;
}


static void
resetVarDefs(int n ARG_LD)		/* set addresses of first N to NULL */
{ VarDef *vd;
  int nvd = LD->comp.nvardefs;

  if ( n > nvd )			/* allocates them */
    getVarDef(n-1 PASS_LD);

  vd = LD->comp.vardefs;
  for( ; --n>=0 ; vd++ )
  { VarDef v;

    if ( (v = *vd) )
    { v->address = NULL;
    } else
    { *vd = v = PL_malloc_atomic(sizeof(vardef));
      memset(v, 0, sizeof(vardef));
      v->functor = FUNCTOR_dvard1;
    }
  }
}


void
freeVarDefs(PL_local_data_t *ld)
{ if ( ld->comp.vardefs )
  { GET_LD
    VarDef *vardefs = ld->comp.vardefs;
    int i, count=ld->comp.nvardefs;

    assert(LD==ld);

    for(i=0; i<count; i++)
    { if ( vardefs[i] )
	freeHeap(vardefs[i], sizeof(vardef));
    }

    GC_FREE(ld->comp.vardefs);
    ld->comp.vardefs = NULL;
    ld->comp.nvardefs = 0;
    ld->comp.filledVars = 0;
  }
}


int
get_head_and_body_clause(term_t clause,
			 term_t head, term_t body, Module *m ARG_LD)
{ Module m0;

  if ( !m )
  { m0 = NULL;
    m = &m0;
  }

  if ( PL_is_functor(clause, FUNCTOR_prove2) )
  { _PL_get_arg(1, clause, head);
    _PL_get_arg(2, clause, body);
    if ( !PL_strip_module_ex(head, m, head) )
      return FALSE;
  } else
  { PL_put_term(head, clause);		/* facts */
    PL_put_atom(body, ATOM_true);
  }

  DEBUG(9, pl_write(clause); Sdprintf(" --->\n\t");
	   Sdprintf("%s:", stringAtom((*m)->name));
	   pl_write(head); Sdprintf(" :- "); pl_write(body); Sdprintf("\n"));

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Analyse the variables of a clause.  `term' is the term to  be  analysed,
which  is  either  a  fact  or  a  clause (:-/2) term.  First of all the
functor and arity of the predicate are determined.   The  first  `arity'
elements  of  the variable definition array are then cleared.  This part
is used for sharing variables that occurr on their own in the head  with
the  argument  part  of the environment frame instead of putting them in
the variable part.

AnalyseVariables2() just scans the term, fills the  variable  definition
array  and  binds  found  variables  to entries of this array.  The last
argument indicates which plain argument we are processing.  It is set to
-1 when called with the head.  While scaning the head  arguments  it  is
set  to  the argument number.  For all other code it is arity (body code
and nested terms of the head).  This is used for  the  argument/variable
block merging.

After this scan the variable definition records are  scanned  to  assign
offsets  and delete singleton variables.  We cannot leave out singletons
that are sharing with the argument block.  Offset `0' is the first entry
of the argument block, offset `arity' of the variable block.  Singletons
are made variables again.

When compiling in `islocal' mode, we  count the compound terms appearing
as arguments to subclauses in ci->argvars and   there is no need to look
inside these terms.

Returns the number  of  variables  found   or  one  of  AVARS_CYCLIC  or
AVARS_MAX
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAX_VARIABLES 1000000000	/* stay safely under signed int */
#define AVARS_CYCLIC    -1
/*	MEMORY_OVERFLOW -5 */
#define AVARS_MAX      -12

static int
analyseVariables2(Word head, int nvars, int argn,
		  CompileInfo ci, int depth ARG_LD)
{
right_recursion:

  deRef(head);

  if ( isVar(*head) || (isAttVar(*head) && !ci->islocal) )
  { VarDef vd;
    int index;

    if ( argn >= 0 && argn < ci->arity )
    { index = argn;
    } else
    { if ( nvars >= MAX_VARIABLES )
      { LD->comp.filledVars = ci->arity+nvars;
	resetVars(PASS_LD1);
	return AVARS_MAX;
      }

      index = ci->arity + nvars++;
    }

    vd = getVarDef(index PASS_LD);
    vd->saved = *head;
    vd->address = head;
    vd->times = 1;
    setVarInfo(*head, index);

    return nvars;
  }

  if ( isVarInfo(*head) )
  { VarDef vd = varInfo(*head);

    vd->times++;
    return nvars;
  }

  if ( isTerm(*head) )
  { Functor f = valueTerm(*head);
    FunctorDef fd = valueFunctor(f->definition);
    int rc;

    if ( ++depth == 10000 && (rc=is_acyclic(head PASS_LD)) != TRUE )
    { LD->comp.filledVars = ci->arity+nvars;
      resetVars(PASS_LD1);

      return rc == FALSE ? AVARS_CYCLIC : rc;
    }

    if ( ci->islocal )
    { if ( ci->subclausearg )
      { DEBUG(MSG_COMP_ARGVAR,
	      Sdprintf("argvar for %s\n", functorName(f->definition)));
	ci->argvars++;

	return nvars;
      } else if ( false(fd, CONTROL_F) )
      { int ar = fd->arity;

	ci->subclausearg++;
	for(head = f->arguments, argn = ci->arity; --ar >= 0; head++, argn++)
	{ nvars = analyseVariables2(head, nvars, argn, ci, depth PASS_LD);
	  if ( nvars < 0 )
	    break;			/* error */
	}
	ci->subclausearg--;

	return nvars;
      } /* else fall through to normal case */
    }

    { int ar = fd->arity;

      head = f->arguments;
      argn = ( argn < 0 ? 0 : ci->arity );

      for(; --ar > 0; head++, argn++)
      { nvars = analyseVariables2(head, nvars, argn, ci, depth PASS_LD);
	if ( nvars < 0 )
	  return nvars;
      }

      goto right_recursion;
    }
  }

  if ( ci->subclausearg && (isString(*head) || isAttVar(*head)) )
  { DEBUG(MSG_COMP_ARGVAR,
	  Sdprintf("argvar for %s\n", isString(*head) ? "string" : "attvar"));
    ci->argvars++;
  }

  return nvars;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Returns TRUE on success and CYCLIC_* or *_OVERFLOW on errors (*_OVERFLOW
has not been implemented yet)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*	MEMORY_OVERFLOW -5 */
#define CYCLIC_HEAD    -10
#define CYCLIC_BODY    -11
/*	AVARS_MAX      -12 */

static int
analyse_variables(Word head, Word body, CompileInfo ci ARG_LD)
{ int nv, nvars = 0;
  int n;
  int body_voids = 0;
  int arity = ci->arity;

  if ( arity > 0 )
    resetVarDefs(arity PASS_LD);

  if ( head )
  { if ( (nvars = analyseVariables2(head, 0, -1, ci, 0 PASS_LD)) < 0 )
      return nvars == AVARS_CYCLIC ? CYCLIC_HEAD : nvars;
  }
  if ( body )
  { if ( (nvars = analyseVariables2(body, nvars, arity, ci, 0 PASS_LD)) < 0 )
      return nvars == AVARS_CYCLIC ? CYCLIC_BODY : nvars;
  }

  for(n=0; n<arity+nvars; n++)
  { VarDef vd = LD->comp.vardefs[n];

    assert(vd->functor == FUNCTOR_dvard1);
    if ( !vd->address )
      continue;
    if ( vd->times == 1 && !ci->islocal ) /* ISVOID */
    { *vd->address = vd->saved;
      vd->address = (Word) NULL;
      if (n >= arity)
	body_voids++;
    } else
      vd->offset = n + ci->argvars - body_voids;
  }

  LD->comp.filledVars = arity + nvars;

  if ( (nv = nvars + arity + ci->argvars - body_voids) > MAX_VARIABLES )
    return AVARS_MAX;

  ci->clause->prolog_vars = nv;
  ci->clause->variables   = nv;
  ci->cut.nextvar	  = nv;
  ci->vartablesize = (nv + BITSPERINT-1)/BITSPERINT;

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The compiler  itself.   First  it  calls  analyseVariables().  Next  the
arguments  of  the  head  and  the subclauses are compiled.  Finally the
bindings made by analyseVariables() are undone and the clause  is  saved
in the heap.

compileClause() maintains an array of  `used_var' (used variables). This
is to( determine when a variable is used   for the first time and thus a
FIRSTVAR instruction is to be generated instead of a VAR one.

Note that the `variables' field of a clause is filled with the number of
variables in the frame AND the arity.   This  saves  us  the  frame-size
calculation at runtime.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define isConjunction(w) hasFunctor(w, FUNCTOR_comma2)

#define A_HEAD		0x01		/* argument in head */
#define A_BODY		0x02		/* argument in body */
#define A_ARG		0x04		/* sub-argument */
#define A_RIGHT		0x08		/* rightmost argument */
#define A_NOARGVAR	0x10		/* do not compile using ci->argvar */

#define NOT_CALLABLE -10		/* return value for not-callable */

#define BLOCK(s) do { s; } while (0)

static void Output_0(CompileInfo ci, vmi c);

#define Output_a(ci,c)		addBuffer(&(ci)->codes, c, code)
#define Output_an(ci,p,n)	addMultipleBuffer(&(ci)->codes, p, n, word)
#define Output_1(ci,c,a)	BLOCK(Output_0(ci, c); \
				      Output_a(ci, a))
#define Output_2(ci,c,a0,a1)	BLOCK(Output_1(ci, c, a0); \
				      Output_a(ci, a1))
#define Output_3(ci,c,a0,a1,a2) BLOCK(Output_2(ci, c, a0, a1); \
				      Output_a(ci, a2))
#define Output_n(ci,c,p,n)	BLOCK(Output_0(ci, c); \
				      Output_an(ci,p,n))

#define PC(ci)		entriesBuffer(&(ci)->codes, code)
#define OpCode(ci, pc)	(baseBuffer(&(ci)->codes, code)[pc])

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable table operations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

forwards int	compileBody(Word, code, compileInfo * ARG_LD);
forwards int	compileArgument(Word, int, compileInfo * ARG_LD);
forwards int	compileSubClause(Word, code, compileInfo *);
forwards bool	isFirstVarSet(VarTable vt, int n);
forwards int	balanceVars(VarTable, VarTable, compileInfo *);
forwards void	orVars(VarTable, VarTable);
forwards int	compileListFF(word arg, compileInfo *ci ARG_LD);
forwards bool	compileSimpleAddition(Word, compileInfo * ARG_LD);
#if O_COMPILE_ARITH
forwards int	compileArith(Word, compileInfo * ARG_LD);
forwards bool	compileArithArgument(Word, compileInfo * ARG_LD);
#endif
#if O_COMPILE_IS
forwards int	compileBodyUnify(Word arg, compileInfo *ci ARG_LD);
forwards int	compileBodyEQ(Word arg, compileInfo *ci ARG_LD);
forwards int	compileBodyNEQ(Word arg, compileInfo *ci ARG_LD);
#endif
forwards int	compileBodyVar1(Word arg, compileInfo *ci ARG_LD);
forwards int	compileBodyNonVar1(Word arg, compileInfo *ci ARG_LD);

static void	initMerge(CompileInfo ci);
static int	mergeInstructions(CompileInfo ci, const vmi_merge *m, vmi c);

static inline int
isIndexedVarTerm(word w ARG_LD)
{ if ( isVarInfo(w) )
  { VarDef v = varInfo(w);
    return v->offset;
  }

  return -1;
}

static void
clearVarTable(compileInfo *ci)
{ int *pi = ci->used_var->entry;
  int n   = ci->vartablesize;

  ci->used_var->isize = n;
  while(--n >= 0)
    *pi++ = 0;
}

static bool
isFirstVarSet(VarTable vt, int n)
{ int m  = 1 << (n % BITSPERINT);
  int *p = &vt->entry[n / BITSPERINT];

  if ( (*p & m) )
    return FALSE;
  *p |= m;
  return TRUE;
}


static bool
isFirstVar(VarTable vt, int n)
{ int m  = 1 << (n % BITSPERINT);
  int *p = &vt->entry[n / BITSPERINT];

  return (*p & m) == 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Emit  C_VAR  statements  for  all  variables  that  are  claimed  to  be
uninitialised in valt1 and initialised in valt2.   It is quite common to
have sequences C_VAR(5), C_VAR(6),  ...   Such  sequences are translated
into C_VAR_N(5,4), where the example 4 represents the sequence length.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

typedef struct c_var_state
{ int at;
  int count;
} c_var_state;

static void
c_var(c_var_state *s, int at, compileInfo *ci)
{ if ( s->count == 0 )
  { s->count = 1;
    s->at = at;
  } else if ( at == s->at+1 )
  { s->count++;
  } else
  { if ( s->count == 1 )
      Output_1(ci, C_VAR, s->at);
    else
      Output_2(ci, C_VAR_N, s->at, s->count);
    s->at = at;
    s->count = 1;
  }
}


static int
balanceVars(VarTable valt1, VarTable valt2, compileInfo *ci)
{ int *p1 = &valt1->entry[0];
  int *p2 = &valt2->entry[0];
  int vts = ci->vartablesize;
  int n;
  int done = 0;
  c_var_state vstate = {0};

  for( n = 0; n < vts; p1++, p2++, n++ )
  { int m = (~(*p1) & *p2);

    if ( m )
    { unsigned int i;

      for(i = 0; i < BITSPERINT; i++)
      { if ( m & (1 << i) )
	{ c_var(&vstate, VAROFFSET(n * BITSPERINT + i), ci);
	  done++;
	}
      }
    }
  }
  if ( vstate.count )
    c_var(&vstate, 0, ci);

  return done;
}

static void
orVars(VarTable valt1, VarTable valt2)
{ int *p1 = &valt1->entry[0];
  int *p2 = &valt2->entry[0];
  int n;

  for( n = 0; n < valt1->isize; n++ )
    *p1++ |= *p2++;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setVars() marks all variables that appear in the argument term.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
setVars(Word t, VarTable vt ARG_LD)
{ int index;

last_arg:

  deRef(t);
  if ( (index = isIndexedVarTerm(*t PASS_LD)) >= 0 )
  { isFirstVarSet(vt, index);
    return;
  }

  if ( isTerm(*t) )
  { int arity;

    arity = arityTerm(*t);

    for(t = argTermP(*t, 0); --arity > 0; t++)
      setVars(t, vt PASS_LD);
    goto last_arg;
  }
}


static VarTable
copyVarTable(VarTable to, VarTable from)
{ int *t = to->entry;
  int *f = from->entry;
  int n  = from->isize;

  to->isize = n;
  while(--n>=0)
    *t++ = *f++;

  return to;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Reset all variables we initialised to the variable analysis  functor  to
become variables again.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
resetVars(ARG1_LD)
{ int n;

  for(n=0; n < LD->comp.filledVars; n++)
  { VarDef vd = LD->comp.vardefs[n];

    if ( vd->address )
    { *vd->address = vd->saved;
    }
  }

  LD->comp.filledVars = 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
True if p points to a first-var allocated at *i.  P is dereferenced.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
isFirstVarP(Word p, compileInfo *ci, int *i ARG_LD)
{ int idx;

  deRef(p);
  if ( (idx=isIndexedVarTerm(*p PASS_LD)) >= 0 &&
       isFirstVar(ci->used_var, idx) )
  { *i = idx;
    succeed;
  }

  fail;
}


static int
allocChoiceVar(CompileInfo ci)
{
#if 0
  int var = VAROFFSET(ci->cut.nextvar);

  if ( ++ci->cut.nextvar > ci->clause->variables )
  { ci->clause->variables = ci->cut.nextvar;
    if ( ci->clause->variables == 0 )
      return PL_error(NULL, 0, NULL, ERR_REPRESENTATION,
		      ATOM_max_frame_size);
  }
#else
  int var = VAROFFSET(ci->clause->variables++);

  if ( ci->clause->variables == 0 )
    return PL_error(NULL, 0, NULL, ERR_REPRESENTATION,
		    ATOM_max_frame_size);
#endif

  return var;
}


		 /*******************************
		 *	   TARGET MODULES	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get the target module for Module:Term or Goal@Module. This is only valid
if Module is either an atom  or   a  non-first-var. Other cases raise an
error.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
getTargetModule(target_module *tm, Word t, CompileInfo ci ARG_LD)
{ int iv;

  deRef(t);
  if ( (iv=isIndexedVarTerm(*t PASS_LD)) >= 0 )
  { if ( ci->islocal || !isFirstVar(ci->used_var, iv) )
    { tm->var_index = iv;
      tm->type = TM_VAR;
    } else
    { PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
      return FALSE;
    }
  } else if ( isTextAtom(*t) )
  { tm->module = lookupModule(*t);
    tm->type = TM_MODULE;
  } else
  { resetVars(PASS_LD1);
    PL_error(NULL, 0, NULL,
	     ERR_TYPE, ATOM_module, pushWordAsTermRef(t));
    popTermRef();
    return FALSE;
  }

  return TRUE;
}


static int
pushTargetModule(target_module *tm, CompileInfo ci)
{ if ( tm->type == TM_MODULE )
  { Output_1(ci, B_CONST, tm->module->name);
  } else					/* TBD: Handle islocal */
  { int index = tm->var_index;

    if ( index < 3 )
      Output_0(ci, B_VAR0+index);
    else
      Output_1(ci, B_VAR, VAROFFSET(index));
  }

  return TRUE;
}



		 /*******************************
		 *      INSTRUCTION MERGING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These  functions  provide  a  small    state-machine   that  merges  new
instructions with the previous one. The  declarations of which sequences
to merge are defined in initVMIMerge().

TBD: After reduction, we should try reducing   with the previous one, as
in: X, Y, Z --> X, YZ --> XYZ.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static vmi_merge *merge_def[I_HIGHEST];

static size_t
mergeCount(vmi_merge *m)
{ size_t count = 0;

  while(m->code != I_HIGHEST)
    m++, count++;

  return count;
}

static void
addMerge(vmi c, vmi_merge *m)
{ if ( merge_def[c] )
  { size_t n = mergeCount(merge_def[c]);

    merge_def[c] = realloc(merge_def[c], sizeof(*m)*(n+2));
    merge_def[c][n] = *m;
    merge_def[c][n+1].code = I_HIGHEST;
  } else
  { merge_def[c] = malloc(sizeof(*m)*2);
    merge_def[c][0] = *m;
    merge_def[c][1].code = I_HIGHEST;
  }
}

static void
mergeSeq(vmi c1, vmi c2, vmi op, int ac, ...)
{ va_list args;
  vmi_merge m;
  int i;

  m.code     = c2;
  m.how      = VMI_REPLACE;
  m.merge_op = op;
  m.merge_ac = ac;

  va_start(args, ac);
  for(i=0; i<ac; i++)
    m.merge_av[i] = va_arg(args, code);
  va_end(args);

  addMerge(c1, &m);
}

static void
mergeStep(vmi c1, vmi c2)
{ vmi_merge m;

  memset(&m, 0, sizeof(m));
  m.code = c2;
  m.how  = VMI_STEP_ARGUMENT;

  addMerge(c1, &m);
}


static void
initVMIMerge(void)
{ mergeStep(H_VOID_N, H_VOID);

  mergeSeq(H_VOID,   H_VOID,     H_VOID_N,   1, (code)2);
  mergeSeq(H_VOID,   I_ENTER,    I_ENTER,    0);
  mergeSeq(H_VOID_N, I_ENTER,    I_ENTER,    0);
  mergeSeq(H_VOID,   I_EXITFACT, I_EXITFACT, 0);
  mergeSeq(H_VOID_N, I_EXITFACT, I_EXITFACT, 0);
  mergeSeq(H_VOID,   H_POP,      H_POP,      0);
  mergeSeq(H_VOID_N, H_POP,      H_POP,      0);
}


static void
initMerge(CompileInfo ci)
{ ci->mstate.candidates = NULL;
}


static int
mergeInstructions(CompileInfo ci, const vmi_merge *m, vmi c)
{ for(; m->code != I_HIGHEST; m++)
  { if ( m->code == c )
    { switch(m->how)
      { case VMI_REPLACE:
	{ DEBUG(2,
		Sdprintf("Replacing %s at %d due to %s)\n",
			 codeTable[decode(OpCode(ci,ci->mstate.merge_pos))].name,
			 ci->mstate.merge_pos,
			 codeTable[c].name));
	  seekBuffer(&ci->codes, ci->mstate.merge_pos, code);
	  ci->mstate.candidates = NULL;
	  Output_n(ci, m->merge_op, m->merge_av, m->merge_ac);
	  return TRUE;
	}
	case VMI_STEP_ARGUMENT:
	{ DEBUG(2,
		Sdprintf("Stepping argument of %s from %ld\n",
			 codeTable[decode(OpCode(ci, ci->mstate.merge_pos))].name,
			 OpCode(ci, ci->mstate.merge_pos+1)));
	  OpCode(ci, ci->mstate.merge_pos+1)++;
	  return TRUE;
	}
      }
      break;
    }
  }

  return FALSE;
}


static void				/* inline is slower! */
Output_0(CompileInfo ci, vmi c)
{ const vmi_merge *m;

  if ( (m=ci->mstate.candidates) )
  { if ( mergeInstructions(ci, m, c) )
      return;
    ci->mstate.candidates = NULL;
  }

  if ( (m = merge_def[c]) )
  { ci->mstate.candidates = m;
    ci->mstate.merge_pos = PC(ci);
  }

  addBuffer(&(ci)->codes, encode(c), code);
}


		 /*******************************
		 *	CODE GENERATION		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Clause	compileClause(Word head, Word body, Procedure proc, Module module)

This is the entry-point of the compiler. The `head' and `body' arguments
are dereferenced pointers to terms. `proc' is the procedure to which the
clause should be added and `module' the compilation context module. This
may be different from the definition-module of `proc' if a clause of the
form `module1:head :- body' is compiled,  `module1' is used to determine
`proc', while the currently loading module is in `module' and is used to
relate terms to procedures in the body.

A special consideration is the compilation   of  goals to support call/1
(see I_USERCALL0 in pl-wam.c). When compiling  such clauses, lTop points
to the location to build the new   clause and `head' is the NULL-pointer
to indicate our clause has no head.   After compilation, the stack looks
as below:

	Old lTop --> LocalFrame
		     Pseudo arguments
		     Normal body variables
		     VM instructions
		     Clause structure
		     ClauseRef struct
	lTop     -->

This routine fills all from  the  `Pseudo   arguments'  and  part of the
LocalFrame structure:

	FR->clause	Point to the ClauseRef struct
	FR->context	Module argument
        FR->predicate	getProcDefinition(proc)

Using `local' compilation, all variables are   shared  with the original
goal-term and therefore initialised. This implies   there  is no need to
play around with variable tables.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
compileClause(Clause *cp, Word head, Word body,
	      Procedure proc, Module module ARG_LD)
{ compileInfo ci;			/* data base for the compiler */
  struct clause clause;
  Clause cl;
  int rc;

					/* See declaration of struct clause */
  DEBUG(0, assert(sizeof(clause) % sizeof(word) == 0));

  if ( head )
  { ci.islocal      = FALSE;
    ci.subclausearg = 0;
    ci.arity        = proc->definition->functor->arity;
    ci.argvars      = 0;
    clause.flags    = 0;
  } else
  { Word g = varFrameP(lTop, VAROFFSET(1));

    ci.islocal      = TRUE;
    ci.subclausearg = 0;
    ci.argvars	    = 1;
    ci.argvar       = 1;
    ci.arity        = 0;
    clause.flags    = GOAL_CLAUSE;
    *g		    = *body;
  }

  clause.procedure  = proc;
  clause.code_size  = 0;
  clause.source_no  = clause.line_no = 0;
  clause.owner_no   = 0;

  ci.clause = &clause;
  ci.module = module;
  ci.colon_context.type = TM_NONE;
#ifdef O_CALL_AT_MODULE
  ci.at_context.type = TM_NONE;
#endif

  if ( (rc=analyse_variables(head, body, &ci PASS_LD)) < 0 )
  { switch ( rc )
    { case CYCLIC_HEAD:
      case CYCLIC_BODY:
	return PL_error(NULL, 0, NULL,
			ERR_REPRESENTATION, ATOM_cyclic_term);
      case AVARS_MAX:
	return PL_error(NULL, 0, NULL,
			ERR_REPRESENTATION, ATOM_max_frame_size);
      case MEMORY_OVERFLOW:
	return PL_error(NULL, 0, NULL, ERR_NOMEM);
      default:
	assert(0);
    }
  }
  ci.cut.var = 0;
  ci.cut.instruction = 0;
  if ( !ci.islocal )
  { ci.used_var = alloca(sizeofVarTable(ci.vartablesize));
    clearVarTable(&ci);
  } else
    ci.used_var = NULL;

  initBuffer(&ci.codes);
  initMerge(&ci);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
First compile  the  head  of  the  term.   The  arguments  are  compiled
left-to-right.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( head )
  { int n;
    Word arg;

    for ( arg = argTermP(*head, 0), n = 0; n < ci.arity; n++, arg++ )
    { if ( (rc=compileArgument(arg, A_HEAD, &ci PASS_LD)) < 0 )
	goto exit_fail;
    }
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Now compile the body. After the  I_ENTER,   we  check whether we need to
insert an I_CONTEXT instruction to change the context. This is the case,
for predicates for which the body is  defined from another module as the
head and the predicate is not a   meta-predicate. In principle, we could
delay this until we decided there may be a meta-call, but this will harm
automatic update if a predicate is later defined as meta-predicate.

Not that this is also the case for predicates that have previous clauses
that have an I_CONTEXT because we need to reset the context.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( body && *body != ATOM_true )
  { size_t bi;

    if ( head )
    { Definition def = proc->definition;

      Output_0(&ci, I_ENTER);
					/* ok; all live in the same module */
      if ( false(def, P_MFCONTEXT) &&
	   ci.module != def->module &&
	   false(proc->definition, P_TRANSPARENT) )
	set(def, P_MFCONTEXT);

      if ( true(def, P_MFCONTEXT) )
	Output_1(&ci, I_CONTEXT, (code)ci.module);
    }

    bi = PC(&ci);
    if ( (rc=compileBody(body, I_DEPART, &ci PASS_LD)) != TRUE )
    { if ( rc == NOT_CALLABLE )
      {	resetVars(PASS_LD1);
	rc = PL_error(NULL, 0, NULL, ERR_TYPE,
		      ATOM_callable, pushWordAsTermRef(body));
	popTermRef();
      }

      goto exit_fail;
    }
    Output_0(&ci, I_EXIT);
    if ( OpCode(&ci, bi) == encode(I_CUT) )
    { set(&clause, COMMIT_CLAUSE);
    }
  } else
  { set(&clause, UNIT_CLAUSE);		/* fact (for decompiler) */
    Output_0(&ci, I_EXITFACT);
  }

  resetVars(PASS_LD1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Finish up the clause.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  clause.code_size = entriesBuffer(&ci.codes, code);

  if ( head )
  { size_t size  = sizeofClause(clause.code_size);

    cl = PL_malloc_atomic(size);
    memcpy(cl, &clause, sizeofClause(0));
    memcpy(cl->codes, baseBuffer(&ci.codes, code), sizeOfBuffer(&ci.codes));

    GD->statistics.codes += clause.code_size;
  } else
  { LocalFrame fr = lTop;
    Word p0 = argFrameP(fr, clause.variables);
    Word p = p0;
    ClauseRef cref;
    size_t space;

    DEBUG(1, Sdprintf("%d argvars; %d prolog vars; %d vars",
		      ci.argvars, clause.prolog_vars, clause.variables));
    assert(ci.argvars == ci.argvar);

					/* check space */
    space = ( clause.variables*sizeof(word) +
	      sizeofClause(clause.code_size) +
	      SIZEOF_CREF_CLAUSE +
	      (size_t)argFrameP((LocalFrame)NULL, MAXARITY) +
	      sizeof(struct choice)
	    );
    if ( addPointer(lTop, space) >= (void*)lMax )
    { rc = LOCAL_OVERFLOW;
      goto exit_fail;
    }

    cref = (ClauseRef)p;
    p = addPointer(p, SIZEOF_CREF_CLAUSE);
    cref->next = NULL;
    cref->value.clause = cl = (Clause)p;
    memcpy(cl, &clause, sizeofClause(0));
    memcpy(cl->codes, baseBuffer(&ci.codes, code), sizeOfBuffer(&ci.codes));
    p = addPointer(p, sizeofClause(clause.code_size));
    cl->variables += (int)(p-p0);

    fr->clause = cref;
    fr->predicate = getProcDefinition(proc);
    setNextFrameFlags(fr, environment_frame);
    setContextModule(fr, module);

    DEBUG(1, Sdprintf("; now %d vars\n", clause.variables));
    lTop = (LocalFrame)p;
  }

  discardBuffer(&ci.codes);

  *cp = cl;
  return TRUE;

exit_fail:
  resetVars(PASS_LD1);
  discardBuffer(&ci.codes);
  return rc;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
compileBody() compiles the clause's body.  Within a body,  a  number  of
constructs are recognised:

SUBGOAL
    For a subgoal we generate code to push the  arguments  on  the  next
    stack  frame  and finally generate either I_CALL for normal calls or
    I_DEPART for the last subgoal  of  the  clause  to  allow  for  tail
    recursion optimisation.

VARIABLE or META CALL
    Single variables or constructs  of  the  form  term:term  imply  the
    generation of a metacall.

A ; B, A -> B, A -> B ; C, \+ A
    The compilation of these statements are  a  bit  more  tricky.   Two
    mechanisms support this compilation:

	C_IFTHEN var	Mark for `soft-cut'
	C_CUT  var	Cut alternatives generated since C_IFTHEN var

    and

	C_OR jmp	Generate a choicepoint.  It the continuation
			fails skip `jmp' instructions and continue
			there.
	C_JMP jmp	Just skip `jmp' instructions.

    This set  is  augmented  with  some  compound  statements  and  some
    statements  with  different  names,  but equal semantics to help the
    decompiler.  See pl-wam.c for more details.

    NOTE: A tricky bit now is that we  can  reach  the  same  point  via
    different  paths.   Each of these paths may result in another set of
    variables  already  instantiated.   This  gives  troubles  with  the
    FIRSTVAR  type  of instructions.  to avoid such trouble the compiler
    generates  SETVAR  instructions  to  balance  both   brances.    See
    balanceVars();

    If you add anything to this, please ensure registerControlFunctors()
    remains consistent.

\+
    NOT is a bit tricky too.  If it succeeds (i.e. the argument fails),
    there are no variable-bindings done. Unfortunately, variables
    introduced inside the not that are set using B_ARGFIRSTVAR create
    references to terms above the (now unwinded) global stack, but
    the GC clearUninitialisedVarsFrame() won't see any initialisation of
    these variables, leaving them invalid. Same holds for the
    source-level debugger using the same function. Therefore we
    explicitely reset these variables at the end of the code created by
    \+.

    For optimisation-reasons however we can consider them uninitialised
    --- I thought :-( When compiling ( A ; \+ B ) the compilation of ;/2
    examines the variables in both branches using setVars() and or's
    them. Using this simple-minded causes the variables inside B to be
    considered initialised at the end of the ;/2 while they are not.
    For this reason, setVars() does not count variables inside \+/1
    when encounted as a control-structure.

    After 5.2.7: Still more tricky than anticipated.  Consider the clause

	foo :- garbage_collect, (a ; \+ foo(A,A)).

    Here the variable A is allocated, clearUninitialisedVarsFrame() takes
    the first (a) branch and doesn't see it, handling an uninitialised
    slot in the frame to GC.  For the moment we consider \+ the same as
    G->fail;true.  This leads to a bit longer and slower code in some
    cases, but at least it works.  Time to rethink the entire issue around
    uninitialised variables ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
compileBody(Word body, code call, compileInfo *ci ARG_LD)
{
right_argument:
  deRef(body);

  if ( isTerm(*body) )
  { functor_t fd = functorTerm(*body);
    FunctorDef fdef = valueFunctor(fd);

    if ( true(fdef, CONTROL_F) )
    { if ( fd == FUNCTOR_comma2 )			/* A , B */
      { int rv;

	if ( (rv=compileBody(argTermP(*body, 0), I_CALL, ci PASS_LD)) != TRUE )
	  return rv;
	body = argTermP(*body, 1);
	goto right_argument;
#if O_COMPILE_OR
      } else if ( fd == FUNCTOR_semicolon2 ||
		  fd == FUNCTOR_bar2 )		/* A ; B and (A -> B ; C) */
      { Word a0 = argTermP(*body, 0);
	VarTable vsave, valt1, valt2;
	int hard;

	if ( !ci->islocal )
	{ vsave = mkCopiedVarTable(ci->used_var);
	  valt1 = mkCopiedVarTable(ci->used_var);
	  valt2 = mkCopiedVarTable(ci->used_var);

	  setVars(argTermP(*body, 0), valt1 PASS_LD);
	  setVars(argTermP(*body, 1), valt2 PASS_LD);
	} else
	  vsave = valt1 = valt2 = NULL;

	deRef(a0);
	if ( (hard=hasFunctor(*a0, FUNCTOR_ifthen2)) || /* A  -> B ; C */
	     hasFunctor(*a0, FUNCTOR_softcut2) )        /* A *-> B ; C */
	{ int var;
	  size_t tc_or, tc_jmp;
	  int rv;
	  cutInfo cutsave = ci->cut;

	  if ( !(var=allocChoiceVar(ci)) )
	    return FALSE;

	  Output_2(ci, hard ? C_IFTHENELSE : C_SOFTIF, var, (code)0);
	  tc_or = PC(ci);
	  ci->cut.var = var;		/* Cut locally in the condition */
	  ci->cut.instruction = hard ? C_LCUT : C_LSCUT;
	  if ( (rv=compileBody(argTermP(*a0, 0), I_CALL, ci PASS_LD)) != TRUE )
	    return rv;
	  ci->cut = cutsave;
	  Output_1(ci, hard ? C_CUT : C_SOFTCUT, var);
	  if ( (rv=compileBody(argTermP(*a0, 1), call, ci PASS_LD)) != TRUE )
	    return rv;
	  if ( !ci->islocal )
	    balanceVars(valt1, valt2, ci);
	  Output_1(ci, C_JMP, (code)0);
	  tc_jmp = PC(ci);
	  OpCode(ci, tc_or-1) = (code)(PC(ci) - tc_or);
	  if ( !ci->islocal )
	    copyVarTable(ci->used_var, vsave);
	  if ( (rv=compileBody(argTermP(*body, 1), call, ci PASS_LD)) != TRUE )
	    return rv;
	  if ( !ci->islocal )
	    balanceVars(valt2, valt1, ci);
	  OpCode(ci, tc_jmp-1) = (code)(PC(ci) - tc_jmp);
	} else					/* A ; B */
	{ size_t tc_or, tc_jmp;
	  int rv;

	  Output_1(ci, C_OR, (code)0);
	  tc_or = PC(ci);
	  if ( (rv=compileBody(argTermP(*body, 0), I_CALL, ci PASS_LD)) != TRUE )
	    return rv;
	  if ( !ci->islocal )
	    balanceVars(valt1, valt2, ci);
	  Output_1(ci, C_JMP, (code)0);
	  tc_jmp = PC(ci);
	  OpCode(ci, tc_or-1) = (code)(PC(ci) - tc_or);
	  if ( !ci->islocal )
	    copyVarTable(ci->used_var, vsave);
	  if ( (rv=compileBody(argTermP(*body, 1), call, ci PASS_LD)) != TRUE )
	    return rv;
	  if ( !ci->islocal )
	    balanceVars(valt2, valt1, ci);
	  OpCode(ci, tc_jmp-1) = (code)(PC(ci) - tc_jmp);
	}

	if ( !ci->islocal )
	{ orVars(valt1, valt2);
	  copyVarTable(ci->used_var, valt1);
	}

	succeed;
      } else if ( fd == FUNCTOR_ifthen2 ||		/* A -> B */
		  fd == FUNCTOR_softcut2 )
      { int var;
	int rv;
	int hard = (fd == FUNCTOR_ifthen2);
	cutInfo cutsave = ci->cut;

	if ( !(var=allocChoiceVar(ci)) )
	  return FALSE;

	Output_1(ci, hard ? C_IFTHEN : C_SOFTIFTHEN, var);
	ci->cut.var = var;		/* Cut locally in the condition */
	ci->cut.instruction = C_LCUTIFTHEN;
	if ( (rv=compileBody(argTermP(*body, 0), I_CALL, ci PASS_LD)) != TRUE )
	  return rv;
	ci->cut = cutsave;
	Output_1(ci, hard ? C_CUT : C_SCUT, var);
	if ( (rv=compileBody(argTermP(*body, 1), call, ci PASS_LD)) != TRUE )
	  return rv;
	Output_0(ci, C_END);

	succeed;
      } else if ( fd == FUNCTOR_not_provable1 )		/* \+/1 */
      { int var;
	size_t tc_or;
	VarTable vsave;
	int rv;
	cutInfo cutsave = ci->cut;

	if ( !(var=allocChoiceVar(ci)) )
	  return FALSE;

	if ( !ci->islocal )
	  vsave = mkCopiedVarTable(ci->used_var);
	else
	  vsave = NULL;

	Output_2(ci, C_NOT, var, (code)0);
	tc_or = PC(ci);
	ci->cut.var = var;
	ci->cut.instruction = C_LCUT;
	if ( (rv=compileBody(argTermP(*body, 0), I_CALL, ci PASS_LD)) != TRUE )
	  return rv;
	ci->cut = cutsave;
	Output_1(ci, C_CUT, var);
	Output_0(ci, C_FAIL);
	if ( ci->islocal )
	{ OpCode(ci, tc_or-1) = (code)(PC(ci) - tc_or);
	} else
	{ size_t tc_jmp;

	  Output_1(ci, C_JMP, (code)0);
	  tc_jmp = PC(ci);
	  OpCode(ci, tc_or-1) = (code)(PC(ci) - tc_or);
	  if ( balanceVars(vsave, ci->used_var, ci) > 0 )
	  { /*copyVarTable(ci->used_var, vsave);   see comment above */
	    OpCode(ci, tc_jmp-1) = (code)(PC(ci) - tc_jmp);
	  } else			/* delete the jmp */
	  { seekBuffer(&ci->codes, tc_jmp-2, code);
	    OpCode(ci, tc_or-1) = (code)(PC(ci) - tc_or);
	  }
	}

	succeed;
#endif /* O_COMPILE_OR */
      } else if ( fd == FUNCTOR_colon2 )	/* Module:Goal */
      { target_module tmsave = ci->colon_context;
	int rc;

	if ( (rc=getTargetModule(&ci->colon_context,
				 argTermP(*body, 0), ci PASS_LD)) != TRUE )
	  return rc;
	rc = compileBody(argTermP(*body, 1), call, ci PASS_LD);
	ci->colon_context = tmsave;

	return rc;
#ifdef O_CALL_AT_MODULE
      } else if ( fd == FUNCTOR_xpceref2 )	/* Call@Module */
      { target_module atsave = ci->at_context;
	int rc;

	if ( (rc=getTargetModule(&ci->at_context,
				 argTermP(*body, 1), ci PASS_LD)) != TRUE )
	  return rc;
	rc = compileBody(argTermP(*body, 0), call, ci PASS_LD);
	ci->at_context = atsave;

	return rc;
#endif /*O_CALL_AT_MODULE*/
      }
      assert(fdef->name == ATOM_call);
    }
  }

  return compileSubClause(body, call, ci);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
compileArgument() is the key function of the compiler.  Its function  is
to   generate  the  term  matching/construction  instructions  both  for
arguments of the head as for arguments to subclauses.   It  distinguises
three  different  places:  compiling plain arguments to the head (HEAD),
arguments of terms occurring in the head (HEADARG) and body arguments
(BODY).

The  isIndexedVar()  macro  detects  a   term   has   been   filled   by
analyseVariables()  and  returns the offset of the variable, or -1 if it
is not produced by this function.

When doing `islocal' compilation,  compound  terms   are  copied  to the
current localframe and a B_VAR instruction is  generated for it. In this
case it can return LOCAL_OVERFLOW.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_DEBUG
static char *
vName(Word adr)
{ GET_LD
  static char name[32];

  deRef(adr);

  if (adr > (Word) lBase)
    Ssprintf(name, "_L%ld", (Word)adr - (Word)lBase);
  else
    Ssprintf(name, "_G%ld", (Word)adr - (Word)gBase);

  return name;
}
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Compile argument to a goal in the   clause. The `islocal' compilation is
one of the complicating factors: atoms   should not be registered (there
is no need as they are  held  by   the  term  anyway). For `big' objects
(strings and compounds) the system should create `argvar' references.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
compileArgument(Word arg, int where, compileInfo *ci ARG_LD)
{ int index;
  bool first;

  deRef(arg);

right_recursion:

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A void.  Generate either B_VOID or H_VOID.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  switch(tag(*arg))
  { case TAG_VAR:
      if ( isVarInfo(*arg) )
	goto isvar;
    var:
      if (where & A_BODY)
      { Output_0(ci, B_VOID);
	return TRUE;
      }
      Output_0(ci, H_VOID);
      return TRUE;
    case TAG_ATTVAR:
      if ( ci->islocal )
      { goto argvar;
      } else
      { goto var;
      }
    case TAG_INTEGER:
      if ( storage(*arg) != STG_INLINE )
      {	Word p = addressIndirect(*arg);
	size_t n = wsizeofInd(*p);

	if ( n == WORDS_PER_INT64 )
	{
#if ( SIZEOF_VOIDP == 8 )
          int64_t val = *(int64_t*)(p+1);
#else
	  union
	  { int64_t i;
	    word w[WORDS_PER_INT64];
	  } cvt;
	  int64_t val;

	  cvt.w[0] = p[1];
	  cvt.w[1] = p[2];
	  val = cvt.i;
#endif

#if SIZEOF_VOIDP == 8
          Output_1(ci, (where&A_HEAD) ? H_INTEGER : B_INTEGER, (intptr_t)val);
#else
          if ( val >= LONG_MIN && val <= LONG_MAX )
	  { Output_1(ci, (where&A_HEAD) ? H_INTEGER : B_INTEGER, (intptr_t)val);
	  } else
	  { int c = ((where&A_HEAD) ? H_INT64 : B_INT64);
	    Output_n(ci, c, (Word)&val, WORDS_PER_INT64);
	  }
#endif
	} else				/* MPZ NUMBER */
	{ int c = (where & A_HEAD) ? H_MPZ : B_MPZ;
	  Output_n(ci, c, p, n+1);
	  return TRUE;
	}
	return TRUE;
      }
      Output_1(ci, (where & A_BODY) ? B_CONST : H_CONST, *arg);
      return TRUE;
    case TAG_ATOM:
      if ( isNil(*arg) )
      {	Output_0(ci, (where & A_BODY) ? B_NIL : H_NIL);
      } else
      { if ( !ci->islocal )
	  PL_register_atom(*arg);
	Output_1(ci, (where & A_BODY) ? B_CONST : H_CONST, *arg);
      }
      return TRUE;
    case TAG_FLOAT:
    { Word p = valIndirectP(*arg);
      int c =  (where & A_BODY) ? B_FLOAT : H_FLOAT;

      Output_n(ci, c, p, WORDS_PER_DOUBLE);
      return TRUE;
    }
    case TAG_STRING:
    if ( ci->islocal )
    { goto argvar;
    } else
    { Word p = addressIndirect(*arg);
      size_t n = wsizeofInd(*p);
      int c = (where & A_HEAD) ? H_STRING : B_STRING;

      Output_n(ci, c, p, n+1);
      return TRUE;
    }
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Non-void variables. There are many cases for this.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

isvar:
  if ( (index = isIndexedVarTerm(*arg PASS_LD)) >= 0 )
  { if ( ci->islocal )
    { VarDef v = LD->comp.vardefs[*arg>>LMASK_BITS];
      int voffset = VAROFFSET(index);
      Word k = varFrameP(lTop, voffset);

      DEBUG(1, Sdprintf("Linking b_var(%d) to %s\n",
			index, vName(v->address)));

      if ( k >= (Word) lMax )
	return LOCAL_OVERFLOW;
      *k = makeRef(v->address);

      if ( index < 3 )
      { Output_0(ci, B_VAR0 + index);
      } else
      { Output_1(ci, B_VAR, voffset);
      }

      return TRUE;
    }

    first = isFirstVarSet(ci->used_var, index);

    if ( index < ci->arity )		/* variable on its own in the head */
    { if ( where & A_BODY )
      { if ( where & A_ARG )
	{ Output_0(ci, B_ARGVAR);
	} else
	{ if ( index < 3 )
	  { Output_0(ci, B_VAR0 + index);
	    return TRUE;
	  }
	  Output_0(ci, B_VAR);
	}
      } else				/* head */
      { if ( !(where & A_ARG) && first )
	{ Output_0(ci, H_VOID);
	  return TRUE;
	}
	Output_0(ci, H_VAR);
      }
      Output_a(ci, VAROFFSET(index));

      return TRUE;
    }

    /* normal variable (i.e. not shared in the head and non-void) */
    if( where & A_BODY )
    { if ( where & A_ARG )
      { Output_0(ci, first ? B_ARGFIRSTVAR : B_ARGVAR);
      } else
      { if ( index < 3 && !first )
	{ Output_0(ci, B_VAR0 + index);
	  return TRUE;
	}
	Output_0(ci, first ? B_FIRSTVAR : B_VAR);
      }
    } else
    { Output_0(ci, first ? H_FIRSTVAR : H_VAR);
    }

    Output_a(ci, VAROFFSET(index));

    return TRUE;
  }

  assert(isTerm(*arg));

  if ( ci->islocal && !(where&A_NOARGVAR) )
  { int voffset;
    Word k;

  argvar:
    voffset = VAROFFSET(ci->argvar);
    k = varFrameP(lTop, voffset);

    if ( k >= (Word)lMax )
      return LOCAL_OVERFLOW;

    if ( isAttVar(*arg) )		/* attributed variable: must make */
      *k = makeRef(arg);		/* a reference to avoid binding a */
    else				/* copy! */
      *k = *arg;
    if ( ci->argvar < 3 )
    { Output_0(ci, B_VAR0 + ci->argvar);
    } else
    { Output_1(ci, B_VAR, voffset);
    }
    DEBUG(MSG_COMP_ARGVAR,
	  Sdprintf("Using argvar %d\n", ci->argvar));
    ci->argvar++;

    return TRUE;
  } else
  { int ar;
    functor_t fdef;
    int isright = (where & A_RIGHT);

    fdef = functorTerm(*arg);
    if ( fdef == FUNCTOR_dot2 )
    { code c;

      if ( (where & A_HEAD) )		/* index in array! */
      { if ( compileListFF(*arg, ci PASS_LD) )
	  return TRUE;
	c = (isright ? H_RLIST : H_LIST);
      } else
      { c = (isright ? B_RLIST : B_LIST);
      }

      Output_0(ci, c);
    } else
    { code c;

      if ( (where & A_HEAD) )		/* index in array! */
	c = (isright ? H_RFUNCTOR : H_FUNCTOR);
      else
	c = (isright ? B_RFUNCTOR : B_FUNCTOR);

      Output_1(ci, c, (word)fdef);
    }
    ar = arityFunctor(fdef);
    where &= ~(A_RIGHT|A_NOARGVAR);
    where |= A_ARG;

    for(arg = argTermP(*arg, 0); --ar > 0; arg++)
    { int rc;

      if ( (rc=compileArgument(arg, where, ci PASS_LD)) < 0 )
	return rc;
    }

    where |= A_RIGHT;
    deRef(arg);

    if ( isVar(*arg) && !(where & (A_BODY|A_ARG)) )
    { if ( !isright )
	Output_0(ci, H_POP);
      return TRUE;
    }

    if ( isright )
    { goto right_recursion;
    } else
    { code c;
      int rc;

      if ( (rc=compileArgument(arg, where, ci PASS_LD)) < 0 )
	return rc;
      c = (where & A_HEAD) ? H_POP : B_POP;
      Output_0(ci, c);
    }

    return TRUE;
  }
}


static int
compileListFF(word arg, compileInfo *ci ARG_LD)
{ Word p = argTermP(arg, 0);
  int i1, i2;

  if ( isFirstVarP(p+0, ci, &i1 PASS_LD) &&
       isFirstVarP(p+1, ci, &i2 PASS_LD) &&
       i1 != i2 )
  { isFirstVarSet(ci->used_var, i1);
    isFirstVarSet(ci->used_var, i2);
    Output_2(ci, H_LIST_FF, VAROFFSET(i1), VAROFFSET(i2));
    succeed;
  }

  fail;
}


static inline code
mcall(code call)
{ switch(call)
  { case I_CALL:
      return I_CALLM;
    case I_DEPART:
      return I_DEPARTM;
    default:
      assert(0);
      return (code)0;
  }
}


#ifdef O_CALL_AT_MODULE
static inline code
callatm(code call)
{ switch(call)
  { case I_CALL:
      return I_CALLATM;
    case I_DEPART:
      return I_DEPARTATM;
    default:
      assert(0);
      return (code)0;
  }
}

static inline code
callatmv(code call)
{ switch(call)
  { case I_CALL:
      return I_CALLATMV;
    case I_DEPART:
      return I_DEPARTATMV;
    default:
      assert(0);
      return (code)0;
  }
}
#endif


static Procedure
lookupBodyProcedure(functor_t functor, Module tm)
{ Procedure proc;

  if ( (proc = isCurrentProcedure(functor, tm)) &&
       ( isDefinedProcedure(proc) ||
	 true(proc->definition, P_REDEFINED)
       )
     )
    return proc;

  if ( tm != MODULE_system &&
       (proc = isCurrentProcedure(functor, MODULE_system)) &&
       isDefinedProcedure(proc) &&
       !GD->bootsession )
    return proc;

  return lookupProcedure(functor, tm);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Compile a single subclause. compileBody() already   took care of control
structures, including :/2 and @/2. This means that the goal is one of:

    - unbound (variable in the body)
    - Some compiler reserved goal (true, !, ...)
    - A normal Prolog goal
    - Not callable (an error)

The context left operators @/2 and   :/2 are available as ci->at_context
and ci->colon_context.

If the goal needs to be translated   into a meta-call, (e.g., calls like
Var:g(x)), we must be aware that   the variable-analyser considers g/1 a
goal and does not make this term a   candidate for the argvar trick used
for `local' compilation.  Therefore, we use A_NOARGVAR.

TBD: A remaining problem is  that   calls  to  non-meta-predicate system
predicates is still subject to  meta-calling   if  the  colon-context is
unbound (e.g. Var:is_list(X)).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
compileSubClause(Word arg, code call, compileInfo *ci)
{ GET_LD
  functor_t functor;
  FunctorDef fdef;
  Procedure proc;
  Module tm;				/* lookup module */

  deRef(arg);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A non-void variable. Create a I_USERCALL0 instruction for it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  if ( isIndexedVarTerm(*arg PASS_LD) >= 0 )
  { int rc;

  metacall:
#ifdef O_CALL_AT_MODULE
    if ( ci->at_context.type != TM_NONE )
    { Output_1(ci, B_FUNCTOR, FUNCTOR_xpceref2);
    }
#endif
    if ( ci->colon_context.type != TM_NONE )
    { Output_1(ci, B_FUNCTOR, FUNCTOR_colon2);
      pushTargetModule(&ci->colon_context, ci);
      if ( (rc=compileArgument(arg, A_BODY|A_RIGHT|A_NOARGVAR, ci PASS_LD)) < 0 )
	return rc;
      Output_0(ci, B_POP);
    } else
    { if ( (rc=compileArgument(arg, A_BODY|A_NOARGVAR, ci PASS_LD)) < 0 )
	return rc;
    }
#ifdef O_CALL_AT_MODULE
    if ( ci->at_context.type != TM_NONE )
    { pushTargetModule(&ci->at_context, ci);
      Output_0(ci, B_POP);
    }
#endif

    Output_0(ci, I_USERCALL0);
    succeed;
  }

  if ( isTerm(*arg) )
  { functor = functorTerm(*arg);
    fdef = valueFunctor(functor);

    if ( true(fdef, ARITH_F) && !ci->islocal )
    { if ( functor == FUNCTOR_is2 &&
	   compileSimpleAddition(arg, ci PASS_LD) )
	succeed;
#if O_COMPILE_ARITH
      if ( truePrologFlag(PLFLAG_OPTIMISE) )
	 return compileArith(arg, ci PASS_LD);
#endif
    }

#ifdef O_COMPILE_IS
    if ( !ci->islocal )
    { if ( functor == FUNCTOR_equals2 )	/* =/2 */
      { int rc;

	if ( (rc=compileBodyUnify(arg, ci PASS_LD)) != FALSE )
	  return rc;
      } else if ( functor == FUNCTOR_strict_equal2 )	/* ==/2 */
      { int rc;

	if ( (rc=compileBodyEQ(arg, ci PASS_LD)) != FALSE )
	  return rc;
      } else if ( functor == FUNCTOR_not_strict_equal2 ) /* \==/2 */
      { int rc;

	if ( (rc=compileBodyNEQ(arg, ci PASS_LD)) != FALSE )
	  return rc;
      } else if ( functor == FUNCTOR_var1 )
      { int rc;

	if ( (rc=compileBodyVar1(arg, ci PASS_LD)) != FALSE )
	  return rc;
      } else if ( functor == FUNCTOR_nonvar1 )
      { int rc;

	if ( (rc=compileBodyNonVar1(arg, ci PASS_LD)) != FALSE )
	  return rc;
      }
    }
#endif
  } else if ( isTextAtom(*arg) )
  { if ( *arg == ATOM_cut )
    { if ( ci->cut.var )			/* local cut for \+ */
	Output_1(ci, ci->cut.instruction, ci->cut.var);
      else
	Output_0(ci, I_CUT);
      succeed;
    } else if ( *arg == ATOM_true )
    { Output_0(ci, I_TRUE);
      succeed;
    } else if ( *arg == ATOM_fail )
    { Output_0(ci, I_FAIL);
      succeed;
    } else if ( *arg == ATOM_dcatch )		/* $catch */
    { Output_0(ci, I_CATCH);
      Output_0(ci, I_EXITCATCH);
      succeed;
    } else if ( *arg == ATOM_dcall_cleanup )	/* $call_cleanup */
    { Output_0(ci, I_CALLCLEANUP);
      Output_0(ci, I_EXITCLEANUP);
      succeed;
    } else if ( *arg == ATOM_dcut )		/* $cut */
    { Output_0(ci, I_CUTCHP);
      succeed;
    } else
    { functor = lookupFunctorDef(*arg, 0);
      fdef = NULL;				/* NULL --> no arguments */
    }
  } else
  { return NOT_CALLABLE;
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Var:Goal call. Handle using meta-calls. TBD:  push this further down the
line; if Goal is  a  reserved  system   predicate  that  is  not a meta-
predicate we can deal with it anyway.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( ci->colon_context.type == TM_VAR )
    goto metacall;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Calling  a  normal  predicate:  push  the  arguments  and  generate  the
appropriate calling instruction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( fdef )				/* term: there are arguments */
  { int ar = fdef->arity;

    for(arg = argTermP(*arg, 0); ar > 0; ar--, arg++)
    { int rc;

      if ( (rc=compileArgument(arg, A_BODY, ci PASS_LD)) < 0 )
	return rc;
    }

    if ( fdef->name == ATOM_call )
    { if ( ci->colon_context.type == TM_NONE
#ifdef O_CALL_AT_MODULE
	   && ci->at_context.type == TM_NONE
#endif
	 )
      { if ( fdef->arity == 1 )
	  Output_0(ci, I_USERCALL0);
	else
	  Output_1(ci, I_USERCALLN, (code)(fdef->arity - 1));
	return TRUE;
      }
    }
  }

  tm = (ci->colon_context.type == TM_MODULE ? ci->colon_context.module
					    : ci->module);
  proc = lookupBodyProcedure(functor, tm);

#ifdef O_CALL_AT_MODULE
  if ( ci->at_context.type != TM_NONE )
  { if ( ci->at_context.type == TM_MODULE )
    { Module cm = ci->at_context.module;
      code ctm = (tm==ci->module) ? (code)0 : (code)tm;

      Output_3(ci, callatm(call), ctm, (code)cm, (code)proc);
    } else
    { int idx = ci->at_context.var_index;
      code ctm = (tm==ci->module) ? (code)0 : (code)tm;

      Output_3(ci, callatmv(call), ctm, VAROFFSET(idx), (code)proc);
    }
  } else
#endif
  { if ( tm == ci->module )
      Output_1(ci, call, (code) proc);
    else
      Output_2(ci, mcall(call), (code)tm, (code)proc);
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
compileSimpleAddition() compiles NewVar is Var   +/- SmallInt. At entry,
vc is known to be a dereferenced pointer to term is/2.  For addition, it
allows for swapping the arguments.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
compileSimpleAddition(Word sc, compileInfo *ci ARG_LD)
{ Word a = argTermP(*sc, 0);
  int rvar;

  if ( isFirstVarP(a, ci, &rvar PASS_LD) ) /* NewVar is ? */
  { int neg = FALSE;

    a++;
    deRef(a);

    if ( hasFunctor(*a, FUNCTOR_plus2) ||
	 (neg=hasFunctor(*a, FUNCTOR_minus2)) )
    { Word a1 = argTermP(*a, 0);
      Word a2 = a1 + 1;
      int vi, swapped = 0;

      deRef(a1);
      deRef(a2);

      while(swapped++ < 2)
      { Word tmp;

	if ( (vi=isIndexedVarTerm(*a1 PASS_LD)) >= 0 &&
	     !isFirstVar(ci->used_var, vi) &&
	     tagex(*a2) == (TAG_INTEGER|STG_INLINE) )
	{ intptr_t i = valInt(*a2);

	  if ( neg )
	    i = -i;			/* tagged int: cannot overflow */

	  isFirstVarSet(ci->used_var, rvar);
	  Output_3(ci, A_ADD_FC, VAROFFSET(rvar), VAROFFSET(vi), i);
	  succeed;
	}

	if ( neg )
	  break;			/* do not swap X is 10 - Y */

	tmp = a1;
	a1 = a2;
	a2 = tmp;
      }
    }
  }

  fail;
}


#if O_COMPILE_ARITH
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Arithmetic compilation compiles is/2, >/2, etc.  Instead of building the
compound terms holding the arithmetic expression as  a  whole  and  then
calling  is/2,  etc.  to evaluate the result, a stack machine is used to
compute the value.  The ARGP virtual machine register, normally used  in
body  mode to push the arguments to the next functioncall now is used to
push the arguments to the arithmetic functions.  Normally, a term f(a,b)
is translated to:

	* Create f and set ARGP to point to first argument of f
	* Push a and b via ARGP
	* pop ARGP

This constructs a term.  In arithmetic mode, we generate:

	* Push a and b via ARGP
	* Call f/2 to pick the top two words from the stack and push
	  the result back onto it.

This has two advantages: No term is created on the global stack and  the
mapping  between  the  term  and  the arithmetic function is done by the
compiler rather than the evaluation routine.

OUT-OF-DATE: now pushes *numbers* rather then tagged Prolog data structures.

Note. This function assumes the functors   of  all arithmetic predicates
are tagged using the functor ARITH_F. See registerArithFunctors().

Returns one of TRUE or *_OVERFLOW
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
compileArith(Word arg, compileInfo *ci ARG_LD)
{ code a_func;
  functor_t fdef = functorTerm(*arg);

  if      ( fdef == FUNCTOR_ar_equals2 )	a_func = A_EQ;	/* =:= */
  else if ( fdef == FUNCTOR_ar_not_equal2 )	a_func = A_NE;	/* =\= */
  else if ( fdef == FUNCTOR_smaller2 )		a_func = A_LT;	/* < */
  else if ( fdef == FUNCTOR_larger2 )		a_func = A_GT;	/* > */
  else if ( fdef == FUNCTOR_smaller_equal2 )	a_func = A_LE;	/* =< */
  else if ( fdef == FUNCTOR_larger_equal2 )	a_func = A_GE;	/* >= */
  else if ( fdef == FUNCTOR_is2 )				/* is */
  { size_t tc_a1 = PC(ci);
    code isvar;
    int rc;

    rc=compileArgument(argTermP(*arg, 0), A_BODY, ci PASS_LD);
    if ( rc != TRUE )
      return rc;
    if ( PC(ci) == tc_a1 + 2 &&	OpCode(ci, tc_a1) == encode(B_FIRSTVAR) )
    { isvar = OpCode(ci, tc_a1+1);
      seekBuffer(&ci->codes, tc_a1, code);
    } else
      isvar = 0;
    Output_0(ci, A_ENTER);
    rc = compileArithArgument(argTermP(*arg, 1), ci PASS_LD);
    if ( rc != TRUE )
      return rc;
    if ( isvar )
      Output_1(ci, A_FIRSTVAR_IS, isvar);
    else
      Output_0(ci, A_IS);
    return TRUE;
  } else
  { assert(0);			/* see pl-func.c, registerArithFunctors() */
    fail;
  }

  Output_0(ci, A_ENTER);
  if ( !compileArithArgument(argTermP(*arg, 0), ci PASS_LD) ||
       !compileArithArgument(argTermP(*arg, 1), ci PASS_LD) )
    fail;

  Output_0(ci, a_func);

  return TRUE;
}


static int
compileArithArgument(Word arg, compileInfo *ci ARG_LD)
{ int index;

  deRef(arg);

  if ( isInteger(*arg) )
  { if ( storage(*arg) == STG_INLINE )
    { Output_1(ci, A_INTEGER, valInt(*arg));
    } else
    { Word p = addressIndirect(*arg);
      size_t  n = wsizeofInd(*p);

      if ( n == sizeof(int64_t)/sizeof(word) )
      { p++;
	{
#if SIZEOF_VOIDP == 8
	  int64_t val = *(int64_t*)p;
	  Output_1(ci, A_INTEGER, val);
#else
	  union
	  { int64_t val;
	    word w[WORDS_PER_INT64];
	  } cvt;
	  Word vp = cvt.w;

	  cpInt64Data(vp, p);

	  if ( cvt.val >= LONG_MIN && cvt.val <= LONG_MAX )
	  { Output_1(ci, A_INTEGER, (word)cvt.val);
	  } else
	  { Output_n(ci, A_INT64, cvt.w, WORDS_PER_INT64);
	  }
#endif
	}
      } else				/* GMP */
      { Output_n(ci, A_MPZ, p, n+1);
      }
    }
    succeed;
  }
  if ( isFloat(*arg) )
  { Word p = valIndirectP(*arg);

    Output_n(ci, A_DOUBLE, p, WORDS_PER_DOUBLE);
    succeed;
  }
					/* variable */
  if ( (index = isIndexedVarTerm(*arg PASS_LD)) >= 0 )
  { int first = isFirstVarSet(ci->used_var, index);

    if ( index < ci->arity )		/* shared in the head */
    { if ( index < 3 )
      { Output_0(ci, A_VAR0 + index);
	succeed;
      }
      Output_0(ci, A_VAR);
    } else
    { if ( index < 3 && !first )
      { Output_0(ci, A_VAR0 + index);
        succeed;
      }
      if ( first )
      { resetVars(PASS_LD1);		/* get clean Prolog data, assume */
					/* calling twice is ok */
	PL_error(NULL, 0, "Unbound variable in arithmetic expression",
		 ERR_TYPE, ATOM_evaluable, pushWordAsTermRef(arg));
	popTermRef();
	return FALSE;
      }
      Output_0(ci, A_VAR);
    }
    Output_a(ci, VAROFFSET(index));
    succeed;
  }

  if ( isVar(*arg) )			/* void variable */
  { PL_error(NULL, 0, "Unbound variable in arithmetic expression",
	     ERR_TYPE, ATOM_evaluable, pushWordAsTermRef(arg));
    popTermRef();
    return FALSE;
  }

  { functor_t fdef;
    int n, ar;
    Word a;

    if ( isTextAtom(*arg) )
    { fdef = lookupFunctorDef(*arg, 0);
      ar = 0;
      a = NULL;
    } else if ( isTerm(*arg) )
    { fdef = functorTerm(*arg);
      ar = arityFunctor(fdef);
      a = argTermP(*arg, 0);
    } else
    { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_evaluable, pushWordAsTermRef(arg));
      popTermRef();
      return FALSE;
    }

    if ( fdef == FUNCTOR_dot2 )		/* "char" */
    { Word a2;
      int chr;

      deRef2(a+1, a2);
      if ( !isNil(*a2) )
      { PL_error(".", 2, "\"x\" must hold one character", ERR_TYPE,
		 ATOM_nil, pushWordAsTermRef(a2));
	popTermRef();
	return FALSE;
      }
      deRef2(a, a2);
      if ( !isVar(*a2) && isIndexedVarTerm(*a2 PASS_LD) < 0 )
      { if ( (chr=arithChar(a2 PASS_LD)) == EOF )
	  fail;

	Output_1(ci, A_INTEGER, chr);
	succeed;
      } else
	return PL_error(".", 2, "Cannot handle [X]",
			ERR_INSTANTIATION);
    }

    if ( (index = indexArithFunction(fdef)) < 0 )
    { PL_error(NULL, 0, "No such arithmetic function",
	       ERR_TYPE, ATOM_evaluable, pushWordAsTermRef(arg));
      popTermRef();
      return FALSE;
    }

    for(n=0; n<ar; a++, n++)
      TRY( compileArithArgument(a, ci PASS_LD) );

    if ( fdef == FUNCTOR_plus2 )
    { Output_0(ci, A_ADD);
      succeed;
    }
    if ( fdef == FUNCTOR_star2 )
    { Output_0(ci, A_MUL);
      succeed;
    }

    switch(ar)
    { case 0:	Output_1(ci, A_FUNC0, index); break;
      case 1:	Output_1(ci, A_FUNC1, index); break;
      case 2:	Output_1(ci, A_FUNC2, index); break;
      default:  Output_2(ci, A_FUNC,  index, (code) ar); break;
    }

    succeed;
  }
}
#endif /* O_COMPILE_ARITH */


#ifdef O_COMPILE_IS
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Compile unifications (=/2) in the body into inline instructions.

Returns one of TRUE, FALSE or *_OVERFLOW
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
skippedVar() must be called for arguments that are not inspected because
the result of the goal is already   determined  by the predicate and the
other argument. If the argument is a firstvar,   it emits a C_VAR on the
variable to guarantee consistency of  the   frame.  The test-case is the
code below, "Y = _". It decides that X   = _ is always true, but it must
initialise Y! Keri Harris.

t(X) :-
	(   X == a
	->  Y = _
	;   Y = x
	),
	writeln(Y).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
skippedVar(Word arg, compileInfo *ci ARG_LD)
{ int i = isIndexedVarTerm(*arg PASS_LD);

  if ( i >= 0 && isFirstVarSet(ci->used_var, i) )
    Output_1(ci, C_VAR, VAROFFSET(i));

  return TRUE;
}


static int
compileBodyUnify(Word arg, compileInfo *ci ARG_LD)
{ Word a1, a2;
  int i1, i2;

  a1 = argTermP(*arg, 0); deRef(a1);
  a2 = argTermP(*arg, 1); deRef(a2);

  if ( isVar(*a1) )			/* Singleton = ? --> true */
  { skippedVar(a2, ci PASS_LD);
  unify_always_yields_true:
    Output_0(ci, I_TRUE);
    return TRUE;
  }

  if ( isVar(*a2) )
  { skippedVar(a1, ci PASS_LD);
    goto unify_always_yields_true;
  }

  i1 = isIndexedVarTerm(*a1 PASS_LD);
  i2 = isIndexedVarTerm(*a2 PASS_LD);

  if ( i1 >= 0 && i2 >= 0 )		/* unify two variables */
  { int f1, f2;

    if ( i1 == i2 )			/* unify a var with itself? */
    { skippedVar(a1, ci PASS_LD);
      Output_0(ci, I_TRUE);
      return TRUE;
    }

    f1 = isFirstVarSet(ci->used_var, i1);
    f2 = isFirstVarSet(ci->used_var, i2);

    if ( f1 && f2 )
      Output_2(ci, B_UNIFY_FF, VAROFFSET(i1), VAROFFSET(i2));
    else if ( f1 )
      Output_2(ci, B_UNIFY_FV, VAROFFSET(i1), VAROFFSET(i2));
    else if ( f2 )
      Output_2(ci, B_UNIFY_FV, VAROFFSET(i2), VAROFFSET(i1));
    else
      Output_2(ci, B_UNIFY_VV, VAROFFSET(i1), VAROFFSET(i2));

    return TRUE;
  }

  if ( i1 >= 0 )			/* Var = Term */
  { int first;
    int rc;

  unify_term:
    first = isFirstVarSet(ci->used_var, i1);
    if ( isConst(*a2) )
    { Output_2(ci, first ? B_UNIFY_FC : B_UNIFY_VC, VAROFFSET(i1), *a2);
      if ( isAtom(*a2) )
	PL_register_atom(*a2);
    } else
    { int where = (first ? A_BODY : A_HEAD|A_ARG);
      Output_1(ci, first ? B_UNIFY_FIRSTVAR : B_UNIFY_VAR, VAROFFSET(i1));
      if ( (rc=compileArgument(a2, where, ci PASS_LD)) < 0 )
	return rc;
      Output_0(ci, B_UNIFY_EXIT);
    }

    return TRUE;
  }
  if ( i2 >= 0 )			/* (Term = Var): as (Var = Term)! */
  { i1 = i2;
    a2 = a1;
    goto unify_term;
  }

  return FALSE;				/* Term = Term */
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Compile ==/2. Note that if either  side   is  a  firstvar, the test will
always fail. When doing optimized compilation we simply generate fail/0.
otherwise we generate a balancing instruction and the normal equivalence
test.  Likewise, an == on a singleton fails.

Returns TRUE if compiled; FALSE if not compiled. Reserved *_OVERFLOW for
errors.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
compileBodyEQ(Word arg, compileInfo *ci ARG_LD)
{ Word a1, a2;
  int i1, i2;

  a1 = argTermP(*arg, 0);
  deRef(a1);
  if ( isVar(*a1) )			/* Singleton == ?: always fail */
  {
  eq_always_false:
    if ( truePrologFlag(PLFLAG_OPTIMISE) )
    { Output_0(ci, I_FAIL);
      return TRUE;
    }

    return FALSE;			/* debugging: compile as normal code */
  }

  a2 = argTermP(*arg, 1);
  deRef(a2);
  if ( isVar(*a2) )			/* ? = Singleton: no need to compile */
    goto eq_always_false;

  i1 = isIndexedVarTerm(*a1 PASS_LD);
  i2 = isIndexedVarTerm(*a2 PASS_LD);

  if ( i1 >=0 && i2 >= 0 )		/* Var1 == Var2 */
  { int f1 = isFirstVar(ci->used_var, i1);
    int f2 = isFirstVar(ci->used_var, i2);

    if ( f1 || f2 )
    { if ( truePrologFlag(PLFLAG_OPTIMISE) )
      {	Output_0(ci, i1 == i2 ? I_TRUE : I_FAIL);
	return TRUE;
      }
    } else
    { if ( f1 ) Output_1(ci, C_VAR, VAROFFSET(i1));
      if ( f2 ) Output_1(ci, C_VAR, VAROFFSET(i2));
    }

    Output_2(ci, B_EQ_VV, VAROFFSET(i1), VAROFFSET(i2));

    return TRUE;
  }

  if ( i1 >= 0 && isConst(*a2) )	/* Var == const */
  { int f1 = isFirstVar(ci->used_var, i1);

    if ( f1 ) Output_1(ci, C_VAR, VAROFFSET(i1));
    Output_2(ci, B_EQ_VC, VAROFFSET(i1), *a2);
    if ( isAtom(*a2) )
      PL_register_atom(*a2);
    return TRUE;
  }
  if ( i2 >= 0 && isConst(*a1) )	/* const == Var */
  { int f2 = isFirstVar(ci->used_var, i2);

    if ( f2 ) Output_1(ci, C_VAR, VAROFFSET(i2));
    Output_2(ci, B_EQ_VC, VAROFFSET(i2), *a1);
    if ( isAtom(*a1) )
      PL_register_atom(*a1);
    return TRUE;
  }

  return FALSE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Compile \==/2. Note that if either  side   is  a firstvar, the test will
always succeed. When doing  optimized   compilation  we  simply generate
true/0. otherwise we generate a  balancing   instruction  and the normal
equivalence test. Likewise, an \== on a singleton succeeds.

Returns TRUE if compiled; FALSE if not compiled. Reserved *_OVERFLOW for
errors.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
compileBodyNEQ(Word arg, compileInfo *ci ARG_LD)
{ Word a1, a2;
  int i1, i2;

  a1 = argTermP(*arg, 0);
  deRef(a1);
  if ( isVar(*a1) )			/* Singleton == ?: always true */
  {
  eq_always_false:
    if ( truePrologFlag(PLFLAG_OPTIMISE) )
    { Output_0(ci, I_TRUE);
      return TRUE;
    }

    return FALSE;			/* debugging: compile as normal code */
  }

  a2 = argTermP(*arg, 1);
  deRef(a2);
  if ( isVar(*a2) )			/* ? = Singleton: no need to compile */
    goto eq_always_false;

  i1 = isIndexedVarTerm(*a1 PASS_LD);
  i2 = isIndexedVarTerm(*a2 PASS_LD);

  if ( i1 >=0 && i2 >= 0 )		/* Var1 == Var2 */
  { int f1 = isFirstVar(ci->used_var, i1);
    int f2 = isFirstVar(ci->used_var, i2);

    if ( f1 || f2 )
    { if ( truePrologFlag(PLFLAG_OPTIMISE) )
      {	Output_0(ci, i1 == i2 ? I_FAIL : I_TRUE);
	return TRUE;
      }
    } else
    { if ( f1 ) Output_1(ci, C_VAR, VAROFFSET(i1));
      if ( f2 ) Output_1(ci, C_VAR, VAROFFSET(i2));
    }

    Output_2(ci, B_NEQ_VV, VAROFFSET(i1), VAROFFSET(i2));

    return TRUE;
  }

  if ( i1 >= 0 && isConst(*a2) )	/* Var == const */
  { int f1 = isFirstVar(ci->used_var, i1);

    if ( f1 ) Output_1(ci, C_VAR, VAROFFSET(i1));
    Output_2(ci, B_NEQ_VC, VAROFFSET(i1), *a2);
    if ( isAtom(*a2) )
      PL_register_atom(*a2);
    return TRUE;
  }
  if ( i2 >= 0 && isConst(*a1) )	/* const == Var */
  { int f2 = isFirstVar(ci->used_var, i2);

    if ( f2 ) Output_1(ci, C_VAR, VAROFFSET(i2));
    Output_2(ci, B_NEQ_VC, VAROFFSET(i2), *a1);
    if ( isAtom(*a1) )
      PL_register_atom(*a1);
    return TRUE;
  }

  return FALSE;
}

#endif /*O_COMPILE_IS*/


static int
compileBodyVar1(Word arg, compileInfo *ci ARG_LD)
{ Word a1;
  int i1;

  a1 = argTermP(*arg, 0);
  deRef(a1);
  if ( isVar(*a1) )			/* Singleton == ?: always true */
  { if ( truePrologFlag(PLFLAG_OPTIMISE) )
    { Output_0(ci, I_TRUE);
      return TRUE;
    }

    return FALSE;
  }

  i1 = isIndexedVarTerm(*a1 PASS_LD);
  if ( i1 >=0 )
  { int f1 = isFirstVar(ci->used_var, i1);

    if ( f1 )
    { if ( truePrologFlag(PLFLAG_OPTIMISE) )
      { Output_0(ci, I_TRUE);
	return TRUE;
      }
      return FALSE;
    }

    Output_1(ci, I_VAR, VAROFFSET(i1));
    return TRUE;
  }

  if ( truePrologFlag(PLFLAG_OPTIMISE) )
  { Output_0(ci, I_FAIL);
    return TRUE;
  }

  return FALSE;
}


static int
compileBodyNonVar1(Word arg, compileInfo *ci ARG_LD)
{ Word a1;
  int i1;

  a1 = argTermP(*arg, 0);
  deRef(a1);
  if ( isVar(*a1) )			/* Singleton == ?: always false */
  { if ( truePrologFlag(PLFLAG_OPTIMISE) )
    { Output_0(ci, I_FAIL);
      return TRUE;
    }

    return FALSE;
  }

  i1 = isIndexedVarTerm(*a1 PASS_LD);
  if ( i1 >=0 )
  { int f1 = isFirstVar(ci->used_var, i1);

    if ( f1 )
    { if ( truePrologFlag(PLFLAG_OPTIMISE) )
      { Output_0(ci, I_FAIL);
	return TRUE;
      }
      return FALSE;
    }

    Output_1(ci, I_NONVAR, VAROFFSET(i1));
    return TRUE;
  }

  if ( truePrologFlag(PLFLAG_OPTIMISE) )
  { Output_0(ci, I_TRUE);
    return TRUE;
  }

  return FALSE;
}


		 /*******************************
		 *	     ATOM-GC		*
		 *******************************/

#ifdef O_ATOMGC

void
forAtomsInClause(Clause clause, void (func)(atom_t a))
{ Code PC, ep;
  code c;

  PC = clause->codes;
  ep = PC + clause->code_size;

  for( ; PC < ep; PC = stepPC(PC) )
  { c = fetchop(PC);

    switch(c)
    { case H_CONST:
      case B_CONST:
      { word w = PC[1];

	if ( isAtom(w) )
	{ atom_t a = w;

	  (*func)(a);
	}
	break;
      }
      case B_EQ_VC:
      case B_UNIFY_FC:
      case B_UNIFY_VC:			/* var, const */
      { word w = PC[2];

	if ( isAtom(w) )
	  PL_unregister_atom(w);
	break;
      }
    }
  }
}

#endif /*O_ATOMGC*/


		 /*******************************
		 *	     VMI LOGIC		*
		 *******************************/

Code
stepDynPC(Code PC, const code_info *ci)
{ const char *ats = ci->argtype;

  for(; *ats; ats++)
  { switch(*ats)
    { case CA1_STRING:
      case CA1_MPZ:
      { word m = *PC++;
	PC += wsizeofInd(m);
	break;
      }
      case CA1_FLOAT:
	PC += WORDS_PER_DOUBLE;
	break;
      case CA1_INT64:
	PC += WORDS_PER_INT64;
	break;
      default:
	PC++;
    }
  }

  return PC;
}


		/********************************
		*  PROLOG DATA BASE MANAGEMENT  *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Assert is used by assert[az] and record_clause/2 (used by  the  compiler
toplevel).  It asserts a term in the database, either at the start or at
the  end  of  the predicate and if a file is present, updates the source
administration, checks for reconsults, etc.

The warnings should help explain what is going on here.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Clause
assert_term(term_t term, int where, atom_t owner, SourceLoc loc ARG_LD)
{ Clause clause;
  Procedure proc;
  Definition def;
  Module source_module = (loc ? LD->modules.source : (Module) NULL);
  Module module = source_module;
  Module mhead;
  term_t tmp  = PL_new_term_refs(3);
  term_t head = tmp+1;
  term_t body = tmp+2;
  Word h, b;
  functor_t fdef;

  if ( !PL_strip_module_ex(term, &module, tmp) )
    return NULL;
  mhead = module;
  if ( !get_head_and_body_clause(tmp, head, body, &mhead PASS_LD) )
    return NULL;
  if ( !get_head_functor(head, &fdef, 0 PASS_LD) )
    return NULL;			/* not callable, arity too high */
  if ( !(proc = isCurrentProcedure(fdef, mhead)) )
  { if ( checkModifySystemProc(fdef) )
      proc = lookupProcedure(fdef, mhead);
    if ( !proc )
      return NULL;
  }

#ifdef O_PROLOG_HOOK
  if ( mhead->hook && isDefinedProcedure(mhead->hook) )
  { fid_t fid = PL_open_foreign_frame();
    term_t t = PL_new_term_ref();
    int rval;
    functor_t f = (where == CL_START ? FUNCTOR_asserta1 : FUNCTOR_assert1);

    if ( *b == ATOM_true )
      PL_unify_term(t,
		    PL_FUNCTOR, f,
		      PL_TERM, head);
    else
      PL_unify_term(t,
		    PL_FUNCTOR, f,
		      PL_FUNCTOR, FUNCTOR_prove2,
		        PL_TERM, head,
		        PL_TERM, body);

    rval = PL_call_predicate(mhead, PL_Q_NORMAL, mhead->hook, t);

    PL_discard_foreign_frame(fid);
    if ( rval )
      return (Clause)-1;
  }
#endif /*O_PROLOG_HOOK*/

  DEBUG(2,
	Sdprintf("compiling ");
	PL_write_term(Serror, term, 1200, PL_WRT_QUOTED);
	Sdprintf(" ... "););

  h = valTermRef(head);
  b = valTermRef(body);
  deRef(h);
  deRef(b);
  if ( compileClause(&clause, h, b, proc, module PASS_LD) != TRUE )
    return NULL;
  DEBUG(2, Sdprintf("ok\n"));
  def = getProcDefinition(proc);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If loc is defined, we  are   called  from  '$record_clause'/2. This code
takes care of reconsult, redefinition, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( loc )
  { SourceFile sf, of;

    if ( !loc->file )
    { loc->file = owner;
      Sdprintf("No source location!?\n");
    }

    sf = lookupSourceFile(loc->file, TRUE);
    clause->line_no   = loc->line;
    clause->source_no = sf->index;
    if ( owner == loc->file )
    { of = sf;
    } else
    { of = lookupSourceFile(owner, TRUE);
    }
    clause->owner_no  = of->index;

    if ( def->module != mhead )
    { if ( !overruleImportedProcedure(proc, mhead) )
      { freeClause(clause);
	return NULL;
      }
      def = getProcDefinition(proc);	/* may be changed */
    }

    if ( proc == of->current_procedure )
      return assertProcedure(proc, clause, where PASS_LD) ? clause : NULL;

    if ( def->impl.any )	/* i.e. is (might be) defined */
    { if ( !redefineProcedure(proc, of, 0) )
      { freeClause(clause);
	return NULL;
      }
    }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This `if' locks predicates as system predicates  if  we  are  in  system
mode, the predicate is still undefined and is not dynamic or multifile.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    if ( !isDefinedProcedure(proc) )
    { if ( SYSTEM_MODE )
      { if ( false(def, SYSTEM) )
	  set(def, SYSTEM|HIDE_CHILDS|LOCKED);
      } else
      { if ( truePrologFlag(PLFLAG_DEBUGINFO) )
	  clear(def, HIDE_CHILDS);
	else
	  set(def, HIDE_CHILDS);
      }
    }

    addProcedureSourceFile(of, proc);
    of->current_procedure = proc;
    return assertProcedure(proc, clause, where PASS_LD) ? clause : NULL;
  }

  /* assert[az]/1 */

  if ( false(def, DYNAMIC) )
  { if ( !setDynamicProcedure(proc, TRUE) )
    { freeClause(clause);
      return NULL;
    }
  }

  if ( assertProcedure(proc, clause, where PASS_LD) )
    return clause;

  freeClause(clause);
  return NULL;
}


static
PRED_IMPL("assertz", 1, assertz1, PL_FA_TRANSPARENT)
{ PRED_LD

  return assert_term(A1, CL_END, NULL_ATOM, NULL PASS_LD) != NULL;
}


static
PRED_IMPL("asserta", 1, asserta1, PL_FA_TRANSPARENT)
{ PRED_LD

  return assert_term(A1, CL_START, NULL_ATOM, NULL PASS_LD) != NULL;
}


static int
mustBeVar(term_t t ARG_LD)
{ if ( !PL_is_variable(t) )
    return PL_error(NULL, 0, NULL, ERR_UNINSTANTIATION, 2, t);

  succeed;
}


static
PRED_IMPL("assertz", 2, assertz2, PL_FA_TRANSPARENT)
{ PRED_LD
  Clause clause;

  if ( !mustBeVar(A2 PASS_LD) )
    fail;
  if ( !(clause = assert_term(A1, CL_END, NULL_ATOM, NULL PASS_LD)) )
    fail;

  return PL_unify_clref(A2, clause);
}


static
PRED_IMPL("asserta", 2, asserta2, PL_FA_TRANSPARENT)
{ PRED_LD
  Clause clause;

  if ( !mustBeVar(A2 PASS_LD) )
    fail;
  if ( !(clause = assert_term(A1, CL_START, NULL_ATOM, NULL PASS_LD)) )
    fail;

  return PL_unify_clref(A2, clause);
}


/** '$record_clause'(+Term, +Owner, +Source)
    '$record_clause'(+Term, +Owner, +Source, -Ref)

Compile a clause from loading a file. Term is the clause to be compiled.
Source defines the origin of the clause.

*/

static int
record_clause(term_t term, term_t owner, term_t source, term_t ref ARG_LD)
{ Clause clause;
  sourceloc loc;
  atom_t a_owner;
  atom_t a;

  if ( !PL_get_atom_ex(owner, &a_owner) )
    return FALSE;

  if ( PL_get_atom(source, &a) && a == ATOM_minus )
  { loc.file = source_file_name;
    loc.line = source_line_no;
    assert(source_line_no != -1);
  } else if ( PL_is_functor(source, FUNCTOR_colon2) )
  { term_t arg = PL_new_term_ref();	/* file:line */

    _PL_get_arg(1, source, arg);
    if ( !PL_get_atom_ex(arg, &loc.file) )
      return FALSE;
    _PL_get_arg(2, source, arg);
    if ( !PL_get_integer_ex(arg, &loc.line) )
      return FALSE;
  } else
  { return PL_type_error("source-location", source);
  }

  if ( (clause = assert_term(term, CL_END, a_owner, &loc PASS_LD)) )
  { if ( ref )
      return PL_unify_clref(ref, clause);
    else
      return TRUE;
  }

  return FALSE;
}


static
PRED_IMPL("$record_clause", 3, record_clause, 0)
{ PRED_LD

  return record_clause(A1, A2, A3, 0 PASS_LD);
}


static
PRED_IMPL("$record_clause", 4, record_clause, 0)
{ PRED_LD

  return record_clause(A1, A2, A3, A4 PASS_LD);
}


/** '$start_aux'(+File, -CurrentPred) is det.
    '$end_aux'(+File, +CurrentPred) is det.
*/

static
PRED_IMPL("$start_aux", 2, start_aux, 0)
{ PRED_LD
  atom_t filename;
  SourceFile sf;

  if ( !PL_get_atom_ex(A1, &filename) )
    fail;

  sf = lookupSourceFile(filename, TRUE);
  if ( sf->current_procedure )
  { if ( unify_definition(NULL, A2, sf->current_procedure->definition, 0,
			  GP_QUALIFY|GP_NAMEARITY) )
    { sf->current_procedure = NULL;
      succeed;
    }
    fail;
  }

  return PL_unify_nil(A2);
}


static
PRED_IMPL("$end_aux", 2, end_aux, 0)
{ PRED_LD
  atom_t filename;
  SourceFile sf;
  Procedure proc;

  if ( !PL_get_atom_ex(A1, &filename) )
    fail;

  sf = lookupSourceFile(filename, TRUE);
  if ( PL_get_nil(A2) )
  { sf->current_procedure = NULL;
  } else
  { if ( get_procedure(A2, &proc, 0, GP_NAMEARITY|GP_EXISTENCE_ERROR) )
    { sf->current_procedure = proc;
      succeed;
    }

    fail;
  }

  succeed;
}


static
PRED_IMPL("redefine_system_predicate",  1, redefine_system_predicate,
	  PL_FA_TRANSPARENT)
{ PRED_LD
  Procedure proc;
  Module m = NULL;
  functor_t fd;
  term_t head = PL_new_term_ref();
  term_t pred = A1;

  PL_strip_module(pred, &m, head);
  if ( !PL_get_functor(head, &fd) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, pred);

  proc = lookupProcedure(fd, m);
  abolishProcedure(proc, m);
  set(proc->definition, P_REDEFINED);	/* flag as redefined */

  succeed;
}


/** '$predefine_foreign'(+PI) is det.

Registers a PI as a foreign predicate   without providing a function for
it. This is used by qsave_program/2 to make sure that foreign predicates
are not left  undefined  and  possibly   auto-imported  from  the  wrong
location.
*/

static
PRED_IMPL("$predefine_foreign",  1, predefine_foreign, PL_FA_TRANSPARENT)
{ Procedure proc;

  if ( !get_procedure(A1, &proc, 0, GP_NAMEARITY|GP_DEFINE) )
    return FALSE;
  set(proc->definition, FOREIGN);

  return TRUE;
}


static
PRED_IMPL("compile_predicates",  1, compile_predicates, PL_FA_TRANSPARENT)
{ PRED_LD
  term_t tail = PL_copy_term_ref(A1);
  term_t head = PL_new_term_ref();

  while( PL_get_list(tail, head, tail) )
  { Procedure proc;

    if ( !get_procedure(head, &proc, 0,
			GP_NAMEARITY|GP_FINDHERE|GP_EXISTENCE_ERROR) )
      fail;

    if ( !setDynamicProcedure(proc, FALSE) )
      fail;
  }

  return PL_get_nil_ex(tail);
}




		/********************************
		*          DECOMPILER           *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
skipArgs() skips skip arguments inside the   code  for a clause-head. If
the skip is into the middle of a   H_VOID_N,  it returns the location of
the H_VOID_N.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Code
skipArgs(Code PC, int skip)
{ int nested = 0;
  Code nextPC;

  for(;; PC=nextPC)
  { code c = decode(*PC);
    nextPC = stepPC(PC);

#if O_DEBUGGER
  again:
#endif
    switch(c)
    { case H_FUNCTOR:
      case H_LIST:
	nested++;
        continue;
      case H_RFUNCTOR:
      case H_RLIST:
	continue;
      case H_POP:
	if ( --nested == 0 && --skip == 0 )
	  return nextPC;
        assert(nested>=0);
        continue;
      case H_CONST:
      case H_NIL:
      case H_INT64:
      case H_INTEGER:
      case H_FLOAT:
      case H_STRING:
      case H_MPZ:
      case H_FIRSTVAR:
      case H_VAR:
      case H_VOID:
      case H_LIST_FF:
	if ( nested )
	  continue;
        if ( --skip == 0 )
	  return nextPC;
	continue;
      case H_VOID_N:
	if ( nested )
	  continue;
	skip -= (int)PC[1];
	if ( skip <= 0 )
	  return PC;
	continue;
      case I_EXITFACT:
      case I_EXIT:
      case I_ENTER:			/* fix H_VOID, H_VOID, I_ENTER */
	return PC;
      case I_NOP:
	continue;
#ifdef O_DEBUGGER
      case D_BREAK:
        c = decode(replacedBreak(PC-1));
	goto again;
#endif
      default:
	assert(0);
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
argKey() determines the indexing key for the  argument at the given code
position after skipping skip argument terms   by  inspecting the virtual
machine code. If constonly is  non-zero,  it   creates  a  key for large
integers and floats. Otherwise it only succeeds on atoms, small integers
and functors.

NOTE: this function must  be  kept   consistent  with  indexOfWord()  in
pl-index.c!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
argKey(Code PC, int skip, int constonly, word *key)
{ if ( skip > 0 )
    PC = skipArgs(PC, skip);

  for(;;)
  { code c = decode(*PC++);

#if O_DEBUGGER
  again:
#endif
    switch(c)
    { case H_FUNCTOR:
      case H_RFUNCTOR:
	*key = (functor_t)*PC;
        succeed;
      case H_CONST:
	*key = *PC;
	succeed;
      case H_NIL:
	*key = ATOM_nil;
        succeed;
      case H_LIST_FF:
      case H_LIST:
      case H_RLIST:
	*key = FUNCTOR_dot2;
        succeed;
      case H_INT64:			/* only on 32-bit hardware! */
	if ( !constonly )
	{ word k = (word)PC[0] ^ (word)PC[1];
	  if ( !k )
	    k++;
	  *key = k;
          succeed;
	} else
	{ *key = 0;
	  fail;
	}
      case H_INTEGER:
	if ( !constonly )
	{ word k;
#if SIZEOF_VOIDP == 4
	  k = (word)*PC;			/* indexOfWord() picks 64-bits */
	  if ( (intptr_t)k < 0L )
	    k ^= -1L;
	  DEBUG(9, Sdprintf("key for %ld = 0x%x\n", *PC, k));
#else
	  k = (word)*PC;
#endif
	  if ( !k )
	    k++;
	  *key = k;
	  succeed;
	} else
	{ *key = 0;
	  fail;
	}
      case H_FLOAT:			/* tbd */
      if ( !constonly )
      { word k;
	switch(WORDS_PER_DOUBLE)
	{ case 2:
	    k = (word)PC[0] ^ (word)PC[1];
	    break;
	  case 1:
	    k = (word)PC[0];
	    break;
	  default:
	    assert(0);
	}

	if ( !k )
	  k++;
	*key = k;
        succeed;
      }
      case H_STRING:
      case H_MPZ:
      case H_FIRSTVAR:
      case H_VAR:
      case H_VOID:
      case H_VOID_N:
      case I_EXITCATCH:
      case I_EXITFACT:
      case I_EXIT:			/* fact */
      case I_ENTER:			/* fix H_VOID, H_VOID, I_ENTER */
	*key = 0;
	fail;
      case I_NOP:
	continue;
#ifdef O_DEBUGGER
      case D_BREAK:
        c = decode(replacedBreak(PC-1));
	goto again;
#endif
      default:
	assert(0);
        fail;
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The decompiler is rather straightforwards.  First it  will  construct  a
term  with  variables  for  the  head  and an array of variables for all
variables in  the  clause.   Next  the  head  arguments  are  filled  by
decompiling  the head code.  Finally the body is decompiled.  The latter
is slightly more complex as it is given in reverse polish notation.   We
first  will  skip  the  argument  filling  code,  looking for the actual
calling code.  This provides us the functor and arity of the  subclause.
Then we create a term, back up and fill the arguments.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#undef PC
#define PC	(di->pc)
#define ARGP	(di->argp)
#define XR(c)	((word)(c))

typedef struct
{ Code	 pc;				/* pc for decompilation */
  Word   argp;				/* argument pointer */
  int	 nvars;				/* size of var block */
  term_t variables;			/* variable table (PL_new_term_refs() array) */
  term_t bindings;			/* [Offset = Var, ...] */
} decompileInfo;

static int decompile_head(Clause, term_t, decompileInfo * ARG_LD);
static int decompileBody(decompileInfo *, code, Code ARG_LD);
static int build_term(functor_t, decompileInfo * ARG_LD);
static int put_functor(Word p, functor_t f ARG_LD);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
decompileHead()  is  public  as  it  is   needed  to  update  the  index
information for clauses if this changes   when  the predicate is already
defined.  Also for intermediate  code  file   loaded  clauses  the index
information is recalculated as the constants   may  be different accross
runs.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline word
valHandle(term_t r ARG_LD)
{ Word p = valTermRef(r);

  deRef(p);
  return *p;
}

bool
decompileHead(Clause clause, term_t head)
{ GET_LD
  decompileInfo di;
  int rc;

  di.nvars    = VAROFFSET(1) + clause->prolog_vars;
  di.bindings = 0;
  if ( clause->prolog_vars )
  { if ( !(di.variables = PL_new_term_refs(clause->prolog_vars)) )
      return FALSE;
  } else
    di.variables = 0;

  rc = decompile_head(clause, head, &di PASS_LD);
  if ( di.variables )
    PL_reset_term_refs(di.variables);
  return rc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The `argument pointer' is a pair of  term-references. The lower one is a
reference to the current argument. The higher  one is a reference to the
last argument. We need that to avoid that the argument pointer points to
the first cell after the term, producing an illegal term.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
get_arg_ref(term_t term, term_t argp ARG_LD)
{ word w = valHandle(term PASS_LD);
  Word p = argTermP(w, 0);
  int ar = arityTerm(w);
  Word ap = valTermRef(argp);

  assert(ar > 0);

  ap[0] = makeRefG(p);			/* points to first argument */
  ap[1] = makeRefG(p+ar-1);		/* points to last argument */
}


static void
next_arg_ref(term_t argp ARG_LD)
{ Word ap = valTermRef(argp);

  if ( ap[0] != ap[1] )
  { Word p = unRef(ap[0]);

    ap[0] = makeRefG(p+1);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note:  unifyVar()  is  used  to    (re-)create  sharing  variables  when
decompiling.  Because  compiled  clauses    cannot   contain  attributed
variables, these to not exist in this context.

The vars argument is an array of  term_t types (di->variables). TBD: Use
PL_new_term_refs() here! The var itself is always (?) a pointer into the
global stack. This means that the   term-reference  is trailed. This may
seem wasteful, but is  needed  for   the  stack-resizing  loop  used for
body-compilation. Even undoing the  head-unification   is  not an option
because this is a true unification that may share with older variables.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
unifyVar(Word var, term_t vars, size_t i ARG_LD)
{ Word v;

  i -= VAROFFSET(0);
  DEBUG(3, Sdprintf("unifyVar(%p, %ld(=%ld))\n", var, i, vars+i));

  if ( tTop+1 >= tMax )
    return TRAIL_OVERFLOW;

  v = valTermRef(vars+i);
  deRef(v);
  deRef(var);
  if ( isVar(*v) && isVar(*var) )
  { if ( v < var )
    { Trail(var, makeRef(v));
    } else
    { Trail(v, makeRef(var));
    }
  } else if ( isVar(*var) )		/* retract called with bounded var */
  { Trail(var, *v);
  } else
  { assert(0);
  }

  return TRUE;
}


static int
unifyVarGC(Word var, term_t vars, size_t i ARG_LD)
{ i -= VAROFFSET(0);

  DEBUG(3, Sdprintf("unifyVarGC(%p, %ld(=%ld))\n", var, i, vars+i));

  return unify_ptrs(var, valTermRef(vars+i), ALLOW_GC|ALLOW_SHIFT PASS_LD);
}


static bool
decompile_head(Clause clause, term_t head, decompileInfo *di ARG_LD)
{ int arity;
  term_t argp = 0;
  int argn = 0;
  int pushed = 0;
  Definition def = getProcDefinition(clause->procedure);

  if ( di->bindings )
  { term_t tail = PL_copy_term_ref(di->bindings);
    term_t head = PL_new_term_ref();
    unsigned int n;

    for(n=0; n<clause->prolog_vars; n++)
    { if ( !PL_unify_list(tail, head, tail) ||
	   !PL_unify_term(head, PL_FUNCTOR, FUNCTOR_equals2,
				    PL_INT, n,
			            PL_TERM, di->variables+n) )
	fail;
    }
    TRY(PL_unify_nil(tail));
  }

  PC = clause->codes;

  if ( true(clause, GOAL_CLAUSE) )
    return PL_unify_atom(head, ATOM_dcall);

  DEBUG(5, Sdprintf("Decompiling head of %s\n", predicateName(def)));
  arity = def->functor->arity;
  TRY( PL_unify_functor(head, def->functor->functor) );
  if ( arity > 0 )
  { if ( !(argp = PL_new_term_refs(2)) )
      return FALSE;
    get_arg_ref(head, argp PASS_LD);
  }

#define NEXTARG { next_arg_ref(argp PASS_LD); if ( !pushed ) argn++; }

  for(;;)
  { code c = decode(*PC++);

#if O_DEBUGGER
  again:
#endif
    switch(c)
    { case I_NOP:
	continue;
#if O_DEBUGGER
      case D_BREAK:
	c = decode(replacedBreak(PC-1));
        goto again;
#endif
      case H_NIL:
	TRY(PL_unify_nil(argp));
        NEXTARG;
        continue;
      case H_STRING:
      case H_MPZ:
        { word copy = globalIndirectFromCode(&PC);
	  if ( !copy || !_PL_unify_atomic(argp, copy) )
	    return FALSE;
	  NEXTARG;
	  continue;
	}
      case H_INTEGER:
        { intptr_t *p = (intptr_t*)PC;
	  intptr_t v = *p++;
	  PC = (Code)p;
	  TRY(PL_unify_int64(argp, v));
	  NEXTARG;
	  continue;
	}
      case H_INT64:
        { Word p = allocGlobal(2+WORDS_PER_INT64);
	  word w;

	  if ( p )
	  { w = consPtr(p, TAG_INTEGER|STG_GLOBAL);
	    *p++ = mkIndHdr(WORDS_PER_INT64, TAG_INTEGER);
	    cpInt64Data(p, PC);
	    *p   = mkIndHdr(WORDS_PER_INT64, TAG_INTEGER);
	    TRY(_PL_unify_atomic(argp, w));
	    NEXTARG;
	    continue;
	  } else
	    return FALSE;
	}
      case H_FLOAT:
        { Word p = allocGlobal(2+WORDS_PER_DOUBLE);
	  word w;

	  if ( p )
	  { w = consPtr(p, TAG_FLOAT|STG_GLOBAL);
	    *p++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
	    cpDoubleData(p, PC);
	    *p   = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
	    TRY(_PL_unify_atomic(argp, w));
	    NEXTARG;
	    continue;
	  } else
	    return FALSE;
	}
      case H_CONST:
	  TRY(_PL_unify_atomic(argp, XR(*PC++)));
          NEXTARG;
	  continue;
      case H_FIRSTVAR:
      case H_VAR:
	  TRY(unifyVarGC(valTermRef(argp), di->variables,
			 *PC++ PASS_LD) );
          NEXTARG;
	  continue;
      case H_VOID:
	{ if ( !pushed )		/* FIRSTVAR in the head */
	    TRY(unifyVarGC(valTermRef(argp), di->variables,
			   VAROFFSET(argn) PASS_LD) );
	  NEXTARG;
	  continue;
	}
      case H_VOID_N:
        { int n = (int)*PC++;

	  while(n-- > 0)
	  { if ( !pushed )		/* FIRSTVAR in the head */
	      TRY(unifyVarGC(valTermRef(argp), di->variables,
			     VAROFFSET(argn) PASS_LD) );
	    NEXTARG;
	  }

	  continue;
	}
      case H_FUNCTOR:
	{ functor_t fdef;
	  term_t t2;

	  fdef = (functor_t) XR(*PC++);
      common_functor:
	  if ( !(t2 = PL_new_term_refs(2)) ||
	       !PL_unify_functor(argp, fdef) )
	    return FALSE;
          get_arg_ref(argp, t2 PASS_LD);
          next_arg_ref(argp PASS_LD);
	  assert(t2 == argp+2);
	  argp = t2;
	  pushed++;
	  continue;
      case H_LIST:
	  fdef = FUNCTOR_dot2;
          goto common_functor;
	}
      case H_RFUNCTOR:
	{ functor_t fdef;

	  fdef = (functor_t) XR(*PC++);
      common_rfunctor:
	  TRY(PL_unify_functor(argp, fdef));
          get_arg_ref(argp, argp PASS_LD);
	  continue;
      case H_RLIST:
	  fdef = FUNCTOR_dot2;
          goto common_rfunctor;
	}
      case H_POP:
      case B_POP:
	  PL_reset_term_refs(argp);
          argp -= 2;
	  pushed--;
	  if ( !pushed )
	    argn++;
	  continue;
      case H_LIST_FF:
      { Word p;

	TRY(PL_unify_functor(argp, FUNCTOR_dot2));
	p = valTermRef(argp);
	deRef(p);
	p = argTermP(*p, 0);
        TRY(unifyVarGC(p+0, di->variables, *PC++ PASS_LD) );
        TRY(unifyVarGC(p+1, di->variables, *PC++ PASS_LD) );
	NEXTARG;
	continue;
      }
      case I_EXITCATCH:
      case I_EXITFACT:
      case I_EXIT:			/* fact */
      case I_ENTER:			/* fix H_VOID, H_VOID, I_ENTER */
	{ assert(argn <= arity);

	  if ( argp )
	  { for(; argn < arity; argn++)
	    { TRY(unifyVarGC(valTermRef(argp), di->variables,
			     VAROFFSET(argn) PASS_LD));
	      next_arg_ref(argp PASS_LD);
	    }
	    PL_reset_term_refs(argp);
	  }

	  succeed;
	}
      default:
	  sysError("Illegal instruction in clause head: %d = %d",
		   PC[-1], decode(PC[-1]));
	  fail;
    }
#undef NEXTARG
  }
}

#define makeVarRef(i)	((i)<<LMASK_BITS|TAG_REFERENCE)
#define isVarRef(w)	((tag(w) == TAG_REFERENCE && \
			  storage(w) == STG_INLINE) ? valInt(w) : -1)

bool
decompile(Clause clause, term_t term, term_t bindings)
{ GET_LD
  decompileInfo dinfo;
  decompileInfo *di = &dinfo;
  term_t body, vbody;

  di->nvars    = VAROFFSET(1) + clause->prolog_vars;
  di->bindings = bindings;
  if ( clause->prolog_vars )
  { if ( !(di->variables = PL_new_term_refs(clause->prolog_vars)) )
      return FALSE;
  } else
    di->variables = 0;

#ifdef O_RUNTIME
  if ( false(getProcDefinition(clause->procedure), DYNAMIC|P_THREAD_LOCAL) )
    fail;
#endif

  if ( true(clause, UNIT_CLAUSE) )	/* fact */
  { if ( decompile_head(clause, term, di PASS_LD) )
    { if ( di->variables )
	PL_reset_term_refs(di->variables);
      succeed;
    }
					/* deal with a :- A */
    if ( PL_is_functor(term, FUNCTOR_prove2) )
    { term_t b = PL_new_term_ref();
      _PL_get_arg(2, term, b);

      if ( PL_unify_atom(b, ATOM_true) )
      { _PL_get_arg(1, term, b);
	return decompile_head(clause, b, di PASS_LD);
      }
    }

    fail;
  } else
  { term_t a = PL_new_term_ref();

    TRY(PL_unify_functor(term, FUNCTOR_prove2));
    _PL_get_arg(1, term, a);
    TRY(decompile_head(clause, a, di PASS_LD));
    _PL_get_arg(2, term, a);
    body = a;
  }

  if ( fetchop(PC) == I_CONTEXT )
  { Module context = (Module)PC[1];
    term_t a = PL_new_term_ref();

    PC += 2;
    TRY(PL_unify_functor(body, FUNCTOR_colon2));
    _PL_get_arg(1, body, a);
    TRY(PL_unify_atom(a, context->name));
    _PL_get_arg(2, body, body);
  }


  for(;;)
  { fid_t fid;
    Code PCsave = di->pc;
    int rc;

    if ( !(fid = PL_open_foreign_frame()) )
      return FALSE;
    vbody = PL_new_term_ref();
    ARGP = valTermRef(vbody);
    rc = decompileBody(di, I_EXIT, (Code) NULL PASS_LD);
    if ( rc == TRUE )
    { rc = PL_unify(body, vbody);
      PL_close_foreign_frame(fid);
      return rc;
    } else if ( rc == FALSE )
    { PL_close_foreign_frame(fid);
      return FALSE;
    } else
    { PL_discard_foreign_frame(fid);
      di->pc = PCsave;
      aTop = LD->query->aSave;		/* reset to base */
      if ( !makeMoreStackSpace(rc, ALLOW_GC|ALLOW_SHIFT) )
	return FALSE;
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Body decompilation.  A previous version of this part of the code  worked
top-down,  refining the term given using unification.  This approach has
three advantages:

  - Decompilation will fail as soon as  unification  of  generated  code
    fails.
  - If the body is instantiated no copy will be created  on  the  global
    stack, thus saving memory.
  - Handling variables is somewhat simpler as no intermediate storage is
    needed.

Unfortunately it also has some serious disadvantages:

  - The call/depart code is written in reverse polish notation.   If  we
    work  top-down  we  will need the functor of the subclause before we
    can start working on the arguments.  This implies we  have  to  skip
    the  argument instructions first to find the call/depart instruction
    and then back-up to fill the arguments, introducing one  more  place
    where we need to know the WAM code semantics.
  - With the  introduction  of  nested  reverse  polish  constructs  for
    arithmic  it  gets  very  difficult  to do the decompilation without
    using a stack for  intermediate  data  storage,  building  the  term
    bottom-up.

In the current implementation the head is decompiled in the  unification
style  and the body is decompiled using a stack machine.  This takes the
best of both approaches: the head is not in reverse polish notation  and
is  not  unlikely  to be instantiated (retract/1), while it is very rare
that clause/retract are used with instantiated body.

The decompilation stack is located on top of the local  stack,  as  this
area is not in use during decompilation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define BUILD_TERM(f) \
	{ int rc; \
	  if ( (rc=build_term((f), di PASS_LD)) != TRUE ) \
	    return rc; \
	}
#define TRY_DECOMPILE(di, end, until) \
	{ int rc; \
	  rc = decompileBody(di, end, until PASS_LD); \
	  if ( rc != TRUE ) \
	    return rc; \
	}

static int
decompileBody(decompileInfo *di, code end, Code until ARG_LD)
{ int nested = 0;		/* nesting in FUNCTOR ... POP */
  int pushed = 0;		/* Subclauses pushed on the stack */
  code op;

  while( PC != until )
  { op = decode(*PC++);

#if O_DEBUGGER
  again:
#endif
    if ( op == end )
    { exit:
      PC--;
      break;
    }

    if ( !nested && ARGP+2 > (Word)lMax ) /* ARGP builds temp refs on local stack */
      return LOCAL_OVERFLOW;
    DEBUG(3, Sdprintf("\t%s\n", codeTable[op].name));

    switch( op )
    {
#if O_DEBUGGER
        case D_BREAK:	    op = decode(replacedBreak(PC-1));
			    goto again;
#endif
        case A_ENTER:
        case I_NOP:	    continue;
	case H_CONST:
	case B_CONST:
			    *ARGP++ = XR(*PC++);
			    continue;
	case H_NIL:
	case B_NIL:
			    *ARGP++ = ATOM_nil;
			    continue;
	case H_INTEGER:
	case B_INTEGER:
	case A_INTEGER:
			  { intptr_t i = (intptr_t)*PC++;
			    int rc;

			    if ( (rc=put_int64(ARGP++, i, 0 PASS_LD)) != TRUE )
			      return rc;
			    continue;
			  }
	case H_INT64:
	case B_INT64:
	case A_INT64:
			  { Word p;

			    if ( !hasGlobalSpace(2+WORDS_PER_INT64) )
			      return GLOBAL_OVERFLOW;

			    p = gTop;
			    gTop += 2+WORDS_PER_INT64;

			    *ARGP++ = consPtr(p, TAG_INTEGER|STG_GLOBAL);
			    *p++ = mkIndHdr(WORDS_PER_INT64, TAG_INTEGER);
			    cpInt64Data(p, PC);
			    *p   = mkIndHdr(WORDS_PER_INT64, TAG_INTEGER);
			    continue;
			  }
	case H_FLOAT:
	case B_FLOAT:
	case A_DOUBLE:
			  { Word p;

			    if ( !hasGlobalSpace(2+WORDS_PER_DOUBLE) )
			      return GLOBAL_OVERFLOW;

			    p = gTop;
			    gTop += 2+WORDS_PER_DOUBLE;

			    *ARGP++ = consPtr(p, TAG_FLOAT|STG_GLOBAL);
			    *p++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
			    cpDoubleData(p, PC);
			    *p   = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
			    continue;
			  }
	case H_STRING:
	case H_MPZ:
	case B_STRING:
	case A_MPZ:
	case B_MPZ:
			  { size_t sz = gsizeIndirectFromCode(PC);

			    if ( !hasGlobalSpace(sz) )
			      return GLOBAL_OVERFLOW;

			    *ARGP++ = globalIndirectFromCode(&PC);
			    continue;
			  }
      { size_t index;

	case B_ARGVAR:
	case B_ARGFIRSTVAR:
	case H_FIRSTVAR:
	case B_FIRSTVAR:
	case B_UNIFY_VAR:
	case B_UNIFY_FIRSTVAR:
	case H_VAR:
	case A_VAR:
	case B_VAR:	    index = *PC++;		goto var_common;
	case A_VAR0:
	case B_VAR0:	    index = VAROFFSET(0);	goto var_common;
	case A_VAR1:
	case B_VAR1:	    index = VAROFFSET(1);	goto var_common;
	case A_VAR2:
	case B_VAR2:	    index = VAROFFSET(2);	var_common:
			    if ( nested )
			    { int rc = unifyVar(ARGP++, di->variables,
						index PASS_LD);
			      if ( rc != TRUE )
				return rc;
			    } else
			    { *ARGP++ = makeVarRef(index);
			    }
			    continue;
      }
      case B_UNIFY_FF:
      case B_UNIFY_FV:
      case B_UNIFY_VV:
			    *ARGP++ = makeVarRef((int)*PC++);
			    *ARGP++ = makeVarRef((int)*PC++);
			    goto b_unify_exit;
      case B_UNIFY_FC:
      case B_UNIFY_VC:
			    *ARGP++ = makeVarRef((int)*PC++);
			    *ARGP++ = (word)*PC++;
			    goto b_unify_exit;
      case B_EQ_VC:
			    *ARGP++ = makeVarRef((int)*PC++);
			    *ARGP++ = (word)*PC++;
			    goto b_eq_vv_cont;
      case B_NEQ_VC:
			    *ARGP++ = makeVarRef((int)*PC++);
			    *ARGP++ = (word)*PC++;
			    goto b_neq_vv_cont;
      case B_EQ_VV:
			    *ARGP++ = makeVarRef((int)*PC++);
			    *ARGP++ = makeVarRef((int)*PC++);
			  b_eq_vv_cont:
			    BUILD_TERM(FUNCTOR_strict_equal2);
			    pushed++;
			    continue;
      case B_NEQ_VV:
			    *ARGP++ = makeVarRef((int)*PC++);
			    *ARGP++ = makeVarRef((int)*PC++);
			  b_neq_vv_cont:
			    BUILD_TERM(FUNCTOR_not_strict_equal2);
			    pushed++;
			    continue;
      case H_VOID:
      case B_VOID:
			    setVar(*ARGP++);
			    continue;
      case H_VOID_N:
      { size_t count = *PC++;

	while(count-->0)
	  setVar(*ARGP++);

	continue;
      }
      case H_FUNCTOR:
      case B_FUNCTOR:
      { functor_t fdef;

	fdef = (functor_t)XR(*PC++);
      common_bfunctor:
        { int rc;
	  word w;

	  if ( (rc=put_functor(&w, fdef PASS_LD)) != TRUE )
	    return rc;
	  *ARGP++ = w;
	  pushArgumentStack(ARGP);
	  ARGP = argTermP(w, 0);
	}
        nested++;
	continue;
      case H_LIST:
      case B_LIST:
	fdef = FUNCTOR_dot2;
        goto common_bfunctor;
      }
      case H_LIST_FF:
      { size_t v1 = *PC++;
	size_t v2 = *PC++;
	int rc;
	word w;
	Word p;

	if ( (rc=put_functor(&w, FUNCTOR_dot2 PASS_LD)) != TRUE )
	  return rc;
	*ARGP++ = w;
	p = argTermP(w, 0);
	if ( (rc=unifyVar(p+0, di->variables, v1 PASS_LD)) != TRUE ||
	     (rc=unifyVar(p+1, di->variables, v2 PASS_LD)) != TRUE )
	  return rc;
	continue;
      }
      case H_RFUNCTOR:
      case B_RFUNCTOR:
      { functor_t fdef;

	fdef = (functor_t)XR(*PC++);
      common_brfunctor:
	{ word w;
	  int rc;

	  if ( (rc=put_functor(&w, fdef PASS_LD)) != TRUE )
	    return rc;

	  *ARGP++ = w;
	  ARGP= argTermP(w, 0);
	}
	continue;
      case H_RLIST:
      case B_RLIST:
	fdef = FUNCTOR_dot2;
        goto common_brfunctor;
      }
      case H_POP:
      case B_POP:
			    ARGP = *--aTop;
			    nested--;
			    continue;
#ifdef O_COMPILE_IS
      case B_UNIFY_EXIT:
			  b_unify_exit:
			    BUILD_TERM(FUNCTOR_equals2);
			    pushed++;
			    continue;
#endif
#if O_COMPILE_ARITH
      case A_ADD:
			    BUILD_TERM(FUNCTOR_plus2);
			    continue;
      case A_MUL:
			    BUILD_TERM(FUNCTOR_star2);
			    continue;
      case A_FUNC0:
      case A_FUNC1:
      case A_FUNC2:
			    BUILD_TERM(functorArithFunction((int)*PC++));
			    continue;
      case A_FUNC:
			    BUILD_TERM(functorArithFunction((int)*PC++));
			    PC++;
			    continue;
      case A_ADD_FC:
      { int rvar = (int)*PC++;
	int ivar = (int)*PC++;
	intptr_t add = (intptr_t)*PC++;

	*ARGP++ = makeVarRef(rvar);
	*ARGP++ = makeVarRef(ivar);
	*ARGP++ = consInt(add);
	BUILD_TERM(FUNCTOR_plus2); /* create B+<n> */
	BUILD_TERM(FUNCTOR_is2);
	pushed++;
	continue;
      }
#endif /* O_COMPILE_ARITH */
      { functor_t f;
#if O_COMPILE_ARITH
	case A_LT:	    f = FUNCTOR_smaller2;	goto f_common;
	case A_LE:	    f = FUNCTOR_smaller_equal2;	goto f_common;
	case A_GT:	    f = FUNCTOR_larger2;	goto f_common;
	case A_GE:	    f = FUNCTOR_larger_equal2;	goto f_common;
	case A_EQ:	    f = FUNCTOR_ar_equals2;	goto f_common;
	case A_NE:	    f = FUNCTOR_ar_not_equal2;	goto f_common;
	case A_FIRSTVAR_IS:
			  { size_t index;
			    index = *PC++;
			    ARGP[0] = ARGP[-1];
			    ARGP[-1] = makeVarRef(index);
			    ARGP++;
			  }
			    /*FALLTHROUGH*/
	case A_IS:	    f = FUNCTOR_is2;		goto f_common;
#endif /* O_COMPILE_ARITH */
#if O_CATCHTHROW
	case B_THROW:	    f = FUNCTOR_dthrow1;	goto f_common;
#endif
        case I_USERCALLN:   f = lookupFunctorDef(ATOM_call, (int)*PC++ + 1);
							f_common:
			    BUILD_TERM(f);
			    pushed++;
			    continue;
      }
      case I_FAIL:	    *ARGP++ = ATOM_fail;
			    pushed++;
			    continue;
      case I_TRUE:	    *ARGP++ = ATOM_true;
			    pushed++;
			    continue;
      case I_VAR:	    *ARGP++ = makeVarRef((int)*PC++);
			    BUILD_TERM(FUNCTOR_var1);
			    pushed++;
			    continue;
      case I_NONVAR:	    *ARGP++ = makeVarRef((int)*PC++);
			    BUILD_TERM(FUNCTOR_nonvar1);
			    pushed++;
			    continue;
      case C_LCUTIFTHEN:
      case C_LSCUT:
      case C_LCUT:	    PC++;
			    /*FALLTHROUGH*/
      case I_CUT:	    *ARGP++ = ATOM_cut;
			    pushed++;
			    continue;
      case I_CUTCHP:	    *ARGP++ = ATOM_dcut;
			    pushed++;
			    continue;
      case I_CATCH:	    *ARGP++ = ATOM_dcatch;
			    pushed++;
			    continue;
      case I_CALLCLEANUP:   *ARGP++ = ATOM_dcall_cleanup;
			    pushed++;
			    if ( *PC == encode(I_EXITCLEANUP) )
			      PC++;
			    continue;
      case I_CONTEXT:	    PC++;
			    assert(0);	/* should never happen */
			    continue;
      case I_DEPART:
      case I_CALL:        { Procedure proc = (Procedure)XR(*PC++);
			    BUILD_TERM(proc->definition->functor->functor);
			    pushed++;
			    continue;
			  }
#ifdef O_CALL_AT_MODULE
      case I_DEPARTATMV:
      case I_CALLATMV:	  { Module pm = (Module)XR(*PC++);
			    size_t cm = XR(*PC++);
			    Procedure proc = (Procedure)XR(*PC++);
			    BUILD_TERM(proc->definition->functor->functor);
			    if ( pm )
			    { ARGP++;
			      ARGP[-1] = ARGP[-2];	/* swap arguments */
			      ARGP[-2] = pm->name;
			      BUILD_TERM(FUNCTOR_colon2);
			    }
			    if ( nested )
			    { int rc = unifyVar(ARGP++, di->variables,
						cm PASS_LD);
			      if ( rc != TRUE )
				return rc;
			    } else
			    { *ARGP++ = makeVarRef(cm);
			    }
			    BUILD_TERM(FUNCTOR_xpceref2);
			    pushed++;
			    continue;
			  }
      case I_DEPARTATM:
      case I_CALLATM:     { Module pm = (Module)XR(*PC++);
			    Module cm = (Module)XR(*PC++);
			    Procedure proc = (Procedure)XR(*PC++);
			    BUILD_TERM(proc->definition->functor->functor);
			    if ( pm )
			    { ARGP++;
			      ARGP[-1] = ARGP[-2];	/* swap arguments */
			      ARGP[-2] = pm->name;
			      BUILD_TERM(FUNCTOR_colon2);
			    }
			    *ARGP++ = cm->name;
			    BUILD_TERM(FUNCTOR_xpceref2);
			    pushed++;
			    continue;
			  }
#endif
      case I_DEPARTM:
      case I_CALLM:       { Module m = (Module)XR(*PC++);
			    Procedure proc = (Procedure)XR(*PC++);
			    BUILD_TERM(proc->definition->functor->functor);
			    ARGP++;
			    ARGP[-1] = ARGP[-2];	/* swap arguments */
			    ARGP[-2] = m->name;
			    BUILD_TERM(FUNCTOR_colon2);
			    pushed++;
			    continue;
			  }
      case I_USERCALL0:	    BUILD_TERM(FUNCTOR_call1);
			    pushed++;
			    continue;
#if O_COMPILE_OR
#define DECOMPILETOJUMP { int to_jump = (int) *PC++; \
			  TRY_DECOMPILE(di, (code)-1, PC+to_jump); \
			}
      case C_CUT:
      case C_VAR:
      case C_JMP:
			    PC++;
			    continue;
      case C_VAR_N:
			    PC += 2;
			    continue;
      case C_OR:				/* A ; B */
			    DECOMPILETOJUMP;	/* A */
			    PC--;		/* get C_JMP argument */
			    DECOMPILETOJUMP;	/* B */
			    BUILD_TERM(FUNCTOR_semicolon2);
			    pushed++;
			    continue;
      case C_NOT:				/* \+ A */
			  { PC += 2;		/* skip the two arguments */
			    TRY_DECOMPILE(di, C_CUT, NULL);   /* A */
			    PC += 3;		/* skip C_CUT <n> and C_FAIL */
			    BUILD_TERM(FUNCTOR_not_provable1);
			    pushed++;
			    continue;
			  }
			  { Code adr1;
			    int jmp;
			    code icut;
			    functor_t f;
      case C_SOFTIF:				/* A *-> B ; C */
			    icut = C_SOFTCUT;
			    f = FUNCTOR_softcut2;
			    goto ifcommon;
      case C_IFTHENELSE:			/* A  -> B ; C */
			    icut = C_CUT;
			    f = FUNCTOR_ifthen2;
			ifcommon:
			    PC++;		/* skip the 'MARK' variable */
			    jmp  = (int) *PC++;
			    adr1 = PC+jmp;

			    TRY_DECOMPILE(di, icut, NULL);   /* A */
			    PC += 2;		/* skip the cut */
			    TRY_DECOMPILE(di, (code)-1, adr1);	    /* B */
			    BUILD_TERM(f);
			    PC--;
			    DECOMPILETOJUMP;	/* C */
			    BUILD_TERM(FUNCTOR_semicolon2);
			    pushed++;
			    continue;
			  }
    { code cut;
      functor_t f;
      case C_IFTHEN:				/* A -> B */
			    cut = C_CUT;
			    f = FUNCTOR_ifthen2;
			    goto c_ifthen;
      case C_SOFTIFTHEN:				/* A *->B */
			    cut = C_SCUT;
			    f = FUNCTOR_softcut2;
			  c_ifthen:
			    PC++;
			    TRY_DECOMPILE(di, cut, NULL);   /* A */
			    PC += 2;
			    TRY_DECOMPILE(di, C_END, NULL);   /* B */
			    PC++;
			    BUILD_TERM(f);
			    pushed++;
			    continue;
    }
#endif /* O_COMPILE_OR */
      case I_EXITCATCH:
	goto exit;
      case I_EXIT:
			    break;
      default:
	  sysError("Decompiler: unknown instruction in clause body: %s",
		   codeTable[op].name);
	  /*NOTREACHED*/
    }
  }

  while( pushed-- > 1)
    BUILD_TERM(FUNCTOR_comma2);

  return TRUE;
}


static int
put_functor(Word p, functor_t f ARG_LD)
{ int arity = arityFunctor(f);
  Word a, t;

  if ( gTop+1+arity > gMax )
    return GLOBAL_OVERFLOW;

  a = t = gTop;
  gTop += (1+arity);

  *a = f;
  while( --arity >= 0 )
    setVar(*++a);

  *p = consPtr(t, TAG_COMPOUND|STG_GLOBAL);
  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Build the actual term.  The arguments are on  the  decompilation  stack.
We  construct a term of requested arity and name, copy `arity' arguments
from the stack into the term and finally  push  the  term  back  on  the
stack.

Returns one of TRUE or *_OVERFLOW
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
build_term(functor_t f, decompileInfo *di ARG_LD)
{ word term;
  int i, arity = arityFunctor(f);
  Word a;

  if ( arity == 0 )
  { *ARGP++ = nameFunctor(f);
    return TRUE;
  }

  if ( gTop+1+arity > gMax )
    return GLOBAL_OVERFLOW;
  a = gTop;
  gTop += 1+arity;
  term = consPtr(a, TAG_COMPOUND|STG_GLOBAL);
  *a = f;
  for(i=0; i<arity; i++)
    setVar(*++a);
					/* now a point to last argument */

  ARGP--;
  for( ; arity-- > 0; a--, ARGP-- )
  { ssize_t var;

    if ( (var = isVarRef(*ARGP)) >= 0 )
    { int rc;

      if ( (rc=unifyVar(a, di->variables, var PASS_LD)) != TRUE )
	return rc;
    } else
    { *a = *ARGP;
    }
  }
  ARGP++;

  *ARGP++ = term;
  return TRUE;
}

#undef PC
#undef ARGP

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unify_definition(+Module, ?Head, +Def, -TheHead, flags)
    Given some definition, unify its Prolog reference (i.e. its head with
    optional module specifier) with ?Head.  If TheHead is specified, the
    plain head (i.e. without module specifier) will be referenced from
    this term-reference.

    This function properly deals with module-inheritance, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
unify_functor(term_t t, functor_t fd, int how)
{ if ( how&GP_NAMEARITY )
  { FunctorDef fdef = valueFunctor(fd);

    return PL_unify_term(t,
			 PL_FUNCTOR, FUNCTOR_divide2,
			   PL_ATOM, fdef->name,
			   PL_INT, fdef->arity);
  } else
  { return PL_unify_functor(t, fd);
  }
}


int
PL_unify_predicate(term_t head, predicate_t pred, int how)
{ return unify_definition(MODULE_user, head, pred->definition, 0, how);
}


int
unify_definition(Module ctx, term_t head, Definition def, term_t thehead, int how)
{ GET_LD

  if ( PL_is_variable(head) )
  { if ( !(how&GP_QUALIFY) &&
	 (def->module == ctx ||
	  ((how&GP_HIDESYSTEM) && true(def->module, SYSTEM))) )
    { if ( !unify_functor(head, def->functor->functor, how) )
	return FALSE;
      if ( thehead )
	PL_put_term(thehead, head);
    } else
    { term_t tmp;

      if ( !(tmp=PL_new_term_ref()) ||
	   !PL_unify_functor(head, FUNCTOR_colon2) ||
	   !PL_get_arg(1, head, tmp) ||
	   !PL_unify_atom(tmp, def->module->name) ||
	   !PL_get_arg(2, head, tmp) ||
	   !unify_functor(tmp, def->functor->functor, how) )
	return FALSE;
      if ( thehead )
	PL_put_term(thehead, tmp);
    }

    succeed;
  } else
  { term_t h;

    if ( PL_is_functor(head, FUNCTOR_colon2) )
    { if ( !(h=PL_new_term_ref()) )
	return FALSE;

      _PL_get_arg(1, head, h);
      if ( !PL_unify_atom(h, def->module->name) )
      { atom_t a;
	Module m;

	if ( !PL_get_atom(h, &a) ||
	     !(m = isCurrentModule(a)) ||
	     !isSuperModule(def->module, m) )
	  fail;
      }

      _PL_get_arg(2, head, h);
    } else
      h = head;

    if ( unify_functor(h, def->functor->functor, how) )
    { if ( thehead )
	PL_put_term(thehead, h);
      succeed;
    }

    fail;
  }
}


static int
unify_head(term_t h, term_t d ARG_LD)
{ if ( !PL_unify(h, d) )
  { term_t h1, d1;
    Module m = NULL;

    if ( !(h1 = PL_new_term_ref()) ||
	 !(d1 = PL_new_term_ref()) )
      return FALSE;

    PL_strip_module(h, &m, h1);
    PL_strip_module(d, &m, d1);

    return PL_unify(h1, d1);
  } else
    return TRUE;
}


/** clause(H, B).
    clause(H, B, Ref).
    clause(H, B, Ref, Bindings).
*/


static
PRED_IMPL("clause", va, clause, PL_FA_TRANSPARENT|PL_FA_NONDETERMINISTIC)
{ PRED_LD
  Procedure proc;
  Definition def;
  struct clause_choice chp_buf;
  ClauseChoice chp;
  ClauseRef cref;
  Word argv;
  Module module = NULL;
  term_t term = PL_new_term_ref();
  term_t h    = PL_new_term_ref();
  term_t b    = PL_new_term_ref();
  LocalFrame fr = environment_frame;
  fid_t fid;

  term_t head     = A1;
  term_t body     = A2;
  term_t ref      = (CTX_ARITY >= 3 ? A3 : 0);
  term_t bindings = (CTX_ARITY >= 4 ? A4 : 0);

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { Clause clause;

      if ( ref && !PL_is_variable(ref) )
      { int rc;

	if ( (rc=PL_get_clref(ref, &clause)) )
	{ term_t tmp;

	  if ( rc < 0 && CTX_ARITY < 4 )
	    return FALSE;			/* erased clause */

	  if ( decompile(clause, term, bindings) != TRUE )
	    return FALSE;
	  proc = clause->procedure;
	  def = getProcDefinition(proc);
	  if ( true(clause, GOAL_CLAUSE) )
	  { tmp = head;
	  } else
	  { tmp = PL_new_term_ref();
	    if ( !unify_definition(contextModule(LD->environment), head, def, tmp, 0) )
	      fail;
	  }
	  if ( !get_head_and_body_clause(term, h, b, NULL PASS_LD) )
	    return FALSE;
	  if ( unify_head(tmp, h PASS_LD) && PL_unify(body, b) )
	    succeed;
	}

	fail;
      }
      if ( !get_procedure(head, &proc, 0, GP_FIND) )
	fail;
      def = getProcDefinition(proc);

      if ( true(def, FOREIGN) ||
	   (   truePrologFlag(PLFLAG_ISO) &&
	       false(def, DYNAMIC)
	   ) )
	return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
			ATOM_access, ATOM_private_procedure, proc);

      chp = NULL;
      enterDefinition(def);		/* reference the predicate */
      break;
    }
    case FRG_REDO:
      chp  = CTX_PTR;
      proc = chp->cref->value.clause->procedure;
      def  = getProcDefinition(proc);
      break;
    case FRG_CUTTED:
      chp = CTX_PTR;
      proc = chp->cref->value.clause->procedure;
      def  = getProcDefinition(proc);
      leaveDefinition(def);
      freeForeignState(chp, sizeof(*chp));
      succeed;
    default:
      assert(0);
  }

  if ( def->functor->arity > 0 )
  { PL_strip_module(head, &module, head);
    argv = valTermRef(head);
    deRef(argv);
    argv = argTermP(*argv, 0);
  } else
    argv = NULL;

  if ( !chp )
  { chp = &chp_buf;
    cref = firstClause(argv, fr, def, chp PASS_LD);
  } else
  { cref = nextClause(chp, argv, fr, def);
  }

  if ( !(fid = PL_open_foreign_frame()) )
    return FALSE;

  while(cref)
  { if ( decompile(cref->value.clause, term, bindings) )
    { if ( !get_head_and_body_clause(term, h, b, NULL PASS_LD) )
	break;
      if ( unify_head(head, h PASS_LD) &&
	   PL_unify(b, body) &&
	   (!ref || PL_unify_clref(ref, cref->value.clause)) )
      { if ( !chp->cref )
	{ leaveDefinition(def);
	  succeed;
	}
	if ( chp == &chp_buf )
	{ chp = allocForeignState(sizeof(*chp));
	  *chp = chp_buf;
	}

	ForeignRedoPtr(chp);
      } else
      { PL_put_variable(h);		/* otherwise they point into */
	PL_put_variable(b);		/* term, which is removed */
      }
    }

    PL_rewind_foreign_frame(fid);
    if ( argv )
    { argv = valTermRef(head);		/* argv may be corrupted in GC */
      deRef(argv);
      argv = argTermP(*argv, 0);
    }
    cref = nextClause(chp, argv, fr, def);
  }

  if ( chp != &chp_buf )
    freeForeignState(chp, sizeof(*chp));
  leaveDefinition(def);
  fail;
}


typedef struct
{ ClauseRef clause;			/* pointer to the clause */
  int       index;			/* nth-1 index */
} *Cref;


word
pl_nth_clause(term_t p, term_t n, term_t ref, control_t h)
{ GET_LD
  Clause clause;
  ClauseRef cref;
  Procedure proc;
  Definition def;
  Cref cr;
#ifdef O_LOGICAL_UPDATE
  gen_t generation = environment_frame->generation;
#endif

  if ( ForeignControl(h) == FRG_CUTTED )
  { cr = ForeignContextPtr(h);

    if ( cr )
    { def = getProcDefinition(cr->clause->value.clause->procedure);
      leaveDefinition(def);
      freeForeignState(cr, sizeof(*cr));
    }
    succeed;
  }

  if ( !PL_is_variable(ref) )
  { if ( PL_get_clref(ref, &clause) == TRUE )
    { int i;

      if ( true(clause, GOAL_CLAUSE) )
	fail;				/* I do not belong to a predicate */

      proc = clause->procedure;
      def  = getProcDefinition(proc);
      for( cref = def->impl.clauses.first_clause, i=1; cref; cref = cref->next)
      { if ( cref->value.clause == clause )
	{ if ( !PL_unify_integer(n, i) ||
	       !unify_definition(contextModule(LD->environment), p, def, 0, 0) )
	    fail;

	  succeed;
	}
	if ( visibleClause(cref->value.clause, generation) )
	  i++;
      }
    }

    fail;
  }

  if ( ForeignControl(h) == FRG_FIRST_CALL )
  { int i;

    if ( !get_procedure(p, &proc, 0, GP_FIND) ||
         true(proc->definition, FOREIGN) )
      fail;

    def = getProcDefinition(proc);
    cref = def->impl.clauses.first_clause;
    while ( cref && !visibleClause(cref->value.clause, generation) )
      cref = cref->next;

    if ( !cref )
      fail;

    if ( PL_get_integer(n, &i) )	/* proc and n specified */
    { i--;				/* 0-based */

      while(i > 0 && cref)
      { do
	{ cref = cref->next;
	} while ( cref && !visibleClause(cref->value.clause, generation) );

	i--;
      }
      if ( i == 0 && cref )
	return PL_unify_clref(ref, cref->value.clause);
      fail;
    }

    cr = allocForeignState(sizeof(*cr));
    cr->clause = cref;
    cr->index  = 1;
    enterDefinition(def);
  } else
  { cr = ForeignContextPtr(h);
    def = getProcDefinition(cr->clause->value.clause->procedure);
  }

  PL_unify_integer(n, cr->index);
  PL_unify_clref(ref, cr->clause->value.clause);

  cref = cr->clause->next;
  while ( cref && !visibleClause(cref->value.clause, generation) )
    cref = cref->next;

  if ( cref )
  { cr->clause = cref;
    cr->index++;
    ForeignRedoPtr(cr);
  }

  freeForeignState(cr, sizeof(*cr));
  leaveDefinition(def);

  succeed;
}


#if O_DEBUGGER				/* to the end of the file */

static int
wouldBindToDefinition(Definition from, Definition to)
{ Module m = from->module;
  Definition def = from;
  Procedure proc;

  for(;;)
  { ListCell c;

    if ( def )
    { if ( def == to )			/* found it */
	succeed;

      if ( def->impl.any ||		/* defined and not the same */
	   true(def, PROC_DEFINED) ||
	   getUnknownModule(def->module) == UNKNOWN_FAIL )
	fail;
    }

    if ( (c = m->supers) )
    { GET_LD

      m = c->value;			/* TBD: multiple supers */
      proc = isCurrentProcedure(from->functor->functor, m);
      def = proc ? getProcDefinition(proc) : (Definition)NULL;
    } else
      break;
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
'$xr_member'(+ClauseRef, ?Object)

Is true if Object is referenced by ClauseRef.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("$xr_member", 2, xr_member, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  Clause clause = NULL;
  Code PC;
  Code end;
  term_t term = A2;

  if ( CTX_CNTRL == FRG_CUTTED )
    succeed;

  if ( PL_get_clref(A1, &clause) != TRUE )
    fail;

  PC  = clause->codes;
  end = &PC[clause->code_size];

  if ( PL_is_variable(term) )
  { int an;

    if ( CTX_CNTRL != FRG_FIRST_CALL)
    { size_t i = CTX_INT;

      PC += i >> 3;
      an = (int)i & 0x7;
    } else
    { an = 0;
    }

    for( ; PC < end; PC = stepPC(PC),an=0 )
    { code op = fetchop(PC);
      const char *ats=codeTable[op].argtype;

      while(ats[an])
      { int rc;

	switch(ats[an++])
	{ case CA1_PROC:
	  { size_t i;
	    Procedure proc = (Procedure) PC[an];
	    rc = unify_definition(MODULE_user, term, getProcDefinition(proc), 0, 0);
	  hit:
	    if ( !rc )
	      return FALSE;		/* out of stack */
	    i = ((PC - clause->codes)<<3) + an;
	    ForeignRedoInt(i);
	  }
	  case CA1_FUNC:
	  { functor_t fd = (functor_t) PC[an];
	    rc = PL_unify_functor(term, fd);
	    goto hit;
	  }
	  case CA1_DATA:
	  { word xr = PC[an];
	    rc = _PL_unify_atomic(term, xr);
	    goto hit;
	  }
	  case CA1_MODULE:
	  { Module xr = (Module)PC[an];
	    rc = PL_unify_atom(term, xr->name);
	    goto hit;
	  }
	}
      }
    }

    fail;
  } else				/* instantiated */
  { Procedure proc;
    functor_t fd;
    int an = 0;

    if ( PL_is_atomic(term) )
    { for( ; PC < end; PC = stepPC(PC),an=0 )
      { code op = fetchop(PC);
	const char *ats=codeTable[op].argtype;

	while(ats[an])
	{ switch(ats[an++])
	  { case CA1_DATA:
	      if ( _PL_unify_atomic(term, PC[an]) )
		succeed;
	      break;
	    case CA1_MODULE:
	    { Module xr = (Module)PC[an];

	      if ( PL_unify_atom(term, xr->name) )
		succeed;
	    }
	  }
	}
      }
    }

    PC = clause->codes;
    if ( PL_get_functor(term, &fd) && fd != FUNCTOR_colon2 )
    { for( ; PC < end; PC = stepPC(PC),an=0 )
      { code op = fetchop(PC);
	const char *ats=codeTable[op].argtype;

	while(ats[an])
	{ switch(ats[an++])
	  { case CA1_FUNC:
	    { functor_t fa = (functor_t) PC[an];

	      if ( fa == fd )
		succeed;
	    }
	  }
	}
      }
    }

    PC = clause->codes;
    if ( get_procedure(term, &proc, 0, GP_FINDHERE|GP_TYPE_QUIET) )
    { Definition pd = getProcDefinition(proc);

      for( ; PC < end; PC = stepPC(PC),an=0 )
      { code op = fetchop(PC);
	const char *ats=codeTable[op].argtype;

	while(ats[an])
	{ switch(ats[an++])
	  { case CA1_PROC:
	    { Procedure pa = (Procedure)PC[an];
	      Definition def = getProcDefinition(pa);

	      if ( pd == def )
		succeed;
	      if ( pd->functor == def->functor &&
		   wouldBindToDefinition(def, pd) )
		succeed;
	    }
	  }
	}
      }
    }
  }

  fail;
}

		 /*******************************
		 *	   SIMPLE VM LIST	*
		 *******************************/

void
vm_list(Code start)
{ Code PC;

  for(PC=start; ; PC=stepPC(PC))
  { code op = fetchop(PC);
    const code_info *ci = &codeTable[op];

    Sdprintf("%-3d %s\n", PC-start, ci->name);
    switch(op)
    { case I_EXIT:
      case I_EXITFACT:
	return;
    }
  }
}


		 /*******************************
		 *	 CLAUSE <-> PROLOG	*
		 *******************************/

#define VARNUM(i) ((int)((i) - (ARGOFFSET / (int) sizeof(word))))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Translate between a clause  and  a   Prolog  description  of the virtual
machine code. Returns pointer to the next   instruction or NULL if there
is an error or unification failed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Code
unify_vmi(term_t t, Code bp)
{ GET_LD
  code op = decode(*bp);
  const code_info *ci;
  int rc;

  if ( op == D_BREAK )
  { term_t t2 = PL_new_term_ref();

    if ( !PL_unify_functor(t, FUNCTOR_break1) ||
	 !PL_get_arg(1, t, t2) )
      return NULL;
    t = t2;

    op = decode(replacedBreak(bp));
  }

  ci = &codeTable[op];
  bp++;					/* skip the instruction */

  if ( ci->arguments == 0 )
  { if ( !PL_unify_atom_chars(t, ci->name) )
      return NULL;
  } else
  { const char *ats = codeTable[op].argtype;
    term_t av = PL_new_term_refs((int)strlen(ats));
    int an;

    for(an=0; ats[an]; an++)
    { int rc;

      switch(ats[an])
      { case CA1_VAR:
	case CA1_CHP:
	{ int vn =  VARNUM(*bp++);

	  rc = PL_put_integer(av+an, vn);
	  break;
	}
	case CA1_INTEGER:
	case CA1_JUMP:
	{ intptr_t i = (intptr_t)*bp++;

	  rc = PL_put_int64(av+an, i);
	  break;
	}
	case CA1_FLOAT:
	{ union
	  { double d;
	    word   w[WORDS_PER_DOUBLE];
	  } v;
	  Word f = v.w;

	  cpDoubleData(f, bp);
	  rc = PL_put_float(av+an, v.d);
	  break;
	}
	case CA1_INT64:
	{ int64_t i;
	  Word dp = (Word)&i;

	  cpInt64Data(dp, bp);
	  rc = PL_put_int64(av+an, i);
	  break;
	}
	case CA1_DATA:
	{ rc = _PL_unify_atomic(av+an, *bp++);
	  break;
	}
	case CA1_FUNC:
	{ functor_t f = (functor_t) *bp++;
	  rc = unify_functor(av+an, f, GP_NAMEARITY);
	  break;
	}
	case CA1_MODULE:
	{ Module m = (Module)*bp++;
	  PL_put_atom(av+an, m->name);
	  rc = TRUE;
	  break;
	}
	case CA1_PROC:
	{ Procedure proc = (Procedure)*bp++;

	  rc = unify_definition(MODULE_user, av+an, proc->definition, 0,
				GP_HIDESYSTEM|GP_NAMEARITY);
	  break;
	}
	case CA1_CLAUSEREF:
	{ ClauseRef cref = (ClauseRef)*bp++;

	  rc = PL_unify_term(av+an, PL_FUNCTOR, FUNCTOR_clause1,
			     PL_POINTER, cref->value.clause);

	  break;
	}
	case CA1_FOREIGN:
	{ void *func = (void*)*bp++;

#ifdef HAVE_DLADDR
	  Dl_info info;

	  if ( dladdr(func, &info) )
	  { if ( info.dli_sname )
	      rc = PL_unify_term(av+an, PL_FUNCTOR, FUNCTOR_colon2,
				 PL_CHARS, info.dli_fname,
				 PL_CHARS, info.dli_sname);
	    else
	      rc = PL_unify_term(av+an, PL_FUNCTOR, FUNCTOR_plus2,
				 PL_CHARS, info.dli_fname,
				 PL_INTPTR, (char*)func-(char*)info.dli_fbase);
	    break;
	  }
#endif
	  rc = PL_put_pointer(av+an, func);
	  break;
	}
	case CA1_AFUNC:
	{ int findex = (int)*bp++;
	  functor_t f = functorArithFunction(findex);
	  rc = unify_functor(av+an, f, GP_NAMEARITY);
	  break;
	}
	case CA1_MPZ:
	case CA1_STRING:
	{ word c = globalIndirectFromCode(&bp);
	  rc = _PL_unify_atomic(av+an, c);
	  break;
	}
	default:
	  Sdprintf("Cannot list %d-th arg of %s (type=%d)\n",
		   an+1, ci->name, ats[an]);
	  return NULL;
      }
      if ( !rc )
	return NULL;			/* resource error */
    }

    switch(an)
    { case 1:
	rc = PL_unify_term(t, PL_FUNCTOR_CHARS, ci->name, an,
			   PL_TERM, av+0);
        break;
      case 2:
	rc = PL_unify_term(t, PL_FUNCTOR_CHARS, ci->name, an,
			   PL_TERM, av+0, PL_TERM, av+1);
        break;
      case 3:
	rc = PL_unify_term(t, PL_FUNCTOR_CHARS, ci->name, an,
			   PL_TERM, av+0, PL_TERM, av+1, PL_TERM, av+2);
        break;
      default:
	assert(0);
        rc = FALSE;
    }
    if ( !rc )
      return NULL;
  }

  return bp;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
$fetch_vm(+ClauseOrProc, +PC, -NextPC, -Instruction) is det.

Intruction is the VM instruction at PC and  NextPC is the PC of the next
instruction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("$fetch_vm", 4, fetch_vm, PL_FA_TRANSPARENT)
{ PRED_LD
  Clause clause = NULL;
  Procedure proc = NULL;
  size_t len;
  intptr_t pcoffset;
  Code base, PC, next;

  term_t from = A1;
  term_t offset = A2;
  term_t noffset = A3;
  term_t instruction = A4;

  if ( PL_is_dbref(from) )
  { if ( PL_get_clref(from, &clause) != TRUE )
      fail;
    base = clause->codes;
    len  = (size_t)clause->code_size;
  } else
  { Module module = NULL;
    functor_t fd;

    if ( !get_functor(from, &fd, &module, 0, GF_PROCEDURE) ||
	 !(proc = resolveProcedure(fd, module)) )
      fail;
    base = proc->definition->codes;
    if ( !base )
      fail;
    len = supervisorLength(base);
  }

  if ( !PL_get_intptr_ex(offset, &pcoffset) )
    fail;
  if ( pcoffset < 0 || pcoffset > (intptr_t)len )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_program_counter, offset);
  if ( pcoffset == (intptr_t)len )
    fail;

  PC = base + pcoffset;

  if ( (next=unify_vmi(instruction, PC)) )
    return PL_unify_int64(noffset, next-base);

  fail;
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
'$vm_assert'(+PI, :VM, -Ref) is det.

Create a clause from VM and  assert  it   to  the  predicate  PI. Ref is
unified with a reference to the new clause.

TBD The current implementation is very incomplete. Using direct jumps is
very unattractive and we should abstract away from some details, such as
the different integer sizes (inline, int, int64, mpz).  Other issues:

	- Automatic variable balancing
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static const code_info *
lookup_vmi(atom_t name)
{ static Table ctable = NULL;
  Symbol s;

  if ( !ctable )
  { PL_LOCK(L_MISC);
    if ( !ctable )
    { int i;

      ctable = newHTable(32);
      for(i=0; i<I_HIGHEST; i++)
       addHTable(ctable, (void*)PL_new_atom(codeTable[i].name), (void*)&codeTable[i]);
    }
    PL_UNLOCK(L_MISC);
  }

  if ( (s=lookupHTable(ctable, (void*)name)) )
    return (const code_info*)s->value;

  return NULL;
}



static int
vm_compile_instruction(term_t t, CompileInfo ci)
{ GET_LD
  atom_t name;
  int arity;

  if ( PL_get_name_arity(t, &name, &arity) )
  { const code_info *cinfo;

    if ( (cinfo = lookup_vmi(name)) )
    { Output_0(ci, cinfo->code);
      if ( cinfo->code == I_EXITFACT )
	set(ci->clause, UNIT_CLAUSE);

      if ( arity == 0 )
      { assert(cinfo->arguments == 0);
      } else
      { const char *ats = cinfo->argtype;
	int an;
	term_t a = PL_new_term_ref();

	assert(cinfo->arguments == VM_DYNARGC ||
	       strlen(ats) == (size_t)cinfo->arguments);

	for(an=0; ats[an]; an++)
	{ _PL_get_arg(an+1, t, a);

	  switch(ats[an])
	  { case CA1_VAR:
	    case CA1_CHP:
	    { size_t vn, i;

	      if ( !PL_get_size_ex(a, &vn) )
		fail;
	      i = VAROFFSET(vn);
	      Output_a(ci, i);
	      if ( ats[an] == CA1_VAR && ci->clause->prolog_vars < i )
		ci->clause->prolog_vars = (unsigned int)i;
	      if ( ci->clause->variables < i )
		ci->clause->variables = (unsigned int)i;
	      break;
	    }
	    case CA1_INTEGER:
	    case CA1_JUMP:
	    { intptr_t val;

	      if ( !PL_get_intptr_ex(a, &val) )
		fail;
	      Output_a(ci, val);
	    }
	    case CA1_FLOAT:
	    { double d;
	      Word p = (Word)&d;

	      if ( !PL_get_float_ex(a, &d) )
		fail;
	      Output_an(ci, p, WORDS_PER_DOUBLE);
	    }
	    case CA1_INT64:
	    { int64_t val;
	      Word p = (Word)&val;

	      if ( !PL_get_int64_ex(a, &val) )
		fail;
	      Output_an(ci, p, WORDS_PER_INT64);
	    }
	    case CA1_MPZ:
	    case CA1_STRING:
	    { Word ap = valTermRef(a);
	      Word p;
	      size_t n;

	      deRef(ap);
	      switch(ats[an])
	      { case CA1_MPZ:
		  if ( !isIndirect(*ap) ||
		       !isInteger(*ap) )
		    return PL_error(NULL, 0, "must be an mpz", ERR_TYPE, ATOM_integer, a);
		  p = addressIndirect(*ap);
		  n = wsizeofInd(*p);
		  if ( n == WORDS_PER_INT64 )
		    return PL_error(NULL, 0, "must be an mpz", ERR_TYPE, ATOM_integer, a);
		  break;
		case CA1_STRING:
		  if ( !isString(*ap) )
		    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_string, a);
		  break;
	      }
	      p = addressIndirect(*ap);
	      n = wsizeofInd(*p);
	      Output_an(ci, p, n+1);
	      break;
	    }
	    case CA1_DATA:
	    { word val = _PL_get_atomic(a);

	      if ( !isConst(val) )
		return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atomic, a);

	      Output_a(ci, val);
	      break;
	    }
	    case CA1_FUNC:
	    { functor_t f;
	      Module m = NULL;

	      if ( !get_functor(a, &f, &m, 0, 0) )
		fail;

	      Output_a(ci, f);
	      break;
	    }
	    case CA1_MODULE:
	    { atom_t name;

	      if ( !PL_get_atom_ex(a, &name) )
		fail;

	      Output_a(ci, (intptr_t)lookupModule(name));
	      break;
	    }
	    case CA1_PROC:
	    { Procedure proc;

	      if ( get_procedure(a, &proc, 0, GP_CREATE|GP_NAMEARITY) )
	      { Output_a(ci, (code)proc);
		break;
	      }
	      fail;
	    }
	  }
	}
      }
      return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_vmi, t);
    }

    return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_vmi, t);
  }

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_vmi, t);
}


static int
vm_compile(term_t t, CompileInfo ci)
{ GET_LD
  term_t head = PL_new_term_ref();

  while(PL_get_list(t, head, t))
  { if ( !vm_compile_instruction(head, ci) )
      fail;
  }

  if ( !PL_get_nil(t) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_list, t);

  succeed;
}


static
PRED_IMPL("$vm_assert", 3, vm_assert, PL_FA_TRANSPARENT)
{ GET_LD
  Procedure proc;
  compileInfo ci;
  struct clause clause;
  Clause cl;
  Module module = NULL;
  size_t size;

  if ( !get_procedure(A1, &proc, 0, GP_DEFINE|GP_NAMEARITY) )
    fail;
  PL_strip_module(A2, &module, A2);

  ci.islocal      = FALSE;
  ci.subclausearg = 0;
  ci.arity        = proc->definition->functor->arity;
  ci.argvars      = 0;

  clause.flags       = 0;
  clause.procedure   = proc;
  clause.code_size   = 0;
  clause.source_no   = clause.line_no = 0;
  clause.variables   = ci.arity;
  clause.prolog_vars = ci.arity;
  ci.clause	     = &clause;
  ci.module	     = module;
  initBuffer(&ci.codes);

  if ( !vm_compile(A2, &ci) )
  { discardBuffer(&ci.codes);
    fail;
  }

  clause.code_size = entriesBuffer(&ci.codes, code);
  size  = sizeofClause(clause.code_size);
  cl = PL_malloc_atomic(size);
  memcpy(cl, &clause, sizeofClause(0));
  GD->statistics.codes += clause.code_size;
  memcpy(cl->codes, baseBuffer(&ci.codes, code), sizeOfBuffer(&ci.codes));
  discardBuffer(&ci.codes);

					/* TBD: see assert_term() */
  if ( !assertProcedure(proc, cl, CL_END PASS_LD) )
    fail;

  return PL_unify_clref(A3, cl);
}


		 /*******************************
		 *     SOURCE LEVEL DEBUGGER	*
		 *******************************/

static Code
find_code1(Code PC, code fop, code ctx)
{ for(;; PC = stepPC(PC))
  { code op = fetchop(PC);

    if ( fop == op && ctx == PC[1] )
      return PC;
    assert(op != I_EXIT);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find C_END, where we need to jump over control structures.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Code
find_if_then_end(Code PC, Code base)
{ for(;;)
  { code op = fetchop(PC);
    Code nextpc = stepPC(PC);

    if ( op == C_END )
      return PC;

    assert(op != I_EXIT);

    switch(op)				/* jump over control structures */
    { case C_OR:
      { Code jmploc;

	jmploc = nextpc + PC[1];
	PC = jmploc + jmploc[-1];
	DEBUG(MSG_SRCLOC, Sdprintf("find_if_then_end: C_OR --> %d\n", PC-base));
	break;
      }
      case C_NOT:
	PC = nextpc + PC[2];
        break;
      case C_SOFTIF:
      case C_IFTHENELSE:
      { Code elseloc = nextpc + PC[2];

	PC = elseloc + elseloc[-1];
	break;
      }
      case C_IFTHEN:
      { Code cutloc = find_code1(nextpc, C_CUT, PC[1]);
	PC = find_if_then_end(cutloc+2, base) + 1; /* returns location of C_END */
	break;
      }
      default:
	PC = nextpc;
        break;
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
'$clause_term_position'(+ClauseRef, +PCoffset, -TermPos)

Find the location of the subterm of the   call for which PCoffset is the
continuation PC, i.e. the PC that follows the call. The term-position is
a list of argument-numbers one has to   use from the clause-term to find
the subterm that sets up the goal.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
add_node(term_t tail, int n ARG_LD)
{ term_t h = PL_new_term_ref();
  int rval;

  rval = PL_unify_list(tail, h, tail) && PL_unify_integer(h, n);
  PL_reset_term_refs(h);

  DEBUG(MSG_SRCLOC, Sdprintf("Added %d\n", n));

  return rval;
}


static void
add_1_if_not_at_end(Code PC, Code end, term_t tail ARG_LD)
{ while(PC < end && (fetchop(PC) == C_VAR || fetchop(PC) == C_VAR_N) )
    PC = stepPC(PC);

  if ( PC != end )
  { DEBUG(MSG_SRCLOC, Sdprintf("not-at-end: adding 1\n"));
    add_node(tail, 1 PASS_LD);
  }
}


static int
not_breakable(atom_t op, Clause clause, int offset)
{ GET_LD
  term_t brk;

  if ( (brk=PL_new_term_ref()) &&
       PL_unify_term(brk,
		     PL_FUNCTOR, FUNCTOR_break2,
		       PL_POINTER, clause,
		       PL_INT, offset) )
    return PL_error(NULL, 0, NULL, ERR_PERMISSION,
		    op, ATOM_break, brk);

  return FALSE;
}


static
PRED_IMPL("$clause_term_position", 3, clause_term_position, 0)
{ PRED_LD
  Clause clause = NULL;
  int pcoffset;
  Code PC, loc, end;
  term_t tail = PL_copy_term_ref(A3);

  if ( !PL_get_clref(A1, &clause) ||
       !PL_get_integer_ex(A2, &pcoffset) )
    fail;
  if ( pcoffset < 0 || pcoffset > (int)clause->code_size )
    return PL_error(NULL, 0, "not in clause executable code",
		    ERR_DOMAIN, ATOM_program_counter, A2);

  PC = clause->codes;
  loc = &PC[pcoffset];
  end = &PC[clause->code_size - 1];		/* forget the final I_EXIT */
  assert(fetchop(end) == I_EXIT ||
	 fetchop(end) == I_EXITFACT ||
	 fetchop(end) == I_EXITCATCH);

  if ( pcoffset == (int)clause->code_size )
    return PL_unify_atom(A3, ATOM_exit);

  if ( true(clause, GOAL_CLAUSE) )
    add_node(tail, 2 PASS_LD);			/* $call :- <Body> */

  while( PC < loc )
  { code op = fetchop(PC);
    Code nextpc = stepPC(PC);

    DEBUG(MSG_SRCLOC,
	  Sdprintf("\t%s at %d\n", codeTable[op].name, PC-clause->codes));

    switch(op)
    { case I_ENTER:
	if ( loc == nextpc )
	{ add_node(tail, 1 PASS_LD);

	  return PL_unify_nil(tail);
	}
	add_node(tail, 2 PASS_LD);
	PC = nextpc;
	continue;
      case I_EXIT:
      case I_EXITFACT:
      case I_EXITCATCH:
      case I_EXITCLEANUP:
	if ( loc == nextpc )
	{ return PL_unify_nil(tail);
	}
        continue;
    { Code endloc;
      case C_OR:			/* C_OR <jmp1> <A> C_JMP <jmp2> <B> */
      { Code jmploc;

	jmploc = nextpc + PC[1];
	endloc = jmploc + jmploc[-1];
	PC = nextpc;

	DEBUG(MSG_SRCLOC,
	      Sdprintf("jmp = %d, end = %d\n",
		       jmploc - clause->codes, endloc - clause->codes));

	if ( loc <= endloc )		/* loc is in the disjunction */
	{ add_1_if_not_at_end(endloc, end, tail PASS_LD);

	  if ( loc <= jmploc )		/* loc is in first branch */
	  { add_node(tail, 1 PASS_LD);
	    end = jmploc-2;
	    continue;
	  }
					/* loc is in second branch */
	  add_node(tail, 2 PASS_LD);
	  PC = jmploc;
	  end = endloc;
	  continue;
	}

      after_construct:
	add_node(tail, 2 PASS_LD);	/* loc is after disjunction */
	PC = endloc;
	continue;
      }
      case C_NOT:	/* C_NOT <var> <jmp> <A> C_CUT <var>, C_FAIL, */
      { Code endnot;	/* [C_JUMP <N>, C_VAR*] */
	endloc = nextpc+PC[2];
	PC = nextpc;

	endnot = endloc-3;
	if ( endnot[0] != encode(C_CUT) )
	{ endnot -= 2;
	  assert(endnot[0] == encode(C_CUT));
	}

	DEBUG(MSG_SRCLOC,
	      Sdprintf("not: PC=%d, endnot=%d, endloc=%d\n",
		       PC - clause->codes,
		       endnot - clause->codes,
		       endloc - clause->codes));

	if ( loc <= endnot )		/* in the \+ argument */
	{ add_1_if_not_at_end(endloc, end, tail PASS_LD);

	  add_node(tail, 1 PASS_LD);
	  end = endnot;			/* C_CUT <var>, C_FAIL */
	  DEBUG(MSG_SRCLOC,
		Sdprintf("Inside not: PC=%d, end = %d\n",
			 PC - clause->codes, end - clause->codes));
	  continue;
	} else if ( loc <= endloc )
	{ return PL_error(NULL, 0, "not a possible continuation",
			  ERR_DOMAIN, ATOM_program_counter, A2);
	}

	goto after_construct;
      }
      case C_SOFTIF:
      case C_IFTHENELSE:	/* C_IFTHENELSE <var> <jmp1> */
				/* <IF> C_CUT <THEN> C_JMP <jmp2> <ELSE> */
      { Code elseloc = nextpc + PC[2];
	code cut = (op == C_IFTHENELSE ? C_CUT : C_SOFTCUT);

	endloc = elseloc + elseloc[-1];

	DEBUG(MSG_SRCLOC,
	      Sdprintf("else = %d, end = %d\n",
		       elseloc - clause->codes, endloc - clause->codes));

	if ( loc <= endloc )
	{ add_1_if_not_at_end(endloc, end, tail PASS_LD);

	  if ( loc <= elseloc )		/* a->b */
	  { Code cutloc = find_code1(nextpc, cut, PC[1]);

	    DEBUG(MSG_SRCLOC, Sdprintf("cut at %d\n", cutloc - clause->codes));
	    add_node(tail, 1 PASS_LD);

	    if ( loc <= cutloc )	/* a */
	    { add_node(tail, 1 PASS_LD);
	      end = cutloc;
	      PC = nextpc;
	    } else			/* b */
	    { add_node(tail, 2 PASS_LD);
	      PC = cutloc + 2;
	      end = elseloc-2;
	    }
	    DEBUG(MSG_SRCLOC, Sdprintf("end = %d\n", end - clause->codes));
	    continue;
	  }
					/* c */
	  add_node(tail, 2 PASS_LD);
	  PC = elseloc;
	  end = endloc;
	  continue;
	}

	goto after_construct;
      }
      case C_IFTHEN:		/* A -> B */
				/* C_IFTHEN <var> <A> C_CUT <var> <B> C_END */
      { Code cutloc = find_code1(nextpc, C_CUT, PC[1]);

	DEBUG(MSG_SRCLOC,
	      Sdprintf("C_IFTHEN: cut at %d\n", cutloc - clause->codes));
	endloc = find_if_then_end(cutloc+2, clause->codes)+1;
	PC = nextpc;

	DEBUG(MSG_SRCLOC,
	      Sdprintf("C_MARK: cut = %d, end = %d\n",
		       cutloc - clause->codes, endloc - clause->codes));

	if ( loc <= endloc )
	{ add_1_if_not_at_end(endloc, end, tail PASS_LD);

	  if ( loc <= cutloc )		/* a */
	  { add_node(tail, 1 PASS_LD);

	    PC = nextpc;
	    end = cutloc;
	  } else			/* b */
	  { add_node(tail, 2 PASS_LD);
	    PC = cutloc+2;
	    end = endloc-1;		/* point at the C_END */
	  }

	  continue;
	}

	goto after_construct;
      }
      }					/* closes the special constructs */
      case I_CONTEXT:			/* used to compile m:head :- body */
	PC = nextpc;
	add_node(tail, 2 PASS_LD);
        continue;
      default:
        if ( loc == nextpc )
	{ add_1_if_not_at_end(nextpc, end, tail PASS_LD);

	  return PL_unify_nil(tail);
	}
        if ( codeTable[op].flags & VIF_BREAK )
	  add_node(tail, 2 PASS_LD);
	PC = nextpc;
        continue;
    }
  }

  fail;					/* assert(0) */
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
'$break_pc'(+ClauseRef, -StartPC, EndPC) is nondet.

Generate (on backtracing), all  possible   break-points  of  the clause.
Works in combination with pl_clause_term_position()   to  find the place
for placing a break-point.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("$break_pc", 3, break_pc, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  Clause clause = NULL;
  size_t offset;
  Code PC, end;

  switch( CTX_CNTRL )
  { case FRG_CUTTED:
      succeed;
    case FRG_FIRST_CALL:
      offset = 0;
      break;
    case FRG_REDO:
    default:
      offset = CTX_INT;
  }

  if ( PL_get_clref(A1, &clause) != TRUE )
    fail;
  PC = clause->codes + offset;
  end = clause->codes + clause->code_size;

  while( PC < end )
  { code op = fetchop(PC);
    Code next = stepPC(PC);

    if ( (codeTable[op].flags & VIF_BREAK) )
    { if ( PL_unify_integer(A2, PC-clause->codes) &&
	   PL_unify_integer(A3, next-clause->codes) )
	ForeignRedoInt(next-clause->codes);
    }

    PC = next;
  }

  fail;
}

		 /*******************************
		 *         BREAK-POINTS		*
		 *******************************/

#define breakTable (GD->comp.breakpoints)

typedef struct
{ Clause	clause;			/* Associated clause */
  int		offset;			/* Offset of the instruction */
  code		saved_instruction;	/* The instruction saved */
} break_point, *BreakPoint;


static bool
setBreak(Clause clause, int offset)	/* offset is already verified */
{ Code PC = clause->codes + offset;
  code op = *PC;

  if ( !breakTable )
    breakTable = newHTable(16);

  if ( (codeTable[decode(op)].flags & VIF_BREAK) )
  { BreakPoint bp = allocHeapOrHalt(sizeof(break_point));

    bp->clause = clause;
    bp->offset = offset;
    bp->saved_instruction = op;

    addHTable(breakTable, PC, bp);
    *PC = encode(D_BREAK);
    set(clause, HAS_BREAKPOINTS);

    return callEventHook(PLEV_BREAK, clause, offset);
  } else
  { return not_breakable(ATOM_set, clause, offset);
  }
}


static int
clearBreak(Clause clause, int offset)
{ GET_LD
  Code PC;
  BreakPoint bp;
  Symbol s;

  PC = clause->codes + offset;
  if ( !breakTable || !(s=lookupHTable(breakTable, PC)) )
  { term_t brk;

    if ( (brk=PL_new_term_ref()) &&
	 PL_unify_term(brk,
		       PL_FUNCTOR, FUNCTOR_break2,
		         PL_POINTER, clause,
		         PL_INT, offset) )
      return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_break, brk);
    else
      return FALSE;			/* resource error */
  }

  bp = (BreakPoint)s->value;
  *PC = bp->saved_instruction;
  freeHeap(bp, sizeof(*bp));
  deleteSymbolHTable(breakTable, s);

  return callEventHook(PLEV_NOBREAK, clause, offset);
}


void
clearBreakPointsClause(Clause clause)
{ if ( breakTable )
  { PL_LOCK(L_BREAK);
    for_unlocked_table(breakTable, s,
		       { BreakPoint bp = (BreakPoint)s->value;

			 if ( bp->clause == clause )
			   clearBreak(bp->clause, bp->offset);
		       });
    PL_UNLOCK(L_BREAK);
  }

  clear(clause, HAS_BREAKPOINTS);
}


code
replacedBreak(Code PC)
{ Symbol s;
  BreakPoint bp;
  code c;

  if ( !breakTable || !(s=lookupHTable(breakTable, PC)) )
    return (code) sysError("No saved instruction for break");
  bp = (BreakPoint)s->value;
  c = bp->saved_instruction;

  return c;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
'$break_at'(+ClauseRef, +PC, +Bool) is det.

Set/clear a breakpoint at PC on ClauseRef.   Setting a break replaces an
instruction with D_BREAK.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("$break_at", 3, break_at, 0)
{ Clause clause = NULL;
  int offset, doit, rc;

  if ( (PL_get_clref(A1, &clause) != TRUE) ||
       !PL_get_bool_ex(A3, &doit) ||
       !PL_get_integer_ex(A2, &offset) )
    fail;
  if ( offset < 0 || offset >= (int)clause->code_size )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_program_counter, A2);

  PL_LOCK(L_BREAK);
  if ( doit )
    rc = setBreak(clause, offset);
  else
    rc = clearBreak(clause, offset);
  PL_UNLOCK(L_BREAK);

  return rc;
}


static
PRED_IMPL("$current_break", 2, current_break, PL_FA_NONDETERMINISTIC)
{ GET_LD
  TableEnum e = NULL;			/* make gcc happy */
  Symbol symb;

  if ( !breakTable )
    fail;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
      e = newTableEnum(breakTable);
      break;
    case FRG_REDO:
      e = CTX_PTR;
      break;
    case FRG_CUTTED:
      e = CTX_PTR;
      freeTableEnum(e);
      succeed;
  }

  while( (symb = advanceTableEnum(e)) )
  { BreakPoint bp = (BreakPoint) symb->value;

    { fid_t cid;

      if ( !(cid=PL_open_foreign_frame()) )
      { freeTableEnum(e);
	return FALSE;
      }

      if ( PL_unify_clref(A1, bp->clause) &&
	   PL_unify_integer(A2, bp->offset) )
      { ForeignRedoPtr(e);
      }

      PL_discard_foreign_frame(cid);
    }
  }

  freeTableEnum(e);
  fail;
}

#endif /*O_DEBUGGER*/

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

#define META PL_FA_TRANSPARENT
#define NDET PL_FA_NONDETERMINISTIC

BeginPredDefs(comp)
  PRED_DEF("$record_clause", 3, record_clause, 0)
  PRED_DEF("$record_clause", 4, record_clause, 0)
  PRED_DEF("$start_aux", 2, start_aux, 0)
  PRED_DEF("$end_aux", 2, end_aux, 0)
  PRED_DEF("assert",  1, assertz1, META)
  PRED_DEF("assertz", 1, assertz1, META|PL_FA_ISO)
  PRED_DEF("asserta", 1, asserta1, META|PL_FA_ISO)
  PRED_DEF("assert",  2, assertz2, META)
  PRED_DEF("assertz", 2, assertz2, META)
  PRED_DEF("asserta", 2, asserta2, META)
  PRED_DEF("redefine_system_predicate", 1, redefine_system_predicate, META)
  PRED_DEF("compile_predicates",  1, compile_predicates, META)
  PRED_DEF("$predefine_foreign",  1, predefine_foreign, PL_FA_TRANSPARENT)
  PRED_SHARE("clause",  2, clause, META|NDET|PL_FA_CREF|PL_FA_ISO)
  PRED_SHARE("clause",  3, clause, META|NDET|PL_FA_CREF)
  PRED_SHARE("$clause", 4, clause, META|NDET|PL_FA_CREF)
#ifdef O_DEBUGGER
  PRED_DEF("$fetch_vm", 4, fetch_vm, META)
  PRED_DEF("$vm_assert", 3, vm_assert, META)
  PRED_DEF("$break_pc", 3, break_pc, NDET)
  PRED_DEF("$clause_term_position", 3, clause_term_position, 0)
  PRED_DEF("$break_at", 3, break_at, 0)
  PRED_DEF("$current_break", 2, current_break, NDET)
  PRED_DEF("$xr_member", 2, xr_member, NDET)
#endif /*O_DEBUGGER*/
EndPredDefs
