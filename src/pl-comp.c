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

/*#define O_DEBUG 1*/
#include "pl-incl.h"

#undef LD			/* Get at most once per function */
#define LD LOCAL_LD

#define setHandle(h, w)		(*valTermRef(h) = (w))
#define valHandleP(h)		valTermRef(h)

#define CODE(c, n, a, e)	{ n, c, a, e }

const code_info codeTable[] = {
/*     ID		name	     #args #argtype */
  CODE(I_NOP,		"i_nop",	0, 0),
  CODE(I_ENTER,		"i_enter",	0, 0),
  CODE(I_CALL,		"i_call",	1, CA1_PROC),
  CODE(I_DEPART,	"i_depart",	1, CA1_PROC),
  CODE(I_EXIT,		"i_exit",	0, 0),
  CODE(B_FUNCTOR,	"b_functor",	1, CA1_FUNC),
  CODE(B_RFUNCTOR,	"b_rfunctor",	1, CA1_FUNC),
  CODE(H_FUNCTOR,	"h_functor",	1, CA1_FUNC),
  CODE(H_RFUNCTOR,	"h_rfunctor",	1, CA1_FUNC),
  CODE(I_POPF,		"i_pop",	0, 0),
  CODE(B_VAR,		"b_var",	1, CA1_VAR),
  CODE(H_VAR,		"h_var",	1, CA1_VAR),
  CODE(B_CONST,		"b_const",	1, CA1_DATA),
  CODE(H_CONST,		"h_const",	1, CA1_DATA),
  CODE(H_INDIRECT,	"h_indirect",	0, CA1_STRING),
  CODE(B_INTEGER,	"b_integer",	1, CA1_INTEGER),
  CODE(H_INTEGER,	"h_integer",	1, CA1_INTEGER),
  CODE(B_FLOAT,		"b_float",	WORDS_PER_DOUBLE, CA1_FLOAT),
  CODE(H_FLOAT,		"h_float",	WORDS_PER_DOUBLE, CA1_FLOAT),
  CODE(B_FIRSTVAR,	"b_firstvar",	1, CA1_VAR),
  CODE(H_FIRSTVAR,	"h_firstvar",	1, CA1_VAR),
  CODE(B_VOID,		"b_void",	0, 0),
  CODE(H_VOID,		"h_void",	0, 0),
  CODE(B_ARGFIRSTVAR,	"b_argfirstvar",1, CA1_VAR),
  CODE(B_ARGVAR,	"b_argvar",	1, CA1_VAR),
  CODE(H_NIL,		"h_nil",	0, 0),
  CODE(B_NIL,		"b_nil",	0, 0),
  CODE(H_LIST,		"h_list",	0, 0),
  CODE(H_RLIST,		"h_rlist",	0, 0),
  CODE(B_LIST,		"b_list",	0, 0),
  CODE(B_RLIST,		"h_rlist",	0, 0),
  CODE(B_VAR0,		"b_var0",	0, 0),
  CODE(B_VAR1,		"b_var1",	0, 0),
  CODE(B_VAR2,		"b_var2",	0, 0),
  CODE(I_USERCALL0,	"i_usercall0",	0, 0),
  CODE(I_USERCALLN,	"i_usercalln",	1, 0),
  CODE(I_CUT,		"i_cut",	0, 0),
  CODE(I_APPLY,		"i_apply",	0, 0),
  CODE(A_ENTER,		"a_enter",	0, 0),
  CODE(A_INTEGER,	"a_integer",	1, CA1_INTEGER),
  CODE(A_DOUBLE,	"a_double",	WORDS_PER_DOUBLE, CA1_FLOAT),
  CODE(A_VAR0,		"a_var0",	0, 0),
  CODE(A_VAR1,		"a_var1",	0, 0),
  CODE(A_VAR2,		"a_var2",	0, 0),
  CODE(A_VAR,		"a_var",	1, 0),
  CODE(A_FUNC0,		"a_func0",	1, 0),
  CODE(A_FUNC1,		"a_func1",	1, 0),
  CODE(A_FUNC2,		"a_func2",	1, 0),
  CODE(A_FUNC,		"a_func",	2, 0),
  CODE(A_LT,		"a_lt",		0, 0),
  CODE(A_GT,		"a_gt",		0, 0),
  CODE(A_LE,		"a_le",		0, 0),
  CODE(A_GE,		"a_ge",		0, 0),
  CODE(A_EQ,		"a_eq",		0, 0),
  CODE(A_NE,		"a_ne",		0, 0),
  CODE(A_IS,		"a_is",		0, 0),
  CODE(C_OR,		"c_or",		1, 0),
  CODE(C_JMP,		"c_jmp",	1, 0),
  CODE(C_MARK,		"c_mark",	1, CA1_VAR),
  CODE(C_CUT,		"c_cut",	1, CA1_VAR),
  CODE(C_IFTHENELSE,	"c_ifthenelse",	2, CA1_VAR),
  CODE(C_VAR,		"c_var",	1, CA1_VAR),
  CODE(C_END,		"c_end",	0, 0),
  CODE(C_NOT,		"c_not",	2, CA1_VAR),
  CODE(C_FAIL,		"c_fail",	0, 0),
  CODE(B_INDIRECT,	"b_indirect",	0, CA1_STRING),
#if O_BLOCK
  CODE(I_CUT_BLOCK,	"i_cut_block",	0, 0),
  CODE(B_EXIT,		"b_exit",	0, 0),
#endif
#if O_INLINE_FOREIGNS
  CODE(I_CALL_FV0,	"i_call_fv0",	1, CA1_PROC),
  CODE(I_CALL_FV1,	"i_call_fv1",	2, CA1_PROC), /* , var */
  CODE(I_CALL_FV2,	"i_call_fv2",	3, CA1_PROC), /* , var, var */
#endif
  CODE(I_FAIL,		"i_fail",	0, 0),
  CODE(I_TRUE,		"i_true",	0, 0),
#ifdef O_SOFTCUT
  CODE(C_SOFTIF,	"c_softif",	2, CA1_VAR),
  CODE(C_SOFTCUT,	"c_softcut",	1, CA1_VAR),
#endif
  CODE(I_EXITFACT,	"i_exitfact",	0, 0),
  CODE(D_BREAK,		"d_break",	0, 0),
#if O_CATCHTHROW
  CODE(I_CATCH,		"i_catch",      0, 0),
  CODE(B_THROW,		"b_throw",	0, 0),
#endif
  CODE(I_CONTEXT,	"i_context",	1, CA1_MODULE),
  CODE(C_LCUT,		"c_lcut",	1, CA1_VAR),
  CODE(I_CALLCLEANUP,	"i_callcleanup",0, 0),
  CODE(I_EXITCLEANUP,	"i_exitcleanup",0, 0),
/*List terminator */
  CODE(0,		NULL,		0, 0)
};

forwards void	checkCodeTable(void);

static void
checkCodeTable(void)
{ const code_info *ci;
  unsigned int n;

  for(ci = codeTable, n = 0; ci->name != NULL; ci++, n++ )
  { if ( ci->code != n )
      sysError("Wrong entry in codeTable: %d", n);
  }

  if ( --n != I_HIGHEST )
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
	if ( r1 > I_HIGHEST ) goto default;
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
All this required where a few pacthes in interpret(), the compiler and
the wic (intermediate code)  generation  code.  The initialisation  is
very critical:

The function interpret() (the VM interpreter)  declares a static array
holding  the label  addresses      of the  various  virtual    machine
instructions.  When it is  called,  it will  store the address of this
table in  the  global  variable  interpreter_jmp_table.   the function
initWamTable() than makes the two  translation tables wam_table[] (wam
code --> label address and dewam_table[] (label address --> wam code).
Note that initWamTable() calles prolog() and thus interpret to get the
table with  the label addresses  out of interpret().   It does so with
the  C-defined  predicate fail/0 (because   it  cannot  yet run prolog
predicates).

BUGS:	Currently there are three  places were all the VM instructions
	are  defined: pl-incl.h;  above and   pl-wam.c.  One day  this
	should  be merged.  For  now, be very carefull  if you add  or
	delete a VM instruction.

NOTE:	If the assert() fails, look at pl-wam.c: VMI(C_NOT, ... for
	more information.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if VMCODE_IS_ADDRESS
void
initWamTable(void)
{ GET_LD
  int n;
  code maxcoded, mincoded;

  if ( interpreter_jmp_table == NULL )
    PL_next_solution(QID_EXPORT_WAM_TABLE);

  wam_table[0] = (code) (interpreter_jmp_table[0]);
  maxcoded = mincoded = wam_table[0];

  for(n = 1; n <= I_HIGHEST; n++)
  { wam_table[n] = (code) (interpreter_jmp_table[n]);
    if ( wam_table[n] > maxcoded )
      maxcoded = wam_table[n];
    if ( wam_table[n] < mincoded )
      mincoded = wam_table[n];
  }
  dewam_table_offset = mincoded;

  assert(wam_table[C_NOT] != wam_table[C_IFTHENELSE]);
  dewam_table = (char *)allocHeap(((maxcoded-dewam_table_offset) + 1) *
				  sizeof(char));
  
  for(n = 0; n <= I_HIGHEST; n++)
    dewam_table[wam_table[n]-dewam_table_offset] = (char) n;

  checkCodeTable();
}

#else /* VMCODE_IS_ADDRESS */

void
initWamTable()
{ checkCodeTable();
}

#endif /* VMCODE_IS_ADDRESS */

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

typedef struct _varDef
{ word		functor;		/* mimic a functor (FUNCTOR_var1) */
  Word		address;		/* address of the variable */
  int		times;			/* occurences */
  int		offset;			/* offset in environment frame */
} vardef;

typedef struct
{ int	isize;
  int	entry[1];
} var_table, *VarTable;

#undef struct_offsetp
#define struct_offsetp(t, f) ((int)((t*)0)->f)
#define sizeofVarTable(isize) (struct_offsetp(var_table, entry) + sizeof(int)*(isize))

#define mkCopiedVarTable(o) copyVarTable(alloca(sizeofVarTable(o->isize)), o)
#define BITSPERINT (sizeof(int)*8)


typedef struct
{ Module	module;			/* module to compile into */
  int		arity;			/* arity of top-goal */
  Clause	clause;			/* clause we are constructing */
  int		vartablesize;		/* size of the vartable */
  int		cutvar;			/* Variable for local cuts */
  int		islocal;		/* Temporary local clause */
  int		subclausearg;		/* processing subclausearg */
  int		argvars;		/* islocal argument pseudo vars */
  int		argvar;			/* islocal current pseudo var */
  tmp_buffer	codes;			/* scratch code table */
  VarTable	used_var;		/* boolean array of used variables */
} compileInfo, *CompileInfo;


static VarDef
getVarDef(int i ARG_LD)
{ VarDef vd;
  VarDef *vardefs = LD->comp.vardefs;
  int nvd = LD->comp.nvardefs;

  if ( i >= nvd )
  { VarDef *vdp;
    int onvd = nvd, n;

    if ( nvd > 0 )
    { nvd *= 2;
      vardefs = realloc(vardefs, sizeof(VarDef) * nvd);
    } else
    { nvd = 32;
      vardefs = malloc(sizeof(VarDef) * nvd);
    }
    if ( !vardefs )
      outOfCore();

    for(vdp = &vardefs[onvd], n=onvd; n++ < nvd; )
      *vdp++ = NULL;

    LD->comp.nvardefs = nvd;
    LD->comp.vardefs = vardefs;
  }

  if ( !(vd = vardefs[i]) )
  { vd = vardefs[i] = allocHeap(sizeof(vardef));
    memset(vd, 0, sizeof(*vd));
    vd->functor = FUNCTOR_var1;
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
    { *vd = v = allocHeap(sizeof(vardef));
      memset(v, 0, sizeof(vardef));
      v->functor = FUNCTOR_var1;
    }
  }
}


void
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
    PL_strip_module(head, m, head);
  } else
  { PL_put_term(head, clause);		/* facts */
    PL_put_atom(body, ATOM_true);
  }
  
  DEBUG(9, pl_write(clause); Sdprintf(" --->\n\t");
	   Sdprintf("%s:", stringAtom(m0->name));
	   pl_write(head); Sdprintf(" :- "); pl_write(body); Sdprintf("\n"));
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
analyseVariables2(Word head, int nvars, int arity, int argn,
		  CompileInfo ci ARG_LD)
{
right_recursion:

  deRef(head);

  if ( isVar(*head) )
  { VarDef vd;
    int index = ((argn >= 0 && argn < arity) ? argn : (arity + nvars++));

    vd = getVarDef(index PASS_LD);
    vd->address = head;
    vd->times = 1;
    *head = (index<<LMASK_BITS)|TAG_ATOM|STG_GLOBAL; /* special mark */

    return nvars;
  }

  if ( tagex(*head) == (TAG_ATOM|STG_GLOBAL) )
  { VarDef vd = LD->comp.vardefs[(*head) >> LMASK_BITS];

    vd->times++;
    return nvars;
  }

  if ( isTerm(*head) )
  { Functor f = valueTerm(*head);
    FunctorDef fd = valueFunctor(f->definition);

    if ( ci->islocal )
    { if ( ci->subclausearg )
      { ci->argvars++;

	return nvars;
      } else if ( false(fd, CONTROL_F) )
      { if ( f->definition == FUNCTOR_colon2 )	/* A:B --> I_USERCALL0 */
	{ ci->argvars++;
	} else
	{ int ar = fd->arity;

	  ci->subclausearg++;
	  for(head = f->arguments, argn = arity; --ar >= 0; head++, argn++)
	    nvars = analyseVariables2(head, nvars, arity, argn, ci PASS_LD);
	  ci->subclausearg--;
	}

	return nvars;
      } /* else fall through to normal case */
    }

    { int ar = fd->arity;

      head = f->arguments;
      argn = ( argn < 0 ? 0 : arity );

      for(; --ar > 0; head++, argn++)
	nvars = analyseVariables2(head, nvars, arity, argn, ci PASS_LD);

      goto right_recursion;
    }
  }

  if ( ci->subclausearg && isString(*head) )
    ci->argvars++;

  return nvars;
}


static void
analyse_variables(Word head, Word body, CompileInfo ci ARG_LD)
{ int nvars = 0;
  int n;
  int body_voids = 0;
  int arity = ci->arity;

  if ( arity > 0 )
    resetVarDefs(arity PASS_LD);

  if ( head )
    nvars = analyseVariables2(head, 0, arity, -1, ci PASS_LD);
  if ( body )
    nvars = analyseVariables2(body, nvars, arity, arity, ci PASS_LD);

  for(n=0; n<arity+nvars; n++)
  { VarDef vd = LD->comp.vardefs[n];

    assert(vd->functor == FUNCTOR_var1);
    if ( !vd->address )
      continue;
    if ( vd->times == 1 && !ci->islocal ) /* ISVOID */
    { setVar(*(vd->address));
      vd->address = (Word) NULL;
      if (n >= arity)
	body_voids++;
    } else
      vd->offset = n + ci->argvars - body_voids;
  }

  LD->comp.filledVars = arity + nvars;

  ci->clause->prolog_vars = nvars + arity + ci->argvars - body_voids;
  ci->clause->variables   = ci->clause->prolog_vars;
  ci->vartablesize = (ci->clause->prolog_vars + BITSPERINT-1)/BITSPERINT;
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

#define A_HEAD	0x01			/* argument in head */
#define A_BODY  0x02			/* argument in body */
#define A_ARG	0x04			/* sub-argument */
#define A_RIGHT	0x08			/* rightmost argument */

#define ISVOID 0			/* compileArgument produced H_VOID */
#define NONVOID 1			/* ... anything else */
#define NOT_CALLABLE -1			/* return value for not-callable */

#define BLOCK(s) do { s; } while (0)

#define Output_0(ci, c)		addBuffer(&(ci)->codes, encode(c), code)
#define Output_a(ci, c)		addBuffer(&(ci)->codes, c, code)
#define Output_1(ci, c, a)	BLOCK(Output_0(ci, c); Output_a(ci, a))
#define Output_2(ci, c, a0, a1)	BLOCK(Output_1(ci, c, a0); Output_a(ci, a1))
#define Output_n(ci, p, n)	addMultipleBuffer(&(ci)->codes, p, n, word)

#define PC(ci)		entriesBuffer(&(ci)->codes, code)
#define OpCode(ci, pc)	(baseBuffer(&(ci)->codes, code)[pc])

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable table operations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

forwards int	compileBody(Word, code, compileInfo * ARG_LD);
forwards int	compileArgument(Word, int, compileInfo * ARG_LD);
forwards int	compileSubClause(Word, code, compileInfo *);
forwards bool	isFirstVar(VarTable vt, int n);
forwards void	balanceVars(VarTable, VarTable, compileInfo *);
forwards void	orVars(VarTable, VarTable);
forwards void	setVars(Word t, VarTable ARG_LD);
#if O_COMPILE_ARITH
forwards int	compileArith(Word, compileInfo * ARG_LD);
forwards bool	compileArithArgument(Word, compileInfo * ARG_LD);
#endif

static inline int
isIndexedVarTerm(word w ARG_LD)
{ if ( tagex(w) == (TAG_ATOM|STG_GLOBAL) )
  { VarDef v = LD->comp.vardefs[w>>LMASK_BITS];
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
isFirstVar(VarTable vt, int n)
{ int m  = 1 << (n % BITSPERINT);
  int *p = &vt->entry[n / BITSPERINT];
  
  if ( (*p & m) )
    return FALSE;
  *p |= m;
  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Emit  C_VAR  statements  for  all  variables  that  are  claimed  to  be
uninitialised in valt1 and initialised in valt2.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
balanceVars(VarTable valt1, VarTable valt2, compileInfo *ci)
{ int *p1 = &valt1->entry[0];
  int *p2 = &valt2->entry[0];
  int vts = ci->vartablesize;
  register int n;

  for( n = 0; n < vts; p1++, p2++, n++ )
  { register int m = (~(*p1) & *p2);

    if ( m )
    { register int i;

      for(i = 0; i < BITSPERINT; i++)
	if ( m & (1 << i) )
	  Output_1(ci, C_VAR, VAROFFSET(n * BITSPERINT + i));
    }
  }
}

static void
orVars(VarTable valt1, VarTable valt2)
{ register int *p1 = &valt1->entry[0];
  register int *p2 = &valt2->entry[0];
  register int n;

  for( n = 0; n < valt1->isize; n++ )
    *p1++ |= *p2++;
}

static void
setVars(register Word t, VarTable vt ARG_LD)
{ int index;

  deRef(t);
  if ( (index = isIndexedVarTerm(*t PASS_LD)) >= 0 )
  { isFirstVar(vt, index);
    return;
  }

  if ( isTerm(*t) )
  { int arity;

    arity = arityTerm(*t);
    for(t = argTermP(*t, 0); arity > 0; t++, arity--)
      setVars(t, vt PASS_LD);
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
      setVar(*(vd->address));
  }
}


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
        FR->predicate	proc->definition

Using `local' compilation, all variables are   shared  with the original
goal-term and therefore initialised. This implies   there  is no need to
play around with variable tables.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Clause
compileClause(Word head, Word body, Procedure proc, Module module ARG_LD)
{ compileInfo ci;			/* data base for the compiler */
  struct clause clause;
  Clause cl;

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

  ci.clause = &clause;
  ci.module = module;

  analyse_variables(head, body, &ci PASS_LD);
  ci.cutvar = 0;
  if ( !ci.islocal )
  { ci.used_var = alloca(sizeofVarTable(ci.vartablesize));
    clearVarTable(&ci);
  } else
    ci.used_var = NULL;

  initBuffer(&ci.codes);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
First compile  the  head  of  the  term.   The  arguments  are  compiled
left-to-right. `lastnonvoid' is maintained to delete void variables just
before the I_ENTER instructions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( head )
  { int n;
    int lastnonvoid = 0;
    Word arg;

    for ( arg = argTermP(*head, 0), n = 0; n < ci.arity; n++, arg++ )
    { if ( compileArgument(arg, A_HEAD, &ci PASS_LD) == NONVOID )
	lastnonvoid = PC(&ci);
    }
    seekBuffer(&ci.codes, lastnonvoid, code);
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Now compile the body. After the  I_ENTER,   we  check whether we need to
insert an I_CONTEXT instruction to change the context. This is the case,
for predicates for which the body is  defined from another module as the
head and the predicate is not a   meta-predicate. In principle, we could
delay this until we decided there may be a meta-call, but this will harm
automatic update if a predicate is later defined as meta-predicate.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( body && *body != ATOM_true )
  { int rv;

    if ( head )
    { Output_0(&ci, I_ENTER);
      if ( ci.module != proc->definition->module &&
	   false(proc->definition, METAPRED) )
      { Output_1(&ci, I_CONTEXT, (code)ci.module);
      }
    }

    if ( (rv=compileBody(body, I_DEPART, &ci PASS_LD)) != TRUE )
    { if ( rv == NOT_CALLABLE )
	PL_error(NULL, 0, NULL, ERR_TYPE,
		 ATOM_callable, wordToTermRef(body));
      
      goto exit_fail;
    }
    Output_0(&ci, I_EXIT);
  } else
  { set(&clause, UNIT_CLAUSE);		/* fact (for decompiler) */
    Output_0(&ci, I_EXITFACT);
  }

  resetVars(PASS_LD1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Finish up the clause.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
#ifdef O_SHIFT_STACKS
  clause.marks = clause.variables - clause.prolog_vars;
#endif

  clause.code_size = entriesBuffer(&ci.codes, code);

  if ( head )
  { int size  = sizeofClause(clause.code_size);

    cl = allocHeap(size);
    memcpy(cl, &clause, sizeofClause(0));

    GD->statistics.codes += clause.code_size;
  } else
  { LocalFrame fr = lTop;
    Word p0 = argFrameP(fr, clause.variables);
    Word p = p0;
    ClauseRef cref;

    DEBUG(1, Sdprintf("%d argvars; %d prolog vars; %d vars",
		      ci.argvars, clause->prolog_vars, clause->variables));
    assert(ci.argvars == ci.argvar);
    requireStack(local,
		 clause.variables*sizeof(word) +
		 sizeofClause(clause.code_size) +
		 sizeof(*cref));

    cref = (ClauseRef)p;
    p = addPointer(p, sizeof(*cref));
    cref->next = NULL;
    cref->clause = cl = (Clause)p;
    memcpy(cl, &clause, sizeofClause(0));
    p = addPointer(p, sizeofClause(clause.code_size));
    cl->variables += p-p0;

    fr->clause = cref;
    fr->context = module;
    fr->predicate = proc->definition;

    DEBUG(1, Sdprintf("; now %d vars\n", clause->variables));
    lTop = (LocalFrame)p;
  }

  memcpy(cl->codes, baseBuffer(&ci.codes, code), sizeOfBuffer(&ci.codes));
  discardBuffer(&ci.codes);

  DEBUG(1,
	if ( !head )
	{ wamListClause(cl);
	});

  return cl;

exit_fail:
  resetVars(PASS_LD1);
  return NULL;
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
    
	C_MARK var	Mark for `soft-cut'
	C_CUT  var	Cut alternatives generated since C_MARK var

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
    references to terms above the (now unwount) global stack, but the
    GC clearUninitialisedVarsFrame() won't see any initialisation of
    these variables, leaving them invalid. Same holds for the
    source-level debugger using the same function.  Therefore we
    explicitely reset these variables at the end of the code created
    by \+. For optimisation-reasons however we can consider them
    uninitialised.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
compileBody(Word body, code call, compileInfo *ci ARG_LD)
{ deRef(body);

  if ( isTerm(*body) )
  { functor_t fd = functorTerm(*body);
    FunctorDef fdef = valueFunctor(fd);

    if ( true(fdef, CONTROL_F) )
    { if ( fd == FUNCTOR_comma2 )			/* A , B */
      { int rv;

	if ( (rv=compileBody(argTermP(*body, 0), I_CALL, ci PASS_LD)) != TRUE )
	  return rv;
	return compileBody(argTermP(*body, 1), call, ci PASS_LD);
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
	{ int var = VAROFFSET(ci->clause->variables++);
	  int tc_or, tc_jmp;
	  int rv;
	  int cutsave = ci->cutvar;

	  Output_2(ci, hard ? C_IFTHENELSE : C_SOFTIF, var, (code)0);
	  tc_or = PC(ci);
	  ci->cutvar = var;		/* Cut locally in the condition */
	  if ( (rv=compileBody(argTermP(*a0, 0), I_CALL, ci PASS_LD)) != TRUE )
	    return rv;
	  ci->cutvar = cutsave;
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
	{ int tc_or, tc_jmp;
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
      } else if ( fd == FUNCTOR_ifthen2 )		/* A -> B */
      { int var = VAROFFSET(ci->clause->variables++);
	int rv;
	int cutsave = ci->cutvar;

	Output_1(ci, C_MARK, var);
	ci->cutvar = var;		/* Cut locally in the condition */
	if ( (rv=compileBody(argTermP(*body, 0), I_CALL, ci PASS_LD)) != TRUE )
	  return rv;
	ci->cutvar = cutsave;
	Output_1(ci, C_CUT, var);
	if ( (rv=compileBody(argTermP(*body, 1), call, ci PASS_LD)) != TRUE )
	  return rv;
	Output_0(ci, C_END);
      
	succeed;
      } else if ( fd == FUNCTOR_softcut2 ) 		/* A *-> B */
      { int rv;						/* compile as A,B */

	if ( (rv=compileBody(argTermP(*body, 0), I_CALL, ci PASS_LD)) != TRUE )
	  return rv;
	return compileBody(argTermP(*body, 1), call, ci PASS_LD);
      } else if ( fd == FUNCTOR_not_provable1 )		/* \+/1 */
      { int var = VAROFFSET(ci->clause->variables++);
	int tc_or;
	VarTable vsave;
	int rv;
	int cutsave = ci->cutvar;

	if ( !ci->islocal )
	  vsave = mkCopiedVarTable(ci->used_var);
	else
	  vsave = NULL;

	Output_2(ci, C_NOT, var, (code)0);
	tc_or = PC(ci);
	ci->cutvar = var;
	if ( (rv=compileBody(argTermP(*body, 0), I_CALL, ci PASS_LD)) != TRUE )
	  return rv;
	ci->cutvar = cutsave;
	Output_1(ci, C_CUT, var);
	Output_0(ci, C_FAIL);
	OpCode(ci, tc_or-1) = (code)(PC(ci) - tc_or);
	if ( !ci->islocal )
	{ balanceVars(vsave, ci->used_var, ci); /* see comment above */
	  copyVarTable(ci->used_var, vsave);
	}
      
	succeed;
#endif /* O_COMPILE_OR */
      }
      assert(0);
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

compileArgument() returns ISVOID if a void instruction resulted from the
compilation.  This is used to detect  the  ...ISVOID,  [I_ENTER,  I_POPF]
sequences,  in  which  case  we  can leave out the VOIDS just before the
I_ENTER or I_POPF instructions.

When doing `islocal' compilation,  compound  terms   are  copied  to the
current localframe and a B_VAR instruction is generated for it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_DEBUG
static char *
vName(Word adr)
{ static char name[32];

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
A void.  Generate either B_VOID or H_VOID.  Note that the  return  value
ISVOID  is reserved for head variables only (B_VOID sets the location to
be a variable, and thus cannot be removed if it is before an I_POPF.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  switch(tag(*arg))
  { case TAG_VAR:
      if (where & A_BODY)
      { Output_0(ci, B_VOID);
	return NONVOID;
      }
      Output_0(ci, H_VOID);
      return ISVOID;
    case TAG_INTEGER:
      if ( storage(*arg) != STG_INLINE )
      {	Output_1(ci, (where&A_HEAD) ? H_INTEGER : B_INTEGER, valBignum(*arg));
	return NONVOID;
      }
      Output_1(ci, (where & A_BODY) ? B_CONST : H_CONST, *arg);
      return NONVOID;
    case TAG_ATOM:
      if ( tagex(*arg) == (TAG_ATOM|STG_GLOBAL) )
	goto isvar;
      if ( isNil(*arg) )
      {	Output_0(ci, (where & A_BODY) ? B_NIL : H_NIL);
      } else
      { if ( !ci->islocal )
	  PL_register_atom(*arg);
	Output_1(ci, (where & A_BODY) ? B_CONST : H_CONST, *arg);
      }
      return NONVOID;
    case TAG_FLOAT:
    { Word p = valIndirectP(*arg);
      Output_0(ci, (where & A_BODY) ? B_FLOAT : H_FLOAT);
      Output_n(ci, p, WORDS_PER_DOUBLE);
      return NONVOID;
    }
    case TAG_STRING:
    if ( ci->islocal )
    { goto argvar;
    } else
    { Word p = addressIndirect(*arg);

      int n  = wsizeofInd(*p);
      Output_0(ci, (where & A_HEAD) ? H_INDIRECT : B_INDIRECT);
      Output_n(ci, p, n+1);
      return NONVOID;
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

      requireStack(local, (voffset+1)*sizeof(word));
      *k = makeRef(v->address);

      if ( index < 3 )
      { Output_0(ci, B_VAR0 + index);
      } else
      { Output_1(ci, B_VAR, voffset);
      }

      return NONVOID;
    }

    first = isFirstVar(ci->used_var, index);

    if ( index < ci->arity )		/* variable on its own in the head */
    { if ( where & A_BODY )
      { if ( where & A_ARG )
	{ Output_0(ci, B_ARGVAR);
	} else
	{ if ( index < 3 )
	  { Output_0(ci, B_VAR0 + index);
	    return NONVOID;
	  }
	  Output_0(ci, B_VAR);
	}
      } else				/* head */
      { if ( !(where & A_ARG) && first )
	{ Output_0(ci, H_VOID);
	  return ISVOID;
	}
	Output_0(ci, H_VAR);
      }
      Output_a(ci, VAROFFSET(index));

      return NONVOID;
    }

    /* normal variable (i.e. not shared in the head and non-void) */
    if( where & A_BODY )
    { if ( where & A_ARG )
      { Output_0(ci, first ? B_ARGFIRSTVAR : B_ARGVAR);
      } else
      { if ( index < 3 && !first )
	{ Output_0(ci, B_VAR0 + index);
	  return NONVOID;
	}
	Output_0(ci, first ? B_FIRSTVAR : B_VAR);
      }
    } else
    { Output_0(ci, first ? H_FIRSTVAR : H_VAR);
    }

    Output_a(ci, VAROFFSET(index));

    return NONVOID;
  }

  assert(isTerm(*arg));
    
  if ( ci->islocal )
  { int voffset;
    Word k;

  argvar:
    voffset = VAROFFSET(ci->argvar);
    k = varFrameP(lTop, voffset);

    requireStack(local, (voffset+1)*sizeof(word));
    *k = *arg;
    if ( ci->argvar < 3 )
    { Output_0(ci, B_VAR0 + ci->argvar);
    } else
    { Output_1(ci, B_VAR, voffset);
    }
    ci->argvar++;

    return NONVOID;
  } else
  { int ar;
    int lastnonvoid;
    functor_t fdef;
    int isright = (where & A_RIGHT);

    fdef = functorTerm(*arg);
    if ( fdef == FUNCTOR_dot2 )
    { code c;

      if ( (where & A_HEAD) )		/* index in array! */
	c = (isright ? H_RLIST : H_LIST);
      else
	c = (isright ? B_RLIST : B_LIST);

      Output_0(ci, c);
    } else
    { code c;

      if ( (where & A_HEAD) )		/* index in array! */
	c = (isright ? H_RFUNCTOR : H_FUNCTOR);
      else
	c = (isright ? B_RFUNCTOR : B_FUNCTOR);

      Output_1(ci, c, (word)fdef);
    }
    lastnonvoid = PC(ci);
    ar = arityFunctor(fdef);
    where &= ~A_RIGHT;
    where |= A_ARG;

    for(arg = argTermP(*arg, 0); --ar > 0; arg++)
    { if ( compileArgument(arg, where, ci PASS_LD) == NONVOID )
	lastnonvoid = PC(ci);
    }

    where |= A_RIGHT;
    deRef(arg);

    if ( tag(*arg) == TAG_VAR && !(where & A_BODY) )
    { seekBuffer(&ci->codes, lastnonvoid, code);
      if ( !isright )
	Output_0(ci, I_POPF);
      return NONVOID;
    }

    if ( isright )
      goto right_recursion;
    else
    { compileArgument(arg, where, ci PASS_LD);
      Output_0(ci, I_POPF);
    }

    return NONVOID;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The task of compileSubClause() is to  generate  code  for  a  subclause.
First  it will call compileArgument for each argument to the call.  Then
an instruction to call the procedure is added.  Before doing all this it
will check for the subclause just beeing a variable or the cut.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
compileSubClause(Word arg, code call, compileInfo *ci)
{ GET_LD
  Module tm = ci->module;

  deRef(arg);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A non-void variable. Create a I_USERCALL0 instruction for it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  if ( isIndexedVarTerm(*arg PASS_LD) >= 0 )
  { compileArgument(arg, A_BODY, ci PASS_LD);
    Output_0(ci, I_USERCALL0);
    succeed;
  }

  if ( isTerm(*arg) )
  { functor_t functor = functorTerm(*arg);
    FunctorDef fdef = valueFunctor(functor);
      
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If the argument is of the form <Module>:<Goal>, <Module> is an atom  and
<Goal>  is  nonvar  then compile to the specified module.  Otherwise use
the meta-call mechanism (BUG: `user:hello:foo' is called  via  meta-call
mechanism, but this only is a bit slower).

This is a bit more complex then expected: foo:assert(baz) should  assert
baz/0  into module foo.  In general: the context module should be set to
the appropriate value.  This needs a  new  virtual  machine  instruction
that  handles  calls  with  specified context module.  For the moment we
will use the meta-call mechanism for all these types of calls.

[Tue Dec 18 2001] Now we do have I_CONTEXT.  Time to reconsider!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    if ( functor == FUNCTOR_colon2 )
    {
  /*							SEE COMMENT ABOVE
      Word mp, g;

      mp = argTermP(*arg, 0); deRef(mp);
      if ( isAtom(*mp) )
      { g = argTermP(*arg, 1); deRef(g);
	if ( isIndexedVarTerm(*g PASS_LD) < 0 )
	{ arg = g;
	  tm = lookupModule(*mp);
	  goto cont;
	}
      }
  */

      compileArgument(arg, A_BODY, ci PASS_LD);
      Output_0(ci, I_USERCALL0);
      succeed;
    }
/*  cont: */

#if O_COMPILE_ARITH
    if ( true(fdef, ARITH_F) &&
	 trueFeature(OPTIMISE_FEATURE) &&
	 !ci->islocal )			/* no use for local compilation */
      return compileArith(arg, ci PASS_LD);
#endif /* O_COMPILE_ARITH */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Term, not a variable and not a module   call. First of all, we check for
`inline calls'. This refers to a  light-weight calling mechanism applied
to deterministic foreign predicates that are   called with simple normal
variable arguments only. This deals with fast  calling of checks such as
var/1 as well as  A  =  B.  If   we  are  compiling  clauses  for call/1
(islocal), we skip this, as it   complicates the compilation process and
the dynamic clause will be used  once   only  anyhow. Moreover, it would
require  one  more  place  to  do    the   special  variable-linking  in
compileArgument().

For normal cases, simply compile the arguments   (push on the stack) and
create a call-instruction. Finally, some  special   atoms  are mapped to
special instructions.

If we call a currently undefined procedure we   check it is not a system
procedure. If it is, we import the  procedure immediately to avoid later
re-definition.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    { Procedure proc = lookupProcedure(functor, tm);
      int ar = fdef->arity;

      if ( !isDefinedProcedure(proc) &&
	   !true(proc->definition, P_REDEFINED) &&
	   !GD->bootsession )
      { Procedure syspred;

	if ( (tm != MODULE_system &&
	      (syspred=isCurrentProcedure(functor, MODULE_system)) &&
	      isDefinedProcedure(syspred)) )
	{ freeHeap(proc->definition, sizeof(struct definition));
	  proc->definition = syspred->definition;
	}
      }

#ifdef O_INLINE_FOREIGNS
#define MAX_FV 2
      if ( true(fdef, INLINE_F) && ar <= MAX_FV && !ci->islocal )
      { int n;
	int vars[MAX_FV];

	for(n = 0; n < ar; n++)
	{ Word a = argTermP(*arg, n);

	  deRef(a);
	  if ( (vars[n] = isIndexedVarTerm(*a PASS_LD)) >= 0 )
	    continue;

	  goto non_fv;
	}

	for(n = 0; n < ar; n++)
	{ if ( isFirstVar(ci->used_var, vars[n]) )
	  { Output_1(ci, C_VAR, VAROFFSET(vars[n]));
	  }
	}

        Output_1(ci, I_CALL_FV0 + ar, (code)proc);
	for(n=0; n<ar; n++)
	  Output_a(ci, VAROFFSET(vars[n]));

	succeed;
      non_fv:;
      }
#endif /*O_INLINE_FOREIGNS*/

      for(arg = argTermP(*arg, 0); ar > 0; ar--, arg++)
	compileArgument(arg, A_BODY, ci PASS_LD);

      if ( fdef->name == ATOM_call && fdef->arity > 1 )
      { Output_1(ci, I_USERCALLN, (code)(fdef->arity - 1));
	succeed;
      } else if ( functor == FUNCTOR_apply2 )
      { Output_0(ci, I_APPLY);
	succeed;
#if O_BLOCK
      } else if ( functor == FUNCTOR_dcut1 )
      { Output_0(ci, I_CUT_BLOCK);
	succeed;
      } else if ( functor == FUNCTOR_dexit2 )
      { Output_0(ci, B_EXIT);
	succeed;
#endif
#if O_CATCHTHROW
      } else if ( functor == FUNCTOR_dthrow1 )
      { Output_0(ci, B_THROW);
	succeed;
#endif
      }
      Output_1(ci, call, (code) proc);

      succeed;
    }
  }

  if ( isAtom(*arg) )
  { if ( *arg == ATOM_cut )
    { if ( ci->cutvar )			/* local cut for \+ */
	Output_1(ci, C_LCUT, ci->cutvar);
      else
	Output_0(ci, I_CUT);
    } else if ( *arg == ATOM_true )
    { Output_0(ci, I_TRUE);
    } else if ( *arg == ATOM_fail )
    { Output_0(ci, I_FAIL);
    } else if ( *arg == ATOM_dcatch )	/* $catch */
    { Output_0(ci, I_CATCH);
    } else if ( *arg == ATOM_dcall_cleanup )	/* $call_cleanup */
    { Output_0(ci, I_CALLCLEANUP);
      Output_0(ci, I_EXITCLEANUP);
    } else
    { functor_t fdef = lookupFunctorDef(*arg, 0);
      code cproc = (code) lookupProcedure(fdef, tm);

#ifdef O_INLINE_FOREIGNS
      if ( true(valueFunctor(fdef), INLINE_F) )
      { Output_1(ci, I_CALL_FV0, cproc);
      } else
#endif /*O_INLINE_FOREIGNS*/
      { Output_1(ci, call, cproc);
      }
    }

    succeed;
  }
    
  return NOT_CALLABLE;
}

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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if O_COMPILE_ARITH
static bool
compileArith(Word arg, compileInfo *ci ARG_LD)
{ code a_func;
  functor_t fdef = functorTerm(*arg);

  if      ( fdef == FUNCTOR_ar_equals2 )	a_func = A_EQ;	/* =:= */
  else if ( fdef == FUNCTOR_ar_not_equal2 )	a_func = A_NE;	/* =\= */
  else if ( fdef == FUNCTOR_smaller2 )	 	a_func = A_LT;	/* < */
  else if ( fdef == FUNCTOR_larger2 )		a_func = A_GT;	/* > */
  else if ( fdef == FUNCTOR_smaller_equal2 )	a_func = A_LE;	/* =< */
  else if ( fdef == FUNCTOR_larger_equal2 )	a_func = A_GE;	/* >= */
  else if ( fdef == FUNCTOR_is2 )				/* is */
  { if ( !compileArgument(argTermP(*arg, 0), A_BODY, ci PASS_LD) )
      fail;
    Output_0(ci, A_ENTER);
    if ( !compileArithArgument(argTermP(*arg, 1), ci PASS_LD) )
      fail;
    Output_0(ci, A_IS);
    succeed;
  } else	
  { assert(0);			/* see pl-func.c, registerArithFunctors() */
    fail;
  }

  Output_0(ci, A_ENTER);
  if ( !compileArithArgument(argTermP(*arg, 0), ci PASS_LD) ||
       !compileArithArgument(argTermP(*arg, 1), ci PASS_LD) )
    fail;

  Output_0(ci, a_func);

  succeed;
}


static bool
compileArithArgument(Word arg, compileInfo *ci ARG_LD)
{ int index;

  deRef(arg);

  if ( isInteger(*arg) )
  { Output_1(ci, A_INTEGER, valInteger(*arg));
    succeed;
  }
  if ( isReal(*arg) )
  { Word p = valIndirectP(*arg);

    Output_0(ci, A_DOUBLE);
    Output_n(ci, p, WORDS_PER_DOUBLE);
    succeed;
  }
					/* variable */
  if ( (index = isIndexedVarTerm(*arg PASS_LD)) >= 0 )
  { int first = isFirstVar(ci->used_var, index);

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
	return PL_error(NULL, 0, "Unbound variable in arithmetic expression",
			ERR_TYPE, ATOM_evaluable, wordToTermRef(arg));
      }
      Output_0(ci, A_VAR);
    }          
    Output_a(ci, VAROFFSET(index));
    succeed;
  }

  if ( isVar(*arg) )			/* void variable */
    return PL_error(NULL, 0, "Unbound variable in arithmetic expression",
		    ERR_TYPE, ATOM_evaluable, wordToTermRef(arg));

  { functor_t fdef;
    int n, ar;
    Word a;

    if ( isAtom(*arg) )
    { fdef = lookupFunctorDef(*arg, 0);
      ar = 0;
      a = NULL;
    } else if ( isTerm(*arg) )
    { fdef = functorTerm(*arg);
      ar = arityFunctor(fdef);
      a = argTermP(*arg, 0);      
    } else
      return PL_error(NULL, 0, NULL, ERR_TYPE,
		      ATOM_evaluable, wordToTermRef(arg));

    if ( (index = indexArithFunction(fdef, ci->module)) < 0 )
    { return PL_error(NULL, 0, "No such aritmetic function",
			ERR_TYPE, ATOM_evaluable, wordToTermRef(arg));
    }

    for(n=0; n<ar; a++, n++)
      TRY( compileArithArgument(a, ci PASS_LD) );

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


		 /*******************************
		 *	     ATOM-GC		*
		 *******************************/

#ifdef O_ATOMGC

void
unregisterAtomsClause(Clause clause)
{ Code PC, ep;
  int c;

  PC = clause->codes;
  ep = PC + clause->code_size;

  for( ; PC < ep; PC += (codeTable[c].arguments + 1) )
  { c = decode(*PC);

#if O_DEBUGGER
  again:
#endif
    switch(c)
    {
#if O_DEBUGGER
      case D_BREAK:
	c = decode(replacedBreak(PC));
        goto again;
#endif
      case H_INDIRECT:		/* only skip the size of the */
      case B_INDIRECT:		/* string + header */
      { word m = PC[1];
	PC += wsizeofInd(m)+1;
	break;
      }
      case H_CONST:
      case B_CONST:
      { word w = PC[1];

	if ( isAtom(w) )
	  PL_unregister_atom(w);
      }
    }
  }
}

#endif /*O_ATOMGC*/

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
assert_term(term_t term, int where, SourceLoc loc ARG_LD)
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

  PL_strip_module(term, &module, tmp);
  mhead = module;
  get_head_and_body_clause(tmp, head, body, &mhead PASS_LD);
  if ( !get_head_functor(head, &fdef, 0 PASS_LD) )
    return NULL;			/* not callable, arity too high */
  if ( !(proc = lookupProcedureToDefine(fdef, mhead)) )
    return NULL;			/* redefine a system predicate */

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
  if ( !(clause = compileClause(h, b, proc, module PASS_LD)) )
    return NULL;
  DEBUG(2, Sdprintf("ok\n"));
  def = proc->definition;

  if ( def->indexPattern && !(def->indexPattern & NEED_REINDEX) )
  { getIndex(argTermP(*h, 0),
	     def->indexPattern,
	     def->indexCardinality,
	     &clause->index
	     PASS_LD);
  } else
    clause->index.key = clause->index.varmask = 0L;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If loc is defined, we are called from record_clause/2.  This code takes
care of reconsult, redefinition, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( loc )
  { SourceFile sf;

    sf = lookupSourceFile(loc->file);
    clause->line_no   = loc->line;
    clause->source_no = sf->index;

    if ( def->module != mhead )
    { if ( true(def->module, SYSTEM) )
	PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
		 ATOM_redefine, ATOM_built_in_procedure, def);
      else
	warning("%s/%d already imported from module %s", 
		stringAtom(def->functor->name), 
		def->functor->arity, 
		stringAtom(proc->definition->module->name) );
      freeClause(clause PASS_LD);
      return NULL;
    }

    if ( proc == sf->current_procedure )
      return assertProcedure(proc, clause, where PASS_LD) ? clause : NULL;

    if ( def->definition.clauses )	/* i.e. is (might be) defined */
      redefineProcedure(proc, sf);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This `if' locks predicates as system predicates  if  we  are  in  system
mode, the predicate is still undefined and is not dynamic or multifile.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    if ( SYSTEM_MODE && false(def, SYSTEM) &&
	 !(true(def, DYNAMIC|FOREIGN|MULTIFILE) ||
	   hasClausesDefinition(def)) )
      set(def, SYSTEM|HIDE_CHILDS|LOCKED);

    addProcedureSourceFile(sf, proc);
    sf->current_procedure = proc;
    return assertProcedure(proc, clause, where PASS_LD) ? clause : NULL;
  }

  /* assert[az]/1 */

  if ( false(def, DYNAMIC) && isDefinedProcedure(proc) )
  { PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, proc);
    freeClause(clause PASS_LD);
    return NULL;
  }
  set(def, DYNAMIC);			/* Make dynamic on first assert */

  return assertProcedure(proc, clause, where PASS_LD) ? clause : (Clause)NULL;
}


static
PRED_IMPL("assertz", 1, assertz1, PL_FA_TRANSPARENT)
{ PRED_LD

  return assert_term(A1, CL_END, NULL PASS_LD) == NULL ? FALSE : TRUE;
}


static
PRED_IMPL("asserta", 1, asserta1, PL_FA_TRANSPARENT)
{ PRED_LD

  return assert_term(A1, CL_START, NULL PASS_LD) == NULL ? FALSE : TRUE;
}


static
PRED_IMPL("assertz", 2, assertz2, PL_FA_TRANSPARENT)
{ PRED_LD
  Clause clause = assert_term(A1, CL_END, NULL PASS_LD);

  if (clause == (Clause)NULL)
    fail;

  return PL_unify_pointer(A2, clause);
}


static
PRED_IMPL("asserta", 2, asserta2, PL_FA_TRANSPARENT)
{ PRED_LD
  Clause clause = assert_term(A1, CL_START, NULL PASS_LD);

  if (clause == (Clause)NULL)
    fail;

  return PL_unify_pointer(A2, clause);
}


static
PRED_IMPL("$record_clause", 3, record_clause, 0)
{ PRED_LD
  Clause clause;
  sourceloc loc;

  term_t term = A1;
  term_t file = A2;
  term_t ref  = A3;

  if ( PL_get_atom(file, &loc.file) )	/* just the name of the file */
  { loc.line = source_line_no;
  } else if ( PL_is_functor(file, FUNCTOR_colon2) )
  { term_t arg = PL_new_term_ref();	/* file:line */

    PL_get_arg(1, file, arg);
    if ( !PL_get_atom_ex(arg, &loc.file) )
      fail;
    PL_get_arg(2, file, arg);
    if ( !PL_get_integer_ex(arg, &loc.line) )
      fail;
  }

  if ( (clause = assert_term(term, CL_END, &loc PASS_LD)) )
    return PL_unify_pointer(ref, clause);
  
  fail;
}  


word
pl_redefine_system_predicate(term_t pred)
{ GET_LD
  Procedure proc;
  Module m = NULL;
  functor_t fd;
  term_t head = PL_new_term_ref();

  PL_strip_module(pred, &m, head);
  if ( !PL_get_functor(head, &fd) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, pred);

  proc = lookupProcedure(fd, m);
  abolishProcedure(proc, m);
  set(proc->definition, P_REDEFINED);	/* flag as redefined */

  succeed;
}


		/********************************
		*          DECOMPILER           *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
decompileArg1()  is  a  simplified  version   of  decompileHead().   Its
function is to extract the relevant   information  for (re)computing the
index information for indexing on the   first argument (the 99.9% case).
See reindexClause().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
arg1Key(Clause clause, word *key)
{ Code PC = clause->codes;

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
      case H_LIST:
      case H_RLIST:
	*key = FUNCTOR_dot2;
        succeed;
      case H_INTEGER:
	*key = (word)*PC;
	succeed;
      case H_FLOAT:			/* tbd */
      case H_INDIRECT:
      case H_FIRSTVAR:
      case H_VAR:
      case H_VOID:
      case I_EXITFACT:
      case I_EXIT:			/* fact */
      case I_ENTER:			/* fix H_VOID, H_VOID, I_ENTER */
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
  Word	 argp;				/* argument pointer */
  int	 nvars;				/* size of var block */
  term_t *variables;			/* variable table */
  term_t bindings;			/* [Offset = Var, ...] */
  Module body_context;			/* I_CONTEXT module (if any) */
} decompileInfo;

forwards bool	decompile_head(Clause, term_t, decompileInfo * ARG_LD);
forwards bool	decompileBody(decompileInfo *, code, Code ARG_LD);
forwards void	build_term(functor_t, decompileInfo * ARG_LD);

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
  di.nvars     = VAROFFSET(1) + clause->prolog_vars;
  di.variables = alloca(di.nvars * sizeof(term_t));
  di.bindings  = 0;

  return decompile_head(clause, head, &di PASS_LD);
}


static void
get_arg_ref(term_t term, term_t argp ARG_LD)
{ word w = valHandle(term PASS_LD);
  setHandle(argp, makeRef(argTermP(w, 0)));
}


static void
next_arg_ref(term_t argp ARG_LD)
{ Word p = valTermRef(argp);
  
  *p = makeRef(unRef(*p)+1);
}


static bool
unifyVar(Word var, term_t *vars, int i ARG_LD)
{ DEBUG(3, Sdprintf("unifyVar(%d, %d, %d)\n", var, vars, i) );

  assert(vars[i]);

  return unify_ptrs(var, valTermRef(vars[i]) PASS_LD);
}


static bool
decompile_head(Clause clause, term_t head, decompileInfo *di ARG_LD)
{ int arity;
  term_t argp;
  int argn = 0;
  int pushed = 0;
  Definition def = clause->procedure->definition;

  if ( di->bindings )
  { term_t *p = &di->variables[VAROFFSET(0)];
    term_t tail = PL_copy_term_ref(di->bindings);
    term_t head = PL_new_term_ref();
    int n;

    for(n=0; n<clause->prolog_vars; n++)
    { p[n] = PL_new_term_ref();

      if ( !PL_unify_list(tail, head, tail) ||
	   !PL_unify_term(head, PL_FUNCTOR, FUNCTOR_equals2,
			  	    PL_INT, n,
			            PL_TERM, p[n]) )
	fail;
    }
    TRY(PL_unify_nil(tail));
  } else
  { term_t *p = &di->variables[VAROFFSET(0)];
    int n;

    for(n=0; n<clause->prolog_vars; n++)
      p[n] = PL_new_term_ref();
  }

  PC = clause->codes;

  if ( true(clause, GOAL_CLAUSE) )
    return PL_unify_atom(head, ATOM_dcall);

  argp  = PL_new_term_ref();

  DEBUG(5, Sdprintf("Decompiling head of %s\n", predicateName(def)));
  arity = def->functor->arity;
  TRY( PL_unify_functor(head, def->functor->functor) );
  if ( arity > 0 )
    get_arg_ref(head, argp PASS_LD);


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
      case H_INDIRECT:
        { word copy = globalIndirectFromCode(&PC);
	  TRY(_PL_unify_atomic(argp, copy));
	  NEXTARG;
	  continue;
	}
      case H_INTEGER:
        { word copy = globalLong(XR(*PC++) PASS_LD);
	  TRY(_PL_unify_atomic(argp, copy));
	  NEXTARG;
	  continue;
	}
      case H_FLOAT:
        { Word p = allocGlobal(2+WORDS_PER_DOUBLE);
	  word w;

	  w = consPtr(p, TAG_FLOAT|STG_GLOBAL);
	  *p++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
	  cpDoubleData(p, PC);
	  *p   = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
	  TRY(_PL_unify_atomic(argp, w));
	  NEXTARG;
	  continue;
	}
      case H_CONST:
	  TRY(_PL_unify_atomic(argp, XR(*PC++)));
          NEXTARG;
	  continue;
      case H_FIRSTVAR:
      case H_VAR:
	  TRY(unifyVar(valTermRef(argp), di->variables,
		       *PC++ PASS_LD) );
          NEXTARG;
	  continue;
      case H_VOID:
	{ if ( !pushed )		/* FIRSTVAR in the head */
	    TRY(unifyVar(valTermRef(argp), di->variables,
			 VAROFFSET(argn) PASS_LD) );
	  NEXTARG;
	  continue;
	}
      case H_FUNCTOR:
	{ functor_t fdef;
	  term_t t2;

	  fdef = (functor_t) XR(*PC++);
      common_functor:
	  t2 = PL_new_term_ref();
	  TRY(PL_unify_functor(argp, fdef));
          get_arg_ref(argp, t2 PASS_LD);
          next_arg_ref(argp PASS_LD);
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
      case I_POPF:
	  resetTermRefs(argp);
          argp--;
	  pushed--;
	  if ( !pushed )
	    argn++;
	  continue;
      case I_EXITFACT:
      case I_EXIT:			/* fact */
      case I_ENTER:			/* fix H_VOID, H_VOID, I_ENTER */
	{ assert(argn <= arity);
	  for(; argn < arity; argn++)
	  { TRY(unifyVar(valTermRef(argp), di->variables,
			 VAROFFSET(argn) PASS_LD));
	    next_arg_ref(argp PASS_LD);
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
  Word body;

  di->nvars        = VAROFFSET(1) + clause->prolog_vars;
  di->variables    = alloca(di->nvars * sizeof(term_t));
  di->bindings     = bindings;
  di->body_context = NULL;

#ifdef O_RUNTIME
  if ( false(clause->procedure->definition, DYNAMIC) )
    fail;
#endif

  if ( true(clause, UNIT_CLAUSE) )	/* fact */
  { if ( decompile_head(clause, term, di PASS_LD) )
      succeed;

					/* deal with a :- A */
    if ( PL_is_functor(term, FUNCTOR_prove2) )
    { term_t b = PL_new_term_ref();
      _PL_get_arg(2, term, b);

      if ( PL_unify_atom(b, ATOM_true) )
      { _PL_get_arg(1, term, term);
	return decompile_head(clause, term, di PASS_LD);
      }
    }

    fail;
  } else
  { term_t a = PL_new_term_ref();

    TRY(PL_unify_functor(term, FUNCTOR_prove2));
    PL_get_arg(1, term, a);
    TRY(decompile_head(clause, a, di PASS_LD));
    PL_get_arg(2, term, a);
    body = valTermRef(a);
    deRef(body);
  }

  ARGP = (Word) lTop;
  decompileBody(di, I_EXIT, (Code) NULL PASS_LD);

  { Word b, ba;
    int var;

    b = newTerm();

    if ( di->body_context )
    { Word b2 = allocGlobal(3);
      b2[0] = FUNCTOR_colon2;
      b2[1] = di->body_context->name;
      setVar(b2[2]);
      ba = &b2[2];
      *b = consPtr(b2, TAG_COMPOUND|STG_GLOBAL);
    } else
    { ba = b;
    }

    ARGP--;
    if ( (var = isVarRef(*ARGP)) >= 0 )
      unifyVar(ba, di->variables, var PASS_LD);
    else
      *ba = *ARGP;

    return unify_ptrs(body, b PASS_LD);
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
style  and the head is decompiled using a stack machine.  This takes the
best of both approaches: the head is not in reverse polish notation  and
is  not  unlikely  to be instantiated (retract/1), while it is very rare
that clause/retract are used with instantiated body.

The decompilation stack is located on top of the local  stack,  as  this
area is not in use during decompilation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
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
    { PC--;
      break;
    }

    switch( op )
    {
#if O_DEBUGGER
        case D_BREAK:	    op = decode(replacedBreak(PC-1));
			    goto again;
#endif	  
        case A_ENTER:
        case I_NOP:	    continue;
	case B_CONST:
			    *ARGP++ = XR(*PC++);
			    continue;
	case B_NIL:
			    *ARGP++ = ATOM_nil;
			    continue;
	case B_INTEGER:
	case A_INTEGER:
			    *ARGP++ = makeNum(*PC++);
			    continue;
	case B_FLOAT:
	case A_DOUBLE:
		  	  { Word p = allocGlobal(2+WORDS_PER_DOUBLE);
			    
			    *ARGP++ = consPtr(p, TAG_FLOAT|STG_GLOBAL);
			    *p++ = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
			    cpDoubleData(p, PC);
			    *p   = mkIndHdr(WORDS_PER_DOUBLE, TAG_FLOAT);
			    continue;
			  }
	case B_INDIRECT:
	  		    *ARGP++ = globalIndirectFromCode(&PC);
			    continue;
      { register int index;      

	case B_ARGVAR:
	case B_ARGFIRSTVAR:
	case B_FIRSTVAR:
	case A_VAR:
	case B_VAR:	    index = *PC++;		goto var_common;
	case A_VAR0:
	case B_VAR0:	    index = VAROFFSET(0);	goto var_common;
	case A_VAR1:
	case B_VAR1:	    index = VAROFFSET(1);	goto var_common;
	case A_VAR2:
	case B_VAR2:	    index = VAROFFSET(2);	var_common:
			    if ( nested )
			      unifyVar(ARGP++, di->variables, index PASS_LD);
			    else
			      *ARGP++ = makeVarRef(index);
			    continue;
      }
      case B_VOID:
			    setVar(*ARGP++);
			    continue;
      case B_FUNCTOR:
      { functor_t fdef;

	fdef = (functor_t)XR(*PC++);
      common_bfunctor:
	*ARGP = globalFunctor(fdef);
        *aTop++ = ARGP + 1;
        requireStack(argument, sizeof(Word));
	ARGP = argTermP(*ARGP, 0);
	nested++;
	continue;
      case B_LIST:
	fdef = FUNCTOR_dot2;
        goto common_bfunctor;
      }
      case B_RFUNCTOR:
      { functor_t fdef;

	fdef = (functor_t)XR(*PC++);
      common_brfunctor:
	*ARGP = globalFunctor(fdef);
	ARGP = argTermP(*ARGP, 0);
	continue;
      case B_RLIST:
	fdef = FUNCTOR_dot2;
        goto common_brfunctor;
      }
      case I_POPF:
			    ARGP = *--aTop;
			    nested--;
			    continue;
#if O_COMPILE_ARITH
      case A_FUNC0:
      case A_FUNC1:
      case A_FUNC2:
			    build_term(functorArithFunction(*PC++), di PASS_LD);
			    continue;
      case A_FUNC:
      			    build_term(functorArithFunction(*PC++), di PASS_LD);
      			    PC++;
			    continue;
#endif /* O_COMPILE_ARITH */
      { functor_t f;
#if O_COMPILE_ARITH
	case A_LT:	    f = FUNCTOR_smaller2;	goto f_common;
	case A_LE:	    f = FUNCTOR_smaller_equal2;	goto f_common;
	case A_GT:	    f = FUNCTOR_larger2;	goto f_common;
	case A_GE:	    f = FUNCTOR_larger_equal2;	goto f_common;
	case A_EQ:	    f = FUNCTOR_ar_equals2;	goto f_common;
	case A_NE:	    f = FUNCTOR_ar_not_equal2;	goto f_common;
	case A_IS:	    f = FUNCTOR_is2;		goto f_common;
#endif /* O_COMPILE_ARITH */
#if O_BLOCK
	case I_CUT_BLOCK:   f = FUNCTOR_dcut1;		goto f_common;
	case B_EXIT:	    f = FUNCTOR_dexit2;		goto f_common;
#endif
#if O_CATCHTHROW
	case B_THROW:	    f = FUNCTOR_dthrow1;	goto f_common;
#endif
        case I_USERCALLN:   f = lookupFunctorDef(ATOM_call, *PC++ + 1);
							goto f_common;
	case I_APPLY:	    f = FUNCTOR_apply2;		f_common:
			    build_term(f, di PASS_LD);
			    pushed++;
			    continue;
      }
      case I_FAIL:	    *ARGP++ = ATOM_fail;
			    pushed++;
			    continue;
      case I_TRUE:	    *ARGP++ = ATOM_true;
			    pushed++;
			    continue;
      case C_LCUT:	    PC++;
			    /*FALLTHROUGH*/
      case I_CUT:	    *ARGP++ = ATOM_cut;
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
      case I_CONTEXT:	    di->body_context = (Module) *PC++;
      			    continue;
      case I_DEPART:
      case I_CALL:        { Procedure proc = (Procedure)XR(*PC++);
			    build_term(proc->definition->functor->functor, di PASS_LD);
			    pushed++;
			    continue;
			  }
      case I_USERCALL0:
			    pushed++;
			    continue;
#if O_INLINE_FOREIGNS
      case I_CALL_FV0:			/* proc */
      case I_CALL_FV1:			/* proc, var */
      case I_CALL_FV2:			/* proc, var, var */
      { int vars = op - I_CALL_FV0;
	int i;

	for(i=0; i<vars; i++)
	{ int index = PC[i+1];		/* = B_VAR <N> (never nested!) */
	  
	  *ARGP++ = makeVarRef(index);
	}
	build_term(((Procedure)XR(*PC))->definition->functor->functor, di PASS_LD);
	pushed++;
	PC += vars+1;
	continue;
      }
#endif /*O_INLINE_FOREIGNS*/
#if O_COMPILE_OR
#define DECOMPILETOJUMP { int to_jump = (int) *PC++; \
			  decompileBody(di, (code)-1, PC+to_jump PASS_LD); \
			}
      case C_CUT:
      case C_VAR:
      case C_JMP:
			    PC++;
			    continue;
      case C_OR:				/* A ; B */
			    DECOMPILETOJUMP;	/* A */
			    PC--;		/* get C_JMP argument */
			    DECOMPILETOJUMP;	/* B */
			    build_term(FUNCTOR_semicolon2, di PASS_LD);
			    pushed++;
			    continue;
      case C_NOT:				/* \+ A */
			  { PC += 2;		/* skip the two arguments */
			    decompileBody(di, C_CUT, (Code)NULL PASS_LD);   /* A */
			    PC += 3;		/* skip C_CUT <n> and C_FAIL */
			    build_term(FUNCTOR_not_provable1, di PASS_LD);
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

			    decompileBody(di, icut, (Code)NULL PASS_LD);   /* A */
			    PC += 2;		/* skip the cut */
			    decompileBody(di, (code)-1, adr1 PASS_LD);	    /* B */
			    build_term(f, di PASS_LD);
			    PC--;
			    DECOMPILETOJUMP;	/* C */
			    build_term(FUNCTOR_semicolon2, di PASS_LD);
			    pushed++;
			    continue;
			  }
      case C_MARK:				/* A -> B */
			    PC++;
			    decompileBody(di, C_CUT, (Code)NULL PASS_LD);   /* A */
			    PC += 2;
			    decompileBody(di, C_END, (Code)NULL PASS_LD);   /* B */
			    PC++;
			    build_term(FUNCTOR_ifthen2, di PASS_LD);
			    pushed++;
			    continue;
#endif /* O_COMPILE_OR */
      case I_EXIT:
			    break;
      default:
	  sysError("Illegal instruction in clause body: %d", PC[-1]);
	  /*NOTREACHED*/
    }
  }

  while( pushed-- > 1)
    build_term(FUNCTOR_comma2, di PASS_LD);

  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Build the actual term.  The arguments are on  the  decompilation  stack.
We  construct a term of requested arity and name, copy `arity' arguments
from the stack into the term and finally  push  the  term  back  on  the
stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
build_term(functor_t f, decompileInfo *di ARG_LD)
{ word term;
  int arity = arityFunctor(f);
  Word a;

  if ( arity == 0 )
  { *ARGP++ = nameFunctor(f);
    return;
  }    

  term = globalFunctor(f);
  a = argTermP(term, arity-1);

  ARGP--;
  for( ; arity-- > 0; a--, ARGP-- )
  { register int var;

    if ( (var = isVarRef(*ARGP)) >= 0 )
      unifyVar(a, di->variables, var PASS_LD);
    else
      *a = *ARGP;
  }
  ARGP++;

  *ARGP++ = term;
}

#undef PC
#undef ARGP

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unify_definition(?Head, +Def, -TheHead, flags)
    Given some definition, unify its Prolog reference (i.e. its head with
    optional module specifier) with ?Head.  If TheHead is specified, the
    plain head (i.e. without module specifier) will be referenced from
    this term-reference.

    This function properly deals with module-inheritance, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
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
unify_definition(term_t head, Definition def, term_t thehead, int how)
{ GET_LD
  if ( PL_is_variable(head) )
  { if ( def->module == MODULE_user ||
	 ((how&GP_HIDESYSTEM) && true(def->module, SYSTEM)) )
    { unify_functor(head, def->functor->functor, how);
      if ( thehead )
	PL_put_term(thehead, head);
    } else
    { term_t tmp = PL_new_term_ref();
      
      PL_unify_functor(head, FUNCTOR_colon2);
      PL_get_arg(1, head, tmp);
      PL_unify_atom(tmp, def->module->name);
      PL_get_arg(2, head, tmp);
      unify_functor(tmp, def->functor->functor, how);
      if ( thehead )
	PL_put_term(thehead, tmp);
    }

    succeed;
  } else
  { term_t h = PL_new_term_ref();
    Module m = NULL;

    if ( !PL_strip_module(head, &m, h) ||
	 !isSuperModule(def->module, m) )
    { if ( PL_is_functor(head, FUNCTOR_colon2) )
      {	PL_get_arg(1, head, h);
	if ( !PL_unify_atom(h, def->module->name) )
	  fail;
	PL_get_arg(2, head, h);
      } else
	fail;
    }


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
  { term_t h1 = PL_new_term_ref();
    term_t d1 = PL_new_term_ref();
    Module m = NULL;

    PL_strip_module(h, &m, h1);
    PL_strip_module(d, &m, d1);

    return PL_unify(h1, d1);
  } else
    return TRUE;
}


word
pl_clause4(term_t head, term_t body, term_t ref, term_t bindings,
	   control_t ctx)
{ GET_LD
  Procedure proc;
  Definition def;
  ClauseRef cref, next;
  Word argv;
  Module module = NULL;
  term_t term = PL_new_term_ref();
  term_t h    = PL_new_term_ref();
  term_t b    = PL_new_term_ref();
  LocalFrame fr = environment_frame;
  mark m;

  switch( ForeignControl(ctx) )
  { case FRG_FIRST_CALL:
    { Clause clause;

      if ( ref )
      { if ( PL_get_pointer(ref, (void **)&clause) )
	{ term_t tmp;
      
	  if ( !isClause(clause) )
	    PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_clause_reference, ref);
	      
	  decompile(clause, term, bindings);
	  proc = clause->procedure;
	  def = proc->definition;
	  if ( true(clause, GOAL_CLAUSE) )
	  { tmp = head;
	  } else
	  { tmp = PL_new_term_ref();
	    if ( !unify_definition(head, def, tmp, 0) )
	      fail;
	  }
	  get_head_and_body_clause(term, h, b, NULL PASS_LD);
	  if ( unify_head(tmp, h PASS_LD) && PL_unify(body, b) )
	    succeed;
	  fail;
	}
	if ( !PL_is_variable(ref) )
	  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_clause_reference, ref);
      }
      if ( !get_procedure(head, &proc, 0, GP_FIND) )
	fail;
      def = proc->definition;

      if ( true(def, FOREIGN) ||
	   (   trueFeature(ISO_FEATURE) &&
	       false(def, DYNAMIC)
	   ) )
	return PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
			ATOM_access, ATOM_private_procedure, def);

      cref = NULL;			/* see below */
      enterDefinition(def);		/* reference the predicate */
      break;
    }
    case FRG_REDO:
    { cref = ForeignContextPtr(ctx);
      proc = cref->clause->procedure;
      def  = proc->definition;
      break;
    }
    case FRG_CUTTED:
    default:
    { cref = ForeignContextPtr(ctx);

      if ( cref )
      { def  = cref->clause->procedure->definition;
	leaveDefinition(def);
      }
      succeed;
    }
  }

  if ( def->functor->arity > 0 )
  { PL_strip_module(head, &module, head);
    argv = valTermRef(head);
    deRef(argv);
    argv = argTermP(*argv, 0);
  } else
    argv = NULL;

  if ( !cref )
  { cref = firstClause(argv, fr, def, &next PASS_LD);
  } else
  { cref = findClause(cref, argv, fr, def, &next PASS_LD);
  }

  Mark(m);
  while(cref)
  { if ( decompile(cref->clause, term, bindings) )
    { get_head_and_body_clause(term, h, b, NULL PASS_LD);
      if ( unify_head(head, h PASS_LD) &&
	   PL_unify(b, body) &&
	   (!ref || PL_unify_pointer(ref, cref->clause)) )
      { if ( !next )
	{ leaveDefinition(def);
	  succeed;
	}

	ForeignRedoPtr(next);
      }
    }

    Undo(m);
    cref = findClause(next, argv, fr, def, &next PASS_LD);
  }

  leaveDefinition(def);
  fail;
}


word
pl_clause3(term_t p, term_t term, term_t ref, control_t h)
{ return pl_clause4(p, term, ref, 0, h);
}


word
pl_clause2(term_t p, term_t term, control_t h)
{ return pl_clause4(p, term, 0, 0, h);
}


typedef struct
{ ClauseRef clause;			/* pointer to the clause */
  int       index;			/* nth-1 index */
} crref, *Cref;


word
pl_nth_clause(term_t p, term_t n, term_t ref, control_t h)
{ GET_LD
  Clause clause;
  ClauseRef cref;
  Procedure proc;
  Definition def;
  Cref cr;
#ifdef O_LOGICAL_UPDATE
  unsigned long generation = environment_frame->generation;
#endif

  if ( ForeignControl(h) == FRG_CUTTED )
  { cr = ForeignContextPtr(h);

    if ( cr )
    { def = cr->clause->clause->procedure->definition;
      leaveDefinition(def);
      freeHeap(cr, sizeof(crref));
    }
    succeed;
  }

  if ( PL_get_pointer(ref, (void **)&clause) )
  { int i;

    if ( !isClause(clause) )
      return PL_error(NULL, 0, "Invalid integer reference", ERR_DOMAIN,
		      ATOM_clause_reference, ref);
	
    if ( true(clause, GOAL_CLAUSE) )
      fail;				/* I'm don't belong to a predicate */

    proc = clause->procedure;
    def  = proc->definition;
    for( cref = def->definition.clauses, i=1; cref; cref = cref->next)
    { if ( cref->clause == clause )
      { if ( !PL_unify_integer(n, i) ||
	     !unify_definition(p, def, 0, 0) )
	  fail;

	succeed;
      }
      if ( visibleClause(cref->clause, generation) )
	i++;
    }

    fail;
  }

  if ( ForeignControl(h) == FRG_FIRST_CALL )
  { int i;

    if ( !get_procedure(p, &proc, 0, GP_FIND) ||
         true(proc->definition, FOREIGN) )
      fail;

    def = proc->definition;
    cref = def->definition.clauses;
    while ( cref && !visibleClause(cref->clause, generation) )
      cref = cref->next;
    
    if ( !cref )
      fail;

    if ( PL_get_integer(n, &i) )	/* proc and n specified */
    { i--;				/* 0-based */

      while(i > 0 && cref)
      { do
	{ cref = cref->next;
	} while ( cref && !visibleClause(cref->clause, generation) );

	i--;
      }
      if ( i == 0 && cref )
	return PL_unify_pointer(ref, cref->clause);
      fail;
    }

    cr = allocHeap(sizeof(crref));
    cr->clause = cref;
    cr->index  = 1;
    enterDefinition(def);
  } else
  { cr = ForeignContextPtr(h);
    def = cr->clause->clause->procedure->definition;
  }

  PL_unify_integer(n, cr->index);
  PL_unify_pointer(ref, cr->clause->clause);

  cref = cr->clause->next;
  while ( cref && !visibleClause(cref->clause, generation) )
    cref = cref->next;

  if ( cref )
  { cr->clause = cref;
    cr->index++;
    ForeignRedoPtr(cr);
  }

  freeHeap(cr, sizeof(crref));
  leaveDefinition(def);

  succeed;
}


int
get_clause_ptr_ex(term_t ref, Clause *cl)
{ GET_LD
  Clause clause;

  if ( !PL_get_pointer(ref, (void **)&clause) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_clause_reference, ref);
  if ( !isClause(clause) )
    return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_clause_reference, ref);

  *cl = clause;
  succeed;
}

#if O_DEBUGGER				/* to the end of the file */

static code
fetchop(Code PC)
{ code op = decode(*PC);

  if ( op == D_BREAK )
    op = decode(replacedBreak(PC));

  return op;
}

static Code
stepPC(Code PC)
{ code op = fetchop(PC++);

  if ( codeTable[op].argtype == CA1_STRING )
  { word m = *PC++;
    PC += wsizeofInd(m);
  }

  PC += codeTable[op].arguments;

  return PC;
}


static int
wouldBindToDefinition(Definition from, Definition to)
{ Module m = from->module;
  Definition def = from;
  Procedure proc;

  for(;;)
  { if ( def )
    { if ( def == to )			/* found it */
	succeed;

      if ( def->definition.clauses ||	/* defined and not the same */
	   true(def, PROC_DEFINED) ||
	   false(def->module, UNKNOWN_ERROR|UNKNOWN_WARNING) )
	fail;
    }

    if ( (m = m->super) )
    { proc = isCurrentProcedure(from->functor->functor, m);
      def = proc ? proc->definition : (Definition)NULL;
    } else
      break;
  }

  fail;
}


word
pl_xr_member(term_t ref, term_t term, control_t h)
{ GET_LD
  Clause clause;
  Code PC;
  Code end;

  if ( ForeignControl(h) == FRG_CUTTED )
    succeed;

  if ( !get_clause_ptr_ex(ref, &clause) )
    fail;

  PC  = clause->codes;
  end = &PC[clause->code_size];

  if ( PL_is_variable(term) )
  { if ( ForeignControl(h) != FRG_FIRST_CALL)
    { long i = ForeignContextInt(h);

      PC += i;
    }

    while( PC < end )
    { bool rval = FALSE;
      code op = fetchop(PC++);
      
      switch(codeTable[op].argtype)
      { case CA1_PROC:
	{ Procedure proc = (Procedure) *PC;
	  rval = unify_definition(term, proc->definition, 0, 0);
	  break;
	}
	case CA1_FUNC:
	{ functor_t fd = (functor_t) *PC;
	  rval = PL_unify_functor(term, fd);
	  break;
	}
	case CA1_DATA:
	{ word xr = *PC;
	  rval = _PL_unify_atomic(term, xr);
	  break;
	}
	case CA1_MODULE:
	{ Module xr = (Module)*PC;
	  rval = _PL_unify_atomic(term, xr->name);
	  break;
	}
	case CA1_INTEGER:
	case CA1_FLOAT:
	  break;
	case CA1_STRING:
	{ word m = *PC++;
	  PC += wsizeofInd(m);
	  break;
	}
      }

      PC += codeTable[op].arguments;

      if ( rval )
      { long i = PC - clause->codes;	/* compensate ++ above! */

	ForeignRedoInt(i);
      }
    }

    fail;
  } else				/* instantiated */
  { GET_LD
    Procedure proc;
    functor_t fd;

    if ( PL_is_atomic(term) )
    { while( PC < end )
      { code op = fetchop(PC);

	if ( codeTable[op].argtype == CA1_DATA &&
	     _PL_unify_atomic(term, PC[1]) )
	    succeed;

	PC = stepPC(PC);
      }
    } else if ( PL_get_functor(term, &fd) && fd != FUNCTOR_colon2 )
    { while( PC < end )
      { code op = fetchop(PC);

	if ( codeTable[op].argtype == CA1_FUNC )
	{ functor_t fa = (functor_t)PC[1];

	  if ( fa == fd )
	  { DEBUG(1,
		  { term_t ref = PL_new_term_ref();
		    long i;
		    
		    PL_unify_pointer(ref, clause);
		    PL_get_long(ref, &i);
		    Sdprintf("Got it, clause %d at %d\n",
			     i, PC-clause->codes);
		  });
	    succeed;
	  }
	}

	PC = stepPC(PC);
      }
    } else if ( get_procedure(term, &proc, 0, GP_FIND) )
    { while( PC < end )
      { code op = fetchop(PC);

	if ( codeTable[op].argtype == CA1_PROC )
	{ Procedure pa = (Procedure)PC[1];

	  if ( pa->definition == proc->definition )
	    succeed;
	  if ( pa->definition->functor == proc->definition->functor &&
	       wouldBindToDefinition(pa->definition, proc->definition) )
	    succeed;
	}

	PC = stepPC(PC);
      }
    }
  }

  fail;
}

		 /*******************************
		 *	     WAM_LIST		*
		 *******************************/

#define VARNUM(i) ((i) - (ARGOFFSET / (int) sizeof(word)))

Code
wamListInstruction(IOSTREAM *out, Clause clause, Code bp)
{ GET_LD
  code op = decode(*bp);
  const code_info *ci;
  int n = 0;
  int isbreak;

  if ( op == D_BREAK )
  { op = decode(replacedBreak(bp));
    isbreak = TRUE;
  } else
    isbreak = FALSE;

  ci = &codeTable[op];

  if ( clause )
    Sfprintf(out, "%4d %s", bp - clause->codes, ci->name);
  else
    Sfprintf(out, "VMI %s", ci->name);

  bp++;					/* skip the instruction */

  switch(op)
  { case B_FIRSTVAR:
    case H_FIRSTVAR:
    case B_ARGFIRSTVAR:
    case B_VAR:
    case B_ARGVAR:
    case H_VAR:
    case C_VAR:
    case C_MARK:
    case C_SOFTCUT:
    case C_CUT:			/* var */
    case C_LCUT:
      assert(ci->arguments == 1);
      Sfprintf(out, " var(%d)", VARNUM(*bp++));
      break;
    case C_SOFTIF:
    case C_IFTHENELSE:		/* var, jump */
    case C_NOT:
    { int var = VARNUM(*bp++);
      int jmp = *bp++;
      assert(ci->arguments == 2);
      Sfprintf(out, " var(%d), jmp(%d)", var, jmp);
      break;
    }
    case I_CALL_FV1:
    case I_CALL_FV2:
    { int vars = op - I_CALL_FV0;
      Procedure proc = (Procedure) *bp++;

      Sfprintf(out, " %s", procedureName(proc));
      for( ; vars > 0; vars-- )
	Sfprintf(out, ", var(%d)", VARNUM(*bp++));
      break;
    }
    default:
      switch(codeTable[op].argtype)
      { case CA1_PROC:
	{ Procedure proc = (Procedure) *bp++;
	  n++;
	  Sfprintf(out, " %s", procedureName(proc));
	  break;
	}
	case CA1_MODULE:
	{ Module m = (Module)*bp++;
	  n++;
	  Sfprintf(out, " %s", stringAtom(m->name));
	  break;
	}
	case CA1_FUNC:
	{ functor_t f = (functor_t) *bp++;
	  FunctorDef fd = valueFunctor(f);
	  n++;
	  Sfprintf(out, " %s/%d", stringAtom(fd->name), fd->arity);
	  break;
	}
	case CA1_DATA:
	{ word xr = *bp++;
	  n++;
	  switch(tag(xr))
	  { case TAG_ATOM:
	      Sfprintf(out, " %s", stringAtom(xr));
	      break;
	    case TAG_INTEGER:
	      Sfprintf(out, " %ld", valInteger(xr));
	      break;
	    case TAG_STRING:
	      Sfprintf(out, " \"%s\"", valString(xr));
	      break;
	    default:
	      assert(0);
	  }
	  break;
	}
	case CA1_INTEGER:
	{ long l = (long) *bp++;
	  n++;
	  Sfprintf(out, " %ld", l);
	  break;
	}
	case CA1_FLOAT:
	{ union
	  { word w[WORDS_PER_DOUBLE];
	    double f;
	  } v;
	  Word p = v.w;
	  n += 2;
	  cpDoubleData(p, bp);
	  Sfprintf(out, " %g", v.f);
#if 0					/* used to discover GDB problem */
	  Sfprintf(out, " [0x%08x 0x%8x]", bp[-2], bp[-1]);
	  Sfprintf(out, " [0x%08x 0x%8x]", v.w[0], v.w[1]);
#endif
	  break;
	}
	case CA1_STRING:
	{ word m = *bp++;
	  int  n = wsizeofInd(m);
	  Sfprintf(out, " \"%s\"", (char *)bp);
	  bp += n;
	  break;
	}
      }
      for(; n < codeTable[op].arguments; n++ )
	Sfprintf(out, "%s%d", n == 0 ? " " : ", ", *bp++);
  }

  if ( isbreak )
    Sfprintf(out, " *break*");

  Sfprintf(out, "\n");

  return bp;
}


void
wamListClause(Clause clause)
{ GET_LD
  Code bp, ep;

  bp = clause->codes;
  ep = bp + clause->code_size;

  while( bp < ep )
    bp = wamListInstruction(Scurout, clause, bp);
}


word
pl_wam_list(term_t ref)
{ Clause clause;

  if ( !get_clause_ptr_ex(ref, &clause) )
    fail;

  wamListClause(clause);

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
$fetch_vm(+Clause, +Offset, -NextOffset, -Instruction)
	fetches the virtual machine instruction at the indicated position
	and return NextOffset with the offset of the next instruction, or
	[] if there is no next instruction.  Instruction is unified with
	a descriptive term of the instruction, but for now only with the
	name of the instruction.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_fetch_vm(term_t ref, term_t offset, term_t noffset, term_t instruction)
{ GET_LD
  Clause clause;
  int pcoffset;
  Code PC;
  code op;
  const code_info *ci;

  if ( !get_clause_ptr_ex(ref, &clause) ||
       !PL_get_integer_ex(offset, &pcoffset) )
    fail;
  if ( pcoffset < 0 || pcoffset >= (int)clause->code_size )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_program_counter, offset);

  PC = clause->codes + pcoffset;
  op = fetchop(PC);
  ci = &codeTable[op];
  
  pcoffset = pcoffset + 1 + ci->arguments;

  if ( PL_unify_integer(noffset, pcoffset) &&
       PL_unify_atom_chars(instruction, ci->name) )
    succeed;

  fail;
}



		 /*******************************
		 *     SOURCE LEVEL DEBUGGER	*
		 *******************************/

#if 0
#undef DEBUG
#define DEBUG(l, g) g
#endif

static Code
find_code1(Code PC, code fop, code ctx)
{ for(;;)
  { code op = fetchop(PC++);

    if ( fop == op && ctx == *PC )
      return &PC[-1];
    assert(op != I_EXIT);

    PC += codeTable[op].arguments;
  }
}


static Code
find_code0(Code PC, code fop)
{ for(;;)
  { code op = fetchop(PC++);

    if ( fop == op )
      return &PC[-1];
    assert(op != I_EXIT);

    PC += codeTable[op].arguments;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
$clause_term_position(+ClauseRef, +PCoffset, -TermPos)
	Find the term-location of the call that ends in the given PC offset.
	The term-position is a list of argument-numbers one has to use from
	the clause-term to find the subterm that sets up the goal.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
add_node(term_t tail, int n ARG_LD)
{ term_t h = PL_new_term_ref();
  int rval;

  rval = PL_unify_list(tail, h, tail) && PL_unify_integer(h, n);
  resetTermRefs(h);

  DEBUG(1, Sdprintf("Added %d\n", n));

  return rval;
}


static void
add_1_if_not_at_end(Code PC, Code end, term_t tail ARG_LD)
{ while(PC < end && fetchop(PC) == C_VAR )
    PC += 2;

  if ( PC != end )
    add_node(tail, 1 PASS_LD);
}



word
pl_clause_term_position(term_t ref, term_t pc, term_t locterm)
{ GET_LD
  Clause clause;
  int pcoffset;
  Code PC, loc, end;
  term_t tail = PL_copy_term_ref(locterm);

  if ( !get_clause_ptr_ex(ref, &clause) ||
       !PL_get_integer_ex(pc, &pcoffset) )
    fail;
  if ( pcoffset < 0 || pcoffset > (int)clause->code_size )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_program_counter, pc);

  PC = clause->codes;
  loc = &PC[pcoffset];
  end = &PC[clause->code_size - 1];	/* forget the final I_EXIT */
  DEBUG(0, assert(fetchop(end) == I_EXIT || fetchop(end) == I_EXITFACT));

  if ( true(clause, GOAL_CLAUSE) )
    add_node(tail, 2 PASS_LD);			/* $call :- <Body> */

  while( PC < loc )
  { code op = fetchop(PC++);
    const code_info *ci;

    ci = &codeTable[op];

    switch(op)
    { case I_ENTER:
	if ( loc == PC )
	{ add_node(tail, 1 PASS_LD);

	  return PL_unify_nil(tail);
	}
	add_node(tail, 2 PASS_LD);
	continue;
      case I_EXIT:
      case I_EXITFACT:
	if ( loc == PC )
	{ return PL_unify_nil(tail);
	}
        continue;
    { Code endloc;
      case C_OR:			/* C_OR <jmp1> <A> C_JMP <jmp2> <B> */
      { Code jmploc;
	code inc = *PC++;
	
	jmploc = PC + inc;
	endloc = jmploc + jmploc[-1];

	DEBUG(1, Sdprintf("jmp = %d, end = %d\n",
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
      case C_NOT:		/* C_NOT <var> <jmp> <A> C_CUT <var>, C_FAIL */
      { endloc = PC + PC[1] + 2;

	DEBUG(1, Sdprintf("not: PC= %d, endloc = %d\n",
			  PC - clause->codes, endloc - clause->codes));

	if ( loc <= endloc )		/* in the \+ argument */
	{ add_1_if_not_at_end(endloc, end, tail PASS_LD);

	  add_node(tail, 1 PASS_LD);
	  PC += 2;
	  end = endloc-3;		/* C_CUT <var>, C_FAIL */
	  continue;
	}

	goto after_construct;
      }
      case C_SOFTIF:
      case C_IFTHENELSE:	/* C_IFTHENELSE <var> <jmp1> */
				/* <IF> C_CUT <THEN> C_JMP <jmp2> <ELSE> */
      { Code elseloc = PC + PC[1] + 2;
	code cut = (op == C_IFTHENELSE ? C_CUT : C_SOFTCUT);

	endloc = elseloc + elseloc[-1];

	DEBUG(1, Sdprintf("else = %d, end = %d\n",
			  elseloc - clause->codes, endloc - clause->codes));

	if ( loc <= endloc )
	{ add_1_if_not_at_end(endloc, end, tail PASS_LD);

	  if ( loc <= elseloc )		/* a->b */
	  { Code cutloc = find_code1(&PC[2], cut, PC[0]);

	    DEBUG(1, Sdprintf("cut at %d\n", cutloc - clause->codes));
	    add_node(tail, 1 PASS_LD);
	    
	    if ( loc <= cutloc )	/* a */
	    { add_node(tail, 1 PASS_LD);
	      end = cutloc;
	      PC = &PC[2];
	    } else			/* b */
	    { add_node(tail, 2 PASS_LD);
	      PC = cutloc + 2;
	      end = elseloc-2;
	    }    
	    DEBUG(1, Sdprintf("end = %d\n", end - clause->codes));
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
      case C_MARK:		/* A -> B */
				/* C_MARK <var> <A> C_CUT <var> <B> C_END */
      { Code cutloc = find_code1(&PC[1], C_CUT, PC[0]);
	
	endloc = find_code0(cutloc+2, C_END);

	DEBUG(1, Sdprintf("cut = %d, end = %d\n",
			  cutloc - clause->codes, endloc - clause->codes));

	if ( loc <= endloc )
	{ add_1_if_not_at_end(endloc+1, end, tail PASS_LD);

	  if ( loc <= cutloc )		/* a */
	  { add_node(tail, 1 PASS_LD);

	    PC += 1;
	    end = cutloc;
	  } else			/* b */
	  { add_node(tail, 2 PASS_LD);
	    PC = cutloc+2;
	    end = endloc;
	  }

	  continue;
	}

	goto after_construct;
      }
      }					/* closes the special constructs */
      case I_CONTEXT:			/* used to compile m:head :- body */
	PC += ci->arguments;
	add_node(tail, 2 PASS_LD);
        continue;
      case I_CALL:
      case I_DEPART:
      case I_CUT:
      case I_FAIL:
      case I_TRUE:
      case I_APPLY:
      case I_USERCALL0:
      case I_USERCALLN:
      case I_CALL_FV0:
      case I_CALL_FV1:
      case I_CALL_FV2:
      case I_CATCH:
	PC += ci->arguments;
        if ( loc == PC )
	{ add_1_if_not_at_end(PC, end, tail PASS_LD);

	  return PL_unify_nil(tail);
	}
	add_node(tail, 2 PASS_LD);
	continue;
      default:
	PC += ci->arguments;
    }
  }

  fail;					/* assert(0) */
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Generate (on backtracing), all  possible   break-points  of  the clause.
Works in combination with pl_clause_term_position()   to  find the place
for placing a break-point.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

word
pl_break_pc(term_t ref, term_t pc, term_t nextpc, control_t h)
{ GET_LD
  Clause clause;
  int offset;
  Code PC, end;

  switch( ForeignControl(h) )
  { case FRG_CUTTED:
      succeed;
    case FRG_FIRST_CALL:
      offset = 0;
    case FRG_REDO:
    default:
      offset = ForeignContextInt(h);
  }

  if ( !get_clause_ptr_ex(ref, &clause) )
    fail;
  PC = clause->codes + offset;
  end = clause->codes + clause->code_size;

  while( PC < end )
  { code op = fetchop(PC);
    Code next;

    next = PC + 1 + codeTable[op].arguments;

    switch(op)
    { case I_ENTER:
      case I_EXIT:
      case I_EXITFACT:
      case I_CALL:
      case I_DEPART:
      case I_CUT:
      case I_FAIL:
      case I_TRUE:
      case I_APPLY:
      case I_USERCALL0:
      case I_USERCALLN:
      case I_CALL_FV0:
      case I_CALL_FV1:
      case I_CALL_FV2:
	if ( PL_unify_integer(pc, PC-clause->codes) &&
	     PL_unify_integer(nextpc, next-clause->codes) )
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
setBreak(Clause clause, int offset)
{ GET_LD
  Code PC = clause->codes + offset;

  if ( !breakTable )
    breakTable = newHTable(16);

  if ( *PC != encode(D_BREAK) )
  { BreakPoint bp = allocHeap(sizeof(break_point));

    bp->clause = clause;
    bp->offset = offset;
    bp->saved_instruction = *PC;

    addHTable(breakTable, PC, bp);
    *PC = encode(D_BREAK);
    set(clause, HAS_BREAKPOINTS);

    callEventHook(PLEV_BREAK, clause, offset);
    succeed;
  }

  fail;
}


static int
clearBreak(Clause clause, int offset)
{ GET_LD
  Code PC;
  BreakPoint bp;
  Symbol s;

  PC = clause->codes + offset;
  if ( !breakTable || !(s=lookupHTable(breakTable, PC)) )
    fail;

  bp = (BreakPoint)s->value;
  *PC = bp->saved_instruction;
  freeHeap(bp, sizeof(*bp));
  deleteSymbolHTable(breakTable, s);

  callEventHook(PLEV_NOBREAK, clause, offset);
  succeed;
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
  { PL_UNLOCK(L_BREAK);
    return (code) sysError("No saved instruction for break");
  }
  bp = (BreakPoint)s->value;
  c = bp->saved_instruction;

  return c;
}


word
pl_break_at(term_t ref, term_t pc, term_t set)
{ Clause clause;
  int offset;
  int doit;

  if ( !get_clause_ptr_ex(ref, &clause) )
    fail;
  if ( !PL_get_bool_ex(set, &doit) ||
       !PL_get_integer_ex(pc, &offset) )
    fail;
  if ( offset < 0 || offset >= (int)clause->code_size )
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_pc, pc);

  PL_LOCK(L_BREAK);
  if ( doit )
    setBreak(clause, offset);
  else
    clearBreak(clause, offset);
  PL_UNLOCK(L_BREAK);

  succeed;
}


word
pl_current_break(term_t ref, term_t pc, control_t h)
{ GET_LD
  TableEnum e = NULL;			/* make gcc happy */
  Symbol symb;
  
  if ( !breakTable )
    fail;

  switch( ForeignControl(h) )
  { case FRG_FIRST_CALL:
      e = newTableEnum(breakTable);
      break;
    case FRG_REDO:
      e = ForeignContextPtr(h);
      break;
    case FRG_CUTTED:
      e = ForeignContextPtr(h);
      freeTableEnum(e);
      succeed;
  }

  while( (symb = advanceTableEnum(e)) )
  { BreakPoint bp = (BreakPoint) symb->value;

    { fid_t cid = PL_open_foreign_frame();

      if ( PL_unify_pointer(ref, bp->clause) &&
	   PL_unify_integer(pc,  bp->offset) )
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

BeginPredDefs(comp)
  PRED_DEF("$record_clause", 3, record_clause, 0)
  PRED_DEF("assert",  1, assertz1, PL_FA_TRANSPARENT)
  PRED_DEF("assertz", 1, assertz1, PL_FA_TRANSPARENT)
  PRED_DEF("asserta", 1, asserta1, PL_FA_TRANSPARENT)
  PRED_DEF("assert",  2, assertz2, PL_FA_TRANSPARENT)
  PRED_DEF("assertz", 2, assertz2, PL_FA_TRANSPARENT)
  PRED_DEF("asserta", 2, asserta2, PL_FA_TRANSPARENT)
EndPredDefs
