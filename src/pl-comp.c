/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: compiler support
*/

#include "pl-incl.h"
#include "pl-buffer.h"

#define CODE(c, n, a, e)	{ n, c, a, e }

struct code_info codeTable[] = {
/*     ID		name	     #args #xr */
  CODE(I_NOP,		"i_nop",	0, 0),
  CODE(I_ENTER,		"i_enter",	0, 0),
  CODE(I_CALL,		"i_call",	1, 1),
  CODE(I_DEPART,	"i_depart",	1, 1),
  CODE(I_EXIT,		"i_exit",	0, 0),
  CODE(B_FUNCTOR,	"b_functor",	1, 1),
  CODE(H_FUNCTOR,	"h_functor",	1, 1),
  CODE(I_POP,		"i_pop",	0, 0),
  CODE(I_POPN,		"i_popn",	1, 0),
  CODE(B_VAR,		"b_var",	1, 0),
  CODE(H_VAR,		"h_var",	1, 0),
  CODE(B_CONST,		"b_const",	1, 1),
  CODE(H_CONST,		"h_const",	1, 1),
  CODE(H_REAL,		"h_real",	1, 1),
  CODE(H_STRING,	"h_string",	1, 1),
  CODE(B_FIRSTVAR,	"b_firstvar",	1, 0),
  CODE(H_FIRSTVAR,	"h_firstvar",	1, 0),
  CODE(B_VOID,		"b_void",	0, 0),
  CODE(H_VOID,		"h_void",	0, 0),
  CODE(B_ARGFIRSTVAR,	"b_argfirstvar",1, 0),
  CODE(B_ARGVAR,	"b_argvar",	1, 0),
  CODE(H_NIL,		"h_nil",	0, 0),
  CODE(H_LIST,		"h_list",	0, 0),
  CODE(B_VAR0,		"b_var0",	0, 0),
  CODE(B_VAR1,		"b_var1",	0, 0),
  CODE(B_VAR2,		"b_var2",	0, 0),
  CODE(I_USERCALL0,	"i_usercall0",	0, 0),
  CODE(I_USERCALLN,	"i_usercalln",	1, 0),
  CODE(I_CUT,		"i_cut",	0, 0),
  CODE(I_APPLY,		"i_apply",	0, 0),
  CODE(A_REAL,		"a_real",	1, 1),
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
  CODE(C_MARK,		"c_mark",	1, 0),
  CODE(C_CUT,		"c_cut",	1, 0),
  CODE(C_IFTHENELSE,	"c_ifthenelse",	2, 0),
  CODE(C_VAR,		"c_var",	1, 0),
  CODE(C_END,		"c_end",	0, 0),
  CODE(C_NOT,		"c_not",	2, 0),
  CODE(C_FAIL,		"c_fail",	0, 0),
  CODE(B_REAL,		"b_real",	1, 1),
  CODE(B_STRING,	"b_string",	1, 1),
#if O_BLOCK
  CODE(I_CUT_BLOCK,	"i_cut_block",	0, 0),
  CODE(B_EXIT,		"b_exit",	0, 0),
#endif
#if O_INLINE_FOREIGNS
  CODE(I_CALL_FV0,	"i_call_fv0",	1, 1),
  CODE(I_CALL_FV1,	"i_call_fv1",	2, 1),
  CODE(I_CALL_FV2,	"i_call_fv2",	3, 1),
#endif
  CODE(I_FAIL,		"i_fail",	0, 0),
  CODE(I_TRUE,		"i_true",	0, 0),
/*List terminator */
  CODE(0,		NULL,		0, 0)
};

forwards void	checkCodeTable(void);

static void
checkCodeTable(void)
{ CodeInfo ci;
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

	Be carefull: pl-wam.o must be loaded in low addresses!
		     make sure you have VM that lets you start the
		     text addresses close to zero!

NOTE:	If the assert() fails, look at pl-wam.c: VMI(C_NOT, ... for
	more information.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if VMCODE_IS_ADDRESS
void
initWamTable(void)
{ int n;
  code maxcoded, mincoded;

  if ( interpreter_jmp_table == NULL )
    PL_next_solution(QID_EXPORT_WAM_TABLE);

  wam_table[0] = (code) ((int)interpreter_jmp_table[0]);
  maxcoded = mincoded = wam_table[0];

  for(n = 1; n <= I_HIGHEST; n++)
  { wam_table[n] = (code) ((int)interpreter_jmp_table[n]);
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
with  a  structure  that  mimics  a term, but isn't one.  For historical
reasons this is the term $VAR$/1.  Future versions will  use  a  functor
which  is  impossible  to  conflict  with  the user's program.  For each
variable it's address is stored, as well  as  the  number  of  times  it
occurred in the clause.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

forwards bool	analyse_variables(Word, Word, int, int*);
forwards int	analyseVariables2(Word, int, int, int);

#if O_COMPILE_ARITH
#define A_NOTARITH	0
#define A_OK		1
#define A_ERROR		2
#endif /* O_COMPILE_ARITH */

static struct vardef
{ FunctorDef	functor;		/* mimic a functor (FUNCTOR_var1) */
  Word		address;		/* address of the variable */
  int		times;			/* occurences */
  int		offset;			/* offset in environment frame */
} vars[MAXVARIABLES];

static int	filledVars;		/* vardef structures filled */

#define VAROFFSET(var) ( (var) + ARGOFFSET / (int) sizeof(word) )

int
get_head_and_body_clause(term_t clause,
			 term_t head, term_t body, Module *m)
{ term_t tmp = PL_new_term_ref();
  Module m0 = NULL;

  if ( m )
    m0 = *m;
  TRY(PL_strip_module(clause, &m0, tmp));

  if ( PL_is_functor(tmp, FUNCTOR_prove2) )
  { PL_get_arg(1, tmp, head);
    PL_get_arg(2, tmp, body);
    PL_strip_module(head, &m0, head);
  } else
  { PL_put_term(head, tmp);		/* facts */
    PL_put_atom(body, ATOM_true);
  }
  
  DEBUG(9, pl_write(clause); Sdprintf(" --->\n\t");
	   Sdprintf("%s:", stringAtom(m0->name));
	   pl_write(head); Sdprintf(" :- "); pl_write(body); Sdprintf("\n"));

  if ( m )
    *m = m0;

  succeed;
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
analyse_variables(Word head, Word body, int arity, int *nv)
{ int nvars = 0;
  register struct vardef * vd;
  register int n;
  int body_voids = 0;

  for(n=0, vd = vars; n<arity; n++, vd++)
    vd->address = (Word) NULL;

  if ( (nvars = analyseVariables2(head, 0, arity, -1)) < 0 )
    fail;
  if (body != (Word) NULL)
    if ( (nvars = analyseVariables2(body, nvars, arity, arity)) < 0 )
      fail;

  for(n=0, vd = vars; n<arity+nvars; n++, vd++)
  { if (vd->address == (Word) NULL)
      continue;
    if (vd->times == 1)				/* ISVOID */
    { setVar(*(vd->address));
      vd->address = (Word) NULL;
      if (n >= arity)
	body_voids++;
    } else
      vd->offset = n - body_voids;
  }

  filledVars = arity + nvars;
  *nv = nvars - body_voids;
  succeed;
}

static int
analyseVariables2(register Word head, int nvars, int arity, int argn)
{ int ar;

  deRef(head);

  if (isVar(*head) )
  { register struct vardef *vd;
    int index = ((argn >= 0 && argn < arity) ? argn : (arity + nvars++));

    if ( index >= MAXVARIABLES-1 )
    { warning("Compiler: Too many variables in clause");
      return -1;
    }
    vd = &vars[index];
    vd->functor = FUNCTOR_var1;
    vd->address = head;
    vd->times = 1;
    *head = (word) vd;

    return nvars;
  }

  if ( isAtomic(*head) )
    return nvars;

  if (functorTerm(*head) == FUNCTOR_var1)
  { ((struct vardef *)*head)->times++;
    return nvars;
  }

  ar = functorTerm(*head)->arity;
  head = argTermP(*head, 0);

  argn = ( argn < 0 ? 0 : arity );

  for(; ar > 0; ar--, head++, argn++)
    nvars = analyseVariables2(head, nvars, arity, argn);

  return nvars;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The compiler  itself.   First  it  calls  analyseVariables().  Next  the
arguments  of  the  head  and  the subclauses are compiled.  Finally the
bindings made by analyseVariables() are undone and the clause  is  saved
in the heap.

compile() maintains an array of `used_var' (used variables).  This is to
determine when a variable is used for the first time and thus a FIRSTVAR
instruction is to be generated instead of a VAR one.

Note that the `variables' field of a clause is filled with the number of
variables in the frame AND the arity.   This  saves  us  the  frame-size
calculation at runtime.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define isConjunction(w) (isTerm(w) && functorTerm(w) == FUNCTOR_comma2)

#define HEAD    2			/* compileArgument on head argument */
#define HEADARG 3			/* ... on functor arg in head */
#define BODY    4			/* compileArgument on body argument */
#define BODYARG 5			/* ... on functor arg in body */

#define ISVOID 0			/* compileArgument produced H_VOID */
#define NONVOID 1			/* ... anything else */

#define BLOCK(s) do { s; } while (0)

#define Output_0(ci, c)		addBuffer(&(ci)->codes, encode(c), code);
#define Output_a(ci, c)		addBuffer(&(ci)->codes, c, code);
#define Output_1(ci, c, a)	BLOCK(Output_0(ci, c); Output_a(ci, a))
#define Output_2(ci, c, a0, a1)	BLOCK(Output_1(ci, c, a0); Output_a(ci, a1))

#define BITSPERINT (sizeof(int)*8)

#define PC(ci)		entriesBuffer(&(ci)->codes, code)
#define OpCode(ci, pc)	(baseBuffer(&(ci)->codes, code)[pc])

static struct vartable
{ int	entry[MAXVARIABLES/BITSPERINT];
} empty_var_table;

typedef struct
{ Module	module;			/* module to compile into */
  int		arity;			/* arity of top-goal */
  Clause	clause;			/* clause we are constructing */
  struct vartable used_var;		/* boolean array of used variables */
  buffer	codes;			/* scratch code table */
} compileInfo;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable table operations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

forwards bool	compileBody(Word, code, compileInfo *);
forwards int	compileArgument(Word, int, compileInfo *);
forwards bool	compileSubClause(Word, code, compileInfo *);
forwards bool	isFirstVar(struct vartable *vt, int n);
forwards void	balanceVars(struct vartable *, struct vartable *, compileInfo *);
forwards void	orVars(struct vartable *, struct vartable *);
forwards void	setVars(Word t, struct vartable *);
forwards Clause	compile(Word, Word, Module);
#if O_COMPILE_ARITH
forwards int	compileArith(Word, compileInfo *);
forwards bool	compileArithArgument(Word, compileInfo *);
#endif

#define isIndexedVarTerm(var) ( functorTerm(var) == FUNCTOR_var1 ? \
					((struct vardef *)var)->offset : \
					-1)

#define ClearVarTable(ci)	((ci)->used_var = empty_var_table)

static bool
isFirstVar(struct vartable *vt, register int n)
{ register int m  = 1 << (n % BITSPERINT);
  register int *p = &vt->entry[n / BITSPERINT];
  register int result;
  
  result = ((*p & m) == 0);
  *p |= m;

  return result;
}

static void
balanceVars(struct vartable *valt1, struct vartable *valt2, compileInfo *ci)
{ int *p1 = &valt1->entry[0];
  int *p2 = &valt2->entry[0];
  register int n;

  for( n = 0; n < MAXVARIABLES/BITSPERINT; p1++, p2++, n++ )
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
orVars(struct vartable *valt1, struct vartable *valt2)
{ register int *p1 = &valt1->entry[0];
  register int *p2 = &valt2->entry[0];
  register int n;

  for( n = 0; n < MAXVARIABLES/BITSPERINT; n++ )
    *p1++ |= *p2++;
}

static void
setVars(register Word t, register struct vartable *vt)
{ deRef(t);

  if ( isTerm(*t) )
  { int index;
    register int arity;

    if ( (index = isIndexedVarTerm(*t)) >= 0 )
    { isFirstVar(vt, index);
      return;
    }
    arity = functorTerm(*t)->arity;
    for(t = argTermP(*t, 0); arity > 0; t++, arity--)
      setVars(t, vt);
  }
}


static Clause
compile(Word head, Word body, Module module)
{ compileInfo ci;			/* data base for the compiler */
  Procedure proc;
  Clause clause;
  int nvars;

  deRef(head);
  deRef(body);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Split the clause into its head and body and determine the procedure  the
clause should belong to.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if (isAtom(*head) )
    proc = lookupProcedureToDefine(lookupFunctorDef((Atom)*head, 0), module);
  else if (isTerm(*head) )
    proc = lookupProcedureToDefine(functorTerm(*head), module);
  else
  { warning("compiler: illegal clause head");
    return (Clause) NULL;
  }
  if ( !proc )
    return NULL;

  if ( (ci.arity = proc->definition->functor->arity) > MAXARITY )
    return (Clause) warning("Compiler: arity too high (%d)\n", ci.arity);

  DEBUG(9, Sdprintf("Splitted and found proc\n"));

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Allocate the clause and fill initialise the field we already know.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  clause = (Clause) allocHeap(sizeof(struct clause));
  clause->flags      = 0;
  clause->code_size  = 0;
  clause->subclauses = 0;
  clause->procedure  = proc;
  clause->source_no  = clause->line_no = 0;

  DEBUG(9, Sdprintf("clause struct initialised\n"));

  { register Definition def = proc->definition;

    if ( def->indexPattern && !(def->indexPattern & NEED_REINDEX) )
      getIndex(argTermP(*head, 0),
	       def->indexPattern, 
	       def->indexCardinality,
	       &clause->index);
    else
      clause->index.key = clause->index.varmask = 0L;
  }

  TRY( analyse_variables(head, body, ci.arity, &nvars) );
  clause->prolog_vars = clause->variables = nvars + ci.arity;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initialise the `compileInfo' structure.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  initBuffer(&ci.codes);
  ci.module = module;
  ci.clause = clause;
  ClearVarTable(&ci);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
First compile  the  head  of  the  term.   The  arguments  are  compiled
left-to-right. `lastnonvoid' is maintained to delete void variables just
before the I_ENTER instructions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  { int n;
    int lastnonvoid = 0;
    Word arg;

    for ( arg = argTermP(*head, 0), n = 0; n < ci.arity; n++, arg++ )
      if ( compileArgument(arg, HEAD, &ci) == NONVOID )
	lastnonvoid = PC(&ci);
    seekBuffer(&ci.codes, lastnonvoid, code);
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Now compile the body.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( body != (Word) NULL && *body != (word) ATOM_true )
  { Output_0(&ci, I_ENTER);
    compileBody(body, I_DEPART, &ci);
  }
  Output_0(&ci, I_EXIT);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Reset all variables we initialised to the variable analysis  functor  to
become variables again.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  { register struct vardef * vd;
    register int n;

    for(vd=vars, n=0; n < filledVars; n++, vd++)
      if (vd->address != (Word) NULL)
	setVar(*(vd->address));
  }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Finish up the clause.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  { clause->codes = (Code) allocHeap(sizeOfBuffer(&ci.codes));
    memcpy(clause->codes, baseBuffer(&ci.codes, code), sizeOfBuffer(&ci.codes));
    clause->code_size = entriesBuffer(&ci.codes, code);

    discardBuffer(&ci.codes);

    statistics.codes += clause->code_size;
  }

  return clause;
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
    variabled  already  instantiated.   This  gives  troubles  with  the
    FIRSTVAR  type  of instructions.  to avoid such trouble the compiler
    generates  SETVAR  instructions  to  balance  both   brances.    See
    balanceVars();
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
compileBody(register Word body, code call, register compileInfo *ci)
{ deRef(body);

  if ( isTerm(*body) )
  { FunctorDef fd = functorTerm(*body);

    if ( fd == FUNCTOR_comma2 )			/* A , B */
    { TRY( compileBody(argTermP(*body, 0), I_CALL, ci) );
      return compileBody(argTermP(*body, 1), call, ci);
#if O_COMPILE_OR
    } else if ( fd == FUNCTOR_semicolon2 ||
		fd == FUNCTOR_bar2 )		/* A ; B and (A -> B ; C) */
    { register Word a0 = argTermP(*body, 0);
      struct vartable vsave, valt1, valt2;

      vsave = valt1 = valt2 = ci->used_var;
      setVars(argTermP(*body, 0), &valt1);
      setVars(argTermP(*body, 1), &valt2);

      deRef(a0);
      if ( isTerm(*a0) && functorTerm(*a0) == FUNCTOR_ifthen2 ) /* A -> B ; C */
      { int var = VAROFFSET(ci->clause->variables++);
	int tc_or, tc_jmp;

	Output_2(ci, C_IFTHENELSE, var, (code)0);
	tc_or = PC(ci);
	TRY( compileBody(argTermP(*a0, 0), I_CALL, ci) );	
	Output_1(ci, C_CUT, var);
	TRY( compileBody(argTermP(*a0, 1), I_CALL, ci) );	
	balanceVars(&valt1, &valt2, ci);
	Output_1(ci, C_JMP, (code)0);
	tc_jmp = PC(ci);
	OpCode(ci, tc_or-1) = (code)(PC(ci) - tc_or);
	ci->used_var = vsave;
	TRY( compileBody(argTermP(*body, 1), call, ci) );
	balanceVars(&valt2, &valt1, ci);
	OpCode(ci, tc_jmp-1) = (code)(PC(ci) - tc_jmp);
      } else					/* A ; B */
      { int tc_or, tc_jmp;

	Output_1(ci, C_OR, (code)0);
	tc_or = PC(ci);
	TRY( compileBody(argTermP(*body, 0), I_CALL, ci) );
	balanceVars(&valt1, &valt2, ci);
	Output_1(ci, C_JMP, (code)0);
	tc_jmp = PC(ci);
	OpCode(ci, tc_or-1) = (code)(PC(ci) - tc_or);
	ci->used_var = vsave;
	TRY( compileBody(argTermP(*body, 1), call, ci) );
	balanceVars(&valt2, &valt1, ci);
	OpCode(ci, tc_jmp-1) = (code)(PC(ci) - tc_jmp);
      }

      orVars(&valt1, &valt2);
      ci->used_var = valt1;

      succeed;
    } else if ( fd == FUNCTOR_ifthen2 )		/* A -> B */
    { int var = VAROFFSET(ci->clause->variables++);

      Output_1(ci, C_MARK, var);
      TRY( compileBody(argTermP(*body, 0), I_CALL, ci) );
      Output_1(ci, C_CUT, var);

      TRY( compileBody(argTermP(*body, 1), call, ci) );
      Output_0(ci, C_END);
      
      succeed;
    } else if ( fd == FUNCTOR_not_provable1 )		/* \+/1 */
    { int var = VAROFFSET(ci->clause->variables++);
      int tc_or;
      struct vartable vsave;

      vsave = ci->used_var;

      Output_2(ci, C_NOT, var, (code)0);
      tc_or = PC(ci);
      TRY( compileBody(argTermP(*body, 0), I_CALL, ci) );	
      Output_1(ci, C_CUT, var);
      Output_0(ci, C_FAIL);
      OpCode(ci, tc_or-1) = (code)(PC(ci) - tc_or);
      ci->used_var = vsave;
      
      succeed;
#endif /* O_COMPILE_OR */
    }
  }

  TRY( compileSubClause(body, call, ci) );
  ci->clause->subclauses++;

  succeed;
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
compilation.  This is used to detect  the  ...ISVOID,  [I_ENTER,  I_POP]
sequences,  in  which  case  we  can leave out the VOIDS just before the
I_ENTER or I_POP instructions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int lastPopped;		/* how many contiguous pops? */

static int
compileArgument(register Word arg, register int where, register compileInfo *ci)
{ int index;
  bool first;

  lastPopped = 0;		/* going to produce something else */
  deRef(arg);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A void.  Generate either B_VOID or H_VOID.  Note that the  return  value
ISVOID  is reserved for head variables only (B_VOID sets the location to
be a variable, and thus cannot be removed if it is before an I_POP.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( isVar(*arg) )
  { if (where & BODY)
    { Output_0(ci, B_VOID);
      return NONVOID;
    }
    Output_0(ci, H_VOID);
    return ISVOID;
  }


  if ( isAtomic(*arg) )
  { if (isNil(*arg) && (where & HEAD))
    { Output_0(ci, H_NIL);
      return NONVOID;
    }

    if ( isIndirect(*arg) )
    { if ( isReal(*arg) )
      { Output_1(ci, where & HEAD ? H_REAL : B_REAL, copyRealToHeap(*arg));
	return NONVOID;
      }
#if O_STRING
      if ( isString(*arg) )
      { Output_1(ci, where & HEAD ? H_STRING : B_STRING,
		 heapString(valString(*arg)));
	return NONVOID;
      }
#endif /* O_STRING */
    }

    Output_1(ci, (where & BODY) ? B_CONST : H_CONST, *arg);
    return NONVOID;
  }
    
  SECURE(				/* should be a term when here */
	if (!isTerm(*arg))
	  return sysError("Illegal type in compileArgument()"));

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Non-void variables. There are many cases for this.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  if ( (index = isIndexedVarTerm(*arg)) >= 0 )
  { first = isFirstVar(&ci->used_var, index);

    if ( index < ci->arity )		/* variable on its own in the head */
    { switch ( where )
      { case BODY:	if ( index < 3 )
			{ Output_0(ci, B_VAR0 + index);
			  return NONVOID;
			}
			Output_0(ci, B_VAR);	break;
	case BODYARG:	Output_0(ci, B_ARGVAR);	break;
	case HEAD:	if ( first )
			{ Output_0(ci, H_VOID);
			  return ISVOID;
			} /*FALLTHROUGH*/
	case HEADARG:	Output_0(ci, H_VAR);	break;
      }
      Output_a(ci, VAROFFSET(index));
      return NONVOID;
    }

    /* normal variable (i.e. not shared in the head and non-void) */
    switch(where)
    { case BODY:	if ( index < 3 && !first )
			{ Output_0(ci, B_VAR0 + index);
			  return NONVOID;
			}
			Output_0(ci, first ? B_FIRSTVAR    : B_VAR);	break;
      case BODYARG:	Output_0(ci, first ? B_ARGFIRSTVAR : B_ARGVAR); break;
      default:		Output_0(ci, first ? H_FIRSTVAR    : H_VAR);	break;
    }
    Output_a(ci, VAROFFSET(index));
    return NONVOID;
  }

  { int ar;
    int lastnonvoid;
    FunctorDef fdef;

    fdef = functorTerm(*arg);
    if ( fdef == FUNCTOR_dot2 && (where & HEAD) )
    { Output_0(ci, H_LIST);
    } else
    { Output_1(ci, where & BODY ? B_FUNCTOR : H_FUNCTOR, (word)fdef);
    }
    lastnonvoid = PC(ci);
    ar = fdef->arity;
    for(arg = argTermP(*arg, 0); ar > 0; ar--, arg++)
    { if ( compileArgument(arg, (where & BODY) ? BODYARG : HEADARG, ci)
							== NONVOID )
	lastnonvoid = PC(ci);
    }
    seekBuffer(&ci->codes, lastnonvoid, code);
    switch(lastPopped)
    { case 0:		Output_0(ci, I_POP);
    			break;
      case 1:		OpCode(ci, PC(ci)-1) = encode(I_POPN);
			Output_a(ci, 2);
			break;
      default:		OpCode(ci, PC(ci)-1)++;
    }
    lastPopped++;
    return NONVOID;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The task of compileSubClause() is to  generate  code  for  a  subclause.
First  it will call compileArgument for each argument to the call.  Then
an instruction to call the procedure is added.  Before doing all this it
will check for the subclause just beeing a variable or the cut.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
compileSubClause(register Word arg, code call, compileInfo *ci)
{ Module tm = ci->module;

  deRef(arg);

  if ( isTerm(*arg) )
  {
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A non-void variable. Create a I_USERCALL0 instruction for it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    if ( isIndexedVarTerm(*arg) >= 0 )
    { compileArgument(arg, BODY, ci);
      Output_0(ci, I_USERCALL0);
      succeed;
    }

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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    if ( functorTerm(*arg) == FUNCTOR_module2 )
    {
  /*							SEE COMMENT ABOVE
      register Word mp, g;

      mp = argTermP(*arg, 0); deRef(mp);
      if ( isAtom(*mp) )
      { g = argTermP(*arg, 1); deRef(g);
	if ( isIndexedVarTerm(*g) < 0 )
	{ arg = g;
	  tm = lookupModule(*mp);
	  goto cont;
	}
      }
  */

      compileArgument(arg, BODY, ci);
      Output_0(ci, I_USERCALL0);
      succeed;
    }
/*  cont: */

#if O_COMPILE_ARITH
    if ( status.optimise )
    { switch( compileArith(arg, ci) )
      { case A_OK:	succeed;
	case A_ERROR:	fail;
      }
    }
#endif /* O_COMPILE_ARITH */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Term, not a variable and not a module call.  Compile the  arguments  and
generate  the  call  instruction.   Note  this  codes traps the $apply/2
operator.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    { FunctorDef fdef = functorTerm(*arg);
      Procedure proc = lookupProcedure(fdef, tm);
      int ar = fdef->arity;

#ifdef O_INLINE_FOREIGNS
#define MAX_FV 2
      if ( true(fdef, INLINE_F) && ar <= MAX_FV )
      { int n;
	int vars[MAX_FV];

	for(n = 0; n < ar; n++)
	{ Word a = argTermP(*arg, n);

	  deRef(a);
	  if ( isTerm(*a) && (vars[n] = isIndexedVarTerm(*a)) >= 0 )
	    continue;

	  goto non_fv;
	}

	for(n = 0; n < ar; n++)
	{ if ( isFirstVar(&ci->used_var, vars[n]) )
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
	compileArgument(arg, BODY, ci);

      if ( fdef->name == ATOM_call )
      { Output_1(ci, I_USERCALLN, (code)(fdef->arity - 1));
	succeed;
      } else if ( fdef == FUNCTOR_apply2 )
      { Output_0(ci, I_APPLY);
	succeed;
#if O_BLOCK
      } else if ( fdef == FUNCTOR_dcut1 )
      { Output_0(ci, I_CUT_BLOCK);
	succeed;
      } else if ( fdef == FUNCTOR_dexit2 )
      { Output_0(ci, B_EXIT);
	succeed;
#endif
      }
      Output_1(ci, call, (code) proc);

      succeed;
    }
  }

  if ( isAtom(*arg) )
  { if ( *arg == (word) ATOM_cut )
    { Output_0(ci, I_CUT);
    } else if ( *arg == (word) ATOM_true )
    { Output_0(ci, I_TRUE);
    } else if ( *arg == (word) ATOM_fail )
    { Output_0(ci, I_FAIL);
    } else
    { FunctorDef fdef = lookupFunctorDef((Atom)*arg, 0);
      code cproc = (code) lookupProcedure(fdef, tm);

#ifdef O_INLINE_FOREIGNS
      if ( true(fdef, INLINE_F) )
      { Output_1(ci, I_CALL_FV0, cproc);
      } else
#endif /*O_INLINE_FOREIGNS*/
      { Output_1(ci, call, cproc);
      }
    }

    succeed;
  }
    
  return warning("assert/1: illegal clause");
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
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if O_COMPILE_ARITH
static int
compileArith(Word arg, compileInfo *ci)
{ code a_func;
  FunctorDef fdef = functorTerm(*arg);

  if      ( fdef == FUNCTOR_ar_equals2 )	a_func = A_EQ;	/* =:= */
  else if ( fdef == FUNCTOR_ar_not_equal2 )	a_func = A_NE;	/* =\= */
  else if ( fdef == FUNCTOR_smaller2 )	 	a_func = A_LT;	/* < */
  else if ( fdef == FUNCTOR_larger2 )		a_func = A_GT;	/* > */
  else if ( fdef == FUNCTOR_smaller_equal2 )	a_func = A_LE;	/* =< */
  else if ( fdef == FUNCTOR_larger_equal2 )	a_func = A_GE;	/* >= */
  else if ( fdef == FUNCTOR_is2 )				/* is */
  { Word a = argTermP(*arg, 1);

    deRef(a);
    if ( isTerm(*a) && isIndexedVarTerm(*a) >= 0 ) /* variable */
      return A_NOTARITH;		/* X is Var: don't just unify */

    a_func = A_IS;
  } else
    return A_NOTARITH;			/* not arith function */

  if ( compileArithArgument(argTermP(*arg, 0), ci) == FALSE )
    return A_ERROR;
  if ( compileArithArgument(argTermP(*arg, 1), ci) == FALSE )
    return A_ERROR;
  Output_0(ci, a_func);

  return A_OK;
}


static bool
compileArithArgument(register Word arg, register compileInfo *ci)
{ int index;

  deRef(arg);

  if ( isInteger(*arg) )		/* integer */
  { Output_1(ci, B_CONST, *arg);
    succeed;
  }
  if ( isReal(*arg) )			/* real (does not need to copy!) */
  { Output_1(ci, A_REAL, copyRealToHeap(*arg));
    succeed;
  }
					/* variable */
  if ( isTerm(*arg) && (index = isIndexedVarTerm(*arg)) >= 0 )
  { int first = isFirstVar(&ci->used_var, index);

    if ( index < ci->arity )	/* shared in the head */
    { if ( index < 3 )
      { Output_0(ci, B_VAR0 + index);
	succeed;
      }
      Output_0(ci, B_VAR);
    } else
    { if ( index < 3 && !first )
      { Output_0(ci, B_VAR0 + index);
        succeed;
      }
      Output_0(ci, first ? B_FIRSTVAR : B_VAR);
    }          
    Output_a(ci, VAROFFSET(index));
    succeed;
  }

  if ( isVar(*arg) )			/* void variable */
  { Output_0(ci, B_VOID);
    succeed;
  }

  { FunctorDef fdef;
    int ar;
    Word a;

    if ( isAtom(*arg) )
    { fdef = lookupFunctorDef((Atom)*arg, 0);
      ar = 0;
      a = NULL;
    } else if ( isTerm(*arg) )
    { fdef = functorTerm(*arg);
      ar = fdef->arity;
      a = argTermP(*arg, 0);      
    } else
      return warning("Illegal argument to arithmic function");

    if ( (index = indexArithFunction(fdef, ci->module)) < 0 )
      return warning("%s/%d: unknown arithmetic operator",
		     stringAtom(fdef->name), fdef->arity);

    for(; ar > 0; a++, ar--)
      TRY( compileArithArgument(a, ci) );

    switch(fdef->arity)
    { case 0:	Output_1(ci, A_FUNC0, index); break;
      case 1:	Output_1(ci, A_FUNC1, index); break;
      case 2:	Output_1(ci, A_FUNC2, index); break;
      default:  Output_2(ci, A_FUNC,  index, (code) fdef->arity); break;
    }

    succeed;
  }
}
#endif /* O_COMPILE_ARITH */


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
assert_term(term_t term, int where, Atom file)
{ Clause clause;
  Procedure proc;
  Definition def;
  Module source_module = (file ? modules.source : (Module) NULL);
  Module module = source_module;
  term_t tmp  = PL_new_term_ref();
  term_t head = PL_new_term_ref();
  term_t body = PL_new_term_ref();

  if ( !PL_strip_module(term, &module, tmp) ||
       !get_head_and_body_clause(tmp, head, body, &module) )
  { warning("compiler: illegal clause");
    return (Clause) NULL;
  }

  DEBUG(9, Sdprintf("compiling "); pl_write(term); Sdprintf(" ... "););
  if ( !(clause = compile(valTermRef(head), valTermRef(body), module)) )
    return NULL;
  DEBUG(9, Sdprintf("ok\n"));
  clause->line_no = source_line_no;
  proc = clause->procedure;
  def = proc->definition;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If file is defined, we are called from record_clause/2.  This code takes
care of reconsult, redefinition, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( file )
  { SourceFile sf;

    sf = lookupSourceFile(file);
    clause->line_no   = source_line_no;
    clause->source_no = sf->index;

    if ( def->module != module )
    { if ( true(def->module, SYSTEM) )
        warning("Attempt to redefine a system predicate: %s", 
		procedureName(proc));
      else
	warning("%s/%d already imported from module %s", 
		stringAtom(def->functor->name), 
		def->functor->arity, 
		stringAtom(proc->definition->module->name) );
      freeClause(clause);
      return NULL;
    }

    if ( proc == sf->current_procedure )
      return assertProcedure(proc, clause, where) ? clause : NULL;

    if ( def->definition.clauses )	/* i.e. is defined */
    { if ( true(def, LOCKED) && !SYSTEM_MODE && false(def, DYNAMIC|MULTIFILE) )
      { warning("Attempt to redefine a system predicate: %s",
		procedureName(proc));
	freeClause(clause);
	return NULL;
      }

      if ( true(def, FOREIGN) )
      { abolishProcedure(proc, module);
	warning("Redefined: foreign predicate %s", procedureName(proc));
      }

      if ( false(def, MULTIFILE) )
      { ClauseRef first = def->definition.clauses;

	if ( first && first->clause->source_no == sf->index )
	{ if ( (debugstatus.styleCheck & DISCONTIGUOUS_STYLE) &&
	       false(def, DISCONTIGUOUS) )
	    warning("Clauses of %s are not together in the source file", 
		    procedureName(proc));
	} else
	{ abolishProcedure(proc, module);
	  warning("Redefined: %s", procedureName(proc));
	}
      }

      addProcedureSourceFile(sf, proc);
      sf->current_procedure = proc;
      
      return assertProcedure(proc, clause, where) ? clause : NULL;
    }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This `if' locks predicates as system predicates  if  we  are  in  system
mode, the predicate is still undefined and is not dynamic or multifile.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

    if ( SYSTEM_MODE && false(def, SYSTEM) )
      set(def, SYSTEM|HIDE_CHILDS|LOCKED);

    addProcedureSourceFile(sf, proc);
    sf->current_procedure = proc;
    return assertProcedure(proc, clause, where) ? clause : NULL;
  }

  /* assert[az]/1 */

  if ( def->module != module && false(def, DYNAMIC) )
  { warning("Attempt to redefine an imported predicate %s", 
			      procedureName(proc) );
    freeClause(clause);
    return (Clause) NULL;
  }
  set(def, DYNAMIC);			/* Make dynamic on first assert */

  return assertProcedure(proc, clause, where) == FALSE ? (Clause) NULL
						       : clause;
}

word
pl_assertz(term_t term)
{ return assert_term(term, CL_END, (Atom)NULL) == NULL ? FALSE : TRUE;
}

word
pl_asserta(term_t term)
{ return assert_term(term, CL_START, (Atom)NULL) == NULL ? FALSE : TRUE;
}


word
pl_assertz2(term_t term, term_t ref)
{ Clause clause = assert_term(term, CL_END, (Atom)NULL);

  if (clause == (Clause)NULL)
    fail;

  return PL_unify_pointer(ref, clause);
}


word
pl_asserta2(term_t term, term_t ref)
{ Clause clause = assert_term(term, CL_START, (Atom)NULL);

  if (clause == (Clause)NULL)
    fail;

  return PL_unify_pointer(ref, clause);
}


word
pl_record_clause(term_t term, term_t file, term_t ref)
{ Clause clause;
  Atom f;

  if ( !PL_get_atom(file, &f) )
    fail;

  if ( (clause = assert_term(term, CL_END, f)) )
    return PL_unify_pointer(ref, clause);
  
  fail;
}  


word
pl_redefine_system_predicate(term_t pred)
{ Procedure proc;
  Module m = NULL;
  FunctorDef fd;
  term_t head = PL_new_term_ref();

  if ( !PL_strip_module(pred, &m, head) ||
       !PL_get_functor(head, &fd) )
    return warning("redefine_system_predicate/1: instantiation fault");

  proc = lookupProcedure(fd, m);
  abolishProcedure(proc, m);

  succeed;
}


		/********************************
		*          DECOMPILER           *
		*********************************/

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
  Word   xr;				/* xr table for decompilation */
  Word	 argp;				/* argument pointer */
  term_t variables[MAXVARIABLES];	/* variable table */
} decompileInfo;

forwards bool	decompile_head(Clause, term_t, decompileInfo *);
forwards bool	decompileBody(decompileInfo *, code, Code);
forwards void	build_term(FunctorDef, decompileInfo *);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
decompileArg1()  is  a  simplified  version   of  decompileHead().   Its
function is to extract the relevant   information  for (re)computing the
index information for indexing on the   first argument (the 99.9% case).
See reindexClause().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
arg1Key(Clause clause, word *key)
{ Code APC = clause->codes;

  for(;;)
  { switch(decode(*APC++))
    { case H_FUNCTOR:
      case H_CONST:
	*key = XR(*APC);
	succeed;
      case H_NIL:
	*key = (word) ATOM_nil;
        succeed;
      case H_LIST:
	*key = (word) FUNCTOR_dot2;
#if O_STRING
      case H_STRING:
#endif
      case H_REAL:
      case H_FIRSTVAR:
      case H_VAR:
      case H_VOID:
      case I_EXIT:			/* fact */
      case I_ENTER:			/* fix H_VOID, H_VOID, I_ENTER */
	fail;
      case I_NOP:
	continue;
      default:
	assert(0);
        fail;
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
decompileHead()  is  public  as  it  is   needed  to  update  the  index
information for clauses if this changes   when  the predicate is already
defined.  Also for intermediate  code  file   loaded  clauses  the index
information is recalculated as the constants   may  be different accross
runs.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define setHandle(h, w)		(*valTermRef(h) = (w))
#define valHandleP(h)		valTermRef(h)

static inline word
valHandle(term_t r)
{ Word p = valTermRef(r);

  deRef(p);
  return *p;
}

bool
decompileHead(Clause clause, term_t head)
{ decompileInfo di;

  return decompile_head(clause, head, &di);
}


static void
get_arg_ref(term_t term, term_t argp)
{ word w = valHandle(term);
  setHandle(argp, makeRef(argTermP(w, 0)));
}


static void
next_arg_ref(term_t argp)
{ Word p = valTermRef(argp);
  
  *p = makeRef(unRef(*p)+1);
}


static int
diff_ref(term_t t1, term_t t2)
{ Word p1 = valTermRef(t1);
  Word p2 = valTermRef(t2);

  return unRef(*p1) - unRef(*p2);
}


static bool
unifyVar(Word var, term_t *vars, int i)
{ DEBUG(3, Sdprintf("unifyVar(%d, %d, %d)\n", var, vars, i) );
  if ( !vars[i] )
  { term_t v = PL_new_term_ref();
    setHandle(v, makeRef(var));
    vars[i] = v;
    succeed;
  }

  return unify_ptrs(var, valTermRef(vars[i]));
}


static bool
decompile_head(Clause clause, term_t head, decompileInfo *di)
{ int arity;
  term_t argp0;
  term_t argp;
  Definition def = clause->procedure->definition;

  { int m, l;
    term_t *p;

    l = VAROFFSET(0);
    m = VAROFFSET(clause->variables);	/* index of highest var + 1 */
    for(p = di->variables + l; m-- > l;)
      *p++ = PL_new_term_ref();
  }

  argp0 = PL_new_term_ref();
  argp  = PL_new_term_ref();

  DEBUG(5, Sdprintf("Decompiling head of %s\n", predicateName(def)));
  arity = def->functor->arity;
  TRY( PL_unify_functor(head, def->functor) );
  get_arg_ref(head, argp);
  get_arg_ref(head, argp0);
  PC = clause->codes;

  for(;;)
  { switch(decode(*PC++))
    { case I_NOP:
	  continue;
      case H_NIL:
	  TRY(PL_unify_nil(argp));
          next_arg_ref(argp);
	  continue;
      case H_REAL:
        { word copy = copyRealToGlobal(XR(*PC++));
	  TRY(_PL_unify_atomic(argp, copy));
	  next_arg_ref(argp);
	  continue;
	}
#if O_STRING
      case H_STRING:
	  TRY(PL_unify_string_chars(argp, valString(XR(*PC++))));
	  next_arg_ref(argp);
	  continue;
#endif /* O_STRING */
      case H_CONST:
	  TRY(_PL_unify_atomic(argp, XR(*PC++)));
	  next_arg_ref(argp);
	  continue;
      case H_FIRSTVAR:
      case H_VAR:
	  TRY(unifyVar(valTermRef(argp), di->variables, *PC++) );
	  next_arg_ref(argp);
	  continue;
      case H_VOID:
	{ int arg = diff_ref(argp, argp0); /* FIRSTVAR in the head */
	  if ( arg < arity && arg >= 0)
	    TRY(unifyVar(valTermRef(argp), di->variables, VAROFFSET(arg)) );
	  next_arg_ref(argp);
	  continue;
	}
      case H_FUNCTOR:
	{ FunctorDef fdef = (FunctorDef) XR(*PC++);
	  term_t t2;

      common_functor:
	  t2 = PL_new_term_ref();
	  TRY(PL_unify_functor(argp, fdef));
          get_arg_ref(argp, t2);
          next_arg_ref(argp);
	  argp = t2;
	  continue;
      case H_LIST:
	  fdef = FUNCTOR_dot2;
          goto common_functor;
	}
      case I_POP:
	  PL_reset_term_refs(argp);
          argp--;
	  continue;
      case I_POPN:
	  argp -= *PC++;
          PL_reset_term_refs(argp+1);
	  continue;
      case I_EXIT:			/* fact */
      case I_ENTER:			/* fix H_VOID, H_VOID, I_ENTER */
	{ int arg = diff_ref(argp, argp0);

	  for(; arg < arity; arg++)
	  { TRY(unifyVar(valTermRef(argp), di->variables, VAROFFSET(arg)));
	    next_arg_ref(argp);
	  }

	  succeed;
	}
      default:
	  sysError("Illegal instruction in clause head: %d = %d",
		   PC[-1], decode(PC[-1]));
	  fail;
    }
  }
}

#define isVarRef(w)	(isRef(w) && (int)unRef(w) < MAXVARIABLES \
						? (int)unRef(w) : -1)

bool
decompile(Clause clause, term_t term)
{ decompileInfo dinfo;
  decompileInfo *di = &dinfo;
  Word body;

#ifdef O_RUNTIME
  if ( false(clause->procedure->definition, DYNAMIC) )
    fail;
#endif

  if ( clause->subclauses == 0 )	/* fact */
  { return decompileHead(clause, term);
  } else
  { term_t a = PL_new_term_ref();

    TRY(PL_unify_functor(term, FUNCTOR_prove2));
    PL_get_arg(1, term, a);
    TRY(decompile_head(clause, a, di));
    PL_get_arg(2, term, a);
    body = valTermRef(a);
    deRef(body);
  }

  ARGP = (Word) lTop;

  decompileBody(di, I_EXIT, (Code) NULL);

  { Word b;
    int var;

    b = newTerm();
    ARGP--;
    if ( (var = isVarRef(*ARGP)) >= 0 )
      unifyVar(b, di->variables, var);
    else
      *b = *ARGP;

    return unify_ptrs(body, b);
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
decompileBody(register decompileInfo *di, code end, Code until)
{ int nested = 0;		/* nesting in FUNCTOR ... POP */
  int pushed = 0;		/* Subclauses pushed on the stack */
  int op;

  for(; decode(*PC) != end && PC != until; )
  { switch((op=decode(*PC++)))
    {   case I_NOP:	    continue;
	case B_CONST:
			    *ARGP++ = XR(*PC++);
			    continue;
	case B_REAL:
			    *ARGP++ = copyRealToGlobal(XR(*PC++));
			    continue;
	case B_STRING:
			    *ARGP++ = globalString(valString(XR(*PC++)));
			    continue;
      { register int index;      

	case B_ARGVAR:
	case B_ARGFIRSTVAR:
	case B_FIRSTVAR:
	case B_VAR:	    index = *PC++;		goto var_common;
	case B_VAR0:	    index = VAROFFSET(0);	goto var_common;
	case B_VAR1:	    index = VAROFFSET(1);	goto var_common;
	case B_VAR2:	    index = VAROFFSET(2);	var_common:
			    if ( nested )
			      unifyVar(ARGP++, di->variables, index);
			    else
			      *ARGP++ = makeRef(index);
			    continue;
      }
      case B_VOID:
			    setVar(*ARGP++);
			    continue;
      case B_FUNCTOR:
			    *ARGP = globalFunctor((FunctorDef)XR(*PC++));
			    *aTop++ = ARGP + 1;
			    verifyStack(argument);
			    ARGP = argTermP(*ARGP, 0);
			    nested++;
			    continue;
      case I_POP:
			    ARGP = *--aTop;
			    nested--;
			    continue;
      case I_POPN:
			    aTop -= *PC;
			    nested -= *PC++;
			    ARGP = *aTop;
			    continue;
#if O_COMPILE_ARITH
      case A_FUNC0:
      case A_FUNC1:
      case A_FUNC2:
			    build_term(functorArithFunction(*PC++), di);
			    continue;
      case A_FUNC:
      			    build_term(functorArithFunction(*PC++), di);
      			    PC++;
			    continue;
#endif /* O_COMPILE_ARITH */
      { FunctorDef f;
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
        case I_USERCALLN:   f = lookupFunctorDef(ATOM_call, *PC++ + 1);
							goto f_common;
	case I_APPLY:	    f = FUNCTOR_apply2;		f_common:
			    build_term(f, di);
			    pushed++;
			    continue;
      }
      case I_FAIL:	    *ARGP++ = (word) ATOM_fail;
			    pushed++;
			    continue;
      case I_TRUE:	    *ARGP++ = (word) ATOM_true;
			    pushed++;
			    continue;
      case I_CUT:	    *ARGP++ = (word) ATOM_cut;
			    pushed++;
			    continue;
      case I_DEPART:
      case I_CALL:        { Procedure proc = (Procedure)XR(*PC++);
			    build_term(proc->definition->functor, di);
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
	  
	  *ARGP++ = makeRef(index);
	}
	build_term(((Procedure)XR(*PC))->definition->functor, di);
	pushed++;
	PC += vars+1;
	continue;
      }
#endif /*O_INLINE_FOREIGNS*/
#if O_COMPILE_OR
#define DECOMPILETOJUMP { int to_jump = (int) *PC++; \
			  decompileBody(di, (code)-1, PC+to_jump); \
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
			    build_term(FUNCTOR_semicolon2, di);
			    pushed++;
			    continue;
      case C_NOT:				/* \+ A */
			  { PC += 2;		/* skip the two arguments */
			    decompileBody(di, C_CUT, (Code)NULL);   /* A */
			    PC += 3;		/* skip C_CUT <n> and C_FAIL */
			    build_term(FUNCTOR_not_provable1, di);
			    pushed++;
			    continue;
			  }
      case C_IFTHENELSE:			/* A -> B ; C */
			  { Code adr1;
			    int jmp;

			    PC++;		/* skip the 'MARK' variable */
			    jmp  = (int) *PC++;
			    adr1 = PC+jmp;

			    decompileBody(di, C_CUT, (Code)NULL);   /* A */
			    PC += 2;		/* skip the cut */
			    decompileBody(di, (code)-1, adr1);	    /* B */
			    build_term(FUNCTOR_ifthen2, di);
			    PC--;
			    DECOMPILETOJUMP;	/* C */
			    build_term(FUNCTOR_semicolon2, di);
			    pushed++;
			    continue;
			  }
      case C_MARK:				/* A -> B */
			    PC++;
			    decompileBody(di, C_CUT, (Code)NULL);   /* A */
			    PC += 2;
			    decompileBody(di, C_END, (Code)NULL);   /* B */
			    PC++;
			    build_term(FUNCTOR_ifthen2, di);
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
    build_term(FUNCTOR_comma2, di);

  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Build the actual term.  The arguments are on  the  decompilation  stack.
We  construct a term of requested arity and name, copy `arity' arguments
from the stack into the term and finally  push  the  term  back  on  the
stack.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
build_term(register FunctorDef f, register decompileInfo *di)
{ word term;
  int arity;
  register Word a;

  if ( f->arity == 0 )
  { *ARGP++ = (word) f->name;
    return;
  }    

  term = globalFunctor(f);
  arity = f->arity;
  a = argTermP(term, arity-1);

  ARGP--;
  for( ; arity-- > 0; a--, ARGP-- )
  { register int var;

    if ( (var = isVarRef(*ARGP)) >= 0 )
      unifyVar(a, di->variables, var);
    else
      *a = *ARGP;
  }
  ARGP++;

  *ARGP++ = term;
}

#undef PC
#undef ARGP

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unify_definition(?Head, +Def, -TheHead)
    Given some definition, unify its Prolog reference (i.e. its head with
    optional module specifier) with ?Head.  If TheHead is specified, the
    plain head (i.e. without module specifier) will be referenced from
    this term-reference.

    This function properly deals with module-inheritance, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
unify_definition(term_t head, Definition def, term_t thehead)
{ if ( PL_is_variable(head) )
  { if ( def->module == MODULE_user )
    { PL_unify_functor(head, def->functor);
      if ( thehead )
	PL_put_term(thehead, head);
    } else
    { term_t tmp = PL_new_term_ref();
      
      PL_unify_functor(head, FUNCTOR_module2);
      PL_get_arg(1, head, tmp);
      PL_unify_atom(tmp, def->module->name);
      PL_get_arg(2, head, tmp);
      PL_unify_functor(tmp, def->functor);
      if ( thehead )
	PL_put_term(thehead, tmp);
    }

    succeed;
  } else
  { term_t h = PL_new_term_ref();
    Module m = NULL;

    if ( !PL_strip_module(head, &m, h) ||
	 !isSuperModule(def->module, m) )
      fail;

    if ( PL_unify_functor(h, def->functor) )
    { if ( thehead )
	PL_put_term(thehead, h);
      succeed;
    }

    fail;
  }
}


word
pl_clause(term_t p, term_t term, term_t ref, word h)
{ Procedure proc;
  Definition def;
  Clause clause;
  ClauseRef cref;
  Module module = NULL;

  if ( ForeignControl(h) == FRG_CUTTED )
    succeed;

  if ( PL_get_pointer(ref, (void **)&clause) ) /* clause(H, B, 2733843) */
  { Module defModule;
    term_t tmp  = PL_new_term_ref();
    term_t head = PL_new_term_ref();
    term_t body = PL_new_term_ref();
    FunctorDef f;

    if ( !inCore(clause) || !isClause(clause) )
      return warning("clause/3: Invalid reference");
	
    if ( !decompile(clause, term) )
      fail;

    proc = clause->procedure;
    def = proc->definition;
    defModule = def->module;

    if ( PL_get_functor(term, &f) && f == FUNCTOR_module2 )
    { PL_strip_module(p, &module, tmp);
      if ( module != defModule )
	fail;
    }

    if ( !unify_definition(p, def, tmp) )
      fail;

    get_head_and_body_clause(term, head, body, NULL);

    return PL_unify(tmp, head);
  } else
  { Word argv;

    if ( ForeignControl(h) == FRG_FIRST_CALL)
    { if ( !get_procedure(p, &proc, 0, GP_FIND) ||
	   true(proc->definition, FOREIGN) )
	fail;
      def = proc->definition;
      cref = def->definition.clauses;
    } else
    { cref = (ClauseRef) ForeignContextAddress(h);
      proc = cref->clause->procedure;
      def  = proc->definition;
    }
  
    if ( def->functor->arity > 0 )
    { term_t head = PL_new_term_ref();

      PL_strip_module(p, &module, head);
      argv = valTermRef(head);
      deRef(argv);
      argv = argTermP(*argv, 0);
    } else
      argv = NULL;

    for(; cref; cref = cref->next)
    { bool det;
  
      if ( !(cref = findClause(cref, argv, def, &det)) )
	fail;
  
      if ( !decompile(cref->clause, term) )
	continue;
      if ( !PL_unify_pointer(ref, cref->clause) )
	continue;
  
      if ( det == TRUE )
	succeed;

      ForeignRedo(cref->next);
    }
  }

  fail;
}


typedef struct
{ ClauseRef clause;			/* pointer to the clause */
  int       index;			/* nth-1 index */
} crref, *Cref;


word
pl_nth_clause(term_t p, term_t n, term_t ref, word h)
{ Clause clause;
  ClauseRef cref;
  Procedure proc;
  Definition def;
  Cref cr;

  if ( ForeignControl(h) == FRG_CUTTED )
  { cr = (Cref) ForeignContextAddress(h);
    def = cr->clause->clause->procedure->definition;
    leaveDefinition(def);
    freeHeap(cr, sizeof(crref));
    succeed;
  }

  if ( PL_get_pointer(ref, (void **)&clause) )
  { int i;

    if (!inCore(clause) || !isClause(clause))
      return warning("nth_clause/3: Invalid integer reference");
	
    proc = clause->procedure;
    def  = proc->definition;
    for( cref = def->definition.clauses, i=1; cref; cref = cref->next, i++)
    { if ( cref->clause == clause )
      { if ( !PL_unify_integer(n, i) ||
	     !unify_definition(p, def, 0) )
	  fail;

	succeed;
      }
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
    while ( cref && true(cref->clause, ERASED) )
      cref = cref->next;
    
    if ( !cref )
      fail;

    if ( PL_get_integer(n, &i) )		/* proc and n specified */
    { while(i > 1 && cref)
      { do
	{ cref = cref->next;
	} while ( cref && true(cref->clause, ERASED) );

	i--;
      }
      if ( i == 1 )
	return PL_unify_pointer(ref, cref->clause);
      fail;
    }

    cr = allocHeap(sizeof(crref));
    cr->clause = cref;
    cr->index  = 1;
    def->references++;
  } else
  { cr = (Cref) ForeignContextAddress(h);
    def = cr->clause->clause->procedure->definition;
  }

  PL_unify_integer(n, cr->index);
  PL_unify_pointer(ref, cr->clause->clause);

  cref = cr->clause->next;
  while ( cref && true(cref->clause, ERASED) )
    cref = cref->next;

  if ( cref )
  { cr->clause = cref;
    cr->index++;
    ForeignRedo(cr);
  }

  freeHeap(cr, sizeof(crref));
  leaveDefinition(def);

  succeed;
}


word
pl_xr_member(term_t ref, term_t term, word h)
{ Clause clause;
  Code PC;
  Code end;

  if ( ForeignControl(h) == FRG_CUTTED )
    succeed;

  if ( !PL_get_pointer(ref, (void **)&clause) ||
       !inCore(clause) || !isClause(clause) )
    return warning("$xr_member/2: Invalid reference");

  PC  = clause->codes;
  end = &PC[clause->code_size];

  if ( PL_is_variable(term) )
  { int an;				/* argument-n */

    if ( ForeignControl(h) == FRG_FIRST_CALL)
    { an = 0;
    } else
    { long i = ForeignContext(h);
      an = i % 4;
      PC += i / 4;
    }

    while( PC < end )
    { bool rval;
      code op = decode(*PC++);
      
      for( ; an < codeTable[op].externals; an++ )
      { word xr = PC[an];

	if ( isAtomic(xr) )
	  rval = _PL_unify_atomic(term, xr);
	else if ( isProcedure(xr) )
	  rval = unify_definition(term, ((Procedure)xr)->definition, 0);
	else
	  rval = PL_unify_functor(term, (FunctorDef)xr);

	if ( rval )
	{ long i = PC - clause->codes - 1; /* compensate ++ above! */

	  ForeignRedo((i * 4) + an + 1);
	}
      }
      PC += codeTable[op].arguments;
      an = 0;
    }

    fail;
  } else				/* instantiated */
  { Procedure proc;
    FunctorDef fd;

    if ( PL_is_atomic(term) )
    { while( PC < end )
      { int an = 0;
	code op = decode(*PC++);

	for( ; an < codeTable[op].externals; an++ )
	{ word xr = PC[an];

	  if ( isAtomic(xr) && _PL_unify_atomic(term, xr) )
	    succeed;
	}

	PC += codeTable[op].arguments;
      }
    } else if ( get_procedure(term, &proc, 0, GP_FIND) )
    { while( PC < end )
      { int an = 0;
	code op = decode(*PC++);

	for( ; an < codeTable[op].externals; an++ )
	{ word xr = PC[an];

	  if ( !isAtomic(xr) &&
	       isProcedure(xr) &&
	       ((Procedure)xr)->definition == proc->definition )
	    succeed;
	}

	PC += codeTable[op].arguments;
      }
    } else if ( PL_get_functor(term, &fd) )
    { word target = (word) fd;

      while( PC < end )
      { int an = 0;
	code op = decode(*PC++);

	for( ; an < codeTable[op].externals; an++ )
	{ word xr = PC[an];

	  if ( xr == target )
	    succeed;
	}

	PC += codeTable[op].arguments;
      }
    }
  }

  fail;
}

		 /*******************************
		 *	     WAM_LIST		*
		 *******************************/

#define VARNUM(i) ((i) - (ARGOFFSET / (int) sizeof(word)))

word
pl_wam_list(term_t ref)
{ Clause clause;
  Code bp, ep;

  if ( !PL_get_pointer(ref, (void **)&clause) ||
       !inCore(clause) || !isClause(clause) )
    return warning("$wam_list/1: Invalid reference");

  bp = clause->codes;
  ep = bp + clause->code_size;

  while( bp < ep )
  { code op = decode(*bp++);
    CodeInfo ci = &codeTable[op];
    int n = 0;

    Putf("%4d %s", bp - 1 - clause->codes, ci->name);

    switch(op)
    { case B_FIRSTVAR:
      case B_ARGFIRSTVAR:
      case B_VAR:
      case B_ARGVAR:
      case H_VAR:
      case C_VAR:
      case C_MARK:
      case C_CUT:			/* var */
	assert(ci->arguments == 1);
	Putf(" var(%d)", VARNUM(*bp++));
	break;
      case C_IFTHENELSE:		/* var, jump */
      case C_NOT:
      { int var = VARNUM(*bp++);
	int jmp = *bp++;
	assert(ci->arguments == 2);
        Putf(" var(%d), jmp(%d)", var, jmp);
        break;
      }
      case I_CALL_FV1:
      case I_CALL_FV2:
      { int vars = op - I_CALL_FV0;
	Procedure proc = (Procedure) *bp++;

	Putf(" %s", procedureName(proc));
	for( ; vars > 0; vars-- )
	  Putf(", var(%d)", VARNUM(*bp++));
        break;
      }
      default:
	for( ; n < codeTable[op].externals; n++ )
	{ word xr = *bp++;

	  Putf("%s", n == 0 ? " " : ", ");
	  if ( isInteger(xr) )
	    Putf("%d", valNum(xr));
	  else if ( isReal(xr) )
	    Putf("%f", valReal(xr));
	  else if ( isString(xr) )
	    Putf("\"%s\"", valString(xr));
	  else if ( isAtom(xr) )
	    Putf("'%s'", stringAtom(xr));
	  else if ( ((FunctorDef)xr)->type == FUNCTOR_TYPE )
	  { FunctorDef f = (FunctorDef) xr;
	    
	    Putf("%s/%d", stringAtom(f->name), f->arity);
	  } else
	  { Procedure p = (Procedure) xr;
	    
	    Putf("%s", procedureName(p));
	  }
	}
        for( ; n < codeTable[op].arguments; n++ )
	  Putf("%s%d", n == 0 ? " " : ", ", *bp++);
    }

    Putf("\n");
  }

  succeed;
}
