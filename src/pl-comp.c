/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2022, University of Amsterdam
                              VU University Amsterdam
			      CWI, Amsterdam
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
#define _GNU_SOURCE			/* get dladdr() */
#include "pl-incl.h"
#include "pl-comp.h"
#include "pl-arith.h"
#include "pl-dbref.h"
#include "pl-event.h"
#include "pl-inline.h"
#include "pl-tabling.h"
#include "pl-supervisor.h"
#include "pl-gvar.h"
#include "pl-fli.h"
#include "pl-write.h"
#include "pl-prims.h"
#include "pl-util.h"
#include "pl-read.h"
#include "pl-modul.h"
#include "pl-proc.h"
#include "pl-wam.h"
#include "pl-funct.h"
#include "pl-srcfile.h"
#include "pl-gc.h"
#include "pl-index.h"
#include "pl-setup.h"
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
static void	cleanupMerge(void);

static void
checkCodeTable(void)
{ const code_info *ci;
  int n;

  if ( sizeof(struct clause) % sizeof(word) != 0 )
    sysError("Invalid alignment of struct clause");

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
initWamTable(DECL_LD)
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

  dewam_table = (unsigned char *)PL_malloc_atomic(((maxcoded-dewam_table_offset) + 1) *
						  sizeof(char));

  for(n = 0; n < I_HIGHEST; n++)
  { int index = wam_table[n]-dewam_table_offset;
    dewam_table[index] = (unsigned char) 0;
  }
  for(n = 0; n < I_HIGHEST; n++)
  { int index = wam_table[n]-dewam_table_offset;
    if ( dewam_table[index] )		/* See SEPARATE_VMI */
      fatalError("WAM Table mismatch: wam_table[%d(%s)] == wam_table[%d(%s)]\n",
		 dewam_table[index], codeTable[dewam_table[index]].name,
		 n,		     codeTable[n].name);
    dewam_table[index] = (unsigned char) n;
  }

  checkCodeTable();
  initSupervisors();
  initVMIMerge();
}

void
cleanupWamTable(void)
{ free(dewam_table);
  dewam_table = NULL;
  cleanupMerge();
}

/* See SEPARATE_VMI */

void
separate_vmi(int nop)
{
}

#else /* VMCODE_IS_ADDRESS */

void
initWamTable(DECL_LD)
{ checkCodeTable();
  initSupervisors();
  initVMIMerge();
}

void
cleanupWamTable(void)
{ cleanupMerge();
}

#endif /* VMCODE_IS_ADDRESS */


		 /*******************************
		 *     WARNING DECLARATIONS	*
		 *******************************/

#define CW_MAX_ARGC 3

typedef struct cw_def
{ const char *name;
  int	      argc;
} cw_def;

#define CW(name, argc) { name, argc }

const static cw_def cw_defs[] =
{ CW("eq_vv",              2),		/* Var == Var */
  CW("eq_singleton",       2),		/* SingleTon == ? */
  CW("neq_vv",             2),		/* Var \== Var */
  CW("neq_singleton",	   2),		/* SingleTon \== ? */
  CW("unify_singleton",    2),		/* SingleTon = ? */
  CW("always",             1),		/* always(Bool, pred(Arg)) */
  CW("nonvar_false",       1),		/* nonvar(SingleTonOrFirst) */
  CW("unbalanced_var",     1),		/* Var initialised in some disjunctions */
  CW("branch_singleton",   1),		/* Singleton in some branch */
  CW("negation_singleton", 1),		/* Singleton in \+(Goal) */
  CW("multiton",	   1),		/* Multiple _Name variables */
  CW("integer_false",      1),		/* integer(VarOrNonInt) */
  CW("integer_true",       1),		/* integer(Integer) */
  CW(NULL,                 0)
};


typedef struct c_warning
{ const cw_def* def;	/* Warning definition */
  size_t	pc;	/* PC offset in clause */
  int		argc;	/* Argument count */
  Word		argv[CW_MAX_ARGC]; /* context arguments */
  term_t	av;	  /* handle argument vector */
  struct c_warning *next; /* next warning */
} c_warning;


		 /*******************************
		 *	  PORTABLE CHECK	*
		 *******************************/

#if SIZEOF_VOIDP == 4
#define is_portable_smallint(w) (tagex(w) == (TAG_INTEGER|STG_INLINE))
#define is_portable_constant(w) isConst(w)
#else
#define is_portable_smallint(w) LDFUNC(is_portable_smallint, w)
#define is_portable_constant(w) (isAtom(w) || is_portable_smallint(w))

#define PORTABLE_INT_MASK 0xffffffff80000000

static inline int
is_portable_smallint(DECL_LD word w)
{ if ( tagex(w) == (TAG_INTEGER|STG_INLINE) )
  { if ( truePrologFlag(PLFLAG_PORTABLE_VMI) )
    { word masked = w&PORTABLE_INT_MASK;

      return !masked || masked == PORTABLE_INT_MASK;
    } else
    { return TRUE;
    }
  } else
    return FALSE;
}

#endif

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
performance is to distinguish between the first time an  entry  from the
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

#define isVarInfo(w)	(tagex(w) == (TAG_VAR|STG_RESERVED))
#define setVarInfo(w,i)	(w = (((word)(i))<<LMASK_BITS)|TAG_VAR|STG_RESERVED)
#define varIndex(w)	((w)>>LMASK_BITS)
#define varInfo(w)	(LD->comp.vardefs[varIndex(w)])


typedef struct _varDef
{ word		functor;		/* mimic a functor (FUNCTOR_dvard1) */
  word		saved;			/* saved value */
  Word		address;		/* address of the variable */
  Word		arg_value;		/* Value unified against */
  atom_t	name;			/* name (if available) */
  int		times;			/* occurrences */
  int		offset;			/* offset in environment frame */
  int		flags;			/* VD_* */
} vardef;

#define VD_MAYBE_SINGLETON  0x01
#define VD_SINGLETON        0x02
#define VD_MAYBE_UNBALANCED 0x04
#define VD_UNBALANCED	    0x08
#define VD_ARGUMENT	    0x10	/* Unified against an argument */

typedef struct
{ int	isize;
  int	entry[1];
} var_table, *VarTable;

#undef struct_offsetp
#define struct_offsetp(t, f) ((size_t)((t*)0)->f)
#define sizeofVarTable(isize) (struct_offsetp(var_table, entry) + sizeof(int)*(isize))

#define mkCopiedVarTable(o) copyVarTable(alloca(sizeofVarTable(o->isize)), o)
#define BITSPERINT (int)(sizeof(int)*8)

typedef struct branch_var
{ VarDef	vdef;			/* Definition record */
  int		saved_times;		/* Times saved from left branch */
  int		saved_flags;		/* Flags saved from left branch */
} branch_var;

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
  Procedure	procedure;		/* Procedure it belongs to */
  int		arity;			/* arity of top-goal */
  int		vartablesize;		/* size of the vartable */
  int		islocal;		/* Temporary local clause */
  int		subclausearg;		/* processing subclausearg */
  int		argvars;		/* islocal argument pseudo vars */
  int		argvar;			/* islocal current pseudo var */
  int		singletons;		/* Marked singletons in disjunctions */
  int		head_unify;		/* In unifications against arguments */
  cutInfo	cut;			/* how to compile ! */
  merge_state	mstate;			/* Instruction merging state */
  VarTable	used_var;		/* boolean array of used variables */
  Buffer	branch_vars;		/* We are in a branch */
  target_module colon_context;		/* Context:Goal */
#ifdef O_CALL_AT_MODULE
  target_module	at_context;		/* Call@Context */
#endif
  term_t	warning_list;		/* see compiler_warning() */
  c_warning    *warnings;
  tmp_buffer	branch_varbuf;		/* Store for branch_vars */
  tmp_buffer	codes;			/* scratch code table */
} compileInfo, *CompileInfo;


#define link_local_var(v, iv, ci) LDFUNC(link_local_var, v, iv, ci)
static int link_local_var(DECL_LD Word v, int iv, CompileInfo ci);


		 /*******************************
		 *	      WARNINGS		*
		 *******************************/

static int
compiler_warning(CompileInfo ci, const char *name, ...)
{ c_warning *w;
  const cw_def *def;

  if ( !ci->warning_list )
    return TRUE;

  for(def = cw_defs; def->name; def++)
  { if ( strcmp(def->name,name) == 0 )
      break;
  }

  if ( !def->name )
  { sysError("Undefined compiler warning: %s", name);
    return FALSE;				/* not reached */
  }

  if ( (w=allocHeap(sizeof(*w))) )
  { va_list args;
    int i;

    memset(w, 0, sizeof(*w));
    w->def = def;
    w->pc  = entriesBuffer(&ci->codes, code);
    va_start(args, name);
    if ( strcmp(name, "always") == 0 )
    { GET_LD
      Word val, pred;

      if ( gTop + 2 > gMax )
	return GLOBAL_OVERFLOW;

      val  = gTop++;
      pred = gTop++;
      *val  = va_arg(args, atom_t);
      *pred = PL_new_atom(va_arg(args, const char *));
      w->argv[w->argc++] = val;
      w->argv[w->argc++] = pred;
    }
    for(i=0; i<def->argc; i++)
      w->argv[w->argc++] = va_arg(args, Word);
    va_end(args);

    w->next = ci->warnings;
    ci->warnings = w;
  }

  return TRUE;
}


static void
free_compiler_warnings(CompileInfo ci)
{ c_warning *cw, *next;

  for(cw=ci->warnings; cw; cw=next)
  { next = cw->next;

    free(cw);
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
push_compiler_warnings() turns compiler warning  terms   into  a  Prolog
list. We do this in two steps.  First, we create term-references for the
Word pointers. From that moment, we can perform   GC,  so we can use all
normal functionality.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define push_compiler_warnings(ci) LDFUNC(push_compiler_warnings, ci)
static int
push_compiler_warnings(DECL_LD CompileInfo ci)
{ PL_put_nil(ci->warning_list);

  if ( ci->warnings )
  { c_warning *cw;
    term_t tmp = PL_new_term_ref();

    for(cw=ci->warnings; cw; cw=cw->next)
    { if ( (cw->av=PL_new_term_refs(cw->argc)) )
      { int i;

	for(i=0; i<cw->argc; i++)
	  *valTermRef(cw->av+i) = linkValI(cw->argv[i]);
      } else
      { free_compiler_warnings(ci);
	return LOCAL_OVERFLOW;
      }
    }

    for(cw=ci->warnings; cw; cw=cw->next)
    { functor_t f;
      atom_t name = PL_new_atom(cw->def->name);
      int rc;

      rc = ((f=PL_new_functor(name, cw->argc)) &&
	    PL_cons_functor_v(tmp, f, cw->av) &&
	    PL_cons_list(ci->warning_list, tmp, ci->warning_list));

      PL_unregister_atom(name);
      if ( !rc )
      { free_compiler_warnings(ci);
	return FALSE;
      }
    }

    free_compiler_warnings(ci);
  }

  return TRUE;
}


		 /*******************************
		 *	    VARIABLES		*
		 *******************************/

#define resetVars(_) LDFUNC(resetVars, _)
static void	resetVars(DECL_LD);

#define getVarDef(i) LDFUNC(getVarDef, i)
static VarDef
getVarDef(DECL_LD int i)
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


#define resetVarDefs(n) LDFUNC(resetVarDefs, n)
static void
resetVarDefs(DECL_LD int n)		/* set addresses of first N to NULL */
{ VarDef *vd;
  int nvd = LD->comp.nvardefs;

  if ( n > nvd )			/* allocates them */
    getVarDef(n-1);

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
  { VarDef *vardefs = ld->comp.vardefs;
    int i, count=ld->comp.nvardefs;

    ld->comp.vardefs = NULL;
    ld->comp.nvardefs = 0;
    ld->comp.filledVars = 0;

    for(i=0; i<count; i++)
    { if ( vardefs[i] )
	freeHeap(vardefs[i], sizeof(vardef));
    }

    GC_FREE(vardefs);
  }
}


static void
pushBranchVar(CompileInfo ci, VarDef v)
{ branch_var bv;

  bv.vdef = v;
  bv.saved_times = 0;
  bv.saved_flags = 0;
  addBuffer(ci->branch_vars, bv, branch_var);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_variable_names() fetches the  global   variable  $variable_names and
updates the vardef records that match.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define get_variable_names(ci) LDFUNC(get_variable_names, ci)
static int
get_variable_names(DECL_LD CompileInfo ci)
{ word w;
  int found = 0;

  if ( gvar_value(ATOM_dvariable_names, &w) )
  { Word p = &w;

    deRef(p);
    while(isList(*p))
    { Word b = argTermP(*p, 0);

      deRef(b);
      if ( hasFunctor(*b, FUNCTOR_equals2) )
      { Word n = argTermP(*b, 0);
	Word v = argTermP(*b, 1);

	deRef(n);
	deRef(v);
	if ( isAtom(*n) && isVarInfo(*v) )
	{ VarDef vd = varInfo(*v);
	  vd->name = *n;
	  found++;
	}
      }

      p = argTermP(*p, 1);
      deRef(p);
    }
  }

  return found;
}


#define is_neck(t, flags) LDFUNC(is_neck, t, flags)
static int
is_neck(DECL_LD term_t t, int *flags)
{ Word p = valTermRef(t);

  deRef(p);
  if ( isTerm(*p) )
  { functor_t f = functorTerm(*p);

    if ( f == FUNCTOR_prove2 )      { return TRUE; }
    if ( f == FUNCTOR_ssu_commit2 ) { *flags = SSU_COMMIT_CLAUSE; return TRUE; }
    if ( f == FUNCTOR_ssu_choice2 ) { *flags = SSU_CHOICE_CLAUSE; return TRUE; }
  }

  return FALSE;
}


int
get_head_and_body_clause(DECL_LD term_t clause,
			 term_t head, term_t body, Module *m,
			 int *flags)
{ Module m0;

  if ( !m )
  { m0 = NULL;
    m = &m0;
  }

  if ( is_neck(clause, flags) )
  { _PL_get_arg(1, clause, head);
    _PL_get_arg(2, clause, body);
    if ( !PL_strip_module_ex(head, m, head) )
      return FALSE;
  } else
  { PL_put_term(head, clause);		/* facts */
    PL_put_atom(body, ATOM_true);
    *flags = UNIT_CLAUSE;
  }

  DEBUG(9, pl_write(clause); Sdprintf(" --->\n\t");
	   Sdprintf("%s:", *m ? stringAtom((*m)->name) : "(nil)");
	   pl_write(head); Sdprintf(" :- "); pl_write(body); Sdprintf("\n"));

  return TRUE;
}


#define is_argument_var(p, ci) LDFUNC(is_argument_var, p, ci)
static VarDef
is_argument_var(DECL_LD Word p, CompileInfo ci)
{ deRef(p);

  if ( isVarInfo(*p) )
  { int index = varIndex(*p);

    if ( index < ci->arity )
      return varInfo(*p);
  }

  return NULL;
}

#define annotate_unify(p1, p2, ci) LDFUNC(annotate_unify, p1, p2, ci)
static int
annotate_unify(DECL_LD Word p1, Word p2, CompileInfo ci)
{ VarDef vd;

  if ( (vd=is_argument_var(p1, ci)) )
  { deRef(p2);

    if ( false(vd, VD_ARGUMENT) && !isVarInfo(*p2) && !isVar(*p2) )
    { set(vd, VD_ARGUMENT);
      vd->arg_value = p2;

      DEBUG(MSG_COMP_ARG_UNIFY,
	    { Word p;
	      deRef2(p1, p);

	      Sdprintf("Annotated unification against arg %d\n",
		       varIndex(*p)+1);
	    });

      return TRUE;
    }
  }

  return FALSE;
}

#define annotate_unification(f, ci) LDFUNC(annotate_unification, f, ci)
static int
annotate_unification(DECL_LD Functor f, CompileInfo ci)
{ return ( annotate_unify(&f->arguments[0], &f->arguments[1], ci) ||
	   annotate_unify(&f->arguments[1], &f->arguments[0], ci) );
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Analyse the variables of a clause.  `term' is the term to  be  analysed,
which  is  either  a  fact  or  a  clause (:-/2) term.  First of all the
functor and arity of the predicate are determined.   The  first  `arity'
elements  of  the variable definition array are then cleared.  This part
is used for sharing variables that occur  on their own in the head  with
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

@param control indicates we are processing toplevel control structures
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAX_VARIABLES 1000000000	/* stay safely under signed int */
#define AVARS_CYCLIC    -1
/*	MEMORY_OVERFLOW -5 */
#define AVARS_MAX      -12

static int
in_branch(const branch_var *from, const branch_var *to, const Word v)
{ for(; from<to; from++)
  { if ( from->vdef->address == v )
      return TRUE;
  }

  return FALSE;
}


#define analyseVariables2(head, nvars, argn, ci, depth, control) LDFUNC(analyseVariables2, head, nvars, argn, ci, depth, control)
static int
analyseVariables2(DECL_LD Word head, int nvars, int argn,
		  CompileInfo ci, int depth, int control)
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
	resetVars();
	return AVARS_MAX;
      }

      index = ci->arity + nvars++;
    }

    vd = getVarDef(index);
    vd->saved   = *head;
    vd->address = head;
    vd->name    = (atom_t)0;
    vd->flags   = 0;
    vd->times   = 1;
    setVarInfo(*head, index);
    if ( ci->branch_vars )
      pushBranchVar(ci, vd);

    return nvars;
  }

  if ( isVarInfo(*head) )
  { VarDef vd = varInfo(*head);

    if ( vd->times++ == 0 && ci->branch_vars )
    { pushBranchVar(ci, vd);
    } else
    { if ( (debugstatus.styleCheck&VARBRANCH_CHECK) )
      { if ( true(vd, VD_MAYBE_UNBALANCED) && false(vd, VD_UNBALANCED) )
	{ set(vd, VD_UNBALANCED);
	  compiler_warning(ci, "unbalanced_var", vd->address);
	}
      }
      if ( (debugstatus.styleCheck&SEMSINGLETON_CHECK) )
      { if ( true(vd, VD_MAYBE_SINGLETON) )
	{ assert(vd->times > 1);
	  DEBUG(MSG_COMP_VARS,
		Sdprintf("Not a singleton: %p\n", vd->address));
	  clear(vd, VD_MAYBE_SINGLETON);
	  ci->singletons--;
	}
      }
    }

    return nvars;
  }

  if ( isTerm(*head) )
  { Functor f = valueTerm(*head);
    FunctorDef fd = valueFunctor(f->definition);
    int rc;

    if ( ++depth == 10000 && (rc=is_acyclic(head)) != TRUE )
    { LD->comp.filledVars = ci->arity+nvars;
      resetVars();

      return rc == FALSE ? AVARS_CYCLIC : rc;
    }

    if ( ci->islocal )
    { if ( ci->subclausearg )
      { DEBUG(MSG_COMP_ARGVAR,
	      Sdprintf("argvar for %s\n", functorName(f->definition)));
	ci->argvars++;

	return nvars;
      } else if ( false(fd, CONTROL_F) )
      { size_t ar = fd->arity;

	ci->subclausearg++;
	for(head = f->arguments, argn = ci->arity; ar-- > 0; head++, argn++)
	{ nvars = analyseVariables2(head, nvars, argn, ci, depth, FALSE);
	  if ( nvars < 0 )
	    break;			/* error */
	}
	ci->subclausearg--;

	return nvars;
      } /* ci->local && control functor --> fall through to normal case */
    }

    /* Check for singletons in branches (A;B).  These are variables
       introduced in a branch, used only once in the branch and not
       used in code after the branches re-unite.
    */

    if ( f->definition == FUNCTOR_semicolon2 && control && !ci->islocal )
    { Buffer obv;
      ssize_t start_vars, at_branch_vars, at_end_vars;

      ci->head_unify = FALSE;

      if ( (obv=ci->branch_vars) == NULL )
      { initBuffer(&ci->branch_varbuf);
	ci->branch_vars = (Buffer)&ci->branch_varbuf;
	start_vars = 0;
      } else
	start_vars = entriesBuffer(ci->branch_vars, branch_var);

      DEBUG(MSG_COMP_VARS, Sdprintf("Branch; start_vars = %d\n", start_vars));

      nvars = analyseVariables2(&f->arguments[0], nvars, argn,
				ci, depth, control);
      if ( nvars < 0 )
	goto error;

      at_branch_vars = entriesBuffer(ci->branch_vars, branch_var);

      if ( at_branch_vars > start_vars )
      { branch_var *bv = baseBuffer(ci->branch_vars, branch_var);
	branch_var *bve;

	DEBUG(MSG_COMP_VARS, Sdprintf("Reset %d vars after left branch\n",
				      at_branch_vars - start_vars));

	bve = bv + at_branch_vars;
	for(bv += start_vars; bv < bve; bv++)
	{ bv->saved_times = bv->vdef->times;
	  bv->saved_flags = bv->vdef->flags;
	  DEBUG(MSG_COMP_VARS, Sdprintf("times=%d, flags = 0x%x\n",
					bv->vdef->times, bv->vdef->flags));
	  bv->vdef->times = 0;
	  bv->vdef->flags = 0;
	}
      } else
      { DEBUG(MSG_COMP_VARS, Sdprintf("No vars in left branch\n"));
      }

      nvars = analyseVariables2(&f->arguments[1], nvars, argn,
				ci, depth, control);
      if ( nvars < 0 )
	goto error;

      at_end_vars = entriesBuffer(ci->branch_vars, branch_var);

      if ( at_end_vars > start_vars )
      { branch_var *bv0 = baseBuffer(ci->branch_vars, branch_var);
	branch_var *bv, *bve;

	DEBUG(MSG_COMP_VARS, Sdprintf("Analyse %d vars\n",
				      at_end_vars - start_vars));

	bve = bv0 + at_end_vars;
	for(bv = bv0+start_vars; bv < bve; bv++)
	{ VarDef vd = bv->vdef;

	  DEBUG(MSG_COMP_VARS,
		Sdprintf("\t %p: saved: t/f=%d/%d; t/f=%d/%d\n",
			 vd->address,
			 bv->saved_times, bv->saved_flags,
			 vd->times, vd->flags));

	  if ( (debugstatus.styleCheck&VARBRANCH_CHECK) )
	  { if ( bv->saved_times > 0 && vd->times == 0 )
	      set(vd, VD_MAYBE_UNBALANCED); /* in left, not in right */
	    if ( vd->times > 0 &&
		 !in_branch(bv0+start_vars, bv0+at_branch_vars, vd->address) )
	      set(vd, VD_MAYBE_UNBALANCED); /* in right, not in left */
	  }
	  if ( (debugstatus.styleCheck&SEMSINGLETON_CHECK) )
	  { if ( (bv->saved_times == 1) || (vd->times == 1) )
	    { if ( false(vd, VD_MAYBE_SINGLETON) )
	      { set(vd, VD_MAYBE_SINGLETON);
		DEBUG(MSG_COMP_VARS,
		      Sdprintf("Possible singleton: %p\n", vd->address));
		ci->singletons++;
	      }
	    }
	  }
	  if ( vd->times < bv->saved_times )
	    vd->times = bv->saved_times;
	  vd->flags |= bv->saved_flags;		/* TBD: Dubious */
	  bv->saved_times = 0;
	  bv->saved_flags = 0;
	}
      }

    error:
      if ( obv == NULL )
      { discardBuffer(ci->branch_vars);
	ci->branch_vars = NULL;
      }

      return nvars;
    }

    /* check \+ Goal for singletons on Goal.  These are variables introduced
       inside the goal and only used once.
    */

    if ( f->definition == FUNCTOR_not_provable1 && control && !ci->islocal)
    { Buffer obv;
      ssize_t start_vars, at_end_vars;

      ci->head_unify = FALSE;

      if ( (obv=ci->branch_vars) == NULL )
      { initBuffer(&ci->branch_varbuf);
	ci->branch_vars = (Buffer)&ci->branch_varbuf;
	start_vars = 0;
      } else
	start_vars = entriesBuffer(ci->branch_vars, branch_var);

      nvars = analyseVariables2(&f->arguments[0], nvars, argn,
				ci, depth, control);
      if ( nvars < 0 )
	goto error_in_not;

      at_end_vars = entriesBuffer(ci->branch_vars, branch_var);
      if ( at_end_vars > start_vars )
      { branch_var *bv = baseBuffer(ci->branch_vars, branch_var);
	branch_var *bve;

	bve = bv + at_end_vars;
	for(bv += start_vars; bv < bve; bv++)
	{ VarDef vd = bv->vdef;

	  if ( vd->times == 1 )
	  { set(vd, VD_SINGLETON);
	    ci->singletons++;
	  }
	}
      }

    error_in_not:
      if ( obv == NULL )
      { discardBuffer(ci->branch_vars);
	ci->branch_vars = NULL;
      } else
      { seekBuffer(ci->branch_vars, start_vars, branch_var);
      }

      return nvars;
    }

    /* Find leading unifications against head arguments */

    if ( control && ci->head_unify )
    { if ( f->definition == FUNCTOR_equals2 )
	annotate_unification(f, ci);
      else if ( f->definition != FUNCTOR_comma2 )
	ci->head_unify = FALSE;
    }

    /* The default term processing case */

    if ( fd->arity > 0 )
    { size_t ar = fd->arity;

      head = f->arguments;
      argn = ( argn < 0 ? 0 : ci->arity );

      if ( control && false(fd, CONTROL_F) )
	control = FALSE;

      for(; --ar > 0; head++, argn++)
      { nvars = analyseVariables2(head, nvars, argn, ci, depth, control);
	if ( nvars < 0 )
	  return nvars;
      }

      goto right_recursion;
    }
  }

  if ( control && *head != ATOM_true )	  /* e.g. atomic goals */
    ci->head_unify = FALSE;

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

#define analyse_variables(head, body, ci) LDFUNC(analyse_variables, head, body, ci)
static int
analyse_variables(DECL_LD Word head, Word body, CompileInfo ci)
{ int nv, nvars = 0;
  int n;
  int body_voids = 0;
  int arity = ci->arity;

  if ( arity > 0 )
    resetVarDefs(arity);

  ci->branch_vars = NULL;
  ci->singletons  = 0;

  if ( head )
  { if ( (nvars = analyseVariables2(head, 0, -1, ci, 0, FALSE)) < 0 )
      return nvars == AVARS_CYCLIC ? CYCLIC_HEAD : nvars;
  }
  if ( body )
  { if ( (nvars = analyseVariables2(body, nvars, arity, ci, 0, TRUE)) < 0 )
      return nvars == AVARS_CYCLIC ? CYCLIC_BODY : nvars;
  }

  if ( ci->warning_list	)
    get_variable_names(ci);

  for(n=0; n<arity+nvars; n++)
  { VarDef vd = LD->comp.vardefs[n];

    assert(vd->functor == FUNCTOR_dvard1);
    if ( !vd->address )
      continue;
    if ( vd->name && (debugstatus.styleCheck&SEMSINGLETON_CHECK) )
    { if ( true(vd, VD_MAYBE_SINGLETON|VD_SINGLETON) &&
	   atom_is_named_var(vd->name) > 0 )
      { const char *type = ( true(vd, VD_MAYBE_SINGLETON) ?
				  "branch_singleton" : "negation_singleton" );
	compiler_warning(ci, type, vd->address);
      } else if ( vd->times > 1 && atom_is_named_var(vd->name) < 0 )
	compiler_warning(ci, "multiton", vd->address);
    }
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
  ci->vartablesize	  = (int)((nv + BITSPERINT-1)/BITSPERINT);

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
#define MAX_ARITY_OVERFLOW -11		/* return value for max_procedure_arity overflow */

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

#if USE_LD_MACROS
#define	compileBody(Word, code, compileInfo)		LDFUNC(compileBody, Word, code, compileInfo)
#define	compileArgument(Word, int, compileInfo)		LDFUNC(compileArgument, Word, int, compileInfo)
#define	compileListFF(arg, ci)				LDFUNC(compileListFF, arg, ci)
#define	compileSimpleAddition(Word, compileInfo)	LDFUNC(compileSimpleAddition, Word, compileInfo)
#define	compileArith(Word, compileInfo)			LDFUNC(compileArith, Word, compileInfo)
#define	compileArithArgument(Word, compileInfo)		LDFUNC(compileArithArgument, Word, compileInfo)
#define	compileBodyUnify(arg, ci)			LDFUNC(compileBodyUnify, arg, ci)
#define	compileBodyEQ(arg, ci)				LDFUNC(compileBodyEQ, arg, ci)
#define	compileBodyNEQ(arg, ci)				LDFUNC(compileBodyNEQ, arg, ci)
#define	compileBodyArg3(arg, ci)			LDFUNC(compileBodyArg3, arg, ci)
#define	compileBodyVar1(arg, ci)			LDFUNC(compileBodyVar1, arg, ci)
#define	compileBodyNonVar1(arg, ci)			LDFUNC(compileBodyNonVar1, arg, ci)
#define	compileBodyTypeTest(functor, arg, ci)		LDFUNC(compileBodyTypeTest, functor, arg, ci)
#define	compileBodyCallContinuation(arg, ci)		LDFUNC(compileBodyCallContinuation, arg, ci)
#define	compileBodyShift(arg, ci, for_copy)		LDFUNC(compileBodyShift, arg, ci, for_copy)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

forwards int	compileBody(Word, code, compileInfo *);
forwards int	compileArgument(Word, int, compileInfo *);
forwards int	compileSubClause(Word, code, compileInfo *);
forwards bool	isFirstVarSet(VarTable vt, int n);
forwards int	balanceVars(VarTable, VarTable, compileInfo *);
forwards void	orVars(VarTable, VarTable);
forwards int	compileListFF(word arg, compileInfo *ci);
forwards bool	compileSimpleAddition(Word, compileInfo *);
#if O_COMPILE_ARITH
forwards int	compileArith(Word, compileInfo *);
forwards bool	compileArithArgument(Word, compileInfo *);
#endif
#if O_COMPILE_IS
forwards int	compileBodyUnify(Word arg, compileInfo *ci);
forwards int	compileBodyEQ(Word arg, compileInfo *ci);
forwards int	compileBodyNEQ(Word arg, compileInfo *ci);
forwards int	compileBodyArg3(Word arg, compileInfo *ci);
#endif
forwards int	compileBodyVar1(Word arg, compileInfo *ci);
forwards int	compileBodyNonVar1(Word arg, compileInfo *ci);
forwards int	compileBodyTypeTest(functor_t functor, Word arg,
				    compileInfo *ci);
forwards int	compileBodyCallContinuation(Word arg, compileInfo *ci);
forwards int	compileBodyShift(Word arg, compileInfo *ci, int for_copy);
static void	initMerge(CompileInfo ci);
static int	mergeInstructions(CompileInfo ci, const vmi_merge *m, vmi c);
static int	try_fast_condition(CompileInfo ci, size_t tc_or);

#undef LDFUNC_DECLARATIONS

#define isIndexedVarTerm(w) LDFUNC(isIndexedVarTerm, w)
static inline int
isIndexedVarTerm(DECL_LD word w)
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


#define argUnifiedTo(w) LDFUNC(argUnifiedTo, w)
static Word
argUnifiedTo(DECL_LD word w)
{ VarDef v = varInfo(w);

  if ( true(v, VD_ARGUMENT) )
    return v->arg_value;

  return NULL;
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

#define setVars(t, vt) LDFUNC(setVars, t, vt)
static void
setVars(DECL_LD Word t, VarTable vt)
{ int index;

last_arg:

  deRef(t);
  if ( (index = isIndexedVarTerm(*t)) >= 0 )
  { isFirstVarSet(vt, index);
    return;
  }

  if ( isTerm(*t) )
  { ssize_t arity;

    arity = arityTerm(*t);

    for(t = argTermP(*t, 0); --arity > 0; t++)
      setVars(t, vt);
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
resetVars(DECL_LD)
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

#define isFirstVarP(p, ci, i) LDFUNC(isFirstVarP, p, ci, i)
static int
isFirstVarP(DECL_LD Word p, compileInfo *ci, int *i)
{ int idx;

  deRef(p);
  if ( (idx=isIndexedVarTerm(*p)) >= 0 &&
       idx >= ci->arity &&
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

#define getTargetModule(tm, t, ci) LDFUNC(getTargetModule, tm, t, ci)
static int
getTargetModule(DECL_LD target_module *tm, Word t, CompileInfo ci)
{ int iv;

  deRef(t);
  if ( (iv=isIndexedVarTerm(*t)) >= 0 )
  { if ( ci->islocal || !isFirstVar(ci->used_var, iv) )
    { if ( ci->islocal )
      { int rc;

	if ( (rc=link_local_var(t, iv, ci)) != TRUE )
	  return rc;
      }
      tm->var_index = iv;
      tm->type = TM_VAR;
    } else
    { PL_error(NULL, 0, NULL, ERR_INSTANTIATION);
      return FALSE;
    }
  } else if ( isTextAtom(*t) )
  { tm->module = lookupModule(*t);
    tm->type = TM_MODULE;
    if ( !ci->islocal && tm->module->class == ATOM_temporary )
      return PL_error(NULL, 0, "temporary module", ERR_PERMISSION,
		      ATOM_reference, ATOM_module, pushWordAsTermRef(t));

  } else
  { resetVars();
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
  { Output_1(ci, B_ATOM, tm->module->name);
    PL_register_atom(tm->module->name);
  } else					/* TBD: Handle islocal */
  { int index = tm->var_index;

    Output_1(ci, B_ARGVAR, VAROFFSET(index));   /* Writing to a @ or : term */
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

  mergeSeq(H_VOID,   H_VOID,	   H_VOID_N,	 1, (code)2);
  mergeSeq(H_VOID,   I_ENTER,	   I_ENTER,	 0);
  mergeSeq(H_VOID_N, I_ENTER,	   I_ENTER,	 0);
  mergeSeq(H_VOID,   I_EXITFACT,   I_EXITFACT,	 0);
  mergeSeq(H_VOID_N, I_EXITFACT,   I_EXITFACT,	 0);
  mergeSeq(H_VOID,   I_SSU_COMMIT, I_SSU_COMMIT, 0);
  mergeSeq(H_VOID_N, I_SSU_COMMIT, I_SSU_COMMIT, 0);
  mergeSeq(H_VOID,   I_SSU_CHOICE, I_SSU_CHOICE, 0);
  mergeSeq(H_VOID_N, I_SSU_CHOICE, I_SSU_CHOICE, 0);
  mergeSeq(H_VOID,   H_POP,	   H_POP,	 0);
  mergeSeq(H_VOID_N, H_POP,	   H_POP,	 0);
}


static void
cleanupMerge(void)
{ for(int i=0; i<I_HIGHEST; i++)
  { if ( merge_def[i] )
    { free(merge_def[i]);
      merge_def[i] = NULL;
    }
  }
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
compileClause()

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

#define compileClauseGuarded(ci, cp, head, body, proc, module, warnings, flags) \
	LDFUNC(compileClauseGuarded, ci, cp, head, body, proc, module, warnings, flags)
static int compileClauseGuarded(DECL_LD CompileInfo ci, Clause *cp, Word head, Word body,
				Procedure proc, Module module, term_t warnings,
				int flags);

#ifdef O_C_STACK_GUARDED
#define cleanupCompile(ci) LDFUNC(cleanupCompile, ci)

static void
cleanupCompile(DECL_LD CompileInfo ci)
{ resetVars();
  discardBuffer(&ci->codes);
}
#endif

int
compileClause(DECL_LD Clause *cp, Word head, Word body,
	      Procedure proc, Module module, term_t warnings,
	      int flags)
{ compileInfo ci;			/* data base for the compiler */
  int rc;

  initBuffer(&ci.codes);

  C_STACK_OVERFLOW_GUARDED(
      rc,
      compileClauseGuarded(&ci, cp, head, body, proc, module, warnings, flags),
      cleanupCompile(&ci));

  return rc;
}

static int
compileClauseGuarded(DECL_LD CompileInfo ci, Clause *cp, Word head, Word body,
		     Procedure proc, Module module, term_t warnings,
		     int flags)
{ struct clause clause = {0};
  Clause cl;
  Definition def = getProcDefinition(proc);
  int rc;

  if ( head )
  { ci->islocal      = FALSE;
    ci->subclausearg = 0;
    ci->arity        = (int)def->functor->arity;
    ci->procedure    = proc;
    ci->argvars      = 0;
    ci->head_unify   = ( (flags&SSU_CHOICE_CLAUSE) ||
			( !(flags & (SSU_COMMIT_CLAUSE)) &&
			  false(def, P_DYNAMIC) &&
			  truePrologFlag(PLFLAG_OPTIMISE_UNIFY)
			)
		      );
    clause.flags    = flags & (SSU_COMMIT_CLAUSE|SSU_CHOICE_CLAUSE);
  } else
  { Word g = varFrameP(lTop, VAROFFSET(1));

    ci->islocal      = TRUE;
    ci->subclausearg = 0;
    ci->argvars	    = 1;
    ci->argvar       = 1;
    ci->arity        = 0;
    ci->procedure    = NULL;		/* no LCO */
    ci->head_unify   = FALSE;
    clause.flags    = GOAL_CLAUSE;
    *g		    = *body;
  }

  clause.predicate  = def;

  ci->clause = &clause;
  ci->module = module;
  ci->colon_context.type = TM_NONE;
#ifdef O_CALL_AT_MODULE
  ci->at_context.type = TM_NONE;
#endif
  ci->warning_list    = warnings;
  ci->warnings        = NULL;

  rc = analyse_variables(head, body, ci);

  if ( rc < 0 )
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
  ci->cut.var = 0;
  ci->cut.instruction = 0;
  if ( !ci->islocal )
  { ci->used_var = alloca(sizeofVarTable(ci->vartablesize));
    clearVarTable(ci);
  } else
    ci->used_var = NULL;

  initMerge(ci);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
First compile  the  head  of  the  term.   The  arguments  are  compiled
left-to-right.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  if ( flags & (SSU_COMMIT_CLAUSE|SSU_CHOICE_CLAUSE) )
    Output_0(ci, I_CHP);

  if ( head )
  { int n;
    Word arg;

    for ( arg = argTermP(*head, 0), n = 0; n < ci->arity; n++, arg++ )
    { if ( (rc=compileArgument(arg, A_HEAD, ci)) < 0 )
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

      switch(flags & (SSU_COMMIT_CLAUSE|SSU_CHOICE_CLAUSE))
      { case 0:
	  Output_0(ci, I_ENTER);
	  break;
	case SSU_COMMIT_CLAUSE:
	  Output_0(ci, I_SSU_COMMIT);
	  break;
	case SSU_CHOICE_CLAUSE:
	  Output_0(ci, I_SSU_CHOICE);
	  break;
        default:
	  assert(0);
      }
					/* ok; all live in the same module */
      if ( false(def, P_MFCONTEXT) &&
	   ci->module != def->module &&
	   false(proc->definition, P_TRANSPARENT) )
	set(def, P_MFCONTEXT);

      if ( true(def, P_MFCONTEXT) )
      { set(&clause, CL_BODY_CONTEXT);
	Output_1(ci, I_CONTEXT, (code)ci->module);
      }
    }

    bi = PC(ci);
    if ( (rc=compileBody(body, I_DEPART, ci)) != TRUE )
    { if ( rc <= NOT_CALLABLE )
      {	resetVars();
	switch(rc)
	{ case NOT_CALLABLE:
	    rc = PL_error(NULL, 0, NULL, ERR_TYPE,
			  ATOM_callable, pushWordAsTermRef(body));
	    popTermRef();
	    break;
	  case MAX_ARITY_OVERFLOW:
	    rc = PL_error(NULL, 0, NULL,
			  ERR_REPRESENTATION, ATOM_max_procedure_arity);
	    break;
	  default:
	    assert(0);
	}
      }

      goto exit_fail;
    }
    Output_0(ci, I_EXIT);
    if ( OpCode(ci, bi) == encode(I_CUT) )
    { set(&clause, COMMIT_CLAUSE);
    }
  } else if ( (flags & (SSU_COMMIT_CLAUSE|SSU_CHOICE_CLAUSE)) )
  { Output_0(ci, (flags&SSU_COMMIT_CLAUSE) ? I_SSU_COMMIT : I_SSU_CHOICE);
    Output_0(ci, I_EXIT);
  } else
  { set(&clause, UNIT_CLAUSE);		/* fact (for decompiler) */
    Output_0(ci, I_EXITFACT);
  }

  resetVars();

  if ( ci->warning_list && (rc=push_compiler_warnings(ci)) != TRUE )
    goto exit_fail;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Finish up the clause.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  clause.code_size = entriesBuffer(&ci->codes, code);

  if ( head )
  { size_t size  = sizeofClause(clause.code_size);
    Module m = proc->definition->module;
    size_t clsize = size + SIZEOF_CREF_CLAUSE;

    if ( m->code_limit && clsize + m->code_size > m->code_limit )
    { rc = PL_error(NULL, 0, NULL, ERR_RESOURCE, ATOM_program_space);
      goto exit_fail;
    }

    cl = PL_malloc_atomic(size);
    ATOMIC_ADD(&m->code_size, clsize);
    memcpy(cl, &clause, sizeofClause(0));
    memcpy(cl->codes, baseBuffer(&ci->codes, code), sizeOfBuffer(&ci->codes));

    ATOMIC_ADD(&GD->statistics.codes, clause.code_size);
    ATOMIC_INC(&GD->statistics.clauses);
  } else
  { LocalFrame fr = lTop;
    Word p0 = argFrameP(fr, clause.variables);
    Word p = p0;
    ClauseRef cref;
    size_t space;

    DEBUG(MSG_COMP_ARGVAR,
	  Sdprintf("%d argvars; %d prolog vars; %d vars",
		   ci->argvars, clause.prolog_vars, clause.variables));
    assert(ci->argvars == ci->argvar);

					/* check space */
    space = ( clause.variables*sizeof(word) +
	      sizeofClause(clause.code_size) +
	      SIZEOF_CREF_CLAUSE +
	      sizeof(word) +		/* possible alignment */
	      (size_t)argFrameP((LocalFrame)NULL, MAXARITY) +
	      sizeof(struct choice)
	    );
    if ( addPointer(lTop, space) >= (void*)lMax )
    { rc = LOCAL_OVERFLOW;
      goto exit_fail;
    }

    cref = (ClauseRef)p;
    p = addPointer(p, SIZEOF_CREF_CLAUSE);
#if ALIGNOF_INT64_T != ALIGNOF_VOIDP
    if ( (uintptr_t)p % sizeof(gen_t) != 0 )
    { p = addPointer(p, sizeof(word));
      assert((uintptr_t)p % sizeof(gen_t) == 0);
    }
#endif
    cref->next = NULL;
    cref->value.clause = cl = (Clause)p;
    memcpy(cl, &clause, sizeofClause(0));
    memcpy(cl->codes, baseBuffer(&ci->codes, code), sizeOfBuffer(&ci->codes));
    p = addPointer(p, sizeofClause(clause.code_size));
    cl->variables += (int)(p-p0);

    fr->clause = cref;
    fr->predicate = getProcDefinition(proc);
    setNextFrameFlags(fr, environment_frame);
    setContextModule(fr, module);

    DEBUG(MSG_COMP_ARGVAR, Sdprintf("; now %d vars\n", clause.variables));
    DEBUG(MSG_COMP_ARGVAR, vm_list(cl->codes, NULL));
    lTop = (LocalFrame)p;
  }

  discardBuffer(&ci->codes);

  *cp = cl;
  return TRUE;

exit_fail:
  resetVars();
  discardBuffer(&ci->codes);
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
    generates  SETVAR  instructions  to  balance  both  branches.    See
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
    when encountered as a control-structure.

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
compileBody(DECL_LD Word body, code call, compileInfo *ci)
{
right_argument:
  deRef(body);

  if ( isTerm(*body) )
  { functor_t fd = functorTerm(*body);
    FunctorDef fdef = valueFunctor(fd);

    if ( true(fdef, CONTROL_F) )
    { if ( fd == FUNCTOR_comma2 )			/* A , B */
      { int rv;

	if ( (rv=compileBody(argTermP(*body, 0), I_CALL, ci)) != TRUE )
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

	  setVars(argTermP(*body, 0), valt1);
	  setVars(argTermP(*body, 1), valt2);
	} else
	  vsave = valt1 = valt2 = NULL;

	deRef(a0);
	if ( (hard=hasFunctor(*a0, FUNCTOR_ifthen2)) || /* A  -> B ; C */
	     hasFunctor(*a0, FUNCTOR_softcut2) )        /* A *-> B ; C */
	{ int var;
	  size_t tc_or, tc_jmp;
	  int rv;
	  cutInfo cutsave = ci->cut;
	  int fast = FALSE;

	  if ( !(var=allocChoiceVar(ci)) )
	    return FALSE;

	  Output_2(ci, hard ? C_IFTHENELSE : C_SOFTIF, var, (code)0);
	  tc_or = PC(ci);
	  ci->cut.var = var;		/* Cut locally in the condition */
	  ci->cut.instruction = hard ? C_LCUT : C_LSCUT;
	  if ( (rv=compileBody(argTermP(*a0, 0), I_CALL, ci)) != TRUE )
	    return rv;
	  if ( hard )
	    fast = try_fast_condition(ci, tc_or);
	  ci->cut = cutsave;
	  Output_1(ci, fast ? C_FASTCUT : hard ? C_CUT : C_SOFTCUT, var);
	  if ( (rv=compileBody(argTermP(*a0, 1), call, ci)) != TRUE )
	    return rv;
	  if ( !ci->islocal )
	    balanceVars(valt1, valt2, ci);
	  Output_1(ci, C_JMP, (code)0);
	  tc_jmp = PC(ci);
	  OpCode(ci, tc_or-1) = (code)(PC(ci) - tc_or);
	  if ( !ci->islocal )
	    copyVarTable(ci->used_var, vsave);
	  if ( (rv=compileBody(argTermP(*body, 1), call, ci)) != TRUE )
	    return rv;
	  if ( !ci->islocal )
	    balanceVars(valt2, valt1, ci);
	  OpCode(ci, tc_jmp-1) = (code)(PC(ci) - tc_jmp);
	} else					/* A ; B */
	{ size_t tc_or, tc_jmp;
	  int rv;

	  Output_1(ci, C_OR, (code)0);
	  tc_or = PC(ci);
	  if ( (rv=compileBody(argTermP(*body, 0), I_CALL, ci)) != TRUE )
	    return rv;
	  if ( !ci->islocal )
	    balanceVars(valt1, valt2, ci);
	  Output_1(ci, C_JMP, (code)0);
	  tc_jmp = PC(ci);
	  OpCode(ci, tc_or-1) = (code)(PC(ci) - tc_or);
	  if ( !ci->islocal )
	    copyVarTable(ci->used_var, vsave);
	  if ( (rv=compileBody(argTermP(*body, 1), call, ci)) != TRUE )
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
		  fd == FUNCTOR_softcut2 )		/* A *-> B */
      { int var;
	int rv;
	int hard = (fd == FUNCTOR_ifthen2);
	cutInfo cutsave = ci->cut;

	if ( !(var=allocChoiceVar(ci)) )
	  return FALSE;

	Output_1(ci, hard ? C_IFTHEN : C_SOFTIFTHEN, var);
	ci->cut.var = var;		/* Cut locally in the condition */
	ci->cut.instruction = C_LCUTIFTHEN;
	if ( (rv=compileBody(argTermP(*body, 0), I_CALL, ci)) != TRUE )
	  return rv;
	ci->cut = cutsave;
	if ( hard )
	  Output_1(ci, C_CUT, var);
	else
	  Output_0(ci, C_SCUT);
	if ( (rv=compileBody(argTermP(*body, 1), call, ci)) != TRUE )
	  return rv;
	Output_0(ci, C_END);

	succeed;
      } else if ( fd == FUNCTOR_not_provable1 ||	/* \+/1 */
		  fd == FUNCTOR_dollar1 )		/* $/1 */
      { int var;
	size_t tc_or, tc_det;
	VarTable vsave;
	int rv;
	cutInfo cutsave = ci->cut;
	int isnot = (fd == FUNCTOR_not_provable1);

	if ( !(var=allocChoiceVar(ci)) )
	  return FALSE;

	if ( !ci->islocal )
	  vsave = mkCopiedVarTable(ci->used_var);
	else
	  vsave = NULL;

	Output_2(ci, isnot ? C_NOT : C_DET, var, (code)0);
	tc_or = PC(ci);
	ci->cut.var = var;
	ci->cut.instruction = C_LCUT;
	if ( (rv=compileBody(argTermP(*body, 0), I_CALL, ci)) != TRUE )
	  return rv;
	ci->cut = cutsave;
	if ( isnot )
	{ Output_1(ci, C_CUT, var);
	  Output_0(ci, C_FAIL);
	  tc_det = 0;			/* silence compiler */
	} else
	{ Output_1(ci, C_DETTRUE, var);
	  Output_1(ci, C_JMP, (code)0);
	  tc_det = PC(ci);
	}
	if ( ci->islocal )
	{ OpCode(ci, tc_or-1) = (code)(PC(ci) - tc_or);
	} else if ( isnot )
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
	} else				/* $(Goal) */
	{ balanceVars(vsave, ci->used_var, ci);
	  OpCode(ci, tc_or-1) = (code)(PC(ci) - tc_or);
	}

	if ( !isnot )
	{ Output_0(ci, C_DETFALSE);
	  OpCode(ci, tc_det-1) = (code)(PC(ci) - tc_det);
	}

	succeed;
#endif /* O_COMPILE_OR */
      } else if ( fd == FUNCTOR_colon2 )	/* Module:Goal */
      { target_module tmsave = ci->colon_context;
	int rc;

	if ( (rc=getTargetModule(&ci->colon_context,
				 argTermP(*body, 0), ci)) != TRUE )
	  return rc;
	rc = compileBody(argTermP(*body, 1), call, ci);
	ci->colon_context = tmsave;

	return rc;
#ifdef O_CALL_AT_MODULE
      } else if ( fd == FUNCTOR_xpceref2 )	/* Call@Module */
      { target_module atsave = ci->at_context;
	int rc;

	if ( (rc=getTargetModule(&ci->at_context,
				 argTermP(*body, 1), ci)) != TRUE )
	  return rc;
	rc = compileBody(argTermP(*body, 0), call, ci);
	ci->at_context = atsave;

	return rc;
#endif /*O_CALL_AT_MODULE*/
      }
      assert(0);
    }
  }

  return compileSubClause(body, call, ci);
}


static int
try_fast_condition(CompileInfo ci, size_t tc_or)
{ Code pc  = &OpCode(ci, tc_or);
  Code end = &OpCode(ci, PC(ci));

  while(pc < end)
  { switch( decode(*pc) )
    { case I_INTEGER:
      case I_FLOAT:
      case I_NUMBER:
      case I_ATOMIC:
      case I_ATOM:
      case I_STRING:
      case I_COMPOUND:
      case I_CALLABLE:
      case I_VAR:
      case I_NONVAR:
      case B_EQ_VV:
      case B_EQ_VC:
      case B_NEQ_VV:
      case B_NEQ_VC:
      case A_ENTER:
      case A_VAR0:
      case A_VAR1:
      case A_VAR2:
      case A_VAR:
      case A_INTEGER:
      case A_INT64:
      case A_MPZ:
      case A_MPQ:
      case A_DOUBLE:
      case A_ROUNDTOWARDS_A:
      case A_ROUNDTOWARDS_V:
      case A_FUNC0:
      case A_FUNC1:
      case A_FUNC2:
      case A_FUNC:
      case A_ADD:
      case A_MUL:
      case A_LT:
      case A_LE:
      case A_GT:
      case A_GE:
      case A_EQ:
      case A_NE:
	break;
      default:
	return FALSE;
    }

    pc = stepPC(pc);
  }

  pc = &OpCode(ci, tc_or);
  assert(decode(pc[-3]) == C_IFTHENELSE);
  pc[-3] = encode(C_FASTCOND);

  return TRUE;
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

static int
link_local_var(DECL_LD Word v, int iv, CompileInfo ci)
{ VarDef vd = LD->comp.vardefs[*v>>LMASK_BITS];
  int voffset = VAROFFSET(iv);
  Word k = varFrameP(lTop, voffset);

  DEBUG(MSG_COMP_ARGVAR,
	{ char vname[32];
	  Sdprintf("Linking b_var(%d) to %s\n",
		   iv, var_name_ptr(vd->address, vname));
	});

  if ( k >= (Word) lMax )
    return LOCAL_OVERFLOW;
  DEBUG(0, assert(vd->address < (Word)lBase));
  *k = makeRefG(vd->address);

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Compile argument to a goal in the   clause. The `islocal' compilation is
one of the complicating factors: atoms   should not be registered (there
is no need as they are  held  by   the  term  anyway). For `big' objects
(strings and compounds) the system should create `argvar' references.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
compileArgument(DECL_LD Word arg, int where, compileInfo *ci)
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
          if ( val >= INTPTR_MIN && val <= INTPTR_MAX )
	  { Output_1(ci, (where&A_HEAD) ? H_INTEGER : B_INTEGER, (intptr_t)val);
	  } else
	  { int c = ((where&A_HEAD) ? H_INT64 : B_INT64);
	    Output_n(ci, c, (Word)&val, WORDS_PER_INT64);
	  }
#endif
	} else				/* MPZ/MPQ NUMBER */
	{ int c;

	  if ( p[1]&MP_RAT_MASK )
	    c = (where & A_HEAD) ? H_MPQ : B_MPQ;
	  else
	    c = (where & A_HEAD) ? H_MPZ : B_MPZ;

	  Output_n(ci, c, p, n+1);
	  return TRUE;
	}
	return TRUE;
      }
      Output_1(ci, (where & A_BODY) ? B_SMALLINT : H_SMALLINT, *arg);
      return TRUE;
    case TAG_ATOM:
      if ( isNil(*arg) )
      {	Output_0(ci, (where & A_BODY) ? B_NIL : H_NIL);
      } else
      { if ( !ci->islocal )
	  PL_register_atom(*arg);
	Output_1(ci, (where & A_BODY) ? B_ATOM : H_ATOM, *arg);
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
  if ( (index = isIndexedVarTerm(*arg)) >= 0 )
  { if ( ci->islocal )
    { int rc;

      if ( (rc=link_local_var(arg, index, ci)) != TRUE )
	return rc;

      if ( index < 3 )
      { Output_0(ci, B_VAR0 + index);
      } else
      { Output_1(ci, B_VAR, VAROFFSET(index));
      }

      return TRUE;
    }

    first = isFirstVarSet(ci->used_var, index);

    if ( index < ci->arity )		/* variable on its own in the head */
    { if ( where & A_BODY )
      { if ( argUnifiedTo(*arg) )
	  set(ci->clause, CL_HEAD_TERMS);

	if ( where & A_ARG )
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
	{ Word p;

	  if ( (p=argUnifiedTo(*arg)) )
	    return compileArgument(p, where, ci);
	  Output_0(ci, H_VOID);
	  return TRUE;
	}
	if ( argUnifiedTo(*arg) )
	  set(ci->clause, CL_HEAD_TERMS);

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
      *k = makeRefG(arg);		/* a reference to avoid binding a */
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
  { ssize_t ar;
    functor_t fdef;
    int isright = (where & A_RIGHT);

    fdef = functorTerm(*arg);
    if ( fdef == FUNCTOR_dot2 )
    { code c;

      if ( (where & A_HEAD) )		/* index in array! */
      { if ( compileListFF(*arg, ci) )
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

      if ( (rc=compileArgument(arg, where, ci)) < 0 )
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
    { if ( ar == 0 )
	goto right_recursion;
    } else
    { int rc;

      if ( ar == 0 )			/* ar == -1 on a() */
      { if ( (rc=compileArgument(arg, where, ci)) < 0 )
	  return rc;
      }
      Output_0(ci, (where & A_HEAD) ? H_POP : B_POP);
    }

    return TRUE;
  }
}


static int
compileListFF(DECL_LD word arg, compileInfo *ci)
{ Word p = argTermP(arg, 0);
  int i1, i2;

  if ( isFirstVarP(p+0, ci, &i1) &&
       isFirstVarP(p+1, ci, &i2) &&
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


Procedure
lookupBodyProcedure(functor_t functor, Module tm)
{ GET_LD
  Procedure proc;

  if ( (proc = isCurrentProcedure(functor, tm)) &&
       ( isDefinedProcedure(proc) ||
	 true(proc->definition, P_REDEFINED)
       )
     )
    return proc;

  if ( tm != MODULE_system &&
       (proc = isCurrentProcedure(functor, MODULE_system)) &&
       true(proc->definition, P_ISO) &&
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

static void lco(CompileInfo ci, size_t pc0);

static int
compileSubClause(Word arg, code call, compileInfo *ci)
{ GET_LD
  functor_t functor;
  FunctorDef fdef;
  Procedure proc;
  Module tm;				/* lookup module */
  size_t pc0;

  deRef(arg);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A non-void variable. Create a I_USERCALL0 instruction for it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  if ( isIndexedVarTerm(*arg) >= 0 )
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
      if ( (rc=compileArgument(arg, A_BODY|A_RIGHT|A_NOARGVAR, ci)) < 0 )
	return rc;
      Output_0(ci, B_POP);
    } else
    { if ( (rc=compileArgument(arg, A_BODY|A_NOARGVAR, ci)) < 0 )
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

    if ( fdef->arity > MAXARITY )
      return MAX_ARITY_OVERFLOW;

    if ( !isTextAtom(fdef->name) && fdef->name != ATOM_nil )
      return NOT_CALLABLE;

    if ( true(fdef, ARITH_F) && !ci->islocal )
    { if ( functor == FUNCTOR_is2 &&
	   compileSimpleAddition(arg, ci) )
	succeed;
#if O_COMPILE_ARITH
      if ( truePrologFlag(PLFLAG_OPTIMISE) )
	 return compileArith(arg, ci);
#endif
    }

#ifdef O_COMPILE_IS
    if ( !ci->islocal )
    { int rc;

      if ( functor == FUNCTOR_equals2 )	/* =/2 */
      { if ( (rc=compileBodyUnify(arg, ci)) != FALSE )
	  return rc;
      } else if ( functor == FUNCTOR_strict_equal2 )	/* ==/2 */
      { if ( (rc=compileBodyEQ(arg, ci)) != FALSE )
	  return rc;
      } else if ( functor == FUNCTOR_not_strict_equal2 ) /* \==/2 */
      { if ( (rc=compileBodyNEQ(arg, ci)) != FALSE )
	  return rc;
      } else if ( functor == FUNCTOR_var1 )
      { if ( (rc=compileBodyVar1(arg, ci)) != FALSE )
	  return rc;
      } else if ( functor == FUNCTOR_nonvar1 )
      { if ( (rc=compileBodyNonVar1(arg, ci)) != FALSE )
	  return rc;
      } else if ( (rc=compileBodyTypeTest(functor, arg, ci)) != FALSE )
      { return rc;
      } else if ( functor == FUNCTOR_dcall_continuation1 )
      { if ( (rc=compileBodyCallContinuation(arg, ci)) != FALSE )
	  return rc;
      } else if ( functor == FUNCTOR_dshift1 )
      { if ( (rc=compileBodyShift(arg, ci, FALSE)) != FALSE )
	  return rc;
      } else if ( functor == FUNCTOR_dshift_for_copy1 )
      { if ( (rc=compileBodyShift(arg, ci, TRUE)) != FALSE )
	  return rc;
      } else if ( functor == FUNCTOR_arg3 )
      { if ( (rc=compileBodyArg3(arg, ci)) != FALSE )
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
    } else if ( *arg == ATOM_dreset )		/* $reset */
    { Output_0(ci, I_RESET);
      Output_0(ci, I_EXITRESET);
      succeed;
    } else if ( *arg == ATOM_dcall_cleanup )	/* $call_cleanup */
    { Output_0(ci, I_CALLCLEANUP);
      Output_0(ci, I_EXITCLEANUP);
      succeed;
    } else if ( *arg == ATOM_dcut )		/* $cut */
    { Output_0(ci, I_CUTCHP);
      succeed;
    } else if ( *arg == ATOM_dyield )		/* $yield */
    { Output_0(ci, I_YIELD);
      succeed;
    } else if ( *arg == ATOM_dollar )		/* $ */
    { Output_0(ci, I_DET);
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

  pc0 = PC(ci);
  if ( fdef )				/* term: there are arguments */
  { size_t ar = fdef->arity;

    for(arg = argTermP(*arg, 0); ar > 0; ar--, arg++)
    { int rc;

      if ( (rc=compileArgument(arg, A_BODY, ci)) < 0 )
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
    { Output_1(ci, call, (code) proc);
      if ( call == I_DEPART &&
	   ci->procedure )
	lco(ci, pc0);
    } else
    { Output_2(ci, mcall(call), (code)tm, (code)proc);
    }
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
compileSimpleAddition() compiles NewVar is Var   +/- SmallInt. At entry,
vc is known to be a dereferenced pointer to term is/2.  For addition, it
allows for swapping the arguments.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
compileSimpleAddition(DECL_LD Word sc, compileInfo *ci)
{ Word a = argTermP(*sc, 0);
  int rvar;

  if ( isFirstVarP(a, ci, &rvar) ) /* NewVar is ? */
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

	if ( (vi=isIndexedVarTerm(*a1)) >= 0 &&
	     !isFirstVar(ci->used_var, vi) &&
	     is_portable_smallint(*a2) )
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Perform LCO optimization when possible.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
reverse_code(Code a, Code z)
{ z--;

  while(a<z)
  { code t = *a;
    *a++ = *z;
    *z-- = t;
  }
}


static void
lco(CompileInfo ci, size_t pc0)
{ size_t pcz = PC(ci);
  Code base  = baseBuffer(&(ci)->codes, code);
  Code s0    = base+pc0;
  Code s     = s0;
  Code e     = base+pcz;
  int oarg   = 0;
  Code z;

#define FIX_BUFFER_SHIFT() \
	do \
	{ intptr_t shift; \
	  if ( (shift=(baseBuffer(&(ci)->codes, code)-base)) ) \
	  { s    += shift; \
	    s0   += shift; \
	    e    += shift; \
	    base += shift; \
	  } \
	} while(0)

  assert(decode(e[-2]) == I_DEPART);
  Output_1(ci, L_NOLCO, (code)0);
  FIX_BUFFER_SHIFT();

  for(; s < e-2; oarg++ )
  { code c = decode(*s++);
    const code_info *vmi = &codeTable[c];
    int bv;

    if ( false(vmi, VIF_LCO) )
    { no_lco:
      seekBuffer(&(ci)->codes, pcz, code);
      return;
    }

    switch(c)
    { case B_VAR0: bv = 0; goto common_bv;
      case B_VAR1: bv = 1; goto common_bv;
      case B_VAR2: bv = 2; goto common_bv;
      case B_VAR:
      { bv = VARNUM(*s++);
      common_bv:
	if ( bv < oarg )		/* would overwrite */
	  goto no_lco;
	if ( bv != oarg )
	{ Output_2(ci, L_VAR, VAROFFSET(oarg), VAROFFSET(bv));
	}
	break;
      }
      case B_VOID:
	Output_1(ci, L_VOID, VAROFFSET(oarg));
        break;
      case B_SMALLINT:
      { code a = *s++;
	Output_2(ci, L_SMALLINT, VAROFFSET(oarg), a);
	break;
      }
      case B_ATOM:
      { code a = *s++;
	PL_register_atom(a);		/* TBD: unregister on failure */
	Output_2(ci, L_ATOM, VAROFFSET(oarg), a);
	break;
      }
      case B_NIL:
	Output_1(ci, L_NIL, VAROFFSET(oarg));
        break;
      default:
	assert(0);
    }

    FIX_BUFFER_SHIFT();
  }

  Procedure depart_proc = (Procedure)e[-1];

  if ( ci->procedure ==	depart_proc )
    Output_0(ci, I_TCALL);
  else
    Output_1(ci, I_LCALL, (code)depart_proc);

  FIX_BUFFER_SHIFT();

  z = topBuffer(&(ci)->codes, code);
  e[1] = (code)(z-e-2);			/* fill L_NOLCO argument */

  reverse_code(s0, e);
  reverse_code(e,  z);
  reverse_code(s0, z);
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
compileArith(DECL_LD Word arg, compileInfo *ci)
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

    rc=compileArgument(argTermP(*arg, 0), A_BODY, ci);
    if ( rc != TRUE )
      return rc;
    if ( PC(ci) == tc_a1 + 2 &&	OpCode(ci, tc_a1) == encode(B_FIRSTVAR) )
    { isvar = OpCode(ci, tc_a1+1);
      seekBuffer(&ci->codes, tc_a1, code);
    } else
      isvar = 0;
    Output_0(ci, A_ENTER);
    rc = compileArithArgument(argTermP(*arg, 1), ci);
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
  if ( !compileArithArgument(argTermP(*arg, 0), ci) ||
       !compileArithArgument(argTermP(*arg, 1), ci) )
    fail;

  Output_0(ci, a_func);

  return TRUE;
}


#define arithVarOffset(arg, ci, offp) LDFUNC(arithVarOffset, arg, ci, offp)
static int
arithVarOffset(DECL_LD Word arg, compileInfo *ci, int *offp)
{ int index;

  if ( (index = isIndexedVarTerm(*arg)) >= 0 )
  { int first = isFirstVarSet(ci->used_var, index);

    if ( index < ci->arity || !first )	/* shared in the head or not first */
    { *offp = index;
      return TRUE;
    } else
    { resetVars();		/* get clean Prolog data, assume */
					/* calling twice is ok */
      PL_error(NULL, 0, "Unbound variable in arithmetic expression",
	       ERR_TYPE, ATOM_evaluable, pushWordAsTermRef(arg));
      popTermRef();
      return -1;
    }
  }

  if ( isVar(*arg) )			/* void variable */
  { PL_error(NULL, 0, "Unbound variable in arithmetic expression",
	     ERR_TYPE, ATOM_evaluable, pushWordAsTermRef(arg));
    popTermRef();
    return -1;
  }

  return FALSE;
}


static int
compileArithArgument(DECL_LD Word arg, compileInfo *ci)
{ int index;
  int rc;

  deRef(arg);

  if ( isRational(*arg) )
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
#ifdef O_GMP
      } else if ( p[1]&MP_RAT_MASK )
      { Output_n(ci, A_MPQ, p, n+1);
      } else
      { Output_n(ci, A_MPZ, p, n+1);
#endif
      }
    }
    succeed;
  }
  if ( isFloat(*arg) )
  { Word p = valIndirectP(*arg);

    Output_n(ci, A_DOUBLE, p, WORDS_PER_DOUBLE);
    succeed;
  }

  if ( (rc=arithVarOffset(arg, ci, &index)) == TRUE )
  { if ( index < 3 )
      Output_0(ci, A_VAR0 + index);
    else
      Output_1(ci, A_VAR, VAROFFSET(index));

    return TRUE;
  } else if ( rc < 0 )
  { return FALSE;
  }

						/* callable (function) */
  { functor_t fdef;
    size_t n, ar;
    Word a;

    if ( isTextAtom(*arg) )
    { fdef = lookupFunctorDef(*arg, 0);
      ar = 0;
      a = NULL;
    } else if ( isTerm(*arg) )
    { fdef = functorTerm(*arg);
      ar = arityFunctor(fdef);
      a = argTermP(*arg, 0);
    } else if ( isString(*arg) )
    { number n;

    case_char_constant:
      if ( !getCharExpression(arg, &n) )
	return FALSE;
      Output_1(ci, A_INTEGER, n.value.i);
      return TRUE;
    } else
    { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_evaluable, pushWordAsTermRef(arg));
      popTermRef();
      return FALSE;
    }

    if ( fdef == FUNCTOR_dot2 )		/* "char" */
      goto case_char_constant;

    if ( (index = indexArithFunction(fdef)) < 0 )
    { PL_error(NULL, 0, "No such arithmetic function",
	       ERR_TYPE, ATOM_evaluable, pushWordAsTermRef(arg));
      popTermRef();
      return FALSE;
    }

    if ( fdef == FUNCTOR_roundtoward2 )
    { Word m;
      int mode;
      int vindex;

      deRef2(a+1, m);
      if ( isAtom(*m) && atom_to_rounding(*m, &mode) )
      { Output_1(ci, A_ROUNDTOWARDS_A, mode);
      } else if ( (rc=arithVarOffset(m, ci, &vindex)) == TRUE )
      { Output_1(ci, A_ROUNDTOWARDS_V, VAROFFSET(vindex));
      } else if ( rc < 0 )
      { return FALSE;
      } else if ( isAtom(*m) )
      { PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_round, pushWordAsTermRef(m));
	popTermRef();
	return FALSE;
      } else
      { PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_atom, pushWordAsTermRef(m));
	popTermRef();
	return FALSE;
      }

      compileArithArgument(a, ci);
    } else
    { for(a+=ar-1, n=ar; n-- > 0; a--)	/* pushed right to left */
      { if ( !compileArithArgument(a, ci) )
	  return FALSE;
      }
    }

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

In addition, if arg is a term,  we   must  recurse down into the term to
deal with variables that are only used there. This is needed for e.g.

foo(bar) :-
	foo(_),
	L = [X|X].

Although X is allocated on the stack, no  code will be generated for it,
causing clearUninitialisedVarsFrame() to ignore this variable. Of course
it would be better to shrink the frame,   but  I doubt that is worth the
trouble.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define skippedVar(arg, ci) LDFUNC(skippedVar, arg, ci)
static int
skippedVar(DECL_LD Word arg, compileInfo *ci)
{ int i;

right_recursion:
  deRef(arg);
  i = isIndexedVarTerm(*arg);

  if ( i >= 0 && isFirstVarSet(ci->used_var, i) )
    Output_1(ci, C_VAR, VAROFFSET(i));
  if ( isTerm(*arg) && !ci->islocal )
  { size_t ar = arityFunctor(functorTerm(*arg));

    if ( ar > 0 )
    { for(ar--, arg = argTermP(*arg, 0); ar-- > 0; arg++)
	skippedVar(arg, ci);
      goto right_recursion;
    }
  }

  return TRUE;
}


#define isUnifiedArg(a1, a2) LDFUNC(isUnifiedArg, a1, a2)
static int
isUnifiedArg(DECL_LD Word a1, Word a2)
{ if ( isVarInfo(*a1) )
  { VarDef vd = varInfo(*a1);

    if ( true(vd, VD_ARGUMENT) &&
	 vd->arg_value == a2 )
      return TRUE;
  }

  return FALSE;
}


static int
compileBodyUnify(DECL_LD Word arg, compileInfo *ci)
{ Word a1, a2;
  int i1, i2;

  a1 = argTermP(*arg, 0); deRef(a1);
  a2 = argTermP(*arg, 1); deRef(a2);

  if ( isVar(*a1) || isVar(*a2) )	/* Singleton = ? --> true */
  { skippedVar(a1, ci);
    skippedVar(a2, ci);

/* always results in a singleton warning anyway
    if ( (debugstatus.styleCheck&NOEFFECT_CHECK) )
      compiler_warning(ci, "unify_singleton", a1, a2);
*/
    Output_0(ci, I_TRUE);
    return TRUE;
  }

  i1 = isIndexedVarTerm(*a1);
  i2 = isIndexedVarTerm(*a2);

  if ( i1 >= 0 && i2 >= 0 )		/* unify two variables */
  { int f1, f2;

    if ( i1 == i2 )			/* unify a var with itself? */
    { skippedVar(a1, ci);
      Output_0(ci, I_TRUE);
      return TRUE;
    }

    f1 = isFirstVarSet(ci->used_var, i1);
    f2 = isFirstVarSet(ci->used_var, i2);

    if ( f1 && f2 )
      Output_2(ci, B_UNIFY_FF, VAROFFSET(i1), VAROFFSET(i2));
    else if ( f1 )
      Output_2(ci, B_UNIFY_FV, VAROFFSET(i1), VAROFFSET(i2));
    else if ( f2 )			/* same, but args swapped */
      Output_2(ci, B_UNIFY_VF, VAROFFSET(i2), VAROFFSET(i1));
    else
      Output_2(ci, B_UNIFY_VV, VAROFFSET(i1), VAROFFSET(i2));

    return TRUE;
  }

  /* check for unifications moved to the head */
  if ( i1 >= 0 && isUnifiedArg(a1, a2) )
    return TRUE;
  if ( i2 >= 0 && isUnifiedArg(a2, a1) )
    return TRUE;

  if ( i1 >= 0 )			/* Var = Term */
  { int first;
    int rc;

  unify_term:
    first = isFirstVarSet(ci->used_var, i1);
    if ( is_portable_constant(*a2) )
    { Output_2(ci, first ? B_UNIFY_FC : B_UNIFY_VC, VAROFFSET(i1), *a2);
      if ( isAtom(*a2) )
	PL_register_atom(*a2);
    } else
    { int where = (first ? A_BODY : A_HEAD|A_ARG);
      Output_1(ci, first ? B_UNIFY_FIRSTVAR : B_UNIFY_VAR, VAROFFSET(i1));
      if ( (rc=compileArgument(a2, where, ci)) < 0 )
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
compileBodyEQ(DECL_LD Word arg, compileInfo *ci)
{ Word a1, a2;
  int i1, i2;

  a1 = argTermP(*arg, 0); deRef(a1);
  a2 = argTermP(*arg, 1); deRef(a2);

  if ( isVar(*a1) || isVar(*a2) )	/* Singleton == ?: always fail */
  { if ( (debugstatus.styleCheck&NOEFFECT_CHECK) )
      compiler_warning(ci, "eq_singleton", a1, a2);
    if ( truePrologFlag(PLFLAG_OPTIMISE) )
    { skippedVar(a1, ci);
      skippedVar(a2, ci);
      Output_0(ci, I_FAIL);
      return TRUE;
    }

    return FALSE;			/* debugging: compile as normal code */
  }

  i1 = isIndexedVarTerm(*a1);
  i2 = isIndexedVarTerm(*a2);

  if ( i1 >=0 && i2 >= 0 )		/* Var1 == Var2 */
  { int f1 = isFirstVar(ci->used_var, i1);
    int f2 = isFirstVar(ci->used_var, i2);

    if ( f1 || f2 )
    { if ( (debugstatus.styleCheck&NOEFFECT_CHECK) )
	compiler_warning(ci, "eq_vv", a1, a2);
      if ( truePrologFlag(PLFLAG_OPTIMISE) )
      {	code op = (i1 == i2) ? I_TRUE : I_FAIL;;

	skippedVar(a1, ci);
	skippedVar(a2, ci);
	Output_0(ci, op);

	return TRUE;
      }
    }

    if ( f1 ) Output_1(ci, C_VAR, VAROFFSET(i1));
    if ( f2 ) Output_1(ci, C_VAR, VAROFFSET(i2));

    Output_2(ci, B_EQ_VV, VAROFFSET(i1), VAROFFSET(i2));

    return TRUE;
  }

  if ( i1 >= 0 && is_portable_constant(*a2) )	/* Var == const */
  { int f1 = isFirstVar(ci->used_var, i1);

    if ( f1 ) Output_1(ci, C_VAR, VAROFFSET(i1));
    Output_2(ci, B_EQ_VC, VAROFFSET(i1), *a2);
    if ( isAtom(*a2) )
      PL_register_atom(*a2);
    return TRUE;
  }
  if ( i2 >= 0 && is_portable_constant(*a1) )	/* const == Var */
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
compileBodyNEQ(DECL_LD Word arg, compileInfo *ci)
{ Word a1, a2;
  int i1, i2;

  a1 = argTermP(*arg, 0); deRef(a1);
  a2 = argTermP(*arg, 1); deRef(a2);

  if ( isVar(*a1) || isVar(*a2) )	/* Singleton \== ?: always true */
  { if ( (debugstatus.styleCheck&NOEFFECT_CHECK) )
      compiler_warning(ci, "neq_singleton", a1, a2);
    if ( truePrologFlag(PLFLAG_OPTIMISE) )
    { skippedVar(a1, ci);
      skippedVar(a2, ci);
      Output_0(ci, I_TRUE);
      return TRUE;
    }

    return FALSE;			/* debugging: compile as normal code */
  }

  i1 = isIndexedVarTerm(*a1);
  i2 = isIndexedVarTerm(*a2);

  if ( i1 >=0 && i2 >= 0 )		/* Var1 == Var2 */
  { int f1 = isFirstVar(ci->used_var, i1);
    int f2 = isFirstVar(ci->used_var, i2);

    if ( f1 || f2 )
    { if ( (debugstatus.styleCheck&NOEFFECT_CHECK) )
	compiler_warning(ci, "neq_vv", a1, a2);
      if ( truePrologFlag(PLFLAG_OPTIMISE) )
      {	skippedVar(a1, ci);
	skippedVar(a2, ci);
	Output_0(ci, i1 == i2 ? I_FAIL : I_TRUE);
	return TRUE;
      }
    }

    if ( f1 ) Output_1(ci, C_VAR, VAROFFSET(i1));
    if ( f2 ) Output_1(ci, C_VAR, VAROFFSET(i2));

    Output_2(ci, B_NEQ_VV, VAROFFSET(i1), VAROFFSET(i2));

    return TRUE;
  }

  if ( i1 >= 0 && is_portable_constant(*a2) )	/* Var == const */
  { int f1 = isFirstVar(ci->used_var, i1);

    if ( f1 ) Output_1(ci, C_VAR, VAROFFSET(i1));
    Output_2(ci, B_NEQ_VC, VAROFFSET(i1), *a2);
    if ( isAtom(*a2) )
      PL_register_atom(*a2);
    return TRUE;
  }
  if ( i2 >= 0 && is_portable_constant(*a1) )	/* const == Var */
  { int f2 = isFirstVar(ci->used_var, i2);

    if ( f2 ) Output_1(ci, C_VAR, VAROFFSET(i2));
    Output_2(ci, B_NEQ_VC, VAROFFSET(i2), *a1);
    if ( isAtom(*a1) )
      PL_register_atom(*a1);
    return TRUE;
  }

  return FALSE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Compile

  - arg(Int, Var, FirstVar)
  - arg(Var, Var, FirstVar)

TBD: we can also handle the error cases here (illegal first or
second argument)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
compileBodyArg3(DECL_LD Word arg, compileInfo *ci)
{ Word av;
  Word a3;
  int v3;

  av = argTermP(*arg, 0);

  deRef2(&av[2], a3);
  if ( (v3=isIndexedVarTerm(*a3)) >= 0 &&
       isFirstVar(ci->used_var, v3) )
  { Word a2;
    int v2;

    deRef2(&av[1], a2);
    if ( (v2=isIndexedVarTerm(*a2)) >= 0 &&
	 !isFirstVar(ci->used_var, v2) )
    { Word a1;
      int v1;

      deRef2(&av[0], a1);
      if ( isTaggedInt(*a1) )
      { isFirstVarSet(ci->used_var, v3);
	Output_3(ci, B_ARG_CF, *a1, VAROFFSET(v2), VAROFFSET(v3));
	return TRUE;
      }
      if ( (v1=isIndexedVarTerm(*a1)) >= 0 &&
	   !isFirstVar(ci->used_var, v1) )
      { isFirstVarSet(ci->used_var, v3);
	Output_3(ci, B_ARG_VF, VAROFFSET(v1), VAROFFSET(v2), VAROFFSET(v3));
	return TRUE;
      }
    }
  }

  return FALSE;
}


#endif /*O_COMPILE_IS*/

#define always(val, pred, arg, ci) LDFUNC(always, val, pred, arg, ci)
static int
always(DECL_LD atom_t val, const char *pred, Word arg, compileInfo *ci)
{ if ( (debugstatus.styleCheck&NOEFFECT_CHECK) )
  { int rc;

    if ( (rc=compiler_warning(ci, "always", val, pred, arg)) != TRUE )
      return rc;
  }
  if ( truePrologFlag(PLFLAG_OPTIMISE) )
  { Output_0(ci, val == ATOM_true ? I_TRUE : I_FAIL);
    return TRUE;
  }

  return FALSE;
}


static int
compileBodyVar1(DECL_LD Word arg, compileInfo *ci)
{ Word a1;
  int i1;

  a1 = argTermP(*arg, 0);
  deRef(a1);
  if ( isVar(*a1) )			/* Singleton: always true */
    return always(ATOM_true, "var", a1, ci);

  i1 = isIndexedVarTerm(*a1);
  if ( i1 >=0 )
  { int f1 = isFirstVar(ci->used_var, i1);

    if ( f1 )				/* first var */
      return always(ATOM_true, "var", a1, ci);

    Output_1(ci, I_VAR, VAROFFSET(i1));
    return TRUE;
  }

  return always(ATOM_false, "var", a1, ci);
}


static int
compileBodyNonVar1(DECL_LD Word arg, compileInfo *ci)
{ Word a1;
  int i1;

  a1 = argTermP(*arg, 0);
  deRef(a1);
  if ( isVar(*a1) )			/* Singleton: always false */
    return always(ATOM_false, "nonvar", a1, ci);

  i1 = isIndexedVarTerm(*a1);
  if ( i1 >=0 )
  { int f1 = isFirstVar(ci->used_var, i1);

    if ( f1 )
      return always(ATOM_false, "nonvar", a1, ci);

    Output_1(ci, I_NONVAR, VAROFFSET(i1));
    return TRUE;
  }

  return always(ATOM_true, "nonvar", a1, ci);
}

#define compileTypeTest(arg, instruction, name, test, ci) LDFUNC(compileTypeTest, arg, instruction, name, test, ci)
static int
compileTypeTest(DECL_LD Word arg,
		code instruction, const char *name, int (*test)(word),
		compileInfo *ci)
{ Word a1;
  int i1;

  a1 = argTermP(*arg, 0);
  deRef(a1);
  if ( isVar(*a1) )			/* Singleton: always false */
    return always(ATOM_false, name, a1, ci);

  i1 = isIndexedVarTerm(*a1);
  if ( i1 >=0 )
  { int f1 = isFirstVar(ci->used_var, i1);

    if ( f1 )
    { int rc;

      if ( (rc=always(ATOM_false, name, a1, ci)) == TRUE )
      { isFirstVarSet(ci->used_var, i1);
	Output_1(ci, C_VAR, VAROFFSET(i1));
      }

      return rc;
    }

    Output_1(ci, instruction, VAROFFSET(i1));
    return TRUE;
  }

  if ( (*test)(*a1) )
    return always(ATOM_true, name, a1, ci);
  else
    return always(ATOM_false, name, a1, ci);
}

typedef struct type_test
{ functor_t	functor;
  code		instruction;
  const char *  name;
  int	        (*test)(word);
} type_test;

#ifdef O_GMP
static int fisInteger(word w)  { GET_LD return isInteger(w);  }
#else
static int fisInteger(word w)  { return isInteger(w);  }
#endif
static int fisRational(word w) { return isRational(w);  }
static int fisFloat(word w)    { return isFloat(w);    }
static int fisNumber(word w)   { return isNumber(w);   }
static int fisAtomic(word w)   { return isAtomic(w);   }
static int fisAtom(word w)     { return isTextAtom(w); }
static int fisString(word w)   { return isString(w);   }
static int fisCompound(word w) { return isTerm(w);     }
static int fisCallable(word w) { GET_LD return isCallable(w); }

const type_test type_tests[] =
{ { FUNCTOR_integer1,  I_INTEGER,  "integer",  fisInteger  },
  { FUNCTOR_rational1, I_RATIONAL, "rational", fisRational },
  { FUNCTOR_float1,    I_FLOAT,	   "float",    fisFloat    },
  { FUNCTOR_number1,   I_NUMBER,   "number",   fisNumber   },
  { FUNCTOR_atomic1,   I_ATOMIC,   "atomic",   fisAtomic   },
  { FUNCTOR_atom1,     I_ATOM,     "atom",     fisAtom     },
  { FUNCTOR_string1,   I_STRING,   "string",   fisString   },
  { FUNCTOR_compound1, I_COMPOUND, "compound", fisCompound },
  { FUNCTOR_callable1, I_CALLABLE, "callable", fisCallable },
  { 0, 0, NULL, NULL }
};

static int
compileBodyTypeTest(DECL_LD functor_t functor, Word arg, compileInfo *ci)
{ const type_test *tt;

  for(tt = type_tests; tt->functor; tt++)
  { if ( functor == tt->functor )
      return compileTypeTest(arg, tt->instruction, tt->name, tt->test,
			     ci);
  }

  return FALSE;
}

static int
compileBodyCallContinuation(DECL_LD Word arg, compileInfo *ci)
{ Word a1;
  int i1;

  a1 = argTermP(*arg, 0);
  deRef(a1);

  if ( (i1 = isIndexedVarTerm(*a1)) >= 0 &&
       !isFirstVar(ci->used_var, i1) )
  { Output_1(ci, I_CALLCONT, VAROFFSET(i1));
    return TRUE;
  }

  return FALSE;
}

static int
compileBodyShift(DECL_LD Word arg, compileInfo *ci, int for_copy)
{ Word a1;
  int i1;

  a1 = argTermP(*arg, 0);
  deRef(a1);

  if ( (i1 = isIndexedVarTerm(*a1)) >= 0 &&
       !isFirstVar(ci->used_var, i1) )
  { Output_1(ci, for_copy ? I_SHIFTCP : I_SHIFT, VAROFFSET(i1));
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
    { case H_ATOM:
      case B_ATOM:
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
      case CA1_MPQ:
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
assert_term(DECL_LD term_t term, Module module, ClauseRef where,
	    atom_t owner, SourceLoc loc,
	    int flags)
{ Clause clause;
  ClauseRef cref;
  Procedure proc;
  Definition def;
  Module source_module = (loc ? LD->modules.source : (Module) NULL);
  Module mhead;
  term_t tmp      = PL_new_term_refs(4);
  term_t head     = tmp+1;
  term_t body     = tmp+2;
  term_t warnings = (owner ? tmp+3 : 0);
  Word h, b;
  functor_t fdef;
  int hflags = 0;

  if ( !module )
    module = source_module;

  if ( !PL_strip_module_ex(term, &module, tmp) )
    return NULL;
  mhead = module;
  if ( !get_head_and_body_clause(tmp, head, body, &mhead, &hflags) )
    return NULL;
  if ( !get_head_functor(head, &fdef, 0) )
    return NULL;			/* not callable, arity too high */
  if ( !(proc = isCurrentProcedure(fdef, mhead)) )
  { if ( checkModifySystemProc(fdef) )
      proc = lookupProcedure(fdef, mhead);
    if ( !proc )
      return NULL;
  }
  if ( flags && !isDefinedProcedure(proc) )
  { if ( (flags&PL_CREATE_INCREMENTAL) )
      tbl_set_incremental_predicate(proc->definition, TRUE);
    if ( (flags&PL_CREATE_THREAD_LOCAL) )
      setAttrDefinition(proc->definition, P_THREAD_LOCAL, TRUE);
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
  if ( compileClause(&clause, h, b, proc, module,
		     warnings, hflags) != TRUE )
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

    if ( !overruleImportedProcedure(proc, mhead) )
    { error:
      freeClause(clause);
      return NULL;
    }
    def = getProcDefinition(proc);	/* may be changed */

    if ( proc != of->current_procedure )
    { if ( def->impl.any.defined )
      { if ( !redefineProcedure(proc, of, 0) )
	  goto error;
      }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This `if` locks predicates as system  predicates   if  we  are in system
mode, the predicate is still undefined and is not dynamic or multifile.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

      if ( !isDefinedProcedure(proc) )
      { if ( SYSTEM_MODE )
	{ if ( false(def, P_LOCKED) )
	    set(def, HIDE_CHILDS|P_LOCKED);
	} else
	{ if ( truePrologFlag(PLFLAG_DEBUGINFO) )
	    clear(def, HIDE_CHILDS);
	  else
	    set(def, HIDE_CHILDS);
	}
      }

      addProcedureSourceFile(of, proc);
      of->current_procedure = proc;
    }

    if ( (cref=assertProcedureSource(of, proc, clause)) )
    { clause = cref->value.clause;

      if ( warnings && !PL_get_nil(warnings) )
      { int rc;
	fid_t fid = PL_open_foreign_frame();
	term_t cl = PL_new_term_ref();

	PL_put_clref(cl, clause);
	rc = printMessage(ATOM_warning,
			  PL_FUNCTOR_CHARS, "compiler_warnings", 2,
			    PL_TERM, cl,
			    PL_TERM, warnings);
	PL_discard_foreign_frame(fid);
	if ( !rc )
	  clause = NULL;
      }
    } else
      clause = NULL;

    return clause;
  }

  /* assert[az]/1 */

  if ( false(def, P_DYNAMIC) )
  { if ( isDefinedProcedure(proc) )
    { PL_error(NULL, 0, NULL, ERR_MODIFY_STATIC_PROC, proc);
    derror:
      freeClause(clause);
      return NULL;
    }
    if ( !setDynamicDefinition(def, TRUE) )
      goto derror;
  }

  if ( (cref=assertProcedure(proc, clause, where)) )
    return cref->value.clause;

  return NULL;
}


static
PRED_IMPL("assertz", 1, assertz1, PL_FA_TRANSPARENT)
{ PRED_LD

  return assert_term(A1, NULL, CL_END, NULL_ATOM, NULL, 0) != NULL;
}


static
PRED_IMPL("asserta", 1, asserta1, PL_FA_TRANSPARENT)
{ PRED_LD

  return assert_term(A1, NULL, CL_START, NULL_ATOM, NULL, 0) != NULL;
}


#define mustBeVar(t) LDFUNC(mustBeVar, t)
static int
mustBeVar(DECL_LD term_t t)
{ if ( !PL_is_variable(t) )
    return PL_error(NULL, 0, NULL, ERR_UNINSTANTIATION, 2, t);

  succeed;
}


static
PRED_IMPL("assertz", 2, assertz2, PL_FA_TRANSPARENT)
{ PRED_LD
  Clause clause;

  if ( !mustBeVar(A2) )
    fail;
  if ( !(clause = assert_term(A1, NULL, CL_END, NULL_ATOM, NULL, 0)) )
    fail;

  return PL_unify_clref(A2, clause);
}


static
PRED_IMPL("asserta", 2, asserta2, PL_FA_TRANSPARENT)
{ PRED_LD
  Clause clause;

  if ( !mustBeVar(A2) )
    fail;
  if ( !(clause = assert_term(A1, NULL, CL_START, NULL_ATOM, NULL, 0)) )
    fail;

  return PL_unify_clref(A2, clause);
}


/** '$record_clause'(+Term, +Owner, +Source)
    '$record_clause'(+Term, +Owner, +Source, -Ref)

Compile a clause from loading a file. Term is the clause to be compiled.
Source defines the origin of the clause.

*/

#define record_clause(term, owner, source, ref) LDFUNC(record_clause, term, owner, source, ref)
static int
record_clause(DECL_LD term_t term, term_t owner, term_t source, term_t ref)
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

  if ( (clause = assert_term(term, NULL, CL_END, a_owner, &loc, 0)) )
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

  return record_clause(A1, A2, A3, 0);
}


static
PRED_IMPL("$record_clause", 4, record_clause, 0)
{ PRED_LD

  return record_clause(A1, A2, A3, A4);
}


/** '$start_aux'(+File, -CurrentPred) is det.
    '$end_aux'(+File, +CurrentPred) is det.
*/

static
PRED_IMPL("$start_aux", 2, start_aux, 0)
{ PRED_LD
  atom_t filename;
  SourceFile sf;
  Procedure proc;

  if ( !PL_get_atom_ex(A1, &filename) )
    return FALSE;

  sf = lookupSourceFile(filename, TRUE);
  if ( (proc=sf->current_procedure) &&
       isDefinedProcedure(proc) )
    return unify_definition(NULL, A2, proc->definition,
			    0, GP_QUALIFY|GP_NAMEARITY);
  else
    return PL_unify_nil(A2);
}


static
PRED_IMPL("$end_aux", 2, end_aux, 0)
{ PRED_LD
  atom_t filename;
  SourceFile sf;
  Procedure proc;
  int rc = TRUE;

  if ( !PL_get_atom_ex(A1, &filename) )
    fail;

  sf = lookupSourceFile(filename, TRUE);
  if ( PL_get_nil(A2) )
  { sf->current_procedure = NULL;
  } else
  { if ( get_procedure(A2, &proc, 0, GP_NAMEARITY) )
      sf->current_procedure = proc;
    else
      rc = FALSE;
  }
  releaseSourceFile(sf);
  releaseSourceFile(sf);		/* for $start_aux/2 */

  return rc;
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

  if ( !PL_strip_module(pred, &m, head) )
    return FALSE;
  if ( !PL_get_functor(head, &fd) )
    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_callable, pred);

  if ( (proc = lookupProcedure(fd, m)) )
  { abolishProcedure(proc, m);
    set(proc->definition, P_REDEFINED);	/* flag as redefined */
    return TRUE;
  } else
  { return FALSE;
  }
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
  set(proc->definition, P_FOREIGN);

  return TRUE;
}


static
PRED_IMPL("compile_predicates",  1, compile_predicates, PL_FA_TRANSPARENT)
{ PRED_LD
  term_t tail = PL_new_term_ref();
  term_t head = PL_new_term_ref();
  term_t desc = PL_new_term_ref();
  term_t modm = PL_new_term_ref();
  Module m = NULL;

  if ( !PL_strip_module_ex(A1, &m, tail) )
    return FALSE;
  PL_put_atom(modm, m->name);

  while( PL_get_list(tail, head, tail) )
  { Procedure proc;

    if ( !PL_cons_functor(desc, FUNCTOR_colon2, modm, head) ||
	 !get_procedure(desc, &proc, 0,
			GP_NAMEARITY|GP_FINDHERE|GP_EXISTENCE_ERROR) )
      return FALSE;

    if ( !setDynamicDefinition(proc->definition, FALSE) )
      return FALSE;
  }

  return PL_get_nil_ex(tail);
}




		/********************************
		*          DECOMPILER           *
		*********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
skipArgs() skips arguments. When used inside  a clause-head and the skip
is into the middle  of  a  H_VOID_N,   it  returns  the  location of the
H_VOID_N.

(*) resortDictsInClause() uses this to skip  values   in  the dict. As a
dict is essentially a compound, if the last  value is H_VOID, it will be
optimised away, resulting in <key-code>,   H_POP  instead of <key-code>,
H_VOID, H_POP.
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
      case B_FUNCTOR:
      case B_LIST:
	nested++;
        continue;
      case H_RFUNCTOR:
      case H_RLIST:
      case B_RFUNCTOR:
      case B_RLIST:
	continue;
      case H_POP:
      case B_POP:
	if ( --nested == 0 && --skip == 0 )
	  return nextPC;
        if ( nested >= 0 )
          continue;
        return PC;			/* See (*) */
      case H_ATOM:
      case H_SMALLINT:
      case H_NIL:
      case H_INT64:
      case H_INTEGER:
      case H_FLOAT:
      case H_STRING:
      case H_MPZ:
      case H_MPQ:
      case H_FIRSTVAR:
      case H_VAR:
      case H_VOID:
      case H_LIST_FF:
      case B_ATOM:
      case B_SMALLINT:
      case B_NIL:
      case B_INT64:
      case B_INTEGER:
      case B_FLOAT:
      case B_STRING:
      case B_MPZ:
      case B_MPQ:
      case B_ARGVAR:
      case B_ARGFIRSTVAR:
      case B_FIRSTVAR:
      case B_VAR0:
      case B_VAR1:
      case B_VAR2:
      case B_VAR:
      case B_VOID:
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
      case T_TRIE_GEN2:
      case T_TRIE_GEN3:
      case I_SSU_CHOICE:
      case I_SSU_COMMIT:
	return PC;
      case I_NOP:
      case I_CHP:
	continue;
#ifdef O_DEBUGGER
      case D_BREAK:
        c = decode(replacedBreak(PC));
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
machine code.

NOTE: this function must  be  kept   consistent  with  indexOfWord()  in
pl-index.c.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
argKey(Code PC, int skip, word *key)
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
        return TRUE;
      case H_ATOM:
      case H_SMALLINT:
	*key = *PC;
	return TRUE;
      case H_NIL:
	*key = ATOM_nil;
        return TRUE;
      case H_LIST_FF:
      case H_LIST:
      case H_RLIST:
	*key = FUNCTOR_dot2;
        return TRUE;
#if SIZEOF_VOIDP == 4
      case H_INT64:			/* only on 32-bit hardware! */
	*key = murmur_key(PC, 2*sizeof(*PC));
	return TRUE;
#endif
      case H_INTEGER:
#if SIZEOF_VOIDP == 4
      { int64_t val;
	val = (int64_t)(intptr_t)*PC;
	*key = murmur_key(&val, sizeof(val));
      }
#else
	*key = murmur_key(PC, sizeof(*PC));
#endif
	return TRUE;
      case H_FLOAT:
	*key = murmur_key(PC, sizeof(double));
        return TRUE;
      case H_STRING:
      case H_MPZ:
      case H_MPQ:
      { word m = *PC++;
	size_t n = wsizeofInd(m);

	*key = murmur_key(PC, n*sizeof(*PC));
	return TRUE;
      }
      case H_FIRSTVAR:
      case H_VAR:
      case H_VOID:
      case H_VOID_N:
      case H_POP:
      case I_EXITCATCH:
      case I_EXITRESET:
      case I_EXITFACT:
      case I_EXIT:			/* fact */
      case I_ENTER:			/* fix H_VOID, H_VOID, I_ENTER */
      case T_TRIE_GEN2:
      case T_TRIE_GEN3:
      case I_SSU_COMMIT:
      case I_SSU_CHOICE:
	*key = 0;
	fail;
      case I_NOP:
      case I_CHP:
	continue;
#ifdef O_DEBUGGER
      case D_BREAK:
        c = decode(replacedBreak(PC-1));
	goto again;
#endif
      default:
	Sdprintf("Unexpected VM code %d at %p\n", c, PC);
	Sdprintf("\topcode=%s\n", codeTable[c].name);
	assert(0);
        fail;
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Similar to argKey(), but does not do   imprecise  keys and only does the
first argument. This is used  by   listSupervisor().  This used to share
with argKey(), but argKey() is time critical and merging complicates it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
arg1Key(Code PC, word *key)
{ for(;;)
  { code c = decode(*PC++);

#if O_DEBUGGER
  again:
#endif
    switch(c)
    { case H_FUNCTOR:
      case H_RFUNCTOR:
	*key = (functor_t)*PC;
        return TRUE;
      case H_ATOM:
      case H_SMALLINT:
	*key = *PC;
	return TRUE;
      case H_NIL:
	*key = ATOM_nil;
        return TRUE;
      case H_LIST_FF:
      case H_LIST:
      case H_RLIST:
	*key = FUNCTOR_dot2;
        return TRUE;
      case H_INT64:
      case H_INTEGER:
      case H_FLOAT:
      case H_STRING:
      case H_MPZ:
      case H_FIRSTVAR:
      case H_VAR:
      case H_VOID:
      case H_VOID_N:
      case I_EXITCATCH:
      case I_EXITRESET:
      case I_EXITFACT:
      case I_EXIT:			/* fact */
      case I_ENTER:			/* fix H_VOID, H_VOID, I_ENTER */
      case I_SSU_CHOICE:
      case I_SSU_COMMIT:
	*key = 0;
	fail;
      case I_NOP:
      case I_CHP:
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
Return the context module  in  which  the   body  of  a  clause  will be
executed. This can  be  different  from   the  predicate's  module  when
compiling a clause into a different module, as in

    target:head :- body.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Module
clauseBodyContext(const Clause cl)
{ if ( true(cl, CL_BODY_CONTEXT) )
  { Code PC = cl->codes;

    for(;; PC = stepPC(PC))
    { code op = fetchop(PC);

      switch(op)
      { case I_CONTEXT:
	  return (Module)PC[1];
        case I_EXIT:
        case I_EXITFACT:
	  assert(0);
	  break;
      }
    }
  }

  return cl->predicate->module;
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
{ Code	 pc;			/* pc for decompilation */
  Word   argp;			/* argument pointer */
  int	 arity;			/* Arity of the predicate */
  int	 nvars;			/* size of var block */
  bit_vector *bvar_access;	/* Accessed as b_var<0..arity-1> */
  term_t bvar_args;		/* Arity size vector for moved unifications */
  term_t variables;		/* variable table (PL_new_term_refs() array) */
  term_t bindings;		/* [Offset = Var, ...] */
} decompileInfo;

#if USE_LD_MACROS
#define	decompile_head(Clause, term_t, decompileInfo)		LDFUNC(decompile_head, Clause, term_t, decompileInfo)
#define	decompileBody(body, decompileInfo, code, Code)		LDFUNC(decompileBody, body, decompileInfo, code, Code)
#define	decompileBodyNoShift(decompileInfo, code, Code)		LDFUNC(decompileBodyNoShift, decompileInfo, code, Code)
#define	build_term(f, di, dir)					LDFUNC(build_term, f, di, dir)
#define	put_functor(p, f)					LDFUNC(put_functor, p, f)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

static int decompile_head(Clause, term_t, decompileInfo *);
static int decompileBody(term_t body, decompileInfo *, code, Code);
static int decompileBodyNoShift(decompileInfo *, code, Code);
static int build_term(functor_t f, decompileInfo *di, int dir);
static int put_functor(Word p, functor_t f);

#undef LDFUNC_DECLARATIONS

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Mark all variables in the head  that   are  accessed  using B_VAR<N>. If
these variables are not marked as H_VOID in the head code they are refer
to body unifications that have been moved to the head.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define mark_bvar_access(cl, di) LDFUNC(mark_bvar_access, cl, di)
static int
mark_bvar_access(DECL_LD Clause cl, decompileInfo *di)
{ Code pc, ep;
  int max = -1;

  pc = cl->codes;
  ep = pc + cl->code_size;

  for( ; pc < ep; pc = stepPC(pc) )
  { code c = fetchop(pc);
    int index;

    switch(c)
    { case B_VAR0:
	index = 0;
        break;
      case B_VAR1:
	index = 1;
        break;
      case B_VAR2:
	index = 2;
        break;
      case B_VAR:
      case H_VAR:
      case B_ARGVAR:
	index = VARNUM(pc[1]);
        break;
      default:
	continue;
    }

    if ( index < di->arity )
    { DEBUG(MSG_COMP_ARG_UNIFY,
	    Sdprintf("Found access to argument %d\n", index+1));

      set_bit(di->bvar_access, index);
      if ( index > max )
	max = index;
    }
  }

  assert(max >= 0);
  if ( !(di->bvar_args = PL_new_term_refs(max+1)) )
    return FALSE;

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
decompileHead()  is  public  as  it  is   needed  to  update  the  index
information for clauses if this changes   when  the predicate is already
defined.  Also for intermediate  code  file   loaded  clauses  the index
information is recalculated as the constants   may  be different accross
runs.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* valHandle(term_t r) moved to pl-inline.h */

bool
decompileHead(Clause clause, term_t head)
{ GET_LD
  decompileInfo di;
  int rc;

  di.nvars       = VAROFFSET(1) + clause->prolog_vars;
  di.arity       = (int)clause->predicate->functor->arity;
  di.bindings    = 0;
  di.bvar_access = NULL;
  if ( clause->prolog_vars )
  { if ( !(di.variables = PL_new_term_refs(clause->prolog_vars)) )
      return FALSE;
  } else
    di.variables = 0;

  rc = decompile_head(clause, head, &di);
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

#define get_arg_ref(term, argp) LDFUNC(get_arg_ref, term, argp)
static void
get_arg_ref(DECL_LD term_t term, term_t argp)
{ word w = valHandle(term);
  Word p = argTermP(w, 0);
  size_t ar = arityTerm(w);
  Word ap = valTermRef(argp);

  if ( ar > 0 )
  { ap[0] = makeRefG(p);		/* points to first argument */
    ap[1] = makeRefG(p+ar-1);		/* points to last argument */
  }
}


#define next_arg_ref(argp) LDFUNC(next_arg_ref, argp)
static void
next_arg_ref(DECL_LD term_t argp)
{ Word ap = valTermRef(argp);

  if ( ap[0] != ap[1] )
  { Word p = unRef(ap[0]);

    ap[0] = makeRefG(p+1);
  }
}


#define set_bvar_argp(bv) LDFUNC(set_bvar_argp, bv)
static term_t
set_bvar_argp(DECL_LD term_t bv)
{ term_t argp;

  if ( (argp=PL_new_term_refs(2)) )
  { Word bvp = valTermRef(bv);
    Word ap = valTermRef(argp);

    ap[0] = makeRefG(bvp);
    ap[1] = ap[0];
  }

  return argp;
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

#define unifyVar(var, vars, i) LDFUNC(unifyVar, var, vars, i)
static int
unifyVar(DECL_LD Word var, term_t vars, size_t i)
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
    { Trail(var, makeRefG(v));
    } else
    { Trail(v, makeRefG(var));
    }
  } else if ( isVar(*var) )		/* retract called with bounded var */
  { Trail(var, *v);
  } else
  { assert(0);
  }

  return TRUE;
}


#define unifyVarGC(var, vars, i) LDFUNC(unifyVarGC, var, vars, i)
static int
unifyVarGC(DECL_LD Word var, term_t vars, size_t i)
{ i -= VAROFFSET(0);

  DEBUG(3, Sdprintf("unifyVarGC(%p, %ld(=%ld))\n", var, i, vars+i));

  return unify_ptrs(var, valTermRef(vars+i), ALLOW_GC|ALLOW_SHIFT);
}


static bool
decompile_head(DECL_LD Clause clause, term_t head, decompileInfo *di)
{ term_t argp = 0;
  int argn = 0;
  int pushed = 0;
  int write_bvar = FALSE;
  Definition def = clause->predicate;

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
  if ( di->arity > 0 )
  { if ( !PL_unify_functor(head, def->functor->functor) ||
	 !(argp = PL_new_term_refs(2)) )
      return FALSE;
    get_arg_ref(head, argp);
  } else
  { if ( !PL_unify_atom(head, def->functor->name) )
      return FALSE;
  }

#define INCARG() \
	do \
	{ argn++;			\
	  if ( write_bvar )		\
	  { PL_reset_term_refs(argp);	\
	    argp -= 2;			\
	    write_bvar = FALSE;		\
	  }				\
	} while(0)

#define NEXTARG \
	do \
	{ if ( !pushed )		\
	    INCARG();			\
	  next_arg_ref(argp);	\
	} while(0)

  if ( decode(*PC) == I_CHP )		/* Upsets move unification as we */
    PC++;				/* see the same arg twice */

  for(;;)
  { code c = decode(*PC++);

#if O_DEBUGGER
  again:
#endif

    if ( !pushed && di->bvar_access &&
	 true_bit(di->bvar_access, argn) )
    { if ( c == D_BREAK )
	c = decode(replacedBreak(PC-1));

      if ( c != H_VOID && c != H_VOID_N &&
	   c != I_ENTER && c != I_SSU_COMMIT && c != I_SSU_CHOICE )
      { term_t t2;

	DEBUG(MSG_COMP_ARG_UNIFY,
	      Sdprintf("Found moved var for arg %d\n", argn+1));

					/* is H_VOID */
	TRY(unifyVarGC(valTermRef(argp), di->variables,
		       VAROFFSET(argn)) );

					/* Move output to di->bvar_args */
	if ( (t2=set_bvar_argp(di->bvar_args+argn)) )
	{ assert(t2 == argp+2);
	  argp = t2;
	  write_bvar = TRUE;
	}
      } else
      { clear_bit(di->bvar_access, argn);
      }
    }

    switch(c)
    { case I_NOP:
      case I_CHP:
	continue;
#if O_DEBUGGER
      case D_BREAK:
	c = decode(replacedBreak(PC-1));
        goto again;
#endif
      case H_NIL:
	TRY(PL_unify_nil(argp));
	break;
      case H_STRING:
      case H_MPZ:
      case H_MPQ:
        { word copy = globalIndirectFromCode(&PC);
	  if ( !copy || !_PL_unify_atomic(argp, copy) )
	    return FALSE;
	  break;
	}
      case H_INTEGER:
        { intptr_t *p = (intptr_t*)PC;
	  intptr_t v = *p++;
	  PC = (Code)p;
	  TRY(PL_unify_int64(argp, v));
	  break;
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
	    break;
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
	    break;
	  } else
	    return FALSE;
	}
      case H_ATOM:
      case H_SMALLINT:
	  TRY(_PL_unify_atomic(argp, XR(*PC++)));
          break;
      case H_FIRSTVAR:
      case H_VAR:
	  TRY(unifyVarGC(valTermRef(argp), di->variables,
			 *PC++) );
	  break;
      case H_VOID:
	{ if ( !pushed )		/* FIRSTVAR in the head */
	    TRY(unifyVarGC(valTermRef(argp), di->variables,
			   VAROFFSET(argn)) );
	  break;
	}
      case H_VOID_N:
        { int n = (int)*PC++;

	  while(n-- > 0)
	  { if ( !pushed )		/* FIRSTVAR in the head */
	      TRY(unifyVarGC(valTermRef(argp), di->variables,
			     VAROFFSET(argn)) );
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
	       !PL_unify_compound(argp, fdef) )
	    return FALSE;
          get_arg_ref(argp, t2);
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
	  TRY(PL_unify_compound(argp, fdef));
          get_arg_ref(argp, argp);
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
	  break;
      case H_LIST_FF:
      { Word p;

	TRY(PL_unify_functor(argp, FUNCTOR_dot2));
	p = valTermRef(argp);
	deRef(p);
	p = argTermP(*p, 0);
        TRY(unifyVarGC(p+0, di->variables, *PC++) );
        TRY(unifyVarGC(p+1, di->variables, *PC++) );
	break;
      }
      case I_EXITCATCH:
      case I_EXITRESET:
      case I_EXITFACT:
      case I_EXIT:			/* fact */
      case I_ENTER:			/* fix H_VOID, H_VOID, I_ENTER */
      case I_SSU_COMMIT:
      case I_SSU_CHOICE:
	{ assert(argn <= di->arity);
	  assert(!pushed);

	  if ( argp )
	  { while(argn < di->arity)
	    { if ( di->bvar_access )
		clear_bit(di->bvar_access, argn);
	      TRY(unifyVarGC(valTermRef(argp), di->variables,
			     VAROFFSET(argn)));
	      NEXTARG;
	    }
	    PL_reset_term_refs(argp);
	  }

	  return TRUE;
	}
      case T_TRIE_GEN2:
      case T_TRIE_GEN3:
	return FALSE;
      default:
	  sysError("Illegal instruction in clause head: %d = %d",
		   PC[-1], decode(PC[-1]));
	  fail;
    }

    NEXTARG;

#undef NEXTARG
  }
}

#define makeVarRef(i)	((i)<<LMASK_BITS|TAG_REFERENCE)
#define isVarRef(w)	((tag(w) == TAG_REFERENCE && \
			  storage(w) == STG_INLINE) ? valInt(w) : -1)

static functor_t
clause_functor(const Clause cl)
{ switch(cl->flags & (SSU_COMMIT_CLAUSE|SSU_CHOICE_CLAUSE))
  { case 0:			return FUNCTOR_prove2;
    case SSU_COMMIT_CLAUSE:	return FUNCTOR_ssu_commit2;
    case SSU_CHOICE_CLAUSE:	return FUNCTOR_ssu_choice2;
    default:			assert(0); return 0;
  }
}


#define moved_unification(i, eq, a, di) LDFUNC(moved_unification, i, eq, a, di)
static int
moved_unification(DECL_LD int i, term_t eq, term_t a, decompileInfo *di)
{ if ( PL_unify_functor(eq, FUNCTOR_equals2) )
  { _PL_get_arg(1, eq, a);
    if ( !PL_unify(a, di->variables+i) )
      return FALSE;
    _PL_get_arg(2, eq, a);
    if ( !PL_unify(a, di->bvar_args+i) )
      return FALSE;

    return TRUE;
  }

  return FALSE;
}

#define moved_unifications(body, di, hasbody) \
	LDFUNC(moved_unifications, body, di, hasbody)
static int
moved_unifications(DECL_LD term_t body, decompileInfo *di, int hasbody)
{ int i, last;
  term_t tmp;
  term_t eq, a;

  if ( !(tmp=PL_new_term_refs(2)) )
    return FALSE;
  eq = tmp;
  a  = tmp+1;

  for(last=0, i=0; i<di->arity; i++)
  { if ( true_bit(di->bvar_access, i) )
      last = i;
  }

  for(i=0; i<=last; i++)
  { if ( true_bit(di->bvar_access, i) )
    { if ( i == last && !hasbody )
      { if ( !moved_unification(i, body, a, di) )
	  return FALSE;
	break;
      }

      if ( PL_unify_functor(body, FUNCTOR_comma2) )
      { _PL_get_arg(1, body, eq);
	if ( !moved_unification(i, eq, a, di) )
	  return FALSE;
	_PL_get_arg(2, body, body);
      }
    }
  }

  PL_reset_term_refs(tmp);

  return TRUE;
}


bool
decompile(Clause clause, term_t term, term_t bindings)
{ GET_LD
  decompileInfo dinfo;
  decompileInfo *di = &dinfo;
  term_t body;

  di->nvars	  = VAROFFSET(1) + clause->prolog_vars;
  di->arity       = (int)clause->predicate->functor->arity;
  di->bindings    = bindings;
  di->bvar_access = NULL;
  if ( clause->prolog_vars )
  { if ( !(di->variables = PL_new_term_refs(clause->prolog_vars)) )
      return FALSE;
  } else
    di->variables = 0;

  if ( true(clause, CL_HEAD_TERMS) )
  { di->bvar_access = alloca(sizeof_bitvector(di->arity));
    init_bitvector(di->bvar_access, di->arity);
    if ( !mark_bvar_access(clause, di) )
      return FALSE;
  }

  if ( true(clause, UNIT_CLAUSE) )	/* fact */
  { if ( decompile_head(clause, term, di) )
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
	return decompile_head(clause, b, di);
      }
    }

    fail;
  } else
  { term_t a = PL_new_term_ref();

    TRY(PL_unify_functor(term, clause_functor(clause)));
    _PL_get_arg(1, term, a);
    TRY(decompile_head(clause, a, di));
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

  if ( di->bvar_access )
  { if ( fetchop(PC) == I_EXIT )
      return moved_unifications(body, di, FALSE);
    else if ( !moved_unifications(body, di, TRUE) )
      return FALSE;
  }

  if ( fetchop(PC) == I_EXIT )
    return PL_unify_atom(body, ATOM_true);

  return decompileBody(body, di, I_EXIT, (Code) NULL);
}


static int
decompileBody(DECL_LD term_t body, decompileInfo *di, code end, Code until)
{ for(;;)
  { fid_t fid;
    Code PCsave = di->pc;
    term_t vbody;
    int rc;

    if ( !(fid = PL_open_foreign_frame()) )
      return FALSE;
    vbody = PL_new_term_ref();
    ARGP = valTermRef(vbody);
    rc = decompileBodyNoShift(di, end, (Code) NULL);
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
	  if ( (rc=build_term((f), di, -1)) != TRUE ) \
	    return rc; \
	}
#define BUILD_TERM_REV(f) \
	{ int rc; \
	  if ( (rc=build_term((f), di, 1)) != TRUE ) \
	    return rc; \
	}
#define TRY_DECOMPILE(di, end, until) \
	{ int rc; \
	  rc = decompileBodyNoShift(di, end, until); \
	  if ( rc != TRUE ) \
	    return rc; \
	}

static int
decompileBodyNoShift(DECL_LD decompileInfo *di, code end, Code until)
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
        case I_NOP:
	case I_CHP:
			    continue;
	case H_ATOM:
	case B_ATOM:
	case H_SMALLINT:
	case B_SMALLINT:
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

			    if ( (rc=put_int64(ARGP++, i, 0)) != TRUE )
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
	case A_MPQ:
	case B_MPQ:
			  { size_t sz = gsizeIndirectFromCode(PC);

			    if ( !hasGlobalSpace(sz) )
			      return GLOBAL_OVERFLOW;

			    *ARGP++ = globalIndirectFromCode(&PC);
			    continue;
			  }
        case A_ROUNDTOWARDS_A:
			  { int i = *PC++;
			    *ARGP++ = float_rounding_name(i);
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
	case A_ROUNDTOWARDS_V:
	case B_VAR:	    index = *PC++;		goto var_common;
	case A_VAR0:
	case B_VAR0:	    index = VAROFFSET(0);	goto var_common;
	case A_VAR1:
	case B_VAR1:	    index = VAROFFSET(1);	goto var_common;
	case A_VAR2:
	case B_VAR2:	    index = VAROFFSET(2);	var_common:
			    if ( nested )
			    { int rc = unifyVar(ARGP++, di->variables,
						index);
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
      case B_UNIFY_VF:
			    ARGP[1] = makeVarRef((int)*PC++);
			    ARGP[0] = makeVarRef((int)*PC++);
			    ARGP += 2;
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
      case B_ARG_VF:
			    *ARGP++ = makeVarRef((int)*PC++);
			    goto arg_vf_cont;
      case B_ARG_CF:
			    *ARGP++ = (word)*PC++;
			    arg_vf_cont:
			    *ARGP++ = makeVarRef((int)*PC++);
			    *ARGP++ = makeVarRef((int)*PC++);
			    BUILD_TERM(FUNCTOR_arg3);
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

	  if ( (rc=put_functor(&w, fdef)) != TRUE )
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

	if ( (rc=put_functor(&w, FUNCTOR_dot2)) != TRUE )
	  return rc;
	*ARGP++ = w;
	p = argTermP(w, 0);
	if ( (rc=unifyVar(p+0, di->variables, v1)) != TRUE ||
	     (rc=unifyVar(p+1, di->variables, v2)) != TRUE )
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

	  if ( (rc=put_functor(&w, fdef)) != TRUE )
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
			    BUILD_TERM_REV(FUNCTOR_plus2);
			    continue;
      case A_MUL:
			    BUILD_TERM_REV(FUNCTOR_star2);
			    continue;
      case A_FUNC0:
      case A_FUNC1:
      case A_FUNC2:
			    BUILD_TERM_REV(functorArithFunction((int)*PC++));
			    continue;
      case A_FUNC:
			    BUILD_TERM_REV(functorArithFunction((int)*PC++));
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
    { functor_t f;
      case I_VAR:	    f = FUNCTOR_var1;
    common_type_test:
			    *ARGP++ = makeVarRef((int)*PC++);
			    BUILD_TERM(f);
			    pushed++;
			    continue;
      case I_NONVAR:	    f = FUNCTOR_nonvar1;   goto common_type_test;
      case I_INTEGER:	    f = FUNCTOR_integer1;  goto common_type_test;
      case I_RATIONAL:	    f = FUNCTOR_rational1; goto common_type_test;
      case I_FLOAT:	    f = FUNCTOR_float1;    goto common_type_test;
      case I_NUMBER:	    f = FUNCTOR_number1;   goto common_type_test;
      case I_ATOMIC:	    f = FUNCTOR_atomic1;   goto common_type_test;
      case I_ATOM:	    f = FUNCTOR_atom1;     goto common_type_test;
      case I_STRING:	    f = FUNCTOR_string1;   goto common_type_test;
      case I_COMPOUND:	    f = FUNCTOR_compound1; goto common_type_test;
      case I_CALLABLE:	    f = FUNCTOR_callable1; goto common_type_test;
      case I_CALLCONT:	    f = FUNCTOR_dcall_continuation1;
						   goto common_type_test;
      case I_SHIFT:	    f = FUNCTOR_dshift1;
						   goto common_type_test;
      case I_SHIFTCP:	    f = FUNCTOR_dshift_for_copy1;
						   goto common_type_test;
    }
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
      case I_RESET:	    *ARGP++ = ATOM_dreset;
			    pushed++;
			    continue;
      case I_YIELD:	    *ARGP++ = ATOM_dyield;
			    pushed++;
			    continue;
      case I_DET:	    *ARGP++ = ATOM_dollar;
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
						cm);
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
      case C_FASTCUT:
      case C_VAR:
      case C_JMP:
			    PC++;
			    continue;
      case L_NOLCO:
			    PC += *PC;
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
      case C_DET:				/* $ A */
			  { PC += 2;		/* skip the two arguments */
			    TRY_DECOMPILE(di, C_DETTRUE, NULL);   /* A */
			    do			/* Skip to after C_DETFALSE */
			    { op = fetchop(PC);
			      PC = stepPC(PC);
			    } while(op != C_DETFALSE);
			    BUILD_TERM(FUNCTOR_dollar1);
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
      case C_FASTCOND:
			    icut = C_FASTCUT;
			    f = FUNCTOR_ifthen2;
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
      case C_IFTHEN:					/* A -> B */
			    cut = C_CUT;		/* C_CUT takes 1 arg */
			    f = FUNCTOR_ifthen2;
			    goto c_ifthen;
      case C_SOFTIFTHEN:				/* A *->B */
			    cut = C_SCUT;		/* C_SCUT takes 0 args */
			    f = FUNCTOR_softcut2;
			  c_ifthen:
			    PC++;
			    TRY_DECOMPILE(di, cut, NULL);     /* A */
                            PC += ( cut == C_CUT ? 2 : 1 );   /* skip C_(S)CUT */
			    TRY_DECOMPILE(di, C_END, NULL);   /* B */
			    PC++;
			    BUILD_TERM(f);
			    pushed++;
			    continue;
    }
#endif /* O_COMPILE_OR */
      case I_EXITCATCH:
      case I_EXITRESET:
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Decompile a range of the body to a  body term, using the local variables
from `fr`.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define decompile_body_range(goal, fr, clause, start, end) LDFUNC(decompile_body_range, goal, fr, clause, start, end)
static int
decompile_body_range(DECL_LD term_t goal, LocalFrame fr, Clause clause,
		     Code start, code end)
{ decompileInfo dinfo;
  decompileInfo *di = &dinfo;

  di->nvars	  = VAROFFSET(1) + clause->prolog_vars;
  di->arity       = (int)clause->predicate->functor->arity;
  di->bindings    = 0;
  di->bvar_access = NULL;
  di->pc          = start;

  if ( clause->prolog_vars )
    di->variables = consTermRef(argFrameP(fr,0));
  else
    di->variables = 0;

  return decompileBody(goal, di, end, NULL);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
det_goal_error() deals with errors of  $/1. First, start_of_cdet() finds
the C_DET instruction that starts the  guarded   area.  We walk the code
from the start of the clause. If we   find  a C_DET, the location of the
C_DETFALSE is easily computed. If this  is   our  error  location we are
done. If not and our error location is a C_DETTRUE we walk the code from
the C_DETTRUE to the C_DETFALSE and we   are  happy if this section only
contains C_JMP and variable balancing instructions.  If not, we have the
wrong C_DET.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Code
start_of_cdet(Clause cl, Code pc_error)
{ Code pc  = cl->codes;
  Code end = pc + cl->code_size;

  for(; pc < end; pc = stepPC(pc))
  { code c = fetchop(pc);

    if ( c == C_DET )
    { Code pcfalse = pc+pc[2]+3;

      assert(fetchop(pcfalse) == C_DETFALSE);
      if ( pc_error == pcfalse )
	return pc;

      if ( fetchop(pc_error) == C_DETTRUE )
      { Code pc2;

	for(pc2=pc_error; pc2<pcfalse; pc2=stepPC(pc2))
	{ c = fetchop(pc2);
	  if ( !(c == C_DETTRUE || c == C_JMP || c == C_VAR_N || c == C_VAR) )
	    goto continue_outer;
	}

	return pc;
      }
    }
  continue_outer:;
  }

  assert(0);
  return NULL;
}

int
det_goal_error(DECL_LD LocalFrame fr, Code pc_error, atom_t found)
{ Clause cl   = fr->clause->value.clause;
  Code pc_det = start_of_cdet(cl, pc_error);
  fid_t fid;
  atom_t a = ATOM_error;

  PL_current_prolog_flag(ATOM_determinism_error, PL_ATOM, &a);
  if ( a == ATOM_silent )
    return TRUE;

  DEBUG(MSG_DETERMINISM,
	{ size_t offset = pc_det - cl->codes;
	  Sdprintf("$/1 in %d-th clause of %s at PC=%zd: Goal %s\n",
		   clauseNo(cl, generationFrame(fr)),
		   predicateName(fr->predicate),
		   offset,
		   found == ATOM_nondet ? "succeeded with choice point"
					: "failed");
	});

  if ( found == ATOM_fail )
    clearUninitialisedVarsFrame(fr, pc_det);

  if ( (fid=PL_open_foreign_frame()) )
  { term_t goal = PL_new_term_ref();
    int rc;

    if ( (rc=decompile_body_range(goal, fr, cl, pc_det+3, C_DETTRUE)) )
    { if ( a == ATOM_warning )
      { rc = printMessage(ATOM_warning,
			  PL_FUNCTOR, FUNCTOR_error2,
			    PL_FUNCTOR, FUNCTOR_determinism_error4,
			      PL_TERM, goal,
			      PL_ATOM, ATOM_det,
			      PL_ATOM, found,
			      PL_ATOM, ATOM_goal,
			    PL_VARIABLE);
      } else
      { rc = PL_error(NULL, 0, NULL, ERR_DET_GOAL,
		      goal, ATOM_det, found);
      }
    }

    PL_close_foreign_frame(fid);

    return rc;
  }

  return FALSE;
}


static int
put_functor(DECL_LD Word p, functor_t f)
{ size_t arity = arityFunctor(f);
  Word a, t;

  if ( gTop+1+arity > gMax )
    return GLOBAL_OVERFLOW;

  a = t = gTop;
  gTop += (1+arity);

  *a = f;
  while( arity-- > 0 )
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
build_term(DECL_LD functor_t f, decompileInfo *di, int dir)
{ word term;
  size_t i, arity = arityFunctor(f);
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
  if ( dir == 1 )
  { a -= arity;
    a++;
  }
					/* now a point to last argument */

  ARGP--;
  for( ; arity-- > 0; a+=dir, ARGP-- )
  { ssize_t var;

    if ( (var = isVarRef(*ARGP)) >= 0 )
    { int rc;

      if ( (rc=unifyVar(a, di->variables, var)) != TRUE )
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
{ GET_LD

  if ( how&GP_NAMEARITY )
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
	  ((how&GP_HIDESYSTEM) && true(def->module, M_SYSTEM))) )
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
      if ( def->module )
      { if ( !PL_unify_atom(h, def->module->name) )
	{ atom_t a;
	  Module m;

	  if ( !PL_get_atom(h, &a) ||
	       !(m = isCurrentModule(a)) ||
	       !isSuperModule(def->module, m) )
	    fail;
	}
      } else
      { PL_unify_atom(h, ATOM_garbage_collected);
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


#define unify_atom_compound(t1, t2) LDFUNC(unify_atom_compound, t1, t2)
static int
unify_atom_compound(DECL_LD term_t t1, term_t t2)
{ if ( !PL_unify(t1, t2) )
  { Word p1 = valTermRef(t1);
    Word p2 = valTermRef(t2);

    deRef(p1);
    deRef(p2);
    if ( isAtom(*p1) && isTerm(*p2) )
    { FunctorDef fd = valueFunctor(functorTerm(*p2));

      if ( fd->name == *p1 && fd->arity == 0 )
	return TRUE;
    } else if ( isAtom(*p2) && isTerm(*p1) )
    { FunctorDef fd = valueFunctor(functorTerm(*p1));

      if ( fd->name == *p2 && fd->arity == 0 )
	return TRUE;
    }

    return FALSE;
  }

  return TRUE;
}


#define unify_head(h, d) LDFUNC(unify_head, h, d)
static int
unify_head(DECL_LD term_t h, term_t d)
{ if ( !unify_atom_compound(h, d) )
  { term_t h1, d1;
    Module m = NULL;

    if ( !(h1 = PL_new_term_ref()) ||
	 !(d1 = PL_new_term_ref()) )
      return FALSE;

    return ( PL_strip_module(h, &m, h1) &&
	     PL_strip_module(d, &m, d1) &&
	     unify_atom_compound(h1, d1)
	   );
  } else
    return TRUE;
}


#define protected_predicate(def) LDFUNC(protected_predicate, def)
static int
protected_predicate(DECL_LD Definition def)
{ if ( true(def, P_FOREIGN) ||
       (   false(def, (P_DYNAMIC|P_CLAUSABLE)) &&
	   (   truePrologFlag(PLFLAG_PROTECT_STATIC_CODE) ||
	       truePrologFlag(PLFLAG_ISO)
	   )
       ) )
  { Procedure proc = getDefinitionProc(def);
    PL_error(NULL, 0, NULL, ERR_PERMISSION_PROC,
	     ATOM_access, ATOM_private_procedure, proc);
    return TRUE;
  }

  return FALSE;
}


/** clause(H, B).
    clause(H, B, Ref).
    '$clause'(H, B, Ref, Bindings).
*/

#define IS_CLAUSE 0x1
#define IS_RULE   0x2


static foreign_t
clause(term_t head, term_t body, term_t ref, term_t bindings, int flags,
       int PL__ac, control_t PL__ctx)
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
  fid_t fid;
  int rc = FALSE;

  if ( CTX_ARITY < 3 ) ref = 0;
  if ( CTX_ARITY < 4 ) bindings = 0;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { Clause clause;
      definition_ref *dref;

      if ( ref && !PL_is_variable(ref) )
      { int rc;

	if ( (rc=PL_get_clref(ref, &clause)) )
	{ term_t tmp;
	  int hflags = 0;

	  if ( rc < 0 && CTX_ARITY < 4 )
	    return FALSE;			/* erased clause */

	  if ( protected_predicate(clause->predicate) )
	    return FALSE;
	  if ( decompile(clause, term, bindings) != TRUE )
	    return FALSE;
	  def = clause->predicate;
	  if ( true(clause, GOAL_CLAUSE) )
	  { tmp = head;
	  } else
	  { tmp = PL_new_term_ref();
	    if ( !unify_definition(contextModule(LD->environment), head, def, tmp, 0) )
	      fail;
	  }
	  if ( !get_head_and_body_clause(term, h, b, NULL, &hflags) )
	    return FALSE;
	  if ( (clause->flags & CLAUSE_TYPE_MASK) != hflags )
	    return FALSE;
	  if ( !unify_head(tmp, h) )
	    return FALSE;
	  if ( PL_unify(body, (flags&IS_CLAUSE) ? b : term) )
	    return TRUE;
	}

	return FALSE;
      }
      if ( !get_procedure(head, &proc, 0, GP_FIND) )
	return FALSE;
      def = getProcDefinition(proc);
      if ( !isDefinedProcedure(proc) && true(def, P_AUTOLOAD) )
	def = trapUndefined(def);

      if ( protected_predicate(def) )
	return FALSE;
      if ( !(dref=pushPredicateAccessObj(def)) )
	return FALSE;

      chp = NULL;
      setGenerationFrameVal(environment_frame, dref->generation);
      break;
    }
    case FRG_REDO:
      chp = CTX_PTR;
      def = chp->cref->value.clause->predicate;
      break;
    case FRG_CUTTED:
      chp = CTX_PTR;
      def = chp->cref->value.clause->predicate;
      popPredicateAccess(def);
      freeForeignState(chp, sizeof(*chp));
      succeed;
    default:
      assert(0);
      fail;
  }

  if ( def->functor->arity > 0 )
  { if ( !PL_strip_module(head, &module, head) )
    { popPredicateAccess(def);
      return FALSE;
    }
    argv = valTermRef(head);
    deRef(argv);
    argv = argTermP(*argv, 0);
  } else
    argv = NULL;

  if ( !chp )
  { chp = &chp_buf;
    cref = firstClause(argv, environment_frame, def, chp);
  } else
  { cref = nextClause(chp, argv, environment_frame, def);
  }

  if ( !(fid = PL_open_foreign_frame()) )
    goto out;

  while(cref)
  { Clause clause = cref->value.clause;

    if ( decompile(clause, term, bindings) )
    { int hflags = 0;
      int rc2;

      if ( (flags&IS_CLAUSE) )
      { if ( !get_head_and_body_clause(term, h, b, NULL, &hflags) )
	  break;
	rc2 = ( unify_head(head, h) &&
		PL_unify(b, body) );
      } else
      { rc2 = PL_unify(term, body);
      }

      if ( rc2 && ref )
	rc2 = PL_unify_clref(ref, clause);

      if ( rc2 )
      { if ( !chp->cref )
	{ rc = TRUE;
	  goto out;
	}
	if ( chp == &chp_buf )
	{ chp = allocForeignState(sizeof(*chp));
	  *chp = chp_buf;
	}

	PL_close_foreign_frame(fid);
	ForeignRedoPtr(chp);
      } else
      { PL_put_variable(h);		/* otherwise they point into */
	PL_put_variable(b);		/* term, which is removed */
      }
    } else if ( exception_term )
    { goto out;
    }

    PL_rewind_foreign_frame(fid);
    if ( argv )
    { argv = valTermRef(head);		/* argv may be corrupted in GC */
      deRef(argv);
      argv = argTermP(*argv, 0);
    }
    cref = nextClause(chp, argv, environment_frame, def);
  }

out:
  if ( fid )
    PL_close_foreign_frame(fid);
  if ( chp != &chp_buf )
    freeForeignState(chp, sizeof(*chp));
  popPredicateAccess(def);
  return rc;
}


static
PRED_IMPL("clause", va, clause, PL_FA_TRANSPARENT|PL_FA_NONDETERMINISTIC)
{ return clause(A1, A2, A3, A4, IS_CLAUSE, PL__ac, PL__ctx);
}

static
PRED_IMPL("rule", va, rule, PL_FA_TRANSPARENT|PL_FA_NONDETERMINISTIC)
{ return clause(A1, A2, A3, A4, IS_RULE, PL__ac, PL__ctx);
}


typedef struct
{ ClauseRef clause;			/* pointer to the clause */
  int       index;			/* nth-1 index */
} *Cref;


static
PRED_IMPL("nth_clause",  3, nth_clause, PL_FA_TRANSPARENT|PL_FA_NONDETERMINISTIC)
{ PRED_LD
  term_t p = A1;
  term_t n = A2;
  term_t ref = A3;

  Clause clause;
  ClauseRef cref;
  Procedure proc;
  Definition def;
  Cref cr;
#ifdef O_LOGICAL_UPDATE
  gen_t generation;
#endif

  if ( CTX_CNTRL == PL_PRUNED )
  { cr = CTX_PTR;

    if ( cr )
    { def = cr->clause->value.clause->predicate;
      popPredicateAccess(def);
      freeForeignState(cr, sizeof(*cr));
    }
    return TRUE;
  }

  if ( !PL_is_variable(ref) )
  { if ( PL_get_clref(ref, &clause) == TRUE )
    { int i;
      definition_ref *dref;

      if ( true(clause, GOAL_CLAUSE) )
	return FALSE;			/* I do not belong to a predicate */

      def = clause->predicate;
      if ( !(dref = pushPredicateAccessObj(def)) )
	return FALSE;
      generation = dref->generation;
      acquire_def(def);
      for( cref = def->impl.clauses.first_clause, i=1; cref; cref = cref->next)
      { if ( cref->value.clause == clause )
	{ Module m = contextModule(LD->environment);
	  int rc = ( PL_unify_integer(n, i) &&
		     unify_definition(m, p, def, 0, 0)
		   );

	  release_def(def);
	  popPredicateAccess(def);
	  return rc;
	}
	if ( visibleClauseCNT(cref->value.clause, generation) )
	  i++;
      }
      release_def(def);
      popPredicateAccess(def);
    }

    return FALSE;
  }

  if ( CTX_CNTRL == PL_FIRST_CALL )
  { int i;
    definition_ref *dref;

    if ( !get_procedure(p, &proc, 0, GP_FIND) ||
         true(proc->definition, P_FOREIGN) )
      return FALSE;

    def = getProcDefinition(proc);
    if ( !(dref = pushPredicateAccessObj(def)) )
      return FALSE;
    generation = dref->generation;
    acquire_def(def);
    cref = def->impl.clauses.first_clause;
    while ( cref && !visibleClauseCNT(cref->value.clause, generation) )
      cref = cref->next;
    release_def(def);

    if ( !cref )
    { popPredicateAccess(def);
      return FALSE;
    }

    if ( PL_get_integer(n, &i) )	/* proc and n specified */
    { i--;				/* 0-based */

      acquire_def(def);
      while(i > 0 && cref)
      { do
	{ cref = cref->next;
	} while ( cref && !visibleClauseCNT(cref->value.clause, generation) );

	i--;
      }
      release_def(def);
      popPredicateAccess(def);
      if ( i == 0 && cref )
	return PL_unify_clref(ref, cref->value.clause);
      return FALSE;
    }

    cr = allocForeignState(sizeof(*cr));
    cr->clause = cref;
    cr->index  = 1;
    setGenerationFrameVal(environment_frame, generation);
  } else
  { cr = CTX_PTR;
    def = cr->clause->value.clause->predicate;
    generation = generationFrame(environment_frame);
  }

  PL_unify_integer(n, cr->index);
  PL_unify_clref(ref, cr->clause->value.clause);

  acquire_def(def);
  cref = cr->clause->next;
  while ( cref && !visibleClauseCNT(cref->value.clause, generation) )
    cref = cref->next;
  release_def(def);

  if ( cref )
  { cr->clause = cref;
    cr->index++;
    ForeignRedoPtr(cr);
  }

  freeForeignState(cr, sizeof(*cr));
  popPredicateAccess(def);

  return TRUE;
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

      if ( def->impl.any.defined ||	/* defined and not the same */
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

	      if ( xr && PL_unify_atom(term, xr->name) )
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
vm_list(Code start, Code end)
{ Code PC;

  for(PC=start; !end || PC < end; PC=stepPC(PC))
  { code op = fetchop(PC);
    const code_info *ci = &codeTable[op];

    Sdprintf("%-3d %s\n", PC-start, ci->name);
    if ( !end )
    { switch(op)
      { case I_EXIT:
	case I_EXITFACT:
	case I_EXITCATCH:
	case I_EXITRESET:
	case I_EXITQUERY:
	case I_FEXITDET:
	case I_FEXITNDET:
	case I_FREDO:
	case S_TRUSTME:
	case S_LIST:
	  return;
      }
    }
  }
}


		 /*******************************
		 *	 CLAUSE <-> PROLOG	*
		 *******************************/

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
        case CA1_FVAR:
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
	  if ( m )			/* I_DEPARTAM can have NULL module */
	    PL_put_atom(av+an, m->name);
	  rc = TRUE;
	  break;
	}
	case CA1_PROC:
	{ Procedure proc = (Procedure)*bp++;

	  rc = unify_definition(MODULE_user, av+an, proc->definition, 0,
				GP_QUALIFY|GP_NAMEARITY);
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
	case CA1_MPQ:
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

static const code_info *lookup_vmi(atom_t name);

/** '$vmi_property'(+VMI, ?Property)
 *
 *  True when Property holds for the VMI instruction. Defined properties
 *  are:
 *
 *    - break(-Boolean)
 *      True if this is a _breakable_ instruction
 *    - argv(-List)
 *      List is a list of arguments processed by the VMI
 */

const char *ca1_name[] = {
  NULL,
  "proc",				/* 1 */
  "func",				/* 2 */
  "data",				/* 3 */
  "integer",				/* 4 */
  "int64",				/* 5 */
  "float",				/* 6 */
  "string",				/* 7 */
  "mpz",				/* 8 */
  "mpq",				/* 9 */
  "module",				/* 10 */
  "var",				/* 11 */
  "fvar",				/* 12 */
  "chp",				/* 13 */
  "foreign",				/* 14 */
  "clauseref",				/* 15 */
  "jump",				/* 16 */
  "afunc",				/* 17 */
  "trie_node"				/* 18 */
};
static atom_t ca1_info[CA1_END];

static void
fill_ca1_info(void)
{ if ( !ca1_info[CA1_END-1] )
  { int i;

    for(i=1; i < CA1_END; i++)
    { ca1_info[i] = PL_new_atom(ca1_name[i]);
    }
  }
}

static
PRED_IMPL("$vmi_property", 2, vmi_property, 0)
{ GET_LD
  atom_t vname;

  if ( PL_get_atom_ex(A1, &vname) )
  { const code_info *ci = lookup_vmi(vname);

    if ( ci )
    { atom_t prop;
      size_t arity;

      if ( PL_get_name_arity(A2, &prop, &arity) && arity == 1 )
      { term_t arg = PL_new_term_ref();

	_PL_get_arg(1, A2, arg);
	if ( prop == ATOM_break )
	{ return PL_unify_bool_ex(arg, (ci->flags&VIF_BREAK));
	} else if ( prop == ATOM_argv )
	{ const char *ats = ci->argtype;
	  term_t tail = PL_copy_term_ref(arg);
	  term_t head = PL_new_term_ref();
	  int an;

	  fill_ca1_info();
	  for(an=0; ats[an]; an++)
	  { if ( !PL_unify_list(tail, head, tail) ||
		 !PL_unify_atom(head, ca1_info[(unsigned)ats[an]]) )
	      return FALSE;
	  }
	  return PL_unify_nil(tail);
	} else
	  return PL_domain_error("vmi_property", A2);
      }

      return PL_type_error("vmi_property", A2);
    }

    return PL_existence_error("vmi", A1);
  }

  return FALSE;
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
'$vm_assert'(+PIorClause, :VM, -Ref) is det.

Create a clause from VM and assert it  to the predicate PI. If the first
argument is a clause  reference,  _replace_   the  given  clause. Ref is
unified with a reference to the new clause.

TBD The current implementation is very incomplete. Using direct jumps is
very unattractive and we should abstract away from some details, such as
the different integer sizes (inline,  int,   int64,  mpz). Also variable
balancing (C_VAR) is missing.  Possibly  we   should  add  these using a
wrapper library in Prolog.

@bug Replacement is not (yet) atomic.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static const code_info *
lookup_vmi(atom_t name)
{ GET_LD
  static Table ctable = NULL;

  if ( !ctable )
  { PL_LOCK(L_MISC);
    if ( !ctable )
    { int i;

      ctable = newHTable(32);
      for(i=0; i<I_HIGHEST; i++)
	addNewHTable(ctable,
		     (void*)PL_new_atom(codeTable[i].name),
		     (void*)&codeTable[i]);
    }
    PL_UNLOCK(L_MISC);
  }

  return lookupHTable(ctable, (void*)name);
}



static int
vm_compile_instruction(term_t t, CompileInfo ci)
{ GET_LD
  atom_t name;
  size_t arity;

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
	    case CA1_FVAR:
	    case CA1_CHP:
	    { size_t vn, i;

	      if ( !PL_get_size_ex(a, &vn) )
		fail;
	      i = VAROFFSET(vn);
	      Output_a(ci, i);
	      vn++;			/* var 1 requires 2 vars */
	      if ( (ats[an] == CA1_VAR || ats[an] == CA1_FVAR) &&
		   ci->clause->prolog_vars < vn )
		ci->clause->prolog_vars = (unsigned int)vn;
	      if ( ci->clause->variables < vn )
		ci->clause->variables = (unsigned int)vn;
	      break;
	    }
	    case CA1_INTEGER:
	    case CA1_JUMP:
	    { intptr_t val;

	      if ( !PL_get_intptr_ex(a, &val) )
		fail;
	      Output_a(ci, val);
	      break;
	    }
	    case CA1_FLOAT:
	    { double d;
	      Word p = (Word)&d;

	      if ( !PL_get_float_ex(a, &d) )
		fail;
	      Output_an(ci, p, WORDS_PER_DOUBLE);
	      break;
	    }
	    case CA1_INT64:
	    { int64_t val;
	      Word p = (Word)&val;

	      if ( !PL_get_int64_ex(a, &val) )
		fail;
	      Output_an(ci, p, WORDS_PER_INT64);
	      break;
	    }
	    case CA1_MPZ:
	    case CA1_MPQ:
	    case CA1_STRING:
	    { Word ap = valTermRef(a);
	      Word p;
	      size_t n;

	      deRef(ap);
	      switch(ats[an])
	      {
#ifdef O_GMP
	        case CA1_MPZ:
		  if ( !isMPQNum(*ap) )
		    return PL_error(NULL, 0, "must be an mpz", ERR_TYPE, ATOM_integer, a);
		  break;
		case CA1_MPQ:
		  if ( !isMPQNum(*ap) )
		    return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_rational, a);
		  break;
#endif
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
	      if ( isAtom(val) )
		PL_register_atom(val);

	      Output_a(ci, val);
	      break;
	    }
	    case CA1_FUNC:
	    { functor_t f;

	      if ( !get_functor(a, &f, NULL, 0, GP_NOT_QUALIFIED|GF_NAMEARITY) )
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
	    case CA1_AFUNC:
	    { functor_t f;
	      int findex;

	      if ( !get_functor(a, &f, NULL, 0, GP_NOT_QUALIFIED|GF_NAMEARITY) )
		fail;
	      findex = indexArithFunction(f);
	      Output_a(ci, (code)findex);
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
	    default:
	      assert(0);
	  }
	} /* for(an=0; ats[an]; an++) */
      } /* if ( arity == 0 ) */
    } else
    { return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_vmi, t);
    }
  } else
  { return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_vmi, t);
  }

  return TRUE;
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
  Clause orig = NULL;
  ClauseRef where = CL_END;
  Definition def;
  compileInfo ci;
  struct clause clause = {0};
  Clause cl;
  ClauseRef cref;
  Module m = NULL;
  size_t size, clsize;

  if ( PL_get_clref(A1, &orig) == TRUE )
  { ClauseRef cr;

    def = orig->predicate;
    acquire_def(def);
    for(cr = def->impl.clauses.first_clause; cr; cr = cr->next)
    { if ( cr->value.clause == orig )
      { where = cr;
	break;
      }
    }
    release_def(def);
  } else
  { Procedure proc;

    if ( !get_procedure(A1, &proc, 0, GP_DEFINE|GP_NAMEARITY) )
      return FALSE;
    def = getProcDefinition(proc);
  }
  if ( !PL_strip_module(A2, &m, A2) )	/* body module */
    return FALSE;

  ci.islocal      = FALSE;
  ci.subclausearg = 0;
  ci.arity        = (int)def->functor->arity;
  ci.argvars      = 0;

  clause.predicate   = def;
  clause.variables   = ci.arity;
  clause.prolog_vars = ci.arity;
  ci.clause	     = &clause;
  ci.module	     = m;
  initBuffer(&ci.codes);
  initMerge(&ci);

  if ( !vm_compile(A2, &ci) )
  { discardBuffer(&ci.codes);
    fail;
  }

  m = def->module;
  clause.code_size = entriesBuffer(&ci.codes, code);
  size   = sizeofClause(clause.code_size);
  clsize = size + SIZEOF_CREF_CLAUSE;
  if ( m->code_limit && clsize + m->code_size > m->code_limit )
  { int rc = PL_error(NULL, 0, NULL, ERR_RESOURCE, ATOM_program_space);
    discardBuffer(&ci.codes);
    return rc;
  }
  cl = PL_malloc_atomic(size);
  ATOMIC_ADD(&m->code_size, clsize);
  memcpy(cl, &clause, sizeofClause(0));
  GD->statistics.codes += clause.code_size;
  memcpy(cl->codes, baseBuffer(&ci.codes, code), sizeOfBuffer(&ci.codes));
  discardBuffer(&ci.codes);
  ATOMIC_ADD(&GD->statistics.codes, cl->code_size);
  ATOMIC_INC(&GD->statistics.clauses);
  if ( orig )
  { cl->line_no   = orig->line_no;
    cl->source_no = orig->source_no;
    cl->owner_no  = orig->owner_no;
    acquireSourceFileNo(cl->source_no);
    if ( cl->source_no != cl->owner_no )
      acquireSourceFileNo(cl->owner_no);
  }

					/* TBD: make atomic */
  if ( !(cref=assertDefinition(def, cl, where)) )
    return FALSE;
  if ( where != CL_END )
    retractClauseDefinition(def, where->value.clause, TRUE);

  return PL_unify_clref(A3, cref->value.clause);
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
      case C_FASTCOND:
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

#define add_node(tail, n) LDFUNC(add_node, tail, n)
static int
add_node(DECL_LD term_t tail, int n)
{ term_t h = PL_new_term_ref();
  int rval;

  rval = PL_unify_list(tail, h, tail) && PL_unify_integer(h, n);
  PL_reset_term_refs(h);

  DEBUG(MSG_SRCLOC, Sdprintf("Added %d\n", n));

  return rval;
}


#define add_1_if_not_at_end(PC, end, tail) LDFUNC(add_1_if_not_at_end, PC, end, tail)
static void
add_1_if_not_at_end(DECL_LD Code PC, Code end, term_t tail)
{ while(PC < end && (fetchop(PC) == C_VAR || fetchop(PC) == C_VAR_N) )
    PC = stepPC(PC);

  if ( PC != end )
  { DEBUG(MSG_SRCLOC, Sdprintf("not-at-end: adding 1\n"));
    add_node(tail, 1);
  }
}


static int
not_breakable(atom_t op, Clause clause, int offset)
{ GET_LD
  term_t av;

  if ( (av=PL_new_term_refs(2)) &&
       PL_put_clref(av+1, clause) &&
       PL_unify_term(av,
		     PL_FUNCTOR, FUNCTOR_break2,
		       PL_TERM, av+1,
		       PL_INT, offset) )
    return PL_error(NULL, 0, NULL, ERR_PERMISSION,
		    op, ATOM_break, av+0);

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
	 fetchop(end) == I_EXITCATCH ||
	 fetchop(end) == I_EXITRESET);

  if ( pcoffset == (int)clause->code_size )
    return PL_unify_atom(A3, ATOM_exit);

  if ( true(clause, GOAL_CLAUSE) )
    add_node(tail, 2);			/* $call :- <Body> */

  while( PC < loc )
  { code op = fetchop(PC);
    Code nextpc = stepPC(PC);

    DEBUG(MSG_SRCLOC,
	  Sdprintf("\t%s at %d\n", codeTable[op].name, PC-clause->codes));

    switch(op)
    { case I_ENTER:
      case I_SSU_CHOICE:
      case I_SSU_COMMIT:
	if ( loc == nextpc )
	{ add_node(tail, 1);

	  return PL_unify_nil(tail);
	}
	add_node(tail, 2);
	PC = nextpc;
	continue;
      case I_EXIT:
      case I_EXITFACT:
      case I_EXITCATCH:
      case I_EXITRESET:
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
	{ add_1_if_not_at_end(endloc, end, tail);

	  if ( loc <= jmploc )		/* loc is in first branch */
	  { add_node(tail, 1);
	    end = jmploc-2;
	    continue;
	  }
					/* loc is in second branch */
	  add_node(tail, 2);
	  PC = jmploc;
	  end = endloc;
	  continue;
	}

      after_construct:
	add_node(tail, 2);	/* loc is after disjunction */
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
	{ add_1_if_not_at_end(endloc, end, tail);

	  add_node(tail, 1);
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
      case C_FASTCOND:		/* <IF> C_CUT <THEN> C_JMP <jmp2> <ELSE> */
      { Code elseloc = nextpc + PC[2];
	code cut = (op == C_IFTHENELSE ? C_CUT :
		    op == C_FASTCOND   ? C_FASTCUT :
		                         C_SOFTCUT);

	endloc = elseloc + elseloc[-1];

	DEBUG(MSG_SRCLOC,
	      Sdprintf("else = %d, end = %d\n",
		       elseloc - clause->codes, endloc - clause->codes));

	if ( loc <= endloc )
	{ add_1_if_not_at_end(endloc, end, tail);

	  if ( loc <= elseloc )		/* a->b */
	  { Code cutloc = find_code1(nextpc, cut, PC[1]);

	    DEBUG(MSG_SRCLOC, Sdprintf("cut at %d\n", cutloc - clause->codes));
	    add_node(tail, 1);

	    if ( loc <= cutloc )	/* a */
	    { add_node(tail, 1);
	      end = cutloc;
	      PC = nextpc;
	    } else			/* b */
	    { add_node(tail, 2);
	      PC = cutloc + 2;
	      end = elseloc-2;
	    }
	    DEBUG(MSG_SRCLOC, Sdprintf("end = %d\n", end - clause->codes));
	    continue;
	  }
					/* c */
	  add_node(tail, 2);
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
	{ add_1_if_not_at_end(endloc, end, tail);

	  if ( loc <= cutloc )		/* a */
	  { add_node(tail, 1);

	    PC = nextpc;
	    end = cutloc;
	  } else			/* b */
	  { add_node(tail, 2);
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
	add_node(tail, 2);
        continue;
      case B_UNIFY_FIRSTVAR:		/* child frame ptr after B_UNIFY_EXIT */
      case B_UNIFY_VAR:			/* see also '$break_pc'/3 */
	do
	{ PC     = nextpc;
	  op     = fetchop(PC);
	  nextpc = stepPC(PC);
	} while(op != B_UNIFY_EXIT);
	if ( loc == nextpc )
	{ add_1_if_not_at_end(nextpc, end, tail);

	  return PL_unify_nil(tail);
	}
	add_node(tail, 2);
	PC = nextpc;
        continue;
      default:
        if ( loc == nextpc )
	{ add_1_if_not_at_end(nextpc, end, tail);

	  return PL_unify_nil(tail);
	}
        if ( codeTable[op].flags & VIF_BREAK )
	  add_node(tail, 2);
	PC = nextpc;
        continue;
    }
  }

  fail;					/* assert(0) */
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
'$break_pc'(+ClauseRef, -StartPC, -EndPC) is nondet.

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
    Code nextpc = stepPC(PC);

    if ( (codeTable[op].flags & VIF_BREAK) )
    { switch(op)
      { case B_UNIFY_FIRSTVAR:
	case B_UNIFY_VAR:
	  do
	  { op     = fetchop(nextpc);
	    nextpc = stepPC(nextpc);
	  } while(op != B_UNIFY_EXIT);
      }
      if ( PL_unify_integer(A2, PC     - clause->codes) &&
	   PL_unify_integer(A3, nextpc - clause->codes) )
	ForeignRedoInt(nextpc - clause->codes);
    }

    PC = nextpc;
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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Breaking on the compiled unification instruction is realised by breaking
both  the  start  (B_UNIFY_VAR   or    B_UNIFY_FIRSTVAR)   and  the  end
(B_UNIFY_EXIT). Note that the end instruction is always present and that
inlined unification cannot be nested.

Concurrency issues are avoided because both  setBreak and clearBreak are
called with L_BREAK locked.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int				/* must hold L_BREAK */
matching_unify_break(Clause clause, int offset, code op)
{ switch(op)
  { case B_UNIFY_VAR:
    case B_UNIFY_FIRSTVAR:
    { Code PC;

      for(PC=clause->codes + offset;
	  PC<clause->codes + clause->code_size;
	  PC = stepPC_unlocked(PC) )
      { if ( fetchop_unlocked(PC) == B_UNIFY_EXIT )
	  return PC-clause->codes;
      }
      assert(0);
    }
    default:
      return 0;				/* a matching unify can never be at 0 */
  }
}


static void
free_break_symbol(void *name, void *value)
{ BreakPoint bp = value;
  Code PC = name;

  *PC = bp->saved_instruction;

  freeHeap(bp, sizeof(*bp));
}

void
cleanupBreakPoints(void)
{ if ( breakTable )
  { destroyHTable(breakTable);
    breakTable = NULL;
  }
}

#define BRK_NOTSET 0
#define BRK_SET    1
#define BRK_EXISTS 2

static int				/* must hold L_BREAK */
setBreak(Clause clause, int offset)	/* offset is already verified */
{ int second_bp = FALSE;
  Code PC;
  code op, dop;

set_second:
  PC = clause->codes + offset;
  op = *PC;
  dop = decode(op);

  if ( !breakTable )
  { breakTable = newHTable(16);
    breakTable->free_symbol = free_break_symbol;
  }

  if ( dop == D_BREAK )
    return BRK_EXISTS;

  if ( (codeTable[dop].flags & VIF_BREAK) || second_bp )
  { BreakPoint bp = allocHeapOrHalt(sizeof(break_point));

    bp->clause = clause;
    bp->offset = offset;
    bp->saved_instruction = op;

    addNewHTable(breakTable, PC, bp);
    *PC = encode(D_BREAK);
    set(clause, HAS_BREAKPOINTS);

    if ( (offset=matching_unify_break(clause, offset, dop)) )
    { second_bp=TRUE;
      goto set_second;
    }

    return BRK_SET;
  } else
  { return not_breakable(ATOM_set, clause, offset);
  }
}


static int				/* must hold L_BREAK */
clearBreak(Clause clause, int offset)
{ GET_LD
  Code PC, PC0;
  BreakPoint bp;
  int second_bp = FALSE;

clear_second:
  PC = PC0 = clause->codes + offset;
  if ( !breakTable || !(bp = lookupHTable(breakTable, PC)) )
  { term_t brk, cl;

    if ( second_bp )
      return TRUE;
    if ( (brk=PL_new_term_ref()) &&
	 (cl=PL_new_term_ref()) &&
	 PL_unify_clref(cl, clause) &&
	 PL_unify_term(brk,
		       PL_FUNCTOR, FUNCTOR_break2,
		         PL_TERM, cl,
		         PL_INT, offset) )
      return PL_error(NULL, 0, NULL, ERR_EXISTENCE, ATOM_break, brk);
    else
      return FALSE;			/* resource error */
  }

  *PC = bp->saved_instruction;
  deleteHTable(breakTable, PC0);
  freeHeap(bp, sizeof(*bp));

  if ( (offset=matching_unify_break(clause, offset, decode(*PC))) )
  { second_bp = TRUE;
    goto clear_second;
  }

  return TRUE;
}


/* returns -1 if there is an exception or # events sent */

int
clearBreakPointsClause(Clause clause)
{ if ( breakTable )
  { int rc = TRUE;

    delayEvents();
    PL_LOCK(L_BREAK);
    for_table(breakTable, name, value,
              { BreakPoint bp = (BreakPoint)value;
		if ( bp->clause == clause )
		{ int offset = bp->offset;
		  clearBreak(clause, bp->offset);
		  rc = callEventHook(PLEV_GCNOBREAK, clause, offset) && rc;
		}
	      })
    PL_UNLOCK(L_BREAK);
    clear(clause, HAS_BREAKPOINTS);
    return sendDelayedEvents(rc);
  }

  return 0;
}


code
replacedBreakUnlocked(Code PC)
{ GET_LD
  BreakPoint bp;
  code c;

  c = decode(*PC);
  if ( c == D_BREAK )
  { if ( (bp = lookupHTable(breakTable, PC)) )
    { c = bp->saved_instruction;
    } else
    { sysError("No saved instruction for break at %p", PC);
      c = (code)-1;
    }
  }

  return c;
}


code
replacedBreak(Code PC)
{ code c;

  PL_LOCK(L_BREAK);
  c = replacedBreakUnlocked(PC);
  PL_UNLOCK(L_BREAK);

  return c;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
'$break_at'(+ClauseRef, +PC, +Bool) is det.

Set/clear a breakpoint at PC on ClauseRef.   Setting a break replaces an
instruction with D_BREAK.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("$break_at", 3, break_at, 0)
{ PRED_LD
  Clause clause = NULL;
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

  if ( rc )
  { pl_event_type et;

    if ( doit )
      et = (rc == BRK_SET ? PLEV_BREAK : PLEV_BREAK_EXISTS);
    else
      et = PLEV_NOBREAK;

    startCritical();			/* Call event handler sig_atomic */
    rc = callEventHook(et, clause, offset);
    rc = endCritical() && rc;
  }

  return rc;
}


static
PRED_IMPL("$current_break", 2, current_break, PL_FA_NONDETERMINISTIC)
{ GET_LD
  TableEnum e = NULL;			/* make gcc happy */
  BreakPoint bp;

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
    case FRG_RESUME:
      assert(0);
  }

  while( advanceTableEnum(e, NULL, (void**)&bp) )
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

  freeTableEnum(e);
  fail;
}

#endif /*O_DEBUGGER*/


		 /*******************************
		 *	      FLI		*
		 *******************************/

int
PL_assert(term_t term, module_t module, int flags)
{ GET_LD
  ClauseRef where = CL_END;

  if ( (flags&PL_ASSERTA) )
    where = CL_START;
  flags &= (PL_CREATE_THREAD_LOCAL|PL_CREATE_INCREMENTAL);

  return assert_term(term, module, where, 0, NULL, flags) != NULL;
}


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
  PRED_SHARE("$rule",   2, rule,   META|NDET|PL_FA_CREF)
  PRED_SHARE("$rule",   3, rule,   META|NDET|PL_FA_CREF)
  PRED_DEF("nth_clause",  3, nth_clause, META|NDET)
#ifdef O_DEBUGGER
  PRED_DEF("$vmi_property",	    2, vmi_property,	     0)
  PRED_DEF("$fetch_vm",		    4, fetch_vm,	     META)
  PRED_DEF("$vm_assert",	    3, vm_assert,	     META)
  PRED_DEF("$break_pc",		    3, break_pc,	     NDET)
  PRED_DEF("$clause_term_position", 3, clause_term_position, 0)
  PRED_DEF("$break_at",		    3, break_at,	     0)
  PRED_DEF("$current_break",	    2, current_break,	     NDET)
  PRED_DEF("$xr_member",	    2, xr_member,	     NDET)
#endif /*O_DEBUGGER*/
EndPredDefs
