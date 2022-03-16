/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, University of Amsterdam
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

#ifndef PL_COMP_H_INCLUDED
#define PL_COMP_H_INCLUDED

#if USE_LD_MACROS
#define	initWamTable(_)							LDFUNC(initWamTable, _)
#define	get_head_and_body_clause(clause, head, body, m, flags)		LDFUNC(get_head_and_body_clause, clause, head, body, m, flags)
#define	compileClause(cp, head, body, proc, module, warnings, flags)	LDFUNC(compileClause, cp, head, body, proc, module, warnings, flags)
#define	assert_term(term, m, where, owner, loc, flags)			LDFUNC(assert_term, term, m, where, owner, loc, flags)
#define	det_goal_error(fr, PC, found)					LDFUNC(det_goal_error, fr, PC, found)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

void		initWamTable(void);
void		cleanupWamTable(void);
void		separate_vmi(int nop);
void		freeVarDefs(PL_local_data_t *ld);
int		get_head_and_body_clause(term_t clause,
				 term_t head, term_t body,
				 Module *m, int *flags);
Procedure	lookupBodyProcedure(functor_t functor, Module tm);
int		compileClause(Clause *cp, Word head, Word body,
			      Procedure proc, Module module,
			      term_t warnings, int flags);
Clause		assert_term(term_t term, Module m, ClauseRef where,
			    atom_t owner, SourceLoc loc, int flags);
void		forAtomsInClause(Clause clause, void (func)(atom_t a));
Code		stepDynPC(Code PC, const code_info *ci);
bool		decompileHead(Clause clause, term_t head);
int		det_goal_error(LocalFrame fr, Code PC,
			       atom_t found);
Code		skipArgs(Code PC, int skip);
int		argKey(Code PC, int skip, word *key);
int		arg1Key(Code PC, word *key);
bool		decompile(Clause clause, term_t term, term_t bindings);
word		pl_nth_clause(term_t p, term_t n, term_t ref,
			      control_t h);
void		wamListClause(Clause clause);
Code		wamListInstruction(IOSTREAM *out, Code relto, Code bp);
int		unify_definition(Module ctx, term_t head, Definition def,
				 term_t thehead, int flags);
void		cleanupBreakPoints(void);
code		replacedBreak(Code PC);
code		replacedBreakUnlocked(Code PC);
int		clearBreakPointsClause(Clause clause) WUNUSED;
int		unify_functor(term_t t, functor_t fd, int how);
void		vm_list(Code code, Code end);
Module		clauseBodyContext(const Clause cl);

#undef LDFUNC_DECLARATIONS

static inline code
fetchop(Code PC)
{ code op = decode(*PC);

  if ( unlikely(op == D_BREAK) )
    op = decode(replacedBreak(PC));

  return op;
}

static inline code			/* caller must hold the L_BREAK lock */
fetchop_unlocked(Code PC)
{ code op = decode(*PC);

  if ( unlikely(op == D_BREAK) )
    op = decode(replacedBreakUnlocked(PC));

  return op;
}

static inline Code
stepPC(Code PC)
{ code op = fetchop(PC++);

  if ( unlikely(codeTable[op].arguments == VM_DYNARGC) )
    return stepDynPC(PC, &codeTable[op]);
  else
    return PC + codeTable[op].arguments;
}


static inline Code
stepPC_unlocked(Code PC)
{ code op = fetchop_unlocked(PC++);

  if ( unlikely(codeTable[op].arguments == VM_DYNARGC) )
    return stepDynPC(PC, &codeTable[op]);
  else
    return PC + codeTable[op].arguments;
}


#endif /*PL_COMP_H_INCLUDED*/
