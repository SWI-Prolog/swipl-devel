/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, University of Amsterdam
                         VU University Amsterdam
		         CWI, Amsterdam
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

COMMON(void)		initWamTable(void);
COMMON(void)		freeVarDefs(PL_local_data_t *ld);
COMMON(int)		get_head_and_body_clause(term_t clause,
					 term_t head, term_t body,
					 Module *m, int *flags ARG_LD);
COMMON(Procedure)	lookupBodyProcedure(functor_t functor, Module tm);
COMMON(int)		compileClause(Clause *cp, Word head, Word body,
				      Procedure proc, Module module,
				      term_t warnings, int flags ARG_LD);
COMMON(Clause)		assert_term(term_t term, Module m, ClauseRef where,
				    atom_t owner, SourceLoc loc, int flags ARG_LD);
COMMON(void)		forAtomsInClause(Clause clause, void (func)(atom_t a));
COMMON(Code)		stepDynPC(Code PC, const code_info *ci);
COMMON(bool)		decompileHead(Clause clause, term_t head);
COMMON(Code)		skipArgs(Code PC, int skip);
COMMON(int)		argKey(Code PC, int skip, word *key);
COMMON(int)		arg1Key(Code PC, word *key);
COMMON(bool)		decompile(Clause clause, term_t term, term_t bindings);
COMMON(word)		pl_nth_clause(term_t p, term_t n, term_t ref,
				      control_t h);
COMMON(void)		wamListClause(Clause clause);
COMMON(Code)		wamListInstruction(IOSTREAM *out, Code relto, Code bp);
COMMON(int)		unify_definition(Module ctx, term_t head, Definition def,
					 term_t thehead, int flags);
COMMON(code)		replacedBreak(Code PC);
COMMON(code)		replacedBreakUnlocked(Code PC);
COMMON(int)		clearBreakPointsClause(Clause clause) WUNUSED;
COMMON(int)		unify_functor(term_t t, functor_t fd, int how);
COMMON(void)		vm_list(Code code, Code end);
COMMON(Module)		clauseBodyContext(const Clause cl);

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
