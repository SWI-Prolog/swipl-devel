/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2025, University of Amsterdam
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

#include "pl-incl.h"

#ifndef _PL_PROC_H
#define _PL_PROC_H

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

#if USE_LD_MACROS
#define	isCurrentProcedure(f, m)		LDFUNC(isCurrentProcedure, f, m)
#define	get_head_functor(head, fdef, flags)	LDFUNC(get_head_functor, head, fdef, flags)
#define	assertDefinition(def, clause, where)	LDFUNC(assertDefinition, def, clause, where)
#define	assertProcedure(proc, clause, where)	LDFUNC(assertProcedure, proc, clause, where)
#define	retract_clause(clause, gen)		LDFUNC(retract_clause, clause, gen)
#define	reconsultFinalizePredicate(rl, def, r)	LDFUNC(reconsultFinalizePredicate, rl, def, r)
#define	resolveProcedure(f, module)		LDFUNC(resolveProcedure, f, module)
#define	trapUndefined(undef)			LDFUNC(trapUndefined, undef)
#define isDefinedProcedure(def)			LDFUNC(isDefinedProcedure, def)
#define hasClausesDefinition(def)		LDFUNC(hasClausesDefinition, def)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

Procedure	lookupProcedure(functor_t f, Module m) WUNUSED;
void		unallocProcedure(Procedure proc);
Procedure	isCurrentProcedure(functor_t f, Module m);
bool		importDefinitionModule(Module m,
				       Definition def, int flags);
Procedure	lookupProcedureToDefine(functor_t def, Module m);
ClauseRef	hasClausesDefinition(Definition def);
Procedure	getDefinitionProc(Definition def);
bool		isDefinedProcedure(Procedure proc);
void		shareDefinition(Definition def);
int		unshareDefinition(Definition def);
void		lingerDefinition(Definition def);
void		setLastModifiedPredicate(Definition def, gen_t gen, int flags);
bool		get_head_functor(term_t head, functor_t *fdef,
				 int flags);
bool		get_functor(term_t descr, functor_t *fdef,
			    Module *m, term_t h, int how);
bool		get_procedure(term_t descr, Procedure *proc,
			      term_t he, int f);
bool		checkModifySystemProc(functor_t f);
bool		overruleImportedProcedure(Procedure proc, Module target);
foreign_t	pl_current_predicate(term_t name, term_t functor, control_t h);
void		clear_meta_declaration(Definition def);
void		setMetapredicateMask(Definition def, const arg_info *args);
bool		isTransparentMetamask(Definition def, const arg_info *args);
ClauseRef	assertDefinition(Definition def, Clause clause,
				 ClauseRef where);
ClauseRef	assertProcedure(Procedure proc, Clause clause,
				ClauseRef where);
bool		abolishProcedure(Procedure proc, Module module);
bool		retract_clause(Clause clause, gen_t gen);
bool		retractClauseDefinition(Definition def, Clause clause,
					int notify);
void		unallocClause(Clause c);
void		freeClause(Clause c);
void		lingerClauseRef(ClauseRef c);
void		acquire_clause(Clause cl);
void		release_clause(Clause cl);
ClauseRef	newClauseRef(Clause cl, word key);
size_t		removeClausesPredicate(Definition def,
				       int sfindex, int fromfile);
void		reconsultFinalizePredicate(sf_reload *rl, Definition def,
					   p_reload *r);
void		destroyDefinition(Definition def);
void		unallocDefinition(Definition def);
void		initProcedures(void);
void		cleanupProcedures(void);
Procedure	resolveProcedure(functor_t f, Module module);
Definition	trapUndefined(Definition undef);
foreign_t	pl_abolish(term_t atom, term_t arity);
foreign_t	pl_abolish1(term_t pred);
bool		redefineProcedure(Procedure proc, SourceFile sf,
				  unsigned int suppress);
foreign_t	pl_index(term_t pred);
Definition	autoImport(functor_t f, Module m);
foreign_t	pl_require(term_t pred);
foreign_t	pl_check_definition(term_t spec);
foreign_t	pl_list_generations(term_t desc);
foreign_t	pl_check_procedure(term_t desc);
void		checkDefinition(Definition def);
Procedure	isStaticSystemProcedure(functor_t fd);
foreign_t	pl_garbage_collect_clauses(void);
bool		setDynamicDefinition(Definition def, bool isdyn);
bool		setThreadLocalDefinition(Definition def, bool isdyn);
bool		setAttrDefinition(Definition def, uint64_t attr, bool val);
bool		PL_meta_predicate(predicate_t def, const char*);
void		ddi_add_access_gen(DirtyDefInfo ddi, gen_t access);
bool		ddi_contains_gen(DirtyDefInfo ddi, gen_t access);
bool		ddi_is_garbage(DirtyDefInfo ddi,
			       gen_t start, Buffer tr_starts,
			       Clause cl);
size_t		sizeof_predicate(Definition def);

#undef LDFUNC_DECLARATIONS

		 /*******************************
		 *	INLINE DEFINITIONS	*
		 *******************************/

static inline Definition lookupDefinition(functor_t f, Module m) WUNUSED;
static inline Definition
lookupDefinition(functor_t f, Module m)
{ Procedure proc = lookupProcedure(f, m);

  return proc ? proc->definition : NULL;
}

/* The nth-0 argument of def has mode `-`
 */

static inline bool
mode_arg_is_unbound(Definition def, int arg0)
{ return def->impl.any.args[arg0].meta == MA_VAR;
}

#endif /*_PL_PROC_H*/
