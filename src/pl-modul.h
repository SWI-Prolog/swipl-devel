/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2020, University of Amsterdam
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

#include "pl-incl.h"

#ifndef _PL_MODUL_H
#define _PL_MODUL_H

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

#if USE_LD_MACROS
#define	lookupModule(name)			LDFUNC(lookupModule, name)
#define	isCurrentModule(name)			LDFUNC(isCurrentModule, name)
#define	acquireModule(name)			LDFUNC(acquireModule, name)
#define	acquireModulePtr(module)		LDFUNC(acquireModulePtr, module)
#define	stripModule(term, module, flags)	LDFUNC(stripModule, term, module, flags)
#define	stripModuleName(term, name)		LDFUNC(stripModuleName, term, name)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

Module		lookupModule(atom_t name);
Module		isCurrentModule(atom_t name);
Module		acquireModule(atom_t name);
void		acquireModulePtr(Module m);
void		releaseModule(Module m);
void		initModules(void);
void		cleanupModules(void);
int		addModuleSourceFile(SourceFile sf, Module m);
int		setSuperModule(Module m, Module s);
int		isSuperModule(Module s, Module m);
void		clearSupersModule(Module m);
int		addSuperModule(Module m, Module s, int where);
int		getUnknownModule(Module m);
Word		stripModule(Word term, Module *module, int flags);
Word		stripModuleName(Word term, atom_t *name);
bool		isPublicModule(Module module, Procedure proc);
int		exportProcedure(Module module, Procedure proc);
int		declareModule(atom_t name, atom_t class, atom_t super,
			      SourceFile sf, int line,
			      int rdef);
word		pl_context_module(term_t module);
int		atomToImportStrength(atom_t a);
word		pl_import(term_t pred);
#ifdef O_PROLOG_HOOK
word		pl_set_prolog_hook(term_t module, term_t old, term_t new);
#endif
ModuleEnum	newModuleEnum(int flags);
Module		advanceModuleEnum(ModuleEnum en);
void		freeModuleEnum(ModuleEnum en);

#undef LDFUNC_DECLARATIONS

#endif /*_PL_MODUL_H*/
