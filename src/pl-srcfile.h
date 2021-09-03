/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2021, VU University Amsterdam
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

#ifndef _PL_SRCFILE_H
#define _PL_SRCFILE_H

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

#if USE_LD_MACROS
#define	reloadHasClauses(sf, proc)			LDFUNC(reloadHasClauses, sf, proc)
#define	assertProcedureSource(sf, proc, clause)		LDFUNC(assertProcedureSource, sf, proc, clause)
#define	setAttrProcedureSource(sf, proc, attr, val)	LDFUNC(setAttrProcedureSource, sf, proc, attr, val)
#define	setMetapredicateSource(sf, proc, args)		LDFUNC(setMetapredicateSource, sf, proc, args)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

int		startConsult(SourceFile f);
int		endConsult(SourceFile f);
size_t		highSourceFileIndex(void);
SourceFile	lookupSourceFile(atom_t name, int create);
int		releaseSourceFileNo(int index);
SourceFile	indexToSourceFile(int index);
void		cleanupSourceFiles(void);
void		unlinkSourceFileModule(SourceFile sf, Module m);
void		addProcedureSourceFile(SourceFile sf, Procedure proc);
int		hasProcedureSourceFile(SourceFile sf, Procedure proc);
int		reloadHasClauses(SourceFile sf, Procedure proc);
ClauseRef	assertProcedureSource(SourceFile sf, Procedure proc,
				      Clause clause);
int		setAttrProcedureSource(SourceFile sf, Procedure proc,
				       unsigned attr, int val);
int		setMetapredicateSource(SourceFile sf, Procedure proc,
				       arg_info *args);
int		exportProcedureSource(SourceFile sf, Module module,
				      Procedure proc);
void		registerReloadModule(SourceFile sf, Module module);

#undef LDFUNC_DECLARATIONS

#ifdef O_DEBUG
void		acquireSourceFile_d(SourceFile f,
				    const char *file, unsigned int line);
int		releaseSourceFile_d(SourceFile f,
				    const char *file, unsigned int line);
#define acquireSourceFile(f) acquireSourceFile_d(f, __FILE__, __LINE__)
#define releaseSourceFile(f) releaseSourceFile_d(f, __FILE__, __LINE__)
void		acquireSourceFileNo(int index);
#else
void		acquireSourceFile(SourceFile sf);
void		acquireSourceFileNo(int index);
int		releaseSourceFile(SourceFile f);
#endif

#endif /*_PL_SRCFILE_H*/