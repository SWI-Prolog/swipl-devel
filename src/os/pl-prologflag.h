/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2024, University of Amsterdam
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

#ifndef _PL_PROLOGFLAG_H
#define _PL_PROLOGFLAG_H
#include "../pl-incl.h"

typedef struct oneof
{ size_t	count;
  int		references;
  atom_t       *values;
} oneof;

typedef struct _prolog_flag
{ unsigned short flags;			/* Type | Flags */
  short		index;			/* index in LD->prolog_flag.mask */
  union
  { atom_t	a;			/* value as atom */
    int64_t	i;			/* value as integer */
    double	f;			/* value as float */
    record_t	t;			/* value as term */
  } value;
  oneof        *oneof;
} prolog_flag;

		 /*******************************
		 *    FUNCTION DECLARATIONS	*
		 *******************************/

#define LDFUNC_DECLARATIONS
void		setPrologFlag(const char *name, unsigned int flags, ...);
int		set_prolog_flag(term_t key, term_t value, unsigned short flags);
bool		PL_get_prolog_flag(atom_t name, term_t value);
bool		setDoubleQuotes(atom_t a, unsigned int *flagp);
bool		setBackQuotes(atom_t a, unsigned int *flagp);
bool		setRationalSyntax(atom_t a, unsigned int *flagp);
void		initPrologFlags(void);
void		setABIVersionPrologFlag(void);
void		cleanupPrologFlags(void);
bool		checkPrologFlagsAccess(void);
prolog_flag *	current_prolog_flag(const char *name);
#undef LDFUNC_DECLARATIONS

#endif /*_PL_PROLOGFLAG_H*/
