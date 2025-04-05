/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2025, University of Amsterdam
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

#ifndef PL_ARGNAMES_H_INCLUDED
#define PL_ARGNAMES_H_INCLUDED

typedef struct argnames
{ int	     references;	/* Allow for sharing */
  Module     module;		/* Module of definition */
  functor_t  functor;		/* Functor to create an instance */
  atom_t    *names;		/* Names of the arguments */
} argnames;

typedef struct argnames_link
{ argnames  *argnames;
  bool	     exported;
} argnames_link;

#if USE_LD_MACROS
#define lookupArgNames(m, name) LDFUNC(lookupArgNames, m, name)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS
const argnames* lookupArgNames(const Module m, atom_t name);
size_t		argNamesArg(const argnames *an, atom_t aname);
size_t		arityArgNames(const argnames *an);
#undef LDFUNC_DECLARATIONS

#endif /*PL_ARGNAMES_H_INCLUDED*/
