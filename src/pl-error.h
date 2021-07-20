/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1997-2021, University of Amsterdam
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

#ifndef _PL_ERROR_H
#define _PL_ERROR_H

#ifndef COMMON
#define COMMON(type) extern type
#endif

typedef enum
{ ERR_NO_ERROR = 0,
				/* Used in os-directory and maybe elsewhere */
  ERR_DOMAIN,			/* atom_t domain, term_t value */
  ERR_EXISTENCE,		/* atom_t type, term_t obj */
  ERR_FILE_OPERATION,		/* atom_t action, atom_t type, term_t */
  ERR_FORMAT,			/* message */
  ERR_FORMAT_ARG,		/* seq, term */
  ERR_INSTANTIATION,		/* void */
  ERR_NOMEM,			/* void */
  ERR_NOT_IMPLEMENTED,		/* const char *what */
  ERR_PERMISSION,		/* atom_t type, atom_t op, term_t obj */
  ERR_RANGE,			/* term_t low, term_t high, term_t val */
  ERR_REPRESENTATION,		/* atom_t what */
  ERR_RESOURCE,			/* atom_t resource */
  ERR_SHELL_FAILED,		/* term_t command */
  ERR_SHELL_SIGNALLED,		/* term_t command, int signal */
  ERR_STREAM_OP,		/* atom_t action, term_t obj */
  ERR_SYSCALL,			/* void */
  ERR_TIMEOUT,			/* op, object */
  ERR_TYPE,			/* atom_t expected, term_t value */
  ERR_UNINSTANTIATION,		/* int argn, term_t term */

				/* Only used on SWI-Prolog itself */
  ERR_EXISTENCE3,		/* atom_t type, term_t obj, term_t in */
  ERR_AR_OVERFLOW,		/* void */
  ERR_AR_RAT_OVERFLOW,		/* void */
  ERR_AR_TYPE,			/* atom_t expected, Number value */
  ERR_AR_DOMAIN,		/* atom_t domain, Number value */
  ERR_AR_UNDEF,			/* void */
  ERR_AR_UNDERFLOW,		/* void */
  ERR_AR_TRIPWIRE,		/* atom_t tripwire, Number value */
  ERR_PTR_TYPE,			/* atom_t expected, Word value */
  ERR_PTR_DOMAIN,		/* atom_t expected, Word value */
  ERR_BUSY,			/* mutexes */
  ERR_CHARS_TYPE,		/* char *, term */
  ERR_CLOSED_STREAM,		/* IOSTREAM * */
  ERR_DDE_OP,			/* op, error */
  ERR_DIV_BY_ZERO,		/* void */
  ERR_EVALUATION,		/* atom_t what */
  ERR_FAILED,			/* predicate_t proc */
  ERR_MODIFY_STATIC_PROC,	/* Procedure proc */
  ERR_MODIFY_STATIC_PREDICATE,	/* Definition def */
  ERR_MODIFY_THREAD_LOCAL_PROC,	/* Procedure proc */
  ERR_NOT_EVALUABLE,		/* functor_t func */
  ERR_NOT_IMPLEMENTED_PROC,	/* name, arity */
  ERR_IMPORT_PROC,		/* proc, dest, [already-from] */
  ERR_OCCURS_CHECK,		/* Word, Word */
  ERR_PERMISSION_PROC,		/* op, type, Definition */
  ERR_PERMISSION_VMI,		/* vmi */
  ERR_SHARED_OBJECT_OP,		/* op, error */
  ERR_SIGNALLED,		/* int sig, char *name */
  ERR_SYNTAX,			/* what */
  ERR_UNDEFINED_PROC,		/* Definition def */
  ERR_DUPLICATE_KEY,		/* term_t */
  ERR_PERMISSION_SSU_DEF,	/* Definition def */
  ERR_DETERMINISM,		/* Definition def, atom_t expect,
				   atom_t found, atom_t decl */
  ERR_DET_GOAL			/* term_t goal, atom_t expect, atom_t found */
} PL_error_code;

#define MSG_ERRNO		((char *)(-1))

#if USE_LD_MACROS
#define	PL_get_atom_ex(t, a)		LDFUNC(PL_get_atom_ex, t, a)
#define	PL_get_size_ex(t, i)		LDFUNC(PL_get_size_ex, t, i)
#define	pl_get_uint64(t, i, ex)		LDFUNC(pl_get_uint64, t, i, ex)
#define	PL_get_uint64_ex(t, i)		LDFUNC(PL_get_uint64_ex, t, i)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

int		PL_error(const char *pred, int arity, const char *msg,
			 PL_error_code id, ...);
int		PL_no_memory(void);
int		printMessage(atom_t severity, ...) WUNUSED;
int		PL_get_atom_ex(term_t t, atom_t *a);
int		PL_get_size_ex(term_t t, size_t *i);
int		pl_get_uint64(term_t t, uint64_t *i, int ex);
int		PL_get_uint64_ex(term_t t, uint64_t *i);
int		PL_get_module_ex(term_t name, module_t *m);
int		PL_get_arg_ex(int n, term_t term, term_t arg);

#undef LDFUNC_DECLARATIONS

#endif /*_PL_ERROR_H*/
