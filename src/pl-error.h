/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#ifndef COMMON
#ifndef SO_LOCAL
#ifdef HAVE_VISIBILITY_ATTRIBUTE
#define SO_LOCAL __attribute__((visibility("hidden")))
#else
#define SO_LOCAL
#endif
#endif
#define COMMON(type) SO_LOCAL type
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
  ERR_PERMISSION,		/* atom_t type, atom_t op, term_t obj*/
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
  ERR_AR_OVERFLOW,		/* void */
  ERR_AR_TYPE,			/* atom_t expected, Number value */
  ERR_AR_UNDEF,			/* void */
  ERR_AR_UNDERFLOW,		/* void */
  ERR_BUSY,			/* mutexes */
  ERR_CHARS_TYPE,		/* char *, term */
  ERR_CLOSED_STREAM,		/* IOSTREAM * */
  ERR_DDE_OP,			/* op, error */
  ERR_DIV_BY_ZERO,		/* void */
  ERR_EVALUATION,		/* atom_t what */
  ERR_FAILED,			/* predicate_t proc */
  ERR_MODIFY_STATIC_PROC,	/* predicate_t proc */
  ERR_MODIFY_THREAD_LOCAL_PROC,	/* Procedure proc */
  ERR_NOT_EVALUABLE,		/* functor_t func */
  ERR_NOT_IMPLEMENTED_PROC,	/* name, arity */
  ERR_OCCURS_CHECK,		/* Word, Word */
  ERR_PERMISSION_PROC,		/* op, type, Definition */
  ERR_SHARED_OBJECT_OP,		/* op, error */
  ERR_SIGNALLED,		/* int sig, char *name */
  ERR_SYNTAX,			/* what */
  ERR_UNDEFINED_PROC		/* Definition def */
} PL_error_code;

#define MSG_ERRNO		((char *)(-1))

COMMON(int) 		PL_error(const char *pred, int arity, const char *msg,
				 PL_error_code id, ...);
COMMON(char *) 		tostr(char *buf, const char *fmt, ...);
COMMON(int) 		printMessage(atom_t severity, ...);
COMMON(int) 		PL_get_chars_ex(term_t t, char **s, unsigned int flags);
COMMON(int) 		PL_get_atom_ex(term_t t, atom_t *a);
#ifdef ARG_LD
COMMON(int) 		PL_get_atom_ex__LD(term_t t, atom_t *a ARG_LD);
#endif
COMMON(int) 		PL_get_integer_ex(term_t t, int *i);
COMMON(int) 		PL_get_long_ex(term_t t, long *i);
COMMON(int) 		PL_get_int64_ex(term_t t, int64_t *i);
COMMON(int) 		PL_get_intptr_ex(term_t t, intptr_t *i);
COMMON(int) 		PL_get_size_ex(term_t t, size_t *i);
COMMON(int) 		PL_get_bool_ex(term_t t, int *i);
COMMON(int) 		PL_get_float_ex(term_t t, double *f);
COMMON(int) 		PL_get_char_ex(term_t t, int *p, int eof);
COMMON(int) 		PL_get_pointer_ex(term_t t, void **addrp);
COMMON(int) 		PL_unify_list_ex(term_t l, term_t h, term_t t);
COMMON(int) 		PL_unify_nil_ex(term_t l);
COMMON(int) 		PL_get_list_ex(term_t l, term_t h, term_t t);
COMMON(int) 		PL_get_nil_ex(term_t l);
COMMON(int) 		PL_unify_bool_ex(term_t t, int val);
COMMON(int) 		PL_get_arg_ex(int n, term_t term, term_t arg);
COMMON(int) 		PL_get_module_ex(term_t name, Module *m);
