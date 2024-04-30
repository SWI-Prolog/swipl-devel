/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2024, University of Amsterdam
			      VU University Amsterdam
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

#ifndef _SWI_PROLOG_H
#define _SWI_PROLOG_H

#ifndef __SWI_PROLOG__	/* use this to switch on Prolog dialect */
#define __SWI_PROLOG__	/* normally defined by the swipl-ld compiler driver */
#endif

#ifndef __WINDOWS__
#if defined(_MSC_VER) || defined(__MINGW32__)
#define __WINDOWS__ 1
#endif
#endif

#include <stdarg.h>
#include <stdlib.h>			/* get size_t */
#include <stddef.h>
#include <inttypes.h>

#ifdef __cplusplus
extern "C" {
#endif

/* PLVERSION: 10000 * <Major> + 100 * <Minor> + <Patch> */
/* PLVERSION_TAG: a string, normally "", but for example "rc1" */

#ifndef PLVERSION
#define PLVERSION 90305
#endif
#ifndef PLVERSION_TAG
#define PLVERSION_TAG ""
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This number is incremented when the   SWI-Prolog PL_*() functions or one
of the data types is modified such that old binary extensions cannot run
reliably with the  current  version.  This   version  is  introduced  in
SWI-Prolog 8.1.30. The  most  recent   violation  of  compatibility  was
between versions 8.1.21 and 8.1.22  with   the  introduction of rational
numbers are atomic type.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define PL_FLI_VERSION      2		/* PL_*() functions */
#define	PL_REC_VERSION      3		/* PL_record_external(), fastrw */
#define PL_QLF_LOADVERSION 68		/* load all versions later >= X */
#define PL_QLF_VERSION     69		/* save version number */


		 /*******************************
		 *	       EXPORT		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Traditional and ELF-based Unix systems  don't   need  all this, but COFF
based systems need  to  import  and   export  symbols  explicitely  from
executables and shared objects (DLL). On some systems (e.g. AIX) this is
achieved using import/export files, on Windows   this  is achieved using
special  declarations  on  exported  symbols.  So,  a  symbol  is  local
(static), shared between the objects building   an executable or DLL (no
special declaration) or exported from the executable or DLL.

Both using native Microsoft MSVC as well   as recent Cygwin (tested 1.1)
compilers support __declspec(...) for exporting symbols.

As SWI-Stream.h can be included separately or together with this file we
duplicated this stuff.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef _PL_EXPORT_DONE
#define _PL_EXPORT_DONE

#if defined(_MSC_VER) || defined(__MINGW32__)
#define HAVE_DECLSPEC 1
#else
#if !defined(HAVE_VISIBILITY_ATTRIBUTE) && (__GNUC__ >= 4 || defined(__clang__))
#define HAVE_VISIBILITY_ATTRIBUTE 1
#endif
#endif

#ifdef HAVE_DECLSPEC
# ifdef PL_KERNEL
#define PL_EXPORT(type)		__declspec(dllexport) extern type
#define PL_EXPORT_DATA(type)	__declspec(dllexport) extern type
# else
#  ifdef __BORLANDC__
#define PL_EXPORT(type)		type _stdcall
#define PL_EXPORT_DATA(type)	extern type
#  else
#   ifdef __MINGW32__
#define PL_EXPORT(type)		extern type
#define PL_EXPORT_DATA(type)	extern type
#   else
#define PL_EXPORT(type)		extern type
#define PL_EXPORT_DATA(type)	__declspec(dllimport) type
#   endif
#  endif
#define install_t		__declspec(dllexport) void
# endif
#else /*!HAVE_DECLSPEC*/
# ifdef PL_SO_EXPORT
#define PL_EXPORT(type)		extern PL_SO_EXPORT type
#define PL_EXPORT_DATA(type)	extern PL_SO_EXPORT type
# else
#define PL_EXPORT(type)		extern type
#define PL_EXPORT_DATA(type)	extern type
# endif
#ifdef HAVE_VISIBILITY_ATTRIBUTE
#define install_t		__attribute__((visibility("default"))) void
#else
#define install_t		void
#endif
#endif /*HAVE_DECLSPEC*/
#endif /*_PL_EXPORT_DONE*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The following two macros are used to define types that are internal to the
SWI-Prolog library  but appear in  public  API signatures.  None of  these
types should be  used by derived code;  the mangling  done by PL_OPAQUE is
intended solely to avoid clutter  in global type namespaces and should not
be relied upon to remain unchanged across versions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
#ifndef PL_OPAQUE
#define PL_OPAQUE(type)		__PL_ ## type
#define _DEFINED_PL_OPAQUE
#endif
#ifndef PL_STRUCT
#define PL_STRUCT(name)		struct PL_OPAQUE(name)
#define _DEFINED_PL_STRUCT
#endif

/* Shorthand defines, for brevity in this file. They get undefined
 * at the bottom, so don't use them in #define definitions (which
 * use lazy evaluation) or in derived code.
 */
#define _PLQ PL_OPAQUE
#define _PLS PL_STRUCT


		 /*******************************
		 *	  GCC ATTRIBUTES	*
		 *******************************/

#if __GNUC__ >= 4
#define WUNUSED __attribute__((warn_unused_result))
#define WDEPRECATED __attribute__((deprecated))
#else
#define WUNUSED
#define WDEPRECATED
#endif


		 /*******************************
		 *	       TYPES		*
		 *******************************/

typedef uint64_t	_PLQ(word);	/* Anonymous Prolog data cell */
#ifndef PL_HAVE_ATOM_T
#define PL_HAVE_ATOM_T
typedef uintptr_t	atom_t;		/* Prolog atom */
#endif
typedef uintptr_t	functor_t;	/* Name/arity pair */
typedef uintptr_t	_PLQ(code);	/* Prolog bytecode type */
typedef _PLS(module) *	module_t;	/* Prolog module */
typedef _PLS(procedure) *predicate_t;	/* Prolog procedure */
typedef _PLS(record) *	record_t;	/* Prolog recorded term */
#ifndef PL_HAVE_TERM_T
#define PL_HAVE_TERM_T
typedef uintptr_t	term_t;		/* opaque term handle */
#endif
typedef _PLS(queryRef) *qid_t;		/* opaque query handle */
typedef uintptr_t	PL_fid_t;	/* opaque foreign context handle */
typedef _PLS(foreign_context) *control_t; /* non-deterministic control arg */
typedef _PLS(PL_local_data) *PL_engine_t; /* opaque engine handle */
typedef _PLQ(word)	PL_atomic_t;	/* same a word */
typedef uintptr_t	foreign_t;	/* return type of foreign functions */
typedef wchar_t		pl_wchar_t;	/* Prolog wide character */
#ifdef __cplusplus
typedef void *		pl_function_t;      /* pass function as void* */
#else
typedef foreign_t	(*pl_function_t)(); /* foreign language functions */
#endif
typedef uintptr_t	buf_mark_t;	/* buffer mark handle */

#define fid_t PL_fid_t			/* avoid AIX name-clash */

typedef struct io_stream IOSTREAM;	/* fully defined in SWI-Stream.h */

					/* values for PL_get_term_value() */
typedef union
{ int64_t i;				/* PL_INTEGER */
  double f;				/* PL_FLOAT */
  char * s;				/* PL_STRING */
  atom_t a;				/* PL_ATOM */
  struct				/* PL_TERM */
  { atom_t name;
    size_t arity;
  } t;
} term_value_t;


#ifndef TRUE
#define TRUE	(1)
#define FALSE	(0)
#endif

		 /*******************************
		 *      TERM-TYPE CONSTANTS	*
		 *******************************/
					/* PL_unify_term() arguments */
#define	PL_VARIABLE	 (1)		/* nothing */
#define PL_ATOM		 (2)		/* const char * */
#define PL_INTEGER	 (3)		/* int */
#define PL_RATIONAL	 (4)		/* rational number */
#define PL_FLOAT	 (5)		/* double */
#define PL_STRING	 (6)		/* const char * */
#define PL_TERM		 (7)

#define PL_NIL		 (8)		/* The constant [] */
#define PL_BLOB		 (9)		/* non-atom blob */
#define PL_LIST_PAIR	 (10)		/* [_|_] term */

					/* PL_unify_term() */
#define PL_FUNCTOR	 (11)		/* functor_t, arg ... */
#define PL_LIST		 (12)		/* length, arg ... */
#define PL_CHARS	 (13)		/* const char * */
#define PL_POINTER	 (14)		/* void * */
					/* PlArg::PlArg(text, type) */
#define PL_CODE_LIST	 (15)		/* [ascii...] */
#define PL_CHAR_LIST	 (16)		/* [h,e,l,l,o] */
#define PL_BOOL		 (17)		/* PL_set_prolog_flag() */
#define PL_FUNCTOR_CHARS (18)		/* PL_unify_term() */
#define _PL_PREDICATE_INDICATOR (19)	/* predicate_t (Procedure) */
#define PL_SHORT	 (20)		/* short */
#define PL_INT		 (21)		/* int */
#define PL_LONG		 (22)		/* long */
#define PL_DOUBLE	 (23)		/* double */
#define PL_NCHARS	 (24)		/* size_t, const char * */
#define PL_UTF8_CHARS	 (25)		/* const char * */
#define PL_UTF8_STRING	 (26)		/* const char * */
#define PL_INT64	 (27)		/* int64_t */
#define PL_NUTF8_CHARS	 (28)		/* size_t, const char * */
#define PL_NUTF8_CODES	 (29)		/* size_t, const char * */
#define PL_NUTF8_STRING	 (30)		/* size_t, const char * */
#define PL_NWCHARS	 (31)		/* size_t, const wchar_t * */
#define PL_NWCODES	 (32)		/* size_t, const wchar_t * */
#define PL_NWSTRING	 (33)		/* size_t, const wchar_t * */
#define PL_MBCHARS	 (34)		/* const char * */
#define PL_MBCODES	 (35)		/* const char * */
#define PL_MBSTRING	 (36)		/* const char * */
#define PL_INTPTR	 (37)		/* intptr_t */
#define PL_CHAR		 (38)		/* int */
#define PL_CODE		 (39)		/* int */
#define PL_BYTE		 (40)		/* int */
					/* PL_skip_list() */
#define PL_PARTIAL_LIST	 (41)		/* a partial list */
#define PL_CYCLIC_TERM	 (42)		/* a cyclic list/term */
#define PL_NOT_A_LIST	 (43)		/* Object is not a list */
					/* dicts */
#define PL_DICT		 (44)
#define PL_SWORD	 (45)		/* Signed word (internal) */

/* Or'ed flags for PL_set_prolog_flag() */
/* MUST fit in a short int! */
#define FF_READONLY	 0x1000		/* Read-only prolog flag */
#define FF_KEEP		 0x2000		/* keep prolog flag if already set */
#define FF_NOCREATE	 0x4000		/* Fail if flag is non-existent */
#define FF_FORCE	 0x8000		/* Force setting, overwrite READONLY */
#define FF_MASK		 0xf000


		/********************************
		*    DETERMINISTIC CALL/RETURN  *
		*********************************/

/* PL_succeed and PL_fail are deprecated */
#define	PL_succeed	return TRUE	/* succeed deterministically */
#define PL_fail		return FALSE	/* fail */


		/********************************
		* NON-DETERMINISTIC CALL/RETURN *
		*********************************/

/*  Note 1: Non-deterministic foreign functions may also use the deterministic
    return methods PL_succeed and PL_fail.

    Note 2: The argument to PL_retry is a sizeof(ptr)-2 bits signed
    integer (use type intptr_t).
*/

#define PL_FIRST_CALL		(0)
#define PL_CUTTED		(1)	/* deprecated */
#define PL_PRUNED		(1)
#define PL_REDO			(2)
#define PL_RESUME		(3)

#define PL_retry(n)		return _PL_retry(n)
#define PL_retry_address(a)	return _PL_retry_address(a)
#define PL_yield_address(a)	return _PL_yield_address(a)

PL_EXPORT(foreign_t)	_PL_retry(intptr_t);
PL_EXPORT(foreign_t)	_PL_retry_address(void *);
PL_EXPORT(foreign_t)	_PL_yield_address(void *);
PL_EXPORT(int)		PL_foreign_control(control_t);
PL_EXPORT(intptr_t)	PL_foreign_context(control_t);
PL_EXPORT(void *)	PL_foreign_context_address(control_t);
PL_EXPORT(predicate_t)	PL_foreign_context_predicate(control_t);


		/********************************
		*      REGISTERING FOREIGNS     *
		*********************************/

typedef struct PL_extension
{ const char   *predicate_name;		/* Name of the predicate */
  short		arity;			/* Arity of the predicate */
  pl_function_t	function;		/* Implementing functions */
  short		flags;			/* Or of PL_FA_... */
} PL_extension;

#define PL_FA_NOTRACE		(0x01)	/* foreign cannot be traced */
#define PL_FA_TRANSPARENT	(0x02)	/* foreign is module transparent (deprecated) */
#define PL_FA_NONDETERMINISTIC	(0x04)	/* foreign is non-deterministic */
#define PL_FA_VARARGS		(0x08)	/* call using t0, ac, ctx */
#define PL_FA_CREF		(0x10)	/* Internal: has clause-reference */
#define PL_FA_ISO		(0x20)	/* Internal: ISO core predicate */
#define PL_FA_META		(0x40)	/* Additional meta-argument spec */
#define PL_FA_SIG_ATOMIC	(0x80)  /* Internal: do not dispatch signals */

extern			PL_extension PL_extensions[]; /* not Win32! */
PL_EXPORT(void)		PL_register_extensions(const PL_extension *e);
PL_EXPORT(void)		PL_register_extensions_in_module(const char *module, const PL_extension *e);
PL_EXPORT(int)		PL_register_foreign(const char *name, int arity,
					    pl_function_t func,
					    int flags, ...);
PL_EXPORT(int)		PL_register_foreign_in_module(const char *module,
						      const char *name, int arity,
						      pl_function_t func,
						      int flags, ...);
PL_EXPORT(void)		PL_load_extensions(const PL_extension *e); /* WDEPRECATED */

		 /*******************************
		 *	      LICENSE		*
		 *******************************/

PL_EXPORT(void)		PL_license(const char *license, const char *module);

		/********************************
		*            MODULES            *
		*********************************/

PL_EXPORT(module_t)	PL_context(void);
PL_EXPORT(atom_t)	PL_module_name(module_t module);
PL_EXPORT(module_t)	PL_new_module(atom_t name);
PL_EXPORT(int)		PL_strip_module(term_t in, module_t *m, term_t out) WUNUSED;

		 /*******************************
		 *	     CONSTANTS		*
		 *******************************/

PL_EXPORT(const atom_t) *_PL_atoms(void); /* base of reserved (meta-)atoms */
#ifndef PL_KERNEL
#define ATOM_nil	(_PL_atoms()[0]) /* `[]` */
#define ATOM_dot	(_PL_atoms()[1]) /* `.` */
#endif /*PL_KERNEL*/


		 /*******************************
		 *	     CALL-BACK		*
		 *******************************/

#ifdef PL_KERNEL
#define PL_Q_DEBUG		0x0001	/* = TRUE for backward compatibility */
#endif
#define PL_Q_NORMAL		0x0002	/* normal usage */
#define PL_Q_NODEBUG		0x0004	/* use this one */
#define PL_Q_CATCH_EXCEPTION	0x0008	/* handle exceptions in C */
#define PL_Q_PASS_EXCEPTION	0x0010	/* pass to parent environment */
#define PL_Q_ALLOW_YIELD	0x0020	/* Support I_YIELD */
#define PL_Q_EXT_STATUS		0x0040	/* Return extended status */
#ifdef PL_KERNEL
#define PL_Q_DETERMINISTIC	0x0100	/* call was deterministic */
#endif

					/* PL_Q_EXT_STATUS return codes */
#define PL_S_NOT_INNER	       -2	/* Query is not inner query */
#define PL_S_EXCEPTION	       -1	/* Query raised exception */
#define PL_S_FALSE		0	/* Query failed */
#define PL_S_TRUE		1	/* Query succeeded with choicepoint */
#define PL_S_LAST		2	/* Query succeeded without CP */
#define PL_S_YIELD	      255	/* Foreign yield */

			/* Foreign context frames */
PL_EXPORT(fid_t)	PL_open_foreign_frame(void);
PL_EXPORT(void)		PL_rewind_foreign_frame(fid_t cid);
PL_EXPORT(void)		PL_close_foreign_frame(fid_t cid);
PL_EXPORT(void)		PL_discard_foreign_frame(fid_t cid);

			/* Finding predicates */
PL_EXPORT(predicate_t)	PL_pred(functor_t f, module_t m);
PL_EXPORT(predicate_t)	PL_predicate(const char *name, int arity,
				     const char* module);
PL_EXPORT(int)		PL_predicate_info(predicate_t pred,
					  atom_t *name, size_t *arity,
					  module_t *module);

			/* Call-back */
PL_EXPORT(qid_t)	PL_open_query(module_t m, int flags,
				      predicate_t pred, term_t t0);
PL_EXPORT(int)		PL_next_solution(qid_t qid) WUNUSED;
PL_EXPORT(int)		PL_close_query(qid_t qid);
PL_EXPORT(int)		PL_cut_query(qid_t qid);
PL_EXPORT(qid_t)	PL_current_query(void);
PL_EXPORT(PL_engine_t)	PL_query_engine(qid_t qid);
PL_EXPORT(int)		PL_can_yield(void);

			/* Simplified (but less flexible) call-back */
PL_EXPORT(int)		PL_call(term_t t, module_t m) WUNUSED;
PL_EXPORT(int)		PL_call_predicate(module_t m, int flags,
					  predicate_t pred, term_t t0); /* TODO: WUNUSED */
			/* Handling exceptions */
PL_EXPORT(term_t)	PL_exception(qid_t qid);
PL_EXPORT(int)		PL_raise_exception(term_t exception);
PL_EXPORT(int)		PL_throw(term_t exception); /* WDEPRECATED */
PL_EXPORT(void)		PL_clear_exception(void);
			/* Engine-based coroutining */
PL_EXPORT(term_t)	PL_yielded(qid_t qid);


		 /*******************************
		 *	      ASSERT		*
		 *******************************/

#define PL_ASSERTZ		0x0000
#define PL_ASSERTA		0x0001
#define PL_CREATE_THREAD_LOCAL	0x0010
#define PL_CREATE_INCREMENTAL	0x0020

PL_EXPORT(int)		PL_assert(term_t term, module_t m, int flags);




		 /*******************************
		 *        TERM-REFERENCES	*
		 *******************************/

			/* Creating and destroying term-refs */
PL_EXPORT(term_t)	PL_new_term_refs(size_t n);
PL_EXPORT(term_t)	PL_new_term_ref(void);
PL_EXPORT(term_t)	PL_copy_term_ref(term_t from);
PL_EXPORT(void)		PL_reset_term_refs(term_t r);

			/* Constants */
PL_EXPORT(atom_t)	PL_new_atom(const char *s);
PL_EXPORT(atom_t)	PL_new_atom_nchars(size_t len, const char *s);
PL_EXPORT(atom_t)	PL_new_atom_wchars(size_t len, const pl_wchar_t *s);
PL_EXPORT(atom_t)	PL_new_atom_mbchars(int rep, size_t len, const char *s);
PL_EXPORT(const char *)	PL_atom_chars(atom_t a); /* WDEPRECATED */
PL_EXPORT(const char *)	PL_atom_nchars(atom_t a, size_t *len);
PL_EXPORT(int)		PL_atom_mbchars(atom_t a, size_t *len, char **s,
					unsigned int flags);
PL_EXPORT(const wchar_t *)	PL_atom_wchars(atom_t a, size_t *len);
PL_EXPORT(void)		PL_register_atom(atom_t a);
PL_EXPORT(void)		PL_unregister_atom(atom_t a);
PL_EXPORT(size_t)	PL_atom_index(atom_t index);
PL_EXPORT(atom_t)	PL_atom_from_index(size_t a);
#ifdef O_DEBUG_ATOMGC
#define PL_register_atom(a) \
	_PL_debug_register_atom(a, __FILE__, __LINE__, __PRETTY_FUNCTION__)
#define PL_unregister_atom(a) \
	_PL_debug_unregister_atom(a, __FILE__, __LINE__, __PRETTY_FUNCTION__)
PL_EXPORT(void)		_PL_debug_register_atom(atom_t a,
						const char *file, int line,
						const char *func);
PL_EXPORT(void)		_PL_debug_unregister_atom(atom_t a,
						  const char *file, int line,
						  const char *func);
#endif
PL_EXPORT(functor_t)	PL_new_functor_sz(atom_t f, size_t a);
PL_EXPORT(functor_t)	PL_new_functor(atom_t f, int a);
PL_EXPORT(atom_t)	PL_functor_name(functor_t f);
PL_EXPORT(int)		PL_functor_arity(functor_t f);
PL_EXPORT(size_t)	PL_functor_arity_sz(functor_t f);

			/* Get C-values from Prolog terms */
PL_EXPORT(int)		PL_get_atom(term_t t, atom_t *a) WUNUSED;
PL_EXPORT(int)		PL_get_bool(term_t t, int *value) WUNUSED;
PL_EXPORT(int)		PL_get_atom_chars(term_t t, char **a) WUNUSED;
#define PL_get_string_chars(t, s, l) PL_get_string(t,s,l)
PL_EXPORT(int)		PL_get_string(term_t t, char **s, size_t *len) WUNUSED; /* WDEPRECATED */
PL_EXPORT(int)		PL_get_chars(term_t t, char **s, unsigned int flags) WUNUSED;
PL_EXPORT(int)		PL_get_list_chars(term_t l, char **s,
					  unsigned int flags) WUNUSED;
PL_EXPORT(int)		PL_get_atom_nchars(term_t t, size_t *len, char **a) WUNUSED;
PL_EXPORT(int)		PL_get_list_nchars(term_t l,
					   size_t *len, char **s,
					   unsigned int flags) WUNUSED;
PL_EXPORT(int)		PL_get_nchars(term_t t,
				      size_t *len, char **s,
				      unsigned int flags) WUNUSED;
PL_EXPORT(int)		PL_get_integer(term_t t, int *i) WUNUSED;
PL_EXPORT(int)		PL_get_long(term_t t, long *i) WUNUSED;
PL_EXPORT(int)		PL_get_intptr(term_t t, intptr_t *i) WUNUSED;
PL_EXPORT(int)		PL_get_pointer(term_t t, void **ptr) WUNUSED;
PL_EXPORT(int)		PL_get_float(term_t t, double *f) WUNUSED;
PL_EXPORT(int)		PL_get_functor(term_t t, functor_t *f) WUNUSED;
PL_EXPORT(int)		PL_get_name_arity_sz(term_t t, atom_t *name,
					     size_t *arity) WUNUSED;
PL_EXPORT(int)		PL_get_compound_name_arity_sz(term_t t, atom_t *name,
						      size_t *arity) WUNUSED;
PL_EXPORT(int)		PL_get_name_arity(term_t t, atom_t *name,
					  int *arity) WUNUSED;
PL_EXPORT(int)		PL_get_compound_name_arity(term_t t, atom_t *name,
						   int *arity) WUNUSED;
PL_EXPORT(int)		PL_get_module(term_t t, module_t *module) WUNUSED;
PL_EXPORT(int)		PL_get_arg_sz(size_t index, term_t t, term_t a) WUNUSED;
PL_EXPORT(int)		PL_get_arg(int index, term_t t, term_t a) WUNUSED;
PL_EXPORT(int)		PL_get_dict_key(atom_t key, term_t dict, term_t value);
PL_EXPORT(int)		PL_get_list(term_t l, term_t h, term_t t) WUNUSED;
PL_EXPORT(int)		PL_get_head(term_t l, term_t h) WUNUSED;
PL_EXPORT(int)		PL_get_tail(term_t l, term_t t) WUNUSED;
PL_EXPORT(int)		PL_get_nil(term_t l) WUNUSED;
PL_EXPORT(int)		PL_get_term_value(term_t t, term_value_t *v) WUNUSED;
PL_EXPORT(char *)	PL_quote(int chr, const char *data);
#define PL_FOR_DICT_SORTED	0x1
PL_EXPORT(int)		PL_for_dict(term_t dict,
				    int (*func)(term_t key, term_t value, void *closure),
				    void *closure,
				    int flags);

			/* Verify types */
PL_EXPORT(int)		PL_term_type(term_t t);
PL_EXPORT(int)		PL_is_variable(term_t t);
PL_EXPORT(int)		PL_is_ground(term_t t);
PL_EXPORT(int)		PL_is_atom(term_t t);
PL_EXPORT(int)		PL_is_integer(term_t t);
PL_EXPORT(int)		PL_is_string(term_t t);
PL_EXPORT(int)		PL_is_float(term_t t);
PL_EXPORT(int)		PL_is_rational(term_t t);
PL_EXPORT(int)		PL_is_compound(term_t t);
PL_EXPORT(int)		PL_is_callable(term_t t);
PL_EXPORT(int)		PL_is_functor(term_t t, functor_t f);
PL_EXPORT(int)		PL_is_list(term_t t);
PL_EXPORT(int)		PL_is_dict(term_t t);
PL_EXPORT(int)		PL_is_pair(term_t t);
PL_EXPORT(int)		PL_is_atomic(term_t t);
PL_EXPORT(int)		PL_is_number(term_t t);
PL_EXPORT(int)		PL_is_acyclic(term_t t);

			/* Assign to term-references */
PL_EXPORT(int)		PL_put_variable(term_t t);
PL_EXPORT(int)		PL_put_atom(term_t t, atom_t a);
PL_EXPORT(int)		PL_put_bool(term_t t, int val);
PL_EXPORT(int)		PL_put_atom_chars(term_t t, const char *chars);
PL_EXPORT(int)		PL_put_string_chars(term_t t, const char *chars) WUNUSED;
PL_EXPORT(int)		PL_put_chars(term_t t, int flags,
				     size_t len, const char *chars) WUNUSED;
PL_EXPORT(int)		PL_put_list_chars(term_t t, const char *chars) WUNUSED;
PL_EXPORT(int)		PL_put_list_codes(term_t t, const char *chars) WUNUSED;
PL_EXPORT(int)		PL_put_atom_nchars(term_t t, size_t l, const char *chars);
PL_EXPORT(int)		PL_put_string_nchars(term_t t, size_t len, const char *chars) WUNUSED;
PL_EXPORT(int)		PL_put_list_nchars(term_t t, size_t l, const char *chars) WUNUSED;
PL_EXPORT(int)		PL_put_list_ncodes(term_t t, size_t l, const char *chars) WUNUSED;
PL_EXPORT(int)		PL_put_integer(term_t t, long i) WUNUSED;
PL_EXPORT(int)		PL_put_pointer(term_t t, void *ptr) WUNUSED;
PL_EXPORT(int)		PL_put_float(term_t t, double f) WUNUSED;
PL_EXPORT(int)		PL_put_functor(term_t t, functor_t functor) WUNUSED;
PL_EXPORT(int)		PL_put_list(term_t l) WUNUSED;
PL_EXPORT(int)		PL_put_nil(term_t l);
PL_EXPORT(int)		PL_put_term(term_t t1, term_t t2) WUNUSED;
PL_EXPORT(int)		PL_put_dict(term_t t, atom_t tag, size_t len,
				    const atom_t *keys, term_t values) WUNUSED;
PL_EXPORT(atom_t)	_PL_cons_small_int(int64_t v);
PL_EXPORT(void)		_PL_unregister_keys(size_t len, atom_t *keys);

			/* construct a functor or list-cell */
PL_EXPORT(int)		PL_cons_functor(term_t h, functor_t f, ...) WUNUSED;
PL_EXPORT(int)		PL_cons_functor_v(term_t h, functor_t fd, term_t a0) WUNUSED;
PL_EXPORT(int)		PL_cons_list(term_t l, term_t h, term_t t) WUNUSED;

			/* Unify term-references */
PL_EXPORT(int)		PL_unify(term_t t1, term_t t2) WUNUSED;
PL_EXPORT(int)		PL_unify_atom(term_t t, atom_t a) WUNUSED;
PL_EXPORT(int)		PL_unify_atom_chars(term_t t, const char *chars) WUNUSED;
PL_EXPORT(int)		PL_unify_list_chars(term_t t, const char *chars) WUNUSED;
PL_EXPORT(int)		PL_unify_list_codes(term_t t, const char *chars) WUNUSED;
PL_EXPORT(int)		PL_unify_string_chars(term_t t, const char *chars) WUNUSED;
PL_EXPORT(int)		PL_unify_atom_nchars(term_t t, size_t l, const char *s) WUNUSED;
PL_EXPORT(int)		PL_unify_list_ncodes(term_t t, size_t l, const char *s) WUNUSED;
PL_EXPORT(int)		PL_unify_list_nchars(term_t t, size_t l, const char *s) WUNUSED;
PL_EXPORT(int)		PL_unify_string_nchars(term_t t,
					       size_t len,
					       const char *chars) WUNUSED;
PL_EXPORT(int)		PL_unify_bool(term_t t, int n) WUNUSED;
PL_EXPORT(int)		PL_unify_integer(term_t t, intptr_t n) WUNUSED;
PL_EXPORT(int)		PL_unify_float(term_t t, double f) WUNUSED;
PL_EXPORT(int)		PL_unify_pointer(term_t t, void *ptr) WUNUSED;
PL_EXPORT(int)		PL_unify_functor(term_t t, functor_t f) WUNUSED;
PL_EXPORT(int)		PL_unify_compound(term_t t, functor_t f) WUNUSED;
PL_EXPORT(int)		PL_unify_list(term_t l, term_t h, term_t t) WUNUSED;
PL_EXPORT(int)		PL_unify_nil(term_t l) WUNUSED;
PL_EXPORT(int)		PL_unify_arg_sz(size_t index, term_t t, term_t a) WUNUSED;
PL_EXPORT(int)		PL_unify_arg(int index, term_t t, term_t a) WUNUSED;
PL_EXPORT(int)		PL_unify_term(term_t t, ...) WUNUSED;
PL_EXPORT(int)		PL_unify_chars(term_t t, int flags,
				       size_t len, const char *s) WUNUSED;

		 /*******************************
		 *	       LISTS		*
		 *******************************/

PL_EXPORT(int)		PL_skip_list(term_t list, term_t tail, size_t *len);


		 /*******************************
		 *    WIDE CHARACTER VERSIONS	*
		 *******************************/

PL_EXPORT(int)		PL_put_wchars(term_t t, int type,
				      size_t len, const pl_wchar_t *s) WUNUSED;
PL_EXPORT(int)		PL_unify_wchars(term_t t, int type,
					size_t len, const pl_wchar_t *s) WUNUSED;
PL_EXPORT(int)		PL_unify_wchars_diff(term_t t, term_t tail, int type,
					size_t len, const pl_wchar_t *s) WUNUSED;
PL_EXPORT(int)		PL_get_wchars(term_t l,
				      size_t *length, pl_wchar_t **s,
				      unsigned flags) WUNUSED;
PL_EXPORT(size_t)	PL_utf8_strlen(const char *s, size_t len) WUNUSED;


		 /*******************************
		 *	   WIDE INTEGERS	*
		 *******************************/


PL_EXPORT(int)		PL_get_int64(term_t t, int64_t *i) WUNUSED;
PL_EXPORT(int)		PL_get_uint64(term_t t, uint64_t *i) WUNUSED;
PL_EXPORT(int)		PL_unify_int64(term_t t, int64_t value) WUNUSED;
PL_EXPORT(int)		PL_unify_uint64(term_t t, uint64_t value) WUNUSED;
PL_EXPORT(int)		PL_put_int64(term_t t, int64_t i) WUNUSED;
PL_EXPORT(int)		PL_put_uint64(term_t t, uint64_t i) WUNUSED;


		 /*******************************
		 *     ATTRIBUTED VARIABLES	*
		 *******************************/

PL_EXPORT(int)		PL_is_attvar(term_t t);
PL_EXPORT(int)		PL_get_attr(term_t v, term_t a);


		 /*******************************
		 *           TABLING            *
		 *******************************/

PL_EXPORT(int)		PL_get_delay_list(term_t l);


		 /*******************************
		 *	      ERRORS		*
		 *******************************/

PL_EXPORT(int)		PL_get_atom_ex(term_t t, atom_t *a);
PL_EXPORT(int)		PL_get_integer_ex(term_t t, int *i);
PL_EXPORT(int)		PL_get_long_ex(term_t t, long *i);
PL_EXPORT(int)		PL_get_int64_ex(term_t t, int64_t *i);
PL_EXPORT(int)		PL_get_uint64_ex(term_t t, uint64_t *i);
PL_EXPORT(int)		PL_get_intptr_ex(term_t t, intptr_t *i);
PL_EXPORT(int)		PL_get_size_ex(term_t t, size_t *i);
PL_EXPORT(int)		PL_get_bool_ex(term_t t, int *i);
PL_EXPORT(int)		PL_get_float_ex(term_t t, double *f);
PL_EXPORT(int)		PL_get_char_ex(term_t t, int *p, int eof);
PL_EXPORT(int)		PL_unify_bool_ex(term_t t, int val);
PL_EXPORT(int)		PL_get_pointer_ex(term_t t, void **addrp);
PL_EXPORT(int)		PL_unify_list_ex(term_t l, term_t h, term_t t);
PL_EXPORT(int)		PL_unify_nil_ex(term_t l);
PL_EXPORT(int)		PL_get_list_ex(term_t l, term_t h, term_t t);
PL_EXPORT(int)		PL_get_nil_ex(term_t l);

PL_EXPORT(int)		PL_instantiation_error(term_t culprit);
PL_EXPORT(int)		PL_uninstantiation_error(term_t culprit);
PL_EXPORT(int)		PL_representation_error(const char *resource);
PL_EXPORT(int)		PL_type_error(const char *expected, term_t culprit);
PL_EXPORT(int)		PL_domain_error(const char *expected, term_t culprit);
PL_EXPORT(int)		PL_existence_error(const char *type, term_t culprit);
PL_EXPORT(int)		PL_permission_error(const char *operation,
					    const char *type, term_t culprit);
PL_EXPORT(int)		PL_resource_error(const char *resource);
PL_EXPORT(int)		PL_syntax_error(const char *msg, IOSTREAM *in);

		 /*******************************
		 *	       BLOBS		*
		 *******************************/

#define PL_BLOB_MAGIC_B	0x75293a00	/* Magic to validate a blob-type */
#define PL_BLOB_VERSION 1		/* Current version */
#define PL_BLOB_MAGIC	(PL_BLOB_MAGIC_B|PL_BLOB_VERSION)

#define PL_BLOB_UNIQUE	0x01		/* Blob content is unique */
#define PL_BLOB_TEXT	0x02		/* blob contains text */
#define PL_BLOB_NOCOPY	0x04		/* do not copy the data */
#define PL_BLOB_WCHAR	0x08		/* wide character string */

typedef struct PL_blob_t
{ uintptr_t		magic;		/* PL_BLOB_MAGIC */
  uintptr_t		flags;		/* PL_BLOB_* */
  const char *		name;		/* name of the type */
  int			(*release)(atom_t a);
  int			(*compare)(atom_t a, atom_t b);
  int			(*write)(IOSTREAM *s, atom_t a, int flags);
  void			(*acquire)(atom_t a);
  int			(*save)(atom_t a, IOSTREAM *s);
  atom_t		(*load)(IOSTREAM *s);
  size_t		padding;	/* Required 0-padding */
					/* private */
  void *		reserved[9];	/* for future extension */
  int			registered;	/* Already registered? */
  int			rank;		/* Rank for ordering atoms */
  struct PL_blob_t *    next;		/* next in registered type-chain */
  atom_t		atom_name;	/* Name as atom */
} PL_blob_t;

PL_EXPORT(int)		PL_is_blob(term_t t, PL_blob_t **type);
PL_EXPORT(int)		PL_unify_blob(term_t t, void *blob, size_t len,
				      PL_blob_t *type);
PL_EXPORT(atom_t)	PL_new_blob(void *blob, size_t len, PL_blob_t *type);
PL_EXPORT(int)		PL_put_blob(term_t t, void *blob, size_t len,
				    PL_blob_t *type);
PL_EXPORT(int)		PL_get_blob(term_t t, void **blob, size_t *len,
				    PL_blob_t **type);

PL_EXPORT(void*)	PL_blob_data(atom_t a,
				     size_t *len,
				     struct PL_blob_t **type);
PL_EXPORT(int)		PL_free_blob(atom_t blob);

PL_EXPORT(void)		PL_register_blob_type(PL_blob_t *type);
PL_EXPORT(PL_blob_t*)	PL_find_blob_type(const char* name);
PL_EXPORT(int)		PL_unregister_blob_type(PL_blob_t *type);


#ifdef __GNU_MP__

		 /*******************************
		 *	       GMP		*
		 *******************************/

PL_EXPORT(int)	PL_get_mpz(term_t t, mpz_t mpz) WUNUSED;
PL_EXPORT(int)	PL_get_mpq(term_t t,  mpq_t mpq) WUNUSED;
PL_EXPORT(int)	PL_unify_mpz(term_t t, mpz_t mpz) WUNUSED;
PL_EXPORT(int)	PL_unify_mpq(term_t t, mpq_t mpq) WUNUSED;

#endif /*__GNU_MP__*/

		 /*******************************
		 *	  FILENAME SUPPORT	*
		 *******************************/

#define PL_FILE_ABSOLUTE	0x01	/* return absolute path */
#define PL_FILE_OSPATH		0x02	/* return path in OS notation */
#define PL_FILE_SEARCH		0x04	/* use file_search_path */
#define PL_FILE_EXIST		0x08	/* demand file to exist */
#define PL_FILE_READ		0x10	/* demand read-access */
#define PL_FILE_WRITE		0x20	/* demand write-access */
#define PL_FILE_EXECUTE		0x40	/* demand execute-access */
#define PL_FILE_NOERRORS	0x80	/* do not raise exceptions */

PL_EXPORT(int)		PL_get_file_name(term_t n, char **name, int flags);
PL_EXPORT(int)		PL_get_file_nameW(term_t n, wchar_t **name, int flags);
PL_EXPORT(void)		PL_changed_cwd(void); /* foreign code changed CWD */
PL_EXPORT(char *)	PL_cwd(char *buf, size_t buflen);


		 /*******************************
		 *    QUINTUS/SICSTUS WRAPPER	*
		 *******************************/

PL_EXPORT(int)		PL_cvt_i_bool(term_t p, int *c); /* Note "int" because C has no "bool" */
PL_EXPORT(int)		PL_cvt_i_char(term_t p, char *c);
PL_EXPORT(int)		PL_cvt_i_schar(term_t p, signed char *c);
PL_EXPORT(int)		PL_cvt_i_uchar(term_t p, unsigned char *c);
PL_EXPORT(int)		PL_cvt_i_short(term_t p, short *s);
PL_EXPORT(int)		PL_cvt_i_ushort(term_t p, unsigned short *s);
PL_EXPORT(int)		PL_cvt_i_int(term_t p, int *c);
PL_EXPORT(int)		PL_cvt_i_uint(term_t p, unsigned int *c);
PL_EXPORT(int)		PL_cvt_i_long(term_t p, long *c);
PL_EXPORT(int)		PL_cvt_i_ulong(term_t p, unsigned long *c);
PL_EXPORT(int)		PL_cvt_i_llong(term_t p, long long *c);
PL_EXPORT(int)		PL_cvt_i_ullong(term_t p, unsigned long long *c);
PL_EXPORT(int)		PL_cvt_i_int32(term_t p, int32_t *c);
PL_EXPORT(int)		PL_cvt_i_uint32(term_t p, uint32_t *c);
PL_EXPORT(int)		PL_cvt_i_int64(term_t p, int64_t *c);
PL_EXPORT(int)		PL_cvt_i_uint64(term_t p, uint64_t *c);
PL_EXPORT(int)		PL_cvt_i_size_t(term_t p, size_t *c);
PL_EXPORT(int)		PL_cvt_i_float(term_t p, double *c);
PL_EXPORT(int)		PL_cvt_i_single(term_t p, float *c);
PL_EXPORT(int)		PL_cvt_i_string(term_t p, char **c);
PL_EXPORT(int)		PL_cvt_i_codes(term_t p, char **c);
PL_EXPORT(int)		PL_cvt_i_atom(term_t p, atom_t *c);
PL_EXPORT(int)		PL_cvt_i_address(term_t p, void *c);
PL_EXPORT(int)		PL_cvt_o_int64(int64_t c, term_t p);
PL_EXPORT(int)		PL_cvt_o_float(double c, term_t p);
PL_EXPORT(int)		PL_cvt_o_single(float c, term_t p);
PL_EXPORT(int)		PL_cvt_o_string(const char *c, term_t p);
PL_EXPORT(int)		PL_cvt_o_codes(const char *c, term_t p);
PL_EXPORT(int)		PL_cvt_o_atom(atom_t c, term_t p);
PL_EXPORT(int)		PL_cvt_o_address(void *address, term_t p);
PL_EXPORT(term_t)	PL_new_nil_ref(void);

/* set/get encoding for PL_cvt_*_string() functions.  The default
   is UTF-8 (REP_UTF8)
*/

PL_EXPORT(int)		PL_cvt_encoding(void);
PL_EXPORT(int)		PL_cvt_set_encoding(int enc);
PL_EXPORT(void)		SP_set_state(int state);
PL_EXPORT(int)		SP_get_state(void);


		 /*******************************
		 *	     COMPARE		*
		 *******************************/

PL_EXPORT(int)		PL_compare(term_t t1, term_t t2);
PL_EXPORT(int)		PL_same_compound(term_t t1, term_t t2);

		 /*******************************
		 *	     MESSAGES		*
		 *******************************/
#if !defined(WPRINTF12)
/* these macros are duplicated in SWI-Streams.h */
#if defined(CHECK_FORMAT)
#define WPRINTF12  __attribute__ ((format (printf, 1, 2)))
#define WPRINTF23  __attribute__ ((format (printf, 2, 3)))
#define WPRINTF34  __attribute__ ((format (printf, 3, 4)))
#else
#define WPRINTF12
#define WPRINTF23
#define WPRINTF34
#endif
#endif

PL_EXPORT(int)		PL_warning(const char *fmt, ...) WPRINTF12;
PL_EXPORT(int)		PL_warningX(const char *fmt, ...);
PL_EXPORT(void)		PL_fatal_error(const char *fmt, ...) WPRINTF12;
PL_EXPORT(void)		PL_system_error(const char *fmt, ...) WPRINTF12;

		 /*******************************
		 *      RECORDED DATABASE	*
		 *******************************/

PL_EXPORT(record_t)	PL_record(term_t term);
PL_EXPORT(int)		PL_recorded(record_t record, term_t term);
PL_EXPORT(void)		PL_erase(record_t record);
PL_EXPORT(record_t)	PL_duplicate_record(record_t r);

PL_EXPORT(char *)	PL_record_external(term_t t, size_t *size);
PL_EXPORT(int)		PL_recorded_external(const char *rec, term_t term);
PL_EXPORT(int)		PL_erase_external(char *rec);

		 /*******************************
		 *	   PROLOG FLAGS		*
		 *******************************/

#define PL_set_feature  PL_set_prolog_flag /* compatibility */
PL_EXPORT(int)		PL_set_prolog_flag(const char *name, int type, ...);


		 /*******************************
		 *	INTERNAL FUNCTIONS	*
		 *******************************/

PL_EXPORT(PL_atomic_t)	_PL_get_atomic(term_t t);
PL_EXPORT(void)		_PL_put_atomic(term_t t, PL_atomic_t a);
PL_EXPORT(int)		PL_unify_atomic(term_t t, PL_atomic_t a);
PL_EXPORT(int)		_PL_get_arg_sz(size_t index, term_t t, term_t a);
PL_EXPORT(int)		_PL_get_arg(int index, term_t t, term_t a);


		 /*******************************
		 *	    CHAR BUFFERS	*
		 *******************************/

#define CVT_ATOM	    0x00000001
#define CVT_STRING	    0x00000002
#define CVT_LIST	    0x00000004
#define CVT_INTEGER	    0x00000008
#define CVT_RATIONAL	    0x00000010
#define CVT_FLOAT	    0x00000020
#define CVT_VARIABLE	    0x00000040
#define CVT_NUMBER	    (CVT_RATIONAL|CVT_FLOAT)
#define CVT_ATOMIC	    (CVT_NUMBER|CVT_ATOM|CVT_STRING)
#define CVT_WRITE	    0x00000080
#define CVT_WRITE_CANONICAL 0x00000100
#define CVT_WRITEQ	    0x00000200
#define CVT_ALL		    (CVT_ATOMIC|CVT_LIST)
#define CVT_XINTEGER	   (0x00000400|CVT_INTEGER)
#define CVT_MASK	    0x00000fff

#define CVT_EXCEPTION	    0x00001000	/* throw exception on error */
#define CVT_VARNOFAIL	    0x00002000	/* return 2 if argument is unbound */

#define BUF_DISCARDABLE	    0x00000000	/* Store in single thread-local buffer */
#define BUF_STACK	    0x00010000	/* Store in stack of buffers */
#define BUF_MALLOC	    0x00020000	/* Store using PL_malloc() */
#define BUF_ALLOW_STACK	    0x00040000	/* Allow pointer into (global) stack */

#define BUF_RING	    BUF_STACK   /* legacy ring buffer */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Output   representation   for   PL_get_chars()     and    friends.   The
prepresentation type REP_FN is for   PL_get_file_name()  and friends. On
Windows we use UTF-8 which is translated   by the `XOS' layer to Windows
UNICODE file functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define REP_ISO_LATIN_1	    0x00000000	/* output representation */
#define REP_UTF8	    0x00100000
#define REP_MB		    0x00200000
#ifdef __WINDOWS__
#define REP_FN		    REP_UTF8
#else
#define REP_FN		    REP_MB
#endif

#define PL_DIFF_LIST	    0x01000000	/* PL_unify_chars() */


		/*******************************
		*         STRING BUFFERS       *
		*******************************/

#define PL_STRINGS_MARK() \
	{ buf_mark_t __PL_mark; \
	  PL_mark_string_buffers(&__PL_mark);
#define PL_STRINGS_RELEASE() \
	  PL_release_string_buffers_from_mark(__PL_mark); \
	}

PL_EXPORT(void)		PL_mark_string_buffers(buf_mark_t *mark);
PL_EXPORT(void)         PL_release_string_buffers_from_mark(buf_mark_t mark);


		 /*******************************
		 *	  STREAM SUPPORT	*
		 *******************************/

					/* Make IOSTREAM known to Prolog */
#define PL_open_stream  PL_unify_stream	/* compatibility */
PL_EXPORT(int)		PL_unify_stream(term_t t, IOSTREAM *s);
PL_EXPORT(int)		PL_get_stream_handle(term_t t, IOSTREAM **s);
PL_EXPORT(int)		PL_get_stream(term_t t, IOSTREAM **s, int flags);
PL_EXPORT(int)		PL_get_stream_from_blob(atom_t a, IOSTREAM**s, int flags);
PL_EXPORT(IOSTREAM*)	PL_acquire_stream(IOSTREAM *s);
PL_EXPORT(int)		PL_release_stream(IOSTREAM *s);
PL_EXPORT(int)		PL_release_stream_noerror(IOSTREAM *s);
PL_EXPORT(IOSTREAM *)	PL_open_resource(module_t m,
					 const char *name,
					 const char *rc_class,
					 const char *mode);

PL_EXPORT(IOSTREAM *)*_PL_streams(void);	/* base of streams */
#ifndef PL_KERNEL
#define Suser_input     (_PL_streams()[0])
#define Suser_output    (_PL_streams()[1])
#define Suser_error     (_PL_streams()[2])
#define Scurrent_input  (_PL_streams()[3])
#define Scurrent_output (_PL_streams()[4])
#endif

#define PL_WRT_QUOTED		       0x01 /* quote atoms */
#define PL_WRT_IGNOREOPS	       0x02 /* ignore list/operators */
#define PL_WRT_NUMBERVARS	       0x04 /* print $VAR(N) as a variable */
#define PL_WRT_PORTRAY		       0x08 /* call portray */
#define PL_WRT_CHARESCAPES	       0x10 /* Output ISO escape sequences */
#define PL_WRT_BACKQUOTED_STRING       0x20 /* Write strings as `...` */
					    /* Write attributed variables */
#define PL_WRT_ATTVAR_IGNORE	      0x040 /* Default: just write the var */
#define PL_WRT_ATTVAR_DOTS	      0x080 /* Write as Var{...} */
#define PL_WRT_ATTVAR_WRITE	      0x100 /* Write as Var{Attributes} */
#define PL_WRT_ATTVAR_PORTRAY	      0x200 /* Use Module:portray_attrs/2 */
#define PL_WRT_ATTVAR_MASK \
	(PL_WRT_ATTVAR_IGNORE | \
	 PL_WRT_ATTVAR_DOTS | \
	 PL_WRT_ATTVAR_WRITE | \
	 PL_WRT_ATTVAR_PORTRAY)
#define PL_WRT_BLOB_PORTRAY	      0x400 /* Use portray for non-text blobs */
#define PL_WRT_NO_CYCLES	      0x800 /* Never emit @(Template,Subst) */
#define PL_WRT_NEWLINE		     0x2000 /* Add a newline */
#define PL_WRT_VARNAMES		     0x4000 /* Internal: variable_names(List) */
#define PL_WRT_BACKQUOTE_IS_SYMBOL   0x8000 /* ` is a symbol char */
#define PL_WRT_DOTLISTS		    0x10000 /* Write lists as .(A,B) */
#define PL_WRT_BRACETERMS           0x20000 /* Write {A} as {}(A) */
#define PL_WRT_NODICT		    0x40000 /* Do not write dicts pretty */
#define PL_WRT_NODOTINATOM          0x80000 /* never write a.b unquoted */
#define PL_WRT_NO_LISTS		   0x100000 /* Do not write lists as [...] */
#define PL_WRT_RAT_NATURAL         0x200000 /* Write rationals as 1/3 */
#define PL_WRT_CHARESCAPES_UNICODE 0x400000 /* Use \uXXXX escapes */
#define PL_WRT_QUOTE_NON_ASCII	   0x800000 /* Quote atoms containing non-ascii */
#define PL_WRT_PARTIAL		  0x1000000 /* Partial output */
#define PL_WRT_NO_CHARESCAPES	  0x2000000 /* Do not Output ISO escapes */

PL_EXPORT(int)	PL_write_term(IOSTREAM *s,
			     term_t term,
			     int precedence,
			     int flags);

					/* PL_ttymode() results */
#define PL_NOTTY	0		/* -tty in effect */
#define PL_RAWTTY	1		/* get_single_char/1 */
#define PL_COOKEDTTY	2		/* normal input */

PL_EXPORT(int)		PL_ttymode(IOSTREAM *s);

PL_EXPORT(int)  PL_put_term_from_chars(term_t t, int flags,
				       size_t len, const char *s);
PL_EXPORT(int)	PL_chars_to_term(const char *chars,
				term_t term);
PL_EXPORT(int)	PL_wchars_to_term(const pl_wchar_t *chars,
				 term_t term);


		 /*******************************
		 *	    EMBEDDING		*
		 *******************************/

#define PL_CLEANUP_STATUS_MASK		(0x0ffff)
#define PL_CLEANUP_NO_RECLAIM_MEMORY	(0x10000)
#define PL_CLEANUP_NO_CANCEL		(0x20000)

#define PL_CLEANUP_CANCELED	0
#define PL_CLEANUP_SUCCESS	1
#define PL_CLEANUP_FAILED      -1
#define PL_CLEANUP_RECURSIVE   -2

PL_EXPORT(int)		PL_initialise(int argc, char **argv);
PL_EXPORT(int)		PL_winitialise(int argc, wchar_t **argv);
PL_EXPORT(int)		PL_is_initialised(int *argc, char ***argv);
PL_EXPORT(int)		PL_set_resource_db_mem(const unsigned char *data,
					       size_t size);
PL_EXPORT(int)		PL_toplevel(void);
PL_EXPORT(int)		PL_cleanup(int status);
PL_EXPORT(void)		PL_cleanup_fork(void);
PL_EXPORT(int)		PL_halt(int status);

		 /*******************************
		 *	  DYNAMIC LINKING	*
		 *******************************/

PL_EXPORT(void *)	PL_dlopen(const char *file, int flags);
PL_EXPORT(const char *) PL_dlerror(void);
PL_EXPORT(void *)	PL_dlsym(void *handle, char *symbol);
PL_EXPORT(int)		PL_dlclose(void *handle);


		 /*******************************
		 *      INPUT/PROMPT/ETC	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE: the functions in this section are   not  documented, as as yet not
adviced for public usage.  They  are   intended  to  provide an abstract
interface for the GNU readline  interface   as  defined  in the readline
package.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
					/* PL_dispatch() modes */
#define PL_DISPATCH_NOWAIT    0		/* Dispatch only once */
#define PL_DISPATCH_WAIT      1		/* Dispatch till input available */
#define PL_DISPATCH_INSTALLED 2		/* dispatch function installed? */

PL_EXPORT(int)		PL_dispatch(int fd, int wait);
PL_EXPORT(void)		PL_add_to_protocol(const char *buf, size_t count);
PL_EXPORT(char *)	PL_prompt_string(int fd);
PL_EXPORT(void)		PL_write_prompt(int dowrite);
PL_EXPORT(void)		PL_prompt_next(int fd);
PL_EXPORT(char *)	PL_atom_generator(const char *prefix, int state);
PL_EXPORT(pl_wchar_t*)	PL_atom_generator_w(const pl_wchar_t *pref,
					    pl_wchar_t *buffer,
					    size_t buflen,
					    int state);


		 /*******************************
		 *	MEMORY ALLOCATION	*
		 *******************************/

PL_EXPORT(void *)	PL_malloc(size_t size);
PL_EXPORT(void *)	PL_malloc_atomic(size_t size);
PL_EXPORT(void *)	PL_malloc_uncollectable(size_t size);
PL_EXPORT(void *)	PL_malloc_atomic_uncollectable(size_t size);
PL_EXPORT(void *)	PL_realloc(void *mem, size_t size);
PL_EXPORT(void *)	PL_malloc_unmanaged(size_t size);
PL_EXPORT(void *)	PL_malloc_atomic_unmanaged(size_t size);
PL_EXPORT(void)		PL_free(void *mem);
PL_EXPORT(int)		PL_linger(void *mem);


		/********************************
		*             HOOKS		*
		********************************/

#define PL_DISPATCH_INPUT   0		/* There is input available */
#define PL_DISPATCH_TIMEOUT 1		/* Dispatch timeout */

typedef int  (*PL_dispatch_hook_t)(int fd);
typedef void (*PL_abort_hook_t)(void);
typedef void (*PL_initialise_hook_t)(int argc, char **argv);
typedef int  (*PL_agc_hook_t)(atom_t a);

PL_EXPORT(PL_dispatch_hook_t)	PL_dispatch_hook(PL_dispatch_hook_t);
PL_EXPORT(void)			PL_abort_hook(PL_abort_hook_t);
PL_EXPORT(void)			PL_initialise_hook(PL_initialise_hook_t);
PL_EXPORT(int)			PL_abort_unhook(PL_abort_hook_t);
PL_EXPORT(PL_agc_hook_t)	PL_agc_hook(PL_agc_hook_t);


		 /*******************************
		 *	      OPTIONS		*
		 *******************************/

typedef enum
{ _OPT_END = -1,
  OPT_BOOL = 0,				/* int */
  OPT_INT,				/* int */
  OPT_INT64,				/* int64_t */
  OPT_UINT64,				/* uint64_t */
  OPT_SIZE,				/* size_t */
  OPT_DOUBLE,				/* double */
  OPT_STRING,				/* char* (UTF-8) */
  OPT_ATOM,				/* atom_t */
  OPT_TERM,				/* term_t */
  OPT_LOCALE				/* void* */
} _PL_opt_enum_t;

#define OPT_TYPE_MASK	0xff
#define OPT_INF		0x100		/* allow 'inf' */

#define OPT_ALL		0x1		/* flags */

typedef struct
{ atom_t		name;		/* Name of option */
  _PL_opt_enum_t	type;		/* Type of option */
  const char *		string;		/* For foreign access */
} PL_option_t;

#define PL_OPTION(name, type) { 0, type, name }
#define PL_OPTIONS_END	      { 0, _OPT_END, (const char*)0 }

PL_EXPORT(int)	PL_scan_options(term_t options, int flags, const char *opttype,
				PL_option_t specs[], ...);


		/********************************
		*            SIGNALS            *
		*********************************/

/* PL_signal() masks (deprecated) */
#define PL_SIGSYNC	0x00010000	/* call handler synchronously */
#define PL_SIGNOFRAME	0x00020000	/* Do not create a Prolog frame */

#define PLSIG_THROW     0x0002		/* throw signal(num, name) */
#define PLSIG_SYNC      0x0004		/* call synchronously */
#define PLSIG_NOFRAME   0x0008		/* Do not create a Prolog frame */
#define PLSIG_IGNORE    0x0010		/* ignore signal entirely */




typedef struct pl_sigaction
{ void        (*sa_cfunction)(int);	/* traditional C function */
  predicate_t sa_predicate;		/* call a predicate */
  int	      sa_flags;			/* additional flags */
  void       *reserved[2];		/* future extentions */
} pl_sigaction_t;


PL_EXPORT(void) (*PL_signal(int sig, void (*func)(int)))(int); /* WDEPRECATED */
PL_EXPORT(int)  PL_sigaction(int sig, pl_sigaction_t *act, pl_sigaction_t *old);
PL_EXPORT(void)	PL_interrupt(int sig);
PL_EXPORT(int)	PL_raise(int sig);
PL_EXPORT(int)	PL_handle_signals(void);
PL_EXPORT(int)	PL_get_signum_ex(term_t sig, int *n);


		/********************************
		*      PROLOG ACTION/QUERY      *
		*********************************/

#define	PL_ACTION_TRACE		1	/* switch to trace mode */
#define PL_ACTION_DEBUG		2	/* switch to debug mode */
#define PL_ACTION_BACKTRACE	3	/* show a backtrace (stack dump) */
#define PL_ACTION_BREAK		4	/* create a break environment */
#define PL_ACTION_HALT		5	/* halt Prolog execution */
#define PL_ACTION_ABORT		6	/* generate a Prolog abort */
					/* 7: Obsolete PL_ACTION_SYMBOLFILE */
#define PL_ACTION_WRITE		8	/* write via Prolog i/o buffer */
#define PL_ACTION_FLUSH		9	/* Flush Prolog i/o buffer */
#define PL_ACTION_GUIAPP	10	/* Win32: set when this is a gui */
#define PL_ACTION_ATTACH_CONSOLE 11	/* MT: Attach a console */
#define PL_GMP_SET_ALLOC_FUNCTIONS 12	/* GMP: do not change allocation functions */
#define PL_ACTION_TRADITIONAL	13	/* Set --traditional */

#define PL_BT_SAFE		0x1	/* Do not try to print goals */
#define PL_BT_USER		0x2	/* Only show user-goals */

PL_EXPORT(int)	PL_action(int, ...);	/* perform some action */
PL_EXPORT(void)	PL_on_halt(int (*)(int, void *), void *);
PL_EXPORT(void)	PL_exit_hook(int (*)(int, void *), void *);
PL_EXPORT(void)	PL_backtrace(int depth, int flags);
PL_EXPORT(char *) PL_backtrace_string(int depth, int flags);
PL_EXPORT(int)	PL_check_data(term_t data);
PL_EXPORT(int)	PL_check_stacks(void);
PL_EXPORT(int)	PL_current_prolog_flag(atom_t name, int type, void *ptr);


		 /*******************************
		 *	      VERSIONS		*
		 *******************************/

#define PL_VERSION_SYSTEM	1	/* Prolog version */
#define PL_VERSION_FLI		2	/* PL_* compatibility */
#define PL_VERSION_REC		3	/* PL_record_external() compatibility */
#define PL_VERSION_QLF		4	/* Saved QLF format version */
#define PL_VERSION_QLF_LOAD	5	/* Min loadable QLF format version */
#define PL_VERSION_VM		6	/* VM signature */
#define PL_VERSION_BUILT_IN	7	/* Built-in predicate signature */

#define PL_version(id) PL_version_info(id)
PL_EXPORT(unsigned int) PL_version_info(int which);


		/********************************
		*         QUERY PROLOG          *
		*********************************/

#define PL_QUERY_ARGC		1	/* return main() argc */
#define PL_QUERY_ARGV		2	/* return main() argv */
					/* 3: Obsolete PL_QUERY_SYMBOLFILE */
					/* 4: Obsolete PL_QUERY_ORGSYMBOLFILE*/
#define PL_QUERY_GETC		5	/* Read character from terminal */
#define PL_QUERY_MAX_INTEGER	6	/* largest integer */
#define PL_QUERY_MIN_INTEGER	7	/* smallest integer */
#define PL_QUERY_MAX_TAGGED_INT	8	/* largest tagged integer */
#define PL_QUERY_MIN_TAGGED_INT	9	/* smallest tagged integer */
#define PL_QUERY_VERSION        10	/* 207006 = 2.7.6 */
#define PL_QUERY_MAX_THREADS	11	/* maximum thread count */
#define PL_QUERY_ENCODING	12	/* I/O encoding */
#define PL_QUERY_USER_CPU	13	/* User CPU in milliseconds */
#define PL_QUERY_HALTING	14	/* If TRUE, we are in PL_cleanup() */

PL_EXPORT(intptr_t)	PL_query(int);	/* get information from Prolog */


		 /*******************************
		 *	  PROLOG THREADS	*
		 *******************************/

#define PL_THREAD_NO_DEBUG	0x01	/* Start thread in nodebug mode */
#define PL_THREAD_NOT_DETACHED	0x02	/* Allow Prolog to join */
#define PL_THREAD_CUR_STREAMS   0x04	/* Copy current{input,output} */

typedef enum
{ PL_THREAD_CANCEL_FAILED = FALSE,	/* failed to cancel; try abort */
  PL_THREAD_CANCEL_JOINED = TRUE,	/* cancelled and joined */
  PL_THREAD_CANCEL_MUST_JOIN		/* cancelled, must join */
} rc_cancel;

typedef struct
{ size_t    stack_limit;		/* Total stack limit (bytes) */
  size_t    table_space;		/* Total tabling space limit (bytes) */
  char *    alias;			/* alias name */
  rc_cancel (*cancel)(int id);		/* cancel function */
  intptr_t  flags;			/* PL_THREAD_* flags */
  size_t    max_queue_size;		/* Max size of associated queue */
  void *    reserved[3];		/* reserved for extensions */
} PL_thread_attr_t;


PL_EXPORT(int)	PL_thread_self(void);	/* Prolog thread id (-1 if none) */
PL_EXPORT(int)	PL_unify_thread_id(term_t t, int i);
PL_EXPORT(int)	PL_get_thread_id_ex(term_t t, int *idp);
PL_EXPORT(int)	PL_get_thread_alias(int tid, atom_t *alias);	/* Locks alias */
PL_EXPORT(int)	PL_thread_attach_engine(PL_thread_attr_t *attr);
PL_EXPORT(int)	PL_thread_destroy_engine(void);
PL_EXPORT(int)	PL_thread_at_exit(void (*function)(void *),
				  void *closure,
				  int global);
PL_EXPORT(int)	PL_thread_raise(int tid, int sig);
#if defined(_WINDOWS_) || defined(_WINDOWS_H)	/* <windows.h> is included */
PL_EXPORT(int)	PL_w32thread_raise(DWORD dwTid, int sig);
PL_EXPORT(int)	PL_wait_for_console_input(void *handle);
PL_EXPORT(int)	PL_w32_wrap_ansi_console(void);
PL_EXPORT(const char*) PL_w32_running_under_wine(void);
#endif

		 /*******************************
		 *	 ENGINES (MT-ONLY)	*
		 *******************************/

#define PL_ENGINE_MAIN	  ((PL_engine_t)0x1)
#define PL_ENGINE_CURRENT ((PL_engine_t)0x2)

#define PL_ENGINE_SET   0		/* engine set successfully */
#define PL_ENGINE_INVAL	2		/* engine doesn't exist */
#define PL_ENGINE_INUSE	3		/* engine is in use */

PL_EXPORT(PL_engine_t)	PL_create_engine(PL_thread_attr_t *attributes);
PL_EXPORT(int)		PL_set_engine(PL_engine_t engine, PL_engine_t *old);
PL_EXPORT(int)		PL_destroy_engine(PL_engine_t engine);


		 /*******************************
		 *	    HASH TABLES		*
		 *******************************/
typedef _PLS(table)	 *hash_table_t;
typedef _PLS(table_enum) *hash_table_enum_t;
typedef uint64_t table_key_t;
typedef uint64_t table_value_t;

#define PL_HT_NEW	0x0001
#define PL_HT_UPDATE	0x0002

PL_EXPORT(hash_table_t)	PL_new_hash_table(size_t size,
					  void (*free_symbol)(table_key_t n, table_value_t v));
PL_EXPORT(int)		PL_free_hash_table(hash_table_t table);
PL_EXPORT(table_value_t) PL_lookup_hash_table(hash_table_t table, table_key_t key);
PL_EXPORT(table_value_t) PL_add_hash_table(hash_table_t table,
					   table_key_t key, table_value_t value, int flags);
PL_EXPORT(table_value_t) PL_del_hash_table(hash_table_t table, table_key_t key);
PL_EXPORT(int)		PL_clear_hash_table(hash_table_t table);
PL_EXPORT(hash_table_enum_t) PL_new_hash_table_enum(hash_table_t table);
PL_EXPORT(void)		PL_free_hash_table_enum(hash_table_enum_t e);
PL_EXPORT(int)		PL_advance_hash_table_enum(hash_table_enum_t e,
						   table_key_t *key, table_value_t *value);


		 /*******************************
		 *	     PROFILER		*
		 *******************************/

typedef struct
{ int	(*unify)(term_t t, void *handle);	/* implementation --> Prolog */
  int   (*get)(term_t t, void **handle);	/* Prolog --> implementation */
  void	(*activate)(int active);		/* (de)activate */
  void  (*release)(void *handle);		/* Release handle */
  void *dummy[4];				/* reserved */
  intptr_t	magic;				/* PROFTYPE_MAGIC */
} PL_prof_type_t;

PL_EXPORT(int)		PL_register_profile_type(PL_prof_type_t *type);
PL_EXPORT(void*)	PL_prof_call(void *handle, PL_prof_type_t *type);
PL_EXPORT(void)		PL_prof_exit(void *node);


		 /*******************************
		 *	      DEBUG		*
		 *******************************/

PL_EXPORT(int)		PL_prolog_debug(const char *topic);
PL_EXPORT(int)		PL_prolog_nodebug(const char *topic);


		 /*******************************
		 *	 WINDOWS MESSAGES	*
		 *******************************/

#if defined(_WINDOWS_) || defined(_WINDOWS_H)	/* <windows.h> is included */
#define PL_MSG_EXCEPTION_RAISED -1
#define PL_MSG_IGNORED 0
#define PL_MSG_HANDLED 1

PL_EXPORT(LRESULT)	PL_win_message_proc(HWND hwnd,
					    UINT message,
					    WPARAM wParam,
					    LPARAM lParam);
#endif /* _WINDOWS_/_WINDOWS_H */


		 /*******************************
		 *       FAST XPCE SUPPORT	*
		 *******************************/

typedef struct
{ int type;				/* PL_INTEGER or PL_ATOM */
  union
  { uintptr_t i;			/* integer reference value */
    atom_t	  a;			/* atom reference value */
  } value;
} xpceref_t;

PL_EXPORT(int)	_PL_get_xpce_reference(term_t t, xpceref_t *ref);
PL_EXPORT(int)	_PL_unify_xpce_reference(term_t t, xpceref_t *ref);
PL_EXPORT(int)	_PL_put_xpce_reference_i(term_t t, uintptr_t r);
PL_EXPORT(int)	_PL_put_xpce_reference_a(term_t t, atom_t name);



		 /*******************************
		 *         TRACE SUPPORT	*
		 *******************************/

typedef struct pl_context_t
{ PL_engine_t   ld;			/* Engine */
  _PLS(queryFrame) *qf;			/* Current query */
  _PLS(localFrame) *fr;			/* Current localframe */
  _PLQ(code) *	pc;			/* Code pointer */
  void *	reserved[10];		/* Reserved for extensions */
} pl_context_t;

PL_EXPORT(int)	PL_get_context(struct pl_context_t *c, int thead_id);
PL_EXPORT(int)	PL_step_context(struct pl_context_t *c);
PL_EXPORT(int)	PL_describe_context(struct pl_context_t *c,
				    char *buf, size_t len);

/* Define as 1 if undefined or defined as empty */
#if !defined(PL_ARITY_AS_SIZE) || (0-PL_ARITY_AS_SIZE-1)==1
#undef PL_ARITY_AS_SIZE
#define PL_ARITY_AS_SIZE 1
#endif

#if PL_ARITY_AS_SIZE
#define PL_new_functor(f,a) PL_new_functor_sz(f,a)
#define PL_functor_arity(f) PL_functor_arity_sz(f)
#define PL_get_name_arity(t,n,a) PL_get_name_arity_sz(t,n,a)
#define PL_get_compound_name_arity(t,n,a) PL_get_compound_name_arity_sz(t,n,a)
#define PL_get_arg(i,t,a) PL_get_arg_sz(i,t,a)
#define PL_unify_arg(i,t,a) PL_unify_arg_sz(i,t,a)
#define _PL_get_arg(i,t,a) _PL_get_arg_sz(i,t,a)
#else
#warning "Term arity has changed from int to size_t."
#warning "Please update your code or use #define PL_ARITY_AS_SIZE 0."
#endif

#ifdef __cplusplus
}
#endif

/* Undefine the shorthands used in this file along with any of PL_OPAQUE
 * or PL_STRUCT that were not provided externally.
 */
#undef _PLQ
#undef _PLS

#ifdef _DEFINED_PL_OPAQUE
#undef _DEFINED_PL_OPAQUE
#undef PL_OPAQUE
#endif

#ifdef _DEFINED_PL_STRUCT
#undef _DEFINED_PL_STRUCT
#undef PL_STRUCT
#endif

#endif /*_SWI_PROLOG_H*/
