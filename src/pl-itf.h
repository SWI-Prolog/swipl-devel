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

#ifndef _FLI_H_INCLUDED
#define _FLI_H_INCLUDED

#include <stdarg.h>
#include <stdlib.h>			/* get size_t */

#ifdef __cplusplus
extern "C" {
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This interface has been changed considerable  between version 2.1.14 and
2.5.0. You may get most old code to work using

	#define PL_OLD_INTERFACE

before loading this file.  See end of this file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* PLVERSION: 10000 * <Major> + 100 * <Minor> + <Patch> */

#ifndef PLVERSION
#define PLVERSION 50103
#endif

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

Both using native Microsoft MSVC as well   as recent CygWin (tested 1.1)
compilers support __declspec(...) for exporting symbols.

As SWI-Prolog.h can be included seperately or together with this file we
duplicated this stuff.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef _PL_EXPORT_DONE
#define _PL_EXPORT_DONE
#if defined(WIN32) && !defined(__WIN32__)
#define __WIN32__
#endif

#if (defined(__WIN32__) || defined(__CYGWIN32__)) && !defined(__LCC__)
#define HAVE_DECLSPEC
#endif

#ifdef HAVE_DECLSPEC
# ifdef PL_KERNEL
#define PL_EXPORT(type)		__declspec(dllexport) type
#define PL_EXPORT_DATA(type)	__declspec(dllexport) type
#define install_t	 	void
# else
#  ifdef __BORLANDC__
#define PL_EXPORT(type)	 	type _stdcall
#define PL_EXPORT_DATA(type)	extern type
#  else
#define PL_EXPORT(type)	 	extern type
#define PL_EXPORT_DATA(type)	__declspec(dllimport) type
#  endif
#define install_t	 	__declspec(dllexport) void
# endif
#else /*HAVE_DECLSPEC*/
#define PL_EXPORT(type)	 	extern type
#define PL_EXPORT_DATA(type)	extern type
#define install_t	 	void
#endif /*HAVE_DECLSPEC*/
#endif /*_PL_EXPORT_DONE*/


		 /*******************************
		 *	       TYPES		*
		 *******************************/

#ifdef _PL_INCLUDE_H
typedef Module		module_t;	/* a module */
typedef Procedure	predicate_t;	/* a predicate handle */
typedef Record		record_t;	/* handle to a recorded term */
#else
typedef	unsigned long	atom_t;		/* Prolog atom */
typedef void *		module_t;	/* Prolog module */
typedef void *		predicate_t;	/* Prolog procedure */
typedef void *		record_t;	/* Prolog recorded term */
typedef unsigned long	term_t;		/* opaque term handle */
typedef unsigned long	qid_t;		/* opaque query handle */
typedef unsigned long	PL_fid_t;	/* opaque foreign context handle */
typedef void *		control_t;	/* non-deterministic control arg */
#endif
typedef unsigned long	functor_t;	/* Name/arity pair */
typedef unsigned long	PL_atomic_t;	/* same a word */
typedef unsigned long	foreign_t;	/* return type of foreign functions */
#ifdef __cplusplus
typedef void * 		pl_function_t;	/* can only pass function as void * */
#else
typedef foreign_t	(*pl_function_t)(); /* foreign language functions */
#endif

#define fid_t PL_fid_t			/* avoid AIX name-clash */

					/* values for PL_get_term_value() */
typedef union
{ long   i;				/* PL_INTEGER */
  double f;				/* PL_FLOAT */
  char * s;				/* PL_STRING */
  atom_t a;				/* PL_ATOM */
  struct				/* PL_TERM */
  { atom_t name;
    int    arity;
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
#define PL_FLOAT	 (4)		/* double */
#define PL_STRING	 (5)		/* const char * */
#define PL_TERM		 (6)

					/* PL_unify_term() */
#define PL_FUNCTOR	 (10)		/* functor_t, arg ... */
#define PL_LIST		 (11)		/* length, arg ... */
#define PL_CHARS	 (12)		/* const char * */
#define PL_POINTER	 (13)		/* void * */
					/* PlArg::PlArg(text, type) */
#define PL_CODE_LIST	 (14)		/* [ascii...] */
#define PL_CHAR_LIST	 (15)		/* [h,e,l,l,o] */
#define PL_BOOL		 (16)		/* PL_set_feature() */
#define PL_FUNCTOR_CHARS (17)		/* PL_unify_term() */
#define _PL_PREDICATE_INDICATOR (18)	/* predicate_t (Procedure) */
#define PL_SHORT	 (19)		/* short */
#define PL_INT		 (20)		/* int */
#define PL_LONG		 (21)		/* long */
#define PL_DOUBLE	 (22)		/* double */
#define PL_NCHARS	 (23)		/* unsigned, const char * */


		/********************************
		*    DETERMINISTIC CALL/RETURN  *
		*********************************/

#define	PL_succeed	return TRUE	/* succeed deterministically */
#define PL_fail		return FALSE	/* fail */


		/********************************
		* NON-DETERMINISTIC CALL/RETURN *
		*********************************/

/*  Note 1: Non-deterministic foreign functions may also use the deterministic
    return methods PL_succeed and PL_fail.

    Note 2: The argument to PL_retry is a 30 bits signed integer (long).
*/

#define PL_FIRST_CALL		(0)
#define PL_CUTTED		(1)
#define PL_REDO			(2)

#define PL_retry(n)		return _PL_retry(n)
#define PL_retry_address(a)	return _PL_retry_address(a)

PL_EXPORT(foreign_t)	_PL_retry(long);
PL_EXPORT(foreign_t)	_PL_retry_address(void *);
PL_EXPORT(int)	 	PL_foreign_control(control_t);
PL_EXPORT(long)	 	PL_foreign_context(control_t);
PL_EXPORT(void *)	PL_foreign_context_address(control_t);


		/********************************
		*      REGISTERING FOREIGNS     *
		*********************************/

typedef struct _PL_extension
{ char 		*predicate_name;	/* Name of the predicate */
  short		arity;			/* Arity of the predicate */
  pl_function_t	function;		/* Implementing functions */
  short		flags;			/* Or of PL_FA_... */
} PL_extension;

#define PL_FA_NOTRACE		(0x01)	/* foreign cannot be traced */
#define PL_FA_TRANSPARENT	(0x02)	/* foreign is module transparent */
#define PL_FA_NONDETERMINISTIC	(0x04)	/* foreign is non-deterministic */
#define PL_FA_VARARGS		(0x08)	/* call using t0, ac, ctx */

extern			PL_extension PL_extensions[]; /* not Win32! */
PL_EXPORT(void)		PL_register_extensions(const PL_extension *e);
PL_EXPORT(void)		PL_load_extensions(const PL_extension *e);
PL_EXPORT(int)		PL_register_foreign(const char *name, int arity,
					    pl_function_t func, int flags);


		 /*******************************
		 *	      LICENSE		*
		 *******************************/

void			PL_license(const char *license, const char *module);

		/********************************
		*            MODULES            *
		*********************************/

PL_EXPORT(module_t)	PL_context(void);
PL_EXPORT(atom_t)	PL_module_name(module_t module);
PL_EXPORT(module_t)	PL_new_module(atom_t name);
PL_EXPORT(int)		PL_strip_module(term_t in, module_t *m, term_t out);


		 /*******************************
		 *	     CALL-BACK		*
		 *******************************/

#ifdef PL_KERNEL
#define PL_Q_DEBUG		0x01	/* = TRUE for backward compatibility */
#endif
#define PL_Q_NORMAL		0x02	/* normal usage */
#define PL_Q_NODEBUG		0x04	/* use this one */
#define PL_Q_CATCH_EXCEPTION	0x08	/* handle exceptions in C */
#define PL_Q_PASS_EXCEPTION	0x10	/* pass to parent environment */
#ifdef PL_KERNEL
#define PL_Q_DETERMINISTIC	0x20	/* call was deterministic */
#endif

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
					  atom_t *name, int *arity,
					  module_t *module);

			/* Call-back */
PL_EXPORT(qid_t)	PL_open_query(module_t m, int flags,
				      predicate_t pred, term_t t0);
PL_EXPORT(int)		PL_next_solution(qid_t qid);
PL_EXPORT(void)		PL_close_query(qid_t qid);
PL_EXPORT(void)		PL_cut_query(qid_t qid);

			/* Simplified (but less flexible) call-back */
PL_EXPORT(int)		PL_call(term_t t, module_t m);
PL_EXPORT(int)		PL_call_predicate(module_t m, int debug,
					  predicate_t pred, term_t t0);
			/* Handling exceptions */
PL_EXPORT(term_t)	PL_exception(qid_t qid);
PL_EXPORT(int)		PL_raise_exception(term_t exception);
PL_EXPORT(int)		PL_throw(term_t exception);


		 /*******************************
		 *        TERM-REFERENCES	*
		 *******************************/

			/* Creating and destroying term-refs */
PL_EXPORT(term_t)	PL_new_term_refs(int n);
PL_EXPORT(term_t)	PL_new_term_ref(void);
PL_EXPORT(term_t)	PL_copy_term_ref(term_t from);
PL_EXPORT(void)		PL_reset_term_refs(term_t r);

			/* Constants */
PL_EXPORT(atom_t)	PL_new_atom(const char *s);
PL_EXPORT(atom_t)	PL_new_atom_nchars(unsigned int len, const char *s);
PL_EXPORT(const char *)	PL_atom_chars(atom_t a);
PL_EXPORT(const char *) PL_atom_nchars(atom_t a, unsigned int *len);
#ifndef O_DEBUG_ATOMGC
PL_EXPORT(void)		PL_register_atom(atom_t a);
PL_EXPORT(void)		PL_unregister_atom(atom_t a);
#endif
PL_EXPORT(functor_t)	PL_new_functor(atom_t f, int a);
PL_EXPORT(atom_t)	PL_functor_name(functor_t f);
PL_EXPORT(int)		PL_functor_arity(functor_t f);

			/* Get C-values from Prolog terms */
PL_EXPORT(int)		PL_get_atom(term_t t, atom_t *a);
PL_EXPORT(int)		PL_get_bool(term_t t, int *value);
PL_EXPORT(int)		PL_get_atom_chars(term_t t, char **a);
#define PL_get_string_chars(t, s, l) PL_get_string(t,s,l)
					/* PL_get_string() is depreciated */
PL_EXPORT(int)		PL_get_string(term_t t, char **s, unsigned int *len);
PL_EXPORT(int)		PL_get_chars(term_t t, char **s, unsigned int flags);
PL_EXPORT(int)		PL_get_list_chars(term_t l, char **s,
					  unsigned int flags);
PL_EXPORT(int)		PL_get_atom_nchars(term_t t,
					   unsigned int *length, char **a);
PL_EXPORT(int)		PL_get_list_nchars(term_t l,
					   unsigned int *length, char **s,
					   unsigned int flags);
PL_EXPORT(int)		PL_get_nchars(term_t t,
				      unsigned int *length, char **s,
				      unsigned int flags);
PL_EXPORT(int)		PL_get_integer(term_t t, int *i);
PL_EXPORT(int)		PL_get_long(term_t t, long *i);
PL_EXPORT(int)		PL_get_pointer(term_t t, void **ptr);
PL_EXPORT(int)		PL_get_float(term_t t, double *f);
PL_EXPORT(int)		PL_get_functor(term_t t, functor_t *f);
PL_EXPORT(int)		PL_get_name_arity(term_t t, atom_t *name, int *arity);
PL_EXPORT(int)		PL_get_module(term_t t, module_t *module);
PL_EXPORT(int)		PL_get_arg(int index, term_t t, term_t a);
PL_EXPORT(int)		PL_get_list(term_t l, term_t h, term_t t);
PL_EXPORT(int)		PL_get_head(term_t l, term_t h);
PL_EXPORT(int)		PL_get_tail(term_t l, term_t t);
PL_EXPORT(int)		PL_get_nil(term_t l);
PL_EXPORT(int)		PL_get_term_value(term_t t, term_value_t *v);
PL_EXPORT(char *)	PL_quote(int chr, const char *data);

			/* Verify types */
PL_EXPORT(int)		PL_term_type(term_t t);
PL_EXPORT(int)		PL_is_variable(term_t t);
PL_EXPORT(int)		PL_is_atom(term_t t);
PL_EXPORT(int)		PL_is_integer(term_t t);
PL_EXPORT(int)		PL_is_string(term_t t);
PL_EXPORT(int)		PL_is_float(term_t t);
PL_EXPORT(int)		PL_is_compound(term_t t);
PL_EXPORT(int)		PL_is_functor(term_t t, functor_t f);
PL_EXPORT(int)		PL_is_list(term_t t);
PL_EXPORT(int)		PL_is_atomic(term_t t);
PL_EXPORT(int)		PL_is_number(term_t t);

			/* Assign to term-references */
PL_EXPORT(void)		PL_put_variable(term_t t);
PL_EXPORT(void)		PL_put_atom(term_t t, atom_t a);
PL_EXPORT(void)		PL_put_atom_chars(term_t t, const char *chars);
PL_EXPORT(void)		PL_put_string_chars(term_t t, const char *chars);
PL_EXPORT(void)		PL_put_list_chars(term_t t, const char *chars);
PL_EXPORT(void)		PL_put_list_codes(term_t t, const char *chars);
PL_EXPORT(void)		PL_put_atom_nchars(term_t t,
					   unsigned int l, const char *chars);
PL_EXPORT(void)		PL_put_string_nchars(term_t t,
					     unsigned int len,
					     const char *chars);
PL_EXPORT(void)		PL_put_list_nchars(term_t t,
					   unsigned int l, const char *chars);
PL_EXPORT(void)		PL_put_list_ncodes(term_t t,
					   unsigned int l, const char *chars);
PL_EXPORT(void)		PL_put_integer(term_t t, long i);
PL_EXPORT(void)		PL_put_pointer(term_t t, void *ptr);
PL_EXPORT(void)		PL_put_float(term_t t, double f);
PL_EXPORT(void)		PL_put_functor(term_t t, functor_t functor);
PL_EXPORT(void)		PL_put_list(term_t l);
PL_EXPORT(void)		PL_put_nil(term_t l);
PL_EXPORT(void)		PL_put_term(term_t t1, term_t t2);

			/* construct a functor or list-cell */
PL_EXPORT(void)		PL_cons_functor(term_t h, functor_t f, ...);
PL_EXPORT(void)		PL_cons_functor_v(term_t h, functor_t fd, term_t a0);
PL_EXPORT(void)		PL_cons_list(term_t l, term_t h, term_t t);

			/* Unify term-references */
PL_EXPORT(int)		PL_unify(term_t t1, term_t t2);
PL_EXPORT(int)		PL_unify_atom(term_t t, atom_t a);
PL_EXPORT(int)		PL_unify_atom_chars(term_t t, const char *chars);
PL_EXPORT(int)		PL_unify_list_chars(term_t t, const char *chars);
PL_EXPORT(int)		PL_unify_list_codes(term_t t, const char *chars);
PL_EXPORT(int)		PL_unify_string_chars(term_t t, const char *chars);
PL_EXPORT(int)		PL_unify_atom_nchars(term_t t,
					     unsigned int l, const char *s);
PL_EXPORT(int)		PL_unify_list_ncodes(term_t t,
					     unsigned int l, const char *s);
PL_EXPORT(int)		PL_unify_list_nchars(term_t t,
					     unsigned int l, const char *s);
PL_EXPORT(int)		PL_unify_string_nchars(term_t t,
					       unsigned int len,
					       const char *chars);
PL_EXPORT(int)		PL_unify_integer(term_t t, long n);
PL_EXPORT(int)		PL_unify_float(term_t t, double f);
PL_EXPORT(int)		PL_unify_pointer(term_t t, void *ptr);
PL_EXPORT(int)		PL_unify_functor(term_t t, functor_t f);
PL_EXPORT(int)		PL_unify_list(term_t l, term_t h, term_t t);
PL_EXPORT(int)		PL_unify_nil(term_t l);
PL_EXPORT(int)		PL_unify_arg(int index, term_t t, term_t a);
PL_EXPORT(int)		PL_unify_term(term_t t, ...);

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
PL_EXPORT(void)		PL_changed_cwd(void); /* foreign code changed CWD */
PL_EXPORT(const char *) PL_cwd();


		 /*******************************
		 *   QUINTUS WRAPPER SUPPORT	*
		 *******************************/

PL_EXPORT(int)		PL_cvt_i_integer(term_t p, long *c);
PL_EXPORT(int)		PL_cvt_i_float(term_t p, double *c);
PL_EXPORT(int)		PL_cvt_i_single(term_t p, float *c);
PL_EXPORT(int)		PL_cvt_i_string(term_t p, char **c);
PL_EXPORT(int)		PL_cvt_i_atom(term_t p, atom_t *c);
PL_EXPORT(int)		PL_cvt_o_integer(long c, term_t p);
PL_EXPORT(int)		PL_cvt_o_float(double c, term_t p);
PL_EXPORT(int)		PL_cvt_o_single(float c, term_t p);
PL_EXPORT(int)		PL_cvt_o_string(const char *c, term_t p);
PL_EXPORT(int)		PL_cvt_o_atom(atom_t c, term_t p);


		 /*******************************
		 *	     COMPARE		*
		 *******************************/

PL_EXPORT(int)		PL_compare(term_t t1, term_t t2);
PL_EXPORT(int)		PL_same_compound(term_t t1, term_t t2);

		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

PL_EXPORT(int)		PL_warning(const char *fmt, ...);
PL_EXPORT(void)		PL_fatal_error(const char *fmt, ...);

		 /*******************************
		 *      RECORDED DATABASE	*
		 *******************************/

PL_EXPORT(record_t)	PL_record(term_t term);
PL_EXPORT(void)		PL_recorded(record_t record, term_t term);
PL_EXPORT(void)		PL_erase(record_t record);

PL_EXPORT(char *)	PL_record_external(term_t t, unsigned int *size);
PL_EXPORT(int)		PL_recorded_external(const char *rec, term_t term);
PL_EXPORT(int)		PL_erase_external(char *rec);

		 /*******************************
		 *	      FEATURES		*
		 *******************************/

PL_EXPORT(int)		PL_set_feature(const char *name, int type, ...);

		 /*******************************
		 *	INTERNAL FUNCTIONS	*
		 *******************************/

PL_EXPORT(PL_atomic_t)	_PL_get_atomic(term_t t);
PL_EXPORT(void)		_PL_put_atomic(term_t t, PL_atomic_t a);
PL_EXPORT(int)		_PL_unify_atomic(term_t t, PL_atomic_t a);
PL_EXPORT(void)		_PL_copy_atomic(term_t t, PL_atomic_t a);
PL_EXPORT(void)		_PL_get_arg(int index, term_t t, term_t a);


		 /*******************************
		 *	    CHAR BUFFERS	*
		 *******************************/

#define CVT_ATOM	0x0001
#define CVT_STRING	0x0002
#define CVT_LIST	0x0004
#define CVT_INTEGER	0x0008
#define CVT_FLOAT	0x0010
#define CVT_VARIABLE	0x0020
#define CVT_NUMBER	(CVT_INTEGER|CVT_FLOAT)
#define CVT_ATOMIC	(CVT_NUMBER|CVT_ATOM|CVT_STRING)
#define CVT_WRITE	0x0040		/* as of version 3.2.10 */
#define CVT_ALL		(CVT_ATOMIC|CVT_LIST)
#define CVT_MASK	0x00ff

#define BUF_DISCARDABLE	0x0000
#define BUF_RING	0x0100
#define BUF_MALLOC	0x0200

#ifdef SIO_MAGIC			/* defined from <SWI-Stream.h> */
		 /*******************************
		 *	  STREAM SUPPORT	*
		 *******************************/

					/* Make IOSTREAM known to Prolog */
PL_EXPORT(int)  	PL_open_stream(term_t t, IOSTREAM *s); /* compat */
PL_EXPORT(int)  	PL_unify_stream(term_t t, IOSTREAM *s);
PL_EXPORT(int)  	PL_get_stream_handle(term_t t, IOSTREAM **s);
PL_EXPORT(void) 	PL_release_stream(IOSTREAM *s);
PL_EXPORT(IOSTREAM *)	PL_open_resource(module_t m,
					 const char *name,
					 const char *rc_class,
					 const char *mode);

PL_EXPORT(IOSTREAM *)*_PL_streams(void);	/* base of streams */
#ifndef PL_KERNEL
#define Suser_input  (_PL_streams()[0])
#define Suser_output (_PL_streams()[1])
#define Suser_error  (_PL_streams()[2])
#endif

#define PL_WRT_QUOTED		0x01	/* quote atoms */
#define PL_WRT_IGNOREOPS	0x02	/* ignore list/operators */
#define PL_WRT_NUMBERVARS	0x04	/* print $VAR(N) as a variable */
#define PL_WRT_PORTRAY		0x08	/* call portray */
#define PL_WRT_CHARESCAPES	0x10	/* Output ISO escape sequences */

PL_EXPORT(int) PL_write_term(IOSTREAM *s,
			      term_t term,
			      int precedence,
			      int flags);

					/* PL_ttymode() results */
#define PL_NOTTY	0		/* -tty in effect */
#define PL_RAWTTY	1		/* get_single_char/1 */
#define PL_COOKEDTTY	2		/* normal input */

PL_EXPORT(int)		PL_ttymode(IOSTREAM *s);

#endif /*SIO_MAGIC*/

PL_EXPORT(int) PL_chars_to_term(const char *chars,
				 term_t term);


		 /*******************************
		 *	    EMBEDDING		*
		 *******************************/

PL_EXPORT(int)		PL_initialise(int argc, char **argv);
PL_EXPORT(int)		PL_is_initialised(int *argc, char ***argv);
install_t		PL_install_readline(void);
PL_EXPORT(int)		PL_toplevel(void);
PL_EXPORT(int)		PL_cleanup(int status);
PL_EXPORT(void)		PL_halt(int status);

		 /*******************************
		 *      INPUT/PROMPT/ETC	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE: the functions in this section are   not  documented, as as yet not
adviced for public usage.  They  are   intended  to  provide an abstract
interface for the GNU readline  interface   as  defined in pl-rl.c. This
abstract interface is necessary to make an embeddable system without the
readline overhead.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
					/* PL_dispatch() modes */
#define PL_DISPATCH_NOWAIT    0		/* Dispatch only once */
#define PL_DISPATCH_WAIT      1		/* Dispatch till input available */
#define PL_DISPATCH_INSTALLED 2		/* dispatch function installed? */

PL_EXPORT(int)		PL_dispatch(int fd, int wait);
PL_EXPORT(void)		PL_add_to_protocol(const char *buf, int count);
PL_EXPORT(char *)	PL_prompt_string(int fd);
PL_EXPORT(void)		PL_write_prompt(int dowrite);
PL_EXPORT(void)		PL_prompt_next(int fd);
PL_EXPORT(char *)	PL_atom_generator(const char *prefix, int state);
PL_EXPORT(void)		PL_clock_wait_ticks(long waited);


		 /*******************************
		 *	MEMORY ALLOCATION	*
		 *******************************/

PL_EXPORT(void *)	PL_malloc(size_t size);
PL_EXPORT(void *)	PL_realloc(void *mem, size_t size);
PL_EXPORT(void)		PL_free(void *mem);

		/********************************
		*             HOOKS		*
		********************************/

#define PL_DISPATCH_INPUT   0		/* There is input available */
#define PL_DISPATCH_TIMEOUT 1		/* Dispatch timeout */

typedef int  (*PL_dispatch_hook_t)(int fd);
typedef void (*PL_abort_hook_t)(void);
typedef void (*PL_initialise_hook_t)(int argc, char **argv);
typedef void (*PL_async_hook_t)(void);	/* Win32 only (O_ASYNC_HOOK) */
typedef int  (*PL_agc_hook_t)(atom_t a);

PL_EXPORT(PL_dispatch_hook_t) 	PL_dispatch_hook(PL_dispatch_hook_t);
PL_EXPORT(void)	       		PL_abort_hook(PL_abort_hook_t);
PL_EXPORT(void)	       		PL_initialise_hook(PL_initialise_hook_t);
PL_EXPORT(int)		      	PL_abort_unhook(PL_abort_hook_t);
PL_EXPORT(PL_async_hook_t)    	PL_async_hook(unsigned int, PL_async_hook_t);
PL_EXPORT(PL_agc_hook_t)      	PL_agc_hook(PL_agc_hook_t);


		/********************************
		*            SIGNALS            *
		*********************************/

#define PL_SIGSYNC	0x00010000	/* call handler synchronously */

PL_EXPORT(void) (*PL_signal(int sig, void (*func)(int)))(int);
PL_EXPORT(void) PL_interrupt(int sig);
PL_EXPORT(int)  PL_raise(int sig);
PL_EXPORT(int)  PL_handle_signals(void);

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

PL_EXPORT(int)	 PL_action(int, ...);	/* perform some action */
PL_EXPORT(void) PL_on_halt(void (*)(int, void *), void *);

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

PL_EXPORT(long)	PL_query(int);	/* get information from Prolog */


		 /*******************************
		 *	  PROLOG THREADS	*
		 *******************************/

typedef struct
{ unsigned long	    local_size;		/* Stack sizes */
  unsigned long	    global_size;
  unsigned long	    trail_size;
  unsigned long	    argument_size;
  char *	    alias;		/* alias name */
  int		  (*cancel)(int id);	/* cancel function */
  void *	    reserved[5];	/* reserved for extensions */
} PL_thread_attr_t;


PL_EXPORT(int)	PL_thread_self(void);	/* Prolog thread id (-1 if none) */
PL_EXPORT(int)	PL_thread_attach_engine(PL_thread_attr_t *attr);
PL_EXPORT(int)	PL_thread_destroy_engine(void);
PL_EXPORT(int)	PL_thread_at_exit(void (*function)(void *),
				  void *closure,
				  int global);
#if defined(_WINDOWS_)			/* <windows.h> is included */
PL_EXPORT(int) PL_w32thread_raise(DWORD dwTid, int sig);
#endif

		 /*******************************
		 *       FAST XPCE SUPPORT	*
		 *******************************/

typedef struct
{ int type;				/* PL_INTEGER or PL_ATOM */
  union
  { unsigned long i;			/* integer reference value */
    atom_t	  a;			/* atom reference value */
  } value;
} xpceref_t;

PL_EXPORT(int)	_PL_get_xpce_reference(term_t t, xpceref_t *ref);
PL_EXPORT(int)  _PL_unify_xpce_reference(term_t t, xpceref_t *ref);
PL_EXPORT(void) _PL_put_xpce_reference_i(term_t t, unsigned long r);
PL_EXPORT(void) _PL_put_xpce_reference_a(term_t t, atom_t name);

		 /*******************************
		 *        COMPATIBILITY		*
		 *******************************/

#ifdef PL_OLD_INTERFACE

typedef term_t term;
typedef PL_atomic_t atomic;

#ifndef _PL_INCLUDE_H
					/* renamed functions */
#define PL_is_var(t)		PL_is_variable(t)
#define PL_is_int(t)		PL_is_integer(t)
#define PL_is_term(t)		PL_is_compound(t)
#define PL_type(t)		PL_term_type(t)
#define PL_atom_value(a)	(char *)PL_atom_chars((atom_t)(a))
#define PL_predicate(f, m)	PL_pred(f, m)

					/* force undefined symbols */
					/* if PL_OLD_INTERFACE isn't set */
#define PL_strip_module(t, m)	_PL_strip_module(t, m)
#define PL_atomic(t)		_PL_atomic(t)
#define PL_float_value(t)	_PL_float_value(t)
#define PL_integer_value(t)	_PL_integer_value(t)
#define PL_string_value(t)	_PL_string_value(t)
#define PL_functor(t)		_PL_functor(t)
#define PL_arg(t, n)		_PL_arg(t, n)
#define PL_new_term()		_PL_new_term()
#define PL_new_integer(i)	_PL_new_integer(i)
#define PL_new_float(f)		_PL_new_float(f)
#define PL_new_string(s)	_PL_new_string(s)
#define PL_new_var()		_PL_new_var()
#define PL_term(a)		_PL_term(a)
#define PL_unify_atomic(t, a)	_PL_unify_atomic(t, (PL_atomic_t) (a))

typedef fid_t			bktrk_buf;
#define PL_mark(b)		(*(b) = PL_open_foreign_frame())
#define PL_bktrk(b)		PL_discard_foreign_frame(*(b))
#endif /*_PL_INCLUDE_H*/

		 /*******************************
		 *	     ANALYSIS		*
		 *******************************/

PL_EXPORT(PL_atomic_t)	_PL_atomic(term_t t);
PL_EXPORT(long)		_PL_integer_value(PL_atomic_t t);
PL_EXPORT(double)	_PL_float_value(PL_atomic_t t);
PL_EXPORT(char *)	_PL_string_value(PL_atomic_t t);
PL_EXPORT(char *)	_PL_list_string_value(term_t t);
PL_EXPORT(functor_t)	_PL_functor(term_t t);
PL_EXPORT(term_t)	_PL_arg(term_t t, int n);


		 /*******************************
		 *	     CONSTRUCT		*
		 *******************************/

PL_EXPORT(term_t)	_PL_new_term(void);
PL_EXPORT(PL_atomic_t)	_PL_new_integer(long i);
PL_EXPORT(PL_atomic_t)	_PL_new_float(double f);
PL_EXPORT(PL_atomic_t)	_PL_new_string(const char *s);
PL_EXPORT(PL_atomic_t)	_PL_new_var(void);
PL_EXPORT(term_t)	_PL_term(PL_atomic_t a);

		 /*******************************
		 *	       UNIFY		*
		 *******************************/

PL_EXPORT(int)		_PL_unify_atomic(term_t t, PL_atomic_t a);

		 /*******************************
		 *	       MODULES		*
		 *******************************/

PL_EXPORT(term_t)	_PL_strip_module(term_t t, module_t *m);

#endif /*PL_OLD_INTERFACE*/

#ifdef __cplusplus
}
#endif

#endif /*_FLI_H_INCLUDED*/
