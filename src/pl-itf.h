/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: Virtual machine instruction interpreter
*/

#ifndef _FLI_H_INCLUDED
#define _FLI_H_INCLUDED

#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This interface has been changed considerable  between version 2.1.14 and
2.5.0. You may get most old code to work using

	#define PL_OLD_INTERFACE

before loading this file.  See end of this file.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef PLVERSION
#define PLVERSION "2.7.8, June 1996"
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
_declspec(dllexport) is used by MSVC++ 2.0 to declare exports from DLL's.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* Get export declarations right.  Also in SWI-Stream.h, hence the
   check to avoid doing it twice.
*/

#ifndef _PL_EXPORT_DONE
#define _PL_EXPORT_DONE
#ifdef WIN32
#ifndef __WIN32__
#define __WIN32__
#endif
#endif

#ifdef __WIN32__
#ifdef PL_KERNEL
#define __pl_export	 _declspec(dllexport)
#define __pl_export_data _declspec(dllexport)
#define install_t	 void
#else
#define __pl_export	 extern
#define __pl_export_data _declspec(dllimport)
#define install_t	 _declspec(dllexport) void
#endif
#else /*__WIN32__*/
#define __pl_export	 extern
#define __pl_export_data extern
#define install_t	 void
#endif /*__WIN32__*/
#endif /*_PL_EXPORT_DONE*/

#ifdef _PL_INCLUDE_H
typedef FunctorDef	functor_t;	/* Name/arity pair */
typedef Module		module_t;	/* a module */
typedef Procedure	predicate_t;	/* a predicate handle */
#else
typedef void *		functor_t;	/* name/arity pair as Prolog */
typedef	unsigned long	atom_t;		/* Prolog atom */
typedef void *		module_t;	/* Prolog module */
typedef void *		predicate_t;	/* Prolog procedure */
typedef unsigned long	term_t;		/* opaque term handle */
typedef unsigned long	qid_t;		/* opaque query handle */
typedef unsigned long	fid_t;		/* opaque foreign context handle */
#endif
typedef unsigned long	atomic_t;	/* same a word */
typedef unsigned long	control_t;	/* non-deterministic control arg */
typedef unsigned long	foreign_t;	/* return type of foreign functions */
typedef foreign_t	(*pl_function_t)(); /* foreign language functions */

#ifndef TRUE
#define TRUE	(1)
#define FALSE	(0)
#endif

		 /*******************************
		 *      TERM-TYPE CONSTANTS	*
		 *******************************/

					/* PL_unify_term() arguments */
#define	PL_VARIABLE	(1)		/* nothing */
#define PL_ATOM		(2)		/* const char * */
#define PL_INTEGER	(3)		/* int */
#define PL_FLOAT	(4)		/* double */
#define PL_STRING	(5)		/* const char * */
#define PL_TERM		(6)

#define PL_FUNCTOR	(10)		/* functor_t, arg ... */
#define PL_LIST		(11)		/* length, arg ... */
#define PL_CHARS	(12)		/* const char * */


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

__pl_export foreign_t		_PL_retry(long);
__pl_export foreign_t		_PL_retry_address(void *);
__pl_export int	 		PL_foreign_control(control_t);
__pl_export long	 	PL_foreign_context(control_t);
__pl_export void *	 	PL_foreign_context_address(control_t);


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

extern			PL_extension PL_extensions[]; /* not Win32! */
__pl_export void	PL_register_extensions(PL_extension *e);
__pl_export int		PL_register_foreign(const char *name, int arity,
					    pl_function_t func, int flags);


		/********************************
		*            MODULES            *
		*********************************/

__pl_export module_t	PL_context(void);
__pl_export atom_t	PL_module_name(module_t module);
__pl_export module_t	PL_new_module(atom_t name);
__pl_export int		PL_strip_module(term_t in, module_t *m, term_t out);


		 /*******************************
		 *	     CALL-BACK		*
		 *******************************/

			/* Foreign context frames */
__pl_export fid_t	PL_open_foreign_frame(void);
__pl_export void	PL_close_foreign_frame(fid_t cid);
__pl_export void	PL_discard_foreign_frame(fid_t cid);

			/* Finding predicates */
__pl_export predicate_t	PL_pred(functor_t f, module_t m);
__pl_export predicate_t	PL_predicate(const char *name, int arity,
				     const char* module);
__pl_export int		PL_predicate_info(predicate_t pred,
					  atom_t *name, int *arity,
					  module_t *module);

			/* Call-back */
__pl_export qid_t	PL_open_query(module_t m, int debug,
				      predicate_t pred, term_t t0);
__pl_export int		PL_next_solution(qid_t qid);
__pl_export void	PL_close_query(qid_t qid);
__pl_export void	PL_cut_query(qid_t qid);

			/* Simplified (but less flexible) call-back */
__pl_export int		PL_call(term_t t, module_t m);
__pl_export int		PL_call_predicate(module_t m, int debug,
					  predicate_t pred, term_t t0);

		 /*******************************
		 *        TERM-REFERENCES	*
		 *******************************/

			/* Creating and destroying term-refs */
__pl_export term_t	PL_new_term_refs(int n);
__pl_export term_t	PL_new_term_ref(void);
__pl_export term_t	PL_copy_term_ref(term_t from);
__pl_export void	PL_reset_term_refs(term_t r);

			/* Constants */
__pl_export atom_t	PL_new_atom(const char *s);
__pl_export const char *PL_atom_chars(atom_t a);
__pl_export functor_t	PL_new_functor(atom_t f, int a);
__pl_export atom_t	PL_functor_name(functor_t f);
__pl_export int		PL_functor_arity(functor_t f);

			/* Get C-values from Prolog terms */
__pl_export int		PL_get_atom(term_t t, atom_t *a);
__pl_export int		PL_get_atom_chars(term_t t, char **a);
__pl_export int		PL_get_string(term_t t, char **s, int *len);
__pl_export int		PL_get_list_chars(term_t l, char **s, unsigned flags);
__pl_export int		PL_get_chars(term_t t, char **s, unsigned flags);
__pl_export int		PL_get_integer(term_t t, int *i);
__pl_export int		PL_get_long(term_t t, long *i);
__pl_export int		PL_get_pointer(term_t t, void **ptr);
__pl_export int		PL_get_float(term_t t, double *f);
__pl_export int		PL_get_functor(term_t t, functor_t *f);
__pl_export int		PL_get_name_arity(term_t t, atom_t *name, int *arity);
__pl_export int		PL_get_module(term_t t, module_t *module);
__pl_export int		PL_get_arg(int index, term_t t, term_t a);
__pl_export int		PL_get_list(term_t l, term_t h, term_t t);
__pl_export int		PL_get_head(term_t l, term_t h);
__pl_export int		PL_get_tail(term_t l, term_t t);
__pl_export int		PL_get_nil(term_t l);

			/* Verify types */
__pl_export int		PL_term_type(term_t t);
__pl_export int		PL_is_variable(term_t t);
__pl_export int		PL_is_atom(term_t t);
__pl_export int		PL_is_integer(term_t t);
__pl_export int		PL_is_string(term_t t);
__pl_export int		PL_is_float(term_t t);
__pl_export int		PL_is_compound(term_t t);
__pl_export int		PL_is_functor(term_t t, functor_t f);
__pl_export int		PL_is_list(term_t t);
__pl_export int		PL_is_atomic(term_t t);
__pl_export int		PL_is_number(term_t t);

			/* Assign to term-references */
__pl_export void	PL_put_variable(term_t t);
__pl_export void	PL_put_atom(term_t t, atom_t a);
__pl_export void	PL_put_atom_chars(term_t t, const char *chars);
__pl_export void	PL_put_string_chars(term_t t, const char *chars);
__pl_export void	PL_put_list_chars(term_t t, const char *chars);
__pl_export void	PL_put_integer(term_t t, long i);
__pl_export void	PL_put_pointer(term_t t, void *ptr);
__pl_export void	PL_put_float(term_t t, double f);
__pl_export void	PL_put_functor(term_t t, functor_t functor);
__pl_export void	PL_put_list(term_t l);
__pl_export void	PL_put_nil(term_t l);
__pl_export void	PL_put_term(term_t t1, term_t t2);

			/* construct a functor or list-cell */
__pl_export void	PL_cons_functor(term_t h, functor_t f, ...);
__pl_export void	PL_cons_list(term_t l, term_t h, term_t t);

			/* Unify term-references */
__pl_export int		PL_unify(term_t t1, term_t t2);
__pl_export int		PL_unify_atom(term_t t, atom_t a);
__pl_export int		PL_unify_atom_chars(term_t t, const char *chars);
__pl_export int		PL_unify_list_chars(term_t t, const char *chars);
__pl_export int		PL_unify_string_chars(term_t t, const char *chars);
__pl_export int		PL_unify_string_nchars(term_t t,
					       int len, const char *chars);
__pl_export int		PL_unify_integer(term_t t, long n);
__pl_export int		PL_unify_float(term_t t, double f);
__pl_export int		PL_unify_pointer(term_t t, void *ptr);
__pl_export int		PL_unify_functor(term_t t, functor_t f);
__pl_export int		PL_unify_list(term_t l, term_t h, term_t t);
__pl_export int		PL_unify_nil(term_t l);
__pl_export int		PL_unify_arg(int index, term_t t, term_t a);
__pl_export int		PL_unify_term(term_t t, ...);

		 /*******************************
		 *	  FILENAME SUPPORT	*
		 *******************************/

__pl_export char *	PL_get_filename(term_t n, char *buf, unsigned int sz);


		 /*******************************
		 *   QUINTUS WRAPPER SUPPORT	*
		 *******************************/

__pl_export int		PL_cvt_i_integer(term_t p, long *c);
__pl_export int		PL_cvt_i_float(term_t p, double *c);
__pl_export int		PL_cvt_i_single(term_t p, float *c);
__pl_export int		PL_cvt_i_string(term_t p, char **c);
__pl_export int		PL_cvt_i_atom(term_t p, atom_t *c);
__pl_export int		PL_cvt_o_integer(long c, term_t p);
__pl_export int		PL_cvt_o_float(double c, term_t p);
__pl_export int		PL_cvt_o_single(float c, term_t p);
__pl_export int		PL_cvt_o_string(const char *c, term_t p);
__pl_export int		PL_cvt_o_atom(atom_t c, term_t p);


		 /*******************************
		 *	     COMPARE		*
		 *******************************/

__pl_export int		PL_compare(term_t t1, term_t t2);

		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

__pl_export int		PL_warning(const char *fmt, ...);
__pl_export void	PL_fatal_error(const char *fmt, ...);

		 /*******************************
		 *	      FEATURES		*
		 *******************************/

__pl_export int		PL_set_feature(const char *name, int type, ...);

		 /*******************************
		 *	INTERNAL FUNCTIONS	*
		 *******************************/

__pl_export atomic_t	_PL_get_atomic(term_t t);
__pl_export void	_PL_put_atomic(term_t t, atomic_t a);
__pl_export int		_PL_unify_atomic(term_t t, atomic_t a);
__pl_export void	_PL_copy_atomic(term_t t, atomic_t a);
	    int		_PL_get_name_arity(term_t t, atom_t *name, int *arity);
	    void	_PL_get_arg(int index, term_t t, term_t a);


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
#define CVT_ALL		0x00ff

#define BUF_DISCARDABLE	0x0000
#define BUF_RING	0x0100
#define BUF_MALLOC	0x0200

#ifdef SIO_MAGIC			/* defined from <SWI-Stream.h> */
		 /*******************************
		 *	  STREAM SUPPORT	*
		 *******************************/

					/* Make IOSTREAM known to Prolog */
__pl_export int PL_open_stream(term_t t, IOSTREAM *s);
#endif


		 /*******************************
		 *	    EMBEDDING		*
		 *******************************/

__pl_export int		PL_initialise(int argc, char **argv, char **env);
install_t		PL_install_readline(void);
__pl_export int		PL_toplevel(void);
__pl_export void	PL_halt(int status);

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

					/* PL_ttymode() results */
#define PL_NOTTY	0		/* -tty in effect */
#define PL_RAWTTY	1		/* get_single_char/1 */
#define PL_COOKEDTTY	2		/* normal input */

__pl_export int		PL_dispatch(int fd, int wait);
__pl_export int		PL_ttymode(int fd);
__pl_export void	PL_add_to_protocol(const char *buf, int count);
__pl_export char *	PL_prompt_string(int fd);
__pl_export void	PL_write_prompt(int fd, int dowrite);
__pl_export void	PL_prompt_next(int fd);
__pl_export char *	PL_atom_generator(char *prefix, int state);
__pl_export void	PL_clock_wait_ticks(long waited);


		/********************************
		*             HOOKS		*
		********************************/

#define PL_DISPATCH_INPUT   0		/* There is input available */
#define PL_DISPATCH_TIMEOUT 1		/* Dispatch timeout */

typedef int  (*PL_dispatch_hook_t)(int fd);
typedef void (*PL_abort_hook_t)(void);
typedef void (*PL_initialise_hook_t)(int argc, char **argv);
typedef void (*PL_async_hook_t)(void);	/* Win32 only (O_ASYNC_HOOK) */
typedef int  (*PL_getkey_hook_t)(int fd);

__pl_export PL_dispatch_hook_t PL_dispatch_hook(PL_dispatch_hook_t);
__pl_export void	       PL_abort_hook(PL_abort_hook_t);
__pl_export void	       PL_initialise_hook(PL_initialise_hook_t);
__pl_export int		       PL_abort_unhook(PL_abort_hook_t);
__pl_export PL_async_hook_t    PL_async_hook(unsigned int, PL_async_hook_t);
__pl_export PL_getkey_hook_t   PL_getkey_hook(PL_getkey_hook_t);


		/********************************
		*            SIGNALS            *
		*********************************/

__pl_export void (*PL_signal(int sig, void (*func)(int)))(int);
__pl_export void PL_interrupt(int sig);

		/********************************
		*      PROLOG ACTION/QUERY      *
		*********************************/

#define	PL_ACTION_TRACE		1	/* switch to trace mode */
#define PL_ACTION_DEBUG		2	/* switch to debug mode */
#define PL_ACTION_BACKTRACE	3	/* show a backtrace (stack dump) */
#define PL_ACTION_BREAK		4	/* create a break environment */
#define PL_ACTION_HALT		5	/* halt Prolog execution */
#define PL_ACTION_ABORT		6	/* generate a Prolog abort */
#define PL_ACTION_SYMBOLFILE	7	/* make arg. the symbol file */
#define PL_ACTION_WRITE		8	/* write via Prolog i/o buffer */
#define PL_ACTION_FLUSH		9	/* Flush Prolog i/o buffer */

__pl_export int	 PL_action(int, void *); /* perform some action */
__pl_export void PL_on_halt(void (*)(int, void *), void *);

		/********************************
		*         QUERY PROLOG          *
		*********************************/

#define PL_QUERY_ARGC		1	/* return main() argc */
#define PL_QUERY_ARGV		2	/* return main() argv */
#define PL_QUERY_SYMBOLFILE	3	/* return current symbol file */
#define PL_QUERY_ORGSYMBOLFILE	4	/* symbol file before first load */
#define PL_QUERY_GETC		5	/* Read character from terminal */
#define PL_QUERY_MAX_INTEGER	6	/* largest integer */
#define PL_QUERY_MIN_INTEGER	7	/* smallest integer */
#define PL_QUERY_MAX_TAGGED_INT	8	/* largest tagged integer */
#define PL_QUERY_MIN_TAGGED_INT	9	/* smallest tagged integer */
#define PL_QUERY_VERSION       10	/* 207006 = 2.7.6 */

__pl_export long	PL_query(int);	/* get information from Prolog */

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

__pl_export int		_PL_get_xpce_reference(term_t t, xpceref_t *ref);
__pl_export int		_PL_unify_xpce_reference(term_t t, xpceref_t *ref);
__pl_export void	_PL_put_xpce_reference_i(term_t t, unsigned long r);
__pl_export void	_PL_put_xpce_reference_a(term_t t, atom_t name);

		 /*******************************
		 *        COMPATIBILITY		*
		 *******************************/

#ifdef PL_OLD_INTERFACE

typedef term_t term;
typedef atomic_t atomic;

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
#define PL_unify_atomic(t, a)	_PL_unify_atomic(t, (atomic_t) (a))

typedef fid_t			bktrk_buf;
#define PL_mark(b)		(*(b) = PL_open_foreign_frame())
#define PL_bktrk(b)		PL_discard_foreign_frame(*(b))
#endif /*_PL_INCLUDE_H*/

		 /*******************************
		 *	     ANALYSIS		*
		 *******************************/

__pl_export atomic_t	_PL_atomic(term_t t);
__pl_export long	_PL_integer_value(atomic_t t);
__pl_export double	_PL_float_value(atomic_t t);
__pl_export char *	_PL_string_value(atomic_t t);
__pl_export char *	_PL_list_string_value(term_t t);
__pl_export functor_t	_PL_functor(term_t t);
__pl_export term_t	_PL_arg(term_t t, int n);


		 /*******************************
		 *	     CONSTRUCT		*
		 *******************************/

__pl_export term_t	_PL_new_term(void);
__pl_export atomic_t	_PL_new_integer(long i);
__pl_export atomic_t	_PL_new_float(double f);
__pl_export atomic_t	_PL_new_string(const char *s);
__pl_export atomic_t	_PL_new_var(void);
__pl_export term_t	_PL_term(atomic_t a);

		 /*******************************
		 *	       UNIFY		*
		 *******************************/

__pl_export int		_PL_unify_atomic(term_t t, atomic_t a);

		 /*******************************
		 *	       MODULES		*
		 *******************************/

__pl_export term_t	_PL_strip_module(term_t t, module_t *m);

#endif /*PL_OLD_INTERFACE*/

#ifdef __cplusplus
}
#endif

#endif /*_FLI_H_INCLUDED*/
