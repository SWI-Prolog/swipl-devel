/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: SWI-Prolog foreign language interface include file
*/

#ifndef PL_INCLUDED
#define PL_INCLUDED

#ifndef PLVERSION
#define PLVERSION "2.1.7, Sep 1995"
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
_declspec(dllexport) is used bu MSVC++ 2.0 to declare exports from
libraries.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if defined(__WIN32__) && defined(PL_KERNEL)
#define __pl_export _declspec(dllexport)
#else
#define __pl_export extern
#endif

#ifdef __WIN32__
#define install_t _declspec(dllexport) void
#else
#define install_t void
#endif

#if __GNUC__ && !__STRICT_ANSI__
#define __pl_constf const		/* const function */
#else
#define __pl_constf
#endif

#ifndef PL_KERNEL
typedef	unsigned long	atomic_t;	/* atomic Prolog datum */
typedef void *		functor_t;	/* name/arity pair as Prolog */
typedef void *		module_t;	/* Prolog module */
typedef void *		predicate_t;	/* Prolog procedure */
typedef atomic_t *	term_t;		/* general term */
typedef unsigned long	foreign_t;	/* return type of foreign functions */
#define O_STRING 1


#define atomic  atomic_t		/* backward compatibility */
#define functor functor_t		/* backward compatibility */
#define module  module_t		/* backward compatibility */
#define term    term_t			/* backward compatibility */

#else
typedef word		atomic_t;
typedef FunctorDef	functor_t;
typedef Procedure	predicate_t;
typedef Module		module_t;
typedef Word		term_t;
typedef word		foreign_t;
#endif

typedef foreign_t	(*pl_function_t)(); /* foreign language functions */

#ifndef TRUE
#define TRUE	(1)
#define FALSE	(0)
#endif

typedef struct
{ unsigned long context[2];
} bktrk_buf;				/* data-backtrack buffer */

typedef struct _PL_extension
{ char 		*predicate_name;	/* Name of the predicate */
  short		arity;			/* Arity of the predicate */
  pl_function_t	function;		/* Implementing functions */
  short		flags;			/* Or of PL_FA_... */
} PL_extension;

extern PL_extension PL_extensions[];	/* see pl-extend.c */


		/********************************
		*            SYMBOLS            *
		*********************************/

/*  The TOS linker on the ATARI_ST only allows for 8 character external
    symbols.  The macros below redefine some of the interface function
    names to avoid name clashes.
*/

#if __TOS__
#define	PL_new_functor		PL_nfunc
#define PL_new_float		PL_nflt
#define PL_functor		PL_funct
#define PL_functor_name		PL_fname
#define PL_functor_arity	PL_farity
#define	PL_unify		PL_uf
#define PL_unify_atomic		PL_ufa
#define PL_unify_functor	PL_uff
#endif

		/********************************
		*           ANALYSES            *
		*********************************/

#define	PL_VARIABLE	(1)
#define PL_ATOM		(2)
#define PL_INTEGER	(3)
#define PL_FLOAT	(4)
#define PL_STRING	(5)
#define PL_TERM		(6)

#define PL_atomic(t)	(*(t))		/* convert term to atomic */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
New style type-checks.  If you want  to   know  whether  `x' is an atom,
PL_is_atom(x) is the same as PL_type(x) ==   PL_ATOM, but the first is a
lot faster.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

__pl_export int __pl_constf PL_is_var(const term_t);
__pl_export int __pl_constf PL_is_int(const term_t);
__pl_export int __pl_constf PL_is_atom(const term_t);
__pl_export int __pl_constf PL_is_float(const term_t);
__pl_export int __pl_constf PL_is_string(const term_t);
__pl_export int __pl_constf PL_is_term(const term_t);


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
General analysis functions.  PL_type() may often   be replaced by one of
the above.  Note  thats  the  `extract'   functions  donot  perform  any
checking, so you'd better verify the type yourself before calling them.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

__pl_export int __pl_constf	PL_type(const term_t);
__pl_export long __pl_constf	PL_integer_value(const atomic_t);
__pl_export double		PL_float_value(const atomic_t);
__pl_export char *		PL_string_value(const atomic_t);
__pl_export char *		PL_list_string_value(const term_t);
__pl_export char * __pl_constf	PL_atom_value(const atomic_t);
__pl_export functor_t __pl_constf PL_functor(const term_t);
__pl_export atomic_t __pl_constf  PL_functor_name(const functor_t);
__pl_export int __pl_constf	PL_functor_arity(const functor_t);
__pl_export term_t __pl_constf	PL_arg(const term_t, int);
__pl_export term_t * PL_univg(const term_t, atomic_t *, int *, term_t *);
__pl_export term_t __pl_constf	PL_strip_module(const term_t, module_t*);


		/********************************
		*         CONSTRUCTION          *
		*********************************/

__pl_export term_t	PL_new_term(void);
__pl_export atomic_t	PL_new_atom(char *);
__pl_export atomic_t	PL_new_integer(int);
__pl_export atomic_t	PL_new_float(double);
#if O_STRING
__pl_export atomic_t	PL_new_string(char *);
#endif /* O_STRING */
__pl_export atomic_t	PL_new_var(void);
__pl_export atomic_t	PL_new_compound(functor_t f, atomic_t *args);
__pl_export term_t	PL_term(atomic_t a);
__pl_export void	PL_term_vector(int size, term_t *t, atomic_t *a);
__pl_export functor_t	PL_new_functor(atomic_t name, int arity);
__pl_export int		PL_unify(term_t, term_t);
__pl_export int		PL_unify_atomic(term_t, atomic_t);
__pl_export int		PL_unify_functor(term_t, functor_t);


		 /*******************************
		 *	   QUINTUS STYLE	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These functions are used by the automatically generated wrappers for the
Quintus' style C-interface.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

__pl_export int	PL_cvt_i_integer(term_t, long *);
__pl_export int	PL_cvt_i_float(term_t, double *);
__pl_export int	PL_cvt_i_single(term_t, float *);
__pl_export int	PL_cvt_i_string(term_t, char **);
__pl_export int	PL_cvt_i_atom(term_t, atomic_t *);

__pl_export int	PL_cvt_o_integer(long, term_t);
__pl_export int	PL_cvt_o_float(double, term_t);
__pl_export int	PL_cvt_o_single(float, term_t);
__pl_export int	PL_cvt_o_string(char *, term_t);
__pl_export int	PL_cvt_o_atom(atomic_t, term_t);

__pl_export int	PL_load_extensions(PL_extension *); /* load extensions */

		/********************************
		*    DETERMINISTIC CALL/RETURN  *
		*********************************/

#define	PL_succeed	return 1	/* succeed deterministically */
#define PL_fail		return 0	/* fail */

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
#define PL_retry_address(a)	return _PL_retry((long) a)

__pl_export int	 		PL_foreign_control(long);
__pl_export foreign_t		_PL_retry(long);
__pl_export foreign_t		_PL_retry_address(void *);
__pl_export long	 	PL_foreign_context(long);
__pl_export void *	 	PL_foreign_context_address(long);

		/********************************
		*      REGISTERING FOREIGNS     *
		*********************************/

#define PL_FA_NOTRACE		(1)	/* foreign cannot be traced */
#define PL_FA_TRANSPARENT	(2)	/* foreign is module transparent */
#define PL_FA_NONDETERMINISTIC	(4)	/* foreign is non-deterministic */
#define PL_FA_GCSAFE		(8)	/* safe to GC and/or shift stacks */

__pl_export int	PL_register_foreign(char *, int, pl_function_t, int flags);

		/********************************
		*        CALLING PROLOG         *
		*********************************/

#define PL_lock(t)	_PL_lock(&(t));
#define PL_unlock(t)	_PL_unlock(&(t));

__pl_export void PL_mark(bktrk_buf *);	/* mark global and trail stack */
__pl_export void PL_bktrk(bktrk_buf *); /* backtrack global stack to mark */
__pl_export void _PL_lock(term_t *);	/* lock term_t variable */
__pl_export void _PL_unlock(term_t *);	/* unlock term_t variable */

__pl_export int	PL_call(term_t, module_t); /* invoke term_t as Prolog goal */

					/* find predicate */
__pl_export predicate_t PL_predicate(functor_t functor, module_t module);
					/* call it directly */
__pl_export int		PL_predicate_arity(predicate_t pred);
__pl_export atomic_t	PL_predicate_name(predicate_t pred);
__pl_export functor_t	PL_predicate_functor(predicate_t pred);
__pl_export module_t	PL_predicate_module(predicate_t pred);
__pl_export int		PL_call_predicate(module_t context, int debug,
					  predicate_t pred,
					  term_t *argv);

		 /*******************************
		 *	    EMBEDDING		*
		 *******************************/

__pl_export int		PL_initialise(int argc, char **argv, char **env);
__pl_export int		PL_toplevel(void);
__pl_export void	PL_halt(int status);


		/********************************
		*            MODULES            *
		*********************************/

__pl_export module_t PL_context(void);	/* context module of predicate */
__pl_export atomic_t PL_module_name(module_t); /* return name of a module */
__pl_export module_t PL_new_module(atomic_t); /* return module from an atom */


		/********************************
		*             HOOKS	*
		********************************/

#define PL_DISPATCH_INPUT   0		/* There is input available */
#define PL_DISPATCH_TIMEOUT 1		/* Dispatch timeout */

typedef int  (*PL_dispatch_hook_t)(void);
typedef void (*PL_abort_hook_t)(void);
typedef void (*PL_reinit_hook_t)(int argc, char **argv);

__pl_export PL_dispatch_hook_t PL_dispatch_hook(PL_dispatch_hook_t);
__pl_export void	       PL_abort_hook(PL_abort_hook_t);
__pl_export void	       PL_reinit_hook(PL_reinit_hook_t);
__pl_export int		       PL_abort_unhook(PL_abort_hook_t);
__pl_export int		       PL_reinit_unhook(PL_reinit_hook_t);


		/********************************
		*            SIGNALS            *
		*********************************/

__pl_export void (*PL_signal(int sig, void (*func)(int)))(int);


		 /*******************************
		 *	      PROMPT		*
		 *******************************/

__pl_export void PL_prompt1(const char *prompt);


		/********************************
		*           WARNINGS            *
		*********************************/

__pl_export int	PL_warning(char *, ...);
__pl_export void PL_fatal_error(char *, ...);	/* Print warning and die */

		/********************************
		*        PROLOG ACTIONS         *
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

__pl_export long	PL_query(int);	/* get information from Prolog */

#ifdef SIO_MAGIC
		 /*******************************
		 *	  STREAM SUPPORT	*
		 *******************************/

					/* Make IOSTREAM known to Prolog */
__pl_export int PL_open_stream(IOSTREAM *s, term_t t);
#endif

#endif /* PL_INCLUDED */
/* DO NOT WRITE BELOW THIS ENDIF */
