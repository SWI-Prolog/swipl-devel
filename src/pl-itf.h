/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: SWI-Prolog foreign language interface include file
*/

#ifndef PL_INCLUDED
#define PL_INCLUDED

#ifndef PLVERSION
#define PLVERSION "1.6.2, July 1992"
#endif

#ifndef P
#if __STDC__ || PROTO
#define P(x) x
#else
#define P(x) ()
#endif
#endif

#ifndef PL_KERNEL
typedef	unsigned long	atomic;		/* atomic Prolog datum */
typedef unsigned long	functor;	/* name/arity pair as Prolog */
typedef unsigned long	module;		/* Prolog module */
typedef unsigned long *	term;		/* general term */
typedef unsigned long	foreign_t;	/* return type of foreign functions */
#define O_STRING 1
#else
typedef word		atomic;
typedef FunctorDef	functor;
typedef Module		module;
typedef Word		term;
typedef word		foreign_t;
#endif

typedef foreign_t	(*function)();	/* foreign language functions */	

#ifndef TRUE
#define TRUE	(1)
#define FALSE	(0)
#endif

typedef struct
{ unsigned long context[2];
} bktrk_buf;				/* data-backtrack buffer */


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

int const PL_is_var P((const term));
int const PL_is_int P((const term));
int const PL_is_atom P((const term));
int const PL_is_float P((const term));
int const PL_is_string P((const term));
int const PL_is_term P((const term));

int	const PL_type P((const term));
long	const PL_integer_value P((const atomic));
double	      PL_float_value P((const atomic));
char *	      PL_string_value P((const atomic));
char *        PL_list_string_value P((const term));
char *	const PL_atom_value P((const atomic));
functor const PL_functor P((const term));
atomic	const PL_functor_name P((const functor));
int	const PL_functor_arity P((const functor));
term	const PL_arg P((const term, int));
term	const PL_strip_module P((const term, module*));

		/********************************
		*         CONSTRUCTION          *
		*********************************/

term	PL_new_term P((void));		/* create a new term (variable) */
atomic	PL_new_atom P((char *));	/* create an atom from a char * */
atomic  PL_new_integer P((int));	/* create a new integer */
atomic	PL_new_float P((double));	/* create a new float */
#if O_STRING
atomic	PL_new_string P((char *));	/* create a new string */
#endif O_STRING
functor	PL_new_functor P((atomic, int)); /* create a new functor */
int	PL_unify P((term, term));	/* unify two terms */
int	PL_unify_atomic P((term, atomic));  /* unify term with atomic value */
int	PL_unify_functor P((term, functor));/* unify term with functor */

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

#define PL_FRG_CUT 	(0x80000000)		/* highest bit */
#define PL_FRG_MASK	(0x40000000)		/* Mask to indicate redo */
#define PL_FRG_MASKMASK	(FRG_CUT|FRG_MASK)	/* Conbined mask */
#define PL_FRG_FIRSTCALL (0L)

#define PL_FIRST_CALL	(0)
#define PL_CUTTED	(1)
#define PL_REDO		(2)

#define PL_retry(v)	return (foreign_t) (((long)(v) & ~PL_FRG_MASKMASK) \
						       | PL_FRG_MASK)
#define PL_foreign_control(h)	((long)(h) == PL_FIRST_CALL ? PL_FIRST_CALL : \
				 (long)(h) & PL_FRG_CUT     ? PL_CUTTED : \
							      PL_REDO)
#define PL_foreign_context(h)	(((long)(h) << 2) >> 2)


		/********************************
		*      REGISTERING FOREIGNS     *
		*********************************/

#define PL_FA_NOTRACE		(1)	/* foreign cannot be traced */
#define PL_FA_TRANSPARENT	(2)	/* foreign is module transparent */
#define PL_FA_NONDETERMINISTIC	(4)	/* foreign is non-deterministic */

int	PL_register_foreign P((char *, int, function, ...));

		/********************************
		*        CALLING PROLOG         *
		*********************************/

void	PL_mark P((bktrk_buf *));	/* mark global and trail stack */
void	PL_bktrk P((bktrk_buf *));	/* backtrack global stack to mark */

int	PL_call P((term, module));	/* invoke term as Prolog goal */

		/********************************
		*            MODULES            *
		*********************************/

module	PL_context P((void));		/* context module of predicate */		
atomic	PL_module_name P((module));	/* return name of a module */
module	PL_new_module P((atomic));	/* return module from an atom */


		/********************************
		*         EVENT HANDLING	*
		********************************/

#define PL_DISPATCH_INPUT   0		/* There is input available */
#define PL_DISPATCH_TIMEOUT 1		/* Dispatch timeout */

extern int (*PL_dispatch_events) P((void));	/* Dispatch user events */


		/********************************
		*      INITIALISATION HOOK	*
		********************************/

extern void (*PL_foreign_reinit_function) P((int argc, char **argv));


		/********************************
		*            SIGNALS            *
		*********************************/

void (*PL_signal P((int sig, void (*func)())))(); /* signal() replacement */


		/********************************
		*             ABORTS		*
		********************************/

void PL_abort_handle P((void (*func)())); /* func called by pl_abort() */


		/********************************
		*           WARNINGS            *
		*********************************/

int	PL_warning P((char *, ...));	/* Print standard Prolog warning */
void	PL_fatal_error P((char *, ...));	/* Print warning and die */

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

int	PL_action P((int, void *));	/* perform some action */

		/********************************
		*         QUERY PROLOG          *
		*********************************/

#define PL_QUERY_ARGC		1	/* return main() argc */
#define PL_QUERY_ARGV		2	/* return main() argv */
#define PL_QUERY_SYMBOLFILE	3	/* return current symbol file */
#define PL_QUERY_ORGSYMBOLFILE	4	/* symbol file before first load */
#define PL_QUERY_GETC		5	/* Read character from terminal */

long	PL_query P((int));		/* get information from Prolog */

		/********************************
		*        STATIC LINKING		*
		********************************/

typedef struct _PL_extension
{ char 		*predicate_name;	/* Name of the predicate */
  short		arity;			/* Arity of the predicate */
  function	function;		/* Implementing functions */
  short		flags;			/* Or of PL_FA_... */
} PL_extension;

extern PL_extension PL_extensions[];	/* see pl-extend.c */

#endif PL_INCLUDED
/* DO NOT WRITE BELOW THIS ENDIF */
