/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    See ../LICENCE to find out about your rights.
    jan@swi.psy.uva.nl

    Purpose: SWI-Prolog foreign language interface include file
*/

#ifndef PL_INCLUDED
#define PL_INCLUDED

#ifndef PLVERSION
#define PLVERSION "1.9.1 October 1994"
#endif

#if __GNUC__ && !__STRICT_ANSI__
#define constf const			/* const function */
#else
#define constf
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

int constf PL_is_var(const term);
int constf PL_is_int(const term);
int constf PL_is_atom(const term);
int constf PL_is_float(const term);
int constf PL_is_string(const term);
int constf PL_is_term(const term);

int	constf PL_type(const term);
long	constf PL_integer_value(const atomic);
double	       PL_float_value(const atomic);
char *	       PL_string_value(const atomic);
char *         PL_list_string_value(const term);
char *	constf PL_atom_value(const atomic);
functor constf PL_functor(const term);
atomic	constf PL_functor_name(const functor);
int	constf PL_functor_arity(const functor);
term	constf PL_arg(const term, int);
term	constf PL_strip_module(const term, module*);

		/********************************
		*         CONSTRUCTION          *
		*********************************/

term	PL_new_term(void);		/* create a new term (variable) */
atomic	PL_new_atom(char *);		/* create an atom from a char * */
atomic  PL_new_integer(int);		/* create a new integer */
atomic	PL_new_float(double);		/* create a new float */
#if O_STRING
atomic	PL_new_string(char *);		/* create a new string */
#endif /* O_STRING */
functor	PL_new_functor(atomic, int);	/* create a new functor */
int	PL_unify(term, term);		/* unify two terms */
int	PL_unify_atomic(term, atomic);  /* unify term with atomic value */
int	PL_unify_functor(term, functor);/* unify term with functor */

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

extern int	 		PL_foreign_control(long);
extern foreign_t		_PL_retry(long);
extern foreign_t		_PL_retry_address(void *);
extern long	 		PL_foreign_context(long);
extern void *	 		PL_foreign_context_address(long);

		/********************************
		*      REGISTERING FOREIGNS     *
		*********************************/

#define PL_FA_NOTRACE		(1)	/* foreign cannot be traced */
#define PL_FA_TRANSPARENT	(2)	/* foreign is module transparent */
#define PL_FA_NONDETERMINISTIC	(4)	/* foreign is non-deterministic */
#define PL_FA_GCSAVE		(8)	/* save to GC and/or shift stacks */

int	PL_register_foreign(char *, int, function, ...);

		/********************************
		*        CALLING PROLOG         *
		*********************************/

#define PL_lock(t)	_PL_lock(&(t));
#define PL_unlock(t)	_PL_unlock(&(t));

void	PL_mark(bktrk_buf *);		/* mark global and trail stack */
void	PL_bktrk(bktrk_buf *);		/* backtrack global stack to mark */
void	_PL_lock(term *);		/* lock term variable */
void	_PL_unlock(term *);		/* unlock term variable */

int	PL_call(term, module);		/* invoke term as Prolog goal */

		/********************************
		*            MODULES            *
		*********************************/

module	PL_context(void);		/* context module of predicate */
atomic	PL_module_name(module);		/* return name of a module */
module	PL_new_module(atomic);		/* return module from an atom */


		/********************************
		*         EVENT HANDLING	*
		********************************/

#define PL_DISPATCH_INPUT   0		/* There is input available */
#define PL_DISPATCH_TIMEOUT 1		/* Dispatch timeout */

extern int (*PL_dispatch_events)(void);	/* Dispatch user events */


		/********************************
		*      INITIALISATION HOOK	*
		********************************/

extern void (*PL_foreign_reinit_function)(int argc, char **argv);


		/********************************
		*            SIGNALS            *
		*********************************/

void (*PL_signal(int sig, void (*func)()))(); /* signal() replacement */


		/********************************
		*             ABORTS		*
		********************************/

void PL_abort_handle(void (*func)());	/* func called by pl_abort() */


		/********************************
		*           WARNINGS            *
		*********************************/

int	PL_warning(char *, ...);	/* Print standard Prolog warning */
void	PL_fatal_error(char *, ...);	/* Print warning and die */

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

int	PL_action(int, void *);		/* perform some action */
void	PL_on_halt(void (*)(int, void *), void *);

		/********************************
		*         QUERY PROLOG          *
		*********************************/

#define PL_QUERY_ARGC		1	/* return main() argc */
#define PL_QUERY_ARGV		2	/* return main() argv */
#define PL_QUERY_SYMBOLFILE	3	/* return current symbol file */
#define PL_QUERY_ORGSYMBOLFILE	4	/* symbol file before first load */
#define PL_QUERY_GETC		5	/* Read character from terminal */

long	PL_query(int);			/* get information from Prolog */

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

#endif /* PL_INCLUDED */
/* DO NOT WRITE BELOW THIS ENDIF */
