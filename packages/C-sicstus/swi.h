/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

#ifndef SWI_H_INCLUDED
#define SWI_H_INCLUDED
#include <sicstus/sicstus.h>

#ifndef _PL_INLINE
#define _PL_INLINE extern __inline
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines an emulation layer to handle SWI-Prolog defined
foreign code without conversion on SICStus.

Status:
=======

	# Tested
	Only very partially
	
	# Restrictions
		+ No context module handling

		+ No non-debugging call-back support

		+ Small differences in exception handling

		+ Poor stream emulation
		This is the hardest bit.  We need a record oriented model
		for SWI-Prolog, where SICStus provides a character oriented
		model.

		Actually the emulated IOSTREAM should be an SP_stream,
		so we can deal with SICStus streams.  But then, there is
		a lot more high-level functionality to SWI-Prolog
		streams, which we would have to emulate on top of the
		SICStus streams.

		For now, we can create a stream and pass it to Prolog,
		or play with it in C.  Don't do both with the same
		stream!

		+ Foreign language interface does not allow for
		defining non-deterministic predicates.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	       TYPES		*
		 *******************************/

typedef SP_term_ref			   term_t;
typedef unsigned long 			   atom_t;
typedef atom_t				   module_t;
typedef void *				   record_t;
typedef int				   foreign_t;

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

typedef struct _predicate_t
{ SP_pred_ref   predicate;
  atom_t	name;
  int		arity;
  module_t	module;
  struct _predicate_t *next;
} predicate_struct, *predicate_t;

typedef struct _functor_t
{ atom_t name;
  int arity;
  struct _functor_t *next;
} functor_struct, *functor_t;

typedef struct _qid_t
{ SP_qid	query;
  int		flags;
} qid_struct, *qid_t;

typedef SP_term_ref			   fid_t;

		 /*******************************
		 *	     CONSTANTS		*
		 *******************************/

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define	PL_VARIABLE SP_TYPE_VARIABLE
#define	PL_INTEGER  SP_TYPE_INTEGER
#define	PL_FLOAT    SP_TYPE_FLOAT
#define	PL_ATOM	    SP_TYPE_ATOM
#define	PL_TERM	    SP_TYPE_COMPOUND 
#define PL_POINTER  6			/* switch will warn on problems */
#define PL_CHARS    7
#define PL_FUNCTOR  8
#define PL_LIST     9

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

#define PL_Q_NORMAL		0x02	/* normal usage */
#define PL_Q_NODEBUG		0x04	/* use this one */
#define PL_Q_CATCH_EXCEPTION	0x08	/* handle exceptions in C */
#define PL_Q_PASS_EXCEPTION	0x10	/* pass to parent environment */
#define _PL_Q_PENDING_EXCEPTION	0x40	/* for emulator */
#define _PL_Q_UNDEFINED_PRED	0x80	/* for emulator */

		 /*******************************
		 *	      CONTROL		*
		 *******************************/

#define PL_succeed	return(TRUE)
#define PL_fail		return(FALSE)


		 /*******************************
		 *      ATOMS AND FUNCTORS	*
		 *******************************/

_PL_INLINE atom_t
PL_new_atom(const char *s)
{ atom_t a = SP_atom_from_string((char *)s);
  SP_register_atom(a);

  return a;
}

#define PL_atom_chars(a)	SP_string_from_atom(a)

extern functor_t PL_new_functor(atom_t a, int arity);
#define PL_functor_name(f)	((f)->name)
#define PL_functor_arity(f)	((f)->arity)

#define _ATOM_user _SP_atom_user()

		 /*******************************
		 *	  TERM_T HANDLING	*
		 *******************************/

#define PL_reset_term_refs(n)	SP_reset_term_refs(n)

_PL_INLINE term_t
PL_new_term_ref()
{ term_t t = SP_new_term_ref();

  SP_put_variable(t);

  return t;
}


_PL_INLINE term_t
PL_new_term_refs(int n)
{ term_t t0 = SP_new_term_refs(n);
  int i;

  for(i=0; i<n; i++)
    SP_put_variable(t0+i);

  return t0;
}


_PL_INLINE term_t
PL_copy_term_ref(term_t in)
{ term_t out = SP_new_term_ref();

  SP_put_term(out, in);
  return out;
}

		 /*******************************
		 *	    TYPE TESTING	*
		 *******************************/

#define PL_term_type(t)		SP_term_type(t)
#define PL_is_atom(t)		SP_is_atom(t)
#define PL_is_integer(t)	SP_is_integer(t)
#define PL_is_variable(t)	SP_is_variable(t)
#define PL_is_float(t)		SP_is_float(t)
#define PL_is_compound(t)	SP_is_compound(t)
#define PL_is_list(t)		SP_is_list(t)
#define PL_is_atomic(t)		SP_is_atomic(t)
#define PL_is_number(t)		SP_is_number(t)

_PL_INLINE int
PL_is_functor(term_t t, functor_t f)
{ atom_t name;
  int arity;

  if ( SP_get_functor(t, &name, &arity) &&
       f->name == name && f->arity == arity )
    return TRUE;

  return FALSE;
}

		 /*******************************
		 *	  ANALYSING TERMS	*
		 *******************************/

#define PL_get_atom(t, a)		SP_get_atom(t, a)
#define PL_get_atom_chars(t, s)		SP_get_string(t, s)
extern int PL_get_chars(term_t t, char **s, unsigned flags);
extern int PL_get_list_chars(term_t t, char **s, unsigned flags);
extern int PL_get_long(term_t t, long *v);
extern int PL_get_integer(term_t t, int *v);
#define PL_get_pointer(t, ptr)		SP_get_pointer(t, ptr)
#define PL_get_float(t, f)		SP_get_float(t, f)
extern int PL_get_functor(term_t t, functor_t *f);
#define PL_get_name_arity(t, n, a)	SP_get_functor(t, n, a)
#define PL_get_module(t, m)		SP_get_atom(t, m)
#define PL_get_arg(n, t, a)		SP_get_arg(n, t, a)
#define _PL_get_arg(n, t, a)		SP_get_arg(n, t, a)
#define PL_get_list(l, h, t)		SP_get_list(l, h, t)
extern int PL_get_head(term_t l, term_t h);
extern int PL_get_tail(term_t l, term_t t);
extern int PL_get_nil(term_t l);
extern int PL_get_term_value(term_t t, term_value_t *v);

		 /*******************************
		 *	 CONSTRUCTING TERMS	*
		 *******************************/

#define PL_put_variable(in)		(void)SP_put_variable(in)
#define PL_put_atom(in, a)		(void)SP_put_atom(in, a)
#define PL_put_atom_chars(in, s)	(void)SP_put_string(in, s)
#define PL_put_list_chars(l, s) 	(void)SP_put_list_chars(l, s)
#define PL_put_integer(l, s) 		(void)SP_put_integer(l, s)
extern void PL_put_pointer(term_t t, void *ptr);
#define PL_put_float(t, f) 		(void)SP_put_integer(t, f)

_PL_INLINE void
PL_put_functor(term_t in, functor_t f)
{ SP_put_functor(in, f->name, f->arity);
}

#define PL_put_list(in)			(void)SP_put_list(in)
extern void PL_put_nil(term_t in);
#define PL_put_term(to, from)		(void)SP_put_term(to, from)
extern void PL_cons_functor(term_t t, functor_t f, ...);
extern void PL_cons_functor_v(term_t h, functor_t fd, term_t a0);
#define PL_cons_list(l, h, t)		(void)SP_cons_list(l, h, t)


		 /*******************************
		 *	  UNIFYING DATA		*
		 *******************************/

#define PL_unify(a, b)			SP_unify(a, b)
extern int PL_unify_atom(term_t t, atom_t a);
extern int PL_unify_atom_chars(term_t t, const char *chars);
extern int PL_unify_list_chars(term_t r, const char *chars);
extern int PL_unify_integer(term_t t, long i);
extern int PL_unify_float(term_t t, double i);
extern int PL_unify_functor(term_t t, functor_t f);
extern int PL_unify_pointer(term_t t, void *ptr);
extern int PL_unify_list(term_t lx, term_t h, term_t t);
extern int PL_unify_nil(term_t t);
extern int PL_unify_arg(int index, term_t t, term_t a);
extern int PL_unify_term(term_t t, ...);


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

extern predicate_t PL_predicate(const char *name, int arity, const char *m);
extern predicate_t PL_pred(functor_t f, module_t m);
extern void	   PL_predicate_info(predicate_t p,
				     atom_t *n, int *a, module_t *m);


		 /*******************************
		 *	      CALLING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This emulation is not complete:

	* PL_discard_foreign_frame()
	This is a dummy

	* flags
	Ignores PL_Q_NODEBUG, PL_Q_CATCH_EXCEPTION

	* ctx
	Context module is ignored.  There will only be very few cases
	where this makes a difference though.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

extern qid_t PL_open_query(module_t ctx, int flags,
			   predicate_t p, term_t t0);
extern int   PL_next_solution(qid_t qid);
extern void  PL_close_query(qid_t qid);
extern void  PL_cut_query(qid_t qid);
extern int   PL_call(term_t goal, module_t context);
extern int   PL_call_predicate(module_t m, int flags,
			       predicate_t p, term_t t0);

#define PL_open_foreign_frame() SP_new_term_ref()
#define PL_close_foreign_frame(f) SP_reset_term_refs(f)
#define PL_discard_foreign_frame(f) SP_reset_term_refs(f)

		 /*******************************
		 *	     EXCEPTIONS		*
		 *******************************/

#define PL_raise_exception(t)	SP_raise_exception(t)
extern term_t PL_exception(qid_t qid);


		 /*******************************
		 *	       MODULES		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Modules are just represented as  atoms   in  the  SICStus interface. The
notion of context-module  is  absent,  but   generally  useless  in  the
presence of the meta_predicate mechanism.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

extern int	PL_strip_module(term_t raw, module_t *m, term_t plain);
#define PL_context() _ATOM_user
#define PL_module_name(m) (m)
#define PL_new_module(m) (m)


		 /*******************************
		 *	      RECORDS  		*
		 *******************************/

extern record_t PL_record(term_t t);
extern void	PL_recorded(record_t record, term_t t);
extern void	PL_erase(record_t record);

#ifdef SIO_MAGIC			/* defined from <SWI-Stream.h> */
		 /*******************************
		 *	  STREAM SUPPORT	*
		 *******************************/

					/* Make IOSTREAM known to Prolog */
extern int PL_open_stream(term_t t, IOSTREAM *s);
extern int PL_get_stream_handle(term_t t, IOSTREAM **s);
extern IOSTREAM *PL_open_resource(module_t m,
				  const char *name,
				  const char *rc_class,
				  const char *mode);

#define PL_WRT_QUOTED		0x1	/* quote atoms */
#define PL_WRT_IGNOREOPS	0x2	/* ignore list/operators */
#define PL_WRT_NUMBERVARS	0x4	/* print $VAR(N) as a variable */
#define PL_WRT_PORTRAY		0x8	/* call portray */

extern int PL_write_term(IOSTREAM *s,
			 term_t term,
			 int precedence,
			 int flags);
#endif

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
#define PL_ACTION_GUIAPP	10	/* Win32: set wether this is a gui */

extern int  PL_action(int, ...);	/* perform some action */
extern void PL_on_halt(void (*)(int, void *), void *);

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

extern long PL_query(int);		/* get information from Prolog */

		 /*******************************
		 *	      SIGNALS		*
		 *******************************/

#define PL_signal(sig, func)		SP_signal(sig, func)
#define PL_handle_signals()		/* dummy */


		 /*******************************
		 *	       HOOKS		*
		 *******************************/

#define PL_DISPATCH_NOWAIT    0		/* Dispatch only once */
#define PL_DISPATCH_WAIT      1		/* Dispatch till input available */
#define PL_DISPATCH_INSTALLED 2		/* dispatch function installed? */

#define PL_DISPATCH_INPUT   0		/* There is input available */
#define PL_DISPATCH_TIMEOUT 1		/* Dispatch timeout */

typedef int  (*PL_dispatch_hook_t)(int fd);
typedef void (*PL_abort_hook_t)(void);

extern int 		  PL_dispatch(int fd, int wait);
extern PL_dispatch_hook_t PL_dispatch_hook(PL_dispatch_hook_t);
extern void	          PL_abort_hook(PL_abort_hook_t);
extern int		  PL_abort_unhook(PL_abort_hook_t);

		 /*******************************
		 *	        MISC		*
		 *******************************/

extern int	PL_warning(const char *fmt, ...);

#endif /*SWI_H_INCLUDED*/

