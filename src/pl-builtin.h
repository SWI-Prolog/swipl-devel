/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2008, University of Amsterdam

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

#ifndef PL_BUILTIN_H_INCLUDED
#define PL_BUILTIN_H_INCLUDED

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This header provides definition that allow us compiling the C files that
define built-in predicates, disclosing as little   as  possible from the
SWI-Prolog kernel. The aim of this seperate   module is to allow reusing
these files in other Prolog systems with  minimal effort by providing an
implementation of this macro layer  that   satisfies  the  target Prolog
system.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *   NON-DET PREDICATE CONTEXT	*
		 *******************************/

typedef enum
{ FRG_FIRST_CALL = 0,		/* Initial call */
  FRG_CUTTED     = 1,		/* Context was cutted */
  FRG_REDO	 = 2		/* Normal redo */
} frg_code;

typedef struct foreign_context
{ uintptr_t		context;	/* context value */
  frg_code		control;	/* FRG_* action */
  struct PL_local_data *engine;		/* invoking engine */
} *control_t;

#define FRG_REDO_MASK	0x03
#define FRG_REDO_BITS	2
#define REDO_INT	0x02		/* Returned an integer */
#define REDO_PTR	0x03		/* returned a pointer */

#define ForeignRedoIntVal(v)	(((uintptr_t)(v)<<FRG_REDO_BITS)|REDO_INT)
#define ForeignRedoPtrVal(v)	(((uintptr_t)(v))|REDO_PTR)

#define ForeignRedoInt(v)	return ForeignRedoIntVal(v)
#define ForeignRedoPtr(v)	return ForeignRedoPtrVal(v)

#define ForeignControl(h)	((h)->control)
#define ForeignContextInt(h)	((intptr_t)(h)->context)
#define ForeignContextPtr(h)	((void *)(h)->context)
#define ForeignEngine(h)	((h)->engine)


		 /*******************************
		 *	   BIND PREDICATES	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Built-in predicates are defined as

PRED_IMPL("name", arity, c_symbol, flags)
{ <implementation body>
}

The macros A1..A10 provide access to the arguments (of type term_t). The
CTX_*  macros  provide  the  actual  arity  and  context  arguments  for
non-deterministic predicates. The implementation returns   using  one of
these  constructs.  The  `Redo'   variations    are   only   allowed  if
PL_FA_NONDETERMINISTIC is present in `flags'.

    * return FALSE
    * return TRUE
    * ForeignRedoInt(intptr_t val)
    * ForeignRedoPtr(void *ptr)

At the end of the file there is   a  section that looks like this, where
each PRED_DEF line  is  a  simple   direct  copy  of  the  corresponding
PRED_IMPL declaration:

BeginPredDefs(module)
  PRED_DEF("name", arity, c_symbol, flags)
  ...
EndPredDefs
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define PRED_IMPL(name, arity, fname, flags) \
        foreign_t \
        pl_ ## fname ## arity ## _va(term_t PL__t0, int PL__ac, \
				     control_t PL__ctx)

#define Arg(N)  (PL__t0+((n)-1))
#define A1      (PL__t0)
#define A2      (PL__t0+1)
#define A3      (PL__t0+2)
#define A3      (PL__t0+2)
#define A4      (PL__t0+3)
#define A5      (PL__t0+4)
#define A6      (PL__t0+5)
#define A7      (PL__t0+6)
#define A8      (PL__t0+7)
#define A9      (PL__t0+8)
#define A10     (PL__t0+9)

#define CTX_CNTRL ForeignControl(PL__ctx)
#define CTX_PTR   ForeignContextPtr(PL__ctx)
#define CTX_INT   ForeignContextInt(PL__ctx)
#define CTX_ARITY PL__ac

#define BeginPredDefs(id) \
        PL_extension PL_predicates_from_ ## id[] = {
#define PRED_DEF(name, arity, fname, flags) \
        { name, arity, pl_ ## fname ## arity ## _va, (flags)|PL_FA_VARARGS },
#define EndPredDefs \
        { NULL, 0, NULL, 0 } \
        };

#endif /*PL_BUILTIN_H_INCLUDED*/
