/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2011, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(varnumbers,
	  [ numbervars/1,			% +Term
	    varnumbers/2,			% +Term, -Copy
	    max_var_number/3,			% +Term, +Start, -Max
	    varnumbers/3			% +Term, +No, -Copy
	  ]).
:- use_module(library(error)).

/** <module> Utilities for numbered terms

This  library  provides  the  inverse   functionality  of  the  built-in
numbervars/3. Note that this library suffers  from the known issues that
'$VAR'(X) is a normal Prolog term and, -unlike the built-in numbervars-,
the inverse predicates do _not_  process   cyclic  terms.  The following
predicate is true for  any  acyclic   term  that  contains no '$VAR'(X),
integer(X) terms and no constraint variables:

  ==
  always_true(X) :-
	copy_term(X, X2),
	numbervars(X),
	varnumbers(X, Copy),
	Copy =@= X2.
  ==

@see	numbervars/4, =@=/2 (variant/2).
@compat	This library was introduced by Quintus and available in
	many related implementations, although not with exactly the
	same set of predicates.
*/

%%	numbervars(+Term) is det.
%
%	Number  variables  in  Term   using    $VAR(N).   Equivalent  to
%	numbervars(Term, 0, _).
%
%	@see numbervars/3, numbervars/4

numbervars(Term) :-
	numbervars(Term, 0, _).

%%	varnumbers(+Term, -Copy) is det.
%
%	Inverse  of  numbervars/1.  Equivalent  to  varnumbers(Term,  0,
%	Copy).

varnumbers(Term, Copy) :-
	varnumbers(Term, 0, Copy).

%%	varnumbers(+Term, +Start, -Copy) is det.
%
%	Inverse of numbervars/3. True when Copy is   a copy of Term with
%	all variables numbered >= Start   consistently replaced by fresh
%	variables. Variables in Term are _shared_  with Copy rather than
%	replaced by fresh variables.
%
%	@error domain_error(acyclic_term, Term) if Term is cyclic.
%	@compat Quintus, SICStus.  Not in YAP version of this library

varnumbers(Term, Min, Copy) :-
	must_be(acyclic, Term),
	MaxStart is Min-1,
	max_var_number(Term, MaxStart, Max),
	NVars is Max-MaxStart,
	(   NVars == 0
	->  Copy = Term
	;   roundup_next_power_two(NVars, Len),
	    functor(Vars, v, Len),
	    varnumbers(Term, MaxStart, Vars, Copy)
	).


varnumbers(Var, _, _, Copy) :-
	var(Var), !,
	Copy = Var.
varnumbers(Var, _, _, Copy) :-
	atomic(Var), !,
	Copy = Var.
varnumbers('$VAR'(I), Min, Vars, Copy) :-
	integer(I),
	I > Min, !,
	Index is I-Min,
	arg(Index, Vars, Copy).
varnumbers(Term, Min, Vars, Copy) :-
	functor(Term, Name, Arity),
	functor(Copy, Name, Arity),
	varnumbers_args(1, Arity, Term, Min, Vars, Copy).

varnumbers_args(I, Arity, Term, Min, Vars, Copy) :-
	I =< Arity, !,
	arg(I, Term, AT),
	arg(I, Copy, CT),
	varnumbers(AT, Min, Vars, CT),
	I2 is I + 1,
	varnumbers_args(I2, Arity, Term, Min, Vars, Copy).
varnumbers_args(_, _, _, _, _, _).


%%	roundup_next_power_two(+Int, -NextPower) is det.
%
%	NextPower is I**2, such that NextPower >= Int.

roundup_next_power_two(1, 1) :- !.
roundup_next_power_two(N, L) :-
	L is 1<<(msb(N-1)+1).

%%	max_var_number(+Term, +Start, -Max) is det.
%
%	True when Max is the  max  of   Start  and  the highest numbered
%	$VAR(N) term.
%
%	@author Vitor Santos Costa
%	@compat YAP

max_var_number(V, Max, Max) :-
	var(V), !.
max_var_number('$VAR'(I), Max0, Max) :-
	integer(I), !,
        Max is max(I,Max0).
max_var_number(S, Max0, Max) :-
        functor(S, _, Ar),
        max_var_numberl(Ar, S, Max0, Max).

max_var_numberl(0, _, Max, Max) :- !.
max_var_numberl(I, T, Max0, Max) :-
	arg(I, T, Arg),
	I2 is I-1,
	max_var_number(Arg, Max0, Max1),
	max_var_numberl(I2, T, Max1, Max).
