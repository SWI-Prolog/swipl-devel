/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The   library(explain)   describes   prolog-terms.   The   most   useful
functionality is its cross-referencing function.

Note  that  the  help-tool  for   XPCE    provides   a   nice  graphical
cross-referencer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(prolog_explain,
	  [ explain/1,
	    explain/2
	  ]).
:- use_module(library(helpidx)).
:- use_module(library(lists)).

explain(Item) :-
	explain(Item, Explanation),
	writeln(Explanation),
	fail.
explain(_).

		/********************************
		*           BASIC TYPES         *
		*********************************/

explain(Var, Explanation) :-
	var(Var), !,
	utter(Explanation, '"~w" is an unbound variable', [Var]).
explain(I, Explanation) :-
	integer(I), !,
	utter(Explanation, '"~w" is an integer', [I]).
explain(F, Explanation) :-
	float(F), !,
	utter(Explanation, '"~w" is a floating point number', [F]).
explain(S, Explanation) :-
	string(S), !,
	utter(Explanation, '"~w" is a string', S).
explain([], Explanation) :- !,
	utter(Explanation, '"[]" is an atom denoting an empty list', []).
explain(A, Explanation) :-
	atom(A),
	utter(Explanation, '"~w" is an atom', [A]).
explain(A, Explanation) :-
	current_op(Pri, F, A),
	op_type(F, Type),
	utter(Explanation, '"~w" is a ~w (~w) operator of priority ~d',
	      [A, Type, F, Pri]).
explain(A, Explanation) :-
	atom(A), !,
	explain_atom(A, Explanation).
explain([H|T], Explanation) :-
	is_list(T), !,
	List = [H|T],
	length(List, L),
	(   utter(Explanation, '"~p" is a proper list with ~d elements',
	          [List, L])
	;   maplist(printable, List),
	    utter(Explanation, '~t~8|Text is "~s"',  [List])
	).
explain([H|T], Explanation) :- !,
	length([H|T], L), !,
	utter(Explanation, '"~p" is a not-closed list with ~d elements',
	      [[H|T], L]).
explain(Name/Arity, Explanation) :-
	atom(Name),
	integer(Arity), !,
	functor(Head, Name, Arity),
	known_predicate(Module:Head),
	(   Module == system
	->  true
	;   \+ predicate_property(Module:Head, imported_from(_))
	),
	explain_predicate(Module:Head, Explanation).
explain(Module:Name/Arity, Explanation) :-
	atom(Module), atom(Name), integer(Arity), !,
	functor(Head, Name, Arity),
	explain_predicate(Module:Head, Explanation).
explain(Module:Head, Explanation) :-
	callable(Head), !,
	explain_predicate(Module:Head, Explanation).
explain(Term, Explanation) :-
	utter(Explanation, '"~w" is a compound term', [Term]).
explain(Term, Explanation) :-
	explain_functor(Term, Explanation).
	
%	known_predicate(:Head)
%	
%	Succeeds if we know anything about this predicate.  Undefined
%	predicates are considered `known' for this purpose, so we can
%	provide referenced messages on them.

known_predicate(Pred) :-
	current_predicate(_, Pred), !.
known_predicate(Pred) :-
	predicate_property(Pred, undefined).

op_type(X, prefix) :-
	atom_chars(X, [f, _]).
op_type(X, infix) :-
	atom_chars(X, [_, f, _]).
op_type(X, postfix) :-
	atom_chars(X, [_, f]).

printable(C) :-
	integer(C),
	between(32, 126, C).

		/********************************
		*             ATOMS             *
		*********************************/

explain_atom(A, Explanation) :-
	referenced(A, Explanation).
explain_atom(A, Explanation) :-
	current_predicate(A, Module:Head),
	(   Module == system
	->  true
	;   \+ predicate_property(Module:Head, imported_from(_))
	),
	explain_predicate(Module:Head, Explanation).
explain_atom(A, Explanation) :-
	predicate_property(Module:Head, undefined),
	functor(Head, A, _),
	explain_predicate(Module:Head, Explanation).


		/********************************
		*            FUNCTOR             *
		*********************************/

explain_functor(Head, Explanation) :-
	referenced(Head, Explanation).
explain_functor(Head, Explanation) :-
	current_predicate(_, Module:Head),
	\+ predicate_property(Module:Head, imported_from(_)),
	explain_predicate(Module:Head, Explanation).
explain_functor(Head, Explanation) :-
	predicate_property(M:Head, undefined),
	(   functor(Head, N, A),
	    utter(Explanation,
		  '~w:~w/~d is an undefined predicate', [M,N,A])
	;   referenced(M:Head, Explanation)
	).
	
	
		/********************************
		*           PREDICATE           *
		*********************************/

lproperty(built_in,	' built-in', []).
lproperty(dynamic,	' dynamic', []).
lproperty(multifile,	' multifile', []).
lproperty(transparent,	' meta', []).

tproperty(imported_from(Module), ' imported from module ~w', [Module]).
tproperty(file(File),		' defined in~n~t~8|~w', [File]).
tproperty(line_count(Number),	':~d', [Number]).

combine_utterances(Pairs, Explanation) :-
	maplist(first, Pairs, Fmts),
	concat_atom(Fmts, Format),
	maplist(second, Pairs, ArgList),
	flatten(ArgList, Args),
	utter(Explanation, Format, Args).

first(A-_B, A).
second(_A-B, B).

%	explain_predicate(+Module:+Head, -Explanation)

explain_predicate(Pred, Explanation) :-
	Pred = Module:Head,
	functor(Head, Name, Arity),
	
	(   predicate_property(Pred, undefined)
	->  utter(Explanation,
		  '~w:~w/~d is an undefined predicate', [Module,Name,Arity])
	;   U0 = '~w:~w/~d is a' - [Module, Name, Arity],
	    findall(Fmt-Arg, (lproperty(Prop, Fmt, Arg),
			      predicate_property(Pred, Prop)),
		    U1),
	    U2 = ' predicate' - [],
	    findall(Fmt-Arg, (tproperty(Prop, Fmt, Arg),
			      predicate_property(Pred, Prop)),
		    U3),
	    flatten([U0, U1, U2, U3], Utters),
	    combine_utterances(Utters, Explanation)
	).
explain_predicate(Pred, Explanation) :-
	predicate_property(Pred, built_in),
	Pred = _Module:Head,
	functor(Head, Name, Arity),
	predicate(Name, Arity, Summary, _, _),
	utter(Explanation, '~t~8|Summary: ``~w''''', [Summary]).
explain_predicate(Pred, Explanation) :-
	referenced(Pred, Explanation).
	
		/********************************
		*          REFERENCES           *
		*********************************/

referenced(Term, Explanation) :-
	current_predicate(_, Module:Head),
	\+ predicate_property(Module:Head, built_in),
	\+ predicate_property(Module:Head, imported_from(_)),
	Module:Head \= help_index:predicate(_,_,_,_,_),
	nth_clause(Module:Head, N, Ref),
	'$xr_member'(Ref, Term),
	utter_referenced(Module:Head, N, Ref,
			 'Referenced', Explanation).
referenced(_:Head, Explanation) :-
	current_predicate(_, Module:Head),
	\+ predicate_property(Module:Head, built_in),
	\+ predicate_property(Module:Head, imported_from(_)),
	nth_clause(Module:Head, N, Ref),
	'$xr_member'(Ref, Head),
	utter_referenced(Module:Head, N, Ref,
			 'Possibly referenced', Explanation).

utter_referenced(_Module:class(_,_,_,_,_,_), _, _, _, _) :-
	current_prolog_flag(xpce, true), !,
	fail.
utter_referenced(_Module:lazy_send_method(_,_,_), _, _, _, _) :-
	current_prolog_flag(xpce, true), !,
	fail.
utter_referenced(_Module:lazy_get_method(_,_,_), _, _, _, _) :-
	current_prolog_flag(xpce, true), !,
	fail.
utter_referenced(pce_xref:exported(_,_), _, _, _, _) :- !,
	fail.
utter_referenced(pce_xref:defined(_,_,_), _, _, _, _) :- !,
	fail.
utter_referenced(pce_xref:called(_,_,_), _, _, _, _) :- !,
	fail.
utter_referenced(pce_principal:send_implementation(_, _, _),
		 _, Ref, Text, Explanation) :-
	current_prolog_flag(xpce, true), !,
	xpce_method_id(Ref, Id),
	utter(Explanation, '~t~8|~w from ~w', [Text, Id]).
utter_referenced(pce_principal:get_implementation(Id, _, _, _),
		 _, Ref, Text, Explanation) :-
	current_prolog_flag(xpce, true), !,
	xpce_method_id(Ref, Id),
	utter(Explanation, '~t~8|~w from ~w', [Text, Id]).
utter_referenced(Module:Head, N, _Ref, Text, Explanation) :-
	functor(Head, Name, Arity),
	utter(Explanation,
	      '~t~8|~w from ~d-th clause of ~w:~w/~d',
	      [Text, N, Module, Name, Arity]).
	
xpce_method_id(Ref, Id) :-
	clause(Head, _Body, Ref),
	strip_module(Head, _, H),
	arg(1, H, Id).



		/********************************
		*             UTTER            *
		*********************************/

utter(Explanation, Fmt, Args) :-
	sformat(Explanation, Fmt, Args).


