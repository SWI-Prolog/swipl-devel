/*  File:    shell.pl
    Purpose: Limited Unix Shell Emulation
    Author:  Jan Wielemaker
    Date:    Dec 15,  1994
*/

:- module(explain,
	  [ explain/1,
	    explain/2
	  ]).
:- use_module(library(helpidx)).

explain(Item) :-
	explain(Item, Explanation),
	write_ln(Explanation),
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
	proper_list(T), !,
	List = [H|T],
	length(List, L),
	(   utter(Explanation, '"~p" is a proper list with ~d elements',
	          [List, L])
	;   checklist(between(32, 127), List),
	    utter(Explanation, '~t~8|Text is "~s"',  [List])
	).
explain([H|T], Explanation) :- !,
	length([H|T], L), !,
	utter(Explanation, '"~p" is a not-closed list with ~d elements',
	      [[H|T], L]).
explain(Name/Arity, Explanation) :-
	atom(Name),
	integer(Arity), !,
	functor(Term, Name, Arity),
	explain_functor(Term, Explanation).
explain(Term, Explanation) :-
	utter(Explanation, '"~w" is a compound term', [Term]).
explain(Term, Explanation) :-
	explain_functor(Term, Explanation).
	
op_type(X, prefix) :-
	atom_chars(X, [0'f, _]).
op_type(X, infix) :-
	atom_chars(X, [_, 0'f, _]).
op_type(X, postfix) :-
	atom_chars(X, [_, 0'f]).

		/********************************
		*             ATOMS             *
		*********************************/

explain_atom(A, Explanation) :-
	current_predicate(A, Module:Head),
	(   Module == system
	->  true
	;   \+ predicate_property(Module:Head, imported_from(_))
	),
	explain_predicate(Module:Head, Explanation).
explain_atom(A, Explanation) :-
	referenced(A, Explanation).

		/********************************
		*            FUNCTOR             *
		*********************************/

explain_functor(Head, Explanation) :-
	current_predicate(_, Module:Head),
	\+ predicate_property(Module:Head, imported_from(_)),
	explain_predicate(Module:Head, Explanation).
explain_functor(Head, Explanation) :-
	referenced(Head, Explanation).
	
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

explain_predicate(Pred, Explanation) :-
	Pred = Module:Head,
	functor(Head, Name, Arity),
	
	U0 = '~w:~w/~d is a' - [Module, Name, Arity],
	findall(Fmt-Arg, (lproperty(Prop, Fmt, Arg),
			  predicate_property(Pred, Prop)),
		U1),
 	U2 = ' predicate' - [],
	findall(Fmt-Arg, (tproperty(Prop, Fmt, Arg),
			  predicate_property(Pred, Prop)),
		U3),
	flatten([U0, U1, U2, U3], Utters),
	combine_utterances(Utters, Explanation).
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
	functor(Head, Name, Arity),
	\+ (Name == '$user_query', Arity == 1),
	utter(Explanation, '~t~8|Referenced from ~d-th clause of ~w:~w/~d',
	                   [N, Module, Name, Arity]).

referenced(_Module:Head, Explanation) :-
	current_predicate(_, Module:Head),
	\+ predicate_property(Module:Head, built_in),
	\+ predicate_property(Module:Head, imported_from(_)),
	nth_clause(Module:Head, N, Ref),
	'$xr_member'(Ref, Head),
	functor(Head, Name, Arity),
	utter(Explanation,
	      '~t~8|Possibly Referenced from ~d-th clause of ~w:~w/~d',
	      [N, Module, Name, Arity]).

		/********************************
		*             UTTER            *
		*********************************/

utter(Explanation, Fmt, Args) :-
	sformat(Explanation, Fmt, Args).
