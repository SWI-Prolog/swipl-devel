:- use_module(library(dialect/iso/iso_predicates)).

mkscript :-
	forall(iso_builtin_predicate(Head),
	       mkreplace(Head)).

mkreplace(Head) :-
	functor(Head, Name, 2),
	is_op(Name, infix), !,
	esc(Name, Esc),
	format('./replaceall "\\\\infixop{~w}" "\\infixop[ISO]{~w}"~n',
	       [Esc, Esc]).
mkreplace(Head) :-
	functor(Head, Name, 1),
	is_op(Name, prefix), !,
	esc(Name, Esc),
	format('./replaceall "\\\\prefixop{~w}" "\\prefixop[ISO]{~w}"~n',
	       [Esc, Esc]).
mkreplace(_) :- !.
mkreplace(Head) :-
	functor(Head, Name, Arity),
	format('./replaceall "\\\\predicate{~w}{~w}" "\\predicate[ISO]{~w}{~w}"~n',
	       [Name, Arity, Name, Arity]).

is_op(Functor, Type) :-
	current_op(_Pri, F, Functor),
	op_type(F, Type).

op_type(fx,  prefix).
op_type(fy,  prefix).
op_type(xf,  postfix).
op_type(yf,  postfix).
op_type(xfx, infix).
op_type(xfy, infix).
op_type(yfx, infix).
op_type(yfy, infix).

esc(In, Out) :-
	sub_atom(In, Pre, _, Post, \), !,
	sub_atom(In, 0, Pre, _, PreA),
	sub_atom(In, _, Post, 0, PostA),
	concat_atom([PreA, \\, PostA], Out).
esc(In, In).
