/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: listing/1
*/

:- module(prolog_listing,
	[ listing/0,
	  listing/1,
	  portray_clause/1
	]).

:- module_transparent
	listing/0, 
	listing/1, 
	list_predicates/2, 
	list_predicate/3, 
	list_clauses/1.

:- system_mode(on).

%   calls listing(Pred) for each current_predicate Pred.

listing :-
	current_predicate(_, Pred), 
	\+ predicate_property(Pred, built_in), 
	nl, 
	functor(Pred, Name, Arity),
	list_predicate(Name, Pred, Arity),
	fail.
listing.


%   listing(PredSpecs)

listing(V) :-
	var(V), !,       % ignore variables
	throw(error(instantiation_error, _)).
listing([]) :- !.
listing([X|Rest]) :- !, 
        listing(X), 
        listing(Rest).
listing(X) :-
	'$find_predicate'(X, Preds), 
	list_predicates(Preds, X).

list_predicates(Preds, _) :-
	member(Pred, Preds),
	nl, 
	'$define_predicate'(Pred),
	'$strip_module'(Pred, _, Head), 
	functor(Head, Name, Arity), 
        list_predicate(Name, Pred, Arity), 
        fail.
list_predicates(_, _).

list_predicate(Name, Pred, Arity) :-
	predicate_property(Pred, undefined), !, 
	format('%   Undefined: ~w/~w~n', [Name, Arity]).
list_predicate(Name, Pred, Arity) :-
	predicate_property(Pred, foreign), !, 
	format('%   Foreign: ~w/~w~n', [Name, Arity]).
list_predicate(Name, Pred, Arity) :-
	'$strip_module'(Pred, Module, Head),
	notify_changed(Module, Head),
	list_declarations(Name, Pred, Arity, []), 
	list_clauses(Pred).

list_declarations(Name, Pred, Arity, Sofar) :-
	\+ member((dynamic Name/Arity), Sofar), 
	predicate_property(Pred, (dynamic)), !, 
	list_declarations(Name, Pred, Arity, [(dynamic Name/Arity)|Sofar]).
list_declarations(Name, Pred, Arity, Sofar) :-
	\+ member((multifile Name/Arity), Sofar), 
	predicate_property(Pred, (multifile)), !, 
	list_declarations(Name, Pred, Arity, [(multifile Name/Arity)|Sofar]).
list_declarations(Name, Pred, Arity, Sofar) :-
	\+ member((module_transparent Name/Arity), Sofar), 
	predicate_property(Pred, (transparent)), !, 
	list_declarations(Name, Pred, Arity, [(module_transparent Name/Arity)|Sofar]).
list_declarations(_, _, _, []) :- !.
list_declarations(_, _, _, List) :-
	write_declarations(List), nl.

write_declarations([]) :- !.
write_declarations([H|T]) :-
	format(':- ~q.~n', [H]),
	write_declarations(T).

list_clauses(Pred) :-
	context_module(Source), 
	'$strip_module'(Pred, Module, Head), 
	clause(Pred, Body), 
	    write_module(Module, Source), 
	    portray_clause((Head:-Body)), 
	fail.

write_module(system, _) :- !.
write_module(Module, Module) :- !.
write_module(Module, _) :-
	format('~q:', [Module]).

notify_changed(user, Head) :-
	'$c_current_predicate'(_, system:Head),
	\+ ( predicate_property(user:Head, imported_from(System)),
	     (System == system ; '$default_module'(System, system, system))
	   ),
	\+ predicate_property(system:Head, (dynamic)), !,
	functor(Head, Name, Arity),
	format('%   NOTE: system definition has been overruled for ~w/~w~n~n',
				[Name, Arity]).
notify_changed(_, _).

%	portray_clause(+Clause)
%	Portray `Clause' on the current output stream.   Layout  of  the
%	clause  is  to our best standards.  As the actual variable names
%	are not available we use A, B, ... Deals with ';', '|',  '->'  and
%	various calls via meta-call predicates.

portray_clause(Term) :-
	numbervars(Term, '$VAR', 0, _), 
	do_portray_clause(Term), 
	fail.					% undo bindings		
portray_clause(_).

do_portray_clause((Head :- true)) :- !, 
	portray_head(Head), 
	put(0'.), nl.
do_portray_clause((Head :- Body)) :- !, 
	portray_head(Head), 
	write(' :-'), 
	portray_body(Body, 2, indent), 
	put(0'.), nl.
do_portray_clause(Fact) :-
	do_portray_clause((Fact :- true)).

portray_head(Head) :-
	pprint(Head).

portray_body(!, _, _) :- !, 
	write(' !').
portray_body((!, Clause), Indent, _) :- !, 
	write(' !,'), 
	portray_body(Clause, Indent, indent).
portray_body(Term, Indent, indent) :- !, 
	nl, portray_indent(Indent), 
	portray_body(Term, Indent, noindent).
portray_body((A, B), Indent, _) :- !, 
	portray_body(A, Indent, noindent), 
	write(','), 
	portray_body(B, Indent, indent).
portray_body(Or, Indent, _) :-
	memberchk(Or, [(_;_), (_|_), (_->_), (_*->_)]), !, 
	write('(   '), 
	portray_or(Or, Indent), 
	nl, portray_indent(Indent), 
	write(')').
portray_body(Meta, Indent, _) :-
	meta_call(Meta, N), !, 
	portray_meta(Meta, N, Indent).
portray_body(Clause, _, _) :-
	pprint(Clause).

portray_or((If -> Then ; Else), Indent) :- !, 
	succ(Indent, NestIndent), 
	portray_body(If, NestIndent, noindent), 	
	nl, portray_indent(Indent),
	write('->  '), 
	portray_body(Then, NestIndent, noindent), 
	nl, portray_indent(Indent), 
	write(';   '), 
	portray_or(Else, Indent).
portray_or((If *-> Then ; Else), Indent) :- !, 
	succ(Indent, NestIndent), 
	portray_body(If, NestIndent, noindent), 	
	nl, portray_indent(Indent),
	write('*-> '), 
	portray_body(Then, NestIndent, noindent), 
	nl, portray_indent(Indent), 
	write(';   '), 
	portray_or(Else, Indent).
portray_or((If -> Then), Indent) :- !, 
	succ(Indent, NestIndent), 
	portray_body(If, NestIndent, noindent), 	
	nl, portray_indent(Indent), 
	write('->  '), 
	portray_or(Then, Indent).
portray_or((A;B), Indent) :- !, 
	succ(Indent, OrIndent), 
	portray_body(A, OrIndent, noindent), 
	nl, portray_indent(Indent), 
	write(';   '), 
	portray_or(B, Indent).
portray_or((A|B), Indent) :- !, 
	succ(Indent, OrIndent), 
	portray_body(A, OrIndent, noindent), 	
	nl, portray_indent(Indent), 
	write('|   '), 
	portray_or(B, Indent).
portray_or(A, Indent) :-
	succ(Indent, OrIndent), 
	portray_body(A, OrIndent, noindent).

meta_call(call(_), 1).
meta_call(once(_), 1).
meta_call(not(_), 1).
meta_call(\+(_), 1).
meta_call(ignore(_), 1).

portray_meta(Term, N, Indent) :-
	arg(N, Term, Arg), 
	memberchk(Arg, [(_, _), (_;_), (_->_), (_*->_)]), !, 
	functor(Term, Name, _), 
	write(Name), write('(('), 
	succ(Indent, CallIndent), 
	portray_body(Arg, CallIndent, indent), 
	nl, portray_indent(CallIndent), 
	write('))').	
portray_meta(Term, _, _) :-
	pprint(Term).	

portray_indent(N) :-
	Tab is N // 2, 
	Space is (N mod 2) * 4, 
	put_tabs(Tab),
	tab(Space).

put_tabs(N) :-
	N > 0, !,
	put(9),
	NN is N - 1,
	put_tabs(NN).
put_tabs(_).

pprint(Term) :-
	writeq(Term).
