/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

:- module(backward_compatibility,
	  [ '$arch'/2,
	    '$version'/1,
	    '$home'/1,
	    '$argv'/1,
	    display/1,
	    display/2,
	    displayq/1,
	    displayq/2,
	    (ed)/1,
	    concat/3,
	    atom_char/2,
	    read_variables/2,
	    read_variables/3,
	    feature/2,
	    set_feature/2
	  ]).

'$arch'(Arch, unknown) :-
	current_prolog_flag(arch, Arch).
'$version'(Version) :-
	current_prolog_flag(version, Version).
'$home'(Home) :-
	current_prolog_flag(home, Home).
'$argv'(Argv) :-
	current_prolog_flag(argv, Argv).

display(Term) :-
	write_term(Term, [ignore_ops(true)]).
display(Stream, Term) :-
	write_term(Stream, Term, [ignore_ops(true)]).

%	or write_canonical/[1,2]

displayq(Term) :-
	write_term(Term, [ignore_ops(true),quoted(true)]).
displayq(Stream, Term) :-
	write_term(Stream, Term, [ignore_ops(true),quoted(true)]).

%	use edit/1

ed(PredName) :-
	atom(PredName), !,
	edit(PredName/_Arity).
ed(Pred) :-
	edit(Pred).

%	concat/3 is superseeded by ISO atom_concat/3

concat(A, B, C) :-
	atom_concat(A, B, C).

%	Replaced by ISO read_term/[2,3].

read_variables(Term, Vars) :-
	read_term(Term, [variable_names(Vars)]).

read_variables(Stream, Term, Vars) :-
	read_term(Stream, Term, [variable_names(Vars)]).

%	feature(?Key, ?Value)
%	set_feature(+Key, @Term)
%
%	Replaced by ISO current_prolog_flag/2 and set_prolog_flag/2.

feature(Key, Value) :-
	current_prolog_flag(Key, Value).

set_feature(Key, Value) :-
	set_prolog_flag(Key, Value).

%	atom_char/2

atom_char(Char, Code) :-
	char_code(Char, Code).
