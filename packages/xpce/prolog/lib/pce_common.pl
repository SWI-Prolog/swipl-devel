/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(pce_common,
	  [ file_exists/1
	  , concat/3
	  , gensym/2
	  , concat_atom/2
	  , checklist/2
	  , maplist/3
	  , sublist/3
	  , forall/2
	  , call/2
	  , call/3
	  , shell/1
	  , strip_module/3
	  , pce_error/2
	  , source_location/2
	  , ignore/1
	  , nth1/3
	  , between/3
	  , free_variables/2
	  , term_to_atom/2
	  ]).

:- meta_predicate
	maplist(:, ?, ?),
	checklist(:, +),
	sublist(:, +, +),
	call(:, +),
	call(:, +, +),
	forall(:, :),
	ignore(:).

:- require([ append/3
	   , nth/3
	   , format_to_chars/3
	   , atom_to_chars/3
	   , read_from_chars/2
	   ]).

%	forall(+Generator, +Goal)
%	Suceeds if Goal can be proven for all solutions of Generator

forall(Generator, Goal) :-
	 \+((
            Generator,
            \+ Goal
            )).


%	checklist(+Goal, +List)
%	Check goal applies to all members of list

checklist(_, []).
checklist(Goal, [H|T]) :-
	call(Goal, H),
	checklist(Goal, T).

%	maplist(+Goal, ?List1, ?List2)

maplist(_, [], []).
maplist(Goal, [H1|T1], [H2|T2]) :-
	call(Goal, H1, H2),
	maplist(Goal, T1, T2).

%	sublist(+Goal, +List1, ?List2)
%	Succeeds if List2 unifies with a list holding those terms for wich
%	apply(Goal, Elem) succeeds.

sublist(_, [], []) :- !.
sublist(Goal, [H|T], Sub) :-
	call(Goal, H), !, 
	Sub = [H|R], 
	sublist(Goal, T, R).
sublist(Goal, [_|T], R) :-
	sublist(Goal, T, R).


%	call(+Template, +Arg, [Arg2])
%	Append Arg to template and call

call(Template, Arg) :-
	strip_module(Template, Module, Term),
	Term =.. List,
	append(List, [Arg], NewList),
	NewTerm =.. NewList,
	Module:NewTerm.


call(Template, Arg1, Arg2) :-
	strip_module(Template, Module, Term),
	Term =.. List,
	append(List, [Arg1, Arg2], NewList),
	NewTerm =.. NewList,
	Module:NewTerm.


%	ignore(+Goal)
%	Call goal once, succeed always

ignore(Goal) :-
	Goal, !.
ignore(_).


%	file_exists(+Path)
%	Test existence of a file.

file_exists(File) :-
	unix(access(File, 0)).

%	gensym(+Atom, -Unique)
%	Create a unique version of an atom

:- dynamic
	gensym_current/2.

gensym(Atom, Unique) :-
	gensym_current(Atom, Nr), !,
	Next is Nr + 1,
	retractall(gensym_current(Atom, _)),
	asserta(gensym_current(Atom, Next)),
	concat(Atom, Next, Unique).
gensym(Atom, Unique) :-
	asserta(gensym_current(Atom, 1)),
	concat(Atom, 1, Unique).


%	concat_atom(+ListOfAtoms, -Atom)
%	Concatenate list of atoms.

concat_atom([A,B], Atom) :- !,
	concat(A, B, Atom).
concat_atom(List, Atom) :-
	atoms_to_string(List, "", String),
	name(Atom, String).

atoms_to_string([], L, L).
atoms_to_string([H|T], Sofar, String) :-
	name(H, S),
	append(Sofar, S, NewSofar),
	atoms_to_string(T, NewSofar, String).


%	concat(?Atom1, ?Atom2, ?Atom3)
%	Logical concatenation of two atomics

concat(A, B, C) :-
	atomic(A), atomic(B), !,
	name(A, S0),
	name(B, S1),
	append(S0, S1, S),
	name(C, S).
concat(A, B, C) :-
	atomic(A), atomic(C), !,
	name(A, S0),
	name(C, S),
	append(S0, S1, S),
	name(B, S1).
concat(A, B, C) :-
	atomic(B), atomic(C), !,
	name(B, S1),
	name(C, S),
	append(S0, S1, S),
	name(A, S0).

%	strip_module(+RawTerm, -Term, -Module).
%	If a term is of the form Module:Term, strip of the module part,
%	return the plain term in Term and the Module in Module.

strip_module(RT, M, T) :-
	strip_module(RT, T, M, user).

strip_module(Module:RT2, T, M, _) :-
	atom(Module), !,
	strip_module(RT2, T, M, Module).
strip_module(T, T, M, M).


%	shell(+Cmd)

shell(Cmd) :-
	unix(system(Cmd)).

		/********************************
		*            MESSAGES		*
		********************************/

%	pce_error(Fmt, Args)
%	Provide (prolog-part) PCE interface error message

pce_error(Fmt, Args) :-
	format(user_error, '[PCE/Prolog: ', []),
	format(user_error, Fmt, Args),
	format(user_error, ']~n', []).


		/********************************
		*         SOURCE_LOCATION	*
		********************************/

%	source_location(-Path, -LineNo)
%	Unify Path and LineNo with the filename and line number of the
%	location where the last term has been read.  Used inside 
%	term_expansion.

source_location(Path, Line) :-
	current_input(Stream),
	current_stream(File, _, Stream),
	absolute_file_name(File, Path),
	line_count(Stream, Line).


		/********************************
		*             BETWEEN		*
		********************************/

%	Copied from the dec10 library; written by R.A. O'Keefe

between(L, U, N) :-
        nonvar(N),
        !,
        integer(L), integer(U), integer(N),
        L =< N, N =< U.
between(L, U, N) :-
        integer(L), integer(U), L =< U,
        between1(L, U, N).


between1(L, _, L).
between1(L, U, N) :-
        L < U,
        M is L+1,
        between1(M, U, N).

		/********************************
		*             LISTS		*
		********************************/

%	nth1(?Index, ?List, ?Elem)
%	Is true when Elem is the Index'th element of List. Counting starts
%	at 1.

nth1(Index, List, Elem) :-
	nth(Index, List, Elem).


		/********************************
		*        FREE-VARIABLES		*
		********************************/

%	free_variables(+Term, -ListOfUnBound)
%	Unify `ListOfUnBound' with a list holding all unbound variables
%	in term.

free_variables(Term, List) :-
	free_variables_(Term, List),
	close_list(List).

free_variables_(Term, List) :-
	var(Term), !,
	add_unbound(Term, List).
free_variables_(Term, _) :-
	atomic(Term), !.
free_variables_(Term, List) :-
	functor(Term, _, Arity), !,
	End is Arity + 1,
	free_variables_(1, End, Term, List).

free_variables_(End, End, _, _) :- !.
free_variables_(I, End, Term, List) :-
	arg(I, Term, Arg), !,
	free_variables_(Arg, List),
	NI is I + 1,
	free_variables_(NI, End, Term, List).

add_unbound(Var, VT) :-
	var(VT), !,
	VT = [Var|_].
add_unbound(Var, [H|_]) :-
	Var == H, !.
add_unbound(Var, [_|T]) :-
	add_unbound(Var, T).
	

close_list(Var) :-
	var(Var), !,
	Var = [].
close_list([_|T]) :-
	close_list(T).


		/********************************
		*         TERM_TO_ATOM		*
		********************************/

%	term_to_atom(?Term, ?Atom)

term_to_atom(Term, Atom) :-
	atom(Atom), !,
	atom_to_chars(Atom, Chars, ". "),
	read_from_chars(Chars, Term).

term_to_atom(Term, Atom) :-
	format_to_chars("~w", [Term], Chars),
	name(Atom, Chars).
