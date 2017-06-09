/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2013, University of Amsterdam
                         VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(test_singleton,
	  [ test_singleton/0,
	    test_singleton/1
	  ]).
:- use_module(library(assoc)).
:- use_module(library(pairs)).
:- use_module(library(lists)).
:- use_module(library(varnumbers)).
:- if(exists_source(library(memfile))).
:- use_module(library(memfile)).
ok(true).
:- else.
ok(false).
:- endif.
:- use_module(library(listing)).

/** <module> Test semantic singleton detection
*/

test_singleton :-
	ok(true), !,
	test_singleton(3).
test_singleton :-
	format(user_error, 'Test requires library(memfile)~n', []).

t(v(V), V, 1).
t(true, _, 1).
t((A;B), V, S) :- S>1, S1 is S-1, t(A, V, S1), t(B, V, S1).
t((A,B), V, S) :- S>1, S1 is S-1, t(A, V, S1), t(B, V, S1).

body(B, Max) :-
	between(1, Max, S),
	t(B, _Var, S).

test_singleton(Max) :-
	forall(body(Body, Max),
	       verify_singletons(Body)).

verify_singletons(Body) :-
	compiler_found_singletons(Body, Found),
	singletons(Body, Singletons),
	(   Found == Singletons
	->  true
	;   format('Clause:~n'),
	    \+ \+ (numbervars(Body, 0, _),
		   portray_clause((t:-Body)),
		   format('Found: ~w~n', [Found]),
		   format('Singletons: ~w~n', [Singletons])),
	    fail
	).

compiler_found_singletons(Body, Singletons) :-
	findall(Copy, copied_singletons(Body, Copy), [Body+Singletons0]),
	sort(Singletons0, Singletons).

copied_singletons(Body, Copy) :-
	numbervars(Body, 0, _),
	new_memory_file(MF),
	setup_call_cleanup(
	    open_memory_file(MF, write, Out),
	    portray_clause(Out, (t:-Body)),
	    close(Out)),
	catch_messages(warning,
	    setup_call_cleanup(
		open_memory_file(MF, read, In),
		load_files(test_singletons, [stream(In), silent(true)]),
		close(In)),
	    Messages),
	free_memory_file(MF),
	phrase(message_singletons(Messages), NamedSingletons),
	varnumbers(Body+NamedSingletons, Copy).

message_singletons([]) --> !.
message_singletons([H|T]) --> !,
	message_singletons(H),
	message_singletons(T).
message_singletons(compiler_warnings(_Clause, Messages)) --> !,
	message_singletons(Messages).
message_singletons(branch_singleton(V)) --> !,
	[V].
message_singletons(singletons(Vars)) --> vars(Vars).

vars([]) --> [].
vars([H|T]) --> ['$VAR'(I)], {var_name(I,H)},vars(T).

%%	singletons(+Body, -Vars)
%
%	True when Vars is the set of all singletons in Body. Singletons
%	are detected as follows:
%
%	  - We count the times a variable appears in a term.  Each
%	    variable appearing exactly once is a singletons.
%	  - Inside a disjunction, we count the left and right and for
%	    each new variable appearing in either branch, we combine
%	    the counts as follows (use symmetric extensions):
%
%	      - 0+N --> N
%	      - 1+N --> 1
%	      - N+N --> 2
%
%	    The 2nd case above expresses the fact that if the variable
%	    is singleton in one of the branches, it needs to appear in
%	    a conjunction to become non-singleton.

singletons(Body, Singletons) :-
	count_vars(Body, Assoc, _),
	assoc_to_list(Assoc, VC),
	transpose_pairs(VC, CV),
	cv_singletons(CV, Singletons).

cv_singletons([1-V|T0], [V|T]) :- !,
	cv_singletons(T0, T).
cv_singletons(_, []).

count_vars(G, B, Vars) :-
	empty_assoc(B0),
	count_vars(G, B0, B, Vars, []).

count_vars((P;Q), B0, B, New, NewT) :- !,
	count_vars(P, BP, NewP),
	count_vars(Q, BQ, NewQ),
	append(NewP, NewQ, NewPQ0),
	sort(NewPQ0, NewPQ),
	append(NewPQ, NewT, New),
	join_vars(NewPQ, BP, BQ, B0, B).
count_vars((P,Q), B0, B, New, NewT) :- !,
	count_vars(P, B0, B1, New, New1),
	count_vars(Q, B1, B, New1, NewT).
count_vars(T, B0, B, New, NewT) :-
	term_vars(T, B0, B, New, NewT).

term_vars(Var, B0, B, New, NewT) :-
	var(Var), !,
	(   get_assoc(Var, B0, C0, B, C)
	->  succ(C0, C),
	    NewT = New
	;   put_assoc(Var, B0, 1, B),
	    New = [Var|NewT]
	).
term_vars(T, B0, B, New, NewT) :-
	compound(T), !,
	T =.. [_|Args],
	term_vars_list(Args, B0, B, New, NewT).
term_vars(_, B, B, New, New).

term_vars_list([], B, B, New, New).
term_vars_list([H|T], B0, B, New, NewT) :-
	term_vars(H, B0, B1, New, NewT0),
	term_vars_list(T, B1, B, NewT0, NewT).

join_vars([], _, _, B, B).
join_vars([H|T], BP, BQ, B0, B) :-
	var_count(H, BP, CP),
	var_count(H, BQ, CQ),
	joint_counts(CP, CQ, CPQ),
	(   get_assoc(H, B0, C0, B1, C)
	->  C is C0+CPQ
	;   put_assoc(H, B0, CPQ, B1)
	),
	join_vars(T, BP, BQ, B1, B).

var_count(V, A, C) :-
	get_assoc(V, A, C), !.
var_count(_, _, 0).

joint_counts(0,N,N) :- !.
joint_counts(N,0,N) :- !.
joint_counts(_,1,1) :- !.
joint_counts(1,_,1) :- !.
joint_counts(_,_,2).


%%	catch_messages(+Kind, :Goal, -Messages) is semidet.

:- thread_local
	message/1.
:- meta_predicate
	catch_messages(?, 0, -).

catch_messages(Kind, Goal, Messages) :-
	setup_call_cleanup(
	    asserta((user:thread_message_hook(Term, Kind, _) :-
		        \+ \+ (prolog_load_context(variable_names, VarNames),
			       bind_variable_names(VarNames),
			       assertz(message(Term)))), Ref),
	    once(Goal),
	    erase(Ref)),
	findall(Msg, retract(message(Msg)), Messages).

bind_variable_names([]).
bind_variable_names([Name='$VAR'(Int)|T]) :- !,
	var_name(Int, Name),
	bind_variable_names(T).
bind_variable_names([_|T]) :-
	bind_variable_names(T).

var_name(N, Name) :-
	atom_codes(Name, [C]),
	between(0'A, 0'Z, C),
	N is C - 0'A.
