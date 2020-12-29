/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2001-2020, University of Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(occurs,
          [ contains_term/2,            % +SubTerm, +Term
            contains_var/2,             % +SubTerm, +Term
            free_of_term/2,             % +SubTerm, +Term
            free_of_var/2,              % +SubTerm, +Term
            occurrences_of_term/3,      % +SubTerm, +Term, ?Tally
            occurrences_of_var/3,       % +SubTerm, +Term, ?Tally
            sub_term/2,                 % -SubTerm, +Term
            sub_var/2,                  % -SubTerm, +Term (SWI extra)
            sub_term_shared_variables/3 % +Sub, +Term, -Vars
          ]).

/** <module> Finding and counting sub-terms

This  is  a  SWI-Prolog  implementation  of  the  corresponding  Quintus
library, based on the generalised arg/3 predicate of SWI-Prolog.

@see library(terms) provides similar predicates and is probably
     more wide-spread than this library.
*/

%!  contains_term(+Sub, +Term) is semidet.
%
%   Succeeds if Sub is contained in Term (=, deterministically)

contains_term(X, X) :- !.
contains_term(X, Term) :-
    compound(Term),
    arg(_, Term, Arg),
    contains_term(X, Arg),
    !.


%!  contains_var(+Sub, +Term) is det.
%
%   Succeeds if Sub is contained in Term (==, deterministically)

contains_var(X0, X1) :-
    X0 == X1,
    !.
contains_var(X, Term) :-
    compound(Term),
    arg(_, Term, Arg),
    contains_var(X, Arg),
    !.

%!  free_of_term(+Sub, +Term)
%
%   Succeeds of Sub does not unify to any subterm of Term

free_of_term(Sub, Term) :-
    \+ contains_term(Sub, Term).

%!  free_of_var(+Sub, +Term)
%
%   Succeeds of Sub is not equal (==) to any subterm of Term

free_of_var(Sub, Term) :-
    \+ contains_var(Sub, Term).

%!  occurrences_of_term(+SubTerm, +Term, ?Count)
%
%   Count the number of SubTerms in Term

occurrences_of_term(Sub, Term, Count) :-
    count(sub_term(Sub, Term), Count).

%!  occurrences_of_var(+SubTerm, +Term, ?Count)
%
%   Count the number of SubTerms in Term

occurrences_of_var(Sub, Term, Count) :-
    count(sub_var(Sub, Term), Count).

%!  sub_term(-Sub, +Term)
%
%   Generates (on backtracking) all subterms of Term.

sub_term(X, X).
sub_term(X, Term) :-
    compound(Term),
    arg(_, Term, Arg),
    sub_term(X, Arg).

%!  sub_var(-Sub, +Term)
%
%   Generates (on backtracking) all subterms (==) of Term.

sub_var(X0, X1) :-
    X0 == X1.
sub_var(X, Term) :-
    compound(Term),
    arg(_, Term, Arg),
    sub_var(X, Arg).


%!  sub_term_shared_variables(+Sub, +Term, -Vars) is det.
%
%   If Sub is a sub term of Term, Vars is bound to the list of variables
%   in Sub that also appear  outside  Sub   in  Term.  Note  that if Sub
%   appears twice in Term, its variables are all considered shared.
%
%   An  example  use-case  is  refactoring  a    large  clause  body  by
%   introducing intermediate predicates. This predicate   can be used to
%   find the arguments that must be passed to the new predicate.

sub_term_shared_variables(Sub, Term, Vars) :-
    term_replace_first(Term, Sub, true, Term2),
    term_variables(Term2, AllVars),
    term_variables(Sub, SubVars),
    intersection_eq(SubVars, AllVars, Vars).

term_replace_first(TermIn, From, To, TermOut) :-
    term_replace_(TermIn, From, To, TermOut, done(_)).

%term_replace(TermIn, From, To, TermOut) :-
%    term_replace_(TermIn, From, To, TermOut, all).

%!  term_replace_(+From, +To, +TermIn, -TermOut, +Done)
%
%   Replace instances (==/2) of From inside TermIn by To.

term_replace_(TermIn, _From, _To, TermOut, done(Done)) :-
    Done == true,
    !,
    TermOut = TermIn.
term_replace_(TermIn, From, To, TermOut, Done) :-
    From == TermIn,
    !,
    TermOut = To,
    (   Done = done(Var)
    ->  Var = true
    ;   true
    ).
term_replace_(TermIn, From, To, TermOut, Done) :-
    compound(TermIn),
    compound_name_arity(TermIn, Name, Arity),
    Arity > 0,
    !,
    compound_name_arity(TermOut, Name, Arity),
    term_replace_compound(1, Arity, TermIn, From, To, TermOut, Done).
term_replace_(Term, _, _, Term, _).

term_replace_compound(I, Arity, TermIn, From, To, TermOut, Done) :-
    I =< Arity,
    !,
    arg(I, TermIn, A1),
    arg(I, TermOut, A2),
    term_replace_(A1, From, To, A2, Done),
    I2 is I+1,
    term_replace_compound(I2, Arity, TermIn, From, To, TermOut, Done).
term_replace_compound(_I, _Arity, _TermIn, _From, _To, _TermOut, _).

%!  intersection_eq(+Small, +Big, -Shared) is det.
%
%   Shared are the variables in Small that   also appear in Big. The
%   variables in Shared are in the same order as Small.

intersection_eq([], _, []).
intersection_eq([H|T0], L, List) :-
    (   member_eq(H, L)
    ->  List = [H|T],
        intersection_eq(T0, L, T)
    ;   intersection_eq(T0, L, List)
    ).

member_eq(E, [H|T]) :-
    (   E == H
    ->  true
    ;   member_eq(E, T)
    ).


                 /*******************************
                 *              UTIL            *
                 *******************************/

%!  count(:Goal, -Count)
%
%   Count number of times Goal succeeds.

:- meta_predicate count(0,-).

count(Goal, Count) :-
    State = count(0),
    (   Goal,
        arg(1, State, N0),
        N is N0 + 1,
        nb_setarg(1, State, N),
        fail
    ;   arg(1, State, Count)
    ).

