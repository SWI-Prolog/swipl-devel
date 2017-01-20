/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2001-2012, University of Amsterdam
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
            sub_var/2                   % -SubTerm, +Term (SWI extra)
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

