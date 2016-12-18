/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2008-2015, University of Amsterdam
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

:- module(prolog_vm,
          [ vm_list/1
          ]).
:- use_module(library(prolog_clause)).
:- use_module(library(lists)).

/** <module> SWI-Prolog Virtual Machine utilities

This is an internal developers  module   to  manage  the virtual machine
instructions.
*/

:- meta_predicate
    vm_list(:).

%!  vm_list(:Spec) is det.
%
%   Lists  the  definition  of  the   predicates  matching  Spec  to
%   =current_output=. Spec is also allowed to be a clause-reference.

vm_list(_:Ref) :-
    blob(Ref, clause),
    !,
    (   nth_clause(_Head, N, Ref),
        format('~40c~nclause ~d (~w):~n~40c~n', [0'-, N, Ref, 0'-]),
        vm_list_clause(Ref),
        fail
    ;   true
    ).
vm_list(Spec) :-
    '$find_predicate'(Spec, List),
    (   member(PI, List),
        pi_to_head(PI, Head),
        unify_args(Head, Spec),
        predicate_name(Head, Name),
        format('~72c~n~w~n~72c~n', [0'=, Name, 0'=]),
        (   '$fetch_vm'(Head, 0, _, _)
        ->  vm_list_clause(Head)
        ;   format('    (No supervisor)~n')
        ),
        (   nth_clause(Head, N, Ref),
            clause(MHead, _, Ref),
            same_head(Head, MHead),
            format('~40c~nclause ~d (~w):~n~40c~n', [0'-, N, Ref, 0'-]),
            vm_list_clause(Ref),
            fail
        ;   true
        ),
        fail
    ;   true
    ).

pi_to_head(M:PI, M:Head) :-
    !,
    pi_to_head(PI, Head).
pi_to_head(Name/Arity, Head) :-
    functor(Head, Name, Arity).

vm_list_clause(Clause) :-
    vm_list_clause(Clause, 0).

vm_list_clause(Clause, PC) :-
    '$fetch_vm'(Clause, PC, NextPC, VMI),
    !,
    format('~t~d~4| ~q~n', [PC, VMI]),
    vm_list_clause(Clause, NextPC).
vm_list_clause(_, _).

%       Unify the arguments of the specification with the given term,
%       so we can partially instantate the head.

unify_args(_, _/_) :- !.                % Name/arity spec
unify_args(X, X) :- !.
unify_args(_:X, X) :- !.
unify_args(_, _).

same_head(X, X) :- !.
same_head(H1, H2) :-
    strip_module(H1, _, H),
    strip_module(H2, _, H).
