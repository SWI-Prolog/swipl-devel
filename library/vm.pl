/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2008-2020, University of Amsterdam
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
          [ vm_list/1,                  % :Spec
            clause_vm/2,                % +ClauseRef,-VM:list
            vmi_labels/2                % ?VMI,?Labeled
          ]).
:- autoload(library(lists),[member/2]).
:- autoload(library(prolog_clause),[predicate_name/2]).


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
    clause_vm(Clause, VM),
    vmi_labels(VM, Labeled),
    vm_list_labeled(Labeled, 0).

vm_list_labeled([], _).
vm_list_labeled([label(L),vmi(break(VMI),Size)|T], PC) :-
    !,
    format('~w: ~t~d~8| ~q % <breakpoint>~n', [L, PC, VMI]),
    PC1 is PC+Size,
    vm_list_labeled(T, PC1).
vm_list_labeled([label(L),vmi(VMI,Size)|T], PC) :-
    format('~w: ~t~d~8| ~q~n', [L, PC, VMI]),
    PC1 is PC+Size,
    vm_list_labeled(T, PC1).
vm_list_labeled([vmi(break(VMI),Size)|T], PC) :-
    !,
    format('~t~d~8| ~q % <breakpoint>~n', [PC, VMI]),
    PC1 is PC+Size,
    vm_list_labeled(T, PC1).
vm_list_labeled([vmi(VMI,Size)|T], PC) :-
    format('~t~d~8| ~q~n', [PC, VMI]),
    PC1 is PC+Size,
    vm_list_labeled(T, PC1).

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


%!  clause_vm(+ClauseRef, -VM:list) is det.
%
%   True when VM  is  the  virtual   machine  code  of  ClauseRef.  Each
%   instruction is a term vmi(VMI,Size).

clause_vm(Ref, VM) :-
    clause_vm(Ref, 0, VM).

clause_vm(Clause, PC, [vmi(VMI,Size)|T]) :-
    '$fetch_vm'(Clause, PC, NextPC, VMI),
    !,
    Size is NextPC-PC,
    clause_vm(Clause, NextPC, T).
clause_vm(_, _, []).


%!  vmi_labels(?VMI, ?Labeled)
%
%   Translated between a raw and  a   labeled  representation  for a VMI
%   sequence  as  produced  by  clause_vm/2.    Assumes   we  only  jump
%   _forwards_.
%
%   In the labeled represention the `jump`   arguments of VMIs are label
%   names and there are entries label(Name) in the list.

vmi_labels(VMI, Labeled) :-
    nonvar(VMI),
    !,
    label_vmi(VMI, 0, 0, [], Labeled).
vmi_labels(VMI, Labeled) :-
    unlabel_vmi(Labeled, 0, [], VMI).

% Raw --> Labeled

label_vmi([], _, _, _, []).
label_vmi([H|T], Here0, LI0, Pending0, Labeled) :-
    H = vmi(VMI0,Size),
    Here is Here0+Size,
    new_labels(VMI0, VMI, LI0, LI1, Here0, Here, Pending0, Pending1),
    (   selectchk(Label-Here0, Pending1, Pending2)
    ->  Labeled = [label(Label),vmi(VMI,Size)|Labeled1]
    ;   Labeled = [vmi(VMI,Size)|Labeled1],
        Pending2 = Pending1
    ),
    label_vmi(T, Here, LI1, Pending2, Labeled1).

new_labels(break(VMI0), break(VMI), LI0, LI, Start, End, Labels0, Labels) :-
    !,
    new_labels(VMI0, VMI, LI0, LI, Start, End, Labels0, Labels).
new_labels(VMI0, VMI, LI0, LI, Start, End, Labels0, Labels) :-
    VMI0 =.. [Name|Argv0],
    '$vmi_property'(Name, argv(ArgvTypes)),
    jmp_rel(Name, Start, End, Rel),
    new_labels_(ArgvTypes, Argv0, Argv, LI0, LI, Rel, Labels0, Labels),
    VMI =.. [Name|Argv].

%!  jmp_rel(+VMIName, +Start, +End, -JmpRel)
%
%   Relative position for the (choice) jump.  This   is  the  end of the
%   instruction for most, but after the   address  for the compiled trie
%   instructions.   Should be made consistent.

jmp_rel(TrieVMI, Start, _End, Rel) :-
    trie_vmi(TrieVMI), !,
    Rel is Start+2.
jmp_rel(_, _, End, End).

trie_vmi(VMI) :- sub_atom(VMI, 0, _, _, t_).

new_labels_([], [], [], LI, LI, _, Labels, Labels).
new_labels_([jump|TT], [Offset|AT], [Label|LT], LI0, LI, End, Labels0, Labels) :-
    !,
    To is End+Offset,
    (   memberchk(Label-To, Labels0)
    ->  Labels1 = Labels0,
        LI1 = LI0
    ;   LI1 is LI0+1,
        atom_concat('L', LI1, Label),
        Labels1 = [Label-To|Labels0]
    ),
    new_labels_(TT, AT, LT, LI1, LI, End, Labels1, Labels).
new_labels_([_|TT], [A|AT], [A|LT], LI0, LI, End, Labels0, Labels) :-
    new_labels_(TT, AT, LT, LI0, LI, End, Labels0, Labels).

% Labeled --> Raw

unlabel_vmi([], _, _, []).
unlabel_vmi([label(L)|T0], Here, Labels0, T) :-
    !,
    resolve_labels(L, Here, Labels0, Labels),
    unlabel_vmi(T0, Here, Labels, T).
unlabel_vmi([vmi(VMI0,Size)|T0], Here0, Labels0, [vmi(VMI,Size)|T]) :-
    Here is Here0+Size,
    get_labels(VMI0, VMI, Here, Labels0, Labels),
    unlabel_vmi(T0, Here, Labels, T).

get_labels(VMI0, VMI, Here, Labels0, Labels) :-
    VMI0 =.. [Name|Argv0],
    '$vmi_property'(Name, argv(ArgvTypes)),
    get_labels_(ArgvTypes, Argv0, Argv, Here, Labels0, Labels),
    VMI =.. [Name|Argv].

get_labels_([], [], [], _, Labels, Labels).
get_labels_([jump|TT], [Label|LT], [Offset|AT], Here,
            Labels0, [l(Label,Here,Offset)|Labels]) :-
    !,
    get_labels_(TT, LT, AT, Here, Labels0, Labels).
get_labels_([_|TT], [A|LT], [A|AT], Here, Labels0, Labels) :-
    get_labels_(TT, LT, AT, Here, Labels0, Labels).

resolve_labels(L, Here, Labels0, Labels) :-
    selectchk(l(L,End,Offset), Labels0, Labels1),
    !,
    Offset is Here-End,
    resolve_labels(L, Here, Labels1, Labels).
resolve_labels(_, _, Labels, Labels).
