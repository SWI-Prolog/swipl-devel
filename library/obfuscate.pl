/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
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

:- module(obfuscate, []).

/** <module> Code obfuscating

This       module       provides       an       implementation       for
prolog:obfuscate_identifiers/1, a hook into   library(qsave) that allows
mapping atoms to other atoms while creating a saved state.

This library is  used  by  qsave_program/2   and  the  `-c`  option. The
following is good way to create a binary  version of your code that runs
on a machine with the same version of SWI-Prolog installed.

    swipl -o myprog -O --autoload=false --obfuscate=true -c load.pl

This implementation is overly conservative: atoms   are  only renamed if
they are used once. This is  verified   by  checking  the atom reference
count. If this is `1`, the  atom  is   _only_  used  in the functor that
defines the predicate.

@tbd Using the current obfuscation scheme we  must verify that all usage
of an atom refers to a predicate   name. Right now, declarations such as
=|:- dynamic p/1.|= ensure `p` is used in   `p/1`  and p(_) and thus has
two references.
*/

:- dynamic
    nomap/1.

prolog:obfuscate_identifiers(_) :-
    print_message(informational, obfuscate(start)),
    flag('$obfuscate_id', Old, 1),
    forall(module_property(M, class(user)),
           obfuscate_module(M)),
    flag('$obfuscate_id', End, Old),
    Obfuscated is End-Old,
    print_message(informational, obfuscate(done(Obfuscated))).

obfuscate_module(M) :-
    forall(private_predicate(M:P),
           obfuscate_predicate(M:P)).

private_predicate(M:Head) :-
    current_predicate(M:Name/Arity),
    functor(Head, Name, Arity),
    \+ predicate_property(M:Head, imported_from(_)),
%   \+ predicate_property(M:Head, exported),
    \+ predicate_property(M:Head, foreign),
    \+ predicate_property(M:Head, public).

obfuscate_predicate(_M:P) :-
    functor(P, Name, _Arity),
    '$atom_references'(Name, 1),
    !,
    flag('$obfuscate_id', Id, Id+1),
    atom_concat('$P$', Id, Name2),
    (   nomap(Name)
    ->  true
    ;   catch('$map_id'(Name, Name2), _, fail)
    ->  print_message(silent, obfuscate(map(Name, Name2)))
    ;   '$unmap_id'(Name),
        asserta(nomap(Name))
    ).
obfuscate_predicate(_).


		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile
    prolog:message//1.

prolog:message(obfuscate(Msg)) -->
    message(Msg).

message(start) -->
    [ 'Obfuscating predicates names (conservative) ...'-[] ].
message(done(Count)) -->
    [ 'Obfuscated ~D predicate names'-[Count] ].
message(map(From, To)) -->
    [ 'Obfuscating ~q as ~q'-[From, To] ].
